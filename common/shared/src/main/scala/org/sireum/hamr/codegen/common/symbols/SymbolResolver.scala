// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.HamrProperties.HAMR__BIT_CODEC_MAX_SIZE
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.resolvers.BTSResolver
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BaseType, TypeUtil}
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, CodeGenPlatform, ExperimentalOptions}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Annex, BTSBLESSAnnexClause, MTransformer => MAirTransformer}
import org.sireum.message.{Position, Reporter}

object SymbolResolver {

  @datatype class AadlMaps(airComponentMap: HashSMap[String, ir.Component],
                           airFeatureMap: HashSMap[String, ir.Feature],
                           airClassifierMap: HashSMap[String, ir.Component],

                           connections: ISZ[ir.Connection],
                           connectionsMap: Map[ISZ[String], ir.Connection],

                           connectionInstances: ISZ[ir.ConnectionInstance],
                           connectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance],
                           altConnectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance],

                           // this map is currently incomplete for the feature groups in pca pump
                           connectionInstanceToConnectionsMap: Map[ISZ[String], ISZ[ir.Connection]]
                          ) {
    def validate(): Unit = {
      for(c <- connections) {
        for(ci <- c.connectionInstances) {
          assert(altConnectionInstancesMap.contains(ci.name), s"Couldn't find conn instance ${ci.name} for ${c.name.name}")
        }
      }
    }
  }

  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              aadlMaps: AadlMaps,
              options: CodeGenConfig,
              reporter: Reporter): SymbolTable = {
    val st = buildSymbolTable(model, aadlTypes, aadlMaps, options, reporter)
    if(reporter.hasError) {
      return st
    } else {
      var annexInfos: HashSMap[AadlComponent, ISZ[AnnexInfo]] = HashSMap.empty
      for(component <- st.componentMap.values) {
        var ais: ISZ[AnnexInfo] = ISZ()
        for(annex <- component.component.annexes) {
          processAnnex(st, aadlTypes, annex, reporter) match {
            case Some(ai) => ais = ais :+ ai
            case _ =>
              assert(reporter.hasError, "No annex info returned so expecting an error")
          }
        }
        annexInfos = annexInfos + (component ~> ais)
      }
      return st(annexInfos = annexInfos)
    }
  }

  def buildSymbolTable(model: ir.Aadl,
                       aadlTypes: AadlTypes,
                       aadlMaps: AadlMaps,
                       options: CodeGenConfig,
                       reporter: Reporter): SymbolTable = {
    var featureMap: HashSMap[String, AadlFeature] = HashSMap.empty

    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty

    var connectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

    // port-paths -> connInstances
    var inConnections: HashSMap[String, ISZ[ir.ConnectionInstance]] = HashSMap.empty
    var outConnections: HashSMap[String, ISZ[ir.ConnectionInstance]] = HashSMap.empty


    val components = model.components.filter(f => f.category != ir.ComponentCategory.Data)
    if (components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    def buildComponentMap(c: ir.Component): Unit = {
      val name = CommonUtil.getName(c.identifier)
      assert(!airComponentMap.contains(name))
      airComponentMap = airComponentMap + (name ~> c)
      if (c.classifier.nonEmpty) {
        airClassifierMap = airClassifierMap + (c.classifier.get.name ~> c)
      }

      for (f <- c.features) {

        def resolveFeature(_f: ir.Feature, path: ISZ[String]): Unit = {
          _f match {
            case fa: ir.FeatureAccess =>
              val featurePath: String = CommonUtil.getName(fa.identifier)
              airFeatureMap = airFeatureMap + (featurePath ~> fa)
            case fe: ir.FeatureEnd =>
              val featurePath: String = CommonUtil.getName(fe.identifier)
              airFeatureMap = airFeatureMap + (featurePath ~> fe)

              if (CommonUtil.isDataPort(fe) && fe.classifier.isEmpty) {
                reporter.warn(None(), CommonUtil.toolName, s"Data type missing for feature ${fe.category} ${CommonUtil.getName(fe.identifier)}")
              }
            case fg: ir.FeatureGroup =>
              for(_fge <- fg.features) {
                resolveFeature(_fge, path)
              }
          }
        }

        resolveFeature(f, c.identifier.name)
      }
      for (sc <- c.subComponents) {
        buildComponentMap(sc)
      }
    }

    buildComponentMap(system)

    def resolver(sys: ir.Component): B = {

      for (c <- airComponentMap.values) {

        for (ci <- c.connectionInstances) {
          if (isHandledConnection(ci, airComponentMap, airFeatureMap)) {
            connectionInstances = connectionInstances :+ ci

            def add(portPath: String, isIn: B): Unit = {
              val map: HashSMap[String, ISZ[ir.ConnectionInstance]] = isIn match {
                case T => inConnections
                case F => outConnections
              }
              var cis: ISZ[ir.ConnectionInstance] = map.get(portPath) match {
                case Some(x) => x
                case _ => ISZ()
              }
              cis = cis :+ ci
              isIn match {
                case T => inConnections = inConnections + (portPath ~> cis)
                case F => outConnections = outConnections + (portPath ~> cis)
              }
            }

            val portNames = getPortConnectionNames(ci, airComponentMap)
            add(portNames._1, F)
            add(portNames._2, T)
          } else {
            val src = airComponentMap.get(CommonUtil.getName(ci.src.component)).get
            val dst = airComponentMap.get(CommonUtil.getName(ci.dst.component)).get

            val srcName: String = if (ci.src.feature.nonEmpty) {
              s"${CommonUtil.getLastName(src.identifier)}.${CommonUtil.getLastName(ci.src.feature.get)}"
            } else {
              CommonUtil.getLastName(src.identifier)
            }

            val dstName: String = if (ci.dst.feature.nonEmpty) {
              s"${CommonUtil.getLastName(dst.identifier)}.${CommonUtil.getLastName(ci.dst.feature.get)}"
            } else {
              CommonUtil.getLastName(dst.identifier)
            }
            reporter.info(None(), CommonUtil.toolName, s"Ignoring ${src.category} to ${dst.category} connection: ${srcName} -> ${dstName}")
          }
        }
      }

      return !reporter.hasError
    }

    resolver(system)

    var componentMap: HashSMap[String, AadlComponent] = HashSMap.empty

    def process(c: ir.Component, parent: Option[String]): AadlComponent = {
      val path = CommonUtil.getName(c.identifier)
      val identifier = CommonUtil.getLastName(c.identifier)

      if (componentMap.contains(path)) {
        return componentMap.get(path).get
      }

      def getFeatureEndType(f: ir.FeatureEnd): Option[AadlType] = {
        val ret: Option[AadlType] = f.classifier match {
          case Some(c) => Some(aadlTypes.typeMap.get(c.name).get)
          case _ => None()
        }
        return ret
      }

      def resolveFeature2(feature: ir.Feature, featureGroupIds: ISZ[String], parentIsThread: B): ISZ[AadlFeature] = {
        feature match {
          case fg: ir.FeatureGroup =>
            var ret: ISZ[AadlFeature] = ISZ()
            for(_f <- fg.features) {
              ret = ret ++ resolveFeature2(_f, featureGroupIds :+ CommonUtil.getLastName(fg.identifier), parentIsThread)
            }
            return ret
          case _ =>
            val featurePath = CommonUtil.getName(feature.identifier)
            val airFeature = airFeatureMap.get(featurePath).get
            assert(airFeature == feature)

            feature match {
              case fe: ir.FeatureEnd if fe.direction == ir.Direction.InOut => {
                if (fe.category == ir.FeatureCategory.FeatureGroup) {
                  // oddly, an empty feature group is treated as a feature end by osate.  Can be
                  // ignored as it doesn't add any features to the component
                }
                else if (parentIsThread){
                  val id = CommonUtil.getLastName(fe.identifier)
                  val mesg = s"Invalid direction: ${fe.direction} for ${id}.  Only uni-directional ports are supported"
                  reporter.error(fe.identifier.pos, CommonUtil.toolName, mesg)
                }
              }
              case _ =>
            }

            val aadlFeature: AadlFeature = feature.category match {
              case ir.FeatureCategory.EventPort => {
                val fend = feature.asInstanceOf[ir.FeatureEnd]
                AadlEventPort(
                  feature = fend,
                  featureGroupIds = featureGroupIds,
                  direction = fend.direction
                )
              }
              case ir.FeatureCategory.DataPort => {
                val fend = feature.asInstanceOf[ir.FeatureEnd]
                AadlDataPort(
                  feature = fend,
                  featureGroupIds = featureGroupIds,
                  direction = fend.direction,
                  aadlType = getFeatureEndType(fend).get
                )
              }
              case ir.FeatureCategory.EventDataPort => {
                val fend = feature.asInstanceOf[ir.FeatureEnd]
                AadlEventDataPort(
                  feature = fend,
                  featureGroupIds = featureGroupIds,
                  direction = fend.direction,
                  aadlType = getFeatureEndType(fend).get)
              }
              case ir.FeatureCategory.BusAccess => {
                val facc = feature.asInstanceOf[ir.FeatureAccess]
                AadlBusAccess(
                  feature = facc,
                  featureGroupIds = featureGroupIds,
                  kind = facc.accessType)
              }
              case ir.FeatureCategory.DataAccess => {
                val facc = feature.asInstanceOf[ir.FeatureAccess]
                AadlDataAccess(
                  feature = facc,
                  featureGroupIds = featureGroupIds,
                  kind = facc.accessType)
              }
              case ir.FeatureCategory.SubprogramAccess => {
                val facc = feature.asInstanceOf[ir.FeatureAccess]
                AadlSubprogramAccess(
                  feature = facc,
                  featureGroupIds = featureGroupIds,
                  kind = facc.accessType)
              }
              case ir.FeatureCategory.SubprogramAccessGroup => {
                val facc = feature.asInstanceOf[ir.FeatureAccess]
                AadlSubprogramGroupAccess(
                  feature = facc,
                  featureGroupIds = featureGroupIds,
                  kind = facc.accessType)
              }
              case _ => AadlFeatureTODO(
                feature = feature,
                featureGroupIds = featureGroupIds)
            }

            featureMap = featureMap + (featurePath ~> aadlFeature)

            return ISZ(aadlFeature)
        }
      }

      def handleDeviceOrThread(): AadlComponent = {
        {
          //assert(c.subComponents.isEmpty) // TODO handle subprograms etc

          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))
          var aadlFeatures: ISZ[AadlFeature] = ISZ()

          for (feature <- c.features) {
            aadlFeatures = aadlFeatures ++ resolveFeature2(feature, ISZ(), T)
          }

          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(x) => x
            case _ =>
              val (protocol, mesg) : (Dispatch_Protocol.Type, String) =c.category match {
                case ir.ComponentCategory.Thread =>
                  val mesg = s"Dispatch Protocol not specified for thread ${identifier}, assuming Sporadic"
                  (Dispatch_Protocol.Sporadic, mesg)
                case ir.ComponentCategory.Device =>
                  val mesg = s"Dispatch Protocol not specified for device ${identifier}, assuming Periodic"
                  (Dispatch_Protocol.Periodic, mesg)
                case _ =>
                  halt("Infeasible")
              }
              // TODO should be handled by a rewriter -- or better yet, just reject the model
              reporter.warn(c.identifier.pos, CommonUtil.toolName, mesg)
              protocol
          }

          val period = PropertyUtil.getPeriod(c)

          val ret: AadlThreadOrDevice = if (c.category == ir.ComponentCategory.Device) {
            AadlDevice(
              component = c,
              parent = parent,
              path = path,
              identifier = identifier,
              dispatchProtocol = dispatchProtocol,
              period = period,
              features = aadlFeatures,
              subComponents = subComponents,
              connectionInstances = c.connectionInstances)

          } else {
            assert(c.category == ir.ComponentCategory.Thread)

            AadlThread(
              component = c,
              parent = parent,
              path = path,
              identifier = identifier,
              dispatchProtocol = dispatchProtocol,
              period = period,
              features = aadlFeatures,
              subComponents = subComponents,
              connectionInstances = c.connectionInstances)
          }
          return ret
        }
      }

      def handleSubprogram(): AadlSubprogram = {
        assert(c.subComponents.isEmpty, s"Need to handle subcomponents of subprograms: ${c}")
        assert(c.connectionInstances.isEmpty, s"Not expecting subprograms to have connection instances: ${c}")

        val parameters: ISZ[AadlParameter] = c.features.map(p => {
          p match {
            case fe: ir.FeatureEnd =>
              val paramType: AadlType = getFeatureEndType(fe).get

              AadlParameter(
                feature = fe,
                featureGroupIds = ISZ(),
                aadlType = paramType,
                direction = fe.direction)

            case _ => halt(s"Unexpected feature type ${p}")
          }
        })

        val ret: AadlSubprogram = AadlSubprogram(
          component = c,
          parent = parent,
          path = path,
          identifier = identifier,
          subComponents = ISZ(),
          parameters = parameters,
          connectionInstances = ISZ())

        return ret
      }

      val aadlComponent: AadlComponent = c.category match {
        case ir.ComponentCategory.System => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlSystem(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)
        }
        case ir.ComponentCategory.Processor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}: ${identifier}")
          AadlProcessor(
            component = c,
            parent = None(),
            path = path,
            identifier = identifier,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances)
        }
        case ir.ComponentCategory.VirtualProcessor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}? ${identifier}")

          val boundProcessor: Option[String] = PropertyUtil.getActualProcessorBinding(c)

          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(Dispatch_Protocol.Periodic) => Dispatch_Protocol.Periodic
            case Some(x) =>
              val mesg = s"Dispatch Protocol for virtual processor ${identifier} must be Periodic instead of ${x}"
              reporter.error(c.identifier.pos, CommonUtil.toolName, mesg)
              x
            case _ =>
              Dispatch_Protocol.Periodic
          }

          AadlVirtualProcessor(
            component = c,
            parent = None(),
            path = path,
            identifier = identifier,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances,
            dispatchProtocol = dispatchProtocol,
            boundProcessor = boundProcessor
          )
        }
        case ir.ComponentCategory.Process => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          val boundProcessor: Option[String] = PropertyUtil.getActualProcessorBinding(c)

          var aadlFeatures: ISZ[AadlFeature] = ISZ()
          for(f <- c.features){
            aadlFeatures = aadlFeatures ++ resolveFeature2(f, ISZ(), F)
          }

          PropertyUtil.getDiscreetPropertyValue(c.properties, "HAMR::Component_Type") match {
            case Some(x) =>
              val msg = s"Identifying VM bound processes via HAMR::Component_Type is no longer supported. Remove the annotation and ensure process ${path} is bound to a virtual processor instead"
              reporter.error(c.identifier.pos, CommonUtil.toolName, msg)
            case _ =>
          }

          AadlProcess(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            boundProcessor = boundProcessor,
            subComponents = subComponents,
            features = aadlFeatures,
            connectionInstances = c.connectionInstances)
        }

        case ir.ComponentCategory.Device => handleDeviceOrThread()

        case ir.ComponentCategory.Thread => handleDeviceOrThread()

        case ir.ComponentCategory.ThreadGroup =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlThreadGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Subprogram => handleSubprogram()

        case ir.ComponentCategory.SubprogramGroup =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlSubprogramGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Data =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlData(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Bus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.VirtualBus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlVirtualBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Memory =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlMemory(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Abstract =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlAbstract(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)
      }

      componentMap = componentMap + (path ~> aadlComponent)
      return aadlComponent
    }

    val aadlSystem = process(system, None()).asInstanceOf[AadlSystem]

    var aadlConnections: ISZ[AadlConnection] = ISZ()
    val shouldUseRawConnections: B = PropertyUtil.getUseRawConnection(system.properties)

    def resolveAadlConnectionInstances(): Unit = {
      for (ci <- connectionInstances) {
        val aadlConnection: AadlConnection = ci.kind match {
          case ir.ConnectionKind.Port => {
            val name = CommonUtil.getName(ci.name)

            val srcFeatureName = CommonUtil.getName(ci.src.feature.get)
            val srcComponent = componentMap.get(CommonUtil.getName(ci.src.component)).get
            val srcFeature = featureMap.get(srcFeatureName).get

            val dstFeatureName = CommonUtil.getName(ci.dst.feature.get)
            val dstComponent = componentMap.get(CommonUtil.getName(ci.dst.component)).get
            val dstFeature = featureMap.get(dstFeatureName).get

            val connectionDataType: AadlType =
              (CommonUtil.isDataPort(srcFeature.feature), CommonUtil.isDataPort(dstFeature.feature)) match {
                case (T, T) =>
                  val srcClassifier = srcFeature.feature.asInstanceOf[ir.FeatureEnd].classifier.get
                  val dstClassifier = dstFeature.feature.asInstanceOf[ir.FeatureEnd].classifier.get

                  val t: AadlType = (aadlTypes.typeMap.get(srcClassifier.name), aadlTypes.typeMap.get(dstClassifier.name)) match {
                    case (Some(srcType), Some(dstType)) =>
                      // TODO: this sanity check is too strong in general, but matches the models currently
                      //       being processed.  Need to test with connectionInstances involving type extensions
                      assert(srcType == dstType, s"Expecting both sides of connection ${name} to have the same type")
                      srcType
                    case _ =>
                      // sanity check
                      halt(s"Expecting both src and dst features of connection ${name} to have resolved types")
                  }

                  t
                case (F, F) =>
                  // sanity checking
                  assert(CommonUtil.isEventPort(srcFeature.feature))
                  assert(CommonUtil.isEventPort(dstFeature.feature))

                  TypeUtil.EmptyType
                case _ =>
                  // sanity check
                  halt(s"Both sides of ${name} must be (event) data ports or just event ports, mixtures not allowed")
              }

            AadlPortConnection(
              name,
              srcComponent,
              srcFeature,
              dstComponent,
              dstFeature,
              connectionDataType,
              ci
            )
          }
          case _ => AadlConnectionTODO()
        }

        aadlConnections = aadlConnections :+ aadlConnection
      }
    }

    resolveAadlConnectionInstances()

    val annexInfos: HashSMap[AadlComponent, ISZ[AnnexInfo]] = HashSMap.empty

    val symbolTable = SymbolTable(

      rootSystem = aadlSystem,
      componentMap = componentMap,
      featureMap = featureMap,
      aadlConnections = aadlConnections,

      annexInfos = annexInfos,
      
      airComponentMap = airComponentMap,
      airFeatureMap = airFeatureMap,
      airClassifierMap = airClassifierMap,

      aadlMaps = aadlMaps,

      connections = connectionInstances,
      inConnections = inConnections,
      outConnections = outConnections)

    {
      for(thread <- symbolTable.getThreads()) {
        if(thread.dispatchProtocol == Dispatch_Protocol.Periodic && thread.period.isEmpty) {
          val mesg = s"Must specify ${OsateProperties.TIMING_PROPERTIES__PERIOD} for periodic thread ${thread.identifier}"
          reporter.error(thread.component.identifier.pos, CommonUtil.toolName, mesg)
        }
      }
    }

    { // restrict when wire protocol and CakeML components are allowed
      if(symbolTable.hasCakeMLComponents() || shouldUseRawConnections) {
        if(options.platform == CodeGenPlatform.SeL4_Only || options.platform == CodeGenPlatform.SeL4_TB) {
          var reasons: ISZ[String] = ISZ()
          if(symbolTable.hasCakeMLComponents()) { reasons = reasons :+ "CakeML components" }
          if(shouldUseRawConnections) { reasons = reasons :+ "wire protocol" }
          val mesg = st"${options.platform} platform does not support ${(reasons, ", ")}".render
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    var validProcessorBindings: B = T

    {
      if (symbolTable.hasVM()) {

        var validProcessors: ISZ[AadlProcessor] = ISZ()
        var seenVirtualProcessor: Set[AadlVirtualProcessor] = Set.empty
        for(process <- symbolTable.getProcesses().filter(p => p.toVirtualMachine(symbolTable))) {
          assert(process.boundProcessor.nonEmpty, s"Unexpected: ${process.identifier} is going to a vm but it isn't bound to a processor?")
          assert(symbolTable.componentMap.contains(process.boundProcessor.get), s"Unexpected: unable to resolve ${process.identifier}'s bound processor ${process.boundProcessor.get}")

          symbolTable.getBoundProcessor(process) match {
            case Some(avp: AadlVirtualProcessor) =>
              if(seenVirtualProcessor.contains(avp)) {
                val msg = s"Multiple processes are bound to the virtual processor ${avp.identifier}. That might be acceptable, but is currently unexpected. Please report"
                reporter.error(avp.component.identifier.pos, CommonUtil.toolName, msg)
                validProcessorBindings = F
              }
              seenVirtualProcessor = seenVirtualProcessor + avp
              avp.boundProcessor match {
                case Some(avpsBoundProcessor) =>
                  symbolTable.componentMap.get(avpsBoundProcessor) match {
                    case Some(avp2: AadlVirtualProcessor) =>
                      val mesg = s"Chained virtual processors is not supported.  Bind virtual processor ${avp.identifier} to an actual processor"
                      reporter.error(avp2.component.identifier.pos, CommonUtil.toolName, mesg)
                      validProcessorBindings = F
                    case Some(ap: AadlProcessor) =>
                      PropertyUtil.getDiscreetPropertyValue(avp.component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__OS) match {
                        case Some(ir.ValueProp(os)) =>
                          if (os != "Linux") {
                            val mesg = s"Invalid OS ${os} for virtual processor ${avp.identifier}.  HAMR only supports Linux based virtual machines"
                            reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                            validProcessorBindings = F
                          }
                        case _ =>
                          val mesg = s"${CasePropertiesProperties.PROP__CASE_PROPERTIES__OS} not provided for virtual processor ${avp.identifier}.  Assuming Linux"
                          reporter.warn(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                      }

                      // ok, virtual processor is bound to an actual processor
                      validProcessors = validProcessors :+ ap
                    case _ =>
                      val mesg = s"Unexpected: couldn't resolve the bound processor ${avpsBoundProcessor} for virtual processor ${avp.identifier}. Please report"
                      reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                      validProcessorBindings = F
                  }
                case _ =>
                  val mesg = s"Virtual processor ${avp.identifier} must be bound to an actual processor since process ${process.identifier} is bound to it"
                  reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                  validProcessorBindings = F
              }
            case Some(x: AadlProcessor) =>
              validProcessors = validProcessors :+ x // ok, process is bound directly to an actual processor
            case x =>
              val mesg = s"Unexpected: process ${process.identifier} is bound to ${x} rather than a processor"
              reporter.error(process.component.identifier.pos, CommonUtil.toolName, mesg)
              validProcessorBindings = F
          }
        }

        for(p <- validProcessors){
          p.getPacingMethod() match {
            case Some(CaseSchedulingProperties.PacingMethod.SelfPacing) =>
              val mesg = s"Model has virtual machines so it must use the pacer component style of pacing"
              reporter.error(p.component.identifier.pos, CommonUtil.toolName, mesg)
            case _ =>
          }
        }
      }
    }

    val willUsePacer: B = validProcessorBindings && PacerUtil.canUseDomainScheduling(symbolTable, options.platform, reporter)

      if (willUsePacer) {
        var seenDomains: Set[Z] = Set.empty
        for (p <- symbolTable.getProcesses()) {
          p.getDomain() match {
            case Some(z) =>
              if (seenDomains.contains(z)) {
                val mesg = s"More than one process is in domain ${z}"
                reporter.warn(None(), CommonUtil.toolName, mesg)
              }
              seenDomains = seenDomains + z
            case _ =>
              if (p.getThreads().nonEmpty) {
                halt(s"This should be infeasible: process ${p.identifier} contains threads but does not not have a domain")
              }
          }
        }
      }

      if(symbolTable.hasVM() && !willUsePacer) {
        val msg = "Model contains VM components so it must use domain scheduling"
        reporter.error(None(), CommonUtil.toolName, msg)
      }



    {
      if (symbolTable.hasCakeMLComponents()) {

        for(cakemlThread <- symbolTable.getThreads().filter((a: AadlThread) => a.isCakeMLComponent())){
          if(cakemlThread.dispatchProtocol != Dispatch_Protocol.Periodic) {
            val mesg = s"CakeML components must be periodic: ${cakemlThread.identifier}"
            reporter.error(cakemlThread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }
        if (!symbolTable.rootSystem.getUseRawConnection()) {
          val mesg = "Raw connections (i.e. byte-arrays) must be used when integrating CakeML components."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }

        if(!willUsePacer) {
          val mesg = "Model contains CakeML components so it must use domain scheduling."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    { // if raw then all data components used in connectionInstances b/w threads
      // must have bit size specified and it must be greater than 0
      if(shouldUseRawConnections) {
        for(conn <- symbolTable.aadlConnections) {
          conn match {
            case apc: AadlPortConnection =>
              if (!TypeUtil.isEmptyType(apc.connectionDataType)) {
                val connName = apc.name
                val typeName = apc.connectionDataType.name
                val pos: Option[Position] = apc.connectionDataType.container match {
                  case Some(c) => c.identifier.pos
                  case _ => None()
                }
                apc.connectionDataType.bitSize match {
                  case Some(z) =>
                    if (z <= 0) {
                      val mesg = s"${HAMR__BIT_CODEC_MAX_SIZE} must be greater than 0 for data type ${typeName} used by connection ${connName}"
                      reporter.error(pos, CommonUtil.toolName, mesg)
                    }
                  case _ =>
                    apc.connectionDataType match {
                      case b: BaseType =>
                        val mesg = s"Unbounded type ${typeName} is not currently supported when using the wire protocol -- used by connection ${conn} "
                        reporter.error(pos, CommonUtil.toolName, mesg)
                      case _ =>
                        val mesg = s"${HAMR__BIT_CODEC_MAX_SIZE} must be specified for data type ${typeName} used by connection ${connName}"
                        reporter.error(pos, CommonUtil.toolName, mesg)
                    }
                }
              }
            case _ =>
          }
        }
      }
    }

    { // sanity checks TODO: move elsewhere

      if (!ExperimentalOptions.useCaseConnectors(options.experimentalOptions)) { // makes sure there are no fan outs from native components to vm components
        for (conns <- outConnections.entries) {
          var vmConns = F
          var nativeConns = F
          for (conn <- conns._2) {
            if (symbolTable.getThreadById(CommonUtil.getName(conn.dst.component)).toVirtualMachine(symbolTable)) {
              vmConns = T
            } else {
              nativeConns = T
            }
          }
          assert(!(vmConns && nativeConns),
            s"Fan out from ${conns._1} involves both native and VM components which is not currently supported")
        }
      }
    }

    return symbolTable
  }

  def getPortConnectionNames(c: ir.ConnectionInstance,
                             componentMap: HashSMap[String, ir.Component]): (String, String) = {
    val src = componentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = componentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret: (String, String) = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.feature.get), CommonUtil.getName(c.dst.feature.get))
      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        (CommonUtil.getName(c.src.component), CommonUtil.getName(c.dst.feature.get))

      case _ => halt(s"Unexpected connection: ${c}")
    }
    return ret
  }

  def isHandledConnection(c: ir.ConnectionInstance,
                          componentMap: HashSMap[String, ir.Component],
                          featureMap: HashSMap[String, ir.Feature]): B = {

    def validFeature(f: ir.Feature): B = {
      var ret: B = f match {
        case fend: ir.FeatureEnd =>
          fend.category match {
            case ir.FeatureCategory.DataAccess => T
            case ir.FeatureCategory.DataPort => T
            case ir.FeatureCategory.EventPort => T
            case ir.FeatureCategory.EventDataPort => T
            case ir.FeatureCategory.SubprogramAccessGroup => T

            case ir.FeatureCategory.AbstractFeature => F
            case ir.FeatureCategory.BusAccess => F
            case ir.FeatureCategory.FeatureGroup => F
            case ir.FeatureCategory.Parameter => F
            case ir.FeatureCategory.SubprogramAccess => F
          }
        case faccess: ir.FeatureAccess =>
          faccess.accessCategory match {
            case ir.AccessCategory.Data => T
            case ir.AccessCategory.SubprogramGroup => T
            case _ => F
          }
        case _ => F
      }
      return ret
    }

    val src = componentMap.get(CommonUtil.getName(c.src.component)).get
    val dst = componentMap.get(CommonUtil.getName(c.dst.component)).get

    val ret: B = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>

        val srcFeature = featureMap.get(CommonUtil.getName(c.src.feature.get)).get
        val dstFeature = featureMap.get(CommonUtil.getName(c.dst.feature.get)).get

        validFeature(srcFeature) && validFeature(dstFeature)

      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        val dstFeature = featureMap.get(CommonUtil.getName(c.dst.feature.get)).get
        validFeature(dstFeature)

      case _ =>
        F
    }

    return ret
  }


  def processAnnex(symbolTable: SymbolTable,
                   aadlTypes: AadlTypes,
                   annex: Annex,
                   reporter: Reporter): Option[AnnexInfo] = {
    val ret: Option[AnnexInfo] = annex.clause match {
      case b: BTSBLESSAnnexClause =>
        val ret: Option[AnnexInfo] =
          BTSResolver().processBTSAnnex(b, symbolTable, aadlTypes, reporter) match {
            case Some(bts) => Some(BTSAnnexInfo(bts.annex, bts))
            case _ => None()
        }
        return ret
      case _ => Some(TodoAnnexInfo(annex.clause))
    }
    return ret
  }

  def buildAadlMaps(aadl: ir.Aadl, reporter: Reporter): AadlMaps = {
    val w = Walker()
    for(c <- aadl.components) {
      w.transformComponent(c)
    }
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty
    for(d <- aadl.dataComponents) {
      airClassifierMap = airClassifierMap + (d.classifier.get.name ~> d)
    }
    var connInst2Conn: Map[ISZ[String], ISZ[ir.Connection]] = Map.empty
    for(ci <- w.connectionInstances) {
      var conns: ISZ[ir.Connection] = ISZ()
      for(cr <- ci.connectionRefs) {
        w.connectionsMap.get(cr.name.name) match {
          case Some(c) => conns = conns :+ c
          case _ =>
            // maybe it's from a feature group?

            val crops = ops.ISZOps(cr.name.name)
            val src: ISZ[String]= {
              val srcops = ops.ISZOps(ci.src.feature.get.name)
              srcops.slice(ci.src.component.name.size, srcops.s.size)
            }
            val dst: ISZ[String] = {
              val dstops = ops.ISZOps(ci.dst.feature.get.name)
              dstops.slice(ci.dst.component.name.size, dstops.s.size)
            }
            val connCand = st"${crops.last}-${(src, "_")}_${(dst, "_")}".render
            val candName = crops.slice(0, crops.s.size - 1) :+ connCand

            w.connectionsMap.get(candName) match {
              case Some(c2) => conns = conns :+ c2
              case _ =>
                /*
                val msg = s"Couldn't find connection reference ${cr.name.name} for connection instance ${ci.name.name}"
                reporter.warn(ci.name.pos, "", msg)
                */
            }
        }
      }
      connInst2Conn = connInst2Conn + (ci.name.name ~> conns)
    }
    val ret = AadlMaps(w.airComponentMap, w.airFeatureMap, airClassifierMap,
      w.connections, w.connectionsMap,
      w.connectionInstances, w.connectionInstancesMap, w.altConnectionInstancesMap,
      connInst2Conn)

    ret.validate()
    return ret;
  }

  @record class Walker() extends MAirTransformer {
    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty

    var connections: ISZ[ir.Connection] = ISZ()
    var connectionsMap: Map[ISZ[String], ir.Connection] = Map.empty

    var connectionInstances: ISZ[ir.ConnectionInstance] = ISZ()
    var connectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance] = Map.empty
    var altConnectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance] = Map.empty

    override def postComponent(o: ir.Component): MOption[ir.Component] = {
      val id = CommonUtil.getName(o.identifier)
      airComponentMap = airComponentMap + (id ~> o)
      return MNone[ir.Component]()
    }
    override def postFeature(o: ir.Feature): MOption[ir.Feature] = {
      val id = CommonUtil.getName(o.identifier)
      airFeatureMap = airFeatureMap + (id ~> o)
      return MNone[ir.Feature]()
    }
    override def postConnection(o: ir.Connection): MOption[ir.Connection] = {
      connections = connections :+ o
      connectionsMap = connectionsMap + (o.name.name ~> o)
      return MNone[ir.Connection]()
    }
    override def postConnectionInstance(o: ir.ConnectionInstance): MOption[ir.ConnectionInstance] = {
      connectionInstances = connectionInstances :+ o
      connectionInstancesMap = connectionInstancesMap + (o.name.name ~> o)

      var altName:ISZ[String] = ISZ()
      for(n <- o.name.name) {
        val contents = ops.StringOps(n)
        val pos = contents.stringIndexOf(" -> ")
        if(pos > 0) {
          val src = ops.StringOps(contents.substring(0, pos))
          val dst = ops.StringOps(contents.substring(pos + 4, contents.size))

          val srcs = ops.ISZOps(src.split((c: C) => c == '.'))
          val dsts = ops.ISZOps(dst.split((c: C) => c == '.'))

          altName = altName ++ (srcs.slice(0, srcs.s.size - 1))
          altName = altName :+ s"${srcs.last} -> ${dsts.first}"
          altName = altName ++ dsts.tail
        } else {
          altName = altName :+ n
        }
      }

      altConnectionInstancesMap = altConnectionInstancesMap + (altName ~> o)

      return MNone[ir.ConnectionInstance]()
    }
  }
}
