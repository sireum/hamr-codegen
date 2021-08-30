// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.HamrProperties.HAMR__BIT_CODEC_MAX_SIZE
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, PropertyUtil}
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

                           connections: ISZ[ir.ConnectionInstance])

  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              options: CodeGenConfig,
              reporter: Reporter): SymbolTable = {
    val st = buildSymbolTable(model, aadlTypes, options, reporter)
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
                       options: CodeGenConfig,
                       reporter: Reporter): SymbolTable = {
    var featureMap: HashSMap[String, AadlFeature] = HashSMap.empty

    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty

    var connections: ISZ[ir.ConnectionInstance] = ISZ()

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
            connections = connections :+ ci

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

      def handleDeviceOrThread(): AadlComponent = {
        {
          //assert(c.subComponents.isEmpty) // TODO handle subprograms etc

          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))
          var aadlPorts: ISZ[AadlFeature] = ISZ()

          def resolveFeature2(feature: ir.Feature, featureGroupIds: ISZ[String]): Unit = {
            feature match {
              case fg: ir.FeatureGroup =>
                for(_f <- fg.features) {
                  resolveFeature2(_f, featureGroupIds :+ CommonUtil.getLastName(fg.identifier))
                }
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
                    else {
                      val id = CommonUtil.getLastName(fe.identifier)
                      val mesg = s"Invalid direction: ${fe.direction} for ${id}.  Only uni-directional ports are supported"
                      reporter.error(fe.identifier.pos, CommonUtil.toolName, mesg)
                    }
                  }
                  case _ =>
                }

                val port: AadlFeature = feature.category match {
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
                    AadlDataPort(
                      feature = fend,
                      featureGroupIds = featureGroupIds,
                      direction = fend.direction,
                      aadlType = getFeatureEndType(fend).get)
                  }
                  case _ => AadlFeatureTODO(
                    feature = feature,
                    featureGroupIds = featureGroupIds)
                }
                aadlPorts = aadlPorts :+ port
                featureMap = featureMap + (featurePath ~> port)
            }
          }
          for (feature <- c.features) {
            resolveFeature2(feature, ISZ())
          }

          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(x) => x
            case _ =>
              // TODO should be handled by a rewriter -- or better yet, just reject the model
              val mesg = s"Dispatch Protocol not specified for ${identifier}, assuming Sporadic"
              reporter.warn(c.identifier.pos, CommonUtil.toolName, mesg)
              Dispatch_Protocol.Sporadic
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
              ports = aadlPorts,
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
              ports = aadlPorts,
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

          AadlVirtualProcessor(
            component = c,
            parent = None(),
            path = path,
            identifier = identifier,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances,
            boundProcessor = boundProcessor
          )
        }
        case ir.ComponentCategory.Process => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          val boundProcessor: Option[String] = PropertyUtil.getActualProcessorBinding(c)

          AadlProcess(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            boundProcessor = boundProcessor,
            subComponents = subComponents,
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

        case _ => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield process(sc, Some(path))

          AadlTODOComponent(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)
        }
      }

      componentMap = componentMap + (path ~> aadlComponent)
      return aadlComponent
    }

    val aadlSystem = process(system, None()).asInstanceOf[AadlSystem]

    var aadlConnections: ISZ[AadlConnection] = ISZ()
    val shouldUseRawConnections: B = PropertyUtil.getUseRawConnection(system.properties)

    def resolveAadlConnectionInstances(): Unit = {
      for (ci <- connections) {
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
                      //       being processed.  Need to test with connections involving type extensions
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

      connections = connections,
      inConnections = inConnections,
      outConnections = outConnections)


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

    {
      if (symbolTable.hasVM()) {
        var validProcessors: ISZ[AadlProcessor] = ISZ()
        for(process <- symbolTable.getProcesses().filter(p => p.toVirtualMachine(symbolTable))) {
          assert(process.boundProcessor.nonEmpty, s"Unexpected: ${process.identifier} is going to a vm but it isn't bound to a processor?")
          assert(symbolTable.componentMap.contains(process.boundProcessor.get), s"Unexpected: unable to resolve ${process.identifier}'s bound processor ${process.boundProcessor.get}")

          symbolTable.componentMap.get(process.boundProcessor.get).get match {
            case avp: AadlVirtualProcessor =>
              avp.boundProcessor match {
                case Some(_parent) =>
                  symbolTable.componentMap.get(_parent) match {
                    case Some(avp2: AadlVirtualProcessor) =>
                      val mesg = s"Chained virtual processors is not supported.  Bind virtual processor ${avp.identifier} to an actual processor"
                      reporter.error(avp2.component.identifier.pos, CommonUtil.toolName, mesg)
                    case Some(ap2: AadlProcessor) =>

                      PropertyUtil.getDiscreetPropertyValue(avp.component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__OS) match {
                        case Some(ir.ValueProp(os)) if os != "Linux" =>
                          val mesg = s"Invalid OS ${os} for virtual processor ${avp.identifier}.  HAMR only supports Linux based virtual machines"
                          reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                        case _ =>
                          val mesg = s"${CasePropertiesProperties.PROP__CASE_PROPERTIES__OS} not provided for virtual processor ${avp.identifier}.  Assuming Linux"
                          reporter.warn(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                      }

                      // ok, virtual processor is bound to an actual processor
                      validProcessors = validProcessors :+ ap2


                    case _ =>
                      val mesg = s"Unexpected: couldn't resolve the bound processor ${_parent} for virtual processor ${avp.identifier}. Please report"
                      reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                  }
                case _ =>
                  val mesg = s"Virtual processor ${avp.identifier} must be bound to an actual processor since process ${process.identifier} is bound to it"
                  reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
              }
            case x: AadlProcessor => validProcessors = validProcessors :+ x // ok, process is bound directly to an actual processor
            case x =>
              val mesg = s"Unexpected: process ${process.identifier} is bound to ${x} rather than a processor"
              reporter.error(process.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }

        // don't use symbolTable.getAllBoundProcessors() here as that expects we always reach an AadlProcessor,
        // but that might not be case due to issues from the above block
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

        if(!PacerUtil.canUseDomainScheduling(symbolTable, options.platform, reporter)) {
          val mesg = "Model contains CakeML components so it must use domain scheduling."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    { // if raw then all data components used in connections b/w threads
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

  def buildAadlMaps(aadl: ir.Aadl): AadlMaps = {
    val w = Walker()
    for(c <- aadl.components) {
      w.transformComponent(c)
    }
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty
    for(d <- aadl.dataComponents) {
      airClassifierMap = airClassifierMap + (d.classifier.get.name ~> d)
    }
    return AadlMaps(w.airComponentMap, w.airFeatureMap, airClassifierMap, w.connections)
  }

  @record class Walker() extends MAirTransformer {
    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty
    var connections: ISZ[ir.ConnectionInstance] = ISZ()

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
    override def postConnectionInstance(o: ir.ConnectionInstance): MOption[ir.ConnectionInstance] = {
      connections = connections :+ o
      return MNone[ir.ConnectionInstance]()
    }
  }
}
