// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, toolName}
import org.sireum.hamr.codegen.common.properties.HamrProperties.HAMR__BIT_CODEC_MAX_SIZE
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.resolvers.{BTSResolver, GclResolver}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BaseType, TypeUtil}
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, CodeGenPlatform, ExperimentalOptions}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Annex, AnnexLib, ComponentCategory}
import org.sireum.message.{Position, Reporter}

object SymbolResolver {

  val defaultAnnexVisitors: MSZ[AnnexVisitor] = MSZ(GclResolver(), BTSResolver())

  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              aadlMaps: AadlMaps,
              options: CodeGenConfig,
              reporter: Reporter): SymbolTable = {

    return SymbolResolver().resolve(model, aadlTypes, aadlMaps, options, defaultAnnexVisitors, reporter)
  }
}

@record class SymbolResolver() {


  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              aadlMaps: AadlMaps,
              options: CodeGenConfig,
              annexVisitors: MSZ[AnnexVisitor],
              reporter: Reporter): SymbolTable = {

    annexVisitors.foreach((f: AnnexVisitor) => f.reset)

    val st = buildSymbolTable(model, aadlTypes, aadlMaps, options, reporter)
    if (reporter.hasError) {
      return st
    } else {
      val annexLibInfos: ISZ[AnnexLibInfo] = processAnnexLibraries(model.annexLib, st, aadlTypes, annexVisitors, reporter)

      var annexClauseInfos: HashSMap[AadlComponent, ISZ[AnnexClauseInfo]] = HashSMap.empty
      for (component <- st.componentMap.values) {
        var ais: ISZ[AnnexClauseInfo] = ISZ()
        for (annex <- component.component.annexes) {
          ais = ais ++ processAnnexSubclauses(component, st, aadlTypes, annex, annexLibInfos, annexVisitors, reporter)
        }
        annexClauseInfos = annexClauseInfos + (component ~> ais)
      }
      return st(annexClauseInfos = annexClauseInfos, annexLibInfos = annexLibInfos)
    }
  }

  def buildSymbolTable(model: ir.Aadl,
                       aadlTypes: AadlTypes,
                       aadlMaps: AadlMaps,
                       options: CodeGenConfig,
                       reporter: Reporter): SymbolTable = {
    var featureMap: HashSMap[IdPath, AadlFeature] = HashSMap.empty

    var airComponentMap: HashSMap[IdPath, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[IdPath, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty

    var connectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

    // port-paths -> connInstances
    var inConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty
    var outConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty


    val components = model.components
    if (components.size != z"1" || components(0).category != ir.ComponentCategory.System) {
      halt(s"Model contains ${components.size} components.  Should only contain a single top-level system")
    }
    val system = components(0)

    { // build airComponent and airClassifier maps
      def buildAirMaps(c: ir.Component): Unit = {
        val path = c.identifier.name
        assert(!airComponentMap.contains(path))
        airComponentMap = airComponentMap + (path ~> c)
        if (c.classifier.nonEmpty) {
          airClassifierMap = airClassifierMap + (c.classifier.get.name ~> c)
        }

        for (f <- c.features) {

          def resolveAirFeature(_f: ir.Feature): Unit = {
            _f match {
              case fa: ir.FeatureAccess =>
                val featurePath: IdPath = fa.identifier.name
                airFeatureMap = airFeatureMap + (featurePath ~> fa)
              case fe: ir.FeatureEnd =>
                val featurePath: IdPath = fe.identifier.name
                airFeatureMap = airFeatureMap + (featurePath ~> fe)

                if (CommonUtil.isDataPort(fe) && fe.classifier.isEmpty) {
                  reporter.warn(None(), CommonUtil.toolName, s"Data type missing for feature ${fe.category} ${CommonUtil.getName(fe.identifier)}")
                }
              case fg: ir.FeatureGroup =>
                for (_fge <- fg.features) {
                  resolveAirFeature(_fge)
                }
            }
          }

          resolveAirFeature(f)
        }
        for (sc <- c.subComponents) {
          buildAirMaps(sc)
        }
      }

      buildAirMaps(system)
    }

    {
      def populateInOutConnectionMaps(sys: ir.Component): B = {

        for (c <- airComponentMap.values) {

          for (ci <- c.connectionInstances) {
            if (isHandledConnection(ci, airComponentMap, airFeatureMap)) {
              connectionInstances = connectionInstances :+ ci

              def add(portPath: IdPath, isIn: B): Unit = {
                val map: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = isIn match {
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
              val src = airComponentMap.get(ci.src.component.name).get
              val dst = airComponentMap.get(ci.dst.component.name).get

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

      populateInOutConnectionMaps(system)
    }

    var componentMap: HashSMap[IdPath, AadlComponent] = HashSMap.empty
    var classifierMap: HashSMap[IdPath, ISZ[AadlComponent]] = HashSMap.empty

    /** Builds an AadlComponents and AadlFeatures from the passed in AIR component
    */
    def buildAadlComponent(c: ir.Component, parent: IdPath): AadlComponent = {
      val (identifier, path, classifierPath): (String, IdPath, IdPath) = c.category match {
        case ComponentCategory.Data =>
          val name: String = if (c.identifier.name.isEmpty) {
            c.classifier.get.name
          } else {
            CommonUtil.getName(c.identifier)
          }
          val path: IdPath = if (parent.isEmpty) ISZ(name)
          else parent :+ name
          (name, path, CommonUtil.splitClassifier(c.classifier.get))
        case _ =>
          val classifierPath: IdPath =
            c.classifier match {
              case Some(s) => CommonUtil.splitClassifier(s)
              case _ =>
                if (!CommonUtil.isSystemInstance(c)) {
                  reporter.error(c.identifier.pos, toolName, s"Unexpected: only the system instance should be missing a classifier, but it's missing for ${CommonUtil.getName(c.identifier)}")
                }
                ISZ()
            }

          (CommonUtil.getLastName(c.identifier), c.identifier.name, classifierPath)
      }

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

      def buildAadlFeature(feature: ir.Feature, featureGroupIds: ISZ[String], parentIsThread: B): ISZ[AadlFeature] = {
        feature match {
          case fg: ir.FeatureGroup =>
            var ret: ISZ[AadlFeature] = ISZ()
            for (_f <- fg.features) {
              ret = ret ++ buildAadlFeature(_f, featureGroupIds :+ CommonUtil.getLastName(fg.identifier), parentIsThread)
            }
            return ret
          case _ =>
            val featurePath = feature.identifier.name
            val airFeature = airFeatureMap.get(featurePath).get
            assert(airFeature == feature)

            feature match {
              case fe: ir.FeatureEnd if fe.direction == ir.Direction.InOut => {
                if (fe.category == ir.FeatureCategory.FeatureGroup) {
                  // oddly, an empty feature group is treated as a feature end by osate.  Can be
                  // ignored as it doesn't add any features to the component
                }
                else if (parentIsThread) {
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
              case ir.FeatureCategory.Parameter => {
                val fend = feature.asInstanceOf[ir.FeatureEnd]
                AadlParameter(
                  feature = fend,
                  featureGroupIds = ISZ(),
                  aadlType = getFeatureEndType(fend).get,
                  direction = fend.direction)
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

      var aadlFeatures: ISZ[AadlFeature] = ISZ()

      for (feature <- c.features) {
        aadlFeatures = aadlFeatures ++ buildAadlFeature(feature, ISZ(), c.category == ir.ComponentCategory.Thread)
      }

      def handleDeviceOrThread(): AadlComponent = {
        {
          //assert(c.subComponents.isEmpty) // TODO handle subprograms etc

          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(x) => x
            case _ =>
              val (protocol, mesg): (Dispatch_Protocol.Type, String) = c.category match {
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

        val ret: AadlSubprogram = AadlSubprogram(
          component = c,
          parent = parent,
          path = path,
          identifier = identifier,
          subComponents = ISZ(),
          features = aadlFeatures,
          connectionInstances = ISZ())

        return ret
      }

      val aadlComponent: AadlComponent = c.category match {
        case ir.ComponentCategory.System => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlSystem(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)
        }
        case ir.ComponentCategory.Processor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}: ${identifier}")
          AadlProcessor(
            component = c,
            parent = ISZ(),
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances)
        }
        case ir.ComponentCategory.VirtualProcessor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}? ${identifier}")

          val boundProcessor: Option[IdPath] = PropertyUtil.getActualProcessorBinding(c)

          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(Dispatch_Protocol.Periodic) => Dispatch_Protocol.Periodic
            case Some(x) =>
              val mesg = s"Dispatch Protocol for virtual processor ${identifier} must be Periodic instead of ${x}"
              reporter.error(c.identifier.pos, CommonUtil.toolName, mesg)
              x
            case _ =>
              Dispatch_Protocol.Periodic
          }

          val period = PropertyUtil.getPeriod(c)

          AadlVirtualProcessor(
            component = c,
            parent = ISZ(),
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances,
            dispatchProtocol = dispatchProtocol,
            period = period,
            boundProcessor = boundProcessor
          )
        }
        case ir.ComponentCategory.Process => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val boundProcessor: Option[IdPath] = PropertyUtil.getActualProcessorBinding(c)

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
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlThreadGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Subprogram => handleSubprogram()

        case ir.ComponentCategory.SubprogramGroup =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlSubprogramGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Data =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val typ: Option[AadlType] = c.classifier match {
            case Some(c) => aadlTypes.typeMap.get(c.name)
            case _ => None()
          }

          AadlData(
            component = c,
            parent = parent,
            path = path,
            typ = typ.get,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Bus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.VirtualBus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlVirtualBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Memory =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlMemory(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

        case ir.ComponentCategory.Abstract =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          AadlAbstract(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)
      }

      componentMap = componentMap + (path ~> aadlComponent)

      val instances: ISZ[AadlComponent] = if (classifierMap.contains(classifierPath)) classifierMap.get(classifierPath).get else ISZ()
      classifierMap = classifierMap + (classifierPath ~> (instances :+ aadlComponent))

      return aadlComponent
    }

    val aadlSystem = buildAadlComponent(system, ISZ()).asInstanceOf[AadlSystem]

    // add data components to componentMap
    for (c <- model.dataComponents) {
      buildAadlComponent(c, ISZ())
    }

    var aadlConnections: ISZ[AadlConnection] = ISZ()
    val shouldUseRawConnections: B = PropertyUtil.getUseRawConnection(system.properties)

    def resolveAadlConnectionInstances(): Unit = {
      for (ci <- connectionInstances) {
        val aadlConnection: AadlConnection = ci.kind match {
          case ir.ConnectionKind.Port => {
            val name = CommonUtil.getName(ci.name)

            val srcFeatureName: IdPath = ci.src.feature.get.name
            val srcComponent = componentMap.get(ci.src.component.name).get
            val srcFeature = featureMap.get(srcFeatureName).get

            val dstFeatureName: IdPath = ci.dst.feature.get.name
            val dstComponent = componentMap.get(ci.dst.component.name).get
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

    val annexClauseInfos: HashSMap[AadlComponent, ISZ[AnnexClauseInfo]] = HashSMap.empty

    val annexLibInfos: ISZ[AnnexLibInfo] = ISZ()

    val symbolTable = SymbolTable(

      rootSystem = aadlSystem,
      componentMap = componentMap,
      classifierMap = classifierMap,
      featureMap = featureMap,
      aadlConnections = aadlConnections,

      annexClauseInfos = annexClauseInfos,
      annexLibInfos = annexLibInfos,

      airComponentMap = airComponentMap,
      airFeatureMap = airFeatureMap,
      airClassifierMap = airClassifierMap,

      aadlMaps = aadlMaps,

      connections = connectionInstances,
      inConnections = inConnections,
      outConnections = outConnections)

    {
      for (thread <- symbolTable.getThreads()) {
        if (thread.toVirtualMachine(symbolTable)) {
          val parent = thread.getParent(symbolTable)
          parent.getBoundProcessor(symbolTable) match {
            case Some(avp: AadlVirtualProcessor) =>
              if (avp.dispatchProtocol != Dispatch_Protocol.Periodic) {
                val mesg = s"Virtual processor ${avp.identifier} has ${avp.dispatchProtocol.name} dispatching. Only periodic dispatching is supported."
                reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
              }
            case x =>
              val mesg = s"Thread ${thread.identifier} is going to a VM so its process ${parent.identifier} must be bound to a virtual processor. Instead it's bound to ${x}"
              reporter.error(thread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        } else {
          if (thread.dispatchProtocol == Dispatch_Protocol.Periodic && thread.period.isEmpty) {
            val mesg = s"Must specify ${OsateProperties.TIMING_PROPERTIES__PERIOD} for periodic thread ${thread.identifier}"
            reporter.error(thread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }
      }
    }

    { // restrict when wire protocol and CakeML components are allowed
      if (symbolTable.hasCakeMLComponents() || shouldUseRawConnections) {
        if (options.platform == CodeGenPlatform.SeL4_Only || options.platform == CodeGenPlatform.SeL4_TB) {
          var reasons: ISZ[String] = ISZ()
          if (symbolTable.hasCakeMLComponents()) {
            reasons = reasons :+ "CakeML components"
          }
          if (shouldUseRawConnections) {
            reasons = reasons :+ "wire protocol"
          }
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
        for (process <- symbolTable.getProcesses().filter(p => p.toVirtualMachine(symbolTable))) {
          assert(process.boundProcessor.nonEmpty, s"Unexpected: ${process.identifier} is going to a vm but it isn't bound to a processor?")
          assert(symbolTable.componentMap.contains(process.boundProcessor.get), s"Unexpected: unable to resolve ${process.identifier}'s bound processor ${process.boundProcessor.get}")

          symbolTable.getBoundProcessor(process) match {
            case Some(avp: AadlVirtualProcessor) =>
              if (seenVirtualProcessor.contains(avp)) {
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

        for (p <- validProcessors) {
          p.getPacingMethod() match {
            case Some(CaseSchedulingProperties.PacingMethod.SelfPacing) =>
              val mesg = s"Model has virtual machines so it must use the pacer component style of pacing"
              reporter.error(p.component.identifier.pos, CommonUtil.toolName, mesg)
            case _ =>
          }
        }
      }
    }

    val willUseDomainScheduling: B = validProcessorBindings && PacerUtil.canUseDomainScheduling(symbolTable, options.platform, reporter)

    if (symbolTable.hasVM() && !willUseDomainScheduling) {
      val msg = "Model contains VM components so it must use domain scheduling"
      reporter.error(None(), CommonUtil.toolName, msg)
    }


    {
      if (symbolTable.hasCakeMLComponents()) {

        for (cakemlThread <- symbolTable.getThreads().filter((a: AadlThread) => a.isCakeMLComponent())) {
          if (cakemlThread.dispatchProtocol != Dispatch_Protocol.Periodic) {
            val mesg = s"CakeML components must be periodic: ${cakemlThread.identifier}"
            reporter.error(cakemlThread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }
        if (!symbolTable.rootSystem.getUseRawConnection()) {
          val mesg = "Raw connections (i.e. byte-arrays) must be used when integrating CakeML components."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }

        if (!willUseDomainScheduling) {
          val mesg = "Model contains CakeML components so it must use domain scheduling."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    { // if raw then all data components used in connectionInstances b/w threads
      // must have bit size specified and it must be greater than 0
      if (shouldUseRawConnections) {
        for (conn <- symbolTable.aadlConnections) {
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
            if (symbolTable.getThreadById(conn.dst.component.name).toVirtualMachine(symbolTable)) {
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
                             componentMap: HashSMap[IdPath, ir.Component]): (IdPath, IdPath) = {
    val src = componentMap.get(c.src.component.name).get
    val dst = componentMap.get(c.dst.component.name).get

    val ret: (IdPath, IdPath) = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>
        (c.src.feature.get.name, c.dst.feature.get.name)
      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        (c.src.component.name, c.dst.feature.get.name)

      case _ => halt(s"Unexpected connection: ${c}")
    }
    return ret
  }

  def isHandledConnection(c: ir.ConnectionInstance,
                          componentMap: HashSMap[IdPath, ir.Component],
                          featureMap: HashSMap[IdPath, ir.Feature]): B = {

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

    val src = componentMap.get(c.src.component.name).get
    val dst = componentMap.get(c.dst.component.name).get

    val ret: B = (src.category, dst.category) match {
      case (ir.ComponentCategory.Thread, ir.ComponentCategory.Thread) =>

        val srcFeature = featureMap.get(c.src.feature.get.name).get
        val dstFeature = featureMap.get(c.dst.feature.get.name).get

        validFeature(srcFeature) && validFeature(dstFeature)

      case (ir.ComponentCategory.Data, ir.ComponentCategory.Thread) =>
        val dstFeature = featureMap.get(c.dst.feature.get.name).get
        validFeature(dstFeature)

      case _ =>
        F
    }

    return ret
  }

  def processAnnexLibraries(libs: ISZ[AnnexLib],
                            symbolTable: SymbolTable,
                            aadlTypes: AadlTypes,
                            annexVisitors: MSZ[AnnexVisitor],
                            reporter: Reporter): ISZ[AnnexLibInfo] = {
    var ret: ISZ[AnnexLibInfo] = ISZ()
    for (v <- annexVisitors) {
      ret = ret ++ v.offerLibraries(libs, symbolTable, aadlTypes, reporter)
    }
    return ret
  }

  def processAnnexSubclauses(context: AadlComponent,
                             symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,
                             annex: Annex,
                             annexLibs: ISZ[AnnexLibInfo],
                             annexVisitors: MSZ[AnnexVisitor],
                             reporter: Reporter): ISZ[AnnexClauseInfo] = {
    var ret: ISZ[AnnexClauseInfo] = ISZ()
    for (v <- annexVisitors) {
      // TODO: what if 2+ visitors can handle the same annex?
      v.offer(context, annex, annexLibs, symbolTable, aadlTypes, reporter) match {
        case Some(ai) => ret = ret :+ ai
        case None() =>
      }
    }
    return ret
  }
}
