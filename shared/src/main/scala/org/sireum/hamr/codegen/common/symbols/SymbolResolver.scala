// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store, toolName}
import org.sireum.hamr.codegen.common.properties.HamrProperties.HAMR__BIT_CODEC_MAX_SIZE
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.resolvers.{BTSResolver, GclResolver}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BaseType, TypeUtil}
import org.sireum.hamr.codegen.common.util.ExperimentalOptions
import org.sireum.hamr.codegen.common.util.HamrCli.{CodegenHamrPlatform, CodegenOption}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.util.AadlUtil
import org.sireum.hamr.ir.{Annex, AnnexLib, ComponentCategory}
import org.sireum.message.{Position, Reporter}

object SymbolResolver {

  val defaultAnnexVisitors: MSZ[AnnexVisitor] = MSZ(GclResolver(), BTSResolver())

  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              aadlMaps: AadlMaps,
              options: CodegenOption,
              store: Store,
              reporter: Reporter): (Option[SymbolTable], Store) = {

    return SymbolResolver().resolve(model, aadlTypes, aadlMaps, options, defaultAnnexVisitors, store, reporter)
  }
}

@record class SymbolResolver() {


  def resolve(model: ir.Aadl,
              aadlTypes: AadlTypes,
              aadlMaps: AadlMaps,
              options: CodegenOption,
              annexVisitors: MSZ[AnnexVisitor],
              store: Store,
              reporter: Reporter): (Option[SymbolTable], Store) = {

    var localStore = store

    annexVisitors.foreach((f: AnnexVisitor) => f.reset)

    val stOpt = buildSymbolTable(model, aadlTypes, aadlMaps, options, reporter)
    if (reporter.hasError) {
      return (stOpt, localStore)
    } else {
      val st = stOpt.get

      val annexLibInfos: (ISZ[AnnexLibInfo], Store) = processAnnexLibraries(model.annexLib, st, aadlTypes, annexVisitors, localStore, reporter)
      localStore = annexLibInfos._2

      var annexClauseInfos: HashSMap[IdPath, ISZ[AnnexClauseInfo]] = HashSMap.empty
      for (component <- st.componentMap.entries) {
        var ais: ISZ[AnnexClauseInfo] = ISZ()
        for (annex <- component._2.component.annexes) {
          val p = processAnnexSubclauses(component._2, st, aadlTypes, annex, annexLibInfos._1, annexVisitors, localStore, reporter)
          ais = ais ++ p._1
          localStore = p._2
        }
        if (ais.nonEmpty) {
          annexClauseInfos = annexClauseInfos + (component._1 ~> ais)
        }
      }
      return (Some(st(annexClauseInfos = annexClauseInfos, annexLibInfos = annexLibInfos._1)), localStore)
    }
  }

  def buildSymbolTable(model: ir.Aadl,
                       aadlTypes: AadlTypes,
                       aadlMaps: AadlMaps,
                       options: CodegenOption,
                       reporter: Reporter): Option[SymbolTable] = {
    var featureMap: HashSMap[IdPath, AadlFeature] = HashSMap.empty

    var airComponentMap: HashSMap[IdPath, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[IdPath, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty

    var connectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

    // port-paths -> connInstances
    var inConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty
    var outConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty


    val components = model.components
    if (components.size != 1 || components(0).category != ir.ComponentCategory.System) {
      reporter.error(None(), CommonUtil.toolName, s"Model contains ${components.size} root components.  Expecting a single root component that must be a system")
      return None()
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
              //reporter.info(None(), CommonUtil.toolName, s"Ignoring ${src.category} to ${dst.category} connection: ${srcName} -> ${dstName}")
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
          (name, path,ISZ(c.classifier.get.name))
        case _ =>
          val classifierPath: IdPath =
            c.classifier match {
              case Some(s) => CommonUtil.splitClassifier(s)
              case _ =>
                if (CommonUtil.isSystem(c) && c != system) {
                  reporter.error(c.identifier.pos, toolName, s"Unexpected: only the root component can be missing a classifier, but it's missing for ${CommonUtil.getName(c.identifier)}")
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
          case Some(c2) => Some(aadlTypes.typeMap.get(c2.name).get)
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
            val dev = AadlDevice(
              component = c,
              parent = parent,
              path = path,
              identifier = identifier,
              dispatchProtocol = dispatchProtocol,
              period = period,
              features = aadlFeatures,
              subComponents = subComponents,
              connectionInstances = c.connectionInstances)

            checkSubcomponents(dev, subComponents)

            dev

          } else {
            assert(c.category == ir.ComponentCategory.Thread)

            val thr = AadlThread(
              component = c,
              parent = parent,
              path = path,
              identifier = identifier,
              dispatchProtocol = dispatchProtocol,
              period = period,
              features = aadlFeatures,
              subComponents = subComponents,
              connectionInstances = c.connectionInstances)

            checkSubcomponents(thr, subComponents)

            thr

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

        checkSubcomponents(ret, ISZ())

        return ret
      }

      val aadlComponent: AadlComponent = c.category match {
        case ir.ComponentCategory.System => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val sys = AadlSystem(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(sys, subComponents)

          sys
        }
        case ir.ComponentCategory.Processor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}: ${identifier}")
          val proc = AadlProcessor(
            component = c,
            parent = ISZ(),
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances)

          checkSubcomponents(proc, ISZ())

          proc
        }
        case ir.ComponentCategory.VirtualProcessor => {
          assert(c.subComponents.isEmpty, s"Need to handle subcomponents of ${c.category}? ${identifier}")

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

          val vproc = AadlVirtualProcessor(
            component = c,
            parent = ISZ(),
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = ISZ(),
            connectionInstances = c.connectionInstances,
            dispatchProtocol = dispatchProtocol,
            period = period
          )

          checkSubcomponents(vproc, ISZ())

          vproc
        }
        case ir.ComponentCategory.Process => {
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val proc = AadlProcess(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            subComponents = subComponents,
            features = aadlFeatures,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(proc, subComponents)

          proc
        }

        case ir.ComponentCategory.Device => handleDeviceOrThread()

        case ir.ComponentCategory.Thread => handleDeviceOrThread()

        case ir.ComponentCategory.ThreadGroup =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val tgroup = AadlThreadGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(tgroup, subComponents)

          tgroup

        case ir.ComponentCategory.Subprogram => handleSubprogram()

        case ir.ComponentCategory.SubprogramGroup =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val sub = AadlSubprogramGroup(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(sub, subComponents)

          sub

        case ir.ComponentCategory.Data =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val typ: Option[AadlType] = c.classifier match {
            case Some(c2) => aadlTypes.typeMap.get(c2.name)
            case _ => None()
          }

          val data = AadlData(
            component = c,
            parent = ISZ(),
            path = path,
            typ = typ.get,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(data, subComponents)

          data

        case ir.ComponentCategory.Bus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val bus = AadlBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(bus, subComponents)

          bus

        case ir.ComponentCategory.VirtualBus =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val vbus = AadlVirtualBus(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(vbus, subComponents)

          vbus

        case ir.ComponentCategory.Memory =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val mem = AadlMemory(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(mem, subComponents)

          mem

        case ir.ComponentCategory.Abstract =>
          val subComponents: ISZ[AadlComponent] = for (sc <- c.subComponents) yield buildAadlComponent(sc, path)

          val abs = AadlAbstract(
            component = c,
            parent = parent,
            path = path,
            identifier = identifier,
            features = aadlFeatures,
            subComponents = subComponents,
            connectionInstances = c.connectionInstances)

          checkSubcomponents(abs, subComponents)

          abs
      }

      aadlComponent match {
        case i:AadlData => componentMap = componentMap + classifierPath ~> aadlComponent
        case _ => componentMap = componentMap + (path ~> aadlComponent)
      }

      val instances: ISZ[AadlComponent] = if (classifierMap.contains(classifierPath)) classifierMap.get(classifierPath).get else ISZ()
      classifierMap = classifierMap + (classifierPath ~> (instances :+ aadlComponent))

      return aadlComponent
    }

    def checkSubcomponents(c: AadlComponent, subcomponents: ISZ[AadlComponent]): B = {

      var allValid = T
      for (sc <- subcomponents) {
        if (!AadlUtil.isValidSubcomponent(c.component.category, sc.component.category)) {
          reporter.error(c.component.identifier.pos, CommonUtil.toolName, s"A ${sc.component.category} cannot be a subcomponent of a ${c.component.category}")
          allValid = F
        }
      }
      return allValid
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

    val annexClauseInfos: HashSMap[IdPath, ISZ[AnnexClauseInfo]] = HashSMap.empty

    val annexLibInfos: ISZ[AnnexLibInfo] = ISZ()

    if (reporter.hasError) {
      return None()
    }

    return Some(SymbolTable(
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
      outConnections = outConnections))
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
                            store: Store,
                            reporter: Reporter): (ISZ[AnnexLibInfo], Store) = {
    var ret: ISZ[AnnexLibInfo] = ISZ()
    var localStore = store
    for (v <- annexVisitors) {
      val p = v.offerLibraries(libs, symbolTable, aadlTypes, localStore, reporter)
      ret = ret ++ p._1
      localStore = p._2
    }
    return (ret, localStore)
  }

  def processAnnexSubclauses(context: AadlComponent,
                             symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,
                             annex: Annex,
                             annexLibs: ISZ[AnnexLibInfo],
                             annexVisitors: MSZ[AnnexVisitor],
                             store: Store,
                             reporter: Reporter): (ISZ[AnnexClauseInfo], Store) = {
    var ret: ISZ[AnnexClauseInfo] = ISZ()
    var localStore = store
    for (v <- annexVisitors) {
      // TODO: what if 2+ visitors can handle the same annex?
      v.offer(context, annex, annexLibs, symbolTable, aadlTypes, localStore, reporter) match {
        case (Some(ai), s) =>
          ret = ret :+ ai
          localStore = s
        case (None(), s) =>
          localStore = s
      }
    }
    return (ret, localStore)
  }
}
