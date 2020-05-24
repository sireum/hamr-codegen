// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Component, RangeProp}
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._

object SymbolResolver {
  
  def resolve(model: ir.Aadl,
              basePackageName: Option[String],
              reporter: Reporter): SymbolTable = {
    
    var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
    var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty
    
    //var topLevelProcess: Option[ir.Component] = None[ir.Component]
    //var typeHeaderFileName: String = ""

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
        airFeatureMap = airFeatureMap + (CommonUtil.getName(f.identifier) ~> f)

        f match {
          case fe: ir.FeatureEnd =>
            if (CommonUtil.isDataPort(fe) && fe.classifier.isEmpty) {
              reporter.warn(None(), CommonUtil.toolName, s"Data type missing for feature ${fe.category} ${CommonUtil.getName(fe.identifier)}")
            }
          case _ =>
        }
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
      
      if(componentMap.contains(path)) {
        return componentMap.get(path).get
      }  
      
      val aadlComponent: AadlComponent = c.category match {
        case ir.ComponentCategory.System =>
          val subComponents: ISZ[AadlComponent] = for(sc <- c.subComponents) yield process(sc, Some(path))

          AadlSystem(component = c, parent = parent, path = path, identifier = identifier, subComponents = subComponents)

        case ir.ComponentCategory.Processor =>
          AadlProcessor(component = c, parent = None(), path = path, identifier = identifier, isVirtual = F)

        case ir.ComponentCategory.VirtualProcessor =>
          AadlProcessor(component = c, parent = None(), path = path, identifier = identifier, isVirtual = T)

        case ir.ComponentCategory.Process =>
          val subComponents: ISZ[AadlComponent] = for(sc <- c.subComponents) yield process(sc, Some(path))
          
          val processor: Option[String] = PropertyUtil.getActualProcessorBinding(c)
          
          AadlProcess(component = c, parent = parent, path = path, identifier = identifier, boundProcessor = processor, subComponents = subComponents)
          
        case ir.ComponentCategory.Thread =>
          assert(c.subComponents.isEmpty) // TODO handle subprograms etc
          var aadlPorts: ISZ[AadlFeature] = ISZ()
          for(feature <- c.features) {
            val fname = CommonUtil.getName(feature.identifier)
            val _feature = airFeatureMap.get(fname).get 
            val port: AadlFeature = _feature.category match {
              case ir.FeatureCategory.EventDataPort => AadlFeaturePort(_feature)
              case ir.FeatureCategory.EventPort => AadlFeaturePort(_feature)
              case ir.FeatureCategory.DataPort => AadlFeaturePort(_feature)
              case _ => AadlFeatureTODO(_feature) 
            }
            aadlPorts = aadlPorts :+ port
          }
          val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(c) match {
            case Some(x) => x
            case _ =>
              // TODO should be handled by a rewriter  
              reporter.warn(None(), CommonUtil.toolName, "Dispatch Protocol not specified, assuming Sporadic")
              Dispatch_Protocol.Sporadic
          }
          
          val period = PropertyUtil.getPeriod(c)
          
          AadlThread(component = c, parent = parent,
            path = path, identifier = identifier,
            dispatchProtocol = dispatchProtocol, period = period,
            ports = aadlPorts)
          
        case _ => AadlTODOComponent(component = c, parent = parent, path = path, identifier = identifier)
      }
      
      componentMap = componentMap + (path ~> aadlComponent)
      return aadlComponent
    }
    
    val aadlSystem = process(system, None()).asInstanceOf[AadlSystem]
    
    
    return SymbolTable(rootSystem = aadlSystem,

      componentMap = componentMap,

      airComponentMap = airComponentMap,
      airFeatureMap = airFeatureMap,
      airClassifierMap = airClassifierMap,

      connections = connections,
      inConnections = inConnections,
      outConnections = outConnections)
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

  }
