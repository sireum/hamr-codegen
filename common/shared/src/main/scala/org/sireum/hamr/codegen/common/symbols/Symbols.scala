// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.ir
import org.sireum.hamr.ir.FeatureEnd

@datatype class SymbolTable(rootSystem: AadlSystem,

                            componentMap: HashSMap[String, AadlComponent],

                            featureMap: HashSMap[String, AadlFeature],

                            // all handled connections
                            aadlConnections: ISZ[AadlConnection],

                            annexInfos: HashSMap[AadlComponent, ISZ[AnnexInfo]],

                            airComponentMap: HashSMap[String, ir.Component],
                            airFeatureMap: HashSMap[String, ir.Feature],
                            airClassifierMap: HashSMap[String, ir.Component],

                            connections: ISZ[ir.ConnectionInstance],

                            // feature name -> incoming connections
                            inConnections: HashSMap[String, ISZ[ir.ConnectionInstance]],

                            // feature name -> outgoing connections
                            outConnections: HashSMap[String, ISZ[ir.ConnectionInstance]]
                           ) {

  def isConnected(featureEnd: FeatureEnd): B = {
    val fid = CommonUtil.getName(featureEnd.identifier)
    return inConnections.contains(fid) || outConnections.contains(fid)
  }

  def getInConnections(featurePath: String): ISZ[ir.ConnectionInstance] = {
    return if(inConnections.contains(featurePath)) inConnections.get(featurePath).get
    else ISZ()
  }

  def computeMaxDomain(): Z = {
    var max: Z = z"2" // threads start a domain 2
    for(p <- getThreads()) {
      p.getDomain(this) match {
        case Some(z) => if((z + z"1") > max) {
          max = z + z"1"
        }
        case _ =>
      }
    }
    return max
  }

  def hasPeriodicThreads(): B = {
    return ops.ISZOps(getThreads()).exists(p => CommonUtil.isPeriodic(p))
  }

  def getThreadById(id: String): AadlThread = {
    return componentMap.get(id).get.asInstanceOf[AadlThread]
  }

  def getThreadByName(name: ir.Name): AadlThread = {
    return getThreadById(CommonUtil.getName(name))
  }

  def getThread(c: ir.Component): AadlThread = {
    return getThreadByName(c.identifier)
  }

  def getFeatureFromId(id: String): ir.Feature = {
    return airFeatureMap.get(id).get
  }

  def getFeatureFromName(name: ir.Name): ir.Feature = {
    return getFeatureFromId(CommonUtil.getName(name))
  }

  def getPeriodicThreads(): ISZ[AadlThread] = {
    return ops.ISZOps(getThreads()).filter(p => CommonUtil.isPeriodic(p))
  }

  def getProcess(id: String): AadlProcess = {
    return componentMap.get(id).get.asInstanceOf[AadlProcess]
  }

  def getProcesses(): ISZ[AadlProcess] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlProcess])
    return c.map(m => m.asInstanceOf[AadlProcess])
  }

  def getThreadOrDevices(): ISZ[AadlThreadOrDevice] = {
    var ret:ISZ[AadlThreadOrDevice] = ISZ()
    for(t <- getThreads()) { ret = ret :+ t }
    for(d <- getDevices()) { ret = ret :+ d }
    return ret
  }

  def getDevices(): ISZ[AadlDevice] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlDevice])
    return c.map(m => m.asInstanceOf[AadlDevice])
  }

  def getThreads(): ISZ[AadlThread] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlThread])
    return c.map(m => m.asInstanceOf[AadlThread])
  }

  def getBoundProcesses(c: AadlProcessor): ISZ[AadlProcess] = {
    val ret: ISZ[AadlComponent] = componentMap.values.filter((p: AadlComponent) => p match {
      case process: AadlProcess =>
        getBoundProcessor(process) match {
          case Some(processor) => processor == c
          case _ => F
        }
      case _ => F
    })
    return ret.map(m => m.asInstanceOf[AadlProcess])
  }

  def getActualBoundProcess(c: AadlVirtualProcessor): Option[AadlProcessor] = {
    val ret: Option[AadlProcessor] = c.boundProcessor match {
      case Some(path) =>
        componentMap.get(path).get match {
          case ap: AadlProcessor => Some(ap)
          case apv: AadlVirtualProcessor =>
            // allow virtual processor chaining here, but symbol checking phase currently
            // rejects this, though maybe this will be allowed in the future
            return getActualBoundProcess(apv)
          case _ => None()
        }
      case _ => None()
    }
    return ret
  }

  def getBoundProcessor(c: AadlProcess): Option[AadlProcessor] = {
    val ret: Option[AadlProcessor] = c.boundProcessor match {
      case Some(path) =>
        componentMap.get(path) match {
          case Some(a: AadlProcessor) => Some(a)
          case Some(v: AadlVirtualProcessor) => getActualBoundProcess(v)
          case Some(x) => halt(s"Unexpected, ${c.identifier} is a process but is bound to ${x} rather than a processor")
          case _ => None()
        }
      case _ => None()
    }
    return ret
  }

  def getAllBoundProcessors(): ISZ[AadlProcessor] = {
    var processors: Set[AadlProcessor] = Set.empty

    for(process <- getProcesses()){
      getBoundProcessor(process) match {
        case Some(aadlProcessor) => processors = processors + aadlProcessor
        case _ =>
          // symbol checking phase should mean this is infeasible
          halt(s"Unexpected: ${process.path} does not have a bound processor")
      }
    }
    return processors.elements
  }

  def hasVM(): B = {
    return ops.ISZOps(getProcesses()).exists(p => p.toVirtualMachine(this))
  }

  def hasCakeMLComponents(): B = {
    return ops.ISZOps(getThreads()).exists(t => t.isCakeMLComponent())
  }
}
