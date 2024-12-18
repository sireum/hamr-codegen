// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{ConnectionInstance, FeatureEnd}
import org.sireum.message.Position

@datatype class SymbolTable(rootSystem: AadlSystem,

                            // id path to aadl component
                            componentMap: HashSMap[IdPath, AadlComponent],

                            // fully qualified classifier name to instances of that classifier
                            classifierMap: HashSMap[IdPath, ISZ[AadlComponent]],

                            featureMap: HashSMap[IdPath, AadlFeature],

                            // all handled connections
                            aadlConnections: ISZ[AadlConnection],

                            annexClauseInfos: HashSMap[IdPath, ISZ[AnnexClauseInfo]],

                            annexLibInfos: ISZ[AnnexLibInfo],

                            airComponentMap: HashSMap[IdPath, ir.Component],
                            airFeatureMap: HashSMap[IdPath, ir.Feature],
                            airClassifierMap: HashSMap[String, ir.Component],

                            aadlMaps: AadlMaps,

                            connections: ISZ[ir.ConnectionInstance],

                            // feature name -> incoming connections
                            inConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]],

                            // feature name -> outgoing connections
                            outConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]]
                           ) {

  def isConnected(featureEnd: FeatureEnd): B = {
    val path = featureEnd.identifier.name
    return inConnections.contains(path) || outConnections.contains(path)
  }

  def getInConnections(featurePath: IdPath): ISZ[ir.ConnectionInstance] = {
    return if (inConnections.contains(featurePath)) inConnections.get(featurePath).get
    else ISZ()
  }

  def getOutConnections(featurePath: IdPath): ISZ[ir.ConnectionInstance] = {
    return if (outConnections.contains(featurePath)) outConnections.get(featurePath).get
    else ISZ()
  }

  def computeMaxDomain(): Z = {
    var max: Z = z"2" // threads start a domain 2
    for (p <- getThreads()) {
      p.getDomain(this) match {
        case Some(z) => if ((z + z"1") > max) {
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

  def getThreadById(id: IdPath): AadlThread = {
    return componentMap.get(id).get.asInstanceOf[AadlThread]
  }

  def getThreadByName(name: ir.Name): AadlThread = {
    return getThreadById(name.name)
  }

  def getThread(c: ir.Component): AadlThread = {
    return getThreadByName(c.identifier)
  }

  def getFeatureFromId(id: IdPath): ir.Feature = {
    return airFeatureMap.get(id).get
  }

  def getFeatureFromName(name: ir.Name): ir.Feature = {
    return getFeatureFromId(name.name)
  }

  def getPeriodicThreads(): ISZ[AadlThread] = {
    return ops.ISZOps(getThreads()).filter(p => CommonUtil.isPeriodic(p))
  }

  def getProcess(path: IdPath): AadlProcess = {
    return componentMap.get(path).get.asInstanceOf[AadlProcess]
  }

  def getProcesses(): ISZ[AadlProcess] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlProcess])
    return c.map(m => m.asInstanceOf[AadlProcess])
  }

  def getThreadOrDevices(): ISZ[AadlThreadOrDevice] = {
    var ret: ISZ[AadlThreadOrDevice] = ISZ()
    for (t <- getThreads()) {
      ret = ret :+ t
    }
    for (d <- getDevices()) {
      ret = ret :+ d
    }
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

  def getBoundProcesses(processor: AadlProcessor): ISZ[AadlProcess] = {
    var ret: Set[AadlProcess] = Set.empty
    for (c <- componentMap.values) {
      c match {
        case process: AadlProcess =>
          for (p <- getBoundProcessors(process) if p == processor) {
            ret = ret + process
          }
        case _ =>
      }
    }
    return ret.elements
  }

  def getActualBoundProcessors(c: AadlVirtualProcessor): ISZ[AadlProcessor] = {
    var ret: Set[AadlProcessor] = Set.empty
    for (p <- getBoundProcessors(c)) {
      p match {
        case ap: AadlProcessor => ret = ret + ap
        case apv: AadlVirtualProcessor =>
          // allow virtual processor chaining here, but symbol checking phase currently
          // rejects this, though maybe this will be allowed in the future
          ret = ret ++ getActualBoundProcessors(apv)
        case _ =>
      }
    }
    return ret.elements
  }

  /**
    --- A thread is bound to the processor specified by the <b>Actual_Processor_Binding</b> property.  The process of
    --- binding threads to processors determines the value of this property.  If there is more than one processor listed,
    --- a scheduler will dynamically assign the thread to one at a time.  This allows modeling of multi-core processors
    --- without explicit binding to one of the cores.<p>
    --- If a device is bound to a processor this indicates the binding of the device driver software.
    --- A virtual processor may be bound to a processor. This indicates that the virtual processor executes on the
    --- processor it is bound to.<p>
    --- Threads, devices, and virtual processors can be bound to virtual processors, which in turn are bound to virtual
    --- processors or processors.
  */
  def getBoundProcessors(c: AadlComponent): ISZ[Processor] = {
    val bindings = PropertyUtil.getActualProcessorBinding(c.component)
    if (bindings.nonEmpty) {
      // AADL let you bind components to systems, devices, and abstract in addition to
      // processor and virtual processors.  Not sure what the semantics are for the former
      // bindings so the linter only allows processors and virtual processors for now
      return (for(b <- bindings) yield componentMap.get(b).get.asInstanceOf[Processor])
    } else {
      if (c.parent.nonEmpty) {
        return getBoundProcessors(componentMap.get(c.parent).get)
      } else {
        return ISZ()
      }
    }
  }

  def getAllBoundProcessors(): ISZ[Processor] = {
    var processors: Set[Processor] = Set.empty
    for (c <- componentMap.values) {
      processors = processors ++ getBoundProcessors(c)
    }
    return processors.elements
  }

  def getAllActualBoundProcessors(): ISZ[AadlProcessor] = {
    var processors: Set[AadlProcessor] = Set.empty

    for (c <- componentMap.values;
         p <- getBoundProcessors(c)) {
      p match {
        case ap: AadlProcessor => processors = processors + ap
        case avp: AadlVirtualProcessor =>
          processors = processors ++ getActualBoundProcessors(avp)
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

  def getConnectionInstancePos(ci: ConnectionInstance): (ISZ[String], Option[Position]) = {
    var dir: ir.Direction.Type = ir.Direction.None

    for (ref <- ci.connectionRefs) {
      val component = airComponentMap.get(ref.context.name).get
      component.connections.filter(c => c.name == ref.name) match {
        case ISZ(ir.Connection(n, ISZ(ir.EndPoint(_, _, Some(srcDir))), ISZ(ir.EndPoint(_, _, Some(dstDir))), _, _, _, _, _)) =>
          (srcDir, dstDir) match {
            case (ir.Direction.In, ir.Direction.In) => dir = ir.Direction.In
            case (ir.Direction.Out, ir.Direction.Out) => dir = ir.Direction.Out
            case (ir.Direction.Out, ir.Direction.In) => return (n.name, n.pos)
            case _ => halt("Infeasible")
          }
        case _ =>
          halt("Infeasible")
      }
    }
    dir match {
      case ir.Direction.In => return (ci.connectionRefs(0).name.name, ci.connectionRefs(0).name.pos)
      case ir.Direction.Out => return (ci.connectionRefs(ci.connectionRefs.lastIndex).name.name, ci.connectionRefs(ci.connectionRefs.lastIndex).name.pos)
      case _ => halt("Infeasible")
    }
  }
}
