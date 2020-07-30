// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.FeatureEnd

@datatype class SymbolTable(rootSystem: AadlSystem,

                            componentMap: HashSMap[String, AadlComponent],

                            airComponentMap: HashSMap[String, ir.Component],
                            airFeatureMap: HashSMap[String, ir.Feature],
                            airClassifierMap: HashSMap[String, ir.Component],

                            // all handled connections
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

  def getMaxDomain(): Z = {
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
    return ops.ISZOps(getThreads()).exists(p => CommonUtil.isPeriodic(p.component))
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
    return ops.ISZOps(getThreads()).filter(p => CommonUtil.isPeriodic(p.component))
  }

  def getProcess(id: String): AadlProcess = { return componentMap.get(id).get.asInstanceOf[AadlProcess] }

  def getProcesses(): ISZ[AadlProcess] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlProcess])
    return c.map(m => m.asInstanceOf[AadlProcess])
  }


  def getThreads(): ISZ[AadlThread] = {
    val c = componentMap.values.filter(f => f.isInstanceOf[AadlThread])
    return c.map(m => m.asInstanceOf[AadlThread])
  }

  def getBoundProcesses(c: AadlProcessor): ISZ[AadlProcess] = {
    val ret: ISZ[AadlComponent] = componentMap.values.filter((p: AadlComponent) => p match {
      case process: AadlProcess =>
        process.boundProcessor match {
          case Some(processor) => processor == c.path
          case _ => F
        }
      case _ => F
    })
    return ret.map(m => m.asInstanceOf[AadlProcess])
  }

  def getBoundProcessor(c: AadlProcess): Option[AadlProcessor] = {
    val ret: Option[AadlProcessor] = c.boundProcessor match {
      case Some(p) => Some(componentMap.get(p).get.asInstanceOf[AadlProcessor])
      case None() => None()
    }
    return ret
  }

  def getAllBoundProcessors(): ISZ[AadlProcessor] = {
    var processors: Set[AadlProcessor] = Set.empty

    for(process <- getProcesses()){
      getBoundProcessor(process) match {
        case Some(processor) =>
          if(!processor.isVirtual) {
            processors = processors + processor
          }
        case _ => halt(s"Unexpected: ${process.path} does not have a bound processor")
      }
    }
    return processors.elements
  }

  def hasVM(): B = {
    return ops.ISZOps(getProcesses()).exists(p => p.toVirtualMachine())
  }

  def hasCakeMLComponents(): B = {
    return ops.ISZOps(getThreads()).exists(t => t.isCakeMLComponent())
  }
}


@sig trait AadlObject

@sig trait AadlComponent extends AadlObject {
  def component: ir.Component
  def parent: Option[String]

  def path: String
  def identifier: String
}

@datatype class AadlSystem(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,

                           subComponents: ISZ[AadlComponent]) extends AadlComponent {

  def rawConnections(): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(component.properties, HamrProperties.HAMR__BIT_CODEC_RAW_CONNECTIONS) match {
      case Some(ir.ValueProp("true")) => T
      case Some(ir.ValueProp("false")) => F
      case _ => F
    }
    return ret
  }
}

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: Option[String],
                              val path: String,
                              val identifier: String,

                              val isVirtual: B) extends AadlComponent {
  def getFramePeriod(): Option[Z] = {
    val ret: Option[Z] = PropertyUtil.getDiscreetPropertyValue(component.properties, OsateProperties.TIMING_PROPERTIES__FRAME_PERIOD) match {
      case Some(ir.UnitProp(value, unit)) => PropertyUtil.convertToMS(value, unit)
      case _ => None()
    }
    return ret
  }

  def getClockPeriod(): Option[Z] = {
    val ret: Option[Z] = PropertyUtil.getDiscreetPropertyValue(component.properties, OsateProperties.TIMING_PROPERTIES__CLOCK_PERIOD) match {
      case Some(ir.UnitProp(value, unit)) => PropertyUtil.convertToMS(value, unit)
      case _ => None()
    }
    return ret
  }

  def getScheduleSourceText(): Option[String] = {
    val ret: Option[String] = PropertyUtil.getDiscreetPropertyValue(component.properties, CaseSchedulingProperties.SCHEDULE_SOURCE_TEXT) match {
      case Some(ir.ValueProp(value)) => Some(value)
      case _ => None()
    }
    return ret
  }
}


@datatype class AadlProcess(val component: ir.Component,
                            val parent: Option[String],
                            val path: String,
                            val identifier: String,

                            boundProcessor: Option[String],

                            subComponents: ISZ[AadlComponent],

                           ) extends AadlComponent {

  def getDomain(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.DOMAIN)
  }

  def toVirtualMachine(): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(component.properties, HamrProperties.HAMR__COMPONENT_TYPE) match {
      case Some(ir.ValueProp("VIRTUAL_MACHINE")) => T
      case Some(t) => halt(s"Unexpected HAMR::Component_Type ${t} attached to process ${identifier}")
      case None() => F
    }
    return ret
  }
}

@datatype class AadlThread(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,

                           dispatchProtocol: Dispatch_Protocol.Type,
                           period: Option[Z],

                           ports: ISZ[AadlFeature]
                          ) extends AadlComponent {

  def isPeriodic(): B = { return dispatchProtocol == Dispatch_Protocol.Periodic }

  def isSporadic(): B = { return dispatchProtocol == Dispatch_Protocol.Sporadic }

  def getMaxComputeExecutionTime(): Z = {
    val ret: Z = PropertyUtil.getDiscreetPropertyValue(component.properties, OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME) match {
      case Some(ir.RangeProp(low, high)) =>
        PropertyUtil.convertToMS(high.value, high.unit) match {
          case Some(z) => z
          case _ => z"0"
        }
      case _ => z"0"
    }
    return ret
  }

  def getParent(symbolTable: SymbolTable): AadlProcess = {
    // TODO: could also belong to a thread group
    return symbolTable.getProcess(parent.get)
  }

  def getFeatureAccesses(): ISZ[ir.FeatureAccess] = { return component.features.filter(f => f.isInstanceOf[ir.FeatureAccess]).map(f => f.asInstanceOf[ir.FeatureAccess]) }

  def getFeatureEnds(): ISZ[ir.FeatureEnd] = { return component.features.filter(f => f.isInstanceOf[ir.FeatureEnd]).map(f => f.asInstanceOf[ir.FeatureEnd]) }

  def getDomain(symbolTable: SymbolTable): Option[Z] = {
    return getParent(symbolTable).getDomain()
  }

  def getComputeEntrypointSourceText(): Option[String] = {
    return PropertyUtil.getComputeEntrypointSourceText(component.properties)
  }

  def toVirtualMachine(symbolTable: SymbolTable): B = {
    return getParent(symbolTable).toVirtualMachine()
  }

  def isCakeMLComponent(): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__COMPONENT_TYPE) match {
      case Some(ir.ValueProp("MONITOR")) => T
      case Some(ir.ValueProp("FILTER")) => T
      case _ => F
    }
    return ret
  }
}

@datatype class AadlTODOComponent(val component: ir.Component,
                                  val parent: Option[String],
                                  val path: String,
                                  val identifier: String) extends  AadlComponent

@sig trait AadlFeature

@datatype class AadlFeaturePort(feature: ir.Feature) extends AadlFeature

@datatype class AadlFeatureTODO(feature: ir.Feature) extends AadlFeature



@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}
