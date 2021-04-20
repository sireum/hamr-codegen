// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.ir
import org.sireum.hamr.ir.FeatureEnd

@datatype class SymbolTable(rootSystem: AadlSystem,

                            componentMap: HashSMap[String, AadlComponent],

                            featureMap: HashSMap[String, AadlFeature],

                            airComponentMap: HashSMap[String, ir.Component],
                            airFeatureMap: HashSMap[String, ir.Feature],
                            airClassifierMap: HashSMap[String, ir.Component],

                            // all handled connections
                            aadlConnections: ISZ[AadlConnection],

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

  def getProcess(id: String): AadlProcess = {
    return componentMap.get(id).get.asInstanceOf[AadlProcess]
  }

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

  def subComponents: ISZ[AadlComponent]

  def connectionInstances: ISZ[ir.ConnectionInstance]
}

@datatype class AadlSystem(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent {

  // returns whether the system has HAMR::Bit_Codec_Raw_Connections set to true.  If true,
  // and if this is the top level system, then the resolver guarantees all data components
  // flowing through connections have the max bit codec property attached
  def getUseRawConnection(): B = {
    return PropertyUtil.getUseRawConnection(component.properties)
  }
}

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: Option[String],
                              val path: String,
                              val identifier: String,
                              val subComponents: ISZ[AadlComponent],
                              val connectionInstances: ISZ[ir.ConnectionInstance],

                              isVirtual: B) extends AadlComponent {

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

  def getMaxDomain(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.MAX_DOMAIN)
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
                            val subComponents: ISZ[AadlComponent],
                            val connectionInstances: ISZ[ir.ConnectionInstance],

                            boundProcessor: Option[String]) extends AadlComponent {

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

@datatype class AadlThreadGroup(val component: ir.Component,
                                val parent: Option[String],
                                val path: String,
                                val identifier: String,
                                val subComponents: ISZ[AadlComponent],
                                val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@sig trait AadlThreadOrDevice extends AadlComponent {

  def dispatchProtocol: Dispatch_Protocol.Type

  def period: Option[Z]

  def ports: ISZ[AadlFeature]

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
    val _parent = symbolTable.componentMap.get(parent.get).get

    val ret: AadlProcess = _parent match {
      case a: AadlThreadGroup =>symbolTable.getProcess(a.parent.get)
      case p: AadlProcess => symbolTable.getProcess(parent.get)
      case _ => halt("Unexpected parent: _parent")
    }
    return ret
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

@datatype class AadlThread(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance],

                           val dispatchProtocol: Dispatch_Protocol.Type,
                           val period: Option[Z],

                           val ports: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlDevice(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance],

                           val dispatchProtocol: Dispatch_Protocol.Type,
                           val period: Option[Z],

                           val ports: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlSubprogram(val component: ir.Component,
                               val parent: Option[String],
                               val path: String,
                               val identifier: String,
                               val subComponents: ISZ[AadlComponent],
                               val features: ISZ[AadlFeature],
                               val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlTODOComponent(val component: ir.Component,
                                  val parent: Option[String],
                                  val path: String,
                                  val identifier: String,
                                  val subComponents: ISZ[AadlComponent],
                                  val connectionInstances: ISZ[ir.ConnectionInstance]) extends  AadlComponent

@sig trait AadlFeature {
  def feature: ir.Feature

  // The identifiers of features groups this feature is nested within.
  // Needed to ensure generated slang/c/etc identifiers are unique
  def featureGroupIds: ISZ[String]

  def identifier: String = {
    val id = CommonUtil.getLastName(feature.identifier)
    val ret: String =
      if(featureGroupIds.nonEmpty) st"${(featureGroupIds, "_")}_${id}".render
      else id
    return ret
  }
}

@sig trait AadlFeatureEvent extends AadlFeature

@sig trait AadlFeatureData extends AadlFeature {
  def aadlType: AadlType
}

@datatype class AadlEventPort(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String]) extends AadlFeatureEvent

@datatype class AadlEventDataPort(val feature: ir.FeatureEnd,
                                  val featureGroupIds: ISZ[String],
                                  val aadlType: AadlType) extends AadlFeatureData with AadlFeatureEvent

@datatype class AadlDataPort(val feature: ir.FeatureEnd,
                             val featureGroupIds: ISZ[String],
                             val aadlType: AadlType) extends AadlFeatureData

@datatype class AadlFeatureTODO(val feature: ir.Feature,
                                val featureGroupIds: ISZ[String]) extends AadlFeature

@sig trait AadlConnection

@datatype class AadlPortConnection(val name: String,

                                  val srcComponent: AadlComponent,
                                  val srcFeature: AadlFeature,
                                  val dstComponent: AadlComponent,
                                  val dstFeature: AadlFeature,

                                  val connectionDataType: AadlType, // will be EmptyType for event ports

                                  val connectionInstance: ir.ConnectionInstance) extends AadlConnection {

  def getConnectionKind(): ir.ConnectionKind.Type = { return connectionInstance.kind}
  def getProperties(): ISZ[ir.Property] = { return connectionInstance.properties}
}

@datatype class AadlConnectionTODO extends AadlConnection

@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}
