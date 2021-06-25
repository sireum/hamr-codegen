// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{AnnexClause, BTSBLESSAnnexClause}


@sig trait AadlSymbol

@sig trait AadlComponent extends AadlSymbol {
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

@sig trait Processor extends AadlComponent {
  def component: ir.Component
  def parent: Option[String]
  def path: String
  def identifier: String
  def subComponents: ISZ[AadlComponent]
  def connectionInstances: ISZ[ir.ConnectionInstance]

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

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: Option[String],
                              val path: String,
                              val identifier: String,
                              val subComponents: ISZ[AadlComponent],
                              val connectionInstances: ISZ[ir.ConnectionInstance]) extends Processor

@datatype class AadlVirtualProcessor(val component: ir.Component,
                                     val parent: Option[String],
                                     val path: String,
                                     val identifier: String,
                                     val subComponents: ISZ[AadlComponent],
                                     val connectionInstances: ISZ[ir.ConnectionInstance],

                                     val boundProcessor: Option[String]) extends Processor


@datatype class AadlProcess(val component: ir.Component,
                            val parent: Option[String],
                            val path: String,
                            val identifier: String,
                            val subComponents: ISZ[AadlComponent],
                            val connectionInstances: ISZ[ir.ConnectionInstance],

                            val boundProcessor: Option[String]) extends AadlComponent {

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
    val ret: B = PropertyUtil.getDiscreetPropertyValue(component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__COMPONENT_LANGUAGE) match {
      case Some(ir.ValueProp("CakeML")) => T
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
                               val parameters: ISZ[AadlParameter],
                               val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlTODOComponent(val component: ir.Component,
                                  val parent: Option[String],
                                  val path: String,
                                  val identifier: String,
                                  val subComponents: ISZ[AadlComponent],
                                  val connectionInstances: ISZ[ir.ConnectionInstance]) extends  AadlComponent

@sig trait AadlFeature extends AadlSymbol {
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

  def path: String = {
    return CommonUtil.getName(feature.identifier)
  }
}

@sig trait AadlPort extends AadlFeature {
  def direction: ir.Direction.Type
}

@sig trait AadlFeatureEvent extends AadlPort

@sig trait AadlFeatureData extends AadlPort {
  def aadlType: AadlType
}

@datatype class AadlEventPort(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val direction: ir.Direction.Type) extends AadlFeatureEvent

@datatype class AadlEventDataPort(val feature: ir.FeatureEnd,
                                  val featureGroupIds: ISZ[String],
                                  val direction: ir.Direction.Type,
                                  val aadlType: AadlType) extends AadlFeatureData with AadlFeatureEvent

@datatype class AadlDataPort(val feature: ir.FeatureEnd,
                             val featureGroupIds: ISZ[String],
                             val direction: ir.Direction.Type,
                             val aadlType: AadlType) extends AadlFeatureData

@datatype class AadlParameter(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val aadlType: AadlType,
                              val direction: ir.Direction.Type) extends AadlFeatureData

@datatype class AadlFeatureTODO(val feature: ir.Feature,
                                val featureGroupIds: ISZ[String]) extends AadlFeature

@sig trait AadlConnection extends AadlSymbol

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

@sig trait AnnexInfo {
  def annex: AnnexClause
}

@datatype class BTSAnnexInfo(val annex: BTSBLESSAnnexClause,
                             val btsSymbolTable: BTSSymbolTable) extends AnnexInfo

@datatype class TodoAnnexInfo(val annex: AnnexClause) extends AnnexInfo

