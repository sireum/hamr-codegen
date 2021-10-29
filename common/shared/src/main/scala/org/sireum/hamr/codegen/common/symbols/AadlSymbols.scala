// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
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

  def features: ISZ[AadlFeature]

  def subComponents: ISZ[AadlComponent]

  def connectionInstances: ISZ[ir.ConnectionInstance]


  def getFeatureAccesses(): ISZ[AadlAccessFeature] = { return features.filter(p => p.isInstanceOf[AadlAccessFeature]).map(m => m.asInstanceOf[AadlAccessFeature])}

  def getPorts(): ISZ[AadlPort] = { return features.filter(p => p.isInstanceOf[AadlPort]).map(m => m.asInstanceOf[AadlPort]) }

}

@datatype class AadlSystem(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val features: ISZ[AadlFeature],
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
      case Some(ir.UnitProp(value, unit)) =>
        assert(unit.nonEmpty, s"frame period's unit not provided for ${identifier}")
        Some(PropertyUtil.convertToMS(value, unit.get))
      case _ => None()
    }
    return ret
  }

  def getClockPeriod(): Option[Z] = {
    val ret: Option[Z] = PropertyUtil.getDiscreetPropertyValue(component.properties, OsateProperties.TIMING_PROPERTIES__CLOCK_PERIOD) match {
      case Some(ir.UnitProp(value, unit)) =>
        assert(unit.nonEmpty, s"clock period's unit not provided for ${identifier}")
        Some(PropertyUtil.convertToMS(value, unit.get))
      case _ => None()
    }
    return ret
  }

  def getMaxDomain(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.MAX_DOMAIN)
  }

  def getSlotTime(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, OsateProperties.TIMING_PROPERTIES__SLOT_TIME)
  }

  def getScheduleSourceText(): Option[String] = {
    val ret: Option[String] = PropertyUtil.getDiscreetPropertyValue(component.properties, CaseSchedulingProperties.SCHEDULE_SOURCE_TEXT) match {
      case Some(ir.ValueProp(value)) => Some(value)
      case _ => None()
    }
    return ret
  }


  def getPacingMethod(): Option[CaseSchedulingProperties.PacingMethod.Type] = {
    val ret: Option[CaseSchedulingProperties.PacingMethod.Type] = PropertyUtil.getDiscreetPropertyValue(component.properties, CaseSchedulingProperties.PACING_METHOD) match {
      case Some(ir.ValueProp("Pacer")) => Some(CaseSchedulingProperties.PacingMethod.Pacer)
      case Some(ir.ValueProp("Self_Pacing")) => Some(CaseSchedulingProperties.PacingMethod.SelfPacing)
      case Some(t) => halt(s"Unexpected ${CaseSchedulingProperties.PACING_METHOD} ${t} attached to process ${identifier}")
      case _ => None()
    }
    return ret
  }
}


@sig trait AadlDispatchableComponent {
  def dispatchProtocol: Dispatch_Protocol.Type

  def period: Option[Z]

  def isPeriodic(): B = { return dispatchProtocol == Dispatch_Protocol.Periodic }

  def isSporadic(): B = { return dispatchProtocol == Dispatch_Protocol.Sporadic }
}

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: Option[String],
                              val path: String,
                              val identifier: String,
                              val features: ISZ[AadlFeature],
                              val subComponents: ISZ[AadlComponent],
                              val connectionInstances: ISZ[ir.ConnectionInstance]) extends Processor

@datatype class AadlVirtualProcessor(val component: ir.Component,
                                     val parent: Option[String],
                                     val path: String,
                                     val identifier: String,
                                     val features: ISZ[AadlFeature],
                                     val subComponents: ISZ[AadlComponent],
                                     val connectionInstances: ISZ[ir.ConnectionInstance],

                                     val dispatchProtocol: Dispatch_Protocol.Type,
                                     val period: Option[Z],

                                     val boundProcessor: Option[String]) extends Processor with AadlDispatchableComponent

@datatype class AadlProcess(val component: ir.Component,
                            val parent: Option[String],
                            val path: String,
                            val identifier: String,
                            val features: ISZ[AadlFeature],
                            val subComponents: ISZ[AadlComponent],
                            val connectionInstances: ISZ[ir.ConnectionInstance],

                            val boundProcessor: Option[String]) extends AadlComponent {

  def getDomain(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.DOMAIN)
  }

  /**
    *  @return T if bound to a virtual processor or it has HAMR::Component_Type => VIRTUAL_MACHINE
    */
  def toVirtualMachine(symbolTable: SymbolTable): B = {

    // or is the parent a virtual processor (symbol checking phase ensures the virtual processor
    // is bound to an actual processor)
    boundProcessor match {
      case Some(_parent) =>
        symbolTable.componentMap.get(_parent).get match {
          case avp: AadlVirtualProcessor => return T
          case _ =>
        }
      case _ =>
    }

    return F
  }

  def getBoundProcessor(symbolTable: SymbolTable): Option[Processor] = {
    return symbolTable.getBoundProcessor(this)
  }

  def getThreads(): ISZ[AadlThread] = {
    return subComponents.filter((p: AadlComponent) => p.isInstanceOf[AadlThread]).map((m: AadlComponent) => m.asInstanceOf[AadlThread])
  }
}

@datatype class AadlThreadGroup(val component: ir.Component,
                                val parent: Option[String],
                                val path: String,
                                val identifier: String,
                                val features: ISZ[AadlFeature],
                                val subComponents: ISZ[AadlComponent],
                                val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@sig trait AadlThreadOrDevice extends AadlComponent with AadlDispatchableComponent {

  def period: Option[Z]

  def getComputeExecutionTime(): Option[(Z, Z)] = {
    val ret: Option[(Z, Z)] = PropertyUtil.getDiscreetPropertyValue(component.properties, OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME) match {
      case Some(ir.RangeProp(low, high)) =>
        assert(low.unit.nonEmpty, s"unit not provided for min compute execution time for ${identifier}")
        assert(high.unit.nonEmpty, s"unit not provided for max compute execution time for ${identifier}")

        val _low = PropertyUtil.convertToMS(low.value, low.unit.get)
        val _high = PropertyUtil.convertToMS(high.value, high.unit.get)

        Some((_low, _high))
      case _ => None()
    }
    return ret
  }

  def getMaxComputeExecutionTime(): Z = {
    val ret: Z = getComputeExecutionTime() match {
      case Some((low, high)) => high
      case _ => z"0"
    }
    return ret
  }

  def getParent(symbolTable: SymbolTable): AadlProcess = {
    val _parent = symbolTable.componentMap.get(parent.get).get

    val ret: AadlProcess = _parent match {
      case a: AadlThreadGroup =>symbolTable.getProcess(a.parent.get)
      case p: AadlProcess => symbolTable.getProcess(parent.get)
      case _ => halt(s"Unexpected parent for ${parent.get}: ${_parent}")
    }
    return ret
  }

  def getDomain(symbolTable: SymbolTable): Option[Z] = {
    this match {
      case a: AadlDevice => return None()
      case a: AadlThread => return getParent(symbolTable).getDomain()
    }
  }

  def getComputeEntrypointSourceText(): Option[String] = {
    return PropertyUtil.getComputeEntrypointSourceText(component.properties)
  }

  def toVirtualMachine(symbolTable: SymbolTable): B = {
    return getParent(symbolTable).toVirtualMachine(symbolTable)
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

                           val features: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlDevice(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance],

                           val dispatchProtocol: Dispatch_Protocol.Type,
                           val period: Option[Z],

                           val features: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlSubprogram(val component: ir.Component,
                               val parent: Option[String],
                               val path: String,
                               val identifier: String,
                               val subComponents: ISZ[AadlComponent],
                               val connectionInstances: ISZ[ir.ConnectionInstance],

                               val features: ISZ[AadlFeature]) extends AadlComponent {
  def getClassifier(): String = {
    var s = ops.StringOps(component.classifier.get.name)
    val index = s.lastIndexOf(':') + 1
    s = ops.StringOps(s.substring(index, component.classifier.get.name.size))
    return StringUtil.replaceAll(s.s, ".", "_")
  }

  def parameters: ISZ[AadlParameter] = {
    return features.filter((f: AadlFeature) => f.isInstanceOf[AadlParameter]).map((m: AadlFeature) => m.asInstanceOf[AadlParameter])
  }
}

@datatype class AadlSubprogramGroup(val component: ir.Component,
                                    val parent: Option[String],
                                    val path: String,
                                    val identifier: String,
                                    val features: ISZ[AadlFeature],
                                    val subComponents: ISZ[AadlComponent],
                                    val connectionInstances: ISZ[ir.ConnectionInstance]

                                   ) extends AadlComponent

@datatype class AadlData(val component: ir.Component,
                         val parent: Option[String],
                         val path: String,
                         val identifier: String,
                         val features: ISZ[AadlFeature],
                         val subComponents: ISZ[AadlComponent],
                         val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlMemory(val component: ir.Component,
                           val parent: Option[String],
                           val path: String,
                           val identifier: String,
                           val features: ISZ[AadlFeature],
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlBus(val component: ir.Component,
                        val parent: Option[String],
                        val path: String,
                        val identifier: String,
                        val features: ISZ[AadlFeature],
                        val subComponents: ISZ[AadlComponent],
                        val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlVirtualBus(val component: ir.Component,
                               val parent: Option[String],
                               val path: String,
                               val identifier: String,
                               val features: ISZ[AadlFeature],
                               val subComponents: ISZ[AadlComponent],
                               val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlAbstract(val component: ir.Component,
                             val parent: Option[String],
                             val path: String,
                             val identifier: String,
                             val features: ISZ[AadlFeature],
                             val subComponents: ISZ[AadlComponent],
                             val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent


/************************************************************************************
*
* Feature
*
***********************************************************************************/

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

@sig trait AadlDirectedFeature extends AadlFeature {

  override def feature: ir.FeatureEnd

  def direction: ir.Direction.Type
}

@sig trait AadlFeatureData {
  def aadlType: AadlType
}

@sig trait AadlPort extends AadlDirectedFeature

@sig trait AadlFeatureEvent

@datatype class AadlEventPort(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val direction: ir.Direction.Type) extends AadlPort with AadlFeatureEvent

@datatype class AadlEventDataPort(val feature: ir.FeatureEnd,
                                  val featureGroupIds: ISZ[String],
                                  val direction: ir.Direction.Type,
                                  val aadlType: AadlType) extends AadlPort with AadlFeatureData with AadlFeatureEvent

@datatype class AadlDataPort(val feature: ir.FeatureEnd,
                             val featureGroupIds: ISZ[String],
                             val direction: ir.Direction.Type,
                             val aadlType: AadlType) extends AadlPort with AadlFeatureData

@datatype class AadlParameter(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val aadlType: AadlType,
                              val direction: ir.Direction.Type) extends AadlDirectedFeature with AadlFeatureData {

  def getName(): String = { return CommonUtil.getLastName(feature.identifier) }
}

@sig trait AadlAccessFeature extends AadlFeature {

  override def feature: ir.FeatureAccess

  def kind: ir.AccessType.Type

}

@datatype class AadlBusAccess(val feature: ir.FeatureAccess,
                              val featureGroupIds: ISZ[String],
                              val kind: ir.AccessType.Type) extends AadlAccessFeature

@datatype class AadlDataAccess(val feature: ir.FeatureAccess,
                               val featureGroupIds: ISZ[String],
                               val kind: ir.AccessType.Type) extends AadlAccessFeature

@datatype class AadlSubprogramAccess(val feature: ir.FeatureAccess,
                                     val featureGroupIds: ISZ[String],
                                     val kind: ir.AccessType.Type) extends AadlAccessFeature

@datatype class AadlSubprogramGroupAccess(val feature: ir.FeatureAccess,
                                          val featureGroupIds: ISZ[String],
                                          val kind: ir.AccessType.Type) extends AadlAccessFeature


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

