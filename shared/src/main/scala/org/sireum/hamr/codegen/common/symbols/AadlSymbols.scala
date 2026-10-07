// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.codegen.common.util.TimeUtil
import org.sireum.hamr.ir
import org.sireum.hamr.ir._
import org.sireum.message.Position

@sig trait AadlSymbol

@sig trait AadlComponent extends AadlSymbol {
  @pure def component: ir.Component

  @pure def properties: ISZ[ir.Property] = {
    return component.properties
  }

  @pure def parent: IdPath

  @pure def path: IdPath

  @pure def pathAsString(sep: String): String = {
    return st"${(path, sep)}".render
  }

  @pure def identifier: String

  @pure def features: ISZ[AadlFeature]

  @pure def subComponents: ISZ[AadlComponent]

  @pure def connectionInstances: ISZ[ir.ConnectionInstance]

  @pure def getFeatureAccesses(): ISZ[AadlAccessFeature] = {
    return features.filter(p => p.isInstanceOf[AadlAccessFeature]).map(m => m.asInstanceOf[AadlAccessFeature])
  }

  @pure def getPorts(): ISZ[AadlPort] = {
    return features.filter(p => p.isInstanceOf[AadlPort]).map(m => m.asInstanceOf[AadlPort])
  }

  @pure def getPortByPath(path: ISZ[String]): Option[AadlPort] = {
    getPorts().filter(p => p.path == path) match {
      case ISZ(p) => return Some(p)
      case _ => return None()
    }
  }

  @pure def annexes(): ISZ[ir.Annex] = {
    return component.annexes
  }

  @pure def classifierAsString: String = {
    return component.classifier.get.name
  }

  @pure def classifier: ISZ[String] = {
    return ops.StringOps(ops.StringOps(classifierAsString).replaceAllLiterally("::", "^")).split(c => c == '^')
  }

  @pure def posOpt: Option[Position] = {
    return component.identifier.pos
  }
}

@datatype class AadlSystem(val component: ir.Component,
                           val parent: IdPath,
                           val path: IdPath,
                           val identifier: String,
                           val features: ISZ[AadlFeature],
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent {

  // returns whether the system has HAMR::Bit_Codec_Raw_Connections set to true.  If true,
  // and if this is the top level system, then the resolver guarantees all data components
  // flowing through connections have the max bit codec property attached
  @pure def getUseRawConnection(): B = {
    return PropertyUtil.getUseRawConnection(component.properties)
  }

  @pure def getDomainMappings(): Map[IdPath, Z] = {
    return PropertyUtil.getDomainMappings(component.properties)
  }
}

@sig trait Processor extends AadlComponent {
  @pure def component: ir.Component

  @pure def parent: IdPath

  @pure def path: IdPath

  @pure def identifier: String

  @pure def subComponents: ISZ[AadlComponent]

  @pure def connectionInstances: ISZ[ir.ConnectionInstance]

  // Frame_Period, Clock_Period and Slot_Time in picoseconds, parsed once by SymbolResolver
  // (doc/ExactTime-design.md, D3)
  @pure def framePeriodPs: Option[Z]

  @pure def clockPeriodPs: Option[Z]

  @pure def slotTimePs: Option[Z]

  // TODO(exact time, step 8): remove; floors to whole ms as the old accessor did
  @pure def getFramePeriod(): Option[Z] = {
    return AadlSymbols.floorToMs(framePeriodPs)
  }

  // TODO(exact time, step 8): remove; floors to whole ms as the old accessor did
  @pure def getClockPeriod(): Option[Z] = {
    return AadlSymbols.floorToMs(clockPeriodPs)
  }

  @pure def getMaxDomain(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.MAX_DOMAIN)
  }

  @pure def getSlotTime(): Option[Z] = {
    return PropertyUtil.getUnitPropZ(component.properties, OsateProperties.TIMING_PROPERTIES__SLOT_TIME)
  }

  @pure def getScheduleSourceText(): Option[String] = {
    val ret: Option[String] = PropertyUtil.getDiscreetPropertyValue(component.properties, CaseSchedulingProperties.SCHEDULE_SOURCE_TEXT) match {
      case Some(ir.ValueProp(value)) => Some(value)
      case _ => None()
    }
    return ret
  }

  @pure def getPacingMethod(): Option[CaseSchedulingProperties.PacingMethod.Type] = {
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
  @pure def dispatchProtocol: Dispatch_Protocol.Type

  // Period in picoseconds, parsed once by SymbolResolver (doc/ExactTime-design.md, D3)
  @pure def periodPs: Option[Z]

  // TODO(exact time, step 8): remove; floors to whole ms as the old period field did
  @pure def period: Option[Z] = {
    return AadlSymbols.floorToMs(periodPs)
  }

  @pure def isPeriodic(): B = {
    return dispatchProtocol == Dispatch_Protocol.Periodic
  }

  @pure def isSporadic(): B = {
    return dispatchProtocol == Dispatch_Protocol.Sporadic
  }
}

@datatype class AadlProcessor(val component: ir.Component,
                              val parent: IdPath,
                              val path: IdPath,
                              val identifier: String,
                              val features: ISZ[AadlFeature],
                              val subComponents: ISZ[AadlComponent],
                              val connectionInstances: ISZ[ir.ConnectionInstance],

                              val framePeriodPs: Option[Z],
                              val clockPeriodPs: Option[Z],
                              val slotTimePs: Option[Z]) extends Processor

@datatype class AadlVirtualProcessor(val component: ir.Component,
                                     val parent: IdPath,
                                     val path: IdPath,
                                     val identifier: String,
                                     val features: ISZ[AadlFeature],
                                     val subComponents: ISZ[AadlComponent],
                                     val connectionInstances: ISZ[ir.ConnectionInstance],

                                     val dispatchProtocol: Dispatch_Protocol.Type,
                                     val periodPs: Option[Z],

                                     val framePeriodPs: Option[Z],
                                     val clockPeriodPs: Option[Z],
                                     val slotTimePs: Option[Z]) extends Processor with AadlDispatchableComponent

@datatype class AadlProcess(val component: ir.Component,
                            val parent: IdPath,
                            val path: IdPath,
                            val identifier: String,
                            val features: ISZ[AadlFeature],
                            val subComponents: ISZ[AadlComponent],
                            val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent {

  @pure def getDomain(symbolTable: SymbolTable): Option[Z] = {
    val ret: Option[Z] = symbolTable.rootSystem.getDomainMappings().get(path) match {
      case Some(z) => Some(z)
      case _ => PropertyUtil.getUnitPropZ(component.properties, CaseSchedulingProperties.DOMAIN)
    }
    return ret
  }

  /**
    *  @return T if bound to a virtual processor or it has HAMR::Component_Type => VIRTUAL_MACHINE
    */
  @pure def toVirtualMachine(symbolTable: SymbolTable): B = {

    // or is the parent a virtual processor (symbol checking phase ensures the virtual processor
    // is bound to an actual processor)
    getBoundProcessor(symbolTable) match {
      case Some(avp: AadlVirtualProcessor) => return T
      case _ => return F
    }
  }

  @pure def getBoundProcessor(symbolTable: SymbolTable): Option[Processor] = {
    symbolTable.getBoundProcessors(this) match {
      case ISZ(p) => return Some(p)
      case x if x.isEmpty => return None()
      case x =>
        halt(s"Infeasible: the linter currently only allows a single processor binding but $identifier has ${x.size}")
    }
  }

  @pure def getThreads(): ISZ[AadlThread] = {
    return subComponents.filter((p: AadlComponent) => p.isInstanceOf[AadlThread]).map((m: AadlComponent) => m.asInstanceOf[AadlThread])
  }
}

@datatype class AadlThreadGroup(val component: ir.Component,
                                val parent: IdPath,
                                val path: IdPath,
                                val identifier: String,
                                val features: ISZ[AadlFeature],
                                val subComponents: ISZ[AadlComponent],
                                val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@sig trait AadlThreadOrDevice extends AadlComponent with AadlDispatchableComponent {

  // Compute_Execution_Time (low, high) in picoseconds, parsed once by SymbolResolver
  // (doc/ExactTime-design.md, D3)
  @pure def computeExecutionTimePs: Option[(Z, Z)]

  // TODO(exact time, step 8): remove; floors to whole ms as the old accessor did
  @pure def getComputeExecutionTime(): Option[(Z, Z)] = {
    computeExecutionTimePs match {
      case Some((low, high)) => return Some((low / TimeUtil.psPerMs, high / TimeUtil.psPerMs))
      case _ => return None()
    }
  }

  // the high end of Compute_Execution_Time in picoseconds, 0 if it is not set
  @pure def getMaxComputeExecutionTimePs(): Z = {
    computeExecutionTimePs match {
      case Some((_, high)) => return high
      case _ => return 0
    }
  }

  @pure def getMaxComputeExecutionTime(): Z = {
    val ret: Z = getComputeExecutionTime() match {
      case Some((low, high)) => high
      case _ => z"0"
    }
    return ret
  }

  @pure def getParent(symbolTable: SymbolTable): AadlProcess = {
    val _parent = symbolTable.componentMap.get(parent).get

    val ret: AadlProcess = _parent match {
      case a: AadlThreadGroup => symbolTable.getProcess(a.parent)
      case p: AadlProcess => symbolTable.getProcess(parent)
      case _ => halt(s"Unexpected parent for ${parent}: ${_parent}")
    }
    return ret
  }

  @pure def getDomain(symbolTable: SymbolTable): Option[Z] = {
    this match {
      case a: AadlDevice => return None()
      case a: AadlThread => return getParent(symbolTable).getDomain(symbolTable)
    }
  }

  @pure def getComputeEntrypointSourceText(): Option[String] = {
    return PropertyUtil.getComputeEntrypointSourceText(component.properties)
  }

  @pure def toVirtualMachine(symbolTable: SymbolTable): B = {
    return getParent(symbolTable).toVirtualMachine(symbolTable)
  }

  @pure def isCakeMLComponent(): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__COMPONENT_LANGUAGE) match {
      case Some(ir.ValueProp("CakeML")) => T
      case _ => F
    }
    return ret
  }

  @pure def stackSizeInBytes(): Option[Z] = {
    return PropertyUtil.getStackSizeInBytes(component)
  }
}

@datatype class AadlThread(val component: ir.Component,
                           val parent: IdPath,
                           val path: IdPath,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance],

                           val dispatchProtocol: Dispatch_Protocol.Type,
                           val periodPs: Option[Z],
                           val computeExecutionTimePs: Option[(Z, Z)],

                           val features: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlDevice(val component: ir.Component,
                           val parent: IdPath,
                           val path: IdPath,
                           val identifier: String,
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance],

                           val dispatchProtocol: Dispatch_Protocol.Type,
                           val periodPs: Option[Z],
                           val computeExecutionTimePs: Option[(Z, Z)],

                           val features: ISZ[AadlFeature]) extends AadlThreadOrDevice

@datatype class AadlSubprogram(val component: ir.Component,
                               val parent: IdPath,
                               val path: IdPath,
                               val identifier: String,
                               val subComponents: ISZ[AadlComponent],
                               val connectionInstances: ISZ[ir.ConnectionInstance],

                               val features: ISZ[AadlFeature]) extends AadlComponent {
  @pure def getClassifier(): String = {
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
                                    val parent: IdPath,
                                    val path: IdPath,
                                    val identifier: String,
                                    val features: ISZ[AadlFeature],
                                    val subComponents: ISZ[AadlComponent],
                                    val connectionInstances: ISZ[ir.ConnectionInstance]

                                   ) extends AadlComponent

@datatype class AadlData(val component: ir.Component,
                         val parent: IdPath,
                         val path: IdPath,
                         val identifier: String,
                         val typ: AadlType,
                         val features: ISZ[AadlFeature],
                         val subComponents: ISZ[AadlComponent],
                         val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlMemory(val component: ir.Component,
                           val parent: IdPath,
                           val path: IdPath,
                           val identifier: String,
                           val features: ISZ[AadlFeature],
                           val subComponents: ISZ[AadlComponent],
                           val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlBus(val component: ir.Component,
                        val parent: IdPath,
                        val path: IdPath,
                        val identifier: String,
                        val features: ISZ[AadlFeature],
                        val subComponents: ISZ[AadlComponent],
                        val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlVirtualBus(val component: ir.Component,
                               val parent: IdPath,
                               val path: IdPath,
                               val identifier: String,
                               val features: ISZ[AadlFeature],
                               val subComponents: ISZ[AadlComponent],
                               val connectionInstances: ISZ[ir.ConnectionInstance]) extends AadlComponent

@datatype class AadlAbstract(val component: ir.Component,
                             val parent: IdPath,
                             val path: IdPath,
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
  @pure def feature: ir.Feature

  // The identifiers of features groups this feature is nested within.
  // Needed to ensure generated slang/c/etc identifiers are unique
  @pure def featureGroupIds: ISZ[String]

  @pure def identifier: String = {
    val id = CommonUtil.getLastName(feature.identifier)
    val ret: String =
      if (featureGroupIds.nonEmpty) st"${(featureGroupIds, "_")}_${id}".render
      else id
    return ret
  }

  @pure def path: IdPath = {
    return feature.identifier.name
  }

  @pure def pathAsString(sep: String): String = {
    return st"${(path, sep)}".render
  }
}

@sig trait AadlDirectedFeature extends AadlFeature {

  @pure override def feature: ir.FeatureEnd

  @pure def direction: ir.Direction.Type
}

@sig trait AadlFeatureData {
  @pure def aadlType: AadlType
}

@sig trait AadlPort extends AadlDirectedFeature {
  @pure def isEvent: B
  @pure def isData: B

  @pure def posOpt: Option[Position] = {
    return feature.identifier.pos
  }
}

@sig trait AadlFeatureEvent extends AadlDirectedFeature {
  @pure def queueSize: Z = {
    return PropertyUtil.getQueueSize(feature, 1)
  }

  @pure def getComputeEntrypointSourceText(): Option[String] = {
    return PropertyUtil.getComputeEntrypointSourceText(feature.properties)
  }
}

@datatype class AadlEventPort(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val direction: ir.Direction.Type) extends AadlPort with AadlFeatureEvent {
  val isEvent: B = T
  val isData: B = F
}

@datatype class AadlEventDataPort(val feature: ir.FeatureEnd,
                                  val featureGroupIds: ISZ[String],
                                  val direction: ir.Direction.Type,
                                  val aadlType: AadlType) extends AadlPort with AadlFeatureData with AadlFeatureEvent {
  val isEvent: B = T
  val isData: B = T
}

@datatype class AadlDataPort(val feature: ir.FeatureEnd,
                             val featureGroupIds: ISZ[String],
                             val direction: ir.Direction.Type,
                             val aadlType: AadlType) extends AadlPort with AadlFeatureData {
  val isEvent: B = F
  val isData: B = T
}

@datatype class AadlParameter(val feature: ir.FeatureEnd,
                              val featureGroupIds: ISZ[String],
                              val aadlType: AadlType,
                              val direction: ir.Direction.Type) extends AadlDirectedFeature with AadlFeatureData {

  @pure def getName(): String = {
    return CommonUtil.getLastName(feature.identifier)
  }
}

@sig trait AadlAccessFeature extends AadlFeature {

  @pure override def feature: ir.FeatureAccess

  @pure def kind: ir.AccessType.Type

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

  @pure def getConnectionKind(): ir.ConnectionKind.Type = {
    return connectionInstance.kind
  }

  @pure def getProperties(): ISZ[ir.Property] = {
    return connectionInstance.properties
  }
}

@datatype class AadlConnectionTODO extends AadlConnection

@enum object Dispatch_Protocol {
  'Periodic
  'Sporadic
}

@sig trait AnnexInfo

@sig trait AnnexLibInfo extends AnnexInfo {
  @pure def annex: AnnexLib
}

@sig trait AnnexClauseInfo extends AnnexInfo {
  @pure def annex: AnnexClause
}

@datatype class GclAnnexLibInfo(val annex: GclLib,
                                val name: IdPath,
                                val gclSymbolTable: GclSymbolTable) extends AnnexLibInfo

@datatype class GclAnnexClauseInfo(val annex: GclSubclause,
                                   val gclSymbolTable: GclSymbolTable) extends AnnexClauseInfo

@datatype class BTSAnnexInfo(val annex: BTSBLESSAnnexClause,
                             val btsSymbolTable: BTSSymbolTable) extends AnnexClauseInfo

@datatype class TodoAnnexInfo(val annex: AnnexClause) extends AnnexClauseInfo

object AadlSymbols {
  // TODO(exact time, step 8): remove with the millisecond accessors; floors to whole ms as
  // PropertyUtil.convertToMS did
  @strictpure def floorToMs(ps: Option[Z]): Option[Z] =
    ps match {
      case Some(v) => Some(v / TimeUtil.psPerMs)
      case _ => None()
    }
}
