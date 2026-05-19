// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil}
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object DomainMonitorPlugin {

  val monitorName: String = "domain_monitor"

  @strictpure def monitorProcessPath(root: IdPath): IdPath = root :+ s"${monitorName}_process"

  @strictpure def monitorThreadPath(root: IdPath): IdPath = monitorProcessPath(root) :+ s"${monitorName}_thread"



  val KEY_DomainMonitorPlugin_Model_Transformed: String = "KEY_DomainMonitorPlugin_Model_Transformed"
  val KEY_DomainMonitorPlugin_handled: String = "KEY_DomainMonitorPlugin_handled"

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_DomainMonitorPlugin_Model_Transformed)

  @strictpure def hasHandled(store: Store): B = store.contains(KEY_DomainMonitorPlugin_handled)
}

@sig trait DomainMonitorPlugin
  extends ModelTransformerPlugin
    with MicrokitPlugin {

  @pure override def canHandleModelTransform(model: Aadl,
                                             options: HamrCli.CodegenOption,
                                             types: AadlTypes,
                                             symbolTable: SymbolTable,
                                             store: Store, reporter: Reporter): B = {
    return (
      options.platform == CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        options.runtimeMonitoring &&
        !DomainMonitorPlugin.haveHandledModelTransform(store) &&
        MicrokitUtil.isDomainScheduling(options, symbolTable.rootSystem))

  }

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store + DomainMonitorPlugin.KEY_DomainMonitorPlugin_Model_Transformed ~> BoolValue(T)

    val sysPath = model.components(0).identifier.name
    val monitorProcessorPath = DomainMonitorPlugin.monitorProcessPath(sysPath)
    val monitorThreadPath = DomainMonitorPlugin.monitorThreadPath(sysPath)

    val s = DefaultMonitorInjector().inject(
      model,
      monitorProcessorPath,
      monitorThreadPath,
      symbolTable, localStore, reporter)
    if (s._1.isEmpty || reporter.hasError) {
      return None()
    }

    localStore = s._2

    val reResult = ModelUtil.resolve(s._1.get, s._1.get.components(0).identifier.pos, "", options, localStore, reporter)
    localStore = reResult._2

    if (reResult._1.nonEmpty) {

      localStore = StoreUtil.addNonModelElement(monitorProcessorPath,
        StoreUtil.addNonModelElement(monitorThreadPath, localStore))

      return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
    } else {
      return None()
    }
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      options.platform == CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        options.runtimeMonitoring &&
        DomainMonitorPlugin.haveHandledModelTransform(store) &&
        !DomainMonitorPlugin.hasHandled(store) &&
        SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
        CRustComponentPlugin.hasCRustComponentContributions(store))
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + DomainMonitorPlugin.KEY_DomainMonitorPlugin_handled ~> BoolValue(T)
    var resources: ISZ[Resource] = ISZ()

    val rawSd = SystemDescriptionProviderPlugin.getMSD("normal", localStore)

    val sysPath = model.components(0).identifier.name
    val monitorProcessorPath = DomainMonitorPlugin.monitorProcessPath(sysPath)
    val monitorThreadPath = DomainMonitorPlugin.monitorThreadPath(sysPath)

    val monitorIdPath = s"${monitorProcessorPath(monitorProcessorPath.lastIndex)}_${monitorThreadPath(monitorThreadPath.lastIndex)}"
    val monitorMonPdName = s"${monitorIdPath}_MON"

    var regularSlots: ISZ[SchedulingDomain] = ISZ()
    var monitorSlotOpt: Option[SchedulingDomain] = None()
    var pacerSlotOpt: Option[SchedulingDomain] = None()
    for (sd <- rawSd.schedulingDomains) {
      if (sd.componentName == "pacer") {
        if (pacerSlotOpt.isEmpty) {
          pacerSlotOpt = Some(sd)
        }
      } else if (sd.componentName == monitorMonPdName) {
        // The monitor is not a user partition — it should not be preemptible by the scheduler
        // in the same way as component partitions.
        monitorSlotOpt = Some(sd(isUserPartition = F))
      } else if (sd.componentName != "padding") {
        regularSlots = regularSlots :+ sd
      }
    }

    (monitorSlotOpt, pacerSlotOpt) match {
      case (Some(monitorSlot), Some(pacerSlot)) =>
        // Strip the monitor MON companion PD, the monitor user PD, and their channels from the "normal" SD.
        val strippedPDs = rawSd.protectionDomains.filter(pd => pd.name != monitorMonPdName && pd.name != monitorIdPath)
        val strippedChannels = rawSd.channels.filter(c =>
          c.firstPD != monitorMonPdName && c.secondPD != monitorMonPdName &&
            c.firstPD != monitorIdPath && c.secondPD != monitorIdPath)

        val boundProcessors = symbolTable.getAllActualBoundProcessors()
        assert(boundProcessors.size == 1, "Linter should have ensured there is exactly one bound processor")
        var framePeriod: Z = 0
        boundProcessors(0).getFramePeriod() match {
          case Some(z) => framePeriod = z
          case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
        }

        val regularCount = regularSlots.size

        // Build normal schedule: pacer → component for each regular component.
        var normalScheds: ISZ[SchedulingDomain] = ISZ()
        var regularBudget: Z = 0
        for (s <- regularSlots) {
          normalScheds = normalScheds :+ pacerSlot :+ s
          regularBudget = regularBudget + s.length
        }
        val normalUsedBudget: Z = regularBudget + regularCount * pacerSlot.length
        val normalPadding: Z = framePeriod - normalUsedBudget
        if (normalPadding > 0) {
          normalScheds = normalScheds :+ SchedulingDomain(id = 0, componentName = "padding", length = normalPadding, isUserPartition = F)
        }
        var nonModelMrNames: Set[String] = Set.empty
        var mrSizes: Map[String, Z] = Map.empty
        for (mr <- rawSd.memoryRegions) {
          mrSizes = mrSizes + mr.name ~> mr.sizeInKiBytes
          mr match {
            case p: PortSharedMemoryRegion if StoreUtil.isNonModelElement(p.outgoingPortPath, localStore) =>
              nonModelMrNames = nonModelMrNames + p.name
            case _ =>
          }
        }
        val normalMemoryRegions: ISZ[MemoryRegion] = rawSd.memoryRegions.filter((mr: MemoryRegion) =>
          mr match {
            case p: PortSharedMemoryRegion => !StoreUtil.isNonModelElement(p.outgoingPortPath, localStore)
            case _ => T
          })

        def compactMemMaps(maps: ISZ[MemoryMap], names: Set[String], sizes: Map[String, Z]): ISZ[MemoryMap] = {
          var result: ISZ[MemoryMap] = ISZ()
          var shift: Z = 0
          for (m <- maps) {
            if (names.contains(m.memoryRegion)) {
              shift = shift + sizes.get(m.memoryRegion).getOrElse(MicrokitUtil.defaultMemoryRegionSizeInKiBytes)
            } else {
              result = result :+ m(vaddrInKiBytes = m.vaddrInKiBytes - shift)
            }
          }
          return result
        }

        def filterDomainMemMaps(d: MicrokitDomain, names: Set[String], sizes: Map[String, Z]): MicrokitDomain = {
          d match {
            case pd: ProtectionDomain =>
              val fc: ISZ[MicrokitDomain] = for (c <- pd.children) yield filterDomainMemMaps(c, names, sizes)
              return pd(
                memMaps = compactMemMaps(pd.memMaps, names, sizes),
                children = fc)
            case vm: VirtualMachine =>
              return vm(memMaps = compactMemMaps(vm.memMaps, names, sizes))
            case x =>
              return x
          }
        }

        val normalPDs: ISZ[ProtectionDomain] = for (pd <- strippedPDs) yield
          filterDomainMemMaps(pd, nonModelMrNames, mrSizes).asInstanceOf[ProtectionDomain]
        localStore = SystemDescriptionProviderPlugin.putMSD("normal", SystemDescription(
          name = "normal",
          schedulingDomains = normalScheds,
          protectionDomains = normalPDs,
          memoryRegions = normalMemoryRegions,
          channels = strippedChannels,
          templateContributions = ISZ()), localStore)

        // Build monitor schedule: pacer → monitor → pacer → component for each component;
        // no monitor after the last component since the frame wraps.
        var monitorScheds: ISZ[SchedulingDomain] = ISZ()
        for (s <- regularSlots) {
          monitorScheds = monitorScheds :+ pacerSlot :+ monitorSlot :+ pacerSlot :+ s
        }
        val monitorUsedBudget: Z = regularBudget + 2 * regularCount * pacerSlot.length + regularCount * monitorSlot.length
        if (monitorUsedBudget > framePeriod) {
          val overrunMs = monitorUsedBudget - framePeriod
          reporter.warn(None(), name,
            s"The inclusion of the runtime monitor extends the frame schedule by ${overrunMs} ms beyond the configured ${framePeriod} ms frame period. Consider increasing the frame period to accommodate monitor execution.")
        }
        val monitorPadding: Z = framePeriod - monitorUsedBudget
        if (monitorPadding > 0) {
          monitorScheds = monitorScheds :+ SchedulingDomain(id = 0, componentName = "padding", length = monitorPadding, isUserPartition = F)
        }
        localStore = SystemDescriptionProviderPlugin.putMSD("monitor", SystemDescription(
          name = "monitor",
          schedulingDomains = monitorScheds,
          protectionDomains = rawSd.protectionDomains,
          memoryRegions = rawSd.memoryRegions,
          channels = rawSd.channels,
          templateContributions = ISZ()), localStore)

      case _ =>
        reporter.error(None(), toolName, "Injected monitor or pacer component was not found")
    }

    return (localStore, resources)
  }
}

@datatype class DefaultDomainMonitorPlugin extends DomainMonitorPlugin {

  val name: String = "DefaultDomainMonitorPlugin"

}
