// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, MonitorInjector}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.monitors.RustModelTransformerPlugin.{KEY_RustModelTransformerPlugin, KEY_RustModelTransformerPlugin_sched_mod}
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.util.{MemoryMap, Perm, ProtectionDomain, SchedulingDomain, SystemDescription}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RAST}

object RustModelTransformerPlugin {

  val KEY_RustModelTransformerPlugin: String = "KEY_RustModelTransformerPlugin"

  val KEY_RustModelTransformerPlugin_sched_mod: String = "KEY_RustModelTransformerPlugin_sched_mod"

}

@sig trait RustModelTransformerPlugin
  extends ModelTransformerPlugin
  with MicrokitPlugin {

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_RustModelTransformerPlugin)

  @pure override def canHandleModelTransform(model: Aadl,
                                              options: HamrCli.CodegenOption,
                                              types: AadlTypes,
                                              symbolTable: SymbolTable,
                                              store: Store, reporter: Reporter): B = {
    return options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !haveHandledModelTransform(store) &&
      !reporter.hasError &&
      options.runtimeMonitoring
  }
      // && GumboXRustUtil.hasGumbo(model, symbolTable)

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store + KEY_RustModelTransformerPlugin ~> BoolValue(T)

    val isMCS: B = CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.MCS
    val rmodel = MonitorInjector.inject(model, symbolTable, isMCS, reporter)
    if (!reporter.hasError) {
      val reResult = ModelUtil.resolve(rmodel, rmodel.components(0).identifier.pos, "", options, localStore, reporter)
      localStore = reResult._2
      if (reResult._1.nonEmpty) {
        val monitorThreadPath = rmodel.components(0).identifier.name :+ MonitorInjector.monitorProcessId :+ MonitorInjector.monitorThreadId
        localStore = StoreUtil.addPluginGeneratedComponent(monitorThreadPath, localStore)
        return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
      }
    }
    return None()
  }


  @strictpure def hasHandled(store: Store): B = store.contains(KEY_RustModelTransformerPlugin_sched_mod)

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return !reporter.hasError &&
      haveHandledModelTransform(store) &&
      SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
      !hasHandled(store) &&
      CRustComponentPlugin.hasCRustComponentContributions(store)
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + KEY_RustModelTransformerPlugin_sched_mod ~> BoolValue(T)
    var resources: ISZ[Resource] = ISZ()

    val rawSd = SystemDescriptionProviderPlugin.getMSD("normal", localStore)

    val monitorIdPath = st"${MonitorInjector.monitorProcessId}_${MonitorInjector.monitorThreadId}".render
    val monitorMonPdName = st"${monitorIdPath}_MON".render

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

    monitorSlotOpt match {
      case Some(monitorSlot) =>
        // Strip the monitor MON companion PD, the monitor user PD, and their channels from the "normal" SD.
        val strippedPDs = rawSd.protectionDomains.filter(pd => pd.name != monitorMonPdName && pd.name != monitorIdPath)
        val strippedChannels = rawSd.channels.filter(c =>
          c.firstPD != monitorMonPdName && c.secondPD != monitorMonPdName &&
          c.firstPD != monitorIdPath && c.secondPD != monitorIdPath)

        if (pacerSlotOpt.nonEmpty) {
          // Pacer-based scheduling (domain scheduler): build interleaved pacer/component schedules.
          val pacerSlot = pacerSlotOpt.get

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
          localStore = SystemDescriptionProviderPlugin.putMSD("normal", SystemDescription(
            name = "normal",
            schedulingDomains = normalScheds,
            protectionDomains = strippedPDs,
            memoryRegions = rawSd.memoryRegions,
            channels = strippedChannels), localStore)

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
            channels = rawSd.channels), localStore)
        } else {
          // MCS scheduling: no pacer; each thread has its own time budget via scheduling domains.
          val boundProcessors = symbolTable.getAllActualBoundProcessors()
          assert(boundProcessors.size == 1, "Linter should have ensured there is exactly one bound processor")
          var framePeriodMs: Z = 0
          boundProcessors(0).getFramePeriod() match {
            case Some(z) => framePeriodMs = z
            case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
          }
          val framePeriodNano: Z = framePeriodMs * 1_000_000

          // Strip the originally-computed pad from regularSlots; pads are recomputed per SD below
          val regularThreadSlots: ISZ[SchedulingDomain] = regularSlots.filter((sd: SchedulingDomain) => sd.componentName != "pad")

          // "normal" SD: [pad?,] regular threads (no monitor); pad uses full remaining budget
          var normalUsedNano: Z = 0
          for (s <- regularThreadSlots) {
            normalUsedNano = normalUsedNano + s.length
          }
          val normalRemainder: Z = framePeriodNano - normalUsedNano
          val normalScheds: ISZ[SchedulingDomain] =
            if (normalRemainder > 0)
              SchedulingDomain(id = 0, componentName = "pad", length = normalRemainder, isUserPartition = F) +: regularThreadSlots
            else regularThreadSlots

          localStore = SystemDescriptionProviderPlugin.putMSD("normal", SystemDescription(
            name = "normal",
            schedulingDomains = normalScheds,
            protectionDomains = strippedPDs,
            memoryRegions = rawSd.memoryRegions,
            channels = strippedChannels), localStore)

          // "monitor" SD: [pad?,] monitor before each thread; no monitor after the
          // last thread since the frame wraps and the monitor runs first next frame.
          var monitorInterleavedScheds: ISZ[SchedulingDomain] = ISZ()
          for (s <- regularThreadSlots) {
            monitorInterleavedScheds = monitorInterleavedScheds :+ monitorSlot :+ s
          }
          var monitorUsedNano: Z = 0
          for (s <- monitorInterleavedScheds) {
            monitorUsedNano = monitorUsedNano + s.length
          }
          if (monitorUsedNano > framePeriodNano) {
            val overrunMs = (monitorUsedNano - framePeriodNano) / 1_000_000
            reporter.warn(None(), name,
              s"The inclusion of the runtime monitor extends the frame schedule by ${overrunMs} ms beyond the configured ${framePeriodMs} ms frame period. Consider increasing the frame period to accommodate monitor execution.")
          }
          val monitorRemainder: Z = framePeriodNano - monitorUsedNano
          val monitorScheds: ISZ[SchedulingDomain] =
            if (monitorRemainder > 0)
              SchedulingDomain(id = 0, componentName = "pad", length = monitorRemainder, isUserPartition = F) +: monitorInterleavedScheds
            else monitorInterleavedScheds

          // Add read-only mappings of the sched_state_mr and sched_schedule_mr regions
          // to the monitor PD.  The virtual addresses must match what CComponentPlugin_MCS
          // assigned to each port's queue variable, since the C code accesses the
          // scheduler data through those addresses.
          val monitorPdName: String = s"${MonitorInjector.monitorProcessId}_${MonitorInjector.monitorThreadId}"
          val schedStatePortPrefix: String = s"${MonitorInjector.schedStatePortName}_"
          val schedSchedulePortPrefix: String = s"${MonitorInjector.schedSchedulePortName}_"
          var schedStateVaddrOpt: Option[Z] = None()
          var schedScheduleVaddrOpt: Option[Z] = None()
          for (pd <- rawSd.protectionDomains) {
            if (pd.name == monitorPdName) {
              for (m <- pd.memMaps) {
                if (m.varAddr.nonEmpty && ops.StringOps(m.varAddr.get).startsWith(schedStatePortPrefix)) {
                  schedStateVaddrOpt = Some(m.vaddrInKiBytes)
                }
                if (m.varAddr.nonEmpty && ops.StringOps(m.varAddr.get).startsWith(schedSchedulePortPrefix)) {
                  schedScheduleVaddrOpt = Some(m.vaddrInKiBytes)
                }
              }
            }
          }
          if (schedStateVaddrOpt.isEmpty) {
            halt("Infeasible: sched_state MemoryMap not found in monitor PD")
          }
          if (schedScheduleVaddrOpt.isEmpty) {
            halt("Infeasible: sched_schedule MemoryMap not found in monitor PD")
          }
          val schedStateMap = MemoryMap(
            memoryRegion = "sched_state_mr",
            vaddrInKiBytes = schedStateVaddrOpt.get,
            perms = ISZ(Perm.READ),
            varAddr = None(),
            cached = None())
          val schedScheduleMap = MemoryMap(
            memoryRegion = "sched_schedule_mr",
            vaddrInKiBytes = schedScheduleVaddrOpt.get,
            perms = ISZ(Perm.READ),
            varAddr = None(),
            cached = None())
          val monitorPdsWithSchedMaps: ISZ[ProtectionDomain] = for (pd <- rawSd.protectionDomains) yield
            if (pd.name == monitorPdName) pd(memMaps = pd.memMaps :+ schedStateMap :+ schedScheduleMap)
            else pd

          localStore = SystemDescriptionProviderPlugin.putMSD("monitor", SystemDescription(
            name = "monitor",
            schedulingDomains = monitorScheds,
            protectionDomains = monitorPdsWithSchedMaps,
            memoryRegions = rawSd.memoryRegions,
            channels = rawSd.channels), localStore)



          // Annotate the monitor's ImplBase with a comment mapping each channel ID to its
          // model-level PD, so the Rust developer knows what channel to expect in notify().
          val channelComment = RAST.CommentRustDoc(ISZ(st"Channel Assignments", st"-------------------") ++
            (for(p <- ops.ISZOps(rawSd.schedulingDomains).sortWith((a: SchedulingDomain, b: SchedulingDomain) => a.id < b.id)) yield st"${p.id} - ${p.componentName}"))

          val monitorThreadPath: IdPath = symbolTable.rootSystem.path :+ MonitorInjector.monitorProcessId :+ MonitorInjector.monitorThreadId

          val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
          contributions.componentContributions.get(monitorThreadPath) match {
            case Some(monitorContrib) =>
              monitorContrib.appStructImpl match {
                case impl: RAST.ImplBase =>
                  val updatedContrib = monitorContrib(appStructImpl = impl(comments = ISZ(channelComment)))
                  localStore = CRustComponentPlugin.putComponentContributions(
                    contributions.replaceComponentContributions(
                      contributions.componentContributions + monitorThreadPath ~> updatedContrib),
                    localStore)
                case _ =>
              }
            case _ =>
          }
        }
      case _ =>
    }

    return (localStore, resources)
  }
}

@datatype class DefaultRustModelTransformerPlugin extends RustModelTransformerPlugin {

  val name: String = "DefaultRustModelTransformerPlugin"

}
