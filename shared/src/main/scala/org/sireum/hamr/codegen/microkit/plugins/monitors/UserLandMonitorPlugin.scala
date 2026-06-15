// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.util.{GenericMemoryRegion, MemoryMap, MemoryRegion, MicrokitDomain, MicrokitUtil, Perm, PortSharedMemoryRegion, ProtectionDomain, SchedulingDomain, SystemDescription, VirtualMachine}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RAST}

@datatype class SDValue(val sd: SystemDescription) extends StoreValue

object UserLandMonitorPlugin {

  val KEY_UserLandMonitorPlugin_Model_Transformed: String = "KEY_UserLandMonitorPlugin_Model_Transformed"
  val KEY_UserLandMonitorPlugin_handled: String = "KEY_RustScheduleAwareMonitorPlugin_handled"
  val MONITOR_ORIG_MSD_KEY: String = "MONITOR_ORIG_MSD"

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_UserLandMonitorPlugin_Model_Transformed)

  @strictpure def hasHandled(store: Store): B = store.contains(KEY_UserLandMonitorPlugin_handled)
}

@sig trait UserLandMonitorPlugin
  extends ModelTransformerPlugin
    with MicrokitPlugin {

  @strictpure def getMonitorName: String = "userland_monitor"

  // Monitor PD process/thread paths derived from an explicit monitor name. The
  // no-arg getMonitorProcessPath/getMonitorThreadPath below delegate using
  // getMonitorName (one monitor per plugin instance). The name-parameterized
  // forms let a single plugin emit several monitors — e.g. the sys-assert
  // monitor, which produces one per composition (design D8): the caller passes
  // the per-composition monitor name (sys_assert_<id>_monitor).
  @strictpure def monitorProcessPathNamed(root: IdPath, monitorName: String): IdPath =
    root :+ s"${monitorName}_process"

  @strictpure def monitorThreadPathNamed(root: IdPath, monitorName: String): IdPath =
    monitorProcessPathNamed(root, monitorName) :+ s"${monitorName}_thread"

  @strictpure def getMonitorProcessPath(root: IdPath): IdPath = monitorProcessPathNamed(root, getMonitorName)

  @strictpure def getMonitorThreadPath(root: IdPath): IdPath = monitorThreadPathNamed(root, getMonitorName)

  // The monitor names this plugin instance emits, in order. Default: a single
  // monitor named getMonitorName. The sys-assert monitor overrides this to
  // return one name per composition (sys_assert_<id>_monitor), so each phase
  // loops over the result to emit N monitor components (design D8, approach
  // (i)). All non-sys-assert monitors keep the singleton behavior (size-1 loop).
  @strictpure def monitorNames(symbolTable: SymbolTable): ISZ[String] = ISZ(getMonitorName)

  @strictpure def getRetainedNonModelPorts(store: Store): ISZ[IdPath] = ISZ()

  @pure def canHandleModelTransformHelper(model: Aadl,
                                                options: HamrCli.CodegenOption,
                                                types: AadlTypes,
                                                symbolTable: SymbolTable,
                                                store: Store, reporter: Reporter): B = {
      return (
        options.platform == CodegenHamrPlatform.Microkit &&
          !isDisabled(store) &&
          !reporter.hasError &&
          options.runtimeMonitoring &&
          MicrokitUtil.isMCS(options, symbolTable.rootSystem))
  }

  @pure override def canHandleModelTransform(model: Aadl,
                                             options: HamrCli.CodegenOption,
                                             types: AadlTypes,
                                             symbolTable: SymbolTable,
                                             store: Store, reporter: Reporter): B = {
    return (
      canHandleModelTransformHelper(model, options, types, symbolTable, store, reporter) &&
        !UserLandMonitorPlugin.haveHandledModelTransform(store))
  }

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    val localStore = store + UserLandMonitorPlugin.KEY_UserLandMonitorPlugin_Model_Transformed ~> BoolValue(T)
    val sysPath = model.components(0).identifier.name
    return injectMonitorPDNamed(model, getMonitorProcessPath(sysPath), getMonitorThreadPath(sysPath),
      options, symbolTable, localStore, reporter)
  }

  // Injects a single monitor PD at the given process/thread paths, re-resolves
  // the model, and records the PD as a non-model element. Factored out of
  // handleModelTransform so a plugin that emits several monitors (the sys-assert
  // monitor: one per composition, design D8 approach (i)) can inject each in
  // turn, threading the returned model/symbolTable into the next injection.
  def injectMonitorPDNamed(model: Aadl,
                           monitorProcessorPath: IdPath,
                           monitorThreadPath: IdPath,
                           options: HamrCli.CodegenOption,
                           symbolTable: SymbolTable,
                           store: Store,
                           reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store

    val s = UserLandMonitorInjector().inject(
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


  @pure def canHandleHelper(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      options.platform == CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        options.runtimeMonitoring &&
        SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
        CRustComponentPlugin.hasCRustComponentContributions(store))
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      canHandleHelper(model, options, types, symbolTable, store, reporter) &&
        UserLandMonitorPlugin.haveHandledModelTransform(store) &&
        !UserLandMonitorPlugin.hasHandled(store))
  }

  // Each monitor plugin's handle method produces a system description variant
  // by filtering the "normal" SD — stripping non-model memory regions (like
  // sv_ state var regions) that aren't retained, and adding monitor-specific
  // scheduling slots. The first monitor to run writes its filtered version
  // back as "normal", which removes the sv_ memory regions from it. If a
  // second monitor then reads "normal", those regions are already gone and
  // its SD variant will be missing the state var memory mappings.
  //
  // To fix this, the first monitor to call getOriginalMsd snapshots the
  // unmodified "normal" SD under a separate store key (not in the MSDs map,
  // to avoid the finalize phase rendering it as a duplicate meta.py).
  // All subsequent monitors read from that snapshot, ensuring every monitor
  // starts from the same baseline that includes all memory regions.
  @pure def getOriginalMsd(store: Store): (SystemDescription, Store) = {
    store.get(UserLandMonitorPlugin.MONITOR_ORIG_MSD_KEY) match {
      case Some(v) => return (v.asInstanceOf[SDValue].sd, store)
      case _ => {
        val normal = SystemDescriptionProviderPlugin.getMSD("normal", store)
        val ustore = store + UserLandMonitorPlugin.MONITOR_ORIG_MSD_KEY ~> SDValue(normal)
        return (normal, ustore)
      }
    }
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + UserLandMonitorPlugin.KEY_UserLandMonitorPlugin_handled ~> BoolValue(T)
    var resources: ISZ[Resource] = ISZ()
    // One MSD/scheduler/.mk bundle per monitor: size-1 for the gumbo and userland
    // monitors, one per composition for the sys-assert monitor (design D8,
    // approach (i)). Each monitor's MSD strips the other monitors' PDs, so it
    // includes only its own; the user selects one via CONFIG=<bundle>.mk.
    for (monitorName <- monitorNames(symbolTable)) {
      val r = handleForMonitor(model, options, symbolTable, monitorName, localStore, reporter)
      localStore = r._1
      resources = resources ++ r._2
    }
    return (localStore, resources)
  }

  def handleForMonitor(model: Aadl, options: HamrCli.CodegenOption, symbolTable: SymbolTable,
                       monitorName: String, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    // Use the original unmodified "normal" SD so that memory regions
    // removed by a prior monitor's filtering are still available
    val (rawSd, s) = getOriginalMsd(localStore)
    localStore = s

    val sysPath = model.components(0).identifier.name
    val monitorProcessorPath = monitorProcessPathNamed(sysPath, monitorName)
    val monitorThreadPath = monitorThreadPathNamed(sysPath, monitorName)

    val monitorThreadId = s"${monitorProcessorPath(monitorProcessorPath.lastIndex)}_${monitorThreadPath(monitorThreadPath.lastIndex)}"
    val monitorMonPdName = s"${monitorThreadId}_MON"

    var otherNonModelPdNames: Set[String] = Set.empty
    for (id <- StoreUtil.getNonModelElements(localStore)) {
      val pdName = st"${(ops.ISZOps(id).drop(1), "_")}".render
      if (pdName != monitorThreadId) {
        otherNonModelPdNames = otherNonModelPdNames + pdName + s"${pdName}_MON"
      }
    }

    var regularSlots: ISZ[SchedulingDomain] = ISZ()
    var monitorSlotOpt: Option[SchedulingDomain] = None()
    for (sd <- rawSd.schedulingDomains) {
      if (sd.componentName == monitorMonPdName) {
        // The monitor is not a user partition — it should not be preemptible by the scheduler
        // in the same way as component partitions.
        monitorSlotOpt = Some(sd(isUserPartition = F))
      } else if (sd.componentName != "padding") {
        regularSlots = regularSlots :+ sd
      }
    }

    monitorSlotOpt match {
      case Some(monitorSlot) =>
        // Strip ALL non-model PDs and their channels from the "normal" SD.
        // Each monitor reads from the original snapshot (MONITOR_ORIG_MSD),
        // so every monitor must strip all non-model elements — not just itself —
        // to avoid restoring items that a prior monitor already removed.
        val allNonModelPdNames: Set[String] = otherNonModelPdNames + monitorMonPdName + monitorThreadId
        val strippedPDs = rawSd.protectionDomains.filter(pd => !allNonModelPdNames.contains(pd.name))
        val strippedChannels = rawSd.channels.filter(c =>
          !allNonModelPdNames.contains(c.firstPD) && !allNonModelPdNames.contains(c.secondPD))

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

        // For the normal variant, also exclude other non-model PDs' scheduling slots
        val normalThreadSlots: ISZ[SchedulingDomain] = regularThreadSlots.filter((sd: SchedulingDomain) => !otherNonModelPdNames.contains(sd.componentName))

        // "normal" SD: [pad?,] regular threads (no monitor); pad uses full remaining budget
        var normalUsedNano: Z = 0
        for (s <- normalThreadSlots) {
          normalUsedNano = normalUsedNano + s.length
        }
        val normalRemainder: Z = framePeriodNano - normalUsedNano
        val normalScheds: ISZ[SchedulingDomain] =
          if (normalRemainder > 0)
            SchedulingDomain(id = 0, componentName = "pad", length = normalRemainder, isUserPartition = F) +: normalThreadSlots
          else normalThreadSlots

        val retainedPorts: ISZ[IdPath] = getRetainedNonModelPorts(localStore)
        var nonModelMrNames: Set[String] = Set.empty
        var retainedMrNames: Set[String] = Set.empty
        var mrSizes: Map[String, Z] = Map.empty
        for (mr <- rawSd.memoryRegions) {
          mrSizes = mrSizes + mr.name ~> mr.sizeInKiBytes
          mr match {
            case p: PortSharedMemoryRegion if StoreUtil.isNonModelElement(p.outgoingPortPath, localStore) =>
              nonModelMrNames = nonModelMrNames + p.name
              if (ops.ISZOps(retainedPorts).contains(p.outgoingPortPath)) {
                retainedMrNames = retainedMrNames + p.name
              }
            case _ =>
          }
        }
        val excludedMrNames: Set[String] = nonModelMrNames -- retainedMrNames.elements

        val normalMemoryRegions: ISZ[MemoryRegion] = rawSd.memoryRegions.filter((mr: MemoryRegion) =>
          !retainedMrNames.contains(mr.name))

        def compactMemMapsMCS(maps: ISZ[MemoryMap], names: Set[String], sizes: Map[String, Z]): ISZ[MemoryMap] = {
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

        def filterDomainMemMapsMCS(d: MicrokitDomain, names: Set[String], sizes: Map[String, Z]): MicrokitDomain = {
          d match {
            case pd: ProtectionDomain =>
              val fc: ISZ[MicrokitDomain] = for (c <- pd.children) yield filterDomainMemMapsMCS(c, names, sizes)
              return pd(
                memMaps = compactMemMapsMCS(pd.memMaps, names, sizes),
                children = fc)
            case vm: VirtualMachine =>
              return vm(memMaps = compactMemMapsMCS(vm.memMaps, names, sizes))
            case x =>
              return x
          }
        }

        val normalPDs: ISZ[ProtectionDomain] = for (pd <- strippedPDs) yield
          filterDomainMemMapsMCS(pd, retainedMrNames, mrSizes).asInstanceOf[ProtectionDomain]

        localStore = SystemDescriptionProviderPlugin.putMSD("normal", SystemDescription(
          name = "normal",
          schedulingDomains = normalScheds,
          protectionDomains = normalPDs,
          memoryRegions = normalMemoryRegions,
          channels = strippedChannels,
          templateContributions = ISZ()), localStore)

        // "monitor" SD: [pad?,] monitor before each thread; no monitor after the
        // last thread since the frame wraps and the monitor runs first next frame.
        val monitorVariantThreadSlots = regularThreadSlots.filter(sd => !otherNonModelPdNames.contains(sd.componentName))
        var monitorInterleavedScheds: ISZ[SchedulingDomain] = ISZ()
        for (s <- monitorVariantThreadSlots) {
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

        // Add read-only mappings of the sched_state and sched_schedule regions
        // to the monitor PD.  The virtual addresses and varAddr names must match
        // what CComponentPlugin_MCS assigned to each port's queue variable, since
        // the C code accesses the scheduler data through those addresses.
        //
        // The GenericMemoryRegion names and MemoryMap.memoryRegion values use the
        // actual MR name that sdfgen writes into the XML mr= attribute (e.g.
        // "sched_state"), NOT the Python variable name.  This is critical because
        // the setvar_mappings dict keys must match the mr= attribute in the XML
        // for the add_setvar_vaddr post-processing to apply the setvar_vaddr
        // attribute.
        val schedStatePortPrefix: String = s"${UserLandMonitorInjector.schedStatePortName}_"
        val schedSchedulePortPrefix: String = s"${UserLandMonitorInjector.schedSchedulePortName}_"
        var schedStateVaddrOpt: Option[Z] = None()
        var schedScheduleVaddrOpt: Option[Z] = None()
        var schedStateVarAddrOpt: Option[String] = None()
        var schedScheduleVarAddrOpt: Option[String] = None()
        def searchPdMemMaps(pds: ISZ[ProtectionDomain]): Unit = {
          for (pd <- pds) {
            if (pd.name == monitorThreadId) {
              for (m <- pd.memMaps) {
                if (m.varAddr.nonEmpty && ops.StringOps(m.varAddr.get).startsWith(schedStatePortPrefix)) {
                  schedStateVaddrOpt = Some(m.vaddrInKiBytes)
                  schedStateVarAddrOpt = m.varAddr
                }
                if (m.varAddr.nonEmpty && ops.StringOps(m.varAddr.get).startsWith(schedSchedulePortPrefix)) {
                  schedScheduleVaddrOpt = Some(m.vaddrInKiBytes)
                  schedScheduleVarAddrOpt = m.varAddr
                }
              }
            }
            for (child <- pd.children) {
              child match {
                case childPd: ProtectionDomain => searchPdMemMaps(ISZ(childPd))
                case _ =>
              }
            }
          }
          return
        }
        searchPdMemMaps(rawSd.protectionDomains)
        if (schedStateVaddrOpt.isEmpty) {
          halt("Infeasible: sched_state MemoryMap not found in monitor PD")
        }
        if (schedScheduleVaddrOpt.isEmpty) {
          halt("Infeasible: sched_schedule MemoryMap not found in monitor PD")
        }
        val schedStateMap = MemoryMap(
          memoryRegion = "sched_state",
          vaddrInKiBytes = schedStateVaddrOpt.get,
          perms = ISZ(Perm.READ),
          varAddr = schedStateVarAddrOpt,
          cached = None())
        val schedScheduleMap = MemoryMap(
          memoryRegion = "sched_schedule",
          vaddrInKiBytes = schedScheduleVaddrOpt.get,
          perms = ISZ(Perm.READ),
          varAddr = schedScheduleVarAddrOpt,
          cached = None())
        val monitorVariantPds = rawSd.protectionDomains.filter(pd => !otherNonModelPdNames.contains(pd.name))
        val monitorVariantPdsCompacted: ISZ[ProtectionDomain] = for (pd <- monitorVariantPds) yield
          filterDomainMemMapsMCS(pd, excludedMrNames, mrSizes).asInstanceOf[ProtectionDomain]
        def addSchedMapsToChild(c: MicrokitDomain): MicrokitDomain = {
          c match {
            case childPd: ProtectionDomain => return addSchedMaps(childPd)
            case other => return other
          }
        }
        def addSchedMaps(pd: ProtectionDomain): ProtectionDomain = {
          if (pd.name == monitorThreadId) {
            return pd(memMaps = pd.memMaps.filter(m =>
              !(m.varAddr.nonEmpty && (
                ops.StringOps(m.varAddr.get).startsWith(schedStatePortPrefix) ||
                  ops.StringOps(m.varAddr.get).startsWith(schedSchedulePortPrefix)))) :+ schedStateMap :+ schedScheduleMap)
          } else {
            val updatedChildren: ISZ[MicrokitDomain] = for (c <- pd.children) yield addSchedMapsToChild(c)
            return pd(children = updatedChildren)
          }
        }
        val monitorPdsWithSchedMaps: ISZ[ProtectionDomain] = for (pd <- monitorVariantPdsCompacted) yield
          addSchedMaps(pd)

        val schedTemplateContributions: ISZ[ST] = ISZ(
          st"""#######################################
              |# SCHEDULE STATE
              |# Broadcast region written by the scheduler before every dispatch.
              |# The runtime monitor maps this region read-only to observe which
              |# protection domain last yielded and which will be dispatched next.
              |# Must match SCHED_STATE_VADDR / SCHED_STATE_SIZE in scheduler_config.h.
              |#######################################
              |SCHED_STATE_VADDR = 0x4_000_000
              |SCHED_STATE_SIZE  = 0x1000  # 4 KB
              |sched_state = MemoryRegion(sdf, "sched_state", SCHED_STATE_SIZE)
              |sdf.add_mr(sched_state)
              |scheduler.add_map(Map(sched_state, SCHED_STATE_VADDR, perms="rw"))
              |
              |#######################################
              |# SCHEDULE
              |# The full user_schedule published by the scheduler at init.
              |# Monitors that map this region read-only can correlate
              |# current_timeslice indices with channel IDs and durations.
              |# Must match SCHED_SCHEDULE_VADDR / SCHED_SCHEDULE_SIZE in scheduler_config.h.
              |#######################################
              |SCHED_SCHEDULE_VADDR = 0x4_001_000
              |SCHED_SCHEDULE_SIZE  = 0x1000  # 4 KB
              |sched_schedule = MemoryRegion(sdf, "sched_schedule", SCHED_SCHEDULE_SIZE)
              |sdf.add_mr(sched_schedule)
              |scheduler.add_map(Map(sched_schedule, SCHED_SCHEDULE_VADDR, perms="rw"))""")

        val monitorMemoryRegions: ISZ[MemoryRegion] = rawSd.memoryRegions.filter((mr: MemoryRegion) =>
          !excludedMrNames.contains(mr.name)) :+
          GenericMemoryRegion(name = "sched_state", sizeInKiBytes = 4) :+
          GenericMemoryRegion(name = "sched_schedule", sizeInKiBytes = 4)

        localStore = SystemDescriptionProviderPlugin.putMSD(monitorName, SystemDescription(
          name = monitorName,
          schedulingDomains = monitorScheds,
          protectionDomains = monitorPdsWithSchedMaps,
          memoryRegions = monitorMemoryRegions,
          channels = rawSd.channels.filter(c =>
            !otherNonModelPdNames.contains(c.firstPD) && !otherNonModelPdNames.contains(c.secondPD)),
          templateContributions = schedTemplateContributions), localStore)


        // Generate channel assignment constants so the monitor behavior code can
        // match on channel IDs by name rather than raw numeric values.
        val channelComment: RAST.Item = RAST.ItemST(st"// Schedule channel IDs used to identify which thread yielded or runs next")
        val filteredSchedDomains = rawSd.schedulingDomains.filter(sd => !otherNonModelPdNames.contains(sd.componentName))
        val channelConsts: ISZ[RAST.Item] =
          for (p <- ops.ISZOps(filteredSchedDomains).sortWith((a: SchedulingDomain, b: SchedulingDomain) => a.id < b.id))
            yield RAST.ItemST(st"pub const ${p.componentName}: u32 = ${p.id};").asInstanceOf[RAST.Item]
        val channelConstants: ISZ[RAST.Item] = ISZ[RAST.Item](channelComment) ++ channelConsts

        val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
        contributions.componentContributions.get(monitorThreadPath) match {
          case Some(monitorContrib) =>
            val updatedContrib = monitorContrib(moduleLevelEntries = monitorContrib.moduleLevelEntries ++ channelConstants)
            localStore = CRustComponentPlugin.putComponentContributions(
              contributions.replaceComponentContributions(
                contributions.componentContributions + monitorThreadPath ~> updatedContrib),
              localStore)
            val schedulerPath = s"${options.sel4OutputDir.get}/scheduler"

            resources = resources :+ ResourceUtil.createResourceH(
              path = s"$schedulerPath/src/$monitorName.scheduler.c",
              content = StaticContent.monitor_scheduler_c(monitorName),
              overwrite = T, isDatatype = F)

            resources = resources :+ ResourceUtil.createResourceH(
              path = s"$schedulerPath/include/$monitorName.scheduler_config.h",
              content = StaticContent.monitor_scheduler_config_h,
              overwrite = T, isDatatype = F)

            resources = resources :+ ResourceUtil.createResourceH(
              path = s"$schedulerPath/include/$monitorName.user_config.h",
              content = StaticContent.monitor_user_config_h(monitorName),
              overwrite = T, isDatatype = F)

            val monitorMkContent: ST =
              st"""${CommentTemplate.doNotEditComment_hash}
                  |
                  |# Monitor configuration for runtime monitoring scheduler variant.
                  |# Usage: make CONFIG=monitor.mk
                  |export MSD := $$(TOP_DIR)/$monitorName.meta.py
                  |export SCHEDULER_C := $$(TOP_DIR)/scheduler/src/$monitorName.scheduler.c
                  |export SCHEDULER_CONFIG_HEADERS := $$(TOP_DIR)/scheduler/include/$monitorName.user_config.h"""

            resources = resources :+ ResourceUtil.createResourceH(
              path = s"${options.sel4OutputDir.get}/${monitorName}.mk",
              content = monitorMkContent,
              overwrite = T, isDatatype = F)
          case _ =>
            reporter.error(None(), toolName, s"Injected monitor contributions for ${monitorName} was not found")
        }
      case _ =>
        reporter.error(None(), toolName, s"Injected monitor component '${monitorName}' was not found")
    }

    return (localStore, resources)
  }
}

@datatype class DefaultUserLandMonitorPlugin extends UserLandMonitorPlugin {

  val name: String = "DefaultUserLandMonitorPlugin"

}

object StaticContent {

  @pure def monitor_scheduler_c(monitorName: String): ST = {
    return (
      st"""#include <stdint.h>
        |#include <stdbool.h>
        |
        |#include <microkit.h>
        |#include <sel4/sel4.h>
        |#include <os/sddf.h>
        |#include <sddf/timer/client.h>
        |#include <sddf/timer/config.h>
        |#include <sddf/util/printf.h>
        |
        |#include <$monitorName.scheduler_config.h>
        |#include <$monitorName.user_config.h>
        |
        |#include <sb_types.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |/* Number of nanoseconds in a second */
        |#define NS_IN_S  1000000000ULL
        |
        |__attribute__((__section__(".timer_client_config"))) timer_client_config_t config;
        |__attribute__((__section__(".user_schedule"))) user_schedule_t user_schedule;
        |
        |uint32_t current_timeslice;
        |
        |// Bitstring for partition ready status. 0 = not ready, 1 = ready.
        |uint64_t part_ready;
        |uint64_t part_ready_check;
        |
        |bool scheduler_running;
        |
        |// Pointer to the schedule state shared memory region.
        |// Monitors that map this region can read current_timeslice.
        |volatile sb_queue_hamr_SchedState_1_t *sb_queue_sched_state = (volatile sb_queue_hamr_SchedState_1_t *)SCHED_STATE_VADDR;
        |
        |hamr_SchedState sched_state = {0};
        |
        |bool put_sched_state() {
        |  sb_queue_hamr_SchedState_1_enqueue((sb_queue_hamr_SchedState_1_t *) sb_queue_sched_state, (hamr_SchedState *) &sched_state);
        |
        |  return true;
        |}
        |
        |// Pointer to the schedule shared memory region.
        |// Monitors that map this region can read the full schedule (all channels, timeslices, user-partition flags).
        |volatile sb_queue_hamr_Schedule_1_t *sb_queue_sched_schedule = (volatile sb_queue_hamr_Schedule_1_t *)SCHED_SCHEDULE_VADDR;
        |
        |hamr_Schedule sched_schedule = {0};
        |
        |bool put_sched_schedule() {
        |  sb_queue_hamr_Schedule_1_enqueue((sb_queue_hamr_Schedule_1_t *) sb_queue_sched_schedule, (hamr_Schedule *) &sched_schedule);
        |
        |  return true;
        |}
        |
        |void notify() {
        |    microkit_channel ch = user_schedule.timeslice_ch[current_timeslice];
        |    if (ch != 0) { // channel 0 is used to pad out a schedule
        |        sched_state.current_timeslice = current_timeslice;
        |        put_sched_state();
        |
        |        microkit_notify(ch);
        |    }
        |    // Set a timeout for the length of this partition's timeslice
        |    sddf_timer_set_timeout(config.driver_id, user_schedule.timeslices[current_timeslice]);
        |}
        |
        |void next_partition() {
        |    current_timeslice = (current_timeslice + 1) % user_schedule.num_timeslices;
        |    notify();
        |}
        |
        |void notified(microkit_channel ch)
        |{
        |    if (ch == config.driver_id) {
        |        // Timer driver fired — either the initial timeout after all partitions reported
        |        // ready (starts the schedule) or a periodic tick (advances to the next timeslice).
        |        if (scheduler_running == false) {
        |            notify();
        |            scheduler_running = true;
        |        } else {
        |            next_partition();
        |        }
        |    } else if ((part_ready_check & (1 << ch)) != 0) {
        |        // Channel not yet marked ready — this is the partition's first notification
        |        // during the initialisation handshake (before the schedule starts running).
        |        if ((part_ready & (1 << ch)) == 0) {
        |            sddf_dprintf("SCHEDULER | Marking partition %d as ready\n", ch);
        |            part_ready |= (1 << ch);  // Set this channel's bit in the readiness bitstring
        |            if (part_ready == part_ready_check) {  // All expected partitions have reported ready
        |                sddf_dprintf("SCHEDULER | All partitions ready, beginning schedule\n");
        |                // Timeout to let the last partition become passive before starting
        |                sddf_timer_set_timeout(config.driver_id, NS_IN_S);
        |            }
        |        }
        |        // Runtime signals from partitions are ignored: the schedule is static and each
        |        // partition runs for its full allotted time regardless of early completion.
        |    } else {
        |        sddf_dprintf("SCHEDULER |received unknown notification on channel: %d\n", ch);
        |    }
        |}
        |
        |void init(void)
        |{
        |    sb_queue_hamr_SchedState_1_init((sb_queue_hamr_SchedState_1_t *) sb_queue_sched_state);
        |    sb_queue_hamr_Schedule_1_init((sb_queue_hamr_Schedule_1_t *) sb_queue_sched_schedule);
        |
        |    current_timeslice = 0;
        |
        |    scheduler_running = false;
        |
        |    part_ready |= (1 << 0); // ch 0 is always 'ready'
        |    part_ready_check |= (1 << 0); // ch 0 is always 'ready' -- keeps padding optional
        |
        |    // Build a bitmask of all channels that must report ready before the schedule
        |    // can start. Each bit position corresponds to a channel ID. During the
        |    // initialisation handshake, partitions notify on their channel; part_ready
        |    // accumulates those bits and is compared against this mask to detect when
        |    // every scheduled partition has checked in.
        |    for (int i = 0; i < user_schedule.num_timeslices; i++) {
        |        part_ready_check |= (1 << user_schedule.timeslice_ch[i]);
        |    }
        |
        |    // Publish the full schedule so monitors can correlate timeslice indices with channels.
        |    sched_schedule.num_timeslices = user_schedule.num_timeslices;
        |    for (uint32_t i = 0; i < user_schedule.num_timeslices; i++) {
        |        sched_schedule.timeslices[i] = user_schedule.timeslices[i];
        |        sched_schedule.timeslice_ch[i] = user_schedule.timeslice_ch[i];
        |        sched_schedule.is_user_partition[i] = user_schedule.is_user_partition[i];
        |    }
        |    put_sched_schedule();
        |}
        |""")
  }

  @pure def monitor_user_config_h(monitorName: String): ST = {
    return (
      st"""#pragma once
        |
        |#include <stdbool.h>
        |#include <stdint.h>
        |#include <$monitorName.scheduler_config.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// The metaprogram will omit a binary with the same format as this struct,
        |// and will be patched into the scheduler at system build time
        |
        |typedef struct user_schedule {
        |    uint64_t timeslices[MAX_SCHEDULE_SLOTS];
        |    // @kwinter: In the future, will also need to keep track of
        |    // all the PD's in a partition so that we can easily suspend them
        |    uint32_t timeslice_ch[MAX_SCHEDULE_SLOTS];
        |    bool is_user_partition[MAX_SCHEDULE_SLOTS];
        |    uint32_t num_timeslices;
        |} user_schedule_t;""")
  }

  val monitor_scheduler_config_h: ST =
    st"""#pragma once
        |
        |#include <microkit.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// @kwinter: This is a first iteration of this config file. We will move it to the
        |// metaprogram after
        |
        |// The max partitions is limited by the number of channels that we can establish
        |// between the scheduler and a partition's initial process in microkit.
        |// One channel is taken by the sDDF timer subsystem.
        |#define MAX_PARTITIONS (MICROKIT_MAX_CHANNELS - 1)
        |
        |// Maximum number of timeslice slots in a schedule.  A thread may appear
        |// in multiple slots per frame period, so this can exceed MAX_PARTITIONS.
        |// Must fit within the 4 KB shared-memory page (struct ≈ 13*N + 4 bytes).
        |#define MAX_SCHEDULE_SLOTS 128
        |
        |// Virtual address at which the schedule state shared memory region is mapped.
        |// Must match SCHED_STATE_VADDR in meta.py. Change both if there is a conflict.
        |#define SCHED_STATE_VADDR 0x4000000UL
        |#define SCHED_STATE_SIZE  0x1000UL  // 4 KB
        |
        |// Virtual address at which the schedule shared memory region is mapped.
        |// Must match SCHED_SCHEDULE_VADDR in meta.py. Change both if there is a conflict.
        |#define SCHED_SCHEDULE_VADDR 0x4001000UL
        |#define SCHED_SCHEDULE_SIZE  0x1000UL  // 4 KB
        |
        |// Schedule state broadcast: written by the scheduler before every dispatch.
        |// Monitors that map this region (read-only) can inspect it to determine
        |// the current timeslice index.
        |typedef struct sched_state {
        |    uint32_t current_timeslice; // index into the schedule of the current timeslice
        |} sched_state_t;
        |
        |typedef struct schedule_config {
        |    uint32_t num_partitions;
        |    // Currently this is in NS
        |    uint64_t timeslices[MAX_PARTITIONS];
        |    microkit_channel partition_initial_pd[MAX_PARTITIONS];
        |} schedule_config_t;
        |"""
}