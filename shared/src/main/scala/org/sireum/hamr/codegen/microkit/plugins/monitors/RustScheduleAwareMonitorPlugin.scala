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
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.monitors.RustScheduleAwareMonitorPlugin.{KEY_RustScheduleAwareMonitorPlugin, KEY_RustScheduleAwareMonitorPlugin_sched_mod}
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.util.{GenericMemoryRegion, MemoryMap, MemoryRegion, MicrokitDomain, MicrokitUtil, Perm, PortSharedMemoryRegion, ProtectionDomain, SchedulingDomain, SystemDescription, VirtualMachine}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RAST}

object RustScheduleAwareMonitorPlugin {

  val KEY_RustScheduleAwareMonitorPlugin: String = "KEY_RustScheduleAwareMonitorPlugin"

  val KEY_RustScheduleAwareMonitorPlugin_sched_mod: String = "KEY_RustScheduleAwareMonitorPlugin_sched_mod"

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_RustScheduleAwareMonitorPlugin)

  @strictpure def hasHandled(store: Store): B = store.contains(KEY_RustScheduleAwareMonitorPlugin_sched_mod)

  val monitor_scheduler_c: ST =
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
        |#include <monitor.scheduler_config.h>
        |#include <monitor.user_config.h>
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
        |"""

  val monitor_user_config_h: ST =
    st"""#pragma once
        |
        |#include <stdbool.h>
        |#include <stdint.h>
        |#include <monitor.scheduler_config.h>
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
        |} user_schedule_t;"""

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

@sig trait RustScheduleAwareMonitorPlugin
  extends ModelTransformerPlugin
  with MicrokitPlugin {

  @pure override def canHandleModelTransform(model: Aadl,
                                              options: HamrCli.CodegenOption,
                                              types: AadlTypes,
                                              symbolTable: SymbolTable,
                                              store: Store, reporter: Reporter): B = {
    return options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !RustScheduleAwareMonitorPlugin.haveHandledModelTransform(store) &&
      !reporter.hasError &&
      (options.runtimeMonitoring || MicrokitUtil.isMCS(options, symbolTable.rootSystem))
  }
      // && GumboXRustUtil.hasGumbo(model, symbolTable)

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store + KEY_RustScheduleAwareMonitorPlugin ~> BoolValue(T)

    val mcs: B = MicrokitUtil.isMCS(options, symbolTable.rootSystem)

    val rmodel: Aadl =
      if (options.runtimeMonitoring) {
        val s = MonitorInjector.inject(model, symbolTable, mcs, localStore, reporter)
        localStore = s._2
        s._1
      } else if (mcs) {
        val s = MonitorInjector.injectMCSTypes(model.dataComponents, localStore)
        localStore = s._2
        model(dataComponents = s._1)
      } else {
        return None()
      }

    if (!reporter.hasError) {
      val reResult = ModelUtil.resolve(rmodel, rmodel.components(0).identifier.pos, "", options, localStore, reporter)
      localStore = reResult._2
      if (reResult._1.nonEmpty) {
        if (options.runtimeMonitoring) {
          val monitorThreadPath = rmodel.components(0).identifier.name :+ MonitorInjector.monitorProcessId :+ MonitorInjector.monitorThreadId
          localStore = StoreUtil.addNonModelElement(monitorThreadPath, localStore)
        }
        return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
      }
    }
    return None()
  }






  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return !reporter.hasError &&
      options.runtimeMonitoring &&
      RustScheduleAwareMonitorPlugin.haveHandledModelTransform(store) &&
      SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
      !RustScheduleAwareMonitorPlugin.hasHandled(store) &&
      CRustComponentPlugin.hasCRustComponentContributions(store)
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + KEY_RustScheduleAwareMonitorPlugin_sched_mod ~> BoolValue(T)
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
            filterDomainMemMapsMCS(pd, nonModelMrNames, mrSizes).asInstanceOf[ProtectionDomain]
          localStore = SystemDescriptionProviderPlugin.putMSD("normal", SystemDescription(
            name = "normal",
            schedulingDomains = normalScheds,
            protectionDomains = normalPDs,
            memoryRegions = normalMemoryRegions,
            channels = strippedChannels,
            templateContributions = ISZ()), localStore)

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
          val monitorPdName: String = s"${MonitorInjector.monitorProcessId}_${MonitorInjector.monitorThreadId}"
          val schedStatePortPrefix: String = s"${MonitorInjector.schedStatePortName}_"
          val schedSchedulePortPrefix: String = s"${MonitorInjector.schedSchedulePortName}_"
          var schedStateVaddrOpt: Option[Z] = None()
          var schedScheduleVaddrOpt: Option[Z] = None()
          var schedStateVarAddrOpt: Option[String] = None()
          var schedScheduleVarAddrOpt: Option[String] = None()
          for (pd <- rawSd.protectionDomains) {
            if (pd.name == monitorPdName) {
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
          }
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
          val monitorPdsWithSchedMaps: ISZ[ProtectionDomain] = for (pd <- rawSd.protectionDomains) yield
            if (pd.name == monitorPdName)
              pd(memMaps = pd.memMaps.filter(m =>
                !(m.varAddr.nonEmpty && (
                  ops.StringOps(m.varAddr.get).startsWith(schedStatePortPrefix) ||
                    ops.StringOps(m.varAddr.get).startsWith(schedSchedulePortPrefix)))) :+ schedStateMap :+ schedScheduleMap)
            else pd

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

          val monitorMemoryRegions: ISZ[MemoryRegion] = rawSd.memoryRegions :+
            GenericMemoryRegion(name = "sched_state", sizeInKiBytes = 4) :+
            GenericMemoryRegion(name = "sched_schedule", sizeInKiBytes = 4)

          localStore = SystemDescriptionProviderPlugin.putMSD("monitor", SystemDescription(
            name = "monitor",
            schedulingDomains = monitorScheds,
            protectionDomains = monitorPdsWithSchedMaps,
            memoryRegions = monitorMemoryRegions,
            channels = rawSd.channels,
            templateContributions = schedTemplateContributions), localStore)



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

    if (MicrokitUtil.isMCS(options, symbolTable.rootSystem)) {
      val schedulerPath = s"${options.sel4OutputDir.get}/scheduler"

      resources = resources :+ ResourceUtil.createResourceH(
        path = s"$schedulerPath/src/monitor.scheduler.c",
        content = RustScheduleAwareMonitorPlugin.monitor_scheduler_c,
        overwrite = T, isDatatype = F)

      resources = resources :+ ResourceUtil.createResourceH(
        path = s"$schedulerPath/include/monitor.scheduler_config.h",
        content = RustScheduleAwareMonitorPlugin.monitor_scheduler_config_h,
        overwrite = T, isDatatype = F)

      resources = resources :+ ResourceUtil.createResourceH(
        path = s"$schedulerPath/include/monitor.user_config.h",
        content = RustScheduleAwareMonitorPlugin.monitor_user_config_h,
        overwrite = T, isDatatype = F)

      val monitorMkContent: ST =
        st"""${CommentTemplate.doNotEditComment_hash}
            |
            |# Monitor configuration for runtime monitoring scheduler variant.
            |# Usage: make CONFIG=monitor.mk
            |export MSD := $$(TOP_DIR)/meta.monitor.py
            |export SCHEDULER_C := $$(TOP_DIR)/scheduler/src/monitor.scheduler.c
            |export SCHEDULER_CONFIG_HEADERS := $$(TOP_DIR)/scheduler/include/monitor.user_config.h"""

      resources = resources :+ ResourceUtil.createResourceH(
        path = s"${options.sel4OutputDir.get}/monitor.mk",
        content = monitorMkContent,
        overwrite = T, isDatatype = F)
    }

    return (localStore, resources)
  }
}

@datatype class DefaultRustScheduleAwareMonitorPlugin extends RustScheduleAwareMonitorPlugin {

  val name: String = "DefaultRustScheduleAwareMonitorPlugin"

}
