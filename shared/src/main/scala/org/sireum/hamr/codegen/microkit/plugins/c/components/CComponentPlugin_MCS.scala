// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.components

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.connections.{ConnectionStore, VMRamVaddr, cConnectionContributions}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.codegen.microkit.vm.{VmMakefileTemplate, VmUser, VmUtil}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@datatype class CComponentPlugin_MCS extends CComponentPlugin {

  val name: String = "CComponentPlugin_MCS"

  val defaultComputeExecutionTime: Z = 50

  override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      MicrokitUtil.isMCS(options, symbolTable.rootSystem)
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = markAsHandled(store)

    var resources = ISZ[Resource]()

    var usedBudgetInMilli: Z = 0


    var makefileContainers: ISZ[MakefileContainer] = ISZ()

    var xmlSchedulingDomains: ISZ[SchedulingDomain] = ISZ()
    var xmlProtectionDomains: ISZ[ProtectionDomain] = ISZ()
    var xmlChannels: ISZ[Channel] = ISZ()
    var xmlMemoryRegions: ISZ[MemoryRegion] = ISZ()

    var makefileCleanEntries: ISZ[ST] = ISZ()
    var largestSchedulingDomain: Z = 2

    def processThread(t: AadlThread,
                      connectionStore: ISZ[ConnectionStore]): Unit = {

      val isVM = t.toVirtualMachine(symbolTable)
      val isRustic = MicrokitUtil.isRusty(t)

      val threadId = MicrokitUtil.getComponentIdPath(t)
      val threadMonId = st"${threadId}_MON"

      var nextMemAddressInKiBytes = 262144

      var sharedMemoryRegions: ISZ[MemoryRegion] = ISZ()

      var cCodeContributions = cConnectionContributions.empty

      val initializeMethodName = st"${threadId}_initialize"

      for (entry <- connectionStore if entry.codeContributions.contains(t.path)) {
        val codeContributions = entry.codeContributions.get(t.path).get
        cCodeContributions = cCodeContributions.combine(codeContributions.cContributions)
        sharedMemoryRegions = sharedMemoryRegions ++ codeContributions.sharedMemoryMapping
      }

      val schedulingDomain: Z = t.getDomain(symbolTable) match {
        case Some(d) =>
          if (d == 0 || d == 1) { // TODO what's upper bound
            reporter.error(t.component.identifier.pos, toolName, s"Domain '$d' is reserved")
          }
          if (d > largestSchedulingDomain) {
            largestSchedulingDomain = d
          }
          d
        case _ => halt("Infeasible")
      }

      val mk = MakefileContainer(
        resourceSuffix = threadId,
        relativePath = Some(s"${MicrokitCodegen.dirComponents}/$threadId"),
        hasHeader = T,
        isVM = isVM,
        isRustic = isRustic,
        hasUserContent = !isVM,
        hasMonitorCompanion = T)
      makefileContainers = makefileContainers :+ mk

      val computeExecutionTimeinMilli: Z = t.getComputeExecutionTime() match {
        case Some((l, h)) =>
          assert(l <= h, s"low must be <= high: $l <= $h")
          h
        case _ => defaultComputeExecutionTime
      }

      usedBudgetInMilli = usedBudgetInMilli + computeExecutionTimeinMilli

      val isUserPartition = !StoreUtil.isNonModelElement(t.path, localStore)

      xmlSchedulingDomains = xmlSchedulingDomains :+
        SchedulingDomain(id = schedulingDomain, componentName = threadMonId.render, length = computeExecutionTimeinMilli * 1_000_000, isUserPartition = isUserPartition)

      var childMemMaps: ISZ[MemoryMap] = ISZ()
      var childIrqs: ISZ[IRQ] = ISZ()

      var vms: ISZ[MicrokitDomain] = ISZ()
      if (isVM) {
        // VM name must differ from the PD name. Microkit SDK 2.2.0 generates
        // capdl object names from PD/VM names + vaddr, so a PD and its VM sharing
        // the same name and mapping the same region at the same vaddr causes a
        // "object names must be unique" panic. This was not required for SDK 1.4.1.
        val vmName = s"${threadId}_VM"

        val guestRam = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.RAM,
          threadPath = t.path,
          sizeInKiBytes = VmUtil.defaultVmRamSizeInKiBytes,
          pageSizeInKiBytes = Some(VmUtil.defaultVmPageSizeInKiBytes),
          physicalAddressInKiBytes = Some(VmUtil.defaultVmPhysicalAddress)
        )

        val hostVaddr = VMRamVaddr("uintptr_t", guestRam.vmmVaddrName)

        cCodeContributions = cCodeContributions(
          cBridge_GlobalVarContributions = cCodeContributions.cBridge_GlobalVarContributions :+ hostVaddr)

        childMemMaps = childMemMaps :+ MemoryMap(
          memoryRegion = guestRam.name,
          vaddrInKiBytes = 1048576, // 0x40_000_000
          perms = ISZ(Perm.READ, Perm.WRITE),
          varAddr = Some(hostVaddr.varName),
          cached = None())

        xmlMemoryRegions = xmlMemoryRegions :+ guestRam

        nextMemAddressInKiBytes = nextMemAddressInKiBytes + guestRam.sizeInKiBytes

        val gicRegion = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.GIC,
          threadPath = t.path,
          sizeInKiBytes = 4, // 0x1000
          pageSizeInKiBytes = None(),
          physicalAddressInKiBytes = Some(131_328) // 0x8_040_000
        )
        xmlMemoryRegions = xmlMemoryRegions :+ gicRegion

        val serialRegion = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.SERIAL,
          threadPath = t.path,
          sizeInKiBytes = 4, // 0x1000
          pageSizeInKiBytes = None(),
          physicalAddressInKiBytes = Some(147_456) // 0x9_000_000
        )
        xmlMemoryRegions = xmlMemoryRegions :+ serialRegion

        childIrqs = childIrqs :+ IRQ(id = 1, irq = 33)

        vms = vms :+ VirtualMachine(
          name = vmName,
          vcpuId = "0",
          schedulingDomain = Some(schedulingDomain),
          memMaps = ISZ(MemoryMap(
            memoryRegion = guestRam.name,
            vaddrInKiBytes = 1048576, // 0x40_000_000
            perms = ISZ(Perm.READ, Perm.WRITE, Perm.EXECUTE),
            varAddr = None(),
            cached = None()),

            // Any access to the GIC from
            //             0x8010000 - 0x8011000 will access the VCPU interface. All other
            //             accesses will result in virtual memory faults, routed to the VMM.
            MemoryMap(
              memoryRegion = gicRegion.name,
              vaddrInKiBytes = 131136, // 0x8_010_000
              perms = ISZ(Perm.READ, Perm.WRITE),
              varAddr = None(),
              cached = Some(F)
            ),
            MemoryMap(
              memoryRegion = serialRegion.name,
              vaddrInKiBytes = 147456, // 0x9_000_000
              perms = ISZ(Perm.READ, Perm.WRITE),
              varAddr = None(),
              cached = Some(F)
            ))
        )

        makefileCleanEntries = makefileCleanEntries :+ st"rm -rf $${TOP_DIR}/${mk.relativePath}/build"

        val boardPath = s"${options.sel4OutputDir.get}/${mk.relativePathVmBoardDir}/qemu_virt_aarch64"

        val vmmMake = VmMakefileTemplate.Makefile(threadId)
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/Makefile", vmmMake, F)

        val vmmLinuxDts = VmMakefileTemplate.linux_dts
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/linux.dts", vmmLinuxDts, F)

        val vmmOverlayDts = VmMakefileTemplate.overlay_dts
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/overlay.dts", vmmOverlayDts, F)

        val vmmSimpleSystem = VmMakefileTemplate.simple_system
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/simple.system", vmmSimpleSystem, F)

        val vmm_config = VmUtil.vmm_config(
          guestDtbVaddrInHex = "0x4f000000",
          guestInitRamDiskVaddrInHex = "0x4d700000",
          maxIrqs = 1
        )
        resources = resources :+ ResourceUtil.createResource(s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${threadId}_user.h", vmm_config, F)
      }

      if (makefileCleanEntries.nonEmpty) {
        localStore = MakefileUtil.addMakefileTargets(
          ISZ("system.mk"),
          ISZ(MakefileTarget(name = "clean", allowMultiple = T, dependencies = ISZ(), body = makefileCleanEntries)),
          localStore
        )
      }

      val childStackSizeInKiBytes: Option[Z] = t.stackSizeInBytes() match {
        case Some(bytes) => Some(MicrokitUtil.bytesToKiBytes(bytes))
        case _ => None()
      }

      for (r <- sharedMemoryRegions) {
        r match {
          case p: PortSharedMemoryRegion =>
            childMemMaps = childMemMaps :+ MemoryMap(
              memoryRegion = p.name,
              vaddrInKiBytes = nextMemAddressInKiBytes,
              perms = p.perms,
              varAddr = Some(p.varAddr),
              cached = None())
            nextMemAddressInKiBytes = nextMemAddressInKiBytes + p.sizeInKiBytes
          case _ => halt("")
        }
      }

      val child =
        ProtectionDomain(
          name = threadId,
          programImage = mk.elfName,
          priority = Some(140),
          budget = None(),
          period = None(),
          passive = Some(T),
          stackSizeInKiBytes = childStackSizeInKiBytes,
          cpu = None(),
          id = Some(s"1"),
          smc = Hamr_Microkit_Properties.getSmc(t.properties),
          schedulingDomain = Some(schedulingDomain),
          memMaps = childMemMaps,
          irqs = childIrqs,
          children = vms)

      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = threadMonId.render,
          programImage = mk.monElfName,
          priority = Some(150),
          budget = None(),
          period = None(),
          passive = Some(T),
          stackSizeInKiBytes = None(),
          cpu = None(),
          id = None(),
          smc = None(),
          schedulingDomain = Some(schedulingDomain),
          memMaps = ISZ(),
          irqs = ISZ(),
          children = ISZ(child))

      ///////////////////////////////////////////////////////////////////////////////////////////////////
      // C Monitor
      ///////////////////////////////////////////////////////////////////////////////////////////////////


      xmlChannels = xmlChannels :+ Channel(
        firstPD = "scheduler", firstId = schedulingDomain,
        secondPD = threadMonId.render, secondId = 0)

      xmlChannels = xmlChannels :+ Channel(
        firstPD = threadMonId.render, firstId = 1,
        secondPD = threadId, secondId = 0)

      val cMonitorSource =
        st"""#include <microkit.h>
            |#include <sddf/util/printf.h>
            |#define printf sddf_dprintf
            |
            |${CommentTemplate.doNotEditComment_slash}
            |
            |#define SCHEDULER_CH 0
            |
            |#define USER_PD 1
            |
            |void partition_init();
            |void partition_startup();
            |
            |void init(void) {
            |  printf("%s | INIT!\n", microkit_name);
            |  partition_init();
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case SCHEDULER_CH:
            |      partition_startup();
            |      microkit_notify(USER_PD);
            |      break;
            |    case USER_PD:
            |      // Signal only used during initialisation (thread signals it has finished init)
            |      microkit_notify(SCHEDULER_CH);
            |      break;
            |  }
            |}
            |"""

      val cMonitorPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.monImplFilename}"
      resources = resources :+ ResourceUtil.createResource(cMonitorPath, cMonitorSource, T)

      val cMonitorUserSource =
        st"""${CommentTemplate.safeToEditComment_slash}
            |
            |void partition_init() {
            |  // Place all initialisation code here. Such as port creation etc.
            |}
            |
            |void partition_startup() {
            |  // This function is called at the start of every partition timeslice.
            |  // For now this can handle any port management needed Place all initialisation code here. Such as port creation etc.
            |}
          """

      val cMonitorUserPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.monImplUserFilename}"
      resources = resources :+ ResourceUtil.createResource(cMonitorUserPath, cMonitorUserSource, F)

      ///////////////////////////////////////////////////////////////////////////////////////////////////
      // C Bridge
      ///////////////////////////////////////////////////////////////////////////////////////////////////

      val cUserNotifyMethodName = st"${threadId}_notify"

      cCodeContributions = cCodeContributions(
        cBridge_InitContributions = cCodeContributions.cBridge_InitContributions :+ st"$initializeMethodName();",
        cBridge_EntrypointMethodSignatures =
          st"void ${initializeMethodName}(void)" +:
            cCodeContributions.cBridge_EntrypointMethodSignatures :+
            st"void ${cUserNotifyMethodName}(microkit_channel channel)",
        cUser_MethodDefaultImpls = st"""void $initializeMethodName(void) {
                                       |  printf("%s: $initializeMethodName invoked\n", microkit_name);
                                       |}""" +: cCodeContributions.cUser_MethodDefaultImpls
      )

      if (t.isPeriodic()) {
        val timeTriggeredMethodName = st"${threadId}_timeTriggered"
        cCodeContributions = cCodeContributions(
          cBridge_EntrypointMethodSignatures = cCodeContributions.cBridge_EntrypointMethodSignatures :+ st"void ${timeTriggeredMethodName}(void)",
          cBridge_ComputeContributions = cCodeContributions.cBridge_ComputeContributions :+ st"${timeTriggeredMethodName}();",
          cUser_MethodDefaultImpls =  cCodeContributions.cUser_MethodDefaultImpls :+
            st"""void $timeTriggeredMethodName(void) {
                |  printf("%s: $timeTriggeredMethodName invoked\n", microkit_name);
                |}""")
      }

      cCodeContributions = cCodeContributions(
        cUser_MethodDefaultImpls = cCodeContributions.cUser_MethodDefaultImpls :+
          st"""void $cUserNotifyMethodName(microkit_channel channel) {
              |  // this method is called when the monitor does not handle the passed in channel
              |  switch (channel) {
              |    default:
              |      printf("%s: Unexpected channel %d\n", microkit_name, channel);
              |  }
              |}""")

      var vaddrEntries: ISZ[ST] = ISZ()
      for (v <- cCodeContributions.cBridge_GlobalVarContributions) {
        if (!v.isInstanceOf[VMRamVaddr]) {
          vaddrEntries = vaddrEntries :+ st"${v.pretty};"
        }
      }

      val cHeaderFileName = s"${threadId}.h"

      val cBridgeSource =
        st"""#include "$cHeaderFileName"
            |
            |${CommentTemplate.doNotEditComment_slash}
            |
            |${(for (u <- cCodeContributions.cBridge_EntrypointMethodSignatures) yield st"$u;", "\n")}
            |
            |${(vaddrEntries, "\n")}
            |
            |#define PORT_FROM_MON 0
            |
            |${(cCodeContributions.cBridge_PortApiMethods, "\n\n")}
            |
            |void init(void) {
            |  printf("%s | INIT!\n", microkit_name);
            |
            |  ${(cCodeContributions.cBridge_InitContributions, "\n\n")}
            |
            |  microkit_notify(PORT_FROM_MON);
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_FROM_MON:
            |      ${(cCodeContributions.cBridge_ComputeContributions, "\n\n")}
            |      break;
            |    default:
            |      ${cUserNotifyMethodName}(channel);
            |  }
            |}
            |"""

      val cBridgePath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(cBridgePath, cBridgeSource, T)

      ///////////////////////////////////////////////////////////////////////////////////////////////////
      // C User
      ///////////////////////////////////////////////////////////////////////////////////////////////////

      val cUserImplPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.cUserImplFilename}"
      val cUserImplSource: ST =
        if (isVM) {
          val cand = cCodeContributions.cBridge_GlobalVarContributions.filter(f => f.isInstanceOf[VMRamVaddr])
          assert(cand.size == 1, s"didn't find a guest ram vaddr for ${t.identifier}: ${cand.size}")
          VmUser.vmUserCode(threadId, cand(0))
        } else {
          st"""#include "$cHeaderFileName"
              |
              |${CommentTemplate.safeToEditComment_slash}
              |
              |${(cCodeContributions.cUser_MethodDefaultImpls, "\n\n")}
              |"""
        }
      resources = resources :+ ResourceUtil.createResource(cUserImplPath, cUserImplSource, F)

      ///////////////////////////////////////////////////////////////////////////////////////////////////
      // C API
      ///////////////////////////////////////////////////////////////////////////////////////////////////

      val utilIncludes: ST =
        if (isVM)
          st"""#include <libvmm/util/printf.h>
              |#include <libvmm/util/util.h>"""
        else
          st"""${MicrokitUtil.microkit_util_imports}"""

      val cApiContent =
        st"""#pragma once
            |
            |$utilIncludes
            |
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <${MicrokitTypeUtil.cAllTypesFilename}>
            |
            |${CommentTemplate.doNotEditComment_slash}
            |
            |
            |${(cCodeContributions.cPortApiMethodSigs, ";\n")};
            |"""
      val cApiPath = s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(cApiPath, cApiContent, T)

    } // end processThread





    val connectionStore = CConnectionProviderPlugin.getCConnectionStore(localStore)

    for (t <- symbolTable.getThreads()) {
      processThread(t, connectionStore)
    }

    val boundProcessors = symbolTable.getAllActualBoundProcessors()
    assert (boundProcessors.size == 1, "Linter should have ensured there is exactly one bound processor")

    var framePeriod: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(z) => framePeriod = z
      case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
    }

    if (usedBudgetInMilli > framePeriod) {
      reporter.error(None(), toolName, s"Frame period ${framePeriod} is too small for the used budget ${usedBudgetInMilli}")
      return (localStore, resources)
    }

    var xmlScheds: ISZ[SchedulingDomain] = ops.ISZOps(xmlSchedulingDomains).sortWith((a, b) => a.id < b.id)

    if (xmlScheds.nonEmpty && framePeriod - usedBudgetInMilli > 0) {
      val remainderInNano = (framePeriod - usedBudgetInMilli) * 1_000_000
      xmlScheds = xmlScheds :+ SchedulingDomain(id = 0, componentName = "pad", length = remainderInNano, isUserPartition = F)
    }

    for (e <- connectionStore;
         s <- e.systemContributions.sharedMemoryRegionContributions) {
      xmlMemoryRegions = xmlMemoryRegions :+ s
    }

    val sdName = "normal"

    val sd = SystemDescription(
      name = sdName,
      schedulingDomains = xmlScheds,
      protectionDomains = xmlProtectionDomains,
      memoryRegions = xmlMemoryRegions,
      channels = xmlChannels,
      templateContributions = ISZ())

    localStore = SystemDescriptionProviderPlugin.putMSD(sdName, sd, localStore)
    localStore = StoreUtil.addMakefileContainers(makefileContainers, localStore)

    return (localStore, resources)
  }
}
