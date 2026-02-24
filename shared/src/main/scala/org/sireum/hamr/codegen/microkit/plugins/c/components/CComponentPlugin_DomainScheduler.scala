// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.components

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.connections.{ConnectionStore, VMRamVaddr, cConnectionContributions}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.{Channel, IRQ, MakefileContainer, MemoryMap, MemoryRegion, MicrokitDomain, MicrokitUtil, Perm, PortSharedMemoryRegion, ProtectionDomain, SchedulingDomain, VirtualMachine, VirtualMachineMemoryRegion, VirtualMemoryRegionType}
import org.sireum.hamr.codegen.microkit.vm.{VmMakefileTemplate, VmUser, VmUtil}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@datatype class CComponentPlugin_DomainScheduler extends CComponentPlugin {
  val name: String = "ComponentPlugin_DomainScheduler"

  val pacerName: String = "pacer"

  val pacerSchedulingDomain: Z = 1

  val pacerComputeExecutionTime: Z = 10

  val defaultComputeExecutionTime: Z = 50

  override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.Domain_Scheduling
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = markAsHandled(store)

    var resources = ISZ[Resource]()

    var usedBudget: Z = 0


    var makefileContainers: ISZ[MakefileContainer] = ISZ()

    var xmlSchedulingDomains: ISZ[SchedulingDomain] = ISZ()
    var xmlProtectionDomains: ISZ[ProtectionDomain] = ISZ()
    var xmlChannels: ISZ[Channel] = ISZ()
    var xmlMemoryRegions: ISZ[MemoryRegion] = ISZ()

    var codePacerDefines: ISZ[ST] = ISZ()
    var codePacerPings: ISZ[ST] = ISZ()
    var largestSchedulingDomain: Z = 2

    var portPacerToEndOfFrame: Z = -1
    var portEndOfFrameToPacer: Z = -1

    var nextPacerChannelId: Z = 61 // FIXME document indicates < 63, but build indicates < 62

    def getNextPacerChannelId: Z = {
      nextPacerChannelId = nextPacerChannelId - 1
      return nextPacerChannelId + 1
    }

    def addPacerComponent(): Unit = {
      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = pacerName,
          schedulingDomain = Some(pacerSchedulingDomain),
          id = None(),
          stackSizeInKiBytes = None(),
          memMaps = ISZ(),
          irqs = ISZ(),
          programImage = s"${pacerName}.elf",
          smc = None(),
          passive = None(),
          children = ISZ())

      portPacerToEndOfFrame = getNextPacerChannelId
      portEndOfFrameToPacer = getNextPacerChannelId

      val mk = MakefileContainer(
        resourceSuffix = pacerName,
        relativePath = Some(s"${MicrokitCodegen.dirComponents}/${ops.StringOps(pacerName).toLower}"),
        hasHeader = F,
        isVM = F,
        isRustic = F,
        hasUserContent = F,
        hasMonitorCompanion = F)
      makefileContainers = makefileContainers :+ mk

      val content =
        st"""#include <stdint.h>
            |#include <microkit.h>
            |
            |${(codePacerDefines, "\n")}
            |
            |void init(void) {}
            |
            |void notified(microkit_channel channel) {
            |  switch(channel) {
            |    ${(codePacerPings, "\n")}
            |  }
            |}
            |"""
      val path = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(path, content, T)
    }

    def processThread(t: AadlThread,
                      connectionStore: ISZ[ConnectionStore]): Z = {

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
        hasMonitorCompanion = F)
      makefileContainers = makefileContainers :+ mk

      val computeExecutionTime: Z = t.getComputeExecutionTime() match {
        case Some((l, h)) =>
          assert(l <= h, s"low must be <= high: $l <= $h")
          h
        case _ => defaultComputeExecutionTime
      }

      xmlSchedulingDomains = xmlSchedulingDomains :+
        SchedulingDomain(id = schedulingDomain, length = computeExecutionTime)

      var childMemMaps: ISZ[MemoryMap] = ISZ()
      var childIrqs: ISZ[IRQ] = ISZ()

      var vms: ISZ[MicrokitDomain] = ISZ()
      if (isVM) {
        val vmName = threadId

        val guestRam = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.RAM,
          threadPath = t.path,
          sizeInKiBytes = MicrokitUtil.defaultVmRamSizeInKiBytes,
          physicalAddressInKiBytes = None()
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
          physicalAddressInKiBytes = Some(131328) // 0x8040000
        )
        xmlMemoryRegions = xmlMemoryRegions :+ gicRegion

        val serialRegion = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.SERIAL,
          threadPath = t.path,
          sizeInKiBytes = 4, // 0x1000
          physicalAddressInKiBytes = Some(147456) // 0x9_000_000
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

        val boardPath = s"${options.sel4OutputDir.get}/${mk.relativePathVmBoardDir}/qemu_virt_aarch64"

        val vmmMake = VmMakefileTemplate.Makefile(threadId)
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/Makefile", vmmMake, T)


        val vmm_config = VmUtil.vmm_config(
          guestDtbVaddrInHex = "0x4f000000",
          guestInitRamDiskVaddrInHex = "0x4d700000",
          maxIrqs = 1
        )
        resources = resources :+ ResourceUtil.createResource(s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${threadId}_user.h", vmm_config, T)
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
          schedulingDomain = Some(schedulingDomain),
          id = Some(s"1"),
          stackSizeInKiBytes = childStackSizeInKiBytes,
          memMaps = childMemMaps,
          irqs = childIrqs,
          programImage = mk.elfName,
          smc = Hamr_Microkit_Properties.getSmc(t.properties),
          passive = Hamr_Microkit_Properties.getPassive(t.properties),
          children = vms)

      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = threadMonId.render,
          schedulingDomain = Some(schedulingDomain),
          id = None(),
          stackSizeInKiBytes = None(),
          memMaps = ISZ(),
          irqs = ISZ(),
          programImage = mk.monElfName,
          smc = None(),
          passive = None(),
          children = ISZ(child))

      val pacerChannelId = getNextPacerChannelId

      xmlChannels = xmlChannels :+
        Channel(
          firstPD = pacerName, firstId = pacerChannelId,
          secondPD = threadMonId.render, secondId = pacerChannelId)

      ///////////////////////////////////////////////////////////////////////////////////////////////////
      // C Monitor
      ///////////////////////////////////////////////////////////////////////////////////////////////////

      val threadMonCaps = s"PORT_TO_${ops.StringOps(threadMonId.render).toUpper}"

      codePacerDefines = codePacerDefines :+ st"""#define $threadMonCaps $pacerChannelId"""
      codePacerPings = codePacerPings :+
        st"""case ${threadMonCaps}:
            |  microkit_notify($threadMonCaps);
            |  break;"""

      val monChannelId = getNextPacerChannelId

      xmlChannels = xmlChannels :+ Channel(
        firstPD = threadMonId.render, firstId = monChannelId,
        secondPD = threadId, secondId = monChannelId)

      val cMonitorSource =
        st"""#include <microkit.h>
            |
            |${MicrokitUtil.doNotEdit}
            |
            |#define PORT_PACER $pacerChannelId
            |
            |#define PORT_TO_CHILD $monChannelId
            |
            |void init(void) {
            |  microkit_notify(PORT_PACER);
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_PACER:
            |      // notify child
            |      microkit_notify(PORT_TO_CHILD);
            |
            |      // send response back to pacer
            |      microkit_notify(PORT_PACER);
            |      break;
            |  }
            |}"""

      val cMonitorPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.monImplFilename}"
      resources = resources :+ ResourceUtil.createResource(cMonitorPath, cMonitorSource, T)

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
            |${MicrokitUtil.doNotEdit}
            |
            |${(for (u <- cCodeContributions.cBridge_EntrypointMethodSignatures) yield st"$u;", "\n")}
            |
            |${(vaddrEntries, "\n")}
            |
            |#define PORT_FROM_MON $monChannelId
            |
            |${(cCodeContributions.cBridge_PortApiMethods, "\n\n")}
            |
            |void init(void) {
            |  ${(cCodeContributions.cBridge_InitContributions, "\n\n")}
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
          VmUser.vmUserCode(threadId, cand(0).pretty)
        } else {
          st"""#include "$cHeaderFileName"
              |
              |${MicrokitUtil.safeToEdit}
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
          st"""#include <printf.h>
              |#include <util.h>"""

      val cApiContent =
        st"""#pragma once
            |
            |$utilIncludes
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <${MicrokitTypeUtil.cAllTypesFilename}>
            |
            |${MicrokitUtil.doNotEdit}
            |
            |
            |${(cCodeContributions.cPortApiMethodSigs, ";\n")};
            |"""
      val cApiPath = s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(cApiPath, cApiContent, T)

      return computeExecutionTime
    } // end processThread





    val connectionStore = CConnectionProviderPlugin.getCConnectionStore(localStore)

    for (t <- symbolTable.getThreads()) {
      usedBudget = usedBudget + processThread(t, connectionStore)
    }

    addPacerComponent()

    val pacerSlot = SchedulingDomain(id = pacerSchedulingDomain, length = pacerComputeExecutionTime)
    val currentScheduleSize = xmlSchedulingDomains.size
    usedBudget = usedBudget + (currentScheduleSize * pacerComputeExecutionTime)

    val boundProcessors = symbolTable.getAllActualBoundProcessors()
    assert (boundProcessors.size == 1, "Linter should have ensured there is exactly one bound processor")

    var framePeriod: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(z) => framePeriod = z
      case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
    }

    if (usedBudget > framePeriod) {
      reporter.error(None(), toolName, s"Frame period ${framePeriod} is too small for the used budget ${usedBudget}")
      return (localStore, resources)
    }

    var xmlScheds: ISZ[SchedulingDomain] = ISZ()
    for (x <- ops.ISZOps(xmlSchedulingDomains).sortWith((a, b) => a.id < b.id)) {
      xmlScheds = xmlScheds :+ pacerSlot :+ x
    }

    if (framePeriod - usedBudget > 0) {
      // switch to domain 0 to use up the rest of the budget
      xmlScheds = xmlScheds :+ SchedulingDomain(id = 0, length = framePeriod - usedBudget)
    }

    for (e <- connectionStore;
         s <- e.systemContributions.sharedMemoryRegionContributions) {
      xmlMemoryRegions = xmlMemoryRegions :+ s
    }

    localStore = StoreUtil.addChannels(xmlChannels, localStore)
    localStore = StoreUtil.addMemoryRegions(xmlMemoryRegions, localStore)
    localStore = StoreUtil.addProtectionDomains(xmlProtectionDomains, localStore)
    //localStore = StoreUtil.addSchedulingDomains(xmlSchedulingDomains, localStore)
    localStore = StoreUtil.addSchedulingDomains(xmlScheds, localStore)
    localStore = StoreUtil.addMakefileContainers(makefileContainers, localStore)

    return (localStore, resources)
  }
}
