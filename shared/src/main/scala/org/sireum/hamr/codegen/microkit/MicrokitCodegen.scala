// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.{pacerSchedulingDomain, toolName}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.lint.Linter
import org.sireum.hamr.codegen.microkit.types.{DefaultTypeStore, TypeStore, TypeUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.codegen.microkit.vm.{VmMakefileTemplate, VmUser, VmUtil}
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object MicrokitCodegen {
  val toolName: String = "Mircokit Codegen"

  val microkitSystemXmlFilename: String = "microkit.system"
  val systemMakeFilename: String = "system.mk"

  val pacerName: String = "pacer"

  val pacerSchedulingDomain: Z = 1

  val dirComponents: String = "components"
  val dirInclude: String = "include"
  val dirSrc: String = "src"

  val pacerComputeExecutionTime: Z = 10

  val defaultComputeExecutionTime: Z = 50
}

@record class MicrokitCodegen {
  var resources: ISZ[FileResource] = ISZ()
  var makefileContainers: ISZ[MakefileContainer] = ISZ()

  var xmlSchedulingDomains: ISZ[SchedulingDomain] = ISZ()
  var xmlProtectionDomains: ISZ[ProtectionDomain] = ISZ()

  var xmlChannels: ISZ[Channel] = ISZ()

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

  def run(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): CodeGenResults = {

    val modelIsRusty: B = symbolTable.getThreads().filter(p => Util.isRust(p)).nonEmpty

    val baseTypesIncludePath = s"${options.sel4OutputDir.get}/${TypeUtil.cTypesDir}/${MicrokitCodegen.dirInclude}"

    @pure def getName(ids: ISZ[String]): ST = {
      return st"${(ops.ISZOps(ids).drop(1), "_")}"
    }

    def addPacerComponent(): Unit = {
      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = MicrokitCodegen.pacerName,
          schedulingDomain = Some(pacerSchedulingDomain),
          id = None(),
          stackSizeInKiBytes = None(),
          memMaps = ISZ(),
          irqs = ISZ(),
          programImage = s"${MicrokitCodegen.pacerName}.elf",
          children = ISZ())

      portPacerToEndOfFrame = getNextPacerChannelId
      portEndOfFrameToPacer = getNextPacerChannelId

      val mk = MakefileContainer(
        resourceSuffix = MicrokitCodegen.pacerName,
        relativePath = Some(s"${MicrokitCodegen.dirComponents}/${ops.StringOps(MicrokitCodegen.pacerName).toLower}"),
        hasHeader = F,
        isVM = F,
        isRustic = F,
        hasUserContent = F)
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

    def processTypes(): Map[AadlType, TypeStore] = {
      var ret: Map[AadlType, TypeStore] = Map.empty

      val eventType = TypeUtil.processDatatype(TypeUtil.eventPortType, reporter)
      ret = ret + TypeUtil.eventPortType ~> DefaultTypeStore(typeName = eventType.c.name, aadlType = TypeUtil.eventPortType)

      if (types.rawConnections) {
        reporter.error(None(), MicrokitCodegen.toolName, "Raw connections are not currently supported")
        return ret
      }

      val touchedTypes: ISZ[TypeUtil.UberTypeProvider] =
        for (t <- Util.getAllTouchedTypes(types, symbolTable, reporter))
          yield TypeUtil.processDatatype(t, reporter)

      var forwardDefs: ISZ[ST] = ISZ()
      var defs: ISZ[ST] = ISZ()
      for (t <- ops.ISZOps(touchedTypes).sortWith((a, b) => CommonTypeUtil.isEnumTypeH(a.c.aadlType))) {
        ret = ret + t.c.aadlType ~> DefaultTypeStore(typeName = t.c.name, aadlType = t.c.aadlType)
        if (!CommonTypeUtil.isBaseTypeH(t.c.aadlType)) {
          if (t.c.cForwardDeclaration.nonEmpty) {
            forwardDefs = forwardDefs :+ t.c.cForwardDeclaration.get
          }
          defs = defs :+ t.c.cTypeDeclaration.get
        }
      }

      val cContent =
        st"""#pragma once
            |
            |#include <stdbool.h>
            |#include <stdint.h>
            |
            |${Util.doNotEdit}
            |
            |${(forwardDefs, "\n\n")}
            |
            |${(defs, "\n\n")}
            |"""

      val outputdir = s"${options.sel4OutputDir.get}/${TypeUtil.cTypesDir}/${MicrokitCodegen.dirInclude}"
      val path = s"$outputdir/${TypeUtil.cAadlTypesFilename}"
      resources = resources :+ ResourceUtil.createResourceH(path, cContent, T, T)

      val cEventCounterPath = s"$baseTypesIncludePath/${TypeUtil.cEventCounterFilename}"
      resources = resources :+ ResourceUtil.createResourceH(
        path = cEventCounterPath, content = TypeUtil.cEventCounterContent, overwrite = T, isDatatype = T)

      if (modelIsRusty) {
        val rustAadlTypesContent =
          st"""#![allow(non_camel_case_types)]
              |#![allow(non_snake_case)]
              |#![allow(non_upper_case_globals)]
              |
              |${Util.doNotEdit}
              |
              |${(for (t <- touchedTypes.filter(t => t.rust.rustTypeDeclaration.nonEmpty)) yield t.rust.rustTypeDeclaration, "\n\n")}
              |"""
        val rustTypesDir = s"${options.sel4OutputDir.get}/${TypeUtil.rustTypesDir}"
        val outRustTypesPath = s"$rustTypesDir/src/${TypeUtil.rustAadlTypesFilename}"
        resources = resources :+ ResourceUtil.createResourceH(outRustTypesPath, rustAadlTypesContent, T, T)

        val rustEventCounterPath = s"$rustTypesDir/src/${TypeUtil.rustEventCounterFilename}"
        resources = resources :+ ResourceUtil.createResourceH(rustEventCounterPath, TypeUtil.rustEventCounterContent, T, T)

        val rustMicrokitTypesPath = s"$rustTypesDir/src/${TypeUtil.rustMicrokitTypesFilename}"
        resources = resources :+ ResourceUtil.createResourceH(rustMicrokitTypesPath, TypeUtil.rustMicrokitTypesContent, T, T)

        val rustAllTypesContents =
          st"""#![no_std]
              |#![allow(non_camel_case_types)]
              |
              |${Util.doNotEdit}
              |
              |mod ${TypeUtil.aadlTypesFilenamePrefix};
              |pub use ${TypeUtil.aadlTypesFilenamePrefix}::*;
              |
              |mod ${TypeUtil.eventCounterFilenamePrefix};
              |pub use ${TypeUtil.eventCounterFilenamePrefix}::*;
              |
              |mod ${TypeUtil.rustMicrokitTypesPrefix};
              |pub use ${TypeUtil.rustMicrokitTypesPrefix}::*;
              |"""
        val rustAllTypesPath = s"$rustTypesDir/src/${TypeUtil.rustTypesFilename}"
        resources = resources :+ ResourceUtil.createResourceH(rustAllTypesPath, rustAllTypesContents, T, T)

        val rustTypesCargoPath = s"$rustTypesDir/Cargo.toml"
        resources = resources :+ ResourceUtil.createResource(rustTypesCargoPath, TypeUtil.rustCargoContent, F)
      }

      return ret
    }

    def processConnections(typeStore: Map[AadlType, TypeStore]): ISZ[ConnectionStore] = {
      var ret: ISZ[ConnectionStore] = ISZ()

      for (srcThread <- symbolTable.getThreads()) {

        for (srcPort <- srcThread.getPorts()
             if srcPort.direction == Direction.Out && symbolTable.outConnections.contains(srcPort.path)) {

          var receiverContributions: Map[ISZ[String], UberConnectionContributions] = Map.empty

          for (outConnection <- symbolTable.getOutConnections(srcPort.path)) {
            symbolTable.componentMap.get(outConnection.dst.component.name).get match {
              case dstThread: AadlThread =>

                val dstPort = symbolTable.featureMap.get(outConnection.dst.feature.get.name).get

                receiverContributions = receiverContributions + dstThread.path ~>
                  ConnectionUtil.processInPort(dstThread, dstPort.asInstanceOf[AadlPort],
                    Some(srcPort), Some(outConnection), typeStore, symbolTable)

              case x =>
                halt(s"Only handling thread to thread connections currently: $x")
            }
          } // end processing out connections for the source port

          val senderContributions = ConnectionUtil.processOutPort(srcPort, receiverContributions, typeStore)

          val typeApiContributions: ISZ[TypeApiContributions] =
            (Set.empty[TypeApiContributions] ++ (for (rc <- receiverContributions.values) yield
              TypeUtil.getTypeApiContributions(rc.aadlType, typeStore.get(rc.aadlType).get, rc.queueSize))).elements

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = senderContributions.sharedMemoryMapping,
                  channelContributions = ISZ()),
              typeApiContributions = typeApiContributions,
              senderName = srcThread.path,
              senderContributions = Some(senderContributions),
              receiverContributions = receiverContributions)
        } // end processing connections for source port

        // now handle unconnected ports

        for (unconnectedOutPort <- srcThread.getPorts().filter(p => p.direction == Direction.Out).filter(p => !symbolTable.outConnections.contains(p.path))) {
          val senderContributions = ConnectionUtil.processOutPort(unconnectedOutPort, Map.empty, typeStore)
          val typeApiContributions =
            TypeUtil.getTypeApiContributions(senderContributions.aadlType, typeStore.get(senderContributions.aadlType).get, senderContributions.queueSize)

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = senderContributions.sharedMemoryMapping,
                  channelContributions = ISZ()),
              typeApiContributions = ISZ(typeApiContributions),
              senderName = srcThread.path,
              senderContributions = Some(senderContributions),
              receiverContributions = Map.empty
            )
        }

        for (unconnectedInPort <- srcThread.getPorts().filter(p => p.direction == Direction.In).filter(p => !symbolTable.inConnections.contains(p.path))) {
          val receiverContributions = ConnectionUtil.processInPort(
            dstThread = srcThread, dstPort = unconnectedInPort,
            srcPort = None(), outConnection = None(),
            typeStore = typeStore, symbolTable = symbolTable)

          val typeApiContributions =
            TypeUtil.getTypeApiContributions(receiverContributions.aadlType, typeStore.get(receiverContributions.aadlType).get, receiverContributions.queueSize)

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = receiverContributions.sharedMemoryMapping,
                  channelContributions = ISZ()),
              typeApiContributions = ISZ(typeApiContributions),
              senderName = srcThread.path,
              senderContributions = None(),
              receiverContributions = Map.empty[ISZ[String], UberConnectionContributions] +
                srcThread.path ~> receiverContributions)
        }
      } // end processing connections for threads
      return ret
    }

    def processThread(t: AadlThread,
                      connectionStore: ISZ[ConnectionStore]): (MicrokitDomain, ISZ[MemoryRegion], Z) = {

      var retMemoryRegions: ISZ[MemoryRegion] = ISZ()

      val isVM = t.toVirtualMachine(symbolTable)
      val isRustic = Util.isRust(t)

      val threadId = getName(t.path).render
      val threadMonId = st"${threadId}_MON"

      var sharedMemoryRegions: ISZ[MemoryRegion] = ISZ()
      var cHeaderImports: ISZ[String] = ISZ()
      var cUserMethodSignatures: ISZ[ST] = ISZ()
      var cUserMethodDefaultImpls: ISZ[ST] = ISZ()
      var cVaddrs: ISZ[GlobalVarContribution] = ISZ()
      var cCodeApiMethodSigs: ISZ[ST] = ISZ()
      var cCodeApiMethods: ISZ[ST] = ISZ()
      var cBridgeInitContributions: ISZ[ST] = ISZ()
      var cComputeContributions: ISZ[ST] = ISZ()
      var nextMemAddressInKiBytes = 262144

      var rustExternMethods: ISZ[ST] = ISZ()
      var rustUnsafeMethods: ISZ[ST] = ISZ()

      val initializeMethodName = st"${threadId}_initialize"

      for (entry <- connectionStore) {

        if (entry.senderName == t.path && entry.senderContributions.nonEmpty) {
          sharedMemoryRegions = sharedMemoryRegions ++ entry.senderContributions.get.sharedMemoryMapping

          cHeaderImports = cHeaderImports ++ entry.senderContributions.get.cContributions.cHeaderImportContributions
          cUserMethodSignatures = cUserMethodSignatures ++ entry.senderContributions.get.cContributions.cUserMethodSignatures
          cUserMethodDefaultImpls = cUserMethodDefaultImpls ++ entry.senderContributions.get.cContributions.cUserMethodDefaultImpls
          cCodeApiMethodSigs = cCodeApiMethodSigs ++ entry.senderContributions.get.cContributions.cApiMethodSigs
          cCodeApiMethods = cCodeApiMethods ++ entry.senderContributions.get.cContributions.cApiMethods
          cVaddrs = cVaddrs ++ entry.senderContributions.get.cContributions.cGlobalVarContributions
          cBridgeInitContributions = cBridgeInitContributions ++ entry.senderContributions.get.cContributions.cInitContributions
          cComputeContributions = cComputeContributions ++ entry.senderContributions.get.cContributions.cComputeContributions

          rustExternMethods = rustExternMethods ++ entry.senderContributions.get.rustContributions.rustExternApis
          rustUnsafeMethods = rustUnsafeMethods ++ entry.senderContributions.get.rustContributions.rustUnsafeExternApisWrappers
        }
        if (entry.receiverContributions.contains(t.path)) {
          val c = entry.receiverContributions.get(t.path).get
          sharedMemoryRegions = sharedMemoryRegions ++ c.sharedMemoryMapping
          cHeaderImports = cHeaderImports ++ c.cContributions.cHeaderImportContributions
          cUserMethodSignatures = cUserMethodSignatures ++ c.cContributions.cUserMethodSignatures
          cUserMethodDefaultImpls = cUserMethodDefaultImpls ++ c.cContributions.cUserMethodDefaultImpls
          cCodeApiMethodSigs = cCodeApiMethodSigs ++ c.cContributions.cApiMethodSigs
          cCodeApiMethods = cCodeApiMethods ++ c.cContributions.cApiMethods
          cVaddrs = cVaddrs ++ c.cContributions.cGlobalVarContributions
          cBridgeInitContributions = cBridgeInitContributions ++ c.cContributions.cInitContributions
          cComputeContributions = cComputeContributions ++ c.cContributions.cComputeContributions

          rustExternMethods = rustExternMethods ++ c.rustContributions.rustExternApis
          rustUnsafeMethods = rustUnsafeMethods ++ c.rustContributions.rustUnsafeExternApisWrappers
        }
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
        hasUserContent = !isVM)
      makefileContainers = makefileContainers :+ mk

      val computeExecutionTime: Z = t.getComputeExecutionTime() match {
        case Some((l, h)) =>
          assert(l <= h, s"low must be <= high: $l <= $h")
          h
        case _ => MicrokitCodegen.defaultComputeExecutionTime
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
          sizeInKiBytes = Util.defaultVmRamSizeInKiBytes,
          physicalAddressInKiBytes = None()
        )

        val hostVaddr = VMRamVaddr("uintptr_t", guestRam.vmmVaddrName)

        cVaddrs = cVaddrs :+ hostVaddr

        childMemMaps = childMemMaps :+ MemoryMap(
          memoryRegion = guestRam.name,
          vaddrInKiBytes = 1048576, // 0x40_000_000
          perms = ISZ(Perm.READ, Perm.WRITE),
          varAddr = Some(hostVaddr.varName),
          cached = None())

        retMemoryRegions = retMemoryRegions :+ guestRam

        nextMemAddressInKiBytes = nextMemAddressInKiBytes + guestRam.sizeInKiBytes

        val gicRegion = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.GIC,
          threadPath = t.path,
          sizeInKiBytes = 4, // 0x1000
          physicalAddressInKiBytes = Some(131328) // 0x8040000
        )
        retMemoryRegions = retMemoryRegions :+ gicRegion

        val serialRegion = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.SERIAL,
          threadPath = t.path,
          sizeInKiBytes = 4, // 0x1000
          physicalAddressInKiBytes = Some(147456) // 0x9_000_000
        )
        retMemoryRegions = retMemoryRegions :+ serialRegion

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
        case Some(bytes) => Some(Util.bytesToKiBytes(bytes))
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
          children = ISZ(child))

      val pacerChannelId = getNextPacerChannelId

      xmlChannels = xmlChannels :+ Channel(
        firstPD = MicrokitCodegen.pacerName, firstId = pacerChannelId,
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
            |${Util.doNotEdit}
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
      cBridgeInitContributions = cBridgeInitContributions :+ st"$initializeMethodName();"
      cUserMethodSignatures = st"void ${initializeMethodName}(void)" +: cUserMethodSignatures
      cUserMethodSignatures = cUserMethodSignatures :+ st"void ${cUserNotifyMethodName}(microkit_channel channel)"

      cUserMethodDefaultImpls =
        st"""void $initializeMethodName(void) {
            |  printf("%s: $initializeMethodName invoked\n", microkit_name);
            |}""" +: cUserMethodDefaultImpls

      if (t.isPeriodic()) {
        val timeTriggeredMethodName = st"${threadId}_timeTriggered"
        cUserMethodSignatures = cUserMethodSignatures :+ st"void ${timeTriggeredMethodName}(void)"
        cComputeContributions = cComputeContributions :+ st"${timeTriggeredMethodName}();"
        cUserMethodDefaultImpls = cUserMethodDefaultImpls :+
          st"""void $timeTriggeredMethodName(void) {
              |  printf("%s: $timeTriggeredMethodName invoked\n", microkit_name);
              |}"""
      }

      cUserMethodDefaultImpls = cUserMethodDefaultImpls :+
        st"""void $cUserNotifyMethodName(microkit_channel channel) {
            |  // this method is called when the monitor does not handle the passed in channel
            |  switch (channel) {
            |    default:
            |      printf("%s: Unexpected channel %d\n", microkit_name, channel);
            |  }
            |}"""

      var vaddrEntries: ISZ[ST] = ISZ()
      for (v <- cVaddrs) {
        if (!v.isInstanceOf[VMRamVaddr]) {
          vaddrEntries = vaddrEntries :+ v.pretty
        }
      }

      val cHeaderFileName = s"${threadId}.h"

      val cBridgeSource =
        st"""#include "$cHeaderFileName"
            |
            |${Util.doNotEdit}
            |
            |${(for (u <- cUserMethodSignatures) yield st"$u;", "\n")}
            |
            |${(vaddrEntries, "\n")}
            |
            |#define PORT_FROM_MON $monChannelId
            |
            |${(cCodeApiMethods, "\n\n")}
            |
            |void init(void) {
            |  ${(cBridgeInitContributions, "\n\n")}
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_FROM_MON:
            |      ${(cComputeContributions, "\n\n")}
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
          val cand = cVaddrs.filter(f => f.isInstanceOf[VMRamVaddr])
          assert(cand.size == 1, s"didn't find a guest ram vaddr for ${t.identifier}: ${cand.size}")
          VmUser.vmUserCode(threadId, cand(0).pretty)
        } else {
          st"""#include "$cHeaderFileName"
              |
              |${Util.safeToEdit}
              |
              |${(cUserMethodDefaultImpls, "\n\n")}
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

      val himports: ISZ[ST] = for (h <- cHeaderImports) yield st"#include <$h>"
      val cApiContent =
        st"""#pragma once
            |
            |$utilIncludes
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <${TypeUtil.cAllTypesFilename}>
            |
            |${Util.doNotEdit}
            |
            |${(himports, "\n")}
            |
            |${(cCodeApiMethodSigs, ";\n")};
            |"""
      val cApiPath = s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(cApiPath, cApiContent, T)

      if (isRustic && !isVM) {
        val rustComponentPath = s"${options.sel4OutputDir.get}/crates/$threadId"
        val rustComponentSrcPath = s"$rustComponentPath/src/"

        var rustUserMethods: ISZ[ST] = ISZ()
        if (t.isPeriodic()) {
          rustUserMethods = rustUserMethods :+
            st"""#[no_mangle]
                |pub extern "C" fn ${threadId}_timeTriggered() {
                |  info!("timeTriggered invoked");
                |}"""
        }
        val rustUserContent = RustUtil.rustUserContent(threadId, rustUserMethods)
        val rustUserContentPath = s"$rustComponentSrcPath/$threadId.rs"
        resources = resources :+ ResourceUtil.createResource(rustUserContentPath, rustUserContent, F)

        val rustApiContent =
          st"""#![allow(dead_code)]
              |#![allow(non_snake_case)]
              |
              |${Util.doNotEdit}
              |
              |use types::*;
              |
              |extern "C" {
              |  ${(for(r <- rustExternMethods) yield st"$r;", "\n")}
              |}
              |
              |${(rustUnsafeMethods, "\n\n")}
              |"""
        val rustApiPath = s"$rustComponentSrcPath/api.rs"
        resources = resources :+ ResourceUtil.createResource(rustApiPath, rustApiContent, T)

        val rustLoggingContent = RustUtil.rustLoggingContent
        val rustLoggingPath = s"$rustComponentSrcPath/logging.rs"
        resources = resources :+ ResourceUtil.createResource(rustLoggingPath, rustLoggingContent, T)

        val rustToolchainPath = s"$rustComponentPath/rust-toolchain.toml"
        resources = resources :+ ResourceUtil.createResource(rustToolchainPath, RustUtil.rustToolchainContent, F)

        val rustComponentMakefile = RustUtil.makefileComponent()
        val rustComponentMakefilePath = s"$rustComponentPath/Makefile"
        resources = resources :+ ResourceUtil.createResource(rustComponentMakefilePath, rustComponentMakefile, F)

        val rustCargoContent = RustUtil.cargoComponentToml(threadId)
        val rustCargoPath = s"$rustComponentPath/Cargo.toml"
        resources = resources :+ ResourceUtil.createResource(rustCargoPath, rustCargoContent, F)
      }

      return (child, retMemoryRegions, computeExecutionTime)
    } // end processThread


    if (!Linter.lint(model, options, types, symbolTable, reporter)) {
      return CodeGenResults(ISZ(), ISZ())
    }

    val boundProcessors = symbolTable.getAllActualBoundProcessors()
    if (boundProcessors.size != 1) {
      reporter.error(None(), toolName, "Currently only handling models with exactly one actual bound processor")
      return CodeGenResults(ISZ(), ISZ())
    }
    var framePeriod: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(z) => framePeriod = z
      case _ => halt("Infeasible") // linter should have ensured bound processor has frame period
    }

    var buildEntries: ISZ[ST] = ISZ()

    val typeStore = processTypes()

    var typeHeaderFilenames: ISZ[String] = ISZ(TypeUtil.cAadlTypesFilename)
    var typeImplFilenames: ISZ[String] = ISZ()
    var typeObjectNames: ISZ[String] = ISZ()

    val connectionStore = processConnections(typeStore)
    for (entry <- connectionStore) {
      val srcPath = s"${options.sel4OutputDir.get}/${TypeUtil.cTypesDir}/${MicrokitCodegen.dirSrc}"

      for (tc <- entry.typeApiContributions) {
        typeHeaderFilenames = typeHeaderFilenames :+ tc.headerFilename
        typeImplFilenames = typeImplFilenames :+ tc.implementationFilename
        typeObjectNames = typeObjectNames :+ tc.objectName
        buildEntries = buildEntries :+ tc.buildEntry

        val headerPath = s"$baseTypesIncludePath/${tc.headerFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = headerPath, content = tc.header, overwrite = T, isDatatype = T)

        val implPath = s"$srcPath/${tc.implementationFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = implPath, content = tc.implementation, overwrite = T, isDatatype = T)
      }
    }


    val allTypesContent =
      st"""#pragma once
          |
          |${Util.doNotEdit}
          |
          |${(for (i <- typeHeaderFilenames) yield st"#include <$i>", "\n")}
          |"""
    val allTypesPath = s"$baseTypesIncludePath/${TypeUtil.cAllTypesFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = allTypesPath, content = allTypesContent, overwrite = T, isDatatype = T)

    var memoryRegions: ISZ[MemoryRegion] = ISZ()
    var usedBudget: Z = 0
    for (t <- symbolTable.getThreads()) {
      val results = processThread(t, connectionStore)
      memoryRegions = memoryRegions ++ results._2
      usedBudget = usedBudget + results._3
    }

    addPacerComponent()

    val pacerSlot = SchedulingDomain(id = MicrokitCodegen.pacerSchedulingDomain, length = MicrokitCodegen.pacerComputeExecutionTime)
    val currentScheduleSize = xmlSchedulingDomains.size
    usedBudget = usedBudget + (currentScheduleSize * MicrokitCodegen.pacerComputeExecutionTime)

    if (usedBudget > framePeriod) {
      reporter.error(None(), toolName, s"Frame period ${framePeriod} is too small for the used budget ${usedBudget}")
      return CodeGenResults(ISZ(), ISZ())
    }

    var xmlScheds: ISZ[SchedulingDomain] = ISZ()
    for (x <- ops.ISZOps(xmlSchedulingDomains).sortWith((a, b) => a.id < b.id)) {
      xmlScheds = xmlScheds :+ pacerSlot :+ x
    }
    xmlScheds = xmlScheds :+ SchedulingDomain(id = 0, length = framePeriod - usedBudget)


    for (e <- connectionStore;
         s <- e.systemContributions.sharedMemoryRegionContributions) {
      memoryRegions = memoryRegions :+ s
    }

    val sd = SystemDescription(
      schedulingDomains = xmlScheds,
      protectionDomains = xmlProtectionDomains,
      memoryRegions = memoryRegions,
      channels = xmlChannels)

    val xmlPath = s"${options.sel4OutputDir.get}/${MicrokitCodegen.microkitSystemXmlFilename}"
    resources = resources :+ ResourceUtil.createResource(path = xmlPath, content = sd.prettyST, overwrite = T)

    val sysDot = sd.toDot
    val dotPath = s"${options.sel4OutputDir.get}/microkit.dot"
    resources = resources :+ ResourceUtil.createResource(path = dotPath, content = sysDot, overwrite = T)

    val makefileContents = MakefileTemplate.mainMakefile
    val makefilePath = s"${options.sel4OutputDir.get}/Makefile"
    resources = resources :+ ResourceUtil.createResource(makefilePath, makefileContents, T)

    buildEntries = buildEntries ++ (for (mk <- makefileContainers) yield mk.buildEntry)

    var elfFiles: ISZ[String] = ISZ()
    var oFiles: ISZ[String] = ISZ()
    for (mk <- makefileContainers) {
      elfFiles = elfFiles ++ mk.getElfNames
      oFiles = oFiles ++ mk.getObjNames
    }

    val elfEntries: ISZ[ST] = for (mk <- makefileContainers) yield mk.elfEntry

    val systemmkContents = MakefileTemplate.systemMakefile(
      elfFiles = elfFiles,
      typeObjectNames = typeObjectNames,
      buildEntries = buildEntries,
      elfEntries = elfEntries)

    val systemmkPath = s"${options.sel4OutputDir.get}/${MicrokitCodegen.systemMakeFilename}"
    resources = resources :+ ResourceUtil.createResource(systemmkPath, systemmkContents, T)


    val utilIncludePath = s"${options.sel4OutputDir.get}/${Util.utilDir}/${MicrokitCodegen.dirInclude}"
    val utilSrcPath = s"${options.sel4OutputDir.get}/${Util.utilDir}/${MicrokitCodegen.dirSrc}"
    resources = resources :+ ResourceUtil.createResource(s"${utilIncludePath}/printf.h", Util.printfh, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilSrcPath}/printf.c", Util.printfc, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilIncludePath}/util.h", Util.utilh, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilSrcPath}/util.c", Util.utilc, T)

    return CodeGenResults(resources = resources, auxResources = ISZ())
  }
}
