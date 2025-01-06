// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.{pacerSchedulingDomain, toolName}
import org.sireum.hamr.codegen.microkit.connections.{ConnectionContributions, ConnectionStore, TypeApiContributions}
import org.sireum.hamr.codegen.microkit.lint.Linter
import org.sireum.hamr.codegen.microkit.types.{QueueTemplate, TypeStore, TypeUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.types._
import org.sireum.hamr.codegen.microkit.util.Util.TAB
import org.sireum.hamr.codegen.microkit.vm.{VmMakefileTemplate, VmUser, VmUtil}

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

  val defaultComputeExecutionTime: Z = 10
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


    @pure def getName(ids: ISZ[String]): ST = {
      return st"${(ops.ISZOps(ids).drop(1), "_")}"
    }

    def processTypes(): Map[AadlType, TypeStore] = {
      var ret: Map[AadlType, TypeStore] = Map.empty

      val (name, _, _) = TypeUtil.processDatatype(TypeUtil.eventPortType, reporter)
      ret = ret + TypeUtil.eventPortType ~> DefaultTypeStore(typeName = name, aadlType = TypeUtil.eventPortType)

      if (types.rawConnections) {
        reporter.error(None(), MicrokitCodegen.toolName, "Raw connections are not currently supported")
        return ret
      }

      var forwardDefs: ISZ[ST] = ISZ()
      var defs: ISZ[ST] = ISZ()
      for (t <- ops.ISZOps(types.typeMap.values).sortWith((a, b) => CommonTypeUtil.isEnumTypeH(a))) {
        val (name, forwardDecl, typeDef) = TypeUtil.processDatatype(t, reporter)
        ret = ret + t ~> DefaultTypeStore(typeName = name, aadlType = t)
        if (!CommonTypeUtil.isBaseTypeH(t)) {
          if (forwardDecl.nonEmpty) {
            forwardDefs = forwardDefs :+ forwardDecl.get
          }
          defs = defs :+ typeDef
        }
      }

      val content =
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

      val outputdir = s"${options.sel4OutputDir.get}/${TypeUtil.typesDir}/${MicrokitCodegen.dirInclude}"
      val path = s"$outputdir/${TypeUtil.aadlTypesFilename}"
      resources = resources :+ ResourceUtil.createResourceH(path, content, T, T)

      return ret
    }

    def processConnections(typeStore: Map[AadlType, TypeStore]): ISZ[ConnectionStore] = {
      var ret: ISZ[ConnectionStore] = ISZ()

      for (srcThread <- symbolTable.getThreads()) {

        for (srcPort <- srcThread.getPorts()
             if srcPort.direction == Direction.Out && symbolTable.outConnections.contains(srcPort.path)) {
          var srcPutContributions: ISZ[ST] = ISZ()

          var outgoingPortType: Option[AadlType] = None()

          var typeApiContributions: ISZ[TypeApiContributions] = ISZ()

          var receiverContributions: Map[ISZ[String], ConnectionContributions] = Map.empty

          var handledQueues: Set[Z] = Set.empty

          for (outConnection <- symbolTable.getOutConnections(srcPort.path)) {
            symbolTable.componentMap.get(outConnection.dst.component.name).get match {
              case dstThread: AadlThread =>
                val dstPort = symbolTable.featureMap.get(outConnection.dst.feature.get.name).get
                val dstContributions: DefaultConnectionContributions = {
                  assert(
                    (srcPort.isInstanceOf[AadlDataPort] && dstPort.isInstanceOf[AadlDataPort]) ||
                      (srcPort.isInstanceOf[AadlEventPort] && dstPort.isInstanceOf[AadlEventPort]) ||
                      (srcPort.isInstanceOf[AadlEventDataPort] && dstPort.isInstanceOf[AadlEventDataPort]),
                    s"Connected ports must be of the same type: ${srcPort.path} -> ${dstPort.path}")

                  val isDataPort = srcPort.isInstanceOf[AadlDataPort]
                  val isEventPort = srcPort.isInstanceOf[AadlEventPort]
                  val isEventDataPort = srcPort.isInstanceOf[AadlEventDataPort]

                  val dstQueueSize: Z = if (isDataPort) 1
                  else dstPort.asInstanceOf[AadlFeatureEvent].queueSize

                  val aadlType: AadlType = (srcPort, dstPort) match {
                    case (_: AadlEventPort, _: AadlEventPort) =>
                      TypeUtil.eventPortType
                    case (srcPort: AadlEventDataPort, dstPort: AadlEventDataPort) =>
                      assert(srcPort.aadlType == dstPort.aadlType, s"Currently expecting connected ports to have the same type ${outConnection.name}")
                      srcPort.aadlType
                    case (srcPort: AadlDataPort, dstPort: AadlDataPort) =>
                      assert(srcPort.aadlType == dstPort.aadlType, s"Currently expecting connected ports to have the same type ${outConnection.name}")
                      srcPort.aadlType
                    case _ =>
                      halt("Infeasible")
                  }

                  val typeName = typeStore.get(aadlType).get

                  if (outgoingPortType.isEmpty) {
                    outgoingPortType = Some(aadlType)
                  } else {
                    assert (outgoingPortType.get == aadlType)
                  }

                  if (!handledQueues.contains(dstQueueSize)) {
                    typeApiContributions = typeApiContributions :+ TypeUtil.getTypeApiContributions(aadlType, typeName, dstQueueSize)

                    val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, dstQueueSize)
                    srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
                      sharedMemoryVarName = sharedMemVarName,
                      queueElementTypeName = typeName.typeName,
                      queueSize = dstQueueSize,
                      isEventPort = isEventPort)

                    handledQueues = handledQueues + dstQueueSize
                  }

                  val sharedMemTypeName = QueueTemplate.getTypeQueueTypeName(typeName.typeName, dstQueueSize)

                  val sharedVarName = QueueTemplate.getClientDequeueSharedVarName(dstPort.identifier, dstQueueSize)
                  val recvQueueTypeName = QueueTemplate.getClientRecvQueueTypeName(typeName.typeName, dstQueueSize)
                  val recvQueueVarName = QueueTemplate.getClientRecvQueueName(dstPort.identifier)

                  var methodApiSigs: ISZ[ST] = ISZ()
                  var methodApis: ISZ[ST] = ISZ()
                  if (isEventPort || isEventDataPort) {
                    methodApiSigs = methodApiSigs :+
                      QueueTemplate.getClientIsEmptyMethodSig(dstPort.identifier) :+
                      QueueTemplate.getClientGetterMethodPollSig(dstPort.identifier, typeName.typeName, isEventPort) :+
                      QueueTemplate.getClientGetterMethodSig(dstPort.identifier, typeName.typeName, isEventPort)
                    methodApis = methodApis :+
                      QueueTemplate.getClientIsEmptyMethod(dstPort.identifier, typeName.typeName, dstQueueSize) :+
                      QueueTemplate.getClientGetterMethodPoll(dstPort.identifier, typeName.typeName, dstQueueSize, isEventPort) :+
                      QueueTemplate.getClientGetterMethod(dstPort.identifier, typeName.typeName, isEventPort)
                  } else {
                    methodApiSigs = methodApiSigs :+
                      QueueTemplate.getClientGetterMethodSig(dstPort.identifier, typeName.typeName, F)
                    methodApis = methodApis :+
                      QueueTemplate.getClientDataGetterMethod(dstPort.identifier, typeName.typeName, dstQueueSize, aadlType)
                  }

                  var userMethodSignatures: ISZ[ST] = ISZ()
                  var userMethodDefaultImpls: ISZ[ST] = ISZ()
                  var computeContributions: ISZ[ST] = ISZ()
                  if (!dstThread.toVirtualMachine(symbolTable)) {
                    if (dstThread.isSporadic()) {
                      userMethodSignatures = userMethodSignatures :+ QueueTemplate.getClientEventHandlerMethodSig(dstPort.identifier)

                      if (isEventPort || isEventDataPort) {
                        computeContributions = computeContributions :+ QueueTemplate.getClientSporadicComputeContributions(dstPort.identifier)
                      }

                      userMethodDefaultImpls = userMethodDefaultImpls :+ QueueTemplate.getClientSporadicDefaultImplContributions(dstPort.identifier)
                    }
                  }

                  DefaultConnectionContributions(
                    portName = dstPort.path,
                    portPriority = None(),
                    headerImportContributions = ISZ(),
                    implementationImportContributions = ISZ(),
                    userMethodSignatures = userMethodSignatures,
                    userMethodDefaultImpls = userMethodDefaultImpls,
                    defineContributions = ISZ(),
                    globalVarContributions = ISZ(
                      PortVaddr(s"volatile $sharedMemTypeName", s"*$sharedVarName"),
                      QueueVaddr(recvQueueTypeName, recvQueueVarName)
                    ),
                    apiMethodSigs = methodApiSigs,
                    apiMethods = methodApis,
                    initContributions = ISZ(QueueTemplate.getClientRecvInitMethodCall(dstPort.identifier, typeName.typeName, dstQueueSize)),
                    computeContributions = computeContributions,
                    sharedMemoryMapping = ISZ(
                      PortSharedMemoryRegion(
                        outgoingPortPath = srcPort.path,
                        queueSize = dstQueueSize,
                        varAddr = sharedVarName,
                        perms = ISZ(Perm.READ),
                        sizeInKiBytes = Util.defaultPageSizeInKiBytes,
                        physicalAddressInKiBytes = None())
                    )
                  )
                }
                receiverContributions = receiverContributions + dstThread.path ~> dstContributions
              case x =>
                halt(s"Only handling thread to thread connections currently: $x")
            }
          }

          val sPortType: String = typeStore.get(outgoingPortType.get).get.typeName
          val isEventPort = outgoingPortType.get == TypeUtil.eventPortType

          val apiMethodSig = QueueTemplate.getClientPutMethodSig(srcPort.identifier, sPortType, isEventPort)
          val apiMethod = QueueTemplate.getClientPutMethod(srcPort.identifier, sPortType, srcPutContributions, isEventPort)

          var sharedMemoryMappings: ISZ[MemoryRegion] = ISZ()
          var sharedMemoryVars: ISZ[GlobalVarContribution] = ISZ()
          var initContributions: ISZ[ST] = ISZ()
          for (queueSize <- handledQueues.elements) {
            val queueType = QueueTemplate.getTypeQueueTypeName(sPortType, queueSize)
            val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
            sharedMemoryVars = sharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

            initContributions = initContributions :+ QueueTemplate.getQueueInitMethod(varName, sPortType, queueSize)

            sharedMemoryMappings = sharedMemoryMappings :+
              PortSharedMemoryRegion(
                outgoingPortPath = srcPort.path,
                queueSize = queueSize,
                varAddr = varName,
                perms = ISZ(Perm.READ, Perm.WRITE),
                sizeInKiBytes = Util.defaultPageSizeInKiBytes,
                physicalAddressInKiBytes = None()
              )
          }

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = sharedMemoryMappings,
                  channelContributions = ISZ()),
              typeApiContributions = typeApiContributions,
              senderName = srcThread.path,
              senderContributions =
                DefaultConnectionContributions(
                  portName = srcPort.path,
                  portPriority = None(),
                  headerImportContributions = ISZ(),
                  implementationImportContributions = ISZ(),
                  userMethodSignatures = ISZ(),
                  userMethodDefaultImpls = ISZ(),
                  defineContributions = ISZ(),
                  globalVarContributions = sharedMemoryVars,
                  apiMethodSigs = ISZ(apiMethodSig),
                  apiMethods = ISZ(apiMethod),
                  initContributions = initContributions,
                  computeContributions = ISZ(),
                  sharedMemoryMapping = sharedMemoryMappings),
              receiverContributions = receiverContributions)
        }
      }
      return ret
    }

    def processThread(t: AadlThread,
                      connectionStore: ISZ[ConnectionStore]):
    (MicrokitDomain, ISZ[MemoryRegion], Z) = {

      var retMemoryRegions: ISZ[MemoryRegion] = ISZ()

      val isVM = t.toVirtualMachine(symbolTable)

      val threadId = getName(t.path)
      val threadMonId = st"${getName(t.path)}_MON"

      var headerImports: ISZ[String] = ISZ()
      var userMethodSignatures: ISZ[ST] = ISZ()
      var userMethodDefaultImpls: ISZ[ST] = ISZ()
      var vaddrs: ISZ[GlobalVarContribution] = ISZ()
      var codeApiMethodSigs: ISZ[ST] = ISZ()
      var codeApiMethods: ISZ[ST] = ISZ()
      var initContributions: ISZ[ST] = ISZ()
      var computeContributions: ISZ[ST] = ISZ()
      var nextMemAddressInKiBytes = 262144
      var sharedMemoryRegions: ISZ[MemoryRegion] = ISZ()

      val initializeMethodName = st"${threadId}_initialize"

      for (entry <- connectionStore) {

        if (entry.senderName == t.path) {
          headerImports = headerImports ++ entry.senderContributions.headerImportContributions
          userMethodSignatures = userMethodSignatures ++ entry.senderContributions.userMethodSignatures
          userMethodDefaultImpls = userMethodDefaultImpls ++ entry.senderContributions.userMethodDefaultImpls
          codeApiMethodSigs = codeApiMethodSigs ++ entry.senderContributions.apiMethodSigs
          codeApiMethods = codeApiMethods ++ entry.senderContributions.apiMethods
          vaddrs = vaddrs ++ entry.senderContributions.globalVarContributions
          initContributions = initContributions ++ entry.senderContributions.initContributions
          computeContributions = computeContributions ++ entry.senderContributions.computeContributions
          sharedMemoryRegions = sharedMemoryRegions ++ entry.senderContributions.sharedMemoryMapping
        }
        if (entry.receiverContributions.contains(t.path)) {
          val c = entry.receiverContributions.get(t.path).get
          headerImports = headerImports ++ c.headerImportContributions
          userMethodSignatures = userMethodSignatures ++ c.userMethodSignatures
          userMethodDefaultImpls = userMethodDefaultImpls ++ c.userMethodDefaultImpls
          codeApiMethodSigs = codeApiMethodSigs ++ c.apiMethodSigs
          codeApiMethods = codeApiMethods ++ c.apiMethods
          vaddrs = vaddrs ++ c.globalVarContributions
          initContributions = initContributions ++ c.initContributions
          computeContributions = computeContributions ++ c.computeContributions
          sharedMemoryRegions = sharedMemoryRegions ++ c.sharedMemoryMapping
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
        resourceSuffix = threadId.render,
        relativePath = Some(s"${MicrokitCodegen.dirComponents}/${threadId.render}"),
        hasHeader = T,
        isVM = isVM,
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
        val vmName = s"${threadId.render}"

        val guestRam = VirtualMachineMemoryRegion(
          typ = VirtualMemoryRegionType.RAM,
          threadPath = t.path,
          sizeInKiBytes = Util.defaultVmRamSizeInKiBytes,
          physicalAddressInKiBytes = None()
        )

        val hostVaddr = VMRamVaddr("uintptr_t", guestRam.vmmVaddrName)

        vaddrs = vaddrs :+ hostVaddr

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

        val vmmMake = VmMakefileTemplate.Makefile(threadId.render)
        resources = resources :+ ResourceUtil.createResource(s"${boardPath}/Makefile", vmmMake, T)


        val vmm_config = VmUtil.vmm_config(
          guestDtbVaddrInHex = "0x4f000000",
          guestInitRamDiskVaddrInHex = "0x4d700000",
          maxIrqs = 1
        )
        resources = resources :+ ResourceUtil.createResource(s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${threadId.render}_user.h", vmm_config, T)
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
            name = threadId.render,
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
      val threadMonCaps = s"PORT_TO_${ops.StringOps(threadMonId.render).toUpper}"

      codePacerDefines = codePacerDefines :+ st"""#define $threadMonCaps $pacerChannelId"""
      codePacerPings = codePacerPings :+
        st"""case ${threadMonCaps}:
            |  microkit_notify($threadMonCaps);
            |  break;"""

      val monChannelId = getNextPacerChannelId

      xmlChannels = xmlChannels :+ Channel(
        firstPD = threadMonId.render, firstId = monChannelId,
        secondPD = threadId.render, secondId = monChannelId)

      val headerFileName = s"${threadId.render}.h"

      val monImplSource =
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

      val monImplPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.monImplFilename}"
      resources = resources :+ ResourceUtil.createResource(monImplPath, monImplSource, T)

      val userNotifyMethodName = st"${threadId}_notify"
        initContributions = initContributions :+ st"$initializeMethodName();"
        userMethodSignatures = st"void ${initializeMethodName}(void)" +: userMethodSignatures
        userMethodSignatures = userMethodSignatures :+ st"void ${userNotifyMethodName}(microkit_channel channel)"

        userMethodDefaultImpls =
          st"""void $initializeMethodName(void) {
              |  printf("%s: $initializeMethodName invoked\n", microkit_name);
              |}""" +: userMethodDefaultImpls

        if (t.isPeriodic()) {
          val timeTriggeredMethodName = st"${threadId}_timeTriggered"
          userMethodSignatures = userMethodSignatures :+ st"void ${timeTriggeredMethodName}(void)"
          computeContributions = computeContributions :+ st"${timeTriggeredMethodName}();"
          userMethodDefaultImpls = userMethodDefaultImpls :+
            st"""void $timeTriggeredMethodName(void) {
                |  printf("%s: $timeTriggeredMethodName invoked\n", microkit_name);
                |}"""
        }

      userMethodDefaultImpls = userMethodDefaultImpls :+
        st"""void $userNotifyMethodName(microkit_channel channel) {
            |  // this method is called when the monitor does not handle the passed in channel
            |  switch (channel) {
            |    default:
            |      printf("%s: Unexpected channel %d\n", microkit_name, channel);
            |  }
            |}"""

      var vaddrEntries: ISZ[ST] = ISZ()
      for (v <- vaddrs) {
        if (!v.isInstanceOf[VMRamVaddr]) {
          vaddrEntries = vaddrEntries :+ v.pretty
        }
      }

      val implSource =
        st"""#include "$headerFileName"
            |
            |${Util.doNotEdit}
            |
            |${(for (u <- userMethodSignatures) yield st"$u;", "\n")}
            |
            |${(vaddrEntries, "\n")}
            |
            |#define PORT_FROM_MON $monChannelId
            |
            |${(codeApiMethods, "\n\n")}
            |
            |void init(void) {
            |  ${(initContributions, "\n\n")}
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_FROM_MON:
            |      ${(computeContributions, "\n\n")}
            |      break;
            |    default:
            |      ${userNotifyMethodName}(channel);
            |  }
            |}
            |"""

      val implPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(implPath, implSource, T)

      val userImplPath = s"${options.sel4OutputDir.get}/${mk.relativePathSrcDir}/${mk.cUserImplFilename}"
      val userImplSource: ST =
        if (isVM) {
          val cand = vaddrs.filter(f => f.isInstanceOf[VMRamVaddr])
          assert (cand.size == 1, s"didn't find a guest ram vaddr for ${t.identifier}: ${cand.size}")
          VmUser.vmUserCode(threadId, cand(0).pretty)
        } else {
          st"""#include "$headerFileName"
              |
              |${Util.safeToEdit}
              |
              |${(userMethodDefaultImpls, "\n\n")}
              |"""
        }
      resources = resources :+ ResourceUtil.createResource(userImplPath, userImplSource, F)

      val utilIncludes: ST =
        if (isVM)
          st"""#include <libvmm/util/printf.h>
              |#include <libvmm/util/util.h>"""
        else
          st"""#include <printf.h>
              |#include <util.h>"""

      val himports: ISZ[ST] = for (h <- headerImports) yield st"#include <$h>"
      val headerSource =
        st"""#pragma once
            |
            |$utilIncludes
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <${TypeUtil.allTypesFilename}>
            |
            |${Util.doNotEdit}
            |
            |${(himports, "\n")}
            |
            |${(codeApiMethodSigs, ";\n")};
            |"""
      val headerPath = s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(headerPath, headerSource, T)

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

    var typeHeaderFilenames: ISZ[String] = ISZ(TypeUtil.aadlTypesFilename)
    var typeImplFilenames: ISZ[String] = ISZ()
    var typeObjectNames: ISZ[String] = ISZ()

    val baseTypesIncludePath = s"${options.sel4OutputDir.get}/${TypeUtil.typesDir}/${MicrokitCodegen.dirInclude}"

    val connectionStore = processConnections(typeStore)
    for (entry <- connectionStore) {
      val srcPath = s"${options.sel4OutputDir.get}/${TypeUtil.typesDir}/${MicrokitCodegen.dirSrc}"

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

    val eventCounterPath = s"$baseTypesIncludePath/${TypeUtil.eventCounterFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = eventCounterPath, content = TypeUtil.eventCounterContent, overwrite = T, isDatatype = T)

    val allTypesContent =
      st"""#pragma once
          |
          |${Util.doNotEdit}
          |
          |${(for (i <- typeHeaderFilenames) yield st"#include <$i>", "\n")}
          |"""
    val allTypesPath = s"$baseTypesIncludePath/${TypeUtil.allTypesFilename}"
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
    for (x <- ops.ISZOps(xmlSchedulingDomains).sortWith((a,b) => a.id < b.id)) {
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
