// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.{pacerSchedulingDomain, toolName}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitInitPlugin, MicrokitLintPlugin, MicrokitPlugin, PluginUtil}
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
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
  var resources: ISZ[Resource] = ISZ()

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

  def run(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, plugins: ISZ[Plugin], paramStore: Store, reporter: Reporter): (CodeGenResults, Store) = {
    var localStore = paramStore

    val baseTypesIncludePath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirInclude}"

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

    def processConnections(): ISZ[ConnectionStore] = {
      var ret: ISZ[ConnectionStore] = ISZ()

      val cTypeProvider = CTypePlugin.getCTypeProvider(localStore).get

      for (srcThread <- symbolTable.getThreads()) {

        for (srcPort <- srcThread.getPorts()
             if srcPort.direction == Direction.Out && symbolTable.outConnections.contains(srcPort.path)) {

          var codeContributions: Map[ISZ[String], UberConnectionContributions] = Map.empty

          for (outConnection <- symbolTable.getOutConnections(srcPort.path)) {
            symbolTable.componentMap.get(outConnection.dst.component.name).get match {
              case dstThread: AadlThread =>

                val dstPort = symbolTable.featureMap.get(outConnection.dst.feature.get.name).get

                codeContributions = codeContributions + dstThread.path ~>
                  ConnectionUtil.processInPort(dstThread, dstPort.asInstanceOf[AadlPort],
                    Some(srcPort),cTypeProvider, symbolTable)

              case x =>
                halt(s"Only handling thread to thread connections currently: $x")
            }
          } // end processing out connections for the source port

          val senderContributions = ConnectionUtil.processOutPort(srcPort, codeContributions, cTypeProvider)
          codeContributions = codeContributions + srcThread.path ~> senderContributions

          val typeApiContributions: ISZ[TypeApiContributions] =
            (Set.empty[TypeApiContributions] ++ (for (rc <- codeContributions.values) yield
              MicrokitTypeUtil.getTypeApiContributions(rc.aadlType, cTypeProvider, rc.queueSize))).elements

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = senderContributions.sharedMemoryMapping,
                  channelContributions = ISZ()),
              typeApiContributions = typeApiContributions,
              senderName = srcThread.path,
              codeContributions = codeContributions)
        } // end processing connections for source port

        // now handle unconnected ports of the source thread
        for (unconnectedPort <- srcThread.getPorts().filter(p => !symbolTable.inConnections.contains(p.path) && !symbolTable.outConnections.contains(p.path))) {
          val srcThreadContributions: UberConnectionContributions =
            if (unconnectedPort.direction == Direction.In) {
              ConnectionUtil.processInPort(
                dstThread = srcThread, dstPort = unconnectedPort,
                srcPort = None(),
                cTypeProvider = cTypeProvider,
                symbolTable = symbolTable)
            } else {
              ConnectionUtil.processOutPort(unconnectedPort, Map.empty, cTypeProvider)
            }

          val typeApiContributions =
            MicrokitTypeUtil.getTypeApiContributions(srcThreadContributions.aadlType, cTypeProvider, srcThreadContributions.queueSize)

          ret = ret :+
            DefaultConnectionStore(
              systemContributions =
                DefaultSystemContributions(
                  sharedMemoryRegionContributions = srcThreadContributions.sharedMemoryMapping,
                  channelContributions = ISZ()),
              typeApiContributions = ISZ(typeApiContributions),
              senderName = srcThread.path,
              codeContributions = Map.empty[ISZ[String], UberConnectionContributions] + srcThread.path ~> srcThreadContributions)
        }
      } // end processing connections for threads
      return ret
    }

    def processThread(t: AadlThread,
                      connectionStore: ISZ[ConnectionStore]): (MicrokitDomain, ISZ[MemoryRegion], Z) = {

      var retMemoryRegions: ISZ[MemoryRegion] = ISZ()

      val isVM = t.toVirtualMachine(symbolTable)
      val isRustic = Util.isRusty(t)

      val threadId = Util.getThreadIdPath(t)
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

        cCodeContributions = cCodeContributions(
          cBridge_GlobalVarContributions = cCodeContributions.cBridge_GlobalVarContributions :+ hostVaddr)

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
          vaddrEntries = vaddrEntries :+ v.pretty
        }
      }

      val cHeaderFileName = s"${threadId}.h"

      val cBridgeSource =
        st"""#include "$cHeaderFileName"
            |
            |${Util.doNotEdit}
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
              |${Util.safeToEdit}
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
            |${Util.doNotEdit}
            |
            |
            |${(cCodeContributions.cPortApiMethodSigs, ";\n")};
            |"""
      val cApiPath = s"${options.sel4OutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(cApiPath, cApiContent, T)

      return (child, retMemoryRegions, computeExecutionTime)
    } // end processThread

    val boundProcessors = symbolTable.getAllActualBoundProcessors()
    if (boundProcessors.size != 1) {
      reporter.error(None(), toolName, "Currently only handling models with exactly one actual bound processor")
      return (CodeGenResults.empty, localStore)
    }

    var framePeriod: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(z) => framePeriod = z
      case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
    }

    if (!reporter.hasError) { // linting phase (one pass)
      var validModel = T
      for (p <- plugins) {
        p match {
          case p: MicrokitLintPlugin =>
            val results = p.validModel(model, options, types, symbolTable, localStore, reporter)
            validModel = validModel && results._2
            localStore = results._1
          case _ =>
        }
      }

      if (!validModel) {
        return (CodeGenResults.empty, localStore)
      }
    }

    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // plugin initialization phase (one pass)
      // TODO: move to an init plugin
      val modelIsRusty: B = ops.ISZOps(symbolTable.getThreads()).exists(p => Util.isRusty(p))
      if (modelIsRusty) {
        localStore = localStore + MicrokitPlugin.KEY_MODEL_IS_RUSTY ~> BoolValue(T)
      }

      for (p <- plugins) {
        p match {
          case p: MicrokitInitPlugin =>
            localStore = p.init(model, options, types, symbolTable, localStore, reporter)
          case _ =>
        }
      }
    }

    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // main phase plugins (multi pass)
      val microkitPlugins = PluginUtil.getMicrokitPlugins(plugins)
      var continue = T
      // keep iterating until plugins stop offering to handle things
      while (continue) {
        var somethingHandled = F
        for (plugin <- microkitPlugins if continue) {
          if (plugin.canHandle(model, options, types, symbolTable, localStore, reporter)) {
            val results = plugin.handle(model, options, types, symbolTable, localStore, reporter)
            localStore = results._1
            resources = resources ++ results._2
            somethingHandled = T
          }
        }
        continue = somethingHandled
      }
    }

    var buildEntries: ISZ[ST] = ISZ()

    var typeHeaderFilenames: ISZ[String] = ISZ(MicrokitTypeUtil.cAadlTypesFilename)
    var typeImplFilenames: ISZ[String] = ISZ()
    var typeObjectNames: ISZ[String] = ISZ()

    val connectionStore = processConnections()

    for (entry <- connectionStore) {
      val srcPath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirSrc}"

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
    val allTypesPath = s"$baseTypesIncludePath/${MicrokitTypeUtil.cAllTypesFilename}"
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
      return (CodeGenResults.empty, localStore)
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

    val makefileContents = MakefileTemplate.mainMakefile(MakefileUtil.getMainMakefileTarget(localStore).elements)
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


    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // finalizing plugins
      val microkitFinalizePlugins = PluginUtil.getMicrokitFinalizePlugins(plugins)
      var continue = T
      // keep iterating until plugins stop offering to handle things
      while (continue) {
        var somethingHandled = F
        for (plugin <- microkitFinalizePlugins if continue) {
          if (plugin.canFinalize(model, options, types, symbolTable, localStore, reporter)) {
            val results = plugin.finalize(model, options, types, symbolTable, localStore, reporter)
            localStore = results._1
            resources = resources ++ results._2
            somethingHandled = T
          }
        }
        continue = somethingHandled
      }
    }

    return (CodeGenResults(resources = resources), localStore)
  }
}
