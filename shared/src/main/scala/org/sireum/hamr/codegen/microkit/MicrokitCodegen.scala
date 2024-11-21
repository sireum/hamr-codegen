// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.connections.{ConnectionContributions, ConnectionStore, TypeApiContributions}
import org.sireum.hamr.codegen.microkit.lint.Linter
import org.sireum.hamr.codegen.microkit.types.{QueueTemplate, TypeStore, TypeUtil}
import org.sireum.hamr.codegen.microkit.util.{Channel, MakefileContainer, MemoryMap, MemoryRegion, Perm, PortSharedMemoryRegion, ProtectionDomain, SchedulingDomain, SharedMemoryRegion, SystemDescription, Util}
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.types._
import org.sireum.hamr.codegen.microkit.util.Util.TAB

object MicrokitCodegen {
  val toolName: String = "Mircokit Codegen"

  val microkitSystemXmlFilename: String = "microkit.system"
  val systemMakeFilename: String = "system.mk"

  val pacerName: String = "pacer"

  val pacerSchedulingDomain: String = "domain1"

  val dirComponents: String = "components"
  val dirInclude: String = "include"
  val dirSrc: String = "src"

  val pacerComputeExecutionTime: Z = 10
  val endOfFrameComputExecutionTime: Z = 10
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
          schedulingDomain = Some("domain1"),
          id = None(),
          stackSizeInKiBytes = None(),
          memMaps = ISZ(),
          programImage = s"${MicrokitCodegen.pacerName}.elf",
          children = ISZ())

      portPacerToEndOfFrame = getNextPacerChannelId
      portEndOfFrameToPacer = getNextPacerChannelId

      val mk = MakefileContainer(resourceSuffix = MicrokitCodegen.pacerName, relativePath = Some(s"${MicrokitCodegen.dirComponents}/${ops.StringOps(MicrokitCodegen.pacerName).toLower}"), hasHeader = F, hasUserContent = F)
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
      val path = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
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
        st"""#ifndef ${Util.toPreprocessorName(TypeUtil.aadlTypesFilename)}
            |#define ${Util.toPreprocessorName(TypeUtil.aadlTypesFilename)}
            |
            |#include <stdint.h>
            |
            |${(forwardDefs, "\n\n")}
            |
            |${(defs, "\n\n")}
            |
            |#endif
            |"""

      val outputdir = s"${options.camkesOutputDir.get}/${MicrokitCodegen.dirInclude}"
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
                  if (dstThread.isSporadic()) {
                    userMethodSignatures = userMethodSignatures :+ QueueTemplate.getClientEventHandlerMethodSig(dstPort.identifier)

                    if (isEventPort || isEventDataPort) {
                      computeContributions = computeContributions :+ QueueTemplate.getClientSporadicComputeContributions(dstPort.identifier)
                    }

                    userMethodDefaultImpls = userMethodDefaultImpls :+ QueueTemplate.getClientSporadicDefaultImplContributions(dstPort.identifier)
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
                      (s"volatile $sharedMemTypeName", s"*$sharedVarName"),
                      (recvQueueTypeName, recvQueueVarName)
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
                        dataSizeInKiBytes = Util.defaultPageSizeInKiBytes)
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

          var sharedMemoryMappings: ISZ[SharedMemoryRegion] = ISZ()
          var sharedMemoryVars: ISZ[(String, String)] = ISZ()
          var initContributions: ISZ[ST] = ISZ()
          for (queueSize <- handledQueues.elements) {
            val queueType = QueueTemplate.getTypeQueueTypeName(sPortType, queueSize)
            val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
            sharedMemoryVars = sharedMemoryVars :+ (s"volatile $queueType", s"*$varName")

            initContributions = initContributions :+ QueueTemplate.getQueueInitMethod(varName, sPortType, queueSize)

            sharedMemoryMappings = sharedMemoryMappings :+
              PortSharedMemoryRegion(
                outgoingPortPath = srcPort.path,
                queueSize = queueSize,
                varAddr = varName,
                perms = ISZ(Perm.READ, Perm.WRITE),
                dataSizeInKiBytes = Util.defaultPageSizeInKiBytes
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

    def processThread(t: AadlThread, connectionStore: ISZ[ConnectionStore]): (ProtectionDomain, Z) = {
      val threadId = getName(t.path)
      val threadMonId = st"${getName(t.path)}_MON"

      var headerImports: ISZ[String] = ISZ()
      var userMethodSignatures: ISZ[ST] = ISZ()
      var userMethodDefaultImpls: ISZ[ST] = ISZ()
      var vaddrs: ISZ[(String, String)] = ISZ()
      var codeApiMethodSigs: ISZ[ST] = ISZ()
      var codeApiMethods: ISZ[ST] = ISZ()
      var initContributions: ISZ[ST] = ISZ()
      var computeContributions: ISZ[ST] = ISZ()
      var nextMemAddressInKiBytes = 262144
      var sharedMemoryRegions: ISZ[SharedMemoryRegion] = ISZ()

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

      val schedulingDomain: String = t.getDomain(symbolTable) match {
        case Some(d) =>
          if (d == 0 || d == 1) { // TODO what's upper bound
            reporter.error(t.component.identifier.pos, toolName, s"Domain '$d' is reserved")
          }
          if (d > largestSchedulingDomain) {
            largestSchedulingDomain = d
          }
          s"domain$d"
        case _ => halt("Infeasible")
      }

      val mk = MakefileContainer(resourceSuffix = threadId.render, relativePath = Some(s"${MicrokitCodegen.dirComponents}/${threadId.render}"), hasHeader = T, hasUserContent = T)
      makefileContainers = makefileContainers :+ mk

      val computeExecutionTime: Z = t.getComputeExecutionTime() match {
        case Some((l, h)) =>
          assert(l <= h, s"low must be <= high: $l <= $h")
          h
        case _ => 100
      }

      xmlSchedulingDomains = xmlSchedulingDomains :+
        SchedulingDomain(name = schedulingDomain, length = computeExecutionTime)

      var childMemMaps: ISZ[MemoryMap] = ISZ()
      for (r <- sharedMemoryRegions) {
        r match {
          case p: PortSharedMemoryRegion =>

            childMemMaps = childMemMaps :+ MemoryMap(
              memoryRegion = p.regionName,
              vaddrInKiBytes = nextMemAddressInKiBytes,
              perms = p.perms,
              varAddr = p.varAddr)
            nextMemAddressInKiBytes = nextMemAddressInKiBytes + p.dataSizeInKiBytes
          case _ => halt("")
        }
      }

      val stackSizeInKiBytes: Option[Z] = t.stackSizeInBytes() match {
        case Some(bytes) => Some(Util.bytesToKiBytes(bytes))
        case _ => None()
      }

      val child = ProtectionDomain(
        name = threadId.render,
        schedulingDomain = Some(schedulingDomain),
        id = Some(s"1"),
        stackSizeInKiBytes = stackSizeInKiBytes,
        memMaps = childMemMaps,
        programImage = mk.elfName,
        children = ISZ())

      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = threadMonId.render,
          schedulingDomain = Some(schedulingDomain),
          id = None(),
          stackSizeInKiBytes = None(),
          memMaps = ISZ(),
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
        st"""#include "$headerFileName"
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

      val monImplPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.monImplFilename}"
      resources = resources :+ ResourceUtil.createResource(monImplPath, monImplSource, T)

      val initializeMethodName = st"${threadId}_initialize"
      userMethodSignatures = st"void ${initializeMethodName}(void)" +: userMethodSignatures

      userMethodDefaultImpls =
        st"""void $initializeMethodName(void) {
            |  // implement me
            |}""" +: userMethodDefaultImpls

      if (t.isPeriodic()) {
        val timeTriggeredMethodName = st"${threadId}_timeTriggered"
        userMethodSignatures = userMethodSignatures :+ st"void ${timeTriggeredMethodName}(void)"
        computeContributions = computeContributions :+ st"${timeTriggeredMethodName}();"
        userMethodDefaultImpls = userMethodDefaultImpls :+
          st"""void $timeTriggeredMethodName(void) {
              |  // implement me
              |}"""
      }

      val vaddrEntries: ISZ[ST] = for (v <- vaddrs) yield st"${v._1} ${v._2};"

      val implSource =
        st"""#include "$headerFileName"
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
            |
            |  $initializeMethodName();
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_FROM_MON:
            |      ${(computeContributions, "\n\n")}
            |      break;
            |  }
            |}
            |"""

      val implPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(implPath, implSource, T)

      val userImplSource =
        st"""#include "$headerFileName"
            |
            |${(userMethodDefaultImpls, "\n\n")}
            |"""
      val userImplPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cUserImplFilename}"
      resources = resources :+ ResourceUtil.createResource(userImplPath, userImplSource, F)

      val himports: ISZ[ST] = for (h <- headerImports) yield st"#include <$h>"
      val headerSource =
        st"""#include <printf.h>
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <${TypeUtil.allTypesFilename}>
            |${(himports, "\n")}
            |
            |${(codeApiMethodSigs, ";\n")};
            |"""
      val headerPath = s"${options.camkesOutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(headerPath, headerSource, T)

      return (child, computeExecutionTime)
    } // end processThread


    if (!Linter.lint(model, options, types, symbolTable, reporter)) {
      return CodeGenResults(ISZ(), ISZ())
    }

    val boundProcessors = symbolTable.getAllBoundProcessors()
    if (boundProcessors.size != 1) {
      reporter.error(None(), toolName, "Currently handling models with exactly one bound processor")
      return CodeGenResults(ISZ(), ISZ())
    }
    var framePeriod: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(z) => framePeriod = z
      case _ => halt("Infeasible") // linter should have ensured bound processor has frame period
    }

    var buildEntries: ISZ[ST] = ISZ()

    val typeStore = processTypes()

    var typeHeaderFilenemes: ISZ[String] = ISZ(TypeUtil.aadlTypesFilename)
    var typeImplFilenames: ISZ[String] = ISZ()
    var objectNames: ISZ[String] = ISZ()

    val baseIncludePath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.dirInclude}"

    val connectionStore = processConnections(typeStore)
    for (entry <- connectionStore) {
      val srcPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.dirSrc}"

      for (tc <- entry.typeApiContributions) {
        typeHeaderFilenemes = typeHeaderFilenemes :+ tc.headerFilename
        typeImplFilenames = typeImplFilenames :+ tc.implementationFilename
        objectNames = objectNames :+ tc.objectName
        buildEntries = buildEntries :+ tc.buildEntry

        val headerPath = s"$baseIncludePath/${tc.headerFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = headerPath, content = tc.header, overwrite = T, isDatatype = T)

        val implPath = s"$srcPath/${tc.implementationFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = implPath, content = tc.implementation, overwrite = T, isDatatype = T)
      }
    }

    val eventCounterPath = s"$baseIncludePath/${TypeUtil.eventCounterFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = eventCounterPath, content = TypeUtil.eventCounterContent, overwrite = T, isDatatype = T)

    val allTypesContent =
      st"""#ifndef ${Util.toPreprocessorName(TypeUtil.allTypesFilename)}
          |#define ${Util.toPreprocessorName(TypeUtil.allTypesFilename)}
          |
          |${(for (i <- typeHeaderFilenemes) yield st"#include <$i>", "\n")}
          |
          |#endif"""
    val allTypesPath = s"$baseIncludePath/${TypeUtil.allTypesFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = allTypesPath, content = allTypesContent, overwrite = T, isDatatype = T)

    var usedBudget: Z = 0
    for (t <- symbolTable.getThreads()) {
      val results = processThread(t, connectionStore)
      usedBudget = usedBudget + results._2
    }

    addPacerComponent()

    val pacerSlot = SchedulingDomain(name = MicrokitCodegen.pacerSchedulingDomain, length = MicrokitCodegen.pacerComputeExecutionTime)
    val currentScheduleSize = xmlSchedulingDomains.size
    usedBudget = usedBudget + (currentScheduleSize * MicrokitCodegen.pacerComputeExecutionTime)

    if (usedBudget > framePeriod) {
      reporter.error(None(), toolName, s"Frame period ${framePeriod} is too small for the used budget ${usedBudget}")
      return CodeGenResults(ISZ(), ISZ())
    }

    var xmlScheds: ISZ[SchedulingDomain] = ISZ()
    for (x <- xmlSchedulingDomains) {
      xmlScheds = xmlScheds :+ pacerSlot :+ x
    }
    xmlScheds = xmlScheds :+ SchedulingDomain(name = "domain0", length = framePeriod - usedBudget)

    var memoryRegions: ISZ[MemoryRegion] = ISZ()
    for (e <- connectionStore;
         s <- e.systemContributions.sharedMemoryRegionContributions) {
      memoryRegions = memoryRegions :+ MemoryRegion(name = s.regionName, sizeInKiBytes = s.dataSizeInKiBytes)
    }

    val sd = SystemDescription(
      schedulingDomains = xmlScheds,
      protectionDomains = xmlProtectionDomains,
      memoryRegions = memoryRegions,
      channels = xmlChannels)

    val xmlPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.microkitSystemXmlFilename}"
    resources = resources :+ ResourceUtil.createResource(path = xmlPath, content = sd.prettyST, overwrite = T)

    val sysDot = sd.toDot
    val dotPath = s"${options.camkesOutputDir.get}/microkit.dot"
    resources = resources :+ ResourceUtil.createResource(path = dotPath, content = sysDot, overwrite = T)

    val makefileContents =
      st"""ifeq ($$(strip $$(MICROKIT_SDK)),)
          |$$(error MICROKIT_SDK must be specified)
          |endif
          |override MICROKIT_SDK := $$(abspath $${MICROKIT_SDK})
          |
          |BUILD_DIR ?= build
          |# By default we make a debug build so that the client debug prints can be seen.
          |MICROKIT_CONFIG ?= debug
          |
          |export CPU := cortex-a53
          |QEMU := qemu-system-aarch64
          |
          |CC := clang
          |LD := ld.lld
          |export MICROKIT_TOOL ?= $$(abspath $$(MICROKIT_SDK)/bin/microkit)
          |
          |export BOARD_DIR := $$(abspath $$(MICROKIT_SDK)/board/qemu_virt_aarch64/debug)
          |export TOP:= $$(abspath $$(dir $${MAKEFILE_LIST}))
          |IMAGE_FILE := $$(BUILD_DIR)/loader.img
          |REPORT_FILE := $$(BUILD_DIR)/report.txt
          |
          |all: $${IMAGE_FILE}
          |
          |qemu $${IMAGE_FILE} $${REPORT_FILE} clean clobber: $$(IMAGE_FILE) $${BUILD_DIR}/Makefile FORCE
          |${TAB}$${MAKE} -C $${BUILD_DIR} MICROKIT_SDK=$${MICROKIT_SDK} $$(notdir $$@)
          |
          |$${BUILD_DIR}/Makefile: ${MicrokitCodegen.systemMakeFilename}
          |${TAB}mkdir -p $${BUILD_DIR}
          |${TAB}cp ${MicrokitCodegen.systemMakeFilename} $${BUILD_DIR}/Makefile
          |
          |FORCE:
          |"""
    val makefilePath = s"${options.camkesOutputDir.get}/Makefile"
    resources = resources :+ ResourceUtil.createResource(makefilePath, makefileContents, T)

    buildEntries = buildEntries ++ (for (mk <- makefileContainers) yield mk.buildEntry)

    var elfFiles: ISZ[String] = ISZ()
    var oFiles: ISZ[String] = ISZ()
    for (mk <- makefileContainers) {
      elfFiles = elfFiles ++ mk.getElfNames
      oFiles = oFiles ++ mk.getObjNames
    }

    val elfEntries: ISZ[ST] = for (mk <- makefileContainers) yield mk.elfEntry

    val systemmkContents =
      st"""ifeq ($$(strip $$(MICROKIT_SDK)),)
          |$$(error MICROKIT_SDK must be specified)
          |endif
          |
          |MICROKIT_TOOL ?= $$(MICROKIT_SDK)/bin/microkit
          |
          |ifeq ("$$(wildcard $$(MICROKIT_TOOL))","")
          |$$(error Microkit tool not found at $${MICROKIT_TOOL})
          |endif
          |
          |ifeq ($$(strip $$(MICROKIT_BOARD)),)
          |$$(error MICROKIT_BOARD must be specified)
          |endif
          |
          |BUILD_DIR ?= build
          |# By default we make a debug build so that the client debug prints can be seen.
          |MICROKIT_CONFIG ?= debug
          |
          |QEMU := qemu-system-aarch64
          |
          |CC := clang
          |LD := ld.lld
          |AR := llvm-ar
          |RANLIB := llvm-ranlib
          |
          |CFLAGS := -mcpu=$$(CPU) \
          |${TAB}-mstrict-align \
          |${TAB}-nostdlib \
          |${TAB}-ffreestanding \
          |${TAB}-g3 \
          |${TAB}-O3 \
          |${TAB}-Wall -Wno-unused-function -Werror -Wno-unused-command-line-argument \
          |${TAB}-target aarch64-none-elf \
          |${TAB}-I$$(BOARD_DIR)/include
          |LDFLAGS := -L$$(BOARD_DIR)/lib
          |LIBS := --start-group -lmicrokit -Tmicrokit.ld --end-group
          |
          |
          |${TypeUtil.make_TYPE_OBJS} := printf.o util.o ${(objectNames, " ")}
          |
          |SYSTEM_FILE := $${TOP}/${MicrokitCodegen.microkitSystemXmlFilename}
          |
          |IMAGES := ${(elfFiles, " ")}
          |IMAGE_FILE = loader.img
          |REPORT_FILE = report.txt
          |
          |all: $$(IMAGE_FILE)
          |${TAB}CHECK_FLAGS_BOARD_MD5:=.board_cflags-$$(shell echo -- $${CFLAGS} $${BOARD} $${MICROKIT_CONFIG}| shasum | sed 's/ *-//')
          |
          |$${CHECK_FLAGS_BOARD_MD5}:
          |${TAB}-rm -f .board_cflags-*
          |${TAB}touch $$@
          |
          |%.o: $${TOP}/%.c Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include
          |
          |printf.o: $${TOP}/${MicrokitCodegen.dirSrc}/printf.c Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include
          |
          |util.o: $${TOP}/${MicrokitCodegen.dirSrc}/util.c Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include
          |
          |${(buildEntries, "\n\n")}
          |
          |${(elfEntries, "\n\n")}
          |
          |$$(IMAGE_FILE): $$(IMAGES) $$(SYSTEM_FILE)
          |${TAB}$$(MICROKIT_TOOL) $$(SYSTEM_FILE) --search-path $$(BUILD_DIR) --board $$(MICROKIT_BOARD) --config $$(MICROKIT_CONFIG) -o $$(IMAGE_FILE) -r $$(REPORT_FILE)
          |
          |
          |qemu: $$(IMAGE_FILE)
          |${TAB}$$(QEMU) -machine virt,virtualization=on \
          |${TAB}${TAB}${TAB}-cpu cortex-a53 \
          |${TAB}${TAB}${TAB}-serial mon:stdio \
          |${TAB}${TAB}${TAB}-device loader,file=$$(IMAGE_FILE),addr=0x70000000,cpu-num=0 \
          |${TAB}${TAB}${TAB}-m size=2G \
          |${TAB}${TAB}${TAB}-nographic
          |
          |clean::
          |${TAB}rm -f ${(oFiles, " ")}
          |
          |clobber:: clean
          |${TAB}rm -f ${(elfFiles, " ")} $${IMAGE_FILE} $${REPORT_FILE}
          |"""
    val systemmkPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.systemMakeFilename}"
    resources = resources :+ ResourceUtil.createResource(systemmkPath, systemmkContents, T)


    val includePath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.dirInclude}"
    val srcPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.dirSrc}"
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/printf.h", Util.printfh, T)
    resources = resources :+ ResourceUtil.createResource(s"${srcPath}/printf.c", Util.printfc, T)
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/util.h", Util.utilh, T)
    resources = resources :+ ResourceUtil.createResource(s"${srcPath}/util.c", Util.utilc, T)

    return CodeGenResults(resources = resources, auxResources = ISZ())
  }
}
