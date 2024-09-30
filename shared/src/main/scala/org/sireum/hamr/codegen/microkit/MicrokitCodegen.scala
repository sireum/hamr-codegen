// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlPort, AadlThread, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.lint.Linter
import org.sireum.hamr.codegen.microkit.util.{SystemDescription, Channel, MakefileContainer, MemoryMap, MemoryRegion, Perm, ProtectionDomain, SchedulingDomain, TypeUtil, Util}
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

import scala.:+

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

  @pure def hexFormat(z: Z): String = {
    val s = conversions.String.toCis(z.string)
    var ret: ISZ[C] = ISZ(s(s.lastIndex))
    for (i <- 1 until s.size) {
      if (i % 3 == 0) {
        ret = '_' +: ret
      }
      ret = s(s.size - 1 - i) +: ret
    }
    return s"0x${conversions.String.fromCis(ret)}"
  }
}

@record class MicrokitCodegen {
  var resources: ISZ[FileResource] = ISZ()
  var makefileContainers: ISZ[MakefileContainer] = ISZ()

  var xmlSchedulingDomains: ISZ[SchedulingDomain] = ISZ()
  var xmlProtectionDomains: ISZ[ProtectionDomain] = ISZ()
  var xmlMemoryRegions: HashSMap[String, MemoryRegion] = HashSMap.empty
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

  var cFileSuffixes: ISZ[String] = ISZ()

  def run(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): CodeGenResults = {

    def addPacerComponent(): Unit = {
      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = MicrokitCodegen.pacerName,
          schedulingDomain = Some("domain1"),
          id = None(),
          stackSize = None(),
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

    def createMemoryRegion(senderIdPath: ISZ[String]): MemoryRegion = {
      val name = getName(senderIdPath)
      if (xmlMemoryRegions.contains(name.render)) {
        return xmlMemoryRegions.get(name.render).get
      }
      val mr = MemoryRegion(name = name.render, size = MicrokitCodegen.hexFormat(1000))
      xmlMemoryRegions = xmlMemoryRegions + name.render ~> mr
      return mr
    }

    def getType(p: AadlPort): AadlType = {
      p match {
        case p: AadlDataPort => return p.aadlType
        case p: AadlEventDataPort => return p.aadlType
        case _ => halt("Infeasible")
      }
    }

    def processTypes(): Unit = {
      if (types.rawConnections) {
        reporter.error(None(), MicrokitCodegen.toolName, "Raw connections are not currently supported")
        return
      }

      var forwardDefs: ISZ[ST] = ISZ()
      var defs: ISZ[ST] = ISZ()
      for (t <- ops.ISZOps(types.typeMap.values).sortWith((a, b) => CommonTypeUtil.isEnumTypeH(a)) if !CommonTypeUtil.isBaseTypeH(t)) {
        val r = TypeUtil.processDatatype(t, reporter)
        if (r._1.nonEmpty) {
          forwardDefs = forwardDefs :+ r._1.get
        }
        defs = defs :+ r._2
      }

      val content =
        st"""#ifndef TYPES_H
            |#define TYPES_H
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
      val path = s"$outputdir/types.h"
      resources = resources :+ ResourceUtil.createResourceH(path, content, T, T)
    }

    def processThread(t: AadlThread): (ProtectionDomain, Z) = {
      assert(t.dispatchProtocol == Dispatch_Protocol.Periodic, "Only processing periodic threads currently")

      val threadId = getName(t.path)
      val threadMonId = st"${getName(t.path)}_MON"
      var childMemMaps: ISZ[MemoryMap] = ISZ()

      var vaddrs: ISZ[(ST, String)] = ISZ() // probably (vaddrName, type)
      var codeApiMethodSigs: ISZ[ST] = ISZ()
      var codeApiMethods: ISZ[ST] = ISZ()
      var nextMemAddress = 10000000
      for (port <- t.getPorts()) {
        port match {
          case d: AadlDataPort =>
            val portType: AadlType = getType(d)
            port.direction match {
              case Direction.In =>
                for (inConn <- symbolTable.getInConnections(port.path)) {
                  val vaddrName = st"${port.identifier}"
                  val region = createMemoryRegion(inConn.src.feature.get.name)
                  childMemMaps = childMemMaps :+ MemoryMap(
                    memoryRegion = region.name, vaddr = s"${MicrokitCodegen.hexFormat(nextMemAddress)}",
                    perms = ISZ(Perm.READ), varAddr = vaddrName.render)
                  nextMemAddress = nextMemAddress + 1000
                  val portTypeName = TypeUtil.getTypeName(portType, reporter)
                  vaddrs = vaddrs :+ (vaddrName, portTypeName)
                  val methodSig: ST =
                    if (TypeUtil.isPrimitive(d.aadlType)) st"$portTypeName get${ops.StringOps(port.identifier).firstToUpper}()"
                    else st"void get${ops.StringOps(port.identifier).firstToUpper}($portTypeName *value)"
                  codeApiMethodSigs = codeApiMethodSigs :+ methodSig
                  val api: ST =
                    (if (TypeUtil.isPrimitive(d.aadlType)) {
                      st"""$methodSig {
                          |  return *$vaddrName;
                          |}"""
                    } else if (CommonTypeUtil.isArrayTypeH(d.aadlType)) {
                      val arr = d.aadlType.asInstanceOf[ArrayType]
                      st"""$methodSig {
                          |  // TODO need memmove or memcpy
                          |  for (int i = 0; i < ${TypeUtil.getArrayDefinedSize(arr)}; i++){
                          |    value[i] = $vaddrName[i];
                          |  }
                          |}"""
                    }
                    else {
                      st"""$methodSig {
                          |  *value = *$vaddrName;
                          |}"""
                    })
                  codeApiMethods = codeApiMethods :+ api
                }
              case Direction.Out =>
                val vaddrName = st"${port.identifier}"
                val region = createMemoryRegion(port.feature.identifier.name)
                childMemMaps = childMemMaps :+ MemoryMap(
                  memoryRegion = region.name, vaddr = s"${MicrokitCodegen.hexFormat(nextMemAddress)}",
                  perms = ISZ(Perm.READ, Perm.WRITE), varAddr = vaddrName.render)
                nextMemAddress = nextMemAddress + 1000
                val portTypeName = TypeUtil.getTypeName(portType, reporter)
                vaddrs = vaddrs :+ (vaddrName, portTypeName)
                val methodSig: ST =
                  if (TypeUtil.isPrimitive(d.aadlType)) st"void put${ops.StringOps(port.identifier).firstToUpper}($portTypeName value)"
                  else st"void put${ops.StringOps(port.identifier).firstToUpper}($portTypeName *value)"
                codeApiMethodSigs = codeApiMethodSigs :+ methodSig
                val api: ST =
                  (if (TypeUtil.isPrimitive(d.aadlType)) {
                    st"""$methodSig {
                        |  *$vaddrName = value;
                        |}"""
                  } else if (CommonTypeUtil.isArrayTypeH(d.aadlType)) {
                    val arr = d.aadlType.asInstanceOf[ArrayType]
                    val bitsize: Z = arr.bitSize match {
                      case Some(b) => b
                      case _ =>
                        reporter.error(None(), MicrokitCodegen.toolName, "Bit size must be specified")
                        0
                    }
                    st"""$methodSig {
                        |  // TODO need memmove or memcpy
                        |  for (int i = 0; i < ${TypeUtil.getArrayDefinedSize(arr)}; i++){
                        |    $vaddrName[i] = value[i];
                        |  }
                        |}"""
                  } else {
                    st"""$methodSig {
                        |  *$vaddrName = *value;
                        |}"""
                  })
                codeApiMethods = codeApiMethods :+ api
              case _ =>
                halt("Infeasible: codegen only supports in and out connections")
            }
          case _ =>
            reporter.error(port.feature.identifier.pos, MicrokitCodegen.toolName, "Only currently handling data ports")
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
          assert (l <= h, s"low must be <= high: $l <= $h")
          h
        case _ => 100
      }

      xmlSchedulingDomains = xmlSchedulingDomains :+
        SchedulingDomain(name = schedulingDomain, length = computeExecutionTime)

      val child = ProtectionDomain(name = threadId.render, schedulingDomain = Some(schedulingDomain), id = Some(s"1"),
        stackSize = None(), memMaps = childMemMaps, programImage = mk.elfName, children = ISZ())

      xmlProtectionDomains = xmlProtectionDomains :+
        ProtectionDomain(
          name = threadMonId.render, schedulingDomain = Some(schedulingDomain), id = None(), stackSize = None(),
          memMaps = ISZ(), programImage = mk.monElfName, children = ISZ(child))

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



      val vaddrEntries: ISZ[ST] = for (v <- vaddrs) yield st"volatile ${v._2} *${v._1};"

      val initializeMethodName = st"${threadId}_initialize"
      val timeTriggeredMethodName = st"${threadId}_timeTriggered"
      val implSource =
        st"""#include "$headerFileName"
            |
            |void ${initializeMethodName}(void);
            |void ${timeTriggeredMethodName}(void);
            |
            |${(vaddrEntries, "\n")}
            |
            |#define PORT_FROM_MON $monChannelId
            |
            |${(codeApiMethods, "\n\n")}
            |
            |void init(void) {
            |  $initializeMethodName();
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case PORT_FROM_MON:
            |      ${timeTriggeredMethodName}();
            |      break;
            |  }
            |}
            |"""

      val implPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(implPath, implSource, T)

      val userImplSource =
        st"""#include "$headerFileName"
            |
            |void ${initializeMethodName}(void) {
            |  // add initialization code here
            |  printf("%s: Init\n", microkit_name);
            |}
            |
            |void ${timeTriggeredMethodName}() {
            |  // add compute phase code here
            |  printf("%s: timeTriggered\n", microkit_name);
            |}
            |"""
      val userImplPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cUserImplFilename}"
      resources = resources :+ ResourceUtil.createResource(userImplPath, userImplSource, F)

      val headerSource =
        st"""#include <printf.h>
            |#include <stdint.h>
            |#include <microkit.h>
            |#include <types.h>
            |
            |${(codeApiMethodSigs, ";\n")};
            |"""
      val headerPath = s"${options.camkesOutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(headerPath, headerSource, T)

      return (child, computeExecutionTime)
    }

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

    processTypes()

    var usedBudget: Z = 0
    for (t <- symbolTable.getThreads()) {
      val results = processThread(t)
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

    val sd = SystemDescription(
      schedulingDomains = xmlScheds,
      protectionDomains = xmlProtectionDomains,
      memoryRegions = xmlMemoryRegions.values,
      channels = xmlChannels)

    val xmlPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.microkitSystemXmlFilename}"
    resources = resources :+ ResourceUtil.createResource(path = xmlPath, content = sd.prettyST, overwrite = T)

    val sysDot = sd.toDot
    val dotPath = s"${options.camkesOutputDir.get}/microkit.dot"
    resources = resources :+ ResourceUtil.createResource(path = dotPath, content = sysDot, overwrite = T)

    val TAB: String = "\t"

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

    val objs: ISZ[ST] = for (mk <- makefileContainers) yield mk.OBJSEntry
    val buildEntries: ISZ[ST] = for (mk <- makefileContainers) yield mk.buildEntry

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
          |PRINTF_OBJS := printf.o util.o
          |${(objs, "\n")}
          |
          |SYSTEM_FILE := $${TOP}/${MicrokitCodegen.microkitSystemXmlFilename}
          |
          |IMAGES := ${(elfFiles, " ")}
          |IMAGE_FILE_DATAPORT = microkit.img
          |IMAGE_FILE = loader.img
          |REPORT_FILE = /report.txt
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
