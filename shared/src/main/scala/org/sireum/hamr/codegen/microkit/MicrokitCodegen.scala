// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlPort, AadlThread, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodegenResults, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.lint.Linter
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.ir.{Aadl, ConnectionInstance, Direction}
import org.sireum.message.Reporter

@datatype class MakefileContainer (val resourceSuffix: String,
                                   val relativePath: Option[String],
                                   val hasHeader: B,
                                   val hasUserContent: B) {

  @strictpure def cHeaderFilename: String = s"$resourceSuffix.h"
  @strictpure def cImplFilename: String = s"$resourceSuffix.c"
  @strictpure def cUserImplFilename: String = s"${resourceSuffix}_user.c"

  @strictpure def oName: String = s"$resourceSuffix.o"
  @strictpure def userOName: String = s"${resourceSuffix}_user.o"
  @strictpure def elfName: String = s"$resourceSuffix.elf"

  @strictpure def relativePathSrcDir: String = if (relativePath.nonEmpty) s"${relativePath.get}/src" else ""
  @strictpure def relativePathIncludeDir: String = if (relativePath.nonEmpty) s"${relativePath.get}/include" else ""

  @strictpure def OBJSEntry: ST = st"${ops.StringOps(resourceSuffix).toUpper}_OBJS := $$(PRINTF_OBJS) $oName"

  @pure def buildEntry: ST = {
    val TAB: String = "\t"
    // FIXME spilt output into include and src directories
    val header: Option[String] = None()//if (hasHeader) Some(s"$${TOP}/$relativePathIncludeDir/$cHeaderFilename ") else None()
    val userContributions: Option[ST] =
      if (hasUserContent)
        Some(st"""$userOName: $header$${TOP}/$relativePathSrcDir/$cUserImplFilename Makefile
                 |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@""")
      else None()
    val ret =
      st"""$userContributions
          |$oName: $header$${TOP}/$relativePathSrcDir/$cImplFilename Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@"""
    return ret
  }

  @pure def elfEntry: ST = {
    val TAB: String = "\t"
    val ret =
      st"""$elfName: $$(PRINTF_OBJS) ${if (hasUserContent) userOName else ""} $oName
          |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@"""
        return ret
  }
}

object MicrokitCodegen {
  val toolName: String = "Mircokit Codegen"

  val microkitSystemXmlFilename: String = "microkit.system"
  val systemMakeFilename: String = "system.mk"

  val pacerName: String = "pacer"

  val pacerSchedulingDomain: String = "domain1"

  val endOfFrameComponentName: String = "end_of_frame_component"

  val dirComponents: String = "components"

  @pure def hexFormat(z: Z): String = {
    val s = conversions.String.toCis(z.string)
    var ret: ISZ[C] = ISZ(s(s.lastIndex))
    for(i <- 1 until s.size) {
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
  var makefileContainers: ISZ[MakefileContainer]= ISZ()

  var xmlProtectionDomains: ISZ[ST] = ISZ()
  var xmlSchedulingDomains: ISZ[ST] = ISZ()
  var xmlMemoryRegions: HashSMap[String, ST] = HashSMap.empty
  var xmlPacerChannels: ISZ[ST] = ISZ()

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

  def run(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): CodegenResults = {

    def addPacerComponent(): Unit = {
      xmlProtectionDomains = st"""<protection_domain name="${MicrokitCodegen.pacerName}" domain="domain1" >
                                 |  <program_image path="${MicrokitCodegen.pacerName}.elf" />
                                 |</protection_domain>""" +:  xmlProtectionDomains

      xmlSchedulingDomains = st"""<domain name="${MicrokitCodegen.pacerSchedulingDomain}" length="100" />""" +: xmlSchedulingDomains

      portPacerToEndOfFrame = getNextPacerChannelId
      portEndOfFrameToPacer = getNextPacerChannelId

      xmlPacerChannels = xmlPacerChannels :+ st"""<channel>
                                                 |  <end pd="${MicrokitCodegen.pacerName}" id="$portPacerToEndOfFrame" />
                                                 |  <end pd="${MicrokitCodegen.endOfFrameComponentName}" id="$portPacerToEndOfFrame" />
                                                 |</channel>"""

      xmlPacerChannels = xmlPacerChannels :+ st"""<channel>
                                                 |  <end pd="${MicrokitCodegen.endOfFrameComponentName}" id="$portEndOfFrameToPacer" />
                                                 |  <end pd="${MicrokitCodegen.pacerName}" id="$portEndOfFrameToPacer" />
                                                 |</channel>"""

      val mk = MakefileContainer(resourceSuffix = MicrokitCodegen.pacerName, relativePath = Some(s"${MicrokitCodegen.dirComponents}/${ops.StringOps(MicrokitCodegen.pacerName).toLower}"), hasHeader = F, hasUserContent = F)
      makefileContainers = makefileContainers :+ mk

      val PORT_TO = s"PORT_TO_${ops.StringOps(MicrokitCodegen.endOfFrameComponentName).toUpper}"
      val PORT_FROM = s"PORT_FROM_${ops.StringOps(MicrokitCodegen.endOfFrameComponentName).toUpper}"
      codePacerDefines = codePacerDefines :+ st"#define $PORT_TO $portPacerToEndOfFrame"
      codePacerDefines = codePacerDefines :+ st"#define $PORT_FROM $portEndOfFrameToPacer"
      codePacerPings = codePacerPings :+ st"microkit_notify($PORT_TO)"
      val content =
        st"""#include <stdint.h>
            |#include <microkit.h>
            |
            |${(codePacerDefines, "\n")}
            |
            |void paceComponents(){
            |  ${(codePacerPings, ";\n")};
            |}
            |
            |void init(void) {}
            |
            |void notified(microkit_channel channel) {
            |  switch(channel) {
            |    case $PORT_FROM:
            |      paceComponents();
            |      break;
            |  }
            |}
            |"""
      val path = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(path, content, T)
    }

    def addEndOfFrameComponent(): Unit = {
      val domain = largestSchedulingDomain + 1
      xmlProtectionDomains = xmlProtectionDomains :+ st"""<protection_domain name="${MicrokitCodegen.endOfFrameComponentName}" domain="domain${domain}" >
                                                   |  <program_image path="${MicrokitCodegen.endOfFrameComponentName}.elf" />
                                                   |</protection_domain>"""
      xmlSchedulingDomains = xmlSchedulingDomains :+ st"""<domain name="domain$domain" length="100" />"""

      val mk = MakefileContainer(resourceSuffix = MicrokitCodegen.endOfFrameComponentName, relativePath = Some(s"${MicrokitCodegen.dirComponents}/${ops.StringOps(MicrokitCodegen.endOfFrameComponentName).toLower}"), hasHeader = F, hasUserContent = F)
      makefileContainers = makefileContainers :+ mk

      val portFrom = s"PORT_FROM_${ops.StringOps(MicrokitCodegen.pacerName).toUpper}"
      val portTo = s"PORT_TO_${ops.StringOps(MicrokitCodegen.pacerName).toUpper}"
      val content =
        st"""#include <stdint.h>
            |#include <microkit.h>
            |
            |#define $portTo $portEndOfFrameToPacer
            |#define $portFrom $portPacerToEndOfFrame
            |
            |void init(void) {
            |  microkit_notify($portTo);
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case $portFrom:
            |      microkit_notify($portTo);
            |      break;
            |  }
            |}"""
      val path = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(path, content, T)
    }

    @pure def getName(ids: ISZ[String]): ST = {
      return st"${(ops.ISZOps(ids).drop(1), "_")}"
    }

    def createMemoryRegion(senderIdPath: ISZ[String]): String = {
      val name = getName(senderIdPath)
      if (xmlMemoryRegions.contains(name.render)) {
        return name.render
      }
      xmlMemoryRegions = xmlMemoryRegions + name.render~> st"""<memory_region name="$name" size="${MicrokitCodegen.hexFormat(1000)}" />"""
      return name.render
    }

    def getType(p: AadlPort): ST = {
      p match {
        case p: AadlDataPort =>
          //p.aadlType
          return st"int"
        case p: AadlEventDataPort =>
          //p.aadlType
          return st"int"
        case _ =>
          halt("Infeasible")
      }
    }
    def processThread(t: AadlThread): ST = {
      assert (t.dispatchProtocol == Dispatch_Protocol.Periodic, "Only processing periodic threads currently")

      val threadId = getName(t.path)
      var xmlEntries: ISZ[ST] = ISZ()
      var codeDefinePortEntries: ISZ[(ST, ST)] = ISZ()
      var vaddrs: ISZ[(ST, ST)] = ISZ() // probably (vaddrName, type)
      var codeApiMethodSigs: ISZ[ST] = ISZ()
      var codeApiMethods: ISZ[ST] = ISZ()
      var nextMemAddress = 10000000
      for(port <- t.getPorts()) {
        port match {
          case d: AadlDataPort =>
            val portType = getType(d)
            port.direction match {
              case Direction.In =>
                for (inConn <- symbolTable.getInConnections(port.path)) {
                  val vaddrName = st"${port.identifier}"
                  val regionName = createMemoryRegion(inConn.src.feature.get.name)
                  xmlEntries = xmlEntries :+ st"""<map mr="$regionName" vaddr="${MicrokitCodegen.hexFormat(nextMemAddress)}" perms="r" setvar_vaddr="$vaddrName" />"""
                  nextMemAddress = nextMemAddress + 1000
                  vaddrs = vaddrs :+ (vaddrName, portType)
                  val methodSig = st"void get${ops.StringOps(port.identifier).firstToUpper}($portType *value)"
                  codeApiMethodSigs = codeApiMethodSigs :+ methodSig
                  codeApiMethods = codeApiMethods :+
                    st"""$methodSig {
                        |  *value = *$vaddrName;
                        |}"""
                }
              case Direction.Out =>
                val vaddrName = st"${port.identifier}"
                val regionName = createMemoryRegion(port.feature.identifier.name)
                xmlEntries = xmlEntries :+ st"""<map mr="$regionName" vaddr="${MicrokitCodegen.hexFormat(nextMemAddress)}" perms="wr" setvar_vaddr="$vaddrName" />"""
                nextMemAddress = nextMemAddress + 1000
                vaddrs = vaddrs :+ (vaddrName, portType)
                val methodSig = st"void put${ops.StringOps(port.identifier).firstToUpper}($portType *value)"
                codeApiMethodSigs = codeApiMethodSigs :+ methodSig
                codeApiMethods = codeApiMethods :+
                  st"""$methodSig {
                      |  *$vaddrName = *value;
                      |}"""
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

      xmlSchedulingDomains = xmlSchedulingDomains :+ st"""<domain name="$schedulingDomain" length="100" />"""

      xmlEntries = xmlEntries :+ st"""<program_image path="${mk.elfName}" />"""

      val ret = st"""<protection_domain name="$threadId" domain="$schedulingDomain" >
                    |  ${(xmlEntries, "\n")}
                    |</protection_domain>"""

      xmlProtectionDomains = xmlProtectionDomains :+ ret

      val pacerChannelId = getNextPacerChannelId
      xmlPacerChannels = xmlPacerChannels :+ st"""<channel>
                                           |  <end pd="${MicrokitCodegen.pacerName}" id="$pacerChannelId" />
                                           |  <end pd="$threadId" id="$pacerChannelId" />
                                           |</channel>"""
      val threadCaps = s"PORT_TO_${ops.StringOps(threadId.render).toUpper}"
      val connFromPacer = st"PORT_FROM_PACER"
      codePacerDefines = codePacerDefines :+ st"""#define $threadCaps $pacerChannelId"""
      codePacerPings = codePacerPings :+ st"""microkit_notify($threadCaps)"""
      codeDefinePortEntries = codeDefinePortEntries :+ (connFromPacer, st"${pacerChannelId}")


      val vaddrEntries: ISZ[ST] = for(v <- vaddrs) yield st"volatile ${v._2} *${v._1};"

      val headerFileName = s"${threadId.render}.h"
      val initializeMethodName = st"${threadId}_initialize"
      val timeTriggeredMethodName = st"${threadId}_timeTriggered"
      val portDefines: ISZ[ST] = for(p <- codeDefinePortEntries) yield st"#define ${p._1} ${p._2}"
      val implSource =
        st"""#include "$headerFileName"
            |
            |void ${initializeMethodName}(void);
            |void ${timeTriggeredMethodName}(void);
            |
            |${(vaddrEntries, "\n")}
            |
            |${(portDefines, "\n")}
            |
            |${(codeApiMethods, "\n\n")}
            |
            |void init(void) {
            |  $initializeMethodName();
            |}
            |
            |void notified(microkit_channel channel) {
            |  switch (channel) {
            |    case $connFromPacer:
            |      ${timeTriggeredMethodName}();
            |      break;
            |  }
            |}
            |"""

      val implPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cImplFilename}"
      resources = resources :+ ResourceUtil.createResource(implPath, implSource, T)

      val userImplSource =
        st"""#include "../../../include/printf.h"
            |#include "$headerFileName"
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
        st"""#include <stdint.h>
            |#include <microkit.h>
            |
            |${(codeApiMethodSigs, ";\n")};
            |"""
      // FIXME: split output into include and src directories
      //val headerPath = s"${options.camkesOutputDir.get}/${mk.relativePathIncludeDir}/${mk.cHeaderFilename}"
      val headerPath = s"${options.camkesOutputDir.get}/${mk.relativePathSrcDir}/${mk.cHeaderFilename}"
      resources = resources :+ ResourceUtil.createResource(headerPath, headerSource, T)

      return ret
    }

    if (!Linter.lint(model, options, types, symbolTable, reporter)) {
      return CodegenResults(ISZ(), ISZ())
    }

    for(t <- symbolTable.getThreads()) {
      processThread(t)
    }
    addPacerComponent()

    addEndOfFrameComponent()

   val xmlDesc = st"""<?xml version="1.0" encoding="UTF-8"?>
                                |<system>
                                |
                                |  <domain_schedule>
                                |    ${(xmlSchedulingDomains, "\n")}
                                |  </domain_schedule>
                                |
                                |  ${(xmlProtectionDomains, "\n\n")}
                                |
                                |  ${(xmlMemoryRegions.values, "\n\n")}
                                |
                                |  ${(xmlPacerChannels, "\n\n")}
                                |</system>
                                """

    val xmlPath = s"${options.camkesOutputDir.get}/${MicrokitCodegen.microkitSystemXmlFilename}"
    resources = resources :+ ResourceUtil.createResource(path = xmlPath, content = xmlDesc, overwrite = T)

    val TAB: String = "\t"

    val makefileContents = st"""ifeq ($$(strip $$(MICROKIT_SDK)),)
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

    val elfFiles: ISZ[String] = (for(mk <- makefileContainers) yield mk.elfName)
    val oFiles: ISZ[String] = for(f <- cFileSuffixes) yield s"$f.o"

    val elfEntries: ISZ[ST] = for( mk <- makefileContainers) yield mk.elfEntry

    val systemmkContents = st"""ifeq ($$(strip $$(MICROKIT_SDK)),)
                               |$$(error MICROKIT_SDK must be specified)
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
                               |MICROKIT_TOOL ?= $$(MICROKIT_SDK)/bin/microkit
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
                               |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@
                               |
                               |printf.o: $${TOP}/include/printf.c Makefile
                               |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@
                               |
                               |util.o: $${TOP}/include/util.c Makefile
                               |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@
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


    val includePath = s"${options.camkesOutputDir.get}/include"
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/printf.h", Util.printfh, T)
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/printf.c", Util.printfc, T)
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/util.h", Util.utilh, T)
    resources = resources :+ ResourceUtil.createResource(s"${includePath}/util.c", Util.utilc, T)

    return CodegenResults(resources = resources, auxResources = ISZ())
  }
}
