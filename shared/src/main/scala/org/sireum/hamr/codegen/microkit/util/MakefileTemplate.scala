// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.types.TypeUtil
import org.sireum.hamr.codegen.microkit.util.Util.TAB

object MakefileTemplate {

  def mainMakefile: ST = {
    val content =
      st"""${Util.doNotEditMakefile}
          |
          |ifeq ($$(strip $$(MICROKIT_SDK)),)
          |$$(error MICROKIT_SDK must be specified)
          |endif
          |override MICROKIT_SDK := $$(abspath $${MICROKIT_SDK})
          |
          |SYSTEM_MAKEFILE ?= system.mk
          |
          |export CPU = cortex-a53
          |export QEMU = qemu-system-aarch64
          |
          |export AR := ar
          |export CC := clang
          |export DTC := dtc
          |export LD := ld.lld
          |export RANLIB := llvm-ranlib
          |
          |export TOP_DIR := $$(abspath $$(dir $${MAKEFILE_LIST}))
          |export TOP_BUILD_DIR := $$(abspath build)
          |
          |export TARGET := aarch64-none-elf
          |
          |# By default we make a debug build so that the client debug prints can be seen.
          |export MICROKIT_CONFIG ?= debug
          |
          |export MICROKIT_SDK := $$(abspath $$(MICROKIT_SDK))
          |export MICROKIT_TOOL := $$(abspath $$(MICROKIT_SDK)/bin/microkit)
          |export MICROKIT_BOARD_DIR := $$(abspath $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(MICROKIT_CONFIG))
          |
          |
          |IMAGE_FILE := $$(TOP_BUILD_DIR)/loader.img
          |REPORT_FILE := $$(TOP_BUILD_DIR)/report.txt
          |
          |all: $${IMAGE_FILE}
          |
          |qemu $${IMAGE_FILE} $${REPORT_FILE} clean clobber: $$(IMAGE_FILE) $${TOP_BUILD_DIR}/Makefile FORCE
          |	$${MAKE} -C $${TOP_BUILD_DIR} $$(notdir $$@)
          |
          |$${TOP_BUILD_DIR}/Makefile: $$(SYSTEM_MAKEFILE)
          |	mkdir -p $${TOP_BUILD_DIR}
          |	cp $$(SYSTEM_MAKEFILE) $${TOP_BUILD_DIR}/Makefile
          |
          |FORCE:
          |"""
    return content
  }

  @pure def systemMakefile(elfFiles: ISZ[String],
                           typeObjectNames: ISZ[String],
                           buildEntries: ISZ[ST],
                           elfEntries: ISZ[ST]): ST = {
    val content =
      st"""${Util.doNotEditMakefile}
          |
          |ifeq ($$(strip $$(MICROKIT_SDK)),)
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
          |CFLAGS := -mcpu=$$(CPU) \
          |${TAB}-mstrict-align \
          |${TAB}-ffreestanding \
          |${TAB}-nostdlib \
          |${TAB}-g3 \
          |${TAB}-O3 \
          |${TAB}-Wall -Wno-unused-function -Werror -Wno-unused-command-line-argument \
          |${TAB}-I$$(MICROKIT_BOARD_DIR)/include \
          |${TAB}-target $$(TARGET)
          |
          |LDFLAGS := -L$$(MICROKIT_BOARD_DIR)/lib
          |LIBS := --start-group -lmicrokit -Tmicrokit.ld --end-group
          |
          |SYSTEM_FILE := $$(TOP_DIR)/${MicrokitCodegen.microkitSystemXmlFilename}
          |
          |IMAGES := ${(elfFiles, " ")}
          |IMAGE_FILE = loader.img
          |REPORT_FILE = report.txt
          |
          |UTIL_OBJS = printf.o util.o
          |
          |TYPES_DIR = $$(TOP_DIR)/types
          |TYPE_OBJS := ${(typeObjectNames, " ")}
          |
          |# exporting TOP_TYPES_INCLUDE in case other makefiles need it
          |export TOP_TYPES_INCLUDE = -I$$(TYPES_DIR)/include
          |
          |TOP_INCLUDE = $$(TOP_TYPES_INCLUDE) -I$$(TOP_DIR)/util/include
          |
          |all: $$(IMAGE_FILE)
          |${TAB}CHECK_FLAGS_BOARD_MD5:=.board_cflags-$$(shell echo -- $${CFLAGS} $${MICROKIT_BOARD} $${MICROKIT_CONFIG}| shasum | sed 's/ *-//')
          |
          |$${CHECK_FLAGS_BOARD_MD5}:
          |${TAB}-rm -f .board_cflags-*
          |${TAB}touch $$@
          |
          |%.o: $${TOP_DIR}/util/src/%.c Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$$(TOP_DIR)/util/include
          |
          |${(buildEntries, "\n\n")}
          |
          |${(elfEntries, "\n\n")}
          |
          |$$(IMAGE_FILE): $$(IMAGES) $$(SYSTEM_FILE)
          |${TAB}$$(MICROKIT_TOOL) $$(SYSTEM_FILE) --search-path $$(TOP_BUILD_DIR) --board $$(MICROKIT_BOARD) --config $$(MICROKIT_CONFIG) -o $$(IMAGE_FILE) -r $$(REPORT_FILE)
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
          |${TAB}rm -f $${(oFiles, " ")}
          |
          |clobber:: clean
          |${TAB}rm -f ${(elfFiles, " ")} $${IMAGE_FILE} $${REPORT_FILE}
          |"""
    return content
  }
}
