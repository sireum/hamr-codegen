// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB

object MakefileTemplate {

  def mainMakefile(miscTargets: ISZ[MakefileTarget], hasRustCrates: B): ST = {
    val rustCommentOpt: Option[ST] =
      if (hasRustCrates)
        Some(st"""# By default, cargo-verus is used to build Rust crates, and it fails if verification does not succeed.
                 |# To skip verification, set the RUST_MAKE_TARGET environment variable to use a cargo build target.
                 |# Example:
                 |#
                 |#   RUST_MAKE_TARGET=build-release make
                 |""")
      else None()
    val miscTargetsOpt: Option[ST] =
      if(miscTargets.nonEmpty) Some(
        st"""${(for(t <- miscTargets) yield t.prettyST, "\n\n")}
            |""")
      else None()

    val content =
      st"""${MicrokitUtil.doNotEditMakefile}
          |
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
          |export CRATES_DIR := $$(TOP_DIR)/crates
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
          |${rustCommentOpt}
          |all: $${IMAGE_FILE}
          |
          |.PHONY: check_microkit
          |check_microkit:
          |${TAB}@if [ -z "$$(strip $$(MICROKIT_BOARD))" ]; then \
          |${TAB}${TAB}echo "MICROKIT_BOARD must be specified"; \
          |${TAB}${TAB}exit 1; \
          |${TAB}fi
          |${TAB}@if [ -z "$$(strip $$(MICROKIT_SDK))" ]; then \
          |${TAB}${TAB}echo "MICROKIT_SDK must be specified"; \
          |${TAB}${TAB}exit 1; \
          |${TAB}fi
          |
          |qemu $${IMAGE_FILE} $${REPORT_FILE}: check_microkit $$(IMAGE_FILE) $${TOP_BUILD_DIR}/Makefile FORCE
          |${TAB}$${MAKE} -C $${TOP_BUILD_DIR} $$(notdir $$@)
          |
          |clean:: $${TOP_BUILD_DIR}/Makefile
          |${TAB}$${MAKE} -C $${TOP_BUILD_DIR} clean
          |
          |clobber:: clean
          |${TAB}rm -rf $$(TOP_DIR)/build
          |
          |$${TOP_BUILD_DIR}/Makefile: $$(SYSTEM_MAKEFILE)
          |${TAB}mkdir -p $${TOP_BUILD_DIR}
          |${TAB}cp $$(SYSTEM_MAKEFILE) $${TOP_BUILD_DIR}/Makefile
          |
          |$miscTargetsOpt
          |FORCE:
          |"""
    return content
  }

  @pure def systemMakefile(elfFiles: ISZ[String],
                           typeObjectNames: ISZ[String],
                           buildEntries: ISZ[ST],
                           elfEntries: ISZ[ST],
                           miscTargets: ISZ[MakefileTarget]): ST = {

    val uniqueBuildEntries: ISZ[String] = (Set.empty[String] ++ (for(be <- buildEntries) yield be.render)).elements

    val miscTargetsOpt: Option[ST] =
      if (miscTargets.nonEmpty) Some(st"""${(for(t <- miscTargets) yield t.prettyST, "\n\n")}""")
      else None()

    val content =
      st"""${MicrokitUtil.doNotEditMakefile}
          |
          |MICROKIT_TOOL ?= $$(MICROKIT_SDK)/bin/microkit
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
          |SCHEDULE_FILE := $$(TOP_DIR)/${MicrokitCodegen.microkitScheduleXmlFilename}
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
          |${(uniqueBuildEntries, "\n\n")}
          |
          |${(elfEntries, "\n\n")}
          |
          |$$(IMAGE_FILE): $$(IMAGES) $$(SYSTEM_FILE)
          |${TAB}xmllint --xinclude $$(SYSTEM_FILE) -o $$(SYSTEM_FILE).merged
          |${TAB}$$(MICROKIT_TOOL) $$(SYSTEM_FILE).merged --search-path $$(TOP_BUILD_DIR) --board $$(MICROKIT_BOARD) --config $$(MICROKIT_CONFIG) -o $$(IMAGE_FILE) -r $$(REPORT_FILE)
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
          |${TAB}rm -f *.o
          |
          |$miscTargetsOpt
          |"""
    return content
  }
}
