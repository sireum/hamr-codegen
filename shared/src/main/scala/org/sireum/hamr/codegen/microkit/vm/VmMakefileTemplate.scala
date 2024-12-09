// #Sireum

package org.sireum.hamr.codegen.microkit.vm

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util

object VmMakefileTemplate {

  @pure def Makefile(threadId: String): ST = {
    val ret =
      st"""${Util.safeToEditMakefile}
          |
          |ifeq ($$(strip $$(MICROKIT_SDK)),)
          |$$(error MICROKIT_SDK must be specified)
          |endif
          |
          |ifeq ($$(strip $$(MICROKIT_BOARD)),)
          |$$(error MICROKIT_BOARD must be specified)
          |endif
          |
          |${threadId}_DIR := $$(shell dirname $$(shell pwd))
          |
          |# @ivanv: need to have a step for putting in the initrd node into the DTB,
          |# 		  right now it is unfortunately hard-coded.
          |
          |# @ivanv: check that the path of SDK_PATH/MICROKIT_BOARD exists
          |# @ivanv: Have a list of supported boards to check with, if it's not one of those
          |# have a helpful message that lists all the support boards.
          |
          |# @ivanv: incremental builds don't work with LINUX_IMAGE_DIR changing
          |
          |LIB_VMM_DIR ?= $$(VMM_DIR)
          |ifeq ("$$(wildcard $$(LIB_VMM_DIR)/tools)","")
          |LIB_VMM_DIR := $${$threadId}/libvmm
          |ifeq ("$$(wildcard $$(${threadId}_DIR))","")
          |$$(error Set VMM_DIR to point to a copy of https://github.com/au-ts/libvmm")
          |endif
          |endif
          |
          |LIB_VMM_TOOLS_DIR := $$(LIB_VMM_DIR)/tools
          |LIB_VMM_SRC_DIR := $$(LIB_VMM_DIR)/src
          |export SDDF=$$(LIB_VMM_DIR)/dep/sddf
          |
          |LINUX_IMAGE_DIR := $$(${threadId}_DIR)/board/$$(MICROKIT_BOARD)
          |LINUX := $$(LINUX_IMAGE_DIR)/linux
          |DTS := $$(LINUX_IMAGE_DIR)/linux.dts
          |DTB := $$(TOP_BUILD_DIR)/linux.dtb
          |INITRD := $$(LINUX_IMAGE_DIR)/rootfs.cpio.gz
          |
          |# Toolchain flags
          |# FIXME: For optimisation we should consider providing the flag -mcpu.
          |# FIXME: We should also consider whether -mgeneral-regs-only should be
          |# used to avoid the use of the FPU and therefore seL4 does not have to
          |# context switch the FPU.
          |# Note we only need -Wno-unused-command-line-argument because in Nix
          |# passes an extra `--gcc-toolchain` flag which we do not need.
          |CFLAGS := \
          |		-mstrict-align \
          |		-ffreestanding \
          |		-g3 \
          |		-O3 \
          |		-Wno-unused-command-line-argument \
          |		-Wall -Wno-unused-function -Werror \
          |		-DMICROKIT_CONFIG_$$(MICROKIT_CONFIG) \
          |		-DBOARD_$$(MICROKIT_BOARD) \
          |		-I$$(LIB_VMM_DIR)/include \
          |		-I$$(MICROKIT_BOARD_DIR)/include \
          |		-I$$(SDDF)/include \
          |		-MD \
          |		-MP \
          |		-target $$(TARGET)
          |
          |LDFLAGS := -L$$(MICROKIT_BOARD_DIR)/lib
          |LIBS := --start-group -lmicrokit -Tmicrokit.ld libvmm.a --end-group
          |
          |CHECK_FLAGS_BOARD_MD5:=.board_cflags-$$(shell echo -- $$(CFLAGS) $$(MICROKIT_BOARD) $$(MICROKIT_CONFIG) | shasum | sed 's/ *-//')
          |
          |$$(CHECK_FLAGS_BOARD_MD5):
          |	-rm -f .board_cflags-*
          |	touch $$@
          |
          |${threadId}_INCLUDE = -I$$(${threadId}_DIR)/include
          |
          |all: $$(TOP_BUILD_DIR)/${threadId}.a
          |
          |$$(DTB):
          |ifeq ("", "$$(shell which $$(DTC))")
          |	$$(error "Could not find dependency: Device Tree Compiler (dtc)")
          |endif
          |	$$(LIB_VMM_TOOLS_DIR)/dtscat $$(DTS) $$(LINUX_IMAGE_DIR)/overlay.dts > $$(TOP_BUILD_DIR)/linux.dts
          |	# @ivanv: Shouldn't supress warnings
          |	$$(DTC) -q -I dts -O dtb $$(TOP_BUILD_DIR)/linux.dts > $$@
          |
          |#$$(TOP_BUILD_DIR)/package_guest_images.o: $$(LIB_VMM_TOOLS_DIR)/package_guest_images.S $$(LINUX_IMAGE_DIR) $$(LINUX) $$(INITRD) $$(DTB)
          |$$(TOP_BUILD_DIR)/package_guest_images.o: $$(LIB_VMM_TOOLS_DIR)/package_guest_images.S $$(LINUX_IMAGE_DIR) $$(DTB)
          |	$$(CC) -c -g3 -x assembler-with-cpp \
          |					-DGUEST_KERNEL_IMAGE_PATH=\"$$(LINUX)\" \
          |					-DGUEST_DTB_IMAGE_PATH=\"$$(DTB)\" \
          |					-DGUEST_INITRD_IMAGE_PATH=\"$$(INITRD)\" \
          |					-target $$(TARGET) \
          |					$$< \
          |					-o $$@
          |
          |# NOTE: the type libraries will be linked in via the top level makefile so here
          |# we just need to include $$(TOP_TYPES_INCLUDE)
          |$$(TOP_BUILD_DIR)/%.o: $$(${threadId}_DIR)/src/%.c $$(TOP_BUILD_DIR)/Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_TYPES_INCLUDE) $$(${threadId}_INCLUDE)
          |
          |-include vmm.d
          |include $$(LIB_VMM_DIR)/vmm.mk
          |vpath %.c $$(LIB_VMM_DIR)
          |
          |$$(TOP_BUILD_DIR)/${threadId}.a: libvmm.a $$(TOP_BUILD_DIR)/${threadId}.o $$(TOP_BUILD_DIR)/${threadId}_user.o $$(TOP_BUILD_DIR)/package_guest_images.o Makefile
          |	$$(AR) r libvmm.a $$(TOP_BUILD_DIR)/${threadId}.o $$(TOP_BUILD_DIR)/${threadId}_user.o $$(TOP_BUILD_DIR)/package_guest_images.o
          |	cp libvmm.a $$@
          |
          |clean::
          |	rm -rf $$(TOP_BUILD_DIR)
          |"""
    return ret
  }

  /*
  @pure def oldMakefile(): ST = {
    val ret =
      st"""# Copyright 2021, Breakaway Consulting Pty. Ltd.
          |# Copyright 2022, UNSW (ABN 57 195 873 179)
          |# Copyright 2024, DornerWorks
          |#
          |# SPDX-License-Identifier: BSD-2-Clause
          |#
          |
          |ifeq ($$(strip $$(MICROKIT_SDK)),)
          |$$(error MICROKIT_SDK must be specified)
          |endif
          |
          |ifeq ($$(strip $$(MICROKIT_BOARD)),)
          |$$(error MICROKIT_BOARD must be specified)
          |endif
          |
          |# All dependencies needed to compile the VMM
          |DTC := dtc
          |CC := clang
          |LD := ld.lld
          |AR := llvm-ar
          |MICROKIT_TOOL ?= $$(MICROKIT_SDK)/bin/microkit
          |
          |BUILD_DIR := ../../../build
          |CONFIG ?= debug
          |
          |# @ivanv: need to have a step for putting in the initrd node into the DTB,
          |# 		  right now it is unfortunately hard-coded.
          |
          |# @ivanv: check that the path of SDK_PATH/MICROKIT_BOARD exists
          |# @ivanv: Have a list of supported boards to check with, if it's not one of those
          |# have a helpful message that lists all the support boards.
          |
          |# @ivanv: incremental builds don't work with IMAGE_DIR changing
          |
          |MICROKIT_BOARD_DIR := $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(CONFIG)
          |VMM := deps/libvmm
          |VMM_TOOLS := $$(VMM)/tools
          |VMM_SRC_DIR := $$(VMM)/src
          |
          |IMAGE_DIR := board/$$(MICROKIT_BOARD)
          |LINUX := $$(IMAGE_DIR)/linux
          |DTS := $$(IMAGE_DIR)/linux.dts
          |DTB := $$(BUILD_DIR)/linux.dtb
          |INITRD := $$(IMAGE_DIR)/rootfs.cpio.gz
          |
          |IMAGE_URL := https://github.com/dornerworks/meta-inspecta-sut/releases/download
          |IMAGE_VER := v0.1.0-baseline
          |
          |# @ivanv: should only compile printf.o in debug
          |# VMM_OBJS := vmm.o printf.o virq.o linux.o guest.o psci.o smc.o fault.o util.o vgic.o vgic_v2.o package_guest_images.o tcb.o vcpu.o
          |VMM_OBJS := vmm.o virq.o linux.o guest.o psci.o smc.o fault.o vmm_util.o vgic.o vgic_v2.o package_guest_images.o tcb.o vcpu.o mmio.o
          |
          |# Toolchain flags
          |# FIXME: For optimisation we should consider providing the flag -mcpu.
          |# FIXME: We should also consider whether -mgeneral-regs-only should be
          |# used to avoid the use of the FPU and therefore seL4 does not have to
          |# context switch the FPU.
          |# Note we only need -Wno-unused-command-line-argument because in Nix
          |# passes an extra `--gcc-toolchain` flag which we do not need.
          |CFLAGS := -mstrict-align \
          |          -g3 \
          |          -O3 \
          |          -ffreestanding \
          |          -nostdlib \
          |          -Wno-unused-command-line-argument \
          |          -Wall -Wno-unused-function -Werror \
          |          -I$$(VMM)/include \
          |          -I$$(VMM_SRC_DIR) \
          |          -I$$(MICROKIT_BOARD_DIR)/include \
          |          -DMICROKIT_BOARD_$$(MICROKIT_BOARD) \
          |          -DCONFIG_$$(CONFIG) \
          |          -target aarch64-none-elf
          |
          |LDFLAGS := -L$$(MICROKIT_BOARD_DIR)/lib
          |LIBS := -lmicrokit -Tmicrokit.ld
          |
          |# all: directories $$(BUILD_DIR)/vmm.elf
          |all: directories $$(addprefix $$(BUILD_DIR)/, $$(VMM_OBJS))
          |
          |directories:
          |	$$(shell mkdir -p $$(BUILD_DIR))
          |
          |$$(DTB): $$(DTS)
          |ifeq ("", "$$(shell which $$(DTC))")
          |	$$(error "Could not find dependency: Device Tree Compiler (dtc)")
          |endif
          |	# @ivanv: Shouldn't supress warnings
          |	$$(DTC) -q -i $$(IMAGE_DIR) -I dts -O dtb $$< > $$@
          |
          |
          |#$$(BUILD_DIR)/package_guest_images.o: $$(VMM_TOOLS)/package_guest_images.S $$(IMAGE_DIR) $$(LINUX) $$(INITRD) $$(DTB)
          |$$(BUILD_DIR)/package_guest_images.o: $$(VMM_TOOLS)/package_guest_images.S $$(IMAGE_DIR) $$(DTB)
          |	$$(CC) -c -g3 -x assembler-with-cpp \
          |					-DGUEST_KERNEL_IMAGE_PATH=\"$$(LINUX)\" \
          |					-DGUEST_DTB_IMAGE_PATH=\"$$(DTB)\" \
          |					-DGUEST_INITRD_IMAGE_PATH=\"$$(INITRD)\" \
          |					-target aarch64-none-elf \
          |					$$< -o $$@
          |
          |$$(BUILD_DIR)/%.o: %.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |$$(BUILD_DIR)/%.o: virtio/%.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |$$(BUILD_DIR)/%.o: $$(VMM_SRC_DIR)/%.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |$$(BUILD_DIR)/vmm_util.o: $$(VMM_SRC_DIR)/util/util.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$(BUILD_DIR)/vmm_util.o
          |
          |$$(BUILD_DIR)/mmio.o: $$(VMM_SRC_DIR)/virtio/mmio.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |$$(BUILD_DIR)/%.o: $$(VMM_SRC_DIR)/arch/aarch64/%.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |$$(BUILD_DIR)/%.o: $$(VMM_SRC_DIR)/arch/aarch64/vgic/%.c Makefile
          |	$$(CC) -c $$(CFLAGS) $$< -o $$@
          |
          |# $$(BUILD_DIR)/vmm.elf: $$(addprefix $$(BUILD_DIR)/, $$(VMM_OBJS))
          |# 	$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@
          |
          |clean:
          |	rm -rf $$(BUILD_DIR)
          |"""

    return ret
  }*/
}
