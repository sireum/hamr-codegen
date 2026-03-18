// #Sireum

package org.sireum.hamr.codegen.microkit.vm

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB

object VmMakefileTemplate {

  @pure def Makefile(threadId: String): ST = {
    val ret =
      st"""${MicrokitUtil.safeToEditMakefile}
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
          |LIB_VMM_DIR := $$(${threadId}_DIR)/libvmm
          |ifeq ("$$(wildcard $$(LIB_VMM_DIR)/tools)","")
          |$$(error Set VMM_DIR to point to a copy of https://github.com/au-ts/libvmm")
          |endif
          |endif
          |
          |LIB_VMM_TOOLS_DIR := $$(LIB_VMM_DIR)/tools
          |LIB_VMM_SRC_DIR := $$(LIB_VMM_DIR)/src
          |export SDDF=$$(LIB_VMM_DIR)/dep/sddf
          |
          |ARCH := aarch64
          |SDDF_CUSTOM_LIBC := 1
          |
          |LINUX_IMAGE_DIR := $$(${threadId}_DIR)/board/$$(MICROKIT_BOARD)
          |LINUX := $$(LINUX_IMAGE_DIR)/linux
          |DTS := $$(LINUX_IMAGE_DIR)/linux.dts
          |DTB := $$(TOP_BUILD_DIR)/linux.dtb
          |INITRD := $$(LINUX_IMAGE_DIR)/rootfs.cpio.gz
          |
          |LINUX_VER ?= 85000f3f42a882e4476e57003d53f2bbec8262b0-linux
          |INITRD_VER ?= 6dcd1debf64e6d69b178cd0f46b8c4ae7cebe2a5-rootfs.cpio.gz
          |
          |# Toolchain flags
          |# FIXME: For optimisation we should consider providing the flag -mcpu.
          |# FIXME: We should also consider whether -mgeneral-regs-only should be
          |# used to avoid the use of the FPU and therefore seL4 does not have to
          |# context switch the FPU.
          |# Note we only need -Wno-unused-command-line-argument because in Nix
          |# passes an extra `--gcc-toolchain` flag which we do not need.
          |CFLAGS := \
          |${TAB}-mstrict-align \
          |${TAB}-ffreestanding \
          |${TAB}-g3 \
          |${TAB}-O3 \
          |${TAB}-Wno-unused-command-line-argument \
          |${TAB}-Wall -Wno-unused-function -Werror \
          |${TAB}-DMICROKIT_CONFIG_$$(MICROKIT_CONFIG) \
          |${TAB}-DBOARD_$$(MICROKIT_BOARD) \
          |${TAB}-I$$(LIB_VMM_DIR)/include \
          |${TAB}-I$$(MICROKIT_BOARD_DIR)/include \
          |${TAB}-I$$(SDDF)/include \
          |${TAB}-I$$(SDDF)/include/sddf/util \
          |${TAB}-I$$(SDDF)/include/sddf/util/custom_libc \
          |${TAB}-I$$(SDDF)/include/microkit \
          |${TAB}-MD \
          |${TAB}-MP \
          |${TAB}-target $$(TARGET)
          |
          |CHECK_FLAGS_BOARD_MD5:=.board_cflags-$$(shell echo -- $$(CFLAGS) $$(MICROKIT_BOARD) $$(MICROKIT_CONFIG) | shasum | sed 's/ *-//')
          |
          |$$(CHECK_FLAGS_BOARD_MD5):
          |${TAB}-rm -f .board_cflags-*
          |${TAB}touch $$@
          |
          |${threadId}_INCLUDE = -I$$(${threadId}_DIR)/include
          |
          |all: $$(TOP_BUILD_DIR)/${threadId}.a
          |
          |$$(LINUX):
          |${TAB}curl -L https://trustworthy.systems/Downloads/libvmm/images/$${LINUX_VER}.tar.gz -o $$@.tar.gz
          |${TAB}tar -xf $$@.tar.gz -C $$(LINUX_IMAGE_DIR)
          |${TAB}cp $$(LINUX_IMAGE_DIR)/$$(LINUX_VER)/linux $$@
          |
          |$$(INITRD):
          |${TAB}curl -L https://trustworthy.systems/Downloads/libvmm/images/$${INITRD_VER}.tar.gz -o $$@.tar.gz
          |${TAB}tar xf $$@.tar.gz -C $$(LINUX_IMAGE_DIR)
          |${TAB}cp $$(LINUX_IMAGE_DIR)/$$(INITRD_VER)/rootfs.cpio.gz $$@
          |
          |$$(DTB):
          |ifeq ("", "$$(shell which $$(DTC))")
          |${TAB}$$(error "Could not find dependency: Device Tree Compiler (dtc)")
          |endif
          |${TAB}$$(LIB_VMM_TOOLS_DIR)/dtscat $$(DTS) $$(LINUX_IMAGE_DIR)/overlay.dts > $$(TOP_BUILD_DIR)/linux.dts
          |${TAB}# @ivanv: Shouldn't supress warnings
          |${TAB}$$(DTC) -q -I dts -O dtb $$(TOP_BUILD_DIR)/linux.dts > $$@
          |
          |#$$(TOP_BUILD_DIR)/package_guest_images.o: $$(LIB_VMM_TOOLS_DIR)/package_guest_images.S $$(LINUX_IMAGE_DIR) $$(LINUX) $$(INITRD) $$(DTB)
          |$$(TOP_BUILD_DIR)/package_guest_images.o: $$(LIB_VMM_TOOLS_DIR)/package_guest_images.S $$(LINUX) $$(INITRD) $$(DTB)
          |${TAB}$$(CC) -c -g3 -x assembler-with-cpp \
          |${TAB}${TAB}-DGUEST_KERNEL_IMAGE_PATH=\"$$(LINUX)\" \
          |${TAB}${TAB}-DGUEST_DTB_IMAGE_PATH=\"$$(DTB)\" \
          |${TAB}${TAB}-DGUEST_INITRD_IMAGE_PATH=\"$$(INITRD)\" \
          |${TAB}${TAB}-target $$(TARGET) \
          |${TAB}${TAB}$$< \
          |${TAB}${TAB}-o $$@
          |
          |# NOTE: the type libraries will be linked in via the top level makefile so here
          |# we just need to include $$(TOP_TYPES_INCLUDE)
          |$$(TOP_BUILD_DIR)/%.o: $$(${threadId}_DIR)/src/%.c $$(TOP_BUILD_DIR)/Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_TYPES_INCLUDE) $$(${threadId}_INCLUDE)
          |
          |-include vmm.d
          |include $$(LIB_VMM_DIR)/vmm.mk
          |include $$(SDDF)/util/util.mk
          |vpath %.c $$(LIB_VMM_DIR)
          |
          |$$(TOP_BUILD_DIR)/${threadId}.a: libvmm.a libsddf_util.a $$(TOP_BUILD_DIR)/${threadId}.o $$(TOP_BUILD_DIR)/${threadId}_user.o $$(TOP_BUILD_DIR)/package_guest_images.o Makefile
          |${TAB}mkdir -p libsddf_util; cd libsddf_util; $$(AR) -x ../libsddf_util.a
          |${TAB}$$(AR) r libvmm.a libsddf_util/*.o $$(TOP_BUILD_DIR)/${threadId}.o $$(TOP_BUILD_DIR)/${threadId}_user.o $$(TOP_BUILD_DIR)/package_guest_images.o
          |${TAB}cp libvmm.a $$@
          |"""
    return ret
  }

  val linux_dts: ST = st"""/dts-v1/;
                          |
                          |/ {
                          |	interrupt-parent = <0x8001>;
                          |	#size-cells = <0x02>;
                          |	#address-cells = <0x02>;
                          |	compatible = "linux,dummy-virt";
                          |
                          |	psci {
                          |		migrate = <0xc4000005>;
                          |		cpu_on = <0xc4000003>;
                          |		cpu_off = <0x84000002>;
                          |		cpu_suspend = <0xc4000001>;
                          |		method = "smc";
                          |		compatible = "arm,psci-0.2\0arm,psci";
                          |	};
                          |
                          |	memory@40000000 {
                          |		reg = <0x00 0x40000000 0x00 0x80000000>;
                          |		device_type = "memory";
                          |	};
                          |
                          |	platform@c000000 {
                          |		interrupt-parent = <0x8001>;
                          |		ranges = <0x00 0x00 0xc000000 0x2000000>;
                          |		#address-cells = <0x01>;
                          |		#size-cells = <0x01>;
                          |		compatible = "qemu,platform\0simple-bus";
                          |	};
                          |
                          |	pl011@9000000 {
                          |		clock-names = "uartclk\0apb_pclk";
                          |		clocks = <0x8000 0x8000>;
                          |		interrupts = <0x00 0x01 0x04>;
                          |		reg = <0x00 0x9000000 0x00 0x1000>;
                          |		compatible = "arm,pl011\0arm,primecell";
                          |	};
                          |
                          |	pmu {
                          |		interrupts = <0x01 0x07 0x104>;
                          |		compatible = "arm,armv8-pmuv3";
                          |	};
                          |
                          |	intc@8000000 {
                          |		phandle = <0x8001>;
                          |		interrupts = <0x01 0x09 0x04>;
                          |		reg = <0x00 0x8000000 0x00 0x10000 0x00 0x8010000 0x00 0x10000 0x00 0x8030000 0x00 0x10000 0x00 0x8040000 0x00 0x10000>;
                          |		compatible = "arm,cortex-a15-gic";
                          |		ranges;
                          |		#size-cells = <0x02>;
                          |		#address-cells = <0x02>;
                          |		interrupt-controller;
                          |		#interrupt-cells = <0x03>;
                          |	};
                          |
                          |	flash@0 {
                          |		bank-width = <0x04>;
                          |		reg = <0x00 0x00 0x00 0x4000000 0x00 0x4000000 0x00 0x4000000>;
                          |		compatible = "cfi-flash";
                          |	};
                          |
                          |	cpus {
                          |		#size-cells = <0x00>;
                          |		#address-cells = <0x01>;
                          |
                          |		cpu@0 {
                          |			reg = <0x00>;
                          |			compatible = "arm,cortex-a53";
                          |			device_type = "cpu";
                          |		};
                          |	};
                          |
                          |	timer {
                          |		interrupts = <0x01 0x0d 0x104 0x01 0x0e 0x104 0x01 0x0b 0x104 0x01 0x0a 0x104>;
                          |		always-on;
                          |		compatible = "arm,armv8-timer\0arm,armv7-timer";
                          |	};
                          |
                          |	apb-pclk {
                          |		phandle = <0x8000>;
                          |		clock-output-names = "clk24mhz";
                          |		clock-frequency = <0x16e3600>;
                          |		#clock-cells = <0x00>;
                          |		compatible = "fixed-clock";
                          |	};
                          |
                          |	chosen {
                          |		stdout-path = "/pl011@9000000";
                          |		rng-seed = <0x8f0ee46 0xecbe7263 0xc724a577 0x271cf683 0xdd190daf 0x103bff62 0x5f2496fb 0xee0cf760>;
                          |		kaslr-seed = <0x198a65b3 0x8b3cef37>;
                          |	};
                          |};
                          |"""

  val overlay_dts: ST = st"""/*
                            | * Copyright 2024, UNSW
                            | *
                            | * SPDX-License-Identifier: BSD-2-Clause
                            | */
                            |/ {
                            |    /* Need to specify how much RAM the guest will have */
                            |    memory@40000000 {
                            |        device_type = "memory";
                            |        reg = <0x00 0x40000000 0x00 0x10000000>;
                            |    };
                            |
                            |    flash@0 {
                            |        status = "disabled";
                            |    };
                            |
                            |    chosen {
                            |        stdout-path = "/pl011@9000000";
                            |        bootargs = "earlycon=pl011,0x9000000 earlyprintk=serial debug loglevel=8";
                            |        linux,stdout-path = "/pl011@9000000";
                            |        linux,initrd-start = <0x4d000000>;
                            |        linux,initrd-end = <0x4d800000>;
                            |    };
                            |};
                            |"""

  val simple_system: ST = st"""<?xml version="1.0" encoding="UTF-8"?>
                              |<!--
                              | Copyright 2023, UNSW
                              |
                              | SPDX-License-Identifier: BSD-2-Clause
                              |-->
                              |<system>
                              |    <!--
                              |     Here we give the guest 256MiB to use as RAM. Note that we use 2MiB page
                              |     sizes for efficiency, it does not have any functional effect.
                              |    -->
                              |    <memory_region name="guest_ram" size="0x10_000_000" page_size="0x200_000" />
                              |
                              |    <!--
                              |     We intend to map in this UART into the guest's virtual address space so
                              |     we define the memory region here.
                              |    -->
                              |    <memory_region name="serial" size="0x1_000" phys_addr="0x9000000" />
                              |
                              |    <!--
                              |     We need to map in the interrupt controller's (GIC) virtual CPU interface.
                              |     This is then mapped into the guest's virtual address space as if it was
                              |     the actual interrupt controller. On ARM GICv2, not all of the interrupt
                              |     controller is hardware virtualised, so we also have a virtual driver in
                              |     the VMM code.
                              |    -->
                              |    <memory_region name="gic_vcpu" size="0x1_000" phys_addr="0x8040000" />
                              |
                              |    <protection_domain name="VMM" priority="254">
                              |        <program_image path="vmm.elf" />
                              |        <!--
                              |            Currently the VMM is expecting the address set to the variable
                              |            "guest_ram_vaddr" to be the same as the address of where the guest
                              |            sees RAM from its perspective. In this case the guest physical
                              |            starting address of RAM is 0x40000000, so we map in the guest RAM
                              |            at the same address in the VMMs virutal address space.
                              |        -->
                              |        <map mr="guest_ram" vaddr="0x40000000" perms="rw" setvar_vaddr="guest_ram_vaddr" />
                              |        <virtual_machine name="linux" >
                              |            <vcpu id="0" />
                              |            <!--
                              |             The device tree given to Linux specifies that RAM will start
                              |             at 0x40000000.
                              |            -->
                              |            <map mr="guest_ram" vaddr="0x40000000" perms="rwx" />
                              |            <!--
                              |             For simplicity we give the guest direct access to the platform's UART.
                              |             This is the same UART used by seL4 and the VMM for debug printing. The
                              |             consequence of this is that the guest can just use the serial without
                              |             trapping into the VMM and hence we do not have to emulate access.
                              |            -->
                              |            <map mr="serial" vaddr="0x9000000" perms="rw" cached="false" />
                              |            <!--
                              |             As stated above, we need to map in the virtual CPU interface into
                              |             the guest's virtual address space. Any access to the GIC from
                              |             0x8010000 - 0x8011000 will access the VCPU interface. All other
                              |             accesses will result in virtual memory faults, routed to the VMM.
                              |            -->
                              |            <map mr="gic_vcpu" vaddr="0x8010000" perms="rw" cached="false" />
                              |        </virtual_machine>
                              |        <!--
                              |            When the serial that is mapped into the guest receives input, we
                              |            want to receive an interrupt from the device. This interrupt is
                              |            delivered to the VMM, which will then deliver the IRQ to the guest,
                              |            so that it can handle it appropriately. The IRQ is for the
                              |            platform's PL011 UART the VMM is expecting the ID of the IRQ to be 1.
                              |         -->
                              |        <irq irq="33" id="1" />
                              |    </protection_domain>
                              |</system>
                              |"""
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
