// #Sireum
package org.sireum.hamr.codegen.act.vm

import org.sireum._
import org.sireum.hamr.codegen.act.ast
import org.sireum.hamr.codegen.act.templates._
import org.sireum.hamr.codegen.act.util.{CMakeOption, CMakeStandardOption}
import org.sireum.hamr.codegen.common.templates.CommentTemplate

object VM_Template {

  val BUILD_CROSSVM: String = "BUILD_CROSSVM"

  val data61_VM_Macros_Location: String = "<configurations/vm.h>"

  val VM_CMAKE_OPTIONS: ISZ[CMakeOption] = ISZ(
    CMakeStandardOption(BUILD_CROSSVM, F, "Checkout and configure linux to build crossvm module instead of using pre-configured rootfs")
  )

  def vm_assembly_preprocessor_includes(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      data61_VM_Macros_Location
    )
    return ret
  }

  def vm_assembly_configuration_entries(vmProcessID: String): ISZ[ast.Configuration] = {
    val ret: ISZ[ast.Configuration] = ISZ(
      ast.GenericConfiguration(s"${vmProcessID}.num_extra_frame_caps = 0;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.extra_frame_map_address = 0;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.cnode_size_bits = 23;", ISZ()),
      ast.GenericConfiguration(s"${vmProcessID}.simple_untyped24_pool = 12;", ISZ()),
    )
    return ret
  }

  def vm_assembly_configuration_macros(vmProcessId: String): ISZ[ast.Configuration] = {
    val ret: ISZ[ast.Configuration] = ISZ(
      ast.GenericConfiguration("VM_GENERAL_CONFIGURATION_DEF()", ISZ()),
      ast.GenericConfiguration(s"VM_CONFIGURATION_DEF(${vmProcessId})", ISZ()),
      ast.GenericConfiguration(s"VM_VIRTUAL_SERIAL_GENERAL_CONFIGURATION_DEF()", ISZ()),
      ast.GenericConfiguration(s"PER_VM_VIRTUAL_SERIAL_CONFIGURATION_DEF(${vmProcessId})", ISZ())
    )
    return ret
  }

  def vm_assembly_imports(): ISZ[String] = {
    val ret: ISZ[String] = ISZ(
      "<global-connectors.camkes>",
      "<seL4VMDTBPassthrough.idl4>",
      "<FileServerInterface.camkes>",
      "<FileServer/FileServer.camkes>",
      "<SerialServer/SerialServer.camkes>",
      "<TimeServer/TimeServer.camkes>",
      "<vm-connectors.camkes>",
      "<devices.camkes>")
    return ret
  }

  def vm_cross_conn_extern_dataport_method(dataportName: String): ST = {
    return st"extern dataport_caps_handle_t ${dataportName}_handle;"
  }

  def vm_cross_conn_extern_emit_method(queueName: String): ST = {
    return st"void ${queueName}_emit_underlying(void);"
  }

  def vm_cross_conn_extern_notification_methods(notificationName: String): ST = {
    return st"seL4_Word ${notificationName}_notification_badge(void);"
  }

  def vm_cross_conn_Connections(methodNamePrefix: String,
                                emitMethodNamePrefix: Option[String],
                                notificationNamePrefix: Option[String],
                                counter: Z): ST = {
    val emit_fn: String =
      if (emitMethodNamePrefix.isEmpty) "NULL"
      else s"${emitMethodNamePrefix.get}_emit_underlying"

    val consume_badge: String =
      if (notificationNamePrefix.isEmpty) "-1"
      else s"${notificationNamePrefix.get}_notification_badge()"

    val ret: ST =
      st"""connections[${counter}] = (struct camkes_crossvm_connection) {
          |  .handle = &${methodNamePrefix}_handle,
          |  .emit_fn = ${emit_fn},
          |  .consume_badge = ${consume_badge},
          |  .connection_name = "${methodNamePrefix}"
          |}"""

    return ret
  }

  def vm_cross_vm_connections(glueCodeMethods: ISZ[ST],
                              connections: ISZ[ST]): ST = {
    val numConnections: Z = connections.size

    val ret: ST =
      st"""/*
          | * Copyright 2019, Data61
          | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
          | * ABN 41 687 119 230.
          | *
          | * This software may be distributed and modified according to the terms of
          | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
          | * See "LICENSE_BSD2.txt" for details.
          | *
          | * @TAG(DATA61_BSD)
          | */
          |
          |#include <autoconf.h>
          |#include <camkes.h>
          |#include <vmlinux.h>
          |#include <sel4vm/guest_vm.h>
          |
          |#include <sel4vmmplatsupport/drivers/cross_vm_connection.h>
          |#include <sel4vmmplatsupport/drivers/pci_helper.h>
          |#include <pci/helper.h>
          |
          |#ifdef CONFIG_PLAT_QEMU_ARM_VIRT
          |#define CONNECTION_BASE_ADDRESS 0xDF000000
          |#else
          |#define CONNECTION_BASE_ADDRESS 0x3F000000
          |#endif
          |
          |#define NUM_CONNECTIONS ${numConnections}
          |static struct camkes_crossvm_connection connections[NUM_CONNECTIONS];
          |
          |${(glueCodeMethods, "\n")}
          |
          |static int consume_callback(vm_t *vm, void *cookie) {
          |    consume_connection_event(vm, (seL4_Word) cookie, true);
          |    return 0;
          |}
          |
          |void init_cross_vm_connections(vm_t *vm, void *cookie) {
          |    ${(connections, ";\n\n")};
          |
          |    for (int i = 0; i < NUM_CONNECTIONS; i++) {
          |        if (connections[i].consume_badge != -1) {
          |            int err = register_async_event_handler(connections[i].consume_badge, consume_callback, (void *)connections[i].consume_badge);
          |            ZF_LOGF_IF(err, "Failed to register_async_event_handler for init_cross_vm_connections.");
          |        }
          |    }
          |
          |    cross_vm_connections_init(vm, CONNECTION_BASE_ADDRESS, connections, ARRAY_SIZE(connections));
          |}
          |
          |DEFINE_MODULE(cross_vm_connections, NULL, init_cross_vm_connections)"""
    return ret
  }

  def settings_cmake_entries(): ISZ[ST] = {
    val ret: ST =
      st"""# Add virtual PCI device to VMM for registering cross component connectors as
          |# devices on the PCI bus.
          |set(VmPCISupport ON CACHE BOOL "" FORCE)
          |
          |# Disable libusb from being compiled.
          |set(LibUSB OFF CACHE BOOL "" FORCE)
          |
          |# Enables the option for the VM to open and load a seperate initrd file
          |set(VmInitRdFile ON CACHE BOOL "" FORCE)
          |
          |# Enable virtio console vmm module
          |set(VmVirtioConsole ON CACHE BOOL "" FORCE)
          |
          |# Make VTimers see absolute time rather than virtual time.
          |set(KernelArmVtimerUpdateVOffset OFF CACHE BOOL "" FORCE)
          |
          |# Don't trap WFI or WFE instructions in a VM.
          |set(KernelArmDisableWFIWFETraps ON CACHE BOOL "" FORCE)
          |
          |if("$${PLATFORM}" STREQUAL "qemu-arm-virt")
          |
          |    set(KernelArmCPU cortex-a53 CACHE STRING "" FORCE)
          |    set(KernelArmExportPCNTUser ON CACHE BOOL "" FORCE)
          |    set(KernelArmExportPTMRUser ON CACHE BOOL "" FORCE)
          |
          |    set(MIN_QEMU_VERSION "4.0.0")
          |    execute_process(COMMAND qemu-system-aarch64 -version OUTPUT_VARIABLE QEMU_VERSION_STR)
          |    string(
          |        REGEX
          |            MATCH
          |            "[0-9](\\.[0-9])+"
          |            QEMU_VERSION
          |            $${QEMU_VERSION_STR}
          |    )
          |    if("$${QEMU_VERSION}" VERSION_LESS "$${MIN_QEMU_VERSION}")
          |        message(WARNING "Warning: qemu version should be at least $${MIN_QEMU_VERSION}")
          |    endif()
          |
          |endif()"""
    return ISZ(ret)
  }

  def vm_cmake_var(varName: String, varValue: String): ST = {
    return st"-D${varName}=${varValue}"
  }

  def vm_cmake_DeclareCamkesArmVM(threadId: String,
                                  connectionFilenames: ISZ[String],
                                  libNames: ISZ[String]): ST = {
    return (
      st"""DeclareCamkesARMVM(
          |  ${threadId}
          |  EXTRA_SOURCES ${(connectionFilenames, "\n")}
          |  EXTRA_LIBS ${(libNames, "\n")})""")
  }

  def vm_cmakelists(vmIDs: ISZ[String],
                    vmCamkesComponents: ISZ[ST],
                    cmakeVars: ISZ[ST]): ST = {

    val ret: ST =
      st"""${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
          |
          |${CommentTemplate.safeToEditComment_cmake}
          |
          |project(arm-vm C)
          |
          |cmake_minimum_required(VERSION 3.8.2)
          |
          |project(arm-vm C)
          |
          |# including https://github.com/seL4/camkes-vm/blob/master/arm_vm_helpers.cmake
          |include($${CAMKES_ARM_VM_HELPERS_PATH})
          |#MESSAGE("CAMKES_ARM_VM_HELPERS_PATH = $${CAMKES_ARM_VM_HELPERS_PATH}")
          |
          |find_package(camkes-vm-linux REQUIRED)
          |
          |# including https://github.com/seL4/camkes-vm-linux/blob/master/vm-linux-helpers.cmake
          |include($${CAMKES_VM_LINUX_HELPERS_PATH})
          |#MESSAGE("CAMKES_VM_LINUX_HELPERS_PATH = $${CAMKES_VM_LINUX_HELPERS_PATH}")
          |
          |# including https://github.com/seL4/camkes-vm-linux/blob/master/linux-module-helpers.cmake
          |include($${CAMKES_VM_LINUX_MODULE_HELPERS_PATH})
          |#MESSAGE("CAMKES_VM_LINUX_MODULE_HELPERS_PATH = $${CAMKES_VM_LINUX_MODULE_HELPERS_PATH}")
          |
          |# including https://github.com/seL4/camkes-vm-linux/blob/master/linux-source-helpers.cmake
          |include($${CAMKES_VM_LINUX_SOURCE_HELPERS_PATH})
          |#MESSAGE("CAMKES_VM_LINUX_SOURCE_HELPERS_PATH = $${CAMKES_VM_LINUX_SOURCE_HELPERS_PATH}")
          |
          |
          |# This Project Depends on External Project(s)
          |# see https://cmake.org/cmake/help/latest/module/ExternalProject.html
          |include(ExternalProject)
          |include(external-project-helpers)
          |
          |
          |find_package(camkes-vm-images REQUIRED)
          |find_package(camkes-arm-vm REQUIRED)
          |
          |
          |#MESSAGE("CAMKES_VM_LINUX_HELPERS_PATH = $${CAMKES_VM_LINUX_HELPERS_PATH}")
          |#MESSAGE("KernelARMPlatform = $${KernelARMPlatform}")
          |#MESSAGE("CAMKES_ARM_VM_DIR = $${CAMKES_ARM_VM_DIR}")
          |#MESSAGE("CAMKES_VM_IMAGES_DIR = $${CAMKES_VM_IMAGES_DIR}")
          |#MESSAGE("CAMKES_VM_LINUX_DIR = $${CAMKES_VM_LINUX_DIR}")
          |
          |#MESSAGE("CMAKE_CURRENT_BINARY_DIR = $${CMAKE_CURRENT_BINARY_DIR}")
          |#MESSAGE("CMAKE_CURRENT_SOURCE_DIR = $${CMAKE_CURRENT_SOURCE_DIR}")
          |#MESSAGE("CMAKE_C_COMPILER = $${CMAKE_C_COMPILER}")
          |#MESSAGE("BASE_C_FLAGS = $${BASE_C_FLAGS}")
          |
          |
          |
          |# Create our CPP Flags based on ARM VM config variables
          |if("$${KernelARMPlatform}" STREQUAL "exynos5422")
          |    set(cpp_flags "-DKERNELARMPLATFORM_EXYNOS5422")
          |    set(linux_repo "https://github.com/hardkernel/linux.git")
          |    set(linux_tag "4.14.87-153")
          |    set(linux_arch "arm")
          |    set(linux_cross_compile "arm-linux-gnueabi-")
          |elseif("$${KernelARMPlatform}" STREQUAL "qemu-arm-virt")
          |    set(cpp_flags "-DKERNELARMPLATFORM_QEMU-ARM-VIRT")
          |    set(linux_repo "https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git")
          |    set(linux_tag "v4.9.189")
          |    set(linux_arch "arm64")
          |    set(linux_cross_compile "aarch64-linux-gnu-")
          |    include(simulation)
          |    set(SIMULATION ON CACHE BOOL "Generate simulation script to run qemu with the proper arguments")
          |    if(SIMULATION)
          |        GenerateSimulateScript()
          |    endif()
          |endif()
          |
          |
          |
          |if(BUILD_CROSSVM)
          |    MESSAGE("Not using preconfigured rootfs, will download a vanilla linux image instead")
          |
          |    set(rootfs_file "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/rootfs.cpio.gz")
          |    # Checkout and configure linux to build crossvm module
          |    ExternalProject_Add(
          |        checkout_linux
          |        GIT_REPOSITORY
          |        $${linux_repo}
          |        GIT_TAG
          |        $${linux_tag}
          |        GIT_SHALLOW
          |        1
          |        GIT_PROGRESS
          |        1
          |        BUILD_COMMAND
          |        ""
          |        INSTALL_COMMAND
          |        ""
          |        CONFIGURE_COMMAND
          |        ""
          |        SOURCE_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/linux_out
          |    )
          |    # Linux config and symvers are to be copied to unpacked archive
          |    set(linux_config "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux_configs/config")
          |    set(linux_symvers "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux_configs/Module.symvers")
          |    # Configure unpacked archive with config and symvers
          |    ConfigureLinux(
          |        $${CMAKE_CURRENT_BINARY_DIR}/linux_out
          |        $${linux_config}
          |        $${linux_symvers}
          |        configure_vm_linux
          |        ARCH
          |        $${linux_arch}
          |        CROSS_COMPILE
          |        $${linux_cross_compile}
          |        DEPENDS
          |        checkout_linux
          |    )
          |
          |    # Compile CrossVM Dataport Module
          |    DefineLinuxModule(
          |        $${CAMKES_VM_LINUX_DIR}/camkes-linux-artifacts/camkes-linux-modules/camkes-connector-modules/connection
          |        output_module
          |        output_module_target
          |        KERNEL_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/linux_out
          |        ARCH
          |        $${linux_arch}
          |        CROSS_COMPILE
          |        $${linux_cross_compile}
          |        DEPENDS
          |        checkout_linux
          |        configure_vm_linux
          |    )
          |    AddFileToOverlayDir(
          |        "connection.ko"
          |        $${output_module}
          |        "lib/modules/4.14.87/kernel/drivers/vmm"
          |        overlay
          |        DEPENDS
          |        output_module_target
          |    )
          |
          |    # Complile CrossVM Dataport Apps
          |    ExternalProject_Add(
          |        dataport-apps
          |        URL
          |        file:///$${CAMKES_VM_LINUX_DIR}/camkes-linux-artifacts/camkes-linux-apps/camkes-connector-apps/pkgs/dataport
          |        BINARY_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/dataport_apps
          |        INSTALL_COMMAND
          |        ""
          |        BUILD_ALWAYS
          |        ON
          |        EXCLUDE_FROM_ALL
          |        CMAKE_ARGS
          |        -DCMAKE_C_COMPILER=$${CMAKE_C_COMPILER}
          |    )
          |    AddExternalProjFilesToOverlay(
          |        dataport-apps
          |        $${CMAKE_CURRENT_BINARY_DIR}/dataport_apps
          |        overlay
          |        "usr/bin"
          |        FILES
          |        dataport_read
          |        dataport_write
          |    )
          |
          |    # Complile CrossVM Event Apps
          |    ExternalProject_Add(
          |        event-apps
          |        URL
          |        file:///$${CAMKES_VM_LINUX_DIR}/camkes-linux-artifacts/camkes-linux-apps/camkes-connector-apps/pkgs/emits_event
          |        BINARY_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/emits_event_apps
          |        INSTALL_COMMAND
          |        ""
          |        BUILD_ALWAYS
          |        ON
          |        EXCLUDE_FROM_ALL
          |        CMAKE_ARGS
          |        -DCMAKE_C_COMPILER=$${CMAKE_C_COMPILER}
          |    )
          |    AddExternalProjFilesToOverlay(
          |        event-apps
          |        $${CMAKE_CURRENT_BINARY_DIR}/emits_event_apps
          |        overlay
          |        "usr/bin"
          |        FILES
          |        emits_event_emit
          |    )
          |
          |    # Complile CrossVM Consume Event Apps
          |    ExternalProject_Add(
          |        consume-event-apps
          |        URL
          |        file:///$${CAMKES_VM_LINUX_DIR}/camkes-linux-artifacts/camkes-linux-apps/camkes-connector-apps/pkgs/consumes_event
          |        BINARY_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/consume_event_apps
          |        INSTALL_COMMAND
          |        ""
          |        BUILD_ALWAYS
          |        ON
          |        EXCLUDE_FROM_ALL
          |        CMAKE_ARGS
          |        -DCMAKE_C_COMPILER=$${CMAKE_C_COMPILER}
          |    )
          |    AddExternalProjFilesToOverlay(
          |        consume-event-apps
          |        $${CMAKE_CURRENT_BINARY_DIR}/consume_event_apps
          |        overlay
          |        "usr/bin"
          |        FILES
          |        consumes_event_wait
          |    )
          |
          |    # Add script to initialise dataport module
          |    AddFileToOverlayDir(
          |        "S90crossvm_module_init"
          |        $${CMAKE_CURRENT_SOURCE_DIR}/overlay_files/init_scripts/cross_vm_module_init
          |        "etc/init.d"
          |        overlay
          |    )
          |else()
          |    MESSAGE("Using pre-configured rootfs")
          |
          |    # Use pre-configured rootfs file with crossvm modules and apps installed
          |    set(rootfs_file "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/rootfs_crossvm.cpio.gz")
          |endif()
          |
          |
          |# Complile CrossVM Event Apps ${(vmIDs, " and ")}
          |foreach(item IN ITEMS ${(vmIDs, " ")})
          |    ExternalProject_Add(
          |        $${item}
          |        SOURCE_DIR
          |        $${CMAKE_CURRENT_SOURCE_DIR}/apps/$${item}
          |        BINARY_DIR
          |        $${CMAKE_CURRENT_BINARY_DIR}/$${item}
          |        INSTALL_COMMAND
          |        ""
          |        BUILD_ALWAYS
          |        ON
          |        EXCLUDE_FROM_ALL
          |        CMAKE_ARGS
          |        -DCMAKE_C_COMPILER=$${CMAKE_C_COMPILER}
          |        ${(cmakeVars, "\n")}
          |        -DCMAKE_C_FLAGS=$${BASE_C_FLAGS}
          |    )
          |
          |    AddExternalProjFilesToOverlay(
          |        $${item}
          |        $${CMAKE_CURRENT_BINARY_DIR}/$${item}
          |        overlay
          |        "usr/bin"
          |        FILES
          |        $${item}
          |    )
          |endforeach()
          |
          |# Overwrite inittab file for using the virtio console hvc0.
          |AddFileToOverlayDir(
          |    "inittab"
          |    $${CMAKE_CURRENT_SOURCE_DIR}/overlay_files/init_scripts/inittab_hvc0
          |    "etc"
          |    overlay
          |)
          |
          |# Construct new rootfs
          |AddOverlayDirToRootfs(
          |    overlay
          |    $${rootfs_file}
          |    "buildroot"
          |    "rootfs_install"
          |    output_overlayed_rootfs_location
          |    rootfs_target
          |    GZIP
          |)
          |
          |AddToFileServer("linux-initrd"
          |                $${output_overlayed_rootfs_location}
          |                DEPENDS rootfs_target)
          |
          |# Add linux kernel image to file server
          |AddToFileServer("linux"
          |                "$${CAMKES_VM_IMAGES_DIR}/$${KernelARMPlatform}/linux")
          |
          |AddCamkesCPPFlag(
          |    cpp_flags
          |    CONFIG_VARS
          |    VmEmmc2NoDMA
          |    VmVUSB
          |    VmVchan
          |    Tk1DeviceFwd
          |    Tk1Insecure
          |    VmVirtioNetVirtqueue
          |)
          |
          |DefineCAmkESVMFileServer()
          |
          |CAmkESAddImportPath($${CMAKE_CURRENT_SOURCE_DIR}/$${KernelARMPlatform}/)
          |
          |# Define our VM Component with our cross vm dataports glue code
          |${(vmCamkesComponents, "\n\n")}
          |
          |CAmkESAddCPPInclude($${CAMKES_ARM_VM_DIR}/components/VM)"""
    return ret
  }

  def vm_qemu_arm_virt_devices_camkes(componentIDs: ISZ[String]): ST = {
    assert(componentIDs.size <= 2, "Currently only expecting two VMs (e.g. sender/receiver")

    var i = 0
    val entries: ISZ[ST] = componentIDs.map(componentID => {
      val vmid = s"VM${i}"
      i = i + 1
      st"""${componentID}.linux_address_config = {
          |  "linux_ram_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_paddr_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_size" : VAR_STRINGIZE(${vmid}_RAM_SIZE),
          |  "linux_ram_offset" : VAR_STRINGIZE(VM_RAM_OFFSET),
          |  "dtb_addr" : VAR_STRINGIZE(${vmid}_DTB_ADDR),
          |  "initrd_max_size" : VAR_STRINGIZE(VM_INITRD_MAX_SIZE),
          |  "initrd_addr" : VAR_STRINGIZE(${vmid}_INITRD_ADDR)
          |};
          |
          |${componentID}.linux_image_config = {
          |  "linux_bootcmdline" : "console=hvc0 nosmp rw debug loglevel=8 pci=nomsi,realloc=off,bios initcall_blacklist=clk_disable_unused",
          |  "linux_stdout" : "hvc0",
          |  "dtb_name": "",
          |  "initrd_name" : "linux-initrd"
          |};
          |
          |${componentID}.dtb = dtb([{}]);
          |
          |${componentID}.irq = [];
          |
          |${componentID}.mmios = [
          |  ${vmid}_MMIOS_ICI, // Interrupt Controller Virtual CPU interface (Virtual Machine view)
          |];
          |
          |${componentID}.untyped_mmios = [
          |  ${vmid}_MMIOS_LKMR, // Linux kernel memory regions
          |];
          |"""
    })
    val ret: ST =
      st"""${CommentTemplate.safeToEditComment_c}
          |
          |#include <configurations/vm.h>
          |#define VM_RAM_OFFSET      0x00000000
          |#define VM_INITRD_MAX_SIZE 0x3200000 //50 MB
          |
          |#define VM0_RAM_BASE       0x40000000
          |#define VM0_RAM_SIZE       0x8000000
          |#define VM0_DTB_ADDR       0x47000000 //VM0_RAM_BASE + 0x7000000
          |#define VM0_INITRD_ADDR    0x43e00000 //VM0_DTB_ADDR - VM_INITRD_MAX_SIZE
          |
          |#define VM0_MMIOS_ICI      "0x8040000:0x1000:12"
          |#define VM0_MMIOS_LKMR     "0x40000000:27"
          |
          |
          |#define VM1_RAM_BASE       0x48000000
          |#define VM1_RAM_SIZE       0x8000000
          |#define VM1_DTB_ADDR       0x4f000000 //VM1_RAM_BASE + 0x7000000
          |#define VM1_INITRD_ADDR    0x4be00000 //VM1_DTB_ADDR - VM_INITRD_MAX_SIZE
          |
          |#define VM1_MMIOS_ICI      "0x8040000:0x1000:12"
          |#define VM1_MMIOS_LKMR     "0x48000000:27"
          |
          |
          |assembly {
          |  composition {}
          |  configuration {
          |    ${(entries, "\n\n")}
          |  }
          |}"""
    return ret
  }

  def vm_exynos5422_devices_camkes(componentIDs: ISZ[String]): ST = {
    assert(componentIDs.size <= 2, "Currently only expecting two VMs (e.g. sender/receiver")

    var i = 0
    val entries: ISZ[ST] = componentIDs.map((componentId: String) => {
      val vmid = s"VM${i}"
      i = i + 1
      st"""${componentId}.linux_address_config = {
          |  "linux_ram_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_paddr_base" : VAR_STRINGIZE(${vmid}_RAM_BASE),
          |  "linux_ram_size" : VAR_STRINGIZE(${vmid}_RAM_SIZE),
          |  "linux_ram_offset" : VAR_STRINGIZE(VM_RAM_OFFSET),
          |  "dtb_addr" : VAR_STRINGIZE(${vmid}_DTB_ADDR),
          |  "initrd_max_size" : VAR_STRINGIZE(VM_INITRD_MAX_SIZE),
          |  "initrd_addr" : VAR_STRINGIZE(${vmid}_INITRD_ADDR),
          |};
          |
          |${componentId}.linux_image_config = {
          |  "linux_bootcmdline" : "console=hvc0 root=/dev/ram0 nosmp rw debug loglevel=8 pci=nomsi initcall_blacklist=clk_disable_unused",
          |  "linux_stdout" : "hvc0",
          |  "dtb_name" : "",
          |  "initrd_name" : "linux-initrd",
          |};
          |
          |${componentId}.mmios = [
          |  "0x10000000:0x1000:12", // CHIP ID
          |  "0x10486000:0x1000:12"  // VCPU
          |];
          |
          |${componentId}.untyped_mmios = [
          |  ${vmid}_RAM_MMIOS_BASE  // RAM
          |];
          |
          |${componentId}.irqs = [];
          |
          |${componentId}.dtb = dtb([{}]);"""
    })

    val ret: ST =
      st"""${CommentTemplate.safeToEditComment_c}
          |
          |#include <configurations/vm.h>
          |
          |#define VM_RAM_OFFSET 0
          |#define VM_INITRD_MAX_SIZE 0x1900000 // 25 MB
          |
          |#define VM0_RAM_BASE       0x40000000
          |#define VM0_RAM_SIZE       0x8000000
          |#define VM0_DTB_ADDR       0x47000000  // VM0_RAM_BASE + 0x7000000
          |#define VM0_INITRD_ADDR    0x45700000  // VM0_DTB_ADDR - VM_INITRD_MAX_SIZE
          |
          |#define VM0_RAM_MMIOS_BASE "0x40000000:27"
          |
          |#define VM1_RAM_BASE       0x50000000
          |#define VM1_RAM_SIZE       0x8000000
          |#define VM1_DTB_ADDR       0x57000000  // VM1_RAM_BASE + 0x7000000
          |#define VM1_INITRD_ADDR    0x55700000  // VM1_DTB_ADDR - VM_INITRD_MAX_SIZE
          |
          |#define VM1_RAM_MMIOS_BASE "0x48000000:27"
          |
          |assembly {
          |  composition {}
          |  configuration {
          |    ${(entries, "\n\n")}
          |  }
          |}"""
    return ret
  }

  def vm_overlay_scripts__init_scripts__cross_vm_module_init(): ST = {
    val ret: ST =
      st"""#!/bin/sh
          |
          |${CommentTemplate.doNotEditComment_cmake}
          |
          |insmod /lib/modules/4.14.87/kernel/drivers/vmm/connection.ko"""
    return ret
  }

  def vm_overlay_script__init_scripts__inittab_hvc0(): ST = {
    val ret: ST =
      st"""# @TAG(CUSTOM)
          |# /etc/inittab
          |#
          |${CommentTemplate.safeToEditComment_cmake}
          |#
          |# Copyright (C) 2001 Erik Andersen <andersen@codepoet.org>
          |#
          |# Note: BusyBox init doesn't support runlevels.  The runlevels field is
          |# completely ignored by BusyBox init. If you want runlevels, use
          |# sysvinit.
          |#
          |# Format for each entry: <id>:<runlevels>:<action>:<process>
          |#
          |# id        == tty to run on, or empty for /dev/console
          |# runlevels == ignored
          |# action    == one of sysinit, respawn, askfirst, wait, and once
          |# process   == program to run
          |
          |# Startup the system
          |::sysinit:/bin/mount -t proc proc /proc
          |::sysinit:/bin/mount -o remount,rw /
          |::sysinit:/bin/mkdir -p /dev/pts /dev/shm
          |::sysinit:/bin/mount -a
          |::sysinit:/sbin/swapon -a
          |null::sysinit:/bin/ln -sf /proc/self/fd /dev/fd
          |null::sysinit:/bin/ln -sf /proc/self/fd/0 /dev/stdin
          |null::sysinit:/bin/ln -sf /proc/self/fd/1 /dev/stdout
          |null::sysinit:/bin/ln -sf /proc/self/fd/2 /dev/stderr
          |::sysinit:/bin/hostname -F /etc/hostname
          |# now run any rc scripts
          |::sysinit:/etc/init.d/rcS
          |
          |# Put a getty on the serial port
          |hvc0:2345:respawn:/sbin/getty -L 9600 hvc0
          |
          |# Stuff to do for the 3-finger salute
          |#::ctrlaltdel:/sbin/reboot
          |
          |# Stuff to do before rebooting
          |::shutdown:/etc/init.d/rcK
          |::shutdown:/sbin/swapoff -a
          |::shutdown:/bin/umount -a -r"""
    return ret
  }

  /* @param libPathNames should be path, libName pairs
   */
  def vm_cmakelists_app(processID: String,
                        libNames: ISZ[String],
                        addSubDirs: ISZ[ST]): ST = {

    val ret: ST =
      st"""${CMakeTemplate.CMAKE_MINIMUM_REQUIRED_VERSION}
          |
          |${CommentTemplate.safeToEditComment_cmake}
          |
          |project(${processID} C)
          |
          |${CMakeTemplate.CMAKE_SET_CMAKE_C_STANDARD}
          |
          |${(addSubDirs, "\n\n")}
          |
          |add_executable(${processID} ${processID}.c)
          |
          |target_link_libraries(${processID}
          |                      ${(libNames, "\n")}
          |                      -static-libgcc -static)"""
    return ret
  }

  def vm_app_dummy(vmProcessId: String,
                   includes: ISZ[String]): ST = {
    val _includes = includes.map((m: String) => st"#include ${m}")

    val ret: ST =
      st"""#include <stdio.h>
          |#include <stdlib.h>
          |#include <fcntl.h>
          |#include <unistd.h>
          |#include <assert.h>
          |#include <string.h>
          |
          |#include <sys/types.h>
          |#include <sys/stat.h>
          |#include <sys/mman.h>
          |#include <errno.h>
          |
          |${(_includes, "\n")}
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |int main(int argc, char *argv[]) {
          |  printf("VM App ${vmProcessId} started\n");
          |  return 0;
          |}"""
    return ret
  }

  def makeDirVariable(s: String): String = {
    return s"${s}_DIR"
  }

  def cmakeReferenceVar(s: String): String = {
    return s"$${${s}}"
  }
}
