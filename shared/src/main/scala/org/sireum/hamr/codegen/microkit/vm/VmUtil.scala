// #Sireum
package org.sireum.hamr.codegen.microkit.vm

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util

object VmUtil {

  @pure def vmm_c(guestRamVaddr: String,
                  initMethodName: String,
                  computeMethodName: String): ST = {

    val content = st"""#include <stddef.h>
                      |#include <stdint.h>
                      |#include <microkit.h>
                      |#include <libvmm/util/util.h>
                      |#include <libvmm/arch/aarch64/vgic/vgic.h>
                      |#include <libvmm/arch/aarch64/linux.h>
                      |#include <libvmm/arch/aarch64/fault.h>
                      |#include <libvmm/arch/aarch64/smc.h>
                      |#include <libvmm/guest.h>
                      |#include <libvmm/virq.h>
                      |#include <libvmm/tcb.h>
                      |#include <libvmm/vcpu.h>
                      |#include "vmm_config.h"
                      |
                      |/* Data for the guest's kernel image. */
                      |extern char _guest_kernel_image[];
                      |extern char _guest_kernel_image_end[];
                      |
                      |/* Data for the device tree to be passed to the kernel. */
                      |extern char _guest_dtb_image[];
                      |extern char _guest_dtb_image_end[];
                      |
                      |/* Data for the initial RAM disk to be passed to the kernel. */
                      |extern char _guest_initrd_image[];
                      |extern char _guest_initrd_image_end[];
                      |
                      |/* Microkit will set this variable to the start of the guest RAM memory region. */
                      |uintptr_t $guestRamVaddr;
                      |
                      |static int get_dev_irq_by_ch(microkit_channel ch) {
                      |  for(int i=0; i<MAX_IRQS; i++) {
                      |    if (mk_irqs[i].channel == ch) {
                      |      return mk_irqs[i].irq;
                      |    }
                      |  }
                      |
                      |    return -1;
                      |}
                      |
                      |static int get_dev_ch_by_irq(int irq, microkit_channel *ch) {
                      |  for(int i=0; i<MAX_IRQS; i++) {
                      |    if (mk_irqs[i].irq == irq) {
                      |      *ch = mk_irqs[i].channel;
                      |      return 0;
                      |    }
                      |  }
                      |
                      |  return -1;
                      |}
                      |
                      |static void pt_dev_ack(size_t vcpu_id, int irq, void *cookie) {
                      |  /*
                      |   * For now we by default simply ack the IRQ, we have not
                      |   * come across a case yet where more than this needs to be done.
                      |   */
                      |  microkit_channel ch = 0;
                      |  int status = get_dev_ch_by_irq(irq, &ch);
                      |  if (!status) {
                      |    microkit_irq_ack(ch);
                      |  }
                      |}
                      |
                      |
                      |void $initMethodName(void) {
                      |  /* Initialise the VMM, the VCPU(s), and start the guest */
                      |  LOG_VMM("starting \"%s\"\n", microkit_name);
                      |  /* Place all the binaries in the right locations before starting the guest */
                      |  size_t kernel_size = _guest_kernel_image_end - _guest_kernel_image;
                      |  size_t dtb_size = _guest_dtb_image_end - _guest_dtb_image;
                      |  size_t initrd_size = _guest_initrd_image_end - _guest_initrd_image;
                      |  uintptr_t kernel_pc = linux_setup_images($guestRamVaddr,
                      |                                          (uintptr_t) _guest_kernel_image,
                      |                                          kernel_size,
                      |                                          (uintptr_t) _guest_dtb_image,
                      |                                          GUEST_DTB_VADDR,
                      |                                          dtb_size,
                      |                                          (uintptr_t) _guest_initrd_image,
                      |                                          GUEST_INIT_RAM_DISK_VADDR,
                      |                                          initrd_size);
                      |
                      |  if (!kernel_pc) {
                      |    LOG_VMM_ERR("Failed to initialise guest images\n");
                      |    return;
                      |  }
                      |
                      |  /* Initialise the virtual GIC driver */
                      |  bool success = virq_controller_init(GUEST_VCPU_ID);
                      |  if (!success) {
                      |    LOG_VMM_ERR("Failed to initialise emulated interrupt controller\n");
                      |    return;
                      |  }
                      |
                      |  /* Register Pass-through device IRQs */
                      |  for(int i=0; i<MAX_IRQS; i++) {
                      |    success = virq_register(GUEST_VCPU_ID, mk_irqs[i].irq, &pt_dev_ack, NULL);
                      |    /* Just in case there are already interrupts available to handle, we ack them here. */
                      |    microkit_irq_ack(mk_irqs[i].channel);
                      |  }
                      |
                      |  /* Finally start the guest */
                      |  guest_start(GUEST_VCPU_ID, kernel_pc, GUEST_DTB_VADDR, GUEST_INIT_RAM_DISK_VADDR);
                      |}
                      |
                      |void $computeMethodName(void) {
                      |  // implement me
                      |}
                      |
                      |/*
                      | * The primary purpose of the VMM after initialisation is to act as a fault-handler.
                      | * Whenever our guest causes an exception, it gets delivered to this entry point for
                      | * the VMM to handle.
                      | */
                      |seL4_Bool fault(microkit_child child, microkit_msginfo msginfo, microkit_msginfo *reply_msginfo) {
                      |    bool success = fault_handle(child, msginfo);
                      |    if (success) {
                      |        /* Now that we have handled the fault successfully, we reply to it so
                      |         * that the guest can resume execution. */
                      |        *reply_msginfo = microkit_msginfo_new(0, 0);
                      |        return seL4_True;
                      |    }
                      |
                      |    return seL4_False;
                      |}
                      |"""
    return content
  }

  def vmm_config(guestDtbVaddrInHex: String,
                 guestInitRamDiskVaddrInHex: String,
                 maxIrqs: Z): ST = {
    val ret =
      st"""/*
          | * Copyright 2024, DornerWorks
          | *
          | * SPDX-License-Identifier: BSD-2-Clause
          | */
          |#pragma once
          |
          |#include <microkit.h>
          |
          |${Util.safeToEdit}
          |
          |#if defined(BOARD_qemu_virt_aarch64)
          |#define GUEST_DTB_VADDR           $guestDtbVaddrInHex
          |#define GUEST_INIT_RAM_DISK_VADDR $guestInitRamDiskVaddrInHex
          |#else
          |#error Need to define guest kernel image address and DTB address
          |#endif
          |
          |#define MAX_IRQS $maxIrqs
          |
          |#if defined(BOARD_qemu_virt_aarch64)
          |#define SERIAL_IRQ_CH 1
          |#define SERIAL_IRQ 33
          |#else
          |#error Need to define IRQs
          |#endif
          |
          |struct mk_irq {
          |  int irq;
          |  microkit_channel channel;
          |};
          |
          |struct mk_irq mk_irqs[MAX_IRQS] = {
          |  { // Serial
          |    .irq = SERIAL_IRQ,
          |    .channel = SERIAL_IRQ_CH
          |  }
          |};
          |"""
    return ret
  }
}
