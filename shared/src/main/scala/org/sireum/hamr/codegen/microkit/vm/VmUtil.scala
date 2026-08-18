// #Sireum
package org.sireum.hamr.codegen.microkit.vm

import org.sireum._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.bytesToKiBytes

object VmUtil {

  // 0x10_000_000
  val defaultVmRamSizeInKiBytes: Z = bytesToKiBytes(26_8435_456)

  // 0x200_0000
  val defaultVmPageSizeInKiBytes: Z = bytesToKiBytes(2_097_152)

  val defaultVmPhysicalAddress: Z = bytesToKiBytes(1_073_741_824)

  /** Address the guest RAM memory region is mapped at in the virtual machine's own
    * address space, i.e. the guest physical address at which the guest sees its RAM.
    * GUEST_RAM_START_GPA in the generated config header must agree with this, as
    * libvmm is told the guest's RAM layout via [[VmUser.vmUserCode]]'s guest_init call.
    */
  // 0x40_000_000
  val defaultVmGuestRamGpaInKiBytes: Z = bytesToKiBytes(1_073_741_824)

  /** Guest physical address the DTB is copied to. */
  val defaultGuestDtbGpaInHex: String = "0x4f000000"

  /** Guest physical address the initial RAM disk is copied to.
    *
    * libvmm copies the initial RAM disk here but does not touch the DTB's chosen node,
    * so the guest finds it only at whatever 'linux,initrd-start' says.  Both this and
    * [[VmMakefileTemplate.overlay_dts]] therefore have to name the same address, which
    * is why the DTS reads it from here rather than repeating the literal.
    */
  val defaultGuestInitRamDiskGpaInHex: String = "0x4d000000"

  /** End of the guest physical window the DTB reserves for the initial RAM disk.
    * Codegen cannot size the RAM disk, which is downloaded at build time, so this
    * reserves the same 16MiB window libvmm's own examples use.
    */
  val defaultGuestInitRamDiskEndGpaInHex: String = "0x4e000000"

  /** The VM component's user-editable configuration header, giving libvmm the guest
    * memory layout and the device IRQs passed through to the guest.
    *
    * @param guestRamStartGpaInHex guest physical address of the guest's RAM
    * @param guestRamSizeInHex size of the guest's RAM
    * @param guestDtbGpaInHex guest physical address the DTB is copied to
    * @param guestInitRamDiskGpaInHex guest physical address the initial RAM disk is copied to
    * @param maxIrqs number of entries in the pass-through IRQ table
    */
  def vmm_config(guestRamStartGpaInHex: String,
                 guestRamSizeInHex: String,
                 guestDtbGpaInHex: String,
                 guestInitRamDiskGpaInHex: String,
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
          |${CommentTemplate.safeToEditComment_slash}
          |
          |#if defined(BOARD_qemu_virt_aarch64)
          |// Where the guest sees its RAM. This must match the memory region the virtual
          |// machine's <map> element gives it in the system description.
          |#define GUEST_RAM_START_GPA       $guestRamStartGpaInHex
          |#define GUEST_RAM_SIZE            $guestRamSizeInHex
          |// Where the DTB and initial RAM disk are copied to within that RAM
          |#define GUEST_DTB_GPA             $guestDtbGpaInHex
          |#define GUEST_INIT_RAM_DISK_GPA   $guestInitRamDiskGpaInHex
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
          |// Device IRQs passed through to the guest: the guest sees interrupt 'irq',
          |// and this component is notified on 'channel' when it fires.
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
