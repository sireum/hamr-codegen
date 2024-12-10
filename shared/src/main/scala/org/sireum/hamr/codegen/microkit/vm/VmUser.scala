// #Sireum

package org.sireum.hamr.codegen.microkit.vm

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util

object VmUser {
  def vmUserCode(componentPath: ST,
                 guestRamVaddr: ST
                ): ST = {
    val content: ST =
      st"""
          |
          |#include <$componentPath.h>
          |#include <${componentPath}_user.h>
          |#include <libvmm/arch/aarch64/linux.h>
          |#include <libvmm/arch/aarch64/fault.h>
          |#include <libvmm/guest.h>
          |#include <libvmm/virq.h>
          |
          |${Util.safeToEdit}
          |
          |// Data for the guest's kernel image.
          |extern char _guest_kernel_image[];
          |extern char _guest_kernel_image_end[];
          |
          |// Data for the device tree to be passed to the kernel.
          |extern char _guest_dtb_image[];
          |extern char _guest_dtb_image_end[];
          |
          |// Data for the initial RAM disk to be passed to the kernel.
          |extern char _guest_initrd_image[];
          |extern char _guest_initrd_image_end[];
          |
          |// Microkit will set this variable to the start of the guest RAM memory region.
          |$guestRamVaddr;
          |
          |static int get_dev_irq_by_ch(microkit_channel ch);
          |static int get_dev_ch_by_irq(int irq, microkit_channel *ch);
          |static void pt_dev_ack(size_t vcpu_id, int irq, void *cookie);
          |
          |void ${componentPath}_initialize(void) {
          |  // Initialise the VMM, the VCPU(s), and start the guest
          |  LOG_VMM("starting \"%s\"\n", microkit_name);
          |
          |  // Place all the binaries in the right locations before starting the guest
          |
          |  size_t kernel_size = _guest_kernel_image_end - _guest_kernel_image;
          |  size_t dtb_size = _guest_dtb_image_end - _guest_dtb_image;
          |  size_t initrd_size = _guest_initrd_image_end - _guest_initrd_image;
          |
          |  // https://github.com/au-ts/libvmm/blob/a996382581b9dbb7f067b25f312e87264c7b8ace/include/libvmm/arch/aarch64/linux.h#L37
          |  // https://github.com/au-ts/libvmm/blob/a996382581b9dbb7f067b25f312e87264c7b8ace/src/arch/aarch64/linux.c#L11
          |  uintptr_t kernel_pc = linux_setup_images(top_impl_Instance_consumer_p_p_consumer_VM_Guest_RAM_vaddr,
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
          |  // Initialise the virtual GIC driver
          |  bool success = virq_controller_init(GUEST_VCPU_ID);
          |  if (!success) {
          |    LOG_VMM_ERR("Failed to initialise emulated interrupt controller\n");
          |    return;
          |  }
          |
          |  // Register Pass-through device IRQs
          |  for(int i=0; i < MAX_IRQS; i++) {
          |    success = virq_register(GUEST_VCPU_ID, mk_irqs[i].irq, &pt_dev_ack, NULL);
          |    // Just in case there are already interrupts available to handle, we ack them here.
          |    microkit_irq_ack(mk_irqs[i].channel);
          |  }
          |
          |  // Finally start the guest /
          |  // https://github.com/au-ts/libvmm/blob/a996382581b9dbb7f067b25f312e87264c7b8ace/include/libvmm/guest.h#L10
          |  // https://github.com/au-ts/libvmm/blob/a996382581b9dbb7f067b25f312e87264c7b8ace/src/guest.c#L11
          |  guest_start(GUEST_VCPU_ID, kernel_pc, GUEST_DTB_VADDR, GUEST_INIT_RAM_DISK_VADDR);
          |
          |  LOG_VMM("Guest started, leaving ${componentPath}_initialize");
          |}
          |
          |void ${componentPath}_timeTriggered(void) {
          |  // implement me
          |  printf("%s: ${componentPath}_timeTriggered invoked\n", microkit_name);
          |}
          |
          |void ${componentPath}_notify(microkit_channel ch) {
          |  switch (ch) {
          |    case SERIAL_IRQ_CH: {
          |      bool success = virq_inject(GUEST_VCPU_ID, SERIAL_IRQ);
          |      if (!success) {
          |        LOG_VMM_ERR("IRQ %d dropped on vCPU %d\n", SERIAL_IRQ, GUEST_VCPU_ID);
          |      }
          |      break;
          |    }
          |    default:
          |      printf("Unexpected channel, ch: 0x%lx\n", ch);
          |  }
          |}
          |
          |/*
          | * The primary purpose of the VMM after initialisation is to act as a fault-handler.
          | * Whenever our guest causes an exception, it gets delivered to this entry point for
          | * the VMM to handle.
          | */
          |seL4_Bool fault(microkit_child child, microkit_msginfo msginfo, microkit_msginfo *reply_msginfo) {
          |    // https://github.com/au-ts/libvmm/blob/29aceb2acb7611cc5678d6fc235913684af7893e/src/arch/aarch64/fault.c#L436
          |    bool success = fault_handle(child, msginfo);
          |    if (success) {
          |        // Now that we have handled the fault successfully, we reply to it so
          |        // that the guest can resume execution.
          |        *reply_msginfo = microkit_msginfo_new(0, 0);
          |        return seL4_True;
          |    }
          |
          |    return seL4_False;
          |}
          |
          |static int get_dev_irq_by_ch(microkit_channel ch) {
          |  for(int i=0; i < MAX_IRQS; i++) {
          |    if (mk_irqs[i].channel == ch) {
          |      return mk_irqs[i].irq;
          |    }
          |  }
          |
          |  return -1;
          |}
          |
          |static int get_dev_ch_by_irq(int irq, microkit_channel *ch) {
          |  for(int i=0; i < MAX_IRQS; i++) {
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
          |  // For now we by default simply ack the IRQ, we have not
          |  // come across a case yet where more than this needs to be done.
          |  microkit_channel ch = 0;
          |  int status = get_dev_ch_by_irq(irq, &ch);
          |  if (!status) {
          |    microkit_irq_ack(ch);
          |  }
          |}
          |"""
    return content
  }

}
