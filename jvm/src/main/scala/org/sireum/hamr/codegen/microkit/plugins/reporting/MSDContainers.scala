// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.message.Position

object MSDContainers {
  @sig trait PosElement {
    def pos: Position
  }

  @datatype class system(val domain_schedule: domain_schedule,
                         val protectionDomains: ISZ[protection_domain],
                         val memoryRegions: ISZ[memory_region],
                         val channels: ISZ[channel],
                         val pos: Position) extends PosElement {
    @pure def getProtectionDomain(name: String): Option[protection_domain] = {
      for (pd <- protectionDomains if pd.name == s"${name}_MON") {
        return Some(pd)
      }
      return None()
    }
  }

  @datatype class domain_schedule(val href: String,
                                  val entries: ISZ[ScheduleEntry],
                                  val pos: Position) extends PosElement

  @datatype class ScheduleEntry(val name: String,
                                val length: String,
                                val pos: Position) extends PosElement

  @sig trait ProtectionDomainItem extends PosElement

  @enum object DomainKind {
    "Monitor"
    "Thread"
    "Pacer"
  }

  @datatype class irq(val irq: String,
                      val id: String,
                      val pos: Position) extends ProtectionDomainItem

  @datatype class protection_domain(val name: String,
                                    val kind: DomainKind.Type,
                                    val schedulingDomain: String,
                                    val optId: Option[String],
                                    val program_image: String,
                                    val maps: ISZ[map],
                                    val protection_domains: ISZ[protection_domain],
                                    val irqs: ISZ[irq],
                                    val virtual_machines: ISZ[virtual_machine],
                                    val pos: Position) extends ProtectionDomainItem

  @datatype class vcpu(val id: String,
                       val pos: Position) extends PosElement

  @datatype class virtual_machine(val name: String,
                                  val vcpu: vcpu,
                                  val maps: ISZ[map],
                                  val pos: Position) extends ProtectionDomainItem

  @datatype class setvar(val symbol: String,
                         val region_paddr: String,
                         val pos: Position) extends ProtectionDomainItem

  @datatype class program_image(val path: String,
                                val pos: Position) extends ProtectionDomainItem

  @datatype class map(val mr: String,
                      val vaddr: String,
                      val perms: String,
                      val setvar_vaddr: Option[String],
                      val pos: Position) extends ProtectionDomainItem

  @datatype class memory_region(val name: String,
                                val size: String,
                                val pos: Position) extends PosElement

  @datatype class end(val pd: String,
                      val id: String,
                      val pos: Position) extends PosElement

  @datatype class channel(val end1: end,
                          val end2: end,
                          val pos: Position) extends PosElement
}