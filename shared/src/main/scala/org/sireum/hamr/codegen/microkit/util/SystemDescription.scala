// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.microkit.util.Util.KiBytesToHex

@datatype class SystemDescription (val schedulingDomains: ISZ[SchedulingDomain],
                                   val protectionDomains: ISZ[ProtectionDomain],
                                   val memoryRegions: ISZ[MemoryRegion],
                                   val channels: ISZ[Channel]) {

  val contentMarker: Marker = Marker(
    beginMarker = "<!-- BEGIN MSD CONTENT MARKER -->",
    endMarker = "<!-- END MSD CONTENT MARKER -->")

  @pure def getMarkers: ISZ[Marker] = {
    return ISZ(contentMarker)
  }

  val stSchedulingDomain: Option[ST] =
    if (schedulingDomains.nonEmpty) Some(
      st"""<domain_schedule>
          |  ${(for( sd <- schedulingDomains) yield sd.prettyST, "\n")}
          |</domain_schedule>""")
    else None()

  @pure def prettyST: ST = {
    val stProtectionDomains: ISZ[ST] = for (pd <- protectionDomains) yield pd.prettyST
    val stMemoryRegions: ISZ[ST] = for (mr <- memoryRegions) yield mr.prettyST
    val stChannels: ISZ[ST] = for (c <- channels) yield c.prettyST
    val ret =
      st"""<?xml version="1.0" encoding="UTF-8"?>
          |<system>
          |  <!-- Content in between markers will be preserved if codegen is rerun -->
          |
          |  $stSchedulingDomain
          |
          |  ${(stProtectionDomains, "\n\n")}
          |
          |  ${(stMemoryRegions, "\n\n")}
          |
          |  ${(stChannels, "\n\n")}
          |
          |  ${contentMarker.beginMarker}
          |  ${contentMarker.endMarker}
          |</system>"""
    return ret
  }

  @pure def toDot: ST = {

    var memConnections: ISZ[ST] = ISZ()
    for(p <- protectionDomains) {
      memConnections = memConnections ++ p.getDotMemConnections()
    }

    val ret =
      st"""digraph microkit {
          |  compound=true;
          |
          |  // protection domains
          |  ${(for(p <- protectionDomains) yield p.toDot, "\n\n")}
          |
          |  // memory regions
          |  ${(for(m <- memoryRegions) yield m.toDot, ";\n\n")};
          |
          |  // channels
          |  ${(for (c <- channels) yield c.toDot, ";\n")};
          |
          |  // shared memory mappings
          |  ${(memConnections, ";\n")};
          |}"""
    return ret
  }
}

@datatype class SchedulingDomain (val id: Z,
                                  val length: Z) {
  @strictpure def prettyST: ST = st"""<domain name="domain_$id" length="$length" />"""
}

@sig trait MicrokitDomain {
  def name: String
  def schedulingDomain: Option[Z]

  def memMaps: ISZ[MemoryMap]

  def prettyST: ST

  def toDot: ST

  def getDotMemConnections: ISZ[ST]

  def getMarkers: ISZ[Marker]
}

@datatype class VirtualMachine(val name: String,
                               val vcpuId: String,
                               val schedulingDomain: Option[Z],
                               val memMaps: ISZ[MemoryMap]) extends MicrokitDomain {

  @pure override def prettyST: ST = {
    val stMaps: Option[ST] =
      if (memMaps.nonEmpty) Some(st"${(for (m <- memMaps) yield m.prettyST, "\n")}")
      else None()
    return (st"""<virtual_machine name="$name">
                |  <vcpu id="$vcpuId" />
                |  $stMaps
                |</virtual_machine>""")
  }

  @pure override def toDot: ST = {
    return st"toDot: TODO"
  }

  @pure override def getDotMemConnections: ISZ[ST] = {
    return ISZ(st"getDotMemConnections: TODO")
  }

  @pure override def getMarkers: ISZ[Marker] = {
    return ISZ()
  }
}

@datatype class ProtectionDomain (val name: String,
                                  val schedulingDomain: Option[Z],
                                  val id: Option[String],
                                  val stackSizeInKiBytes: Option[Z],
                                  val smc: Option[B],
                                  val passive: Option[B],

                                  val memMaps: ISZ[MemoryMap],
                                  val irqs: ISZ[IRQ],
                                  val programImage: String,

                                  val children: ISZ[MicrokitDomain]) extends MicrokitDomain {
  val contentMarker: Marker = Marker(
    beginMarker = s"<!-- BEGIN CONTENT MARKER $name -->",
    endMarker = s"<!-- END CONTENT MARKER $name -->")

  @pure override def getMarkers: ISZ[Marker] = {
    return ISZ(contentMarker) ++ ((for (c <- children) yield c.getMarkers).flatMap((s: ISZ[Marker]) => s))
  }

  @pure def prettyST: ST = {
    val domain: Option[ST] =
      if (schedulingDomain.nonEmpty) Some(st""" domain="domain_${schedulingDomain.get}"""")
      else None()
    val stId: Option[ST] =
      if (id.nonEmpty) Some(st""" id="${id.get}"""")
      else None()
    val stackSizeOpt: Option[ST] =
      stackSizeInKiBytes match {
        case Some(k) => Some(st""" stack_size="${KiBytesToHex(k)}"""")
        case _ => None()
      }
    val smcOpt: Option[ST] = {
      smc match {
        case Some(v) => Some(st""" smc="${if(v) "true" else "false"}"""")
        case _ => None()
      }
    }
    val passiveOpt: Option[ST] =
      passive match {
        case Some(v) => Some(st""" passive="${if(v) "true" else "false"}"""")
        case _ => None()
      }

    val stChildren: Option[ST] =
      if (children.nonEmpty) Some(st"${(for (c <- children) yield c.prettyST, "\n")}")
      else None()
    val stMaps: Option[ST] =
      if (memMaps.nonEmpty) Some(st"${(for (m <- memMaps) yield m.prettyST, "\n")}")
      else None()
    val irqsOpt: Option[ST] =
      if (irqs.nonEmpty) Some(st"${(for (i <- irqs) yield i.prettyST, "\n")}")
      else None()

    val ret =
      st"""<protection_domain name="$name"$domain$stId$stackSizeOpt$smcOpt$passiveOpt>
          |  <program_image path="$programImage" />
          |  $stMaps
          |  $irqsOpt
          |  $stChildren
          |
          |  ${contentMarker.beginMarker}
          |  ${contentMarker.endMarker}
          |</protection_domain>"""
    return ret
  }

  @pure def toDot: ST = {
    val dotChildren: Option[ST] =
      if (children.nonEmpty) Some(st"${(for (c <- children) yield  c.toDot)}")
      else None()
    val dotMemMaps: Option[ST] =
      if (memMaps.nonEmpty) Some(st"${(for (m <- memMaps) yield st"pd_${name}_${m.toDotVaddr}", ";\n" )};")
      else None()
    val ret =
      st"""graph [style=rounded]
          |subgraph cluster_$name {
          |  label = "$name";
          |  ${name}_INVIS [label="", style=invis, width=.5, height=.5, fixedsize=true]
          |
          |  $dotChildren
          |  $dotMemMaps
          |}"""
    return ret
  }

  @pure def getDotMemConnections: ISZ[ST] = {
    var ret: ISZ[ST] = for(m <- memMaps) yield st"pd_${name}_${m.toDotConnection}"
    for (c <- children) {
      ret = ret ++ c.getDotMemConnections
    }
    return ret
  }
}

@sig trait MemoryRegion {
  def name: String
  def sizeInKiBytes: Z
  def physicalAddressInKiBytes: Option[Z]

  @pure def prettyST: ST = {
    val physAddr: Option[ST] =
      if (physicalAddressInKiBytes.nonEmpty) Some(
        st"""
            |               phys_addr="${KiBytesToHex(physicalAddressInKiBytes.get)}"""")
      else None()
    return (
      st"""<memory_region name="$name"
          |               size="${KiBytesToHex(sizeInKiBytes)}"$physAddr />""")
  }

  @pure def toDot: ST = {
    return st"$name"
  }
}

@datatype class PortSharedMemoryRegion(val outgoingPortPath: ISZ[String],
                                       val queueSize: Z,
                                       val varAddr: String,
                                       val perms: ISZ[Perm.Type],
                                       val sizeInKiBytes: Z,
                                       val physicalAddressInKiBytes: Option[Z]) extends MemoryRegion {

  def name: String = {
    return st"${(outgoingPortPath, "_")}_${queueSize}_Memory_Region".render
  }
}

@enum object VirtualMemoryRegionType {
  "GIC"
  "RAM"
  "SERIAL"
}

@datatype class VirtualMachineMemoryRegion(val typ: VirtualMemoryRegionType.Type,
                                            val threadPath: ISZ[String],
                                           val sizeInKiBytes: Z,
                                           val physicalAddressInKiBytes: Option[Z]) extends MemoryRegion {
  def name: String = {
    val suffix: String = typ match {
      case VirtualMemoryRegionType.GIC => "GIC"
      case VirtualMemoryRegionType.RAM => "Guest_RAM"
      case VirtualMemoryRegionType.SERIAL => "Serial"
    }
    return st"${(threadPath, "_")}_VM_${suffix}".render
  }

  def vmmVaddrName: String = {
    return s"${name}_vaddr"
  }
}

@datatype class Channel (val firstPD: String,
                         val firstId: Z,
                         val secondPD: String,
                         val secondId: Z) {
  @strictpure def prettyST: ST =
    st"""<channel>
        |  <end pd="$firstPD" id="$firstId" />
        |  <end pd="$secondPD" id="$secondId" />
        |</channel>"""

  @strictpure def toDot: ST = {
    st"${firstPD}_INVIS -> ${secondPD}_INVIS [lhead=cluster_${secondPD}, minlen=2, dir=both]"
  }
}

@datatype class IRQ(val id: Z,
                    val irq: Z) {
  @pure def prettyST: ST = {
    return st"""<irq irq="$irq" id="$id" />"""
  }
}

@datatype class MemoryMap (val memoryRegion: String,
                           val vaddrInKiBytes: Z,
                           val perms: ISZ[Perm.Type],
                           val varAddr: Option[String],
                           val cached: Option[B]) {
  @pure def prettyST: ST = {
    val stPerms = st"""${(for (p <- perms) yield (
      if (p == Perm.READ) "r"
      else if (p == Perm.WRITE) "w"
      else "x"), "")}"""
    val setVarAddr: Option[String] =
      if (varAddr.nonEmpty) Some(s"setvar_vaddr=\"${varAddr.get}\"")
      else None()
    val cachedOpt: Option[ST] =
      if (cached.nonEmpty) Some(st"""cached="${if(cached.get) "true" else "false"}"""")
      else None()
    return (
    st"""<map mr="$memoryRegion"
        |     vaddr="${KiBytesToHex(vaddrInKiBytes)}"
        |     perms="$stPerms"
        |     $setVarAddr
        |     $cachedOpt
        |/>""")
  }

  @pure def toDotConnection: ST = {
    val style: Option[ST] = {
      if (perms.size == 3){
        assert (ops.ISZOps(perms).contains(Perm.EXECUTE) &&
          ops.ISZOps(perms).contains(Perm.READ) && ops.ISZOps(perms).contains(Perm.WRITE))
      }
      if (perms.size == 2) {
        assert (ops.ISZOps(perms).contains(Perm.READ) && ops.ISZOps(perms).contains(Perm.WRITE))
        Some(st"dir=both,")
      } else if (perms.size == 1) {
        assert (perms(0) == Perm.READ)
        Some(st"dir=back,")
      }
      else {
        halt("Infeasible")
      }
    }
    return st"$varAddr -> $memoryRegion [$style style=dashed]"
  }

  @pure def toDotVaddr: ST = {
    return st"$varAddr [label=$varAddr]"
  }
}

@enum object Perm {
  "EXECUTE"
  "READ"
  "WRITE"
}