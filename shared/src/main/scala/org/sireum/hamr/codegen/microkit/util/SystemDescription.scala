// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util.{KiBytesToHex}

@datatype class SystemDescription (val schedulingDomains: ISZ[SchedulingDomain],
                                   val protectionDomains: ISZ[ProtectionDomain],
                                   val memoryRegions: ISZ[MemoryRegion],
                                   val channels: ISZ[Channel]) {

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
          |  $stSchedulingDomain
          |
          |  ${(stProtectionDomains, "\n\n")}
          |
          |  ${(stMemoryRegions, "\n\n")}
          |
          |  ${(stChannels, "\n\n")}
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

@datatype class SchedulingDomain (val name: String,
                                  val length: Z) {
  @strictpure def prettyST: ST = st"""<domain name="$name" length="$length" />"""
}

@datatype class ProtectionDomain (val name: String,
                                  val schedulingDomain: Option[String],
                                  val id: Option[String],
                                  val stackSizeInKiBytes: Option[Z],

                                  val memMaps: ISZ[MemoryMap],
                                  val programImage: String,

                                  val children: ISZ[ProtectionDomain]) {
  @pure def prettyST: ST = {
    val domain: Option[ST] =
      if (schedulingDomain.nonEmpty) Some(st""" domain="${schedulingDomain.get}"""")
      else None()
    val stId: Option[ST] =
      if (id.nonEmpty) Some(st""" id="${id.get}"""")
      else None()

    val stChildren: Option[ST] =
      if (children.nonEmpty) Some(st"${(for (c <- children) yield c.prettyST, "\n")}")
      else None()
    val stMaps: Option[ST] =
      if (memMaps.nonEmpty) Some(st"${(for (m <- memMaps) yield m.prettyST, "\n")}")
      else None()
    val stackSizeOpt: Option[ST] =
      stackSizeInKiBytes match {
        case Some(k) => Some(st""" stack_size="${KiBytesToHex(k)}"""")
        case _ => None()
      }
    val ret =
      st"""<protection_domain name="$name"$domain$stId$stackSizeOpt>
          |  $stChildren
          |  $stMaps
          |  <program_image path="$programImage" />
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

@datatype class MemoryRegion (name: String,
                              sizeInKiBytes: Z) {
  @strictpure def prettyST: ST = st"""<memory_region name="$name" size="${KiBytesToHex(sizeInKiBytes)}" />"""

  @strictpure def toDot: ST = st"$name"
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

@datatype class MemoryMap (val memoryRegion: String,
                           val vaddrInKiBytes: Z,
                           val perms: ISZ[Perm.Type],
                           val varAddr: String) {
  @pure def prettyST: ST = {
    val stPerms = st"""${(for (p <- perms) yield (if (p == Perm.READ) "r" else "w"), "")}"""
    return (
    st"""<map mr="$memoryRegion"
        |     vaddr="${KiBytesToHex(vaddrInKiBytes)}"
        |     perms="$stPerms"
        |     setvar_vaddr="$varAddr" />""")
  }

  @pure def toDotConnection: ST = {
    val style: Option[ST] =
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
    return st"$varAddr -> $memoryRegion [$style style=dashed]"
  }

  @pure def toDotVaddr: ST = {
    return st"$varAddr [label=$varAddr]"
  }
}

@enum object Perm {
  "READ"
  "WRITE"
}