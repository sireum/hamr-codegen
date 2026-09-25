// #Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.types._

// The memory layout of the types HAMR generates for Microkit, computed by HAMR itself
// (SharedMemorySafety-design.md, D1).  Nothing here is read from the model: sizes used to
// come from Memory_Properties::Data_Size, a number the developer could get wrong and the
// generated C then trusted.
//
// The rules are those of the AArch64 C ABI (AAPCS64): base types have their natural size
// and alignment, records lay their fields out in declaration order with padding, enums are
// 32-bit integers, and arrays are dimension * element size.  HAMR's Microkit types use only
// fixed-width integers, bool, floats, char and 32-bit enums, so the same layout holds on
// every 64-bit target -- the AArch64 seL4 image and the host builds of `make test`.
//
// The generated C and Rust assert this layout at build time (D5), so a disagreement between
// HAMR and a compiler fails the build instead of producing two views of shared memory.

@datatype class Layout(val size: Z,
                       val align: Z,
                       // record fields in declaration order, with their byte offsets
                       val fieldOffsets: ISZ[(String, Z)])

object MicrokitLayout {

  // Microkit maps memory regions in whole pages
  val pageSizeInBytes: Z = 4096

  // sb_event_counter_t (_Atomic uintmax_t), the numSent field that heads every queue
  val eventCounterSizeInBytes: Z = 8

  // On Microkit a string is a fixed Character array that must hold its terminating NUL
  // (MicrokitTypeUtil.getAllTouchedTypes substitutes it for Base_Types::String)
  val stringTypeName: String = "Base_Types::String"

  @strictpure def alignUp(n: Z, align: Z): Z = ((n + align - 1) / align) * align

  @pure def baseTypeLayout(name: String): Layout = {
    val size: Z = name match {
      case "Base_Types::Boolean" => 1
      case "Base_Types::Character" => 1
      case "Base_Types::Integer_8" => 1
      case "Base_Types::Unsigned_8" => 1
      case "Base_Types::Integer_16" => 2
      case "Base_Types::Unsigned_16" => 2
      case "Base_Types::Integer_32" => 4
      case "Base_Types::Unsigned_32" => 4
      case "Base_Types::Float_32" => 4
      case "Base_Types::Integer_64" => 8
      case "Base_Types::Unsigned_64" => 8
      case "Base_Types::Float_64" => 8
      case x => halt(s"No Microkit layout for base type $x")
    }
    return Layout(size = size, align = size, fieldOffsets = ISZ())
  }

  // `subs` is the Microkit type substitution map (Base_Types::String -> the HAMR-sized
  // Character array); see MicrokitTypeUtil.getAllTouchedTypes.
  @pure def layoutOf(t: AadlType, subs: Map[String, AadlType]): Layout = {
    subs.getOrElse(t.name, t) match {
      case b: BaseType => return baseTypeLayout(b.name)
      case _: EnumType => return Layout(size = 4, align = 4, fieldOffsets = ISZ())
      case a: ArrayType =>
        val elem = layoutOf(a.baseType, subs)
        var n: Z = 1
        for (d <- a.dimensions) {
          n = n * d
        }
        return Layout(size = n * elem.size, align = elem.align, fieldOffsets = ISZ())
      case r: RecordType =>
        var offset: Z = 0
        var align: Z = 1
        var offsets: ISZ[(String, Z)] = ISZ()
        for (f <- r.fields.entries) {
          val fl = layoutOf(f._2, subs)
          offset = alignUp(offset, fl.align)
          offsets = offsets :+ ((f._1, offset))
          offset = offset + fl.size
          if (fl.align > align) {
            align = fl.align
          }
        }
        return Layout(size = alignUp(offset, align), align = align, fieldOffsets = offsets)
      case x => halt(s"No Microkit layout for $x")
    }
  }

  // Does some bit pattern of t's bytes fail to be a value of t?  A bool (0 or 1), an enum
  // (0 .. n-1) or a string (a NUL within its bounds) somewhere inside t makes one: every bit
  // pattern of the integer, float and character types is a value.  A queue checks the
  // elements it receives only when this holds (SharedMemorySafety-design.md, D6), so a queue
  // of, say, a byte array costs no more than it did before the check.
  @pure def hasInvalidBitPatterns(t: AadlType, subs: Map[String, AadlType]): B = {
    if (t.name == stringTypeName) {
      return T
    }
    subs.getOrElse(t.name, t) match {
      case b: BaseType => return b.name == "Base_Types::Boolean"
      case _: EnumType => return T
      case a: ArrayType => return hasInvalidBitPatterns(a.baseType, subs)
      case r: RecordType =>
        for (f <- r.fields.values) {
          if (hasInvalidBitPatterns(f, subs)) {
            return T
          }
        }
        return F
      case x => halt(s"No Microkit layout for $x")
    }
  }

  // Bytes of sb_queue_<T>_<q>_t: the numSent counter, then q + 1 elements (one slot is
  // always dirty).  The struct is aligned to the stricter of the two.
  @pure def queueStructBytes(elem: Layout, queueSize: Z): Z = {
    val align: Z = if (elem.align > eventCounterSizeInBytes) elem.align else eventCounterSizeInBytes
    val eltOffset = alignUp(eventCounterSizeInBytes, elem.align)
    return alignUp(eltOffset + (queueSize + 1) * elem.size, align)
  }

  // The shared memory region a queue of `t` needs, in whole pages (D4).  It used to be sized
  // from one element's declared size, which a large element or any undeclared record
  // outgrew.
  @pure def queueRegionBytes(t: AadlType, subs: Map[String, AadlType], queueSize: Z): Z = {
    return alignUp(queueStructBytes(layoutOf(t, subs), queueSize), pageSizeInBytes)
  }

  @pure def queueRegionKiBytes(t: AadlType, subs: Map[String, AadlType], queueSize: Z): Z = {
    return queueRegionBytes(t, subs, queueSize) / 1024
  }
}
