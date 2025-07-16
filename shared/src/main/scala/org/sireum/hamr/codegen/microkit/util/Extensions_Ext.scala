package org.sireum.hamr.codegen.microkit.util

import org.sireum._

object Extensions_Ext {

  def bitsToBytes(bits: Z): Z = {
    val b = Math.ceil((bits.toLong * 1.0) / 8.0)
    return Z(b.toLong)
  }

  def bytesToKiBytes(bytes: Z): Z= {
    val calc = Math.ceil((bytes.toLong * 1.0) / 1024.0)
    return Z(calc.toLong)
  }
}
