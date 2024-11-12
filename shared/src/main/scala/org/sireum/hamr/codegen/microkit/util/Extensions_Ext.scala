package org.sireum.hamr.codegen.microkit.util

import org.sireum._

object Extensions_Ext {

  def bytesToKiBytes(bytes: Z): Z= {
    val calc = (bytes.toInt * 1.0) / 1024.0
    return if (calc < 1.0) 1 else Z(Math.round(calc))
  }
}
