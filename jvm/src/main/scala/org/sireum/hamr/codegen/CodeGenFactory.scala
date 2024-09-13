package org.sireum.hamr.codegen

import org.sireum.{ISZ, ST, halt}

object CodeGenFactory {

  def stringToST(s: String): ST = {
    if (s == null) {
      halt("Argument cannot be null")
    }
    import org.sireum._
    return st"$s"
  }

  def iszToST[T](elements: ISZ[T], sep: String): ST = {
    import org.sireum._
    return st"${(elements, sep)}"
  }
}
