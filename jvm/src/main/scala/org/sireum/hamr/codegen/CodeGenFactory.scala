package org.sireum.hamr.codegen

import org.sireum.{ST, halt}

object CodeGenFactory {

  def stringToST(s: String): ST = {
    if (s == null) {
      halt("Argument cannot be null")
    }
    import org.sireum._
    return st"$s"
  }
}
