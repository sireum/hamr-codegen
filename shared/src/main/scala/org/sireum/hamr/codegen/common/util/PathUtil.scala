// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._

object PathUtil {

  def convertWinPathSepToNix(s: String): String = {
    return ops.StringOps(s).replaceAllChars('\\', '/')
  }

  def value(value: String): String = {
    return convertWinPathSepToNix(value)
  }
}
