// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._
import org.sireum.ops.{COps, StringOps}

object StringUtil {

  def replaceAll(s: String, from: String, to: String): String = {
    return StringOps(s).replaceAllLiterally(from, to)
  }

  def toLowerCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => COps(c).toLower))
  }

  def toUpperCase(s: String):String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => COps(c).toUpper))
  }

  def sanitizeName(s: String): String = {
    return replaceAll(replaceAll(s, "-", "_"), ".", "_")
  }
}
