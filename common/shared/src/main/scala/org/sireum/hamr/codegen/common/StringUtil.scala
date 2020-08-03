// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._

object StringUtil {

  def replaceAll(s: String, from: String, to: String): String = {
    return ops.StringOps(s).replaceAllLiterally(from, to)
  }

  def toLowerCase(s: String): String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => ops.COps(c).toLower))
  }

  def toUpperCase(s: String): String = {
    val cms = conversions.String.toCms(s)
    return conversions.String.fromCms(cms.map((c: C) => ops.COps(c).toUpper))
  }

  def sanitizeName(s: String): String = {
    return replaceAll(replaceAll(s, "-", "_"), ".", "_")
  }

  def macroize(str: String): String = {
    return toUpperCase(sanitizeName(str))
  }
}
