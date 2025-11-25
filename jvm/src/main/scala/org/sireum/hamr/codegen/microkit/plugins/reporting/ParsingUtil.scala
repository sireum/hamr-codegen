// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil

object ParsingUtil {

  @pure def getName(prefix: String, s: ops.StringOps): Option[String] = {
    return getNameH(prefix, s, F, F)
  }

  @pure def getNameH(prefix: String, s: ops.StringOps, scanTillEnd: B, includePeriod: B): Option[String] = {
    val cis = conversions.String.toCis(s.s)
    var start: Z = s.stringIndexOf(prefix) + prefix.size + 1

    while (cis(start).isWhitespace) {
      start = start + 1
    }

    if (scanTillEnd) {
      return Some(conversions.String.fromCis(ops.ISZOps(cis).slice(start, s.s.size)))
    }

    var end: Z = -1
    var ii: Z = start

    while (end == -1 && ii < cis.size) {
      if (StringUtil.isLetter(cis(ii)) || StringUtil.isNumber(cis(ii)) || cis(ii) == '_' ||
        (includePeriod && cis(ii) == '.')) {
        ii = ii + 1
      } else {
        end = ii
      }
    }
    if (end == -1 && ii == cis.size) {
      end = cis.size
    }

    if (end != -1) {
      val s2 = conversions.String.fromCis(ops.ISZOps(cis).slice(start, end))
      return Some(s2)
    } else {
      return None()
    }
  }
}
