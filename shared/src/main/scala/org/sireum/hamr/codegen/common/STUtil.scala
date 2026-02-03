// #Sireum
package org.sireum.hamr.codegen.common

import org.sireum._

object STUtil {

  @pure def splitST(s: ST) : ISZ[ST] = {
    val sep: String = {
      if(ops.StringOps(st"""a
                           |b""".render).contains("\r\n")) "\r\n"
      else "\n"
    }
    val hamrIcon: C = '⛏'
    val o = ops.StringOps(s.render).replaceAllLiterally(sep, s"$hamrIcon")
    val oo = StringUtil.split_PreserveEmptySegments(o, c => c == hamrIcon)
    return (for (s <- oo) yield st"$s")
  }
}
