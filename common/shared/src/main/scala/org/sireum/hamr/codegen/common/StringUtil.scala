// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.message.Reporter
import org.sireum.ops.StringOps

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

  def replaceSections(s: String,
                      replacements: ISZ[(Z, Z, String)], reporter: Reporter): String = {

    val lines = split_PreserveEmptyLines(s, c => c == '\n')

    // sanity checks
    val checkIndex = 1
    if(replacements(0)._1 >= replacements(0)._2) {
      reporter.error(None(), "Error", s"The following replacement is invalid, please report: ${replacements(0)}")
      return s
    }
    while(checkIndex < replacements.size) {
      if(replacements(checkIndex - 1)._2 > replacements(checkIndex)._1) {
        reporter.error(None(), "Error", s"Replacements must be sequential, please report: ${replacements(checkIndex - 1)} and ${replacements(checkIndex)}")
        return s
      }
    }
    if(replacements(checkIndex - 1)._2 >= lines.size) {
      reporter.error(None(), "Error", s"Replacement offset is more than the content size, please report: size=${lines.size}, replacement=${replacements(checkIndex - 1)}")
      return s
    }

    var r: ISZ[String] = ISZ()

    var i: Z = 0
    var ri: Z = 0
    while(i < lines.size) {
      if(ri < replacements.size &&
         i == replacements(ri)._1) {
        val rtext = split_PreserveEmptyLines(replacements(ri)._3, c => c == '\n')
        r = r :+ lines(i) // add start marker
        r = r ++ rtext // substitute new section content
        i = replacements(ri)._2 // set index to location of end marker

        ri = ri + 1
      } else {
        r = r :+ lines(i)
        i = i + 1
      }
    }
    return st"${(r, "\n")}".render
  }

  def collectSections(s: String,
                      errorKind: String,
                      markers: ISZ[Marker],
                      reporter: Reporter): HashSMap[Marker, (Z, Z, String)] = {
    def fetchMarker(so: ops.StringOps, searchBeginMarkers: B): Option[Marker] = {
      for(marker <- markers) {
        val m: String = if(searchBeginMarkers) marker.beginMarker else marker.endMarker
        if(so.startsWith(m)) {
          return Some(marker)
        }
      }
      return None()
    }

    var r = HashSMap.empty[Marker, (Z, Z, String)]
    val lines = split_PreserveEmptyLines(s, c => c == '\n')
    val size = lines.size
    var i = 0

    while (i < size) {
      val line = lines(i)
      val lOps = ops.StringOps(ops.StringOps(line).trim)
      val beginMarker = fetchMarker(lOps, T)
      if (beginMarker.nonEmpty) {
        val beginLine = i
        i = i + 1
        var found = F
        var code = ISZ[String]()
        while (i < size && !found) {
          val line2 = lines(i)
          val lOps2 = ops.StringOps(ops.StringOps(line2).trim)
          val endMarker = fetchMarker(lOps2, F)
          if (endMarker.nonEmpty) {
            found = T
            if(beginMarker != endMarker) {
              reporter.error(None(), errorKind,
                s"Mismatch marker at lines ${beginLine + 1} and ${i + 1} (${beginMarker.get} != ${endMarker.get})")

              return r
            }
            r = r + (beginMarker.get ~> ((beginLine, i, st"${(code, "\n")}".render)))
          } else {
            code = code :+ ops.StringOps(line2).trimTrailing
          }
          i = i + 1
        }
        if (!found) {
          reporter.error(None(), errorKind, s"Unclosed marker at line ${beginLine + 1} for ${beginMarker.get}")
        }
      }
      i = i + 1
    }
    return r
  }

  // version of split that preserves empty lines
  @pure def split_PreserveEmptyLines(s: String, isSep: C => B @pure): ISZ[String] = {
    var r = ISZ[String]()
    val cis = conversions.String.toCis(s)
    var last = 0
    val size = s.size
    while (last < size && isSep(cis(last))) {
      r = r :+ "" // preserve empty lines at start of s
      last = last + 1
    }
    var i = last
    while (i < size) {
      if (isSep(cis(i)) && last != i) {
        r = r :+ StringOps.substring(cis, last, i)
        i = i + 1
        while (i < size && isSep(cis(i))) {
          r = r :+ "" // preserve empty lines
          i = i + 1
        }
        last = i
      }
      i = i + 1
    }
    if (last < size) {
      r = r :+ StringOps.substring(cis, last, i)
    }
    return r
  }
}

