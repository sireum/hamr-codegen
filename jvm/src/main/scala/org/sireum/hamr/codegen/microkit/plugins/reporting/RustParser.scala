// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil

object RustParser {

  @pure def findLineNumber(str: String, start: Z, content: ISZ[String]): Z = {
    assert(start < content.size)

    for (i <- start until content.size) {
      if (ops.StringOps(content(i)).contains(str)) {
        return i
      }
    }
    return -1
  }


  @pure def scanForClosingBrace(beginLine: Z, content: ISZ[String]): (Z, HashSMap[String, (Z, Z)]) = {
    assert(beginLine >= 0)
    assert(beginLine < content.size)

    var markerLocs: HashSMap[String, (Z, Z)] = HashSMap.empty

    var stack: Stack[String] = Stack.empty
    var lineIndex = beginLine
    while (lineIndex < content.size) {
      val line = conversions.String.toCis(content(lineIndex))

      var inWhitespacePrefix: B = T

      var charIndex: Z = 0
      while (charIndex < line.size) {
        val c = line(charIndex)

        if (c == '{') {
          if (stack.isEmpty || stack.peek.get != "/*") {
            stack = stack.push("{")
          }
        }
        else if (c == '}') {
          if (stack.peek.nonEmpty && stack.peek.get != "/*") {
            // not in a comment block
            if (stack.peek.nonEmpty && stack.peek.get == "{") {
              // found closing curly brace
              stack = stack.pop.get._2
              if (stack.isEmpty) {
                return (lineIndex, markerLocs)
              }
            } else {
              halt("Dangling '}'")
            }
          }
        }
        else if (c == '/' && line(charIndex + 1) == '*') {
          stack = stack.push("/*")
          charIndex = charIndex + 1
        }
        else if (c == '*' && line(charIndex + 1) == '/') {
          assert(stack.nonEmpty && stack.peek.get == "/*")
          stack = stack.pop.get._2
          charIndex = charIndex + 1
        }
        else if (inWhitespacePrefix && c == '/' && line(charIndex + 1) == '/') {
          // BEGIN MARKER ...
          if ((charIndex + 15 < line.size) &&
            line(charIndex + 3) == 'B' && line(charIndex + 4) == 'E' && line(charIndex + 5) == 'G' && line(charIndex + 6) == 'I' && line(charIndex + 7) == 'N' && line(charIndex + 8) == ' ' && line(charIndex + 9) == 'M' && line(charIndex + 10) == 'A' && line(charIndex + 11) == 'R' && line(charIndex + 12) == 'K' && line(charIndex + 13) == 'E' && line(charIndex + 14) == 'R' && line(charIndex + 15) == ' ') {
            val name = ops.StringOps(conversions.String.fromCis(ops.ISZOps(line).slice(charIndex + 16, line.size))).trim
            assert(!markerLocs.contains(name), s"'$name'")
            markerLocs = markerLocs + name ~> (lineIndex, -1)
          }
          // END MARKER ...
          if ((charIndex + 13 < line.size) &&
            line(charIndex + 3) == 'E' && line(charIndex + 4) == 'N' && line(charIndex + 5) == 'D' && line(charIndex + 6) == ' ' && line(charIndex + 7) == 'M' && line(charIndex + 8) == 'A' && line(charIndex + 9) == 'R' && line(charIndex + 10) == 'K' && line(charIndex + 11) == 'E' && line(charIndex + 12) == 'R') {
            val name = ops.StringOps(conversions.String.fromCis(ops.ISZOps(line).slice(charIndex + 14, line.size))).trim
            val marker = markerLocs.get(name).get
            markerLocs = markerLocs + name ~> (marker._1, lineIndex)
          }
          // ignore rest of characters
          charIndex = line.size
        }

        inWhitespacePrefix = inWhitespacePrefix && c.isWhitespace

        charIndex = charIndex + 1
      }
      lineIndex = lineIndex + 1
    }
    halt("Infeasible if program is well formed")
  }

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
