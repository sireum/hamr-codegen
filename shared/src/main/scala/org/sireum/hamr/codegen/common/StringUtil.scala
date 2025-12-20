// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{BlockMarker, Marker, PlaceholderMarker}
import org.sireum.message.{FlatPos, Position, Reporter}
import org.sireum.ops.StringOps
import org.sireum.U32._

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

  @enum object MarkerType {
    "begin"
    "end"
    "placeholder"
  }
  def replaceSectionsR(content: ISZ[C],
                       replacements: ISZ[(Z, Z, String)],
                       toolName: String,
                       reporter: Reporter): String = {
    var i = 0
    while (i < replacements.size) {
      if (replacements(i)._1 > replacements(i)._2) {
        reporter.error(None(), toolName, s"Replacement's begin offset must be greater than end offset: ${replacements(0)}")
        return conversions.String.fromCis(content)
      }
      if (0 > replacements(i)._1 || replacements(i)._2 > content.size) {
        reporter.error(None(), toolName, s"Replacement offsets outside of content offsets: ${replacements(0)} vs (0, ${content.size})")
        return conversions.String.fromCis(content)
      }
      if (i > 0 && replacements(i - 1)._2 > replacements(i)._2) {
        reporter.error(None(), toolName, s"Replacements must be sequential, please report: ${replacements(i - 1)} and ${replacements(i)}")
        return conversions.String.fromCis(content)
      }
      i = i + 1
    }
    val o = ops.ISZOps(content)

    var newContent: ISZ[C] = ISZ()
    var offset: Z = 0

    if (replacements.nonEmpty) {
      for (r <- replacements) {
        newContent = newContent ++ o.slice(offset, r._1 - 1)
        newContent = newContent ++ conversions.String.toCis(r._3)
        offset = r._2
      }
      newContent = newContent ++ o.slice(offset, content.size)
      return conversions.String.fromCis(newContent)
    } else {
      return conversions.String.fromCis(content)
    }
  }

  /*
  // @return returned string wil contain LF style line endings
  def replaceSections(s: String,
                      replacements: ISZ[(Z, Z, String)],
                      errorKind: String,
                      reporter: Reporter): String = {

    val lines = split_PreserveEmptySegments(s, c => c == '\n')

    // sanity checks
    var checkIndex = 1
    if (replacements(0)._1 >= replacements(0)._2) {
      reporter.error(None(), errorKind, s"The following replacement is invalid, please report: ${replacements(0)}")
      return s
    }
    while (checkIndex < replacements.size) {
      if (replacements(checkIndex - 1)._2 > replacements(checkIndex)._1) {
        reporter.error(None(), errorKind, s"Replacements must be sequential, please report: ${replacements(checkIndex - 1)} and ${replacements(checkIndex)}")
        return s
      }
      checkIndex = checkIndex + 1
    }
    if (replacements(checkIndex - 1)._2 >= lines.size) {
      reporter.error(None(), errorKind, s"Replacement offset is more than the content size, please report: size=${lines.size}, replacement=${replacements(checkIndex - 1)}")
      return s
    }

    var r: ISZ[String] = ISZ()

    var i: Z = 0
    var ri: Z = 0
    while (i < lines.size) {
      if (ri < replacements.size &&
        i == replacements(ri)._1) {
        val rtext = split_PreserveEmptySegments(replacements(ri)._3, c => c == '\n')
        r = r :+ lines(i) // add start marker
        r = r ++ rtext // substitute new section content
        i = replacements(ri)._2 // set index to location of end marker

        ri = ri + 1
      } else {
        r = r :+ lines(i)
        i = i + 1
      }
    }
    // Note: ST uses CRLF on windows so always normalize to LF
    return ops.StringOps(st"${(r, "\n")}".render).replaceAllLiterally("\r\n", "\n")
  }
  */

  @datatype class PosMarker(val beginOffset: Z,
                            val endOffset: Z,
                            val content: String,
                            val marker: Marker)

  def collectSectionsR(uriOpt: Option[String],
                       content: ISZ[C],
                       errorKind: String,
                       markers: ISZ[Marker],
                       reporter: Reporter): HashSMap[String, PosMarker] = {

    val contentSize = content.size

    var r: HashSMap[String, PosMarker] = HashSMap.empty

    // begin building maps
    var placeholderMarkers = Map.empty[String, Marker]
    var beginMarkers = Map.empty[String, Marker]
    var endMarkers = Map.empty[String, Marker]
    var idMarkers = Map.empty[String, Marker]

    for(marker <- markers) {
      marker match {
        case p: PlaceholderMarker =>
          idMarkers = idMarkers + (p.id ~> p)

          placeholderMarkers = placeholderMarkers + (p.marker ~> p)

        case b: BlockMarker =>
          idMarkers = idMarkers + b.id ~> b

          beginMarkers = beginMarkers + b.beginMarker ~> b
          endMarkers = endMarkers + b.endMarker ~> b
      }
    }
    // end of building maps

    var offset:Z = 0
    var line: Z = 1
    var col: Z = 0

    @pure def buildPos(beginLine: Z, beginCol: Z, beginOffset: Z): Option[Position] = {
      return Some(FlatPos(
        uriOpt = uriOpt,
        beginLine32 = conversions.Z.toU32(beginLine),
        beginColumn32 = conversions.Z.toU32(beginCol),
        endLine32 = conversions.Z.toU32(line),
        endColumn32 = conversions.Z.toU32(col),
        offset32 = conversions.Z.toU32(beginOffset),
        length32 = conversions.Z.toU32(offset - beginOffset)))
    }

    @pure def isPlaceholderPrefix(): B = {
      var xoffset = offset
      if (xoffset + 3 < contentSize && content(xoffset) == '/' && content(xoffset + 1) == '/' && content(xoffset + 2) == ' ') {
        // '//' possible code placeholder
        xoffset = xoffset + 3
      } else if (xoffset + 2 < contentSize && content(xoffset) == '#' && content(xoffset + 1) == ' ') {
        // '#' possible makefile/python placeholder
        xoffset = xoffset + 2
      } else if (xoffset + 5 < contentSize && content(xoffset) == '<' && content(xoffset + 1) == '!' &&
        content(xoffset + 2) == '-' && content(xoffset + 3) == '-' && content(xoffset + 4) == ' ') {
        // '<!--' possible xml placeholder
        xoffset = xoffset + 5
      }
      if (xoffset != offset) {
        if (xoffset + 12 < contentSize &&
          content(xoffset + 0) == 'P' &&
          content(xoffset + 1) == 'L' &&
          content(xoffset + 2) == 'A' &&
          content(xoffset + 3) == 'C' &&
          content(xoffset + 4) == 'E' &&
          content(xoffset + 5) == 'H' &&
          content(xoffset + 6) == 'O' &&
          content(xoffset + 7) == 'L' &&
          content(xoffset + 8) == 'D' &&
          content(xoffset + 9) == 'E' &&
          content(xoffset + 10) == 'R' &&
          content(xoffset + 11) == ' ') {

          // consume the prefix
          offset = xoffset + 12

          return T
        }
      }
      return F
    }

    def consumeWhitespace(): Unit = {
      while(offset < contentSize && content(offset).isWhitespace) {
        if (content(offset) == '\n') {
          line = line + 1
          col = 0
        }
        offset = offset + 1
        col = col + 1
      }
    }

    def consumeTillNewLine(): ISZ[C] = {
      assert ((offset < contentSize) -->: !content(offset).isWhitespace, s"not expecting whitespace but found '${content(offset)}'")
      var ret = ISZ[C]()
      while(offset < contentSize && content(offset) != '\r' && content(offset) != '\n') {
        ret = ret :+ content(offset)
        offset = offset + 1
        col = col + 1
      }
      return ret
    }

    def matchMarker(str: String): Option[(Marker, MarkerType.Type)] = {
      endMarkers.get(str) match {
        case Some(m) => return Some((m, MarkerType.end))
        case _ =>
      }
      beginMarkers.get(str) match {
        case Some(m) => return Some((m, MarkerType.begin))
        case _ =>
      }
      placeholderMarkers.get(str) match {
        case Some(m) => return Some((m, MarkerType.placeholder))
        case _ =>
      }
      return None()
    }

    while (offset < contentSize) {

      consumeWhitespace()

      val beginPlaceholderMarkerBeginOffset = offset

      if (isPlaceholderPrefix()) {
        // content has a placeholder prefix

        // offset will be advanced past the placeholder prefix so now try matching by id
        val id: ISZ[C] = consumeTillNewLine()
        val idString: String = ops.StringOps(conversions.String.fromCis(id)).trim

        idMarkers.get(idString) match {
          case Some(m) =>
            // content contains a placeholder marker that matches an expected marker
            val markerContent = conversions.String.fromCis(
              ops.ISZOps(content).slice(beginPlaceholderMarkerBeginOffset, offset))

            r = r + m.id ~> PosMarker(
              beginOffset = beginPlaceholderMarkerBeginOffset + 1,
              endOffset = offset,
              content = markerContent,
              marker = m)
          case _ =>
          // the content has a placeholder prefix that doesn't appear to be
          // associated with an id created by codegen so ignore it
        }
      } else {
        // didn't find a placeholder marker in content so see if it matches
        // a beginMarker of a BlockMarker

        consumeWhitespace()

        val beginMarkerBeginOffset = offset

        var lineC: ISZ[C] = consumeTillNewLine()
        var lineString: String = ops.StringOps(conversions.String.fromCis(lineC)).trim

        beginMarkers.get(lineString) match {
          case Some(beginMarker: BlockMarker) =>
            // the content contained a block marker with the same id as found
            // in the expected begin markers

            var found = F
            while (offset < contentSize && !found) {

              consumeWhitespace()

              val beginOffset = offset
              val beginLine = line
              val beginCol = col

              lineC = consumeTillNewLine()
              lineString = ops.StringOps(conversions.String.fromCis(lineC)).trim

              matchMarker(lineString) match {
                case Some((b: BlockMarker, t)) =>
                  if (t == MarkerType.begin) {
                    reporter.error(buildPos(beginLine, beginCol, beginOffset), errorKind,
                      s"Encountered '${b.beginMarker}' while looking for the end marker '${beginMarker.endMarker}'")
                    return r
                  } else if (beginMarker.id != b.id) {
                    reporter.error(buildPos(beginLine, beginCol, beginOffset), errorKind,
                      s"Encountered '${b.endMarker}' while looking for the end marker of '${beginMarker.id}'")
                    return r
                  } else {
                    // found matching end marker
                    val markerContent = conversions.String.fromCis(
                      ops.ISZOps(content).slice(beginMarkerBeginOffset, offset))

                    r = r + (beginMarker.id ~> PosMarker(
                      beginOffset = beginMarkerBeginOffset + 1,
                      endOffset = offset,
                      content = markerContent,
                      marker = beginMarker))
                    found = T
                  }
                case Some((p: PlaceholderMarker, _)) =>
                  reporter.error(buildPos(beginLine, beginCol, beginOffset), errorKind,
                    s"Encountered '${p.string}' while looking for the end marker of '${beginMarker.id}'")
                  return r
                case _ =>
              }
            }
          case Some(p: PlaceholderMarker) =>
            halt("Infeasible") // if conditional makes this infeasible
          case _ =>
        }
      }
    }

    return r
  }

  /*
  def collectSections(s: String,
                      errorKind: String,
                      markers: ISZ[Marker],
                      reporter: Reporter): HashSMap[String, PosMarker] = {
    def fetchMarker(so: ops.StringOps, searchBeginMarkers: B): Option[Marker] = {
      for (m <- markers) {
        m match {
          case marker: BlockMarker =>
            val str: String = if (searchBeginMarkers) marker.beginMarker else marker.endMarker
            if (so.startsWith(str)) {
              return Some(marker)
            }
          case _ =>
        }
      }
      return None()
    }

    var r = HashSMap.empty[String, PosMarker]
    val lines = split_PreserveEmptySegments(s, c => c == '\n')
    val size = lines.size
    var i = 0

    while (i < size) {
      val line = lines(i)
      val lOps = ops.StringOps(ops.StringOps(line).trim)
      val beginMarker = fetchMarker(lOps, T)
      if (beginMarker.nonEmpty) {
        if (r.contains(beginMarker.get.id)) {
          reporter.error(None(), errorKind, s"Duplicate use of key detected: ${beginMarker.get}")
          return r
        }
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
            if (beginMarker != endMarker) {
              reporter.error(None(), errorKind,
                s"Mismatch marker at lines ${beginLine + 1} and ${i + 1} (${beginMarker.get} != ${endMarker.get})")
              return r
            }
            val str: String =
              if (code.nonEmpty && code(0) == "") {
                // ST hack.  ST's sequence expansion will drop entries until it finds one
                // that is not the empty string "".  To preserve these lines, replace the
                // first entry with a space if the first entry is the empty string
                val _code = " " +: ops.ISZOps(code).drop(1)
                st"${(_code, "\n")}".render
              } else {
                st"${(code, "\n")}".render
              }
            r = r + (beginMarker.get.id ~> (PosMarker(beginLine, i, str, beginMarker.get)))
          } else {
            i = i + 1
            //code = code :+ ops.StringOps(line2).trimTrailing
            code = code :+ line2
          }
        }
        if (!found) {
          reporter.error(None(), errorKind, s"Unclosed marker at line ${beginLine + 1} for ${beginMarker.get}")
        }
      }
      i = i + 1
    }
    return r
  }
  */

  @pure def isNumber(c: C): B = {
    val u = conversions.C.toU32(c)
    return u >= u32"48" && u <= u32"57"
  }

  @pure def isLetter(c: C): B = {
    val u = conversions.C.toU32(c)
    return (u >= u32"65" && u <= u32"90") || (u >= u32"97" && u <= u32"122")
  }

  // version of split that preserves empty segments
  // e.g. splitting "||x|" on '|' yields ["", "", "x", ""], whereas
  //      splitting "||x"  on '|' yields ["", "", "x"]
  @pure def split_PreserveEmptySegments(s: String, isSep: C => B@pure): ISZ[String] = {
    var r = ISZ[String]()
    val cis = conversions.String.toCis(s)
    var last = 0
    val size = s.size
    while (last < size && isSep(cis(last))) {
      r = r :+ "" // preserve empty segments at start of s
      last = last + 1
    }
    var i = last
    while (i < size) {
      if (isSep(cis(i)) && last != i) {
        r = r :+ StringOps.substring(cis, last, i)
        i = i + 1
        while (i < size && isSep(cis(i))) {
          r = r :+ "" // preserve empty segments
          i = i + 1
        }
        last = i
      }
      i = i + 1
    }
    if (last < size) {
      r = r :+ StringOps.substring(cis, last, size)
    }
    if (size > 0 && isSep(cis(size - 1))) {
      r = r :+ "" // preserve empty segment at the end of s
    }
    return r
  }
}

