// #Sireum

package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.microkit.plugins.reporting.CContainers._
import org.sireum.message.{FlatPos, Position}

object CParser {

  @sig trait Brace {
    @pure def startingOffset: Z
  }

  @enum object Phase {
    "ProcessingGlobals"
    "ProcessingMethods"
  }

  @pure def parse(f: Os.Path, rootDir: Os.Path): CFile = {

    val content = conversions.String.toCis(f.read)

    var braceStack: Stack[Brace] = Stack.empty[Brace]

    var cItems: Stack[CItem] = Stack.empty

    @pure def subString(start: Z, end: Z): String = {
      val slice = ops.ISZOps(content).slice(start, end)
      return conversions.String.fromCis(slice)
    }

    @pure def buildPosition(beginLine: Z, beginCol: Z, beginOffset: Z,
                            endLine: Z, endCol: Z, endOffset: Z): Position = {
      assert(beginLine >= 0)
      assert(beginCol >= 0)

      return FlatPos(
        uriOpt = Some(rootDir.relativize(f).value),
        beginLine32 = conversions.Z.toU32(beginLine),
        beginColumn32 = conversions.Z.toU32(beginCol),
        endLine32 = conversions.Z.toU32(endLine),
        endColumn32 = conversions.Z.toU32(endCol),
        offset32 = conversions.Z.toU32(beginOffset),
        length32 = conversions.Z.toU32(endOffset - beginOffset))
    }

    var line: Z = 1
    var col: Z = 1
    var offset: Z = 0


    @pure def consumeWhiteSpaceAndComments(): Unit = {
      while (offset < content.size) {
        if (content(offset).isWhitespace) {
          increment()
        }
        else if (offset + 1 < content.size && content(offset) == '/' && content(offset + 1) == '/') {
          col = col + 2
          offset = offset + 2

          // just consume till find \n or end of file
          while (offset < content.size && content(offset) != '\n') {
            increment()
          }
        }
        else if (offset + 1 < content.size && content(offset) == '/' && content(offset + 1) == '*') {
          col = col + 2
          offset = offset + 2

          // just consume until find */ or end of file
          var foundEnd = F
          while (offset < content.size && !foundEnd) {
            if (offset + 1 < content.size && content(offset) == '*' && content(offset + 1) == '/') {
              foundEnd = T
              col = col + 2
              offset = offset + 2
            } else {
              increment()
            }
          }
        }
        else {
          // something other than a whitespace or the start of a comment
          return
        }
      }
      return
    }

    @pure def scanToChar(chars: ISZ[C]): Unit = {
      while (offset < content.size) {
        if (ops.ISZOps(chars).contains(content(offset))) {
          return
        }

        increment()

        consumeWhiteSpaceAndComments()
      }
      return
    }

    @pure def increment(): Unit = {
      col = col + 1
      if (content(offset) == '\n') {
        line = line + 1
        col = 1
      }
      offset = offset + 1
    }

    @pure def balanceBrace(): Unit = {
      assert (content(offset) == '{')

      var s: Stack[C] = Stack.empty
      while (offset < content.size) {
        if (content(offset) == '{') {
          s = s.push('{')
        } else if (content(offset) == '}') {
          assert (s.peek.nonEmpty && s.peek.get == '{')
          s = s.pop.get._2
          if (s.isEmpty) {
            return
          }
        }

        increment()

        consumeWhiteSpaceAndComments()
      }
    }

    @pure def consumeName(): String = {
      consumeWhiteSpaceAndComments()
      if (content(offset) == '*') {
        increment()
        consumeWhiteSpaceAndComments()
      }
      val start = offset
      while (StringUtil.isLetter(content(offset)) || StringUtil.isNumber(content(offset)) ||
        content(offset) == '_') {
        increment()
      }
      return subString(start, offset)
    }

    @pure def isEndOfKeyword(z: Z): B = {
      return content(z).isWhitespace ||
        (z + 1 < content.size && content(z) == '/' && (content(z + 1) == '/' || content(z + 1) == '*'))
    }


    var phase: Phase.Type = Phase.ProcessingGlobals

    while (offset < content.size) {
      consumeWhiteSpaceAndComments()

      if (offset < content.size) {
        val beginLine = line
        val beginCol = col
        val beginOffset = offset

        @strictpure def isVolatile: B = offset + 8 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'v' && content(offset + 1) == 'o' && content(offset + 2) == 'l' &&
          content(offset + 3) == 'a' && content(offset + 4) == 't' && content(offset + 5) == 'i' &&
          content(offset + 6) == 'l' && content(offset + 7) == 'e' && isEndOfKeyword(offset + 8)

        phase match {
          case Phase.ProcessingGlobals =>
            if (isVolatile) {

              // volatile ...

              col = col + 8
              offset = offset + 8
              consumeWhiteSpaceAndComments()

              val sharedMemTyp = consumeName()
              val sharedMemIdentifier = consumeName()

              assert (content(offset) == ';')

              cItems = cItems.push(CField(identifier = sharedMemIdentifier, typ = sharedMemTyp, isVolatile = T,
                pos = buildPosition(beginLine = beginLine, beginCol = beginCol, beginOffset = beginOffset,
                  endLine = line, endCol = col, endOffset = offset)))

              increment()
              consumeWhiteSpaceAndComments()

              if (!isVolatile && StringUtil.isLetter(content(offset))) {
                val xbeginLine = line
                val xbeginCol = col
                val xbeginOffset = offset

                val queueType = consumeName()
                val queueIdentifier = consumeName()

                cItems = cItems.push(CField(identifier = queueIdentifier, typ = queueType, isVolatile = F,
                  pos = buildPosition(beginLine = xbeginLine, beginCol = xbeginCol, beginOffset = xbeginOffset,
                    endLine = line, endCol = col, endOffset = offset)))

                increment()
              }
            }
            else if (offset + 7 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
              content(offset) == '#' && content(offset + 1) == 'd' && content(offset + 2) == 'e' &&
              content(offset + 3) == 'f' && content(offset + 4) == 'i' && content(offset + 5) == 'n' &&
              content(offset + 6) == 'e' && isEndOfKeyword(offset + 7)) {

              // #define ... done processing globals

              col = col + 7
              offset = offset + 7

              phase = Phase.ProcessingMethods
            } else {
              // just consume character
              increment()
            }
          case Phase.ProcessingMethods =>
            if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
              content(offset) == 'b' && content(offset + 1) == 'o' && content(offset + 2) == 'o' &&
              content(offset + 3) == 'l' && isEndOfKeyword(offset + 4)) {

              // bool ...

              col = col + 4
              offset = offset + 4

              val methodName = consumeName()
              consumeWhiteSpaceAndComments()

              assert (content(offset) == '(')

              scanToChar(ISZ('{'))

              balanceBrace()

              cItems = cItems.push(CMethod(identifier = methodName, returnType = "bool",
                pos = buildPosition(beginLine = beginLine, beginCol = beginCol, beginOffset = beginOffset,
                  endLine = line, endCol = col, endOffset = offset)))

              increment()
            }
            else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
              content(offset) == 'v' && content(offset + 1) == 'o' && content(offset + 2) == 'i' &&
              content(offset + 3) == 'd' && isEndOfKeyword(offset + 4)) {

              // void ...

              col = col + 4
              offset = offset + 4

              val methodName = consumeName()
              consumeWhiteSpaceAndComments()

              assert (content(offset) == '(')

              scanToChar(ISZ('{'))

              balanceBrace()

              cItems = cItems.push(CMethod(identifier = methodName, returnType = "void",
                pos = buildPosition(beginLine = beginLine, beginCol = beginCol, beginOffset = beginOffset,
                  endLine = line, endCol = col, endOffset = offset)))

              increment()
            }
            else {
              // just consume character
              increment()
            }
        }
      }
    } // end while loop

    return CFile(cItems.elements)
  }
}
