// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.microkit.plugins.reporting.RustContainers._
import org.sireum.message.{FlatPos, Position}

object RustParserSimple {

  @sig trait Token {
    @pure def beginLine: Z

    @pure def beginCol: Z

    @pure def endLine: Z

    @pure def endCol: Z

    @pure def offset: Z

    @pure def len: Z
  }

  @datatype class pub(val beginLine: Z,
                      val beginCol: Z,
                      val endLine: Z,
                      val endCol: Z,
                      val offset: Z,
                      val len: Z) extends Token

  @datatype class const(val beginLine: Z,
                        val beginCol: Z,
                        val endLine: Z,
                        val endCol: Z,
                        val offset: Z,
                        val len: Z) extends Token

  @datatype class open(val beginLine: Z,
                       val beginCol: Z,
                       val endLine: Z,
                       val endCol: Z,
                       val offset: Z,
                       val len: Z) extends Token

  @datatype class exec(val beginLine: Z,
                       val beginCol: Z,
                       val endLine: Z,
                       val endCol: Z,
                       val offset: Z,
                       val len: Z) extends Token

  @datatype class spec(val beginLine: Z,
                       val beginCol: Z,
                       val endLine: Z,
                       val endCol: Z,
                       val offset: Z,
                       val len: Z) extends Token

  @datatype class fn(val beginLine: Z,
                     val beginCol: Z,
                     val endLine: Z,
                     val endCol: Z,
                     val offset: Z,
                     val len: Z,

                     val name: String) extends Token

  @datatype class rtrait(val beginLine: Z,
                         val beginCol: Z,
                         val endLine: Z,
                         val endCol: Z,
                         val offset: Z,
                         val len: Z,

                         val leftCurlyBraceOffset: Z,
                         val name: String) extends Token

  @datatype class struct(val beginLine: Z,
                         val beginCol: Z,
                         val endLine: Z,
                         val endCol: Z,
                         val offset: Z,
                         val len: Z,

                         val leftCurlyBraceOffset: Z,
                         val name: String) extends Token

  @datatype class impl(val beginLine: Z,
                       val beginCol: Z,
                       val endLine: Z,
                       val endCol: Z,
                       val offset: Z,
                       val len: Z,

                       val leftCurlyBraceOffset: Z,
                       val name: String) extends Token

  @datatype class extern(val beginLine: Z,
                         val beginCol: Z,
                         val endLine: Z,
                         val endCol: Z,
                         val offset: Z,
                         val len: Z,

                         val leftCurlyBraceOffset: Z,
                         val name: String) extends Token

  @datatype class verusMacro(val beginLine: Z,
                             val beginCol: Z,
                             val endLine: Z,
                             val endCol: Z,
                             val offset: Z,
                             val len: Z,

                             val leftCurlyBraceOffset: Z) extends Token

  @datatype class Field(val beginLine: Z,
                        val beginCol: Z,
                        val endLine: Z,
                        val endCol: Z,
                        val offset: Z,
                        val len: Z,

                        val name: String,
                        val isGhost: B) extends Token

  @sig trait Brace {
    @pure def startingOffset: Z
  }

  @datatype class LeftCurlyBrace(val startingOffset: Z) extends Brace

  @pure def parse(f: Os.Path, rootDir: Os.Path, userModifable: B): RustFile = {

    val content = conversions.String.toCis(f.read)

    var tokenStack: Stack[Token] = Stack.empty[Token]

    var braceStack: Stack[Brace] = Stack.empty[Brace]

    var rustItems: Stack[RustItem] = Stack.empty

    @pure def processingStruct(): B = {
      rustItems.peek match {
        case Some(r: RustStruct) =>
          // return true if it hasn't been finalized
          return r.pos.endLine == 0 && r.pos.endColumn == 0 && r.pos.length == 0
        case _ => return F
      }
    }
    @pure def addField(field: RustField): Unit = {
      rustItems.pop match {
        case Some((i: RustStruct, stack)) =>
          val ui = i(fields = i.fields + field.name ~> field)
          rustItems = stack.push(ui)
        case x => halt(s"Expecting an impl for field ${field.name} but found $x")
      }
    }

    @pure def addFunction(fn: RustContainers.RustFn): Unit = {
      if (rustItems.isEmpty) {
        rustItems = rustItems.push(fn)
      } else {
        rustItems.peek.get match {
          case r: RustExtern if !r.finalized =>
            val (_, s) = rustItems.pop.get
            rustItems = s.push(r(methods = r.methods + fn.name ~> fn))
          case r: RustTrait if !r.finalized =>
            val (_, s) = rustItems.pop.get
            rustItems = s.push(r(methods = r.methods + fn.name ~> fn))
          case r: RustImpl if !r.finalized =>
            val (_, s) = rustItems.pop.get
            rustItems = s.push(r(methods = r.methods + fn.name ~> fn))
          case _ =>
            // must be a top level function
            rustItems = rustItems.push(fn)
        }
      }
    }

    @pure def popTokens(): ISZ[Token] = {
      var ret: ISZ[Token] = ISZ()

      @pure def pop(): Unit = {
        val (sym, s) = tokenStack.pop.get
        tokenStack = s
        ret = ret :+ sym
      }

      while (tokenStack.nonEmpty) {
        tokenStack.peek.get match {
          case p: pub => pop()
          case o: open => pop()
          case s: spec => pop()
          case c: const => pop()
          case _ => return ret
        }
      }
      return ret
    }

    @pure def buildPosition(t: Token): Position = {
      assert(t.beginLine >= 0)
      assert(t.beginCol >= 0)

      return FlatPos(
        uriOpt = Some(rootDir.relativize(f).value),
        beginLine32 = conversions.Z.toU32(t.beginLine),
        beginColumn32 = conversions.Z.toU32(t.beginCol),
        endLine32 = conversions.Z.toU32(t.endLine),
        endColumn32 = conversions.Z.toU32(t.endCol),
        offset32 = conversions.Z.toU32(t.offset),
        length32 = conversions.Z.toU32(t.len))
    }

    var line: Z = 1
    var col: Z = 1
    var offset: Z = 0

    @pure def consumeWhiteSpaceAndComments(): Unit = {
      consumeWhiteSpaceAndCommentsH(T)
    }

    @pure def consumeWhiteSpaceAndCommentsH(consumeNewLines: B): (Option[Marker], Option[GumboId]) = {
      while (offset < content.size) {
        if (content(offset).isWhitespace) {
          if (content(offset) == '\n' && !consumeNewLines) {
              return (None(), None())
          }
          increment()
        }
        else if (offset + 1 < content.size && content(offset) == '/' && content(offset + 1) == '/') {
          col = col + 2
          offset = offset + 2

          if (!consumeNewLines) {
            if (offset + 13 < content.size && content(offset).isWhitespace &&
              content(offset + 1) == 'B' && content(offset + 2) == 'E' && content(offset + 3) == 'G' && content(offset + 4) == 'I' && content(offset + 5) == 'N' &&
              content(offset + 6).isWhitespace &&
              content(offset + 7) == 'M' && content(offset + 8) == 'A' && content(offset + 9) == 'R' && content(offset + 10) == 'K' && content(offset + 11) == 'E' && content(offset + 12) == 'R' &&
              content(offset + 13).isWhitespace) {
              // BEGIN MARKER
              val beginCol = col
              val beginOffset = offset
              col = col + 14
              offset = offset + 14
              scanToChar(ISZ('\n'))
              val marker = Marker(id = subString(beginOffset + 14, offset), isBegin = T, pos = buildPosition(pub(beginLine = line, beginCol = beginCol, endLine = line, endCol = col - 1, offset = offset - 1, len = offset - beginOffset - 1)))
              increment()
              return ((Some(marker), None()))
            }
            else if (offset + 11 < content.size && content(offset).isWhitespace &&
              content(offset + 1) == 'E' && content(offset + 2) == 'N' && content(offset + 3) == 'D' &&
              content(offset + 4).isWhitespace &&
              content(offset + 5) == 'M' && content(offset + 6) == 'A' && content(offset + 7) == 'R' && content(offset + 8) == 'K' && content(offset + 9) == 'E' && content(offset + 10) == 'R' &&
              content(offset + 11).isWhitespace) {
              // END MARKER
              val beginCol = col
              val beginOffset = offset
              col = col + 12
              offset = offset + 12
              scanToChar(ISZ('\n'))
              val marker = Marker(id = subString(beginOffset + 12, offset), isBegin = F, pos = buildPosition(pub(beginLine = line, beginCol = beginCol, endLine = line, endCol = col - 1, offset = offset - 1, len = offset - beginOffset - 1)))
              increment()
              return ((Some(marker), None()))
            }
            else if (offset + 5 < content.size && content(offset).isWhitespace &&
              content(offset + 1) == 'c' && content(offset + 2) == 'a' && content(offset + 3) == 's' && content(offset + 4) == 'e' &&
              content(offset + 5).isWhitespace) {
              // case
              val beginCol = col
              val beginOffset = offset
              col = col + 6
              offset = offset + 6
              scanToChar(ISZ('\n'))
              val gumboId = GumboId(id = subString(beginOffset + 6, offset), pos = buildPosition(pub(beginLine = line, beginCol = beginCol, endLine = line, endCol = col - 1, offset = offset - 1, len = offset - beginOffset - 1)))
              increment()
              return ((None(), Some(gumboId)))
            }
            else if (offset + 7 < content.size && content(offset).isWhitespace &&
              content(offset + 1) == 'a' && content(offset + 2) == 's' && content(offset + 3) == 's' && content(offset + 4) == 'u' && content(offset + 5) == 'm' && content(offset + 6) == 'e' &&
              content(offset + 7).isWhitespace) {
              // assume
              val beginCol = col
              val beginOffset = offset
              col = col + 8
              offset = offset + 8
              scanToChar(ISZ('\n'))
              val gumboId = GumboId(id = subString(beginOffset + 8, offset), pos = buildPosition(pub(beginLine = line, beginCol = beginCol, endLine = line, endCol = col - 1, offset = offset - 1, len = offset - beginOffset - 1)))
              increment()
              return ((None(), Some(gumboId)))
            }
            else if (offset + 10 < content.size && content(offset).isWhitespace &&
              content(offset + 1) == 'g' && content(offset + 2) == 'u' && content(offset + 3) == 'a' && content(offset + 4) == 'r' && content(offset + 5) == 'a' && content(offset + 6) == 'n' && content(offset + 7) == 't' && content(offset + 8) == 'e' && content(offset + 9) == 'e' &&
              content(offset + 10).isWhitespace) {
              // guarantee
              val beginCol = col
              val beginOffset = offset
              col = col + 11
              offset = offset + 11
              scanToChar(ISZ('\n'))
              val gumboId = GumboId(id = subString(beginOffset + 11, offset), pos = buildPosition(pub(beginLine = line, beginCol = beginCol, endLine = line, endCol = col - 1, offset = offset - 1, len = offset - beginOffset - 1)))
              increment()
              return ((None(), Some(gumboId)))
            }
          }

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
          return (None(), None())
        }
      }
      return (None(), None())
    }

    @pure def subString(start: Z, end: Z): String = {
      val slice = ops.ISZOps(content).slice(start, end)
      return conversions.String.fromCis(slice)
    }

    @pure def consumeName(): String = {
      consumeWhiteSpaceAndComments()
      val start = offset
      while (StringUtil.isLetter(content(offset)) || StringUtil.isNumber(content(offset)) || content(offset) == '_') {
        increment()
      }
      return subString(start, offset)
    }

    @pure def consumeGeneric(): GenericParam = {
      assert(content(offset) == '<')
      increment()
      consumeWhiteSpaceAndComments()
      val genParam = consumeName()
      consumeWhiteSpaceAndComments()
      assert(content(offset) == ':')
      increment()
      consumeWhiteSpaceAndComments()
      val traitName = consumeName()
      consumeWhiteSpaceAndComments()
      assert(content(offset) == '>')
      increment()
      return GenericParam(genParam, traitName)
    }

    @pure def scanToChar(chars: ISZ[C]): Unit = {
      val charsContainsNewLine = ops.ISZOps(chars).contains('\n')
      while (offset < content.size) {
        if (ops.ISZOps(chars).contains(content(offset))) {
          return
        }

        increment()

        consumeWhiteSpaceAndCommentsH(!charsContainsNewLine)
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

    @pure def isEndOfKeyword(z: Z): B = {
      return content(z).isWhitespace ||
        (z + 1 < content.size && content(z) == '/' && (content(z + 1) == '/' || content(z + 1) == '*'))
    }

    @pure def parseContract(): (Option[VerusBlock], Option[VerusBlock]) = {
      var blocks: Stack[VerusBlock] = Stack.empty
      var optMarker: Option[Marker] = None()

      while (offset < content.size && content(offset) != '{') {
        consumeWhiteSpaceAndCommentsH(F) match {
          case ((Some(marker), None())) =>
            if (optMarker.isEmpty) {
              assert (marker.isBegin, s"Did not find the begin marker for ${marker.id}")
              optMarker = Some(marker)
            } else {
              assert (!marker.isBegin, s"Was expecting to find end marker for ${optMarker.get.id}, but found ${marker.id} instead")
              optMarker = None()
            }
          case ((None(), Some(gumboId))) if !userModifable || optMarker.nonEmpty =>
            consumeWhiteSpaceAndComments()
            var lastCharCol = col
            var lastCharOffset = offset
            var lastValidLine = line
            while(offset < content.size &&
              content(offset) != ',' &&
              content(offset) != '/' &&
              !(offset + 7 < content.size && content(offset) == 'e' && content(offset + 1) == 'n' && content(offset + 2) == 's' && content(offset + 3) == 'u' && content(offset + 4) == 'r' && content(offset + 5) == 'e' && content(offset + 6) == 's' && isEndOfKeyword(offset + 7)) &&
              content(offset) != '{' ) {
              if (!content(offset).isWhitespace) {
                lastCharCol = col
                lastCharOffset = offset
                lastValidLine = line
              }
              increment()
            }
            if (blocks.isEmpty) {
              assume(T)
            }
            val (block, stack) = blocks.pop.get
            val clause = GumboClause(id = gumboId.id, pos = buildPosition(pub(
              beginLine = gumboId.pos.beginLine, beginCol = gumboId.pos.beginColumn,
              endLine = lastValidLine, endCol = lastCharCol, offset = gumboId.pos.offset, len = lastCharOffset - gumboId.pos.offset)))
            blocks = stack.push(block(gumboClauses = block.gumboClauses :+ clause))
          case _ =>
        }
        if (offset + 7 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'r' && content(offset + 1) == 'e' &&content(offset + 2) == 'q' && content(offset + 3) == 'u' && content(offset + 4) == 'i' && content(offset + 5) == 'r' && content(offset + 6) == 'e' && content(offset + 7) == 's' &&
          isEndOfKeyword(offset + 8)) {
          // requires

          blocks = blocks.push (VerusBlock(T, ISZ()))

          col = col + 8
          offset = offset + 8
        }
        else if (offset + 7 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'e' && content(offset + 1) == 'n' &&content(offset + 2) == 's' && content(offset + 3) == 'u' && content(offset + 4) == 'r' && content(offset + 5) == 'e' && content(offset + 6) == 's' &&
          isEndOfKeyword(offset + 7)) {
          // ensures

          blocks = blocks.push (VerusBlock(F, ISZ()))

          col = col + 7
          offset = offset + 7
        } else if (content(offset) != '{') {
          increment()
        }
      }

      var a: Option[VerusBlock] = None()
      var b: Option[VerusBlock] = None()
      for (e <- blocks.elements) {
        if (e.isRequiresBlock) {
          assert (a.isEmpty)
          a = Some(e)
        } else {
          assert(b.isEmpty)
          b = Some(e)
        }
      }
      return (a,b)
    } // end parseContract

    while (offset < content.size) {
      consumeWhiteSpaceAndComments()

      if (offset < content.size) {

        val beginLine = line
        val beginCol = col
        val beginOffset = offset

        if (content(offset) == '{') {
          balanceBrace()

          increment()
        }
        else if (content(offset) == '}') {
          val (brace, stack) = braceStack.pop.get
          braceStack = stack
          assert(brace.isInstanceOf[LeftCurlyBrace], s"Not expecting $brace")

          val (token, remainingTokens) = tokenStack.pop.get
          tokenStack = remainingTokens

          token match {
            case t: verusMacro =>
              assert(t.leftCurlyBraceOffset == brace.startingOffset, s"${t.leftCurlyBraceOffset} vs ${brace.startingOffset}")
              assert(tokenStack.isEmpty)
              increment()
            case t: rtrait =>
              assert(t.leftCurlyBraceOffset == brace.startingOffset, s"${t.leftCurlyBraceOffset} vs ${brace.startingOffset}")

              val us = t(
                endLine = line,
                endCol = col,
                len = offset - t.offset)

              val (item, remainingRustItems) = rustItems.pop.get
              val uitem = item.asInstanceOf[RustTrait]

              rustItems = remainingRustItems.push(uitem(pos = buildPosition(us)))

              increment()
            case t: struct =>
              assert(t.leftCurlyBraceOffset == brace.startingOffset, s"${t.leftCurlyBraceOffset} vs ${brace.startingOffset}")

              val us = t(
                endLine = line,
                endCol = col,
                len = offset - t.offset)

              val (item, remainingRustItems) = rustItems.pop.get
              val uitem = item.asInstanceOf[RustStruct]

              rustItems = remainingRustItems.push(uitem(pos = buildPosition(us)))

              increment()
            case t: impl =>
              assert(t.leftCurlyBraceOffset == brace.startingOffset, s"${t.leftCurlyBraceOffset} vs ${brace.startingOffset}")
              assert (t.endLine == 0)
              assert (t.endCol == 0)
              assert (t.len == 0)

              val ui = t(
                endLine = line,
                endCol = col,
                len = offset - t.offset)

              val (item, remainingRustItems) = rustItems.pop.get
              val uitem = item.asInstanceOf[RustImpl]

              rustItems = remainingRustItems.push(uitem(pos = buildPosition(ui)))

              increment()
            case t: extern =>
              assert(t.leftCurlyBraceOffset == brace.startingOffset, s"${t.leftCurlyBraceOffset} vs ${brace.startingOffset}")
              assert (t.endLine == 0)
              assert (t.endCol == 0)
              assert (t.len == 0)

              val ui = t(
                endLine = line,
                endCol = col,
                len = offset - t.offset)

              val (item, remainingRustItems) = rustItems.pop.get
              val uitem = item.asInstanceOf[RustExtern]

              rustItems = remainingRustItems.push(uitem(pos = buildPosition(ui)))

              increment()
            case x => halt(s"Found unmatched $x after '}' at {$line,$offset}")
          }
        }
        else if (content(offset) == '\n') {
          increment()
        }
        else if (offset + 3 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'p' && content(offset + 1) == 'u' && content(offset + 2) == 'b' &&
          isEndOfKeyword(offset + 3)) {
          // pub
          col = col + 3
          offset = offset + 3
          consumeWhiteSpaceAndComments()
          if (processingStruct()) {
            // must be a field
            if (offset + 5 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
              content(offset) == 'g' && content(offset + 1) == 'h' && content(offset + 2) == 'o' && content(offset + 3) == 's' &&
              content(offset + 4) == 't' && isEndOfKeyword(offset + 5)) {
              // pub ghost <field-name>
              col = col + 5
              offset = offset + 5
              consumeWhiteSpaceAndComments()
              val name = consumeName()

              scanToChar(ISZ(',', '\n'))

              val field = Field(
                beginLine = beginLine, beginCol = beginCol,
                endLine = line, endCol = col - 1,
                offset = beginOffset, len = offset - beginOffset - 1,
                name = name, isGhost = T)

              val rustField = RustField(
                name = name,
                visibility = Visibility.Public,
                isGhost = T,
                pos = buildPosition(field))

              addField(rustField)

            } else {
              // pub <field-name>
              // record non-ghost struct field
              consumeWhiteSpaceAndComments()
              val name = consumeName()

              scanToChar(ISZ(',', '\n'))

              val field = Field(
                beginLine = beginLine, beginCol = beginCol,
                endLine = line, endCol = col - 1,
                offset = beginOffset, len = offset - beginOffset - 1,
                name = name, isGhost = F)

              val rustField = RustField(
                name = name,
                visibility = Visibility.Public,
                isGhost = F,
                pos = buildPosition(field))

              addField(rustField)
            }
          } else {
            // pub
            tokenStack = tokenStack.push(pub(
              beginLine = beginLine, beginCol = beginCol,
              endLine = line, endCol = col - 1,
              offset = beginOffset, len = offset - beginOffset - 1))
          }
        }
        else if (offset + 5 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'c' && content(offset + 1) == 'o' && content(offset + 2) == 'n' && content(offset + 3) == 's' && content(offset + 4) == 't' &&
          isEndOfKeyword(offset + 5)) {
          // const
          col = col + 5
          offset = offset + 5
          tokenStack = tokenStack.push(const(
            beginLine = line, beginCol = beginCol,
            endLine = line, endCol = col - 1,
            offset = beginOffset, len = offset - beginOffset - 1))
        }
        else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'o' && content(offset + 1) == 'p' && content(offset + 2) == 'e' && content(offset + 3) == 'n' &&
          isEndOfKeyword(offset + 4)) {
          // open
          col = col + 4
          offset = offset + 4
          tokenStack = tokenStack.push(open(
            beginLine = line, beginCol = beginCol,
            endLine = line, endCol = col - 1,
            offset = beginOffset, len = offset - beginOffset - 1))
        }
        else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 's' && content(offset + 1) == 'p' && content(offset + 2) == 'e' && content(offset + 3) == 'c' &&
          isEndOfKeyword(offset + 4)) {
          // spec
          col = col + 4
          offset = offset + 4
          tokenStack = tokenStack.push(spec(
            beginLine = line, beginCol = beginCol,
            endLine = line, endCol = col - 1,
            offset = beginOffset, len = offset - beginOffset - 1))
        }
        else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'e' && content(offset + 1) == 'x' && content(offset + 2) == 'e' && content(offset + 3) == 'x' &&
          isEndOfKeyword(offset + 4)) {
          // exec
          col = col + 4
          offset = offset + 4
          tokenStack = tokenStack.push(exec(
            beginLine = line, beginCol = beginCol,
            endLine = line, endCol = col - 1,
            offset = beginOffset, len = offset - beginOffset - 1))
        }
        else if (offset + 2 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'f' && content(offset + 1) == 'n' && isEndOfKeyword(offset + 2)) {
          // fn <name>
          col = col + 2
          offset = offset + 2
          consumeWhiteSpaceAndComments()
          val name = consumeName()

          val oldLine = line
          val oldCol = col
          val oldOffest = offset

          scanToChar(ISZ(';', '{'))

          val contracts: (Option[VerusBlock], Option[VerusBlock]) =
            if (content(offset) == '{') {
              line = oldLine
              col = oldCol
              offset = oldOffest
              val r = parseContract()

              assert (content(offset) == '{')

              balanceBrace()

              r
            } else {
              (None(), None())
            }

          val token = fn(
            beginLine = beginLine, beginCol = beginCol,
            endLine = line, endCol = col,
            offset = beginOffset, len = offset - beginOffset,
            name = name)

          val poppedTokens = ops.ISZOps(popTokens())

          val kind: RustFnKind.Type =
            if (poppedTokens.filter(p => p.isInstanceOf[spec]).nonEmpty) RustFnKind.Spec
            else if (poppedTokens.filter(p => p.isInstanceOf[exec]).nonEmpty) RustFnKind.Exec
            else RustFnKind.Rust

          addFunction(RustFn(
            name = name,
            visibility = if (poppedTokens.filter(p => p.isInstanceOf[pub]).nonEmpty) Visibility.Public else Visibility.Private,
            isConst = poppedTokens.filter(p => p.isInstanceOf[const]).nonEmpty,
            kind = kind,
            requires = contracts._1,
            ensures = contracts._2,
            pos = buildPosition(token)))

          increment()
        }
        else if (offset + 6 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 's' && content(offset + 1) == 't' && content(offset + 2) == 'r' && content(offset + 3) == 'u' &&
          content(offset + 4) == 'c' && content(offset + 5) == 't' && isEndOfKeyword(offset + 6)) {
          // struct <name>
          col = col + 6
          offset = offset + 6
          consumeWhiteSpaceAndComments()
          val name = consumeName()
          scanToChar(ISZ(';', '{'))

          val tokens = popTokens()
          assert(ops.ISZOps(tokens).forall(c => c.isInstanceOf[pub]), tokens.string)

          val token: Token =
            if (content(offset) == ';') {
              struct(
                beginLine = beginLine, beginCol = beginCol,
                endLine = 0, endCol = 0,
                offset = beginOffset, len = 0,
                leftCurlyBraceOffset = offset,
                name = name)
            } else {
              assert (content(offset) == '{' )

              braceStack = braceStack.push(LeftCurlyBrace(offset))

              val token = struct(
                beginLine = beginLine, beginCol = beginCol,
                endLine = 0, endCol = 0,
                offset = beginOffset, len = 0,
                leftCurlyBraceOffset = offset,
                name = name)

              tokenStack = tokenStack.push(token)
              token
            }

          rustItems = rustItems.push(RustStruct(name = name, fields = Map.empty, pos = buildPosition(token)))

          increment()
        }
        else if (offset + 6 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'e' && content(offset + 1) == 'x' && content(offset + 2) == 't' && content(offset + 3) == 'e' &&
          content(offset + 4) == 'r' && content(offset + 5) == 'n' && isEndOfKeyword(offset + 6)) {
          // extern
          col = col + 6
          offset = offset + 6
          consumeWhiteSpaceAndComments()
          val name = consumeName()
          scanToChar(ISZ('{'))

          val tokens = popTokens()
          assert(ops.ISZOps(tokens).forall(c => c.isInstanceOf[pub]), tokens.string)

          val token: Token = {

            assert (content(offset) == '{' )

            braceStack = braceStack.push(LeftCurlyBrace(offset))

              val token = extern(
                beginLine = beginLine, beginCol = beginCol,
                endLine = 0, endCol = 0,
                offset = beginOffset, len = 0,
                leftCurlyBraceOffset = offset,
                name = name)

              tokenStack = tokenStack.push(token)
              token
            }

          rustItems = rustItems.push(RustExtern(name = name, methods = Map.empty, pos = buildPosition(token)))

          increment()
        }
        else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'v' && content(offset + 1) == 'e' && content(offset + 2) == 'r' && content(offset + 3) == 'u' && content(offset + 4) == 's' && content(offset + 5) == '!' &&
          (isEndOfKeyword(offset + 6) || content(offset + 6) == '{')) {
          // verus! {...}
          col = col + 6
          offset = offset + 6
          consumeWhiteSpaceAndComments()
          scanToChar(ISZ('{'))
          braceStack = braceStack.push(LeftCurlyBrace(offset))

          val token = verusMacro(
            beginLine = beginLine, beginCol = beginCol,
            endLine = 0, endCol = 0,
            offset = beginOffset, len = 0,
            leftCurlyBraceOffset = offset)

          tokenStack = tokenStack.push(token)

          increment()
        }
        else if (offset + 4 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 'i' && content(offset + 1) == 'm' && content(offset + 2) == 'p' && content(offset + 3) == 'l' &&
          (isEndOfKeyword(offset + 4) || content(offset + 4) == '<')) {
          // impl <name>
          col = col + 4
          offset = offset + 4
          consumeWhiteSpaceAndComments()
          var genericParam: Option[GenericParam] = None()
          if (content(offset) == '<') {
            genericParam = Some(consumeGeneric())
            consumeWhiteSpaceAndComments()
          }
          val name = consumeName()
          scanToChar(ISZ('{'))
          braceStack = braceStack.push(LeftCurlyBrace(offset))

          val token = impl(
            beginLine = beginLine, beginCol = beginCol,
            endLine = 0, endCol = 0,
            offset = beginOffset, len = 0,
            leftCurlyBraceOffset = offset,
            name = name)

          tokenStack = tokenStack.push(token)

          rustItems = rustItems.push(RustImpl(genericParam = genericParam, name = name, methods = Map.empty, pos = buildPosition(token)))

          increment()
        }
        else if (offset + 5 < content.size && (offset == 0 || content(offset - 1).isWhitespace) &&
          content(offset) == 't' && content(offset + 1) == 'r' && content(offset + 2) == 'a' && content(offset + 3) == 'i' && content(offset + 4) == 't' &&
          isEndOfKeyword(offset + 5)) {
          // trait <name>
          col = col + 5
          offset = offset + 5
          consumeWhiteSpaceAndComments()
          val name = consumeName()
          scanToChar(ISZ('{'))
          braceStack = braceStack.push(LeftCurlyBrace(offset))

          val token = rtrait(
            beginLine = beginLine, beginCol = beginCol,
            endLine = 0, endCol = 0,
            offset = beginOffset, len = 0,
            leftCurlyBraceOffset = offset,
            name = name)

          tokenStack = tokenStack.push(token)

          rustItems = rustItems.push(RustTrait(name = name, methods = Map.empty, pos = buildPosition(token)))

          increment()
        }
        else {
          // just consume character
          increment()
        }
      }
    } // end while loop

    return RustFile(rustItems.elements)
  }
}