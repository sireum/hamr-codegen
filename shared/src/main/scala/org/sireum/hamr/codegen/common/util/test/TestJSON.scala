// #Sireum
// @formatter:off

// This file is auto-generated from TestResult.scala

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.Json.Printer._
import org.sireum.hamr.codegen.common.util.test.TestResource
import org.sireum.hamr.codegen.common.util.test.TestMarker
import org.sireum.hamr.codegen.common.util.test.ITestResource
import org.sireum.hamr.codegen.common.util.test.ETestResource
import org.sireum.hamr.codegen.common.util.test.TestResult

object TestJSON {

  object Printer {

    @pure def printTestResource(o: TestResource): ST = {
      o match {
        case o: ITestResource => return printITestResource(o)
        case o: ETestResource => return printETestResource(o)
      }
    }

    @pure def printTestMarker(o: TestMarker): ST = {
      return printObject(ISZ(
        ("type", st""""TestMarker""""),
        ("beginMarker", printString(o.beginMarker)),
        ("endMarker", printString(o.endMarker))
      ))
    }

    @pure def printITestResource(o: ITestResource): ST = {
      return printObject(ISZ(
        ("type", st""""ITestResource""""),
        ("content", printString(o.content)),
        ("markers", printISZ(F, o.markers, printTestMarker _)),
        ("invertMarkers", printB(o.invertMarkers)),
        ("overwrite", printB(o.overwrite)),
        ("makeExecutable", printB(o.makeExecutable)),
        ("makeCRLF", printB(o.makeCRLF)),
        ("isDatatype", printB(o.isDatatype))
      ))
    }

    @pure def printETestResource(o: ETestResource): ST = {
      return printObject(ISZ(
        ("type", st""""ETestResource""""),
        ("content", printString(o.content)),
        ("symlink", printB(o.symlink))
      ))
    }

    @pure def printTestResult(o: TestResult): ST = {
      return printObject(ISZ(
        ("type", st""""TestResult""""),
        ("map", printMap(F, o.map, printString _, printTestResource _))
      ))
    }

  }

  @record class Parser(val input: String) {
    val parser: Json.Parser = Json.Parser.create(input)

    def errorOpt: Option[Json.ErrorMsg] = {
      return parser.errorOpt
    }

    def parseTestResource(): TestResource = {
      val t = parser.parseObjectTypes(ISZ("ITestResource", "ETestResource"))
      t.native match {
        case "ITestResource" => val r = parseITestResourceT(T); return r
        case "ETestResource" => val r = parseETestResourceT(T); return r
        case _ => val r = parseETestResourceT(T); return r
      }
    }

    def parseTestMarker(): TestMarker = {
      val r = parseTestMarkerT(F)
      return r
    }

    def parseTestMarkerT(typeParsed: B): TestMarker = {
      if (!typeParsed) {
        parser.parseObjectType("TestMarker")
      }
      parser.parseObjectKey("beginMarker")
      val beginMarker = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("endMarker")
      val endMarker = parser.parseString()
      parser.parseObjectNext()
      return TestMarker(beginMarker, endMarker)
    }

    def parseITestResource(): ITestResource = {
      val r = parseITestResourceT(F)
      return r
    }

    def parseITestResourceT(typeParsed: B): ITestResource = {
      if (!typeParsed) {
        parser.parseObjectType("ITestResource")
      }
      parser.parseObjectKey("content")
      val content = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("markers")
      val markers = parser.parseISZ(parseTestMarker _)
      parser.parseObjectNext()
      parser.parseObjectKey("invertMarkers")
      val invertMarkers = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("overwrite")
      val overwrite = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("makeExecutable")
      val makeExecutable = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("makeCRLF")
      val makeCRLF = parser.parseB()
      parser.parseObjectNext()
      parser.parseObjectKey("isDatatype")
      val isDatatype = parser.parseB()
      parser.parseObjectNext()
      return ITestResource(content, markers, invertMarkers, overwrite, makeExecutable, makeCRLF, isDatatype)
    }

    def parseETestResource(): ETestResource = {
      val r = parseETestResourceT(F)
      return r
    }

    def parseETestResourceT(typeParsed: B): ETestResource = {
      if (!typeParsed) {
        parser.parseObjectType("ETestResource")
      }
      parser.parseObjectKey("content")
      val content = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("symlink")
      val symlink = parser.parseB()
      parser.parseObjectNext()
      return ETestResource(content, symlink)
    }

    def parseTestResult(): TestResult = {
      val r = parseTestResultT(F)
      return r
    }

    def parseTestResultT(typeParsed: B): TestResult = {
      if (!typeParsed) {
        parser.parseObjectType("TestResult")
      }
      parser.parseObjectKey("map")
      val map = parser.parseMap(parser.parseString _, parseTestResource _)
      parser.parseObjectNext()
      return TestResult(map)
    }

    def eof(): B = {
      val r = parser.eof()
      return r
    }

  }

  def to[T](s: String, f: Parser => T): Either[T, Json.ErrorMsg] = {
    val parser = Parser(s)
    val r = f(parser)
    parser.eof()
    parser.errorOpt match {
      case Some(e) => return Either.Right(e)
      case _ => return Either.Left(r)
    }
  }

  def fromTestResource(o: TestResource, isCompact: B): String = {
    val st = Printer.printTestResource(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toTestResource(s: String): Either[TestResource, Json.ErrorMsg] = {
    def fTestResource(parser: Parser): TestResource = {
      val r = parser.parseTestResource()
      return r
    }
    val r = to(s, fTestResource _)
    return r
  }

  def fromTestMarker(o: TestMarker, isCompact: B): String = {
    val st = Printer.printTestMarker(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toTestMarker(s: String): Either[TestMarker, Json.ErrorMsg] = {
    def fTestMarker(parser: Parser): TestMarker = {
      val r = parser.parseTestMarker()
      return r
    }
    val r = to(s, fTestMarker _)
    return r
  }

  def fromITestResource(o: ITestResource, isCompact: B): String = {
    val st = Printer.printITestResource(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toITestResource(s: String): Either[ITestResource, Json.ErrorMsg] = {
    def fITestResource(parser: Parser): ITestResource = {
      val r = parser.parseITestResource()
      return r
    }
    val r = to(s, fITestResource _)
    return r
  }

  def fromETestResource(o: ETestResource, isCompact: B): String = {
    val st = Printer.printETestResource(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toETestResource(s: String): Either[ETestResource, Json.ErrorMsg] = {
    def fETestResource(parser: Parser): ETestResource = {
      val r = parser.parseETestResource()
      return r
    }
    val r = to(s, fETestResource _)
    return r
  }

  def fromTestResult(o: TestResult, isCompact: B): String = {
    val st = Printer.printTestResult(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toTestResult(s: String): Either[TestResult, Json.ErrorMsg] = {
    def fTestResult(parser: Parser): TestResult = {
      val r = parser.parseTestResult()
      return r
    }
    val r = to(s, fTestResult _)
    return r
  }

}