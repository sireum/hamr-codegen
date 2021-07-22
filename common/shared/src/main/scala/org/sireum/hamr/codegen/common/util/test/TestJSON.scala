// #Sireum
// @formatter:off

// This file is auto-generated from TestResult.scala

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.Json.Printer._

object TestJSON {

  object Printer {

    @pure def printTestResource(o: TestResource): ST = {
      return printObject(ISZ(
        ("type", st""""TestResource""""),
        ("content", printString(o.content)),
        ("overwrite", printB(o.overwrite)),
        ("makeExecutable", printB(o.makeExecutable)),
        ("makeCRLF", printB(o.makeCRLF))
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
      val r = parseTestResourceT(F)
      return r
    }

    def parseTestResourceT(typeParsed: B): TestResource = {
      if (!typeParsed) {
        parser.parseObjectType("TestResource")
      }
      parser.parseObjectKey("content")
      val content = parser.parseString()
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
      return TestResource(content, overwrite, makeExecutable, makeCRLF)
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