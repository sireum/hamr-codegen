// #Sireum
// @formatter:off

// This file is auto-generated from IntegrationConstraintReporting.scala

package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.Json.Printer._

object IntegrationConstraintJSON {

  object Printer {

    @pure def printIntegrationConstraintReportingSmt2QueryResultType(o: IntegrationConstraintReporting.Smt2QueryResult.Type): ST = {
      val value: String = o match {
        case IntegrationConstraintReporting.Smt2QueryResult.Unsat => "Unsat"
        case IntegrationConstraintReporting.Smt2QueryResult.Sat => "Sat"
        case IntegrationConstraintReporting.Smt2QueryResult.Unknown => "Unknown"
        case IntegrationConstraintReporting.Smt2QueryResult.Timeout => "Timeout"
        case IntegrationConstraintReporting.Smt2QueryResult.Error => "Error"
      }
      return printObject(ISZ(
        ("type", printString("IntegrationConstraintReporting.Smt2QueryResult")),
        ("value", printString(value))
      ))
    }

    @pure def printIntegrationConstraintReportingGclIntegrationConstraint(o: IntegrationConstraintReporting.GclIntegrationConstraint): ST = {
      return printObject(ISZ(
        ("type", st""""IntegrationConstraintReporting.GclIntegrationConstraint""""),
        ("id", printString(o.id)),
        ("descriptor", printOption(T, o.descriptor, printString _)),
        ("position", printPosition(o.position))
      ))
    }

    @pure def printIntegrationConstraintReportingIntegrationConstraint(o: IntegrationConstraintReporting.IntegrationConstraint): ST = {
      return printObject(ISZ(
        ("type", st""""IntegrationConstraintReporting.IntegrationConstraint""""),
        ("srcPort", printString(o.srcPort)),
        ("srcPortPos", printPosition(o.srcPortPos)),
        ("srcPortIntegrationConstraint", printOption(F, o.srcPortIntegrationConstraint, printIntegrationConstraintReportingGclIntegrationConstraint _)),
        ("dstPort", printString(o.dstPort)),
        ("dstPortPos", printPosition(o.dstPortPos)),
        ("dstPortIntegrationConstraint", printIntegrationConstraintReportingGclIntegrationConstraint(o.dstPortIntegrationConstraint)),
        ("connectionMidPoint", printPosition(o.connectionMidPoint)),
        ("claim", printString(o.claim)),
        ("smt2QueryResult", printIntegrationConstraintReportingSmt2QueryResultType(o.smt2QueryResult)),
        ("logikaMessage", printString(o.logikaMessage)),
        ("smt2Query", printString(o.smt2Query))
      ))
    }

  }

  @record class Parser(val input: String) {
    val parser: Json.Parser = Json.Parser.create(input)

    def errorOpt: Option[Json.ErrorMsg] = {
      return parser.errorOpt
    }

    def parseIntegrationConstraintReportingSmt2QueryResultType(): IntegrationConstraintReporting.Smt2QueryResult.Type = {
      val r = parseIntegrationConstraintReportingSmt2QueryResultT(F)
      return r
    }

    def parseIntegrationConstraintReportingSmt2QueryResultT(typeParsed: B): IntegrationConstraintReporting.Smt2QueryResult.Type = {
      if (!typeParsed) {
        parser.parseObjectType("IntegrationConstraintReporting.Smt2QueryResult")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      IntegrationConstraintReporting.Smt2QueryResult.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for IntegrationConstraintReporting.Smt2QueryResult.")
          return IntegrationConstraintReporting.Smt2QueryResult.byOrdinal(0).get
      }
    }

    def parseIntegrationConstraintReportingGclIntegrationConstraint(): IntegrationConstraintReporting.GclIntegrationConstraint = {
      val r = parseIntegrationConstraintReportingGclIntegrationConstraintT(F)
      return r
    }

    def parseIntegrationConstraintReportingGclIntegrationConstraintT(typeParsed: B): IntegrationConstraintReporting.GclIntegrationConstraint = {
      if (!typeParsed) {
        parser.parseObjectType("IntegrationConstraintReporting.GclIntegrationConstraint")
      }
      parser.parseObjectKey("id")
      val id = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("descriptor")
      val descriptor = parser.parseOption(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("position")
      val position = parser.parsePosition()
      parser.parseObjectNext()
      return IntegrationConstraintReporting.GclIntegrationConstraint(id, descriptor, position)
    }

    def parseIntegrationConstraintReportingIntegrationConstraint(): IntegrationConstraintReporting.IntegrationConstraint = {
      val r = parseIntegrationConstraintReportingIntegrationConstraintT(F)
      return r
    }

    def parseIntegrationConstraintReportingIntegrationConstraintT(typeParsed: B): IntegrationConstraintReporting.IntegrationConstraint = {
      if (!typeParsed) {
        parser.parseObjectType("IntegrationConstraintReporting.IntegrationConstraint")
      }
      parser.parseObjectKey("srcPort")
      val srcPort = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("srcPortPos")
      val srcPortPos = parser.parsePosition()
      parser.parseObjectNext()
      parser.parseObjectKey("srcPortIntegrationConstraint")
      val srcPortIntegrationConstraint = parser.parseOption(parseIntegrationConstraintReportingGclIntegrationConstraint _)
      parser.parseObjectNext()
      parser.parseObjectKey("dstPort")
      val dstPort = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("dstPortPos")
      val dstPortPos = parser.parsePosition()
      parser.parseObjectNext()
      parser.parseObjectKey("dstPortIntegrationConstraint")
      val dstPortIntegrationConstraint = parseIntegrationConstraintReportingGclIntegrationConstraint()
      parser.parseObjectNext()
      parser.parseObjectKey("connectionMidPoint")
      val connectionMidPoint = parser.parsePosition()
      parser.parseObjectNext()
      parser.parseObjectKey("claim")
      val claim = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("smt2QueryResult")
      val smt2QueryResult = parseIntegrationConstraintReportingSmt2QueryResultType()
      parser.parseObjectNext()
      parser.parseObjectKey("logikaMessage")
      val logikaMessage = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("smt2Query")
      val smt2Query = parser.parseString()
      parser.parseObjectNext()
      return IntegrationConstraintReporting.IntegrationConstraint(srcPort, srcPortPos, srcPortIntegrationConstraint, dstPort, dstPortPos, dstPortIntegrationConstraint, connectionMidPoint, claim, smt2QueryResult, logikaMessage, smt2Query)
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

  def fromIntegrationConstraintReportingGclIntegrationConstraint(o: IntegrationConstraintReporting.GclIntegrationConstraint, isCompact: B): String = {
    val st = Printer.printIntegrationConstraintReportingGclIntegrationConstraint(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toIntegrationConstraintReportingGclIntegrationConstraint(s: String): Either[IntegrationConstraintReporting.GclIntegrationConstraint, Json.ErrorMsg] = {
    def fIntegrationConstraintReportingGclIntegrationConstraint(parser: Parser): IntegrationConstraintReporting.GclIntegrationConstraint = {
      val r = parser.parseIntegrationConstraintReportingGclIntegrationConstraint()
      return r
    }
    val r = to(s, fIntegrationConstraintReportingGclIntegrationConstraint _)
    return r
  }

  def fromIntegrationConstraintReportingIntegrationConstraint(o: IntegrationConstraintReporting.IntegrationConstraint, isCompact: B): String = {
    val st = Printer.printIntegrationConstraintReportingIntegrationConstraint(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toIntegrationConstraintReportingIntegrationConstraint(s: String): Either[IntegrationConstraintReporting.IntegrationConstraint, Json.ErrorMsg] = {
    def fIntegrationConstraintReportingIntegrationConstraint(parser: Parser): IntegrationConstraintReporting.IntegrationConstraint = {
      val r = parser.parseIntegrationConstraintReportingIntegrationConstraint()
      return r
    }
    val r = to(s, fIntegrationConstraintReportingIntegrationConstraint _)
    return r
  }

}