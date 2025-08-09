// #Sireum
// @formatter:off

// This file is auto-generated from CodegenReport.scala, MicrokitReport.scala

package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.Json.Printer._
import org.sireum.hamr.codegen.common.reporting.CodegenReport
import org.sireum.hamr.codegen.common.reporting.CodegenReports
import org.sireum.hamr.codegen.common.reporting.ResourceReport
import org.sireum.hamr.codegen.common.reporting.ToolReport

object JSON {

  object Printer {

    @pure def printCodegenReport(o: CodegenReport): ST = {
      o match {
        case o: ToolReport => return printToolReport(o)
        case o: org.sireum.hamr.codegen.microkit.reporting.MicrokitReport => return print_microkitreportingMicrokitReport(o)
      }
    }

    @pure def printCodegenReports(o: CodegenReports): ST = {
      return printObject(ISZ(
        ("type", st""""CodegenReports""""),
        ("reports", printMap(F, o.reports, printString _, printCodegenReport _))
      ))
    }

    @pure def printStatusType(o: Status.Type): ST = {
      val value: String = o match {
        case Status.Success => "Success"
        case Status.Failure => "Failure"
      }
      return printObject(ISZ(
        ("type", printString("Status")),
        ("value", printString(value))
      ))
    }

    @pure def printResourceReport(o: ResourceReport): ST = {
      return printObject(ISZ(
        ("type", st""""ResourceReport""""),
        ("path", printString(o.path)),
        ("overwrittenIfExists", printB(o.overwrittenIfExists))
      ))
    }

    @pure def printToolReport(o: ToolReport): ST = {
      return printObject(ISZ(
        ("type", st""""ToolReport""""),
        ("commandLineArgs", printString(o.commandLineArgs)),
        ("status", printStatusType(o.status)),
        ("warningMessages", printISZ(F, o.warningMessages, printMessage _)),
        ("errorMessages", printISZ(F, o.errorMessages, printMessage _)),
        ("resources", printISZ(F, o.resources, printResourceReport _))
      ))
    }

    @pure def print_microkitreportingIdPos(o: org.sireum.hamr.codegen.microkit.reporting.IdPos): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.IdPos""""),
        ("id", printString(o.id)),
        ("pos", printPosition(o.pos))
      ))
    }

    @pure def print_microkitreportingIdPathR(o: org.sireum.hamr.codegen.microkit.reporting.IdPathR): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.IdPathR""""),
        ("idPath", printISZ(T, o.idPath, printString _))
      ))
    }

    @pure def print_microkitreportingMicrokitReport(o: org.sireum.hamr.codegen.microkit.reporting.MicrokitReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.MicrokitReport""""),
        ("componentReport", printHashSMap(F, o.componentReport, print_microkitreportingIdPathR _, print_microkitreportingComponentReport _))
      ))
    }

    @pure def print_microkitreportingComponentKindType(o: org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Type): ST = {
      val value: String = o match {
        case org.sireum.hamr.codegen.microkit.reporting.ComponentKind.System => "System"
        case org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Process => "Process"
        case org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Thread => "Thread"
        case org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Data => "Data"
      }
      return printObject(ISZ(
        ("type", printString("org.sireum.hamr.codegen.microkit.reporting.ComponentKind")),
        ("value", printString(value))
      ))
    }

    @pure def print_microkitreportingPretty(o: org.sireum.hamr.codegen.microkit.reporting.Pretty): ST = {
      o match {
        case o: org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact => return print_microkitreportingPortLanguageArtifact(o)
        case o: org.sireum.hamr.codegen.microkit.reporting.PortReport => return print_microkitreportingPortReport(o)
      }
    }

    @pure def print_microkitreportingModelProperties(o: org.sireum.hamr.codegen.microkit.reporting.ModelProperties): ST = {
      o match {
        case o: org.sireum.hamr.codegen.microkit.reporting.SimpleProperty => return print_microkitreportingSimpleProperty(o)
      }
    }

    @pure def print_microkitreportingSimpleProperty(o: org.sireum.hamr.codegen.microkit.reporting.SimpleProperty): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.SimpleProperty""""),
        ("name", printString(o.name)),
        ("value", printString(o.value))
      ))
    }

    @pure def print_microkitreportingComponentReport(o: org.sireum.hamr.codegen.microkit.reporting.ComponentReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.ComponentReport""""),
        ("path", printISZ(T, o.path, printString _)),
        ("kind", print_microkitreportingComponentKindType(o.kind)),
        ("optModelType", printOption(F, o.optModelType, print_microkitreportingIdPos _)),
        ("modelImplementation", print_microkitreportingIdPos(o.modelImplementation)),
        ("modelProperties", printISZ(F, o.modelProperties, print_microkitreportingModelProperties _)),
        ("subcomponents", printISZ(F, o.subcomponents, print_microkitreportingIdPathR _)),
        ("ports", printHashSMap(F, o.ports, print_microkitreportingIdPathR _, print_microkitreportingPortReport _)),
        ("cCodeReport", printOption(F, o.cCodeReport, print_microkitreportingCCodeReport _)),
        ("rustReport", printOption(F, o.rustReport, print_microkitreportingRustReport _)),
        ("gumboReport", printOption(F, o.gumboReport, print_microkitreportingGumboReport _)),
        ("gumboXReport", printOption(F, o.gumboXReport, print_microkitreportingGumboXReport _))
      ))
    }

    @pure def print_microkitreportingPortKindType(o: org.sireum.hamr.codegen.microkit.reporting.PortKind.Type): ST = {
      val value: String = o match {
        case org.sireum.hamr.codegen.microkit.reporting.PortKind.Event => "Event"
        case org.sireum.hamr.codegen.microkit.reporting.PortKind.EventData => "EventData"
        case org.sireum.hamr.codegen.microkit.reporting.PortKind.Data => "Data"
      }
      return printObject(ISZ(
        ("type", printString("org.sireum.hamr.codegen.microkit.reporting.PortKind")),
        ("value", printString(value))
      ))
    }

    @pure def print_microkitreportingPortDirectionType(o: org.sireum.hamr.codegen.microkit.reporting.PortDirection.Type): ST = {
      val value: String = o match {
        case org.sireum.hamr.codegen.microkit.reporting.PortDirection.In => "In"
        case org.sireum.hamr.codegen.microkit.reporting.PortDirection.Out => "Out"
      }
      return printObject(ISZ(
        ("type", printString("org.sireum.hamr.codegen.microkit.reporting.PortDirection")),
        ("value", printString(value))
      ))
    }

    @pure def print_microkitreportingPortLangRealization(o: org.sireum.hamr.codegen.microkit.reporting.PortLangRealization): ST = {
      o match {
        case o: org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact => return print_microkitreportingPortLanguageArtifact(o)
      }
    }

    @pure def print_microkitreportingPortLanguageArtifact(o: org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact""""),
        ("name", printString(o.name)),
        ("title", printString(o.title)),
        ("pos", printPosition(o.pos))
      ))
    }

    @pure def print_microkitreportingPortReport(o: org.sireum.hamr.codegen.microkit.reporting.PortReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.PortReport""""),
        ("name", printISZ(T, o.name, printString _)),
        ("kind", print_microkitreportingPortKindType(o.kind)),
        ("direction", print_microkitreportingPortDirectionType(o.direction)),
        ("payload", printOption(T, o.payload, printString _)),
        ("modelPos", printPosition(o.modelPos)),
        ("languageRealizations", printISZ(F, o.languageRealizations, print_microkitreportingPortLangRealization _))
      ))
    }

    @pure def print_microkitreportingCCodeReport(o: org.sireum.hamr.codegen.microkit.reporting.CCodeReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.CCodeReport"""")
      ))
    }

    @pure def print_microkitreportingRustReport(o: org.sireum.hamr.codegen.microkit.reporting.RustReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.RustReport""""),
        ("entrypointReport", printHashSMap(F, o.entrypointReport, printString _, printPosition _)),
        ("apiReport", print_microkitreportingRustApiReport(o.apiReport))
      ))
    }

    @pure def print_microkitreportingRustApiReport(o: org.sireum.hamr.codegen.microkit.reporting.RustApiReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.RustApiReport""""),
        ("extern_c_apiPath", printString(o.extern_c_apiPath)),
        ("developerApiPath", printString(o.developerApiPath)),
        ("developerApiReport", printHashSMap(F, o.developerApiReport, printString _, printPosition _)),
        ("testApiPath", printString(o.testApiPath))
      ))
    }

    @pure def print_microkitreportingGumboReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.GumboReport""""),
        ("stateReport", printHashSMap(F, o.stateReport, printString _, printPosition _)),
        ("methodsReport", printHashSMap(F, o.methodsReport, printString _, printPosition _)),
        ("invariantsReport", printHashSMap(F, o.invariantsReport, printString _, printPosition _)),
        ("integrationReport", printOption(F, o.integrationReport, print_microkitreportingGubmoIntegrationReport _)),
        ("initializeReport", printHashSMap(F, o.initializeReport, printString _, printPosition _)),
        ("computeReport", printOption(F, o.computeReport, print_microkitreportingGumboComputeReport _))
      ))
    }

    @pure def print_microkitreportingGubmoIntegrationReport(o: org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport""""),
        ("assumesReport", printHashSMap(F, o.assumesReport, printString _, printPosition _)),
        ("guaranteesReport", printHashSMap(F, o.guaranteesReport, printString _, printPosition _))
      ))
    }

    @pure def print_microkitreportingGumboComputeReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport""""),
        ("assumesReport", printHashSMap(F, o.assumesReport, printString _, printPosition _)),
        ("guaranteesReport", printHashSMap(F, o.guaranteesReport, printString _, printPosition _)),
        ("casesReport", printHashSMap(F, o.casesReport, printString _, printPosition _)),
        ("handlers", printHashSMap(F, o.handlers, printString _, printPosition _))
      ))
    }

    @pure def print_microkitreportingGumboXReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboXReport): ST = {
      return printObject(ISZ(
        ("type", st""""org.sireum.hamr.codegen.microkit.reporting.GumboXReport""""),
        ("gumboxMethods", printHashSMap(F, o.gumboxMethods, printString _, printPosition _))
      ))
    }

  }

  @record class Parser(val input: String) {
    val parser: Json.Parser = Json.Parser.create(input)

    def errorOpt: Option[Json.ErrorMsg] = {
      return parser.errorOpt
    }

    def parseCodegenReport(): CodegenReport = {
      val t = parser.parseObjectTypes(ISZ("ToolReport", "org.sireum.hamr.codegen.microkit.reporting.MicrokitReport"))
      t.native match {
        case "ToolReport" => val r = parseToolReportT(T); return r
        case "org.sireum.hamr.codegen.microkit.reporting.MicrokitReport" => val r = parse_microkitreportingMicrokitReportT(T); return r
        case _ => val r = parse_microkitreportingMicrokitReportT(T); return r
      }
    }

    def parseCodegenReports(): CodegenReports = {
      val r = parseCodegenReportsT(F)
      return r
    }

    def parseCodegenReportsT(typeParsed: B): CodegenReports = {
      if (!typeParsed) {
        parser.parseObjectType("CodegenReports")
      }
      parser.parseObjectKey("reports")
      val reports = parser.parseMap(parser.parseString _, parseCodegenReport _)
      parser.parseObjectNext()
      return CodegenReports(reports)
    }

    def parseStatusType(): Status.Type = {
      val r = parseStatusT(F)
      return r
    }

    def parseStatusT(typeParsed: B): Status.Type = {
      if (!typeParsed) {
        parser.parseObjectType("Status")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      Status.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for Status.")
          return Status.byOrdinal(0).get
      }
    }

    def parseResourceReport(): ResourceReport = {
      val r = parseResourceReportT(F)
      return r
    }

    def parseResourceReportT(typeParsed: B): ResourceReport = {
      if (!typeParsed) {
        parser.parseObjectType("ResourceReport")
      }
      parser.parseObjectKey("path")
      val path = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("overwrittenIfExists")
      val overwrittenIfExists = parser.parseB()
      parser.parseObjectNext()
      return ResourceReport(path, overwrittenIfExists)
    }

    def parseToolReport(): ToolReport = {
      val r = parseToolReportT(F)
      return r
    }

    def parseToolReportT(typeParsed: B): ToolReport = {
      if (!typeParsed) {
        parser.parseObjectType("ToolReport")
      }
      parser.parseObjectKey("commandLineArgs")
      val commandLineArgs = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("status")
      val status = parseStatusType()
      parser.parseObjectNext()
      parser.parseObjectKey("warningMessages")
      val warningMessages = parser.parseISZ(parser.parseMessage _)
      parser.parseObjectNext()
      parser.parseObjectKey("errorMessages")
      val errorMessages = parser.parseISZ(parser.parseMessage _)
      parser.parseObjectNext()
      parser.parseObjectKey("resources")
      val resources = parser.parseISZ(parseResourceReport _)
      parser.parseObjectNext()
      return ToolReport(commandLineArgs, status, warningMessages, errorMessages, resources)
    }

    def parse_microkitreportingIdPos(): org.sireum.hamr.codegen.microkit.reporting.IdPos = {
      val r = parse_microkitreportingIdPosT(F)
      return r
    }

    def parse_microkitreportingIdPosT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.IdPos = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.IdPos")
      }
      parser.parseObjectKey("id")
      val id = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("pos")
      val pos = parser.parsePosition()
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.IdPos(id, pos)
    }

    def parse_microkitreportingIdPathR(): org.sireum.hamr.codegen.microkit.reporting.IdPathR = {
      val r = parse_microkitreportingIdPathRT(F)
      return r
    }

    def parse_microkitreportingIdPathRT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.IdPathR = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.IdPathR")
      }
      parser.parseObjectKey("idPath")
      val idPath = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.IdPathR(idPath)
    }

    def parse_microkitreportingMicrokitReport(): org.sireum.hamr.codegen.microkit.reporting.MicrokitReport = {
      val r = parse_microkitreportingMicrokitReportT(F)
      return r
    }

    def parse_microkitreportingMicrokitReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.MicrokitReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.MicrokitReport")
      }
      parser.parseObjectKey("componentReport")
      val componentReport = parser.parseHashSMap(parse_microkitreportingIdPathR _, parse_microkitreportingComponentReport _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.MicrokitReport(componentReport)
    }

    def parse_microkitreportingComponentKindType(): org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Type = {
      val r = parse_microkitreportingComponentKindT(F)
      return r
    }

    def parse_microkitreportingComponentKindT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.ComponentKind.Type = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.ComponentKind")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      org.sireum.hamr.codegen.microkit.reporting.ComponentKind.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for org.sireum.hamr.codegen.microkit.reporting.ComponentKind.")
          return org.sireum.hamr.codegen.microkit.reporting.ComponentKind.byOrdinal(0).get
      }
    }

    def parse_microkitreportingPretty(): org.sireum.hamr.codegen.microkit.reporting.Pretty = {
      val t = parser.parseObjectTypes(ISZ("org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact", "org.sireum.hamr.codegen.microkit.reporting.PortReport"))
      t.native match {
        case "org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact" => val r = parse_microkitreportingPortLanguageArtifactT(T); return r
        case "org.sireum.hamr.codegen.microkit.reporting.PortReport" => val r = parse_microkitreportingPortReportT(T); return r
        case _ => val r = parse_microkitreportingPortReportT(T); return r
      }
    }

    def parse_microkitreportingModelProperties(): org.sireum.hamr.codegen.microkit.reporting.ModelProperties = {
      val t = parser.parseObjectTypes(ISZ("org.sireum.hamr.codegen.microkit.reporting.SimpleProperty"))
      t.native match {
        case "org.sireum.hamr.codegen.microkit.reporting.SimpleProperty" => val r = parse_microkitreportingSimplePropertyT(T); return r
        case _ => val r = parse_microkitreportingSimplePropertyT(T); return r
      }
    }

    def parse_microkitreportingSimpleProperty(): org.sireum.hamr.codegen.microkit.reporting.SimpleProperty = {
      val r = parse_microkitreportingSimplePropertyT(F)
      return r
    }

    def parse_microkitreportingSimplePropertyT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.SimpleProperty = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.SimpleProperty")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("value")
      val value = parser.parseString()
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.SimpleProperty(name, value)
    }

    def parse_microkitreportingComponentReport(): org.sireum.hamr.codegen.microkit.reporting.ComponentReport = {
      val r = parse_microkitreportingComponentReportT(F)
      return r
    }

    def parse_microkitreportingComponentReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.ComponentReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.ComponentReport")
      }
      parser.parseObjectKey("path")
      val path = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("kind")
      val kind = parse_microkitreportingComponentKindType()
      parser.parseObjectNext()
      parser.parseObjectKey("optModelType")
      val optModelType = parser.parseOption(parse_microkitreportingIdPos _)
      parser.parseObjectNext()
      parser.parseObjectKey("modelImplementation")
      val modelImplementation = parse_microkitreportingIdPos()
      parser.parseObjectNext()
      parser.parseObjectKey("modelProperties")
      val modelProperties = parser.parseISZ(parse_microkitreportingModelProperties _)
      parser.parseObjectNext()
      parser.parseObjectKey("subcomponents")
      val subcomponents = parser.parseISZ(parse_microkitreportingIdPathR _)
      parser.parseObjectNext()
      parser.parseObjectKey("ports")
      val ports = parser.parseHashSMap(parse_microkitreportingIdPathR _, parse_microkitreportingPortReport _)
      parser.parseObjectNext()
      parser.parseObjectKey("cCodeReport")
      val cCodeReport = parser.parseOption(parse_microkitreportingCCodeReport _)
      parser.parseObjectNext()
      parser.parseObjectKey("rustReport")
      val rustReport = parser.parseOption(parse_microkitreportingRustReport _)
      parser.parseObjectNext()
      parser.parseObjectKey("gumboReport")
      val gumboReport = parser.parseOption(parse_microkitreportingGumboReport _)
      parser.parseObjectNext()
      parser.parseObjectKey("gumboXReport")
      val gumboXReport = parser.parseOption(parse_microkitreportingGumboXReport _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.ComponentReport(path, kind, optModelType, modelImplementation, modelProperties, subcomponents, ports, cCodeReport, rustReport, gumboReport, gumboXReport)
    }

    def parse_microkitreportingPortKindType(): org.sireum.hamr.codegen.microkit.reporting.PortKind.Type = {
      val r = parse_microkitreportingPortKindT(F)
      return r
    }

    def parse_microkitreportingPortKindT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.PortKind.Type = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.PortKind")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      org.sireum.hamr.codegen.microkit.reporting.PortKind.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for org.sireum.hamr.codegen.microkit.reporting.PortKind.")
          return org.sireum.hamr.codegen.microkit.reporting.PortKind.byOrdinal(0).get
      }
    }

    def parse_microkitreportingPortDirectionType(): org.sireum.hamr.codegen.microkit.reporting.PortDirection.Type = {
      val r = parse_microkitreportingPortDirectionT(F)
      return r
    }

    def parse_microkitreportingPortDirectionT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.PortDirection.Type = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.PortDirection")
      }
      parser.parseObjectKey("value")
      var i = parser.offset
      val s = parser.parseString()
      parser.parseObjectNext()
      org.sireum.hamr.codegen.microkit.reporting.PortDirection.byName(s) match {
        case Some(r) => return r
        case _ =>
          parser.parseException(i, s"Invalid element name '$s' for org.sireum.hamr.codegen.microkit.reporting.PortDirection.")
          return org.sireum.hamr.codegen.microkit.reporting.PortDirection.byOrdinal(0).get
      }
    }

    def parse_microkitreportingPortLangRealization(): org.sireum.hamr.codegen.microkit.reporting.PortLangRealization = {
      val t = parser.parseObjectTypes(ISZ("org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact"))
      t.native match {
        case "org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact" => val r = parse_microkitreportingPortLanguageArtifactT(T); return r
        case _ => val r = parse_microkitreportingPortLanguageArtifactT(T); return r
      }
    }

    def parse_microkitreportingPortLanguageArtifact(): org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact = {
      val r = parse_microkitreportingPortLanguageArtifactT(F)
      return r
    }

    def parse_microkitreportingPortLanguageArtifactT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact")
      }
      parser.parseObjectKey("name")
      val name = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("title")
      val title = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("pos")
      val pos = parser.parsePosition()
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact(name, title, pos)
    }

    def parse_microkitreportingPortReport(): org.sireum.hamr.codegen.microkit.reporting.PortReport = {
      val r = parse_microkitreportingPortReportT(F)
      return r
    }

    def parse_microkitreportingPortReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.PortReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.PortReport")
      }
      parser.parseObjectKey("name")
      val name = parser.parseISZ(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("kind")
      val kind = parse_microkitreportingPortKindType()
      parser.parseObjectNext()
      parser.parseObjectKey("direction")
      val direction = parse_microkitreportingPortDirectionType()
      parser.parseObjectNext()
      parser.parseObjectKey("payload")
      val payload = parser.parseOption(parser.parseString _)
      parser.parseObjectNext()
      parser.parseObjectKey("modelPos")
      val modelPos = parser.parsePosition()
      parser.parseObjectNext()
      parser.parseObjectKey("languageRealizations")
      val languageRealizations = parser.parseISZ(parse_microkitreportingPortLangRealization _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.PortReport(name, kind, direction, payload, modelPos, languageRealizations)
    }

    def parse_microkitreportingCCodeReport(): org.sireum.hamr.codegen.microkit.reporting.CCodeReport = {
      val r = parse_microkitreportingCCodeReportT(F)
      return r
    }

    def parse_microkitreportingCCodeReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.CCodeReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.CCodeReport")
      }
      return org.sireum.hamr.codegen.microkit.reporting.CCodeReport()
    }

    def parse_microkitreportingRustReport(): org.sireum.hamr.codegen.microkit.reporting.RustReport = {
      val r = parse_microkitreportingRustReportT(F)
      return r
    }

    def parse_microkitreportingRustReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.RustReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.RustReport")
      }
      parser.parseObjectKey("entrypointReport")
      val entrypointReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("apiReport")
      val apiReport = parse_microkitreportingRustApiReport()
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.RustReport(entrypointReport, apiReport)
    }

    def parse_microkitreportingRustApiReport(): org.sireum.hamr.codegen.microkit.reporting.RustApiReport = {
      val r = parse_microkitreportingRustApiReportT(F)
      return r
    }

    def parse_microkitreportingRustApiReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.RustApiReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.RustApiReport")
      }
      parser.parseObjectKey("extern_c_apiPath")
      val extern_c_apiPath = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("developerApiPath")
      val developerApiPath = parser.parseString()
      parser.parseObjectNext()
      parser.parseObjectKey("developerApiReport")
      val developerApiReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("testApiPath")
      val testApiPath = parser.parseString()
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.RustApiReport(extern_c_apiPath, developerApiPath, developerApiReport, testApiPath)
    }

    def parse_microkitreportingGumboReport(): org.sireum.hamr.codegen.microkit.reporting.GumboReport = {
      val r = parse_microkitreportingGumboReportT(F)
      return r
    }

    def parse_microkitreportingGumboReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.GumboReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.GumboReport")
      }
      parser.parseObjectKey("stateReport")
      val stateReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("methodsReport")
      val methodsReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("invariantsReport")
      val invariantsReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("integrationReport")
      val integrationReport = parser.parseOption(parse_microkitreportingGubmoIntegrationReport _)
      parser.parseObjectNext()
      parser.parseObjectKey("initializeReport")
      val initializeReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("computeReport")
      val computeReport = parser.parseOption(parse_microkitreportingGumboComputeReport _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.GumboReport(stateReport, methodsReport, invariantsReport, integrationReport, initializeReport, computeReport)
    }

    def parse_microkitreportingGubmoIntegrationReport(): org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport = {
      val r = parse_microkitreportingGubmoIntegrationReportT(F)
      return r
    }

    def parse_microkitreportingGubmoIntegrationReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport")
      }
      parser.parseObjectKey("assumesReport")
      val assumesReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("guaranteesReport")
      val guaranteesReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport(assumesReport, guaranteesReport)
    }

    def parse_microkitreportingGumboComputeReport(): org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport = {
      val r = parse_microkitreportingGumboComputeReportT(F)
      return r
    }

    def parse_microkitreportingGumboComputeReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport")
      }
      parser.parseObjectKey("assumesReport")
      val assumesReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("guaranteesReport")
      val guaranteesReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("casesReport")
      val casesReport = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      parser.parseObjectKey("handlers")
      val handlers = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport(assumesReport, guaranteesReport, casesReport, handlers)
    }

    def parse_microkitreportingGumboXReport(): org.sireum.hamr.codegen.microkit.reporting.GumboXReport = {
      val r = parse_microkitreportingGumboXReportT(F)
      return r
    }

    def parse_microkitreportingGumboXReportT(typeParsed: B): org.sireum.hamr.codegen.microkit.reporting.GumboXReport = {
      if (!typeParsed) {
        parser.parseObjectType("org.sireum.hamr.codegen.microkit.reporting.GumboXReport")
      }
      parser.parseObjectKey("gumboxMethods")
      val gumboxMethods = parser.parseHashSMap(parser.parseString _, parser.parsePosition _)
      parser.parseObjectNext()
      return org.sireum.hamr.codegen.microkit.reporting.GumboXReport(gumboxMethods)
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

  def fromCodegenReport(o: CodegenReport, isCompact: B): String = {
    val st = Printer.printCodegenReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toCodegenReport(s: String): Either[CodegenReport, Json.ErrorMsg] = {
    def fCodegenReport(parser: Parser): CodegenReport = {
      val r = parser.parseCodegenReport()
      return r
    }
    val r = to(s, fCodegenReport _)
    return r
  }

  def fromCodegenReports(o: CodegenReports, isCompact: B): String = {
    val st = Printer.printCodegenReports(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toCodegenReports(s: String): Either[CodegenReports, Json.ErrorMsg] = {
    def fCodegenReports(parser: Parser): CodegenReports = {
      val r = parser.parseCodegenReports()
      return r
    }
    val r = to(s, fCodegenReports _)
    return r
  }

  def fromResourceReport(o: ResourceReport, isCompact: B): String = {
    val st = Printer.printResourceReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toResourceReport(s: String): Either[ResourceReport, Json.ErrorMsg] = {
    def fResourceReport(parser: Parser): ResourceReport = {
      val r = parser.parseResourceReport()
      return r
    }
    val r = to(s, fResourceReport _)
    return r
  }

  def fromToolReport(o: ToolReport, isCompact: B): String = {
    val st = Printer.printToolReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def toToolReport(s: String): Either[ToolReport, Json.ErrorMsg] = {
    def fToolReport(parser: Parser): ToolReport = {
      val r = parser.parseToolReport()
      return r
    }
    val r = to(s, fToolReport _)
    return r
  }

  def from_microkitreportingIdPos(o: org.sireum.hamr.codegen.microkit.reporting.IdPos, isCompact: B): String = {
    val st = Printer.print_microkitreportingIdPos(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingIdPos(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.IdPos, Json.ErrorMsg] = {
    def f_microkitreportingIdPos(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.IdPos = {
      val r = parser.parse_microkitreportingIdPos()
      return r
    }
    val r = to(s, f_microkitreportingIdPos _)
    return r
  }

  def from_microkitreportingIdPathR(o: org.sireum.hamr.codegen.microkit.reporting.IdPathR, isCompact: B): String = {
    val st = Printer.print_microkitreportingIdPathR(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingIdPathR(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.IdPathR, Json.ErrorMsg] = {
    def f_microkitreportingIdPathR(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.IdPathR = {
      val r = parser.parse_microkitreportingIdPathR()
      return r
    }
    val r = to(s, f_microkitreportingIdPathR _)
    return r
  }

  def from_microkitreportingMicrokitReport(o: org.sireum.hamr.codegen.microkit.reporting.MicrokitReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingMicrokitReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingMicrokitReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.MicrokitReport, Json.ErrorMsg] = {
    def f_microkitreportingMicrokitReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.MicrokitReport = {
      val r = parser.parse_microkitreportingMicrokitReport()
      return r
    }
    val r = to(s, f_microkitreportingMicrokitReport _)
    return r
  }

  def from_microkitreportingPretty(o: org.sireum.hamr.codegen.microkit.reporting.Pretty, isCompact: B): String = {
    val st = Printer.print_microkitreportingPretty(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingPretty(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.Pretty, Json.ErrorMsg] = {
    def f_microkitreportingPretty(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.Pretty = {
      val r = parser.parse_microkitreportingPretty()
      return r
    }
    val r = to(s, f_microkitreportingPretty _)
    return r
  }

  def from_microkitreportingModelProperties(o: org.sireum.hamr.codegen.microkit.reporting.ModelProperties, isCompact: B): String = {
    val st = Printer.print_microkitreportingModelProperties(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingModelProperties(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.ModelProperties, Json.ErrorMsg] = {
    def f_microkitreportingModelProperties(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.ModelProperties = {
      val r = parser.parse_microkitreportingModelProperties()
      return r
    }
    val r = to(s, f_microkitreportingModelProperties _)
    return r
  }

  def from_microkitreportingSimpleProperty(o: org.sireum.hamr.codegen.microkit.reporting.SimpleProperty, isCompact: B): String = {
    val st = Printer.print_microkitreportingSimpleProperty(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingSimpleProperty(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.SimpleProperty, Json.ErrorMsg] = {
    def f_microkitreportingSimpleProperty(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.SimpleProperty = {
      val r = parser.parse_microkitreportingSimpleProperty()
      return r
    }
    val r = to(s, f_microkitreportingSimpleProperty _)
    return r
  }

  def from_microkitreportingComponentReport(o: org.sireum.hamr.codegen.microkit.reporting.ComponentReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingComponentReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingComponentReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.ComponentReport, Json.ErrorMsg] = {
    def f_microkitreportingComponentReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.ComponentReport = {
      val r = parser.parse_microkitreportingComponentReport()
      return r
    }
    val r = to(s, f_microkitreportingComponentReport _)
    return r
  }

  def from_microkitreportingPortLangRealization(o: org.sireum.hamr.codegen.microkit.reporting.PortLangRealization, isCompact: B): String = {
    val st = Printer.print_microkitreportingPortLangRealization(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingPortLangRealization(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.PortLangRealization, Json.ErrorMsg] = {
    def f_microkitreportingPortLangRealization(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.PortLangRealization = {
      val r = parser.parse_microkitreportingPortLangRealization()
      return r
    }
    val r = to(s, f_microkitreportingPortLangRealization _)
    return r
  }

  def from_microkitreportingPortLanguageArtifact(o: org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact, isCompact: B): String = {
    val st = Printer.print_microkitreportingPortLanguageArtifact(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingPortLanguageArtifact(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact, Json.ErrorMsg] = {
    def f_microkitreportingPortLanguageArtifact(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.PortLanguageArtifact = {
      val r = parser.parse_microkitreportingPortLanguageArtifact()
      return r
    }
    val r = to(s, f_microkitreportingPortLanguageArtifact _)
    return r
  }

  def from_microkitreportingPortReport(o: org.sireum.hamr.codegen.microkit.reporting.PortReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingPortReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingPortReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.PortReport, Json.ErrorMsg] = {
    def f_microkitreportingPortReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.PortReport = {
      val r = parser.parse_microkitreportingPortReport()
      return r
    }
    val r = to(s, f_microkitreportingPortReport _)
    return r
  }

  def from_microkitreportingCCodeReport(o: org.sireum.hamr.codegen.microkit.reporting.CCodeReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingCCodeReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingCCodeReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.CCodeReport, Json.ErrorMsg] = {
    def f_microkitreportingCCodeReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.CCodeReport = {
      val r = parser.parse_microkitreportingCCodeReport()
      return r
    }
    val r = to(s, f_microkitreportingCCodeReport _)
    return r
  }

  def from_microkitreportingRustReport(o: org.sireum.hamr.codegen.microkit.reporting.RustReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingRustReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingRustReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.RustReport, Json.ErrorMsg] = {
    def f_microkitreportingRustReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.RustReport = {
      val r = parser.parse_microkitreportingRustReport()
      return r
    }
    val r = to(s, f_microkitreportingRustReport _)
    return r
  }

  def from_microkitreportingRustApiReport(o: org.sireum.hamr.codegen.microkit.reporting.RustApiReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingRustApiReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingRustApiReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.RustApiReport, Json.ErrorMsg] = {
    def f_microkitreportingRustApiReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.RustApiReport = {
      val r = parser.parse_microkitreportingRustApiReport()
      return r
    }
    val r = to(s, f_microkitreportingRustApiReport _)
    return r
  }

  def from_microkitreportingGumboReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingGumboReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingGumboReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.GumboReport, Json.ErrorMsg] = {
    def f_microkitreportingGumboReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.GumboReport = {
      val r = parser.parse_microkitreportingGumboReport()
      return r
    }
    val r = to(s, f_microkitreportingGumboReport _)
    return r
  }

  def from_microkitreportingGubmoIntegrationReport(o: org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingGubmoIntegrationReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingGubmoIntegrationReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport, Json.ErrorMsg] = {
    def f_microkitreportingGubmoIntegrationReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.GubmoIntegrationReport = {
      val r = parser.parse_microkitreportingGubmoIntegrationReport()
      return r
    }
    val r = to(s, f_microkitreportingGubmoIntegrationReport _)
    return r
  }

  def from_microkitreportingGumboComputeReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingGumboComputeReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingGumboComputeReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport, Json.ErrorMsg] = {
    def f_microkitreportingGumboComputeReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.GumboComputeReport = {
      val r = parser.parse_microkitreportingGumboComputeReport()
      return r
    }
    val r = to(s, f_microkitreportingGumboComputeReport _)
    return r
  }

  def from_microkitreportingGumboXReport(o: org.sireum.hamr.codegen.microkit.reporting.GumboXReport, isCompact: B): String = {
    val st = Printer.print_microkitreportingGumboXReport(o)
    if (isCompact) {
      return st.renderCompact
    } else {
      return st.render
    }
  }

  def to_microkitreportingGumboXReport(s: String): Either[org.sireum.hamr.codegen.microkit.reporting.GumboXReport, Json.ErrorMsg] = {
    def f_microkitreportingGumboXReport(parser: Parser): org.sireum.hamr.codegen.microkit.reporting.GumboXReport = {
      val r = parser.parse_microkitreportingGumboXReport()
      return r
    }
    val r = to(s, f_microkitreportingGumboXReport _)
    return r
  }

}