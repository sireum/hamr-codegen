// #Sireum
package org.sireum.hamr.codegen.microkit.reporting

import org.sireum._
import org.sireum.U32._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.{EResource, IResource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.reporting.{CodegenReporting, CodegenReports, JSON, ResourceReport, Status, ToolReport}
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenResults, HamrCli}
import org.sireum.hamr.codegen.microkit.plugins.gumbo.GumboRustUtil
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.ir.{Aadl, Direction, GclAssume, GclGuarantee, GclNamedElement, GclSubclause}
import org.sireum.message.{Level, Position, Reporter}

@datatype class MicrokitReporterPlugin() extends Plugin {
  val name: String = "MicrokitReporterPlugin"

  @pure override def canFinalize(model: Aadl, aadlTypes: Option[AadlTypes], symbolTable: Option[SymbolTable], codegenResults: CodeGenResults, store: Store, options: HamrCli.CodegenOption, reporter: Reporter): B = {
    return (
      CodegenReporting.getCodegenReport(name, store).isEmpty &&
      !reporter.hasError &&
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      symbolTable.nonEmpty)
  }

  @pure override def finalizePlugin(model: Aadl,
                                    aadlTypes: Option[AadlTypes],
                                    symbolTable: Option[SymbolTable],
                                    codegenResults: CodeGenResults,
                                    store: Store,
                                    options: HamrCli.CodegenOption,
                                    reporter: Reporter): Store = {
    val st = symbolTable.get
    var localStore = store

    val workspaceRoot: Os.Path =
      options.workspaceRootDir match {
        case Some(wdir) =>
          val d = Os.path(wdir)
          assert(d.exists, d.value)
          d
        case _ =>
          halt(s"Couldn't determine workspace root dir")
      }

    val sel4OutputDir: Os.Path =
      options.sel4OutputDir match {
        case Some(d) => Os.path(d)
        case _ =>
          options.outputDir match {
            case Some(f) => Os.path(f) / "microkit"
            case _ => halt("Infeasible: no output directory was specified")
          }
      }

    {
      var toolReport: ToolReport = CodegenReporting.getCodegenReport(CodegenReporting.KEY_TOOL_REPORT, store).get.asInstanceOf[ToolReport]

      val cstatus: Status.Type =
        if (reporter.hasError) Status.Failure
        else Status.Success

      val warningMessages = reporter.messages.filter(m => m.level == Level.Warning)
      val errorMessages = reporter.messages.filter(m => m.level == Level.Error)

      val resources: ISZ[ResourceReport] =
        if (reporter.hasError) {
          ISZ()
        }
        else {
          // must have written resources out so sel4OutputDir must exist
          assert(sel4OutputDir.exists, sel4OutputDir.value)

          var r: ISZ[ResourceReport] = ISZ()
          for (resource <- codegenResults.resources) {
            resource match {
              case i: IResource =>
                r = r :+ ResourceReport(
                  path = sel4OutputDir.relativize(Os.path(i.dstPath)).value,
                  overwrittenIfExists = i.overwrite)
              case e: EResource =>
                halt("Not expecting $e")
              case e => halt("Not expecting $e")
            }
          }
          r
        }

      toolReport = toolReport(
        status = cstatus,
        warningMessages = warningMessages,
        errorMessages = errorMessages,
        resources = resources)

      localStore = CodegenReporting.addCodegenReport(CodegenReporting.KEY_TOOL_REPORT, toolReport, localStore)
    }

    if (reporter.hasError) {
      return localStore
    }



    assert(sel4OutputDir.exists, sel4OutputDir.value)



    var componentReports: HashSMap[IdPathR, ComponentReport] = HashSMap.empty

    for (t <- st.getThreads()) {

      var cCodeReport: Option[CCodeReport] = None()
      var rustReport: Option[RustReport] = None()
      var gumboReport: Option[GumboReport] = None()
      var gumboxReport: Option[GumboXReport] = None()

      val threadid = Util.getThreadIdPath(t)

      val inPorts = t.getPorts().filter(p => p.direction == Direction.In)
      val outPorts = t.getPorts().filter(p => p.direction == Direction.Out)

      if (Util.isRusty(t)) {
        assert(t.isPeriodic())

        val rustBridgeDir = sel4OutputDir / "crates" / threadid / "src" / "bridge"
        assert(rustBridgeDir.exists, rustBridgeDir.value)

        val rustComponentDir = sel4OutputDir / "crates" / threadid / "src" / "component"
        assert(rustComponentDir.exists, rustComponentDir.value)

        val externApiFile = rustBridgeDir / "extern_c_api.rs"
        assert(externApiFile.exists, externApiFile.value)

        val testApiFile = rustBridgeDir / "test_api.rs"
        assert(testApiFile.exists, testApiFile.value)

        val rustComponentApiFile = rustBridgeDir / s"${threadid}_api.rs"
        assert(rustComponentApiFile.exists, rustComponentApiFile.value)

        val rustComponentApiContent = StringUtil.split_PreserveEmptySegments(rustComponentApiFile.read, c => c == '\n')

        var developerApiReport: HashSMap[String, Position] = HashSMap.empty

        // impl<API: operator_interface_oip_oit_Get_Api> operator_interface_oip_oit_Application_Api<API> {
        val getApiLine = ReportUtil.findLineNumber(s"impl<API: ${threadid}_Get_Api>", 0, rustComponentApiContent)

        // impl<API: operator_interface_oip_oit_Put_Api> operator_interface_oip_oit_Application_Api<API> {
        val putApiLine = ReportUtil.findLineNumber(s"impl<API: ${threadid}_Put_Api>", 0, rustComponentApiContent)

        { // process api artifacts

          if (inPorts.nonEmpty) {
            assert(getApiLine != -1, rustComponentApiFile.toUri)
            for (in <- inPorts) {
              val inLine = ReportUtil.findLineNumber(s"pub fn get_${in.identifier}", getApiLine, rustComponentApiContent)
              assert(inLine != -1, s"$getApiLine 'pub fn get_${in.identifier}' ${rustComponentApiFile.toUri}")
              ReportUtil.scanForClosingBrace(inLine, rustComponentApiContent) match {
                case (-1, _) =>
                  reporter.warn(None(), name, s"Coulnd't find put api for ${in.identifier} in $rustComponentApiFile")
                case (end, _) =>
                  developerApiReport = developerApiReport + in.identifier ~>
                    ReportUtil.buildPos(inLine, end, rustComponentApiFile, workspaceRoot, sel4OutputDir)
              }
            }
          }

          if (outPorts.nonEmpty) {
            assert(putApiLine != -1)
            for (out <- outPorts) {
              val outLine = ReportUtil.findLineNumber(s"pub fn put_${out.identifier}", putApiLine, rustComponentApiContent)
              assert(outLine != -1, s"$getApiLine 'pub fn put_${out.identifier}' ${rustComponentApiFile.toUri}")
              ReportUtil.scanForClosingBrace(outLine, rustComponentApiContent) match {
                case (-1, _) =>
                  reporter.warn(None(), name, s"Coulnd't find get api for ${out.identifier} in $rustComponentApiFile")
                case (end, _) =>
                  developerApiReport = developerApiReport + out.identifier ~>
                    ReportUtil.buildPos(outLine, end, rustComponentApiFile, workspaceRoot, sel4OutputDir)
              }
            }
          }
        }


        val rustComponentAppFile = rustComponentDir / s"${threadid}_app.rs"
        assert(rustComponentAppFile.exists, rustComponentAppFile.value)

        var structLoc: Option[Position] = None()
        var implLoc: Option[Position] = None()

        var methodLocs: HashSMap[String, Position] = HashSMap.empty
        var gumboMethodLocs: HashSMap[String, Position] = HashSMap.empty
        var markerLocs: HashSMap[String, (Z, Z)] = HashSMap.empty


        @pure def collectFromMarkedRegion(marker: String, prefix: String, singleLine: B, elems: ISZ[GclNamedElement], content: ISZ[String], source: Os.Path): HashSMap[String, Position] = {
          var ret = HashSMap.empty[String, Position]
          markerLocs.get(marker) match {
            case Some((start, end)) =>
              val region = ops.ISZOps(content).slice(start + 1, end)
              for (e <- elems) {
                ReportUtil.locateText(s"$prefix ${e.id}", singleLine, region) match {
                  case (-1, -1) =>
                    reporter.warn(None(), name, s"Couldn't locate $prefix ${e.id} in $source")
                  case (rstart, rend) =>
                    ret = ret + e.id ~> ReportUtil.buildPos(start + 1 + rstart, start + 1 + rend + 1, source, workspaceRoot, sel4OutputDir)
                }
              }
            case _ => reporter.warn(None(), name, s"Marker block '$marker' not found in $source")
          }
          return ret
        }

        val appContent = StringUtil.split_PreserveEmptySegments(rustComponentAppFile.read, c => c == '\n')

        { // process gumboX

          val gumboxFile = rustBridgeDir / s"${threadid}_GUMBOX.rs"
          if (gumboxFile.exists) {

            val gumboxContent = StringUtil.split_PreserveEmptySegments(gumboxFile.read, c => c == '\n')
            var i: Z = 0
            while (i < gumboxContent.size) {
              val lineo = ops.StringOps(gumboxContent(i))
              val trimmed = ops.StringOps(ops.StringOps(lineo.s).trim)

              if (trimmed.startsWith("pub fn")) {
                ReportUtil.getName("fn", trimmed) match {
                  case Some(name) =>
                    val endOfBlock = ReportUtil.scanForClosingBrace(i, gumboxContent)
                    gumboMethodLocs = gumboMethodLocs + name ~> ReportUtil.buildPos(i, endOfBlock._1, gumboxFile, workspaceRoot, sel4OutputDir)
                    i = endOfBlock._1
                  case _ => halt("")
                }
              }
              i = i + 1
            }

            if (gumboMethodLocs.nonEmpty) {
              gumboxReport = Some(GumboXReport(gumboMethodLocs))
            }
          }
        }

        { // process app contents
          var i: Z = 0
          while (i < appContent.size) {
            val line = appContent(i)
            val lineo = ops.StringOps(line)
            val trimmed = lineo.trim
            val trimmedo = ops.StringOps(trimmed)

            if (trimmedo.startsWith(s"pub struct $threadid")) {
              val endOfBlock = ReportUtil.scanForClosingBrace(i, appContent)
              markerLocs = markerLocs ++ endOfBlock._2.entries

              structLoc = Some(ReportUtil.buildPos(i, endOfBlock._1, rustComponentAppFile, workspaceRoot, sel4OutputDir))
              i = endOfBlock._1
            }

            if (trimmedo.startsWith(s"impl $threadid")) {
              //val endOfBlock = scanForClosingBrace()
              implLoc = Some(ReportUtil.buildPos(i, i, rustComponentAppFile, workspaceRoot, sel4OutputDir))
            }

            if (trimmedo.startsWith(s"pub fn")) {
              ReportUtil.getName("fn", trimmedo) match {
                case Some(name) =>
                  val endOfBlock = ReportUtil.scanForClosingBrace(i, appContent)
                  markerLocs = markerLocs ++ endOfBlock._2.entries
                  methodLocs = methodLocs + name ~> ReportUtil.buildPos(i, endOfBlock._1, rustComponentAppFile, workspaceRoot, sel4OutputDir)
                  i = endOfBlock._1
                case _ => halt("")
              }
            }

            if (trimmedo.startsWith(s"pub open spec fn")) {
              ReportUtil.getName("fn", trimmedo) match {
                case Some(name) =>
                  val endOfBlock = ReportUtil.scanForClosingBrace(i, appContent)
                  markerLocs = markerLocs ++ endOfBlock._2.entries
                  gumboMethodLocs = gumboMethodLocs + name ~> ReportUtil.buildPos(i, endOfBlock._1, rustComponentAppFile, workspaceRoot, sel4OutputDir)
                  i = endOfBlock._1
                case _ => halt("")
              }

            }

            i = i + 1
          }
        }

        if (ops.ISZOps(t.annexes()).exists(p => p.clause.isInstanceOf[GclSubclause])) {
          // process gumbo artifacts

          val subclauseInfo = GumboRustUtil.getGumboSubclause(t.path, st)

          var stateReport = HashSMap.empty[String, Position]
          var gumboMethodsReport = HashSMap.empty[String, Position]
          var invariantReport = HashSMap.empty[String, Position]
          var integrationReport: Option[GubmoIntegrationReport] = None()
          var initializeReport = HashSMap.empty[String, Position]
          var computeReport: Option[GumboComputeReport] = None()

          if (subclauseInfo.annex.state.nonEmpty) {
            stateReport = collectFromMarkedRegion("STATE VARS", "pub", T, subclauseInfo.annex.state.asInstanceOf[ISZ[GclNamedElement]], appContent, rustComponentAppFile)
          }

          if (subclauseInfo.annex.initializes.nonEmpty) {
            initializeReport = collectFromMarkedRegion("INITIALIZATION ENSURES", "// guarantee", F, subclauseInfo.annex.initializes.get.guarantees.asInstanceOf[ISZ[GclNamedElement]], appContent, rustComponentAppFile)
          }

          if (subclauseInfo.annex.compute.nonEmpty) {
            var assumesReport = HashSMap.empty[String, Position]
            var guaranteesReport = HashSMap.empty[String, Position]
            var casesReport = HashSMap.empty[String, Position]
            var handlers = HashSMap.empty[String, Position]

            markerLocs.get("TIME TRIGGERED REQUIRES") match {
              case Some((start, end)) =>
                assumesReport = collectFromMarkedRegion("TIME TRIGGERED REQUIRES", "// assume", F, subclauseInfo.annex.compute.get.assumes.asInstanceOf[ISZ[GclNamedElement]], appContent, rustComponentAppFile)
              case _ =>
            }

            markerLocs.get("TIME TRIGGERED ENSURES") match {
              case Some((start, end)) =>
                // guarantee lastCmd
                if (subclauseInfo.annex.compute.get.guarantees.nonEmpty) {
                  guaranteesReport = collectFromMarkedRegion("TIME TRIGGERED ENSURES", "// guarantee", F, subclauseInfo.annex.compute.get.guarantees.asInstanceOf[ISZ[GclNamedElement]], appContent, rustComponentAppFile)
                }

                // case REQ_MHS_1
                if (subclauseInfo.annex.compute.get.cases.nonEmpty) {
                  casesReport = collectFromMarkedRegion("TIME TRIGGERED ENSURES", "// case", F, subclauseInfo.annex.compute.get.cases.asInstanceOf[ISZ[GclNamedElement]], appContent, rustComponentAppFile)
                }

                for (h <- subclauseInfo.annex.compute.get.handlers) {
                  halt("Need to handle sporadic compute handlers")
                }

              case _ => reporter.warn(None(), name, s"Marker block for GUMBO compute clauses was not found in $rustComponentAppFile")
            }

            computeReport = Some(GumboComputeReport(
              assumesReport = assumesReport,
              guaranteesReport = guaranteesReport,
              casesReport = casesReport,
              handlers = handlers))
          }

          for (m <- subclauseInfo.annex.methods) {
            gumboMethodLocs.get(m.method.sig.id.value) match {
              case Some(pos) =>
                gumboMethodsReport = gumboMethodsReport + m.method.sig.id.value ~> pos
              case _ =>
                reporter.warn(None(), name, s"Did not find GUMBO function ${m.method.sig.id} in $rustComponentAppFile")
            }
          }

          for (i <- subclauseInfo.annex.invariants) {
            halt("Need to handle type invariants")
          }

          if (subclauseInfo.annex.integration.nonEmpty) {
            var assumes = HashSMap.empty[String, Position]
            var guars =  HashSMap.empty[String, Position]
            for (p <- t.getPorts()) {
              subclauseInfo.gclSymbolTable.integrationMap.get(p) match {
                case Some(assu: GclAssume) =>
                  ReportUtil.findLineNumber(s"// assume ${assu.id}", putApiLine, rustComponentApiContent) match {
                    case -1 =>
                      reporter.warn(None(), name, s"Didn't find assume clause for integration constraint ${assu.id} in $rustComponentApiFile")
                    case beginLine =>
                      assumes = assumes + assu.id ~> ReportUtil.buildPos(
                        beginLine = beginLine, endLine = beginLine + 1, file = rustComponentApiFile, workspaceRoot, sel4OutputDir)
                  }
                case Some(guar: GclGuarantee) =>
                  ReportUtil.findLineNumber(s"// guarantee ${guar.id}", putApiLine, rustComponentApiContent) match {
                    case -1 =>
                      reporter.warn(None(), name, s"Didn't find guarantee clause for integration constraint ${guar.id} in $rustComponentApiFile")
                    case beginLine =>
                      guars = guars + guar.id ~> ReportUtil.buildPos(
                        beginLine = beginLine, endLine = beginLine + 1, file = rustComponentApiFile, workspaceRoot, sel4OutputDir)
                  }

                case Some(x) => halt(s"Unexpected: $x")
                case _ =>
              }
            }
            integrationReport = Some(GubmoIntegrationReport(assumes, guars))
          }

          gumboReport = Some(GumboReport(
            stateReport = stateReport,
            methodsReport = gumboMethodsReport,
            invariantsReport = invariantReport,
            integrationReport = integrationReport,
            initializeReport = initializeReport,
            computeReport = computeReport))
        }

        val entrypointReport = HashSMap.empty[String, Position] ++
          methodLocs.entries.filter(p => p._1 == "initialize" || p._1 == "timeTriggered" || ops.StringOps(p._1).startsWith("handle"))

        rustReport = Some(RustReport(
          entrypointReport = entrypointReport,
          apiReport = RustApiReport(
            extern_c_apiPath = sel4OutputDir.relativize(externApiFile).value,
            developerApiPath = sel4OutputDir.relativize(rustComponentApiFile).value,
            developerApiReport = developerApiReport,
            testApiPath = sel4OutputDir.relativize(testApiFile).value)))

      } // end rust handling

      componentReports = componentReports + IdPathR(t.path) ~> ComponentReport(
        cCodeReport = cCodeReport,
        rustReport = rustReport,
        gumboReport = gumboReport,
        gumboXReport = gumboxReport)

    } // end for loop processing threads

    val systemDescription = sel4OutputDir / "microkit.system"
    assert(systemDescription.exists)

    val report = MicrokitReport(
      systemDescriptionUri = sel4OutputDir.relativize(systemDescription).value,
      componentReport = componentReports)

    genReadme(
      report,
      workspaceRoot,
      sel4OutputDir,
      model,
      aadlTypes,
      st,
      codegenResults,
      store,
      options,
      reporter)

    localStore = CodegenReporting.addCodegenReport(name, report, localStore)

    val codegenReports = CodegenReports(CodegenReporting.getCodegenReports(localStore).get)
    val json = JSON.fromCodegenReports(codegenReports, F)
    val outputDir = sel4OutputDir / "reporting"
    outputDir.mkdirAll()

    val outjson = outputDir / "codegen_report.json"
    outjson.writeOver(json)

    return localStore
  }

  @pure def genReadme(microkitReport: MicrokitReport,
                      workspaceRoot: Os.Path,
                      sel4OutputDir: Os.Path,
                      model: Aadl, aadlTypes: Option[AadlTypes], symbolTable: SymbolTable,
                      codegenResults: CodeGenResults, store: Store,
                      options: HamrCli.CodegenOption, reporter: Reporter): Unit = {

    val systemName = symbolTable.rootSystem.classifierAsString

    val archDescOpt: Option[ST]= None()

    var componentAadlReports: ISZ[ST] = ISZ()
    componentAadlReports = componentAadlReports :+
      st"""|System: [$systemName]()|
          ||:--|"""

    val packageContent = ReportUtil.parseAadl(workspaceRoot, sel4OutputDir)

    var behaviorCodeReports: ISZ[ST] = ISZ()

    for (t <- symbolTable.getThreads()) {
      val classifier = t.classifier
      assert (classifier.size == 2)

      val packageName = classifier(0)
      val componentName = ops.StringOps(classifier(classifier.lastIndex))

      val packageReport = packageContent.get(packageName).get
      val componentReport = packageReport.components.get(componentName.s).get

      var gumboOpt: Option[ST] =
        componentReport.annexSubclauses.get("GUMBO") match {
          case (Some(r)) => Some(
            st"""<br>
                |GUMBO: [Subclause](${ReportUtil.createLink(r, sel4OutputDir)})""")
          case _ => None()
        }

      val c: ST =
        if (componentName.contains(".")) {
          val typname = componentName.substring(0, componentName.lastIndexOf('.'))
          val typeReport = packageReport.components.get(typname).get
          typeReport.annexSubclauses.get("GUMBO") match {
            case Some(r) =>
              assert(gumboOpt.isEmpty)
              gumboOpt = Some(
                st"""<br>GUMBO: [Subclause](${ReportUtil.createLink(r, sel4OutputDir)})""")
            case _ =>
          }
          var s = st"Type: [$typname](${ReportUtil.createLink(typeReport.pos, sel4OutputDir)})<br>"
          s = st"${s}Implementation: [${componentName.s}](${ReportUtil.createLink(componentReport.pos, sel4OutputDir)})"
          s
        } else {
          st"Implementation: [${componentName.s}](${ReportUtil.createLink(componentReport.pos, sel4OutputDir)})"
        }

      val properties: String = {
        val typ: String = if (t.isPeriodic()) "Periodic " else "Sporadic "
        if (t.getMaxComputeExecutionTime() > 0)
          s"$typ: ${t.getMaxComputeExecutionTime()} ms"
        else typ
      }

      componentAadlReports = componentAadlReports :+
        st"""|Thread: $packageName::${componentName.s} |
            ||:--|
            ||$c$gumboOpt|
            ||$properties|"""
    }


    /************************************************************
    * Now process codegen artifacts
    ************************************************************/

    for (c <- microkitReport.componentReport.entries) {
      val idPath = c._1.idPath
      val id = idPath(idPath.lastIndex)

      val component = symbolTable.componentMap.get(idPath).get

      var bh =
        st"""#### $id: ${component.classifierAsString}
            |
            | - **Entry Points**
            |"""

      c._2.rustReport match {
        case Some(rust) =>
          for(entrypoint <- rust.entrypointReport.entries) {
            bh =
              st"""$bh
                  |    ${ops.StringOps(entrypoint._1).firstToUpper}: [Rust](${ReportUtil.createLink(entrypoint._2, sel4OutputDir)})
                  |"""
          }

          val apiReport = ReportUtil.generateApiReport(rust.apiReport, component.asInstanceOf[AadlThread], workspaceRoot, sel4OutputDir)
          bh =
            st"""$bh
                |
                | - **APIs**
                |
                |     $apiReport
                |"""
        case _ =>
      }



      val gumboReportOpt: Option[ST] =
      if (c._2.gumboReport.nonEmpty) {
        val gumboOpt = GumboRustUtil.getGumboSubclauseOpt(idPath, symbolTable)
        Some(ReportUtil.generateGumboReport(c._2.gumboReport.get, c._2.gumboXReport.get, component.asInstanceOf[AadlThread], gumboOpt, workspaceRoot, sel4OutputDir))
      } else {
        None()
      }

      behaviorCodeReports = behaviorCodeReports :+ st"""$bh
                                                       |$gumboReportOpt"""
    }

    val diagramOpt: Option[ST] = {
      ReportUtil.getArchDiagram(workspaceRoot) match {
        case Some(f) => Some(st"![${f.name}](${sel4OutputDir.relativize(f)})")
        case _ => None()
      }
    }

    val readmeContent =
      st"""# $systemName
          |
          |## AADL Architecture
          |$diagramOpt
          |$archDescOpt
          |${(componentAadlReports, "\n\n")}
          |
          |
          |## Rust Code
          |
          |[Microkit System Description](${microkitReport.systemDescriptionUri})
          |
          |### Behavior Code
          |${(behaviorCodeReports, "\n\n")}
          |"""

    val readme = sel4OutputDir / "codegen_readme.md"

    readme.writeOver(readmeContent.render)
  }
}
