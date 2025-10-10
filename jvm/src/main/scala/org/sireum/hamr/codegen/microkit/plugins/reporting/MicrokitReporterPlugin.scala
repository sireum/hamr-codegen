// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.U32._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{EResource, InternalResource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.reporting.{CodegenReporting, CodegenReports, JSON, ResourceReport, Status, ToolReport}
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenResults, HamrCli}
import org.sireum.hamr.codegen.microkit.plugins.gumbo.GumboRustUtil
import org.sireum.hamr.codegen.microkit.plugins.reporting.RustContainers.RustFn
import org.sireum.hamr.codegen.microkit.reporting._
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.ir.{Aadl, Direction, GclAssume, GclGuarantee, GclSubclause}
import org.sireum.message.{Level, Position, Reporter}

@datatype class MicrokitReporterPlugin() extends Plugin {
  val name: String = "MicrokitReporterPlugin"

  val KEY_MICROKIT_REPORTER_PLUGIN: String = "KEY_MICROKIT_REPORTER_PLUGIN"

  @strictpure def hasFinalized(store: Store): B = store.contains(KEY_MICROKIT_REPORTER_PLUGIN)

  @pure override def canFinalize(model: Aadl, aadlTypes: Option[AadlTypes], symbolTable: Option[SymbolTable], codegenResults: CodeGenResults, store: Store, options: HamrCli.CodegenOption, reporter: Reporter): B = {
    return (
      !hasFinalized(store) &&
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
    var localStore = store + KEY_MICROKIT_REPORTER_PLUGIN ~> CommonUtil.BoolValue(T)

    val sel4OutputDir: Os.Path =
      options.sel4OutputDir match {
        case Some(d) => Os.path(d)
        case _ =>
          options.outputDir match {
            case Some(f) => Os.path(f) / "microkit"
            case _ => halt("Infeasible: no output directory was specified")
          }
      }

    assert(sel4OutputDir.exists, sel4OutputDir.value)

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
              case i: InternalResource =>
                r = r :+ ResourceReport(
                  path = ReportUtil.deWin(sel4OutputDir.relativize(Os.path(i.dstPath)).value),
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

    val systemDescription = sel4OutputDir / "microkit.system"
    assert(systemDescription.exists, systemDescription.value)

    val msdOpt = MSDParser.parse(systemDescription, sel4OutputDir)

    if (msdOpt.isEmpty) {
      println(s"Was not able to parse $systemDescription. No report generated")
      return localStore
    }

    val msd = msdOpt.get

    val workspaceRoot: Os.Path =
      options.workspaceRootDir match {
        case Some(wdir) =>
          val d = Os.path(wdir)
          assert(d.exists, d.value)
          d
        case _ =>
          println("Model workspace option was not provided. Cannot generate Microkit codegen report")

          return localStore
      }

    var componentReports: HashSMap[IdPathR, ComponentReport] = HashSMap.empty

    for (t <- st.getThreads()) {

      var ports: HashSMap[IdPathR, PortReport] = HashSMap.empty

      var cCodeReport: Option[CCodeReport] = None()
      var rustReport: Option[RustReport] = None()
      var gumboReport: Option[GumboReport] = None()
      var gumboxReport: Option[GumboXReport] = None()

      val threadid = MicrokitUtil.getThreadIdPath(t)

      val protectionDomain = msd.getProtectionDomain(threadid).get.protection_domains(0)

      val inPorts = t.getPorts().filter(p => p.direction == Direction.In)
      val outPorts = t.getPorts().filter(p => p.direction == Direction.Out)

      val cComponentDir = sel4OutputDir / "components" / threadid / "src"
      assert(cComponentDir.exists, cComponentDir.value)

      val cBridgeFile = cComponentDir / s"$threadid.c"
      assert (cBridgeFile.exists, cBridgeFile.value)

      val cFile = CParser.parse(cBridgeFile, sel4OutputDir)

      @pure def addPort(p: AadlPort): Unit = {
        val (kind, payload, queueSize): (PortKind.Type, Option[String], Z) = p match {
          case a: AadlDataPort => (PortKind.Data, Some(a.aadlType.name), 1)
          case a: AadlEventDataPort => (PortKind.EventData, Some(a.aadlType.name), a.queueSize)
          case a: AadlEventPort => (PortKind.Event, None(), a.queueSize)
          case x => halt(s"Unexpected: $x")
        }

        var realizations: ISZ[PortLangRealization] = ISZ()

        val direction: PortDirection.Type = if (p.direction == Direction.In) PortDirection.In else PortDirection.Out

        val key = s"${p.identifier}_queue_$queueSize"

        val methodName = s"${if (p.direction == Direction.In) "get" else "put"}_${p.identifier}"
        val cMethod = cFile.getMethgod(methodName)
        realizations = realizations :+ PortLanguageArtifact(
          name = "C Interface",
          title = "C Interface",
          pos = cMethod.pos)


        val sharedMemVar = cFile.getField(key)
        realizations = realizations :+ PortLanguageArtifact(
          name = "C var_addr",
          title = "C Shared Memory Variable",
          pos = sharedMemVar.pos)

        protectionDomain.maps.filter(m => ops.StringOps(m.setvar_vaddr.get).startsWith(key)) match {
          case ISZ(e) =>
            realizations = realizations :+ PortLanguageArtifact(
              name = "Memory Map",
              title = "Memory Map",
              pos = e.pos)
          case x => halt(s"Didn't find a unique map for ${key}: Found ${x.size} in $x")
        }

        ports = ports + IdPathR(p.path) ~> PortReport(
          name = p.path,
          kind = kind,
          direction = direction,
          payload = payload,
          modelPos = ReportUtil.buildPosA(p.feature.identifier.pos.get, workspaceRoot, sel4OutputDir),
          languageRealizations = realizations)
      }

      for (i <- inPorts) {
        addPort(i)
      }

      for (o <- outPorts) {
        addPort(o)
      }

      if (MicrokitUtil.isRusty(t)) {
        assert(t.isPeriodic())

        val rustBridgeDir = sel4OutputDir / "crates" / threadid / "src" / "bridge"
        assert(rustBridgeDir.exists, rustBridgeDir.value)

        val rustComponentDir = sel4OutputDir / "crates" / threadid / "src" / "component"
        assert(rustComponentDir.exists, rustComponentDir.value)

        val externApiFile = rustBridgeDir / "extern_c_api.rs"
        assert(externApiFile.exists, externApiFile.value)
        val parsedExternApiFile = RustParserSimple.parse(externApiFile, sel4OutputDir)

        val rustTestDir = sel4OutputDir / "crates" / threadid / "src" / "test"
        val rustTestUtilDir = rustTestDir / "util"

        val testApiFile = rustTestUtilDir / "test_apis.rs"
        assert(testApiFile.exists, testApiFile.value)

        val rustComponentApiFile = rustBridgeDir / s"${threadid}_api.rs"
        assert(rustComponentApiFile.exists, rustComponentApiFile.value)
        val parsedRustComponentApiFile = RustParserSimple.parse(rustComponentApiFile, sel4OutputDir)

        val rustComponentAppFile = rustComponentDir / s"${threadid}_app.rs"
        assert(rustComponentAppFile.exists, rustComponentAppFile.value)
        val parsedRustComponentAppFile = RustParserSimple.parse(rustComponentAppFile, sel4OutputDir)

        val componentAppStruct = parsedRustComponentAppFile.structs.get(threadid).get
        val componentAppImpl = parsedRustComponentAppFile.getImpl(threadid).get

        val gumboxFile = rustBridgeDir / s"${threadid}_GUMBOX.rs"
        val parsedGumboXFile: Option[RustContainers.RustFile] =
          if (gumboxFile.exists) Some(RustParserSimple.parse(gumboxFile, sel4OutputDir))
          else None()

        var developerApiReport: HashSMap[String, Position] = HashSMap.empty


        val getApiImpl = parsedRustComponentApiFile.getImplH(
          Some(RustContainers.GenericParam("API", s"${threadid}_Get_Api")), s"${threadid}_Application_Api")
        val putApiImpl = parsedRustComponentApiFile.getImplH(
          Some(RustContainers.GenericParam("API", s"${threadid}_Put_Api")), s"${threadid}_Application_Api")

        { // process api artifacts
          @pure def addPath(pports: ISZ[AadlPort], rustImpl: RustContainers.RustImpl, rustTrait: RustContainers.RustTrait): Unit = {
            for (p <- pports) {
              val typ: String = if (p.direction == Direction.In) "get" else "put"

              val fn1 = rustImpl.methods.get(s"${typ}_${p.identifier}").get
              var langRs = ISZ[PortLangRealization]() :+ PortLanguageArtifact(
                name = "Rust/Verus API",
                title = "Rust/Verus API",
                pos = fn1.pos)

              val fn2 = rustTrait.methods.get(s"unverified_${typ}_${p.identifier}").get
              langRs = langRs :+ PortLanguageArtifact(
                name = "Unverified Rust Interface",
                title = "Unverified Rust Interface",
                pos = fn2.pos)

              val fn3 = parsedExternApiFile.functions.get(s"unsafe_${typ}_${p.identifier}").get
              langRs = langRs :+ PortLanguageArtifact(
                name = "Rust/C Interface",
                title = "Rust/C Interface",
                pos = fn3.pos)

              assert (parsedExternApiFile.externs.size == 1)
              val fn4 = parsedExternApiFile.externs()(0).methods.get(s"${typ}_${p.identifier}").get
              langRs = langRs :+ PortLanguageArtifact(
                name = "C Extern",
                title = "C Extern",
                pos = fn4.pos)

              val e: PortReport = ports.get(IdPathR(p.path)).get

              ports = ports + IdPathR(p.path) ~> e(languageRealizations = langRs ++ e.languageRealizations)
            }
          }

          if (inPorts.nonEmpty) {
            val getTrait = parsedRustComponentApiFile.traits.get(s"${threadid}_Get_Api").get
            addPath(inPorts, getApiImpl.get, getTrait)
          }

          if (outPorts.nonEmpty) {
            val putTrait = parsedRustComponentApiFile.traits.get(s"${threadid}_Put_Api").get
            addPath(outPorts, putApiImpl.get, putTrait)
          }
        }

        parsedGumboXFile match {
          case Some(g) =>
            gumboxReport = Some(GumboXReport(HashSMap.empty[String, Position] ++ (for(fn <- g.functions.values) yield fn.name ~> fn.pos)))
          case _ =>
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

          for (s <- subclauseInfo.annex.state) {
            stateReport = stateReport + s.name ~> componentAppStruct.fields.get(s.name).get.pos
          }

          if (subclauseInfo.annex.initializes.nonEmpty) {
            val init = componentAppImpl.methods.get("initialize").get
            for (guar <- init.ensures.get.gumboClauses) {
              initializeReport = initializeReport + guar.id ~> guar.pos
            }
          }

          if (subclauseInfo.annex.compute.nonEmpty) {
            val timeTriggered = componentAppImpl.methods.get("timeTriggered").get
            val computeClause = subclauseInfo.annex.compute.get

            var assumesReport = HashSMap.empty[String, Position]
            var guaranteesReport = HashSMap.empty[String, Position]
            var casesReport = HashSMap.empty[String, Position]
            var handlers = HashSMap.empty[String, Position]

            for (assumes <- computeClause.assumes) {
              assumesReport = assumesReport + assumes.id ~> timeTriggered.getAssumeClausePos(assumes.id)
            }

            for (guar <- computeClause.guarantees) {
              guaranteesReport = guaranteesReport + guar.id ~> timeTriggered.getGuaranteeClausePos(guar.id)
            }

            for (case_ <- computeClause.cases) {
              casesReport = casesReport + case_.id ~> timeTriggered.getGuaranteeClausePos(case_.id)
            }

            for (h <- computeClause.handlers) {
              halt("Need to handle sporadic compute handlers")
            }

            computeReport = Some(GumboComputeReport(
              assumesReport = assumesReport,
              guaranteesReport = guaranteesReport,
              casesReport = casesReport,
              handlers = handlers))
          }

          for (m <- subclauseInfo.annex.methods) {
            val id = m.method.sig.id.value
            gumboMethodsReport = gumboMethodsReport + id ~> componentAppImpl.methods.get(id).get.pos
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
                  val fn = getApiImpl.get.methods.get(s"get_${p.identifier}").get
                  val pos = fn.getGuaranteeClausePos(assu.id)
                  assumes = assumes + assu.id ~> pos

                case Some(guar: GclGuarantee) =>
                  val fn = putApiImpl.get.methods.get(s"put_${p.identifier}").get
                  val pos = fn.getAssumeClausePos(guar.id)
                  guars = guars + guar.id ~> pos
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
        } // end gumbo processing


        val entrypointReport = HashSMap.empty[String, Position] ++
          (for(e <- componentAppImpl.methods.entries.filter(p => p._1 == "initialize" || p._1 == "timeTriggered" ||
            ops.StringOps(p._1).startsWith("handle"))) yield e._2.name ~> e._2.pos)

        rustReport = Some(RustReport(
          entrypointReport = entrypointReport,
          apiReport = RustApiReport(
            extern_c_apiPath = ReportUtil.deWin(sel4OutputDir.relativize(externApiFile).value),
            developerApiPath = ReportUtil.deWin(sel4OutputDir.relativize(rustComponentApiFile).value),
            developerApiReport = developerApiReport,
            testApiPath = ReportUtil.deWin(sel4OutputDir.relativize(testApiFile).value))))

      } // end rust handling

      val subcomponents: ISZ[IdPathR] = for(s <- t.subComponents) yield IdPathR(s.path)

      val modelImplementation = IdPos(id = t.classifierAsString, pos = t.component.identifier.pos.get)

      componentReports = componentReports + IdPathR(t.path) ~>
        ComponentReport(
          path = t.path,
          kind = ComponentKind.Thread,
          optModelType = None(),
          modelImplementation = modelImplementation,
          modelProperties = ISZ(),
          subcomponents = subcomponents,
          ports = ports,
          cCodeReport = cCodeReport,
          rustReport = rustReport,
          gumboReport = gumboReport,
          gumboXReport = gumboxReport)

    } // end for loop processing threads

    val report = MicrokitReport(componentReports)

    val isAadl = ops.StringOps(st.rootSystem.component.identifier.pos.get.uriOpt.get).endsWith(".aadl")

    if (isAadl) {
      val readmeContent = genReadme(
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

      val readme = sel4OutputDir / s"codegen_readme_${if(isAadl) "aadl" else "sysml"}.md"
      ReportUtil.writeOutResource(readmeContent, readme, F, options.verbose)

    } else {
      println("TODO: generate codegen readme for SysMLv2 models")
    }

    localStore = CodegenReporting.addCodegenReport(name, report, localStore)

    val codegenReports = CodegenReports(CodegenReporting.getCodegenReports(localStore).get)
    val json = JSON.fromCodegenReports(codegenReports, F)
    val outputDir = sel4OutputDir / "reporting"
    outputDir.mkdirAll()

    val outjson = outputDir / s"codegen_report_${if(isAadl) "aadl" else "sysml"}.json"
    ReportUtil.writeOutResourceH(json, outjson, F, options.verbose)

    return localStore
  }

  @pure def genReadme(microkitReport: MicrokitReport,
                      workspaceRoot: Os.Path,
                      sel4OutputDir: Os.Path,
                      model: Aadl, aadlTypes: Option[AadlTypes], symbolTable: SymbolTable,
                      codegenResults: CodeGenResults, store: Store,
                      options: HamrCli.CodegenOption, reporter: Reporter): ST = {

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
                |GUMBO: [Subclause](${ReportUtil.createLink(r)})""")
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
                st"""<br>GUMBO: [Subclause](${ReportUtil.createLink(r)})""")
            case _ =>
          }
          var s = st"Type: [$typname](${ReportUtil.createLink(typeReport.pos)})<br>"
          s = st"${s}Implementation: [${componentName.s}](${ReportUtil.createLink(componentReport.pos)})"
          s
        } else {
          st"Implementation: [${componentName.s}](${ReportUtil.createLink(componentReport.pos)})"
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
            |
            |"""

      c._2.rustReport match {
        case Some(rust) =>
          for(entrypoint <- rust.entrypointReport.entries) {
            bh =
              st"""$bh
                  |    ${ops.StringOps(entrypoint._1).firstToUpper}: [Rust](${ReportUtil.createLink(entrypoint._2)})
                  |"""
          }
        case _ =>
      }

      bh =
        st"""$bh
            |
            |${c._2.prettyPortReport(ReportUtil.createFullLink _)}"""


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
        case Some(f) => Some(st"![${f.name}](${ReportUtil.deWin(sel4OutputDir.relativize(f).value)})")
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
          |
          |### Behavior Code
          |${(behaviorCodeReports, "\n\n")}
          |"""

    return readmeContent
  }
}
