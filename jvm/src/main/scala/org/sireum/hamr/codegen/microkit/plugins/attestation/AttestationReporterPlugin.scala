// #Sireum

package org.sireum.hamr.codegen.microkit.plugins.attestation

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.reporting.{CodegenReport, CodegenReporting}
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.{GclAnnexLibInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenResults, HamrCli}
import org.sireum.hamr.codegen.microkit.plugins.reporting.{MicrokitReporterPlugin, ReportUtil}
import org.sireum.hamr.codegen.microkit.plugins.rust.gumbo.GumboRustUtil
import org.sireum.hamr.codegen.microkit.reporting.MicrokitReport
import org.sireum.hamr.ir
import org.sireum.hamr.ir.GclMethod
import org.sireum.message.{Position, Reporter}
import org.sireum.lang.{ast => AST}
import org.sireum.lang.symbol.Info

@datatype class AttestationReporterPlugin() extends Plugin {

  val name: String = "AttestationReporterPlugin"

  val KEY_ATTESTATION_REPORTER_PLUGIN: String = "KEY_ATTESTATION_REPORTER_PLUGIN"

  @strictpure def hasFinalized(store: Store): B = store.contains(KEY_ATTESTATION_REPORTER_PLUGIN)

  @pure override def canFinalize(model: ir.Aadl, aadlTypes: Option[AadlTypes], symbolTable: Option[SymbolTable], codegenResults: CodeGenResults, store: Store, options: HamrCli.CodegenOption, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !hasFinalized(store) &&
        options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        //symbolTable.nonEmpty &&
        CodegenReporting.getCodegenReport(MicrokitReporterPlugin.name, store).nonEmpty)
  }

  @pure override def finalizePlugin(model: ir.Aadl,
                                    aadlTypes: Option[AadlTypes],
                                    symbolTable: Option[SymbolTable],
                                    codegenResults: CodeGenResults,
                                    store: Store,
                                    options: HamrCli.CodegenOption,
                                    reporter: Reporter): Store = {
    var localStore = store + KEY_ATTESTATION_REPORTER_PLUGIN ~> CommonUtil.BoolValue(T)

    val isAadl = ops.StringOps(symbolTable.get.rootSystem.component.identifier.pos.get.uriOpt.get).endsWith(".aadl")

    val lang: String = if (isAadl) "aadl" else "sysml"

    val workspaceRoot: Os.Path =
      options.workspaceRootDir match {
        case Some(wdir) =>
          val d = Os.path(wdir)
          assert(d.exists, d.value)
          d.canon
        case _ =>
          println("Model workspace option was not provided. Cannot generate attestation artifacts")
          return localStore
      }

    val sel4OutputDir: Os.Path =
      options.sel4OutputDir match {
        case Some(d) => Os.path(d).canon
        case _ =>
          options.outputDir match {
            case Some(f) => (Os.path(f) / "microkit").canon
            case _ => halt("Infeasible: no output directory was specified")
          }
      }
    assert(sel4OutputDir.exists, sel4OutputDir.value)

    val attestationDir = sel4OutputDir / "attestation"

    val microkitReport: MicrokitReport = CodegenReporting.getCodegenReport(MicrokitReporterPlugin.name, store).get.asInstanceOf[MicrokitReport]

    var componentReports: ISZ[AttestationReportContainers.Report] = ISZ()

    // just handle rusty, gumboy components for now
    for (component <- microkitReport.componentReport.entries if component._2.gumboReport.nonEmpty) {
      val aadlComponent = symbolTable.get.getThreadById(component._1.idPath)
      val subclauseInfo = GumboRustUtil.getGumboSubclause(aadlComponent.path, symbolTable.get)
      val rustReport = component._2.rustReport.get
      val gumboReport = component._2.gumboReport.get

      var componentLevelReports: ISZ[AttestationReportContainers.ComponentLevelReport] = ISZ()

      var inSlices: ISZ[AttestationReportContainers.Slice] = ISZ()
      var outSlices: ISZ[AttestationReportContainers.Slice] = ISZ()

      def reset(): Unit = {
        inSlices = ISZ()
        outSlices = ISZ()
      }

      def processSymbols(symbols: ISZ[AST.ResolvedInfo]): Unit = {
        var symbolsToProcess: ISZ[AST.ResolvedInfo] = symbols

        var seen: Set[AST.ResolvedInfo] = Set.empty

        def processH(symbol: AST.ResolvedInfo): Unit = {
          symbol match {
            case m: AST.ResolvedInfo.Method =>
              val id = st"${(m.owner, ".")}.${m.id}".render

              if (AttestationUtil.isLibraryMethod(m)) {
                assert (m.owner.size == 2)

                val libAnnex: GclAnnexLibInfo = AttestationUtil.getGumboLibAnnex(m.owner, symbolTable.get.annexLibInfos)
                val gumboLibMethod: GclMethod = AttestationUtil.getGumboLibAnnexMethod(m.id, libAnnex.annex.methods)

                inSlices = inSlices :+ AttestationReportContainers.Slice(
                  kind = AttestationReportContainers.SliceKind.Model,
                  meta = Some(s"Invoked GUMBO Annex Library Method: ${m.owner(0)}::${m.id}"),
                  pos = AttestationUtil.toPos(gumboLibMethod.posOpt.get, workspaceRoot, attestationDir))
              } else {
                AttestationUtil.getGumboSubclauseMethod(m.id, subclauseInfo.annex.methods) match {
                  case Some(gumboMethod) =>
                    val rustMethodPos: Position = gumboReport.methodsReport.get(m.id).get

                    inSlices = inSlices :+ AttestationReportContainers.Slice(
                      kind = AttestationReportContainers.SliceKind.Model,
                      meta = Some(s"Invoked GUMBO Subclause Method: ${m.id}"),
                      pos = AttestationUtil.toPos(gumboMethod.posOpt.get, workspaceRoot, attestationDir))

                    outSlices = outSlices :+ AttestationReportContainers.Slice(
                      kind = AttestationReportContainers.SliceKind.Model,
                      meta = Some(s"Invoked Verus Method: ${m.id}"),
                      pos = AttestationUtil.toPos(rustMethodPos, sel4OutputDir, attestationDir))
                  case _ =>
                }
              }

              // collect symbols from method's exp
              subclauseInfo.gclSymbolTable.slangTypeHierarchy.nameMap.get(m.owner :+ m.id).get match {
                case im: Info.Method =>
                  im.ast.bodyOpt.get.stmts(0) match {
                    case r: AST.Stmt.Return =>
                      assert (r.expOpt.nonEmpty, "Currently expecting GUMBO methods to return values")

                      assert (r.expOpt.get.typedOpt.nonEmpty)

                      val sc = AttestationUtil.CollectSymbols()
                      sc.transform_langastExp(r.expOpt.get)
                      symbolsToProcess = symbolsToProcess ++ sc.symbols

                    case x =>
                  }
                case _ =>
              }

            case v: AST.ResolvedInfo.Var =>
              gumboReport.stateReport.get(v.id) match {
                case Some(sv) =>

                  val gsv = AttestationUtil.getGumboStateVariable(v.id, subclauseInfo.annex.state)

                  inSlices = inSlices :+ AttestationReportContainers.Slice(
                    kind = AttestationReportContainers.SliceKind.Model,
                    meta = Some(s"Touched GUMBO state variable: ${v.id}"),
                    pos = AttestationUtil.toPos(gsv.posOpt.get, workspaceRoot, attestationDir))

                  outSlices = outSlices :+ AttestationReportContainers.Slice(
                    kind = AttestationReportContainers.SliceKind.Rust,
                    meta = Some(s"Touched Rust realization of GUMBO state variable: ${v.id}"),
                    pos = AttestationUtil.toPos(sv, sel4OutputDir, attestationDir))
                case _ =>
                  gumboReport.integrationReport match {
                    case Some (ireport) =>

                      aadlComponent.getPortByPath(aadlComponent.path :+ v.id) match {
                        case Some(p) =>
                          subclauseInfo.gclSymbolTable.integrationMap.get(p) match {
                            case Some(gumboIntegrationConstraint) =>

                              val sc = AttestationUtil.CollectSymbols()
                              sc.transform_langastExp(gumboIntegrationConstraint.exp)
                              symbolsToProcess = symbolsToProcess ++ sc.symbols

                              p.direction match {
                                case ir.Direction.In =>
                                  ireport.assumesReport.get(gumboIntegrationConstraint.id) match {
                                    case Some(verusOutput) =>
                                      inSlices = inSlices :+ AttestationReportContainers.Slice(
                                        kind = AttestationReportContainers.SliceKind.Model,
                                        meta = Some(s"GUMBO integration constraint '${gumboIntegrationConstraint.id}' on touched incoming port ${v.id}"),
                                        pos = AttestationUtil.toPos(gumboIntegrationConstraint.posOpt.get, workspaceRoot, attestationDir))

                                      outSlices = outSlices :+ AttestationReportContainers.Slice(
                                        kind = AttestationReportContainers.SliceKind.Verus,
                                        meta = Some(s"Verus realization of GUMBO integration constraint '${gumboIntegrationConstraint.id}' on touched incoming port ${v.id}"),
                                        pos = AttestationUtil.toPos(verusOutput, sel4OutputDir, attestationDir))
                                    case _ =>
                                      halt(s"Didn't find verus report for integration constraint ${gumboIntegrationConstraint.id}")
                                  }
                                  assume(T)
                                case ir.Direction.Out =>
                                  ireport.guaranteesReport.get(gumboIntegrationConstraint.id) match {
                                    case Some(verusOutput) =>
                                      inSlices = inSlices :+ AttestationReportContainers.Slice(
                                        kind = AttestationReportContainers.SliceKind.Model,
                                        meta = Some(s"GUMBO integration constraint '${gumboIntegrationConstraint.id}' on touched outgoing port ${v.id}"),
                                        pos = AttestationUtil.toPos(gumboIntegrationConstraint.posOpt.get, workspaceRoot, attestationDir))

                                      outSlices = outSlices :+ AttestationReportContainers.Slice(
                                        kind = AttestationReportContainers.SliceKind.Verus,
                                        meta = Some(s"Verus realization of GUMBO integration constraint '${gumboIntegrationConstraint.id}' on touched outgoing port ${v.id}"),
                                        pos = AttestationUtil.toPos(verusOutput, sel4OutputDir, attestationDir))
                                    case _ =>
                                      halt(s"Didn't find verus report for integration constraint ${gumboIntegrationConstraint.id}")
                                  }
                                  assume(T)
                                case _ =>
                              }
                            case _ =>
                          }
                        case _ =>
                      }
                      assume(T)
                    case _ =>
                  }
              }

            case x =>
              halt(s"Not expecting $x")
          }
        }

        while(symbolsToProcess.nonEmpty) {
          val s = symbolsToProcess(0)
          symbolsToProcess = ops.ISZOps(symbolsToProcess).tail
          if (!seen.contains(s)) {
            seen = seen + s
            processH(s)
          }
        }
      }

      subclauseInfo.annex.initializes match {
        case Some(i) =>
          for (g <- i.guarantees) {
            val verusReport = gumboReport.initializeReport.get(g.id).get

            reset()

            inSlices = inSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Model,
              meta = Some("GUMBO initializes contract"),
              pos = AttestationUtil.toPos(g.posOpt.get, workspaceRoot, attestationDir))

            outSlices = outSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Verus,
              meta = Some("Verus realization of GUMBO initializes contract"),
              pos = AttestationUtil.toPos(verusReport, sel4OutputDir, attestationDir))

            val sc = AttestationUtil.CollectSymbols()
            sc.transform_langastExp(g.exp)
            processSymbols(sc.symbols)

            componentLevelReports = componentLevelReports :+ AttestationReportContainers.ComponentContractReport(
              id = g.id,
              kind = AttestationReportContainers.ContractKind.Initialize,
              meta = Some(s"Initializes contract for ${aadlComponent.classifierAsString}"),
              slices = inSlices ++ outSlices)
          }
        case _ =>
      }
      subclauseInfo.annex.compute match {
        case Some(c) =>
          for(assumes <- c.assumes) {
            val verusReport = gumboReport.computeReport.get.assumesReport.get(assumes.id).get

            reset()

            inSlices = inSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Model,
              meta = Some("GUMBO general assumes contract"),
              pos = AttestationUtil.toPos(assumes.posOpt.get, workspaceRoot, attestationDir))

            outSlices = outSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Verus,
              meta = Some("Verus realization of GUMBO general assumes contract"),
              pos = AttestationUtil.toPos(verusReport, sel4OutputDir, attestationDir))

            val sc = AttestationUtil.CollectSymbols()
            sc.transform_langastExp(assumes.exp)
            processSymbols(sc.symbols)

            componentLevelReports = componentLevelReports :+ AttestationReportContainers.ComponentContractReport(
              id = assumes.id,
              kind = AttestationReportContainers.ContractKind.Assume,
              meta = Some(s"General assume contract for ${aadlComponent.classifierAsString}"),
              slices = inSlices ++ outSlices)
          }

          for (guarantees <- c.guarantees) {
            val verusReport = gumboReport.computeReport.get.guaranteesReport.get(guarantees.id).get

            reset()

            inSlices = inSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Model,
              meta = Some("GUMBO general guarantees contract"),
              pos = AttestationUtil.toPos(guarantees.posOpt.get, workspaceRoot, attestationDir))

            outSlices = outSlices :+ AttestationReportContainers.Slice(
              kind = AttestationReportContainers.SliceKind.Verus,
              meta = Some("Verus realization of GUMBO general guarantees contract"),
              pos = AttestationUtil.toPos(verusReport, sel4OutputDir, attestationDir))

            val sc = AttestationUtil.CollectSymbols()
            sc.transform_langastExp(guarantees.exp)
            processSymbols(sc.symbols)

            componentLevelReports = componentLevelReports :+ AttestationReportContainers.ComponentContractReport(
              id = guarantees.id,
              kind = AttestationReportContainers.ContractKind.Guarantee,
              meta = Some(s"General guarantee contract for ${aadlComponent.classifierAsString}"),
              slices = inSlices ++ outSlices)
          }
        case _ =>
      }

      componentReports = componentReports :+ AttestationReportContainers.ComponentReport(
        idPath = aadlComponent.path,
        classifier = aadlComponent.classifier,
        meta = None(),
        reports = componentLevelReports)
    }

    val attesationReport = AttestationReportContainers.AttestationReport(reports = componentReports)

    val attestationContent = attesationReport.prettyST
    val attestationReportPath = attestationDir / s"${lang}_attestation_report.json"

    ReportUtil.writeOutResource(attestationContent, attestationReportPath, F, options.verbose)
    println(s"Wrote: ${attestationReportPath.toUri}")

    return localStore
  }
}
