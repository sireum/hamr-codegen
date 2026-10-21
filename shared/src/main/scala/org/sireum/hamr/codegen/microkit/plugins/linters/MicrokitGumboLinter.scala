// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.linters

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.{AadlFeatureEvent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.sysvc.VCGenerator
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, HamrCli}
import org.sireum.hamr.codegen.microkit.plugins.MicrokitLintPlugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.GumboRustUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.ir.Aadl
import org.sireum.message.{Position, Reporter}

@sig trait MicrokitGumboLinter extends MicrokitLintPlugin {

  // Components with GUMBO contracts -- Rust or C -- support only single-element port queues:
  // the contracts and their run-time checks (the monitors, the test controller) see one element
  // per dispatch, which matches a thread only when its queue holds one.  So does an event port
  // a composition reads, on any thread: the system assertions' frame value of it is one
  // element per dispatch too.  An error when those run-time checks are generated (system
  // testing, --runtime-monitoring); otherwise a warning, since a Verus proof of the thread does
  // not read the queue that way.
  @pure override def validModel(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, B) = {
    val checked: B = options.runtimeMonitoring || ExperimentalOptions.enableTestScheduler(options.experimentalOptions)
    def report(posOpt: Option[Position], msg: String): Unit = {
      if (checked) {
        reporter.error(posOpt, name, msg)
      } else {
        reporter.warn(posOpt, name, s"$msg; this is an error when system testing or runtime monitoring is requested")
      }
    }
    for (t <- symbolTable.getThreads()) {
      var alreadyReported: B = F
      GumboRustUtil.getGumboSubclauseOpt(t.path, symbolTable) match {
        case Some(g) =>
          for (p <- t.getPorts() if p.isEvent && !alreadyReported) {
            val queueSize = p.asInstanceOf[AadlFeatureEvent].queueSize
            if (queueSize > 1) {
              alreadyReported = T
              report(p.posOpt,
                s"${MicrokitUtil.getComponentIdPath(t)} has GUMBO contracts, so its event ports must have single-element queues, but ${p.identifier} has a Queue_Size of $queueSize")
            }
          }
        case _ =>
      }
    }

    // event ports compositions read, on threads without contracts (those with are checked above)
    val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)
    var reported: Set[ISZ[String]] = Set.empty
    for (composition <- VCGenerator.getCompositions(symbolTable); pa <- composition.portAliases) {
      val segs = pa.portPath.name
      if (segs.size >= z"2") {
        val threadOpt: Option[AadlThread] = resolvedAliasMap.get(segs(0)) match {
          case Some(path) =>
            symbolTable.componentMap.get(path) match {
              case Some(t: AadlThread) => Some(t)
              case _ => None()
            }
          case _ => None()
        }
        threadOpt match {
          case Some(t) if GumboRustUtil.getGumboSubclauseOpt(t.path, symbolTable).isEmpty =>
            for (p <- t.getPorts() if p.isEvent && p.identifier == segs(segs.size - 1) && !reported.contains(p.path)) {
              val queueSize = p.asInstanceOf[AadlFeatureEvent].queueSize
              if (queueSize > 1) {
                reported = reported + p.path
                report(p.posOpt,
                  s"Composition ${composition.id} reads ${MicrokitUtil.getComponentIdPath(t)}'s ${p.identifier}, so it must have a single-element queue, but it has a Queue_Size of $queueSize")
              }
            }
          case _ =>
        }
      }
    }
    return (store, T)
  }
}

@datatype class DefaultMicrokitGumboLinter extends MicrokitGumboLinter {
  val name: String = "DefaultMicrokitGumboLinter"
}
