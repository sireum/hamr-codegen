// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.rust.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.{AadlFeatureEvent, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.MicrokitLintPlugin
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@sig trait MicrokitGumboLinter extends MicrokitLintPlugin {

  @pure override def validModel(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, B) = {
    for (t <- symbolTable.getThreads() if MicrokitUtil.isRusty(t)) {
      var alreadyWarned: B = F
      GumboRustUtil.getGumboSubclauseOpt(t.path, symbolTable) match {
        case Some(g) =>
          for (p <- t.getPorts() if p.isEvent && !alreadyWarned) {
            if (p.asInstanceOf[AadlFeatureEvent].queueSize > 1) {
              alreadyWarned = T
              reporter.error(p.posOpt, name, "Rust components with GUMBO contracts currently only support single element port queues")
            }
          }
        case _ =>
      }
    }
    return (store, T)
  }
}

@datatype class DefaultMicrokitGumboLinter extends MicrokitGumboLinter {
  val name: String = "DefaultMicrokitGumboLinter"
}
