// #Sireum
package org.sireum.hamr.codegen.common.plugin

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.CodeGenResults
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object Plugin {
}

@sig trait Plugin {
  @pure def name: String

  // finalizePlugin is called prior to codegen returning
  @pure def finalizePlugin(model: Aadl,
                           aadlTypes: Option[AadlTypes],
                           symbolTable: Option[SymbolTable],
                           codegenResults: CodeGenResults,
                           store: Store,
                           options: CodegenOption,
                           reporter: Reporter): Store = {
    return store
  }
}