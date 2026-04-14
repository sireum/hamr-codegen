// #Sireum
package org.sireum.hamr.codegen.common.plugin

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{Store, UnitValue}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenResults, HamrCli}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object Plugin {
}

@sig trait Plugin {
  @pure def name: String

  @strictpure def disable(store: Store): Store =
    store + s"${name}_DISABLED" ~> UnitValue()

  @strictpure def isDisabled(store: Store): B =
    store.contains(s"${name}_DISABLED")

  @pure def canFinalize(model: Aadl,
                        aadlTypes: Option[AadlTypes],
                        symbolTable: Option[SymbolTable],
                        codegenResults: CodeGenResults,
                        store: Store,
                        options: CodegenOption,
                        reporter: Reporter): B = {
    return F
  }

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

@sig trait ModelTransformerPlugin extends Plugin {
  @pure def canHandleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): B

  @pure def handleModelTransform(model: Aadl,
                                 options: HamrCli.CodegenOption,
                                 types: AadlTypes,
                                 symbolTable: SymbolTable,
                                 store: Store,
                                 reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)]
}