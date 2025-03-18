// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.microkit.plugins.types.{DefaultCRustTypePlugin, DefaultCTypePlugin}
import org.sireum.hamr.codegen.microkit.plugins.apis.DefaultCRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.component.DefaultCRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{DefaultGumboRustPlugin, DefaultGumboXPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitLinterPlugin
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object MicrokitPlugin {

  val defaultMicrokitPlugins: ISZ[Plugin] = ISZ(
    // lint-ers
    DefaultMicrokitLinterPlugin(),

    // type-ers
    DefaultCTypePlugin(), DefaultCRustTypePlugin(),

    // api-ers
    DefaultCRustApiPlugin(),

    // component-ers
    DefaultCRustComponentPlugin(),

    // gumbo-ers
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin()
  )

  val KEY_MODEL_IS_RUSTY: String = "KEY_MODEL_IS_RUSTY"
  @pure def modelIsRusty(store: Store): B = {
    return store.contains(KEY_MODEL_IS_RUSTY)
  }
}

// lint plugins are called as the first step in microkit codegen.  Subsequent phases will not be
// initiated if any lint plugin return F or if the reporter contains errors after linting
@sig trait MicrokitLintPlugin extends Plugin {
  def validModel(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, B)
}

// init plugins are called after linting in order to do any initialization, pre-processing, etc that is
// needed by a plugin
@sig trait MicrokitInitPlugin extends Plugin {
  def init(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): Store
}

@sig trait MicrokitPlugin extends Plugin {

  // TODO: maybe pass in the touched types
  @pure def canHandle(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B

  @pure def handle(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource])
}

/** Finalize plugins are called after all MicrokitPlugins return F for canHandle and the reporter
  * does not contain error messages
  */
@sig trait MicrokitFinalizePlugin extends Plugin {

  @pure def canFinalize(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B

  @pure def finalize(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource])
}
