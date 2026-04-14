// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.rust.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, MonitorInjector}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.microkit.plugins.rust.gumbo.GumboRustModelTransformerPlugin.KEY_GumboRustModelTransformerPlugin
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object GumboRustModelTransformerPlugin {

  val KEY_GumboRustModelTransformerPlugin: String = "KEY_GumboRustModelTransformerPlugin"

}
@sig trait GumboRustModelTransformerPlugin extends ModelTransformerPlugin {

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_GumboRustModelTransformerPlugin)

  @strictpure override def canHandleModelTransform(model: Aadl,
                                                   options: HamrCli.CodegenOption,
                                                   types: AadlTypes,
                                                   symbolTable: SymbolTable
                                                   , store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !haveHandledModelTransform(store) &&
      !reporter.hasError &&
      options.runtimeMonitoring
      // && GumboXRustUtil.hasGumbo(model, symbolTable)

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store + KEY_GumboRustModelTransformerPlugin ~> BoolValue(T)

    val rmodel = MonitorInjector.inject(model, symbolTable, reporter)
    if (!reporter.hasError) {
      val reResult = ModelUtil.resolve(rmodel, rmodel.components(0).identifier.pos, "", options, localStore, reporter)
      localStore = reResult._2
      if (reResult._1.nonEmpty) {
        return Some(localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable)
      }
    }
    return None()
  }
}

@datatype class DefaultGumboRustModelTransformerPlugin extends GumboRustModelTransformerPlugin {

  val name: String = "DefaultGumboRustModelTransformerPlugin"
}
