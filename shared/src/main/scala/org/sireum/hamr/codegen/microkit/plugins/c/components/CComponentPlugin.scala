// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.components

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.properties.{Hamr_Microkit_Properties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlSystem, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.ir.Aadl
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object CComponentPlugin {
  val name: String = "CComponentPlugin"

  val CComponentPlugins: ISZ[Plugin] = ISZ(
    CComponentPlugin_DomainScheduler(),
    CComponentPlugin_MCS()
  )

  @strictpure def processedThreads(store: Store): B = store.contains(name)

}

@sig trait CComponentPlugin extends MicrokitPlugin {

  @strictpure def hasHandled(store: Store): B = CComponentPlugin.processedThreads(store)

  @strictpure def markAsHandled(store: Store): Store = store + CComponentPlugin.name ~> BoolValue(T)

  @strictpure def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
      !hasHandled(store)
}