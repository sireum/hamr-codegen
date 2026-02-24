// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object SystemDescriptionProviderPlugin {
  val systemDescriptionProviderPlugins: ISZ[Plugin] = ISZ(
    SystemDescriptionProvider_DomainScheduler(),
    SystemDescriptionProvider_MCS()
  )
}

@sig trait SystemDescriptionProviderPlugin extends MicrokitPlugin {

  @pure def hasHandled(store: Store): B

  @strictpure def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CComponentPlugin.processedThreads(store) &&
      !hasHandled(store)
}

