// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.sysvc.{ScheduleNextRel, VCGenerator}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.MicrokitFinalizePlugin
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@sig trait GumboSysAssertVcGenPlugin extends MicrokitFinalizePlugin {

  @pure override def canFinalizeMicrokit(model: Aadl,
                                          options: HamrCli.CodegenOption,
                                          types: AadlTypes,
                                          symbolTable: SymbolTable,
                                          store: Store,
                                          reporter: Reporter): B = {
    return (options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !reporter.hasError &&
      !isDisabled(store) &&
      VCGenerator.hasSystemSchedule(symbolTable))
  }

  @pure override def finalizeMicrokit(model: Aadl,
                                       options: HamrCli.CodegenOption,
                                       types: AadlTypes,
                                       symbolTable: SymbolTable,
                                       store: Store,
                                       reporter: Reporter): (Store, ISZ[Resource]) = {
    val scheduleOpt = VCGenerator.getSystemSchedule(symbolTable)
    scheduleOpt match {
      case Some(schedule) =>
        val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)
        val nextRel = ScheduleNextRel.build(schedule)
        val vcs = VCGenerator.generate(schedule, nextRel, resolvedAliasMap, symbolTable)

        for (vc <- vcs) {
          println(vc)
        }
        // TODO: serialize VCs to Verus proof functions

        return (store, ISZ())
      case _ =>
        return (store, ISZ())
    }
  }
}

@datatype class DefaultGumboSysAssertVcGenPlugin extends GumboSysAssertVcGenPlugin {

  val name: String = "DefaultGumboSysAssertVcGenPlugin"
}
