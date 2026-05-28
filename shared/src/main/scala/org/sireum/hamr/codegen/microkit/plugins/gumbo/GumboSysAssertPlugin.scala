// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.monitors.UserLandMonitorPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{Aadl, GclBodyMethod, GclSpecMethod, GclSubclause}
import org.sireum.message.Reporter

object GumboSysAssertPlugin {

  val GUMBO_MONITOR_CBACKEND_KEY: String = "KEY_gumbo_monitor_CBackend"

  val sysAssertFunctionsModuleName: String = "sys_assert_functions"

  @pure def hasSystemSchedule(symbolTable: SymbolTable): B = {
    for (annex <- symbolTable.rootSystem.component.annexes) {
      annex.clause match {
        case gsc: GclSubclause if gsc.schedule.nonEmpty => return T
        case _ =>
      }
    }
    return F
  }
}

// Extends GumboMonitorPlugin to create a separate monitor protection domain for
// system-level assertion checking based on the GclSchedule. Inherits all phases
// from GumboMonitorPlugin — each plugin instance gets its own store key namespace
// via getMonitorName, so the two monitors operate independently. Overrides
// handleMonitorMethod to generate the sys assert dispatch logic derived from
// the schedule's Petri net walk. Only activates when the root system implementation
// has a schedule block in its GUMBO subclause.
@sig trait GumboSysAssertPlugin extends GumboMonitorPlugin {

  @strictpure override def getMonitorName: String = "sys_assert_monitor"

  @pure override def canHandleModelTransform(model: Aadl,
                                              options: HamrCli.CodegenOption,
                                              types: AadlTypes,
                                              symbolTable: SymbolTable,
                                              store: Store,
                                              reporter: Reporter): B = {
    return super.canHandleModelTransform(model, options, types, symbolTable, store, reporter) &&
      GumboSysAssertPlugin.hasSystemSchedule(symbolTable)
  }


  // The C backend phase adds state var guards and is_monitoring_enabled() to thread
  // components. These are shared infrastructure — they only need to be added once
  // regardless of how many monitor plugins are active. Skip if the gumbo monitor
  // already set it up; otherwise run normally (e.g. if gumbo monitor is disabled).
  @pure override def handleCBackend(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                     symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    if (store.contains(GumboSysAssertPlugin.GUMBO_MONITOR_CBACKEND_KEY)) {
      return (store + keyCBackend ~> BoolValue(T), ISZ())
    }
    return super.handleCBackend(model, options, types, symbolTable, store, reporter)
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      GumboSysAssertPlugin.hasSystemSchedule(symbolTable)
  }

  @pure override def handleMonitorMethod(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                          symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {

    val (parentStore, parentResources) = super.handleMonitorMethod(model, options, types, symbolTable, store, reporter)

    val monitorThreadPath: ISZ[String] = getMonitorThreadPath(symbolTable.rootSystem.path)
    val contributions = CRustComponentPlugin.getCRustComponentContributions(parentStore)

    var localStore = parentStore

    contributions.componentContributions.get(monitorThreadPath) match {
      case Some(monitorContrib) =>
        val existingImpl = monitorContrib.appStructImpl.asInstanceOf[RAST.ImplBase]
        var updatedImplItems: ISZ[RAST.Item] = ISZ()
        for (item <- existingImpl.items) {
          item match {
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "monitor" =>
              updatedImplItems = updatedImplItems :+ fn(
                body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                  st"// TODO: sys assert dispatch logic")))))
            case _ =>
              updatedImplItems = updatedImplItems :+ item
          }
        }

        val updatedImpl = existingImpl(items = updatedImplItems)

        // Transpile system-level GUMBO functions to Rust and emit as a separate
        // module so they stay outside the verus! macro (they use bitwise bool ops
        // via the implies!/impliesL! macros which Verus rejects)
        val (moduleResources, modAndUse) =
          getSystemGumboFunctions(symbolTable, options, types, localStore, reporter)

        val updatedContrib = monitorContrib(
          appStructImpl = updatedImpl,
          appModDirectives = monitorContrib.appModDirectives ++ modAndUse._1,
          appUses = monitorContrib.appUses ++ modAndUse._2)

        localStore = CRustComponentPlugin.putComponentContributions(
          contributions.replaceComponentContributions(
            contributions.componentContributions + monitorThreadPath ~> updatedContrib),
          localStore)

        return (localStore, parentResources ++ moduleResources)

      case _ =>
    }

    return (localStore, parentResources)
  }

  // Transpile the GUMBO functions from the root system implementation's subclause
  // to Rust functions for runtime assertion checking. Returns:
  //   - Resource for the module file
  //   - (appModDirectives, appUses) items to add to the app file
  // The functions are emitted in a separate module to keep them outside the verus!
  // macro, since they use bitwise bool ops via implies!/impliesL! macros.
  @pure def getSystemGumboFunctions(symbolTable: SymbolTable,
                                     options: HamrCli.CodegenOption,
                                     types: AadlTypes,
                                     store: Store,
                                     reporter: Reporter): (ISZ[Resource], (ISZ[RAST.Item], ISZ[RAST.Item])) = {

    val rootSystem = symbolTable.rootSystem
    val subclauseInfoOpt = GumboRustUtil.getGumboSubclauseOpt(rootSystem.path, symbolTable)

    subclauseInfoOpt match {
      case Some(subclauseInfo) =>
        val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(store).get
        var functions: ISZ[ST] = ISZ()

        for (m <- subclauseInfo.annex.methods) {
          m match {
            case g: GclBodyMethod =>
              val fn = GumboRustUtil.processGumboBodyMethod(
                m = g,
                owner = rootSystem.classifier,
                optComponent = None(),
                isLibraryMethod = F,
                inVerus = F,
                options = options,
                aadlTypes = types,
                tp = crustTypeProvider,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)
              functions = functions :+ fn.prettyST
            case _: GclSpecMethod =>
              reporter.warn(None(), "GumboSysAssertPlugin", "Spec methods in system-level GUMBO subclauses are not yet supported")
          }
        }

        val monitorThread = symbolTable.componentMap.get(
          getMonitorThreadPath(symbolTable.rootSystem.path)).get.asInstanceOf[AadlThread]
        val componentDir = CRustComponentPlugin.componentDirectory(monitorThread, options)

        val moduleContent: ST =
          st"""${CommentTemplate.doNotEditComment_slash}
              |
              |use data::*;
              |
              |${GumboRustUtil.RustImplicationMacros}
              |
              |${(functions, "\n\n")}
              |"""

        val moduleResource = ResourceUtil.createResource(
          path = s"$componentDir/${GumboSysAssertPlugin.sysAssertFunctionsModuleName}.rs",
          content = moduleContent,
          overwrite = T)

        val modName = GumboSysAssertPlugin.sysAssertFunctionsModuleName
        val modDirective: RAST.Item = RAST.ItemST(
          st"""#[path = "$modName.rs"]
              |pub mod $modName;""")
        val useItem: RAST.Item = RAST.Use(ISZ(), RAST.IdentString(s"$modName::*"))

        return (ISZ(moduleResource), (ISZ(modDirective), ISZ(useItem)))

      case _ => return (ISZ(), (ISZ(), ISZ()))
    }
  }
}

@datatype class DefaultGumboSysAssertPlugin extends GumboSysAssertPlugin {

  val name: String = "DefaultGumboSysAssertPlugin"
}
