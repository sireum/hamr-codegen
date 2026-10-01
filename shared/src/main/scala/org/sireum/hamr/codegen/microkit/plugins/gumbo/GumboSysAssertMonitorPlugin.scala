// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.sysvc.{ScheduleNextRel, VCGenerator}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{Aadl, GclBodyMethod, GclComposition, GclSpecMethod}
import org.sireum.message.Reporter

object GumboSysAssertMonitorPlugin {


  @strictpure def hasCompositions(symbolTable: SymbolTable): B =
    VCGenerator.hasCompositions(symbolTable)

  @strictpure def getCompositions(symbolTable: SymbolTable): ISZ[GclComposition] =
    VCGenerator.getCompositions(symbolTable)

  // The per-composition monitor name: sys_<id>_monitor -- composition-first so a
  // composition's artifacts (sys_<id>_proof, sys_<id>_monitor) group together. Each
  // composition gets its own monitor component/crate (design D8, approach (i)); the
  // id-derived name feeds the PD process/thread paths, the sys_<id>_monitor.{meta.py,mk}
  // bundle, the per-composition store sub-keys, and (via the crate-name override
  // registered at model transform) the monitor's crates/sys_<id>_monitor crate -- the
  // name is unique per composition, so the crate does not need the thread id's longer
  // <..>_process_<..>_thread suffix.
  @strictpure def monitorNameForComposition(id: String): String =
    s"sys_${id}_monitor"

}

// Extends GumboMonitorPlugin to create a separate monitor protection domain for
// system-level assertion checking based on the composition schema. Inherits all phases
// from GumboMonitorPlugin — each plugin instance gets its own store key namespace
// via getMonitorName, so the two monitors operate independently. Overrides
// handleMonitorMethod to generate the sys assert dispatch logic derived from
// the schedule's Petri net walk. Only activates when the root system implementation
// has a schedule block in its GUMBO subclause.
@sig trait GumboSysAssertMonitorPlugin extends GumboMonitorPlugin {

  @strictpure override def getMonitorName: String = "sys_assert_monitor"

  // One monitor per composition (design D8, approach (i)): each phase loops over
  // these names. getMonitorName above remains the plugin's lifecycle/store-key
  // namespace; the per-composition artifacts use these id-derived names.
  @strictpure override def monitorNames(symbolTable: SymbolTable): ISZ[String] =
    for (c <- GumboSysAssertMonitorPlugin.getCompositions(symbolTable))
      yield GumboSysAssertMonitorPlugin.monitorNameForComposition(c.id)

  @pure override def canHandleModelTransform(model: Aadl,
                                              options: HamrCli.CodegenOption,
                                              types: AadlTypes,
                                              symbolTable: SymbolTable,
                                              store: Store,
                                              reporter: Reporter): B = {
    return super.canHandleModelTransform(model, options, types, symbolTable, store, reporter) &&
      GumboSysAssertMonitorPlugin.hasCompositions(symbolTable)
  }


  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      GumboSysAssertMonitorPlugin.hasCompositions(symbolTable)
  }

  // The workflow-net walk and the system assertions live in crates/observers
  // (ContractObserverPlugin, one SysAssert_<id> per composition); this monitor locates
  // itself in the schedule and hands the thread that just yielded to it.
  @pure override def handleMonitorMethod(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                          symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {

    val (parentStore, parentResources) = super.handleMonitorMethod(model, options, types, symbolTable, store, reporter)

    var localStore = parentStore

    val compositions = GumboSysAssertMonitorPlugin.getCompositions(symbolTable)
    if (compositions.isEmpty) {
      return (localStore, parentResources)
    }

    val obs = ContractObserverPlugin.crateName

    // One monitor component per composition (design D8, approach (i)): each
    // composition's dispatch/check body is baked into its own id-named monitor
    // component (sys_assert_<id>_monitor). Re-fetch the contributions each
    // iteration since the prior iteration updated a (different) monitor component.
    for (composition <- compositions) {
      val monitorName = GumboSysAssertMonitorPlugin.monitorNameForComposition(composition.id)
      val monitorThreadPath: ISZ[String] = monitorThreadPathNamed(symbolTable.rootSystem.path, monitorName)
      val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
      contributions.componentContributions.get(monitorThreadPath) match {
      case Some(monitorContrib) =>
        val existingImpl = monitorContrib.appStructImpl.asInstanceOf[RAST.ImplBase]
        val sysType: String =
          s"$obs::${ContractObserverPlugin.sysAssertModuleName(composition.id)}::${ContractObserverPlugin.sysAssertTypeName(composition.id)}"

        // Uses its own sys_assert_last_index sentinel (independent of gumbo_monitor's
        // last_index) so initialization and conformance checking run correctly even
        // though gumbo_monitor executes first.
        val sysAssertBody: ST =
          st"""let state = api.get_sched_state();
              |
              |if self.sys_assert_last_index == u32::MAX {
              |  let schedule = api.get_sched_schedule();
              |  buildUserChannelTables(
              |    &schedule, &mut self.prev_user_ch, &mut self.next_user_ch);
              |  $sysType::validate_schedule(schedule.num_timeslices as usize, &schedule.timeslice_ch,
              |    &schedule.is_user_partition, thread_of, &mut LogSink);
              |}
              |
              |let idx = state.current_timeslice as usize;
              |
              |if self.sys_assert_last_index == u32::MAX {
              |  // First compute phase — initialize ready set and cascade
              |  let mut view = MonitorView { api: api, focus: None };
              |  self.sys_assert.on_init(&mut view, &mut LogSink);
              |  if let Some(next) = thread_of(self.next_user_ch[idx]) {
              |    note_dispatch(next);
              |    latch_received(next, &mut view);
              |  }
              |  self.sys_assert_last_index = state.current_timeslice;
              |  return;
              |}
              |
              |// the thread that just yielded: fire its transition and check the assertions
              |let mut view = MonitorView { api: api, focus: None };
              |if let Some(prev) = thread_of(self.prev_user_ch[idx]) {
              |  producer_completed(prev);
              |  self.sys_assert.on_complete(prev, &mut view, &mut LogSink);
              |}
              |
              |// the thread that runs next: what it receives on an input a composition aliases is
              |// latched now, once the completion above has ended the frame if it was the last --
              |// so it belongs to the frame the thread runs in (the GUMBO layer read it earlier in
              |// this run; the per-run cache gives the same value)
              |if let Some(next) = thread_of(self.next_user_ch[idx]) {
              |  note_dispatch(next);
              |  latch_received(next, &mut view);
              |}
              |
              |self.sys_assert_last_index = state.current_timeslice;"""

        // Find the gumbo_monitor method so we can copy its signature
        var monitorMethodOpt: Option[RAST.FnImpl] = None()
        for (item <- existingImpl.items) {
          item match {
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "gumbo_monitor" =>
              monitorMethodOpt = Some(fn)
            case _ =>
          }
        }

        // Update impl: append sys_assert_monitor call to timeTriggered,
        // add the sys-assert field inits to new(), add sys_assert_monitor method
        var updatedImplItems: ISZ[RAST.Item] = ISZ()
        for (item <- existingImpl.items) {
          item match {
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "timeTriggered" =>
              val existingItems: ISZ[RAST.BodyItem] = fn.body match {
                case Some(mb) => mb.items
                case _ => ISZ()
              }
              // The run starts with begin_monitor_run.  The gumbo layer's body already does
              // that when the model has thread contracts; otherwise it is added here.
              val startsRun: B = ops.ISZOps(existingItems).exists((i: RAST.BodyItem) =>
                ops.StringOps(i.prettyST.render).contains("begin_monitor_run("))
              val runStart: ISZ[RAST.BodyItem] =
                if (startsRun) ISZ() else ISZ(RAST.BodyItemST(st"${ContractObserverPlugin.beginMonitorRunCall}"))
              updatedImplItems = updatedImplItems :+ fn(
                body = Some(RAST.MethodBody((runStart ++ existingItems) :+
                  RAST.BodyItemST(st"self.sys_assert_monitor(api);"))))
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "new" =>
              var updatedBodyItems: ISZ[RAST.BodyItem] = ISZ()
              fn.body match {
                case Some(mb) =>
                  for (bi <- mb.items) {
                    bi match {
                      case bis: RAST.BodyItemSelf =>
                        updatedBodyItems = updatedBodyItems :+ bis(items = bis.items :+
                          st"sys_assert_last_index: u32::MAX," :+ st"sys_assert: $sysType::new(),")
                      case other =>
                        updatedBodyItems = updatedBodyItems :+ other
                    }
                  }
                case _ =>
              }
              updatedImplItems = updatedImplItems :+ fn(
                body = Some(RAST.MethodBody(updatedBodyItems)))
            case _ =>
              updatedImplItems = updatedImplItems :+ item
          }
        }

        monitorMethodOpt match {
          case Some(monitorFn) =>
            updatedImplItems = updatedImplItems :+ monitorFn(
              sig = monitorFn.sig(ident = RAST.IdentString("sys_assert_monitor")),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(sysAssertBody)))))
          case _ =>
            // The sys-assert monitor builds on the GUMBO monitor's method, adapter and
            // schedule tables, which exist only when some thread has GUMBO contracts.
            reporter.error(None(), name,
              s"Runtime monitoring of composition '${composition.id}' needs at least one thread with GUMBO contracts; none has any")
            return (localStore, parentResources)
        }

        val updatedStruct = monitorContrib.appStructDef(
          items = monitorContrib.appStructDef.items :+
            RAST.StructField(
              visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("sys_assert_last_index"),
              fieldType = RAST.TyPath(ISZ(ISZ("u32")), None())) :+
            RAST.StructField(
              visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("sys_assert"),
              fieldType = RAST.TyPath(ISZ(ISZ(sysType)), None())))

        val updatedContrib = monitorContrib(
          appStructDef = updatedStruct,
          appStructImpl = existingImpl(items = updatedImplItems))

        localStore = CRustComponentPlugin.putComponentContributions(
          contributions.replaceComponentContributions(
            contributions.componentContributions + monitorThreadPath ~> updatedContrib),
          localStore)

      case _ =>
      }
    }

    return (localStore, parentResources)
  }
}

@datatype class DefaultGumboSysAssertMonitorPlugin extends GumboSysAssertMonitorPlugin {

  val name: String = "DefaultGumboSysAssertMonitorPlugin"
}
