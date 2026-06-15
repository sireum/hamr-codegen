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

  val GUMBO_MONITOR_CBACKEND_KEY: String = "KEY_gumbo_monitor_CBackend"

  val sysAssertFunctionsModuleName: String = "sys_assert_functions"

  @strictpure def hasCompositions(symbolTable: SymbolTable): B =
    VCGenerator.hasCompositions(symbolTable)

  @strictpure def getCompositions(symbolTable: SymbolTable): ISZ[GclComposition] =
    VCGenerator.getCompositions(symbolTable)

  // The per-composition monitor name. Each composition gets its own monitor
  // component/crate (design D8, approach (i)); the id-derived name feeds the PD
  // process/thread paths, the sys_assert_<id>_monitor.{meta.py,scheduler.c,mk}
  // bundle, and the per-composition store sub-keys.
  @strictpure def monitorNameForComposition(id: String): String =
    s"sys_assert_${id}_monitor"

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


  // The C backend phase adds state var guards and is_monitoring_enabled() to thread
  // components. These are shared infrastructure — they only need to be added once
  // regardless of how many monitor plugins are active. Skip if the gumbo monitor
  // already set it up; otherwise run normally (e.g. if gumbo monitor is disabled).
  @pure override def handleCBackend(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                     symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    if (store.contains(GumboSysAssertMonitorPlugin.GUMBO_MONITOR_CBACKEND_KEY)) {
      return (store + keyCBackend ~> BoolValue(T), ISZ())
    }
    return super.handleCBackend(model, options, types, symbolTable, store, reporter)
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      GumboSysAssertMonitorPlugin.hasCompositions(symbolTable)
  }

  @pure override def handleMonitorMethod(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                          symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {

    val (parentStore, parentResources) = super.handleMonitorMethod(model, options, types, symbolTable, store, reporter)

    var localStore = parentStore
    var resources = parentResources

    val compositions = GumboSysAssertMonitorPlugin.getCompositions(symbolTable)
    if (compositions.isEmpty) {
      return (localStore, parentResources)
    }

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

        // Build the NextRel from the composition's schema
        val nextRel = ScheduleNextRel.build(composition)

        // Assign bit positions to places
        var placeBits: Map[ScheduleNextRel.PlaceId, Z] = Map.empty
        for (i <- z"0" until nextRel.places.size) {
          placeBits = placeBits + nextRel.places(i) ~> i
        }

        // Build component alias → thread ID mapping
        val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(localStore)
        var aliasToThreadId: Map[String, String] = Map.empty
        for (entry <- resolvedAliasMap.entries) {
          symbolTable.componentMap.get(entry._2) match {
            case Some(thread: AadlThread) =>
              aliasToThreadId = aliasToThreadId + entry._1 ~> MicrokitUtil.getComponentIdPath(thread)
            case _ =>
          }
        }

        // Generate Rust constants for place bit positions
        var placeConstants: ISZ[ST] = ISZ()
        for (pi <- nextRel.places) {
          val bit = placeBits.get(pi).get
          val constName = ops.StringOps(pi.name).toUpper
          placeConstants = placeConstants :+ st"const PLACE_${constName}: u64 = 1u64 << $bit;"
        }

        // Generate transition mask constants and the cascade array.
        // Group component transitions by channel so that components scheduled
        // multiple times per hyperperiod get a single match arm that checks
        // which transition is enabled.
        var cpTransitions: ISZ[ST] = ISZ()
        var channelTransitions: Map[String, ISZ[(ST, ST)]] = Map.empty

        for (i <- z"0" until nextRel.transitions.size) {
          val t = nextRel.transitions(i)
          val inMask = computeMask(t.inPlaces)
          val outMask = computeMask(t.outPlaces)

          t.kind match {
            case ScheduleNextRel.TransitionKind.ControlPoint =>
              cpTransitions = cpTransitions :+ st"($inMask, $outMask)"

            case ScheduleNextRel.TransitionKind.Component =>
              t.inPlaces match {
                case ISZ(inPlace) =>
                  nextRel.activationMap.get(inPlace) match {
                    case Some(compRef) =>
                      val compName = ScheduleNextRel.getComponentName(compRef)
                      aliasToThreadId.get(compName) match {
                        case Some(threadId) =>
                          val existing = channelTransitions.getOrElse(threadId, ISZ())
                          channelTransitions = channelTransitions + threadId ~> (existing :+ ((inMask, outMask)))
                        case _ =>
                      }
                    case _ =>
                  }
                case _ =>
              }
          }
        }

        // Build a flat array of (channel, in_mask, out_mask) for all component
        // transitions — used by the schedule conformance validator
        var componentTransitionEntries: ISZ[ST] = ISZ()
        for (entry <- channelTransitions.entries) {
          val threadId = entry._1
          for (t <- entry._2) {
            componentTransitionEntries = componentTransitionEntries :+
              st"(${threadId}_MON, ${t._1}, ${t._2})"
          }
        }

        var componentMatchArms: ISZ[ST] = ISZ()
        for (entry <- channelTransitions.entries) {
          val threadId = entry._1
          val transitions = entry._2
          if (transitions.size == z"1") {
            val inMask = transitions(0)._1
            val outMask = transitions(0)._2
            componentMatchArms = componentMatchArms :+
              st"""${threadId}_MON => {
                  |  self.sys_assert_ready = (self.sys_assert_ready & !$inMask) | $outMask;
                  |}"""
          } else {
            var ifChain: ISZ[ST] = ISZ()
            for (j <- z"0" until transitions.size) {
              val inMask = transitions(j)._1
              val outMask = transitions(j)._2
              val keyword: String = if (j == z"0") "if" else "} else if"
              ifChain = ifChain :+
                st"""$keyword self.sys_assert_ready & $inMask != 0 {
                    |  self.sys_assert_ready = (self.sys_assert_ready & !$inMask) | $outMask;"""
            }
            componentMatchArms = componentMatchArms :+
              st"""${threadId}_MON => {
                  |  ${(ifChain, "\n")}
                  |  }
                  |}"""
          }
        }

        // Start and end constant names
        val startConst = placeConstName(nextRel.startPlace)
        val endConst = placeConstName(nextRel.endPlace)

        // Build substitution map: port alias name → API getter call.
        // State var aliases use the same pattern as port aliases.
        var portSubstitutions: Map[String, String] = Map.empty
        for (pa <- composition.portAliases) {
          val pathSegments = pa.portPath.name
          if (pathSegments.size >= z"2") {
            val componentRef = pathSegments(0)
            val portName = pathSegments(pathSegments.size - 1)
            aliasToThreadId.get(componentRef) match {
              case Some(threadId) =>
                portSubstitutions = portSubstitutions + pa.name ~> s"api.get_${threadId}_${portName}()"
              case _ =>
                // Component ref might be a direct subcomponent, not an alias
                portSubstitutions = portSubstitutions + pa.name ~> s"api.get_UNRESOLVED_${pa.name}()"
            }
          }
        }
        for (sva <- composition.stateVarAliases) {
          val pathSegments = sva.stateVarPath.name
          if (pathSegments.size >= z"2") {
            val componentRef = pathSegments(0)
            val svName = pathSegments(pathSegments.size - 1)
            aliasToThreadId.get(componentRef) match {
              case Some(threadId) =>
                portSubstitutions = portSubstitutions + sva.name ~> s"api.get_${threadId}_sv_${svName}()"
              case _ =>
                portSubstitutions = portSubstitutions + sva.name ~> s"api.get_UNRESOLVED_${sva.name}()"
            }
          }
        }

        // Transpile assertion expressions to Rust
        val rootSystem = symbolTable.rootSystem
        val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

        // Per-property checks over shared place observations (design D8): each
        // property's bindings resolve against the net and are checked when the
        // bound place appears in the walk; violations are attributed
        // (property, point) -- the same coordinates as the proof crate's VC names,
        // so a runtime violation maps onto the proof chain whose TCB assumptions
        // broke.
        var assertionChecks: ISZ[ST] = ISZ()
        for (property <- composition.properties) {
          val decoration = ScheduleNextRel.decorate(nextRel, property, reporter)
          for (e <- decoration.entries) {
            val placeId = e._1
            val b = e._2
            val constName = ops.StringOps(placeId.name).toUpper

            val rustExp = SlangExpUtil.rewriteExpH(
              rexp = b.exp,
              owner = rootSystem.classifier,
              optComponent = Some(rootSystem),
              context = SlangExpUtil.Context.compute_clause,
              substitutions = portSubstitutions,
              inRequires = F,
              inVerus = T,
              tp = crustTypeProvider,
              aadlTypes = types,
              store = localStore,
              reporter = reporter)

            assertionChecks = assertionChecks :+
              st"""if visited & PLACE_${constName} != 0 {
                  |  if !($rustExp) {
                  |    log::warn!("*** SYS ASSERT VIOLATION: property ${property.id}, ${b.point.prettyST.render} ***");
                  |  }
                  |}"""
          }
        }

        val externalBodyAttr: String =
          if (options.verusAttributeSyntax) "#[verus_verify(external_body)]"
          else "#[verifier::external_body]"

        // Build the cascade function as a module-level item
        val cascadeFn = RAST.ItemST(
          st"""${(placeConstants, "\n")}
              |
              |const CP_TRANSITIONS: [(u64, u64); ${cpTransitions.size}] = [
              |  ${(cpTransitions, ",\n")}
              |];
              |
              |/// Fires all enabled control-point transitions in the system contract's
              |/// Petri net until no more are enabled.
              |///
              |/// The system contract is modeled as a workflow net (WF-net) where places
              |/// represent assertion points and transitions are either component executions
              |/// or control points (splits/joins). Component transitions are fired
              |/// explicitly when a component executes; control-point transitions fire
              |/// automatically and instantaneously.
              |///
              |/// The marking is a u64 bitmask where each bit corresponds to a place.
              |/// A control-point transition is enabled when all its in-place bits are set.
              |/// Firing removes the in-place bits and sets the out-place bits:
              |///   ready = (ready & !in_mask) | out_mask
              |///
              |/// For a split (1 in-place -> N out-places), this fans one token into N.
              |/// For a join (N in-places -> 1 out-place), this merges N tokens into one
              |/// and only fires when all N branches have completed.
              |///
              |/// # Arguments
              |/// * `ready` - the current marking (bitmask of places holding a token)
              |///
              |/// # Returns
              |/// The steady-state marking after all enabled control-point transitions
              |/// have been fired.
              |$externalBodyAttr
              |fn sys_assert_cascade(mut ready: u64) -> u64 {
              |  let mut changed = true;
              |  while changed {
              |    changed = false;
              |    for &(in_mask, out_mask) in CP_TRANSITIONS.iter() {
              |      if (ready & in_mask) == in_mask {
              |        ready = (ready & !in_mask) | out_mask;
              |        changed = true;
              |      }
              |    }
              |  }
              |  ready
              |}
              |
              |/// Same as [`sys_assert_cascade`] but also returns the accumulated set of
              |/// all places that held a token at any point during the cascade.
              |///
              |/// When a component fires, the subsequent cascade may pass through several
              |/// intermediate markings before reaching a steady state. For example, after
              |/// the last branch of a split completes, the join fires, then the next
              |/// assertion's pass-through fires, then possibly another split fires — all
              |/// instantaneously. Each intermediate marking contains assertion places
              |/// that should be checked. The accumulated bitmask is the union of all
              |/// these intermediate markings, so the monitor can check every assertion
              |/// that was active at any point during the cascade.
              |///
              |/// # Arguments
              |/// * `ready` - the current marking (bitmask of places holding a token)
              |///
              |/// # Returns
              |/// A tuple of `(final_marking, accumulated)` where `final_marking` is
              |/// the steady-state marking (same as [`sys_assert_cascade`] would return)
              |/// and `accumulated` is the bitwise OR of every intermediate marking
              |/// observed during the cascade, including the initial `ready` value.
              |$externalBodyAttr
              |fn sys_assert_cascade_acc(mut ready: u64) -> (u64, u64) {
              |  let mut accumulated = ready;
              |  let mut changed = true;
              |  while changed {
              |    changed = false;
              |    for &(in_mask, out_mask) in CP_TRANSITIONS.iter() {
              |      if (ready & in_mask) == in_mask {
              |        ready = (ready & !in_mask) | out_mask;
              |        accumulated |= ready;
              |        changed = true;
              |      }
              |    }
              |  }
              |  (ready, accumulated)
              |}
              |
              |/// Component transitions as (channel, in_mask, out_mask) triples.
              |/// Used by the schedule conformance validator to look up which
              |/// transition to fire for a given channel.
              |const COMPONENT_TRANSITIONS: [(u32, u64, u64); ${componentTransitionEntries.size}] = [
              |  ${(componentTransitionEntries, ",\n")}
              |];
              |
              |/// Validates that the concrete schedule conforms to the system contract's
              |/// ordering constraints. Simulates the full Petri net walk through all
              |/// user partition timeslices in the schedule. For each user component,
              |/// checks that its transition is enabled (in-place tokens are present)
              |/// and fires it, cascading control points after each step. Reports any
              |/// component whose transition is not enabled and whether the walk reaches
              |/// the END place at the end of the hyperperiod.
              |///
              |/// # Arguments
              |/// * `sched` - the concrete schedule from the Microkit system description
              |$externalBodyAttr
              |fn validate_schedule_conformance(sched: &hamr::Schedule) {
              |  let n = sched.num_timeslices as usize;
              |  let mut ready: u64 = ${startConst};
              |  ready = sys_assert_cascade(ready);
              |  let mut violations = 0u32;
              |
              |  for i in 0..n {
              |    if !sched.is_user_partition[i] {
              |      continue;
              |    }
              |    let ch = sched.timeslice_ch[i];
              |
              |    let mut fired = false;
              |    for &(t_ch, in_mask, out_mask) in COMPONENT_TRANSITIONS.iter() {
              |      if t_ch == ch && (ready & in_mask) == in_mask {
              |        ready = (ready & !in_mask) | out_mask;
              |        ready = sys_assert_cascade(ready);
              |        fired = true;
              |        break;
              |      }
              |    }
              |
              |    if !fired {
              |      log::error!("*** SCHEDULE CONFORMANCE VIOLATION: no enabled transition for channel {} at timeslice {} ***", ch, i);
              |      violations += 1;
              |    }
              |  }
              |
              |  if ready != ${endConst} {
              |    log::error!("*** SCHEDULE CONFORMANCE VIOLATION: walk did not reach END (ready = 0x{:x}) ***", ready);
              |    violations += 1;
              |  }
              |
              |  if violations == 0 {
              |    log::info!("Schedule conformance check passed");
              |  } else {
              |    log::error!("Schedule conformance check failed with {} violation(s)", violations);
              |  }
              |}""")

        // Build the sys_assert_monitor method body. Uses its own
        // sys_assert_last_index sentinel (independent of gumbo_monitor's
        // last_index) so initialization and conformance checking run
        // correctly even though gumbo_monitor executes first.
        val sysAssertBody: ST =
          st"""let state = api.get_sched_state();
              |
              |if self.sys_assert_last_index == u32::MAX {
              |  let schedule = api.get_sched_schedule();
              |  buildUserChannelTables(
              |    &schedule, &mut self.prev_user_ch, &mut self.next_user_ch);
              |  validate_schedule_conformance(&schedule);
              |}
              |
              |let idx = state.current_timeslice as usize;
              |
              |if self.sys_assert_last_index == u32::MAX {
              |  // First compute phase — initialize ready set and cascade
              |  self.sys_assert_ready = ${startConst};
              |  self.sys_assert_ready = sys_assert_cascade(self.sys_assert_ready);
              |  self.sys_assert_last_index = state.current_timeslice;
              |  return;
              |}
              |
              |let prev_ch = self.prev_user_ch[idx];
              |
              |match prev_ch {
              |  ${(componentMatchArms, "\n")}
              |  _ => {
              |    self.sys_assert_last_index = state.current_timeslice;
              |    return;
              |  }
              |}
              |
              |// Fire splits/joins and collect all assertion places visited during the cascade
              |let (final_ready, visited) = sys_assert_cascade_acc(self.sys_assert_ready);
              |self.sys_assert_ready = final_ready;
              |
              |${(assertionChecks, "\n")}
              |
              |if self.sys_assert_ready == ${endConst} {
              |  self.sys_assert_ready = ${startConst};
              |  self.sys_assert_ready = sys_assert_cascade(self.sys_assert_ready);
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
        // add sys_assert_ready field init to new(), add sys_assert_monitor method
        var updatedImplItems: ISZ[RAST.Item] = ISZ()
        for (item <- existingImpl.items) {
          item match {
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "timeTriggered" =>
              val existingItems: ISZ[RAST.BodyItem] = fn.body match {
                case Some(mb) => mb.items
                case _ => ISZ()
              }
              updatedImplItems = updatedImplItems :+ fn(
                body = Some(RAST.MethodBody(existingItems :+
                  RAST.BodyItemST(st"self.sys_assert_monitor(api);"))))
            case fn: RAST.FnImpl if fn.sig.ident.prettyST.render == "new" =>
              var updatedBodyItems: ISZ[RAST.BodyItem] = ISZ()
              fn.body match {
                case Some(mb) =>
                  for (bi <- mb.items) {
                    bi match {
                      case bis: RAST.BodyItemSelf =>
                        updatedBodyItems = updatedBodyItems :+ bis(items = bis.items :+ st"sys_assert_last_index: u32::MAX," :+ st"sys_assert_ready: 0,")
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

        // Add the sys_assert_monitor method
        monitorMethodOpt match {
          case Some(monitorFn) =>
            updatedImplItems = updatedImplItems :+ monitorFn(
              sig = monitorFn.sig(ident = RAST.IdentString("sys_assert_monitor")),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(sysAssertBody)))))
          case _ =>
        }

        // Add sys_assert_ready field to struct
        val updatedStruct = monitorContrib.appStructDef(
          items = monitorContrib.appStructDef.items :+
            RAST.StructField(
              visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("sys_assert_last_index"),
              fieldType = RAST.TyPath(ISZ(ISZ("u32")), None())) :+
            RAST.StructField(
              visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("sys_assert_ready"),
              fieldType = RAST.TyPath(ISZ(ISZ("u64")), None())))

        val updatedImpl = existingImpl(items = updatedImplItems)

        // Transpile system-level GUMBO functions to Rust and emit as a separate
        // module so they stay outside the verus! macro (they use bitwise bool ops
        // via the implies!/impliesL! macros which Verus rejects)
        val (moduleResources, modAndUse) =
          getSystemGumboFunctions(symbolTable, options, types, monitorThreadPath, localStore, reporter)

        val updatedContrib = monitorContrib(
          appStructDef = updatedStruct,
          appStructImpl = updatedImpl,
          moduleLevelEntries = monitorContrib.moduleLevelEntries :+ cascadeFn,
          appModDirectives = monitorContrib.appModDirectives ++ modAndUse._1,
          appUses = monitorContrib.appUses ++ modAndUse._2)

        localStore = CRustComponentPlugin.putComponentContributions(
          contributions.replaceComponentContributions(
            contributions.componentContributions + monitorThreadPath ~> updatedContrib),
          localStore)

        resources = resources ++ moduleResources

      case _ =>
      }
    }

    return (localStore, resources)
  }

  @strictpure def placeConstName(p: ScheduleNextRel.PlaceId): String =
    s"PLACE_${ops.StringOps(p.name).toUpper}"

  @strictpure def computeMask(places: ISZ[ScheduleNextRel.PlaceId]): ST = {
    val parts: ISZ[ST] = for (p <- places) yield st"${placeConstName(p)}"
    if (parts.size == z"1") parts(0)
    else st"(${(parts, " | ")})"
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
                                     monitorThreadPath: ISZ[String],
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
              reporter.warn(None(), "GumboSysAssertMonitorPlugin", "Spec methods in system-level GUMBO subclauses are not yet supported")
          }
        }

        val monitorThread = symbolTable.componentMap.get(
          monitorThreadPath).get.asInstanceOf[AadlThread]
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
          path = s"$componentDir/${GumboSysAssertMonitorPlugin.sysAssertFunctionsModuleName}.rs",
          content = moduleContent,
          overwrite = T)

        val modName = GumboSysAssertMonitorPlugin.sysAssertFunctionsModuleName
        val modDirective: RAST.Item = RAST.ItemST(
          st"""#[path = "$modName.rs"]
              |pub mod $modName;""")
        val useItem: RAST.Item = RAST.Use(ISZ(), RAST.IdentString(s"$modName::*"))

        return (ISZ(moduleResource), (ISZ(modDirective), ISZ(useItem)))

      case _ => return (ISZ(), (ISZ(), ISZ()))
    }
  }
}

@datatype class DefaultGumboSysAssertMonitorPlugin extends GumboSysAssertMonitorPlugin {

  val name: String = "DefaultGumboSysAssertMonitorPlugin"
}
