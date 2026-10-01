// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlPortConnection, AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.monitors.{MonitorInjector, UserLandMonitorPlugin}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiPlugin, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.{CRustComponentPlugin, ComponentContributions}
import org.sireum.hamr.codegen.microkit.plugins.{ComponentGenProfile, MicrokitFinalizePlugin, MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclStateVar}
import org.sireum.message.Reporter

object GumboMonitorPlugin {




  val stateVarPortPrefix: String = "sv_"

  @strictpure def stateVarPortName(stateVarName: String): String =
    s"${stateVarPortPrefix}${stateVarName}"

  @strictpure def monitorStateVarPortName(threadId: String, stateVarName: String): String =
    s"${threadId}_${stateVarPortPrefix}${stateVarName}"

  val KEY_RUST_MONITORING: String = "KEY_RUST_MONITORING"


  @strictpure def getRustMonitoringStore(store: Store): Option[RustMonitoringStore] =
    store.get(KEY_RUST_MONITORING).asInstanceOf[Option[RustMonitoringStore]]


}

@datatype class RustMonitoringStateVarInfo(val name: String)

@datatype class RustMonitoringStore(val entries: HashSMap[ISZ[String], ISZ[RustMonitoringStateVarInfo]]) extends StoreValue

@sig trait GumboMonitorPlugin
  extends UserLandMonitorPlugin with MicrokitFinalizePlugin {

  @strictpure override def getMonitorName: String = "gumbo_monitor"

  // The gumbo and sys-assert monitors derive their behavior entirely from GUMBO
  // contracts; users aren't expected to edit them, and their bodies use exec-only
  // implication macros that can't live inside Verus. So they are fully generated:
  // plain Rust (no Verus), overwritten on regen (no markers), and no test harness.
  // (Inherited by GumboSysAssertMonitorPlugin.)
  @strictpure override def getMonitorGenProfile: ComponentGenProfile =
    ComponentGenProfile(verusVerified = F, userEditable = F, emitTestHarness = F)

  // Phase store keys derived from getMonitorName so each subtype gets its own namespace
  @strictpure def keyModelTransformed: String = s"KEY_${getMonitorName}_Model_Transformed"

  @strictpure def keyModelHandled: String = s"KEY_${getMonitorName}_Model_handled"

  @strictpure def keyRustFinalized: String = s"KEY_${getMonitorName}_RustFinalized"

  @strictpure def keyMonitorMethod: String = s"KEY_${getMonitorName}_MonitorMethod"

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(keyModelTransformed)

  @strictpure def hasHandled(store: Store): B = store.contains(keyModelHandled)

  @strictpure def haveRustFinalized(store: Store): B = store.contains(keyRustFinalized)

  @strictpure def haveHandledMonitorMethod(store: Store): B = store.contains(keyMonitorMethod)

  @strictpure override def getRetainedNonModelPorts(store: Store): ISZ[IdPath] =
    for (id <- StoreUtil.getSyntheticElements(store) if id.nonEmpty && ops.StringOps(id(id.lastIndex)).startsWith(GumboMonitorPlugin.stateVarPortPrefix)) yield id

  @pure override def canHandleModelTransform(model: Aadl,
                                             options: HamrCli.CodegenOption,
                                             types: AadlTypes,
                                             symbolTable: SymbolTable,
                                             store: Store,
                                             reporter: Reporter): B = {
    return (
      canHandleModelTransformHelper(model, options, types, symbolTable, store, reporter) &&
        hasThreadsWithStateVars(symbolTable) &&
        !haveHandledModelTransform(store))
  }

  override def handleModelTransform(origModel: Aadl,
                                    options: HamrCli.CodegenOption,
                                    origTypes: AadlTypes,
                                    origSymbolTable: SymbolTable,
                                    origStore: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    // Inject and wire one monitor per name: size-1 for the component-level gumbo
    // monitor, one per composition for the sys-assert monitor (design D8,
    // approach (i)). Setting KEY_UserLandMonitorPlugin_Model_Transformed
    // replicates the side effect of the prior super[UserLandMonitorPlugin].
    // handleModelTransform call. Each iteration re-resolves, so the shared
    // source-side sv_ ports (guarded by existingThreadFeatureNames against the
    // current symbol table) are created on the first iteration and reused after.
    var localStore: Store = origStore + keyModelTransformed ~> BoolValue(T) +
      UserLandMonitorPlugin.KEY_UserLandMonitorPlugin_Model_Transformed ~> BoolValue(T)
    var curModel = origModel
    var curTypes = origTypes
    var curSymbolTable = origSymbolTable

    for (mName <- monitorNames(origSymbolTable)) {
      val sysPath = curModel.components(0).identifier.name
      val mProcessPath = monitorProcessPathNamed(sysPath, mName)
      val mThreadPath = monitorThreadPathNamed(sysPath, mName)

      // monitor names are unique (gumbo_monitor; sys_<composition id>_monitor per
      // composition), so each monitor's crate drops the thread id's
      // <..>_process_<..>_thread suffix (crates/<monitorName>); only crate-level
      // names (crates/ dir, Cargo package, staticlib) are affected
      localStore = StoreUtil.putCrateNameOverride(mThreadPath, mName, localStore)

      injectMonitorPDNamed(curModel, mProcessPath, mThreadPath, options, curSymbolTable, localStore, reporter) match {
        case Some((s, m, t, st)) =>
          localStore = s
          curModel = m
          curTypes = t
          curSymbolTable = st
        case _ => return None()
      }

      wireMonitorStateVars(curModel, options, curSymbolTable, mProcessPath, mThreadPath, localStore, reporter) match {
        case Some((s, m, t, st)) =>
          localStore = s
          curModel = m
          curTypes = t
          curSymbolTable = st
        case _ => return None()
      }
    }

    return Some((localStore, curModel, curTypes, curSymbolTable))
  }

  // Wires each source thread's state-variable (sv_) ports to the given monitor:
  // source-side ports are created once (guarded by existingThreadFeatureNames
  // against the passed symbol table) and the monitor-side ports, delegations,
  // fan-out connections, and connection instances are created for this monitor.
  // Factored out of handleModelTransform so each monitor (one per composition
  // for the sys-assert monitor, design D8 (i)) is wired in turn.
  def wireMonitorStateVars(model: Aadl,
                           options: HamrCli.CodegenOption,
                           symbolTable: SymbolTable,
                           monitorProcessPath: IdPath,
                           monitorThreadPath: IdPath,
                           store: Store,
                           reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
        var localStore = store

        val system = model.components(0)
        val systemPath = system.identifier.name

        var updatedSubComponents: ISZ[ir.Component] = system.subComponents
        var additionalMonitorThreadFeatures: ISZ[ir.Feature] = ISZ()
        var additionalMonitorProcessFeatures: ISZ[ir.Feature] = ISZ()
        var additionalMonitorProcessConnections: ISZ[ir.Connection] = ISZ()
        var systemFanOutConnections: ISZ[ir.Connection] = ISZ()
        var systemConnectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

        // Track which source threads already have sv_ ports (from a prior
        // monitor plugin's transform). These are shared across monitors.
        val existingThreadFeatureNames: Set[String] = {
          var s = Set.empty[String]
          for (thread <- symbolTable.getThreads()) {
            for (f <- thread.component.features) {
              s = s + CommonUtil.getLastName(f.identifier)
            }
          }
          s
        }

        // Track which ports the current monitor thread already has (from a
        // prior plugin that shares the same monitor thread, if any)
        val existingMonitorFeatureNames: Set[String] = {
          var s = Set.empty[String]
          symbolTable.componentMap.get(monitorThreadPath) match {
            case Some(monThread) =>
              for (f <- monThread.component.features) {
                s = s + CommonUtil.getLastName(f.identifier)

              }
            case _ =>
          }
          s
        }

        for (thread <- symbolTable.getThreads()) {
          val stateVars = getStateVars(thread.path, symbolTable)
          if (stateVars.nonEmpty) {
            val srcProcess = thread.getParent(symbolTable)

            var additionalThreadFeatures: ISZ[ir.Feature] = ISZ()
            var additionalProcessFeatures: ISZ[ir.Feature] = ISZ()
            var additionalProcessConnections: ISZ[ir.Connection] = ISZ()

            for (sv <- stateVars) {
              val svPortName = GumboMonitorPlugin.stateVarPortName(sv.name)
              val monPortName = GumboMonitorPlugin.monitorStateVarPortName(MicrokitUtil.getComponentIdPath(thread), sv.name)
              val classifier = Some(ir.Classifier(sv.classifier))
              val threadPortPath: ISZ[String] = thread.path :+ svPortName
              val processPortPath: ISZ[String] = srcProcess.path :+ svPortName

              // Thread-side and process-side sv_ ports are shared across monitor instances and
              // normally created already by StateVarPortsPlugin -- only add them if missing
              if (!existingThreadFeatureNames.contains(svPortName)) {
                val (tf, pf, deleg) = StateVarPortsPlugin.sourcePortElements(thread.path, srcProcess.path, sv)
                localStore = StoreUtil.addSyntheticElement(threadPortPath, localStore)
                additionalThreadFeatures = additionalThreadFeatures :+ tf
                additionalProcessFeatures = additionalProcessFeatures :+ pf
                additionalProcessConnections = additionalProcessConnections :+ deleg
              }

              // Monitor-side ports, delegations, fan-out connections, and
              // connection instances — skip if a prior monitor plugin already
              // added them to this monitor thread
              if (!existingMonitorFeatureNames.contains(monPortName)) {

                // Input data port on monitor thread
                val monitorThreadPortPath: ISZ[String] = monitorThreadPath :+ monPortName

                // Input data port on monitor process
                val monitorProcessPortPath: ISZ[String] = monitorProcessPath :+ monPortName

                additionalMonitorThreadFeatures = additionalMonitorThreadFeatures :+ ir.FeatureEnd(
                  identifier = ir.Name(name = monitorThreadPortPath, pos = None()),
                  direction = ir.Direction.In,
                  category = ir.FeatureCategory.DataPort,
                  classifier = classifier,
                  properties = ISZ(),
                  uriFrag = "")

                additionalMonitorProcessFeatures = additionalMonitorProcessFeatures :+ ir.FeatureEnd(
                  identifier = ir.Name(name = monitorProcessPortPath, pos = None()),
                  direction = ir.Direction.In,
                  category = ir.FeatureCategory.DataPort,
                  classifier = classifier,
                  properties = ISZ(),
                  uriFrag = "")

                // Delegation: monitorProcess.port → monitorThread.port (In-to-In going down)
                val monitorDelegConnName: ISZ[String] = monitorProcessPath :+ s"deleg_${monPortName}"
                additionalMonitorProcessConnections = additionalMonitorProcessConnections :+
                  ir.Connection(
                    name = ir.Name(name = monitorDelegConnName, pos = None()),
                    src = ISZ(ir.EndPoint(
                      component = ir.Name(name = monitorProcessPath, pos = None()),
                      feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                      direction = Some(ir.Direction.In))),
                    dst = ISZ(ir.EndPoint(
                      component = ir.Name(name = monitorThreadPath, pos = None()),
                      feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                      direction = Some(ir.Direction.In))),
                    kind = ir.ConnectionKind.Port,
                    isBiDirectional = F,
                    connectionInstances = ISZ(),
                    properties = ISZ(),
                    uriFrag = "")

                // System-level fan-out: srcProcess.svPort → monitorProcess.port
                val systemFanOutConnName: ISZ[String] = systemPath :+ s"sv_mon_${monPortName}"
                systemFanOutConnections = systemFanOutConnections :+
                  ir.Connection(
                    name = ir.Name(name = systemFanOutConnName, pos = None()),
                    src = ISZ(ir.EndPoint(
                      component = ir.Name(name = srcProcess.path, pos = None()),
                      feature = Some(ir.Name(name = processPortPath, pos = None())),
                      direction = Some(ir.Direction.Out))),
                    dst = ISZ(ir.EndPoint(
                      component = ir.Name(name = monitorProcessPath, pos = None()),
                      feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                      direction = Some(ir.Direction.In))),
                    kind = ir.ConnectionKind.Port,
                    isBiDirectional = F,
                    connectionInstances = ISZ(),
                    properties = ISZ(),
                    uriFrag = "")

                // ConnectionInstance: srcThread.svPort → monitorThread.port
                val connInstNameStr: String =
                  st"${(threadPortPath, ".")} -> ${(monitorThreadPortPath, ".")}".render
                systemConnectionInstances = systemConnectionInstances :+
                  ir.ConnectionInstance(
                    name = ir.Name(name = ISZ(connInstNameStr), pos = None()),
                    src = ir.EndPoint(
                      component = ir.Name(name = thread.path, pos = None()),
                      feature = Some(ir.Name(name = threadPortPath, pos = None())),
                      direction = Some(ir.Direction.Out)),
                    dst = ir.EndPoint(
                      component = ir.Name(name = monitorThreadPath, pos = None()),
                      feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                      direction = Some(ir.Direction.In)),
                    kind = ir.ConnectionKind.Port,
                    connectionRefs = ISZ(
                      ir.ConnectionReference(
                        name = ir.Name(name = systemFanOutConnName, pos = None()),
                        context = ir.Name(name = systemPath, pos = None()),
                        isParent = T),
                      ir.ConnectionReference(
                        name = ir.Name(name = monitorDelegConnName, pos = None()),
                        context = ir.Name(name = monitorProcessPath, pos = None()),
                        isParent = F)),
                    properties = ISZ())
              } // end if (!existingFeatureNames.contains(monPortName))
            }

            // Update the source thread and process with the new state var ports
            updatedSubComponents = StateVarPortsPlugin.updateThreadInModel(
              subComponents = updatedSubComponents,
              processPath = srcProcess.path,
              threadPath = thread.path,
              additionalThreadFeatures = additionalThreadFeatures,
              additionalProcessFeatures = additionalProcessFeatures,
              additionalProcessConnections = additionalProcessConnections)
          }
        }

        // Update the monitor process with the new state var ports
        updatedSubComponents = updateMonitorProcess(
          subComponents = updatedSubComponents,
          monitorProcessPath = monitorProcessPath,
          monitorThreadPath = monitorThreadPath,
          additionalMonitorThreadFeatures = additionalMonitorThreadFeatures,
          additionalMonitorProcessFeatures = additionalMonitorProcessFeatures,
          additionalMonitorProcessConnections = additionalMonitorProcessConnections)

        val updatedSystem = system(
          subComponents = updatedSubComponents,
          connections = system.connections ++ systemFanOutConnections,
          connectionInstances = system.connectionInstances ++ systemConnectionInstances)
        val updatedModel = model(components = ISZ(updatedSystem))

        if (!reporter.hasError) {
          val reResult = ModelUtil.resolve(updatedModel, updatedModel.components(0).identifier.pos, "", options, localStore, reporter)
          localStore = reResult._2
          if (reResult._1.nonEmpty) {
            return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
          }
        }
        return None()
  }


  // The plugin's handle method executes in 2 phases, each gated by a store key
  // so it runs exactly once per phase. Multiple phases are needed because each
  // depends on contributions from other plugins that run between phases.  The
  // thread side of state-variable observation (is_monitoring_enabled() and the
  // guarded put_sv_* methods) is StateVarPortsPlugin's, shared with system testing.
  //
  // Phase 2 (UserLand): Runs after the connection store is ready. Delegates to
  //   UserLandMonitorPlugin.handle to inject the monitor protection domain, its
  //   channels, and shared memory regions into the Microkit system description.
  //
  // Phase 3 (Monitor Method): Runs after GumboXRustPlugin and CRustComponentPlugin
  //   have produced their contributions. Modifies the monitor thread's Rust
  //   ComponentContributions to add scheduling state fields to the struct, update
  //   new() and timeTriggered(), and append the monitor method with the per-component
  //   contract checking dispatch logic.
  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {

    val commonBase =
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        options.runtimeMonitoring &&
        haveHandledModelTransform(store)

    if (!commonBase) {
      return F
    } else {
      val canDoUserLandHandle: B =
        !hasHandled(store) &&
          canHandleHelper(model, options, types, symbolTable, store, reporter)

      val canDoMonitorMethod: B =
        !haveHandledMonitorMethod(store) &&
          GumboXRustPlugin.getGumboXContributions(store).nonEmpty &&
          ContractObserverPlugin.hasObservers(store) &&
          CRustComponentPlugin.hasCRustComponentContributions(store)

      return canDoUserLandHandle || canDoMonitorMethod
    }
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                            symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store
    var resources: ISZ[Resource] = ISZ()

    if (!hasHandled(localStore) &&
      canHandleHelper(model, options, types, symbolTable, localStore, reporter)) {
      localStore = localStore + keyModelHandled ~> BoolValue(T)

      // delegate to UserLandMonitorPlugin.handle to inject the monitor protection domain, its
      // channels, and shared memory regions into the Microkit system description.
      val s = super[UserLandMonitorPlugin].handle(model, options, types, symbolTable, localStore, reporter)

      localStore = s._1
      resources = resources ++ s._2
    }

    if (!haveHandledMonitorMethod(localStore) &&
      GumboXRustPlugin.getGumboXContributions(localStore).nonEmpty &&
      CRustComponentPlugin.hasCRustComponentContributions(localStore)) {
      val r = handleMonitorMethod(model, options, types, symbolTable, localStore, reporter)
      localStore = r._1
      resources = resources ++ r._2
    }

    return (localStore, resources)
  }

  // Phase 3: Modifies the monitor thread's Rust ComponentContributions to add
  // per-component contract checking. Adds scheduling state fields (frame_period,
  // last_index, prev/next user channel tables) and pre-state fields to the struct,
  // updates new() with their initializers, replaces timeTriggered() to call
  // self.gumbo_monitor(api), and appends the gumbo_monitor method. The method dispatches
  // on prev_user_ch to run post-condition checks and on next_user_ch to capture
  // pre-state for each component. Also appends buildUserChannelTables as a
  // module-level function.
  // The contract checks themselves live in crates/observers (ContractObserverPlugin): this
  // monitor only locates itself in the schedule -- which thread just yielded, which runs
  // next, and whether this is its first run -- and hands those to the shared
  // ComponentContracts, reading through the monitor's own API (MonitorView) and logging
  // through LogSink exactly as before.
  @pure def handleMonitorMethod(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store + keyMonitorMethod ~> BoolValue(T)

    val gumboxContribs = GumboXRustPlugin.getGumboXContributions(localStore).get
    val info = ContractObserverPlugin.getInfo(localStore).get
    val obs = ContractObserverPlugin.crateName

    if (gumboxContribs.allComponentContributions.nonEmpty) {
      // One monitor component per name: size-1 for the gumbo monitor, one per
      // composition for the sys-assert monitor (design D8, approach (i)).
      // Re-fetch contributions each iteration since the prior iteration updated
      // a (different) monitor component.
      for (monitorName <- monitorNames(symbolTable)) {
      val monitorThreadPath: ISZ[String] = monitorThreadPathNamed(symbolTable.rootSystem.path, monitorName)
      val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
      contributions.componentContributions.get(monitorThreadPath) match {
        case Some(monitorContrib) =>
          // Plain-Rust (non-Verus) monitors get no external_body directive.
          val monitorVerus: B = getMonitorGenProfile.verusVerified
          val externalBodyAttr: String =
            if (!monitorVerus) ""
            else if (options.verusAttributeSyntax) "#[verus_verify(external_body)]"
            else "#[verifier::external_body]"

          val monitorThread = symbolTable.componentMap.get(monitorThreadPath).get.asInstanceOf[AadlThread]
          val monitorThreadId = MicrokitUtil.getComponentIdPath(monitorThread)
          val appApiType = CRustApiPlugin.applicationApiType(monitorThread)

          val externalBodyAttrContent: ST =
            if (options.verusAttributeSyntax) st"verus_verify(external_body)"
            else st"verifier::external_body"
          val externalBodyAttributes: ISZ[RAST.Attribute] =
            if (!monitorVerus) ISZ()
            else ISZ(RAST.AttributeST(inner = F, content = externalBodyAttrContent))

          val monitorMethod = RAST.FnImpl(
            sig = RAST.FnSig(
              ident = RAST.IdentString("gumbo_monitor"),
              generics = Some(RAST.Generics(ISZ(RAST.GenericParam(
                ident = RAST.IdentString("API"),
                attributes = ISZ(),
                bounds = RAST.GenericBoundFixMe(st"${monitorThreadId}_Full_Api"))))),
              fnDecl = RAST.FnDecl(
                inputs = ISZ(
                  RAST.ParamFixMe(st"&mut self"),
                  RAST.ParamImpl(
                    ident = RAST.IdentString("api"),
                    kind = RAST.TyRef(None(), RAST.MutTy(
                      ty = RAST.TyPath(ISZ(ISZ(appApiType), ISZ("API")), None()), mutbl = RAST.Mutability.Mut)))),
                outputs = RAST.FnRetTyDefault()),
              verusHeader = None(), fnHeader = RAST.FnHeader(F)),
            comments = ISZ(), attributes = externalBodyAttributes, visibility = RAST.Visibility.Public, meta = ISZ(),
            verusAttributeSyntax = options.verusAttributeSyntax && monitorVerus, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""let state = api.get_sched_state();
                  |
                  |if self.last_index == u32::MAX {
                  |  let schedule = api.get_sched_schedule();
                  |  buildUserChannelTables(
                  |    &schedule, &mut self.prev_user_ch, &mut self.next_user_ch);
                  |}
                  |
                  |// Detect schedule wraparound: if the current timeslice index is not
                  |// greater than the last one seen, the schedule has started a new frame
                  |if state.current_timeslice <= self.last_index {
                  |  self.frame_period = self.frame_period + 1;
                  |}
                  |
                  |let idx = state.current_timeslice as usize;
                  |let mut view = MonitorView { api: api, focus: None };
                  |
                  |if self.last_index == u32::MAX {
                  |  // First compute phase, check initialization guarantees
                  |  self.components.on_init(&mut view, &mut LogSink);
                  |  init_checked();
                  |} else if let Some(prev) = thread_of(self.prev_user_ch[idx]) {
                  |  // the thread that just yielded: check its post-condition
                  |  self.components.on_complete(prev, &mut view, &mut LogSink);
                  |}
                  |
                  |// the thread that runs next: save its pre-state, check its pre-condition
                  |if let Some(next) = thread_of(self.next_user_ch[idx]) {
                  |  self.components.on_dispatch(next, &mut view, &mut LogSink);
                  |}
                  |
                  |self.last_index = state.current_timeslice;""")))))

          // Scheduling state struct fields, and the shared contract checks
          val schedFields: ISZ[RAST.Item] = ISZ(
            RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("frame_period"),
              fieldType = RAST.TyPath(ISZ(ISZ("i32")), None())),
            RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("last_index"),
              fieldType = RAST.TyPath(ISZ(ISZ("u32")), None())),
            RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("prev_user_ch"),
              fieldType = RAST.TyPath(ISZ(ISZ("hamr", "ScheduleChannels")), None())),
            RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("next_user_ch"),
              fieldType = RAST.TyPath(ISZ(ISZ("hamr", "ScheduleChannels")), None())),
            RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
              ident = RAST.IdentString("components"),
              fieldType = RAST.TyPath(ISZ(ISZ(obs, "components", "ComponentContracts")), None())))

          val updatedStruct = monitorContrib.appStructDef(
            items = monitorContrib.appStructDef.items ++ schedFields)

          // Initializer values for new()
          val allInits: ISZ[ST] = ISZ(
            st"frame_period: 0,",
            st"last_index: u32::MAX,",
            st"prev_user_ch: [0; hamr::hamr_ScheduleChannels_DIM_0],",
            st"next_user_ch: [0; hamr::hamr_ScheduleChannels_DIM_0],",
            st"components: $obs::components::ComponentContracts::new(),")

          // Update impl: replace new() and timeTriggered bodies, append monitor method
          val existingImpl = monitorContrib.appStructImpl.asInstanceOf[RAST.ImplBase]
          var updatedImplItems: ISZ[RAST.Item] = ISZ()
          for (item <- existingImpl.items) {
            item match {
              case fn: RAST.FnImpl =>
                if (fn.sig.ident.prettyST.render == "new") {
                  updatedImplItems = updatedImplItems :+ fn(
                    body = Some(RAST.MethodBody(ISZ(RAST.BodyItemSelf(allInits)))))
                } else if (fn.sig.ident.prettyST.render == "timeTriggered") {
                  updatedImplItems = updatedImplItems :+ fn(
                    body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                      st"""${ContractObserverPlugin.beginMonitorRunCall}
                          |self.gumbo_monitor(api);""")))))
                } else {
                  updatedImplItems = updatedImplItems :+ item
                }
              case _ =>
                updatedImplItems = updatedImplItems :+ item
            }
          }
          updatedImplItems = updatedImplItems :+ monitorMethod

          val updatedImpl = existingImpl(items = updatedImplItems)

          // buildUserChannelTables standalone function
          val buildUserChannelTablesFn = RAST.ItemST(
            st"""// For each timeslice index, finds the nearest preceding and following user
                |// partition channels. This lets the monitor know which thread just yielded
                |// (prev) and which will run next (next) so it can check post-conditions
                |// and capture pre-state at the right time.
                |$externalBodyAttr
                |pub fn buildUserChannelTables(
                |  sched: &hamr::Schedule,
                |  prev: &mut hamr::ScheduleChannels,
                |  next: &mut hamr::ScheduleChannels)
                |{
                |  let n = sched.num_timeslices as usize;
                |  for i in 0..n {
                |    let mut found_prev = false;
                |    let mut found_next = false;
                |    for offset in 1..n {
                |      if !found_prev {
                |        let backward = (i + n - offset) % n;
                |        if sched.is_user_partition[backward] {
                |          prev[i] = sched.timeslice_ch[backward];
                |          found_prev = true;
                |        }
                |      }
                |      if !found_next {
                |        let forward = (i + offset) % n;
                |        if sched.is_user_partition[forward] {
                |          next[i] = sched.timeslice_ch[forward];
                |          found_next = true;
                |        }
                |      }
                |      if found_prev && found_next {
                |        break;
                |      }
                |    }
                |  }
                |}""")

          val adapters = RAST.ItemST(ContractObserverPlugin.monitorAdapterItems(monitorThreadId, appApiType, info))

          val updatedContrib = monitorContrib(
            appStructDef = updatedStruct,
            appStructImpl = updatedImpl,
            moduleLevelEntries = monitorContrib.moduleLevelEntries :+ buildUserChannelTablesFn :+ adapters,
            crateDependencies = monitorContrib.crateDependencies :+ ContractObserverPlugin.crateDependency)

          localStore = CRustComponentPlugin.putComponentContributions(
            contributions.replaceComponentContributions(
              contributions.componentContributions + monitorThreadPath ~> updatedContrib),
            localStore)

        case _ =>
      }
      }
    }

    return (localStore, ISZ())
  }

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                         symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !isDisabled(store) &&
        GumboMonitorPlugin.getRustMonitoringStore(store).nonEmpty &&
        CRustComponentPlugin.hasCRustComponentContributions(store) &&
        store.contains(s"FINALIZED_DefaultCRustComponentPlugin") &&
        !haveRustFinalized(store))
  }

  // The GUMBOX and container modules the monitor crates used to carry in src/gumbox now
  // live in crates/observers (ContractObserverPlugin), shared with the test controller.
  // Nothing here writes a crate's src/lib.rs either: the observation points that post
  // state vars to shared memory are contributed to CRustComponentPlugin during handle.
  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                      symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    return (store + keyRustFinalized ~> BoolValue(T), ISZ())
  }

  @pure def hasThreadsWithStateVars(symbolTable: SymbolTable): B = {
    for (thread <- symbolTable.getThreads()) {
      if (getStateVars(thread.path, symbolTable).nonEmpty) {
        return T
      }
    }
    return F
  }

  @pure def getStateVars(threadPath: ISZ[String], symbolTable: SymbolTable): ISZ[GclStateVar] = {
    symbolTable.annexClauseInfos.get(threadPath) match {
      case Some(clauses) =>
        for (clause <- clauses) {
          clause match {
            case gclInfo: GclAnnexClauseInfo =>
              return gclInfo.annex.state
            case _ =>
          }
        }
        return ISZ()
      case _ => return ISZ()
    }
  }

  @pure def updateMonitorProcess(subComponents: ISZ[ir.Component],
                                 monitorProcessPath: ISZ[String],
                                 monitorThreadPath: ISZ[String],
                                 additionalMonitorThreadFeatures: ISZ[ir.Feature],
                                 additionalMonitorProcessFeatures: ISZ[ir.Feature],
                                 additionalMonitorProcessConnections: ISZ[ir.Connection]): ISZ[ir.Component] = {
    var result: ISZ[ir.Component] = ISZ()
    for (comp <- subComponents) {
      if (comp.identifier.name == monitorProcessPath) {
        var updatedSubs: ISZ[ir.Component] = ISZ()
        for (sub <- comp.subComponents) {
          if (sub.identifier.name == monitorThreadPath) {
            updatedSubs = updatedSubs :+ sub(features = sub.features ++ additionalMonitorThreadFeatures)
          } else {
            updatedSubs = updatedSubs :+ sub
          }
        }
        result = result :+ comp(
          features = comp.features ++ additionalMonitorProcessFeatures,
          subComponents = updatedSubs,
          connections = comp.connections ++ additionalMonitorProcessConnections)
      } else {
        result = result :+ comp
      }
    }
    return result
  }
}

@datatype class DefaultGumboMonitorPlugin extends GumboMonitorPlugin {

  val name: String = "DefaultGumboMonitorPlugin"
}
