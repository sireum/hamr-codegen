// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.sysvc.{ScheduleNextRel, VCGenerator}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.testing.TestSchedulerPlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, RustUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclBodyMethod, GclComposition, GclSpecMethod}
import org.sireum.message.Reporter

// Stage 7 of TestScheduler-design.md, steps 1-2: the GUMBO contract checks and the
// system-assertion checks, generated once into crates/observers as two layers --
// ComponentContracts and one SysAssert_<id> per composition -- generic over SystemView
// (how a consumer reads ports and state variables) and ViolationSink (what it does with
// a violation).  The gumbo and sys-assert monitor PDs are thin wrappers over them, and the
// test controller hosts them too (step 4).
//
// Threads are identified by the generated `Thread` enum rather than by channel id:
// channel ids belong to an MSD variant, and the crate is shared by all of them.  Each
// consumer maps its own channels onto Thread.

/** A composition's alias of a CONNECTED input port `reader.port`: the system assertions read
  * what `reader` received there -- latched at its dispatch -- not what the producer last sent,
  * which on a feedback edge is the following frame's.  `underlying` is the getter of the
  * region the reader reads through (named after the producer's output). */
@datatype class ReceivedGetter(val name: String,
                               val reader: String,
                               val underlying: String,
                               val isEvent: B)

@datatype class ContractObserverInfo(val threads: ISZ[String],
                                     val hasComponentLayer: B,
                                     val compositionIds: ISZ[String],
                                     // the (non-abstract) property ids of each composition, in
                                     // compositionIds order
                                     val compositionProperties: ISZ[ISZ[String]],
                                     // (getter name, rust return type): every SystemView method
                                     val getters: ISZ[(String, String)],
                                     // the getters that read an event or event-data port
                                     val eventGetters: ISZ[String],
                                     // those of eventGetters that read a pure event port: typed
                                     // Option<T> like an event-data port, but polled through a
                                     // monitor API that returns bool
                                     val pureEventGetters: ISZ[String],
                                     // aliases of connected inputs, read as the reader received them
                                     val receivedGetters: ISZ[ReceivedGetter]) extends StoreValue {
  @strictpure def receivedNames: ISZ[String] = for (g <- receivedGetters) yield g.name
  @strictpure def hasLayers: B = hasComponentLayer || compositionIds.nonEmpty
}

object ContractObserverPlugin {

  val KEY_ContractObserverPlugin: String = "KEY_ContractObserverPlugin"

  val crateName: String = "observers"

  val sysAssertFunctionsModuleName: String = "sys_assert_functions"

  @strictpure def getInfo(store: Store): Option[ContractObserverInfo] =
    store.get(KEY_ContractObserverPlugin).asInstanceOf[Option[ContractObserverInfo]]

  @strictpure def hasObservers(store: Store): B = store.contains(KEY_ContractObserverPlugin)

  @strictpure def crateDirectory(options: HamrCli.CodegenOption): String =
    s"${options.sel4OutputDir.get}/crates/$crateName"

  @strictpure def crateDependency: ST = st"""$crateName = { path = "../$crateName" }"""

  @strictpure def sysAssertModuleName(compositionId: String): String = s"sys_$compositionId"

  @strictpure def sysAssertTypeName(compositionId: String): String = s"SysAssert_$compositionId"

  // Generated only when something consumes it: the gumbo / sys-assert monitor PDs, or the
  // test controller.  Ask whether a consumer was actually injected rather than re-deriving its
  // gate: a monitor needs --runtime-monitoring, GUMBO state variables AND the MCS user-land
  // scheduler, so testing only the first two gave a domain-scheduled model an observers crate
  // nothing used.  Model transforms run before this plugin, so the consumers' keys are already
  // in the store.  System testing alone requests it (design D22); the test scheduler is
  // MCS-only too.
  @pure def isRequested(store: Store): B = {
    return DefaultGumboMonitorPlugin().haveHandledModelTransform(store) ||
      DefaultGumboSysAssertMonitorPlugin().haveHandledModelTransform(store) ||
      TestSchedulerPlugin.hasTransformed(store)
  }

  // The monitor-API getter a contract parameter is read through, e.g. get_<threadId>_sv_<var>.
  @pure def getterName(param: GumboXRustUtil.GGParam,
                       threadId: String,
                       dstPortToMonitorPortName: Map[ISZ[String], String]): String = {
    param match {
      case sv: GumboXRustUtil.GGStateVarParam =>
        return s"get_${threadId}_sv_${sv.originName}"
      case pp: GumboXRustUtil.GGPortParam =>
        if (pp.isIn) {
          dstPortToMonitorPortName.get(pp.port.path) match {
            case Some(mn) => return s"get_$mn"
            case _ => return s"get_UNKNOWN_${pp.originName}"
          }
        } else {
          return s"get_${threadId}_${pp.originName}"
        }
      case _ => return "get_UNKNOWN"
    }
  }

  // The rust type a contract parameter's getter returns -- the type of the matching
  // PreState/PostState field (see GumboXRustPlugin.generateMonitorContainerContent).
  @pure def paramType(param: GumboXRustUtil.GGParam): String = {
    return if (param.isOptional) st"Option<${param.langType}>".render else param.langType.render
  }

  // Destination port path -> the monitor-side port name its value is observed through.
  // Connected inputs are observed at their producer's output; UNCONNECTED inputs are
  // observed directly (the monitor maps the consumer's own input region -- see
  // MonitorInjector), under the same <threadId>_<portId> naming.
  @pure def dstPortToMonitorPortName(symbolTable: SymbolTable, store: Store): Map[ISZ[String], String] = {
    var ret: Map[ISZ[String], String] = Map.empty
    for (conn <- symbolTable.aadlConnections) {
      conn match {
        case pc: AadlPortConnection =>
          val srcThread = pc.srcComponent.asInstanceOf[AadlThread]
          val portName = CommonUtil.getLastName(pc.srcFeature.feature.identifier)
          ret = ret + pc.dstFeature.path ~> s"${MicrokitUtil.getComponentIdPath(srcThread)}_$portName"
        case _ =>
      }
    }
    for (t <- symbolTable.getThreads() if !StoreUtil.isSynthetic(t.path, store)) {
      for (p <- t.getPorts()
           if p.direction == ir.Direction.In &&
             !symbolTable.inConnections.contains(p.path) &&
             !StoreUtil.isSynthetic(p.path, store)) {
        ret = ret + p.path ~> s"${MicrokitUtil.getComponentIdPath(t)}_${p.identifier}"
      }
    }
    return ret
  }

  @pure def modelThreadIds(symbolTable: SymbolTable, store: Store): ISZ[String] = {
    return for (t <- symbolTable.getThreads() if !StoreUtil.isSynthetic(t.path, store))
      yield MicrokitUtil.getComponentIdPath(t)
  }

  // How a monitor's timeTriggered starts each run: the schedule and the timeslice it runs
  // in let the adapter tell when a new frame has started.
  val beginMonitorRunCall: String = "begin_monitor_run(api);"

  // Module-level items for a monitor PD's app module: the channel -> Thread map, the
  // SystemView adapter over the monitor's API, and the sink that logs exactly what the
  // monitors logged before the checks moved into crates/observers.  Kept in the monitor's
  // own module so the log records keep their target.
  @pure def monitorAdapterItems(monitorThreadId: String,
                                appApiType: String,
                                info: ContractObserverInfo): ST = {
    val threadArms: ISZ[ST] = for (t <- info.threads) yield
      st"${t}_MON => Some($crateName::Thread::$t),"
    // The monitor's API dequeues on every call, through one cursor per port.
    //  - Data ports and state variables: the first read of a run is cached for the rest of
    //    it -- within a run the checks describe one instant.
    //  - Event ports: each reader (a thread whose check reads the port, or the system layer)
    //    sees an event once, at its first check after the event arrived -- the semantics the
    //    thread itself has.  One cursor cannot give that: the producer's check would consume
    //    the event before a consumer's check in a later run.  So every event port is polled
    //    into an event count at the start of each run, and each thread keeps the count it
    //    last saw (EventTrack).  The poll drains the port -- a region deeper than one holds
    //    every element sent since the last run -- and keeps the latest, as the controller
    //    does; taking one per run would leave the rest to arrive, as new events, later.
    //  - The system layer reads an event port as what its producer's latest dispatch in the
    //    current frame sent -- nothing, if it sent nothing -- whichever of its assertions reads
    //    it, at whichever place, as the test controller does.  Polling at the start of every
    //    run stamps an event with the frame it arrived in; producer_completed records a
    //    dispatch that sent nothing; the frame advances when the composition's frame ends
    //    (frame_ended -- a monitor checks one composition).
    val readers: Z = info.threads.size + 1
    var getterImpls: ISZ[ST] = ISZ()
    var cacheFields: ISZ[ST] = ISZ()
    var cacheInits: ISZ[ST] = ISZ()
    var tracks: ISZ[ST] = ISZ()
    var polls: ISZ[ST] = ISZ()
    var initMarks: ISZ[ST] = ISZ()
    var dispatchNotes: Map[String, ISZ[ST]] = Map.empty
    var completionNotes: Map[String, ISZ[ST]] = Map.empty
    val receivedNames: ISZ[String] = info.receivedNames
    for (g <- info.getters if !ops.ISZOps(receivedNames).contains(g._1)) {
      val name = g._1
      val ty = g._2
      if (ops.ISZOps(info.eventGetters).contains(name)) {
        // Every event getter is Option<T> (a pure event port's T is its empty payload, as
        // GUMBOX reads it); a pure event port's monitor API returns bool, so it is polled as a
        // flag.
        val isFlag: B = ops.ISZOps(info.pureEventGetters).contains(name)
        val inner: String = ops.StringOps(ty).substring(7, ty.size - 1) // Option<T> -> T
        polls = polls :+ (
          if (isFlag)
            st"""while api.$name() {
                |  EV_$name.seq += 1;
                |  EV_$name.value = Some(Default::default());
                |  EV_$name.sys_frame = Some(FRAME);
                |  EV_$name.sys_value = Some(Default::default());
                |}"""
          else
            st"""while let Some(v) = api.$name() {
                |  EV_$name.seq += 1;
                |  EV_$name.value = Some(v.clone());
                |  EV_$name.sys_frame = Some(FRAME);
                |  EV_$name.sys_value = Some(v);
                |}""")
        // the thread whose output this is (the getter is named after it): what it sent while
        // initializing is not its first dispatch's output
        var producer: String = ""
        for (t <- info.threads if ops.StringOps(name).startsWith(s"get_${t}_") && t.size > producer.size) {
          producer = t
        }
        if (producer.size > 0) {
          initMarks = initMarks :+ st"EV_$name.seen[$crateName::Thread::$producer as usize] = EV_$name.seq;"
          dispatchNotes = dispatchNotes + producer ~> (dispatchNotes.getOrElse(producer, ISZ()) :+
            st"EV_$name.dispatch_seq = EV_$name.seq;")
          completionNotes = completionNotes + producer ~> (completionNotes.getOrElse(producer, ISZ()) :+
            st"""if EV_$name.seq == EV_$name.dispatch_seq {
                |  EV_$name.sys_frame = Some(FRAME);
                |  EV_$name.sys_value = None;
                |}""")
        }
        val result: ST = st"value"
        tracks = tracks :+ st"static mut EV_$name: EventTrack<$inner> = EventTrack { seq: 0, dispatch_seq: 0, value: None, sys_frame: None, sys_value: None, seen: [0; READERS] };"
        cacheFields = cacheFields :+ st"$name: [Option<$ty>; READERS],"
        cacheInits = cacheInits :+ st"$name: [None; READERS],"
        getterImpls = getterImpls :+
          st"""fn $name(&mut self) -> $ty {
              |  let r = reader_index(self.focus);
              |  unsafe {
              |    if let Some(v) = &VIEW_CACHE.$name[r] {
              |      return v.clone();
              |    }
              |    let (present, value) = if r == READERS - 1 {
              |      let p = EV_$name.sys_frame == Some(FRAME) && EV_$name.sys_value.is_some();
              |      (p, if p { EV_$name.sys_value.clone() } else { None })
              |    } else {
              |      let p = EV_$name.seen[r] < EV_$name.seq;
              |      (p, if p { EV_$name.value.clone() } else { None })
              |    };
              |    let _ = (&present, &value);
              |    EV_$name.seen[r] = EV_$name.seq;
              |    let v: $ty = $result;
              |    VIEW_CACHE.$name[r] = Some(v.clone());
              |    v
              |  }
              |}"""
      } else {
        cacheFields = cacheFields :+ st"$name: Option<$ty>,"
        cacheInits = cacheInits :+ st"$name: None,"
        getterImpls = getterImpls :+
          st"""fn $name(&mut self) -> $ty {
              |  unsafe {
              |    if let Some(v) = &VIEW_CACHE.$name {
              |      return v.clone();
              |    }
              |  }
              |  let v = self.api.$name();
              |  unsafe { VIEW_CACHE.$name = Some(v.clone()); }
              |  v
              |}"""
      }
    }
    // Aliases of connected inputs: what the reader received, latched just before its dispatch
    // through the reader's own view -- the same read its own checks make in that run -- and,
    // for an event, only for the frame it was received in.
    var recvStatics: ISZ[ST] = ISZ()
    var recvArms: Map[String, ISZ[ST]] = Map.empty
    for (rg <- info.receivedGetters) {
      val ty: String = info.getters.filter((g: (String, String)) => g._1 == rg.name)(0)._2
      recvStatics = recvStatics :+ st"static mut RECV_${rg.name}: Option<(u32, $ty)> = None;"
      val stamp: String = if (rg.isEvent) "FRAME" else "0"
      recvArms = recvArms + rg.reader ~> (recvArms.getOrElse(rg.reader, ISZ()) :+
        st"""{
            |  let v = $crateName::SystemView::${rg.underlying}(view);
            |  unsafe { RECV_${rg.name} = Some(($stamp, v)); }
            |}""")
      val result: ST =
        if (!rg.isEvent)
          st"""match &RECV_${rg.name} {
              |  Some((_, v)) => v.clone(),
              |  None => Default::default(),
              |}"""
        else
          st"""match &RECV_${rg.name} {
              |  Some((f, v)) if *f == FRAME => v.clone(),
              |  _ => None,
              |}"""
      getterImpls = getterImpls :+
        st"""fn ${rg.name}(&mut self) -> $ty {
            |  unsafe {
            |    $result
            |  }
            |}"""
    }
    val latchArms: ISZ[ST] =
      for (e <- recvArms.entries) yield
        st"""$crateName::Thread::${e._1} => {
            |  view.focus = Some($crateName::Thread::${e._1});
            |  ${(e._2, "\n")}
            |}"""
    val latchBody: ST =
      if (latchArms.isEmpty) st"let _ = (t, view); // no alias of a connected input"
      else
        st"""let focus = view.focus;
            |match t {
            |  ${(latchArms :+ st"_ => {}", "\n")}
            |}
            |view.focus = focus;"""

    def threadFn(fnName: String, doc: String, arms: Map[String, ISZ[ST]]): ST = {
      val armSts: ISZ[ST] = for (e <- arms.entries) yield
        st"""$crateName::Thread::${e._1} => {
            |  ${(e._2, "\n")}
            |}"""
      val body: ST =
        if (arms.isEmpty) st"let _ = t;"
        else
          st"""unsafe {
              |  match t {
              |    ${(armSts :+ st"_ => {}", "\n")}
              |  }
              |}"""
      return (
        st"""/// $doc
            |pub fn $fnName(t: $crateName::Thread) {
            |  $body
            |}""")
    }
    val dispatchFns: ST = threadFn("note_dispatch",
      "`t` is about to be dispatched: what its outputs carry from here on is its next dispatch's.",
      dispatchNotes)
    val completionFns: ST = threadFn("producer_completed",
      "`t` has completed: an output it sent nothing on since its dispatch carries nothing this frame.",
      completionNotes)

    val initBody: ST =
      if (initMarks.isEmpty) st"// no thread's own event output is read"
      else
        st"""unsafe {
            |  ${(initMarks, "\n")}
            |}"""
    val runStart: ST =
      if (polls.isEmpty) st"let _ = api; // no event port is read"
      else st"${(polls, "\n")}"
    // The system reader's reads cached in this run describe the frame that just ended; the
    // START checks that follow in the same run must read afresh.
    val sysCacheResets: ISZ[ST] =
      for (g <- info.getters if ops.ISZOps(info.eventGetters).contains(g._1)) yield st"VIEW_CACHE.${g._1}[READERS - 1] = None;"
    val frameEndedFns: ISZ[ST] =
      if (tracks.isEmpty) ISZ()
      else ISZ(
        st"""fn frame_ended(&mut self, _composition: usize) {
            |  unsafe {
            |    FRAME = FRAME.wrapping_add(1);
            |    ${(sysCacheResets, "\n")}
            |  }
            |}""")
    val eventTrackDecls: ST =
      if (tracks.isEmpty) st""
      else
        st"""
            |/// One reader per thread, plus the system layer.
            |const READERS: usize = $readers;
            |
            |fn reader_index(focus: Option<$crateName::Thread>) -> usize {
            |  match focus {
            |    Some(t) => t as usize,
            |    None => READERS - 1,
            |  }
            |}
            |
            |/// An event port's events so far and the latest, the count each reader last saw, and,
            |/// for the system layer, what the producer's latest dispatch sent and in which frame.
            |struct EventTrack<T> {
            |  seq: u32,
            |  /// seq when the producer was last dispatched
            |  dispatch_seq: u32,
            |  value: Option<T>,
            |  sys_frame: Option<u32>,
            |  sys_value: Option<T>,
            |  seen: [u32; READERS],
            |}
            |
            |/// The system layer's frame, counted from the first.
            |static mut FRAME: u32 = 0;
            |
            |${(tracks, "\n")}
            |"""
    return (
      st"""// Maps a schedule channel to the thread it dispatches.  Channel ids belong to this
          |// variant's system description; the checks in crates/$crateName identify threads
          |// by Thread instead.
          |pub fn thread_of(ch: u32) -> Option<$crateName::Thread> {
          |  match ch {
          |    ${(threadArms, "\n")}
          |    _ => None,
          |  }
          |}
          |
          |$eventTrackDecls
          |// What this monitor run has read: one value per getter, and per reader for an event
          |// port.  Cleared by begin_monitor_run at the start of every run.
          |struct ViewCache {
          |  ${(cacheFields, "\n")}
          |}
          |
          |static mut VIEW_CACHE: ViewCache = ViewCache {
          |  ${(cacheInits, "\n")}
          |};
          |
          |pub fn begin_monitor_run<API: ${monitorThreadId}_Full_Api>(api: &mut $appApiType<API>) {
          |  unsafe {
          |    $runStart
          |    VIEW_CACHE = ViewCache {
          |      ${(cacheInits, "\n")}
          |    };
          |  }
          |}
          |
          |// The contract checks read ports and state variables through this monitor's API.
          |pub struct MonitorView<'a, API: ${monitorThreadId}_Full_Api> {
          |  pub api: &'a mut $appApiType<API>,
          |  /// the thread whose check is reading, or None for the system layer
          |  pub focus: Option<$crateName::Thread>,
          |}
          |
          |${(recvStatics, "\n")}
          |
          |/// `t` is about to be dispatched: latch what it receives on each connected input a
          |/// composition aliases, for the system assertions (see ReceivedGetter).
          |pub fn latch_received<'a, API: ${monitorThreadId}_Full_Api>(t: $crateName::Thread, view: &mut MonitorView<'a, API>) {
          |  $latchBody
          |}
          |
          |$dispatchFns
          |
          |$completionFns
          |
          |/// The initialization checks are done: what a thread sent while initializing that they
          |/// did not read is not its first dispatch's output.
          |pub fn init_checked() {
          |  $initBody
          |}
          |
          |impl<'a, API: ${monitorThreadId}_Full_Api> $crateName::SystemView for MonitorView<'a, API> {
          |  fn focus(&mut self, t: Option<$crateName::Thread>) {
          |    self.focus = t;
          |  }
          |
          |  ${((frameEndedFns ++ getterImpls), "\n\n")}
          |}
          |
          |// Reports a violation the way the monitors always have: as log lines.
          |pub struct LogSink;
          |
          |impl $crateName::ViolationSink for LogSink {
          |  fn report(&mut self, e: $crateName::Event) {
          |    match e {
          |      $crateName::Event::IepPostViolation { thread, post } => {
          |        log::warn!("*** CONTRACT VIOLATION: {} IEP_Post not satisfied ***", thread);
          |        log::warn!("{} post: {:?}", thread, post);
          |      }
          |      $crateName::Event::CepPreViolation { thread, pre } => {
          |        log::warn!("*** CONTRACT VIOLATION: {} CEP_Pre not satisfied ***", thread);
          |        log::warn!("{} pre: {:?}", thread, pre);
          |      }
          |      $crateName::Event::CepPostViolation { thread, pre, post } => {
          |        log::warn!("*** CONTRACT VIOLATION: {} CEP_Post not satisfied ***", thread);
          |        log::warn!("{} pre: {:?}", thread, pre);
          |        log::warn!("{} post: {:?}", thread, post);
          |      }
          |      $crateName::Event::CepPostSkipped { thread } => {
          |        log::warn!("{} post check skipped: no saved pre-state", thread);
          |      }
          |      $crateName::Event::CepPostExcused { thread } => {
          |        log::warn!("{} post check skipped: assumption not met", thread);
          |      }
          |      // never raised here: the monitor's reads are never "missing"
          |      $crateName::Event::CheckSkipped { .. } => {}
          |      $crateName::Event::SysAssertViolation { property, point } => {
          |        log::warn!("*** SYS ASSERT VIOLATION: property {}, {} ***", property, point);
          |      }
          |      $crateName::Event::ScheduleNoTransition { ch, timeslice } => {
          |        log::error!("*** SCHEDULE CONFORMANCE VIOLATION: no enabled transition for channel {} at timeslice {} ***", ch, timeslice);
          |      }
          |      $crateName::Event::ScheduleNoEnd { ready } => {
          |        log::error!("*** SCHEDULE CONFORMANCE VIOLATION: walk did not reach END (ready = 0x{:x}) ***", ready);
          |      }
          |      $crateName::Event::ScheduleConformance { violations } => {
          |        if violations == 0 {
          |          log::info!("Schedule conformance check passed");
          |        } else {
          |          log::error!("Schedule conformance check failed with {} violation(s)", violations);
          |        }
          |      }
          |    }
          |  }
          |}""")
  }

  // The system-level GUMBO functions of the root system implementation's subclause,
  // transpiled for runtime checking.  None when the root system has no subclause.
  @pure def systemGumboFunctions(symbolTable: SymbolTable,
                                 options: HamrCli.CodegenOption,
                                 types: AadlTypes,
                                 store: Store,
                                 reporter: Reporter): ISZ[ST] = {
    val rootSystem = symbolTable.rootSystem
    GumboRustUtil.getGumboSubclauseOpt(rootSystem.path, symbolTable) match {
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
                target = SlangExpUtil.TargetLanguage.rust,
                options = options,
                aadlTypes = types,
                tp = crustTypeProvider,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)
              functions = functions :+ fn.prettyST
            case _: GclSpecMethod =>
              reporter.warn(None(), "ContractObserverPlugin", "Spec methods in system-level GUMBO subclauses are not yet supported")
          }
        }
        return functions
      case _ => return ISZ()
    }
  }

  @pure def stateVarsOf(threadPath: ISZ[String], symbolTable: SymbolTable): ISZ[ir.GclStateVar] = {
    symbolTable.annexClauseInfos.get(threadPath) match {
      case Some(clauses) =>
        for (clause <- clauses) {
          clause match {
            case gclInfo: GclAnnexClauseInfo => return gclInfo.annex.state
            case _ =>
          }
        }
        return ISZ()
      case _ => return ISZ()
    }
  }

  @strictpure def placeConstName(p: ScheduleNextRel.PlaceId): String =
    s"PLACE_${ops.StringOps(p.name).toUpper}"

  @pure def computeMask(places: ISZ[ScheduleNextRel.PlaceId]): ST = {
    val parts: ISZ[ST] = for (p <- places) yield st"${placeConstName(p)}"
    if (parts.size == z"1") {
      return parts(0)
    } else {
      return st"(${(parts, " | ")})"
    }
  }
}

@sig trait ContractObserverPlugin extends MicrokitPlugin {

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        ContractObserverPlugin.isRequested(store) &&
        CRustTypePlugin.hasCRustTypeProvider(store) &&
        GumboXRustPlugin.getGumboXContributions(store).nonEmpty &&
        !ContractObserverPlugin.hasObservers(store))
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                            symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    val crateName = ContractObserverPlugin.crateName
    val crateDir = ContractObserverPlugin.crateDirectory(options)
    val gumboxContribs = GumboXRustPlugin.getGumboXContributions(store).get
    val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(store).get
    val dstMap = ContractObserverPlugin.dstPortToMonitorPortName(symbolTable, store)
    val threads = ContractObserverPlugin.modelThreadIds(symbolTable, store)

    var resources: ISZ[Resource] = ISZ()

    // every SystemView getter, in first-use order, with its return type
    var getterNames: ISZ[String] = ISZ()
    var getterTypes: Map[String, String] = Map.empty

    def addGetter(name: String, ty: String): Unit = {
      if (!getterTypes.contains(name)) {
        getterNames = getterNames :+ name
        getterTypes = getterTypes + name ~> ty
      }
    }

    // the getters that read an event or event-data port (see monitorAdapterItems)
    var eventGetterNames: ISZ[String] = ISZ()
    var pureEventGetterNames: ISZ[String] = ISZ()
    def notePureEvent(name: String): Unit = {
      if (!ops.ISZOps(pureEventGetterNames).contains(name)) {
        pureEventGetterNames = pureEventGetterNames :+ name
      }
    }
    // aliases of connected inputs (see ReceivedGetter)
    var receivedGetters: ISZ[ReceivedGetter] = ISZ()
    def noteEventGetter(name: String): Unit = {
      if (!ops.ISZOps(eventGetterNames).contains(name)) {
        eventGetterNames = eventGetterNames :+ name
      }
    }

    // Event ports whose queue holds more than one element (TestScheduler-design.md, "Event
    // ports").  A consumer's checks read one element per check -- the controller the next
    // one through the reader's own cursor, a monitor the latest it polled -- which is what
    // the thread sees only if it consumes one per dispatch.  A producer's own check takes
    // what it sent last in both, so only the consuming side is warned.  Once per port.
    var deepQueuesWarned: Set[ISZ[String]] = Set.empty
    def noteDeepQueue(threadId: String, port: AadlPort): Unit = {
      port match {
        case e: AadlFeatureEvent if port.direction == ir.Direction.In && e.queueSize > 1 && !deepQueuesWarned.contains(port.path) =>
          deepQueuesWarned = deepQueuesWarned + port.path
          reporter.warn(port.feature.identifier.pos, "ContractObserverPlugin",
            s"The run-time contract checks of $threadId read event port ${port.identifier}, whose queue holds ${e.queueSize} elements. They see one element per dispatch, which matches the thread only if it consumes exactly one per dispatch; a thread that drains the queue, or leaves elements queued, may be checked against an element other than the one it used.")
        case _ =>
      }
    }

    def add(path: String, content: ST): Unit = {
      resources = resources :+ ResourceUtil.createResource(path = s"$crateDir/$path", content = content, overwrite = T)
    }

    // ---------------------------------------------------------------------------------
    // Component layer: GUMBOX modules, containers, and ComponentContracts
    // ---------------------------------------------------------------------------------

    // every thread with checkable contracts, Rust or C
    val hasComponentLayer: B = gumboxContribs.allComponentContributions.nonEmpty
    var gumboxModDecls: ISZ[String] = ISZ()

    if (hasComponentLayer) {
      var fields: ISZ[ST] = ISZ()
      var fieldInits: ISZ[ST] = ISZ()
      var forgets: ISZ[ST] = ISZ()
      var initChecks: ISZ[ST] = ISZ()
      var completeArms: ISZ[ST] = ISZ()
      var dispatchArms: ISZ[ST] = ISZ()
      var containerUses: ISZ[ST] = ISZ()

      for (entry <- gumboxContribs.allComponentContributions.entries) {
        val thread = symbolTable.componentMap.get(entry._1).get.asInstanceOf[AadlThread]
        val threadId = MicrokitUtil.getComponentIdPath(thread)
        val contribs = entry._2

        add(s"src/gumbox/${threadId}_GUMBOX.rs", GumboXRustPlugin.generateGumboxModuleContent(contribs))
        add(s"src/gumbox/${threadId}_containers.rs", GumboXRustPlugin.generateMonitorContainerContent(threadId, contribs))
        gumboxModDecls = gumboxModDecls :+ s"${threadId}_GUMBOX" :+ s"${threadId}_containers"
        containerUses = containerUses :+ st"use crate::gumbox::${threadId}_containers::*;"

        def fieldInit(p: GumboXRustUtil.GGParam): ST = {
          val g = ContractObserverPlugin.getterName(p, threadId, dstMap)
          addGetter(g, ContractObserverPlugin.paramType(p))
          p match {
            case pp: GumboXRustUtil.GGPortParam =>
              noteDeepQueue(threadId, pp.port)
              if (pp.isEvent) {
                noteEventGetter(g)
              }
              if (pp.port.isInstanceOf[AadlEventPort]) {
                notePureEvent(g)
              }
            case _ =>
          }
          return st"${p.name}: s.$g(),"
        }

        // the post check references the saved pre-state, so either contract needs one
        val hasCepPre: B = contribs.computeContributions.CEP_Pre.nonEmpty
        val hasPreOrPost: B = hasCepPre || contribs.computeContributions.CEP_Post.nonEmpty
        if (hasPreOrPost) {
          fields = fields :+ st"pre_$threadId: Option<PreState_$threadId>,"
          fields = fields :+ st"pre_ok_$threadId: bool,"
          fieldInits = fieldInits :+ st"pre_$threadId: None," :+ st"pre_ok_$threadId: true,"
          forgets = forgets :+ st"self.pre_$threadId = None;"
        }

        // IEP_Post: initialization guarantees
        if (contribs.initializeContributions.IEP_Guarantee.nonEmpty) {
          val iepPostParams = GumboXRustUtil.sortParams(contribs.initializeContributions.IEP_Post_Params)
          val postFieldInits: ISZ[ST] = for (p <- iepPostParams) yield fieldInit(p)
          val postArgs: ISZ[ST] = for (p <- iepPostParams) yield st"post_$threadId.${p.name}"
          initChecks = initChecks :+
            st"""{
                |  s.focus(Some(crate::Thread::$threadId));
                |  let post_$threadId = PostState_$threadId {
                |    ${(postFieldInits, "\n")}
                |  };
                |  if s.missing() {
                |    out.report(crate::Event::CheckSkipped { thread: "$threadId", check: "IEP_Post" });
                |  } else if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getInitialize_IEP_Post_MethodName}(
                |    ${(postArgs, ", ")}) {
                |    out.report(crate::Event::IepPostViolation { thread: "$threadId", post: &post_$threadId });
                |  }
                |}"""
        }

        // CEP_Post: checked when the thread completes a dispatch
        if (contribs.computeContributions.CEP_Post.nonEmpty) {
          val cepPostParams = GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Post_Params)
          val postOnlyParams = cepPostParams.filter(p =>
            p.kind == GumboXRustUtil.SymbolKind.StateVar || p.isOutPort)
          val postFieldInits: ISZ[ST] = for (p <- postOnlyParams) yield fieldInit(p)
          val cepPostArgs: ISZ[ST] = for (p <- cepPostParams) yield
            st"${if (p.kind == GumboXRustUtil.SymbolKind.StateVarPre || p.isInPort) "pre" else "post"}.${p.name}"
          completeArms = completeArms :+
            st"""crate::Thread::$threadId => {
                |  s.focus(Some(crate::Thread::$threadId));
                |  let post = PostState_$threadId {
                |    ${(postFieldInits, "\n")}
                |  };
                |  if s.missing() {
                |    out.report(crate::Event::CheckSkipped { thread: "$threadId", check: "CEP_Post" });
                |  } else if let Some(pre) = &self.pre_$threadId {
                |    if self.excuse_post_on_failed_pre && !self.pre_ok_$threadId {
                |      out.report(crate::Event::CepPostExcused { thread: "$threadId" });
                |    } else if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getCompute_CEP_Post_MethodName}(
                |      ${(cepPostArgs, ", ")}) {
                |      out.report(crate::Event::CepPostViolation { thread: "$threadId", pre: pre, post: &post });
                |    }
                |  } else {
                |    out.report(crate::Event::CepPostSkipped { thread: "$threadId" });
                |  }
                |}"""
        }

        // pre-state capture, and CEP_Pre: when the thread is about to be dispatched
        if (hasPreOrPost) {
          val preParams: ISZ[GumboXRustUtil.GGParam] =
            if (hasCepPre) GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Pre_Params)
            else GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Post_Params).filter(p =>
              p.kind == GumboXRustUtil.SymbolKind.StateVarPre || p.isInPort)
          val preFieldInits: ISZ[ST] = for (p <- preParams) yield fieldInit(p)
          val preCheck: ST =
            if (hasCepPre) {
              val preArgs: ISZ[ST] = for (p <- preParams) yield st"pre.${p.name}"
              st"""if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getCompute_CEP_Pre_MethodName}(
                  |  ${(preArgs, ", ")}) {
                  |  out.report(crate::Event::CepPreViolation { thread: "$threadId", pre: &pre });
                  |  self.pre_ok_$threadId = false;
                  |} else {
                  |  self.pre_ok_$threadId = true;
                  |}"""
            } else {
              st"self.pre_ok_$threadId = true;"
            }
          dispatchArms = dispatchArms :+
            st"""crate::Thread::$threadId => {
                |  s.focus(Some(crate::Thread::$threadId));
                |  let pre = PreState_$threadId {
                |    ${(preFieldInits, "\n")}
                |  };
                |  if s.missing() {
                |    // no pre-state to hold the completion to
                |    out.report(crate::Event::CheckSkipped { thread: "$threadId", check: "CEP_Pre" });
                |    self.pre_$threadId = None;
                |  } else {
                |    $preCheck
                |    self.pre_$threadId = Some(pre);
                |  }
                |}"""
        }
      }

      add("src/components.rs",
        st"""${CommentTemplate.doNotEditComment_slash}
            |
            |//! The GUMBO contract checks of every thread: IEP_Post at initialization, CEP_Pre
            |//! when a thread is about to be dispatched, and CEP_Post when it completes, against
            |//! the pre-state saved at dispatch.
            |
            |use data::*;
            |use crate::{Event, SystemView, ViolationSink};
            |${(containerUses, "\n")}
            |
            |pub struct ComponentContracts {
            |  /// When set, a dispatch whose CEP_Pre failed is excused from its CEP_Post
            |  /// (reported as CepPostExcused instead of being checked).  GUMBO is
            |  /// assume-guarantee: a component owes nothing when its assumptions fail.
            |  /// The monitor PDs leave it off.
            |  pub excuse_post_on_failed_pre: bool,
            |  ${(fields, "\n")}
            |}
            |
            |impl ComponentContracts {
            |  pub const fn new() -> Self {
            |    ComponentContracts {
            |      excuse_post_on_failed_pre: false,
            |      ${(fieldInits, "\n")}
            |    }
            |  }
            |
            |  /// Initialization guarantees, checked once every thread has initialized and
            |  /// before any has computed.
            |  pub fn on_init<V: SystemView, S: ViolationSink>(&mut self, s: &mut V, out: &mut S) {
            |    ${(initChecks, "\n")}
            |  }
            |
            |  /// `prev` has completed a dispatch: check its CEP_Post against the pre-state
            |  /// saved when it was dispatched.
            |  pub fn on_complete<V: SystemView, S: ViolationSink>(&mut self, prev: crate::Thread, s: &mut V, out: &mut S) {
            |    match prev {
            |      ${(completeArms, "\n")}
            |      _ => {}
            |    }
            |  }
            |
            |  /// Drops every saved pre-state, so each thread's next completion is skipped rather
            |  /// than checked against a dispatch it no longer describes -- after a dispatch went
            |  /// unobserved, or a thread overran its slot.
            |  pub fn forget(&mut self) {
            |    ${(forgets, "\n")}
            |  }
            |
            |  /// `next` is about to be dispatched: save its pre-state and check its CEP_Pre.
            |  pub fn on_dispatch<V: SystemView, S: ViolationSink>(&mut self, next: crate::Thread, s: &mut V, out: &mut S) {
            |    match next {
            |      ${(dispatchArms, "\n")}
            |      _ => {}
            |    }
            |  }
            |}
            |""")

      val modEntries: ISZ[ST] = for (m <- gumboxModDecls) yield st"pub mod $m;"
      add("src/gumbox/mod.rs",
        st"""${CommentTemplate.doNotEditComment_slash}
            |
            |${(modEntries, "\n")}
            |""")
    }

    // ---------------------------------------------------------------------------------
    // System layer: one SysAssert_<id> per composition
    // ---------------------------------------------------------------------------------

    val compositions: ISZ[GclComposition] = VCGenerator.getCompositions(symbolTable)
    var compositionIds: ISZ[String] = ISZ()
    var compositionProperties: ISZ[ISZ[String]] = ISZ()

    if (compositions.nonEmpty) {
      val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)
      var aliasToThread: Map[String, AadlThread] = Map.empty
      for (entry <- resolvedAliasMap.entries) {
        symbolTable.componentMap.get(entry._2) match {
          case Some(thread: AadlThread) => aliasToThread = aliasToThread + entry._1 ~> thread
          case _ =>
        }
      }

      // the getter a port alias reads, typed as the monitor API's getter returns it -- except
      // a pure event port's, which is Option of its empty payload, as a contract reads it.  An
      // alias of a CONNECTED input reads what the reader received there, latched at its
      // dispatch (ReceivedGetter): its own getter, get_recv_<reader>_<port>, backed by the
      // getter of the region the reader reads through, which is named after the producer's
      // output.  An unconnected input's region is the reader's own, so it is read directly.
      def portGetter(thread: AadlThread, portName: String): String = {
        val threadId = MicrokitUtil.getComponentIdPath(thread)
        val own = s"${threadId}_$portName"
        var name = s"get_$own"
        var underlyingOpt: Option[String] = None()
        for (p <- thread.getPorts() if p.identifier == portName && p.direction == ir.Direction.In) {
          dstMap.get(p.path) match {
            case Some(observed) if observed != own =>
              underlyingOpt = Some(s"get_$observed")
              name = s"get_recv_$own"
            case _ =>
          }
        }
        for (p <- thread.getPorts() if p.identifier == portName) {
          val tyOpt: Option[(String, B)] = p match {
            case dp: AadlDataPort =>
              Some((crustTypeProvider.getTypeNameProvider(dp.aadlType).qualifiedRustName, F))
            case edp: AadlEventDataPort =>
              noteDeepQueue(threadId, edp)
              Some((s"Option<${crustTypeProvider.getTypeNameProvider(edp.aadlType).qualifiedRustName}>", T))
            case ep: AadlEventPort =>
              noteDeepQueue(threadId, ep)
              // as a contract reads it (GUMBOX): Option of the empty payload, so a port read by
              // both a contract and a composition has one getter type
              Some((s"Option<${crustTypeProvider.getTypeNameProvider(crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(ep))).qualifiedRustName}>", T))
            case _ => None()
          }
          tyOpt match {
            case Some((ty, isEvent)) =>
              addGetter(name, ty)
              if (p.isInstanceOf[AadlEventPort]) {
                notePureEvent(name)
                underlyingOpt match {
                  case Some(u) => notePureEvent(u)
                  case _ =>
                }
              }
              underlyingOpt match {
                case Some(u) =>
                  addGetter(u, ty)
                  if (isEvent) {
                    noteEventGetter(u)
                  }
                  if (!ops.ISZOps(receivedGetters).exists((g: ReceivedGetter) => g.name == name)) {
                    receivedGetters = receivedGetters :+ ReceivedGetter(name = name, reader = threadId, underlying = u, isEvent = isEvent)
                  }
                case _ =>
                  if (isEvent) {
                    noteEventGetter(name)
                  }
              }
            case _ =>
          }
        }
        return name
      }

      def stateVarGetter(thread: AadlThread, svName: String): String = {
        val threadId = MicrokitUtil.getComponentIdPath(thread)
        val name = s"get_${threadId}_sv_$svName"
        for (sv <- ContractObserverPlugin.stateVarsOf(thread.path, symbolTable) if sv.name == svName) {
          types.typeMap.get(sv.classifier) match {
            case Some(aadlType) =>
              addGetter(name, crustTypeProvider.getTypeNameProvider(
                crustTypeProvider.getRepresentativeType(aadlType)).qualifiedRustName)
            case _ =>
          }
        }
        return name
      }

      var compositionIndex: Z = 0
      for (composition <- compositions) {
        // this composition's index among the model's, for SystemView::focus_system/frame_ended
        val compIdx: Z = compositionIndex
        compositionIndex = compositionIndex + 1
        compositionIds = compositionIds :+ composition.id
        compositionProperties = compositionProperties :+
          (for (property <- composition.properties if !property.isAbstract) yield property.id)
        val modName = ContractObserverPlugin.sysAssertModuleName(composition.id)
        val typeName = ContractObserverPlugin.sysAssertTypeName(composition.id)

        val nextRel = ScheduleNextRel.build(composition)

        var placeBits: Map[ScheduleNextRel.PlaceId, Z] = Map.empty
        for (i <- z"0" until nextRel.places.size) {
          placeBits = placeBits + nextRel.places(i) ~> i
        }

        var placeConstants: ISZ[ST] = ISZ()
        for (pi <- nextRel.places) {
          placeConstants = placeConstants :+
            st"const ${ContractObserverPlugin.placeConstName(pi)}: u64 = 1u64 << ${placeBits.get(pi).get};"
        }

        // Control-point transitions fire on their own; component transitions fire when
        // their thread completes a dispatch.  Grouped by thread so a thread scheduled
        // several times per hyperperiod gets one match arm choosing the enabled one.
        var cpTransitions: ISZ[ST] = ISZ()
        var threadTransitions: Map[String, ISZ[(ST, ST)]] = Map.empty
        for (t <- nextRel.transitions) {
          val inMask = ContractObserverPlugin.computeMask(t.inPlaces)
          val outMask = ContractObserverPlugin.computeMask(t.outPlaces)
          t.kind match {
            case ScheduleNextRel.TransitionKind.ControlPoint =>
              cpTransitions = cpTransitions :+ st"($inMask, $outMask)"
            case ScheduleNextRel.TransitionKind.Component =>
              t.inPlaces match {
                case ISZ(inPlace) =>
                  nextRel.activationMap.get(inPlace) match {
                    case Some(compRef) =>
                      aliasToThread.get(ScheduleNextRel.getComponentName(compRef)) match {
                        case Some(thread) =>
                          val threadId = MicrokitUtil.getComponentIdPath(thread)
                          threadTransitions = threadTransitions + threadId ~>
                            (threadTransitions.getOrElse(threadId, ISZ()) :+ ((inMask, outMask)))
                        case _ =>
                      }
                    case _ =>
                  }
                case _ =>
              }
          }
        }

        var componentTransitionEntries: ISZ[ST] = ISZ()
        var componentArms: ISZ[ST] = ISZ()
        for (entry <- threadTransitions.entries) {
          val threadId = entry._1
          val transitions = entry._2
          for (t <- transitions) {
            componentTransitionEntries = componentTransitionEntries :+
              st"(crate::Thread::$threadId, ${t._1}, ${t._2})"
          }
          if (transitions.size == z"1") {
            componentArms = componentArms :+
              st"""crate::Thread::$threadId => {
                  |  self.ready = (self.ready & !${transitions(0)._1}) | ${transitions(0)._2};
                  |  ${transitions(0)._2}
                  |}"""
          } else {
            var ifChain: ISZ[ST] = ISZ()
            for (j <- z"0" until transitions.size) {
              val keyword: String = if (j == z"0") "if" else "} else if"
              ifChain = ifChain :+
                st"""$keyword self.ready & ${transitions(j)._1} != 0 {
                    |  self.ready = (self.ready & !${transitions(j)._1}) | ${transitions(j)._2};
                    |  ${transitions(j)._2}"""
            }
            componentArms = componentArms :+
              st"""crate::Thread::$threadId => {
                  |  ${(ifChain, "\n")}
                  |  } else {
                  |    0
                  |  }
                  |}"""
          }
        }

        val startConst = ContractObserverPlugin.placeConstName(nextRel.startPlace)
        val endConst = ContractObserverPlugin.placeConstName(nextRel.endPlace)

        // port and state-var aliases -> the SystemView getter that reads them
        var substitutions: Map[String, String] = Map.empty
        for (pa <- composition.portAliases) {
          val segs = pa.portPath.name
          if (segs.size >= z"2") {
            aliasToThread.get(segs(0)) match {
              case Some(thread) =>
                substitutions = substitutions + pa.name ~> s"s.${portGetter(thread, segs(segs.size - 1))}()"
              case _ =>
                substitutions = substitutions + pa.name ~> s"s.get_UNRESOLVED_${pa.name}()"
            }
          }
        }
        for (sva <- composition.stateVarAliases) {
          val segs = sva.stateVarPath.name
          if (segs.size >= z"2") {
            aliasToThread.get(segs(0)) match {
              case Some(thread) =>
                substitutions = substitutions + sva.name ~> s"s.${stateVarGetter(thread, segs(segs.size - 1))}()"
              case _ =>
                substitutions = substitutions + sva.name ~> s"s.get_UNRESOLVED_${sva.name}()"
            }
          }
        }

        // Per-property checks over shared place observations (design D8), attributed
        // (property, point) -- the coordinates of the proof crate's VC names.  Abstract
        // bases are not instantiated (D9).
        var assertionChecks: ISZ[ST] = ISZ()
        var renderedChecks: ISZ[String] = ISZ()
        for (property <- composition.properties if !property.isAbstract) {
          val decoration = ScheduleNextRel.decorate(nextRel, property, reporter)
          for (e <- decoration.entries) {
            val b = e._2
            val rustExp = SlangExpUtil.rewriteExpH(
              rexp = b.exp,
              owner = symbolTable.rootSystem.classifier,
              optComponent = Some(symbolTable.rootSystem),
              context = SlangExpUtil.Context.compute_clause,
              substitutions = substitutions,
              inRequires = F,
              inEnsures = F,
              // executable code, not verus spec: implication renders via implies!/impliesL!
              target = SlangExpUtil.TargetLanguage.rust,
              tp = crustTypeProvider,
              aadlTypes = types,
              store = store,
              reporter = reporter)
            renderedChecks = renderedChecks :+ rustExp.render
            assertionChecks = assertionChecks :+
              st"""if visited & ${ContractObserverPlugin.placeConstName(e._1)} != 0 {
                  |  s.focus(None);
                  |  s.focus_system($compIdx);
                  |  if !($rustExp) && !s.missing() {
                  |    out.report(crate::Event::SysAssertViolation { property: "${property.id}", point: "${b.point.prettyST.render}" });
                  |  }
                  |}"""
          }
        }

        // A composition may leave threads out: it is about part of the system, and its proof
        // is about its schema.  But a left-out thread that feeds a thread whose values the
        // composition reads runs, on the deployed schedule, between steps the schema treats
        // as adjacent -- the proof's conclusion need not hold there, and the run-time checks
        // may report violations where the schema has none.  And a component the composition
        // names but its schema never fires is read without being tracked at all.  Say so.
        // Only the aliases the checks actually read count: an alias no concrete property uses
        // is never read, so its thread's values need no tracking.  A check reads an alias
        // through the getter `substitutions` maps it to; the trailing "()" keeps one getter
        // from matching as a prefix of another.
        @strictpure def isRead(alias: String): B =
          substitutions.get(alias) match {
            case Some(getter) => ops.ISZOps(renderedChecks).exists((c: String) => ops.StringOps(c).contains(getter))
            case _ => F
          }
        var aliasedThreads: Set[String] = Set.empty
        for (pa <- composition.portAliases if pa.portPath.name.size >= z"2" && isRead(pa.name)) {
          aliasToThread.get(pa.portPath.name(0)) match {
            case Some(t) => aliasedThreads = aliasedThreads + MicrokitUtil.getComponentIdPath(t)
            case _ =>
          }
        }
        for (sva <- composition.stateVarAliases if sva.stateVarPath.name.size >= z"2" && isRead(sva.name)) {
          aliasToThread.get(sva.stateVarPath.name(0)) match {
            case Some(t) => aliasedThreads = aliasedThreads + MicrokitUtil.getComponentIdPath(t)
            case _ =>
          }
        }
        for (tId <- aliasedThreads.elements if !threadTransitions.contains(tId)) {
          reporter.warn(None(), name,
            st"Thread $tId is named in composition '${composition.id}' but its schema never fires it: the values the composition reads from it are not tracked, and change wherever $tId runs".render)
        }
        for (t <- symbolTable.getThreads() if !StoreUtil.isSynthetic(t.path, store)) {
          val tId = MicrokitUtil.getComponentIdPath(t)
          if (!threadTransitions.contains(tId)) {
            for (c <- symbolTable.aadlConnections) {
              c match {
                case pc: AadlPortConnection if pc.srcComponent.path == t.path =>
                  val dstId = MicrokitUtil.getComponentIdPath(pc.dstComponent)
                  // only a member whose values the composition reads is affected
                  if (threadTransitions.contains(dstId) && aliasedThreads.contains(dstId)) {
                    reporter.warn(None(), name,
                      st"Thread $tId is not in composition '${composition.id}' but writes ${CommonUtil.getLastName(pc.srcFeature.feature.identifier)}, which $dstId in it reads: the composition's proof does not account for it, and its run-time checks may fail where $tId runs".render)
                  }
                case _ =>
              }
            }
          }
        }

        add(s"src/$modName.rs",
          st"""${CommentTemplate.doNotEditComment_slash}
              |
              |//! The system assertions of composition `${composition.id}`, checked along the
              |//! schedule's workflow net: a place is an assertion point, a transition is a
              |//! thread's dispatch or a control point (split/join).
              |
              |use data::*;
              |use crate::${ContractObserverPlugin.sysAssertFunctionsModuleName}::*;
              |use crate::{Event, SystemView, ViolationSink};
              |
              |${(placeConstants, "\n")}
              |
              |const CP_TRANSITIONS: [(u64, u64); ${cpTransitions.size}] = [
              |  ${(cpTransitions, ",\n")}
              |];
              |
              |/// Component transitions as (thread, in_mask, out_mask).
              |const COMPONENT_TRANSITIONS: [(crate::Thread, u64, u64); ${componentTransitionEntries.size}] = [
              |  ${(componentTransitionEntries, ",\n")}
              |];
              |
              |/// Fires every enabled control-point transition until none is enabled.  The
              |/// marking is a bitmask with one bit per place; firing a transition removes its
              |/// in-place bits and sets its out-place bits.
              |fn cascade(mut ready: u64) -> u64 {
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
              |/// As [`cascade`], also returning every place it entered -- the out-places of
              |/// each transition it fired.  A place that was already marked and stays marked
              |/// (waiting at a join, say) is not among them.
              |fn cascade_acc(mut ready: u64) -> (u64, u64) {
              |  let mut entered = 0u64;
              |  let mut changed = true;
              |  while changed {
              |    changed = false;
              |    for &(in_mask, out_mask) in CP_TRANSITIONS.iter() {
              |      if (ready & in_mask) == in_mask {
              |        ready = (ready & !in_mask) | out_mask;
              |        entered |= out_mask;
              |        changed = true;
              |      }
              |    }
              |  }
              |  (ready, entered)
              |}
              |
              |pub struct $typeName {
              |  ready: u64,
              |}
              |
              |impl $typeName {
              |  pub const fn new() -> Self {
              |    $typeName { ready: 0 }
              |  }
              |
              |  /// Walks the whole net over one hyperperiod of the schedule -- its first
              |  /// `num_timeslices` slots' channels and user-partition bits -- checking that every
              |  /// user slot of a thread in the composition has an enabled transition and that the
              |  /// walk reaches END.  Threads the composition leaves out are passed over.
              |  /// Takes plain slices rather than a `hamr::Schedule`: the test controller has its
              |  /// own copy of the schedule, and without runtime monitoring the data crate has no
              |  /// `hamr` module at all.
              |  pub fn validate_schedule<S: ViolationSink>(num_timeslices: usize,
              |                                            timeslice_ch: &[u32],
              |                                            is_user_partition: &[bool],
              |                                            thread_of: fn(u32) -> Option<crate::Thread>,
              |                                            out: &mut S) {
              |    let n = core::cmp::min(num_timeslices, core::cmp::min(timeslice_ch.len(), is_user_partition.len()));
              |    let mut ready: u64 = $startConst;
              |    ready = cascade(ready);
              |    let mut violations = 0u32;
              |
              |    for i in 0..n {
              |      if !is_user_partition[i] {
              |        continue;
              |      }
              |      let ch = timeslice_ch[i];
              |      let th = thread_of(ch);
              |      // A thread the composition leaves out is not part of what it claims, so its
              |      // slots are passed over: the composition may describe part of the system.
              |      if !COMPONENT_TRANSITIONS.iter().any(|&(t_th, _, _)| th == Some(t_th)) {
              |        continue;
              |      }
              |
              |      let mut fired = false;
              |      for &(t_th, in_mask, out_mask) in COMPONENT_TRANSITIONS.iter() {
              |        if th == Some(t_th) && (ready & in_mask) == in_mask {
              |          ready = (ready & !in_mask) | out_mask;
              |          ready = cascade(ready);
              |          fired = true;
              |          break;
              |        }
              |      }
              |
              |      if !fired {
              |        out.report(Event::ScheduleNoTransition { ch: ch, timeslice: i });
              |        violations += 1;
              |      }
              |    }
              |
              |    if ready != $endConst {
              |      out.report(Event::ScheduleNoEnd { ready: ready });
              |      violations += 1;
              |    }
              |
              |    out.report(Event::ScheduleConformance { violations: violations });
              |  }
              |
              |  /// Puts the marking at START, fires the initial cascade, and checks the
              |  /// assertions at the places it entered.
              |  pub fn on_init<V: SystemView, S: ViolationSink>(&mut self, s: &mut V, out: &mut S) {
              |    let entered = self.restart();
              |    Self::check(entered, s, out);
              |  }
              |
              |  /// Marks START and cascades; returns the places entered.
              |  fn restart(&mut self) -> u64 {
              |    let (ready, entered) = cascade_acc($startConst);
              |    self.ready = ready;
              |    $startConst | entered
              |  }
              |
              |  /// `prev` has completed a dispatch: fire its transition, cascade, and check
              |  /// the assertions at every place entered on the way -- the places this completion
              |  /// reached, not every place still marked.  An assertion "after X" is about the
              |  /// moment X completes; checking it again at later, unrelated completions while its
              |  /// place waits at a join would compare X's outputs with inputs that have moved on.
              |  ///
              |  /// When this completion ends the frame, the next one starts here.  Events belong to
              |  /// the frame they arrived in, so what START sees is only what arrives from here on.
              |  pub fn on_complete<V: SystemView, S: ViolationSink>(&mut self, prev: crate::Thread, s: &mut V, out: &mut S) {
              |    if self.complete(prev, s, out) {
              |      s.frame_ended($compIdx);
              |      self.restart_frame(s, out);
              |    }
              |  }
              |
              |  /// The first half of `on_complete`: fire, cascade and check; returns whether the
              |  /// marking reached END.  Each composition keeps its own frame: its end is reported
              |  /// to the view as `frame_ended` for this composition alone, before `restart_frame`.
              |  pub fn complete<V: SystemView, S: ViolationSink>(&mut self, prev: crate::Thread, s: &mut V, out: &mut S) -> bool {
              |    // the out-places of the transition this completion fired
              |    let entered: u64 = match prev {
              |      ${(componentArms, "\n")}
              |      _ => {
              |        return false;
              |      }
              |    };
              |
              |    let (final_ready, cascaded) = cascade_acc(self.ready);
              |    self.ready = final_ready;
              |    Self::check(entered | cascaded, s, out);
              |    self.ready == $endConst
              |  }
              |
              |  /// The second half of `on_complete`, once the frame has ended: mark START, cascade,
              |  /// and check the assertions at the places entered.
              |  pub fn restart_frame<V: SystemView, S: ViolationSink>(&mut self, s: &mut V, out: &mut S) {
              |    let started = self.restart();
              |    Self::check(started, s, out);
              |  }
              |
              |  /// Checks the assertions at the places in `visited`.
              |  fn check<V: SystemView, S: ViolationSink>(visited: u64, s: &mut V, out: &mut S) {
              |    ${(assertionChecks, "\n")}
              |  }
              |}
              |""")
      }

      val sysFunctions = ContractObserverPlugin.systemGumboFunctions(symbolTable, options, types, store, reporter)
      add(s"src/${ContractObserverPlugin.sysAssertFunctionsModuleName}.rs",
        st"""${CommentTemplate.doNotEditComment_slash}
            |
            |//! The root system's GUMBO functions, for the system assertions.
            |
            |use data::*;
            |
            |${GumboRustUtil.RustImplicationMacros}
            |
            |${(sysFunctions, "\n\n")}
            |""")
    }

    // ---------------------------------------------------------------------------------
    // crate root, manifest, toolchain
    // ---------------------------------------------------------------------------------

    val getters: ISZ[(String, String)] = for (g <- getterNames) yield (g, getterTypes.get(g).get)
    val threadVariants: ISZ[ST] = for (t <- threads) yield st"$t,"
    val getterSigs: ISZ[ST] = for (g <- getters) yield st"fn ${g._1}(&mut self) -> ${g._2};"

    val modDecls: ISZ[ST] =
      (if (hasComponentLayer) ISZ(st"pub mod gumbox;", st"pub mod components;") else ISZ[ST]()) ++
        (if (compositions.nonEmpty)
          ISZ(st"""#[macro_use]
                  |pub mod ${ContractObserverPlugin.sysAssertFunctionsModuleName};""")
        else ISZ[ST]()) ++
        (for (id <- compositionIds) yield st"pub mod ${ContractObserverPlugin.sysAssertModuleName(id)};")

    add("src/lib.rs",
      st"""#![cfg_attr(not(test), no_std)]
          |
          |#![allow(non_camel_case_types)]
          |#![allow(non_snake_case)]
          |#![allow(non_upper_case_globals)]
          |
          |#![allow(dead_code)]
          |#![allow(unreachable_code)]
          |#![allow(unreachable_patterns)]
          |#![allow(unused_imports)]
          |#![allow(unused_macros)]
          |#![allow(unused_parens)]
          |#![allow(unused_variables)]
          |
          |// Required by the Verus build of the containers; unused on a plain cargo build.
          |#![allow(unused_features)]
          |#![allow(unexpected_cfgs)]
          |
          |#![feature(proc_macro_hygiene)]
          |#![cfg_attr(not(verus_keep_ghost), feature(stmt_expr_attributes))]
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |//! Contract checks shared by the gumbo and sys-assert monitor PDs and the test
          |//! controller (TestScheduler-design.md, stage 7).  Each consumer supplies a
          |//! SystemView to read ports and state variables through, a ViolationSink to
          |//! report to, and the thread each check is about.
          |
          |${(modDecls, "\n")}
          |
          |use data::*;
          |
          |/// The model's threads.  Channel ids belong to an MSD variant; each consumer maps
          |/// its own channels onto these.
          |#[derive(Clone, Copy, PartialEq, Eq, Debug)]
          |pub enum Thread {
          |  ${(threadVariants, "\n")}
          |}
          |
          |/// How the checks read ports and state variables.  Each getter returns what the
          |/// monitors' API getter of the same name returns (a `get_recv_` alias: what the getter it
          |/// latches returns), except for a pure event port: its getter is `Option` of the empty
          |/// payload, as GUMBOX reads it, where the API's is `bool`.
          |pub trait SystemView {
          |  ${(getterSigs, "\n")}
          |
          |  /// Called before the reads of one check: `Some(t)` for a check of thread `t`'s
          |  /// contract, `None` for a system assertion.  A view whose reads depend on who is
          |  /// reading (the test controller's event-port cursors) switches on it, and it
          |  /// clears `missing`.
          |  fn focus(&mut self, _t: Option<Thread>) {}
          |
          |  /// Whether a read since the last `focus` found a region that was never written, so
          |  /// the value it returned describes nothing and the check is skipped.
          |  fn missing(&self) -> bool { false }
          |
          |  /// Called after `focus(None)` before a system assertion: the index of the composition
          |  /// it belongs to.  Each composition has its own frame (they need not end at the same
          |  /// completion), and a view that keeps per-frame values answers for that one.
          |  fn focus_system(&mut self, _composition: usize) {}
          |
          |  /// Composition `composition`'s frame is over, and its next one starts: from here its
          |  /// assertions see only events that arrive in the new frame.
          |  fn frame_ended(&mut self, _composition: usize) {}
          |}
          |
          |/// A check's outcome worth reporting.
          |pub enum Event<'a> {
          |  IepPostViolation { thread: &'static str, post: &'a dyn core::fmt::Debug },
          |  CepPreViolation { thread: &'static str, pre: &'a dyn core::fmt::Debug },
          |  CepPostViolation { thread: &'static str, pre: &'a dyn core::fmt::Debug, post: &'a dyn core::fmt::Debug },
          |  /// No pre-state was saved for the completing dispatch.
          |  CepPostSkipped { thread: &'static str },
          |  /// The dispatch's CEP_Pre failed and excuse_post_on_failed_pre is set.
          |  CepPostExcused { thread: &'static str },
          |  /// A value the check reads was never written (`SystemView::missing`).
          |  CheckSkipped { thread: &'static str, check: &'static str },
          |  SysAssertViolation { property: &'static str, point: &'static str },
          |  ScheduleNoTransition { ch: u32, timeslice: usize },
          |  ScheduleNoEnd { ready: u64 },
          |  ScheduleConformance { violations: u32 },
          |}
          |
          |pub trait ViolationSink {
          |  fn report(&mut self, e: Event);
          |}
          |""")

    val libDeps: ISZ[ST] = for (lib <- GumboRustPlugin.getGclLibraryAnnexes(symbolTable))
      yield st"""${lib.name(0)} = { path = "../${lib.name(0)}" }"""
    add("Cargo.toml",
      st"""${CommentTemplate.doNotEditComment_hash}
          |
          |[package]
          |name = "$crateName"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[dependencies]
          |data = { path = "../data" }
          |${(libDeps, "\n")}
          |
          |${RustUtil.verusCargoDependencies(store)}
          |
          |${RustUtil.commonCargoTomlEntries}
          |""")
    resources = resources :+ ResourceUtil.createResource(
      path = s"$crateDir/rust-toolchain.toml",
      content = RustUtil.defaultRustToolChainToml(store),
      overwrite = F)

    val info = ContractObserverInfo(
      threads = threads,
      hasComponentLayer = hasComponentLayer,
      compositionIds = compositionIds,
      compositionProperties = compositionProperties,
      getters = getters,
      eventGetters = eventGetterNames,
      pureEventGetters = pureEventGetterNames,
      receivedGetters = receivedGetters)

    return (store + ContractObserverPlugin.KEY_ContractObserverPlugin ~> info, resources)
  }
}

@datatype class DefaultContractObserverPlugin extends ContractObserverPlugin {

  val name: String = "DefaultContractObserverPlugin"
}
