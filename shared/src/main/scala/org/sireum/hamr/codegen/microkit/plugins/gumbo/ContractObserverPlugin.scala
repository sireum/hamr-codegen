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
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, RustUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclBodyMethod, GclComposition, GclSpecMethod}
import org.sireum.message.Reporter

// Stage 7 of TestScheduler-design.md, steps 1-2: the GUMBO contract checks and the
// system-assertion checks, generated once into crates/observers as two layers --
// ComponentContracts and one SysAssert_<id> per composition -- generic over SystemView
// (how a consumer reads ports and state variables) and ViolationSink (what it does with
// a violation).  The gumbo and sys-assert monitor PDs are thin wrappers over them; the
// test controller will be another consumer.
//
// Threads are identified by the generated `Thread` enum rather than by channel id:
// channel ids belong to an MSD variant, and the crate is shared by all of them.  Each
// consumer maps its own channels onto Thread.

@datatype class ContractObserverInfo(val threads: ISZ[String],
                                     val hasComponentLayer: B,
                                     val compositionIds: ISZ[String],
                                     // (getter name, rust return type): every SystemView method
                                     val getters: ISZ[(String, String)]) extends StoreValue

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

  // Generated only when something consumes it: today the gumbo / sys-assert monitor PDs,
  // which exist under --runtime-monitoring when some thread has GUMBO state variables
  // (GumboMonitorPlugin's gate).  The test controller becomes the second consumer in
  // stage 7 step 4, when system testing alone will request it (design D22).
  @pure def isRequested(options: HamrCli.CodegenOption, symbolTable: SymbolTable): B = {
    if (!options.runtimeMonitoring) {
      return F
    }
    for (t <- symbolTable.getThreads()) {
      if (stateVarsOf(t.path, symbolTable).nonEmpty) {
        return T
      }
    }
    return F
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

  // Module-level items for a monitor PD's app module: the channel -> Thread map, the
  // SystemView adapter over the monitor's API, and the sink that logs exactly what the
  // monitors logged before the checks moved into crates/observers.  Kept in the monitor's
  // own module so the log records keep their target.
  @pure def monitorAdapterItems(monitorThreadId: String,
                                appApiType: String,
                                info: ContractObserverInfo): ST = {
    val threadArms: ISZ[ST] = for (t <- info.threads) yield
      st"${t}_MON => Some($crateName::Thread::$t),"
    val getterImpls: ISZ[ST] = for (g <- info.getters) yield
      st"fn ${g._1}(&mut self) -> ${g._2} { self.api.${g._1}() }"
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
          |// The contract checks read ports and state variables through this monitor's API.
          |pub struct MonitorView<'a, API: ${monitorThreadId}_Full_Api> {
          |  pub api: &'a mut $appApiType<API>,
          |}
          |
          |impl<'a, API: ${monitorThreadId}_Full_Api> $crateName::SystemView for MonitorView<'a, API> {
          |  ${(getterImpls, "\n")}
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
        ContractObserverPlugin.isRequested(options, symbolTable) &&
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

    def add(path: String, content: ST): Unit = {
      resources = resources :+ ResourceUtil.createResource(path = s"$crateDir/$path", content = content, overwrite = T)
    }

    // ---------------------------------------------------------------------------------
    // Component layer: GUMBOX modules, containers, and ComponentContracts
    // ---------------------------------------------------------------------------------

    val hasComponentLayer: B = gumboxContribs.componentContributions.nonEmpty
    var gumboxModDecls: ISZ[String] = ISZ()

    if (hasComponentLayer) {
      var fields: ISZ[ST] = ISZ()
      var fieldInits: ISZ[ST] = ISZ()
      var initChecks: ISZ[ST] = ISZ()
      var completeArms: ISZ[ST] = ISZ()
      var dispatchArms: ISZ[ST] = ISZ()
      var containerUses: ISZ[ST] = ISZ()

      for (entry <- gumboxContribs.componentContributions.entries) {
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
          return st"${p.name}: s.$g(),"
        }

        // the post check references the saved pre-state, so either contract needs one
        val hasCepPre: B = contribs.computeContributions.CEP_Pre.nonEmpty
        val hasPreOrPost: B = hasCepPre || contribs.computeContributions.CEP_Post.nonEmpty
        if (hasPreOrPost) {
          fields = fields :+ st"pre_$threadId: Option<PreState_$threadId>,"
          fields = fields :+ st"pre_ok_$threadId: bool,"
          fieldInits = fieldInits :+ st"pre_$threadId: None," :+ st"pre_ok_$threadId: true,"
        }

        // IEP_Post: initialization guarantees
        if (contribs.initializeContributions.IEP_Guarantee.nonEmpty) {
          val iepPostParams = GumboXRustUtil.sortParams(contribs.initializeContributions.IEP_Post_Params)
          val postFieldInits: ISZ[ST] = for (p <- iepPostParams) yield fieldInit(p)
          val postArgs: ISZ[ST] = for (p <- iepPostParams) yield st"post_$threadId.${p.name}"
          initChecks = initChecks :+
            st"""{
                |  let post_$threadId = PostState_$threadId {
                |    ${(postFieldInits, "\n")}
                |  };
                |  if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getInitialize_IEP_Post_MethodName}(
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
                |  let post = PostState_$threadId {
                |    ${(postFieldInits, "\n")}
                |  };
                |  if let Some(pre) = &self.pre_$threadId {
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
                |  let pre = PreState_$threadId {
                |    ${(preFieldInits, "\n")}
                |  };
                |  $preCheck
                |  self.pre_$threadId = Some(pre);
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

    if (compositions.nonEmpty) {
      val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)
      var aliasToThread: Map[String, AadlThread] = Map.empty
      for (entry <- resolvedAliasMap.entries) {
        symbolTable.componentMap.get(entry._2) match {
          case Some(thread: AadlThread) => aliasToThread = aliasToThread + entry._1 ~> thread
          case _ =>
        }
      }

      // the getter a port alias reads, typed as the monitor API's getter returns it
      def portGetter(thread: AadlThread, portName: String): String = {
        val threadId = MicrokitUtil.getComponentIdPath(thread)
        val name = s"get_${threadId}_$portName"
        for (p <- thread.getPorts() if p.identifier == portName) {
          p match {
            case dp: AadlDataPort =>
              addGetter(name, crustTypeProvider.getTypeNameProvider(dp.aadlType).qualifiedRustName)
            case edp: AadlEventDataPort =>
              addGetter(name, s"Option<${crustTypeProvider.getTypeNameProvider(edp.aadlType).qualifiedRustName}>")
            case _: AadlEventPort =>
              addGetter(name, "bool")
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

      for (composition <- compositions) {
        compositionIds = compositionIds :+ composition.id
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
                  |}"""
          } else {
            var ifChain: ISZ[ST] = ISZ()
            for (j <- z"0" until transitions.size) {
              val keyword: String = if (j == z"0") "if" else "} else if"
              ifChain = ifChain :+
                st"""$keyword self.ready & ${transitions(j)._1} != 0 {
                    |  self.ready = (self.ready & !${transitions(j)._1}) | ${transitions(j)._2};"""
            }
            componentArms = componentArms :+
              st"""crate::Thread::$threadId => {
                  |  ${(ifChain, "\n")}
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
            assertionChecks = assertionChecks :+
              st"""if visited & ${ContractObserverPlugin.placeConstName(e._1)} != 0 {
                  |  if !($rustExp) {
                  |    out.report(crate::Event::SysAssertViolation { property: "${property.id}", point: "${b.point.prettyST.render}" });
                  |  }
                  |}"""
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
              |/// As [`cascade`], also returning the union of every intermediate marking --
              |/// each assertion place the cascade passed through.
              |fn cascade_acc(mut ready: u64) -> (u64, u64) {
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
              |pub struct $typeName {
              |  ready: u64,
              |}
              |
              |impl $typeName {
              |  pub const fn new() -> Self {
              |    $typeName { ready: 0 }
              |  }
              |
              |  /// Walks the whole net over one hyperperiod of `sched`, checking that every
              |  /// user slot's thread has an enabled transition and that the walk reaches END.
              |  pub fn validate_schedule<S: ViolationSink>(sched: &hamr::Schedule,
              |                                            thread_of: fn(u32) -> Option<crate::Thread>,
              |                                            out: &mut S) {
              |    let n = sched.num_timeslices as usize;
              |    let mut ready: u64 = $startConst;
              |    ready = cascade(ready);
              |    let mut violations = 0u32;
              |
              |    for i in 0..n {
              |      if !sched.is_user_partition[i] {
              |        continue;
              |      }
              |      let ch = sched.timeslice_ch[i];
              |      let th = thread_of(ch);
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
              |  /// Puts the marking at START and fires the initial cascade.
              |  pub fn on_init(&mut self) {
              |    self.ready = $startConst;
              |    self.ready = cascade(self.ready);
              |  }
              |
              |  /// `prev` has completed a dispatch: fire its transition, cascade, and check
              |  /// the assertions at every place the cascade visited.
              |  pub fn on_complete<V: SystemView, S: ViolationSink>(&mut self, prev: crate::Thread, s: &mut V, out: &mut S) {
              |    match prev {
              |      ${(componentArms, "\n")}
              |      _ => {
              |        return;
              |      }
              |    }
              |
              |    let (final_ready, visited) = cascade_acc(self.ready);
              |    self.ready = final_ready;
              |
              |    ${(assertionChecks, "\n")}
              |
              |    if self.ready == $endConst {
              |      self.ready = $startConst;
              |      self.ready = cascade(self.ready);
              |    }
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
          |/// monitors' API getter of the same name returns.
          |pub trait SystemView {
          |  ${(getterSigs, "\n")}
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
      getters = getters)

    return (store + ContractObserverPlugin.KEY_ContractObserverPlugin ~> info, resources)
  }
}

@datatype class DefaultContractObserverPlugin extends ContractObserverPlugin {

  val name: String = "DefaultContractObserverPlugin"
}
