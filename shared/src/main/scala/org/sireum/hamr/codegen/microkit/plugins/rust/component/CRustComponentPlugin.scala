// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.rust.component

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.{ComponentGenProfile, MicrokitFinalizePlugin, MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object CRustComponentPlugin {

  val KEY_CrustComponentPlugin: String = "KEY_CRustComponentPlugin"
  val MarkerR2U2Import: String = "MARKER R2U2 MONITOR IMPORT"
  val MarkerR2U2Module: String = "MARKER R2U2 MONITOR MODULE"

  @strictpure def hasCRustComponentContributions(store: Store): B = store.contains(KEY_CrustComponentPlugin)

  @strictpure def getCRustComponentContributions(store: Store): CRustComponentContributions = store.get(KEY_CrustComponentPlugin).get.asInstanceOf[CRustComponentContributions]

  @strictpure def putComponentContributions(contributions: CRustComponentContributions, store: Store): Store = store + KEY_CrustComponentPlugin ~> contributions


  // The thread's Rust crate name (directory under crates/, Cargo package name, and thus
  // the staticlib the linker consumes). Defaults to the thread's id path; an injector may
  // register a shorter unique name via StoreUtil.putCrateNameOverride (e.g. the sys-assert
  // monitor's crate is sys_<composition>_monitor rather than its full <..>_process_<..>_thread id).
  @strictpure def componentCrateName(thread: AadlThread, store: Store): String =
    StoreUtil.getCrateNameOverride(thread.path, store) match {
      case Some(n) => n
      case _ => MicrokitUtil.getComponentIdPath(thread)
    }

  @strictpure def componentCrateDirectory(thread: AadlThread, options: HamrCli.CodegenOption, store: Store): String = s"${options.sel4OutputDir.get}/crates/${componentCrateName(thread, store)}"

  @strictpure def componentDirectory(thread: AadlThread, options: HamrCli.CodegenOption, store: Store): String = s"${componentCrateDirectory(thread, options, store)}/src/component"

  @strictpure def appModuleName(component: AadlComponent): String = s"${MicrokitUtil.getComponentIdPath(component)}_app"
}

object ComponentContributions {}

@datatype class ComponentContributions( // markers for component/<thread-path>_app.rs
                                        val markers: ISZ[Marker],

                                        // items for component/<thread-path>_app.rs
                                        val requiresVerus: B,
                                        val requiresR2U2: B,
                                        val appModDirectives: ISZ[RAST.Item],
                                        val appUses: ISZ[RAST.Item],
                                        val appStructDef: RAST.StructDef,
                                        val appStructImpl: RAST.Impl,
                                        val appR2U2SpecDef: Option[RAST.R2U2SpecDef],
                                        val appR2U2MonitorMethods: ISZ[RAST.Item],
                                        val moduleLevelEntries: ISZ[RAST.Item],

                                        // Contributions to the crate root, src/lib.rs.  This plugin owns that
                                        // file's skeleton; anything another plugin needs woven into it arrives
                                        // here rather than by re-emitting the file at the same path and relying
                                        // on running later.  The entrypoint slots name positions inside the
                                        // generated extern "C" bodies, which a contributor cannot otherwise
                                        // reach.
                                        val libModDecls: ISZ[RAST.Item],
                                        val libUses: ISZ[RAST.Item],
                                        val libModuleLevelEntries: ISZ[RAST.Item],
                                        // before the app instance is constructed in <thread>_initialize
                                        val libInitializePre: ISZ[RAST.BodyItem],
                                        // after _app.initialize(..), before app = Some(_app)
                                        val libInitializePost: ISZ[RAST.BodyItem],
                                        // after the compute entrypoint dispatches to the app
                                        val libComputePost: ISZ[RAST.BodyItem],

                                        // items for Cargo.toml's [dependencies] table
                                        val crateDependencies: ISZ[ST])

@sig trait CRustComponentContributions extends StoreValue {
  @pure def componentContributions: Map[IdPath, ComponentContributions]

  @pure def replaceComponentContributions(m: Map[IdPath, ComponentContributions]): CRustComponentContributions
}

@datatype class DefaultCRustComponentContributions(val componentContributions: Map[IdPath, ComponentContributions]) extends CRustComponentContributions {

  @strictpure override def replaceComponentContributions(m: Map[IdPath, ComponentContributions]): CRustComponentContributions =
    DefaultCRustComponentContributions(m)
}

@sig trait CRustComponentPlugin extends MicrokitPlugin with MicrokitFinalizePlugin {

  @strictpure def haveHandled(store: Store): B = CRustComponentPlugin.hasCRustComponentContributions(store)

  @strictpure def haveFinalized(store: Store): B = store.contains(s"FINALIZED_$name")

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CRustTypePlugin.hasCRustTypeProvider(store) &&
      //CRustApiPlugin.getCRustApiContributions(store).nonEmpty &&
      !haveHandled(store)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !isDisabled(store) &&
        haveHandled(store) &&
        !haveFinalized(store))
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    var ret: Map[IdPath, ComponentContributions] = Map.empty

    var makefileTestEntries: ISZ[ST] = ISZ()
    var makefileCleanEntries: ISZ[ST] = ISZ()
    for (thread <- symbolTable.getThreads() if MicrokitUtil.isRusty(thread)) {
      val threadId = MicrokitUtil.getComponentIdPath(thread)

      // Code-generation policy for this component (seeded from provenance, possibly
      // set explicitly by an injector). Drives whether the app is Verus-verified.
      val genProfile = StoreUtil.getComponentGenProfile(thread.path, store)

      val appApiType = CRustApiPlugin.applicationApiType(thread)

      val modDirectives: ISZ[RAST.Item] = ISZ()

      val uses: ISZ[RAST.Item] = ISZ(
        RAST.Use(ISZ(), RAST.IdentString(CRustTypePlugin.usePath)),
        RAST.Use(ISZ(), RAST.IdentString(s"crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::*")))

      val struct = RAST.StructDef(
        visibility = RAST.Visibility.Public,
        ident = RAST.IdentString(threadId),
        comments = ISZ(), attributes = ISZ(), items = ISZ())

      val newFn = RAST.FnImpl(
        sig = RAST.FnSig(
          ident = RAST.IdentString("new"),
          fnDecl = RAST.FnDecl(inputs = ISZ(), outputs = RAST.FnRetTyImpl(RAST.TyPath(ISZ(ISZ("Self")), None()))),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
        verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemSelf(ISZ())))))

      val initFn = RAST.FnImpl(
        sig = RAST.FnSig(
          ident = RAST.IdentString("initialize"),
          generics = Some(RAST.Generics(ISZ(RAST.GenericParam(
            ident = RAST.IdentString("API"),
            attributes = ISZ(),
            bounds = RAST.GenericBoundFixMe(st"${CRustApiPlugin.putApiType(thread)}"))))),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(
              RAST.ParamFixMe(st"&mut self"),
              RAST.ParamImpl(
                ident = RAST.IdentString("api"),
                kind = RAST.TyRef(None(), RAST.MutTy(
                  ty = RAST.TyPath(ISZ(ISZ(appApiType), ISZ("API")), None()), mutbl = RAST.Mutability.Mut)))
            ),
            outputs = RAST.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RAST.FnHeader(F)),
        comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
        verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
        body = Some(RAST.MethodBody(ISZ(
          RAST.BodyItemST(
            st"""log_info("initialize entrypoint invoked");""")))))

      val entrypointFns: ISZ[RAST.Item] =
        if (thread.isPeriodic())
          ISZ(RAST.FnImpl(
            sig = RAST.FnSig(
              ident = RAST.IdentString("timeTriggered"),
              generics = Some(RAST.Generics(ISZ(RAST.GenericParam(
                ident = RAST.IdentString("API"),
                attributes = ISZ(),
                bounds = RAST.GenericBoundFixMe(st"${CRustApiPlugin.fullApiType(thread)}"))))),
              fnDecl = RAST.FnDecl(
                inputs = ISZ(
                  RAST.ParamFixMe(st"&mut self"),
                  RAST.ParamImpl(
                    ident = RAST.IdentString("api"),
                    kind = RAST.TyRef(None(), RAST.MutTy(
                      ty = RAST.TyPath(ISZ(ISZ(appApiType), ISZ("API")), None()), mutbl = RAST.Mutability.Mut)))
                ),
                outputs = RAST.FnRetTyDefault()),
              verusHeader = None(), fnHeader = RAST.FnHeader(F)),
            comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
            verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""log_info("compute entrypoint invoked");"""))))))
        else ISZ(RAST.CommentNonDoc(ISZ(st"NOT YET FOR SPORADIC")))

      val notify = RAST.FnImpl(
        sig = RAST.FnSig(
          ident = RAST.IdentString("notify"),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(
              RAST.ParamFixMe(st"&mut self"),
              RAST.ParamImpl(
                ident = RAST.IdentString("channel"),
                kind = RAST.TyPath(ISZ(ISZ("microkit_channel")), None()))),
            outputs = RAST.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
        verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
          st"""// this method is called when the monitor does not handle the passed in channel
              |match channel {
              |  _ => {
              |    log_warn_channel(channel)
              |  }
              |}""")))))

      val impl = RAST.ImplBase(
        forIdent = RAST.IdentString(threadId),
        items = ISZ[RAST.Item](newFn, initFn) ++ entrypointFns :+ notify,
        comments = ISZ(), attributes = ISZ(), implIdent = None())

      val r2u2Spec: Option[RAST.R2U2SpecDef] = None()

      var funcs: ISZ[RAST.Item] = ISZ()

      funcs = funcs :+ RAST.FnImpl(
        sig = RAST.FnSig(
          ident = RAST.IdentString("log_info"),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(
              RAST.ParamImpl(
                ident = RAST.IdentString("msg"),
                kind = RAST.TyPath(ISZ(ISZ("&str")), None()))),
            outputs = RAST.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
        verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
          st"""log::info!("{0}", msg);""")))))

      funcs = funcs :+ RAST.FnImpl(
        sig = RAST.FnSig(
          ident = RAST.IdentString("log_warn_channel"),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(
              RAST.ParamImpl(
                ident = RAST.IdentString("channel"),
                kind = RAST.TyPath(ISZ(ISZ("u32")), None()))),
            outputs = RAST.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
        verusAttributeSyntax = options.verusAttributeSyntax && genProfile.verusVerified, contract = None(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
          st"""log::warn!("Unexpected channel: {0}", channel);""")))))

      ret = ret + thread.path ~>
        ComponentContributions(
          markers = ISZ(),
          requiresVerus = genProfile.verusVerified,
          requiresR2U2 = F,
          appModDirectives = modDirectives,
          appUses = uses,
          appStructDef = struct,
          appStructImpl = impl,
          appR2U2SpecDef = r2u2Spec,
          appR2U2MonitorMethods = ISZ(),
          moduleLevelEntries = funcs,
          libModDecls = ISZ(),
          libUses = ISZ(),
          libModuleLevelEntries = ISZ(),
          libInitializePre = ISZ(),
          libInitializePost = ISZ(),
          libComputePost = ISZ(),
          crateDependencies = ISZ())

      if (genProfile.emitTestHarness) {
        makefileTestEntries = makefileTestEntries :+ st"make -C $${CRATES_DIR}/${CRustComponentPlugin.componentCrateName(thread, localStore)} test"
      }

      makefileCleanEntries = makefileCleanEntries :+ st"make -C $${CRATES_DIR}/${CRustComponentPlugin.componentCrateName(thread, localStore)} clean"
    } // end handling crusty components

    localStore = MakefileUtil.addMakefileTargets(
      ISZ("Makefile"),
      ISZ(MakefileTarget(name = "test", allowMultiple = T, dependencies = ISZ(st"$${TOP_BUILD_DIR}/Makefile"), body = ISZ(st"$${MAKE} -C $${TOP_BUILD_DIR} test"))),
      localStore)

    localStore = MakefileUtil.addMakefileTargets(
      ISZ("system.mk"),
      ISZ(
        MakefileTarget(name = "test", allowMultiple = T, dependencies = ISZ(), body = makefileTestEntries),
        MakefileTarget(name = "clean", allowMultiple = T, dependencies = ISZ(), body = makefileCleanEntries)),
      localStore)

    localStore = CRustComponentPlugin.putComponentContributions(DefaultCRustComponentContributions(ret), localStore)

    return (localStore, resources)
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    for (e <- CRustComponentPlugin.getCRustComponentContributions(store).componentContributions.entries) {
      val thread = symbolTable.componentMap.get(e._1).get.asInstanceOf[AadlThread]
      val threadId = MicrokitUtil.getComponentIdPath(thread)

      val genProfile = StoreUtil.getComponentGenProfile(e._1, store)

      val modName = CRustComponentPlugin.appModuleName(thread)

      val crateName = CRustComponentPlugin.componentCrateName(thread, store)
      val componentCrateDir = CRustComponentPlugin.componentCrateDirectory(thread, options, store)
      val componentSrcDir = s"$componentCrateDir/src"
      val componentDir = CRustComponentPlugin.componentDirectory(thread, options, store)

      { // src/lib.rs -- this plugin owns the skeleton; other plugins weave into it
        // through the lib* fields of ComponentContributions, rather than re-emitting
        // this path and relying on running later.

        val contribs = e._2

        // Each slot renders to None when nothing was contributed, so a component with
        // no contributors produces exactly the text it did before the slots existed.
        // Placed after `mod component;` so contributed modules land in the alphabetical
        // position the hand-written monitor template used (bridge, component, gumbox,
        // logging).
        val libModDecls: Option[ST] =
          if (contribs.libModDecls.isEmpty) None()
          else Some(st"${(for (i <- contribs.libModDecls) yield i.prettyST, "\n")}")
        val libUses: Option[ST] =
          if (contribs.libUses.isEmpty) None()
          else Some(st"${(for (i <- contribs.libUses) yield i.prettyST, "\n")}")
        val libModuleLevelEntries: Option[ST] =
          if (contribs.libModuleLevelEntries.isEmpty) None()
          else Some(st"${(for (i <- contribs.libModuleLevelEntries) yield i.prettyST, "\n")}")
        // The initialize slots sit between statement groups that are separated by blank
        // lines, so the blank lines belong to the slot rather than to the contributor.
        val initializePre: Option[ST] =
          if (contribs.libInitializePre.isEmpty) None()
          else Some(st"""${(for (i <- contribs.libInitializePre) yield i.prettyST, "\n")}
                        |""")
        var initializePost: Option[ST] =
          if (contribs.libInitializePost.isEmpty) None()
          else Some(st"""
                        |${(for (i <- contribs.libInitializePost) yield i.prettyST, "\n")}
                        |""")

        if (e._2.requiresR2U2) {
          initializePost = Some(st"""$initializePost
                                    |_app.r2u2_monitor_initialize();""")
        }

        val computePre: Option[ST] = 
          if (e._2.requiresR2U2)
            Some(st"_app.r2u2_monitor_pre_timeTriggered(&compute_api);")
          else None()

        var computePost: Option[ST] =
          if (contribs.libComputePost.isEmpty) None()
          else Some(st"${(for (i <- contribs.libComputePost) yield i.prettyST, "\n")}")
          
        if (e._2.requiresR2U2) {
          computePost = Some(st"""$computePost
               |_app.r2u2_monitor_post_timeTriggered(&mut compute_api);""")
        }

        val entrypoints: ISZ[ST] =
          if (thread.isPeriodic())
            ISZ(
              st"""#[no_mangle]
                  |pub extern "C" fn ${threadId}_timeTriggered() {
                  |  unsafe {
                  |    if let Some(_app) = app.as_mut() {
                  |      $computePre
                  |      _app.timeTriggered(&mut compute_api);
                  |      $computePost
                  |    } else {
                  |      panic!("Unexpected: app is None");
                  |    }
                  |  }
                  |}""")
          else ISZ(st"NOT YET")

        // Only declare the test module for components that get a test harness
        // (see ComponentGenProfile.emitTestHarness); fully-generated monitors don't.
        val testModDecl: Option[ST] =
          if (genProfile.emitTestHarness) Some(st"""#[cfg(test)]
                                                   |mod test;""")
          else None()

        val content =
          st"""#![cfg_attr(not(test), no_std)]
              |
              |${RustUtil.defaultCrateLevelAttributes}
              |
              |${CommentTemplate.doNotEditComment_slash}
              |
              |mod bridge;
              |mod component;
              |$libModDecls
              |mod logging;
              |
              |$testModDecl
              |
              |use crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::{self as api, *};
              |$libUses
              |use crate::component::${CRustComponentPlugin.appModuleName(thread)}::*;
              |use data::*;
              |
              |static mut app: Option<$threadId> = None;
              |static mut init_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.initializationApiType(thread)}> = api::init_api();
              |static mut compute_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.computeApiType(thread)}> = api::compute_api();
              |$libModuleLevelEntries
              |
              |#[no_mangle]
              |pub extern "C" fn ${threadId}_initialize() {
              |  logging::init_logging();
              |
              |  unsafe {
              |    #[cfg(test)]
              |    crate::bridge::extern_c_api::initialize_test_globals();
              |
              |    $initializePre
              |    let mut _app = $threadId::new();
              |    _app.initialize(&mut init_api);
              |    $initializePost
              |    app = Some(_app);
              |  }
              |}
              |
              |${(entrypoints, "\n\n")}
              |
              |#[no_mangle]
              |pub extern "C" fn ${threadId}_notify(channel: microkit_channel) {
              |  unsafe {
              |    if let Some(_app) = app.as_mut() {
              |      _app.notify(channel);
              |    } else {
              |      panic!("Unexpected: app is None");
              |    }
              |  }
              |}
              |
              |// Need a Panic handler in a no_std environment
              |#[panic_handler]
              |#[cfg(not(test))]
              |fn panic(info: &core::panic::PanicInfo) -> ! {
              |  log::error!("PANIC: {info:#?}");
              |  loop {}
              |}
              |"""
        val path = s"$componentSrcDir/lib.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/logging.rs
        val content =
          st"""${CommentTemplate.safeToEditComment_slash}
              |
              |use log::LevelFilter;
              |
              |#[cfg(feature = "sel4")]
              |use sel4::debug_print;
              |
              |#[cfg(feature = "sel4")]
              |use sel4_logging::{Logger, LoggerBuilder};
              |
              |#[cfg(test)]
              |use std::sync::Once;
              |
              |const LOG_LEVEL: LevelFilter = {
              |  // LevelFilter::Off // lowest level of logging
              |  // LevelFilter::Error
              |  // LevelFilter::Warn
              |  // LevelFilter::Info
              |  // LevelFilter::Debug
              |  LevelFilter::Trace // highest level of logging
              |};
              |
              |#[cfg(feature = "sel4")]
              |pub static LOGGER: Logger = LoggerBuilder::const_default()
              |    .level_filter(LOG_LEVEL)
              |    .write(|s| debug_print!("{}", s))
              |    .build();
              |
              |#[cfg(test)]
              |static INIT: Once = Once::new();
              |
              |pub fn init_logging() {
              |    #[cfg(all(feature = "sel4", not(test)))]
              |    {
              |        LOGGER.set().unwrap();
              |    }
              |
              |    #[cfg(test)]
              |    {
              |        INIT.call_once(|| {
              |            let _ = env_logger::builder()
              |                .is_test(cfg!(test))
              |                .filter_level(LOG_LEVEL)
              |                .try_init();
              |        });
              |    }
              |}"""
        val path = s"$componentSrcDir/logging.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }


      { // src/component/<threadid>_app.rs file for user behavior code
        val uses = e._2.appUses
        var appMarkers = e._2.markers

        val r2u2ImportMarker = Marker.createSlashMarker(CRustComponentPlugin.MarkerR2U2Import)
        appMarkers = appMarkers :+ r2u2ImportMarker
        val r2u2Import: ST =
          if (e._2.requiresR2U2) {
            RAST.MarkerWrap(
              marker = r2u2ImportMarker,
              items = ISZ(RAST.ItemST(st"use super::r2u2_monitor::*;")),
              sep = "\n",
              optLastItemSep = None()).prettyST
          } else {
            RAST.MarkerPlaceholder(
              Marker.createSlashPlaceholderMarker(CRustComponentPlugin.MarkerR2U2Import)).prettyST
          }
        var body: ST =
          st"""${e._2.appStructDef.prettyST}
              |
              |${e._2.appStructImpl.prettyST}"""

        if (e._2.moduleLevelEntries.nonEmpty) {
          body =
            st"""$body
                |
                |${(for(f <- e._2.moduleLevelEntries) yield f.prettyST, "\n\n")}"""
        }
        if (e._2.requiresVerus && !options.verusAttributeSyntax) {
          body = RAST.MacCall(
            macName = "verus",
            items = ISZ(RAST.ItemST(body))).prettyST
        }

        // userEditable components keep user edits across regen (markers +
        // overwrite=F + "safe to edit"); fully-generated components (e.g. the
        // gumbo/sys-assert monitors) are overwritten and marked "do not edit".
        val editHeader: String =
          if (genProfile.userEditable) CommentTemplate.safeToEditComment_slash
          else CommentTemplate.doNotEditComment_slash

        val content =
          st"""${(for (d <- e._2.appModDirectives) yield d.prettyST, "\n")}
              |
              |$editHeader
              |
              |${(for (u <- uses) yield u.prettyST, "\n")}
              |$r2u2Import
              |
              |$body
              |"""
        val path = s"$componentDir/$modName.rs"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(
          path = path,
          content = content,
          markers = if (genProfile.userEditable) appMarkers else ISZ(),
          invertMarkers = F,
          overwrite = !genProfile.userEditable)
      }

      if (e._2.requiresR2U2) { // src/component/r2u2_monitor.rs
        val externalBody: String =
          if (options.verusAttributeSyntax) "verus_verify(external_body)"
          else "verifier::external_body"
        val external: String =
          if (options.verusAttributeSyntax) "verus_verify(external)"
          else "verifier::external"
        val monitorImplAttributes: ISZ[RAST.Attribute] =
          if (options.verusAttributeSyntax && e._2.requiresVerus)
            ISZ(RAST.AttributeST(inner = F, content = st"verus_verify"))
          else ISZ()
        val monitorImpl = RAST.ImplBase(
          comments = ISZ(),
          attributes = monitorImplAttributes,
          implIdent = None(),
          forIdent = RAST.IdentString(threadId),
          items = e._2.appR2U2MonitorMethods)
        val specs = e._2.appR2U2SpecDef.get
        val numSpecs = specs.ftspecs.size + specs.ptspecs.size
        val monitorSpec =
          st"""#[$externalBody]
              |pub struct R2U2Monitor {
              |  inner: r2u2_core::Monitor,
              |  // Cache latest verdict (if applicable) per C2PO specification between monitor steps.
              |  verdict_cache: [Option<r2u2_core::r2u2_verdict>; $numSpecs],
              |}
              |
              |#[$external]
              |impl core::ops::Deref for R2U2Monitor {
              |  type Target = r2u2_core::Monitor;
              |  fn deref(&self) -> &Self::Target { &self.inner }
              |}
              |
              |#[$external]
              |impl core::ops::DerefMut for R2U2Monitor {
              |  fn deref_mut(&mut self) -> &mut Self::Target { &mut self.inner }
              |}
              |
              |#[$externalBody]
              |pub(super) fn default_r2u2_monitor() -> R2U2Monitor {
              |  R2U2Monitor {
              |    inner: r2u2_core::Monitor::default(),
              |    verdict_cache: [None; $numSpecs],
              |  }
              |}"""
        var monitorBody =
          st"""${monitorImpl.prettyST}
              |
              |$monitorSpec"""
        if (e._2.requiresVerus && !options.verusAttributeSyntax) {
          monitorBody = RAST.MacCall(
            macName = "verus",
            items = ISZ(RAST.ItemST(monitorBody))).prettyST
        }
        val content =
          st"""${CommentTemplate.doNotEditComment_slash}
              |
              |use crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::*;
              |use crate::bridge::${threadId}_GUMBOX as GUMBOX;
              |use data::*;
              |use super::$modName::$threadId;
              |${if (e._2.requiresVerus) "use vstd::prelude::*;" else ""}
              |
              |$monitorBody
              |"""
        val path = s"$componentDir/r2u2_monitor.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/component/mod.rs
        val r2u2ModuleMarker = Marker.createSlashMarker(CRustComponentPlugin.MarkerR2U2Module)
        val r2u2Module: ST =
          if (e._2.requiresR2U2) {
            RAST.MarkerWrap(
              marker = r2u2ModuleMarker,
              items = ISZ(RAST.ItemST(st"mod r2u2_monitor;")),
              sep = "\n",
              optLastItemSep = None()).prettyST
          } else {
            RAST.MarkerPlaceholder(
              Marker.createSlashPlaceholderMarker(CRustComponentPlugin.MarkerR2U2Module)).prettyST
          }
        val content =
          st"""${CommentTemplate.safeToEditComment_slash}
              |
              |pub mod $modName;
              |$r2u2Module
              |"""
        val path = s"$componentDir/mod.rs"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(
          path = path,
          content = content,
          markers = ISZ(r2u2ModuleMarker),
          invertMarkers = F,
          overwrite = F)
      }

      { // src/component/spec.c2po + src/component/spec.map
        if (e._2.requiresR2U2){
          val spec_content =
            st"""${CommentTemplate.doNotEditComment_c2po}
                 |
                 |${e._2.appR2U2SpecDef.get.prettyST}
                 |"""
          val spec_path = s"$componentDir/spec.c2po"
          resources = resources :+ ResourceUtil.createResource(spec_path, spec_content, T)
          val map_content = st"""${CommentTemplate.doNotEditComment_c2po}
                 |${e._2.appR2U2SpecDef.get.printMap}
                 |"""
          val map_path = s"$componentDir/spec.map"
          resources = resources :+ ResourceUtil.createResource(map_path, map_content, T)
        } else {
          for (filename <- ISZ("r2u2_monitor.rs", "spec.c2po", "spec.map")) {
            resources = resources :+ ResourceUtil.createRemoveResource(
              s"$componentDir/$filename",
              CommentTemplate.doNotEditComment)
          }
        }
      }

      { // Cargo.toml
        val versions = MicrokitUtil.getMicrokitVersions(localStore)

        val optDeps: Option[ST] =
          if (e._2.crateDependencies.nonEmpty) Some(st"${(e._2.crateDependencies, "\n")}")
          else None()

        val r2u2CargoMarker = Marker.createHashMarker("MARKER R2U2 CARGO DEPENDENCIES")
        val r2u2CargoItems: ISZ[RAST.Item] =
          if (e._2.requiresR2U2) ISZ(RAST.ItemST(RustUtil.r2u2CargoDependencies(localStore)))
          else ISZ()
        val r2u2CargoSection = RAST.MarkerWrap(
          marker = r2u2CargoMarker,
          items = r2u2CargoItems,
          sep = "\n",
          optLastItemSep = None()).prettyST

        val content =
          st"""${CommentTemplate.safeToEditComment_hash}
              |
              |[package]
              |name = "$crateName"
              |version = "0.1.0"
              |edition = "2021"
              |
              |[dependencies]
              |data = { path = "../data" }
              |linux-raw-sys = { version = "=${versions.get("linux-raw-sys").get}", default-features = false }
              |log = "${versions.get("log").get}"
              |$optDeps
              |
              |${RustUtil.sel4CargoDependencies(localStore)}
              |
              |${RustUtil.verusCargoDependencies(localStore)}
              |
              |$r2u2CargoSection
              |
              |[dev-dependencies]
              |lazy_static = "${versions.get("lazy_static").get}"
              |once_cell = "${versions.get("once_cell").get}"
              |serial_test = "${versions.get("serial_test").get}"
              |proptest = "${versions.get("proptest").get}"
              |env_logger = "${versions.get("env_logger").get}"
              |
              |[lib]
              |path = "src/lib.rs"
              |crate-type = ["staticlib"]
              |
              |[features]
              |sel4 = ["dep:sel4", "dep:sel4-logging" ]
              |
              |${RustUtil.commonCargoTomlEntries}
              |"""
        val path = s"$componentCrateDir/Cargo.toml"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(
          path = path,
          content = content,
          markers = ISZ(r2u2CargoMarker),
          invertMarkers = F,
          overwrite = F)
      }

      { // Makefile
        val r2u2MakeMarker = Marker.createHashMarker("MARKER R2U2 MAKE RULES")
        val r2u2MakeItems: ISZ[RAST.Item] =
          if (e._2.requiresR2U2) {
            ISZ(RAST.ItemST(
              st""".DEFAULT_GOAL := all
                  |R2U2_SPEC_BIN := src/component/spec.bin
                  |
                  |r2u2_cli:
                  |${TAB}@echo "Checking/Updating r2u2_cli from crates.io..."
                  |${TAB}cargo +stable install r2u2_cli --version ${MicrokitUtil.getMicrokitVersions(localStore).get("r2u2").get}
                  |
                  |$$(R2U2_SPEC_BIN): r2u2_cli
                  |${TAB}mkdir -p .cargo && \
                  |${TAB}cd src/component && \
                  |${TAB}sed '/^--/d' spec.map > temp.map && \
                  |${TAB}r2u2_cli compile -o . -b ../../.cargo/config.toml spec.c2po temp.map && \
                  |${TAB}rm temp.map"""))
          } else {
            ISZ()
          }
        val r2u2MakeSection = RAST.MarkerWrap(
          marker = r2u2MakeMarker,
          items = r2u2MakeItems,
          sep = "\n",
          optLastItemSep = None()).prettyST

        val content =
          st"""${CommentTemplate.safeToEditComment_hash}
              |
              |microkit_sdk_config_dir := $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(MICROKIT_CONFIG)
              |
              |sel4_include_dirs := $$(firstword $$(wildcard $$(microkit_sdk_config_dir)/include \
              |                                            $$(microkit_sdk_config_dir)/debug/include))
              |
              |$r2u2MakeSection
              |
              |R2U2_BUILD_DEPS = $$(R2U2_SPEC_BIN)
              |
              |# The toolchain is pinned to a stable release channel (see rust-toolchain.toml),
              |# which rejects the #![feature(..)] attributes the generated crates declare, so
              |# every cargo invocation -- building, verifying and testing alike -- needs this.
              |ENV_VARS = RUSTC_BOOTSTRAP=1
              |
              |BUILD_ENV_VARS = $$(ENV_VARS) \
              |                 SEL4_INCLUDE_DIRS=$$(abspath $$(sel4_include_dirs))
              |
              |CARGO_FLAGS = -Z build-std=core,alloc,compiler_builtins \
              |              -Z build-std-features=compiler-builtins-mem \
              |              --target aarch64-unknown-none
              |
              |${RustUtil.smtOptsMakeVar}
              |
              |all: build-verus-release
              |
              |# NOTE: cargo-verus requires Verus-relevant cargo options (e.g. --features,
              |#       --release, --package, --manifest-path) to precede Verus-irrelevant
              |#       ones (e.g. --target, -Z ...), otherwise it errors out.  CARGO_FLAGS
              |#       holds the Verus-irrelevant options, so it must come last.
              |
              |build-verus-release: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(BUILD_ENV_VARS) cargo-verus build --features sel4 --release $$(CARGO_FLAGS) -- $$(SMT_OPTS)
              |
              |build-verus: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(BUILD_ENV_VARS) cargo-verus build --features sel4 $$(CARGO_FLAGS) -- $$(SMT_OPTS)
              |
              |build-release: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(BUILD_ENV_VARS) cargo build --features sel4 $$(CARGO_FLAGS) --release
              |
              |build: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(BUILD_ENV_VARS) cargo build --features sel4 $$(CARGO_FLAGS)
              |
              |verus: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(ENV_VARS) cargo-verus verify $$(CARGO_FLAGS) -- $$(SMT_OPTS)
              |
              |verus-json: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(ENV_VARS) cargo-verus verify $$(CARGO_FLAGS) -- $$(SMT_OPTS) --output-json --time > verus_results.json
              |
              |# Test Example:
              |#   Run all unit tests
              |#   Usage: make test
              |#
              |#   Run only unit tests whose name contains 'proptest'
              |#   Usage: make test args=proptest
              |
              |test-release: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(ENV_VARS) cargo test $$(args) --release
              |
              |test: $$(R2U2_BUILD_DEPS)
              |${TAB}$$(ENV_VARS) cargo test $$(args)
              |
              |# Coverage Example:
              |#   Generate a test coverage report combining the results of all unit tests
              |#   Usage: make coverage
              |#
              |#   Generate a test coverage report for unit tests whose name contains 'proptest'
              |#   Usage: make coverage args=proptest
              |
              |coverage: $$(R2U2_BUILD_DEPS)
              |${TAB}cargo install grcov
              |${TAB}@exists=0; if [ -f target/coverage/report/index.html ]; then exists=1; fi; \
              |${TAB}rm -rf target/coverage; \
              |${TAB}$$(ENV_VARS) CARGO_INCREMENTAL=0 RUSTFLAGS='-Cinstrument-coverage' LLVM_PROFILE_FILE='target/coverage/cargo-test-%p-%m.profraw' \
              |${TAB}cargo test $$(args); \
              |${TAB}grcov . --binary-path ./target/debug/deps/ -s . -t html --branch --ignore-not-existing -o target/coverage/report; \
              |${TAB}if [ $$$$exists -eq 0 ]; then open target/coverage/report/index.html; fi
              |
              |clean:
              |${TAB}cargo clean
              |${TAB}rm -f src/component/spec.bin"""
        val path = s"$componentCrateDir/Makefile"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(
          path = path,
          content = content,
          markers = ISZ(r2u2MakeMarker),
          invertMarkers = F,
          overwrite = F)
      }

      { // rust-toolchain.toml
        val content = RustUtil.defaultRustToolChainToml(store)

        val path = s"$componentCrateDir/rust-toolchain.toml"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }
    }
    return (localStore + s"FINALIZED_$name" ~> BoolValue(T), resources)
  }
}

@datatype class DefaultCRustComponentPlugin extends CRustComponentPlugin {
  @strictpure override def name: String = "DefaultCRustComponentPlugin"
}