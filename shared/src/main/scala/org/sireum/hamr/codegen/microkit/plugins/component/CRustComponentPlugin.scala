// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.component

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.util.Util.TAB
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, RustUtil, Util}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RustAst}

object CRustComponentPlugin {

  val KEY_CrustComponentPlugin: String = "KEY_CRustComponentPlugin"

  @strictpure def hasCRustComponentContributions(store: Store): B = store.contains(KEY_CrustComponentPlugin)

  @strictpure def getCRustComponentContributions(store: Store): CRustComponentContributions = store.get(KEY_CrustComponentPlugin).get.asInstanceOf[CRustComponentContributions]

  @strictpure def putComponentContributions( contributions: CRustComponentContributions, store: Store): Store = store + KEY_CrustComponentPlugin ~> contributions


  @strictpure def componentCrateDirectory(thread: AadlThread, options: HamrCli.CodegenOption): String = s"${options.sel4OutputDir.get}/crates/${Util.getThreadIdPath(thread)}"

  @strictpure def componentDirectory(thread: AadlThread, options: HamrCli.CodegenOption): String = s"${componentCrateDirectory(thread, options)}/src/component"

  @strictpure def appModuleName(thread: AadlThread): String = s"${Util.getThreadIdPath(thread)}_app"
}

object ComponentContributions {}

@datatype class ComponentContributions( // markers for component/<thread-path>_app.rs
                                        val markers: ISZ[Marker],

                                        // items for component/<thread-path>_app.rs
                                        val requiresVerus: B,
                                        val appModDirectives: ISZ[RustAst.Item],
                                        val appUses: ISZ[RustAst.Item],
                                        val appStructDef: RustAst.StructDef,
                                        val appStructImpl: RustAst.Impl,


                                        // items for tests.rs
                                        val testEntries: ISZ[RustAst.Item]
                                      )

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

  @strictpure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    !isDisabled(store) &&
      haveHandled(store) &&
      !haveFinalized(store)

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    var ret: Map[IdPath, ComponentContributions] = Map.empty

    var makefileEntries: ISZ[ST] = ISZ()
    for (thread <- symbolTable.getThreads() if Util.isRusty(thread)) {
      val threadId = Util.getThreadIdPath(thread)

      val appApiType = CRustApiPlugin.applicationApiType(thread)

      val modDirectives: ISZ[RustAst.Item] = ISZ(
        RustAst.AttributeST(T, st"allow(non_camel_case_types)"),
        RustAst.AttributeST(T, st"allow(non_snake_case)"))

      val uses: ISZ[RustAst.Item] = ISZ(
        RustAst.Use(ISZ(), RustAst.IdentString(CRustTypePlugin.usePath)),
        RustAst.Use(ISZ(), RustAst.IdentString(s"crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::*")),
        RustAst.Use(ISZ(
          RustAst.AttributeST(F, st"cfg(feature = \"sel4\")"),
          RustAst.AttributeST(F, st"allow(unused_imports)")),
          RustAst.IdentString(s"log::{error, warn, info, debug, trace}")))

      val struct = RustAst.StructDef(
        attributes = ISZ(),
        visibility = RustAst.Visibility.Public,
        ident = RustAst.IdentString(threadId),
        items = ISZ())

      val newFn = RustAst.FnImpl(
        sig = RustAst.FnSig(
          ident = RustAst.IdentString("new"),
          fnDecl = RustAst.FnDecl(inputs = ISZ(), outputs = RustAst.FnRetTyImpl(RustAst.TyPath(ISZ(ISZ("Self")), None()))),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemSelf(ISZ())))))

      val initFn = RustAst.FnImpl(
        sig = RustAst.FnSig(
          ident = RustAst.IdentString("initialize"),
          generics = Some(RustAst.Generics(ISZ(RustAst.GenericParam(
            ident = RustAst.IdentString("API"),
            attributes = ISZ(),
            bounds = RustAst.GenericBoundFixMe(st"${CRustApiPlugin.putApiType(thread)}"))))),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(
              RustAst.ParamFixMe(st"&mut self"),
              RustAst.ParamImpl(
                ident = RustAst.IdentString("api"),
                kind = RustAst.TyRef(None(), RustAst.MutTy(
                  ty = RustAst.TyPath(ISZ(ISZ(appApiType), ISZ("API")), None()), mutbl = RustAst.Mutability.Mut)))
            ),
            outputs = RustAst.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F)),
        comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(
          RustAst.BodyItemST(
            st"""#[cfg(feature = "sel4")]
                |info!("initialize entrypoint invoked");""")))))

      val entrypointFns: ISZ[RustAst.Item] =
        if (thread.isPeriodic())
          ISZ(RustAst.FnImpl(
            sig = RustAst.FnSig(
              ident = RustAst.IdentString("timeTriggered"),
              generics = Some(RustAst.Generics(ISZ(RustAst.GenericParam(
                ident = RustAst.IdentString("API"),
                attributes = ISZ(),
                bounds = RustAst.GenericBoundFixMe(st"${CRustApiPlugin.fullApiType(thread)}"))))),
              fnDecl = RustAst.FnDecl(
                inputs = ISZ(
                  RustAst.ParamFixMe(st"&mut self"),
                  RustAst.ParamImpl(
                    ident = RustAst.IdentString("api"),
                    kind = RustAst.TyRef(None(), RustAst.MutTy(
                      ty = RustAst.TyPath(ISZ(ISZ(appApiType), ISZ("API")), None()), mutbl = RustAst.Mutability.Mut)))
                ),
                outputs = RustAst.FnRetTyDefault()),
              verusHeader = None(), fnHeader = RustAst.FnHeader(F)),
            comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
            body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
              st"""#[cfg(feature = "sel4")]
                  |info!("compute entrypoint invoked");"""))))))
        else ISZ(RustAst.CommentNonDoc(ISZ(st"NOT YET FOR SPORADIC")))

      val notify = RustAst.FnImpl(
        sig = RustAst.FnSig(
          ident = RustAst.IdentString("notify"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(
              RustAst.ParamFixMe(st"&mut self"),
              RustAst.ParamImpl(
                ident = RustAst.IdentString("channel"),
                kind = RustAst.TyPath(ISZ(ISZ("microkit_channel")), None()))),
            outputs = RustAst.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        comments = ISZ(), contract = None(), visibility = RustAst.Visibility.Public, attributes = ISZ(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""// this method is called when the monitor does not handle the passed in channel
              |match channel {
              |  _ => {
              |    #[cfg(feature = "sel4")]
              |    warn!("Unexpected channel {}", channel)
              |  }
              |}""")))))

      val impl = RustAst.ImplBase(
        forIdent = RustAst.IdentString(threadId),
        items = ISZ[RustAst.Item](newFn, initFn) ++ entrypointFns :+ notify,
        comments = ISZ(), attributes = ISZ(), implIdent = None())

      val testEntries = genTestEntries(thread)

      ret = ret + thread.path ~>
        ComponentContributions(
          markers = ISZ(),
          requiresVerus = F,
          appModDirectives = modDirectives,
          appUses = uses,
          appStructDef = struct,
          appStructImpl = impl,

          testEntries = testEntries)

      makefileEntries = makefileEntries :+ st"make -C $${CRATES_DIR}/$threadId test"
    } // end handling crusty components

    return (
      MakefileUtil.addMainMakefileTarget(MakefileTarget(name = "test", dependencies = ISZ(), body = makefileEntries),
        CRustComponentPlugin.putComponentContributions(DefaultCRustComponentContributions(ret), localStore)),
      resources)
  }

  @pure def genTestEntries(thread: AadlThread): ISZ[RustAst.Item] = {
    val threadId = Util.getThreadIdPath(thread)
    assert(thread.isPeriodic(), s"Not yet handling sporadic threads: ${threadId}")

    return ISZ(RustAst.ItemST(
      st"""mod tests {
          |  // NOTE: need to run tests sequentially to prevent race conditions
          |  //       on the app and the testing apis which are static
          |  use serial_test::serial;
          |
          |  use crate::bridge::test_api;
          |  use ${CRustTypePlugin.usePath};
          |
          |  #[test]
          |  #[serial]
          |  fn test_initialization() {
          |    unsafe {
          |      crate::${threadId}_initialize();
          |    }
          |  }
          |
          |  #[test]
          |  #[serial]
          |  fn test_compute() {
          |    unsafe {
          |      crate::${threadId}_initialize();
          |
          |      crate::${threadId}_timeTriggered();
          |    }
          |  }
          |}"""))
  }


  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    for (e <- CRustComponentPlugin.getCRustComponentContributions(store).componentContributions.entries) {
      val thread = symbolTable.componentMap.get(e._1).get.asInstanceOf[AadlThread]
      val threadId = Util.getThreadIdPath(thread)

      val modName = CRustComponentPlugin.appModuleName(thread)

      val componentCrateDir = CRustComponentPlugin.componentCrateDirectory(thread, options)
      val componentSrcDir = s"${CRustComponentPlugin.componentCrateDirectory(thread, options)}/src"
      val componentDir = CRustComponentPlugin.componentDirectory(thread, options)

      { // for now just emit src/lib.rs as a resource

        val entrypoints: ISZ[ST] =
          if (thread.isPeriodic())
            ISZ(
              st"""#[no_mangle]
                  |pub extern "C" fn ${threadId}_timeTriggered() {
                  |  unsafe {
                  |    if let Some(_app) = app.as_mut() {
                  |      _app.timeTriggered(&mut compute_api);
                  |    } else {
                  |      panic!("Unexpected: app is None");
                  |    }
                  |  }
                  |}""")
          else ISZ(st"NOT YET")

        val content =
          st"""#![cfg_attr(not(test), no_std)]
              |
              |${RustUtil.defaultCrateLevelAttributes}
              |
              |${Util.doNotEdit}
              |
              |mod bridge;
              |mod component;
              |mod logging;
              |mod tests;
              |
              |use crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::{self as api, *};
              |use crate::component::${CRustComponentPlugin.appModuleName(thread)}::*;
              |use data::*;
              |
              |#[allow(unused_imports)]
              |use log::{error, warn, info, debug, trace};
              |
              |static mut app: Option<$threadId> = None;
              |static mut init_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.initializationApiType(thread)}> = api::init_api();
              |static mut compute_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.computeApiType(thread)}> = api::compute_api();
              |
              |#[no_mangle]
              |pub extern "C" fn ${threadId}_initialize() {
              |  #[cfg(not(test))]
              |  #[cfg(feature = "sel4")]
              |  logging::LOGGER.set().unwrap();
              |
              |  unsafe {
              |    #[cfg(test)]
              |    crate::bridge::extern_c_api::initialize_test_globals();
              |
              |    let mut _app = $threadId::new();
              |    _app.initialize(&mut init_api);
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
              |  error!("PANIC: {info:#?}");
              |  loop {}
              |}
              |"""
        val path = s"$componentSrcDir/lib.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/logging.rs
        val content =
          st"""#![cfg(feature = "sel4")]
              |
              |${Util.safeToEdit}
              |
              |use sel4::debug_print;
              |use sel4_logging::{LevelFilter, Logger, LoggerBuilder};
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
              |pub static LOGGER: Logger = LoggerBuilder::const_default()
              |  .level_filter(LOG_LEVEL)
              |  .write(|s| debug_print!("{}", s))
              |  .build();
              |"""
        val path = s"$componentSrcDir/logging.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }


      { // src/component/<threadid>_app.rs file for user behavior code
        var uses = e._2.appUses

        var body: ST =
          st"""${e._2.appStructDef.prettyST}
              |
              |${e._2.appStructImpl.prettyST}"""

        if (e._2.requiresVerus) {
          uses = uses :+ RustAst.Use(ISZ(), RustAst.IdentString("vstd::prelude::*"))

          body =
            st"""verus! {
                |
                |  $body
                |
                |}"""
        }
        val content =
          st"""${(for (d <- e._2.appModDirectives) yield d.prettyST, "\n")}
              |
              |${Util.safeToEdit}
              |
              |${(for (u <- uses) yield u.prettyST, "\n")}
              |
              |$body
              |"""
        val path = s"$componentDir/$modName.rs"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(path, content, e._2.markers, F)
      }

      { // src/component/mod.rs
        val content =
          st"""${Util.doNotEdit}
              |
              |pub mod $modName;
              |"""
        val path = s"$componentDir/mod.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // tests.rs
        val content =
          st"""#![cfg(test)]
              |
              |${Util.safeToEdit}
              |
              |${(for(i <- e._2.testEntries) yield i.prettyST, "\n\n")}
              |"""
        val path = s"${CRustComponentPlugin.componentCrateDirectory(thread, options)}/src/tests.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // Cargo.toml
        val content =
          st"""${Util.safeToEditMakefile}
              |
              |[package]
              |name = "$threadId"
              |version = "0.1.0"
              |edition = "2021"
              |
              |[dependencies]
              |log = "0.4.27"
              |sel4 = { git = "https://github.com/seL4/rust-sel4", features = ["single-threaded"], optional = true }
              |sel4-logging = { git = "https://github.com/seL4/rust-sel4", optional = true}
              |${RustUtil.verusCargoDependencies}
              |data = { path = "../data" }
              |
              |[dev-dependencies]
              |lazy_static = "1.5.0"
              |once_cell = "1.21.3"
              |serial_test = "3.2.0"
              |proptest = "1.7.0"
              |
              |[lib]
              |path = "src/lib.rs"
              |crate-type = ["staticlib"]
              |
              |[features]
              |sel4 = ["dep:sel4", "dep:sel4-logging" ]
              |
              |[package.metadata.verus]
              |verify = true
              |"""
        val path = s"$componentCrateDir/Cargo.toml"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // Makefile
        val content =
          st"""${Util.safeToEditMakefile}
              |
              |microkit_sdk_config_dir := $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(MICROKIT_CONFIG)
              |
              |sel4_include_dirs := $$(firstword $$(wildcard $$(microkit_sdk_config_dir)/include \
              |                                            $$(microkit_sdk_config_dir)/debug/include))
              |
              |all:
              |${TAB}RUSTC_BOOTSTRAP=1 \
              |${TAB}SEL4_INCLUDE_DIRS=$$(abspath $$(sel4_include_dirs)) \
              |${TAB}cargo build \
              |${TAB}${TAB}--features sel4 \
              |${TAB}${TAB}-Z build-std=core,alloc,compiler_builtins \
              |${TAB}${TAB}-Z build-std-features=compiler-builtins-mem \
              |${TAB}${TAB}--target aarch64-unknown-none \
              |${TAB}${TAB}--release
              |
              |verus:
              |${TAB}RUSTC_BOOTSTRAP=1 \
              |${TAB}SEL4_INCLUDE_DIRS=$$(abspath $$(sel4_include_dirs)) \
              |${TAB}cargo verus verify \
              |${TAB}${TAB}-Z build-std=core,alloc,compiler_builtins \
              |${TAB}${TAB}-Z build-std-features=compiler-builtins-mem \
              |${TAB}${TAB}--target aarch64-unknown-none \
              |${TAB}${TAB}--release
              |
              |test:
              |${TAB}cargo test
              |
              |clean:
              |${TAB}cargo clean
              |"""
        val path = s"$componentCrateDir/Makefile"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // rust-toolchain.toml
        val content = RustUtil.defaultRustToolChainToml

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