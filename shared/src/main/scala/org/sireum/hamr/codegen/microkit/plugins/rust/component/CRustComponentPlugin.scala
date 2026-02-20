// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.rust.component

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object CRustComponentPlugin {

  val KEY_CrustComponentPlugin: String = "KEY_CRustComponentPlugin"

  @strictpure def hasCRustComponentContributions(store: Store): B = store.contains(KEY_CrustComponentPlugin)

  @strictpure def getCRustComponentContributions(store: Store): CRustComponentContributions = store.get(KEY_CrustComponentPlugin).get.asInstanceOf[CRustComponentContributions]

  @strictpure def putComponentContributions(contributions: CRustComponentContributions, store: Store): Store = store + KEY_CrustComponentPlugin ~> contributions


  @strictpure def componentCrateDirectory(thread: AadlThread, options: HamrCli.CodegenOption): String = s"${options.sel4OutputDir.get}/crates/${MicrokitUtil.getComponentIdPath(thread)}"

  @strictpure def componentDirectory(thread: AadlThread, options: HamrCli.CodegenOption): String = s"${componentCrateDirectory(thread, options)}/src/component"

  @strictpure def appModuleName(component: AadlComponent): String = s"${MicrokitUtil.getComponentIdPath(component)}_app"
}

object ComponentContributions {}

@datatype class ComponentContributions(// markers for component/<thread-path>_app.rs
                                       val markers: ISZ[Marker],

                                       // items for component/<thread-path>_app.rs
                                       val requiresVerus: B,
                                       val appModDirectives: ISZ[RAST.Item],
                                       val appUses: ISZ[RAST.Item],
                                       val appStructDef: RAST.StructDef,
                                       val appStructImpl: RAST.Impl,

                                       val crateLevelEntries: ISZ[RAST.Item],

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
        comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
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
        comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
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
            comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
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
        comments = ISZ(), contract = None(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
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
        comments = ISZ(), contract = None(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
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
        comments = ISZ(), contract = None(), visibility = RAST.Visibility.Public, attributes = ISZ(), meta = ISZ(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
          st"""log::warn!("Unexpected channel: {0}", channel);""")))))

      ret = ret + thread.path ~>
        ComponentContributions(
          markers = ISZ(),
          requiresVerus = F,
          appModDirectives = modDirectives,
          appUses = uses,
          appStructDef = struct,
          appStructImpl = impl,
          crateLevelEntries = funcs,
          crateDependencies = ISZ())

      makefileTestEntries = makefileTestEntries :+ st"make -C $${CRATES_DIR}/$threadId test"

      makefileCleanEntries = makefileCleanEntries :+ st"make -C $${CRATES_DIR}/$threadId clean"
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

      val modName = CRustComponentPlugin.appModuleName(thread)

      val componentCrateDir = CRustComponentPlugin.componentCrateDirectory(thread, options)
      val componentSrcDir = s"$componentCrateDir/src"
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
              |${MicrokitUtil.doNotEdit}
              |
              |mod bridge;
              |mod component;
              |mod logging;
              |
              |#[cfg(test)]
              |mod test;
              |
              |use crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::{self as api, *};
              |use crate::component::${CRustComponentPlugin.appModuleName(thread)}::*;
              |use data::*;
              |
              |static mut app: Option<$threadId> = None;
              |static mut init_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.initializationApiType(thread)}> = api::init_api();
              |static mut compute_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.computeApiType(thread)}> = api::compute_api();
              |
              |#[no_mangle]
              |pub extern "C" fn ${threadId}_initialize() {
              |  logging::init_logging();
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
              |  log::error!("PANIC: {info:#?}");
              |  loop {}
              |}
              |"""
        val path = s"$componentSrcDir/lib.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/logging.rs
        val content =
          st"""// This file will not be overwritten if codegen is rerun
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
        var uses = e._2.appUses

        var body: ST =
          st"""${e._2.appStructDef.prettyST}
              |
              |${e._2.appStructImpl.prettyST}"""

        if (e._2.crateLevelEntries.nonEmpty) {
          body =
            st"""$body
                |
                |${(for(f <- e._2.crateLevelEntries) yield f.prettyST, "\n\n")}"""
        }

        if (e._2.requiresVerus) {
          uses = uses :+ RAST.Use(ISZ(), RAST.IdentString("vstd::prelude::*"))

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
              |${MicrokitUtil.safeToEdit}
              |
              |${(for (u <- uses) yield u.prettyST, "\n")}
              |
              |$body
              |"""
        val path = s"$componentDir/$modName.rs"
        resources = resources :+ ResourceUtil.createResourceWithMarkers(
          path = path,
          content = content,
          markers= e._2.markers,
          invertMarkers = F,
          overwrite = F)
      }

      { // src/component/mod.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |pub mod $modName;
              |"""
        val path = s"$componentDir/mod.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // Cargo.toml
        val versions = MicrokitUtil.getMicrokitVersions(localStore)

        val optDeps: Option[ST] =
          if (e._2.crateDependencies.nonEmpty) Some(st"${(e._2.crateDependencies, "\n")}")
          else None()

        val content =
          st"""${MicrokitUtil.safeToEditMakefile}
              |
              |[package]
              |name = "$threadId"
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
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // Makefile
        val content =
          st"""${MicrokitUtil.safeToEditMakefile}
              |
              |microkit_sdk_config_dir := $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(MICROKIT_CONFIG)
              |
              |sel4_include_dirs := $$(firstword $$(wildcard $$(microkit_sdk_config_dir)/include \
              |                                            $$(microkit_sdk_config_dir)/debug/include))
              |
              |ENV_VARS = RUSTC_BOOTSTRAP=1
              |
              |BUILD_ENV_VARS = $$(ENV_VARS) \
              |                 SEL4_INCLUDE_DIRS=$$(abspath $$(sel4_include_dirs))
              |
              |CARGO_FLAGS = -Z build-std=core,alloc,compiler_builtins \
              |              -Z build-std-features=compiler-builtins-mem \
              |              --target aarch64-unknown-none
              |
              |all: build-verus-release
              |
              |build-verus-release:
              |${TAB}$$(BUILD_ENV_VARS) cargo-verus build --features sel4 $$(CARGO_FLAGS) --release
              |
              |build-verus:
              |${TAB}$$(BUILD_ENV_VARS) cargo-verus build --features sel4 $$(CARGO_FLAGS)
              |
              |build-release:
              |${TAB}$$(BUILD_ENV_VARS) cargo build --features sel4 $$(CARGO_FLAGS) --release
              |
              |build:
              |${TAB}$$(BUILD_ENV_VARS) cargo build --features sel4 $$(CARGO_FLAGS)
              |
              |verus:
              |${TAB}$$(ENV_VARS) cargo-verus verify $$(CARGO_FLAGS)
              |
              |verus-json:
              |${TAB}$$(ENV_VARS) cargo-verus verify $$(CARGO_FLAGS) -- --output-json --time > verus_results.json
              |
              |# Test Example:
              |#   Run all unit tests
              |#   Usage: make test
              |#
              |#   Run only unit tests whose name contains 'proptest'
              |#   Usage: make test args=proptest
              |
              |test-release:
              |${TAB}cargo test $$(args) --release
              |
              |test:
              |${TAB}cargo test $$(args)
              |
              |# Coverage Example:
              |#   Generate a test coverage report combining the results of all unit tests
              |#   Usage: make coverage
              |#
              |#   Generate a test coverage report for unit tests whose name contains 'proptest'
              |#   Usage: make coverage args=proptest
              |
              |coverage:
              |${TAB}cargo install grcov
              |${TAB}@exists=0; if [ -f target/coverage/report/index.html ]; then exists=1; fi; \
              |${TAB}rm -rf target/coverage; \
              |${TAB}CARGO_INCREMENTAL=0 RUSTFLAGS='-Cinstrument-coverage' LLVM_PROFILE_FILE='target/coverage/cargo-test-%p-%m.profraw' \
              |${TAB}cargo test $$(args); \
              |${TAB}grcov . --binary-path ./target/debug/deps/ -s . -t html --branch --ignore-not-existing -o target/coverage/report; \
              |${TAB}if [ $$$$exists -eq 0 ]; then open target/coverage/report/index.html; fi
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