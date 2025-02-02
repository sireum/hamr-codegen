// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util.TAB

object RustUtil {

  def rustUserContent(threadId: String,
                      methods: ISZ[ST]): ST = {
    return (st"""#![cfg_attr(not(test), no_std)]
                |#![allow(non_snake_case)]
                |
                |#[allow(unused_imports)]
                |use log::{error, warn, info, debug, trace};
                |
                |mod logging;
                |mod api;
                |use types::*;
                |
                |#[no_mangle]
                |pub extern "C" fn ${threadId}_initialize() {
                |  #[cfg(not(test))]
                |  logging::LOGGER.set().unwrap();
                |
                |  info!("initialize invoked");
                |}
                |
                |${(methods, "\n\n")}
                |
                |#[no_mangle]
                |pub extern "C" fn ${threadId}_notify(channel: microkit_channel) {
                |  // this method is called when the monitor does not handle the passed in channel
                |  match channel {
                |      _ => warn!("Unexpected channel {}", channel)
                |  }
                |}
                |
                |// Need a Panic handler in a no_std environment
                |#[cfg(not(test))]
                |#[panic_handler]
                |fn panic(info: &core::panic::PanicInfo) -> ! {
                |  error!("PANIC: {info:#?}");
                |  loop {}
                |}
                |""")
  }

  val rustToolchainContent: ST =
    st"""${Util.safeToEditMakefile}
        |
        |[toolchain]
        |channel = "nightly-2025-01-30"
        |components = [ "rustfmt", "rust-src", "rustc-dev", "llvm-tools-preview", "rust-analyzer" ]
        |"""

  val rustLoggingContent: ST =
    st"""${Util.doNotEdit}
        |
        |#[cfg(not(test))]
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

  def cargoComponentToml(componentName: String): ST = {
    return (st"""${Util.safeToEditMakefile}
                |
                |[package]
                |name = "$componentName"
                |version = "0.1.0"
                |edition = "2021"
                |
                |[dependencies]
                |log = "0.4.17"
                |types = { path = "../types" }
                |
                |[target.'cfg(not(unix))'.dependencies]
                |sel4 = { git = "https://github.com/seL4/rust-sel4", features = ["single-threaded"] }
                |sel4-logging = { git = "https://github.com/seL4/rust-sel4" }
                |
                |[lib]
                |path = "src/$componentName.rs"
                |crate-type = ["staticlib"]""")
  }

  def makefileComponent(): ST = {
    return (st"""${Util.safeToEditMakefile}
                |
                |microkit_sdk_config_dir := $$(MICROKIT_SDK)/board/$$(MICROKIT_BOARD)/$$(MICROKIT_CONFIG)
                |
                |sel4_include_dirs := $$(microkit_sdk_config_dir)/include
                |
                |all:
                |${TAB}SEL4_INCLUDE_DIRS=$$(abspath $$(sel4_include_dirs)) \
                |${TAB}${TAB}cargo build \
                |${TAB}${TAB}${TAB}-Z build-std=core,alloc,compiler_builtins \
                |${TAB}${TAB}${TAB}-Z build-std-features=compiler-builtins-mem \
                |${TAB}${TAB}${TAB}--target aarch64-unknown-none \
                |${TAB}${TAB}${TAB}--release
                |
                |test:
                |${TAB}cargo test
                |
                |clean:
                |${TAB}cargo clean
                |""")
  }
}
