// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store

object RustUtil {

  val verusChannel: String = "nightly"

  val defaultCrateLevelAttributes: ST =
    st"""#![allow(non_camel_case_types)]
        |#![allow(non_snake_case)]
        |#![allow(non_upper_case_globals)]
        |
        |#![allow(dead_code)]
        |#![allow(static_mut_refs)]
        |#![allow(unused_imports)]
        |#![allow(unused_macros)]
        |#![allow(unused_parens)]
        |#![allow(unused_unsafe)]
        |#![allow(unused_variables)]"""

  val defaultRustToolChainToml: ST =
    st"""${MicrokitUtil.safeToEditMakefile}
        |
        |[toolchain]
        |channel = "$verusChannel"
        |components = [ "rustfmt", "rust-src", "rustc-dev", "llvm-tools-preview", "rust-analyzer" ]
        |"""

  //val verusCargoDependencies: ST =
  //  st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, rev="$verusCommitTip"}
  //      |builtin = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }
  //      |builtin_macros = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }"""

  @pure def verusCargoDependencies(store: Store): ST = {
    val versions = MicrokitUtil.getMicrokitVersions(store)
    return (
      st"""vstd = { version = "${versions.get("vstd").get}", default-features=false }
          |verus_builtin = { version = "${versions.get("verus_builtin").get}" }
          |verus_builtin_macros = { version = "${versions.get("verus_builtin_macros").get}" }""")
  }

  val commonCargoTomlEntries: ST =
    st"""[package.metadata.verus]
        |verify = true
        |"""

  @pure def sel4CargoDependencies(store: Store) : ST = {
    val versions = MicrokitUtil.getMicrokitVersions(store)
    val sel4Opt: Option[ST] = versions.get("sel4") match {
      case Some(v) => Some(st""", tag="$v" """)
      case _ => None()
    }
    val sel4LoggingOpt: Option[ST] = versions.get("sel4-logging") match {
      case Some(v) => Some(st""", tag="$v" """)
      case _ => None()
    }
    return (
      st"""sel4 = { git = "https://github.com/seL4/rust-sel4", features = ["single-threaded"], optional = true$sel4Opt}
          |sel4-logging = { git = "https://github.com/seL4/rust-sel4", optional = true$sel4LoggingOpt}""")
  }
}
