// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._

object RustUtil {

  val verusTag: String = "release/0.2025.09.07.6129810"
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
    st"""${Util.safeToEditMakefile}
        |
        |[toolchain]
        |channel = "$verusChannel"
        |components = [ "rustfmt", "rust-src", "rustc-dev", "llvm-tools-preview", "rust-analyzer" ]
        |"""

  //val verusCargoDependencies: ST =
  //  st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, rev="$verusCommitTip"}
  //      |builtin = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }
  //      |builtin_macros = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }"""

  val verusCargoDependencies: ST =
    st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, tag="$verusTag"}
        |verus_builtin = { git = "https://github.com/verus-lang/verus.git", tag="$verusTag" }
        |verus_builtin_macros = { git = "https://github.com/verus-lang/verus.git", tag="$verusTag" }"""

  val commonCargoTomlEntries: ST =
    st"""[package.metadata.verus]
        |verify = true
        |"""
}
