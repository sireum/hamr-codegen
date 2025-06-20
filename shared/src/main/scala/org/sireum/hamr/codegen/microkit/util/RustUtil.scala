// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.util.Util.TAB

object RustUtil {

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
        |channel = "nightly-2024-12-30"
        |components = [ "rustfmt", "rust-src", "rustc-dev", "llvm-tools-preview", "rust-analyzer" ]
        |"""

  // a known commit tip that allows cargo verus verify to succeed.  The next commit causes verus to fail during compilation
  // https://github.com/verus-lang/verus/commit/a11f0f9114dc637dd9e983994386d8b06a1e0954
  //val verusCommitTip: String = "c2a4cd1"

  //val verusCargoDependencies: ST =
  //  st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, rev="$verusCommitTip"}
  //      |builtin = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }
  //      |builtin_macros = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }"""

  val verusTag: String = "release/0.2025.06.14.9b557d7"

  val verusCargoDependencies: ST =
    st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, tag="$verusTag"}
        |builtin = { git = "https://github.com/verus-lang/verus.git", tag="$verusTag" }
        |builtin_macros = { git = "https://github.com/verus-lang/verus.git", tag="$verusTag" }"""
}
