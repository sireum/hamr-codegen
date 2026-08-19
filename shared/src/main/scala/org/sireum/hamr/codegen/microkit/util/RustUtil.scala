// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.templates.CommentTemplate

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
        |#![allow(unused_variables)]
        |
        |// The two features below are required by the Verus build but go unused on a
        |// plain cargo build, and `verus_keep_ghost` is set by Verus rather than
        |// declared to cargo, so both lints fire only on the non-Verus path.
        |#![allow(unused_features)]
        |#![allow(unexpected_cfgs)]
        |
        |#![feature(proc_macro_hygiene)]
        |#![cfg_attr(not(verus_keep_ghost), feature(stmt_expr_attributes))]"""

  // Extra arguments forwarded to Verus (i.e. placed after `--`) by every
  // cargo-verus target in the generated Makefiles. Carries a raised resource limit
  // and a fixed SMT random seed; see the emitted comment for why each is needed.
  // Exposed as a Make variable so a developer can override it without editing
  // generated files.
  val smtOptsMakeVar: ST =
    st"""# Extra arguments forwarded to Verus (placed after `--`), for every
        |# cargo-verus target below. These apply to dependency crates too -- notably
        |# vstd, which is verified from source on a clean build, so its proofs must
        |# discharge under these settings as well as our own.
        |#
        |# --rlimit 100
        |#   Verus' SMT resource limit for a function body check. Its default of 10
        |#   is not enough here. 100 is an order-of-magnitude bump that stays inside
        |#   the range Verus itself uses on its heavier projects.
        |#
        |# --smt-option smt.random_seed=7
        |#   7 is arbitrary. Z3's default seed is 0, and 0 is the one value at which
        |#   vstd's GhostSubseq::agree_map (resource/impls/seq.rs) fails its
        |#   postcondition; seeds 1, 2, 3, 7 and 42 all discharge it. The point is to
        |#   be off the default and identical on every machine, not this number.
        |SMT_OPTS ?= --rlimit 100 --smt-option smt.random_seed=7"""

  @pure def defaultRustToolChainToml(store: Store): ST = {
    val versions = MicrokitUtil.getMicrokitVersions(store)
    val channel = versions.get("rust-channel").get
    return st"""${CommentTemplate.safeToEditComment_hash}
               |
               |# Verus hooks into rustc internals (rustc-dev, rustc_driver) which change
               |# release-to-release with no stability guarantees, so the channel is pinned
               |# to the toolchain the Verus release in Cargo.toml was built against (see
               |# `verus --version`) -- a different channel may cause compilation failures.
               |# If you update the Verus dependencies, update the channel to match.
               |# The rustc-dev and rust-src components are required for Verus verification;
               |# llvm-tools-preview is needed for microkit's no_std linking;
               |# rust-analyzer provides IDE support (code completion, type hints, etc.).
               |
               |[toolchain]
               |channel = "$channel"
               |components = [ "rustfmt", "rust-src", "rustc-dev", "llvm-tools-preview", "rust-analyzer" ]
               |"""
  }

  //val verusCargoDependencies: ST =
  //  st"""vstd = { git = "https://github.com/verus-lang/verus.git", default-features=false, rev="$verusCommitTip"}
  //      |builtin = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }
  //      |builtin_macros = { git = "https://github.com/verus-lang/verus.git", rev="$verusCommitTip" }"""

  @pure def verusCargoDependencies(store: Store): ST = {
    val versions = MicrokitUtil.getMicrokitVersions(store)
    return (
      st"""# -----------------------------------------------------------------------------
          |# Verus crate dependencies
          |#
          |# It is recommended to use the Verus release:
          |# https://github.com/verus-lang/verus/releases/tag/release/${versions.get("verus-release").get}
          |# -----------------------------------------------------------------------------
          |vstd = { version = "=${versions.get("vstd").get}", default-features=false }
          |verus_builtin = { version = "=${versions.get("verus_builtin").get}" }
          |verus_builtin_macros = { version = "=${versions.get("verus_builtin_macros").get}" }""")
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
      st"""# -----------------------------------------------------------------------------
          |# seL4 Rust crate dependencies
          |# -----------------------------------------------------------------------------
          |sel4 = { git = "https://github.com/seL4/rust-sel4", features = ["single-threaded"], optional = true$sel4Opt}
          |sel4-logging = { git = "https://github.com/seL4/rust-sel4", optional = true$sel4LoggingOpt}""")
  }
}
