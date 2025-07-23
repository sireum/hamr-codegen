// #Sireum

import org.sireum._

// HAMR does not rely on Sireum's CLI, so we define equivalent option data structures here.
// The corresponding Sireum structures can be copied into these local versions.

// Ignore the Tipe warning "'hamr' is not a member of package 'org.sireum'"
println(org.sireum.cli.JSON.fromCliOpt(org.sireum.hamr.codegen.HamrCodegenCli.codeGenTool, T))