// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.apis.DefaultCRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.attestation.AttestationPlugin
import org.sireum.hamr.codegen.microkit.plugins.component.DefaultCRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{DefaultGumboRustPlugin, DefaultGumboXPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.plugins.reporting.MicrokitReporterPlugin
import org.sireum.hamr.codegen.microkit.plugins.types.{DefaultCRustTypePlugin, DefaultCTypePlugin}

object MicrokitPlugins {

  // MicrokitReportPlugin needs access to Os.Path so define the default plugins in JVM
  val defaultMicrokitPlugins: ISZ[Plugin] = ISZ(
    // lint-ers
    DefaultMicrokitLinterPlugin(),

    // type-ers
    DefaultCTypePlugin(), DefaultCRustTypePlugin(),

    // api-ers
    DefaultCRustApiPlugin(),

    // component-ers
    DefaultCRustComponentPlugin(),

    // gumbo-ers
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin(),

    AttestationPlugin(),
    MicrokitReporterPlugin()
  )
}
