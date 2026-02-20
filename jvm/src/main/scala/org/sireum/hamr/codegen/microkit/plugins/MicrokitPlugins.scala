// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.attestation.{AttestationPlugin, AttestationReporterPlugin}
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin_DomainScheduler
import org.sireum.hamr.codegen.microkit.plugins.c.types.DefaultCTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.c.connections.DefaultCConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.plugins.reporting.MicrokitReporterPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.DefaultCRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.component.DefaultCRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.gumbo.{DefaultGumboRustPlugin, DefaultGumboXPlugin}
import org.sireum.hamr.codegen.microkit.plugins.rust.testing.DefaultCRustTestingPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.DefaultCRustTypePlugin

object MicrokitPlugins {

  // MicrokitReportPlugin needs access to Os.Path so define the default plugins in JVM
  val defaultMicrokitPlugins: ISZ[Plugin] = ISZ(
    // lint-ers
    DefaultMicrokitLinterPlugin(),

    // type-ers
    DefaultCTypePlugin(), DefaultCRustTypePlugin(),

    // connection-ers
    DefaultCConnectionProviderPlugin(),

    // api-ers
    DefaultCRustApiPlugin(),

    // component-ers
    CComponentPlugin_DomainScheduler(),
    DefaultCRustComponentPlugin(),

    // test-ers
    DefaultCRustTestingPlugin(),

    // gumbo-ers
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin(),

    MicrokitReporterPlugin(),
    AttestationPlugin(),
    AttestationReporterPlugin()
  )
}
