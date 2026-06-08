// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{DefaultGumboMonitorPlugin, DefaultGumboRustPlugin, DefaultGumboSysAssertMonitorPlugin, DefaultGumboSysAssertVcGenPlugin, DefaultGumboXPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitGumboLinter

object GumboPlugins {

  val gumboPlugins: ISZ[Plugin] = ISZ(
    DefaultGumboMonitorPlugin(),
    DefaultGumboSysAssertMonitorPlugin(),
    DefaultGumboSysAssertVcGenPlugin(),
    DefaultMicrokitGumboLinter(),
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin()
  )
}
