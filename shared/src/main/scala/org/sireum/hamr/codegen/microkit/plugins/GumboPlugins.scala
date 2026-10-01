// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{DefaultContractObserverPlugin, DefaultGumboCPlugin, DefaultGumboMonitorPlugin, DefaultGumboRustPlugin, DefaultGumboSysAssertMonitorPlugin, DefaultGumboSysAssertVcGenPlugin, DefaultGumboXPlugin, DefaultStateVarPortsPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitGumboLinter

object GumboPlugins {

  val gumboPlugins: ISZ[Plugin] = ISZ(
    DefaultContractObserverPlugin(),
    DefaultStateVarPortsPlugin(),
    DefaultGumboMonitorPlugin(),
    DefaultGumboSysAssertMonitorPlugin(),
    DefaultGumboSysAssertVcGenPlugin(),
    DefaultMicrokitGumboLinter(),
    DefaultGumboCPlugin(),
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin()
  )
}
