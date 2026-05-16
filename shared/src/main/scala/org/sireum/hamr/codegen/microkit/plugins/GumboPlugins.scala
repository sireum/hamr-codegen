// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{DefaultGumboMonitorPlugin, DefaultGumboRustPlugin, DefaultGumboXPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.DefaultMicrokitGumboLinter
import org.sireum.hamr.codegen.microkit.plugins.monitors.DefaultRustScheduleAwareMonitorPlugin

object GumboPlugins {

  val gumboPlugins: ISZ[Plugin] = ISZ(
    DefaultRustScheduleAwareMonitorPlugin(),
    DefaultGumboMonitorPlugin(),
    DefaultMicrokitGumboLinter(),
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin()
  )
}
