// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.plugins.rust.gumbo._

object GumboPlugins {

  val gumboPlugins: ISZ[Plugin] = ISZ(
    DefaultMicrokitGumboLinter(),
    DefaultGumboRustPlugin(),
    DefaultGumboXPlugin()
  )
}
