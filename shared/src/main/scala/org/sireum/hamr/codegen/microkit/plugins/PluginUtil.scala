// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.plugin.Plugin

object PluginUtil {
  @pure def getMicrokitPlugins(plugins: ISZ[Plugin]): ISZ[MicrokitPlugin] = {
    var ret: ISZ[MicrokitPlugin] = ISZ()
    for(p <- plugins) {
      p match {
        case p: MicrokitPlugin => ret = ret :+ p
        case _ =>
      }
    }
    return ret
  }

  @pure def getMicrokitFinalizePlugins(plugins: ISZ[Plugin]): ISZ[MicrokitFinalizePlugin] = {
    var ret: ISZ[MicrokitFinalizePlugin] = ISZ()
    for (p <- plugins) {
      p match {
        case p: MicrokitFinalizePlugin => ret = ret :+ p
        case _ =>
      }
    }
    return ret
  }
}
