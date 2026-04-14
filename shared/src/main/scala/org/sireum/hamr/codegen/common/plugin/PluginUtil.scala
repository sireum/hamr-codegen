// #Sireum
package org.sireum.hamr.codegen.common.plugin

import org.sireum._

object PluginUtil {

  @pure def getModelTransformerPlugins(plugins: ISZ[Plugin]): ISZ[ModelTransformerPlugin] = {
    var ret: ISZ[ModelTransformerPlugin] = ISZ()
    for (p <- plugins) {
      p match {
        case p: ModelTransformerPlugin => ret = ret :+ p
        case _ =>
      }
    }
    return ret
  }
}
