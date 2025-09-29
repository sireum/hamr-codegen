// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._

object JvmMicrokitUtil {
  @pure def getMicrokitVersions: Map[String, String] = {
    return JvmMicrokitUtilExt.getMicrokitVersions
  }
}

@ext object JvmMicrokitUtilExt {
  @pure def getMicrokitVersions: Map[String, String] = $
}
