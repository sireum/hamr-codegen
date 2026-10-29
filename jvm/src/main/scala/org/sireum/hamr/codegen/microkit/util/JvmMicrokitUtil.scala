// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._

object JvmMicrokitUtil {
  @pure def getMicrokitVersions: Map[String, String] = {
    return JvmMicrokitUtilExt.getMicrokitVersions
  }

  // the coverage report's files (resources/coverage-report), by file name
  @pure def getCoverageReportFiles: Map[String, String] = {
    return JvmMicrokitUtilExt.getCoverageReportFiles
  }
}

@ext object JvmMicrokitUtilExt {
  @pure def getMicrokitVersions: Map[String, String] = $

  @pure def getCoverageReportFiles: Map[String, String] = $
}
