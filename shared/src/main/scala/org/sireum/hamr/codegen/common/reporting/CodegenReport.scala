// #Sireum
package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.message.Message

@sig trait CodegenReport

@datatype class CodegenReports(val reports: Map[String, CodegenReport])

@enum object Status {
  "Success"
  "Failure"
}

@datatype class ResourceReport(val path: String,
                               val overwrittenIfExists: B)

@datatype class ToolReport(val commandLineArgs: String,
                           val status: Status.Type,
                           val warningMessages: ISZ[Message],
                           val errorMessages: ISZ[Message],
                           val resources: ISZ[ResourceReport]) extends CodegenReport