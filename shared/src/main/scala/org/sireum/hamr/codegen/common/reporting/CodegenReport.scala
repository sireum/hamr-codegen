// #Sireum
package org.sireum.hamr.codegen.common.reporting

import org.sireum._

@sig trait CodegenReport

@datatype class CodegenReports(val reports: Map[String, CodegenReport])

@datatype class ToolReport(val commandLineArgs: String) extends CodegenReport