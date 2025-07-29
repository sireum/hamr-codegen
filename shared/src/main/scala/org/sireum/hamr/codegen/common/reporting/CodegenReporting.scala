// #Sireum
package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.Store

object CodegenReporting {
  val KEY_CODEGEN_REPORTS: String = "KEY_CODEGEN_REPORTS"

  val KEY_TOOL_REPORT: String = "KEY_TOOL_REPORT"

  type ReportStruct = Map[String, CodegenReport]

  @strictpure def emptyToolReport: ToolReport = ToolReport(
    commandLineArgs = "<empty-arguments>",
    status = Status.Failure,
    warningMessages = ISZ(),
    errorMessages = ISZ(),
    resources = ISZ())

  @strictpure def getCodegenReports(store: Store): Option[ReportStruct] =
    store.get(KEY_CODEGEN_REPORTS) match {
      case Some(r) => Some(r.asInstanceOf[CommonUtil.MapValue[String, CodegenReport]].map)
      case _ => None()
    }

  @strictpure def getCodegenReport(id: String, store: Store): Option[CodegenReport] =
    getCodegenReports(store) match {
      case Some(reports) =>
        reports.get(id) match {
          case Some(r) => Some(r)
          case _ => None()
        }
      case _ => None()
    }

  @strictpure def addCodegenReport(id: String, r: CodegenReport, store: Store): Store =
    getCodegenReports(store) match {
      case Some(reports) => store + (KEY_CODEGEN_REPORTS ~> (CommonUtil.MapValue(reports + id ~> r)))
      case _ => store + (KEY_CODEGEN_REPORTS ~> (CommonUtil.MapValue(Map.empty[String, CodegenReport] + id ~> r)))
    }
}
