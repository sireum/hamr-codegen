// #Sireum
package org.sireum.hamr.codegen.microkit.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.reporting.CodegenReport
import org.sireum.message.Position

object MicrokitReport {
  @pure def empty(systemDescriptionUri: String): MicrokitReport = {
    return MicrokitReport(systemDescriptionUri = systemDescriptionUri, componentReport = HashSMap.empty)
  }
}

@datatype class IdPathR(val idPath: ISZ[String])

@datatype class MicrokitReport(val systemDescriptionUri: String,
                               val componentReport: HashSMap[IdPathR, ComponentReport]) extends CodegenReport

@datatype class ComponentReport(val cCodeReport: Option[CCodeReport],
                                val rustReport: Option[RustReport],
                                val gumboReport: Option[GumboReport],
                                val gumboXReport: Option[GumboXReport])

@datatype class CCodeReport()

@datatype class RustReport(val entrypointReport: HashSMap[String, Position],
                           val apiReport: RustApiReport)

@datatype class RustApiReport(val extern_c_apiPath: String,

                              val developerApiPath: String,
                              val developerApiReport: HashSMap[String, Position],

                              val testApiPath: String)

@datatype class IdPos(val id: String,
                      val pos: Position)

@datatype class GumboReport(val stateReport: HashSMap[String, Position],
                            val methodsReport: HashSMap[String, Position],
                            val invariantsReport: HashSMap[String, Position],
                            val integrationReport: Option[GubmoIntegrationReport],
                            val initializeReport: HashSMap[String, Position],
                            val computeReport: Option[GumboComputeReport])

@datatype class GubmoIntegrationReport(val assumesReport: HashSMap[String, Position],
                                       val guaranteesReport: HashSMap[String, Position])

@datatype class GumboComputeReport(val assumesReport: HashSMap[String, Position],
                                   val guaranteesReport: HashSMap[String, Position],
                                   val casesReport: HashSMap[String, Position],
                                   val handlers: HashSMap[String, Position])

@datatype class GumboXReport(val gumboxMethods: HashSMap[String, Position])