// #Sireum
package org.sireum.hamr.codegen.microkit.reporting

import org.sireum._
import org.sireum.hamr.codegen.common.reporting.CodegenReport
import org.sireum.message.Position

@datatype class IdPos(val id: String,
                      val pos: Position)

@datatype class IdPathR(val idPath: ISZ[String])


@datatype class MicrokitReport(val componentReport: HashSMap[IdPathR, ComponentReport]) extends CodegenReport

@enum object ComponentKind {
  "System"
  "Process"
  "Thread"
  "Data"
}

@sig trait Pretty {
  @pure def pretty(linkRenderer: (String, String, Position) => ST): ST
}

@sig trait ModelProperties

@datatype class SimpleProperty(val name: String,
                               val value: String) extends ModelProperties

@datatype class ComponentReport(val path: ISZ[String],
                                val kind: ComponentKind.Type,

                                val optModelType: Option[IdPos],
                                val modelImplementation: IdPos,

                                val modelProperties: ISZ[ModelProperties],

                                val subcomponents: ISZ[IdPathR],

                                val ports: HashSMap[IdPathR, PortReport],

                                val cCodeReport: Option[CCodeReport],
                                val rustReport: Option[RustReport],
                                val gumboReport: Option[GumboReport],
                                val gumboXReport: Option[GumboXReport]) {
  @pure def prettyPortReport(linkRenderer: (String, String, Position) => ST): ST = {
    return(
      st"""- **APIs**
          |
          |    <table>
          |    <tr><th>Port Name</th><th>Direction</th><th>Kind</th><th>Payload</th><th>Realizations</th></tr>
          |    ${(for(p <- ports.values) yield p.pretty(linkRenderer), "\n")}
          |    </table>""")
  }
}

@enum object PortKind {
  "Event"
  "EventData"
  "Data"
}

@enum object PortDirection {
  "In"
  "Out"
}

@sig trait PortLangRealization extends Pretty {
  @pure def name: String
  @pure def title: String
  @pure def pos: Position
}

@datatype class PortLanguageArtifact(val name: String,
                                     val title: String,
                                     val pos: Position) extends PortLangRealization {

  @pure override def pretty(linkRenderer: (String, String, Position) => ST): ST = {
    return linkRenderer(name, title, pos)
  }
}

@datatype class PortReport(val name: ISZ[String],
                           val kind: PortKind.Type,
                           val direction: PortDirection.Type,
                           val payload: Option[String],
                           val modelPos: Position,
                           val languageRealizations: ISZ[PortLangRealization]) extends Pretty {
  @pure override def pretty(linkRenderer: (String, String, Position) => ST): ST = {
    val kinds: String = kind match {
      case PortKind.EventData => "Event Data"
      case PortKind.Event => "Event"
      case PortKind.Data => "Data"
      case x => halt(s"Unexpected: $x")
    }


    val realizations: ST =
      if (direction == PortDirection.In)
        st"${(for (r <- ops.ISZOps(languageRealizations).reverse) yield r.pretty(linkRenderer), " -> ")}"
      else
        st"${(for (r <- languageRealizations) yield r.pretty(linkRenderer), " -> ")}"

    val id = name(name.lastIndex)
    return (
    st"""<tr><td>${linkRenderer(id, "Model", modelPos)}</td>
        |    <td>$direction</td><td>$kinds</td>
        |    <td>$payload</td><td>$realizations</td></tr>""")
  }
}

@datatype class CCodeReport()

@datatype class RustReport(val entrypointReport: HashSMap[String, Position],
                           val apiReport: RustApiReport)

@datatype class RustApiReport(val extern_c_apiPath: String,

                              val developerApiPath: String,
                              val developerApiReport: HashSMap[String, Position],

                              val testApiPath: String)

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