// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.attestation

import org.sireum._

object AttestationReportContainers {

  @sig trait Report extends PrettySTProvider

  @datatype class AttestationReport(val reports: ISZ[Report]) extends Report {
    override def prettyST: ST = {
      return (
        st"""{
            |  "type": "AttestationReport",
            |  "reports": [
            |    ${(for (r <- reports) yield r.prettyST, ",\n")}
            |  ]
            |}""")
    }
  }

  @datatype class ComponentReport(val idPath: ISZ[String],
                                  val classifier: ISZ[String],
                                  val meta: Option[String],
                                  val reports: ISZ[ComponentLevelReport]) extends Report with MetaProvider {
    override def prettyST: ST = {
      return (
        st"""{
            |  "type": "ComponentReport",
            |  "idPath": [${(for(i <- idPath) yield s"\"$i\"", ", ")}],
            |  "classifier": [${(for(i <- classifier) yield s"\"$i\"", ", ")}],
            |$metaOpt
            |  "reports": [
            |    ${(for (r <- reports) yield r.prettyST, ",\n")}
            |   ]
            |}""")
    }
  }

  @sig trait SliceReport {
    @pure def slices: ISZ[Slice]
  }

  @enum object ContractKind {
    "Assume"
    "Guarantee"
    "ComputeCase"
    "Initialize"
    "Integration"
    "DatatypeInvariant"
  }

  @sig trait ComponentLevelReport extends SliceReport with PrettySTProvider with MetaProvider

  @datatype class ComponentContractReport(val id: String,
                                          val kind: ContractKind.Type,
                                          val meta: Option[String],
                                          val slices: ISZ[Slice]) extends ComponentLevelReport {
    override def prettyST: ST = {
      return (
        st"""{
            |  "type": "ComponentContractReport",
            |  "id": "$id",
            |  "kind": "$kind",
            |$metaOpt
            |  "slices": [
            |    ${(for (s <- slices) yield s.prettyST, ",\n")}
            |  ]
            |}""")
    }
  }

  @enum object SliceKind {
    "C"
    "Microkit"
    "Model"
    "Rust"
    "Verus"
  }

  @datatype class Slice(val kind: SliceKind.Type,
                        val meta: Option[String],
                        val pos: Position) extends PrettySTProvider with MetaProvider {

    override def prettyST: ST = {
      return (
        st"""{
            |  "type": "Slice",
            |  "kind": "$kind",
            |$metaOpt
            |  "pos":
            |    ${pos.prettyST}
            |}""")
    }
  }

  @datatype class Position(val uri: String,
                           val beginLine: Z,
                           val beginCol: Z,
                           val endLine: Z,
                           val endCol: Z,
                           val offset: Z,
                           val length: Z) extends PrettySTProvider {
    override def prettyST: ST = {
      return (
      st"""{
          |  "type": "Position",
          |  "uri": "$uri",
          |  "beginLine": $beginLine,
          |  "beginCol": $beginCol,
          |  "endLine": $endLine,
          |  "endCol": $endCol,
          |  "offset": $offset,
          |  "length": $length
          |}""")
    }
  }


  @sig trait PrettySTProvider {
    def prettyST: ST
  }

  @sig trait MetaProvider {
    def metaOpt: Option[ST] = {
      return (if(meta.nonEmpty) Some(st"""  "meta": "${meta.get}",""")
      else None())
    }

    def meta: Option[String]
  }
}
