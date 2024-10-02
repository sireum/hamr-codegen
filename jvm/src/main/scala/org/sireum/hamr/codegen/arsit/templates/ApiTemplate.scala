// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.arsit.gcl.GumboGen.GclApiContributions
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeature, AadlFeatureData, AadlFeatureEvent, AadlPort}
import org.sireum.hamr.codegen.common.types.{AadlType, TypeUtil}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir._

object ApiTemplate {

  val apiOperationalId: String = "operational_api"
  val apiOperationalBridgeId: String = "c_operational_api"

  val apiInitializationId: String = "initialization_api"
  val apiInitializationBridgeId: String = "c_initialization_api"

  def api(packageName: String,
          basePackageName: String,
          names: NameProvider,
          ports: ISZ[Port],
          integrationContracts: Map[AadlPort, GclApiContributions]): ST = {

    val portDefs: ISZ[ST] = st"id: Art.BridgeId" +:
      ops.ISZOps(ports).map((p: Port) => st"${p.name}_Id : Art.PortId")

    val portTraitDefs: ISZ[ST] = ops.ISZOps(portDefs).map((s: ST) => st"def ${s}")
    val portParams: ISZ[ST] = ops.ISZOps(portDefs).map((s: ST) => st"val ${s}")

    val inPorts = ports.filter((p: Port) => CommonUtil.isInPort(p.feature))
    val outPorts = ports.filter((p: Port) => !CommonUtil.isInPort(p.feature))

    def getContract(f: AadlFeature): Option[GclApiContributions] = {
      val ret: Option[GclApiContributions] = f match {
        case i: AadlPort => integrationContracts.get(i)
        case _ => None()
      }
      return ret
    }

    var traitSPFs: ISZ[ST] = ISZ()
    val getters = inPorts.map((p: Port) => {
      val gclApiContributions = getContract(p.aadlFeature)
      gclApiContributions match {
        case Some(c) if c.objectContributions.nonEmpty => traitSPFs = traitSPFs :+ st"${(c.objectContributions, "\n\n")}"
        case _ =>
      }
      getterApi(p, gclApiContributions)
    })

    var opSPFs: ISZ[ST] = ISZ()
    val setters = outPorts.map((p: Port) => {
      val gclApiContributions = getContract(p.aadlFeature)
      gclApiContributions match {
        case Some(c) if c.objectContributions.nonEmpty => opSPFs = opSPFs :+ st"${(c.objectContributions, "\n\n")}"
        case _ =>
      }
      setterApi(p, gclApiContributions)
    })

    def collect(name: String, v: ISZ[ST]): Option[ST] = {
      return if (v.nonEmpty)
        Some(
          st"""object ${name} {
              |  ${(v, "\n\n")}
              |}
              |
              |import ${name}._
              |""")
      else None()
    }

    val ret: ST =
      st"""// #Sireum
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import art._
          |import ${basePackageName}._
          |
          |${collect(names.api, traitSPFs ++ opSPFs)}
          |@sig trait ${names.api} {
          |  ${(portTraitDefs, "\n")}
          |
          |  ${(setters, "\n\n")}
          |
          |  def logInfo(msg: String): Unit = {
          |    Art.logInfo(id, msg)
          |  }
          |
          |  def logDebug(msg: String): Unit = {
          |    Art.logDebug(id, msg)
          |  }
          |
          |  def logError(msg: String): Unit = {
          |    Art.logError(id, msg)
          |  }
          |}
          |
          |@datatype class ${names.apiInitialization} (
          |  ${(portParams, ",\n")}) extends ${names.api}
          |
          |@datatype class ${names.apiOperational} (
          |  ${(portParams, ",\n")}) extends ${names.api} {
          |
          |  ${(getters, "\n\n")}
          |}
          |"""

    return ret
  }

  def entryPointParams(names: NameProvider): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ(
      st"${apiInitializationId}: ${names.apiInitialization}",
      st"${apiOperationalId}: ${names.apiOperational}")
    return ret
  }

  def apiBridgeEntry(names: NameProvider,
                     bridgeCompanionObjectName: String,
                     ports: ISZ[Port],
                     isEntry: B): ST = {
    val (id, typ, companionId): (String, String, String) = {
      if (isEntry) {
        (apiInitializationId, names.apiInitialization, apiInitializationBridgeId)
      } else {
        (apiOperationalId, names.apiOperational, apiOperationalBridgeId)
      }
    }

    val _ports = ops.ISZOps(ports).map((p: Port) => s"${p.name}.id")
    val ret: ST = {
      st"""val ${id} : ${typ} = {
          |  val api = ${typ}(
          |    id,
          |    ${(_ports, ",\n")}
          |  )
          |  ${bridgeCompanionObjectName}.${companionId} = Some(api)
          |  api
          |}"""
    }
    return ret
  }

  def companionObjectApiInstances(names: NameProvider): ST = {
    val ret: ST =
      st"""var ${apiInitializationBridgeId}: Option[${names.apiInitialization}] = None()
          |var ${apiOperationalBridgeId}: Option[${names.apiOperational}] = None()"""
    return ret
  }

  def addId(s: String): String = {
    return s"${s}_Id"
  }

  @pure def putValue(p: Port): ST = {
    val q = p.getPortTypeNames.qualifiedPayloadName
    val isEmpty = p.getPortTypeNames.isEmptyType
    return st"""Art.putValue(${addId(p.name)}, ${q}${if (isEmpty) "()" else "(value)"})"""
  }

  def genContracts(p: Port, gclApiContributions: Option[GclApiContributions]): (Option[ST], Option[ST], Option[ST]) = {

    val isIncoming = p.feature.direction == Direction.In

    val (aadlType, isEvent, isData): (AadlType, B, B) = p.aadlFeature match {
      case i: AadlEventDataPort => (i.aadlType, T, T)
      case i: AadlDataPort => (i.aadlType, F, T)
      case i: AadlEventPort => (TypeUtil.EmptyType, T, F)
      case x => halt("Unexpected port type: $x")
    }

    val portDir: String = if (isIncoming) "incoming" else "outgoing"
    val portType = st"${if (isEvent) "event " else ""}${if (isData) "data " else ""}"

    val specType: String =
      if (p.aadlFeature.isInstanceOf[AadlDataPort]) aadlType.nameProvider.qualifiedReferencedTypeName
      else  s"Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]"

    var dtcontributions = ISZ(
      st"""// Logika spec var representing port state for ${portDir} ${portType}port
          |@spec var ${p.name}: ${specType} = $$"""
    )
    gclApiContributions match {
      case Some(GclApiContributions(_, datatypeContributions, _, _)) if datatypeContributions.nonEmpty =>
        dtcontributions = dtcontributions ++ datatypeContributions
      case _ =>
    }
    val optDatatypeContributions: Option[ST] = Some(st"${(dtcontributions, "\n")}")

    val optRequires: Option[ST] = gclApiContributions match {
      case Some(GclApiContributions(_, _, requires, _)) if requires.nonEmpty =>
        Some(
          st"""Requires(
              |  ${(requires, ",\n")}
              |),""")
      case _ => None()
    }

    val optEnsures: Option[ST] = gclApiContributions match {
      case Some(GclApiContributions(_, _, _, ensures)) if ensures.nonEmpty =>
        Some(st"${(ensures, ",\n")},")
      case _ => None()
    }

    return (optDatatypeContributions, optRequires, optEnsures)
  }

  def setterApi(p: Port, gclApiContributions: Option[GclApiContributions]): ST = {
    val q = p.getPortTypeNames.qualifiedReferencedTypeName
    val isEmpty = p.getPortTypeNames.isEmptyType

    val isEvent = p.aadlFeature.isInstanceOf[AadlFeatureEvent]
    val isData = p.aadlFeature.isInstanceOf[AadlFeatureData]

    val (optDatatypeContributions, optRequires, optEnsures) = genContracts(p, gclApiContributions)

    val outValue: String =
      if (isEvent) s"Some(${if (isData) "value" else "Empty()"})"
      else "value"

    val contract: ST =
      st"""Contract(
          |  $optRequires
          |  Modifies(${p.name}),
          |  Ensures(
          |    $optEnsures
          |    ${p.name} == $outValue
          |  )
          |)
          |Spec {
          |  ${p.name} = $outValue
          |}
          |"""

    val ret: ST = p.feature.category match {
      case FeatureCategory.DataPort =>
        st"""def put_${p.name}(value : ${q}) : Unit = {
            |  ${contract}
            |  ${putValue(p)}
            |}"""
      case FeatureCategory.EventPort =>
        st"""def put_${p.name}(${if (isEmpty) "" else s"value : ${q}"}) : Unit = {
            |  ${contract}
            |  ${putValue(p)}
            |}"""
      case FeatureCategory.EventDataPort =>
        st"""def put_${p.name}(${if (isEmpty) "" else s"value : ${q}"}) : Unit = {
            |  ${contract}
            |  ${putValue(p)}
            |}"""
      case _ => halt("Unexpected: $p")
    }

    return (st"""$optDatatypeContributions
                |
                |$ret""")
  }

  @pure def getterApi(p: Port, gclApiContributions: Option[GclApiContributions]): ST = {
    val isEvent = CommonUtil.isAadlEventPort(p.feature)
    val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
    val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
    val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
    val value: String = if (isEvent) "Empty()" else "v"

    val (optDatatypeContributions, optRequires, optEnsures) = genContracts(p, gclApiContributions)

    val inValue: String =
      if(p.aadlFeature.isInstanceOf[AadlDataPort]) s"Some(${p.name})"
      else p.name

    val ret: ST =
      st"""$optDatatypeContributions
          |
          |def get_${p.name}() : Option[${typeName}] = {
          |  Contract(
          |    $optRequires
          |    Ensures(
          |      $optEnsures
          |      Res == $inValue
          |    )
          |  )
          |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) =>
          |      Art.logError(id, s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |      None[${typeName}]()
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}"""

    return ret
  }
}
