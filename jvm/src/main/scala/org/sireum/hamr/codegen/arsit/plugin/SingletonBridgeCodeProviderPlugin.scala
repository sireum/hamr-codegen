// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.arsit.templates.ApiTemplate
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil
import org.sireum.message.Reporter

@record class SingletonBridgeCodeProviderPlugin extends BridgeCodeProviderPlugin {
  @strictpure def name: String = "Singleton Bridge Code Provider Plugin"

  @pure def generate(nameProvider: NameUtil.NameProvider,
                     component: AadlThreadOrDevice,
                     ports: ISZ[Port],

                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     reporter: Reporter): BridgeCodeProviderPlugin.BridgeCodeContributions = {

    val portParams: ISZ[String] = ports.map((p: Port) => {
      val artPortType: String = if (p.urgency.nonEmpty) "UrgentPort" else "Port"
      val portType = p.getPortTypeNames.qualifiedReferencedTypeName
      s"${p.name}: ${artPortType}[${portType}]"
    })

    val entryPointTemplate = SingletonEntryPointProviderPlugin.getEntryPointTemplate(nameProvider, component, ports)

    val apiDecls: ISZ[ST] = ISZ(
      ApiTemplate.apiBridgeEntry(nameProvider, nameProvider.bridge, ports, T),
      ApiTemplate.apiBridgeEntry(nameProvider, nameProvider.bridge, ports, F))

    val imports: ISZ[ST] = ISZ(
      st"${nameProvider.packageName}.{${nameProvider.componentSingletonType} => component}"
    )

    val portOpt: Option[ST] = if (ports.isEmpty) None()
    else Some(
      st"""
          |${(ports.map((p: Port) => s"${p.name}.id"), ",\n")},""")

    val e = (arg: EntryPointProviderPlugin.EntryPointContributions) =>
      st"""// #Sireum
          |
          |package ${nameProvider.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${nameProvider.basePackage}._
          |${addImports(imports ++ arg.imports.map((m: String) => st"$m"))}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@datatype class ${nameProvider.bridge}(
          |  val id: Art.BridgeId,
          |  val name: String,
          |  val dispatchProtocol: DispatchPropertyProtocol,
          |  val dispatchTriggers: Option[ISZ[Art.PortId]],
          |
          |  ${(portParams, ",\n")}
          |  ) extends Bridge {
          |
          |  val ports : Bridge.Ports = Bridge.Ports(
          |    dataIns = ISZ[art.UPort](${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    dataOuts = ISZ[art.UPort](${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventIns = ISZ[art.UPort](${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventOuts = ISZ[art.UPort](${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")})
          |  )
          |
          |  ${(apiDecls, "\n\n")}
          |
          |  val entryPoints : Bridge.EntryPoints =
          |    ${nameProvider.bridge}.EntryPoints(
          |      id,
          |      $portOpt
          |
          |      dispatchTriggers,
          |
          |      ${(ISZ(ApiTemplate.apiInitializationId, ApiTemplate.apiOperationalId), ",\n")})
          |}
          |
          |object ${nameProvider.bridge} {
          |
          |  ${ApiTemplate.companionObjectApiInstances(nameProvider)}
          |  ${(arg.bridgeCompanionBlocks, "\n\n")}
          |
          |  ${arg.entryPoint}
          |}"""

    return BridgeCodeProviderPlugin.BridgeCodeContributions(entryPointTemplate, e, ISZ())
  }

  @pure def addImports(imports: ISZ[ST]): Option[ST] = {
    val s: Set[String] = Set.empty[String] ++ (imports.map((m: ST) => s"import ${m.render}"))
    return if (s.nonEmpty) Some(st"${(s.elements, "\n")}") else None()
  }
}