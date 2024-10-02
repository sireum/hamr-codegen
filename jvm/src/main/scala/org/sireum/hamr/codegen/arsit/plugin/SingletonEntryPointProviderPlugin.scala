// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.templates.{ApiTemplate, EntryPointTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, AnnexClauseInfo, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.FeatureCategory
import org.sireum.message.Reporter

@record class SingletonEntryPointProviderPlugin extends EntryPointProviderPlugin {

  @strictpure def name: String = "Singleton Entry Point Provider Plugin"

  @strictpure def canHandleEntryPointProvider(component: AadlThreadOrDevice, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo], arsitOptions: ArsitOptions, symbolTable: SymbolTable, aadlTypes: AadlTypes): B =
    T

  @pure def handleEntryPointProvider(component: AadlThreadOrDevice, nameProvider: NameProvider, ports: ISZ[Port],

                                     resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                     entryPointTemplate: EntryPointTemplate,

                                     arsitOptions: ArsitOptions, symbolTable: SymbolTable, aadlTypes: AadlTypes, projectDirectories: ProjectDirectories, reporter: Reporter): EntryPointProviderPlugin.EntryPointContributions = {

    return EntryPointProviderPlugin.EntryPointContributions(
      imports = ISZ(),
      bridgeCompanionBlocks = ISZ(),
      entryPoint = entryPointTemplate.generateDefault(),
      resources = ISZ()
    )
  }
}

object SingletonEntryPointProviderPlugin {

  @pure def getEntryPointTemplate(nameProvider: NameProvider,
                                  component: AadlThreadOrDevice,
                                  ports: ISZ[Port]): EntryPointTemplate = {
    val componentName = "component"

    val parameters: ISZ[ST] = (
      (st"${nameProvider.bridge}Id : Art.BridgeId" +:
        ports.map((p: Port) => st"${addId(p.name)} : Art.PortId")) :+
        st"dispatchTriggers : Option[ISZ[Art.PortId]]") ++
      ApiTemplate.entryPointParams(nameProvider)

    val localVars: ISZ[ST] = ISZ(
      st"""
          |val dataInPortIds: ISZ[Art.PortId] = IS(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |val eventInPortIds: ISZ[Art.PortId] = IS(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |val dataOutPortIds: ISZ[Art.PortId] = IS(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |val eventOutPortIds: ISZ[Art.PortId] = IS(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})"""
    )

    val entryPointTemplate = EntryPointTemplate(
      parameters = parameters,
      localVars = localVars,
      defaultActivateBody = st"${activateBody(componentName, nameProvider)}",
      defaultInitialiseBody = st"${initialiseBody(componentName, nameProvider, F)}",
      defaultTestInitialiseBody = st"${initialiseBody(componentName, nameProvider, T)}",
      defaultComputeBody = st"${computeBody(s"${nameProvider.bridge}Id", componentName, nameProvider, ports, component.dispatchProtocol, F, F)}",
      defaultTestComputeBody = st"${computeBody(s"${nameProvider.bridge}Id", componentName, nameProvider, ports, component.dispatchProtocol, T, F)}",
      defaultDeactivateBody = st"${deactivateBody(componentName, nameProvider)}",
      defaultFinaliseBody = st"${finaliseBody(componentName, nameProvider)}",
      defaultRecoverBody = st"${recoverBody(componentName, nameProvider)}")

    return entryPointTemplate
  }

  @strictpure def addId(s: String): String = s"${s}_Id"

  @pure def initialiseBody(componentName: String,
                           names: NameProvider,
                           isTesting: B): ST = {
    // determine communication substrate method name based on if this is test infrastructure
    //  or a "regular" entry point call
    val sendOutputName: String = if (isTesting) "releaseOutput" else "sendOutput"

    // generate call to application code Initialize EP
    //  e.g., component.initialise(initialization_api)
    // generate call to propagate writes to output ports to the communication substrate
    val ret: ST =
    st"""// implement the following method in '${componentName}':  def ${EntryPoints.initialise.string}(api: ${names.apiInitialization}): Unit = {}
        |${componentName}.${EntryPoints.initialise.string}(${ApiTemplate.apiInitializationId})
        |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
    return ret
  }

  @pure def activateBody(componentName: String, nameProvider: NameProvider): ST = {
    val ep = EntryPoints.activate
    return (
      st"""// implement the following method in '${componentName}':  def ${ep.string}(api: ${nameProvider.apiOperational}): Unit = {}
          |${componentName}.${ep.name}(${ApiTemplate.apiOperationalId})""")
  }

  @pure def deactivateBody(componentName: String, nameProvider: NameProvider): ST = {
    val ep = EntryPoints.deactivate
    return (
      st"""// implement the following method in '${componentName}':  def ${ep.string}(api: ${nameProvider.apiOperational}): Unit = {}
          |${componentName}.${ep.name}(${ApiTemplate.apiOperationalId})""")
  }

  @pure def finaliseBody(componentName: String, nameProvider: NameProvider): ST = {
    val ep = EntryPoints.finalise
    return (
      st"""// implement the following method in '${componentName}':  def ${ep.string}(api: ${nameProvider.apiOperational}): Unit = {}
          |${componentName}.${ep.name}(${ApiTemplate.apiOperationalId})""")
  }

  @pure def recoverBody(componentName: String, nameProvider: NameProvider): ST = {
    val ep = EntryPoints.recover
    return (
      st"""// implement the following method in '${componentName}':  def ${ep.string}(api: ${nameProvider.apiOperational}): Unit = {}
          |${componentName}.${ep.name}(${ApiTemplate.apiOperationalId})""")
  }


  @pure def computeBody(bridgeName: String,
                        componentName: String,
                        names: NameProvider,
                        ports: ISZ[Port],
                        dispatchProtocol: Dispatch_Protocol.Type,
                        isTesting: B,
                        isBless: B): ST = {
    val sendOutputName: String = if (isTesting) "releaseOutput" else "sendOutput"

    if (!isBless) {
      dispatchProtocol match {
        case Dispatch_Protocol.Sporadic =>
          var isFirst = T
          val cases: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isEventPort(p.feature) && CommonUtil.isInFeature(p.feature)).map((m: Port) => {
            val ret = portCase(componentName, m, names.apiOperational, isFirst)
            isFirst = F
            ret
          })
          val ret: ST =
            st"""// transpiler friendly filter
                |def filter(receivedEvents: ISZ[Art.PortId], triggers: ISZ[Art.PortId]): ISZ[Art.PortId] = {
                |  var r = ISZ[Art.PortId]()
                |  val opsTriggers = ops.ISZOps(triggers)
                |  for(e <- receivedEvents) {
                |    if(opsTriggers.contains(e)) {
                |      r = r :+ e
                |    }
                |  }
                |  return r
                |}
                |
                |// fetch received events ordered by highest urgency then earliest arrival-time
                |val EventTriggered(receivedEvents) = Art.dispatchStatus(${bridgeName})
                |
                |// remove non-dispatching event ports
                |val dispatchableEventPorts: ISZ[Art.PortId] =
                |  if(dispatchTriggers.isEmpty) receivedEvents
                |  else filter(receivedEvents, dispatchTriggers.get)
                |
                |Art.receiveInput(eventInPortIds, dataInPortIds)
                |
                |for(portId <- dispatchableEventPorts) {
                |  ${(cases, "\n")}
                |}
                |
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
          return ret
        case Dispatch_Protocol.Periodic =>
          val apiId = ApiTemplate.apiOperationalId
          val ret: ST =
            st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                |
                |// implement the following in 'component':  def timeTriggered(api: ${names.apiOperational}): Unit = {}
                |${componentName}.timeTriggered(${apiId})
                |
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
          return ret
      }
    } else {
      val apiId = ApiTemplate.apiOperationalId
      val ret: ST =
        dispatchProtocol match {
          case Dispatch_Protocol.Sporadic =>
            st"""val EventTriggered(dispatchedPortIds) = Art.dispatchStatus(${bridgeName})
                |Art.receiveInput(dispatchedPortIds, dataInPortIds)
                |${componentName}.compute(${apiId}, dispatchedPortIds)
                |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
          case Dispatch_Protocol.Periodic =>
            st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                |${componentName}.compute(${apiId}, ISZ())
                |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        }

      return ret
    }
  }

  @pure def portCase(cname: String, v: Port, operationalApiType: String, first: B): ST = {
    val apiId = ApiTemplate.apiOperationalId
    val methodName = s"handle_${v.name}"
    val cMethodName = s"${cname}.${methodName}"

    v.feature.category match {
      case FeatureCategory.EventDataPort =>

        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}){
              |  val Some(${v.getPortTypeNames.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
              |
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}, value: ${v.getPortTypeNames.qualifiedReferencedTypeName}): Unit = {}
              |  ${cMethodName}(${apiId}, value)
              |}"""
        return ret
      case FeatureCategory.EventPort =>
        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}) {
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}): Unit = {}
              |  ${cMethodName}(${apiId})
              |}"""
        return ret
      case _ => halt(s"Unexpected ${v.feature.category}")
    }
  }
}
