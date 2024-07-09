// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, ResourceUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, Component, ConnectionInstance}
import org.sireum.message.Reporter
import org.sireum.ops.ISZOps

@datatype class Ros2Results(val fileResources: ISZ[FileResource])

@record class Ros2Codegen {

  val toolName: String = "Ros2Codegen"

  var resources: ISZ[FileResource] = ISZ()
  var connections: ISZ[ST] = ISZ()
  var seenConnections: HashMap[ISZ[String], ISZ[ISZ[String]]] = HashMap.empty

  def run(model: Aadl, options: CodeGenConfig, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): Ros2Results = {

    for (t <- aadlTypes.typeMap.entries) {
      resources = resources :+ ResourceUtil.createResourceH(
        path = s"types/${t._2.nameProvider.packageName}/${t._2.nameProvider.typeName}.cpp",
        content = st"// ${t._2.nameProvider.qualifiedTypeName} is a ${t._2.nameProvider.kind}",
        overwrite = T, isDatatype = T)
    }

    assert(model.components.size == 1)

    processComponent(symbolTable.rootSystem, symbolTable, reporter)

    resources = resources :+ ResourceUtil.createResource(
      path = s"arch/connections.cpp",
      content =
        st"""// Connections
            |  ${(connections, "\n")}""",
      overwrite = T
    )

    return Ros2Results(fileResources = resources)
  }

  def processComponent(c: AadlComponent, symbolTable: SymbolTable, reporter: Reporter): Unit = {
    for(ci <- c.connectionInstances if allowConnection(ci, c.component, symbolTable, reporter)) {
      processConnection(ci, symbolTable)
    }

    c match {
      case s: AadlThread => processThread(s)
      case _ =>
    }

    for (sc <- c.subComponents) {
      processComponent(sc, symbolTable, reporter)
    }
  }

  def processThread(s: AadlThread): Unit = {
    var ports: ISZ[ST] = ISZ()
    for (p <- s.getPorts()) {
      p match {
        case a: AadlDataPort =>
          ports = ports :+ st"// ${a.identifier} is a data port with payload ${a.aadlType.nameProvider.qualifiedTypeName}"
        case a: AadlEventPort =>
          ports = ports :+ st"// ${a.identifier} is an event port"
        case a: AadlEventDataPort =>
          ports = ports :+ st"// ${a.identifier} is an event data port with payload ${a.aadlType.nameProvider.qualifiedTypeName}"
      }
    }

    resources = resources :+ ResourceUtil.createResource(
      path = s"components/${s.identifier}.cpp",
      content = st"""// ${s.identifier} is an ${if (s.isSporadic()) "sporadic" else "periodic"} AADL Thread
                    |
                    |// Ports:
                    |  ${(ports, "\n")}""",
      overwrite = T
    )
  }

  def processConnection(ci: ConnectionInstance, symbolTable: SymbolTable): Unit = {
    val srcComponentId = CommonUtil.getName(ci.src.component)
    val srcFeatureId = ci.src.feature.get.name
    val srcComponentFeatureId = symbolTable.featureMap.get(srcFeatureId).get.identifier

    val dstComponentId = CommonUtil.getName(ci.dst.component)
    val dstFeatureId = ci.dst.feature.get.name
    val dstComponentFeatureId = symbolTable.featureMap.get(dstFeatureId).get.identifier

    connections = connections :+ st"""// Connection(from = s"${srcComponentId}.${srcComponentFeatureId}",
                                     |//              to = s"${dstComponentId}.${dstComponentFeatureId}")"""
  }

  def allowConnection(c: ConnectionInstance, srcComponent: Component, symbolTable: SymbolTable, reporter: Reporter): B = {
    val str = s"${CommonUtil.getName(c.name)}  from  ${CommonUtil.getName(srcComponent.identifier)}"

    if (c.src.component == c.dst.component) {
      reporter.info(None(), toolName, s"Skipping: Port connected to itself. $str")
      return F
    }
    if (c.kind != ir.ConnectionKind.Port) {
      reporter.info(None(), toolName, s"Skipping: ${c.kind} connection.  $str")
      return F
    }

    val allowedComponents: ISZ[ir.ComponentCategory.Type] = {
      //if (options.devicesAsThreads) ISZ(ir.ComponentCategory.Device, ir.ComponentCategory.Thread)
      //else ISZ(ir.ComponentCategory.Thread)
      ISZ(ir.ComponentCategory.Thread)
    }

    val catSrc = symbolTable.airComponentMap.get(c.src.component.name).get.category
    val catDest = symbolTable.airComponentMap.get(c.dst.component.name).get.category

    if (!ISZOps(allowedComponents).contains(catSrc) || !ISZOps(allowedComponents).contains(catDest)) {
      reporter.info(None(), toolName, s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return F
    }

    val srcName = c.src.feature.get.name
    val dstName = c.dst.feature.get.name

    if (seenConnections.contains(srcName) && ISZOps(seenConnections.get(srcName).get).contains(dstName)) {
      reporter.info(None(), toolName, s"Skipping: already handled connection: ${srcName} to ${dstName}")
      return F
    }

    val seq: ISZ[ISZ[String]] =
      if (!seenConnections.contains(srcName)) ISZ(dstName)
      else seenConnections.get(srcName).get :+ dstName

    seenConnections = seenConnections + (srcName ~> seq)

    return T
  }

}
