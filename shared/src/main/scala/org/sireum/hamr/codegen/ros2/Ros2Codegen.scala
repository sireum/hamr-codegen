// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread, SymbolTable}
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
  var threadComponents: ISZ[AadlThread] = ISZ()
  var connectionMap: Map[ISZ[String], ISZ[ISZ[String]]] = Map.empty

  def run(model: Aadl, options: CodeGenConfig, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): Ros2Results = {
    assert(model.components.size == 1)

    val modelName = getModelName(symbolTable)

    mapConnections(symbolTable.rootSystem, symbolTable, reporter)

    var files: ISZ[(ISZ[String], ST)] = IS()

    options.ros2NodesLanguage.name match {
      case "Cpp" => files = Generator.genCppNodePkg(modelName, threadComponents, connectionMap, options.strictAadlMode)
      case "Python" => files = Generator.genPyNodePkg(modelName, threadComponents, connectionMap, options.strictAadlMode)
      case _ => reporter.error(None(), toolName, s"Unknown code type: ${options.ros2NodesLanguage.name}")
    }

    options.ros2LaunchLanguage.name match {
      case "Xml" => files = files ++ Generator.genXmlLaunchPkg(modelName, threadComponents)
      case "Python" => files = files ++ Generator.genPyLaunchPkg(modelName, threadComponents)
      case _ => reporter.error(None(), toolName, s"Unknown code type: ${options.ros2NodesLanguage.name}")
    }

    for (file <- files) {
      var filePath: String = ""
      for (s <- file._1) {
        filePath = s"$filePath$s/"
      }
      filePath = ops.StringOps(filePath).substring(0, filePath.size - 1)

      val absPath: String = options.ros2OutputWorkspaceDir match {
        case Some(p) => s"${p}/$filePath"
        case _ => filePath
      }

      resources = resources :+ ResourceUtil.createResource(
        path = absPath,
        content = file._2,
        overwrite = T
      )
    }

    return Ros2Results(fileResources = resources)
  }

  // Also adds threads to threadComponents
  def mapConnections(c: AadlComponent, symbolTable: SymbolTable, reporter: Reporter): Unit = {
    for(ci <- c.connectionInstances) {
      processConnection(ci, c.component, symbolTable, reporter)
    }

    if (c.isInstanceOf[AadlThread]) {
      threadComponents = threadComponents :+ c.asInstanceOf[AadlThread]
    }

    for (sc <- c.subComponents) {
      mapConnections(sc, symbolTable, reporter)
    }
  }

  // Checks if a connection is allowed, and if so, processes it
  def processConnection(c: ConnectionInstance, srcComponent: Component, symbolTable: SymbolTable, reporter: Reporter): B = {
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

    if (connectionMap.contains(srcName) && ISZOps(connectionMap.get(srcName).get).contains(dstName)) {
      reporter.info(None(), toolName, s"Skipping: already handled connection: ${srcName} to ${dstName}")
      return F
    }

    val seq: ISZ[ISZ[String]] =
      if (!connectionMap.contains(srcName)) ISZ(dstName)
      else connectionMap.get(srcName).get :+ dstName

    connectionMap = connectionMap + (srcName ~> seq)

    return T
  }

  def getModelName(symbolTable: SymbolTable): String = {

    val nname = symbolTable.rootSystem.component.classifier.get.name
    val packageName = conversions.String.toCis(ops.StringOps(ops.StringOps(nname).replaceAllLiterally("::", "^")).split(c => c == '^')(0))

    var cis: ISZ[C] = ISZ[C]() :+ ops.COps(packageName(0)).toLower
    for (i <- 1 until packageName.size) {
      val cand = ops.COps(packageName(i))
      if (cand.toLower != cand.c) {
        if (packageName(i - 1) == '_' ||
          (ops.COps(packageName(i - 1)).toLower != packageName(i - 1))) {
          cis = cis :+ cand.toLower
        } else {
          cis = cis :+ '_' :+ cand.toLower
        }
      } else {
        cis = cis :+ cand.c
      }
    }

    return conversions.String.fromCis(cis)
  }
}
