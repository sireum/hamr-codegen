// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, BaseType, EnumType, RecordType}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.common.util.ResourceUtil
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
  var datatypeMap: Map[AadlType, (String, ISZ[String])] = Map.empty

  def run(model: Aadl, options: CodegenOption, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): Ros2Results = {
    assert(model.components.size == 1)

    val modelName = getModelName(symbolTable)

    mapConnections(symbolTable.rootSystem, symbolTable, reporter)

    mapDatatypes(aadlTypes, reporter)

    var files: ISZ[(ISZ[String], ST)] = IS()

    options.ros2NodesLanguage.name match {
      case "Cpp" => files = Generator.genCppNodePkg(modelName, threadComponents, connectionMap, datatypeMap, options.strictAadlMode, reporter)
      case "Python" => files = Generator.genPyNodePkg(modelName, threadComponents, connectionMap, options.strictAadlMode)
      case _ => reporter.error(None(), toolName, s"Unknown code type: ${options.ros2NodesLanguage.name}")
    }

    options.ros2LaunchLanguage.name match {
      case "Xml" => files = files ++ Generator.genXmlLaunchPkg(modelName, threadComponents)
      case "Python" => files = files ++ Generator.genPyLaunchPkg(modelName, threadComponents)
      case _ => reporter.error(None(), toolName, s"Unknown code type: ${options.ros2NodesLanguage.name}")
    }

    files = files ++ Generator.genInterfacesPkg(modelName, datatypeMap)

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

  def mapDatatypes(aadlTypes: AadlTypes, reporter: Reporter): Unit = {
    for (t <- aadlTypes.typeMap.entries) {
      resolveDatatype(t._2, reporter)
    }
  }

  // Generates datatype name and msg file content
  def resolveDatatype(t: AadlType, reporter: Reporter): B = {
    // Check if t is in keys of datatypemap
    for (dtype <- datatypeMap.keys) {
      if (dtype.name == t.name) {
        return true
      }
    }

    t match {
      case _: BaseType => return aadlToRosBaseType(t, reporter)
      case _: EnumType => return aadlToRosEnumType(t)
      case _: ArrayType => return aadlToRosArrayType(t, reporter)
      case _: RecordType => return aadlToRosRecordType(t, reporter)
      case x =>
        reporter.error(None(), toolName, s"Unknown datatype: ${x}")
        return false
    }
  }

  def aadlToRosBaseType(t: AadlType, reporter: Reporter): B = {
    t.name match {
      case "Base_Types::Boolean" => datatypeMap = datatypeMap + (t ~> ("Boolean", ISZ("bool data")))
      case "Base_Types::Integer" => datatypeMap = datatypeMap + (t ~> ("Integer64", ISZ("int64 data")))
      case "Base_Types::Float" => datatypeMap = datatypeMap + (t ~> ("Float64", ISZ("float64 data")))
      case "Base_Types::Character" => datatypeMap = datatypeMap + (t ~> ("Character", ISZ("char data")))
      case "Base_Types::String" => datatypeMap = datatypeMap + (t ~> ("String", ISZ("string data")))
      case "Base_Types::Integer_8" => datatypeMap = datatypeMap + (t ~> ("Integer8", ISZ("int8 data")))
      case "Base_Types::Integer_16" => datatypeMap = datatypeMap + (t ~> ("Integer16", ISZ("int16 data")))
      case "Base_Types::Integer_32" => datatypeMap = datatypeMap + (t ~> ("Integer32", ISZ("int32 data")))
      case "Base_Types::Integer_64" => datatypeMap = datatypeMap + (t ~> ("Integer64", ISZ("int64 data")))
      case "Base_Types::Unsigned_8" => datatypeMap = datatypeMap + (t ~> ("Unsigned8", ISZ("uint8 data")))
      case "Base_Types::Unsigned_16" => datatypeMap = datatypeMap + (t ~> ("Unsigned16", ISZ("uint16 data")))
      case "Base_Types::Unsigned_32" => datatypeMap = datatypeMap + (t ~> ("Unsigned32", ISZ("uint32 data")))
      case "Base_Types::Unsigned_64" => datatypeMap = datatypeMap + (t ~> ("Unsigned64", ISZ("uint64 data")))
      case "Base_Types::Float_32" => datatypeMap = datatypeMap + (t ~> ("Float32", ISZ("float32 data")))
      case "Base_Types::Float_64" => datatypeMap = datatypeMap + (t ~> ("Float64", ISZ("float64 data")))
      case x =>
        reporter.error(None(), toolName, s"Unknown base type: ${x}")
        return false
    }
    return true
  }

  // Currently, ROS2 does not explicitly support enums.  This code implements a workaround where enum
  // values are assigned to constants, and a uint8 field determines which value is indicated.
  def aadlToRosEnumType(t: AadlType): B = {
    val s: String = formatFieldName(t.name)
    var msg: ISZ[String] = IS(s"uint8 ${s}")

    var i = 0
    for (value <- t.asInstanceOf[EnumType].values)
    {
      msg = msg :+ s"uint8 ${ops.StringOps(s).toUpper}_${ops.StringOps(value).toUpper}=${i}"
      i = i + 1
    }

    datatypeMap = datatypeMap + (t ~> (getDatatypeName(t.name), msg))

    return true
  }

  // Currently, >1D arrays are handled by using a constant int16 array with a series of dimensions to index into
  // a 1D array with a number of elements equal to the product of the dimensions.
  def aadlToRosArrayType(t: AadlType, reporter: Reporter): B = {
    val arr: ArrayType = t.asInstanceOf[ArrayType]
    val s: String = getDatatypeName(t.name)
    var msg: String = ""

    if (!resolveDatatype(arr.baseType, reporter)) {
      reporter.error(None(), toolName, s"Cannot handle array: ${t} due to unhandled type: ${arr.baseType}")
      return false
    }
    val baseType: String = datatypeMap.get(arr.baseType).get._1

    if (arr.dimensions.size == 0) {
      msg = s"${baseType}[] arr"
    } else if (arr.dimensions.size == 1) {
      msg = s"${baseType}[${arr.dimensions.apply(0)}] arr"
    } else {
      var length: Z = 1
      msg = s"int16[${arr.dimensions.size}] DIMENSIONS = ["
      for (dim <- arr.dimensions) {
        length = length * dim
        msg = s"${msg}${dim},"
      }
      msg = ops.StringOps(msg).substring(0, msg.size - 1)
      msg = s"${msg}]\n${baseType}[${length}] arr"
    }

    datatypeMap = datatypeMap + (t ~> (s, ISZ(msg)))

    return true
  }

  def aadlToRosRecordType(t: AadlType, reporter: Reporter): B = {
    val record: RecordType = t.asInstanceOf[RecordType]
    val s: String = getDatatypeName(t.name)
    var msg: ISZ[String] = IS()

    for (subtype <- record.fields.values)
    {
      if (!resolveDatatype(subtype, reporter)) {
        reporter.error(None(), toolName, s"Cannot handle record: ${t} due to unhandled type: ${subtype}")
        return false
      }
    }
    for (key <- record.fields.keys)
    {
      var datatype = record.fields.get(key).get
      for (dtype <- datatypeMap.keys) {
        if (dtype.name == datatype.name) {
          datatype = dtype
        }
      }

      val ros2Datatype = datatypeMap.get(datatype).get._1
      msg = msg :+ s"${ros2Datatype} ${formatFieldName(key)}"
    }

    datatypeMap = datatypeMap + (t ~> (s, msg))

    return true
  }

  def getDatatypeName(name: String): String = {
    var str = ops.StringOps(name).substring(ops.StringOps(name).lastIndexOf(':') + 1, name.size)
    str = ops.StringOps(ops.StringOps(str).replaceAllLiterally(".", "")).replaceAllLiterally("_", "")

    return str
  }

  def formatFieldName(name: String): String = {
    var str = ops.StringOps(name).substring(ops.StringOps(name).lastIndexOf(':') + 1, name.size)
    str = ops.StringOps(str).replaceAllLiterally(".", "_")

    var char: C = 'A'
    while (char <= 'Z') {
      var index = ops.StringOps(str).indexOf(char)
      while (index != -1) {
        str = s"${ops.StringOps(str).substring(0, index)}_${char + '\u0020'}${ops.StringOps(str).substring(index + 1, str.size)}"
        index = ops.StringOps(str).indexOf(char)
      }
      char = char + '\u0001'
    }

    if (ops.StringOps(str).startsWith("_")) {
      str = ops.StringOps(str).substring(1, str.size)
    }

    str = ops.StringOps(str).replaceAllLiterally("__", "_")

    return str
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
