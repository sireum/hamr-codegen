// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{BlockMarker, Marker}
import org.sireum.hamr.codegen.common.properties.Hamr_Ros_Properties
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlPort, AadlProcess, AadlSystem, AadlThread, Dispatch_Protocol}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlType, ArrayType, BaseType, EnumType, RecordType}
import org.sireum.hamr.ir.Direction
import org.sireum.message.{Position, Reporter}
import org.sireum.ops.{ISZOps, StringOps}

object Generator {

  val toolName: String = "Ros2Codegen"

  val node_executable_filename_suffix: String = "_exe"
  val launch_node_decl_suffix: String = "_node"
  val py_launch_file_name_suffix: String = ".launch.py"
  val xml_launch_file_name_suffix: String = ".launch.xml"
  val py_package_name_suffix: String = "_py_pkg"
  val py_src_node_name_suffix: String = "_src.py"
  val py_src_node_entry_point_name: String = "main"
  val cpp_package_name_suffix: String = "_cpp_pkg"
  val cpp_src_node_name_suffix: String = "_src.cpp"
  val cpp_src_node_header_name_suffix: String = "_src.hpp"
  val cpp_node_runner_name_suffix: String = "_runner.cpp"

  val microros_package_name_suffix: String = "_microros_pkg"
  val c_src_node_name_suffix: String = "_src.c"
  val c_src_node_header_name_suffix: String = "_src.h"
  val c_node_runner_name_suffix: String = "_runner.c"

  // This value will work for Python and C++ code
  val callback_group_type: String = "Reentrant"
  val callback_group_name: String = "cb_group_"
  val subscription_options_name: String = "subscription_options_"
  // Mutex is used for thread locking in C++
  val mutex_name: String = "mutex_"


  def genPyLaunchFileName(compNameS: String): String = {
    // create launch file name
    val nodeNameT: String = s"${compNameS}${py_launch_file_name_suffix}"
    return nodeNameT
  }

  def genXmlLaunchFileName(compNameS: String): String = {
    // create launch file name
    val nodeNameT: String = s"${compNameS}${xml_launch_file_name_suffix}"
    return nodeNameT
  }

  def genCppPackageName(packageNameS: String): String = {
    // create target package name
    val packageNameT: String = s"${packageNameS}${cpp_package_name_suffix}"
    return packageNameT
  }

  def genPyPackageName(packageNameS: String): String = {
    // create target package name
    val packageNameT: String = s"${packageNameS}${py_package_name_suffix}"
    return packageNameT
  }

  def genPyNodeSourceName(compNameS: String): String = {
    // create target node name
    val nodeNameT: String = s"${compNameS}${py_src_node_name_suffix}"
    return nodeNameT
  }

  def genCppNodeSourceName(compNameS: String): String = {
    // create node file name
    val nodeNameT: String = s"${compNameS}${cpp_src_node_name_suffix}"
    return nodeNameT
  }

  def genCppNodeSourceHeaderName(compNameS: String): String = {
    // create node header file name
    val nodeNameT: String = s"${compNameS}${cpp_src_node_header_name_suffix}"
    return nodeNameT
  }

  def genCppNodeRunnerName(compNameS: String): String = {
    // create runner file name
    val nodeNameT: String = s"${compNameS}${cpp_node_runner_name_suffix}"
    return nodeNameT
  }

  def genExecutableFileName(componentNameS: String): String = {
    // create target executable name
    val executableFileNameT: String = s"${componentNameS}${node_executable_filename_suffix}"
    return executableFileNameT
  }

  def genNodeName(component: AadlThread): String = {
    var name: ST = st""
    var i: Z = 1
    while (i < component.path.size) {
      name = st"${name}_${component.path.apply(i)}"
      i = i + 1
    }
    return ops.StringOps(name.render).substring(1, name.render.size)
  }

  def genPortName(port: AadlPort): String = {
    var name: ST = st""
    var i: Z = 1
    while (i < port.path.size) {
      name = st"${name}_${port.path.apply(i)}"
      i = i + 1
    }
    return ops.StringOps(name.render).substring(1, name.render.size)
  }

  def isEventPort(portType: String): B = {
    return ops.StringOps(portType).substring(portType.size - 7, portType.size) == "::Empty"
  }

  def isSporadic(component: AadlThread): B = {
    return component.dispatchProtocol == Dispatch_Protocol.Sporadic
  }

  def getPortNames(portNames: ISZ[ISZ[String]]): ISZ[String] = {
    var names: ISZ[String] = IS()
    for (portName <- portNames) {
      var name: ST = st""
      var i: Z = 1
      while (i < portName.size) {
        name = st"${name}_${portName.apply(i)}"
        i = i + 1
      }
      names = names :+ ops.StringOps(name.render).substring(1, name.render.size)
    }
    return names
  }

  // Returns the C++ type a port's messages are carried in.  For a generated type this is the
  // type in the model's interfaces package; for a platform-provided type it is the native ROS
  // type itself (e.g. "sensor_msgs::msg::Joy"), which is what makes generated nodes able to
  // exchange messages with preexisting ROS nodes.
  def genPortDatatype(port: AadlPort, packageName: String, datatypeMap: Map[AadlType, Ros2Datatype], reporter: Reporter): String = {
    val interfacesPackageName: String = s"${packageName}_interfaces"

    def resolve(aadlType: AadlType): String = {
      datatypeMap.get(aadlType) match {
        case Some(dtype) => return dtype.cppType(interfacesPackageName)
        case _ =>
          reporter.error(None(), toolName, s"Port ${port.identifier}: datatype unknown, setting datatype to Empty")
          return s"${interfacesPackageName}::msg::Empty"
      }
    }

    val s: String = port match {
      case dp: AadlDataPort => resolve(dp.aadlType)
      case edp: AadlEventDataPort => resolve(edp.aadlType)
      case _ => s"${interfacesPackageName}::msg::Empty"
    }
    return s
  }

  // The ports codegen emits data-plane code for.  Infrastructure-realized ports (an out `rosout`,
  // whose publisher is created and driven by rcl logging rather than by application code) are
  // excluded: no publisher, no put_ API, no queue, and no executor handle is generated for them.
  @strictpure def generatedPorts(component: AadlThread): ISZ[AadlPort] =
    ISZOps(component.getPorts()).filter(p => !RosUtil.isInfrastructureRealized(p))

  // Arguments to the rclcpp Node base constructor.  The two-argument (name, namespace) form is
  // used only when the model assigns a namespace, so unnamespaced nodes keep the plain form.
  def genCppNodeCtorArgs(component: AadlThread): ST = {
    val nodeName = genNodeName(component)
    RosUtil.getRosNamespace(component) match {
      case string"" => return st""""${nodeName}""""
      case ns => return st""""${nodeName}", "${ns}""""
    }
  }

  // Topic bindings for every port, resolved once per run: port path -> the topics that port's
  // publishers/subscriptions bind to (one per edge for a fan-out out port, otherwise one).
  // Populated by resolveTopicBindings; a port absent from the map keeps the path-derived default.
  var topicBindings: Map[ISZ[String], ISZ[String]] = Map.empty

  // The topics an in port subscribes to.  The platform pins the reserved `rosout` port to
  // /rosout; otherwise a resolved binding wins over the path-derived default.
  def subscriptionTopicNames(inPort: AadlPort, derived: ISZ[String]): ISZ[String] = {
    if (RosUtil.isRosoutPort(inPort)) {
      return ISZ(RosUtil.ROSOUT_TOPIC)
    }
    topicBindings.get(inPort.path.toISZ) match {
      case Some(topics) if topics.nonEmpty => return topics
      case _ => return derived
    }
  }

  // The topics an out port publishes to, one per edge (or one when explicitly named/unconnected).
  def publisherTopicNames(outPort: AadlPort, derived: ISZ[String]): ISZ[String] = {
    topicBindings.get(outPort.path.toISZ) match {
      case Some(topics) if topics.nonEmpty => return topics
      case _ => return derived
    }
  }

  // Checks the communication graph that topic *names* induce, which is what ROS actually wires up
  // -- ROS has no connections, its graph emerges from (topic, type) matching.  Where that graph
  // contradicts itself or the modeled one, say so at generation time rather than leaving it to
  // surface at runtime as endpoints that silently never talk.
  //
  // Ports are grouped by the fully-qualified topic they bind to, since a relative "joy" under
  // namespace uros_demo and an absolute "/uros_demo/joy" name the same topic.
  def validateTopicConsistency(threads: ISZ[AadlThread],
                               connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               invertTopicBinding: B,
                               datatypeMap: Map[AadlType, Ros2Datatype],
                               reporter: Reporter): Unit = {
    var portOf: Map[ISZ[String], AadlPort] = Map.empty
    var namespaceOf: Map[ISZ[String], String] = Map.empty
    for (thread <- threads;
         port <- generatedPorts(thread)) {
      portOf = portOf + (port.path.toISZ ~> port)
      namespaceOf = namespaceOf + (port.path.toISZ ~> RosUtil.getRosNamespace(thread))
    }

    // topic -> the ports bound to it
    var groups: Map[String, ISZ[ISZ[String]]] = Map.empty
    for (entry <- portOf.entries) {
      val portPath = entry._1
      val derived: ISZ[String] = ISZ(getPortNames(ISZ(portPath))(0))
      for (binding <- subscriptionTopicNames(entry._2, derived)) {
        val topic = RosUtil.absolutizeTopicName(binding, namespaceOf.get(portPath).get)
        val members: ISZ[ISZ[String]] = groups.get(topic) match {
          case Some(ms) => ms
          case _ => ISZ()
        }
        if (!ISZOps(members).contains(portPath)) {
          groups = groups + (topic ~> (members :+ portPath))
        }
      }
    }

    var modeled: Set[(ISZ[String], ISZ[String])] = Set.empty
    for (edge <- normalizedEdges(connectionMap, invertTopicBinding)) {
      modeled = modeled + edge
    }

    for (entry <- groups.entries) {
      val topic = entry._1
      val members = entry._2
      val ports: ISZ[AadlPort] = for (m <- members) yield portOf.get(m).get

      // 1. type disagreement -- ROS surfaces this only at runtime as incompatible endpoints
      var wireTypes: ISZ[String] = ISZ()
      for (p <- ports) {
        val wt = wireTypeOf(p, datatypeMap)
        if (!ISZOps(wireTypes).contains(wt)) {
          wireTypes = wireTypes :+ wt
        }
      }
      if (wireTypes.size > 1) {
        reporter.error(ports(0).posOpt, RosUtil.toolName,
          st"""Ports bound to topic '${topic}' disagree on message type: ${(wireTypes, ", ")}.
              |ROS matches endpoints by (topic, type), so these would never communicate.""".render)
      }

      val writers: ISZ[ISZ[String]] = ISZOps(members).filter(m => portOf.get(m).get.direction == Direction.Out)

      // 2. data-port fan-in -- several writers over a single logical value is nondeterministic
      // last-write-wins.  Event and event-data topics are multi-writer by design, so they are fine.
      if (writers.size > 1 && ISZOps(ports).exists(p => p.isInstanceOf[AadlDataPort])) {
        reporter.error(ports(0).posOpt, RosUtil.toolName,
          st"""Topic '${topic}' has ${writers.size} writers with data port semantics:
              |${(for (w <- writers) yield st"  ${(w, ".")}", "\n")}
              |A data port holds one logical value, so multiple writers make its value
              |nondeterministic (last write wins).""".render)
      }

      // 3. informational: the effective graph exceeding the intended one is legitimate (that is
      // how generated nodes join stock ones), so report the derived edges rather than flagging
      for (w <- writers;
           r <- members if portOf.get(r).get.direction == Direction.In && !modeled.contains((w, r))) {
        reporter.info(portOf.get(w).get.posOpt, RosUtil.toolName,
          st"""Topic '${topic}' induces a connection not declared in the model:
              |${(w, ".")} -> ${(r, ".")}.  Declaring it would let the static checks and
              |contract composition see this edge.""".render)
      }
    }
  }

  // connectionMap is keyed by the anchor end, which invertTopicBinding flips; normalize to
  // (out port path, in port path) pairs so callers do not have to care which way it is keyed
  def normalizedEdges(connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                      invertTopicBinding: B): ISZ[(ISZ[String], ISZ[String])] = {
    var edges: ISZ[(ISZ[String], ISZ[String])] = ISZ()
    for (entry <- connectionMap.entries;
         peer <- entry._2) {
      edges = edges :+ (if (invertTopicBinding) (peer, entry._1) else (entry._1, peer))
    }
    return edges
  }

  // The C type rosidl generates for a sequence of this element type, or None() when codegen has
  // no name for it (a nested message or an enum, which needs its own struct rather than a scalar
  // backing array).
  def cSequenceElementType(baseType: AadlType): Option[String] = {
    baseType.name match {
      case "Base_Types::Boolean" => return Some("bool")
      case "Base_Types::Character" => return Some("signed char")
      case "Base_Types::Integer_8" => return Some("int8_t")
      case "Base_Types::Integer_16" => return Some("int16_t")
      case "Base_Types::Integer_32" => return Some("int32_t")
      case "Base_Types::Integer_64" => return Some("int64_t")
      case "Base_Types::Integer" => return Some("int64_t")
      case "Base_Types::Unsigned_8" => return Some("uint8_t")
      case "Base_Types::Unsigned_16" => return Some("uint16_t")
      case "Base_Types::Unsigned_32" => return Some("uint32_t")
      case "Base_Types::Unsigned_64" => return Some("uint64_t")
      case "Base_Types::Float_32" => return Some("float")
      case "Base_Types::Float" => return Some("double")
      case "Base_Types::Float_64" => return Some("double")
      case _ => return None()
    }
  }

  // The mirror's bounded array fields of a message type, as (field name, C element type,
  // capacity).  These are the numbers micro-ROS needs: the mirror's size projection ("the model
  // assumes at most 8 axes") and the receive buffer's capacity must be the same value, or
  // contracts touching the tail elements are vacuous.
  //
  // Only top-level fields are walked.  Nested messages would need their own storage, and an
  // opaque mirror has no fields to derive anything from -- both are reported by
  // validateMicroRosCapacities rather than guessed at.
  def mirrorSequenceFields(aadlType: AadlType): ISZ[(String, String, Z)] = {
    var fields: ISZ[(String, String, Z)] = ISZ()
    aadlType match {
      case r: RecordType =>
        for (entry <- r.fields.entries) {
          entry._2 match {
            case a: ArrayType if a.dimensions.size == 1 && a.dimensions(0) > 0 =>
              cSequenceElementType(a.baseType) match {
                case Some(cType) => fields = fields :+ ((entry._1, cType, a.dimensions(0)))
                case _ =>
              }
            case _ =>
          }
        }
      case _ =>
    }
    return fields
  }

  // The type a port's messages appear as on the wire.  Comparing these rather than AadlTypes is
  // what matters for topic compatibility: distinct model types can map onto the same ROS message.
  def wireTypeOf(port: AadlPort, datatypeMap: Map[AadlType, Ros2Datatype]): String = {
    portAadlTypeOpt(port) match {
      case Some(aadlType) =>
        datatypeMap.get(aadlType) match {
          case Some(dtype) =>
            dtype.nativePackageOpt match {
              case Some(nativePackage) => return s"${nativePackage}/msg/${dtype.name}"
              case _ => return dtype.name
            }
          case _ => return aadlType.name
        }
      // event ports carry no payload; they all ride the generated Empty message
      case _ => return "<event>"
    }
  }

  // Resolves every port's topic bindings from the model, replacing the path-derived default
  // wherever Ros_Topic_Name is modeled.
  //
  // A topic is a property of the *edge*, not of one port: an explicit name on either endpoint
  // governs both ends, so a connected peer that declares nothing still follows its partner rather
  // than falling back to its own path-derived name (which would silently sever the edge).  The
  // declaring end keeps its literal string -- rcl expands a relative name against that node's own
  // namespace -- while the peer is bound to the resolved absolute form, since the same relative
  // string under the peer's namespace would name a different topic.
  def resolveTopicBindings(threads: ISZ[AadlThread],
                           connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                           invertTopicBinding: B,
                           reporter: Reporter): Map[ISZ[String], ISZ[String]] = {
    var portOf: Map[ISZ[String], AadlPort] = Map.empty
    var namespaceOf: Map[ISZ[String], String] = Map.empty
    for (thread <- threads;
         port <- thread.getPorts()) {
      portOf = portOf + (port.path.toISZ ~> port)
      namespaceOf = namespaceOf + (port.path.toISZ ~> RosUtil.getRosNamespace(thread))
    }

    val edges = normalizedEdges(connectionMap, invertTopicBinding)

    var bindings: Map[ISZ[String], ISZ[String]] = Map.empty

    def bind(portPath: ISZ[String], topic: String): Unit = {
      val existing: ISZ[String] = bindings.get(portPath) match {
        case Some(ts) => ts
        case _ => ISZ()
      }
      if (!ISZOps(existing).contains(topic)) {
        bindings = bindings + (portPath ~> (existing :+ topic))
      }
    }

    var connected: Set[ISZ[String]] = Set.empty

    for (edge <- edges) {
      val (outPath, inPath) = edge
      connected = connected + outPath + inPath

      val explicitOut: Option[String] = portOf.get(outPath) match {
        case Some(p) => RosUtil.getExplicitTopicName(p)
        case _ => None()
      }
      val explicitIn: Option[String] = portOf.get(inPath) match {
        case Some(p) => RosUtil.getExplicitTopicName(p)
        case _ => None()
      }

      (explicitOut, explicitIn) match {
        case (Some(outName), Some(inName)) =>
          // the model asserts these ports communicate and simultaneously configures them not to
          if (RosUtil.absolutizeTopicName(outName, namespaceOf.get(outPath).get) !=
              RosUtil.absolutizeTopicName(inName, namespaceOf.get(inPath).get)) {
            val posOpt: Option[Position] = portOf.get(outPath) match {
              case Some(p) => p.posOpt
              case _ => None()
            }
            reporter.error(posOpt, RosUtil.toolName,
              st"""Connected ports declare different ${Hamr_Ros_Properties.HAMR_ROS__Ros_Topic_Name} values,
                  |so the model both asserts they communicate and configures them not to:
                  |  ${(outPath, ".")} publishes to '${outName}'
                  |  ${(inPath, ".")} subscribes to '${inName}'""".render)
          }
          bind(outPath, outName)
          bind(inPath, inName)
        case (Some(outName), _) =>
          bind(outPath, outName)
          bind(inPath, RosUtil.absolutizeTopicName(outName, namespaceOf.get(outPath).get))
        case (_, Some(inName)) =>
          bind(inPath, inName)
          bind(outPath, RosUtil.absolutizeTopicName(inName, namespaceOf.get(inPath).get))
        case _ =>
          // no explicit name anywhere on the edge: today's path-derived default, which already
          // unifies the edge by anchoring on one end
          val defaultTopic: String =
            if (invertTopicBinding) getPortNames(ISZ(outPath))(0) else getPortNames(ISZ(inPath))(0)
          bind(outPath, defaultTopic)
          bind(inPath, defaultTopic)
      }
    }

    // unconnected ports still get a topic -- that is what lets non-generated components join
    for (thread <- threads;
         port <- thread.getPorts() if !connected.contains(port.path.toISZ)) {
      RosUtil.getExplicitTopicName(port) match {
        case Some(name) => bind(port.path.toISZ, name)
        case _ =>
      }
    }

    return bindings
  }

  // The payload type carried by the given port, or None() for ports that carry no data
  @strictpure def portAadlTypeOpt(port: AadlPort): Option[AadlType] =
    port match {
      case dp: AadlDataPort => Some(dp.aadlType)
      case edp: AadlEventDataPort => Some(edp.aadlType)
      case _ => None()
    }

  // True when the port's payload is a platform-provided type.  Codegen generates no
  // MESSAGE_TO_STRING printer for such a payload (the model's mirror fields are projections
  // rather than the native type's layout), so generated example code must not try to render it.
  @strictpure def isPlatformProvidedPayload(port: AadlPort): B =
    portAadlTypeOpt(port) match {
      case Some(aadlType) => RosUtil.isPlatformProvided(aadlType)
      case _ => F
    }

  // The example "Received <port>" log line emitted into generated user code.  msgExpr is the
  // C++ expression holding the message; a platform-provided payload has no printer, so only
  // the port name is logged.
  @strictpure def genCppReceivedLog(port: AadlPort, msgExpr: String): ST =
    if (isPlatformProvidedPayload(port)) st"""PRINT_INFO("Received ${port.identifier}");"""
    else st"""PRINT_INFO("Received ${port.identifier}: %s", MESSAGE_TO_STRING(${msgExpr}));"""

  // The example "Sent <port>" log line emitted into generated micro-ROS user code; as with
  // genCppReceivedLog, a platform-provided payload is logged by port name only.
  @strictpure def genCSentLog(port: AadlPort): ST =
    if (isPlatformProvidedPayload(port)) st"""PRINT_INFO("Sent ${port.identifier}");"""
    else st"""PRINT_INFO("Sent ${port.identifier}: %s", MESSAGE_TO_STRING(&${port.identifier}));"""

  // The native ROS packages that the given threads' ports depend on -- i.e. the packages
  // supplying their platform-provided payload types.  These become build/runtime dependencies
  // of the generated node packages, so the walk follows generated code: an infrastructure-
  // realized port contributes nothing, since no generated code names its payload type.
  def getNativePackages(threadComponents: ISZ[AadlThread], datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[String] = {
    var packages: ISZ[String] = IS()
    for (comp <- threadComponents;
         port <- generatedPorts(comp)) {
      portAadlTypeOpt(port) match {
        case Some(aadlType) =>
          datatypeMap.get(aadlType) match {
            case Some(dtype) =>
              dtype.nativePackageOpt match {
                case Some(nativePackage) =>
                  if (!ISZOps(packages).contains(nativePackage)) {
                    packages = packages :+ nativePackage
                  }
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
    }
    return packages
  }

  def formatDatatypeForInclude(datatype: String): String = {
    var prefix = ops.StringOps(datatype).substring(0, ops.StringOps(datatype).lastIndexOf(':') + 1)
    prefix = ops.StringOps(prefix).replaceAllLiterally("::", "/")
    var msg = ops.StringOps(datatype).substring(ops.StringOps(datatype).lastIndexOf(':') + 1, datatype.size)

    var char: C = 'A'
    while (char <= 'Z') {
      var index = ops.StringOps(msg).indexOf(char)
      while (index != -1) {
        msg = s"${ops.StringOps(msg).substring(0, index)}_${char + '\u0020'}${ops.StringOps(msg).substring(index + 1, msg.size)}"
        index = ops.StringOps(msg).indexOf(char)
      }
      char = char + '\u0001'
    }

    if (ops.StringOps(msg).startsWith("_")) {
      msg = ops.StringOps(msg).substring(1, msg.size)
    }

    msg = ops.StringOps(msg).replaceAllLiterally("__", "_")

    return s"${prefix}${msg}"
  }

  def seqToString(seq: ISZ[String], separator: String): String = {
    var str = ""
    for (s <- seq) {
      str = s"$str$s$separator"
    }
    str = ops.StringOps(str).substring(0, str.size - 1)
    return str
  }

  //================================================
  //  Setup file for node source package (Python)
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_py_pkg/setup.py
  //================================================

  // genPySetupEntryPointDecl - generates entry point declaration
  //   (console scripts entry) in setup file

  //  Example:
  //   "ts_exe = tc_py_pkg.ts_src:main"
  def genPySetupEntryPointDecl(modelName: String,
                               componentName: String): ST = {
    val node_source_file_nameT = genPyNodeSourceName(componentName)
    val py_package_nameT = genPyPackageName(modelName)
    val node_executable_file_nameT = genExecutableFileName(componentName)
    val entryPointDecl:ST
      = st"\"${node_executable_file_nameT} = ${py_package_nameT}.${node_source_file_nameT}:${py_src_node_entry_point_name}\""
    return entryPointDecl
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_py_pkg/setup.py
  def genPySetupFile(modelName: String, threadComponents: ISZ[AadlThread]): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "setup.py"

    // build entry point declarations
    var entry_point_decls: ISZ[ST] = IS()
    for (comp <- threadComponents) {
      val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(genNodeName(comp))
      entry_point_decls =
        entry_point_decls :+ genPySetupEntryPointDecl(modelName, genNodeName(comp))
    }

    val setupFileBody =
      st"""# ${fileName}   in  src/${top_level_package_nameT}
          |
          |from setuptools import find_packages, setup
          |
          |${CommentTemplate.doNotEditComment_hash}
          |
          |package_name = '${top_level_package_nameT}'
          |
          |setup(
          |    name=package_name,
          |    version='0.0.0',
          |    packages=find_packages(exclude=['test']),
          |    data_files=[
          |        ('share/ament_index/resource_index/packages',
          |            ['resource/' + package_name]),
          |        ('share/' + package_name, ['package.xml']),
          |    ],
          |    install_requires=['setuptools'],
          |    zip_safe=True,
          |    maintainer='sireum',
          |    maintainer_email='sireum@todo.todo',
          |    description='TODO: Package description',
          |    license='TODO: License declaration',
          |    tests_require=['pytest'],
          |    entry_points={
          |        'console_scripts': [
          |            ${(entry_point_decls, ",\n")}
          |        ],
          |    },
          |)
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody)
  }


  //================================================
  //  Setup files for node source package (C++)
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/CMakeLists.txt
  //             https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/package.xml
  //================================================

  def genCppCMakeListsEntryPointDecl(modelName: String, componentName: String, hasEnumConverter: B,
                                     nativePackages: ISZ[String]): ST = {
    val node_executable_file_nameT = genExecutableFileName(componentName)

    var source_files: ISZ[String] = IS()
    source_files = source_files :+ s"src/base_code/${componentName}_runner.cpp"
    source_files = source_files :+ s"src/user_code/${componentName}_src.cpp"
    source_files = source_files :+ s"src/base_code/${componentName}_base_src.cpp"

    if (hasEnumConverter) {
      source_files = source_files :+s"src/base_code/enum_converter.cpp"
    }

    val packages: ISZ[String] = ISZ[String](s"${genCppPackageName(modelName)}_interfaces") ++ nativePackages

    val entryPointDecl: ST =
      st"""add_executable(${node_executable_file_nameT} ${(source_files, " ")})
          |ament_target_dependencies(${node_executable_file_nameT} rclcpp ${(packages, " ")})"""
    return entryPointDecl
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/CMakeLists.txt
  def genCppCMakeListsFile(modelName: String, threadComponents: ISZ[AadlThread], hasEnumConverter: B,
                           nativePackages: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"

    // build entry point declarations
    var entry_point_decls: ISZ[ST] = IS()
    var entry_point_executables: ISZ[String] = IS()
    for (comp <- threadComponents) {
      entry_point_decls =
        entry_point_decls :+ genCppCMakeListsEntryPointDecl(modelName, genNodeName(comp), hasEnumConverter, nativePackages)
      entry_point_executables =
        entry_point_executables :+ genExecutableFileName(genNodeName(comp))
    }

    val packages: ISZ[String] = ISZ[String](s"${top_level_package_nameT}_interfaces") ++ nativePackages
    val pkgRequirements: ISZ[ST] = genCMakeListsPkgRequirements(packages)

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "#",
      optBeginSuffix = None(),
      endPrefix = "#",
      optEndSuffix = None())

    val setupFileBody =
      st"""cmake_minimum_required(VERSION 3.8)
          |project(${top_level_package_nameT})
          |
          |${CommentTemplate.invertedMarkerComment_hash}
          |
          |if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
          |    add_compile_options(-Wall -Wextra -Wpedantic)
          |endif()
          |
          |find_package(ament_cmake REQUIRED)
          |find_package(rclcpp REQUIRED)
          |${(pkgRequirements, "\n")}
          |
          |${marker.beginMarker}
          |
          |${marker.endMarker}
          |
          |include_directories(include)
          |
          |${(entry_point_decls, "\n\n")}
          |
          |install(TARGETS
          |    ${(entry_point_executables, "\n")}
          |    DESTINATION lib/$${PROJECT_NAME}
          |)
          |
          |ament_package()
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody, T, IS(marker))
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/package.xml
  def genCppPackageFile(modelName: String, nativePackages: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "<!--",
      optBeginSuffix = Some("-->"),
      endPrefix = "<!--",
      optEndSuffix = Some("-->")
    )

    val packages: ISZ[String] = ISZ[String](s"${top_level_package_nameT}_interfaces") ++ nativePackages
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
          |
          |${CommentTemplate.invertedMarkerComment_xml}
          |
          |<package format="3">
          |    <name>${top_level_package_nameT}</name>
          |    <version>0.0.0</version>
          |    <description>TODO: Package description</description>
          |    <maintainer email="sireum@todo.todo">sireum</maintainer>
          |    <license>TODO: License declaration</license>
          |
          |    <buildtool_depend>ament_cmake</buildtool_depend>
          |
          |    <depend>rclcpp</depend>
          |    ${(pkgDependencies, "\n")}
          |
          |    ${marker.beginMarker}
          |
          |    ${marker.endMarker}
          |
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody, T, IS(marker))
  }


  //================================================
  //  L a u n c h  File Setup Files
  //================================================

  def genLaunchCMakeListsFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"

    val setupFileBody =
      st"""cmake_minimum_required(VERSION 3.8)
          |project(${top_level_package_nameT}_bringup)
          |
          |${CommentTemplate.doNotEditComment_hash}
          |
          |if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
          |    add_compile_options(-Wall -Wextra -Wpedantic)
          |endif()
          |
          |find_package(ament_cmake REQUIRED)
          |
          |install(DIRECTORY
          |    launch
          |    DESTINATION share/$${PROJECT_NAME}
          |)
          |
          |ament_package()
        """

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", fileName)

    return (filePath, setupFileBody, T, IS())
  }

  // execDepends are the ROS packages supplying platform-provided components, so that
  // `rosdep install` pulls the stock nodes the launch file starts -- the component-level analog
  // of the interfaces-package dependency for platform-provided types.
  def genLaunchPackageFile(modelName: String, execDepends: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"
    val nativeExecDepends: ST =
      if (execDepends.isEmpty) st""
      else st"""
               |${(for (d <- execDepends) yield st"<exec_depend>${d}</exec_depend>", "\n")}"""

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "<!--",
      optBeginSuffix = Some("-->"),
      endPrefix = "<!--",
      optEndSuffix = Some("-->")
    )
    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
          |
          |${CommentTemplate.invertedMarkerComment_xml}
          |
          |<package format="3">
          |    <name>${top_level_package_nameT}_bringup</name>
          |    <version>0.0.0</version>
          |    <description>TODO: Package description</description>
          |    <maintainer email="sireum@todo.todo">sireum</maintainer>
          |    <license>TODO: License declaration</license>
          |
          |    <buildtool_depend>ament_cmake</buildtool_depend>
          |
          |    <exec_depend>${top_level_package_nameT}</exec_depend>${nativeExecDepends}
          |
          |    ${marker.beginMarker}
          |
          |    ${marker.endMarker}
          |
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", fileName)

    return (filePath, setupFileBody, T, IS(marker))
  }


  //================================================
  //  L a u n c h  File (Python Format)
  //================================================

  // Example:
  //     tc_node = Node(           ## Example is "tc_node" python variable name
  //        package="tc_cpp_pkg",
  //        executable="tc_exe"
  //        )
  def genPyFormatLaunchNodeDeclName(componentNameS: String): String = {
    // create target launch node decl name
    val launch_node_decl_nameT: String = s"${componentNameS}${launch_node_decl_suffix}"
    return launch_node_decl_nameT
  }

  // genLaunchNodeDecl() - generate node declaration
  //   Example:
  //     tc_node = Node(
  //        package="tc_cpp_pkg",
  //        executable="tc_exe"
  //        )
  def genPyFormatLaunchNodeDecl(launch_node_decl_nameT: String,
                                top_level_package_nameT: String,
                                component: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(genNodeName(component))
    var args: ISZ[ST] = ISZ(
      st"""package = "${top_level_package_nameT}"""",
      st"""executable = "${node_executable_file_nameT}"""")
    RosUtil.getRosNamespace(component) match {
      case string"" =>
      case ns => args = args :+ st"""namespace = "${ns}""""
    }
    val s =
      st"""
          |${launch_node_decl_nameT} = Node(
          |   ${(args, ",\n")}
          |   )
        """
    return s
  }

  // Example:
  //    ld.add_action(tc_node)
  def genPyFormatLaunchAddAction(launch_node_decl_nameT: String): ST = {
    val s = st"""ld.add_action(${launch_node_decl_nameT})"""
    return s
  }

  // For example, see https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_bringup/launch/tc.launch.py
  def genPyFormatLaunchFile(modelName: String, threadComponents: ISZ[AadlThread]): (ISZ[String], ST) = {
    val fileName = genPyLaunchFileName(modelName)

    val top_level_package_nameT: String = genPyPackageName(modelName)

    var node_decls: ISZ[ST] = IS()
    var ld_entries: ISZ[ST] = IS()

    for (comp <- threadComponents) {
      val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(genNodeName(comp))
      node_decls = node_decls :+ genPyFormatLaunchNodeDecl(launch_node_decl_nameT, top_level_package_nameT, comp)
      ld_entries = ld_entries :+ genPyFormatLaunchAddAction(launch_node_decl_nameT)
    }

    val launchFileBody =
      st"""from launch import LaunchDescription
          |from launch_ros.actions import Node
          |
          |${CommentTemplate.doNotEditComment_hash}
          |
          |def generate_launch_description():
          |    ld = LaunchDescription()
          |
          |    ${(node_decls, "\n")}
          |    ${(ld_entries, "\n")}
          |
          |    return ld
        """

    val filePath: ISZ[String] = IS("src", s"${modelName}_bringup", "launch", fileName)

    return (filePath, launchFileBody)
  }

  //================================================
  //  L a u n c h  File (XML Format)
  //================================================

  // Every node entry carries a preserved block for <param>, <remap> and the like.  A parameter
  // has to be a child of its <node>, so there is nowhere else it could go, and codegen cannot
  // know which nodes a project will parameterize -- so every node gets one.
  //
  // Deliberately not modeled: simple constant parameters could have been properties, but
  // launch-time dynamics (<arg> plus $(var ...) substitution) are launch-language features that
  // should not be shadowed in the model.  This is the node_options philosophy applied to the
  // launch layer.
  // "--" is illegal inside an XML comment, and a marker id is rendered into one at both ends.
  // Collapsing it here means a later edit to the prose cannot silently produce a launch file
  // that no XML parser will accept -- the failure would otherwise surface only at `ros2 launch`.
  @strictpure def xmlCommentSafe(text: String): String =
    ops.StringOps(text).replaceAllLiterally("--", "-")

  def launchNodeConfigMarker(thread: AadlThread): BlockMarker = {
    return BlockMarker(
      id = xmlCommentSafe(s"LAUNCH CONFIG ${genNodeName(thread)} -- additions within these tags will be preserved when re-running Codegen"),
      beginPrefix = "<!--",
      optBeginSuffix = Some("-->"),
      endPrefix = "<!--",
      optEndSuffix = Some("-->"))
  }

  // File-level block for <arg> declarations.  A <param> can reference $(var name) but cannot
  // introduce the argument, so parameterizing a launch file needs somewhere above the nodes too.
  val launchArgsMarker: BlockMarker = BlockMarker(
    id = xmlCommentSafe("LAUNCH ARGUMENTS - additions within these tags will be preserved when re-running Codegen"),
    beginPrefix = "<!--",
    optBeginSuffix = Some("-->"),
    endPrefix = "<!--",
    optEndSuffix = Some("-->"))

  def genXmlLaunchArgsBlock(): ST = {
    return (
      st"""${launchArgsMarker.beginMarker}
          |<!-- Declare launch arguments here, then reference them from a node's parameters, e.g.
          |         <arg name="log_file" default="uros-demo.txt"/>
          |     with, inside that node's config block:
          |         <param name="log_file_name" value="$$(var log_file)"/> -->
          |${launchArgsMarker.endMarker}""")
  }

  // Generate node launch code
  //   Example:
  //     <node pkg="tc_cpp_pkg" exec="tc_test_exe"></node>
  def genXmlFormatLaunchNodeDecl(top_level_package_nameT: String,
                                 thread: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(genNodeName(thread))
    val marker = launchNodeConfigMarker(thread)
    val s =
      st"""
          |<node pkg="${top_level_package_nameT}" exec="${node_executable_file_nameT}"${genXmlLaunchNamespaceAttr(thread)}>
          |    ${marker.beginMarker}
          |    ${marker.endMarker}
          |</node>
        """
    return s
  }

  // A micro-ROS executable is built against rmw_microxrcedds and has to run under it.  Without
  // the pin the node loads the default RMW, whose C typesupport manages sequences dynamically:
  // it calls free() on the statically allocated receive buffers, so the process aborts with
  // "free(): invalid size" on the first message it receives -- with no diagnostic naming the
  // middleware as the cause.  `ros2 run` sets nothing, so the launch entry must.
  def genXmlFormatMicroRosLaunchNodeDecl(microrosPkgName: String, thread: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(genNodeName(thread))
    val marker = launchNodeConfigMarker(thread)
    return (
      st"""
          |<node pkg="${microrosPkgName}" exec="${node_executable_file_nameT}"${genXmlLaunchNamespaceAttr(thread)}>
          |    <env name="RMW_IMPLEMENTATION" value="rmw_microxrcedds"/>
          |    ${marker.beginMarker}
          |    ${marker.endMarker}
          |</node>
        """)
  }

  @strictpure def genXmlLaunchNamespaceAttr(thread: AadlThread): String =
    RosUtil.getRosNamespace(thread) match {
      case string"" => ""
      case ns => s" namespace=\"${ns}\""
    }

  // The launch entry for a platform-provided component.  Nothing else is generated for it, so
  // every field is derived from the model: package/executable from the classifier (or
  // Native_Name), node name from the subcomponent usage name, namespace from Ros_Namespace.
  //
  // The body carries a preserved block for parameters, remappings and launch arguments.  Simple
  // constant parameters could have been model properties, but launch-time dynamics
  // (DeclareLaunchArgument and substitutions) are launch-language features that should not be
  // shadowed in the model -- the node_options philosophy applied to the launch layer.
  def genXmlFormatPlatformProvidedNodeDecl(thread: AadlThread, reporter: Reporter): ST = {
    val nodeName = thread.identifier
    RosUtil.getNativeExecutable(thread, reporter) match {
      case Some((nativePackage, nativeExecutable)) =>
        val marker = launchNodeConfigMarker(thread)
        return (
          st"""
              |<!-- realized by `ros2 run ${nativePackage} ${nativeExecutable}` - no code is generated for it -->
              |<node pkg="${nativePackage}" exec="${nativeExecutable}" name="${nodeName}"${genXmlLaunchNamespaceAttr(thread)}>
              |    ${marker.beginMarker}
              |    ${marker.endMarker}
              |</node>
            """)
      case _ => return st""
    }
  }

  // Generate system launch code (including a system launch file)
  //   Example:
  //     <include file="$(find-pkg-share gazebo_ros)/launch/gazebo.launch.py"/>
  def genXmlFormatLaunchSystemDecl(top_level_package_nameT: String,
                                   system: AadlSystem): ST = {
    val launchFileName: String = genXmlLaunchFileName(system.identifier)
    val s =
      st"""
          |<include file="$$(find-pkg-share ${top_level_package_nameT}_bringup)/launch/${launchFileName}"/>
        """
    return s
  }

  def genXmlFormatLaunchDecls(component: AadlComponent, ros2PkgName: String,
                             microrosPkgName: String, microRosThreadPaths: Set[ISZ[String]],
                             reporter: Reporter): (ISZ[ST], ISZ[ST], ISZ[Marker], ISZ[Marker]) = {
    var ros2Decls: ISZ[ST] = IS()
    var microRosDecls: ISZ[ST] = IS()
    var ros2Markers: ISZ[Marker] = IS()
    var microRosMarkers: ISZ[Marker] = IS()

    for (comp <- component.subComponents) {
      comp match {
        // a platform-provided component is a stock ROS 2 node, so it belongs with the ros2 half
        // regardless of what else the model contains
        case thread: AadlThread if RosUtil.isPlatformProvidedComponent(thread) =>
          ros2Decls = ros2Decls :+ genXmlFormatPlatformProvidedNodeDecl(thread, reporter)
          ros2Markers = ros2Markers :+ launchNodeConfigMarker(thread)
        case thread: AadlThread if microRosThreadPaths.contains(thread.path.toISZ) =>
          microRosDecls = microRosDecls :+ genXmlFormatMicroRosLaunchNodeDecl(microrosPkgName, thread)
          microRosMarkers = microRosMarkers :+ launchNodeConfigMarker(thread)
        case thread: AadlThread =>
          ros2Decls = ros2Decls :+ genXmlFormatLaunchNodeDecl(ros2PkgName, thread)
          ros2Markers = ros2Markers :+ launchNodeConfigMarker(thread)
        case system: AadlSystem =>
          ros2Decls = ros2Decls :+ genXmlFormatLaunchSystemDecl(ros2PkgName, system)
        case process: AadlProcess =>
          val (subRos2, subMicroRos, subRos2Markers, subMicroRosMarkers) = genXmlFormatLaunchDecls(process, ros2PkgName, microrosPkgName, microRosThreadPaths, reporter)
          ros2Decls = ros2Decls ++ subRos2
          microRosDecls = microRosDecls ++ subMicroRos
          ros2Markers = ros2Markers ++ subRos2Markers
          microRosMarkers = microRosMarkers ++ subMicroRosMarkers
        case _ =>
      }
    }

    return (ros2Decls, microRosDecls, ros2Markers, microRosMarkers)
  }

  // For example, see https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_bringup/launch/tc.launch.py
  // Creates a launch file for each system component in the model
  def genXmlFormatLaunchFiles(modelName: String, threadComponents: ISZ[AadlThread],
                              systemComponents: ISZ[AadlSystem],
                              microRosThreads: ISZ[AadlThread],
                              reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val ros2PkgName: String = genCppPackageName(modelName)
    val microrosPkgName: String = genMicroRosPackageName(modelName)

    var microRosThreadPaths: Set[ISZ[String]] = Set.empty
    for (t <- microRosThreads) {
      microRosThreadPaths = microRosThreadPaths + t.path.toISZ
    }

    var launchFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (system <- systemComponents) {
      val fileName = genXmlLaunchFileName(system.identifier)
      val launchDir: ISZ[String] = IS("src", s"${ros2PkgName}_bringup", "launch")

      val (ros2Decls, microRosDecls, ros2NodeMarkers, microRosNodeMarkers) = genXmlFormatLaunchDecls(system, ros2PkgName, microrosPkgName, microRosThreadPaths, reporter)

      // Every launch file now carries preserved blocks -- per node for parameters, and one for
      // launch arguments -- so the header must say the file is regenerated except between
      // markers rather than wholly generated.
      val header: String = CommentTemplate.invertedMarkerComment_xml

      if (microRosDecls.isEmpty) {
        // no micro-ROS nodes: one launch file, nothing to separate
        val body: ST =
          st"""${header}
              |
              |<launch>
              |    ${genXmlLaunchArgsBlock()}
              |
              |    ${(ros2Decls, "\n")}
              |</launch>
          """
        launchFiles = launchFiles :+ (launchDir :+ fileName, body, T, ros2NodeMarkers :+ launchArgsMarker)
      } else {
        // A micro-ROS node runs on a microcontroller in a real deployment, where it is flashed
        // rather than launched by ROS, and the agent is infrastructure that must already be up.
        // Emitting one file for everything would bake in the host-simulation assumption and leave
        // no way to launch just the ROS 2 half once the micro-ROS node is on hardware.  So the
        // two halves are separate files and the top level includes both.
        val ros2FileName = genXmlLaunchFileName(s"${system.identifier}_ros2")
        val microRosFileName = genXmlLaunchFileName(s"${system.identifier}_microros")

        val ros2Body: ST =
          st"""${header}
              |
              |<launch>
              |    ${genXmlLaunchArgsBlock()}
              |
              |    ${(ros2Decls, "\n")}
              |</launch>
          """
        launchFiles = launchFiles :+ (launchDir :+ ros2FileName, ros2Body, T, ros2NodeMarkers :+ launchArgsMarker)

        val microRosBody: ST =
          st"""${header}
              |
              |<!-- The micro-ROS half of the system, valid for a host deployment only.  On an
              |     embedded target these nodes are flashed rather than launched, and the agent
              |     may run elsewhere; in that case launch ${ros2FileName} alone.
              |
              |     The agent's transport must agree with RMW_UXRCE_TRANSPORT and the
              |     RMW_UXRCE_DEFAULT_UDP_* settings in microros_apps/colcon.meta.  Those live in
              |     a preserved block there, so changing them does not update the line below. -->
              |
              |<launch>
              |    <!-- micro-ROS agent: bridges rmw_microxrcedds nodes to the ROS2 DDS world.
              |         Invoked through `ros2 run` because the binary lives in the package's lib
              |         directory rather than on PATH. -->
              |    <executable cmd="ros2 run micro_ros_agent micro_ros_agent udp4 --port 8888" output="screen"/>
              |
              |    ${genXmlLaunchArgsBlock()}
              |
              |    ${(microRosDecls, "\n")}
              |</launch>
          """
        launchFiles = launchFiles :+ (launchDir :+ microRosFileName, microRosBody, T, microRosNodeMarkers :+ launchArgsMarker)

        val topBody: ST =
          st"""${header}
              |
              |<!-- Brings up the whole system on a host.  Launch ${ros2FileName} on its own when
              |     the micro-ROS nodes run on hardware rather than on this machine. -->
              |
              |<launch>
              |    ${genXmlLaunchArgsBlock()}
              |
              |    <include file="$$(find-pkg-share ${ros2PkgName}_bringup)/launch/${ros2FileName}"/>
              |    <include file="$$(find-pkg-share ${ros2PkgName}_bringup)/launch/${microRosFileName}"/>
              |</launch>
          """
        launchFiles = launchFiles :+ (launchDir :+ fileName, topBody, T, IS(launchArgsMarker))
      }
    }

    return launchFiles
  }


  //================================================
  //  I n t e r f a c e s  Setup Files
  //================================================
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  // The "Empty" datatype, which has no data fields, is used for event ports

  def genMsgFiles(modelName: String, datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var msg_files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    // platform-provided types already exist on the target platform, so no .msg file is emitted
    for (datatype <- datatypeMap.values if !datatype.isPlatformProvided) {
      msg_files = msg_files :+ genMsgFile(modelName, datatype.name, datatype.content)
    }
    msg_files = msg_files :+ (ISZ("src", s"${genCppPackageName(modelName)}_interfaces", "msg", "Empty.msg"), st"${CommentTemplate.doNotEditComment_hash}", T, IS())
    return msg_files
  }

  def genMsgFile(modelName: String, datatypeName: String, datatypeContent: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)

    val fileBody =
      st"""${CommentTemplate.doNotEditComment_hash}
           |
           |${(datatypeContent, "\n")}"""

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_interfaces", "msg", s"${datatypeName}.msg")

    return (filePath, fileBody, T, IS())
  }

  def genInterfacesCMakeListsFile(modelName: String, datatypeMap: Map[AadlType, Ros2Datatype]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"
    var msgTypes: ISZ[String] = IS()
    for (msg <- datatypeMap.valueSet.elements if !msg.isPlatformProvided) {
      msgTypes = msgTypes :+ s"msg/${msg.name}.msg"
    }
    msgTypes = msgTypes :+ s"msg/Empty.msg"

    val setupFileBody =
      st"""cmake_minimum_required(VERSION 3.8)
          |project(${top_level_package_nameT}_interfaces)
          |
          |${CommentTemplate.doNotEditComment_hash}
          |
          |if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
          |    add_compile_options(-Wall -Wextra -Wpedantic)
          |endif()
          |
          |find_package(ament_cmake REQUIRED)
          |
          |find_package(rosidl_default_generators REQUIRED)
          |
          |rosidl_generate_interfaces($${PROJECT_NAME}
          |  ${(msgTypes, "\n")}
          |)
          |
          |ament_export_dependencies(rosidl_default_runtime)
          |
          |ament_package()
        """

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_interfaces", fileName)

    return (filePath, setupFileBody, T, IS())
  }

  def genInterfacesPackageFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
          |
          |${CommentTemplate.doNotEditComment_xml}
          |
          |<package format="3">
          |    <name>${top_level_package_nameT}_interfaces</name>
          |    <version>0.0.0</version>
          |    <description>TODO: Package description</description>
          |    <maintainer email="sireum@todo.todo">sireum</maintainer>
          |    <license>TODO: License declaration</license>
          |
          |    <buildtool_depend>ament_cmake</buildtool_depend>
          |
          |    <build_depend>rosidl_default_generators</build_depend>
          |    <exec_depend>rosidl_default_runtime</exec_depend>
          |    <member_of_group>rosidl_interface_packages</member_of_group>
          |
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_interfaces", fileName)

    return (filePath, setupFileBody, T, IS())
  }


  //================================================
  //  Node files (C++)
  //    Example: https://github.com/santoslab/ros-examples/tree/main/tempControlcpp_ws/src/tc_cpp_pkg/src
  //================================================

  // Example:
  //  rclcpp::CallbackGroup::SharedPtr cb_group_;
  def genCppCallbackGroupVarHeader(): ST = {
    val callbackGroup: ST =
      st"rclcpp::CallbackGroup::SharedPtr ${callback_group_name};"
    return callbackGroup
  }

  // Example:
  //  cb_group_ = this->create_callback_group(rclcpp::CallbackGroupType::Reentrant);
  def genCppCallbackGroupVar(): ST = {
    val callbackGroup: ST =
      st"${callback_group_name} = this->create_callback_group(rclcpp::CallbackGroupType::${callback_group_type});"
    return callbackGroup
  }

  def genCppHeaderFileMsgTypeIncludes(msgTypes: ISZ[String]): ISZ[ST] = {
    var includes: ISZ[ST] = IS()

    for (msgType <- msgTypes) {
      val formattedInclude = formatDatatypeForInclude(msgType)
      includes = includes :+ st"#include \"${formattedInclude}.hpp\""
    }

    return includes
  }

  def genCMakeListsPkgRequirements(packages: ISZ[String]): ISZ[ST] = {
    var requirements: ISZ[ST] = IS()

    for (pkg <- packages) {
      requirements = requirements :+ st"find_package(${pkg} REQUIRED)"
    }

    return requirements
  }

  def genPackageFilePkgDependencies(packages: ISZ[String]): ISZ[ST] = {
    var requirements: ISZ[ST] = IS()

    for (pkg <- packages) {
      requirements = requirements :+ st"<depend>${pkg}</depend>"
    }

    return requirements
  }

  // Example:
  //  rclcpp::Subscription<example_interfaces::msg::Int32>::SharedPtr temp_control_currentTemp_subscription;
  def genCppTopicSubscriptionVarHeader(inPort: AadlPort, portType: String, outputPortCount: Z): ST = {
    val portName = genPortName(inPort)

    if (outputPortCount == 1) {
      val varHeader: ST =
        st"rclcpp::Subscription<${portType}>::SharedPtr ${portName}_subscription_;"
      return varHeader
    }

    // If the port is a fan in port
    var inPortHeaders: ISZ[ST] = IS()
    for (i <- 1 to outputPortCount) {
      inPortHeaders = inPortHeaders :+
        st"rclcpp::Subscription<${portType}>::SharedPtr ${portName}_subscription_${i};"
    }

    val varHeader: ST =
      st"${(inPortHeaders, "\n")}"

    return varHeader
  }

  // Example:
  //  temp_control_currentTemp_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
  //    "temp_control_currentTemp",
  //     1,
  //     std::bind(&TempControl::handle_currentTemp, this, std::placeholders::_1));
  def genCppTopicSubscription(inPort: AadlPort, nodeName: String, portType: String, derivedTopicNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier
    val outPortNames = subscriptionTopicNames(inPort, derivedTopicNames)

    var handler: ST = st""
    if (isEventPort(portType)) {
      handler = st"&${nodeName}::event_handle_${handlerName}"
    }
    else {
      handler = st"&${nodeName}::handle_${handlerName}"
    }

    if (outPortNames.size == 1) {
      val topicName = outPortNames.apply(0)
      val portCode: ST =
        st"""${portName}_subscription_ = this->create_subscription<${portType}>(
            |    "${topicName}",
            |    1,
            |    std::bind(${handler}, this, std::placeholders::_1), ${subscription_options_name});
          """

      return portCode
    }

    // If the port is a fan in port
    var inputInstances: ISZ[ST] = IS()
    var counter = 1

    for (outPortName <- outPortNames) {
      inputInstances = inputInstances :+
        st"""${portName}_subscription_${counter} = this->create_subscription<${portType}>(
            |    "${outPortName}",
            |    1,
            |    std::bind(${handler}, this, std::placeholders::_1), ${subscription_options_name});
          """

      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(inputInstances, "\n")}"

    return fanPortCode
  }

  def genCppTopicSubscriptionStrict(inPort: AadlPort, nodeName: String, portType: String, derivedTopicNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier
    val outPortNames = subscriptionTopicNames(inPort, derivedTopicNames)

    val handler: ST = st"${nodeName}::accept_${handlerName}"

    if (outPortNames.size == 1) {
      val topicName = outPortNames.apply(0)

      val portCode: ST =
        st"""${portName}_subscription_ = this->create_subscription<${portType}>(
            |    "${topicName}",
            |    1,
            |    std::bind(&${handler}, this, std::placeholders::_1), ${subscription_options_name});
        """
      return portCode
    }

    // If the port is a fan in port
    var inputInstances: ISZ[ST] = IS()
    var counter = 1

    for (outPortName <- outPortNames) {
      inputInstances = inputInstances :+
        st"""${portName}_subscription_${counter} = this->create_subscription<${portType}>(
            |    "${outPortName}",
            |    1,
            |    std::bind(&${handler}, this, std::placeholders::_1), ${subscription_options_name});
        """

      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(inputInstances, "\n")}"

    return fanPortCode
  }

  def genCppSubscriptionHandlerHeaderStrictS(nodeName: String, isSporadic: B): ST = {
    // Handles all ports in periodic components, or data-ports in sporadic components
    var portCode: ST =
      st"""void dataSubscriptionHandler(MsgType msg, std::queue<MsgType>& queue);"""

    // Event-port handler for sporadic components
    if (isSporadic) {
      portCode =
        st"""${portCode}
            |
            |void eventSubscriptionHandler(MsgType msg, std::queue<MsgType>& infrastructureQueue, std::queue<MsgType>& applicationQueue, void (${nodeName}::*handleMsg)(MsgType));
          """
    }

    return portCode
  }

  def genCppMessageAcceptorHeader(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    return st"void accept_${handlerName}(${portType} msg);"
  }

  def genCppMessageAcceptor(inPort: AadlPort, nodeName: String, isSporadic: B, portType: String): ST = {
    val handlerName = inPort.identifier

    val handler: ST =
    if (!isSporadic || inPort.isInstanceOf[AadlDataPort]) st"enqueue(infrastructureIn_${handlerName}, msg);"
    else
      st"""enqueue(infrastructureIn_${handlerName}, msg);
          |std::thread([this]() {
          |    std::lock_guard<std::mutex> lock(mutex_);
          |    receiveInputs(infrastructureIn_${handlerName}, applicationIn_${handlerName});
          |    if (applicationIn_${handlerName}.empty()) return;
          |    handle_${handlerName}_base(applicationIn_${handlerName}.front());
          |    applicationIn_${handlerName}.pop();
          |    sendOutputs();
          |}).detach();"""

    return st"""void ${nodeName}::accept_${handlerName}(${portType} msg)
               |{
               |    ${handler}
               |}
               |"""
  }

  // Example:
  //  rclcpp::Publisher<example_interfaces::msg::Int32>::SharedPtr temp_control_currentTemp_publisher;
  def genCppTopicPublisherVarHeader(outPort: AadlPort, portType: String, inputPortCount: Z): ST = {
    val portName = genPortName(outPort)

    if (inputPortCount == 1) {
      val varHeader: ST =
        st"rclcpp::Publisher<${portType}>::SharedPtr ${portName}_publisher_;"

      return varHeader
    }

    // If the port is a fan out port
    var outPortHeaders: ISZ[ST] = IS()
    for (i <- 1 to inputPortCount) {
      outPortHeaders = outPortHeaders :+
        st"rclcpp::Publisher<${portType}>::SharedPtr ${portName}_publisher_${i};"
    }

    val varHeader: ST =
      st"${(outPortHeaders, "\n")}"

    return varHeader
  }

  // Example:
  //  temp_control_currentTemp_publisher_ = this->create_publisher<example_interfaces::msg::Int32>(
  //    "operator_interface_currentTemp",
  //     1);
  def genCppTopicPublisher(outPort: AadlPort, portType: String, derivedTopicNames: ISZ[String]): ST = {
    val portName = genPortName(outPort)
    val inPortNames = publisherTopicNames(outPort, derivedTopicNames)

    if (inPortNames.size == 1) {
      val inPortName = inPortNames.apply(0)

      val portCode: ST =
        st"""${portName}_publisher_ = this->create_publisher<${portType}>(
            |    "${inPortName}",
            |    1);
          """
      return portCode
    }

    // If the port is a fan out port
    var outputInstances: ISZ[ST] = IS()
    var counter = 1

    for (inPortName <- inPortNames) {
      outputInstances = outputInstances :+
        st"""${portName}_publisher_${counter} = this->create_publisher<${portType}>(
            |    "${inPortName}",
            |    1);
          """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(outputInstances, "\n")}"

    return fanPortCode
  }

  // Example:
  //  void put_currentTemp(example_interfaces::msg::Int32 msg);
  def genCppPutMsgMethodHeader(outPort: AadlPort, portType: String): ST = {
    val handlerName = outPort.identifier

    var publisherHeader: ST = st"void put_${handlerName}("

    if (!isEventPort(portType)) {
      publisherHeader = st"${publisherHeader}${portType} msg"
    }

    publisherHeader = st"${publisherHeader});"
    return publisherHeader
  }

  // Only used for strict mode (in lax mode, the put_msg method publishes instantly)
  def genCppTopicPublishMethodHeaderStrict(outPort: AadlPort): ST = {
    val handlerName = outPort.identifier

    val publisherHeaders: ST =
      st"void sendOut_${handlerName}(MsgType msg);"

    return publisherHeaders
  }

  // Example:
  //  void TempControl::put_currentTemp(example_interfaces::msg::Int32 msg)
  //  {
  //    temp_control_currentTemp_publisher->publish(msg);
  //  }
  def genCppTopicPublishMethod(outPort: AadlPort, nodeName: String, portType: String, inputPortCount: Z): ST = {
    val portName = genPortName(outPort)
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"${portName}_publisher_->publish(msg);"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"${portName}_publisher_${i}->publish(msg);"
      }
    }

    var publisherCode: ST = st""

    if (isEventPort(portType)) {
      publisherCode =
        st"""void ${nodeName}::put_${handlerName}()
            |{
            |    ${portType} msg = ${portType}();
            |
            |    ${(publishers, "\n")}
            |}
          """
    }
    else {
      publisherCode =
        st"""void ${nodeName}::put_${handlerName}(${portType} msg)
            |{
            |    ${(publishers, "\n")}
            |}
          """
    }

    return publisherCode
  }

  def genCppTopicPublishMethodStrict(outPort: AadlPort, nodeName: String, portType: String, inputPortCount: Z): ST = {
    val portName = genPortName(outPort)
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"${portName}_publisher_->publish(*typedMsg);"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"${portName}_publisher_${i}->publish(*typedMsg);"
      }
    }

    val publisherCode: ST =
      st"""void ${nodeName}::sendOut_${handlerName}(MsgType msg)
          |{
          |    if (auto typedMsg = std::get_if<${portType}>(&msg)) {
          |        ${(publishers, "\n")}
          |    } else {
          |        PRINT_ERROR("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.");
          |    }
          |}
         """

    return publisherCode
  }

  // This method is called by the user, and it puts a message into a port's outApplication queue
  def genCppPutMsgMethodStrict(outPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = outPort.identifier

    var putMsgCode: ST = st""

    if (isEventPort(portType)) {
      putMsgCode =
        st"""void ${nodeName}::put_${handlerName}()
            |{
            |    enqueue(applicationOut_${handlerName}, ${portType}());
            |}
        """
    }
    else {
      putMsgCode =
        st"""void ${nodeName}::put_${handlerName}(${portType} msg)
            |{
            |    enqueue(applicationOut_${handlerName}, msg);
            |}
        """
    }

    return putMsgCode
  }

  // Example:
  //  virtual void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) = 0;
  def genCppSubscriptionHandlerVirtualHeader(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""void event_handle_${handlerName}(const ${portType}::SharedPtr msg);
                                      |virtual void handle_${handlerName}() = 0;"""
    }
    else {
      subscriptionHandlerHeader = st"virtual void handle_${handlerName}(const ${portType}::SharedPtr msg) = 0;"
    }
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerVirtualHeaderStrict(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"virtual void handle_${handlerName}() = 0;"
    }
    else {
      subscriptionHandlerHeader = st"virtual void handle_${handlerName}(const ${portType} msg) = 0;"
    }

    return subscriptionHandlerHeader
  }

  def genCppEventPortHandler(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = inPort.identifier

    val handler: ST = st"""void ${nodeName}::event_handle_${handlerName}(const ${portType}::SharedPtr msg)
                                    |{
                                    |    (void)msg;
                                    |    handle_${handlerName}();
                                    |}
                                    """

    return handler
  }

  def genCppSubscriptionHandlerSporadicWithExamples(inPort: AadlPort, nodeName: String, portType: String,
                                                    inDataPorts: ISZ[AadlPort], packageName: String,
                                                    datatypeMap: Map[AadlType, Ros2Datatype],
                                                    reporter: Reporter): ST = {
    val handlerName = inPort.identifier

    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"// example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {
        val dataPortType: String = genPortDatatype(inDataPort, packageName, datatypeMap, reporter)
        exampleUsage =
          st"""${exampleUsage}
              |${dataPortType}::SharedPtr ${inDataPort.identifier} = get_${inDataPort.identifier}();
              |${genCppReceivedLog(inDataPort, inDataPort.identifier)}"""
      }
    }


    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}()
                                      |{
                                      |    // Handle ${handlerName} event
                                      |    PRINT_INFO("Received ${handlerName}");"""
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType}::SharedPtr msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    ${genCppReceivedLog(inPort, "msg")}"""
    }

    if (inDataPorts.size > 0) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
            |
            |    ${exampleUsage}"""
    }

    subscriptionHandlerHeader =
      st"""${subscriptionHandlerHeader}
          |}
        """

    return subscriptionHandlerHeader
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) {}
  def genCppSubscriptionHandlerSporadic(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}()
                                      |{
                                      |    // Handle ${handlerName} event
                                      |    PRINT_INFO("Received ${handlerName}");
                                      |}
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType}::SharedPtr msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    ${genCppReceivedLog(inPort, "msg")}
                                      |}
                                    """
    }

    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerSporadicStrictWithExamples(inPort: AadlPort, nodeName: String, portType: String,
                                                          inDataPorts: ISZ[AadlPort], packageName: String,
                                                          datatypeMap: Map[AadlType, Ros2Datatype],
                                                          reporter: Reporter): ST = {
    val handlerName = inPort.identifier

    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"// example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {
        val dataPortType: String = genPortDatatype(inDataPort, packageName, datatypeMap, reporter)
        exampleUsage =
          st"""${exampleUsage}
              |${dataPortType} ${inDataPort.identifier} = get_${inDataPort.identifier}();
              |${genCppReceivedLog(inDataPort, inDataPort.identifier)}"""
      }
    }

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}()
                                      |{
                                      |    // Handle ${handlerName} event
                                      |    PRINT_INFO("Received ${handlerName}");"""
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType} msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    ${genCppReceivedLog(inPort, "msg")}"""
    }

    if (inDataPorts.size > 0) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
            |
            |    ${exampleUsage}"""
    }

    subscriptionHandlerHeader =
      st"""${subscriptionHandlerHeader}
          |}
        """

    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerSporadicStrict(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}()
                                      |{
                                      |    // Handle ${handlerName} event
                                      |    PRINT_INFO("Received ${handlerName}");
                                      |}
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType} msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    ${genCppReceivedLog(inPort, "msg")}
                                      |}
                                    """
    }

    return subscriptionHandlerHeader
  }

  // Only used for strict mode currently
  def genCppSubscriptionHandlerBaseSporadicHeader(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    val handlerCode: ST =
      st"""void handle_${handlerName}_base(MsgType msg);"""

    return handlerCode
  }

  def genCppExamplePublisher(outPort: AadlPort, packageName: String,
                                   datatypeMap: Map[AadlType, Ros2Datatype],
                                   reporter: Reporter): ST = {
    val handlerName = outPort.identifier
    val dataPortType: String = genPortDatatype(outPort, packageName, datatypeMap, reporter)

    var publisherCode: ST = st""

    if (isEventPort(dataPortType)) {
      publisherCode =
        st"put_${handlerName}();"
    } else {
      val initExpr = portExampleInit(outPort, s"${dataPortType}()", datatypeMap)
      publisherCode =
        st"""${dataPortType} ${handlerName} = ${initExpr};
            |put_${handlerName}(${handlerName});"""
    }

    return publisherCode
  }

  // Used to convert the type of the msg from MsgType to the intended type before calling the user-defined handler
  def genCppSubscriptionHandlerBaseSporadic(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = inPort.identifier

    var handlerCode: ST = st""
    if (isEventPort(portType)) {
      handlerCode = st"""void ${nodeName}::handle_${handlerName}_base(MsgType msg)
                        |{
                        |    (void)msg;
                        |    handle_${handlerName}();
                        |}
                      """
    }
    else {
      handlerCode = st"""void ${nodeName}::handle_${handlerName}_base(MsgType msg)
                        |{
                        |    if (auto typedMsg = std::get_if<${portType}>(&msg)) {
                        |        handle_${handlerName}(*typedMsg);
                        |    } else {
                        |        PRINT_ERROR("Receiving wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.");
                        |    }
                        |}
                      """
    }

    return handlerCode
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg);
  def genCppSubscriptionHandlerHeader(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"void handle_${handlerName}();"
    }
    else {
      subscriptionHandlerHeader = st"void handle_${handlerName}(const ${portType}::SharedPtr msg);"
    }

    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerHeaderStrict(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"void handle_${handlerName}();"
    }
    else {
      subscriptionHandlerHeader = st"void handle_${handlerName}(const ${portType} msg);"
    }

    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerPeriodic(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val handlerName = inPort.identifier

    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const ${portType}::SharedPtr msg)
          |{
          |    ${handlerName}_msg_holder = msg;
          |}
        """
    return subscriptionHandlerHeader
  }

  // Example:
  // example_interfaces::msg::Int32::SharedPtr currentTemp_msg_holder;
  def genCppSubscriptionMessageVar(inPort: AadlPort, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessageVar: ST =
      st"${portType}::SharedPtr ${portName}_msg_holder;"
    return subscriptionMessageVar
  }

  def genCppInfrastructureInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val inMsgQueue: ST =
      st"std::queue<MsgType> infrastructureIn_${portName};"
    return inMsgQueue
  }

  def genCppApplicationInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val inMsgQueue: ST =
      st"std::queue<MsgType> applicationIn_${portName};"
    return inMsgQueue
  }

  def genCppInfrastructureOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val outMsgQueue: ST =
      st"std::queue<MsgType> infrastructureOut_${portName};"
    return outMsgQueue
  }

  def genCppApplicationOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val outMsgQueue: ST =
      st"std::queue<MsgType> applicationOut_${portName};"
    return outMsgQueue
  }

  def genCppGetSubscriptionMessageHeader(inPort: AadlPort, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessageHeader: ST =
      st"${portType}::SharedPtr get_${portName}();"
    return subscriptionMessageHeader
  }

  def genCppGetApplicationInValueHeader(inPort: AadlPort, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessageHeader: ST =
      st"${portType} get_${portName}();"
    return subscriptionMessageHeader
  }

  def genCppGetApplicationInValue(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessageHeader: ST =
      st"""${portType} ${nodeName}::get_${portName}() {
          |    MsgType msg = applicationIn_${portName}.front();
          |    return std::get<${portType}>(msg);
          |}
        """
    return subscriptionMessageHeader
  }

  def genCppReceiveInputsSporadicHeader(): ST = {
    val method: ST =
      st"""void receiveInputs(std::queue<MsgType>& infrastructureQueue, std::queue<MsgType>& applicationQueue);"""
    return method
  }

  def genCppReceiveInputsSporadic(nodeName: String): ST = {
    val method: ST =
      st"""void ${nodeName}::receiveInputs(std::queue<MsgType>& infrastructureQueue, std::queue<MsgType>& applicationQueue) {
          |    if (!infrastructureQueue.empty()) {
          |        MsgType eventMsg = infrastructureQueue.front();
          |        infrastructureQueue.pop();
          |        enqueue(applicationQueue, eventMsg);
          |    }
          |
          |    for (std::tuple<std::queue<MsgType>*, std::queue<MsgType>*> port : inDataPortTupleVector) {
          |        auto infrastructureQueue = std::get<0>(port);
          |        if (!infrastructureQueue->empty()) {
          |            auto msg = infrastructureQueue->front();
          |            enqueue(*std::get<1>(port), msg);
          |        }
          |    }
          |}
        """
    return method
  }

  def genCppReceiveInputsPeriodicHeader(): ST = {
    val method: ST =
      st"""void receiveInputs();"""
    return method
  }

  def genCppReceiveInputsPeriodic(nodeName: String): ST = {
    val method: ST =
      st"""void ${nodeName}::receiveInputs() {
          |    for (std::tuple<std::queue<MsgType>*, std::queue<MsgType>*> port : inDataPortTupleVector) {
          |        auto infrastructureQueue = std::get<0>(port);
          |        if (!infrastructureQueue->empty()) {
          |            auto msg = infrastructureQueue->front();
          |            enqueue(*std::get<1>(port), msg);
          |        }
          |    }
          |    for (std::tuple<std::queue<MsgType>*, std::queue<MsgType>*> port : inEventPortTupleVector) {
          |        auto infrastructureQueue = std::get<0>(port);
          |        if (!infrastructureQueue->empty()) {
          |            auto msg = infrastructureQueue->front();
          |            infrastructureQueue->pop();
          |            enqueue(*std::get<1>(port), msg);
          |        }
          |    }
          |}
        """
    return method
  }

  def genCppSendOutputsHeader(): ST = {
    val method: ST =
      st"void sendOutputs();"
    return method
  }

  // For all non-empty out application port queues, pop the queue and add the message to the corresponding infrastructure queue
  // Then, for all non-empty infrastructure queues, pop the queue and publish the message
  def genCppSendOutputs(nodeName: String): ST = {
    val method: ST =
      st"""void ${nodeName}::sendOutputs() {
          |    for (std::tuple<std::queue<MsgType>*, std::queue<MsgType>*, void (${nodeName}::*)(MsgType)> port : outPortTupleVector) {
          |        auto applicationQueue = std::get<0>(port);
          |        if (applicationQueue->size() != 0) {
          |            auto msg = applicationQueue->front();
          |            applicationQueue->pop();
          |            enqueue(*std::get<1>(port), msg);
          |        }
          |    }
          |
          |    for (std::tuple<std::queue<MsgType>*, std::queue<MsgType>*, void (${nodeName}::*)(MsgType)> port : outPortTupleVector) {
          |        auto infrastructureQueue = std::get<1>(port);
          |        if (infrastructureQueue->size() != 0) {
          |            auto msg = infrastructureQueue->front();
          |            infrastructureQueue->pop();
          |            (this->*std::get<2>(port))(msg);
          |        }
          |    }
          |}
        """
    return method
  }

  def genCppEnqueueHeader(): ST = {
    val method: ST =
      st"""void enqueue(std::queue<MsgType>& queue, MsgType val);"""
    return method
  }

  // Currently, all queues are treated as having a size of 1.
  def genCppEnqueue(nodeName: String): ST = {
    val method: ST =
      st"""void ${nodeName}::enqueue(std::queue<MsgType>& queue, MsgType val) {
          |    if (queue.size() >= 1) {
          |        queue.pop();
          |    }
          |    queue.push(val);
          |}
        """
    return method
  }

  def genCppGetSubscriptionMessage(inPort: AadlPort, nodeName: String, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessage: ST =
      st"""${portType}::SharedPtr ${nodeName}::get_${portName}() {
          |    return ${portName}_msg_holder;
          |}
        """
    return subscriptionMessage
  }

  def genCppDataPortInitializerHeader(inDataPort: AadlPort, portType: String): ST = {
    val portName = inDataPort.identifier

    val initializerHeader: ST =
      st"void init_${portName}(${portType} val);"
    return initializerHeader
  }

  def genCppDataPortInitializerHeaderStrict(inDataPort: AadlPort, portType: String): ST = {
    val portName = inDataPort.identifier

    val initializerHeader: ST =
      st"void init_${portName}(${portType} val);"
    return initializerHeader
  }

  def genCppDataPortInitializer(inDataPort: AadlPort, nodeName: String, portType: String): ST = {
    val portName = inDataPort.identifier

    val initializer: ST =
      st"""void ${nodeName}::init_${portName}(${portType} val) {
          |    ${portName}_msg_holder = std::make_shared<${portType}>(val);
          |}"""
    return initializer
  }

  def genCppDataPortInitializerStrict(inDataPort: AadlPort, nodeName: String, portType: String): ST = {
    val portName = inDataPort.identifier

    val initializer: ST =
      st"""void ${nodeName}::init_${portName}(${portType} val) {
          |    enqueue(infrastructureIn_${portName}, val);
          |}"""
    return initializer
  }

  def genCppTimeTriggeredMethodHeader(): ST = {
    val timeTriggeredHeader: ST =
      st"void timeTriggered();"
    return timeTriggeredHeader
  }

  def genCppTimeTriggeredMethod(nodeName: String, inDataPorts: ISZ[AadlPort], examplePublishers: ISZ[ST],
                                packageName: String, datatypeMap: Map[AadlType, Ros2Datatype],
                                strictAADLMode: B, reporter: Reporter): ST = {
    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"// example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {
        val dataPortType: String = genPortDatatype(inDataPort, packageName, datatypeMap, reporter)

        if (strictAADLMode) {
          exampleUsage =
            st"""${exampleUsage}
                |${dataPortType} ${inDataPort.identifier} = get_${inDataPort.identifier}();
                |${genCppReceivedLog(inDataPort, inDataPort.identifier)}"""
        }
        else {
          exampleUsage =
            st"""${exampleUsage}
                |${dataPortType}::SharedPtr ${inDataPort.identifier} = get_${inDataPort.identifier}();
                |${genCppReceivedLog(inDataPort, inDataPort.identifier)}"""
        }
      }
    }

    var timeTriggered: ST =
      st"""void ${nodeName}::timeTriggered()
          |{
          |    // Handle communication
        """

    if (inDataPorts.size > 0) {
      timeTriggered =
        st"""${timeTriggered}
            |    ${exampleUsage}
          """
    }

    if (examplePublishers.nonEmpty) {
      timeTriggered =
        st"""${timeTriggered}
            |    // Example publishing messages
            |    ${(examplePublishers, "\n")}"""
    }

    timeTriggered =
      st"""${timeTriggered}
          |}
        """

    return timeTriggered
  }

  def genCppTimeTriggeredTimerHeader(): ST = {
    val timer: ST =
      st"rclcpp::TimerBase::SharedPtr periodTimer_;"
    return timer
  }

  def genCppTimeTriggeredTimer(nodeName: String, component: AadlThread): ST = {
    val period = component.period.get

    val timer: ST =
      st"""periodTimer_ = this->create_wall_timer(std::chrono::milliseconds(${period}),
          |    std::bind(&${nodeName}::timeTriggered, this), ${callback_group_name});"""
    return timer
  }

  def genCppTimeTriggeredTimerStrict(nodeName: String, component: AadlThread): ST = {
    val period = component.period.get

    val timer: ST =
      st"""periodTimer_ = this->create_wall_timer(std::chrono::milliseconds(${period}),
          |    std::bind(&${nodeName}::timeTriggeredCaller, this), ${callback_group_name});"""
    return timer
  }

  def genCppTimeTriggeredCallerHeader(): ST = {
    val timeTriggeredHeader: ST =
      st"""void timeTriggeredCaller();"""
    return timeTriggeredHeader
  }

  // Used for strict mode to handle infrastructure and application port communication
  def genCppTimeTriggeredCaller(nodeName: String): ST = {
    val timeTriggered: ST =
      st"""void ${nodeName}::timeTriggeredCaller() {
          |    receiveInputs();
          |    timeTriggered();
          |    sendOutputs();
          |}
        """
    return timeTriggered
  }

  // The outPortTupleVector is a vector of tuples, each tuple containing pointers to the port's out application queue,
  // out infrastructure queue, and put_portName method.
  def genCppOutPortTupleVectorHeader(nodeName: String): ST = {
    val vector: ST =
      st"std::vector<std::tuple<std::queue<MsgType>*, std::queue<MsgType>*, void (${nodeName}::*)(MsgType)>> outPortTupleVector;"
    return vector
  }

  def genCppOutPortTupleVector(nodeName: String, portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&applicationOut_${name}, &infrastructureOut_${name}, &${nodeName}::sendOut_${name}}"
    }

    val vector: ST =
      st"""outPortTupleVector = {
          |    ${(tuples, ",\n")}
          |};
        """
    return vector
  }

  // The inPortTupleVector is a vector of tuples, each tuple containing pointers to the port's in infrastructure queue and
  // in application queue.  It only holds tuples of data ports (or all ports, for periodic components).
  def genCppInDataPortTupleVectorHeader(): ST = {
    val vector: ST =
      st"std::vector<std::tuple<std::queue<MsgType>*, std::queue<MsgType>*>> inDataPortTupleVector;"
    return vector
  }

  def genCppInDataPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&infrastructureIn_${name}, &applicationIn_${name}}"
    }

    val vector: ST =
      st"""inDataPortTupleVector = {
          |    ${(tuples, ",\n")}
          |};
        """
    return vector
  }

  // This vector is only used when a periodic component has event or eventdata ports, which probably shouldn't happen.
  // It's here to make sure ports don't get skipped over if they are marked event or eventdata in a periodic component.
  def genCppInEventPortTupleVectorHeader(): ST = {
    val vector: ST =
      st"std::vector<std::tuple<std::queue<MsgType>*, std::queue<MsgType>*>> inEventPortTupleVector;"
    return vector
  }

  def genCppInEventPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&infrastructureIn_${name}, &applicationIn_${name}}"
    }

    val vector: ST =
      st"""inEventPortTupleVector = {
          |    ${(tuples, ",\n")}
          |};
        """
    return vector
  }


  def genCppBaseNodeHeaderFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               datatypeMap: Map[AadlType, Ros2Datatype], hasEnumConverter: B,
                               strictAADLMode: B, invertTopicBinding: B,
                               reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = s"${genNodeName(component)}_base"
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHeaders: ISZ[ST] = IS()
    var publisherHeaders: ISZ[ST] = IS()
    var putMethodHeaders: ISZ[ST] = IS()
    var subscriptionHandlerHeaders: ISZ[ST] = IS()
    var inMsgVars: ISZ[ST] = IS()
    var outMsgVars: ISZ[ST] = IS()
    var subscriptionMessageAcceptorHeaders: ISZ[ST] = IS()
    var subscriptionMessageGetterHeaders: ISZ[ST] = IS()
    var strictPublisherHeaders: ISZ[ST] = IS()
    var msgTypes: ISZ[String] = IS()
    var dataPortInitializerHeaders: ISZ[ST] = IS()
    var msgToStringMacro: ST = st""

    for (p <- generatedPorts(component)) {
      val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!ISZOps(msgTypes).contains(portDatatype)) {
        msgTypes = msgTypes :+ portDatatype
      }
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializerHeaders = dataPortInitializerHeaders :+
              genCppDataPortInitializerHeaderStrict(p, portDatatype)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, outputPorts.size)
            }
            else {
              // In ports with no connections should still subscribe to a topic (for other non-generated components
              // to publish to, for example)
              subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, 1)
            }
          }
          else {
            subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, 1)
          }
          subscriptionMessageAcceptorHeaders = subscriptionMessageAcceptorHeaders :+
            genCppMessageAcceptorHeader(p, portDatatype)

          inMsgVars = inMsgVars :+ genCppInfrastructureInQueue(p)
          inMsgVars = inMsgVars :+ genCppApplicationInQueue(p)
          if (!p.isInstanceOf[AadlDataPort] && isSporadic(component)) {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerVirtualHeaderStrict(p, portDatatype)
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerBaseSporadicHeader(p)
          }
          else {
            subscriptionMessageGetterHeaders = subscriptionMessageGetterHeaders :+ genCppGetApplicationInValueHeader(p, portDatatype)
          }
        }
        else {
          outMsgVars = outMsgVars :+ genCppInfrastructureOutQueue(p)
          outMsgVars = outMsgVars :+ genCppApplicationOutQueue(p)
          strictPublisherHeaders = strictPublisherHeaders :+ genCppTopicPublishMethodHeaderStrict(p)
          putMethodHeaders = putMethodHeaders :+ genCppPutMsgMethodHeader(p, portDatatype)
          if (invertTopicBinding) {
            publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, 1)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, inputPorts.size)
            }
            else {
              // Out ports with no connections should still publish to a topic (for other non-generated components
              // to subscribe to, for example)
              publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, 1)
            }
          }
        }
      }
      else {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializerHeaders = dataPortInitializerHeaders :+
              genCppDataPortInitializerHeader(p, portDatatype)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, outputPorts.size)
            }
            else {
              // In ports with no connections should still subscribe to a topic (for other non-generated components
              // to publish to, for example)
              subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, 1)
            }
          }
          else {
            subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p, portDatatype, 1)
          }
          if (isSporadic(component) && !p.isInstanceOf[AadlDataPort]) {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerVirtualHeader(p, portDatatype)
          }
          else {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerHeader(p, portDatatype)
            inMsgVars = inMsgVars :+ genCppSubscriptionMessageVar(p, portDatatype)
            subscriptionMessageGetterHeaders = subscriptionMessageGetterHeaders :+ genCppGetSubscriptionMessageHeader(p, portDatatype)
          }
        }
        else {
          if (invertTopicBinding) {
            publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, 1)
            putMethodHeaders = putMethodHeaders :+ genCppPutMsgMethodHeader(p, portDatatype)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, inputPorts.size)
              putMethodHeaders = putMethodHeaders :+ genCppPutMsgMethodHeader(p, portDatatype)
            }
            else {
              // Out ports with no connections should still publish to a topic
              publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, portDatatype, 1)
              putMethodHeaders = putMethodHeaders :+ genCppPutMsgMethodHeader(p, portDatatype)
            }
          }
        }
      }
    }

    if (subscriptionHeaders.size > 0) {
      subscriptionHeaders = subscriptionHeaders :+ st""
    }

    val typeIncludes: ISZ[ST] = genCppHeaderFileMsgTypeIncludes(msgTypes)
    var stdIncludes: ST =
      st"""#include <queue>
          |#include <sstream>"""

    if (strictAADLMode) {
      stdIncludes =
        st"""${stdIncludes}
            |#include <vector>
            |#include <variant>
            |#include <mutex>"""
    }

    val enumConverterInclude: ST = if (hasEnumConverter) st"""#include "${packageName}/base_headers/enum_converter.hpp"""" else st""
    val exampleTypesInclude: ST = st"""#include "${packageName}/base_headers/example_types.hpp""""
    val msgToStringBlockOpt = genCppMsgToStringBlock(component, packageName, datatypeMap, hasEnumConverter, reporter)
    val msgToStringHelpers: ST = msgToStringBlockOpt match {
      case Some(block) => st"\n${block}\n"
      case _ => st""
    }
    if (msgToStringBlockOpt.nonEmpty) {
      if (strictAADLMode) {
        msgToStringMacro = st"#define MESSAGE_TO_STRING(message) _messageToString(message).c_str()"
      } else {
        msgToStringMacro = st"#define MESSAGE_TO_STRING(message) _messageToString(*message).c_str()"
      }
    }

    var fileBody =
      st"""#include "rclcpp/rclcpp.hpp"
          |${(typeIncludes, "\n")}
          |${(stdIncludes, "\n")}
          |${enumConverterInclude}
          |${exampleTypesInclude}
          |
          |${CommentTemplate.doNotEditComment_slash}
          |${msgToStringHelpers}
          |class ${nodeName} : public rclcpp::Node
          |{
          |protected:"""

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |    using MsgType = std::variant<${(msgTypes, ", ")}>;
          """
    }

    fileBody =
      st"""${fileBody}
          |    ${nodeName}();
          |
          |    //=================================================
          |    //  C o m m u n i c a t i o n
          |    //=================================================
          |
          |    ${msgToStringMacro}
          |    #define PRINT_INFO(...) RCLCPP_INFO(this->get_logger(), __VA_ARGS__)
          |    #define PRINT_WARN(...) RCLCPP_WARN(this->get_logger(), __VA_ARGS__)
          |    #define PRINT_ERROR(...) RCLCPP_ERROR(this->get_logger(), __VA_ARGS__)
          |
          |    ${(putMethodHeaders, "\n")}
        """

    if (subscriptionMessageGetterHeaders.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(subscriptionMessageGetterHeaders, "\n")}
          """
    }

    if (dataPortInitializerHeaders.size > 0) {
      fileBody =
        st"""${fileBody}
            |    // Methods to be used to set initial values for data ports
            |    ${(dataPortInitializerHeaders, "\n")}
          """
    }

    fileBody =
      st"""${fileBody}
          |private:
          |    ${genCppCallbackGroupVarHeader()}
        """

    if (strictAADLMode) {
      if (subscriptionMessageAcceptorHeaders.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(subscriptionMessageAcceptorHeaders, "\n")}
          """
      }

      val receiveInputsHeader: ST = if (isSporadic(component)) genCppReceiveInputsSporadicHeader()
                              else genCppReceiveInputsPeriodicHeader()

      fileBody =
        st"""${fileBody}
            |    // Methods for working with port queues
            |    ${receiveInputsHeader}
            |
            |    ${genCppSendOutputsHeader()}
            |
            |    ${genCppEnqueueHeader()}
          """
    }

    fileBody =
      st"""${fileBody}
          |    // SubscriptionOptions for assigning subscriptions to the callback group
          |    rclcpp::SubscriptionOptions ${subscription_options_name};
        """

    if (subscriptionHandlerHeaders.size > 0) {
      if (strictAADLMode) {
        fileBody =
          st"""${fileBody}
              |    ${genCppSubscriptionHandlerHeaderStrictS(nodeName, isSporadic(component))}"""
      }

      fileBody =
        st"""${fileBody}
            |
            |    //=================================================
            |    //  C o m p u t e    E n t r y    P o i n t
            |    //=================================================
            |    ${(subscriptionHandlerHeaders, "\n")}
          """
    }

    if (inMsgVars.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(inMsgVars, "\n")}
          """
    }

    if (outMsgVars.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(outMsgVars, "\n")}
            |
            |    ${(strictPublisherHeaders, "\n")}
          """
    }

    fileBody =
      st"""${fileBody}
          |    //=================================================
          |    //  C o m m u n i c a t i o n
          |    //=================================================
          |    ${(subscriptionHeaders ++ publisherHeaders, "\n")}
        """

    if (!isSporadic(component)) {
      fileBody =
        st"""${fileBody}
            |    //=================================================
            |    //  C a l l b a c k   a n d   T i m e r
            |    //=================================================
            |    virtual void timeTriggered() = 0;
            |
            |    ${genCppTimeTriggeredTimerHeader()}
          """

      if (strictAADLMode) {
        fileBody =
          st"""${fileBody}
              |    ${genCppTimeTriggeredCallerHeader()}
          """
      }
    }

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |    // Used for thread locking
            |    std::mutex ${mutex_name};
            |
            |    // Used by receiveInputs
            |    ${genCppInDataPortTupleVectorHeader()}"""

      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
              |    // Used by receiveInputs
              |    ${genCppInEventPortTupleVectorHeader()}"""
      }

      fileBody =
        st"""${fileBody}
            |    // Used by sendOutputs
            |    ${genCppOutPortTupleVectorHeader(nodeName)}"""
    }

    fileBody = st"""${fileBody}
                   |};
                 """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "base_headers", fileName)

    return (filePath, fileBody, T, IS())
  }

  def genCppBaseNodeCppFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                            datatypeMap: Map[AadlType, Ros2Datatype], strictAADLMode: B,
                            invertTopicBinding: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = s"${genNodeName(component)}_base"
    val fileName = genCppNodeSourceName(nodeName)

    var subscribers: ISZ[ST] = IS()
    var publishers: ISZ[ST] = IS()
    var subscriberMethods: ISZ[ST] = IS()
    var publisherMethods: ISZ[ST] = IS()
    var subscriptionMessageGetters: ISZ[ST] = IS()
    var eventPortHandlers: ISZ[ST] = IS()
    var dataPortInitializers: ISZ[ST] = IS()

    var outPortNames: ISZ[String] = IS()
    var inPortNames: ISZ[String] = IS()
    var strictPutMsgMethods: ISZ[ST] = IS()
    var strictSubscriptionMessageAcceptorMethods: ISZ[ST] = IS()
    var strictSubscriptionHandlerBaseMethods: ISZ[ST] = IS()

    var hasInPorts = F
    for (p <- generatedPorts(component)) {
      val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializers = dataPortInitializers :+ genCppDataPortInitializerStrict(p, nodeName, portDatatype)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              val outputPortNames = getPortNames(outputPorts)

              subscribers = subscribers :+
                genCppTopicSubscriptionStrict(p, nodeName, portDatatype, outputPortNames)
            }
            else {
              // In ports with no connections should still subscribe to a topic
              subscribers = subscribers :+
                genCppTopicSubscriptionStrict(p, nodeName, portDatatype, getPortNames(IS(p.path.toISZ)))
            }
          }
          else {
            subscribers = subscribers :+
              genCppTopicSubscriptionStrict(p, nodeName, portDatatype, getPortNames(IS(p.path.toISZ)))
          }

          strictSubscriptionMessageAcceptorMethods = strictSubscriptionMessageAcceptorMethods :+
            genCppMessageAcceptor(p, nodeName, isSporadic(component), portDatatype)

          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            inPortNames = inPortNames :+ p.identifier
            subscriptionMessageGetters = subscriptionMessageGetters :+ genCppGetApplicationInValue(p, nodeName, portDatatype)
          }
          else {
            strictSubscriptionHandlerBaseMethods = strictSubscriptionHandlerBaseMethods :+
              genCppSubscriptionHandlerBaseSporadic(p, nodeName, portDatatype)
          }

          hasInPorts = T
        }
        else {
          outPortNames = outPortNames :+ p.identifier
          if (invertTopicBinding) {
            publishers = publishers :+ genCppTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            publisherMethods = publisherMethods :+
              genCppTopicPublishMethodStrict(p, nodeName, portDatatype, 1)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              val inputPortNames = getPortNames(inputPorts)
              publishers = publishers :+ genCppTopicPublisher(p, portDatatype, inputPortNames)
              publisherMethods = publisherMethods :+
                genCppTopicPublishMethodStrict(p, nodeName, portDatatype, inputPortNames.size)
            }
            else {
              // Out ports with no connections should still publish to a topic
              publishers = publishers :+ genCppTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
              publisherMethods = publisherMethods :+
                genCppTopicPublishMethodStrict(p, nodeName, portDatatype, 1)
            }
          }
          strictPutMsgMethods = strictPutMsgMethods :+ genCppPutMsgMethodStrict(p, nodeName, portDatatype)
        }
      }
      else {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializers = dataPortInitializers :+ genCppDataPortInitializer(p, nodeName, portDatatype)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              val outputPortNames = getPortNames(outputPorts)
              subscribers = subscribers :+ genCppTopicSubscription(p, nodeName, portDatatype, outputPortNames)
            }
            else {
              // In ports with no connections should still subscribe to a topic
              subscribers = subscribers :+
                genCppTopicSubscription(p, nodeName, portDatatype, getPortNames(IS(p.path.toISZ)))
            }
          }
          else {
            subscribers = subscribers :+
              genCppTopicSubscription(p, nodeName, portDatatype, getPortNames(IS(p.path.toISZ)))
          }
          // Specifically for event ports, not eventdata ports (no data to be handled)
          if (isEventPort(portDatatype)) {
            eventPortHandlers = eventPortHandlers :+ genCppEventPortHandler(p, nodeName, portDatatype)
          }
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            subscriberMethods = subscriberMethods :+
              genCppSubscriptionHandlerPeriodic(p, nodeName, portDatatype)
            subscriptionMessageGetters = subscriptionMessageGetters :+ genCppGetSubscriptionMessage(p, nodeName, portDatatype)
          }
          hasInPorts = T
        }
        else {
          if (invertTopicBinding) {
            publishers = publishers :+ genCppTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            publisherMethods = publisherMethods :+
              genCppTopicPublishMethod(p, nodeName, portDatatype, 1)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              val inputPortNames = getPortNames(inputPorts)
              publishers = publishers :+ genCppTopicPublisher(p, portDatatype, inputPortNames)
              publisherMethods = publisherMethods :+
                genCppTopicPublishMethod(p, nodeName, portDatatype, inputPortNames.size)
            }
            else {
              // Out ports with no connections should still publish to a topic
              publishers = publishers :+ genCppTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
              publisherMethods = publisherMethods :+
                genCppTopicPublishMethod(p, nodeName, portDatatype, 1)
            }
          }
        }
      }
    }

    var fileBody =
      st"""#include "${packageName}/base_headers/${nodeName}${cpp_src_node_header_name_suffix}"
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${nodeName}::${nodeName}() : Node(${genCppNodeCtorArgs(component)})
          |{
          |    ${genCppCallbackGroupVar()}"""

    if (hasInPorts) {
      fileBody =
        st"""${fileBody}
            |    ${subscription_options_name}.callback_group = ${callback_group_name};
          """
    }

    fileBody =
      st"""${fileBody}
          |    // Setting up connections
          |    ${(subscribers ++ publishers, "\n")}"""

    if (!isSporadic(component)) {
      if (strictAADLMode) {
        fileBody =
          st"""${fileBody}
              |    // timeTriggeredCaller callback timer
              |    ${genCppTimeTriggeredTimerStrict(nodeName, component)}
            """
      }
      else {
        fileBody =
          st"""${fileBody}
              |    // timeTriggered callback timer
              |    ${genCppTimeTriggeredTimer(nodeName, component)}
            """
      }
    }

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |    // Used by receiveInputs
            |    ${genCppInDataPortTupleVector(inPortNames)}"""

      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
              |    // Used by receiveInputs
              |    ${genCppInEventPortTupleVector(inPortNames)}"""
      }

      fileBody =
        st"""${fileBody}
            |    // Used by sendOutputs
            |    ${genCppOutPortTupleVector(nodeName, outPortNames)}"""
    }

    fileBody =
      st"""${fileBody}
          |}
        """

    if (dataPortInitializers.size > 0) {
      fileBody =
        st"""${fileBody}
            |${(dataPortInitializers, "\n\n")}
          """
    }

    if (subscriberMethods.size > 0 || publisherMethods.size > 0 || (strictAADLMode && subscribers.size > 0)) {
      fileBody =
        st"""${fileBody}
            |//=================================================
            |//  C o m m u n i c a t i o n
            |//=================================================
          """

      if (strictSubscriptionMessageAcceptorMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(strictSubscriptionMessageAcceptorMethods, "\n")}"""
      }

      if (subscriberMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(subscriberMethods, "\n")}"""
      }

      if (subscriptionMessageGetters.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(subscriptionMessageGetters, "\n")}"""
      }

      if (eventPortHandlers.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(eventPortHandlers, "\n")}"""
      }

      if (strictSubscriptionHandlerBaseMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(strictSubscriptionHandlerBaseMethods, "\n")}"""
      }

      if (publisherMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(publisherMethods, "\n")}
              |${(strictPutMsgMethods, "\n")}"""
      }
    }

    if (strictAADLMode) {
      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
              |${genCppTimeTriggeredCaller(nodeName)}"""
      }

      val receiveInputs: ST = if (isSporadic(component)) genCppReceiveInputsSporadic(nodeName)
                              else genCppReceiveInputsPeriodic(nodeName)

      fileBody =
        st"""${fileBody}
            |${receiveInputs}
            |${genCppEnqueue(nodeName)}
            |${genCppSendOutputs(nodeName)}"""
    }

    val filePath: ISZ[String] = IS("src", packageName, "src", "base_code", fileName)

    return (filePath, fileBody, T, IS())
  }

  def genCppUserNodeHeaderFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, Ros2Datatype],
                               strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- generatedPorts(component)) {
        val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
        if (p.direction == Direction.In && !p.isInstanceOf[AadlDataPort]) {
          if (strictAADLMode) {
            subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeaderStrict(p, portDatatype)
          }
          else {
            subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeader(p, portDatatype)
          }
        }
      }
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+ genCppTimeTriggeredMethodHeader()
    }

    var fileBody =
      st"""#include "${packageName}/base_headers/${nodeName}_base${cpp_src_node_header_name_suffix}"
          |
          |${CommentTemplate.invertedMarkerComment_slash}
          |
          |class ${nodeName} : public ${nodeName}_base
          |{
          |public:
          |    ${nodeName}();
          |
          |private:
          |    //=================================================
          |    //  I n i t i a l i z e    E n t r y    P o i n t
          |    //=================================================
          |    void initialize();
        """

    if (subscriptionHandlers.size > 0) {
      fileBody =
        st"""${fileBody}
            |    //=================================================
            |    //  C o m p u t e    E n t r y    P o i n t
            |    //=================================================
            |    ${(subscriptionHandlers, "\n")}
          """
    }

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "//",
      optBeginSuffix = None(),
      endPrefix = "//",
      optEndSuffix = None())

    fileBody =
      st"""${fileBody}
          |    //=================================================
          |    //  Include any additional declarations here
          |    //=================================================
          |    ${marker.beginMarker}
          |
          |    ${marker.endMarker}
          |};
          """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "user_headers", fileName)

    return (filePath, fileBody, T, IS(marker))
  }

  def genCppUserNodeCppFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, Ros2Datatype],
                            hasConverterFiles: B, strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeSourceName(nodeName)
    var examplePublishers: ISZ[ST] = IS()
    var inDataPorts: ISZ[AadlPort] = IS()

    for (p <- generatedPorts(component)) {
      if (p.direction == Direction.Out) {
        examplePublishers = examplePublishers :+ genCppExamplePublisher(p, packageName, datatypeMap, reporter)
      }
      else if (p.direction == Direction.In && p.isInstanceOf[AadlDataPort]) {
        inDataPorts = inDataPorts :+ p
      }
    }

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      var firstSubscriptionHandler: B = true

      for (p <- generatedPorts(component)) {
        val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
        if (p.direction == Direction.In && !p.isInstanceOf[AadlDataPort]) {
          if (strictAADLMode) {
            if (firstSubscriptionHandler) {
              subscriptionHandlers = subscriptionHandlers :+
                genCppSubscriptionHandlerSporadicStrictWithExamples(p, nodeName, portDatatype, inDataPorts,
                  packageName, datatypeMap, reporter)
              firstSubscriptionHandler = false
            } else {
              subscriptionHandlers = subscriptionHandlers :+
                genCppSubscriptionHandlerSporadicStrict(p, nodeName, portDatatype)
            }
          }
          else {
            if (firstSubscriptionHandler) {
              subscriptionHandlers = subscriptionHandlers :+
                genCppSubscriptionHandlerSporadicWithExamples(p, nodeName, portDatatype, inDataPorts,
                  packageName, datatypeMap, reporter)
              firstSubscriptionHandler = false
            } else {
              subscriptionHandlers = subscriptionHandlers :+
                genCppSubscriptionHandlerSporadic(p, nodeName, portDatatype)
            }
          }
        }
      }
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+
        genCppTimeTriggeredMethod(nodeName, inDataPorts, examplePublishers, packageName, datatypeMap, strictAADLMode, reporter)
    }

    var includeFiles: ST = st"#include \"${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}\""

    if (hasConverterFiles) {
      includeFiles =
        st"""${includeFiles}
            |#include "${packageName}/base_headers/enum_converter.hpp""""
    }

    val inDataPortInitializers: ISZ[ST] = genCppUserDataPortInitializers(inDataPorts, packageName, datatypeMap, reporter)

    var fileBody =
      st"""${includeFiles}
          |
          |${CommentTemplate.safeToEditComment_slash}
          |
          |//=================================================
          |//  I n i t i a l i z e    E n t r y    P o i n t
          |//=================================================
          |void ${nodeName}::initialize()
          |{
          |    PRINT_INFO("Initialize Entry Point invoked");
          |
          |    // Initialize the node"""

    if (inDataPortInitializers.size != 0) {
      fileBody =
        st"""${fileBody}
          |    // Initialize the node's incoming data port values here
          |    ${(inDataPortInitializers, "\n")}"""
    }

    fileBody =
      st"""${fileBody}
          |}
          |
          |//=================================================
          |//  C o m p u t e    E n t r y    P o i n t
          |//=================================================
          |${(subscriptionHandlers, "\n")}
        """

    val filePath: ISZ[String] = IS("src", packageName, "src", "user_code", fileName)

    return (filePath, fileBody, F, IS())
  }

  def genCppUserDataPortInitializers(inDataPorts: ISZ[AadlPort], packageName: String,
                                           datatypeMap: Map[AadlType, Ros2Datatype], reporter: Reporter): ISZ[ST] = {
    var initializers: ISZ[ST] = IS()

    for (p <- inDataPorts) {
      val portDatatype = genPortDatatype(p, packageName, datatypeMap, reporter)
      val portName = p.identifier
      val initExpr = portExampleInit(p, s"${portDatatype}()", datatypeMap)

      initializers = initializers :+
        st"""${portDatatype} ${portName} = ${initExpr};
            |init_${portName}(${portName});
          """
    }

    return initializers
  }

  def genCppNodeRunnerFile(packageName: String, component: AadlThread): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeRunnerName(nodeName)

    val fileBody =
      st"""#include "${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}"
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${nodeName}::${nodeName}() : ${nodeName}_base()
          |{
          |    // Invoke initialize entry point
          |    initialize();
          |
          |    PRINT_INFO("${nodeName} infrastructure set up");
          |}
          |
          |int main(int argc, char **argv)
          |{
          |    rclcpp::init(argc, argv);
          |    auto executor = rclcpp::executors::MultiThreadedExecutor();
          |    auto node = std::make_shared<${nodeName}>();
          |    executor.add_node(node);
          |    executor.spin();
          |    rclcpp::shutdown();
          |    return 0;
          |}
        """

    val filePath: ISZ[String] = IS("src", packageName, "src", "base_code", fileName)

    return (filePath, fileBody, T, IS())
  }

  def genCppNodeFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                      datatypeMap: Map[AadlType, Ros2Datatype], hasConverterFiles: B, strictAADLMode: B,
                      invertTopicBinding: B, reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genCppPackageName(modelName)

    var cpp_files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (comp <- threadComponents) {
      cpp_files =
        cpp_files :+ genCppBaseNodeHeaderFile(top_level_package_nameT, comp, connectionMap, datatypeMap, hasConverterFiles,
                                              strictAADLMode, invertTopicBinding, reporter)
      cpp_files =
        cpp_files :+ genCppBaseNodeCppFile(top_level_package_nameT, comp, connectionMap, datatypeMap, strictAADLMode,
                                           invertTopicBinding, reporter)
      cpp_files =
        cpp_files :+ genCppUserNodeHeaderFile(top_level_package_nameT, comp, datatypeMap, strictAADLMode, reporter)
      cpp_files =
        cpp_files :+ genCppUserNodeCppFile(top_level_package_nameT, comp, datatypeMap, hasConverterFiles, strictAADLMode, reporter)
      cpp_files =
        cpp_files :+ genCppNodeRunnerFile(top_level_package_nameT, comp)
    }

    return cpp_files
  }

  def genCppEnumConverterHeaderFile(packageName: String, enumTypes: ISZ[(String, AadlType)]): (ISZ[String], ST, B, ISZ[Marker]) = {
    var includes: ISZ[ST] = IS()
    var converterHeaders: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")

      includes = includes :+ st"#include \"${packageName}_interfaces/msg/${enum._1}.hpp\""

      converterHeaders = converterHeaders :+
        st"const char* enumToString(const ${packageName}_interfaces::msg::${enumName}& value);"
    }

    val fileBody =
      st"""#ifndef ENUM_CONVERTER_HPP
          |#define ENUM_CONVERTER_HPP
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |#include <string>
          |${(includes, "\n")}
          |
          |${(converterHeaders, "\n")}
          |
          |#endif
        """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "base_headers", "enum_converter.hpp")

    return (filePath, fileBody, T, IS())
  }

  def genCppEnumConverters(packageName: String, enumTypes: ISZ[(String, AadlType)]): ISZ[ST] = {
    var converters: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")
      val enumValues: ISZ[String] = enum._2.asInstanceOf[EnumType].values

      var cases: ISZ[ST] = IS()

      for (value <- enumValues) {
        cases = cases :+
          st"""case ${packageName}_interfaces::msg::${enumName}::${StringOps(enum._1).toUpper}_${StringOps(value).toUpper}:
              |    return "${enumName} ${value}";"""
      }

      converters = converters :+
        st"""const char* enumToString(const ${packageName}_interfaces::msg::${enumName}& value) {
            |    switch (value.${enum._1}) {
            |        ${(cases, "\n")}
            |        default:
            |            return "Unknown value for ${enumName}";
            |    }
            |}
        """
    }

    return converters
  }

  def genCppEnumConverterFile(packageName: String, enumTypes: ISZ[(String, AadlType)]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val fileBody =
      st"""#include "${packageName}/base_headers/enum_converter.hpp"
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(genCppEnumConverters(packageName, enumTypes), "\n")}
        """

    val filePath: ISZ[String] = IS("src", packageName, "src", "base_code", "enum_converter.cpp")

    return (filePath, fileBody, T, IS())
  }

  def genCppEnumConverterFiles(modelName: String, datatypeMap: Map[AadlType, Ros2Datatype],
                               strictAADLMode: B): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var enumTypes: ISZ[(String, AadlType)] = IS()

    for (key <- datatypeMap.keys if !datatypeMap.get(key).get.isPlatformProvided) {
      key match {
        case _: EnumType =>
          val datatype: String = datatypeMap.get(key).get.content.apply(0)
          val datatypeName: String = StringOps(datatype).substring(StringOps(datatype).indexOf(' ') + 1, datatype.size)
          enumTypes = enumTypes :+ (datatypeName, key)
        case x =>
      }
    }

    if (enumTypes.size == 0) {
      return IS()
    }

    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    val packageName: String = genCppPackageName(modelName)

    files = files :+ genCppEnumConverterHeaderFile(packageName, enumTypes)
    files = files :+ genCppEnumConverterFile(packageName, enumTypes)

    return files
  }


  //================================================
  //  P a c k a g e   G e n e r a t o r s
  //================================================

  def genPyNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                 strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    files = files :+ genPyFormatLaunchFile(modelName, threadComponents)
    files = files :+ genPySetupFile(modelName, threadComponents)

    return files
  }

  // KNOWN GAP: this emits the bringup package but no launch file, so selecting the Python launch
  // language yields a bringup package with nothing to launch -- and Python is the codegen CLI's
  // default (HamrCli's ros2LaunchLanguage), so that is what an unqualified invocation produces.
  // The XML path (genXmlFormatLaunchFiles) is the working one.
  //
  // Wiring genPyFormatLaunchFile in below is not sufficient on its own.  It is currently dead
  // code -- its only other caller, genPyNodePkg, is unreachable because Ros2Codegen handles only
  // ros2NodesLanguage == Cpp -- and it needs three fixes first:
  //   - its output path is src/<modelName>_bringup/... whereas the bringup package files above
  //     land in src/<cppPkgName>_bringup/..., so the launch file would not be inside its package;
  //   - micro-ROS threads are not passed in (Ros2Codegen calls this with ros2Threads only), and
  //     the XML version additionally emits the micro_ros_agent <executable> entry a mixed model
  //     needs to run at all;
  //   - Ros_Namespace is honored by genPyFormatLaunchNodeDecl but not by GeneratorPy's copy.
  // Ros2TestUtil no longer pins the launch language, so a test can select Python once this works.
  def genPyLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread],
                     reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files :+ genLaunchCMakeListsFile(modelName)
    // the bringup package declares its stock-node dependencies regardless of launch format
    files = files :+ genLaunchPackageFile(modelName, getNativeExecPackages(threadComponents, reporter))

    return files
  }

  def genCppNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                    datatypeMap: Map[AadlType, Ros2Datatype], strictAADLMode: B, invertTopicBinding: B,
                    reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = ISZ()

    val converterFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = genCppEnumConverterFiles(modelName, datatypeMap, strictAADLMode)
    val hasConverterFiles: B = (converterFiles.size > 0)

    files = files ++
      genCppNodeFiles(modelName, threadComponents, connectionMap, datatypeMap, hasConverterFiles, strictAADLMode,
                      invertTopicBinding, reporter)
    files = files ++ converterFiles
    files = files :+ genCppExampleTypesFile(modelName, datatypeMap)

    // the packages supplying the platform-provided payload types the nodes use
    val nativePackages: ISZ[String] = getNativePackages(threadComponents, datatypeMap)
    files = files :+ genCppCMakeListsFile(modelName, threadComponents, hasConverterFiles, nativePackages)
    files = files :+ genCppPackageFile(modelName, nativePackages)

    return files
  }

  // The distinct ROS packages supplying the platform-provided components among the given threads
  def getNativeExecPackages(threadComponents: ISZ[AadlThread], reporter: Reporter): ISZ[String] = {
    var packages: ISZ[String] = ISZ()
    for (thread <- threadComponents if RosUtil.isPlatformProvidedComponent(thread)) {
      RosUtil.getNativeExecutable(thread, reporter) match {
        case Some((nativePackage, _)) =>
          if (!ISZOps(packages).contains(nativePackage)) {
            packages = packages :+ nativePackage
          }
        case _ =>
      }
    }
    return packages
  }

  def genXmlLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread], systemComponents: ISZ[AadlSystem],
                     microRosThreads: ISZ[AadlThread], reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genXmlFormatLaunchFiles(modelName, threadComponents, systemComponents, microRosThreads, reporter)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName, getNativeExecPackages(threadComponents, reporter))

    return files
  }

  //================================================
  //  M i c r o R O S   C   H e l p e r s
  //================================================

  def genMicroRosPackageName(packageNameS: String): String = {
    return s"${packageNameS}${microros_package_name_suffix}"
  }

  // Convert C++ type "pkg_interfaces::msg::Foo" to C struct name "pkg_interfaces__msg__Foo"
  def cppTypeToCStructName(cppType: String): String = {
    return ops.StringOps(cppType).replaceAllLiterally("::", "__")
  }

  // Non-empty parts of splitting C++ type "pkg::msg::Foo" by ':'
  def cppTypeParts(cppType: String): ISZ[String] = {
    return ISZOps(ops.StringOps(cppType).split(c => c == ':')).filter(s => s.size > 0)
  }

  // "pkg_interfaces::msg::Foo" → "ROSIDL_GET_MSG_TYPE_SUPPORT(pkg_interfaces, msg, Foo)"
  def cppTypeToROSIDLSupport(cppType: String): String = {
    val parts = cppTypeParts(cppType)
    return s"ROSIDL_GET_MSG_TYPE_SUPPORT(${parts(0)}, ${parts(1)}, ${parts(2)})"
  }

  // "pkg_interfaces::msg::Foo" → "pkg_interfaces/msg/foo.h"  (using existing snake_case formatter)
  def cppTypeToCHeaderPath(cppType: String): String = {
    return s"${formatDatatypeForInclude(cppType)}.h"
  }

  // Generate C #include lines for a set of C++ type strings
  def genCHeaderFileMsgTypeIncludes(msgTypes: ISZ[String]): ISZ[ST] = {
    var includes: ISZ[ST] = IS()
    for (msgType <- msgTypes) {
      val path = cppTypeToCHeaderPath(msgType)
      includes = includes :+ st"""#include "${path}""""
    }
    return includes
  }

  // Derive the C put function parameter declaration for an out port
  // For data/eventdata ports: "CStructName * msg"  For event ports: no payload param
  def isEventPortType(portType: String): B = {
    return isEventPort(portType)
  }

  //================================================
  //  E x a m p l e   T y p e s
  //================================================

  // Returns "example_TypeName()" for RecordType ports, fallback otherwise.
  // fallback is e.g. "DataType()" for C++ or "{0}" for C.
  def portExampleInit(port: AadlPort, fallback: String,
      datatypeMap: Map[AadlType, Ros2Datatype]): String = {
    portAadlTypeOpt(port) match {
      case Some(aadlType) =>
        aadlType match {
          case _: RecordType =>
            datatypeMap.get(aadlType) match {
              // no example builder is generated for a platform-provided type: its fields are
              // mirrors rather than a layout claim, so codegen cannot construct one
              case Some(dtype) if !dtype.isPlatformProvided => return s"example_${dtype.name}()"
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
    return fallback
  }

  def cBaseTypeZeroLiteral(ros2TypeName: String): String = {
    if (ros2TypeName == "float32") {
      return "0.0f"
    } else if (ros2TypeName == "float64") {
      return "0.0"
    } else if (ros2TypeName == "bool") {
      return "false"
    } else {
      return "0"
    }
  }

  // Returns C field-init statements for a RecordType's fields, setting all to zero/first-enum-value.
  // accessPrefix ends with ".", e.g. "msg." or "msg.low."
  def genCExampleFieldInits(aadlType: AadlType, accessPrefix: String,
      cppPkgName: String, datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[ST] = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get.content
    val fieldLines: ISZ[String] = ISZOps(content).filter(line => !ops.StringOps(line).contains("="))
    var stmts: ISZ[ST] = IS()
    for (line <- fieldLines) {
      val parts = ops.StringOps(line).split(c => c == ' ')
      val fieldTypeName = parts(0)
      val fieldName = parts(1)
      val fieldAccess: String = st"${accessPrefix}${fieldName}".render
      lookupAadlTypeByRos2Name(fieldTypeName, datatypeMap) match {
        case Some(nestedType) =>
          nestedType match {
            case _: BaseType =>
              val ros2Type = ops.StringOps(datatypeMap.get(nestedType).get.content(0)).split(c => c == ' ')(0)
              if (ros2Type == "string") {
                // TODO: investigate replacing the default heap allocator with a static memory pool allocator
                //       (e.g. via rcutils_allocator_t / micro_ros_utilities) to avoid malloc on embedded targets
                stmts = stmts :+ st"""rosidl_runtime_c__String__assign(&${fieldAccess}.data, "");"""
              } else {
                stmts = stmts :+ st"${fieldAccess}.data = ${cBaseTypeZeroLiteral(ros2Type)};"
              }
            case et: EnumType =>
              val enumContent = datatypeMap.get(nestedType).get.content
              val enumFieldLine = ISZOps(enumContent).filter(l => !ops.StringOps(l).contains("="))(0)
              val enumFieldName = ops.StringOps(enumFieldLine).split(c => c == ' ')(1)
              val firstConstLine = ISZOps(enumContent).filter(l => ops.StringOps(l).contains("="))(0)
              val firstConst = ops.StringOps(ops.StringOps(firstConstLine).split(c => c == ' ')(1)).split(c => c == '=')(0)
              val enumCStructName = cppTypeToCStructName(st"${cppPkgName}_interfaces::msg::${et.simpleName}".render)
              stmts = stmts :+ st"${fieldAccess}.${enumFieldName} = ${enumCStructName}__${firstConst};"
            case _: RecordType =>
              val simpleTypeName = datatypeMap.get(nestedType).get.name
              stmts = stmts :+ st"${fieldAccess} = example_${simpleTypeName}();"
            case _ =>
          }
        case _ =>
      }
    }
    return stmts
  }

  // Returns C++ field-init statements for a RecordType's fields.
  def genCppExampleFieldInits(aadlType: AadlType, accessPrefix: String,
      cppPkgName: String, datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[ST] = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get.content
    val fieldLines: ISZ[String] = ISZOps(content).filter(line => !ops.StringOps(line).contains("="))
    var stmts: ISZ[ST] = IS()
    for (line <- fieldLines) {
      val parts = ops.StringOps(line).split(c => c == ' ')
      val fieldTypeName = parts(0)
      val fieldName = parts(1)
      val fieldAccess: String = st"${accessPrefix}${fieldName}".render
      lookupAadlTypeByRos2Name(fieldTypeName, datatypeMap) match {
        case Some(nestedType) =>
          nestedType match {
            case _: BaseType =>
              val ros2Type = ops.StringOps(datatypeMap.get(nestedType).get.content(0)).split(c => c == ' ')(0)
              if (ros2Type == "string") {
                stmts = stmts :+ st"""${fieldAccess}.data = "";"""
              } else {
                stmts = stmts :+ st"${fieldAccess}.data = ${cBaseTypeZeroLiteral(ros2Type)};"
              }
            case et: EnumType =>
              val enumContent = datatypeMap.get(nestedType).get.content
              val enumFieldLine = ISZOps(enumContent).filter(l => !ops.StringOps(l).contains("="))(0)
              val enumFieldName = ops.StringOps(enumFieldLine).split(c => c == ' ')(1)
              val firstConstLine = ISZOps(enumContent).filter(l => ops.StringOps(l).contains("="))(0)
              val firstConst = ops.StringOps(ops.StringOps(firstConstLine).split(c => c == ' ')(1)).split(c => c == '=')(0)
              val enumCppType = st"${cppPkgName}_interfaces::msg::${et.simpleName}".render
              stmts = stmts :+ st"${fieldAccess}.${enumFieldName} = ${enumCppType}::${firstConst};"
            case _: RecordType =>
              val simpleTypeName = datatypeMap.get(nestedType).get.name
              stmts = stmts :+ st"${fieldAccess} = example_${simpleTypeName}();"
            case _ =>
          }
        case _ =>
      }
    }
    return stmts
  }

  def genMicroRosExampleTypesFile(modelName: String, cppPkgName: String,
      datatypeMap: Map[AadlType, Ros2Datatype]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName = genMicroRosPackageName(modelName)
    val interfacesPkg = st"${cppPkgName}_interfaces".render

    var includes: ISZ[ST] = IS()
    var forwardDecls: ISZ[ST] = IS()
    var definitions: ISZ[ST] = IS()

    // platform-provided types are skipped: their fields are specification-level mirrors,
    // so codegen cannot construct an example value of the native type
    for (key <- datatypeMap.keys if !datatypeMap.get(key).get.isPlatformProvided) {
      key match {
        case _: RecordType =>
          val typeName = datatypeMap.get(key).get.name
          val cppType = st"${interfacesPkg}::msg::${typeName}".render
          val cStructName = cppTypeToCStructName(cppType)
          includes = includes :+ st"#include \"${cppTypeToCHeaderPath(cppType)}\""
          forwardDecls = forwardDecls :+ st"static inline ${cStructName} example_${typeName}(void);"
          val fieldInits = genCExampleFieldInits(key, "msg.", cppPkgName, datatypeMap)
          val bodyLines: ISZ[ST] = ISZ(st"${cStructName} msg = {0};") ++ fieldInits :+ st"return msg;"
          definitions = definitions :+
            st"""static inline ${cStructName} example_${typeName}(void) {
                |    ${(bodyLines, "\n")}
                |}"""
        case _ =>
      }
    }

    val stringFunctionsInclude: ST =
      if (includes.nonEmpty) st"#include <rosidl_runtime_c/string_functions.h>" else st""

    val fileBody =
      st"""#ifndef EXAMPLE_TYPES_H
          |#define EXAMPLE_TYPES_H
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(includes, "\n")}
          |${stringFunctionsInclude}
          |
          |${(forwardDecls, "\n")}
          |
          |${(definitions, "\n\n")}
          |
          |#endif  // EXAMPLE_TYPES_H
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "include", microrosPkgName, "base_headers", "example_types.h")
    return (filePath, fileBody, T, IS())
  }

  def genCppExampleTypesFile(modelName: String, datatypeMap: Map[AadlType, Ros2Datatype]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val packageName = genCppPackageName(modelName)
    val interfacesPkg = st"${packageName}_interfaces".render

    var includes: ISZ[ST] = IS()
    var forwardDecls: ISZ[ST] = IS()
    var definitions: ISZ[ST] = IS()

    // platform-provided types are skipped: their fields are specification-level mirrors,
    // so codegen cannot construct an example value of the native type
    for (key <- datatypeMap.keys if !datatypeMap.get(key).get.isPlatformProvided) {
      key match {
        case _: RecordType =>
          val typeName = datatypeMap.get(key).get.name
          val cppType = st"${interfacesPkg}::msg::${typeName}".render
          includes = includes :+ st"#include \"${formatDatatypeForInclude(cppType)}.hpp\""
          forwardDecls = forwardDecls :+ st"static inline ${cppType} example_${typeName}();"
          val fieldInits = genCppExampleFieldInits(key, "msg.", packageName, datatypeMap)
          val bodyLines: ISZ[ST] = ISZ(st"${cppType} msg;") ++ fieldInits :+ st"return msg;"
          definitions = definitions :+
            st"""static inline ${cppType} example_${typeName}() {
                |    ${(bodyLines, "\n")}
                |}"""
        case _ =>
      }
    }

    val fileBody =
      st"""#ifndef EXAMPLE_TYPES_HPP
          |#define EXAMPLE_TYPES_HPP
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(includes, "\n")}
          |
          |${(forwardDecls, "\n")}
          |
          |${(definitions, "\n\n")}
          |
          |#endif  // EXAMPLE_TYPES_HPP
        """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "base_headers", "example_types.hpp")
    return (filePath, fileBody, T, IS())
  }

  //================================================
  //  C p p   M e s s a g e   T o   S t r i n g
  //================================================

  // Returns an oss << chain fragment for the given AadlType at the given access expression.
  // For BaseType: "accessExpr.data"
  // For EnumType: "enumToString(accessExpr)"
  // For RecordType: "\"TypeName{field1: \" << val1 << \", field2: \" << val2 << \"}\""
  def genCppOssChain(aadlType: AadlType, accessExpr: String, simpleTypeName: String,
      datatypeMap: Map[AadlType, Ros2Datatype], hasEnumConverter: B): ST = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get.content
    val fieldLines: ISZ[String] = ISZOps(content).filter(line => !ops.StringOps(line).contains("="))
    val r: ST = aadlType match {
      case _: BaseType =>
        st"${accessExpr}.data"
      case _: EnumType =>
        if (hasEnumConverter) {
          st"enumToString(${accessExpr})"
        } else {
          val fieldName = ops.StringOps(fieldLines(0)).split(c => c == ' ')(1)
          st"static_cast<int>(${accessExpr}.${fieldName})"
        }
      case _: RecordType =>
        var parts: ISZ[ST] = IS()
        var isFirst: B = T
        for (line <- fieldLines) {
          val lineParts = ops.StringOps(line).split(c => c == ' ')
          val fieldTypeName = lineParts(0)
          val fieldName = lineParts(1)
          val labelStr: String = if (isFirst) st"${simpleTypeName}{${fieldName}: ".render else st", ${fieldName}: ".render
          isFirst = F
          val nestedAccess: String = st"${accessExpr}.${fieldName}".render
          lookupAadlTypeByRos2Name(fieldTypeName, datatypeMap) match {
            case Some(nestedType) =>
              val innerChain = genCppOssChain(nestedType, nestedAccess, fieldTypeName, datatypeMap, hasEnumConverter)
              parts = parts :+ st""""${labelStr}" << ${innerChain}"""
            case _ =>
              parts = parts :+ st""""${labelStr}" << ${nestedAccess}"""
          }
        }
        st"""${(parts, " << ")} << "}""""
      case _ =>
        st""""unknown""""
    }
    return r
  }

  // Generate inline _messageToString overloads for all data port types of the component.
  // Returns None if there are no data ports.
  def genCppMsgToStringBlock(component: AadlThread, packageName: String,
      datatypeMap: Map[AadlType, Ros2Datatype], hasEnumConverter: B,
      reporter: Reporter): Option[ST] = {
    var seen: ISZ[String] = IS()
    var helpers: ISZ[ST] = IS()
    for (p <- generatedPorts(component)) {
      val portDatatype = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!isEventPort(portDatatype)) {
        if (!ISZOps(seen).contains(portDatatype)) {
          seen = seen :+ portDatatype
          val parts = cppTypeParts(portDatatype)
          val simpleTypeName = parts(parts.size - 1)
          portAadlTypeOpt(p) match {
            // no printer is generated for a platform-provided payload: the model's mirror
            // fields (if any) are projections, not the native type's layout
            case Some(rawType) if !RosUtil.isPlatformProvided(rawType) =>
              var aadlType = rawType
              for (key <- datatypeMap.keys) {
                if (key.name == rawType.name) {
                  aadlType = key
                }
              }
              val ossChain = genCppOssChain(aadlType, "msg", simpleTypeName, datatypeMap, hasEnumConverter)
              helpers = helpers :+
                st"""static inline std::string _messageToString(const ${portDatatype}& msg) {
                    |    std::ostringstream oss;
                    |    oss << ${ossChain};
                    |    return oss.str();
                    |}"""
            case _ =>
          }
        }
      }
    }
    if (helpers.isEmpty) {
      return None()
    }
    return Some(st"${(helpers, "\n\n")}")
  }

  //================================================
  //  M i c r o R O S   M e s s a g e   T o   S t r i n g
  //================================================

  def ros2BaseTypeFmt(ros2TypeName: String): String = {
    if (ros2TypeName == "float32") {
      return "%f"
    } else if (ros2TypeName == "float64") {
      return "%lf"
    } else if (ros2TypeName == "int8" || ros2TypeName == "int16" || ros2TypeName == "int32" || ros2TypeName == "bool") {
      return "%d"
    } else if (ros2TypeName == "int64") {
      return "%ld"
    } else if (ros2TypeName == "uint8" || ros2TypeName == "uint16" || ros2TypeName == "uint32") {
      return "%u"
    } else if (ros2TypeName == "uint64") {
      return "%lu"
    } else if (ros2TypeName == "char") {
      return "%c"
    } else if (ros2TypeName == "string") {
      return "%s"
    } else {
      return "%p"
    }
  }

  def ros2BaseTypeCast(ros2TypeName: String): String = {
    if (ros2TypeName == "bool") {
      return "(int)"
    } else {
      return ""
    }
  }

  def lookupAadlTypeByRos2Name(name: String, datatypeMap: Map[AadlType, Ros2Datatype]): Option[AadlType] = {
    for (entry <- datatypeMap.entries) {
      if (entry._2.name == name) {
        return Some(entry._1)
      }
    }
    return None()
  }

  // Returns (printf format pattern, snprintf args) for the given AadlType.
  // accessPath ends with "->" (top-level pointer) or "." (nested struct value), e.g. "msg->" or "msg->degrees."
  def genCMsgFmtArgs(aadlType: AadlType, accessPath: String,
      datatypeMap: Map[AadlType, Ros2Datatype], hasEnumConverter: B): (String, ISZ[String]) = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get.content
    val fieldLines: ISZ[String] = ISZOps(content).filter(line => !ops.StringOps(line).contains("="))
    val r: (String, ISZ[String]) = aadlType match {
      case _: BaseType =>
        val ros2Type = ops.StringOps(fieldLines(0)).split(c => c == ' ')(0)
        val fmt = ros2BaseTypeFmt(ros2Type)
        val cast = ros2BaseTypeCast(ros2Type)
        if (ros2Type == "string") {
          (fmt, ISZ(st"${accessPath}data".render))
        } else {
          (fmt, ISZ(st"${cast}${accessPath}data".render))
        }
      case et: EnumType =>
        val fieldName = ops.StringOps(fieldLines(0)).split(c => c == ' ')(1)
        if (hasEnumConverter) {
          ("%s", ISZ(st"enumToString_${et.simpleName}(${accessPath}${fieldName})".render))
        } else {
          ("%d", ISZ(st"(int)(${accessPath}${fieldName})".render))
        }
      case _: RecordType =>
        var fmtParts: ISZ[String] = IS()
        var args: ISZ[String] = IS()
        for (line <- fieldLines) {
          val lineParts = ops.StringOps(line).split(c => c == ' ')
          val ros2TypeName = lineParts(0)
          val fieldName = lineParts(1)
          lookupAadlTypeByRos2Name(ros2TypeName, datatypeMap) match {
            case Some(nestedType) =>
              val subAccessPath = st"${accessPath}${fieldName}.".render
              val (subFmt, subArgs) = genCMsgFmtArgs(nestedType, subAccessPath, datatypeMap, hasEnumConverter)
              fmtParts = fmtParts :+ st"${fieldName}: ${subFmt}".render
              args = args ++ subArgs
            case _ =>
              fmtParts = fmtParts :+ st"${fieldName}: %p".render
              args = args :+ st"(void*)&${accessPath}${fieldName}".render
          }
        }
        (st"${(fmtParts, ", ")}".render, args)
      case _ =>
        ("%p", ISZ(st"(void*)${accessPath}".render))
    }
    return r
  }

  // Generate a MESSAGE_TO_STRING macro block for the given out data ports.
  // Returns None if there are no data out ports.
  def genCMsgToStringBlock(outPorts: ISZ[AadlPort], cppPkgName: String,
      datatypeMap: Map[AadlType, Ros2Datatype], hasEnumConverter: B,
      reporter: Reporter): Option[ST] = {
    var seen: ISZ[String] = IS()
    var helpers: ISZ[ST] = IS()
    var caseLines: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      if (!isEventPort(portDatatype)) {
        val cStructName = cppTypeToCStructName(portDatatype)
        if (!ISZOps(seen).contains(cStructName)) {
          seen = seen :+ cStructName
          val parts = cppTypeParts(portDatatype)
          val simpleTypeName = parts(parts.size - 1)
          val helperName = st"_MESSAGE_TO_STRING_${simpleTypeName}".render
          portAadlTypeOpt(p) match {
            // no printer is generated for a platform-provided payload: the model's mirror
            // fields (if any) are projections, not the native type's layout
            case Some(rawType) if !RosUtil.isPlatformProvided(rawType) =>
              var aadlType = rawType
              for (key <- datatypeMap.keys) {
                if (key.name == rawType.name) {
                  aadlType = key
                }
              }
              val (fmt, args) = genCMsgFmtArgs(aadlType, "msg->", datatypeMap, hasEnumConverter)
              val argsSection: ST = if (args.nonEmpty) st",\n        ${(args, ",\n        ")}" else st""
              helpers = helpers :+
                st"""static inline const char* ${helperName}(
                    |        const ${cStructName}* msg, char* _buf, int _buf_size) {
                    |    snprintf(_buf, _buf_size, "${simpleTypeName}{${fmt}}"${argsSection});
                    |    return _buf;
                    |}"""
              caseLines = caseLines :+
                st"    ${cStructName}*: ${helperName}((msg), _MESSAGE_TO_STRING_buf, sizeof(_MESSAGE_TO_STRING_buf)), \\"
            case _ =>
          }
        }
      }
    }
    if (helpers.isEmpty) {
      return None()
    }
    val macroLines: ISZ[ST] =
      ISZ(st"#define MESSAGE_TO_STRING(msg) _Generic((msg), \\") ++
      caseLines :+
      st"""    default: "(unknown type)")"""
    val block: ST =
      st"""${(helpers, "\n\n")}
          |
          |static char _MESSAGE_TO_STRING_buf[512];
          |${(macroLines, "\n")}"""
    return Some(block)
  }

  //================================================
  //  M i c r o R O S   B a s e   H e a d e r
  //================================================

  def genMicroRosPublisherStructFields(outPorts: ISZ[AadlPort], cppPkgName: String,
                                      datatypeMap: Map[AadlType, Ros2Datatype],
                                      connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                      invertTopicBinding: B, reporter: Reporter): ISZ[ST] = {
    var fields: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val numPubs: Z = if (invertTopicBinding) 1 else if (connectionMap.get(p.path).nonEmpty) connectionMap.get(p.path).get.size else 1
      if (numPubs == 1) {
        fields = fields :+ st"rcl_publisher_t ${portName}_publisher;"
      } else {
        var i: Z = 1
        while (i <= numPubs) {
          fields = fields :+ st"rcl_publisher_t ${portName}_publisher_${i};"
          i = i + 1
        }
      }
    }
    return fields
  }

  def genMicroRosPutFunctionDecls(outPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                  datatypeMap: Map[AadlType, Ros2Datatype],
                                  reporter: Reporter): ISZ[ST] = {
    var decls: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portId = p.identifier
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      if (isEventPort(portDatatype)) {
        decls = decls :+ st"void put_${portId}(${nodeName}_base_t * self);"
      } else {
        val cType = cppTypeToCStructName(portDatatype)
        decls = decls :+ st"void put_${portId}(${nodeName}_base_t * self, ${cType} * msg);"
      }
    }
    return decls
  }

  def genMicroRosSubscriberStructFields(inPorts: ISZ[AadlPort], cppPkgName: String,
                                        datatypeMap: Map[AadlType, Ros2Datatype],
                                        reporter: Reporter): ISZ[ST] = {
    var fields: ISZ[ST] = IS()
    for (p <- inPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val cType = cppTypeToCStructName(portDatatype)
      fields = fields :+ st"rcl_subscription_t ${portName}_subscription;"
      fields = fields :+ st"${cType} ${portName}_msg;"
    }
    return fields
  }

  def genMicroRosSubscriptionInits(inPorts: ISZ[AadlPort], cppPkgName: String,
                                    datatypeMap: Map[AadlType, Ros2Datatype],
                                    invertTopicBinding: B,
                                    connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                    reporter: Reporter): ISZ[ST] = {
    var inits: ISZ[ST] = IS()
    for (p <- inPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val rosidlSupport = cppTypeToROSIDLSupport(portDatatype)
      val derivedTopicName: String =
        if (invertTopicBinding && connectionMap.get(p.path).nonEmpty)
          getPortNames(connectionMap.get(p.path).get)(0)
        else
          getPortNames(IS(p.path.toISZ))(0)
      val topicName: String = subscriptionTopicNames(p, ISZ(derivedTopicName))(0)
      inits = inits :+
        st"""RCL_CHECK(rclc_subscription_init_default(
            |    &self->${portName}_subscription,
            |    &self->node,
            |    ${rosidlSupport},
            |    "${topicName}"));
          """
    }
    return inits
  }

  def genMicroRosHandleForwardDecls(inPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                     datatypeMap: Map[AadlType, Ros2Datatype],
                                     reporter: Reporter): ISZ[ST] = {
    val nodeNameBase = s"${nodeName}_base"
    var decls: ISZ[ST] = IS()
    for (p <- inPorts) {
      if (!p.isInstanceOf[AadlDataPort]) {
        val portId = p.identifier
        val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
        val cType = cppTypeToCStructName(portDatatype)
        if (isEventPort(portDatatype)) {
          decls = decls :+ st"void ${nodeName}_handle_${portId}(${nodeNameBase}_t * self);"
        } else {
          decls = decls :+ st"void ${nodeName}_handle_${portId}(${nodeNameBase}_t * self, const ${cType} * msg);"
        }
      }
    }
    return decls
  }

  def genMicroRosSubscriptionCallbacks(inPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                        datatypeMap: Map[AadlType, Ros2Datatype],
                                        reporter: Reporter): ISZ[ST] = {
    var callbacks: ISZ[ST] = IS()
    for (p <- inPorts) {
      val portName = genPortName(p)
      val portId = p.identifier
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val cType = cppTypeToCStructName(portDatatype)
      if (p.isInstanceOf[AadlDataPort]) {
        callbacks = callbacks :+
          st"""static void ${portName}_subscription_callback(const void * msgin)
              |{
              |    const ${cType} * msg = (const ${cType} *) msgin;
              |    if (g_self != NULL) {
              |        g_self->${portName}_msg = *msg;
              |    }
              |}
            """
      } else if (isEventPort(portDatatype)) {
        callbacks = callbacks :+
          st"""static void ${portName}_subscription_callback(const void * msgin)
              |{
              |    (void)msgin;
              |    if (g_self != NULL) {
              |        ${nodeName}_handle_${portId}(g_self);
              |    }
              |}
            """
      } else {
        callbacks = callbacks :+
          st"""static void ${portName}_subscription_callback(const void * msgin)
              |{
              |    const ${cType} * msg = (const ${cType} *) msgin;
              |    if (g_self != NULL) {
              |        ${nodeName}_handle_${portId}(g_self, msg);
              |    }
              |}
            """
      }
    }
    return callbacks
  }

  def genMicroRosSubscriptionExecutorAdds(inPorts: ISZ[AadlPort]): ISZ[ST] = {
    var adds: ISZ[ST] = IS()
    for (p <- inPorts) {
      val portName = genPortName(p)
      adds = adds :+
        st"RCL_CHECK(rclc_executor_add_subscription(&self->executor, &self->${portName}_subscription, &self->${portName}_msg, ${portName}_subscription_callback, ON_NEW_DATA));"
    }
    return adds
  }

  def genMicroRosGetFunctionDecls(dataInPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                   datatypeMap: Map[AadlType, Ros2Datatype],
                                   reporter: Reporter): ISZ[ST] = {
    val nodeNameBase = s"${nodeName}_base"
    var decls: ISZ[ST] = IS()
    for (p <- dataInPorts) {
      val portId = p.identifier
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val cType = cppTypeToCStructName(portDatatype)
      decls = decls :+ st"${cType} * get_${portId}(${nodeNameBase}_t * self);"
    }
    return decls
  }

  def genMicroRosGetFunctionImpls(dataInPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                   datatypeMap: Map[AadlType, Ros2Datatype],
                                   reporter: Reporter): ISZ[ST] = {
    val nodeNameBase = s"${nodeName}_base"
    var impls: ISZ[ST] = IS()
    for (p <- dataInPorts) {
      val portId = p.identifier
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val cType = cppTypeToCStructName(portDatatype)
      impls = impls :+
        st"""${cType} * get_${portId}(${nodeNameBase}_t * self)
            |{
            |    return &self->${portName}_msg;
            |}
          """
    }
    return impls
  }

  def genMicroRosBaseNodeHeaderFile(microrosPkgName: String, cppPkgName: String, component: AadlThread,
                                    connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                    datatypeMap: Map[AadlType, Ros2Datatype],
                                    hasEnumConverter: B, invertTopicBinding: B,
                                    reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeNameBase}${c_src_node_header_name_suffix}"
    val guardName = ops.StringOps(s"${nodeNameBase}_h").toUpper

    val outPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.Out)
    val inPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.In)
    val dataInPorts = ISZOps(inPorts).filter(p => p.isInstanceOf[AadlDataPort])

    var msgTypes: ISZ[String] = IS()
    for (p <- generatedPorts(component)) {
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      if (!ISZOps(msgTypes).contains(portDatatype)) {
        msgTypes = msgTypes :+ portDatatype
      }
    }

    val msgIncludes = genCHeaderFileMsgTypeIncludes(msgTypes)
    val publisherFields = genMicroRosPublisherStructFields(outPorts, cppPkgName, datatypeMap, connectionMap, invertTopicBinding, reporter)
    val putDecls = genMicroRosPutFunctionDecls(outPorts, nodeName, cppPkgName, datatypeMap, reporter)
    val enumConverterInclude: ST = if (hasEnumConverter) st"""#include "${microrosPkgName}/base_headers/enum_converter.h"""" else st""
    val exampleTypesInclude: ST = st"""#include "${microrosPkgName}/base_headers/example_types.h""""
    val msgToStringBlockOpt = genCMsgToStringBlock(outPorts, cppPkgName, datatypeMap, hasEnumConverter, reporter)
    val msgToStringSection: ST = msgToStringBlockOpt match {
      case Some(block) => st"\n${block}\n"
      case _ => st""
    }

    val callbackAndTimerSection: ST =
      if (isSporadic(component)) {
        val subscriberFields = genMicroRosSubscriberStructFields(inPorts, cppPkgName, datatypeMap, reporter)
        st"""    //=================================================
            |    //  S u b s c r i p t i o n s
            |    //=================================================
            |    ${(subscriberFields, "\n")}
            |
            |    //=================================================
            |    //  E x e c u t o r
            |    //=================================================
            |    rclc_executor_t executor;"""
      } else {
        st"""    //=================================================
            |    //  C a l l b a c k   a n d   T i m e r
            |    //=================================================
            |    rcl_timer_t period_timer;
            |    rclc_executor_t executor;"""
      }

    val getDecls = genMicroRosGetFunctionDecls(dataInPorts, nodeName, cppPkgName, datatypeMap, reporter)
    val getSection: ST =
      if (isSporadic(component) && getDecls.nonEmpty)
        st"""
            |//=================================================
            |//  D a t a   P o r t   A c c e s s
            |//=================================================
            |
            |${(getDecls, "\n")}
            |"""
      else st""

    val fileBody =
      st"""#ifndef ${guardName}
          |#define ${guardName}
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |#include <stdio.h>
          |#include <rcl/rcl.h>
          |#include <rclc/rclc.h>
          |#include <rclc/executor.h>
          |#include <rcutils/logging_macros.h>
          |${(msgIncludes, "\n")}
          |${enumConverterInclude}
          |${exampleTypesInclude}
          |
          |// Logger name used by the PRINT_* macros.  It defaults to the node name and is
          |// updated to the node's actual logger name (rcl_node_get_logger_name) during
          |// ${nodeNameBase}_init.
          |extern const char * ${nodeName}_logger_name;
          |
          |#define PRINT_INFO(fmt, ...) RCUTILS_LOG_INFO_NAMED(${nodeName}_logger_name, fmt, ##__VA_ARGS__)
          |#define PRINT_WARN(fmt, ...) RCUTILS_LOG_WARN_NAMED(${nodeName}_logger_name, fmt, ##__VA_ARGS__)
          |#define PRINT_ERROR(fmt, ...) RCUTILS_LOG_ERROR_NAMED(${nodeName}_logger_name, fmt, ##__VA_ARGS__)
          |
          |// rcl/rclc report entity-creation failures by return code rather than by trapping,
          |// and on an MCU the usual causes -- an exhausted RMW_UXRCE_MAX_* pool, an
          |// unreachable agent -- are exactly the ones worth seeing.  Running on past one
          |// leaves a node that spins normally but silently never publishes or receives, so
          |// ${nodeNameBase}_init stops at the first failure and hands the status back.
          |// Expands to a return, so it is usable only in a function returning rcl_ret_t.
          |#define RCL_CHECK(fn) do { rcl_ret_t rc_ = (fn); if (rc_ != RCL_RET_OK) { PRINT_ERROR("rcl call failed at %s:%d with status %d", __FILE__, __LINE__, (int) rc_); return rc_; } } while (0)
          |${msgToStringSection}
          |
          |//=================================================
          |//  N o d e   S t a t e
          |//=================================================
          |
          |typedef struct {
          |    rcl_node_t node;
          |    rclc_support_t support;
          |    rcl_allocator_t allocator;
          |
          |    //=================================================
          |    //  C o m m u n i c a t i o n
          |    //=================================================
          |    ${(publisherFields, "\n")}
          |
          |${callbackAndTimerSection}
          |} ${nodeNameBase}_t;
          |
          |// Returns RCL_RET_OK, or the status of the first rcl/rclc call that failed.
          |rcl_ret_t ${nodeNameBase}_init(${nodeNameBase}_t * self);
          |void ${nodeNameBase}_spin(${nodeNameBase}_t * self);
          |
          |//=================================================
          |//  C o m m u n i c a t i o n
          |//=================================================
          |
          |${(putDecls, "\n")}
          |${getSection}
          |#endif  // ${guardName}
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "include", microrosPkgName, "base_headers", fileName)
    return (filePath, fileBody, T, IS())
  }

  //================================================
  //  M i c r o R O S   B a s e   S o u r c e
  //================================================

  def genMicroRosPublisherInits(outPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                datatypeMap: Map[AadlType, Ros2Datatype],
                                connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                invertTopicBinding: B, reporter: Reporter): ISZ[ST] = {
    var inits: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val rosidlSupport = cppTypeToROSIDLSupport(portDatatype)

      val derivedTopicNames: ISZ[String] = if (invertTopicBinding) getPortNames(IS(p.path.toISZ)) else if (connectionMap.get(p.path).nonEmpty) getPortNames(connectionMap.get(p.path).get) else getPortNames(IS(p.path.toISZ))
      val topicNames: ISZ[String] = publisherTopicNames(p, derivedTopicNames)

      if (topicNames.size == 1) {
        inits = inits :+
          st"""RCL_CHECK(rclc_publisher_init_default(
              |    &self->${portName}_publisher,
              |    &self->node,
              |    ${rosidlSupport},
              |    "${topicNames(0)}"));
            """
      } else {
        var i: Z = 1
        while (i <= topicNames.size) {
          val topic = topicNames(i - 1)
          inits = inits :+
            st"""RCL_CHECK(rclc_publisher_init_default(
                |    &self->${portName}_publisher_${i},
                |    &self->node,
                |    ${rosidlSupport},
                |    "${topic}"));
              """
          i = i + 1
        }
      }
    }
    return inits
  }

  def genMicroRosPutFunctionImpls(outPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                  datatypeMap: Map[AadlType, Ros2Datatype],
                                  connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                  invertTopicBinding: B, reporter: Reporter): ISZ[ST] = {
    var impls: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portId = p.identifier
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)

      val numPubs: Z = if (invertTopicBinding) 1 else if (connectionMap.get(p.path).nonEmpty) connectionMap.get(p.path).get.size else 1
      val cType = cppTypeToCStructName(portDatatype)

      val msgArg: String = if (isEventPort(portDatatype)) "&msg" else "msg"

      var publishStmts: ISZ[ST] = IS()
      if (numPubs == 1) {
        publishStmts = IS(st"""rcl_ret_t ret = rcl_publish(&self->${portName}_publisher, ${msgArg}, NULL);
                              |if (ret != RCL_RET_OK) {
                              |    PRINT_ERROR("Failed to publish ${portId}");
                              |}""")
      } else {
        var i: Z = 1
        while (i <= numPubs) {
          publishStmts = publishStmts :+
            st"""rcl_ret_t ret${i} = rcl_publish(&self->${portName}_publisher_${i}, ${msgArg}, NULL);
                |if (ret${i} != RCL_RET_OK) {
                |    PRINT_ERROR("Failed to publish ${portId} (${i})");
                |}"""
          i = i + 1
        }
      }

      if (isEventPort(portDatatype)) {
        impls = impls :+
          st"""void put_${portId}(${nodeName}_base_t * self)
              |{
              |    ${cType} msg;
              |    ${cType}__init(&msg);
              |    ${(publishStmts, "\n")}
              |}
            """
      } else {
        impls = impls :+
          st"""void put_${portId}(${nodeName}_base_t * self, ${cType} * msg)
              |{
              |    ${(publishStmts, "\n")}
              |}
            """
      }
    }
    return impls
  }

  // The user-editable block holding this node's rcl arguments.  Its contents are preserved
  // across regeneration, so it is the deployment-time seam for remap rules, domain id,
  // enclave, log levels, etc.  Embedded micro-ROS targets have no command line, so this
  // array -- rather than argc/argv -- is how arguments reach rcl, identically on host-Linux
  // and on an MCU.
  val nodeOptionsMarker: BlockMarker = BlockMarker(
    id = "NODE OPTIONS - additions within these tags will be preserved when re-running Codegen",
    beginPrefix = "//",
    optBeginSuffix = None(),
    endPrefix = "//",
    optEndSuffix = None())

  // File-scope escape hatch for storage codegen cannot derive from the model.  Kept separate
  // from the node_options block so that block stays purely about rcl arguments -- the codegen
  // report treats an edited node_options as "this node carries remap rules the model-level
  // consistency checks cannot see", which stops being a meaningful signal if unrelated globals
  // live there too.
  val userDeclarationsMarker: BlockMarker = BlockMarker(
    id = "USER DECLARATIONS - additions within these tags will be preserved when re-running Codegen",
    beginPrefix = "//",
    optBeginSuffix = None(),
    endPrefix = "//",
    optEndSuffix = None())

  // Init-time counterpart, emitted after any model-derived buffer attachments so a user rule
  // overrides rather than races the generated one, and before the executor is initialized so
  // storage is in place before a message can be delivered.
  val userInitMarker: BlockMarker = BlockMarker(
    id = "USER INIT - additions within these tags will be preserved when re-running Codegen",
    beginPrefix = "//",
    optBeginSuffix = None(),
    endPrefix = "//",
    optEndSuffix = None())

  // Both are emitted unconditionally.  The generated buffer section renders to nothing when a
  // payload has no bounded mirror fields, which is precisely the case -- an opaque mirror, as in
  // the structure and naming mockups -- where the user has the most need of somewhere to write.
  def genMicroRosUserDeclarations(): ST = {
    return (
      st"""${userDeclarationsMarker.beginMarker}
          |// Storage for message fields codegen could not size from the model, e.g. a sequence
          |// or string field of a platform-provided type whose mirror declares no dimensions:
          |//     static float joy_axes_buf[8];
          |${userDeclarationsMarker.endMarker}""")
  }

  def genMicroRosUserInit(): ST = {
    return (
      st"""${userInitMarker.beginMarker}
          |// Attach storage declared above to the corresponding message fields, e.g.:
          |//     self->proc_ttj_joy_msg.axes.data = joy_axes_buf;
          |//     self->proc_ttj_joy_msg.axes.capacity = 8;
          |//     self->proc_ttj_joy_msg.axes.size = 0;
          |${userInitMarker.endMarker}""")
  }

  // The seed content is behaviorally inert: rcl accepts an empty "--ros-args" section, and
  // keeping the array non-empty makes the initializer valid ISO C (T a[] = {} is not).
  def genMicroRosNodeOptions(): ST = {
    return (
      st"""${nodeOptionsMarker.beginMarker}
          |// Add rcl arguments after "--ros-args", e.g. a remap rule binding one of this
          |// node's topics to a preexisting node's topic:
          |//     "-r", "some_port:=/some/other/topic"
          |// Write the match side of a remap rule relative (no leading '/') so that it keeps
          |// matching if the node is later placed in a namespace.
          |static const char * const node_options[] = {
          |    "--ros-args"
          |};
          |${nodeOptionsMarker.endMarker}""")
  }

  // rcl parses node_options only when the micro-ROS firmware is built with
  // RCL_COMMAND_LINE_ENABLED=ON (the micro-ROS fork of rcl strips the argument machinery by
  // default), which is why the generated colcon.meta sets it unconditionally.
  // Micro-ROS is static-memory: the rclc executor deserializes each received message into a
  // pre-allocated struct the node supplies, and a sequence field arrives as a {data, size,
  // capacity} triple whose storage the node must attach before the executor runs.  Capacities are
  // compile-time constants from the mirror, so the storage is a static array -- no heap, no added
  // dependency on micro_ros_utilities, and buffer sizes stay greppable back to the model.
  //
  // Undersizing is silent: ucdr sets an error flag and writes no elements, the typesupport
  // recovers by delivering the field with size 0 (or, if it is the final member, fails the whole
  // message so rmw_take reports "no data").  Neither reaches the application as a diagnostic.
  def genMicroRosSequenceBuffers(inPorts: ISZ[AadlPort]): ISZ[ST] = {
    var decls: ISZ[ST] = ISZ()
    for (p <- inPorts) {
      portAadlTypeOpt(p) match {
        case Some(aadlType) =>
          for (f <- mirrorSequenceFields(aadlType)) {
            val (fieldName, cType, capacity) = f
            decls = decls :+ st"static ${cType} ${genPortName(p)}_${fieldName}_buf[${capacity}];"
          }
        case _ =>
      }
    }
    return decls
  }

  // Both sections render to nothing when no port has a bounded sequence field, so nodes without
  // one keep byte-identical output.
  def genMicroRosSequenceBufferSection(inPorts: ISZ[AadlPort]): ST = {
    val decls = genMicroRosSequenceBuffers(inPorts)
    if (decls.isEmpty) {
      return st""
    }
    return (
      st"""
          |// Static receive buffers for the bounded sequence fields of subscription messages.
          |// Capacities come from the model's mirror dimensions -- see the design doc's
          |// Micro-ROS Memory Configuration.
          |${(decls, "\n")}
        """)
  }

  def genMicroRosSequenceInitSection(inPorts: ISZ[AadlPort]): ST = {
    val inits = genMicroRosSequenceInits(inPorts)
    if (inits.isEmpty) {
      return st""
    }
    return (
      st"""
          |
          |    // Attach the static receive buffers before the executor can deliver a message
          |    ${(inits, "\n")}""")
  }

  def genMicroRosSequenceInits(inPorts: ISZ[AadlPort]): ISZ[ST] = {
    var inits: ISZ[ST] = ISZ()
    for (p <- inPorts) {
      portAadlTypeOpt(p) match {
        case Some(aadlType) =>
          val portName = genPortName(p)
          for (f <- mirrorSequenceFields(aadlType)) {
            val (fieldName, _, capacity) = f
            inits = inits :+
              st"""self->${portName}_msg.${fieldName}.data = ${portName}_${fieldName}_buf;
                  |self->${portName}_msg.${fieldName}.capacity = ${capacity};
                  |self->${portName}_msg.${fieldName}.size = 0;"""
          }
        case _ =>
      }
    }
    return inits
  }

  // A micro-ROS subscription whose payload codegen cannot fully size is a latent silent-drop:
  // the native type may have unbounded fields the mirror says nothing about.  Codegen only knows
  // the model, so it cannot confirm which native fields are unbounded -- it reports what it could
  // not size and leaves the decision to the modeler.
  def validateMicroRosCapacities(microRosThreads: ISZ[AadlThread], reporter: Reporter): Unit = {
    for (thread <- microRosThreads;
         p <- generatedPorts(thread) if p.direction == Direction.In) {
      portAadlTypeOpt(p) match {
        case Some(aadlType) if RosUtil.isPlatformProvided(aadlType) =>
          val sized = mirrorSequenceFields(aadlType)
          val mirrored: B = aadlType match {
            case r: RecordType => r.fields.nonEmpty
            case _ => F
          }
          if (!mirrored) {
            reporter.warn(p.posOpt, RosUtil.toolName,
              st"""${thread.identifier}.${p.identifier} receives the opaque platform-provided type ${aadlType.name},
                  |so codegen cannot size its static receive buffer.  If the native type has unbounded fields
                  |(sequences or strings), mirror them with dimensions or attach storage in the preserved block --
                  |otherwise an oversized message is delivered with the field empty, or dropped without a diagnostic.""".render)
          } else if (sized.isEmpty) {
            reporter.warn(p.posOpt, RosUtil.toolName,
              st"""${thread.identifier}.${p.identifier} receives ${aadlType.name}, whose mirror declares no bounded
                  |sequence fields.  Any unbounded native field is unsized, and overflow of an unsized field is
                  |silent -- the field arrives empty or the message is dropped with no diagnostic.""".render)
          }
        case _ =>
      }
    }
  }

  // rcl_logging_configure is declared in rcl/logging.h, which is pulled in only when a rosout
  // out port makes the generated init call it
  @strictpure def genMicroRosBaseNodeIncludes(microrosPkgName: String, nodeNameBase: String, component: AadlThread): ST =
    if (RosUtil.producesRosout(component))
      st"""#include "${microrosPkgName}/base_headers/${nodeNameBase}${c_src_node_header_name_suffix}"
          |#include "rcl/logging.h""""
    else st"""#include "${microrosPkgName}/base_headers/${nodeNameBase}${c_src_node_header_name_suffix}""""

  def genMicroRosSupportInit(component: AadlThread): ST = {
    val supportInit: ST =
      st"""rcl_init_options_t init_options = rcl_get_zero_initialized_init_options();
          |RCL_CHECK(rcl_init_options_init(&init_options, self->allocator));
          |
          |RCL_CHECK(rclc_support_init_with_options(
          |    &self->support,
          |    (int) (sizeof(node_options) / sizeof(node_options[0])), node_options,
          |    &init_options, &self->allocator));"""

    if (!RosUtil.producesRosout(component)) {
      // no rosout out port: logging stays console-only (rcutils to stderr)
      return supportInit
    }

    // A rosout out port is the micro-ROS enablement trigger.  The /rosout publisher is created
    // and driven by rcl as a side effect of this call, never by application code -- which is why
    // no publisher or put_ API is generated for the port.
    return (
      st"""${supportInit}
          |
          |// Route this node's own log records to /rosout.  Requires the firmware to be built
          |// with RCL_LOGGING_ENABLED=ON plus a backend; see microros_apps/colcon.meta.
          |// Checked because the failure mode is invisible otherwise: the node runs, but its
          |// log records never reach /rosout and any node subscribed to them just sees silence.
          |RCL_CHECK(rcl_logging_configure(&self->support.context.global_arguments, &self->allocator));""")
  }

  def genMicroRosBaseNodeCFile(microrosPkgName: String, cppPkgName: String, component: AadlThread,
                               connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               datatypeMap: Map[AadlType, Ros2Datatype],
                               invertTopicBinding: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeNameBase}${c_src_node_name_suffix}"

    val outPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.Out)
    val inPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.In)
    val dataInPorts = ISZOps(inPorts).filter(p => p.isInstanceOf[AadlDataPort])

    val publisherInits = genMicroRosPublisherInits(outPorts, nodeName, cppPkgName, datatypeMap, connectionMap, invertTopicBinding, reporter)
    val putImpls = genMicroRosPutFunctionImpls(outPorts, nodeName, cppPkgName, datatypeMap, connectionMap, invertTopicBinding, reporter)

    val fileBody: ST =
      if (isSporadic(component)) {
        val handleForwardDecls = genMicroRosHandleForwardDecls(inPorts, nodeName, cppPkgName, datatypeMap, reporter)
        val subscriptionCallbacks = genMicroRosSubscriptionCallbacks(inPorts, nodeName, cppPkgName, datatypeMap, reporter)
        val subscriptionInits = genMicroRosSubscriptionInits(inPorts, cppPkgName, datatypeMap, invertTopicBinding, connectionMap, reporter)
        val executorAdds = genMicroRosSubscriptionExecutorAdds(inPorts)
        val getImpls = genMicroRosGetFunctionImpls(dataInPorts, nodeName, cppPkgName, datatypeMap, reporter)
        val numHandles: Z = inPorts.size
        val getSection: ST =
          if (getImpls.nonEmpty)
            st"""
                |//=================================================
                |//  D a t a   P o r t   A c c e s s
                |//=================================================
                |
                |${(getImpls, "\n")}"""
          else st""
        st"""${genMicroRosBaseNodeIncludes(microrosPkgName, nodeNameBase, component)}
            |
            |${CommentTemplate.invertedMarkerComment_slash}
            |
            |// Forward declarations of user compute entry points
            |${(handleForwardDecls, "\n")}
            |
            |// Static instance pointer for subscription callback context (heap-free, MCU-compatible)
            |static ${nodeNameBase}_t * g_self = NULL;
            |
            |// Logger name used by the PRINT_* macros; updated to the node's actual logger
            |// name once the node has been initialized
            |const char * ${nodeName}_logger_name = "${nodeName}";
            |
            |${genMicroRosNodeOptions()}
            |${genMicroRosSequenceBufferSection(inPorts)}
            |${genMicroRosUserDeclarations()}
            |
            |//=================================================
            |//  S u b s c r i p t i o n   C a l l b a c k s
            |//=================================================
            |
            |${(subscriptionCallbacks, "\n")}
            |//=================================================
            |//  I n i t i a l i z a t i o n
            |//=================================================
            |
            |rcl_ret_t ${nodeNameBase}_init(${nodeNameBase}_t * self)
            |{
            |    g_self = self;
            |
            |    self->allocator = rcl_get_default_allocator();
            |
            |    ${genMicroRosSupportInit(component)}
            |
            |    RCL_CHECK(rclc_node_init_default(&self->node, "${nodeName}", "${RosUtil.getRosNamespace(component)}", &self->support));
            |
            |    // Retrieve the node's registered logger name for use by the PRINT_* macros
            |    const char * logger_name = rcl_node_get_logger_name(&self->node);
            |    if (logger_name != NULL) {
            |        ${nodeName}_logger_name = logger_name;
            |    }
            |
            |    // Setting up connections
            |    ${(publisherInits, "\n")}
            |    // Setting up subscriptions
            |    ${(subscriptionInits, "\n")}${genMicroRosSequenceInitSection(inPorts)}
            |
            |    ${genMicroRosUserInit()}
            |
            |    RCL_CHECK(rclc_executor_init(&self->executor, &self->support.context, ${numHandles}, &self->allocator));
            |    ${(executorAdds, "\n")}
            |
            |    return RCL_RET_OK;
            |}
            |
            |void ${nodeNameBase}_spin(${nodeNameBase}_t * self)
            |{
            |    rclc_executor_spin(&self->executor);
            |}
            |
            |//=================================================
            |//  C o m m u n i c a t i o n
            |//=================================================
            |
            |${(putImpls, "\n")}${getSection}
          """
      } else {
        val period = component.period.get
        st"""${genMicroRosBaseNodeIncludes(microrosPkgName, nodeNameBase, component)}
            |
            |${CommentTemplate.invertedMarkerComment_slash}
            |
            |// Forward declaration of user compute entry point
            |void ${nodeName}_timeTriggered(${nodeNameBase}_t * self);
            |
            |// Static instance pointer for timer callback context (heap-free, MCU-compatible)
            |static ${nodeNameBase}_t * g_self = NULL;
            |
            |// Logger name used by the PRINT_* macros; updated to the node's actual logger
            |// name once the node has been initialized
            |const char * ${nodeName}_logger_name = "${nodeName}";
            |
            |${genMicroRosNodeOptions()}
            |
            |${genMicroRosUserDeclarations()}
            |
            |//=================================================
            |//  C a l l b a c k   a n d   T i m e r
            |//=================================================
            |
            |static void period_timer_callback(rcl_timer_t * timer, int64_t last_call_time)
            |{
            |    (void)timer;
            |    (void)last_call_time;
            |    if (g_self != NULL) {
            |        ${nodeName}_timeTriggered(g_self);
            |    }
            |}
            |
            |//=================================================
            |//  I n i t i a l i z a t i o n
            |//=================================================
            |
            |rcl_ret_t ${nodeNameBase}_init(${nodeNameBase}_t * self)
            |{
            |    g_self = self;
            |
            |    self->allocator = rcl_get_default_allocator();
            |
            |    ${genMicroRosSupportInit(component)}
            |
            |    RCL_CHECK(rclc_node_init_default(&self->node, "${nodeName}", "${RosUtil.getRosNamespace(component)}", &self->support));
            |
            |    // Retrieve the node's registered logger name for use by the PRINT_* macros
            |    const char * logger_name = rcl_node_get_logger_name(&self->node);
            |    if (logger_name != NULL) {
            |        ${nodeName}_logger_name = logger_name;
            |    }
            |
            |    // Setting up connections
            |    ${(publisherInits, "\n")}
            |    // timeTriggered callback timer
            |    RCL_CHECK(rclc_timer_init_default(
            |        &self->period_timer,
            |        &self->support,
            |        RCL_MS_TO_NS(${period}),
            |        period_timer_callback));
            |
            |    ${genMicroRosUserInit()}
            |
            |    RCL_CHECK(rclc_executor_init(&self->executor, &self->support.context, 1, &self->allocator));
            |    RCL_CHECK(rclc_executor_add_timer(&self->executor, &self->period_timer));
            |
            |    return RCL_RET_OK;
            |}
            |
            |void ${nodeNameBase}_spin(${nodeNameBase}_t * self)
            |{
            |    rclc_executor_spin(&self->executor);
            |}
            |
            |//=================================================
            |//  C o m m u n i c a t i o n
            |//=================================================
            |
            |${(putImpls, "\n")}
          """
      }

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "src", "base_code", fileName)
    return (filePath, fileBody, T, IS(nodeOptionsMarker, userDeclarationsMarker, userInitMarker))
  }

  //================================================
  //  M i c r o R O S   R u n n e r
  //================================================

  def genMicroRosRunnerFile(microrosPkgName: String, component: AadlThread): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeName}${c_node_runner_name_suffix}"

    val fileBody =
      st"""#include "${microrosPkgName}/user_headers/${nodeName}${c_src_node_header_name_suffix}"
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |static ${nodeNameBase}_t node;
          |
          |int main(int argc, char ** argv)
          |{
          |    (void)argc;
          |    (void)argv;
          |
          |    // A failure here means the node could not create the entities it needs, so
          |    // spinning would busy-wait forever on a node that can never publish or
          |    // receive.  Exiting non-zero instead lets the launching layer notice.
          |    rcl_ret_t init_status = ${nodeNameBase}_init(&node);
          |    if (init_status != RCL_RET_OK) {
          |        PRINT_ERROR("${nodeName} initialization failed with status %d; aborting", (int) init_status);
          |        return 1;
          |    }
          |
          |    // Invoke initialize entry point
          |    ${nodeName}_initialize(&node);
          |
          |    PRINT_INFO("${nodeName} infrastructure set up");
          |
          |    ${nodeNameBase}_spin(&node);
          |
          |    return 0;
          |}
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "src", "base_code", fileName)
    return (filePath, fileBody, T, IS())
  }

  //================================================
  //  M i c r o R O S   U s e r   C o d e
  //================================================

  def genMicroRosUserNodeHeaderFile(microrosPkgName: String, cppPkgName: String, component: AadlThread,
                                     datatypeMap: Map[AadlType, Ros2Datatype],
                                     reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeName}${c_src_node_header_name_suffix}"
    val guardName = ops.StringOps(s"${nodeName}_src_h").toUpper

    val computeEntryPointDecls: ST =
      if (isSporadic(component)) {
        val inPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.In)
        val handleDecls = genMicroRosHandleForwardDecls(inPorts, nodeName, cppPkgName, datatypeMap, reporter)
        st"${(handleDecls, "\n")}"
      } else {
        st"void ${nodeName}_timeTriggered(${nodeNameBase}_t * self);"
      }

    val fileBody =
      st"""#ifndef ${guardName}
          |#define ${guardName}
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |#include "${microrosPkgName}/base_headers/${nodeNameBase}${c_src_node_header_name_suffix}"
          |
          |//=================================================
          |//  I n i t i a l i z e    E n t r y    P o i n t
          |//=================================================
          |void ${nodeName}_initialize(${nodeNameBase}_t * self);
          |
          |//=================================================
          |//  C o m p u t e    E n t r y    P o i n t
          |//=================================================
          |${computeEntryPointDecls}
          |
          |#endif  // ${guardName}
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "include", microrosPkgName, "user_headers", fileName)
    return (filePath, fileBody, T, IS())
  }

  def genMicroRosUserNodeCFile(microrosPkgName: String, cppPkgName: String, component: AadlThread,
                               datatypeMap: Map[AadlType, Ros2Datatype],
                               hasEnumConverter: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeName}${c_src_node_name_suffix}"

    val outPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.Out)
    val enumConverterInclude: ST = if (hasEnumConverter) st"""#include "${microrosPkgName}/base_headers/enum_converter.h"""" else st""

    val computeSection: ST =
      if (isSporadic(component)) {
        val inPorts = ISZOps(generatedPorts(component)).filter(p => p.direction == Direction.In)
        val dataInPorts = ISZOps(inPorts).filter(p => p.isInstanceOf[AadlDataPort])
        val eventInPorts = ISZOps(inPorts).filter(p => !p.isInstanceOf[AadlDataPort])

        var dataPortExamples: ISZ[ST] = IS()
        for (dp <- dataInPorts) {
          val dpId = dp.identifier
          val dpDatatype = genPortDatatype(dp, cppPkgName, datatypeMap, reporter)
          val dpCType = cppTypeToCStructName(dpDatatype)
          dataPortExamples = dataPortExamples :+
            st"""${dpCType} * ${dpId} = get_${dpId}(self);"""
        }

        var examplePublishes: ISZ[ST] = IS()
        for (p <- outPorts) {
          val portId = p.identifier
          val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
          if (isEventPort(portDatatype)) {
            examplePublishes = examplePublishes :+ st"put_${portId}(self);"
          } else {
            val cType = cppTypeToCStructName(portDatatype)
            val initExpr = portExampleInit(p, "{0}", datatypeMap)
            examplePublishes = examplePublishes :+
              st"""${cType} ${portId} = ${initExpr};
                  |put_${portId}(self, &${portId});
                  |${genCSentLog(p)}"""
          }
        }

        var handlers: ISZ[ST] = IS()
        var isFirstHandler: B = T
        for (p <- eventInPorts) {
          val portId = p.identifier
          val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
          val cType = cppTypeToCStructName(portDatatype)

          var extraBodyLines: ISZ[ST] = IS()
          if (isFirstHandler) {
            isFirstHandler = F
            if (dataPortExamples.nonEmpty) {
              extraBodyLines = extraBodyLines :+ st""
              extraBodyLines = extraBodyLines :+ st"    // example receiving messages on data ports"
              for (ex <- dataPortExamples) {
                extraBodyLines = extraBodyLines :+ st"    ${ex}"
              }
            }
            if (examplePublishes.nonEmpty) {
              extraBodyLines = extraBodyLines :+ st""
              extraBodyLines = extraBodyLines :+ st"    // example publishing messages"
              for (ex <- examplePublishes) {
                extraBodyLines = extraBodyLines :+ st"    ${ex}"
              }
            }
          }

          if (isEventPort(portDatatype)) {
            var bodyLines: ISZ[ST] = IS(st"    // Handle ${portId} event")
            bodyLines = bodyLines :+ st"""    PRINT_INFO("Received ${portId}");"""
            for (l <- extraBodyLines) {
              bodyLines = bodyLines :+ l
            }
            handlers = handlers :+
              st"""void ${nodeName}_handle_${portId}(${nodeNameBase}_t * self)
                  |{
                  |${(bodyLines, "\n")}
                  |}
                """
          } else {
            var bodyLines: ISZ[ST] = IS(st"    // Handle ${portId} msg")
            bodyLines = bodyLines :+ st"""    PRINT_INFO("Received ${portId}");"""
            for (l <- extraBodyLines) {
              bodyLines = bodyLines :+ l
            }
            handlers = handlers :+
              st"""void ${nodeName}_handle_${portId}(${nodeNameBase}_t * self, const ${cType} * msg)
                  |{
                  |${(bodyLines, "\n")}
                  |}
                """
          }
        }

        st"""//=================================================
            |//  C o m p u t e    E n t r y    P o i n t
            |//=================================================
            |${(handlers, "\n")}"""
      } else {
        var examplePublishes: ISZ[ST] = IS()
        for (p <- outPorts) {
          val portId = p.identifier
          val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
          if (isEventPort(portDatatype)) {
            examplePublishes = examplePublishes :+ st"put_${portId}(self);"
          } else {
            val cType = cppTypeToCStructName(portDatatype)
            val initExpr = portExampleInit(p, "{0}", datatypeMap)
            examplePublishes = examplePublishes :+
              st"""${cType} ${portId} = ${initExpr};
                  |put_${portId}(self, &${portId});
                  |${genCSentLog(p)}"""
          }
        }
        st"""//=================================================
            |//  C o m p u t e    E n t r y    P o i n t
            |//=================================================
            |void ${nodeName}_timeTriggered(${nodeNameBase}_t * self)
            |{
            |    // Handle communication
            |
            |    // Example publishing messages
            |    ${(examplePublishes, "\n")}
            |}"""
      }

    val fileBody =
      st"""#include "${microrosPkgName}/user_headers/${nodeName}${c_src_node_header_name_suffix}"
          |${enumConverterInclude}
          |
          |${CommentTemplate.safeToEditComment_slash}
          |
          |//=================================================
          |//  I n i t i a l i z e    E n t r y    P o i n t
          |//=================================================
          |void ${nodeName}_initialize(${nodeNameBase}_t * self)
          |{
          |    PRINT_INFO("Initialize Entry Point invoked");
          |
          |    // Initialize the node
          |}
          |
          |${computeSection}
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "src", "user_code", fileName)
    return (filePath, fileBody, F, IS())
  }

  //================================================
  //  M i c r o R O S   E n u m   C o n v e r t e r
  //================================================

  def genMicroRosEnumConverterHeaderFile(microrosPkgName: String, cppPkgName: String,
                                         enumTypes: ISZ[(String, AadlType)]): (ISZ[String], ST, B, ISZ[Marker]) = {
    var includes: ISZ[ST] = IS()
    var converterHeaders: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")
      val msgTypeCpp: String = s"${cppPkgName}_interfaces::msg::${enumName}"
      val headerPath: String = cppTypeToCHeaderPath(msgTypeCpp)

      includes = includes :+ st"""#include "${headerPath}""""
      converterHeaders = converterHeaders :+
        st"const char* enumToString_${enumName}(uint8_t value);"
    }

    val fileBody =
      st"""#ifndef ENUM_CONVERTER_H
          |#define ENUM_CONVERTER_H
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |#include <stdint.h>
          |${(includes, "\n")}
          |
          |// C does not support function overloading; enum types are encoded in the name
          |${(converterHeaders, "\n")}
          |
          |#endif  // ENUM_CONVERTER_H
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "include", microrosPkgName, "base_headers", "enum_converter.h")
    return (filePath, fileBody, T, IS())
  }

  def genMicroRosEnumConverterCFile(microrosPkgName: String, cppPkgName: String,
                                    enumTypes: ISZ[(String, AadlType)]): (ISZ[String], ST, B, ISZ[Marker]) = {
    var converters: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")
      val cPkgPrefix: String = ops.StringOps(s"${cppPkgName}_interfaces").replaceAllLiterally("::", "__")
      val enumValues: ISZ[String] = enum._2.asInstanceOf[EnumType].values
      val fieldUpper: String = ops.StringOps(enum._1).toUpper

      var cases: ISZ[ST] = IS()
      for (value <- enumValues) {
        val cConst = s"${cPkgPrefix}__msg__${enumName}__${fieldUpper}_${ops.StringOps(value).toUpper}"
        cases = cases :+ st"""case ${cConst}: return "${value}";"""
      }

      converters = converters :+
        st"""const char* enumToString_${enumName}(uint8_t value)
            |{
            |    switch (value) {
            |        ${(cases, "\n")}
            |        default: return "Unknown";
            |    }
            |}
          """
    }

    val fileBody =
      st"""#include "${microrosPkgName}/base_headers/enum_converter.h"
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(converters, "\n")}
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, "src", "base_code", "enum_converter.c")
    return (filePath, fileBody, T, IS())
  }

  def genMicroRosEnumConverterFiles(modelName: String, cppPkgName: String,
                                    datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var enumTypes: ISZ[(String, AadlType)] = IS()

    for (key <- datatypeMap.keys if !datatypeMap.get(key).get.isPlatformProvided) {
      key match {
        case _: EnumType =>
          val datatype: String = datatypeMap.get(key).get.content.apply(0)
          val datatypeName: String = StringOps(datatype).substring(StringOps(datatype).indexOf(' ') + 1, datatype.size)
          enumTypes = enumTypes :+ (datatypeName, key)
        case _ =>
      }
    }

    if (enumTypes.size == 0) {
      return IS()
    }

    val microrosPkgName: String = genMicroRosPackageName(modelName)

    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    files = files :+ genMicroRosEnumConverterHeaderFile(microrosPkgName, cppPkgName, enumTypes)
    files = files :+ genMicroRosEnumConverterCFile(microrosPkgName, cppPkgName, enumTypes)
    return files
  }

  //================================================
  //  M i c r o R O S   B u i l d   F i l e s
  //================================================

  def genMicroRosCMakeListsFile(modelName: String, cppPkgName: String, microRosThreads: ISZ[AadlThread],
                                hasEnumConverter: B, nativePackages: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val interfacesPkg: String = s"${cppPkgName}_interfaces"
    val fileName: String = "CMakeLists.txt"

    var entryPointDecls: ISZ[ST] = IS()
    var entryPointExecutables: ISZ[String] = IS()

    val packages: ISZ[String] = ISZ[String]("rclc", "rcutils", interfacesPkg) ++ nativePackages
    val pkgRequirements: ISZ[ST] = genCMakeListsPkgRequirements(packages)
    // note: the sequence-with-separator form is an ST feature, so this must be st"..." rather
    // than s"..." -- the latter would render the tuple's string representation
    val nativePackagesSuffix: String = if (nativePackages.isEmpty) "" else st" ${(nativePackages, " ")}".render

    for (comp <- microRosThreads) {
      val nodeName = genNodeName(comp)
      var srcFiles: ISZ[String] = IS(
        s"src/base_code/${nodeName}${c_node_runner_name_suffix}",
        s"src/user_code/${nodeName}${c_src_node_name_suffix}",
        s"src/base_code/${nodeName}_base${c_src_node_name_suffix}"
      )
      if (hasEnumConverter) {
        srcFiles = srcFiles :+ s"src/base_code/enum_converter.c"
      }
      val execName = genExecutableFileName(nodeName)
      entryPointDecls = entryPointDecls :+
        st"""add_executable(${execName} ${(srcFiles, " ")})
            |ament_target_dependencies(${execName} rclc rcutils ${interfacesPkg}${nativePackagesSuffix})"""
      entryPointExecutables = entryPointExecutables :+ execName
    }

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "#",
      optBeginSuffix = None(),
      endPrefix = "#",
      optEndSuffix = None())

    val fileBody =
      st"""cmake_minimum_required(VERSION 3.8)
          |project(${microrosPkgName})
          |
          |${CommentTemplate.invertedMarkerComment_hash}
          |
          |if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
          |    add_compile_options(-Wall -Wextra -Wpedantic)
          |endif()
          |
          |find_package(ament_cmake REQUIRED)
          |${(pkgRequirements, "\n")}
          |
          |${marker.beginMarker}
          |
          |${marker.endMarker}
          |
          |include_directories(include)
          |
          |${(entryPointDecls, "\n\n")}
          |
          |install(TARGETS
          |    ${(entryPointExecutables, "\n")}
          |    DESTINATION lib/$${PROJECT_NAME}
          |)
          |
          |ament_package()
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, fileName)
    return (filePath, fileBody, T, IS(marker))
  }

  // The per-process entity counts the micro-ROS rmw layer's static pools must be sized for.
  //
  // Each generated node is its own executable, so the pools compiled into rmw_microxrcedds
  // must hold the LARGEST node's entities rather than the sum over nodes.  The counts mirror
  // what genMicroRosPublisherInits and genMicroRosSubscriptionInits actually emit: one
  // publisher per connected in port (one for an unconnected out port), and one subscription
  // per in port -- but only for sporadic nodes, since periodic nodes generate none.
  def getMicroRosEntityCounts(microRosThreads: ISZ[AadlThread],
                              connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                              invertTopicBinding: B): (Z, Z) = {
    var maxPublishers: Z = 0
    var maxSubscriptions: Z = 0

    for (comp <- microRosThreads) {
      var publishers: Z = 0
      for (outPort <- ISZOps(generatedPorts(comp)).filter(port => port.direction == Direction.Out)) {
        val fanOut: Z =
          if (!invertTopicBinding && connectionMap.get(outPort.path).nonEmpty) connectionMap.get(outPort.path).get.size
          else 1
        publishers = publishers + fanOut
      }

      val subscriptions: Z =
        if (isSporadic(comp)) ISZOps(generatedPorts(comp)).filter(port => port.direction == Direction.In).size
        else 0

      if (publishers > maxPublishers) {
        maxPublishers = publishers
      }
      if (subscriptions > maxSubscriptions) {
        maxSubscriptions = subscriptions
      }
    }

    return (maxPublishers, maxSubscriptions)
  }

  // The micro-ROS firmware build's configuration.
  //
  // Entries outside the marked blocks are derived from the model and are regenerated on every
  // run; the marked blocks hold deployment configuration and are preserved.  A derived value
  // can be overridden by restating its -D flag inside a marked block: colcon passes cmake-args
  // through in order and CMake takes the last occurrence of a flag.
  def genMicroRosColconMetaFile(maxPublishers: Z, maxSubscriptions: Z, hasRosoutProducer: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    // The knobs are emitted either way so they stay discoverable; a rosout out port is what
    // decides whether rcl logging is built in, and rcl_logging_spdlog is the host-side backend
    // (embedded targets generally want rcl_logging_noop).
    val loggingEnabled: String = if (hasRosoutProducer) "ON" else "OFF"
    val loggingImpl: String = if (hasRosoutProducer) "rcl_logging_spdlog" else "rcl_logging_noop"
    val buildProfileMarker = BlockMarker(
      id = "BUILD PROFILE - additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "#",
      optBeginSuffix = None(),
      endPrefix = "#",
      optEndSuffix = None())

    val transportMarker = BlockMarker(
      id = "TRANSPORT AND TUNING - additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "#",
      optBeginSuffix = None(),
      endPrefix = "#",
      optEndSuffix = None())

    // colcon.meta is YAML (the JSON-looking content is YAML flow style), so it can carry comments
    val fileBody =
      st"""${CommentTemplate.invertedMarkerComment_hash}
          |#
          |# Firmware configuration for the micro-ROS packages in this directory.
          |{
          |    "names": {
          |        ${buildProfileMarker.beginMarker}
          |        # Build profile -- seeded for a host-Linux micro-ROS build.  Embedded
          |        # targets generally want -DBUILD_SHARED_LIBS=OFF throughout.
          |        #
          |        # The UCLIENT_PROFILE_* transport enabled here must agree with
          |        # RMW_UXRCE_TRANSPORT below.
          |        "microxrcedds_client": {
          |            "cmake-args": [
          |                "-DBUILD_SHARED_LIBS=ON",
          |                "-DUCLIENT_PROFILE_UDP=ON"
          |            ]
          |        },
          |        "microcdr": {
          |            "cmake-args": [
          |                "-DBUILD_SHARED_LIBS=ON"
          |            ]
          |        },
          |        "rosidl_typesupport_microxrcedds_c": {
          |            "cmake-args": [
          |                "-DBUILD_SHARED_LIBS=ON"
          |            ]
          |        },
          |        "rosidl_typesupport_microxrcedds_cpp": {
          |            "cmake-args": [
          |                "-DBUILD_SHARED_LIBS=ON"
          |            ]
          |        },
          |        ${buildProfileMarker.endMarker}
          |        "rcl": {
          |            "cmake-args": [
          |                "-DBUILD_TESTING=OFF",
          |                # Required for the rcl arguments in each node's node_options block
          |                # (topic remap rules in particular) to be parsed: the micro-ROS fork
          |                # of rcl makes its argument machinery compile-time removable and
          |                # strips it by default.
          |                "-DRCL_COMMAND_LINE_ENABLED=ON",
          |                # Generated nodes log through rcutils, which reaches stderr without
          |                # rcl logging.  Routing a node's own records to /rosout additionally
          |                # needs RCL_LOGGING_ENABLED=ON plus a backend -- rcl_logging_spdlog
          |                # on host, rcl_logging_noop on embedded.  These follow the model:
          |                # they are ON when some node declares a rosout out port.
          |                "-DRCL_LOGGING_ENABLED=${loggingEnabled}",
          |                "-DRCL_LOGGING_IMPLEMENTATION=${loggingImpl}"
          |            ]
          |        },
          |        "rcutils": {
          |            "cmake-args": [
          |                "-DENABLE_TESTING=OFF"
          |            ]
          |        },
          |        "rmw_microxrcedds": {
          |            "cmake-args": [
          |                # Static entity pools, sized from the model.  Each generated node is
          |                # its own executable, so these hold the largest node's entities
          |                # rather than the sum over nodes.  Generated nodes create no services
          |                # or clients.  Undersizing them makes entity creation fail silently.
          |                "-DRMW_UXRCE_MAX_NODES=1",
          |                "-DRMW_UXRCE_MAX_PUBLISHERS=${maxPublishers}",
          |                "-DRMW_UXRCE_MAX_SUBSCRIPTIONS=${maxSubscriptions}",
          |                "-DRMW_UXRCE_MAX_SERVICES=0",
          |                "-DRMW_UXRCE_MAX_CLIENTS=0",
          |                ${transportMarker.beginMarker}
          |                # Transport -- deployment configuration, seeded for a micro-ROS agent
          |                # reachable over udp4 at 127.0.0.1:8888.
          |                "-DRMW_UXRCE_TRANSPORT=udp",
          |                "-DRMW_UXRCE_DEFAULT_UDP_IP=127.0.0.1",
          |                "-DRMW_UXRCE_DEFAULT_UDP_PORT=8888",
          |                # Stream and history depth bound how large a serialized message may be
          |                # and how many may be in flight.  A message that does not fit is
          |                # dropped without a diagnostic, so raise these if messages go missing.
          |                "-DRMW_UXRCE_STREAM_HISTORY=32",
          |                "-DRMW_UXRCE_MAX_HISTORY=10"
          |                ${transportMarker.endMarker}
          |            ]
          |        }
          |    }
          |}"""

    val filePath: ISZ[String] = IS("microros_apps", "colcon.meta")
    return (filePath, fileBody, T, IS(buildProfileMarker, transportMarker))
  }

  def genMicroRosPackageFile(modelName: String, cppPkgName: String, nativePackages: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val interfacesPkg: String = s"${cppPkgName}_interfaces"
    val fileName: String = "package.xml"

    val packages: ISZ[String] = ISZ[String]("rclc", "rcutils", interfacesPkg) ++ nativePackages
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "<!--",
      optBeginSuffix = Some("-->"),
      endPrefix = "<!--",
      optEndSuffix = Some("-->"))

    val fileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
          |
          |${CommentTemplate.invertedMarkerComment_xml}
          |
          |<package format="3">
          |    <name>${microrosPkgName}</name>
          |    <version>0.0.0</version>
          |    <description>TODO: Package description</description>
          |    <maintainer email="sireum@todo.todo">sireum</maintainer>
          |    <license>TODO: License declaration</license>
          |
          |    <buildtool_depend>ament_cmake</buildtool_depend>
          |
          |    ${(pkgDependencies, "\n")}
          |
          |    ${marker.beginMarker}
          |
          |    ${marker.endMarker}
          |
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("microros_apps", microrosPkgName, fileName)
    return (filePath, fileBody, T, IS(marker))
  }

  //================================================
  //  M i c r o R O S   N o d e   F i l e s
  //================================================

  def genMicroRosNodeFiles(modelName: String, microRosThreads: ISZ[AadlThread],
                           connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                           datatypeMap: Map[AadlType, Ros2Datatype],
                           hasEnumConverter: B, invertTopicBinding: B,
                           reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val cppPkgName: String = genCppPackageName(modelName)

    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (comp <- microRosThreads) {
      files = files :+ genMicroRosBaseNodeHeaderFile(microrosPkgName, cppPkgName, comp, connectionMap, datatypeMap, hasEnumConverter, invertTopicBinding, reporter)
      files = files :+ genMicroRosBaseNodeCFile(microrosPkgName, cppPkgName, comp, connectionMap, datatypeMap, invertTopicBinding, reporter)
      files = files :+ genMicroRosRunnerFile(microrosPkgName, comp)
      files = files :+ genMicroRosUserNodeHeaderFile(microrosPkgName, cppPkgName, comp, datatypeMap, reporter)
      files = files :+ genMicroRosUserNodeCFile(microrosPkgName, cppPkgName, comp, datatypeMap, hasEnumConverter, reporter)
    }

    return files
  }

  def genMicroRosNodePkg(modelName: String, microRosThreads: ISZ[AadlThread],
                         connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                         datatypeMap: Map[AadlType, Ros2Datatype],
                         invertTopicBinding: B, reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val cppPkgName: String = genCppPackageName(modelName)

    val converterFiles = genMicroRosEnumConverterFiles(modelName, cppPkgName, datatypeMap)
    val hasEnumConverter: B = converterFiles.size > 0

    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    files = files ++ genMicroRosNodeFiles(modelName, microRosThreads, connectionMap, datatypeMap, hasEnumConverter, invertTopicBinding, reporter)
    files = files ++ converterFiles
    files = files :+ genMicroRosExampleTypesFile(modelName, cppPkgName, datatypeMap)

    // the packages supplying the platform-provided payload types the nodes use
    val nativePackages: ISZ[String] = getNativePackages(microRosThreads, datatypeMap)
    files = files :+ genMicroRosCMakeListsFile(modelName, cppPkgName, microRosThreads, hasEnumConverter, nativePackages)
    files = files :+ genMicroRosPackageFile(modelName, cppPkgName, nativePackages)
    val (maxPublishers, maxSubscriptions) = getMicroRosEntityCounts(microRosThreads, connectionMap, invertTopicBinding)
    // rcl logging is needed only if some node routes its own records to /rosout
    val anyRosoutProducer: B = ISZOps(microRosThreads).exists(t => RosUtil.producesRosout(t))
    files = files :+ genMicroRosColconMetaFile(maxPublishers, maxSubscriptions, anyRosoutProducer)
    return files
  }

  // The same datatype package will work regardless of other packages' types
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  def genInterfacesPkg(modelName: String, datatypeMap: Map[AadlType, Ros2Datatype]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genMsgFiles(modelName, datatypeMap)
    files = files :+ genInterfacesCMakeListsFile(modelName, datatypeMap)
    files = files :+ genInterfacesPackageFile(modelName)

    return files
  }

  //================================================
  //  R E A D M E
  //================================================

  def genReadme(modelName: String, ros2Threads: ISZ[AadlThread],
                microRosThreads: ISZ[AadlThread]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val ros2PkgName: String = genCppPackageName(modelName)
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val dollar: String = "$"

    val nodeTableRows: ISZ[ST] =
      (for (t <- ros2Threads) yield
        st"| `${genExecutableFileName(genNodeName(t))}` | `${ros2PkgName}` | ROS2 (rclcpp) | ${if (isSporadic(t)) "Sporadic" else "Periodic"} |") ++
      (for (t <- microRosThreads) yield
        st"| `${genExecutableFileName(genNodeName(t))}` | `${microrosPkgName}` | microROS (rclc + rmw_microxrcedds) | ${if (isSporadic(t)) "Sporadic" else "Periodic"} |")

    val safeToEdit: String = CommentTemplate.safeToEditComment_xml

    val content: ST =
      if (microRosThreads.nonEmpty) {
        val microRosRunEntries: ISZ[ST] = for (t <- microRosThreads) yield
          st"""# ${genNodeName(t)} — microROS node
              |RMW_IMPLEMENTATION=rmw_microxrcedds ros2 run ${microrosPkgName} ${genExecutableFileName(genNodeName(t))}
              |"""
        val ros2RunEntries: ISZ[ST] = for (t <- ros2Threads) yield
          st"""# ${genNodeName(t)} — ROS2 node
              |ros2 run ${ros2PkgName} ${genExecutableFileName(genNodeName(t))}
              |"""
        st"""${safeToEdit}
            |
            |# ${modelName} — Mixed ROS2 / microROS Workspace
            |
            |- [Prerequisites](#prerequisites)
            |- [Quick Start](#quick-start)
            |- [Manual Steps](#manual-steps)
            |  - [Build](#build)
            |  - [Run](#run)
            |
            || Node | Package | Type | Dispatch |
            ||---|---|---|---|
            |${(nodeTableRows, "\n")}
            |
            |The microROS node(s) communicate via a micro-XRCE-DDS agent that bridges them to the ROS2 DDS bus.
            |
            |## Prerequisites
            |
            |- [ROS2 Humble](https://docs.ros.org/en/humble/Installation.html)
            |- micro-ROS Firmware Workspace (one-time setup)
            |
            |  microROS nodes require a firmware workspace containing the micro-ROS client stack and agent.
            |  This workspace is built once and shared across all your microROS projects — set `MICROROS_WS`
            |  to a stable location outside any individual project and reuse it everywhere.
            |
            |  **Step 1 — choose a location** (edit this, then add it to your shell profile):
            |
            |  ```bash
            |  export MICROROS_WS=/path/to/microros_ws
            |  ```
            |
            |  **Step 2 — build the firmware workspace** (copy-paste as-is once `MICROROS_WS` is set):
            |
            |  ```bash
            |  mkdir -p ${dollar}MICROROS_WS && cd ${dollar}MICROROS_WS
            |  source /opt/ros/${dollar}ROS_DISTRO/setup.bash
            |
            |  # 1. Add micro_ros_setup and build it
            |  git clone -b ${dollar}ROS_DISTRO https://github.com/micro-ROS/micro_ros_setup.git src/micro_ros_setup
            |  colcon build --packages-select micro_ros_setup
            |  source install/setup.bash
            |
            |  # 2. Download the micro-ROS client stack
            |  ros2 run micro_ros_setup create_firmware_ws.sh host
            |
            |  # 3. Ignore packages with known build failures that are not needed
            |  touch src/ros2/example_interfaces/COLCON_IGNORE
            |  touch src/uros/micro-ROS-demos/COLCON_IGNORE
            |
            |  # 4. Build the full micro-ROS stack (takes a while, but only done once)
            |  ros2 run micro_ros_setup build_firmware.sh
            |  source install/setup.bash
            |
            |  # 5. Build the micro-XRCE-DDS agent
            |  ros2 run micro_ros_setup create_agent_ws.sh
            |  ros2 run micro_ros_setup build_agent.sh
            |  source install/setup.bash
            |  ```
            |
            |## Quick Start
            |
            |Run from this directory with `MICROROS_WS` set.
            |
            || Target | Description |
            ||---|---|
            || `make` | Build everything and launch all nodes in separate terminals |
            || `make run` | Same as `make` |
            || `make stop` | Kill all running nodes |
            || `make clean` | Remove local build artifacts and copied packages from `MICROROS_WS` |
            || `make microros-config` | Apply `microros_apps/colcon.meta` to `MICROROS_WS` and rebuild the micro-ROS stack (see below) |
            |
            |## Firmware Configuration
            |
            |`microros_apps/colcon.meta` holds the build configuration the generated nodes need
            |from the micro-ROS middleware.  It is **not** consumed from where it sits: it
            |configures packages such as `rcl` and `rmw_microxrcedds`, which live in the firmware
            |workspace and are built by step 4 above -- not by `make build`, which builds only the
            |application packages.  Applying it is therefore a separate step:
            |
            |```bash
            |make microros-config
            |```
            |
            |Run it once after generating, and again whenever `colcon.meta` changes.  Two of its
            |settings matter in ways that fail quietly if it is never applied:
            |
            |- `RCL_COMMAND_LINE_ENABLED=ON` -- the micro-ROS fork of `rcl` strips its
            |  argument-parsing machinery by default.  Without this flag, the rcl arguments in each
            |  node's `node_options` block (topic remap rules in particular) are parsed by nothing.
            |- `RMW_UXRCE_MAX_PUBLISHERS` / `RMW_UXRCE_MAX_SUBSCRIPTIONS` -- these are derived from
            |  the model's port counts and regenerated on every codegen run.  If the firmware's
            |  pools are smaller than the nodes need, entity creation fails without a diagnostic,
            |  so re-apply after adding ports.
            |
            |Entries outside the marked blocks in `colcon.meta` are derived from the model and are
            |overwritten on each run; the marked blocks (build profile, transport and tuning) are
            |preserved.  To override a derived value, restate its `-D` flag inside a marked block --
            |colcon passes `cmake-args` through in order and CMake takes the last occurrence.
            |
            |Because `MICROROS_WS` is shared across projects, `make microros-config` backs up any
            |`colcon.meta` already there to `colcon.meta.bak`.  If you maintain your own firmware
            |configuration, merge the two rather than letting one replace the other.
            |
            |### On a host workspace this step is effectively a no-op
            |
            |A firmware workspace created for the **host** platform
            |(`create_firmware_ws.sh host generic`) does not check out `rcl` at all -- on host,
            |micro-ROS is `rmw_microxrcedds` and `rclc` layered over the ROS 2 distribution's own
            |`rcl`, so there is no micro-ROS `rcl` to configure.  `make microros-config` will copy
            |`colcon.meta` into place and rebuild successfully, but the `rcl` entry matches no
            |package and is silently inert; `find_package(rcl)` keeps resolving to
            |`/opt/ros/$$ROS_DISTRO`.
            |
            |This is usually invisible, because the distribution's `rcl` is built with both
            |argument parsing and logging enabled -- the very things the flags above turn on.  So
            |remap rules and `/rosout` routing work on host whether or not this step is ever run.
            |They stop working the moment the same model is deployed to an embedded target, where
            |the micro-ROS `rcl` fork is used and ships with both features off.  Applying the
            |configuration matters there, and an embedded workspace
            |(`create_firmware_ws.sh <rtos> <board>`) does check `rcl` out, under
            |`firmware/mcu_ws`.
            |
            |One setting to revisit when moving off host: `RCL_LOGGING_IMPLEMENTATION` is emitted
            |as `rcl_logging_spdlog`, which suits a host build.  Embedded targets generally want
            |`rcl_logging_noop`.
            |
            |## Manual Steps
            |
            |The Makefile targets automate the following steps.
            |
            |### Build
            |
            |Run from this directory. Requires `MICROROS_WS` to be set to the firmware workspace above.
            |
            |```bash
            |source /opt/ros/${dollar}ROS_DISTRO/setup.bash && source ${dollar}MICROROS_WS/install/setup.bash
            |
            |# Copy the interfaces and microROS app into the firmware workspace and build them
            |cp -r src/${ros2PkgName}_interfaces ${dollar}MICROROS_WS/src/
            |cp -r microros_apps/${microrosPkgName} ${dollar}MICROROS_WS/src/
            |cd ${dollar}MICROROS_WS && colcon build --packages-select ${ros2PkgName}_interfaces ${microrosPkgName}
            |
            |# Build the ROS2 packages from this workspace
            |cd - && colcon build
            |source install/setup.bash
            |```
            |
            |### Run
            |
            |Each terminal requires:
            |
            |```bash
            |source /opt/ros/${dollar}ROS_DISTRO/setup.bash && source ${dollar}MICROROS_WS/install/setup.bash
            |```
            |
            |Terminals running ROS2 nodes also need:
            |
            |```bash
            |source <path-to-this-workspace>/install/setup.bash
            |```
            |
            |Start the agent before the microROS node(s).
            |
            |```bash
            |# Terminal 1 — micro-XRCE-DDS agent (must start first)
            |ros2 run micro_ros_agent micro_ros_agent udp4 --port 8888
            |
            |${(microRosRunEntries, "\n")}
            |${(ros2RunEntries, "\n")}```
            """
      } else {
        val ros2RunEntries: ISZ[ST] = for (t <- ros2Threads) yield
          st"""# ${genNodeName(t)}
              |ros2 run ${ros2PkgName} ${genExecutableFileName(genNodeName(t))}
              |"""
        st"""${safeToEdit}
            |
            |# ${modelName} — ROS2 Workspace
            |
            |## Table of Contents
            |
            |- [Quick Start](#quick-start)
            |- [Manual Steps](#manual-steps)
            |  - [Build](#build)
            |  - [Run](#run)
            |
            || Node | Package | Dispatch |
            ||---|---|---|
            |${(for (t <- ros2Threads) yield st"| `${genExecutableFileName(genNodeName(t))}` | `${ros2PkgName}` | ${if (isSporadic(t)) "Sporadic" else "Periodic"} |", "\n")}
            |
            |## Quick Start
            |
            || Target | Description |
            ||---|---|
            || `make` | Build and launch all nodes in separate terminals |
            || `make run` | Same as `make` |
            || `make stop` | Kill all running nodes |
            || `make clean` | Remove build artifacts |
            |
            |## Manual Steps
            |
            |The Makefile targets automate the following steps.
            |
            |### Build
            |
            |```bash
            |source /opt/ros/${dollar}ROS_DISTRO/setup.bash
            |colcon build
            |source install/setup.bash
            |```
            |
            |### Run
            |
            |Each terminal requires:
            |
            |```bash
            |source /opt/ros/${dollar}ROS_DISTRO/setup.bash && source install/setup.bash
            |```
            |
            |```bash
            |${(ros2RunEntries, "\n")}```
            """
      }

    return (IS("readme.md"), content, F, IS())
  }

  //================================================
  //  M A K E F I L E
  //================================================

  def genMakefile(modelName: String, ros2Threads: ISZ[AadlThread],
                  microRosThreads: ISZ[AadlThread]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val ros2PkgName: String = genCppPackageName(modelName)
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val safeToEdit: String = CommentTemplate.safeToEditComment_hash

    val tab: String = "\t"

    val content: ST =
      if (microRosThreads.nonEmpty) {
        val runLines: ISZ[ST] =
          ISZ[ST](
            st"""${tab}gnome-terminal --title="[agent] micro-XRCE-DDS" -- bash -c "$$(SOURCE_BASE); ros2 run micro_ros_agent micro_ros_agent udp4 --port 8888; exec bash"""",
            st"${tab}sleep 2"
          ) ++
          (for (t <- microRosThreads) yield
            st"""${tab}gnome-terminal --title="[microROS] ${genExecutableFileName(genNodeName(t))}" -- bash -c "$$(SOURCE_BASE); RMW_IMPLEMENTATION=rmw_microxrcedds ros2 run $$(MICROROS_PKG) ${genExecutableFileName(genNodeName(t))} $$(ROS_ARGS); exec bash"""") ++
          (for (t <- ros2Threads) yield
            st"""${tab}gnome-terminal --title="[ROS2] ${genExecutableFileName(genNodeName(t))}" -- bash -c "$$(SOURCE_BASE); $$(SOURCE_LOCAL); ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))} $$(ROS_ARGS); exec bash"""")

        val stopLines: ISZ[ST] =
          ISZ[ST](st"${tab}-pkill -f 'micro_ros_agent udp4' 2>/dev/null || true") ++
          (for (t <- microRosThreads) yield
            st"${tab}-pkill -f 'ros2 run $$(MICROROS_PKG) ${genExecutableFileName(genNodeName(t))}' 2>/dev/null || true") ++
          (for (t <- ros2Threads) yield
            st"${tab}-pkill -f 'ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))}' 2>/dev/null || true")

        st"""${safeToEdit}
            |
            |MICROROS_WS ?= $$(error MICROROS_WS is not set. Export it to the path of your micro-ROS firmware workspace.)
            |ROS2_PKG       := ${ros2PkgName}
            |INTERFACES_PKG := ${ros2PkgName}_interfaces
            |MICROROS_PKG   := ${microrosPkgName}
            |
            |# Extra rcl arguments appended to every `ros2 run` below, e.g.
            |#   make run ROS_ARGS="--ros-args -p log_file_name:=uros-demo.txt"
            |# Node parameters must be declared by the node before a value here takes effect.
            |ROS_ARGS ?=
            |
            |SOURCE_BASE  := source /opt/ros/$$$${ROS_DISTRO}/setup.bash && source $$(MICROROS_WS)/install/setup.bash
            |SOURCE_LOCAL := source $$(CURDIR)/install/setup.bash
            |
            |.PHONY: all build build-microros build-ros2 microros-config run stop clean check-ros2
            |
            |all: run
            |
            |build: check-ros2 build-microros build-ros2
            |
            |# Applies microros_apps/colcon.meta to the firmware workspace and rebuilds the
            |# micro-ROS stack with it.  This is NOT part of `make build`: colcon.meta configures
            |# the micro-ROS middleware (rcl, rmw_microxrcedds, ...), which lives in MICROROS_WS
            |# and is built once, not the application packages built from here.
            |#
            |# Run this once after generating, and again whenever colcon.meta changes -- notably
            |# after adding ports, since the RMW_UXRCE_MAX_* pool sizes are derived from the model.
            |# Until it is run, RCL_COMMAND_LINE_ENABLED is off in the firmware and the rcl
            |# arguments in each node's node_options block are silently ignored.
            |#
            |# MICROROS_WS is shared across projects, so any existing colcon.meta there is backed
            |# up rather than discarded.
            |microros-config: check-ros2
            |	@test -f "$$(MICROROS_WS)/colcon.meta" && cp "$$(MICROROS_WS)/colcon.meta" "$$(MICROROS_WS)/colcon.meta.bak" && echo "Backed up existing colcon.meta to colcon.meta.bak" || true
            |	cp microros_apps/colcon.meta $$(MICROROS_WS)/colcon.meta
            |	cd $$(MICROROS_WS) && bash -c "$$(SOURCE_BASE); ros2 run micro_ros_setup build_firmware.sh"
            |	@echo "Firmware rebuilt with microros_apps/colcon.meta. Re-run 'make build' to rebuild the app packages against it."
            |
            |check-ros2:
            |	@test -n "$$$${ROS_DISTRO}" || { echo "ERROR: ROS_DISTRO is not set. Source a ROS2 installation first (e.g., source /opt/ros/jazzy/setup.bash)."; exit 1; }
            |	@test -f "/opt/ros/$$$${ROS_DISTRO}/setup.bash" || { echo "ERROR: /opt/ros/$$$${ROS_DISTRO}/setup.bash not found."; exit 1; }
            |
            |build-microros:
            |	cp -r src/$$(INTERFACES_PKG) $$(MICROROS_WS)/src/
            |	cp -r microros_apps/$$(MICROROS_PKG) $$(MICROROS_WS)/src/
            |	cd $$(MICROROS_WS) && bash -c "$$(SOURCE_BASE); colcon build --packages-select $$(INTERFACES_PKG) $$(MICROROS_PKG)"
            |
            |build-ros2:
            |	bash -c "$$(SOURCE_BASE); colcon build; $$(SOURCE_LOCAL)"
            |
            |run: build
            |${(runLines, "\n")}
            |	@echo "Nodes launched. Run 'make stop' to kill them."
            |
            |stop:
            |${(stopLines, "\n")}
            |
            |clean:
            |	rm -rf build install log
            |	rm -rf $$(MICROROS_WS)/src/$$(INTERFACES_PKG) $$(MICROROS_WS)/src/$$(MICROROS_PKG)
            |	rm -rf $$(MICROROS_WS)/build/$$(INTERFACES_PKG) $$(MICROROS_WS)/build/$$(MICROROS_PKG)
            |	rm -rf $$(MICROROS_WS)/install/$$(INTERFACES_PKG) $$(MICROROS_WS)/install/$$(MICROROS_PKG)
            """
      } else {
        val runLines: ISZ[ST] = for (t <- ros2Threads) yield
          st"""${tab}gnome-terminal --title="[ROS2] ${genExecutableFileName(genNodeName(t))}" -- bash -c "source /opt/ros/$$$${ROS_DISTRO}/setup.bash && source $$(CURDIR)/install/setup.bash && ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))} $$(ROS_ARGS); exec bash""""

        val stopLines: ISZ[ST] = for (t <- ros2Threads) yield
          st"${tab}-pkill -f 'ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))}' 2>/dev/null || true"

        st"""${safeToEdit}
            |
            |ROS2_PKG     := ${ros2PkgName}
            |SOURCE_ROS   := source /opt/ros/$$$${ROS_DISTRO}/setup.bash
            |SOURCE_LOCAL := source $$(CURDIR)/install/setup.bash
            |
            |# Extra rcl arguments appended to every `ros2 run` below, e.g.
            |#   make run ROS_ARGS="--ros-args -p log_file_name:=uros-demo.txt"
            |# Node parameters must be declared by the node before a value here takes effect.
            |ROS_ARGS ?=
            |
            |.PHONY: all build run stop clean check-ros2
            |
            |all: run
            |
            |check-ros2:
            |	@test -n "$$$${ROS_DISTRO}" || { echo "ERROR: ROS_DISTRO is not set. Source a ROS2 installation first (e.g., source /opt/ros/jazzy/setup.bash)."; exit 1; }
            |	@test -f "/opt/ros/$$$${ROS_DISTRO}/setup.bash" || { echo "ERROR: /opt/ros/$$$${ROS_DISTRO}/setup.bash not found."; exit 1; }
            |
            |build: check-ros2
            |	bash -c "$$(SOURCE_ROS); colcon build; $$(SOURCE_LOCAL)"
            |
            |run: build
            |${(runLines, "\n")}
            |	@echo "Nodes launched. Run 'make stop' to kill them."
            |
            |stop:
            |${(stopLines, "\n")}
            |
            |clean:
            |	rm -rf build install log
            """
      }

    return (IS("Makefile"), content, F, IS())
  }
}
