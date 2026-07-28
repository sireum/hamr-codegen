// #Sireum
package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.hamr.codegen.common.properties._
import org.sireum.hamr.codegen.common.symbols.{AadlEventDataPort, AadlFeatureData, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, RecordType, TypeUtil}
import org.sireum.message.{Position, Reporter}

// A ROS 2 message type as seen by the generated code.
//
//   name:              the message name (e.g. "TempSensor", "Joy")
//   content:           the .msg file's lines; empty for platform-provided types,
//                      for which codegen emits no .msg file
//   nativePackageOpt:  Some(<ros package>) when the type is platform-provided
//                      (e.g. Some("sensor_msgs")), None() when codegen generates
//                      it into the model's <model>_interfaces package
@datatype class Ros2Datatype(val name: String,
                             val content: ISZ[String],
                             val nativePackageOpt: Option[String]) {

  @strictpure def isPlatformProvided: B = nativePackageOpt.nonEmpty

  // The C++ type the generated code uses as the port/topic payload type, e.g.
  // "sensor_msgs::msg::Joy" for a platform-provided type, or
  // "uros_demo_cpp_pkg_interfaces::msg::TempSensor" for a generated one
  @strictpure def cppType(interfacesPackageName: String): String =
    nativePackageOpt match {
      case Some(nativePackage) => s"${nativePackage}::msg::${name}"
      case _ => s"${interfacesPackageName}::msg::${name}"
    }
}

object RosUtil {
  val toolName: String = "Ros2Codegen"

  @pure def isMicroRos(aadlThread: AadlThread): B = {
    PropertyUtil.getDiscreetPropertyValue(aadlThread.properties, Hamr_Ros_Properties.HAMR_ROS__Ros_Node_Kind) match {
      case Some(ir.ValueProp("microRos")) => return T
      case _ => return F
    }
  }

  @pure def isPlatformProvided(aadlType: AadlType): B = {
    return HamrProperties.getProvenance(aadlType.properties, HamrProperties.HAMR__TYPE_PROVENANCE) ==
      HamrProperties.Provenances.Platform_Provided
  }

  // A platform-provided component is realized by an executable that already exists on the target
  // (a stock node such as turtlesim).  Codegen emits no code for it -- only a launch entry and an
  // exec_depend -- but it stays in the model, so connections, type checking and contract
  // composition still see the true topology.
  @pure def isPlatformProvidedComponent(thread: AadlThread): B = {
    return HamrProperties.getProvenance(thread.properties, HamrProperties.HAMR__COMPONENT_PROVENANCE) ==
      HamrProperties.Provenances.Platform_Provided
  }

  // Resolves the ROS package and executable of a platform-provided component.
  //
  // The structural convention is the type convention one level up: SysML package = ROS package,
  // part def = executable name, so turtlesim::turtlesim_node denotes
  // `ros2 run turtlesim turtlesim_node`.  Native_Name overrides it, taking the two-segment
  // "<package>/<executable>" form here (types use the three-segment "<pkg>/msg/<Type>" form).
  //
  // Both are trust-me strings to verification -- the same stance as mirror fields, applied to a
  // component's identity.
  def getNativeExecutable(thread: AadlThread, reporter: Reporter): Option[(String, String)] = {
    val posOpt = thread.component.identifier.pos
    PropertyUtil.getDiscreetPropertyValue(thread.properties, HamrProperties.HAMR__NATIVE_NAME) match {
      case Some(ir.ValueProp(nativeName)) =>
        val segments = ops.StringOps(nativeName).split(c => c == '/')
        if (segments.size != 2) {
          reporter.error(posOpt, toolName,
            s"${HamrProperties.HAMR__NATIVE_NAME} of the platform-provided component ${thread.identifier} must have the form '<package>/<executable>' but found '$nativeName'")
          return None()
        }
        return Some((segments(0), segments(1)))
      case Some(x) =>
        reporter.error(posOpt, toolName, s"Expecting a string value for ${HamrProperties.HAMR__NATIVE_NAME} but found: $x")
        return None()
      case _ =>
        thread.component.classifier match {
          case Some(classifier) =>
            val segments = ops.StringOps(classifier.name).split(c => c == ':')
            if (segments.size != 2) {
              reporter.error(posOpt, toolName,
                st"""The executable of the platform-provided component ${thread.identifier} cannot be derived from its
                    |classifier '${classifier.name}'.  The structural convention is <ros package>::<executable>, i.e. the
                    |component must be declared directly in a package named after its ROS package.  Use
                    |${HamrProperties.HAMR__NATIVE_NAME} to supply the name explicitly.""".render)
              return None()
            }
            return Some((segments(0), segments(1)))
          case _ =>
            reporter.error(posOpt, toolName,
              s"The platform-provided component ${thread.identifier} has no classifier to derive its executable from; use ${HamrProperties.HAMR__NATIVE_NAME}")
            return None()
        }
    }
  }

  // Codegen emits nothing for a platform-provided component, so "contracts only" holds by
  // construction; what remains is hygiene -- properties that only direct generation are
  // meaningless on it.  Dispatch_Protocol/Period stay legal: they inform scheduling analysis
  // rather than generation.
  def validatePlatformProvidedComponents(threads: ISZ[AadlThread], reporter: Reporter): Unit = {
    for (thread <- threads if isPlatformProvidedComponent(thread)) {
      for (p <- ISZ(HamrProperties.HAMR__IMPLEMENTATION_LANGUAGE, Hamr_Ros_Properties.HAMR_ROS__Ros_Node_Kind)) {
        if (PropertyUtil.getDiscreetPropertyValue(thread.properties, p).nonEmpty) {
          reporter.warn(thread.component.identifier.pos, toolName,
            s"${p} is a generation-only property, but no code is generated for the platform-provided component ${thread.identifier}")
        }
      }
    }
  }

  // The ROS namespace a node is deployed under, or "" when unset (rcl's "no namespace").
  //
  // This is per-node deployment configuration rather than a platform-provided-component feature,
  // so it applies to any thread and is normally set at the usage site; the frontend has already
  // resolved usage-over-definition by the time the property reaches AIR.  The value is passed
  // through to rcl verbatim -- relative names compose with it during rcl's own name expansion,
  // which is why codegen performs no string concatenation of its own.
  @pure def getRosNamespace(thread: AadlThread): String = {
    PropertyUtil.getDiscreetPropertyValue(thread.properties, Hamr_Ros_Properties.HAMR_ROS__Ros_Namespace) match {
      case Some(ir.ValueProp(ns)) => return ns
      case _ => return ""
    }
  }

  def validateRosNamespaces(threads: ISZ[AadlThread], reporter: Reporter): Unit = {
    for (thread <- threads) {
      PropertyUtil.getDiscreetPropertyValue(thread.properties, Hamr_Ros_Properties.HAMR_ROS__Ros_Namespace) match {
        case Some(ir.ValueProp(_)) =>
        case Some(x) =>
          reporter.error(thread.component.identifier.pos, toolName,
            s"Expecting a string value for ${Hamr_Ros_Properties.HAMR_ROS__Ros_Namespace} on ${thread.identifier} but found: $x")
        case _ =>
      }
    }
  }

  // An explicitly modeled topic name for the port, or None() when the port takes the
  // path-derived default.  The frontend has already collapsed a usage-site port refinement over
  // the port definition, so at most one value reaches AIR.
  @pure def getExplicitTopicName(port: AadlPort): Option[String] = {
    PropertyUtil.getDiscreetPropertyValue(port.feature.properties, Hamr_Ros_Properties.HAMR_ROS__Ros_Topic_Name) match {
      case Some(ir.ValueProp(name)) => return Some(name)
      case _ => return None()
    }
  }

  @strictpure def isAbsoluteTopicName(name: String): B =
    ops.StringOps(name).startsWith("/")

  // Resolves a topic name to its fully-qualified form under the given namespace, mirroring rcl's
  // name expansion.  Absolute names are used verbatim; a relative name composes with the
  // namespace, and with no namespace simply gains the leading slash.
  //
  // Codegen only needs this for the *peer* end of an edge: the declaring end passes its literal
  // string through and lets rcl expand it against that node's own namespace, but binding the peer
  // to the same relative string would resolve it under the peer's namespace instead and silently
  // sever the edge.
  @pure def absolutizeTopicName(name: String, namespace: String): String = {
    if (isAbsoluteTopicName(name)) {
      return name
    }
    val ns = ops.StringOps(namespace)
    val trimmed: String =
      if (ns.startsWith("/")) ns.substring(1, namespace.size)
      else namespace
    if (trimmed == "") {
      return s"/${name}"
    }
    return s"/${trimmed}/${name}"
  }

  // `rosout` is a reserved port name: every ROS node has a built-in /rosout log publisher, so a
  // model port so named necessarily denotes that built-in rather than declaring something new.
  // Its declaration must therefore match the built-in's fixed identity (see validateRosoutPorts).
  // A port with any other name carrying rcl_interfaces::Log is entirely ordinary.
  val ROSOUT_PORT_ID: String = "rosout"

  // The platform pins the rosout topic; default (port-path) name derivation does not apply to it
  val ROSOUT_TOPIC: String = "/rosout"

  val ROSOUT_CLASSIFIER: ISZ[String] = ISZ("rcl_interfaces", "Log")

  @strictpure def isRosoutPort(port: AadlPort): B = port.identifier == ROSOUT_PORT_ID

  // An out `rosout` port is realized by the rcl logging infrastructure as a side effect of
  // enabling logging -- never by application code -- so codegen emits no publisher and no put_
  // API for it.  On micro-ROS its presence is additionally what turns rcl logging on.
  @strictpure def isInfrastructureRealized(port: AadlPort): B =
    isRosoutPort(port) && port.direction == ir.Direction.Out

  @strictpure def producesRosout(thread: AadlThread): B =
    ops.ISZOps(thread.getPorts()).exists(p => isInfrastructureRealized(p))

  // A port named `rosout` refers to the platform's built-in log topic, so its declaration must
  // agree with that built-in: an event data port carrying rcl_interfaces::Log.  Port_Provenance
  // is optional on it -- documentation rather than trigger -- but must be Infrastructure when
  // given.  Any disagreement is a validation error rather than something codegen works around.
  def validateRosoutPorts(threads: ISZ[AadlThread], reporter: Reporter): Unit = {
    for (thread <- threads;
         port <- thread.getPorts() if isRosoutPort(port)) {
      port match {
        case p: AadlEventDataPort =>
          if (p.aadlType.classifier != ROSOUT_CLASSIFIER) {
            reporter.error(port.posOpt, toolName,
              st"""The reserved port name '${ROSOUT_PORT_ID}' denotes the platform's built-in log topic, so it must
                  |carry ${(ROSOUT_CLASSIFIER, "::")} but ${thread.identifier}'s carries ${p.aadlType.name}.
                  |Rename the port if an ordinary topic was intended.""".render)
          }
        case _ =>
          reporter.error(port.posOpt, toolName,
            st"""The reserved port name '${ROSOUT_PORT_ID}' denotes the platform's built-in log topic, so it must be
                |an event data port carrying ${(ROSOUT_CLASSIFIER, "::")}.
                |Rename the port if an ordinary port was intended.""".render)
      }

      PropertyUtil.getDiscreetPropertyValue(port.feature.properties, HamrProperties.HAMR__PORT_PROVENANCE) match {
        case Some(ir.ValueProp("Infrastructure")) =>
        case Some(x) =>
          reporter.error(port.posOpt, toolName,
            s"${HamrProperties.HAMR__PORT_PROVENANCE} of the reserved port '${ROSOUT_PORT_ID}' must be Infrastructure but found: $x")
        case _ => // absent is fine -- the reserved name is itself the trigger
      }
    }
  }

  // Resolves the native (ROS) package and message name of a platform-provided type.
  //
  // By default these come from model structure -- SysML/AADL package name = ROS package
  // name and data component name = message name, so sensor_msgs::Joy denotes
  // sensor_msgs/msg/Joy.  An explicit HAMR::Native_Name property overrides that
  // derivation; its value must be of the form "<package>/msg/<Type>".
  def getNativeTypeName(aadlType: AadlType, reporter: Reporter): Option[(String, String)] = {
    val posOpt: Option[Position] = aadlType.container match {
      case Some(c) => c.identifier.pos
      case _ => None()
    }
    PropertyUtil.getDiscreetPropertyValue(aadlType.properties, HamrProperties.HAMR__NATIVE_NAME) match {
      case Some(ir.ValueProp(nativeName)) =>
        val segments = ops.StringOps(nativeName).split(c => c == '/')
        if (segments.size != 3 || segments(1) != "msg") {
          reporter.error(posOpt, toolName,
            s"${HamrProperties.HAMR__NATIVE_NAME} of the platform-provided type ${aadlType.name} must have the form '<package>/msg/<Type>' but found '$nativeName'")
          return None()
        }
        return Some((segments(0), segments(2)))
      case Some(x) =>
        reporter.error(posOpt, toolName, s"Expecting a string value for ${HamrProperties.HAMR__NATIVE_NAME} but found: $x")
        return None()
      case _ =>
        if (aadlType.classifier.size != 2) {
          reporter.error(posOpt, toolName,
            st"""The native name of the platform-provided type ${aadlType.name} cannot be derived from its classifier.
                |The structural convention is <ros package>::<Message>, i.e. the type must be declared directly in a
                |package named after its ROS package.  Use ${HamrProperties.HAMR__NATIVE_NAME} to supply the name explicitly.""".render)
          return None()
        }
        return Some((aadlType.classifier(0), aadlType.classifier(1)))
    }
  }

  // Returns the types reachable from the model's ports, ordered so that a type always
  // follows the types it depends on.
  //
  // Platform-provided types are collected but not descended into: their fields are
  // specification-level mirrors rather than a layout claim, so codegen never emits them
  // (nor anything they reference).  They carry no dependencies and are therefore appended
  // after the ordered generated types.
  def getTouchedTypes(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ISZ[AadlType] = {
    var generated: Set[AadlType] = Set.empty
    var platformProvided: Set[AadlType] = Set.empty

    def add(posOpt: Option[Position], aadlType: AadlType): Unit = {
      if (isPlatformProvided(aadlType)) {
        platformProvided = platformProvided + aadlType
        return
      }

      aadlType match {
        case t: ArrayType =>
          add(posOpt, t.baseType)
        case t: RecordType =>
          for (f <- t.fields.values) {
            if (isPlatformProvided(f)) {
              reporter.error(posOpt, toolName,
                s"Platform-provided type ${f.name} is used as a field of the generated type ${t.name}, which is not currently supported")
            }
            add(posOpt, f)
          }
        case _ =>
      }

      generated = generated + aadlType
    }

    for (thread <- symbolTable.getThreads();
         port <- thread.getPorts()) {
      port match {
        case d: AadlFeatureData => add(port.feature.identifier.pos, d.aadlType)
        case _ =>
      }
    }

    val orderedGenerated: ISZ[AadlType] =
      if (generated.isEmpty) ISZ()
      else for (typeName <- TypeUtil.orderTypeDependencies(generated.elements)) yield aadlTypes.typeMap.get(typeName).get

    return orderedGenerated ++ platformProvided.elements
  }
}
