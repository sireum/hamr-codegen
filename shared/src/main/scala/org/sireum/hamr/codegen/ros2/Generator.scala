// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{BlockMarker, Marker}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlPort, AadlProcess, AadlSystem, AadlThread, Dispatch_Protocol}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlType, BaseType, EnumType, RecordType}
import org.sireum.hamr.ir.Direction
import org.sireum.message.Reporter
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

  def genPortDatatype(port: AadlPort, packageName: String, datatypeMap: Map[AadlType, (String, ISZ[String])], reporter: Reporter): String = {
    val s: String = port match {
      case dp: AadlDataPort =>
        val dtype = datatypeMap.get(dp.aadlType)
        if (dtype.nonEmpty) {
          s"${packageName}_interfaces::msg::${dtype.get._1}"
        }
        else {
          reporter.error(None(), toolName, s"Port ${port.identifier}: datatype unknown, setting datatype to Empty")
          s"${packageName}_interfaces::msg::Empty"
        }
      case edp: AadlEventDataPort =>
        val dtype = datatypeMap.get(edp.aadlType)
        if (dtype.nonEmpty) {
          s"${packageName}_interfaces::msg::${dtype.get._1}"
        }
        else {
          reporter.error(None(), toolName, s"Port ${port.identifier}: datatype unknown, setting datatype to Empty")
          s"${packageName}_interfaces::msg::Empty"
        }
      case _ => s"${packageName}_interfaces::msg::Empty"
    }
    return s
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

  def genCppCMakeListsEntryPointDecl(modelName: String,
                                     componentName: String, hasEnumConverter: B): ST = {
    val node_executable_file_nameT = genExecutableFileName(componentName)

    var source_files: ISZ[String] = IS()
    source_files = source_files :+ s"src/base_code/${componentName}_runner.cpp"
    source_files = source_files :+ s"src/user_code/${componentName}_src.cpp"
    source_files = source_files :+ s"src/base_code/${componentName}_base_src.cpp"

    if (hasEnumConverter) {
      source_files = source_files :+s"src/base_code/enum_converter.cpp"
    }

    val packages: ISZ[String] = IS(s"${genCppPackageName(modelName)}_interfaces")

    val entryPointDecl: ST =
      st"""add_executable(${node_executable_file_nameT} ${(source_files, " ")})
          |ament_target_dependencies(${node_executable_file_nameT} rclcpp ${(packages, " ")})"""
    return entryPointDecl
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/CMakeLists.txt
  def genCppCMakeListsFile(modelName: String, threadComponents: ISZ[AadlThread], hasEnumConverter: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"

    // build entry point declarations
    var entry_point_decls: ISZ[ST] = IS()
    var entry_point_executables: ISZ[String] = IS()
    for (comp <- threadComponents) {
      entry_point_decls =
        entry_point_decls :+ genCppCMakeListsEntryPointDecl(modelName, genNodeName(comp), hasEnumConverter)
      entry_point_executables =
        entry_point_executables :+ genExecutableFileName(genNodeName(comp))
    }

    val packages: ISZ[String] = IS(s"${top_level_package_nameT}_interfaces")
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
  def genCppPackageFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    val marker = BlockMarker(
      id = "Additions within these tags will be preserved when re-running Codegen",
      beginPrefix = "<!--",
      optBeginSuffix = Some("-->"),
      endPrefix = "<!--",
      optEndSuffix = Some("-->")
    )

    val packages: ISZ[String] = IS(s"${top_level_package_nameT}_interfaces")
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

  def genLaunchPackageFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

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
          |    <exec_depend>${top_level_package_nameT}</exec_depend>
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
    val s =
      st"""
          |${launch_node_decl_nameT} = Node(
          |   package = ${top_level_package_nameT},
          |   executable = ${node_executable_file_nameT}
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

  // Generate node launch code
  //   Example:
  //     <node pkg="tc_cpp_pkg" exec="tc_test_exe"></node>
  def genXmlFormatLaunchNodeDecl(top_level_package_nameT: String,
                                 thread: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(genNodeName(thread))
    val s =
      st"""
          |<node pkg="${top_level_package_nameT}" exec="${node_executable_file_nameT}">
          |</node>
        """
    return s
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
                             microrosPkgName: String, microRosThreadPaths: Set[ISZ[String]]): ISZ[ST] = {
    var launch_decls: ISZ[ST] = IS()

    for (comp <- component.subComponents) {
      comp match {
        case thread: AadlThread =>
          val pkgName: String = if (microRosThreadPaths.contains(thread.path.toISZ)) microrosPkgName else ros2PkgName
          launch_decls = launch_decls :+ genXmlFormatLaunchNodeDecl(pkgName, thread)
        case system: AadlSystem =>
          launch_decls = launch_decls :+ genXmlFormatLaunchSystemDecl(ros2PkgName, system)
        case process: AadlProcess =>
          launch_decls = launch_decls ++ genXmlFormatLaunchDecls(process, ros2PkgName, microrosPkgName, microRosThreadPaths)
        case _ =>
      }
    }

    return launch_decls
  }

  // For example, see https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_bringup/launch/tc.launch.py
  // Creates a launch file for each system component in the model
  def genXmlFormatLaunchFiles(modelName: String, threadComponents: ISZ[AadlThread],
                              systemComponents: ISZ[AadlSystem],
                              microRosThreads: ISZ[AadlThread]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val ros2PkgName: String = genCppPackageName(modelName)
    val microrosPkgName: String = genMicroRosPackageName(modelName)

    var microRosThreadPaths: Set[ISZ[String]] = Set.empty
    for (t <- microRosThreads) {
      microRosThreadPaths = microRosThreadPaths + t.path.toISZ
    }

    var launchFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (system <- systemComponents) {
      val fileName = genXmlLaunchFileName(system.identifier)

      val launch_decls: ISZ[ST] = genXmlFormatLaunchDecls(system, ros2PkgName, microrosPkgName, microRosThreadPaths)

      val launchFileBody: ST =
        if (microRosThreads.nonEmpty)
          st"""${CommentTemplate.doNotEditComment_xml}
              |
              |<launch>
              |    <!-- micro-ROS agent: bridges rmw_microxrcedds nodes to the ROS2 DDS world -->
              |    <executable cmd="micro_ros_agent udp4 --port 8888" output="screen"/>
              |
              |    ${(launch_decls, "\n")}
              |</launch>
          """
        else
          st"""${CommentTemplate.doNotEditComment_xml}
              |
              |<launch>
              |    ${(launch_decls, "\n")}
              |</launch>
          """

      val filePath: ISZ[String] = IS("src", s"${ros2PkgName}_bringup", "launch", fileName)

      launchFiles = launchFiles :+ (filePath, launchFileBody, T, IS())
    }

    return launchFiles
  }


  //================================================
  //  I n t e r f a c e s  Setup Files
  //================================================
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  // The "Empty" datatype, which has no data fields, is used for event ports

  def genMsgFiles(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var msg_files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    for (datatype <- datatypeMap.entries) {
      msg_files = msg_files :+ genMsgFile(modelName, datatype._2._1, datatype._2._2)
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

  def genInterfacesCMakeListsFile(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"
    var msgTypes: ISZ[String] = IS()
    for (msg <- datatypeMap.valueSet.elements) {
      msgTypes = msgTypes :+ s"msg/${msg._1}.msg"
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
  def genCppTopicSubscription(inPort: AadlPort, nodeName: String, portType: String, outPortNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier

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

  def genCppTopicSubscriptionStrict(inPort: AadlPort, nodeName: String, portType: String, outPortNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier

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
  def genCppTopicPublisher(outPort: AadlPort, portType: String, inPortNames: ISZ[String]): ST = {
    val portName = genPortName(outPort)

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
                                                    datatypeMap: Map[AadlType, (String, ISZ[String])],
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
              |PRINT_INFO("Received ${inDataPort.identifier}: %s", MESSAGE_TO_STRING(${inDataPort.identifier}));"""
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
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));"""
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
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));
                                      |}
                                    """
    }

    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerSporadicStrictWithExamples(inPort: AadlPort, nodeName: String, portType: String,
                                                          inDataPorts: ISZ[AadlPort], packageName: String,
                                                          datatypeMap: Map[AadlType, (String, ISZ[String])],
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
              |PRINT_INFO("Received ${inDataPort.identifier}: %s", MESSAGE_TO_STRING(${inDataPort.identifier}));"""
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
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));"""
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
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));
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
                                   datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                packageName: String, datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                |PRINT_INFO("Received ${inDataPort.identifier}: %s", MESSAGE_TO_STRING(${inDataPort.identifier}));"""
        }
        else {
          exampleUsage =
            st"""${exampleUsage}
                |${dataPortType}::SharedPtr ${inDataPort.identifier} = get_${inDataPort.identifier}();
                |PRINT_INFO("Received ${inDataPort.identifier}: %s", MESSAGE_TO_STRING(${inDataPort.identifier}));"""
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
                               datatypeMap: Map[AadlType, (String, ISZ[String])], hasEnumConverter: B,
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

    for (p <- component.getPorts()) {
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
                            datatypeMap: Map[AadlType, (String, ISZ[String])], strictAADLMode: B,
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
    for (p <- component.getPorts()) {
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
          |${nodeName}::${nodeName}() : Node("${genNodeName(component)}")
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

  def genCppUserNodeHeaderFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, (String, ISZ[String])],
                               strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
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

  def genCppUserNodeCppFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, (String, ISZ[String])],
                            hasConverterFiles: B, strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeSourceName(nodeName)
    var examplePublishers: ISZ[ST] = IS()
    var inDataPorts: ISZ[AadlPort] = IS()

    for (p <- component.getPorts()) {
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

      for (p <- component.getPorts()) {
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
                                           datatypeMap: Map[AadlType, (String, ISZ[String])], reporter: Reporter): ISZ[ST] = {
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
                      datatypeMap: Map[AadlType, (String, ISZ[String])], hasConverterFiles: B, strictAADLMode: B,
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

  def genCppEnumConverterFiles(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])],
                               strictAADLMode: B): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var enumTypes: ISZ[(String, AadlType)] = IS()

    for (key <- datatypeMap.keys) {
      key match {
        case _: EnumType =>
          val datatype: String = datatypeMap.get(key).get._2.apply(0)
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

  def genPyLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    //files = files :+ genXmlFormatLaunchFile(modelName, threadComponents)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

    return files
  }

  def genCppNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                    datatypeMap: Map[AadlType, (String, ISZ[String])], strictAADLMode: B, invertTopicBinding: B,
                    reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = ISZ()

    val converterFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = genCppEnumConverterFiles(modelName, datatypeMap, strictAADLMode)
    val hasConverterFiles: B = (converterFiles.size > 0)

    files = files ++
      genCppNodeFiles(modelName, threadComponents, connectionMap, datatypeMap, hasConverterFiles, strictAADLMode,
                      invertTopicBinding, reporter)
    files = files ++ converterFiles
    files = files :+ genCppExampleTypesFile(modelName, datatypeMap)
    files = files :+ genCppCMakeListsFile(modelName, threadComponents, hasConverterFiles)
    files = files :+ genCppPackageFile(modelName)

    return files
  }

  def genXmlLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread], systemComponents: ISZ[AadlSystem],
                     microRosThreads: ISZ[AadlThread]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genXmlFormatLaunchFiles(modelName, threadComponents, systemComponents, microRosThreads)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

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
      datatypeMap: Map[AadlType, (String, ISZ[String])]): String = {
    val aadlTypeOpt: Option[AadlType] = port match {
      case dp: AadlDataPort => Some(dp.aadlType)
      case edp: AadlEventDataPort => Some(edp.aadlType)
      case _ => None()
    }
    aadlTypeOpt match {
      case Some(aadlType) =>
        aadlType match {
          case _: RecordType =>
            datatypeMap.get(aadlType) match {
              case Some((typeName, _)) => return s"example_${typeName}()"
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
      cppPkgName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): ISZ[ST] = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get._2
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
              val ros2Type = ops.StringOps(datatypeMap.get(nestedType).get._2(0)).split(c => c == ' ')(0)
              if (ros2Type == "string") {
                // TODO: investigate replacing the default heap allocator with a static memory pool allocator
                //       (e.g. via rcutils_allocator_t / micro_ros_utilities) to avoid malloc on embedded targets
                stmts = stmts :+ st"""rosidl_runtime_c__String__assign(&${fieldAccess}.data, "");"""
              } else {
                stmts = stmts :+ st"${fieldAccess}.data = ${cBaseTypeZeroLiteral(ros2Type)};"
              }
            case et: EnumType =>
              val enumContent = datatypeMap.get(nestedType).get._2
              val enumFieldLine = ISZOps(enumContent).filter(l => !ops.StringOps(l).contains("="))(0)
              val enumFieldName = ops.StringOps(enumFieldLine).split(c => c == ' ')(1)
              val firstConstLine = ISZOps(enumContent).filter(l => ops.StringOps(l).contains("="))(0)
              val firstConst = ops.StringOps(ops.StringOps(firstConstLine).split(c => c == ' ')(1)).split(c => c == '=')(0)
              val enumCStructName = cppTypeToCStructName(st"${cppPkgName}_interfaces::msg::${et.simpleName}".render)
              stmts = stmts :+ st"${fieldAccess}.${enumFieldName} = ${enumCStructName}__${firstConst};"
            case _: RecordType =>
              val simpleTypeName = datatypeMap.get(nestedType).get._1
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
      cppPkgName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): ISZ[ST] = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get._2
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
              val ros2Type = ops.StringOps(datatypeMap.get(nestedType).get._2(0)).split(c => c == ' ')(0)
              if (ros2Type == "string") {
                stmts = stmts :+ st"""${fieldAccess}.data = "";"""
              } else {
                stmts = stmts :+ st"${fieldAccess}.data = ${cBaseTypeZeroLiteral(ros2Type)};"
              }
            case et: EnumType =>
              val enumContent = datatypeMap.get(nestedType).get._2
              val enumFieldLine = ISZOps(enumContent).filter(l => !ops.StringOps(l).contains("="))(0)
              val enumFieldName = ops.StringOps(enumFieldLine).split(c => c == ' ')(1)
              val firstConstLine = ISZOps(enumContent).filter(l => ops.StringOps(l).contains("="))(0)
              val firstConst = ops.StringOps(ops.StringOps(firstConstLine).split(c => c == ' ')(1)).split(c => c == '=')(0)
              val enumCppType = st"${cppPkgName}_interfaces::msg::${et.simpleName}".render
              stmts = stmts :+ st"${fieldAccess}.${enumFieldName} = ${enumCppType}::${firstConst};"
            case _: RecordType =>
              val simpleTypeName = datatypeMap.get(nestedType).get._1
              stmts = stmts :+ st"${fieldAccess} = example_${simpleTypeName}();"
            case _ =>
          }
        case _ =>
      }
    }
    return stmts
  }

  def genMicroRosExampleTypesFile(modelName: String, cppPkgName: String,
      datatypeMap: Map[AadlType, (String, ISZ[String])]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName = genMicroRosPackageName(modelName)
    val interfacesPkg = st"${cppPkgName}_interfaces".render

    var includes: ISZ[ST] = IS()
    var forwardDecls: ISZ[ST] = IS()
    var definitions: ISZ[ST] = IS()

    for (key <- datatypeMap.keys) {
      key match {
        case _: RecordType =>
          val typeName = datatypeMap.get(key).get._1
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

  def genCppExampleTypesFile(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val packageName = genCppPackageName(modelName)
    val interfacesPkg = st"${packageName}_interfaces".render

    var includes: ISZ[ST] = IS()
    var forwardDecls: ISZ[ST] = IS()
    var definitions: ISZ[ST] = IS()

    for (key <- datatypeMap.keys) {
      key match {
        case _: RecordType =>
          val typeName = datatypeMap.get(key).get._1
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
      datatypeMap: Map[AadlType, (String, ISZ[String])], hasEnumConverter: B): ST = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get._2
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
      datatypeMap: Map[AadlType, (String, ISZ[String])], hasEnumConverter: B,
      reporter: Reporter): Option[ST] = {
    var seen: ISZ[String] = IS()
    var helpers: ISZ[ST] = IS()
    for (p <- component.getPorts()) {
      val portDatatype = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!isEventPort(portDatatype)) {
        if (!ISZOps(seen).contains(portDatatype)) {
          seen = seen :+ portDatatype
          val parts = cppTypeParts(portDatatype)
          val simpleTypeName = parts(parts.size - 1)
          val aadlTypeOpt: Option[AadlType] = p match {
            case dp: AadlDataPort => Some(dp.aadlType)
            case edp: AadlEventDataPort => Some(edp.aadlType)
            case _ => None()
          }
          aadlTypeOpt match {
            case Some(rawType) =>
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

  def lookupAadlTypeByRos2Name(name: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): Option[AadlType] = {
    for (entry <- datatypeMap.entries) {
      if (entry._2._1 == name) {
        return Some(entry._1)
      }
    }
    return None()
  }

  // Returns (printf format pattern, snprintf args) for the given AadlType.
  // accessPath ends with "->" (top-level pointer) or "." (nested struct value), e.g. "msg->" or "msg->degrees."
  def genCMsgFmtArgs(aadlType: AadlType, accessPath: String,
      datatypeMap: Map[AadlType, (String, ISZ[String])], hasEnumConverter: B): (String, ISZ[String]) = {
    val content: ISZ[String] = datatypeMap.get(aadlType).get._2
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
      datatypeMap: Map[AadlType, (String, ISZ[String])], hasEnumConverter: B,
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
          val aadlTypeOpt: Option[AadlType] = p match {
            case dp: AadlDataPort => Some(dp.aadlType)
            case edp: AadlEventDataPort => Some(edp.aadlType)
            case _ => None()
          }
          aadlTypeOpt match {
            case Some(rawType) =>
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
                                      datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                  datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                        datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                    datatypeMap: Map[AadlType, (String, ISZ[String])],
                                    invertTopicBinding: B,
                                    connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                    reporter: Reporter): ISZ[ST] = {
    var inits: ISZ[ST] = IS()
    for (p <- inPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val rosidlSupport = cppTypeToROSIDLSupport(portDatatype)
      val topicName: String =
        if (invertTopicBinding && connectionMap.get(p.path).nonEmpty)
          getPortNames(connectionMap.get(p.path).get)(0)
        else
          getPortNames(IS(p.path.toISZ))(0)
      inits = inits :+
        st"""rclc_subscription_init_default(
            |    &self->${portName}_subscription,
            |    &self->node,
            |    ${rosidlSupport},
            |    "${topicName}");
          """
    }
    return inits
  }

  def genMicroRosHandleForwardDecls(inPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                     datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                        datatypeMap: Map[AadlType, (String, ISZ[String])],
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
        st"rclc_executor_add_subscription(&self->executor, &self->${portName}_subscription, &self->${portName}_msg, ${portName}_subscription_callback, ON_NEW_DATA);"
    }
    return adds
  }

  def genMicroRosGetFunctionDecls(dataInPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                   datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                   datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                                    datatypeMap: Map[AadlType, (String, ISZ[String])],
                                    hasEnumConverter: B, invertTopicBinding: B,
                                    reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeNameBase}${c_src_node_header_name_suffix}"
    val guardName = ops.StringOps(s"${nodeNameBase}_h").toUpper

    val outPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.Out)
    val inPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.In)
    val dataInPorts = ISZOps(inPorts).filter(p => p.isInstanceOf[AadlDataPort])

    var msgTypes: ISZ[String] = IS()
    for (p <- component.getPorts()) {
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
          |${(msgIncludes, "\n")}
          |${enumConverterInclude}
          |${exampleTypesInclude}
          |
          |#define PRINT_INFO(fmt, ...) printf("[INFO] [${nodeName}] " fmt "\n", ##__VA_ARGS__)
          |#define PRINT_WARN(fmt, ...) printf("[WARN] [${nodeName}] " fmt "\n", ##__VA_ARGS__)
          |#define PRINT_ERROR(fmt, ...) printf("[ERROR] [${nodeName}] " fmt "\n", ##__VA_ARGS__)
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
          |void ${nodeNameBase}_init(${nodeNameBase}_t * self);
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
                                datatypeMap: Map[AadlType, (String, ISZ[String])],
                                connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                                invertTopicBinding: B, reporter: Reporter): ISZ[ST] = {
    var inits: ISZ[ST] = IS()
    for (p <- outPorts) {
      val portName = genPortName(p)
      val portDatatype = genPortDatatype(p, cppPkgName, datatypeMap, reporter)
      val rosidlSupport = cppTypeToROSIDLSupport(portDatatype)

      val topicNames: ISZ[String] = if (invertTopicBinding) getPortNames(IS(p.path.toISZ)) else if (connectionMap.get(p.path).nonEmpty) getPortNames(connectionMap.get(p.path).get) else getPortNames(IS(p.path.toISZ))

      if (topicNames.size == 1) {
        inits = inits :+
          st"""rclc_publisher_init_default(
              |    &self->${portName}_publisher,
              |    &self->node,
              |    ${rosidlSupport},
              |    "${topicNames(0)}");
            """
      } else {
        var i: Z = 1
        while (i <= topicNames.size) {
          val topic = topicNames(i - 1)
          inits = inits :+
            st"""rclc_publisher_init_default(
                |    &self->${portName}_publisher_${i},
                |    &self->node,
                |    ${rosidlSupport},
                |    "${topic}");
              """
          i = i + 1
        }
      }
    }
    return inits
  }

  def genMicroRosPutFunctionImpls(outPorts: ISZ[AadlPort], nodeName: String, cppPkgName: String,
                                  datatypeMap: Map[AadlType, (String, ISZ[String])],
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

  def genMicroRosBaseNodeCFile(microrosPkgName: String, cppPkgName: String, component: AadlThread,
                               connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               datatypeMap: Map[AadlType, (String, ISZ[String])],
                               invertTopicBinding: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeNameBase}${c_src_node_name_suffix}"

    val outPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.Out)
    val inPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.In)
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
        st"""#include "${microrosPkgName}/base_headers/${nodeNameBase}${c_src_node_header_name_suffix}"
            |
            |${CommentTemplate.doNotEditComment_slash}
            |
            |// Forward declarations of user compute entry points
            |${(handleForwardDecls, "\n")}
            |
            |// Static instance pointer for subscription callback context (heap-free, MCU-compatible)
            |static ${nodeNameBase}_t * g_self = NULL;
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
            |void ${nodeNameBase}_init(${nodeNameBase}_t * self)
            |{
            |    g_self = self;
            |
            |    self->allocator = rcl_get_default_allocator();
            |
            |    rclc_support_init(&self->support, 0, NULL, &self->allocator);
            |
            |    rclc_node_init_default(&self->node, "${nodeName}", "", &self->support);
            |
            |    // Setting up connections
            |    ${(publisherInits, "\n")}
            |    // Setting up subscriptions
            |    ${(subscriptionInits, "\n")}
            |    rclc_executor_init(&self->executor, &self->support.context, ${numHandles}, &self->allocator);
            |    ${(executorAdds, "\n")}
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
        st"""#include "${microrosPkgName}/base_headers/${nodeNameBase}${c_src_node_header_name_suffix}"
            |
            |${CommentTemplate.doNotEditComment_slash}
            |
            |// Forward declaration of user compute entry point
            |void ${nodeName}_timeTriggered(${nodeNameBase}_t * self);
            |
            |// Static instance pointer for timer callback context (heap-free, MCU-compatible)
            |static ${nodeNameBase}_t * g_self = NULL;
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
            |void ${nodeNameBase}_init(${nodeNameBase}_t * self)
            |{
            |    g_self = self;
            |
            |    self->allocator = rcl_get_default_allocator();
            |
            |    rclc_support_init(&self->support, 0, NULL, &self->allocator);
            |
            |    rclc_node_init_default(&self->node, "${nodeName}", "", &self->support);
            |
            |    // Setting up connections
            |    ${(publisherInits, "\n")}
            |    // timeTriggered callback timer
            |    rclc_timer_init_default(
            |        &self->period_timer,
            |        &self->support,
            |        RCL_MS_TO_NS(${period}),
            |        period_timer_callback);
            |
            |    rclc_executor_init(&self->executor, &self->support.context, 1, &self->allocator);
            |    rclc_executor_add_timer(&self->executor, &self->period_timer);
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
    return (filePath, fileBody, T, IS())
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
          |    ${nodeNameBase}_init(&node);
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
                                     datatypeMap: Map[AadlType, (String, ISZ[String])],
                                     reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeName}${c_src_node_header_name_suffix}"
    val guardName = ops.StringOps(s"${nodeName}_src_h").toUpper

    val computeEntryPointDecls: ST =
      if (isSporadic(component)) {
        val inPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.In)
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
                               datatypeMap: Map[AadlType, (String, ISZ[String])],
                               hasEnumConverter: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val nodeNameBase = s"${nodeName}_base"
    val fileName = s"${nodeName}${c_src_node_name_suffix}"

    val outPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.Out)
    val enumConverterInclude: ST = if (hasEnumConverter) st"""#include "${microrosPkgName}/base_headers/enum_converter.h"""" else st""

    val computeSection: ST =
      if (isSporadic(component)) {
        val inPorts = ISZOps(component.getPorts()).filter(p => p.direction == Direction.In)
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
                  |PRINT_INFO("Sent ${portId}: %s", MESSAGE_TO_STRING(&${portId}));"""
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
                  |PRINT_INFO("Sent ${portId}: %s", MESSAGE_TO_STRING(&${portId}));"""
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
                                    datatypeMap: Map[AadlType, (String, ISZ[String])]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var enumTypes: ISZ[(String, AadlType)] = IS()

    for (key <- datatypeMap.keys) {
      key match {
        case _: EnumType =>
          val datatype: String = datatypeMap.get(key).get._2.apply(0)
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
                                hasEnumConverter: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val interfacesPkg: String = s"${cppPkgName}_interfaces"
    val fileName: String = "CMakeLists.txt"

    var entryPointDecls: ISZ[ST] = IS()
    var entryPointExecutables: ISZ[String] = IS()

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
            |ament_target_dependencies(${execName} rclc ${interfacesPkg})"""
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
          |find_package(rclc REQUIRED)
          |find_package(${interfacesPkg} REQUIRED)
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

  def genMicroRosPackageFile(modelName: String, cppPkgName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val microrosPkgName: String = genMicroRosPackageName(modelName)
    val interfacesPkg: String = s"${cppPkgName}_interfaces"
    val fileName: String = "package.xml"

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
          |    <depend>rclc</depend>
          |    <depend>${interfacesPkg}</depend>
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
                           datatypeMap: Map[AadlType, (String, ISZ[String])],
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
                         datatypeMap: Map[AadlType, (String, ISZ[String])],
                         invertTopicBinding: B, reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val cppPkgName: String = genCppPackageName(modelName)

    val converterFiles = genMicroRosEnumConverterFiles(modelName, cppPkgName, datatypeMap)
    val hasEnumConverter: B = converterFiles.size > 0

    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    files = files ++ genMicroRosNodeFiles(modelName, microRosThreads, connectionMap, datatypeMap, hasEnumConverter, invertTopicBinding, reporter)
    files = files ++ converterFiles
    files = files :+ genMicroRosExampleTypesFile(modelName, cppPkgName, datatypeMap)
    files = files :+ genMicroRosCMakeListsFile(modelName, cppPkgName, microRosThreads, hasEnumConverter)
    files = files :+ genMicroRosPackageFile(modelName, cppPkgName)
    return files
  }

  // The same datatype package will work regardless of other packages' types
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  def genInterfacesPkg(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
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
        st"| `${genExecutableFileName(genNodeName(t))}` | `${ros2PkgName}` | ROS2 (rclcpp) |") ++
      (for (t <- microRosThreads) yield
        st"| `${genExecutableFileName(genNodeName(t))}` | `${microrosPkgName}` | microROS (rclc + rmw_microxrcedds) |")

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
            |- [Prerequisites — micro-ROS Firmware Workspace (one-time setup)](#prerequisites--micro-ros-firmware-workspace-one-time-setup)
            |- [Quick Start](#quick-start)
            |- [Manual Steps](#manual-steps)
            |  - [Build](#build)
            |  - [Run](#run)
            |
            || Node | Package | Type |
            ||---|---|---|
            |${(nodeTableRows, "\n")}
            |
            |The microROS node(s) communicate via a micro-XRCE-DDS agent that bridges them to the ROS2 DDS bus.
            |
            |## Prerequisites — micro-ROS Firmware Workspace (one-time setup)
            |
            |microROS nodes require a firmware workspace containing the micro-ROS client stack and agent.
            |This workspace is built once and shared across all your microROS projects — set `MICROROS_WS`
            |to a stable location outside any individual project and reuse it everywhere.
            |
            |**Step 1 — choose a location** (edit this, then add it to your shell profile):
            |
            |```bash
            |export MICROROS_WS=/path/to/microros_ws
            |```
            |
            |**Step 2 — build the firmware workspace** (copy-paste as-is once `MICROROS_WS` is set):
            |
            |```bash
            |mkdir -p ${dollar}MICROROS_WS && cd ${dollar}MICROROS_WS
            |source /opt/ros/${dollar}ROS_DISTRO/setup.bash
            |
            |# 1. Add micro_ros_setup and build it
            |git clone -b ${dollar}ROS_DISTRO https://github.com/micro-ROS/micro_ros_setup.git src/micro_ros_setup
            |colcon build --packages-select micro_ros_setup
            |source install/setup.bash
            |
            |# 2. Download the micro-ROS client stack
            |ros2 run micro_ros_setup create_firmware_ws.sh host
            |
            |# 3. Ignore packages with known build failures that are not needed
            |touch src/ros2/example_interfaces/COLCON_IGNORE
            |touch src/uros/micro-ROS-demos/COLCON_IGNORE
            |
            |# 4. Build the full micro-ROS stack (takes a while, but only done once)
            |ros2 run micro_ros_setup build_firmware.sh
            |source install/setup.bash
            |
            |# 5. Build the micro-XRCE-DDS agent
            |ros2 run micro_ros_setup create_agent_ws.sh
            |ros2 run micro_ros_setup build_agent.sh
            |source install/setup.bash
            |```
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
            || Node | Package |
            ||---|---|
            |${(for (t <- ros2Threads) yield st"| `${genExecutableFileName(genNodeName(t))}` | `${ros2PkgName}` |", "\n")}
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

    return (IS("README.md"), content, F, IS())
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
            st"""${tab}gnome-terminal --title="[microROS] ${genExecutableFileName(genNodeName(t))}" -- bash -c "$$(SOURCE_BASE); RMW_IMPLEMENTATION=rmw_microxrcedds ros2 run $$(MICROROS_PKG) ${genExecutableFileName(genNodeName(t))}; exec bash"""") ++
          (for (t <- ros2Threads) yield
            st"""${tab}gnome-terminal --title="[ROS2] ${genExecutableFileName(genNodeName(t))}" -- bash -c "$$(SOURCE_BASE); $$(SOURCE_LOCAL); ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))}; exec bash"""")

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
            |SOURCE_BASE  := source /opt/ros/$$$${ROS_DISTRO}/setup.bash && source $$(MICROROS_WS)/install/setup.bash
            |SOURCE_LOCAL := source $$(CURDIR)/install/setup.bash
            |
            |.PHONY: all build build-microros build-ros2 run stop clean check-ros2
            |
            |all: run
            |
            |build: check-ros2 build-microros build-ros2
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
          st"""${tab}gnome-terminal --title="[ROS2] ${genExecutableFileName(genNodeName(t))}" -- bash -c "source /opt/ros/$$$${ROS_DISTRO}/setup.bash && source $$(CURDIR)/install/setup.bash && ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))}; exec bash""""

        val stopLines: ISZ[ST] = for (t <- ros2Threads) yield
          st"${tab}-pkill -f 'ros2 run $$(ROS2_PKG) ${genExecutableFileName(genNodeName(t))}' 2>/dev/null || true"

        st"""${safeToEdit}
            |
            |ROS2_PKG     := ${ros2PkgName}
            |SOURCE_ROS   := source /opt/ros/$$$${ROS_DISTRO}/setup.bash
            |SOURCE_LOCAL := source $$(CURDIR)/install/setup.bash
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
