// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlPort, AadlProcess, AadlSystem, AadlThread, Dispatch_Protocol}
import org.sireum.hamr.codegen.common.types.{AadlType, ArrayType, BaseType, EnumType, RecordType}
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

    val startMarker: String = "# Additions within these tags will be preserved when re-running Codegen"
    val endMarker: String = "# Additions within these tags will be preserved when re-running Codegen"

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

    val setupFileBody =
      st"""cmake_minimum_required(VERSION 3.8)
          |project(${top_level_package_nameT})
          |
          |if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
          |    add_compile_options(-Wall -Wextra -Wpedantic)
          |endif()
          |
          |find_package(ament_cmake REQUIRED)
          |find_package(rclcpp REQUIRED)
          |${(pkgRequirements, "\n")}
          |
          |${startMarker}
          |
          |${endMarker}
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

    return (filePath, setupFileBody, F, IS(Marker(startMarker, endMarker)))
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/package.xml
  def genCppPackageFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    val startMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"
    val endMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"

    val packages: ISZ[String] = IS(s"${top_level_package_nameT}_interfaces")
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
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
          |    ${startMarker}
          |
          |    ${endMarker}
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

    return (filePath, setupFileBody, F, IS(Marker(startMarker, endMarker)))
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

    val startMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"
    val endMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"

    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
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
          |    ${startMarker}
          |
          |    ${endMarker}
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

    return (filePath, setupFileBody, F, IS(Marker(startMarker, endMarker)))
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

  def genXmlFormatLaunchDecls(component: AadlComponent, packageName: String): ISZ[ST] = {
    var launch_decls: ISZ[ST] = IS()

    for (comp <- component.subComponents) {
      comp match {
        case thread: AadlThread =>
          launch_decls = launch_decls :+ genXmlFormatLaunchNodeDecl(packageName, thread)
        case system: AadlSystem =>
          launch_decls = launch_decls :+ genXmlFormatLaunchSystemDecl(packageName, system)
        case process: AadlProcess =>
          launch_decls = launch_decls ++ genXmlFormatLaunchDecls(process, packageName)
        case _ =>
      }
    }

    return launch_decls
  }

  // For example, see https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_bringup/launch/tc.launch.py
  // Creates a launch file for each system component in the model
  def genXmlFormatLaunchFiles(modelName: String, threadComponents: ISZ[AadlThread],
                              systemComponents: ISZ[AadlSystem]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genCppPackageName(modelName)

    var launchFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (system <- systemComponents) {
      val fileName = genXmlLaunchFileName(system.identifier)

      val launch_decls: ISZ[ST] = genXmlFormatLaunchDecls(system, top_level_package_nameT)

      val launchFileBody =
        st"""<launch>
            |    ${(launch_decls, "\n")}
            |</launch>
        """

      val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", "launch", fileName)

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
    msg_files = msg_files :+ (ISZ("src", s"${genCppPackageName(modelName)}_interfaces", "msg", "Empty.msg"), st"", T, IS())
    return msg_files
  }

  def genMsgFile(modelName: String, datatypeName: String, datatypeContent: ISZ[String]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)

    val fileBody = st"${(datatypeContent, "\n")}"

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
                                      |    PRINT_INFO("Received ${handlerName}");
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType}::SharedPtr msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));
                                    """
    }

    if (!exampleUsage.render.value.equals("")) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
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
                                      |    PRINT_INFO("Received ${handlerName}");
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""void ${nodeName}::handle_${handlerName}(const ${portType} msg)
                                      |{
                                      |    // Handle ${handlerName} msg
                                      |    PRINT_INFO("Received ${handlerName}: %s", MESSAGE_TO_STRING(msg));
                                    """
    }

    if (!exampleUsage.render.value.equals("")) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
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
      publisherCode =
        st"""${dataPortType} ${handlerName} = ${dataPortType}();
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

  def genCppTimeTriggeredMethodHeader(): ST = {
    val timeTriggeredHeader: ST =
      st"void timeTriggered();"
    return timeTriggeredHeader
  }

  def genCppTimeTriggeredMethod(nodeName: String, examplePublishers: ISZ[ST]): ST = {
    val timeTriggered: ST =
      st"""void ${nodeName}::timeTriggered()
          |{
          |    // Handle communication
          |
          |    // Example publishing messages
          |    ${(examplePublishers, "\n")}
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
                               datatypeMap: Map[AadlType, (String, ISZ[String])], strictAADLMode: B,
                               invertTopicBinding: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
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
    var msgToString: ST = st""

    for (p <- component.getPorts()) {
      val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!ISZOps(msgTypes).contains(portDatatype)) {
        msgTypes = msgTypes :+ portDatatype
      }
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
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
      st"""#include <queue>"""

    if (strictAADLMode) {
      stdIncludes =
        st"""${stdIncludes}
            |#include <vector>
            |#include <variant>
            |#include <mutex>"""
    }

    var fileBody =
      st"""#include "rclcpp/rclcpp.hpp"
          |${(typeIncludes, "\n")}
          |${(stdIncludes, "\n")}
          |
          |//========================================================
          |// Re-running Codegen will overwrite changes to this file
          |//========================================================
          |
          |class ${nodeName} : public rclcpp::Node
          |{
          |protected:"""

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |    using MsgType = std::variant<${(msgTypes, ", ")}>;
          """

      msgToString = st"#define MESSAGE_TO_STRING(message) ${packageName}_interfaces::msg::to_yaml(message).c_str()"
    } else {
      msgToString = st"#define MESSAGE_TO_STRING(message) ${packageName}_interfaces::msg::to_yaml(*message).c_str()"
    }

    fileBody =
      st"""${fileBody}
          |    ${nodeName}();
          |
          |    //=================================================
          |    //  C o m m u n i c a t i o n
          |    //=================================================
          |
          |    ${msgToString}
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
          else {
            // Out ports with no connections should still publish to a topic
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
          |//========================================================
          |// Re-running Codegen will overwrite changes to this file
          |//========================================================
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

    val startMarker: String = "// Additions within these tags will be preserved when re-running Codegen"
    val endMarker: String = "// Additions within these tags will be preserved when re-running Codegen"

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
          |//========================================================
          |// Re-running Codegen will overwrite changes to this file
          |//========================================================
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

    fileBody =
      st"""${fileBody}
          |    //=================================================
          |    //  Include any additional declarations here
          |    //=================================================
          |    ${startMarker}
          |
          |    ${endMarker}
          |};
          """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "user_headers", fileName)

    return (filePath, fileBody, F, IS(Marker(startMarker, endMarker)))
  }

  def genCppUserNodeCppFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, (String, ISZ[String])],
                            hasConverterFiles: B, strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeSourceName(nodeName)
    var examplePublishers: ISZ[ST] = IS()

    for (p <- component.getPorts()) {
      if (p.direction == Direction.Out) {
        examplePublishers = examplePublishers :+ genCppExamplePublisher(p, packageName, datatypeMap, reporter)
      }
    }

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      var inDataPorts: ISZ[AadlPort] = IS()

      for (p <- component.getPorts()) {
        if (p.direction == Direction.In && p.isInstanceOf[AadlDataPort]) {
          inDataPorts = inDataPorts :+ p
        }
      }

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
      subscriptionHandlers = subscriptionHandlers :+ genCppTimeTriggeredMethod(nodeName, examplePublishers)
    }

    var includeFiles: ST = st"#include \"${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}\""

    if (hasConverterFiles) {
      includeFiles =
        st"""${includeFiles}
            |#include "${packageName}/base_headers/enum_converter.hpp""""
    }

    val fileBody =
      st"""${includeFiles}
          |
          |//===========================================================
          |// This file will not be overwritten when re-running Codegen
          |//===========================================================
          |
          |//=================================================
          |//  I n i t i a l i z e    E n t r y    P o i n t
          |//=================================================
          |void ${nodeName}::initialize()
          |{
          |    PRINT_INFO("Initialize Entry Point invoked");
          |
          |    // Initialize the node
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

  def genCppNodeRunnerFile(packageName: String, component: AadlThread): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genCppNodeRunnerName(nodeName)

    val fileBody =
      st"""#include "${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}"
          |
          |//========================================================
          |// Re-running Codegen will overwrite changes to this file
          |//========================================================
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
        cpp_files :+ genCppBaseNodeHeaderFile(top_level_package_nameT, comp, connectionMap, datatypeMap, strictAADLMode,
                                              invertTopicBinding, reporter)
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

  def genCppEnumConverterHeaderFile(packageName: String, enumTypes: ISZ[(String, AadlType)],
                                    strictAADLMode: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    var includes: ISZ[ST] = IS()
    var converterHeaders: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")

      includes = includes :+ st"#include \"${packageName}_interfaces/msg/${enum._1}.hpp\""

      if (strictAADLMode) {
        converterHeaders = converterHeaders :+
          st"const char* enumToString(${packageName}_interfaces::msg::${enumName} value);"
      }
      else {
        converterHeaders = converterHeaders :+
          st"const char* enumToString(${packageName}_interfaces::msg::${enumName}* value);"
      }
    }

    val fileBody =
      st"""#ifndef ENUM_CONVERTER_HPP
          |#define ENUM_CONVERTER_HPP
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

  def genCppEnumConverters(packageName: String, enumTypes: ISZ[(String, AadlType)], strictAADLMode: B): ISZ[ST] = {
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

      if (strictAADLMode) {
        converters = converters :+
          st"""const char* enumToString(${packageName}_interfaces::msg::${enumName} value) {
              |    switch (value.${enum._1}) {
              |        ${(cases, "\n")}
              |        default:
              |            return "Unknown value for ${enumName}";
              |    }
              |}
        """
      }
      else {
        converters = converters :+
          st"""const char* enumToString(${packageName}_interfaces::msg::${enumName}* value) {
              |    switch (value->${enum._1}) {
              |        ${(cases, "\n")}
              |        default:
              |            return "Unknown value for ${enumName}";
              |    }
              |}
        """
      }
    }

    return converters
  }

  def genCppEnumConverterFile(packageName: String, enumTypes: ISZ[(String, AadlType)],
                              strictAADLMode: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    val fileBody =
      st"""#include "${packageName}/base_headers/enum_converter.hpp"
          |
          |//========================================================
          |// Re-running Codegen will overwrite changes to this file
          |//========================================================
          |
          |${(genCppEnumConverters(packageName, enumTypes, strictAADLMode), "\n")}
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

    files = files :+ genCppEnumConverterHeaderFile(packageName, enumTypes, strictAADLMode)
    files = files :+ genCppEnumConverterFile(packageName, enumTypes, strictAADLMode)

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
    files = files :+ genCppCMakeListsFile(modelName, threadComponents, hasConverterFiles)
    files = files :+ genCppPackageFile(modelName)

    return files
  }

  def genXmlLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread], systemComponents: ISZ[AadlSystem]): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genXmlFormatLaunchFiles(modelName, threadComponents, systemComponents)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

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
}
