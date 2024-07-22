// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, ResourceUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, Component, ConnectionInstance, Direction, UnitProp, ValueProp}
import org.sireum.message.Reporter
import org.sireum.ops.ISZOps

object Generator {

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

  // TODO: Reentrant or mutually exclusive (or single-threaded executor)?
  // This value will work for Python and C++ code
  val callback_group_type: String = "Reentrant"
  // TODO: Confirm this name (and maybe remove based on how callback groups are done)
  val callback_group_name: String = "cb_group_"
  val subscription_options_name: String = "subscription_options_"

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

  def isSporadic(component: AadlThread): Boolean = {
    return component.dispatchProtocol == Dispatch_Protocol.Sporadic
  }

  def getPortNames(portNames: ISZ[ISZ[String]]): ISZ[String] = {
    var names: ISZ[String] = IS()
    for (name <- portNames) {
      names = names :+ seqToString(name, "_")
    }
    return names
  }

  def seqToString(seq: ISZ[String], separator: String = ""): String = {
    var str = ""
    for (s <- seq) {
      str = str.toString + s.toString + separator.toString
    }
    str = str.toString.stripSuffix(separator.toString)
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
      val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(comp.pathAsString("_"))
      entry_point_decls =
        entry_point_decls :+ genPySetupEntryPointDecl(modelName, comp.pathAsString("_"))
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

  // TODO: Datatype stuff
  //  Example:
  //   "add_executable(ts_exe src/ts_src.cpp)
  //    ament_target_dependencies(ts_exe rclcpp example_interfaces)"
  def genCppCMakeListsEntryPointDecl(modelName: String,
                                     componentName: String): ST = {
    val node_executable_file_nameT = genExecutableFileName(componentName)

    var source_files: ISZ[String] = IS()
    source_files = source_files :+ s"src/base_code/${componentName}_runner.cpp"
    source_files = source_files :+ s"src/user_code/${componentName}_src.cpp"
    source_files = source_files :+ s"src/base_code/${componentName}_base_src.cpp"

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up (into a map, component as key?) while looping through each port
    val packages: ISZ[String] = IS("example_interfaces")

    // TODO: Add interface type packages after rclcpp
    val entryPointDecl: ST =
      st"""add_executable(${node_executable_file_nameT} ${(source_files, " ")})
          |ament_target_dependencies(${node_executable_file_nameT} rclcpp ${(packages, " ")})"""
    return entryPointDecl
  }

  // TODO: Datatype stuff
  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/CMakeLists.txt
  def genCppCMakeListsFile(modelName: String, threadComponents: ISZ[AadlThread]): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"

    // build entry point declarations
    var entry_point_decls: ISZ[ST] = IS()
    var entry_point_executables: ISZ[String] = IS()
    for (comp <- threadComponents) {
      entry_point_decls =
        entry_point_decls :+ genCppCMakeListsEntryPointDecl(modelName, comp.pathAsString("_"))
      entry_point_executables =
        entry_point_executables :+ genExecutableFileName(comp.pathAsString("_"))
    }

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up while looping through each port
    val packages: ISZ[String] = IS("example_interfaces")
    val pkgRequirements: ISZ[ST] = genCMakeListsPkgRequirements(packages)

    // TODO: Add 'find interface type packages' under rclcpp
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

    return (filePath, setupFileBody)
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControlcpp_ws/src/tc_cpp_pkg/package.xml
  def genCppPackageFile(modelName: String): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up while looping through each port
    val packages: ISZ[String] = IS("example_interfaces")
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    // TODO: Add interface type package dependencies under rclcpp
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
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody)
  }


  //================================================
  //  L a u n c h  File Setup Files
  //================================================

  def genLaunchCMakeListsFile(modelName: String): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "CMakeLists.txt"

    // TODO: Add 'find interface type packages' under rclcpp
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

    val filePath: ISZ[String] = IS("src", top_level_package_nameT.toString() + "_bringup", fileName)

    return (filePath, setupFileBody)
  }

  def genLaunchPackageFile(modelName: String): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genCppPackageName(modelName)
    val fileName: String = "package.xml"

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up while looping through each port
    val packages: ISZ[String] = IS("example_interfaces")
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    // TODO: Add interface type package dependencies under rclcpp
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
          |    <test_depend>ament_lint_auto</test_depend>
          |    <test_depend>ament_lint_common</test_depend>
          |
          |    <export>
          |        <build_type>ament_cmake</build_type>
          |    </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT.toString() + "_bringup", fileName)

    return (filePath, setupFileBody)
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
    val node_executable_file_nameT = genExecutableFileName(component.pathAsString("_"))
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
      val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(comp.pathAsString("_"))
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

    val filePath: ISZ[String] = IS("src", modelName.toString() + "_bringup", "launch", fileName)

    return (filePath, launchFileBody)
  }

  //================================================
  //  L a u n c h  File (XML Format)
  //================================================

  // genLaunchNodeDecl() - generate node declaration
  //   Example:
  //     <node pkg="tc_cpp_pkg" exec="tc_test_exe"></node>
  def genXmlFormatLaunchNodeDecl(top_level_package_nameT: String,
                                 component: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(component.pathAsString("_"))
    val s =
      st"""
          |<node pkg="${top_level_package_nameT}" exec="${node_executable_file_nameT}">
          |</node>
        """
    return s
  }

  // For example, see https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_bringup/launch/tc.launch.py
  def genXmlFormatLaunchFile(modelName: String, threadComponents: ISZ[AadlThread]): (ISZ[String], ST) = {
    val fileName = genXmlLaunchFileName(modelName)

    val top_level_package_nameT: String = genCppPackageName(modelName)

    var node_decls: ISZ[ST] = IS()

    for (comp <- threadComponents) {
      node_decls = node_decls :+ genXmlFormatLaunchNodeDecl(top_level_package_nameT, comp)
    }

    val launchFileBody =
      st"""<launch>
          |    ${(node_decls, "\n")}
          |</launch>
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT.toString() + "_bringup", "launch", fileName)

    return (filePath, launchFileBody)
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
      includes = includes :+ st"#include \"${msgType}\""
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
  //  temp_control_currentTemp_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
  //    "temp_control_currentTemp",
  //     10,
  //     std::bind(&TempControl::handle_currentTemp, this, std::placeholders::_1));
  def genCppTopicSubscription(inPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    val portName = seqToString(inPort.path, "_")
    val handlerName = inPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // Probably: if strict then portName = inPort.name

    // Int32 is a placeholder message value
    val portCode: ST =
      st"""${portName}_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
          |    "${portName}",
          |    10,
          |    std::bind(&${nodeName}::handle_${handlerName}, this, std::placeholders::_1), ${subscription_options_name});
        """
    return portCode
  }

  // Example:
  //  temp_control_currentTemp_publisher_ = this->create_publisher<example_interfaces::msg::Int32>(
  //    "operator_interface_currentTemp",
  //     10);
  def genCppTopicPublisher(outPort: AadlPort, nodeName: String, inPortNames: ISZ[String]): ST = {
    val portName = seqToString(outPort.path, "_")

    if (inPortNames.size == 1) {
      val inPortName = inPortNames.apply(0)

      // Int32 is a placeholder message value
      val portCode: ST =
        st"""${portName}_publisher_ = this->create_publisher<example_interfaces::msg::Int32>(
            |    "${inPortName}",
            |    10);
          """
      return portCode
    }

    // If the port is a fan out port
    var outputInstances: ISZ[ST] = IS()
    var counter = 1

    for (inPortName <- inPortNames) {
      outputInstances = outputInstances :+
        st"""${portName}_publisher_${counter} = this->create_publisher<example_interfaces::msg::Int32>(
            |    "${inPortName}",
            |    10);
          """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(outputInstances, "\n")}"

    return fanPortCode
  }

  // Example:
  //  rclcpp::Subscription<example_interfaces::msg::Int32>::SharedPtr temp_control_currentTemp_subscription;
  def genCppTopicSubscriptionVarHeader(inPort: AadlPort): ST = {
    val portName = seqToString(inPort.path, "_")

    // Int32 is a placeholder message value
    val varHeader: ST =
      st"rclcpp::Subscription<example_interfaces::msg::Int32>::SharedPtr ${portName}_subscription_;"
    return varHeader
  }

  // Example:
  //  rclcpp::Publisher<example_interfaces::msg::Int32>::SharedPtr temp_control_currentTemp_publisher;
  def genCppTopicPublisherVarHeader(outPort: AadlPort, inputPortCount: Z): ST = {
    val portName = seqToString(outPort.path, "_")

    if (inputPortCount == 1) {
      // Int32 is a placeholder message value
      val varHeader: ST =
        st"rclcpp::Publisher<example_interfaces::msg::Int32>::SharedPtr ${portName}_publisher_;"

      return varHeader
    }

    // If the port is a fan out port
    var outPortHeaders: ISZ[ST] = IS()
    for (i <- 1 to inputPortCount) {
      // Int32 is a placeholder message value
      outPortHeaders = outPortHeaders :+
        st"rclcpp::Publisher<example_interfaces::msg::Int32>::SharedPtr ${portName}_publisher_${i};"
    }

    val varHeader: ST =
      st"${(outPortHeaders, "\n")}"

    return varHeader
  }

  // Example:
  //  void TempControl::put_currentTemp(example_interfaces::msg::Int32 msg)
  //  {
  //    temp_control_currentTemp_publisher->publish(msg);
  //  }
  def genCppTopicPublishMethod(outPort: AadlPort, nodeName: String, inputPortCount: Z, strictAADLMode: B): ST = {
    val portName = seqToString(outPort.path, "_")
    val handlerName = outPort.identifier
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // See genCppTopicSubscription()

    if (inputPortCount == 1) {
      // Int32 is a placeholder message value
      val publisherCode: ST =
        st"""void ${nodeName}::put_${handlerName}(example_interfaces::msg::Int32 msg)
            |{
            |    ${portName}_publisher_->publish(msg);
            |}
        """
      return publisherCode
    }

    // If the port is a fan out port
    var publisherPart: ISZ[ST] = IS()
    for (i <- 1 to inputPortCount) {
      // Int32 is a placeholder message value
      publisherPart = publisherPart :+
        st"${portName}_publisher_${i}->publish(msg);"
    }

    val publisherCode: ST =
      st"""void ${nodeName}::put_${handlerName}(example_interfaces::msg::Int32 msg)
          |{
          |    ${(publisherPart, "\n")}
          |}
         """

    return publisherCode
  }

  // Example:
  //  void put_currentTemp(example_interfaces::msg::Int32 msg);
  def genCppTopicPublishMethodHeader(outPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    //val portName = seqToString(outPort.path)
    val handlerName = outPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // See genCppTopicSubscription()

    // Int32 is a placeholder message value
    val publisherHeader: ST =
      st"void put_${handlerName}(example_interfaces::msg::Int32 msg);"
    return publisherHeader
  }

  // Example:
  //  virtual void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) = 0;
  def genCppSubscriptionHandlerVirtualHeader(inPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    //val portName = seqToString(inPort.path)
    val handlerName = inPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // See genCppTopicSubscription()

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"virtual void handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg) = 0;"
    return subscriptionHandlerHeader
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg);
  def genCppSubscriptionHandlerHeader(inPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    //val portName = seqToString(outPort.path)
    val handlerName = inPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  handlerName = handlerName.stripPrefix(nodeName.toString() + "_")
    //}
    // See genCppTopicSubscription()

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"void handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg);"
    return subscriptionHandlerHeader
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) {}
  def genCppSubscriptionHandlerSporadic(inPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    //val portName = seqToString(outPort.path)
    val handlerName = inPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // See genCppTopicSubscription()

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg)
          |{
          |    // Handle ${handlerName} msg
          |}
        """
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerPeriodic(inPort: AadlPort, nodeName: String, strictAADLMode: B): ST = {
    //val portName = seqToString(outPort.path)
    val handlerName = inPort.identifier
    //var handlerName = portName
    //if (!strictAADLMode) {
    //  val strippedNodeName = nodeName.toString().stripSuffix("_base".toString)
    //  handlerName = handlerName.stripPrefix(strippedNodeName + "_")
    //}
    // See genCppTopicSubscription()

    // Int32 is a placeholder message value
    // TODO: Is this good handling of inputs for a periodic thread?
    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg)
          |{
          |    ${handlerName}_msg_holder = msg;
          |}
        """
    return subscriptionHandlerHeader
  }

  // Example:
  // example_interfaces::msg::Int32::SharedPtr currentTemp_msg;
  def genCppSubscriptionMessageVar(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageVar: ST =
      st"example_interfaces::msg::Int32::SharedPtr ${portName}_msg_holder;"
    return subscriptionMessageVar
  }

  def genCppGetSubscriptionMessageHeader(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageHeader: ST =
      st"example_interfaces::msg::Int32::SharedPtr get_${portName}();"
    return subscriptionMessageHeader
  }

  def genCppGetSubscriptionMessage(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessage: ST =
      st"""example_interfaces::msg::Int32::SharedPtr ${nodeName}::get_${portName}() {
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

  def genCppTimeTriggeredMethod(nodeName: String, inPorts: ISZ[AadlPort]): ST = {
    var messages: ISZ[ST] = IS()

    for (port <- inPorts) {
      val portName = port.identifier

      val msg: ST = st"auto ${portName}_msg = get_${portName}();"
      messages = messages :+ msg
    }

    var timeTriggered: ST =
      st"""void ${nodeName}::timeTriggered()
          |{"""

    if (messages.size > 0) {
      timeTriggered =
        st"""${timeTriggered}
            |    // Port messages
            |    ${(messages, "\n")}
          """
    }

    timeTriggered =
      st"""${timeTriggered}
          |    // Handle communication
          |
          |}
        """

    return timeTriggered
  }

  def genCppTimeTriggeredCaller(nodeName: String, component: AadlThread): ST = {
    val period = component.period.get

    val timeTriggered: ST =
      st"""periodTimer = this->create_wall_timer(std::chrono::milliseconds(${period}),
          |    std::bind(&${nodeName}::timeTriggered, this), ${callback_group_name});"""
    return timeTriggered
  }

  def genCppBaseNodeHeaderFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = s"${component.pathAsString("_")}_base"
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var connectionsHeaders: ISZ[ST] = IS()
    var publisherMethodHeaders: ISZ[ST] = IS()
    var subscriptionHandlerHeaders: ISZ[ST] = IS()
    var subscriptionMessageVars: ISZ[ST] = IS()
    var subscriptionMessageGetterHeaders: ISZ[ST] = IS()

    for (p <- component.getPorts()) {
      // TODO: Datatypes
      if (p.direction == Direction.In) {
        connectionsHeaders = connectionsHeaders :+ genCppTopicSubscriptionVarHeader(p)
        if (isSporadic(component)) {
          subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
            genCppSubscriptionHandlerVirtualHeader(p, nodeName, strictAADLMode)
        }
        else {
          subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
            genCppSubscriptionHandlerHeader(p, nodeName.toString().stripSuffix("_base".toString), strictAADLMode)
          subscriptionMessageVars = subscriptionMessageVars :+ genCppSubscriptionMessageVar(p, nodeName)
          subscriptionMessageGetterHeaders = subscriptionMessageGetterHeaders :+ genCppGetSubscriptionMessageHeader(p, nodeName)
        }
      }
      else {
        if (connectionMap.get(p.path).nonEmpty) {
          val inputPorts = connectionMap.get(p.path).get
          connectionsHeaders = connectionsHeaders :+ genCppTopicPublisherVarHeader(p, inputPorts.size)
          publisherMethodHeaders = publisherMethodHeaders :+
            genCppTopicPublishMethodHeader(p, nodeName, strictAADLMode)
        }
      }
    }

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up while looping through each port
    val msgTypes: ISZ[String] = IS("example_interfaces/msg/int32.hpp")
    val includes: ISZ[ST] = genCppHeaderFileMsgTypeIncludes(msgTypes)

    var fileBody =
      st"""#include "rclcpp/rclcpp.hpp"
          |${(includes, "\n")}
          |
          |//=================================================
          |//  D O   N O T   E D I T   T H I S   F I L E
          |//=================================================
          |
          |class ${nodeName} : public rclcpp::Node
          |{
          |protected:
          |    ${nodeName}();
          |
          |    //=================================================
          |    //  C o m m u n i c a t i o n
          |    //=================================================
          |
          |    #define PRINT_INFO(...) RCLCPP_INFO(this->get_logger(), __VA_ARGS__)
          |    #define PRINT_WARN(...) RCLCPP_WARN(this->get_logger(), __VA_ARGS__)
          |    #define PRINT_ERROR(...) RCLCPP_ERROR(this->get_logger(), __VA_ARGS__)
          |
          |    ${(publisherMethodHeaders, "\n")}
        """


    if (subscriptionMessageGetterHeaders.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(subscriptionMessageGetterHeaders, "\n")}
          """
    }

    fileBody =
      st"""${fileBody}
          |    ${genCppCallbackGroupVarHeader()}
          |
          |private:"""

    if (subscriptionHandlerHeaders.size > 0) {
      fileBody =
        st"""${fileBody}
            |    // SubscriptionOptions for assigning subscriptions to the callback group
            |    rclcpp::SubscriptionOptions ${subscription_options_name};
            |
            |    //=================================================
            |    //  C o m p u t e    E n t r y    P o i n t
            |    //=================================================
            |    ${(subscriptionHandlerHeaders, "\n")}
          """
    }

    if (subscriptionMessageVars.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(subscriptionMessageVars, "\n")}
          """
    }

    if (connectionsHeaders.size > 0) {
      fileBody =
        st"""${fileBody}
            |    //=================================================
            |    //  C o m m u n i c a t i o n
            |    //=================================================
            |    ${(connectionsHeaders, "\n")}
          """
    }

    if (!isSporadic(component)) {
      fileBody =
        st"""${fileBody}
            |    //=================================================
            |    //  C a l l b a c k   a n d   T i m e r
            |    //=================================================
            |    virtual void timeTriggered() = 0;
            |
            |    rclcpp::TimerBase::SharedPtr periodTimer;
          """
    }

    fileBody =
      st"""${fileBody}
          |};
        """

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "base_headers", fileName)

    return (filePath, fileBody)
  }

  def genCppBaseNodeCppFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                            strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = s"${component.pathAsString("_")}_base"
    val fileName = genCppNodeSourceName(nodeName)

    var connectionsCode: ISZ[ST] = IS()
    var subscriberMethods: ISZ[ST] = IS()
    var publisherMethods: ISZ[ST] = IS()
    var subscriptionMessageGetters: ISZ[ST] = IS()

    var hasInPorts = F
    for (p <- component.getPorts()) {
      // TODO: Datatypes
      if (p.direction == Direction.In) {
        connectionsCode = connectionsCode :+ genCppTopicSubscription(p, nodeName, strictAADLMode)
        if (!isSporadic(component)) {
          subscriberMethods = subscriberMethods :+
            genCppSubscriptionHandlerPeriodic(p, nodeName, strictAADLMode)
          subscriptionMessageGetters = subscriptionMessageGetters :+ genCppGetSubscriptionMessage(p, nodeName)
        }
        hasInPorts = T
      }
      else {
        if (connectionMap.get(p.path).nonEmpty) {
          val inputPorts = connectionMap.get(p.path).get
          val inputPortNames = getPortNames(inputPorts)
          connectionsCode = connectionsCode :+ genCppTopicPublisher(p, nodeName, inputPortNames)
          publisherMethods = publisherMethods :+
            genCppTopicPublishMethod(p, nodeName, inputPortNames.size, strictAADLMode)
        }
      }
    }

    var fileBody =
      st"""#include "${packageName}/base_headers/${nodeName}${cpp_src_node_header_name_suffix}"
          |
          |//=================================================
          |//  D O   N O T   E D I T   T H I S   F I L E
          |//=================================================
          |
          |${nodeName}::${nodeName}() : Node("${component.pathAsString("_")}")
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
          |    ${(connectionsCode, "\n")}"""

    if (!isSporadic(component)) {
      val timeTriggeredCaller = genCppTimeTriggeredCaller(nodeName, component)

      fileBody =
        st"""${fileBody}
            |    // timeTriggered callback timer
            |    ${timeTriggeredCaller}"""
    }

    fileBody =
      st"""${fileBody}
          |}
        """

    if (subscriberMethods.size > 0 || publisherMethods.size > 0) {
      fileBody =
        st"""${fileBody}
            |//=================================================
            |//  C o m m u n i c a t i o n
            |//=================================================
          """

      if (subscriberMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(subscriberMethods, "\n")}
              |${(subscriptionMessageGetters, "\n")}"""
      }
      if (publisherMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(publisherMethods, "\n")}
            """
      }
    }

    val filePath: ISZ[String] = IS("src", packageName, "src", "base_code", fileName)

    return (filePath, fileBody)
  }

  def genCppUserNodeHeaderFile(packageName: String, component: AadlThread, strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
        // TODO: Datatypes
        if (p.direction == Direction.In) {
          subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeader(p, nodeName, strictAADLMode)
        }
      }

      /*for (inDataPort <- component.ports.dataIns) {
        // TODO: Add data type
        subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeader(inDataPort, nodeName, strictAADLMode)
      }
      for (inEventPort <- component.ports.eventIns) {
        // TODO: Add "empty" data type
        subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeader(inEventPort, nodeName, strictAADLMode)
      }*/
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+ genCppTimeTriggeredMethodHeader()
    }

    var fileBody =
      st"""#include "${packageName}/base_headers/${nodeName}_base${cpp_src_node_header_name_suffix}"
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
          |
          |};
          """

    // TODO: Maybe add user_code directory to include files (in case users want to add class variables or methods)
    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "user_headers", fileName)

    return (filePath, fileBody)
  }

  def genCppUserNodeCppFile(packageName: String, component: AadlThread, strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genCppNodeSourceName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
        // TODO: Datatypes
        if (p.direction == Direction.In) {
          subscriptionHandlers = subscriptionHandlers :+
            genCppSubscriptionHandlerSporadic(p, nodeName, strictAADLMode)
        }
      }

      /*for (inDataPort <- component.ports.dataIns) {
        // TODO: Add data type
        subscriptionHandlers = subscriptionHandlers :+
          genCppSubscriptionHandlerSporadic(inDataPort, nodeName, strictAADLMode)
      }
      for (inEventPort <- component.ports.eventIns) {
        // TODO: Add "empty" data type
        subscriptionHandlers = subscriptionHandlers :+
          genCppSubscriptionHandlerSporadic(inEventPort, nodeName, strictAADLMode)
      }*/
    }
    else {
      var inPorts: ISZ[AadlPort] = IS()
      for (p <- component.getPorts()) {
        if (p.direction == Direction.In) {
          inPorts = inPorts :+ p
        }
      }
      subscriptionHandlers = subscriptionHandlers :+ genCppTimeTriggeredMethod(nodeName, inPorts)
    }

    val fileBody =
      st"""#include "${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}"
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

    return (filePath, fileBody)
  }

  def genCppNodeRunnerFile(packageName: String, component: AadlThread): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genCppNodeRunnerName(nodeName)

    val fileBody =
      st"""#include "${packageName}/user_headers/${nodeName}${cpp_src_node_header_name_suffix}"
          |
          |//=================================================
          |//  D O   N O T   E D I T   T H I S   F I L E
          |//=================================================
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

    return (filePath, fileBody)
  }

  def genCppNodeFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                      strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    val top_level_package_nameT: String = genCppPackageName(modelName)

    // Maps out ports to lists of in ports
    //val connectionMap = genConnections(threadComponents, ad.connections)

    // generate map of ports
    //var ports: ISZ[AadlPort] = IS()

    //for (comp <- threadComponents) {
    //  ports = ports ++ comp.ports.dataIns ++ comp.ports.eventIns ++ comp.ports.dataOuts ++ comp.ports.eventOuts
    //}

    //val portMap = genPortMap(ports)

    var cpp_files: ISZ[(ISZ[String], ST)] = IS()
    for (comp <- threadComponents) {
      cpp_files =
        cpp_files :+ genCppBaseNodeHeaderFile(top_level_package_nameT, comp, connectionMap, strictAADLMode)
      cpp_files =
        cpp_files :+ genCppBaseNodeCppFile(top_level_package_nameT, comp, connectionMap, strictAADLMode)
      cpp_files =
        cpp_files :+ genCppUserNodeHeaderFile(top_level_package_nameT, comp, strictAADLMode)
      cpp_files =
        cpp_files :+ genCppUserNodeCppFile(top_level_package_nameT, comp, strictAADLMode)
      cpp_files =
        cpp_files :+ genCppNodeRunnerFile(top_level_package_nameT, comp)
    }

    return cpp_files
  }


  //================================================
  //  P a c k a g e   G e n e r a t o r s
  //================================================

  def genPyFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                 strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    files = files :+ genPyFormatLaunchFile(modelName, threadComponents)
    files = files :+ genPySetupFile(modelName, threadComponents)

    return files
  }

  def genCppFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                  strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    genCppNodeFiles(modelName, threadComponents, connectionMap, strictAADLMode).foreach(file => files = files :+ file)

    files = files :+ genCppCMakeListsFile(modelName, threadComponents)
    files = files :+ genCppPackageFile(modelName)
    files = files :+ genXmlFormatLaunchFile(modelName, threadComponents)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

    return files
  }
}