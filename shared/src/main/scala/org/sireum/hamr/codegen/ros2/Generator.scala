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

  def isSporadic(component: AadlThread): B = {
    return component.dispatchProtocol == Dispatch_Protocol.Sporadic
  }

  def getPortNames(portNames: ISZ[ISZ[String]]): ISZ[String] = {
    var names: ISZ[String] = IS()
    for (name <- portNames) {
      names = names :+ seqToString(name, "_")
    }
    return names
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

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", fileName)

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

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", fileName)

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

    val filePath: ISZ[String] = IS("src", s"${modelName}_bringup", "launch", fileName)

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

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", "launch", fileName)

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
  //  rclcpp::Subscription<example_interfaces::msg::Int32>::SharedPtr temp_control_currentTemp_subscription;
  def genCppTopicSubscriptionVarHeader(inPort: AadlPort): ST = {
    val portName = seqToString(inPort.path, "_")

    // Int32 is a placeholder message value
    val varHeader: ST =
      st"rclcpp::Subscription<example_interfaces::msg::Int32>::SharedPtr ${portName}_subscription_;"
    return varHeader
  }

  // Example:
  //  temp_control_currentTemp_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
  //    "temp_control_currentTemp",
  //     1,
  //     std::bind(&TempControl::handle_currentTemp, this, std::placeholders::_1));
  def genCppTopicSubscription(inPort: AadlPort, nodeName: String): ST = {
    val topicName = seqToString(inPort.path, "_")
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val portCode: ST =
      st"""${topicName}_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
          |    "${topicName}",
          |    1,
          |    std::bind(&${nodeName}::handle_${portName}, this, std::placeholders::_1), ${subscription_options_name});
        """
    return portCode
  }

  def genCppTopicSubscriptionStrict(inPort: AadlPort, nodeName: String, isSporadic: B): ST = {
    val topicName = seqToString(inPort.path, "_")
    val portName = inPort.identifier

    val handler: ST =
      if (!isSporadic || inPort.isInstanceOf[AadlDataPort]) st"enqueue(infrastructureIn_${portName}, msg);"
      else
        st"""enqueue(infrastructureIn_${portName}, msg);
            |std::thread([this]() {
            |    std::lock_guard<std::mutex> lock(mutex_);
            |    receiveInputs(infrastructureIn_${portName}, applicationIn_${portName});
            |    if (applicationIn_${portName}.empty()) return;
            |    handle_${portName}_base(applicationIn_${portName}.front());
            |    applicationIn_${portName}.pop();
            |    sendOutputs();
            |}).detach();"""

    // Int32 is a placeholder message value
    val portCode: ST =
      st"""${topicName}_subscription_ = this->create_subscription<example_interfaces::msg::Int32>(
          |    "${topicName}",
          |    1,
          |    [this](example_interfaces::msg::Int32 msg) {
          |        ${handler}
          |    },
          |    ${subscription_options_name});
        """
    return portCode
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
  //  temp_control_currentTemp_publisher_ = this->create_publisher<example_interfaces::msg::Int32>(
  //    "operator_interface_currentTemp",
  //     1);
  def genCppTopicPublisher(outPort: AadlPort, inPortNames: ISZ[String]): ST = {
    val portName = seqToString(outPort.path, "_")

    if (inPortNames.size == 1) {
      val inPortName = inPortNames.apply(0)

      // Int32 is a placeholder message value
      val portCode: ST =
        st"""${portName}_publisher_ = this->create_publisher<example_interfaces::msg::Int32>(
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
        st"""${portName}_publisher_${counter} = this->create_publisher<example_interfaces::msg::Int32>(
            |    "${inPortName}",
            |    1);
          """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(outputInstances, "\n")}"

    return fanPortCode
  }

  // TODO: Probably use MsgType if strict....?
  // Example:
  //  void put_currentTemp(example_interfaces::msg::Int32 msg);
  def genCppPutMsgMethodHeader(outPort: AadlPort): ST = {
    val handlerName = outPort.identifier

    // Int32 is a placeholder message value
    val publisherHeader: ST =
      st"void put_${handlerName}(example_interfaces::msg::Int32 msg);"
    return publisherHeader
  }

  // Only used for strict mode (in lax mode, the put_msg method publishes instantly)
  def genCppTopicPublishMethodHeaderStrict(outPort: AadlPort): ST = {
    val handlerName = outPort.identifier

    // Int32 is a placeholder message value
    val publisherHeaders: ST =
      st"void sendOut_${handlerName}(MsgType msg);"

    return publisherHeaders
  }

  // Example:
  //  void TempControl::put_currentTemp(example_interfaces::msg::Int32 msg)
  //  {
  //    temp_control_currentTemp_publisher->publish(msg);
  //  }
  def genCppTopicPublishMethod(outPort: AadlPort, nodeName: String, inputPortCount: Z): ST = {
    val portName = seqToString(outPort.path, "_")
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

    // Int32 is a placeholder message value
    val publisherCode: ST =
      st"""void ${nodeName}::put_${handlerName}(example_interfaces::msg::Int32 msg)
          |{
          |    ${(publishers, "\n")}
          |}
         """

    return publisherCode
  }

  def genCppTopicPublishMethodStrict(outPort: AadlPort, nodeName: String, inputPortCount: Z): ST = {
    val portName = seqToString(outPort.path, "_")
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

    // Int32 is a placeholder message value
    val publisherCode: ST =
      st"""void ${nodeName}::sendOut_${handlerName}(MsgType msg)
          |{
          |    if (auto typedMsg = std::get_if<example_interfaces::msg::Int32>(&msg)) {
          |        ${(publishers, "\n")}
          |    } else {
          |        PRINT_ERROR("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.");
          |    }
          |}
         """

    return publisherCode
  }

  // This method is called by the user, and it puts a message into a port's outApplication queue
  def genCppPutMsgMethodStrict(outPort: AadlPort, nodeName: String): ST = {
    val handlerName = outPort.identifier

    // Int32 is a placeholder message value
    val putMsgCode: ST =
      st"""void ${nodeName}::put_${handlerName}(example_interfaces::msg::Int32 msg)
          |{
          |    enqueue(applicationOut_${handlerName}, msg);
          |}
        """
    return putMsgCode
  }

  // Example:
  //  virtual void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) = 0;
  def genCppSubscriptionHandlerVirtualHeader(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"virtual void handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg) = 0;"
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerVirtualHeaderStrict(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"virtual void handle_${handlerName}(const example_interfaces::msg::Int32 msg) = 0;"
    return subscriptionHandlerHeader
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg) {}
  def genCppSubscriptionHandlerSporadic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg)
          |{
          |    // Handle ${handlerName} msg
          |}
        """
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerSporadicStrict(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const example_interfaces::msg::Int32 msg)
          |{
          |    // Handle ${handlerName} msg
          |}
        """
    return subscriptionHandlerHeader
  }

  // Only used for strict mode currently
  def genCppSubscriptionHandlerBaseSporadicHeader(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val handlerCode: ST =
      st"""void handle_${handlerName}_base(MsgType msg);"""

    return handlerCode
  }

  // Used to convert the type of the msg from MsgType to the intended type before calling the user-defined handler
  def genCppSubscriptionHandlerBaseSporadic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val handlerCode: ST =
      st"""void ${nodeName}::handle_${handlerName}_base(MsgType msg)
          |{
          |    if (auto typedMsg = std::get_if<example_interfaces::msg::Int32>(&msg)) {
          |        handle_${handlerName}(*typedMsg);
          |    } else {
          |        PRINT_ERROR("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.");
          |    }
          |}
         """

    return handlerCode
  }

  // Example:
  //  void handle_currentTemp(const example_interfaces::msg::Int32::SharedPtr currentTempMsg);
  def genCppSubscriptionHandlerHeader(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"void handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg);"
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerHeaderStrict(inPort: AadlPort): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"void handle_${handlerName}(const example_interfaces::msg::Int32 msg);"
    return subscriptionHandlerHeader
  }

  def genCppSubscriptionHandlerPeriodic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""void ${nodeName}::handle_${handlerName}(const example_interfaces::msg::Int32::SharedPtr msg)
          |{
          |    ${handlerName}_msg_holder = msg;
          |}
        """
    return subscriptionHandlerHeader
  }

  // Example:
  // example_interfaces::msg::Int32::SharedPtr currentTemp_msg_holder;
  def genCppSubscriptionMessageVar(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageVar: ST =
      st"example_interfaces::msg::Int32::SharedPtr ${portName}_msg_holder;"
    return subscriptionMessageVar
  }

  def genCppInfrastructureInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val inMsgQueue: ST =
      st"std::queue<MsgType> infrastructureIn_${portName};"
    return inMsgQueue
  }

  def genCppApplicationInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val inMsgQueue: ST =
      st"std::queue<MsgType> applicationIn_${portName};"
    return inMsgQueue
  }

  def genCppInfrastructureOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val outMsgQueue: ST =
      st"std::queue<MsgType> infrastructureOut_${portName};"
    return outMsgQueue
  }

  def genCppApplicationOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val outMsgQueue: ST =
      st"std::queue<MsgType> applicationOut_${portName};"
    return outMsgQueue
  }

  def genCppGetSubscriptionMessageHeader(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageHeader: ST =
      st"example_interfaces::msg::Int32::SharedPtr get_${portName}();"
    return subscriptionMessageHeader
  }

  def genCppGetApplicationInValueHeader(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageHeader: ST =
      st"example_interfaces::msg::Int32 get_${portName}();"
    return subscriptionMessageHeader
  }

  def genCppGetApplicationInValue(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageHeader: ST =
      st"""example_interfaces::msg::Int32 ${nodeName}::get_${portName}() {
          |    MsgType msg = applicationIn_${portName}.front();
          |    return std::get<example_interfaces::msg::Int32>(msg);
          |}"""
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

  // TODO: If/when migrating to a helper function file, find a way to make the publisher method static (probably by passing in the publisher var)
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

  def genCppTimeTriggeredMethod(nodeName: String): ST = {
    val timeTriggered: ST =
      st"""void ${nodeName}::timeTriggered()
          |{
          |    // Handle communication
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

  // TODO: Datatypes
  def genCppBaseNodeHeaderFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                               strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = s"${component.pathAsString("_")}_base"
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHeaders: ISZ[ST] = IS()
    var publisherHeaders: ISZ[ST] = IS()
    var putMethodHeaders: ISZ[ST] = IS()
    var subscriptionHandlerHeaders: ISZ[ST] = IS()
    var inMsgVars: ISZ[ST] = IS()
    var outMsgVars: ISZ[ST] = IS()
    var subscriptionMessageGetterHeaders: ISZ[ST] = IS()
    var strictPublisherHeaders: ISZ[ST] = IS()

    for (p <- component.getPorts()) {
      // TODO: Datatypes
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p)
          inMsgVars = inMsgVars :+ genCppInfrastructureInQueue(p)
          inMsgVars = inMsgVars :+ genCppApplicationInQueue(p)
          if (!p.isInstanceOf[AadlDataPort] && isSporadic(component)) {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerVirtualHeaderStrict(p)
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerBaseSporadicHeader(p)
          }
          else {
            subscriptionMessageGetterHeaders = subscriptionMessageGetterHeaders :+ genCppGetApplicationInValueHeader(p)
          }
        }
        else {
          outMsgVars = outMsgVars :+ genCppInfrastructureOutQueue(p)
          outMsgVars = outMsgVars :+ genCppApplicationOutQueue(p)
          strictPublisherHeaders = strictPublisherHeaders :+ genCppTopicPublishMethodHeaderStrict(p)
          putMethodHeaders = putMethodHeaders :+
            genCppPutMsgMethodHeader(p)
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, inputPorts.size)
          }
        }
      }
      else {
        if (p.direction == Direction.In) {
          subscriptionHeaders = subscriptionHeaders :+ genCppTopicSubscriptionVarHeader(p)
          if (isSporadic(component) && !p.isInstanceOf[AadlDataPort]) {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerVirtualHeader(p)
          }
          else {
            subscriptionHandlerHeaders = subscriptionHandlerHeaders :+
              genCppSubscriptionHandlerHeader(p)
            inMsgVars = inMsgVars :+ genCppSubscriptionMessageVar(p)
            subscriptionMessageGetterHeaders = subscriptionMessageGetterHeaders :+ genCppGetSubscriptionMessageHeader(p)
          }
        }
        else {
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            publisherHeaders = publisherHeaders :+ genCppTopicPublisherVarHeader(p, inputPorts.size)
            putMethodHeaders = putMethodHeaders :+
              genCppPutMsgMethodHeader(p)
          }
        }
      }
    }

    if (subscriptionHeaders.size > 0) {
      subscriptionHeaders = subscriptionHeaders :+ st""
    }

    // TODO: This list of message types is currently just a placeholder
    // It should probably be built up while looping through each port
    val msgTypes: ISZ[String] = IS("example_interfaces/msg/int32.hpp")
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

    // TODO: datatypes for variant (also remove variant if lax?)
    var fileBody =
      st"""#include "rclcpp/rclcpp.hpp"
          |${(typeIncludes, "\n")}
          |${(stdIncludes, "\n")}
          |
          |//=================================================
          |//  D O   N O T   E D I T   T H I S   F I L E
          |//=================================================
          |
          |class ${nodeName} : public rclcpp::Node
          |{
          |protected:"""

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |    using MsgType = std::variant<example_interfaces::msg::Int32>;
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

    return (filePath, fileBody)
  }

  // TODO: Datatypes
  def genCppBaseNodeCppFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                            strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = s"${component.pathAsString("_")}_base"
    val fileName = genCppNodeSourceName(nodeName)

    var subscribers: ISZ[ST] = IS()
    var publishers: ISZ[ST] = IS()
    var subscriberMethods: ISZ[ST] = IS()
    var publisherMethods: ISZ[ST] = IS()
    var subscriptionMessageGetters: ISZ[ST] = IS()

    var outPortNames: ISZ[String] = IS()
    var inTuplePortNames: ISZ[String] = IS()
    var strictPutMsgMethods: ISZ[ST] = IS()
    var strictSubscriptionHandlerBaseMethods: ISZ[ST] = IS()

    var hasInPorts = F
    for (p <- component.getPorts()) {
      // TODO: Datatypes
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          subscribers = subscribers :+ genCppTopicSubscriptionStrict(p, nodeName, isSporadic(component))
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            inTuplePortNames = inTuplePortNames :+ p.identifier
            subscriptionMessageGetters = subscriptionMessageGetters :+ genCppGetApplicationInValue(p, nodeName)
          }
          else {
            strictSubscriptionHandlerBaseMethods = strictSubscriptionHandlerBaseMethods :+
              genCppSubscriptionHandlerBaseSporadic(p, nodeName)
          }
          hasInPorts = T
        }
        else {
          outPortNames = outPortNames :+ p.identifier
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            val inputPortNames = getPortNames(inputPorts)
            publishers = publishers :+ genCppTopicPublisher(p, inputPortNames)
            publisherMethods = publisherMethods :+
              genCppTopicPublishMethodStrict(p, nodeName, inputPortNames.size)
          }
          else {
            publisherMethods = publisherMethods :+
              genCppTopicPublishMethodStrict(p, nodeName, 0)
          }
          strictPutMsgMethods = strictPutMsgMethods :+ genCppPutMsgMethodStrict(p, nodeName)
        }
      }
      else {
        if (p.direction == Direction.In) {
          subscribers = subscribers :+ genCppTopicSubscription(p, nodeName)
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            subscriberMethods = subscriberMethods :+
              genCppSubscriptionHandlerPeriodic(p, nodeName)
            subscriptionMessageGetters = subscriptionMessageGetters :+ genCppGetSubscriptionMessage(p, nodeName)
          }
          hasInPorts = T
        }
        else {
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            val inputPortNames = getPortNames(inputPorts)
            publishers = publishers :+ genCppTopicPublisher(p, inputPortNames)
            publisherMethods = publisherMethods :+
              genCppTopicPublishMethod(p, nodeName, inputPortNames.size)
          }
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
            |    ${genCppInDataPortTupleVector(inTuplePortNames)}"""

      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
              |    // Used by receiveInputs
              |    ${genCppInEventPortTupleVector(inTuplePortNames)}"""
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
              |${(subscriberMethods, "\n")}"""
      }

      if (subscriptionMessageGetters.size > 0) {
        fileBody =
          st"""${fileBody}
              |${(subscriptionMessageGetters, "\n")}"""
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

    return (filePath, fileBody)
  }

  // TODO: Datatypes
  def genCppUserNodeHeaderFile(packageName: String, component: AadlThread, strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genCppNodeSourceHeaderName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
        // TODO: Datatypes
        if (p.direction == Direction.In && !p.isInstanceOf[AadlDataPort]) {
          if (strictAADLMode) {
            subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeaderStrict(p)
          }
          else {
            subscriptionHandlers = subscriptionHandlers :+ genCppSubscriptionHandlerHeader(p)
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

    val filePath: ISZ[String] = IS("src", packageName, "include", packageName, "user_headers", fileName)

    return (filePath, fileBody)
  }

  // TODO: Datatypes
  def genCppUserNodeCppFile(packageName: String, component: AadlThread, strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genCppNodeSourceName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
        // TODO: Datatypes
        if (p.direction == Direction.In && !p.isInstanceOf[AadlDataPort]) {
          if (strictAADLMode) {
            subscriptionHandlers = subscriptionHandlers :+
              genCppSubscriptionHandlerSporadicStrict(p, nodeName)
          }
          else {
            subscriptionHandlers = subscriptionHandlers :+
              genCppSubscriptionHandlerSporadic(p, nodeName)
          }
        }
      }
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+ genCppTimeTriggeredMethod(nodeName)
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

  // TODO: Python pkgs
  def genPyNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                 strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    files = files :+ genPyFormatLaunchFile(modelName, threadComponents)
    files = files :+ genPySetupFile(modelName, threadComponents)

    return files
  }
 // TODO: Python pkgs
  def genPyLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread]): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    // TODO
    //files = files :+ genXmlFormatLaunchFile(modelName, threadComponents)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

    return files
  }

  def genCppNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                  strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = ISZ()

    files = files ++ genCppNodeFiles(modelName, threadComponents, connectionMap, strictAADLMode)
    files = files :+ genCppCMakeListsFile(modelName, threadComponents)
    files = files :+ genCppPackageFile(modelName)

    return files
  }

  def genXmlLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread]): ISZ[(ISZ[String], ST)] = {
    var files: ISZ[(ISZ[String], ST)] = IS()

    files = files :+ genXmlFormatLaunchFile(modelName, threadComponents)
    files = files :+ genLaunchCMakeListsFile(modelName)
    files = files :+ genLaunchPackageFile(modelName)

    return files
  }
}