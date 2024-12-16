// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlPort, AadlThread, Dispatch_Protocol}
import org.sireum.hamr.ir.Direction

object GeneratorPy {
  val node_executable_filename_suffix: String = "_exe"
  val launch_node_decl_suffix: String = "_node"
  val py_launch_file_name_suffix: String = ".launch.py"
  val py_package_name_suffix: String = "_py_pkg"
  val py_src_node_name_suffix: String = "_src.py"
  val py_src_node_entry_point_name: String = "main"
  val py_node_runner_name_suffix: String = "_runner.py"

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
    val entryPointDecl: ST
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

  def genPackageFilePkgDependencies(packages: ISZ[String]): ISZ[ST] = {
    var requirements: ISZ[ST] = IS()

    for (pkg <- packages) {
      requirements = requirements :+ st"<depend>${pkg}</depend>"
    }

    return requirements
  }

  //================================================
  //  L a u n c h  File Setup Files
  //================================================

  def genLaunchCMakeListsFile(modelName: String): (ISZ[String], ST) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
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
    val top_level_package_nameT: String = genPyPackageName(modelName)
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
  //  Node files (Py)
  //    Example: https://github.com/santoslab/ros-examples/tree/main/tempControl_ws/src/tc_py_pkg/tc_py_pkg
  //================================================

  // Example:
  // self.subscription = self.create_subscription(
  //            String,
  //            'fanCmd',
  //            self.handle_fanCmd,
  //            10)
  def genPyTopicSubscrptionStrict(inPort: AadlPort, nodeName: String, isSporadic: B): ST = {
    val topicName = seqToString(inPort.path, "_")
    val portName = inPort.identifier

    val handler: ST =
      if (!isSporadic || inPort.isInstanceOf[AadlDataPort]) {
        st"enqueue(infrastructureIn_${portName}, msg)"
      }
      else {
        // TODO: CPP to Py
        st"""enqueue(infrastructureIn_${portName}, msg)
          |
          |"""
      }

    // Int32 is a placeholder message value
    val portCode: ST =
      st"""self.${topicName}_subscription_ = self.create_subscription(
         |                                  Int32,
         |                                  "${topicName}",
         |                                  self.${handler},
         |                                  1,
         |                                  ${subscription_options_name})
         |"""
    return portCode
  }

  def genPyGetApplicationInValue(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessageHeader: ST =
      st"""self.get_${portName}()
         |  MsgType msg = applicationIn_${portName}.front()
         |  return (Int32)msg
         |"""
    return subscriptionMessageHeader
  }

  def genPySubscriptionHandlerBaseSporadic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val handlerCode: ST =
      st"""def handle_${handlerName}_base(self, msg):
         |  if isinstance(&msg, Int32):
         |    handle_${handlerName}(*typedMsg)
         |  else:
         |    PRINT_ERROR("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.")
         |"""
    return handlerCode
  }

  // Example:
  // self.${portName}_publisher = self.create_publisher(
  //     Int32,
  //     "${inPortName}",
  //     10,
  //     callback_group=self.${callback_group_name})
  def genPyTopicPublisher(outPort: AadlPort, inPortNames: ISZ[String]): ST = {
    val portName = seqToString(outPort.path, "_")

    if (inPortNames.size == 1) {
      val inPortName = inPortNames.apply(0)

      // Int32 is a placeholder message value
      val portCode: ST =
        st"""self.${portName}_publisher_ = self.create_publisher(
           |                              Int32,
           |                              "${inPortName}",
           |                              1)
           |"""
      return portCode
    }

    // If the port is a fan out port
    var outputInstances: ISZ[ST] = IS()
    var counter = 1

    for (inPortName <- inPortNames) {
      outputInstances = outputInstances :+
        st"""self.${portName}_publisher_${counter} = self.create_publisher(
           |                    Int32,
           |                    "${inPortName}",
           |                    1)
           |"""
      counter = counter + 1;
    }

    val fanPortCode: ST =
      st"${(outputInstances, "\n")}"

    return fanPortCode
  }

  def genPyTopicPublishMethodStrict(outPort: AadlPort, nodeName: String, inputPortCount: Z): ST = {
    val portName = seqToString(outPort.path, "_")
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"self.${portName}_publisher_.publish(*typedMsg)"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"self.${portName}_publisher_${i}.publish(*typedMsg)"
      }
    }

    // Int32 is a placeholder message value
    val publisherCode: ST =
      st"""def sendOut_${handlerName}(self, msg):
         |  if isinstance(&msg, Int32):
         |    ${(publishers, "\n")}
         |   else:
         |    PRINT_ERROR("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.")
         |"""
    return publisherCode
  }

  def genPyPutMsgMethodStrict(outPort: AadlPort, nodeName: String): ST = {
    val handlerName = outPort.identifier

    // Int32 is a placeholder message value
    val putMsgCode: ST =
      st"""def put_${handlerName}(self, msg):
         |  enqueue(applicationOut_${handlerName}, msg)
         |"""
    return putMsgCode
  }

  // Example:
  // self.subscription = self.create_subscription(
  //            String,
  //            'fanCmd',
  //            self.handle_fanCmd,
  //            10)
  def genPyTopicSubscription(inPort: AadlPort, nodeName: String): ST = {
    val topicName = seqToString(inPort.path, "_")
    val portName = inPort.identifier

    // Int32 in a placeholder message value
    val portCode: ST =
      st"""self.${topicName}_subscription_ = self.create_subscription(
         |  Int32,
         |  "${topicName}",
         |  1,
         |  ${subscription_options_name}
         |"""
    return portCode
  }

  def genPySubscriptionHandlerPeriodic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""def handle_${handlerName}(self, msg):
         |  self.${handlerName}_msg_holder = msg
         |"""
    return subscriptionHandlerHeader
  }

  def genPyGetSubscriptionMessage(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionMessage: ST =
      st"""def get_${portName}():
         |  return ${portName}_msg_holder
         |"""
    return subscriptionMessage
  }

  def genPyTopicPublishMethod(outPort: AadlPort, nodeName: String, inputPortCount: Z): ST = {
    val portName = seqToString(outPort.path, "_")
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"self.${portName}_publisher_.publish(msg)"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"${portName}_publisher_${i}.publish(msg)"
      }
    }

    // Int32 is a placeholder message value
    val publisherCode: ST =
      st"""def put_${handlerName}(self, msg):
         |  ${(publishers, "\n")}
         |"""
    return publisherCode
  }

  def genPyCallbackGroupVar(): ST = {
    val callbackGroup: ST =
      st"${callback_group_name} = this.create_callback_group(${callback_group_type})"
    return callbackGroup
  }

  def genPyTimeTriggeredStrict(nodeName: String, component: AadlThread): ST = {
    val period = component.period.get

    val timer: ST =
      st"""self.periodTimer_ = self.create_wall_timer(${period}, self.timeTriggeredCaller, ${callback_group_name})"""
    return timer
  }

  def genPyTimeTriggeredTimer(nodeName: String, component: AadlThread): ST = {
    val period = component.period.get

    val timer: ST =
      st"""self.periodTimer_ = self.create_wall_timer(${period}, self.timeTriggered, ${callback_group_name})"""
    return timer
  }

  def genPyInDataPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&infrastructureIn_${name}, &applicationIn_${name}}"
    }

    val vector: ST =
      st"""inDataPortTupleVector =
        | ${(tuples, ",\n")}
      """
    return vector
  }

  def genPyInEventPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&infrastructureIn_${name}, &applicationIn_${name}}"
    }

     val vector: ST =
       st"""inEventPortTupleVector =
         |  ${(tuples, ",\n")}
       """
     return vector
  }

  def genPyOutPortTupleVector(nodeName: String, portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"{&applicationOut_${name}, &infrastructureOut_${name}, &self.sendOut_${name}}"
    }

    val vector: ST =
      st"""outPortTupleVector =
        | ${(tuples, ",\n")}
      """
    return vector
  }

  def genPyTimeTriggeredCaller(nodeName: String): ST = {
    val timeTriggered: ST =
      st"""def timeTriggeredCaller(self):
        | receiveInputs()
        | timeTriggered()
        | sendOutputs()
      """
    return timeTriggered
  }

  def genPyReceiveInputsSporadic(nodeName: String): ST = {
    val method: ST =
      st"""def receiveInputs(self, infrastructureQueue, applicationQueue):
        | if !infrastructureQueue.empty():
        |   eventMsg = infrastructureQueue.front()
        |   infrastructureQueue.pop()
        |   enqueue(applicationQueue, eventMsg)
        |
        | for port in inDataPortTupleVector:
        |   infrastructureQueue = std::get<0>(port)
        |   if !infrastructureQueue.empty():
        |      msg = infrastructureQueue.front()
        |      enqueue(*std::get<1>(port), msg)
      """
    return method
  }

  def genPyReceiveInputsPeriodic(nodeName: String): ST = {
    val method: ST =
      st"""def receiveInputs(self):
        | for port in inDataPortTupleVector:
        |   auto infrastructureQueue = std::get<0>(port)
        |   if !infrastructureQueue.empty():
        |     msg = infrastructureQueue.front()
        |     enqueue(*std::get<1>(port), msg)
        |
        | for port in inEventPortTupleVector:
        |   auto infrastructureQueue = std::get<0>(port)
        |     if !infrastructureQueue.empty():
        |       msg = infrastructureQueue.front()
        |       infrastructureQueue->pop()
        |       enqueue(*std::get<1>(port), msg)
      """
    return method
  }

  def genPyEnqueue(nodeName: String): ST = {
    val method: ST =
      st"""def enqueue(self, queue, val):
          | if queue.size() >= 1:
          |   queue.pop()
          | queue.push(val)
        """
    return method
  }

  def genPySendOutputs(nodeName: String): ST = {
    val method: ST =
      st"""def sendOutputs(self):
          | for port in outPortTupleVector:
          |   applicationQueue = std::get<0>(port)
          |   if applicationQueue.size() != 0:
          |     msg = applicationQueue.front()
          |     applicationQueue.pop()
          |     enqueue(*std::get<1>(port), msg)
          |
          | for port in outPortTupleVector:
          |   infrastructureQueue = std::get<1>(port)
          |   if infrastructureQueue.size() != 0:
          |     msg = infrastructureQueue.front()
          |     infrastructureQueue.pop()
          |     (this->*std::get<2>(port))(msg)
        """
    return method
  }

  def genPyBaseNodePyFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                          strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = s"${component.pathAsString("_")}_base"
    val fileName = genPyNodeSourceName(nodeName)

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
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          subscribers = subscribers :+ genPyTopicSubscrptionStrict(p, nodeName, isSporadic(component))
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            inTuplePortNames = inTuplePortNames :+ p.identifier
            subscriptionMessageGetters = subscriptionMessageGetters :+ genPyGetApplicationInValue(p, nodeName)
          }
          else {
            strictSubscriptionHandlerBaseMethods = strictSubscriptionHandlerBaseMethods :+
              genPySubscriptionHandlerBaseSporadic(p, nodeName)
          }
          hasInPorts = T
        }
        else {
          outPortNames = outPortNames :+ p.identifier
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            val inputPortNames = getPortNames(inputPorts)
            publishers = publishers :+ genPyTopicPublisher(p, inputPortNames)
            publisherMethods = publisherMethods :+
              genPyTopicPublishMethodStrict(p, nodeName, inputPortNames.size)
          }
          else {
            publisherMethods = publisherMethods :+
              genPyTopicPublishMethodStrict(p, nodeName, 0)
          }
          strictPutMsgMethods = strictPutMsgMethods :+ genPyPutMsgMethodStrict(p, nodeName)
        }
      }
      else {
        if (p.direction == Direction.In) {
          subscribers = subscribers :+ genPyTopicSubscription(p, nodeName)
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            subscriberMethods = subscriberMethods :+
              genPySubscriptionHandlerPeriodic(p, nodeName)
            subscriptionMessageGetters = subscriptionMessageGetters :+ genPyGetSubscriptionMessage(p, nodeName)
          }
          hasInPorts = T
        }
        else {
          if (connectionMap.get(p.path).nonEmpty) {
            val inputPorts = connectionMap.get(p.path).get
            val inputPortNames = getPortNames(inputPorts)
            publishers = publishers :+ genPyTopicPublisher(p, inputPortNames)
            publisherMethods = publisherMethods :+
              genPyTopicPublishMethod(p, nodeName, inputPortNames.size)
          }
        }
      }
    }

    var fileBody =
      st"""#!/usr/bin/env python3
        |import rclpy
        |from rclpy.node import Node
        |
        |//=================================================
        |//  D O   N O T   E D I T   T H I S   F I L E
        |//=================================================
        |
        |class ${nodeName}(Node):
        |   def __init__(self):
        |       super().__init__("${component.pathAsString("_")}")
        |
        |       ${genPyCallbackGroupVar()}
      """

    if (hasInPorts) {
      fileBody =
        st"""${fileBody}
           |  ${subscription_options_name}.callback_group = ${callback_group_name}
         """
    }

    fileBody =
      st"""${fileBody}
         |  // Setting up connections
         |  ${(subscribers ++ publishers, "\n")}"""

    if(!isSporadic(component)) {
      if (strictAADLMode) {
        fileBody =
          st"""${fileBody}
             |  // timeTriggeredCaller callback timer
             |  ${genPyTimeTriggeredStrict(nodeName, component)}
           """
      }
      else {
        fileBody =
          st"""${fileBody}
             |  // timeTriggered callback timer
             |  ${genPyTimeTriggeredTimer(nodeName, component)}
           """
      }
    }

    if(strictAADLMode) {
      fileBody =
        st"""${fileBody}
           |  // Used by receiveInputs
           |  ${genPyInDataPortTupleVector(inTuplePortNames)}"""

      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
             |  // Used by receiveInputs
             |  ${genPyInEventPortTupleVector(inTuplePortNames)}"""
      }

      fileBody =
        st"""${fileBody}
           |  // Used by sendOutputs
           |  ${genPyOutPortTupleVector(nodeName, outPortNames)}"""
    }

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
             |${genPyTimeTriggeredCaller(nodeName)}"""
      }

      val receiveInputs: ST = if (isSporadic(component)) genPyReceiveInputsSporadic(nodeName)
                              else genPyReceiveInputsPeriodic(nodeName)

      fileBody =
        st"""${fileBody}
            |${receiveInputs}
            |${genPyEnqueue(nodeName)}
            |${genPySendOutputs(nodeName)}"""
    }

    val filePath: ISZ[String] = IS("src", packageName, "src", "base_code", fileName)

    return (filePath, fileBody)
  }

  def genPySubscriptionHandlerSporadicStrict(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""def handle_${handlerName}(self, msg)
          |{
          |    // Handle ${handlerName} msg
          |}
        """
    return subscriptionHandlerHeader
  }

  def genPySubscriptionHandlerSporadic(inPort: AadlPort, nodeName: String): ST = {
    val handlerName = inPort.identifier

    // Int32 is a placeholder message value
    val subscriptionHandlerHeader: ST =
      st"""def handle_${handlerName}(self, msg)
          |{
          |    // Handle ${handlerName} msg
          |}
        """
    return subscriptionHandlerHeader
  }

  def genPyTimeTriggeredMethod(nodeName: String): ST = {
    val timeTriggered: ST =
      st"""def timeTriggered(self)
          |{
          |    // Handle communication
          |}
        """
    return timeTriggered
  }

  def genPyUserNodePyFile(packageName: String, component: AadlThread, strictAADLMode: B): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genPyNodeSourceName(nodeName)

    var subscriptionHandlers: ISZ[ST] = IS()
    if (isSporadic(component)) {
      for (p <- component.getPorts()) {
        // TODO: Datatypes
        if (p.direction == Direction.In && !p.isInstanceOf[AadlDataPort]) {
          if (strictAADLMode) {
            subscriptionHandlers = subscriptionHandlers :+
              genPySubscriptionHandlerSporadicStrict(p, nodeName)
          }
          else {
            subscriptionHandlers = subscriptionHandlers :+
              genPySubscriptionHandlerSporadic(p, nodeName)
          }
        }
      }
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+ genPyTimeTriggeredMethod(nodeName)
    }

    val fileBody =
      st"""#!/usr/bin/env python3
          |import rclpy
          |from rclpy.node import Node
          |//=================================================
          |//  I n i t i a l i z e    E n t r y    P o i n t
          |//=================================================
          |def initialize(self)
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

  def genPyNodeRunnerName(compNameS: String): String = {
    // create runner file name
    val nodeNameT: String = s"${compNameS}${py_node_runner_name_suffix}"
    return nodeNameT
  }

  def genPyNodeRunnerFile(packageName: String, component: AadlThread): (ISZ[String], ST) = {
    val nodeName = component.pathAsString("_")
    val fileName = genPyNodeRunnerName(nodeName)

    val fileBody =
      st"""#!/usr/bin/env python3
          |import rclpy
          |from rclpy.node import Node
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

  def genPyNodeFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                     strictAADLMode: B): ISZ[(ISZ[String], ST)] = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    var py_files: ISZ[(ISZ[String], ST)] = IS()
    for (comp <- threadComponents) {
      py_files =
        py_files :+ genPyBaseNodePyFile(top_level_package_nameT, comp, connectionMap, strictAADLMode)
      py_files =
        py_files :+ genPyUserNodePyFile(top_level_package_nameT, comp, strictAADLMode)
      py_files :+ genPyNodeRunnerFile(top_level_package_nameT, comp)
    }
    return py_files
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

    for(file <- genPyNodeFiles(modelName, threadComponents, connectionMap, strictAADLMode)) {
      files = files :+ file
    }

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
}
