// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlPort, AadlProcess, AadlSystem, AadlThread, Dispatch_Protocol}
import org.sireum.hamr.codegen.common.types.{AadlType, EnumType}
import org.sireum.hamr.ir.Direction
import org.sireum.message.Reporter
import org.sireum.ops.{ISZOps, StringOps}

object GeneratorPy {

  val toolName: String = "Ros2Codegen"

  val node_executable_filename_suffix: String = "_exe"
  val py_package_name_suffix: String = "_py_pkg"
  val py_src_node_name_suffix: String = "_src.py"
  val py_src_node_entry_point_name: String = "main"
  val py_node_runner_name_suffix: String = "_runner.py"

  val callback_group_type: String = "ReentrantCallbackGroup"
  val callback_group_name: String = "cb_group_"

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

  def isEventPort(portType: String): B = {
    return ops.StringOps(portType).substring(0, portType.size) == "Empty"
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
          s"${dtype.get._1}"
        }
        else {
          reporter.error(None(), toolName, s"Port ${port.identifier}: datatype unknown, setting datatype to Empty")
          s"Empty"
        }
      case edp: AadlEventDataPort =>
        val dtype = datatypeMap.get(edp.aadlType)
        if (dtype.nonEmpty) {
          s"${dtype.get._1}"
        }
        else {
          reporter.error(None(), toolName, s"Port ${port.identifier}: datatype unknown, setting datatype to Empty")
          s"Empty"
        }
      case _ => s"Empty"
    }
    return s
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
    val node_source_file_nameT = st"${componentName}_runner"
    val py_package_nameT = genPyPackageName(modelName)
    val node_executable_file_nameT = genExecutableFileName(componentName)
    val entryPointDecl: ST
    = st"\"${node_executable_file_nameT} = ${py_package_nameT}.base_code.${node_source_file_nameT}:${py_src_node_entry_point_name}\""
    return entryPointDecl
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_py_pkg/setup.py
  def genPySetupFile(modelName: String, threadComponents: ISZ[AadlThread]): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "setup.py"

    // build entry point declarations
    var entry_point_decls: ISZ[ST] = IS()
    for (comp <- threadComponents) {
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

    return (filePath, setupFileBody, true, IS())
  }

  def genPackageFilePkgDependencies(packages: ISZ[String]): ISZ[ST] = {
    var requirements: ISZ[ST] = IS()

    for (pkg <- packages) {
      requirements = requirements :+ st"<depend>${pkg}</depend>"
    }

    return requirements
  }

  //  Setup file for node source package
  //    Example: https://github.com/santoslab/ros-examples/blob/main/tempControl_ws/src/tc_py_pkg/setup.cfg
  def genCfgSetupFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "setup.cfg"

    val setupFileBody =
      st"""# ${fileName}   in  src/${top_level_package_nameT}
          |[develop]
          |script_dir=$$base/lib/${top_level_package_nameT}
          |[install]
          |install_scripts=$$base/lib/${top_level_package_nameT}
       """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody, true, IS())
  }

  def genXmlPackageFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "package.xml"

    val startMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"
    val endMarker: String = "<!-- Additions within these tags will be preserved when re-running Codegen -->"

    val packages: ISZ[String] = IS(s"${genPyPackageName(modelName)}_interfaces")
    val pkgDependencies: ISZ[ST] = genPackageFilePkgDependencies(packages)

    val setupFileBody =
      st"""<?xml version="1.0"?>
          |<?xml-model href="http://download.ros.org/schema/package_format3.xsd" schematypens="http://www.w3.org/2001/XMLSchema"?>
          |<package format="3">
          |  <name>${top_level_package_nameT}</name>
          |  <version>0.0.0</version>
          |  <description>TODO: Package description</description>
          |  <maintainer email="todo.todo@todo.com">ed</maintainer>
          |  <license>TODO: License declaration</license>
          |
          |  <depend>rclpy</depend>
          |  <depend>rosidl_runtime_py</depend>
          |  ${(pkgDependencies, "\n")}
          |
          |    ${startMarker}
          |
          |    ${endMarker}
          |
          |  <test_depend>ament_copyright</test_depend>
          |  <test_depend>ament_flake8</test_depend>
          |  <test_depend>ament_pep257</test_depend>
          |  <test_depend>python3-pytest</test_depend>
          |
          |  <export>
          |    <build_type>ament_python</build_type>
          |  </export>
          |</package>
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, fileName)

    return (filePath, setupFileBody, F, IS(Marker(startMarker, endMarker)))
  }

  def genPyInitFile(packageName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val fileName = "__init__.py"

    val fileBody =
      st"""
       """

    val filePath: ISZ[String] = IS("src", packageName, packageName, fileName)

    return (filePath, fileBody, T, IS())
  }

  def genPySubInitFile(modelName: String, subModelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "__init__.py"

    val setupFileBody =
      st"""
       """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, top_level_package_nameT, subModelName, fileName)

    return (filePath, setupFileBody, T, IS())
  }

  def genPyResourceFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)

    val setupFileBody =
      st"""
       """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, "resource", top_level_package_nameT)

    return (filePath, setupFileBody, T, IS())
  }

  def genPyCopyrightFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "test_copyright.py"

    val setupFileBody =
      st"""# Copyright 2015 Open Source Robotics Foundation, Inc.
          |#
          |# Licensed under the Apache License, Version 2.0 (the "License");
          |# you may not use this file except in compliance with the License.
          |# You may obtain a copy of the License at
          |#
          |#     http://www.apache.org/licenses/LICENSE-2.0
          |#
          |# Unless required by applicable law or agreed to in writing, software
          |# distributed under the License is distributed on an "AS IS" BASIS,
          |# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          |# See the License for the specific language governing permissions and
          |# limitations under the License.
          |
          |from ament_copyright.main import main
          |import pytest
          |
          |
          |# Remove the `skip` decorator once the source file(s) have a copyright header
          |@pytest.mark.skip(reason='No copyright header has been placed in the generated source file.')
          |@pytest.mark.copyright
          |@pytest.mark.linter
          |def test_copyright():
          |    rc = main(argv=['.', 'test'])
          |    assert rc == 0, 'Found errors'
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, "test", fileName)

    return (filePath, setupFileBody, T, IS())
  }

  def genPyFlakeFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "test_flake8.py"

    val setupFileBody =
      st"""# Copyright 2017 Open Source Robotics Foundation, Inc.
          |#
          |# Licensed under the Apache License, Version 2.0 (the "License");
          |# you may not use this file except in compliance with the License.
          |# You may obtain a copy of the License at
          |#
          |#     http://www.apache.org/licenses/LICENSE-2.0
          |#
          |# Unless required by applicable law or agreed to in writing, software
          |# distributed under the License is distributed on an "AS IS" BASIS,
          |# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          |# See the License for the specific language governing permissions and
          |# limitations under the License.
          |
          |from ament_flake8.main import main_with_errors
          |import pytest
          |
          |
          |@pytest.mark.flake8
          |@pytest.mark.linter
          |def test_flake8():
          |    rc, errors = main_with_errors(argv=[])
          |    assert rc == 0, \\
          |        'Found %d code style errors / warnings:\n' % len(errors) + \\
          |        '\n'.join(errors)
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, "test", fileName)

    return (filePath, setupFileBody, T, IS())
  }

  def genPyPrepFile(modelName: String): (ISZ[String], ST, B, ISZ[Marker]) = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    val fileName: String = "test_prep257.py"

    val setupFileBody =
      st"""# Copyright 2015 Open Source Robotics Foundation, Inc.
          |#
          |# Licensed under the Apache License, Version 2.0 (the "License");
          |# you may not use this file except in compliance with the License.
          |# You may obtain a copy of the License at
          |#
          |#     http://www.apache.org/licenses/LICENSE-2.0
          |#
          |# Unless required by applicable law or agreed to in writing, software
          |# distributed under the License is distributed on an "AS IS" BASIS,
          |# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          |# See the License for the specific language governing permissions and
          |# limitations under the License.
          |
          |from ament_pep257.main import main
          |import pytest
          |
          |
          |@pytest.mark.linter
          |@pytest.mark.pep257
          |def test_pep257():
          |    rc = main(argv=['.', 'test'])
          |    assert rc == 0, 'Found code style errors / warnings'
        """

    val filePath: ISZ[String] = IS("src", top_level_package_nameT, "test", fileName)

    return (filePath, setupFileBody, T, IS())
  }

  //================================================
  //  Node files (Py)
  //    Example: https://github.com/santoslab/ros-examples/tree/main/tempControl_ws/src/tc_py_pkg/tc_py_pkg
  //================================================

  def genPyDataPortInitializerStrict(inDataPort: AadlPort, nodeName: String, portType: String): ST = {
    val portName = inDataPort.identifier

    val initializer: ST =
      st"""def init_${portName}(self, val):
          |    self.enqueue(self.infrastructureIn_${portName}, val)
        """
    return initializer
  }

  // Example:
  // self.subscription = self.create_subscription(
  //            String,
  //            'fanCmd',
  //            self.handle_fanCmd,
  //            10)
  def genPyTopicSubscriptionStrict(inPort: AadlPort, portType: String, outPortNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier

    val handler: ST = st"self.accept_${handlerName}"

    if (outPortNames.size == 1) {
      val topicName = outPortNames.apply(0)

      val portCode: ST =
        st"""self.${portName}_subscription_ = self.create_subscription(
            |    ${portType},
            |    "${topicName}",
            |    ${handler},
            |    1,
            |    callback_group=self.${callback_group_name})
        """
      return portCode
    }

    // If the port is a fan in port
    var inputInstances: ISZ[ST] = IS()
    var counter = 1

    for (outPortName <- outPortNames) {
      inputInstances = inputInstances :+
        st"""self.${portName}_subscription_${counter} = self.create_subscription(
            |    ${portType},
            |    "${outPortName}",
            |    ${handler},
            |    1,
            |    callback_group=self.${callback_group_name})
        """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(inputInstances, "\n")}"

    return fanPortCode
  }

  def genPyThread(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    val thread: ST =
      st"""def ${handlerName}_thread(self):
        |    with self.lock_:
        |        self.receiveInputs(self.infrastructureIn_${handlerName}, self.applicationIn_${handlerName})
        |        if len(self.applicationIn_${handlerName}) == 0: return
        |        self.handle_${handlerName}_base(self.applicationIn_${handlerName}[0])
        |        self.applicationIn_${handlerName}.pop()
        |        self.sendOutputs()
      """

    return thread
  }

  def genPyMessageAcceptor(inPort: AadlPort, isSporadic: B, portType: String): ST = {
    val handlerName = inPort.identifier

    val handler: ST =
      if (!isSporadic || inPort.isInstanceOf[AadlDataPort]) st"self.enqueue(self.infrastructureIn_${handlerName}, msg)"
      else
        st"""self.enqueue(self.infrastructureIn_${handlerName}, msg)
            |thread = threading.Thread(target=self.${handlerName}_thread)
            |thread.daemon = True
            |thread.start()
         """

    val method: ST =
      st"""def accept_${handlerName}(self, msg):
          |    ${handler}
        """
    return method
  }

  def genPyInfrastructureInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val inMsgQueue: ST =
      st"self.infrastructureIn_${portName} = deque()"
    return inMsgQueue
  }

  def genPyApplicationInQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val inMsgQueue: ST =
      st"self.applicationIn_${portName} = deque()"
    return inMsgQueue
  }

  def genPyGetApplicationInValue(inPort: AadlPort, portType: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessageHeader: ST =
      st"""def get_${portName}(self):
         |    msg = self.applicationIn_${portName}[0]
         |    return msg
       """
    return subscriptionMessageHeader
  }

  def genPySubscriptionHandlerVirtualStrict(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var handlerCode: ST = st""
    if (isEventPort(portType)) {
      handlerCode =
        st"""def handle_${handlerName}(self):
           |    raise NotImplementedError("Subclasses must implement this method")
         """
    } else {
      handlerCode =
        st"""def handle_${handlerName}(self, msg):
            |    raise NotImplementedError("Subclasses must implement this method")
          """
    }

    return handlerCode
  }

  def genPySubscriptionHandlerBaseSporadic(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var handlerCode: ST = st""
    if (isEventPort(portType)) {
      handlerCode =
        st"""def handle_${handlerName}_base(self, msg):
          |    self.handle_${handlerName}()
        """
    }
    else {
      handlerCode =
        st"""def handle_${handlerName}_base(self, msg):
          |    if type(msg) is ${portType}:
          |        self.handle_${handlerName}(msg)
          |    else:
          |        self.get_logger.error("Receiving wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.")
        """
    }
    return handlerCode
  }

  // Example:
  // self.${portName}_publisher = self.create_publisher(
  //     Int32,
  //     "${inPortName}",
  //     10,
  //     callback_group=self.${callback_group_name})
  def genPyTopicPublisher(outPort: AadlPort, portType: String, inPortNames: ISZ[String]): ST = {
    val portName = genPortName(outPort)

    if (inPortNames.size == 1) {
      val inPortName = inPortNames.apply(0)

      val portCode: ST =
        st"""self.${portName}_publisher_ = self.create_publisher(
           |    ${portType},
           |    "${inPortName}",
           |    1)
         """
      return portCode
    }

    // If the port is a fan out port
    var outputInstances: ISZ[ST] = IS()
    var counter = 1

    for (inPortName <- inPortNames) {
      outputInstances = outputInstances :+
        st"""self.${portName}_publisher_${counter} = self.create_publisher(
           |    ${portType},
           |    "${inPortName}",
           |    1)
         """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(outputInstances, "\n")}"

    return fanPortCode
  }

  def genPyTopicPublishMethodStrict(outPort: AadlPort, portType: String, inputPortCount: Z): ST = {
    val portName = genPortName(outPort)
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"self.${portName}_publisher_.publish(msg)"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"self.${portName}_publisher_${i}.publish(msg)"
      }
    }

    val publisherCode: ST =
      st"""def sendOut_${handlerName}(self, msg):
         |    if type(msg) is ${portType}:
         |        ${(publishers, "\n")}
         |    else:
         |        self.get_logger().error("Sending out wrong type of variable on port ${handlerName}.\nThis shouldn't be possible.  If you are seeing this message, please notify this tool's current maintainer.")
       """
    return publisherCode
  }

  def genPyPutMsgMethodStrict(outPort: AadlPort, portType: String): ST = {
    val handlerName = outPort.identifier

    var putMsgCode: ST = st""

    if (isEventPort(portType)) {
      putMsgCode =
        st"""def put_${handlerName}(self):
          |    self.enqueue(self.applicationOut_${handlerName}, ${portType}())
        """
    }
    else {
      putMsgCode =
        st"""def put_${handlerName}(self, msg):
          |    self.enqueue(self.applicationOut_${handlerName}, msg)
        """
    }
    return putMsgCode
  }

  def genPyDataPortInitializer(inDataPort: AadlPort): ST = {
    val portName = inDataPort.identifier

    val initializer: ST =
      st"""def init_${portName}(self, val):
          |    self.${portName}_msg_holder = val"""
    return initializer
  }

  // Example:
  // self.subscription = self.create_subscription(
  //            String,
  //            'fanCmd',
  //            self.handle_fanCmd,
  //            10)
  def genPyTopicSubscription(inPort: AadlPort, portType: String, outPortNames: ISZ[String]): ST = {
    val portName = genPortName(inPort)
    val handlerName = inPort.identifier

    var handler: ST = st""

    if(isEventPort(portType)) {
      handler = st"self.event_handle_${handlerName}"
    }
    else {
      handler = st"self.handle_${handlerName}"
    }

    if (outPortNames.size == 1) {
      val topicName = outPortNames.apply(0)
      val portCode: ST =
        st"""self.${portName}_subscription_ = self.create_subscription(
          |    ${portType},
          |    "${topicName}",
          |    ${handler},
          |    1,
          |    callback_group=self.${callback_group_name})
        """
      return portCode
    }

    // If the port is a fan in port
    var inputInstances: ISZ[ST] = IS()
    var counter = 1

    for (outPortName <- outPortNames) {
      inputInstances = inputInstances :+
        st"""self.${portName}_subscription_${counter} = self.create_subscription(
            |    ${portType},
            |    "${outPortName}",
            |    ${handler},
            |    1,
            |    callback_group=self.${callback_group_name})
          """
      counter = counter + 1
    }

    val fanPortCode: ST =
      st"${(inputInstances, "\n")}"

    return fanPortCode
  }

  def genPyEventPortHandler(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    val handler: ST =
      st"""def event_handle_${handlerName}(self, msg):
        |    self.handle_${handlerName}()
      """

    return handler
  }

  def genPyMessageToString(): ST = {
    val message: ST =
      st"""def message_to_string(self, msg):
        |    yaml_str = message_to_yaml(msg)
        |    return yaml_str
      """

    return message
  }

  def genPySubscriptionHandlerSporadicWithExamples(inPort: AadlPort, portType: String,
                                                   inDataPorts: ISZ[AadlPort]): ST = {
    val handlerName = inPort.identifier

    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"# Example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {
        exampleUsage =
          st"""${exampleUsage}
              |${inDataPort.identifier} = get_${inDataPort.identifier}()
              |self.get_logger().info(f"Received ${inDataPort.identifier}: {self.message_to_string(${inDataPort.identifier})}")
            """
      }
    }


    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""def handle_${handlerName}(self):
                                      |    # Handle ${handlerName} event
                                      |    self.get_logger().info("Received ${handlerName}")
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""def handle_${handlerName}(self, msg):
                                      |    # Handle ${handlerName} msg
                                      |    self.get_logger().info(f"Received ${handlerName}: {self.message_to_string(msg)}")
                                    """
    }

    if (inDataPorts.size > 0) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
            |
            |    ${exampleUsage}"""
    }

    return subscriptionHandlerHeader
  }

  def genPySubscriptionHandlerPeriodic(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    val subscriptionHandlerHeader: ST =
      st"""def handle_${handlerName}(self, msg):
         |    self.${handlerName}_msg_holder = msg
       """
    return subscriptionHandlerHeader
  }

  def genPyGetSubscriptionMessage(inPort: AadlPort, nodeName: String): ST = {
    val portName = inPort.identifier

    val subscriptionMessage: ST =
      st"""def get_${portName}(self):
         |    return self.${portName}_msg_holder
       """
    return subscriptionMessage
  }

  def genPySubscriptionMessageVar(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val subscriptionMessageVar: ST =
      st"self.${portName}_msg_holder = None"
    return subscriptionMessageVar
  }

  def genPySubscriptionHandlerVirtual(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var handlerCode: ST = st""
    if (isEventPort(portType)) {
      handlerCode =
        st"""def handle_${handlerName}(self):
            |    raise NotImplementedError("Subclasses must implement this method")
          """
    } else {
      handlerCode =
        st"""def handle_${handlerName}(self, msg):
            |    raise NotImplementedError("Subclasses must implement this method")
          """
    }

    return handlerCode
  }

  def genPyInfrastructureOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val outMsgQueue: ST =
      st"self.infrastructureOut_${portName} = deque()"
    return outMsgQueue
  }

  def genPyApplicationOutQueue(inPort: AadlPort): ST = {
    val portName = inPort.identifier

    val outMsgQueue: ST =
      st"self.applicationOut_${portName} = deque()"
    return outMsgQueue
  }

  def genPyFileMsgTypeIncludes(packageName: String, msgTypes: ISZ[String]): ISZ[ST] = {
    var includes: ISZ[ST] = IS()

    for (msgType <- msgTypes) {
      includes = includes :+ st"from ${packageName}_interfaces.msg import ${msgType}"
    }

    return includes
  }

  def genPyTopicPublishMethod(outPort: AadlPort, portType: String, inputPortCount: Z): ST = {
    val portName = genPortName(outPort)
    val handlerName = outPort.identifier

    var publishers: ISZ[ST] = IS()
    if (inputPortCount == 1) {
      publishers = publishers :+
        st"self.${portName}_publisher_.publish(msg)"
    }
    else {
      for (i <- 1 to inputPortCount) {
        publishers = publishers :+
          st"self.${portName}_publisher_${i}.publish(msg)"
      }
    }

    var publisherCode: ST = st""

    if (isEventPort(portType)) {
      publisherCode =
        st"""def put_${handlerName}(self):
          |    msg = ${portType}()
          |    ${(publishers, "\n")}
        """
    }
    else {
      publisherCode =
        st"""def put_${handlerName}(self, msg):
          |    ${(publishers, "\n")}
        """
    }
    return publisherCode
  }

  def genPyCallbackGroupVar(): ST = {
    val callbackGroup: ST =
      st"self.${callback_group_name} = ${callback_group_type}()"
    return callbackGroup
  }

  def genPyTimeTriggeredBaseMethod(): ST = {
    val timeTriggered: ST =
      st"""def timeTriggered(self):
          |    raise NotImplementedError("Subclasses must implement this method")
        """
    return timeTriggered
  }

  def genPyTimeTriggeredStrict(component: AadlThread): ST = {
    val period = component.period.get / 1000

    val timer: ST =
      st"""self.periodTimer_ = self.create_timer(${period}, self.timeTriggeredCaller, callback_group=self.${callback_group_name})"""
    return timer
  }

  def genPyTimeTriggeredTimer(component: AadlThread): ST = {
    val period = component.period.get / 1000

    val timer: ST =
      st"""self.periodTimer_ = self.create_timer(${period}, self.timeTriggered, callback_group=self.${callback_group_name})"""
    return timer
  }

  def genPyInDataPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"[self.infrastructureIn_${name}, self.applicationIn_${name}]"
    }

    val vector: ST =
      st"""self.inDataPortTupleVector = [
        |    ${(tuples, ",\n")}
        |]
      """
    return vector
  }

  def genPyInEventPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"[self.infrastructureIn_${name}, self.applicationIn_${name}]"
    }

     val vector: ST =
       st"""self.inEventPortTupleVector = [
         |    ${(tuples, ",\n")}
         |]
       """
     return vector
  }

  def genPyOutPortTupleVector(portNames: ISZ[String]): ST = {
    var tuples: ISZ[String] = IS()

    for (name <- portNames) {
      tuples = tuples :+ s"[self.applicationOut_${name}, self.infrastructureOut_${name}, self.sendOut_${name}]"
    }

    val vector: ST =
      st"""self.outPortTupleVector = [
        |    ${(tuples, ",\n")}
        |]
      """
    return vector
  }

  def genPyTimeTriggeredCaller(): ST = {
    val timeTriggered: ST =
      st"""def timeTriggeredCaller(self):
        |    self.receiveInputs()
        |    self.timeTriggered()
        |    self.sendOutputs()
      """
    return timeTriggered
  }

  def genPyReceiveInputsSporadic(): ST = {
    val method: ST =
      st"""def receiveInputs(self, infrastructureQueue, applicationQueue):
        |    if not(len(infrastructureQueue) == 0):
        |        eventMsg = infrastructureQueue[0]
        |        infrastructureQueue.pop()
        |        self.enqueue(applicationQueue, eventMsg)
        |
        |    for port in self.inDataPortTupleVector:
        |        infrastructureQueue = port[0]
        |        if not(len(infrastructureQueue) == 0):
        |            msg = infrastructureQueue[0]
        |            self.enqueue(port[1], msg)
      """
    return method
  }

  def genPyReceiveInputsPeriodic(): ST = {
    val method: ST =
      st"""def receiveInputs(self):
        |    for port in self.inDataPortTupleVector:
        |        infrastructureQueue = port[0]
        |        if not(len(infrastructureQueue) == 0):
        |            msg = infrastructureQueue[0]
        |            self.enqueue(port[1], msg)
        |
        |    for port in self.inEventPortTupleVector:
        |        infrastructureQueue = port[0]
        |        if not(len(infrastructureQueue) == 0):
        |            msg = infrastructureQueue[0]
        |            infrastructureQueue.pop()
        |            self.enqueue(port[1], msg)
      """
    return method
  }

  def genPyEnqueue(): ST = {
    val method: ST =
      st"""def enqueue(self, queue, val):
          |    if len(queue) >= 1:
          |        queue.pop()
          |    queue.append(val)
        """
    return method
  }

  def genPySendOutputs(): ST = {
    val method: ST =
      st"""def sendOutputs(self):
          |    for port in self.outPortTupleVector:
          |        applicationQueue = port[0]
          |        if len(applicationQueue) != 0:
          |            msg = applicationQueue[0]
          |            applicationQueue.pop()
          |            self.enqueue(port[1], msg)
          |
          |    for port in self.outPortTupleVector:
          |        infrastructureQueue = port[1]
          |        if len(infrastructureQueue) != 0:
          |            msg = infrastructureQueue[0]
          |            infrastructureQueue.pop()
          |            (port[2])(msg)
        """
    return method
  }

  def genPyBaseNodePyFile(packageName: String, component: AadlThread, connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                          datatypeMap: Map[AadlType, (String, ISZ[String])], strictAADLMode: B,
                          invertTopicBinding: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = s"${genNodeName(component)}_base"
    val fileName = genPyNodeSourceName(nodeName)

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
    var subscriptionHandlerMethods: ISZ[ST] = IS()
    var msgTypes: ISZ[String] = IS()

    var inMsgVars: ISZ[ST] = IS()
    var outMsgVars: ISZ[ST] = IS()

    var hasInPorts = F
    for (p <- component.getPorts()) {
      val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!ISZOps(msgTypes).contains(portDatatype)) {
        msgTypes = msgTypes :+ portDatatype
      }
      if (strictAADLMode) {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializers = dataPortInitializers :+ genPyDataPortInitializerStrict(p, nodeName, portDatatype)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              val outputPortNames = getPortNames(outputPorts)
              subscribers = subscribers :+ genPyTopicSubscriptionStrict(p, portDatatype, outputPortNames)
            }
            else {
              subscribers = subscribers :+ genPyTopicSubscriptionStrict(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            }
          }
          else {
            subscribers = subscribers :+ genPyTopicSubscriptionStrict(p, portDatatype, getPortNames(IS(p.path.toISZ)))
          }

          if (isSporadic(component) && !p.isInstanceOf[AadlDataPort]) {
            strictSubscriptionMessageAcceptorMethods = strictSubscriptionMessageAcceptorMethods :+ genPyThread(p, portDatatype)
          }

          strictSubscriptionMessageAcceptorMethods = strictSubscriptionMessageAcceptorMethods :+ genPyMessageAcceptor(p, isSporadic(component), portDatatype)

          inMsgVars = inMsgVars :+ genPyInfrastructureInQueue(p)
          inMsgVars = inMsgVars :+ genPyApplicationInQueue(p)

          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            inPortNames = inPortNames :+ p.identifier
            subscriptionMessageGetters = subscriptionMessageGetters :+ genPyGetApplicationInValue(p, portDatatype)
          }
          else {
            subscriptionHandlerMethods = subscriptionHandlerMethods :+
              genPySubscriptionHandlerVirtualStrict(p, portDatatype)
            subscriptionHandlerMethods = subscriptionHandlerMethods :+
              genPySubscriptionHandlerBaseSporadic(p, portDatatype)
          }
          hasInPorts = T
        }
        else {
          outMsgVars = outMsgVars :+ genPyInfrastructureOutQueue(p)
          outMsgVars = outMsgVars :+ genPyApplicationOutQueue(p)
          outPortNames = outPortNames :+ p.identifier
          if (invertTopicBinding) {
            publishers = publishers :+ genPyTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            publisherMethods = publisherMethods :+
              genPyTopicPublishMethodStrict(p, portDatatype, 1)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              val inputPortNames = getPortNames(inputPorts)
              publishers = publishers :+ genPyTopicPublisher(p, portDatatype, inputPortNames)
              publisherMethods = publisherMethods :+
                genPyTopicPublishMethodStrict(p, portDatatype, inputPortNames.size)
            }
            else {
              // Out ports with no connections should still publish to a topic
              publishers = publishers :+ genPyTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
              publisherMethods = publisherMethods :+
                genPyTopicPublishMethodStrict(p, portDatatype, 1)
            }
          }
          strictPutMsgMethods = strictPutMsgMethods :+ genPyPutMsgMethodStrict(p, portDatatype)
        }
      }
      else {
        if (p.direction == Direction.In) {
          if (p.isInstanceOf[AadlDataPort]) {
            dataPortInitializers = dataPortInitializers :+ genPyDataPortInitializer(p)
          }

          if (invertTopicBinding) {
            if (connectionMap.get(p.path).nonEmpty) {
              val outputPorts = connectionMap.get(p.path).get
              val outputPortNames = getPortNames(outputPorts)
              subscribers = subscribers :+ genPyTopicSubscription(p, portDatatype, outputPortNames)
            }
            else {
              // In ports with no connections should still subscribe to a topic
              subscribers = subscribers :+
                genPyTopicSubscription(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            }
          }
          else {
            subscribers = subscribers :+
              genPyTopicSubscription(p, portDatatype, getPortNames(IS(p.path.toISZ)))
          }
          // Specifically for event ports, not eventdata ports (no data to be handled)
          if (isEventPort(portDatatype)) {
            eventPortHandlers = eventPortHandlers :+ genPyEventPortHandler(p, portDatatype)
          }
          if (!isSporadic(component) || p.isInstanceOf[AadlDataPort]) {
            subscriberMethods = subscriberMethods :+
              genPySubscriptionHandlerPeriodic(p, portDatatype)
            subscriptionMessageGetters = subscriptionMessageGetters :+ genPyGetSubscriptionMessage(p, nodeName)
            inMsgVars = inMsgVars :+ genPySubscriptionMessageVar(p)
          } else {
            subscriptionHandlerMethods = subscriptionHandlerMethods :+
              genPySubscriptionHandlerVirtual(p, portDatatype)
          }
          hasInPorts = T
        }
        else {
          if (invertTopicBinding) {
            publishers = publishers :+ genPyTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
            publisherMethods = publisherMethods :+
              genPyTopicPublishMethod(p, portDatatype, 1)
          }
          else {
            if (connectionMap.get(p.path).nonEmpty) {
              val inputPorts = connectionMap.get(p.path).get
              val inputPortNames = getPortNames(inputPorts)
              publishers = publishers :+ genPyTopicPublisher(p, portDatatype, inputPortNames)
              publisherMethods = publisherMethods :+
                genPyTopicPublishMethod(p, portDatatype, inputPortNames.size)
            }
            else {
              // Out ports with no connections should still publish to a topic
              publishers = publishers :+ genPyTopicPublisher(p, portDatatype, getPortNames(IS(p.path.toISZ)))
              publisherMethods = publisherMethods :+
                genPyTopicPublishMethod(p, portDatatype, 1)
            }
          }
        }
      }
    }

    val typeIncludes: ISZ[ST] = genPyFileMsgTypeIncludes(packageName, msgTypes)
    var stdIncludes: ST =
      st"""from collections import deque"""

    if (strictAADLMode) {
      stdIncludes =
        st"""${stdIncludes}
            |from typing import Union
            |import threading"""
    }

    if (!strictAADLMode && subscribers.size > 0) {
      stdIncludes =
        st"""${stdIncludes}
            |from ${packageName}.user_code.${genNodeName(component)}_src import *"""
    }

    var fileBody =
      st"""#!/usr/bin/env python3
        |import rclpy
        |from rclpy.node import Node
        |${(stdIncludes, "\n")}
        |from rclpy.callback_groups import ${callback_group_type}
        |${(typeIncludes, "\n")}
        |
        |#========================================================
        |# Re-running Codegen will overwrite changes to this file
        |#========================================================
        |
        |class ${nodeName}(Node):
        |    def __init__(self):
        |        super().__init__("${genNodeName(component)}")
        |
        |        ${genPyCallbackGroupVar()}
      """

    if (strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |        self.lock_ = threading.Lock()
          """
    }

    fileBody =
      st"""${fileBody}
         |        # Setting up connections
         |        ${(subscribers ++ publishers, "\n")}"""

    if(!isSporadic(component)) {
      if (strictAADLMode) {
        fileBody =
          st"""${fileBody}
             |        # timeTriggeredCaller callback timer
             |        ${genPyTimeTriggeredStrict(component)}
           """
      }
      else {
        fileBody =
          st"""${fileBody}
             |        # timeTriggered callback timer
             |        ${genPyTimeTriggeredTimer(component)}
           """
      }
    }
      if (inMsgVars.size > 0) {
        fileBody =
          st"""${fileBody}
              |        ${(inMsgVars, "\n")}
            """
      }

      if (outMsgVars.size > 0) {
        fileBody =
          st"""${fileBody}
              |        ${(outMsgVars, "\n")}
            """
      }

    if(strictAADLMode) {
      fileBody =
        st"""${fileBody}
            |        # Used by receiveInputs
            |        ${genPyInDataPortTupleVector(inPortNames)}"""

      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
              |        # Used by receiveInputs
              |        ${genPyInEventPortTupleVector(inPortNames)}"""
      }

      fileBody =
        st"""${fileBody}
           |        # Used by sendOutputs
           |        ${genPyOutPortTupleVector(outPortNames)}"""
    }

    if (dataPortInitializers.size > 0) {
      fileBody =
        st"""${fileBody}
            |    ${(dataPortInitializers, "\n\n")}
          """
    }

    fileBody =
      st"""${fileBody}
          |    ${genPyTimeTriggeredBaseMethod()}"""

    if (subscriberMethods.size > 0 || publisherMethods.size > 0 || (strictAADLMode && subscribers.size > 0)) {
      fileBody =
        st"""${fileBody}
            |    #=================================================
            |    #  C o m m u n i c a t i o n
            |    #=================================================
          """

      if (strictSubscriptionMessageAcceptorMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(strictSubscriptionMessageAcceptorMethods, "\n")}"""
      }

      if (subscriberMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(subscriberMethods, "\n")}"""
      }

      if (subscriptionMessageGetters.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(subscriptionMessageGetters, "\n")}"""
      }

      if (eventPortHandlers.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(eventPortHandlers, "\n")}"""
      }

      if (publisherMethods.size > 0) {
        fileBody =
          st"""${fileBody}
              |    ${(publisherMethods, "\n")}
              |    ${(strictPutMsgMethods, "\n")}"""
      }
    }

    if (subscriptionHandlerMethods.size > 0) {
      fileBody =
        st"""${fileBody}
           |    #=================================================
           |    #  C o m p u t e    E n t r y    P o i n t
           |    #=================================================
           |    ${(subscriptionHandlerMethods, "\n")}
         """
    }

    if (strictAADLMode) {
      if (!isSporadic(component)) {
        fileBody =
          st"""${fileBody}
             |    ${genPyTimeTriggeredCaller()}"""
      }

      val receiveInputs: ST = if (isSporadic(component)) genPyReceiveInputsSporadic()
                              else genPyReceiveInputsPeriodic()

      fileBody =
        st"""${fileBody}
            |    ${receiveInputs}
            |    ${genPyEnqueue()}
            |    ${genPySendOutputs()}"""
    }

    val filePath: ISZ[String] = IS("src", packageName, packageName, "base_code", fileName)

    return (filePath, fileBody, T, IS())
  }

  def genPySubscriptionHandlerSporadicStrict(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader =
        st"""def handle_${handlerName}(self):
            |    # Handle ${handlerName} event
            |    self.get_logger().info("Received ${handlerName}")
          """
    }
    else {
      subscriptionHandlerHeader =
        st"""def handle_${handlerName}(self, msg):
            |    # Handle ${handlerName} msg
            |    self.get_logger().info(f"Received ${handlerName}: {self.message_to_string(msg)}")
          """
    }
    return subscriptionHandlerHeader
  }

  def genPySubscriptionHandlerSporadic(inPort: AadlPort, portType: String): ST = {
    val handlerName = inPort.identifier

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader =
        st"""def handle_${handlerName}(self):
            |    # Handle ${handlerName} event
            |    self.get_logger().info("Received ${handlerName}")
          """
    }
    else {
      subscriptionHandlerHeader =
        st"""def handle_${handlerName}(self, msg):
            |    # Handle ${handlerName} msg
            |    self.get_logger().info(f"Received ${handlerName}: {self.message_to_string(msg)}")
          """
    }
    return subscriptionHandlerHeader
  }

  def genPySubscriptionHandlerSporadicStrictWithExamples(inPort: AadlPort, portType: String,
                                                         inDataPorts: ISZ[AadlPort]): ST = {
    val handlerName = inPort.identifier

    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"# Example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {
        exampleUsage =
          st"""${exampleUsage}
              |${inDataPort.identifier} = self.get_${inDataPort.identifier}()
              |self.get_logger().info(f"Received ${inDataPort.identifier}: {self.message_to_string(${inDataPort.identifier})}")
            """
      }
    }

    var subscriptionHandlerHeader: ST = st""
    if (isEventPort(portType)) {
      subscriptionHandlerHeader = st"""def handle_${handlerName}(self):
                                      |    # Handle ${handlerName} event
                                      |    self.get_logger().info("Received ${handlerName}")
                                    """
    }
    else {
      subscriptionHandlerHeader = st"""def handle_${handlerName}(self, msg):
                                      |    # Handle ${handlerName} msg
                                      |    self.get_logger().info(f"Received ${handlerName}: {self.message_to_string(msg)}")
                                    """
    }

    if (inDataPorts.size > 0) {
      subscriptionHandlerHeader =
        st"""${subscriptionHandlerHeader}
            |
            |    ${exampleUsage}"""
    }

    return subscriptionHandlerHeader
  }

  def genPyExamplePublisher(outPort: AadlPort, packageName: String,
                            datatypeMap: Map[AadlType, (String, ISZ[String])],
                            reporter: Reporter): ST = {
    val handlerName = outPort.identifier
    val dataPortType: String = genPortDatatype(outPort, packageName, datatypeMap, reporter)

    var publisherCode: ST = st""

    if (isEventPort(dataPortType)) {
      publisherCode =
        st"self.put_${handlerName}()"
    } else {
      publisherCode =
        st"""${handlerName} = ${dataPortType}()
            |self.put_${handlerName}(${handlerName})
          """
    }

    return publisherCode
  }

  def genPyTimeTriggeredMethod(inDataPorts: ISZ[AadlPort], examplePublishers: ISZ[ST],
                               strictAADLMode: B): ST = {
    var exampleUsage: ST = st""
    if (inDataPorts.size > 0) {
      exampleUsage = st"# Example receiving messages on data ports"
      for (inDataPort <- inDataPorts) {

        if (strictAADLMode) {
          exampleUsage =
            st"""${exampleUsage}
                |${inDataPort.identifier} = self.get_${inDataPort.identifier}()
                |self.get_logger().info(f"Received ${inDataPort.identifier}: {self.message_to_string(${inDataPort.identifier})}")
              """
        }
        else {
          exampleUsage =
            st"""${exampleUsage}
                |${inDataPort.identifier} = self.get_${inDataPort.identifier}()
                |self.get_logger().info(f"Received ${inDataPort.identifier}: {self.message_to_string(${inDataPort.identifier})}")
              """
        }
      }
    }

    var timeTriggered: ST =
      st"""def timeTriggered(self):
          |    # Handle communication
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
            |    # Example publishing messages
            |    ${(examplePublishers, "\n")}"""
    }

    return timeTriggered
  }

  def genPyUserDataPortInitializers(inDataPorts: ISZ[AadlPort], packageName: String,
                                     datatypeMap: Map[AadlType, (String, ISZ[String])], reporter: Reporter): ISZ[ST] = {
    var initializers: ISZ[ST] = IS()

    for (p <- inDataPorts) {
      val portDatatype = genPortDatatype(p, packageName, datatypeMap, reporter)
      val portName = p.identifier

      initializers = initializers :+
        st"""${portName} = ${portDatatype}()
            |self.init_${portName}(${portName})
          """
    }

    return initializers
  }

  def genPyUserNodePyFile(packageName: String, component: AadlThread, datatypeMap: Map[AadlType, (String, ISZ[String])],
                          hasConverterFiles: B, strictAADLMode: B, reporter: Reporter): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genPyNodeSourceName(nodeName)
    var examplePublishers: ISZ[ST] = IS()
    var inDataPorts: ISZ[AadlPort] = IS()
    var msgTypes: ISZ[String] = IS()

    val startMarker: String = "# Additions within these tags will be preserved when re-running Codegen"
    val endMarker: String = "# Additions within these tags will be preserved when re-running Codegen"

    for (p <- component.getPorts()) {
      if (p.direction == Direction.Out) {
        examplePublishers = examplePublishers :+ genPyExamplePublisher(p, packageName, datatypeMap, reporter)
      }
      else if (p.direction == Direction.In && p.isInstanceOf[AadlDataPort]) {
        inDataPorts = inDataPorts :+ p
      }

      val portDatatype: String = genPortDatatype(p, packageName, datatypeMap, reporter)
      if (!ISZOps(msgTypes).contains(portDatatype)) {
        msgTypes = msgTypes :+ portDatatype
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
                genPySubscriptionHandlerSporadicStrictWithExamples(p, portDatatype, inDataPorts)
                firstSubscriptionHandler = false
            } else {
              subscriptionHandlers = subscriptionHandlers :+
                genPySubscriptionHandlerSporadicStrict(p, portDatatype)
            }
          }
          else {
            if (firstSubscriptionHandler) {
              subscriptionHandlers = subscriptionHandlers :+
                genPySubscriptionHandlerSporadicWithExamples(p, portDatatype, inDataPorts)
              firstSubscriptionHandler = false
            } else {
              subscriptionHandlers = subscriptionHandlers :+
                genPySubscriptionHandlerSporadic(p, portDatatype)
            }
          }
        }
      }
    }
    else {
      subscriptionHandlers = subscriptionHandlers :+ genPyTimeTriggeredMethod(inDataPorts, examplePublishers, strictAADLMode)
    }

    val inDataPortInitializers: ISZ[ST] = genPyUserDataPortInitializers(inDataPorts, packageName, datatypeMap, reporter)

    val typeIncludes: ISZ[ST] = genPyFileMsgTypeIncludes(packageName, msgTypes)

    var includeFiles: ST =
      st"""#!/usr/bin/env python3
          |import rclpy
          |from rclpy.node import Node
          |from rosidl_runtime_py.convert import message_to_yaml
          |from ${packageName}.base_code.${nodeName}_base_src import ${nodeName}_base"""

    if (typeIncludes.size != 0) {
      includeFiles =
        st"""${includeFiles}
            |${(typeIncludes, "\n")}"""
    }

    if (hasConverterFiles) {
      includeFiles =
        st"""${includeFiles}
          |from ${packageName}.base_code.enum_converter import *"""
    }

    var fileBody =
      st"""${includeFiles}
          |
          |#===========================================================
          |# This file will not be overwritten when re-running Codegen
          |#===========================================================
          |class ${nodeName}(${nodeName}_base):
          |    def __init__(self):
          |        super().__init__()
          |        # invoke initialize entry point
          |        self.initialize()
          |
          |        self.get_logger().info("${nodeName} infrastructure set up")
          |
          |#=================================================
          |#  I n i t i a l i z e    E n t r y    P o i n t
          |#=================================================
          |    def initialize(self):
          |        self.get_logger().info("Initialize Entry Point invoked")
          |
          |        # Initialize the node
          |
          |        # Initialize the node's incoming data port values here
        """

    if (inDataPortInitializers.size != 0) {
      fileBody =
        st"""${fileBody}
          |        # Initialize the node's incoming data port values here
          |        ${(inDataPortInitializers, "\n")}"""
    }

    fileBody =
      st"""${fileBody}
         |
         |#=================================================
         |#  C o m p u t e    E n t r y    P o i n t
         |#=================================================
         |    ${genPyMessageToString()}
         |    ${(subscriptionHandlers, "\n")}
       """

    fileBody =
      st"""${fileBody}
          |#=================================================
          |#  Include any additional declarations here
          |#=================================================
          |${startMarker}
          |
          |${endMarker}
        """

    val filePath: ISZ[String] = IS("src", packageName, packageName, "user_code", fileName)

    return (filePath, fileBody, F, IS())
  }

  def genPyNodeRunnerName(compNameS: String): String = {
    // create runner file name
    val nodeNameT: String = s"${compNameS}${py_node_runner_name_suffix}"
    return nodeNameT
  }

  def genPyNodeRunnerFile(packageName: String, component: AadlThread): (ISZ[String], ST, B, ISZ[Marker]) = {
    val nodeName = genNodeName(component)
    val fileName = genPyNodeRunnerName(nodeName)

    val fileBody =
      st"""#!/usr/bin/env python3
          |import rclpy
          |from rclpy.node import Node
          |from rclpy.executors import MultiThreadedExecutor
          |from ${packageName}.user_code.${nodeName}_src import ${nodeName}
          |#========================================================
          |# Re-running Codegen will overwrite changes to this file
          |#========================================================
          |def main(args=None):
          |    rclpy.init(args=args)
          |    node = ${nodeName}()
          |    executor = MultiThreadedExecutor()
          |    executor.add_node(node)
          |    executor.spin()
          |    rclpy.shutdown()
          |
          |if __name__ == "__main__":
          |    main()
        """

    val filePath: ISZ[String] = IS("src", packageName, packageName, "base_code", fileName)

    return (filePath, fileBody, T, IS())
  }

  def genPyNodeFiles(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                     datatypeMap: Map[AadlType, (String, ISZ[String])], hasConverterFiles: B, strictAADLMode: B,
                     invertTopicBinding: B, reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genPyPackageName(modelName)
    var py_files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    for (comp <- threadComponents) {
      py_files =
        py_files :+ genPyBaseNodePyFile(top_level_package_nameT, comp, connectionMap, datatypeMap, strictAADLMode, invertTopicBinding, reporter)
      py_files =
        py_files :+ genPyUserNodePyFile(top_level_package_nameT, comp, datatypeMap, hasConverterFiles, strictAADLMode, reporter)
      py_files =
        py_files :+ genPyNodeRunnerFile(top_level_package_nameT, comp)
    }
    return py_files
  }

  def genPyEnumConverters(enumTypes: ISZ[(String, AadlType)], strictAADLMode: B): ISZ[ST] = {
    var converters: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")
      val enumValues: ISZ[String] = enum._2.asInstanceOf[EnumType].values

      var cases: ISZ[ST] = IS()

      for (value <- enumValues) {
        cases = cases :+
          st"""case ${enumName}.${StringOps(enum._1).toUpper}_${StringOps(value).toUpper}:
              |    return "${enumName} ${value}""""
      }

      //Only difference was a pointer?
      if (strictAADLMode) {
        converters = converters :+
          st"""def enumToString(value):
              |    typedValue = ${enumName}()
              |    typedValue.data = value
              |    match (typedValue.${enum._1}):
              |        ${(cases, "\n")}
              |        case default:
              |            return "Unknown value for ${enumName}"
            """
      }
      else {
        converters = converters :+
          st"""def enumToString(value):
              |    typedValue = ${enumName}()
              |    typedValue.data = value
              |    match (typedValue.${enum._1}):
              |        ${(cases, "\n")}
              |        case default:
              |            return "Unknown value for ${enumName}"
            """
      }
    }

    return converters
  }

  def genPyEnumConverterFile(packageName: String, enumTypes: ISZ[(String, AadlType)],
                              strictAADLMode: B): (ISZ[String], ST, B, ISZ[Marker]) = {
    var includes: ISZ[ST] = IS()

    for (enum <- enumTypes) {
      val enumName: String = ops.StringOps(enum._2.classifier.apply(enum._2.classifier.size - 1)).replaceAllLiterally("_", "")
      includes = includes :+ st"from ${packageName}_interfaces.msg import ${enumName}"
    }

    val fileBody =
      st"""#!/usr/bin/env python3
          |${(includes, "\n")}
          |
          |#========================================================
          |# Re-running Codegen will overwrite changes to this file
          |#========================================================
          |
          |${(genPyEnumConverters(enumTypes, strictAADLMode), "\n")}
        """

    val filePath: ISZ[String] = IS("src", packageName, packageName, "base_code", "enum_converter.py")

    return (filePath, fileBody, T, IS())
  }

  def genPyEnumConverterFiles(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])],
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
    val packageName: String = genPyPackageName(modelName)

    files = files :+ genPyEnumConverterFile(packageName, enumTypes, strictAADLMode)

    return files
  }

  //================================================
  //  P a c k a g e   G e n e r a t o r s
  //================================================
  def genPyNodePkg(modelName: String, threadComponents: ISZ[AadlThread], connectionMap: Map[ISZ[String], ISZ[ISZ[String]]],
                   datatypeMap: Map[AadlType, (String, ISZ[String])], strictAADLMode: B, invertTopicBinding: B,
                   reporter: Reporter): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    val converterFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = genPyEnumConverterFiles(modelName, datatypeMap, strictAADLMode)
    val hasConverterFiles: B = (converterFiles.size > 0)
    val top_level_package_nameT: String = genPyPackageName(modelName)

    files = files ++ converterFiles
    files = files ++ genPyNodeFiles(modelName, threadComponents, connectionMap, datatypeMap, hasConverterFiles, strictAADLMode,
                                    invertTopicBinding, reporter)

    files = files :+ genPyInitFile(top_level_package_nameT)
    files = files :+ genPySubInitFile(modelName, "base_code")
    files = files :+ genPySubInitFile(modelName, "user_code")
    files = files :+ genPyResourceFile(modelName)
    files = files :+ genPySetupFile(modelName, threadComponents)
    files = files :+ genXmlPackageFile(modelName)
    files = files :+ genCfgSetupFile(modelName)
    files = files :+ genPyCopyrightFile(modelName)
    files = files :+ genPyFlakeFile(modelName)
    files = files :+ genPyPrepFile(modelName)

    return files
  }
}
