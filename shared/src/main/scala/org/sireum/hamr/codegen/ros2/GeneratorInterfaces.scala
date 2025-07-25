// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.types.AadlType

object GeneratorInterfaces {

  val py_package_name_suffix: String = "_py_pkg"
  val cpp_package_name_suffix: String = "_cpp_pkg"

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

  def genLaunchPackageType(modelName: String, launchType: String): String = {
    launchType match {
      case "Cpp" => return genCppPackageName(modelName)
      case "Python" => return genPyPackageName(modelName)
    }
  }

  //================================================
  //  I n t e r f a c e s  Setup Files
  //================================================
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  // The "Empty" datatype, which has no data fields, is used for event ports

  def genMsgFile(modelName: String, datatypeName: String, datatypeContent: ISZ[String], top_level_package_nameT: String): (ISZ[String], ST, B, ISZ[Marker]) = {

    val fileBody = st"${(datatypeContent, "\n")}"

    val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_interfaces", "msg", s"${datatypeName}.msg")

    return (filePath, fileBody, T, IS())
  }

  def genMsgFiles(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])], top_level_package_nameT: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var msg_files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    for (datatype <- datatypeMap.entries) {
      msg_files = msg_files :+ genMsgFile(modelName, datatype._2._1, datatype._2._2, top_level_package_nameT)
    }
    msg_files = msg_files :+ (ISZ("src", s"${top_level_package_nameT}_interfaces", "msg", "Empty.msg"), st"", T, IS())
    return msg_files
  }

  def genInterfacesCMakeListsFile(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])], top_level_package_nameT: String): (ISZ[String], ST, B, ISZ[Marker]) = {
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

  def genInterfacesPackageFile(modelName: String, top_level_package_nameT: String): (ISZ[String], ST, B, ISZ[Marker]) = {
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

  // The same datatype package will work regardless of other packages' types
  // ROS2 data/message types are defined in a "{package_name}_interfaces" package according to convention
  def genInterfacesPkg(modelName: String, datatypeMap: Map[AadlType, (String, ISZ[String])], nodeLanguage: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genLaunchPackageType(modelName, nodeLanguage)
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genMsgFiles(modelName, datatypeMap, top_level_package_nameT)
    files = files :+ genInterfacesCMakeListsFile(modelName, datatypeMap, top_level_package_nameT)
    files = files :+ genInterfacesPackageFile(modelName, top_level_package_nameT)

    return files
  }
}
