// #Sireum

package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlProcess, AadlSystem, AadlThread}

object GeneratorLaunch {

  val node_executable_filename_suffix: String = "_exe"
  val launch_node_decl_suffix: String = "_node"
  val py_package_name_suffix: String = "_py_pkg"
  val cpp_package_name_suffix: String = "_cpp_pkg"
  val py_launch_file_name_suffix: String = ".launch.py"
  val xml_launch_file_name_suffix: String = ".launch.xml"

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
  //  L a u n c h  File Setup Files
  //================================================
  def genLaunchCMakeListsFile(top_level_package_nameT: String): (ISZ[String], ST, B, ISZ[Marker]) = {
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

  def genLaunchPackageFile(top_level_package_nameT: String): (ISZ[String], ST, B, ISZ[Marker]) = {
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
  //  L a u n c h  XML Files
  //================================================

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

  def genXmlLaunchFileName(compNameS: String): String = {
    // create launch file name
    val nodeNameT: String = s"${compNameS}${xml_launch_file_name_suffix}"
    return nodeNameT
  }

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

  def genXmlFormatLaunchFiles(modelName: String, threadComponents: ISZ[AadlThread],
                              systemComponents: ISZ[AadlSystem], top_level_package_nameT: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
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

  def genXmlLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread], systemComponents: ISZ[AadlSystem], launchType: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genLaunchPackageType(modelName, launchType)
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    files = files ++ genXmlFormatLaunchFiles(modelName, threadComponents, systemComponents, top_level_package_nameT)
    files = files :+ genLaunchCMakeListsFile(top_level_package_nameT)
    files = files :+ genLaunchPackageFile(top_level_package_nameT)

    return files
  }

  //================================================
  //  L a u n c h  Python Files
  //================================================
  def genPyLaunchFileName(compNameS: String): String = {
    // create launch file name
    val nodeNameT: String = s"${compNameS}${py_launch_file_name_suffix}"
    return nodeNameT
  }

  def genPyFormatLaunchNodeDeclName(componentNameS: String): String = {
    // create target launch node decl name
    val launch_node_decl_nameT: String = s"${componentNameS}${launch_node_decl_suffix}"
    return launch_node_decl_nameT
  }

  def genPyFormatLaunchNodeDecl(top_level_package_nameT: String,
                                thread: AadlThread): ST = {
    val node_executable_file_nameT = genExecutableFileName(genNodeName(thread))
    val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(genNodeName(thread))
    val s =
      st"""${launch_node_decl_nameT} = Node(
          |    package = "${top_level_package_nameT}",
          |    executable = "${node_executable_file_nameT}"
          |)
        """
    return s
  }

  def genPyFormatLaunchSystemDecl(top_level_package_nameT: String,
                                  system: AadlSystem): ST = {
    val launch_node_decl_nameT = genPyFormatLaunchNodeDeclName(system.identifier)
    val launchFileName: String = genPyLaunchFileName(system.identifier)
    val s =
      st"""${launch_node_decl_nameT} = IncludeLaunchDescription(
          |    PythonLaunchDescriptionSource(
          |        os.path.join(get_package_share_directory('${top_level_package_nameT}_bringup'),
          |                     'launch/${launchFileName}')
          |    )
          |)
      """
    return s
  }

  def genPyFormatLaunchDecls(component: AadlComponent, packageName: String): ISZ[ST] = {
    var launch_decls: ISZ[ST] = IS()

    for (comp <- component.subComponents) {
      comp match {
        case thread: AadlThread =>
          launch_decls = launch_decls :+ genPyFormatLaunchNodeDecl(packageName, thread)
        case system: AadlSystem =>
          launch_decls = launch_decls :+ genPyFormatLaunchSystemDecl(packageName, system)
        case process: AadlProcess =>
          launch_decls = launch_decls ++ genPyFormatLaunchDecls(process, packageName)
        case _ =>
      }
    }

    return launch_decls
  }

  def genPyFormatLaunchAddAction(component: AadlComponent): ISZ[ST] = {
    var ld_entries: ISZ[ST] = IS()

    for(comp <- component.subComponents) {
      comp match {
        case thread: AadlThread =>
          ld_entries = ld_entries :+ st"""ld.add_action(${genPyFormatLaunchNodeDeclName(genNodeName(thread))})"""
        case system: AadlSystem =>
          ld_entries = ld_entries :+ st"""ld.add_action(${genPyFormatLaunchNodeDeclName(system.identifier)})"""
        case process: AadlProcess =>
          ld_entries = ld_entries ++ genPyFormatLaunchAddAction(process)
        case _ =>
      }
    }

    return ld_entries
  }

  def genPyFormatLaunchFile(modelName: String, threadComponents: ISZ[AadlThread],
                            systemComponents: ISZ[AadlSystem], top_level_package_nameT: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    var launchFiles: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()

    for (system <- systemComponents) {
      val fileName = genPyLaunchFileName(system.identifier)

      val node_decls: ISZ[ST] = genPyFormatLaunchDecls(system, top_level_package_nameT)
      val ld_entries: ISZ[ST] = genPyFormatLaunchAddAction(system)

      val launchFileBody =
        st"""from launch import LaunchDescription
            |from launch_ros.actions import Node
            |
            |import os
            |from ament_index_python.packages import get_package_share_directory
            |from launch.actions import IncludeLaunchDescription
            |from launch.launch_description_sources import PythonLaunchDescriptionSource
            |
            |def generate_launch_description():
            |    ld = LaunchDescription()
            |
            |    ${(node_decls, "\n")}
            |    ${(ld_entries, "\n")}
            |
            |    return ld
          """

      val filePath: ISZ[String] = IS("src", s"${top_level_package_nameT}_bringup", "launch", fileName)

      launchFiles = launchFiles :+ (filePath, launchFileBody, T, IS())
    }

    return launchFiles
  }

  def genPyLaunchPkg(modelName: String, threadComponents: ISZ[AadlThread], systemComponents: ISZ[AadlSystem], nodeLanguage: String): ISZ[(ISZ[String], ST, B, ISZ[Marker])] = {
    val top_level_package_nameT: String = genLaunchPackageType(modelName, nodeLanguage)
    var files: ISZ[(ISZ[String], ST, B, ISZ[Marker])] = IS()
    files = files :+ genLaunchCMakeListsFile(top_level_package_nameT)
    files = files :+ genLaunchPackageFile(top_level_package_nameT)
    files = files ++ genPyFormatLaunchFile(modelName, threadComponents, systemComponents, top_level_package_nameT)

    return files
  }
}
