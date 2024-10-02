// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._

object CMakeTemplate {

  def cmake_settingsFilename(libName: String): String = {
    return s"settings_${libName}.cmake"
  }

  def cmake_add_definition(defs: ISZ[String]): ST = {
    return st"add_definitions(${(defs, "\n")})"
  }

  def cmake_sel4_addCamkesDefinition(): String = {
    return cmake_add_definition(ISZ("-DCAMKES")).render
  }

  def cmake_sel4_add_muscl(libraryName: String): ST = {
    val ret: ST =
      st"""if(TARGET muslc)
          |  target_link_libraries(${libraryName}
          |                        muslc)
          |endif()"""
    return ret
  }

  def cmake_sel4_settings_cmake(libraryName: String): ST = {
    val ret: ST =
      st"""${cmake_sel4_addCamkesDefinition()}
          |
          |${cmake_sel4_add_muscl(libraryName)}"""
    return ret
  }
}
