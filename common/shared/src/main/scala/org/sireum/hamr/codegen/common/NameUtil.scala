// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._

object SeL4NixNamesUtil {

  def apiHelperFilename(names: Names): String = { return s"${names.componentImpl}_api" }

  def apiHelperMethodName(portName: String, names: Names): String = {
    return s"api_${portName}__${names.cComponentImpl}"
  }

  def apiHelperGetterMethodName(portName: String, names: Names): String = {
    return apiHelperMethodName(s"get_${portName}", names)
  }

  def apiHelperSetterMethodName(portName: String, names: Names): String = {
    return apiHelperMethodName(s"send_${portName}", names)
  }
}