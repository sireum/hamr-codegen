// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._

object BitCodecNameUtil {
  def numBytesConstName(qualifiedCTypeName: String): String = {
    return s"numBytes_${qualifiedCTypeName}"
  }

  def numBitsConstName(qualifiedCTypeName: String): String = {
    return s"numBits_${qualifiedCTypeName}"
  }
}

object NixSeL4NameUtil {

  def apiHelperFilename(names: NameProvider): String = {
    return s"${names.componentSingletonType}_api"
  }

  def apiHelperMethodName(portName: String, names: NameProvider): String = {
    return s"api_${portName}__${names.cComponentType}"
  }

  def apiHelperGetterMethodName(portName: String, names: NameProvider): String = {
    return apiHelperMethodName(s"get_${portName}", names)
  }

  def apiHelperSetterMethodName(portName: String, names: NameProvider): String = {
    return apiHelperMethodName(s"put_${portName}", names)
  }

  def apiHelperLoggerMethodName(loggerName: String, componentType: String): String = {
    return s"api_${loggerName}__${componentType}"
  }
}