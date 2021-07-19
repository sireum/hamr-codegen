// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource

object ResourceUtil {

  def makeCRLF(content: String): String = {
    //return ops.StringOps(content).replaceAllLiterally("\n", "\r\n")
    return content
  }

  def createResource(path: String,
                     content: ST,
                     overwrite: B): Resource = {
    return Resource(path, content, overwrite, F)
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): Resource = {
    return Resource(path, content, overwrite, T)
  }

  def createStringResource(path: String,
                           content: String,
                           overwrite: B): Resource = {
    return createResource(path, st"${content}", overwrite)
  }

  def createExeStringResource(path: String,
                              content: String,
                              overwrite: B): Resource = {
    return createExeResource(path, st"${content}", overwrite)
  }
}
