// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource

object ResourceUtil {

  def makeCRLF(content: String): String = {
    return ops.StringOps(content).replaceAllLiterally("\n", "\r\n")
  }

  def createResource(path: String,
                     content: String,
                     overwrite: B,
                     makeExecutable: B): Resource = {
    return Resource(path, content, overwrite, makeExecutable)
  }

  def createStResource(path: String,
                       content: ST,
                       overwrite: B): Resource = {
    return createStringResource(path, content.render, overwrite)
  }

  def createStringResource(path: String,
                           content: String,
                           overwrite: B): Resource = {
    return createResource(path, content, overwrite, F)
  }

  def createExeStResource(path: String,
                          content: ST,
                          overwrite: B): Resource = {
    return createExeStringResource(path, content.render, overwrite)
  }

  def createExeStringResource(path: String,
                              content: String,
                              overwrite: B): Resource = {
    return createResource(path, content, overwrite, T)
  }
}
