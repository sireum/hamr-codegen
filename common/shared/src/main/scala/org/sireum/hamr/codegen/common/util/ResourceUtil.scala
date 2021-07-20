// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource

object ResourceUtil {

  def createResource(path: String,
                     content: ST,
                     overwrite: B): Resource = {
    return Resource(path, content, overwrite, F, F)
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): Resource = {
    return Resource(path, content, overwrite, T, F)
  }

  def createExeCrlfResource(path: String,
                        content: ST,
                        overwrite: B): Resource = {
    return Resource(path, content, overwrite, T, T)
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
