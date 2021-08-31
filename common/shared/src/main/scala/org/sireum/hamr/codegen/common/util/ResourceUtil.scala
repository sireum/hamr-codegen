// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Resource}

object ResourceUtil {

  def createResource(path: String,
                     content: ST,
                     overwrite: B): Resource = {
    return IResource(path, content, overwrite, F, F)
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): Resource = {
    return IResource(path, content, overwrite, T, F)
  }

  def createExeCrlfResource(path: String,
                        content: ST,
                        overwrite: B): Resource = {
    return IResource(path, content, overwrite, T, T)
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

  def createExternalResource(srcPath: String, dstPath: String, symlink: B): Resource = {
    return EResource(
      srcPath = srcPath,
      dstPath = dstPath,
      symlink = symlink)
  }
}
