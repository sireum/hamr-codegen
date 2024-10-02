// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Marker, FileResource}

object ResourceUtil {

  def createResource(path: String,
                     content: ST,
                     overwrite: B): FileResource = {
    return createResourceH(path, content, overwrite, F)
  }

  def createResourceH(path: String,
                     content: ST,
                     overwrite: B,
                     isDatatype: B): FileResource = {
    return IResource(path, content, ISZ(), overwrite, F, F, isDatatype)
  }

  def createResourceWithMarkers(path: String,
                                content: ST,
                                markers: ISZ[Marker],
                                overwrite: B): FileResource = {
    return createResourceWithMarkersH(path, content, markers, overwrite, F)
  }

  def createResourceWithMarkersH(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 overwrite: B,
                                 isDatatype: B): FileResource = {
    return IResource(path, content, markers, overwrite, F, F, isDatatype)
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): FileResource = {
    return IResource(path, content, ISZ(), overwrite, T, F, F)
  }

  def createExeCrlfResource(path: String,
                            content: ST,
                            overwrite: B): FileResource = {
    return IResource(path, content, ISZ(), overwrite, T, T, F)
  }

  def createStringResource(path: String,
                           content: String,
                           overwrite: B): FileResource = {
    return createStringResourceH(path, content, overwrite, F)
  }

  def createStringResourceH(path: String,
                           content: String,
                           overwrite: B,
                           isDatatype: B): FileResource = {
    return createResourceH(path, st"${content}", overwrite, isDatatype)
  }

  def createExeStringResource(path: String,
                              content: String,
                              overwrite: B): FileResource = {
    return createExeResource(path, st"${content}", overwrite)
  }

  def createExternalResource(srcPath: String, dstPath: String, symlink: B): FileResource = {
    return EResource(
      srcPath = srcPath,
      dstPath = dstPath,
      symlink = symlink)
  }
}
