// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Marker, FileResource}

object ResourceUtil {

  def createResource(path: String,
                     content: ST,
                     overwrite: B): FileResource = {
    return createResourceH(path = path, content = content, overwrite = overwrite, isDatatype = F)
  }

  def createResourceH(path: String,
                      content: ST,
                      overwrite: B,
                      isDatatype: B): FileResource = {
    return IResource(
      dstPath = path,
      content = content,
      markers = ISZ(),
      invertMarkers = F,
      overwrite = overwrite,
      makeExecutable = F,
      makeCRLF = F,
      isDatatype = isDatatype)
  }

  def createResourceWithMarkers(path: String,
                                content: ST,
                                markers: ISZ[Marker],
                                invertMarkers: B,
                                overwrite: B): FileResource = {
    return createResourceWithMarkersH(
      path = path,
      content = content,
      markers = markers,
      invertMarkers = invertMarkers,
      overwrite = overwrite,
      isDatatype = F)
  }

  def createResourceWithMarkersH(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 invertMarkers: B,
                                 overwrite: B,
                                 isDatatype: B): FileResource = {

    assert (invertMarkers -->: !overwrite, "Overwrite should be false when inverted markers are used")

    return IResource(
      dstPath = path,
      content = content,
      markers = markers,
      invertMarkers = invertMarkers,
      overwrite = overwrite,
      makeExecutable = F,
      makeCRLF = F,
      isDatatype = isDatatype)
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): FileResource = {
    return IResource(
      dstPath = path,
      content = content,
      markers = ISZ(),
      invertMarkers = F,
      overwrite = overwrite,
      makeExecutable = T,
      makeCRLF = F,
      isDatatype = F)
  }

  def createExeCrlfResource(path: String,
                            content: ST,
                            overwrite: B): FileResource = {
    return IResource(
      dstPath = path,
      content = content,
      markers = ISZ(),
      invertMarkers = F,
      overwrite = overwrite,
      makeExecutable = T,
      makeCRLF = T,
      isDatatype = F)
  }

  def createStringResource(path: String,
                           content: String,
                           overwrite: B): FileResource = {
    return createStringResourceH(
      path = path,
      content = content,
      overwrite = overwrite,
      isDatatype = F)
  }

  def createStringResourceH(path: String,
                            content: String,
                            overwrite: B,
                            isDatatype: B): FileResource = {
    return createResourceH(path = path, content = st"${content}", overwrite = overwrite, isDatatype = isDatatype)
  }

  def createExeStringResource(path: String,
                              content: String,
                              overwrite: B): FileResource = {
    return createExeResource(path = path, content = st"${content}", overwrite = overwrite)
  }

  def createExternalResource(srcPath: String, dstPath: String, symLink: B): FileResource = {
    return EResource(
      srcPath = srcPath,
      dstPath = dstPath,
      symLink = symLink)
  }
}
