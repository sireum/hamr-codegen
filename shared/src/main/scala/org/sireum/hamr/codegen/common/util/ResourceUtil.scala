// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Marker, FileResource}
import org.sireum.hamr.codegen.common.templates.CommentTemplate

object ResourceUtil {

  def checkOverwriteConsistency(path: String, content: ST, overwrite: B, invertedMarker: B): B = {
    val renderedContent = content.render

    if (invertedMarker) {
      if (!ops.StringOps(renderedContent).contains(CommentTemplate.invertedMarkerComment)) {
        halt(s"Resource at '$path' has inverted markers but it doesn't contain the inverted marker comment")
      }
      if (!overwrite) {
        halt(s"Resource at '$path' has inverted markers but its overwrite is set to F")
      }
    } else {
      if (overwrite && !ops.StringOps(renderedContent).contains(CommentTemplate.doNotEditComment)) {
        halt(s"Resource at '$path' will be overwritten, but it doesn't have a 'do not edit' comment")
      }
      if (!overwrite && !ops.StringOps(renderedContent).contains(CommentTemplate.safeToEditComment)) {
        halt(s"Resource at '$path' has will not be overwritten, but it doesn't contain a 'safe to edit' comment")
      }
    }

    return T
  }

  def createResource(path: String,
                     content: ST,
                     overwrite: B): FileResource = {
    return createResourceH(path = path, content = content, overwrite = overwrite, isDatatype = F)
  }

  def createResourceH(path: String,
                      content: ST,
                      overwrite: B,
                      isDatatype: B) : FileResource = {
    return createResourceI(path = path, content = content, overwrite = overwrite, isDatatype = isDatatype, skipCommentChecks = F)
  }

  def createResourceI(path: String,
                      content: ST,
                      overwrite: B,
                      isDatatype: B,
                      skipCommentChecks: B): FileResource = {
    assert(skipCommentChecks || checkOverwriteConsistency(path, content, overwrite, F))
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
    assert (invertMarkers -->: markers.nonEmpty, s"invertMarkers is T but markers are empty for $path")
    assert (invertMarkers -->: overwrite, s"Overwrite should be T when inverted markers are used for $path")
    assert(checkOverwriteConsistency(path, content, overwrite, invertMarkers))

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
    assert(checkOverwriteConsistency(path, content, overwrite, F))
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
    assert(checkOverwriteConsistency(path, content, overwrite, F))
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
    return createStringResourceI(path = path, content = content, overwrite = overwrite, isDatatype = isDatatype, skipCommentChecks = F)
  }

  def createStringResourceI(path: String,
                            content: String,
                            overwrite: B,
                            isDatatype: B,
                            skipCommentChecks: B): FileResource = {
    return createResourceI(path = path, content = st"${content}", overwrite = overwrite, isDatatype = isDatatype, skipCommentChecks = skipCommentChecks)
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
