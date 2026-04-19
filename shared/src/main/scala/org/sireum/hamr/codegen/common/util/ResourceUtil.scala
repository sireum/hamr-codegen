// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Marker, FileResource}
import org.sireum.hamr.codegen.common.templates.{CommentStyle, CommentTemplate}

object ResourceUtil {

  def checkConsistency(path: String, content: ST, overwrite: B, invertedMarker: B, markers: ISZ[Marker]): B = {
    val renderedContent = content.render

    if (invertedMarker) {
      if (markers.isEmpty) {
        halt(s"Resource at '$path' has inverted markers but does not contain markers")
      }
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
    return createResourceI(
      path = path, content = content, overwrite = overwrite, isDatatype = isDatatype, skipConsistencyChecks = F)
  }

  def createResourceI(path: String,
                      content: ST,
                      overwrite: B,
                      isDatatype: B,
                      skipConsistencyChecks: B): FileResource = {
    return createResourceWithMarkersI(
      path = path, content = content, markers = ISZ(), invertMarkers = F, overwrite = overwrite,
      isDatatype = isDatatype, skipConsistencyChecks = skipConsistencyChecks)
  }

  def createResourceWithMarkers(path: String,
                                content: ST,
                                markers: ISZ[Marker],
                                invertMarkers: B,
                                overwrite: B): FileResource = {
    return createResourceWithMarkersH(
      path = path, content = content, markers = markers, invertMarkers = invertMarkers,
      overwrite = overwrite, isDatatype = F)
  }

  def createResourceWithMarkersH(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 invertMarkers: B,
                                 overwrite: B,
                                 isDatatype: B): FileResource = {
    return createResourceWithMarkersI(
      path = path, content = content, markers = markers, invertMarkers = invertMarkers,
      overwrite = overwrite, isDatatype = isDatatype, skipConsistencyChecks = F)
  }

  def createResourceWithMarkersI(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 invertMarkers: B,
                                 overwrite: B,
                                 isDatatype: B,
                                 skipConsistencyChecks: B): FileResource = {
    return createResourceWithMarkersJ(
      path = path, content = content, markers = markers, invertMarkers = invertMarkers,
      overwrite = overwrite, makeExecutable = F, makeCRLF = F, isDatatype = isDatatype,
      skipConsistencyChecks = skipConsistencyChecks)
  }

  def createResourceWithMarkersJ(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 invertMarkers: B,
                                 overwrite: B,
                                 isDatatype: B,
                                 makeExecutable: B,
                                 makeCRLF: B,
                                 skipConsistencyChecks: B): FileResource = {
    return createResourceWithMarkersK(
      path = path,
      content = content,
      markers = markers,
      invertMarkers = invertMarkers,
      overwrite = overwrite,
      makeExecutable = makeExecutable,
      makeCRLF = makeCRLF,
      isDatatype = isDatatype,
      commentStyle = detectCommentStyle(path),
      skipConsistencyChecks = skipConsistencyChecks)
  }

  def createResourceWithMarkersK(path: String,
                                 content: ST,
                                 markers: ISZ[Marker],
                                 invertMarkers: B,
                                 overwrite: B,
                                 isDatatype: B,
                                 makeExecutable: B,
                                 makeCRLF: B,
                                 commentStyle: CommentStyle.Type,
                                 skipConsistencyChecks: B): FileResource = {
    assert(skipConsistencyChecks || checkConsistency(path, content, overwrite, invertMarkers, markers))

    return IResource(
      dstPath = path,
      content = content,
      markers = markers,
      invertMarkers = invertMarkers,
      overwrite = overwrite,
      makeExecutable = makeExecutable,
      makeCRLF = makeCRLF,
      isDatatype = isDatatype,
      commentStyle = commentStyle)
  }

  @pure def detectCommentStyle(path: String): CommentStyle.Type = {
    val s = ops.StringOps(path)

    if (s.endsWith("c") ||
      s.endsWith("h") ||
      s.endsWith("cpp") ||
      s.endsWith("hpp") ||
      s.endsWith("camkes") ||
      s.endsWith("dot") ||
      s.endsWith("rs") ||
      s.endsWith("cmd") ||
      s.endsWith(".idl4") ||
      s.endsWith("sbt") ||
      s.endsWith("scala") ||
      s.endsWith("slang")) {
      return CommentStyle.Slash
    }

    if (s.endsWith("xml") ||
      s.endsWith("system") ||
      s.endsWith("md")) {
      return CommentStyle.Xml
    }

    if (s.endsWith("cmake") ||
      s.endsWith("Makefile") ||
      s.endsWith("CMakeLists.txt") ||
      s.endsWith("mk") ||
      s.endsWith("properties") ||
      s.endsWith(".S") ||
      s.endsWith("msg") ||
      s.endsWith("cross_vm_module_init") ||
      s.endsWith("hvc0") ||
      s.endsWith("py") ||
      s.endsWith("toml")) {
      return CommentStyle.Hash
    }

    if (s.endsWith("dts")) {
      return CommentStyle.Block
    }

    if (s.endsWith("smt2")) {
      return CommentStyle.Smt2
    }

    halt("path")
  }

  def createExeResource(path: String,
                        content: ST,
                        overwrite: B): FileResource = {
    return createResourceWithMarkersJ(
      path = path, content = content, markers = ISZ(), invertMarkers = F, overwrite = overwrite,
      isDatatype = F, makeExecutable = T, makeCRLF = F, skipConsistencyChecks = F)
  }

  def createExeCrlfResource(path: String,
                            content: ST,
                            overwrite: B): FileResource = {
    return createResourceWithMarkersJ(
      path = path, content = content, markers = ISZ(), invertMarkers = F, overwrite = overwrite,
      isDatatype = F, makeExecutable = T, makeCRLF = T, skipConsistencyChecks = F)
  }

  def createStringResource(path: String,
                           content: String,
                           overwrite: B): FileResource = {
    return createStringResourceH(path = path, content = content, overwrite = overwrite, isDatatype = F)
  }

  def createStringResourceH(path: String,
                            content: String,
                            overwrite: B,
                            isDatatype: B): FileResource = {
    return createStringResourceI(
      path = path, content = content, overwrite = overwrite, isDatatype = isDatatype, skipConsistencyChecks = F)
  }

  def createStringResourceI(path: String,
                            content: String,
                            overwrite: B,
                            isDatatype: B,
                            skipConsistencyChecks: B): FileResource = {
    return createResourceI(
      path = path, content = st"${content}", overwrite = overwrite, isDatatype = isDatatype,
      skipConsistencyChecks = skipConsistencyChecks)
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
