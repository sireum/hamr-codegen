package org.sireum.hamr.codegen

import org.sireum._

import java.nio.file.{AccessDeniedException, FileAlreadyExistsException, Files, NotDirectoryException, Paths}

object AirOutputExt_Ext {

  // java.nio rather than Os.Path: Os.Path.mkdirAll halts on an existing file in the way, and halt
  // prints a stack trace before the error codegen reports
  def writeOver(path: String, content: String): Option[String] = {
    try {
      val p = Paths.get(path.value)
      val parent = p.getParent
      if (parent != null) {
        Files.createDirectories(parent)
      }
      Files.write(p, content.value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      return None()
    } catch {
      case e: FileAlreadyExistsException => return Some(String(s"${e.getFile} exists and is not a directory"))
      case e: NotDirectoryException => return Some(String(s"${e.getFile} is not a directory"))
      case e: AccessDeniedException => return Some(String(s"permission denied for ${e.getFile}"))
      case scala.util.control.NonFatal(e) =>
        val msg: Predef.String = if (e.getMessage == null) e.getClass.getName else s"${e.getClass.getSimpleName}: ${e.getMessage}"
        return Some(String(msg))
    }
  }
}
