// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, FileResource, IResource, Marker, Resource}

object TestUtil {

  def writeExpected(normalizedResultMap: TestResult, expected: Os.Path): Unit = {
    expected.writeOver(TestJSON.fromTestResult(normalizedResultMap, F))
  }

  def readExpected(expected: Os.Path): TestResult = {
    return TestJSON.toTestResult(expected.read).left
  }

  def convertToTestResult(resources: ISZ[Resource], resultsDir: Os.Path): TestResult = {
    def normalize(t: TestResult): TestResult = {
      val nmap: ISZ[(String, TestResource)] = t.map.entries.map(m => {

        val dstPath: String =
          if (Os.isWin) {
            ops.StringOps(m._1).replaceAllChars('\\', '/')
          }
          else {
            m._1
          }

        m._2 match {
          case i: ITestResource =>
            val content: String = {
              val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
              val replace: String = if (i.makeCRLF) "\r\n" else "\n"
              ops.StringOps(i.content).replaceAllLiterally(lineSep, replace)
            }
            (dstPath, ITestResource(content = content, overwrite = i.overwrite, makeExecutable = i.makeExecutable, makeCRLF = i.makeCRLF, markers = i.markers, isDatatype = i.isDatatype))

          case e: ETestResource =>
            (dstPath, ETestResource(content = e.content, symlink = e.symlink))
        }
      })
      return TestResult(Map(nmap))
    }

    var map = Map.empty[String, TestResource]
    for (r <- resources) {
      r match {
        case r: FileResource =>
          val key = resultsDir.relativize(Os.path(r.dstPath)).value
          r match {
            case i: IResource =>
              val testMarkers = i.markers.map((m: Marker) => TestMarker(beginMarker = m.beginMarker, endMarker = m.endMarker))
              map = map + (key, ITestResource(content = i.content.render, overwrite = i.overwrite, makeExecutable = i.makeExecutable, makeCRLF = i.makeCRLF, markers = testMarkers, isDatatype = i.isDatatype))
            case e: EResource =>
              val src = resultsDir.relativize(Os.path(e.srcPath)).value
              val dst = resultsDir.relativize(Os.path(e.dstPath)).value
              map = map + (key, ETestResource(src, e.symlink))
          }
        case _ =>
      }
    }
    return normalize(TestResult(map))
  }
}
