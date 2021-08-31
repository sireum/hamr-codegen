// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, Resource}

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
          if(Os.isWin) { ops.StringOps(m._1).replaceAllChars('\\', '/') }
          else { m._1 }

        m._2 match {
          case i: ITestResource =>
            val contents: String = {
              val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
              val replace: String = if (i.makeCRLF) "\r\n" else "\n"
              ops.StringOps(i.content).replaceAllLiterally(lineSep, replace)
            }
            (dstPath, ITestResource(content = i.content, overwrite = i.overwrite, makeExecutable = i.makeExecutable, makeCRLF = i.makeCRLF))

          case e: ETestResource =>
            //val srcPath: String =
            //  if(Os.isWin) { ops.StringOps(e.srcPath).replaceAllChars('\\', '/') }
            //  else { e.srcPath }
            (dstPath, ETestResource(content = e.content, e.symlink))
          }
      })
      return TestResult(Map(nmap))
    }

    return normalize(TestResult(Map.empty[String, TestResource] ++ (resources.map((m: Resource) => {
      val key = resultsDir.relativize(Os.path(m.dstPath)).value
      m match {
        case i: IResource =>
          (key, ITestResource (i.content.render, i.overwrite, i.makeExecutable, i.makeCRLF) )
        case e: EResource =>
          val src = resultsDir.relativize(Os.path(e.srcPath)).value
          val dst = resultsDir.relativize(Os.path(e.dstPath)).value
          (key, ETestResource (src, e.symlink))
      }
    }))))
  }
}
