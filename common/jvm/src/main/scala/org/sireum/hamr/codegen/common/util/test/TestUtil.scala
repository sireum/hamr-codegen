// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource

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
        val path: String =
          if(Os.isWin) { ops.StringOps(m._1).replaceAllChars('\\', '/') }
          else { m._1 }

        val contents: String = {
          val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
          val replace: String = if (m._2.makeCRLF) "\r\n" else "\n"
          ops.StringOps(m._2.content).replaceAllLiterally(lineSep, replace)
        }
        (path, TestResource(content = contents, overwrite = m._2.overwrite, makeExecutable = m._2.makeExecutable, makeCRLF = m._2.makeCRLF))
      })
      return TestResult(Map(nmap))
    }

    return normalize(TestResult(Map.empty[String, TestResource] ++ (resources.map((m: Resource) => {
      val key = resultsDir.relativize(Os.path(m.path)).value
      (key, TestResource(m.content.render, m.overwrite, m.makeExecutable, m.makeCRLF))
    }))))
  }
}
