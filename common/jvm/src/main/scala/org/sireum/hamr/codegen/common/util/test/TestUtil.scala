// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

object TestUtil {

  def writeExpected(resultMap: TestResult, expected: Os.Path): Unit = {
    expected.writeOver(TestJSON.fromTestResult(normalize(resultMap, T), F))
  }

  def readExpected(expected: Os.Path): TestResult = {
    return normalize(TestJSON.toTestResult(expected.read).left, F)
  }

  def normalize(t: TestResult, writing: B): TestResult = {
    if(Os.isLinux || Os.isMac) {
      return t
    } else {
      val winSep: String = "\\"
      assert(Os.isWin && Os.fileSep == winSep)
      val (fromSep, toSep): (C, C) = {
        if(writing) ('\\', '/')
        else ('/', '\\')
      }
      val (fromLinefeed, toLinefeed): (String, String) = {
        if(writing) ("\r\n", "\n")
        else ("\n", "\r\n")
      }
      val nmap: ISZ[(String, TestResource)] = t.map.entries.map(m => {
        val path = ops.StringOps(m._1).replaceAllChars(fromSep, toSep)
        val contents = ops.StringOps(m._2.content).replaceAllLiterally(fromLinefeed, toLinefeed)
        (path, TestResource(content = contents, overwrite = m._2.overwrite, makeExecutable = m._2.makeExecutable))
      })
      return TestResult(Map(nmap))
    }
  }
}
