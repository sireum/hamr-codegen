// #Sireum

package org.sireum.hamr.arsit.util

import org.sireum._
import org.sireum.message.Reporter

object ReporterUtil {

  val reporter: Reporter = Reporter.create

  // singleton so must reset before each unique invocation of Arsit from same JVM
  def resetReporter(): Unit = {
    reporter.setMessages(ISZ())
  }

  def addReports(dst: Reporter): Unit = {
    dst.reports(reporter.messages)
  }

}
