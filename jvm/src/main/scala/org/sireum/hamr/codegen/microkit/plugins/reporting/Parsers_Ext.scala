package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.microkit.plugins.reporting.CContainers.CFile
import org.sireum.hamr.codegen.microkit.plugins.reporting.MSDContainers.system
import org.sireum.hamr.codegen.microkit.plugins.reporting.RustContainers.RustFile
import org.sireum.message.{Level, Reporter}

object Parsers_Ext {

  // The reporting parsers are hand-written and only approximate C, Rust, and the MSD
  // format, so a parse failure must not fail codegen. Each parser runs against a local
  // reporter; its errors (and any exception it throws) are passed on to 'reporter' as
  // warnings and None is returned so the caller can suspend report generation.
  def tryParse[T](f: Os.Path, reporter: Reporter, parser: Reporter => Option[T]): Option[T] = {
    val localReporter = Reporter.create
    System.setProperty("org.sireum.silenthalt", "true")
    val r: Option[T] =
      try {
        parser(localReporter)
      } catch {
        case scala.util.control.NonFatal(e) =>
          if (!localReporter.hasError) {
            localReporter.error(None(), "Parsers", s"Exception while parsing $f: $e")
          }
          None()
      } finally {
        System.setProperty("org.sireum.silenthalt", "false")
      }
    reporter.reports(for (m <- localReporter.messages) yield
      if (m.level == Level.Error) m(level = Level.Warning) else m)
    return if (localReporter.hasError) None() else r
  }

  def parseC(f: Os.Path, rootDir: Os.Path, reporter: Reporter): Option[CFile] = {
    return tryParse(f, reporter, (r: Reporter) => Some(CParser.parse(f, rootDir, r)))
  }

  def parseRust(f: Os.Path, rootDir: Os.Path, userModifable: B, reporter: Reporter): Option[RustFile] = {
    return tryParse(f, reporter, (r: Reporter) => Some(RustParser.parse(f, rootDir, userModifable, r)))
  }

  def parseMSD(xml: Os.Path, rootDir: Os.Path, reporter: Reporter): Option[system] = {
    return tryParse(xml, reporter, (r: Reporter) => MSDParser.parse(xml, rootDir, r))
  }

}
