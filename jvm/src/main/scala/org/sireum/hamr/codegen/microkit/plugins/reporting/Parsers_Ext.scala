package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.hamr.codegen.microkit.plugins.reporting.CContainers.CFile
import org.sireum.hamr.codegen.microkit.plugins.reporting.MSDContainers.system
import org.sireum.hamr.codegen.microkit.plugins.reporting.RustContainers.RustFile
import org.sireum.message.Reporter

object Parsers_Ext {
  @pure def parseC(f: Os.Path, rootDir: Os.Path, reporter: Reporter): CFile = {
    System.setProperty("org.sireum.silenthalt", "true")
    try {
      return CParser.parse(f, rootDir, reporter)
    } catch {
      case e: Error => return CFile(ISZ())
    } finally {
      System.setProperty("org.sireum.silenthalt", "false")
    }
  }

  @pure def parseRust(f: Os.Path, rootDir: Os.Path, userModifable: B, reporter: Reporter): RustFile = {
    System.setProperty("org.sireum.silenthalt", "true")
    try {
      return RustParser.parse(f, rootDir, userModifable, reporter)
    } catch {
      case e: Error => return RustFile(ISZ())
    } finally {
      System.setProperty("org.sireum.silenthalt", "false")
    }
  }

  @pure def parseMSD(xml: Os.Path, rootDir: Os.Path, reporter: Reporter): Option[system] = {
    System.setProperty("org.sireum.silenthalt", "true")
    try {
      return MSDParser.parse(xml, rootDir, reporter)
    } catch {
      case e: Error => return None()
    } finally {
      System.setProperty("org.sireum.silenthalt", "false")
    }
  }

}
