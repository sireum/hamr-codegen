package org.sireum.hamr.arsit.util

import org.sireum.$internal.RC
import org.sireum._

import java.io.StringReader

object ArsitLibrary_Ext {

  def getFiles: ISZ[(String, String)] = {
    val map = RC.text(Vector(
      "../../../../../../../../../../art/shared/src/main/scala/",
      "../../../../../../../resources/util")) { (p, f) => true }
    ISZ(map.toSeq.map(p => (String(p._1.mkString("/")), String(p._2))): _*)
  }

  def trimCli(s: String): String = {
    val w = ops.StringOps(s)
    return w.substring(w.stringIndexOf("import org.sireum._"), s.size)
  }

  def getCompileCli: String = {
    val compileCli = getFiles.filter(p => Os.path(p._1).name.native == "cliCompile.cmd")(0)._2
    return trimCli(compileCli)
  }

  def getRunCli: String = {
    val runCli = getFiles.filter(p => Os.path(p._1).name.native == "cliRun.cmd")(0)._2
    return trimCli(runCli)
  }

  def getTranspileSeL4Cli: String = {
    val transpileCli = getFiles.filter(p => Os.path(p._1).name.native == "cliTranspile.cmd")(0)._2
    return trimCli(transpileCli)
  }

  def getTranspileSlashCli: String = {
    val transpileCli = getFiles.filter(p => Os.path(p._1).name.native == "cliTranspile-alt.cmd")(0)._2
    return trimCli(transpileCli)
  }

  def getBuildSbtProperties(): java.util.Properties = {
    val str = Map(getFiles).get("codegen_versions.properties").get
    val p = new java.util.Properties()
    p.load(new StringReader(str.native))
    return p
  }

  def getArtVersion(): String = {
    return getBuildSbtProperties().getProperty("art.version")
  }

  def getKekinianVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.kekinian.version")
  }

  def getSireumScalacVersionVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scalac-plugin")
  }

  def getScalaVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scala")
  }

  def getScalaTestVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scalatest")
  }

  def getSBTVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.sbt")
  }

  def getSbtAssemblyVersion(): String = {
    return getBuildSbtProperties().getProperty("sbtassembly.version")
  }

  def getFormsRtVersion(): String = {
    return getBuildSbtProperties().getProperty("com.intellij.forms_rt.version")
  }

  def getInspectorVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.inspector.version")
  }

} 