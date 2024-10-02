// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.plugin.PlatformProviderPlugin
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.templates.CommentTemplate

object PlatformTemplate {
  val setupMarker: Marker = Marker.createMarker("PLATFORM SETUP")
  val teardownMarker: Marker = Marker.createMarker("PLATFORM TEARDOWN")

  val markers: ISZ[Marker] = ISZ(setupMarker, teardownMarker)

  def createPlatform(basePackage: String, plaformSetupEntries: ISZ[(String, PlatformProviderPlugin.PlatformContributions)]): ST = {
    var imports = ISZ[ST]()
    var setupBlocks = ISZ[ST]()
    var tearDownBlocks = ISZ[ST]()
    for (pc <- plaformSetupEntries) {
      imports = imports ++ pc._2.imports
      def blockIt(pluginName: String, a: ISZ[ST]): ST = {
        return(
        st"""{
            |  // Contributions from $pluginName
            |  ${(a, "\n\n")}
            |}""")
      }
      pc match {
        case (pluginName, PlatformProviderPlugin.PlatformSetupContributions(_, a, _)) => setupBlocks = setupBlocks :+ blockIt(pluginName, a)
        case (pluginName, PlatformProviderPlugin.PlatformTearDownContributions(_, a, _)) => tearDownBlocks = tearDownBlocks :+ blockIt(pluginName, a)
      }
    }
    val importOpt: Option[ST] =
      if (imports.nonEmpty) Some(st"${((for (i <- imports) yield st"import $i"), "\n")}")
      else None()
    val setupOpt: Option[ST] =
      if (setupBlocks.nonEmpty) Some(st"${(setupBlocks, "\n\n")}")
      else None()
    val tearDownOpt: Option[ST] =
      if (tearDownBlocks.nonEmpty) Some(st"${(tearDownBlocks, "\n\n")}")
      else None()

    val ret =
      st"""// #Sireum
          |
          |package ${basePackage}
          |
          |import org.sireum._
          |$importOpt
          |
          |${CommentTemplate.safeToEditComment_scala}
          |
          |object Platform {
          |
          |  def setup(): Unit = {
          |    ${setupMarker.beginMarker}
          |    $setupOpt
          |    ${setupMarker.endMarker}
          |  }
          |
          |  def tearDown(): Unit = {
          |    ${teardownMarker.beginMarker}
          |    $tearDownOpt
          |    ${teardownMarker.endMarker}
          |  }
          |}"""
    return ret
  }
}
