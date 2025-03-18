// #Sireum

package org.sireum.hamr.codegen.common.containers

import org.sireum._

object Resource {
  @pure def projectTranspilerConfigs(auxResources: ISZ[Resource]): ISZ[SireumSlangTranspilersCOption] = {
    var ret: ISZ[SireumSlangTranspilersCOption] = ISZ()
    for (r <- auxResources) {
      r match {
        case t: SireumSlangTranspilersCOption => ret = ret :+ t
        case _ =>
      }
    }
    return ret
  }

  @pure def projectSergenConfigs(auxResources: ISZ[Resource]): ISZ[SireumToolsSergenOption] = {
    var ret: ISZ[SireumToolsSergenOption] = ISZ()
    for (r <- auxResources) {
      r match {
        case t: SireumToolsSergenOption => ret = ret :+ t
        case _ =>
      }
    }
    return ret
  }

  @pure def projectSlangCheckConfigs(auxResources: ISZ[Resource]): ISZ[SireumToolsSlangcheckGeneratorOption] = {
    var ret: ISZ[SireumToolsSlangcheckGeneratorOption] = ISZ()
    for (r <- auxResources) {
      r match {
        case t: SireumToolsSlangcheckGeneratorOption => ret = ret :+ t
        case _ =>
      }
    }
    return ret
  }
}

@sig trait Resource

@sig trait FileResource extends Resource {
  def dstPath: String

  @pure def name: String = {
    return ops.ISZOps(ops.StringOps(dstPath).split(c => c == '/')).last
  }
}

object Marker {
  val beginMarkerPrefix: String = "// BEGIN MARKER"
  val endMarkerPrefix: String = "// END MARKER"

  @strictpure def createMarker(id: String): Marker =
    Marker(s"$beginMarkerPrefix $id", s"$endMarkerPrefix $id")
}

@datatype class Marker(val beginMarker: String,
                       val endMarker: String)

@sig trait InternalResource extends FileResource {
  def content: ST
  def markers: ISZ[Marker]
  def overwrite: B
  def makeExecutable: B
  def makeCRLF: B
  def isDatatype: B
}

// Internal Resource
@datatype class IResource(val dstPath: String,
                          val content: ST,
                          val markers: ISZ[Marker],
                          val overwrite: B,
                          val makeExecutable: B,
                          val makeCRLF: B,

                          // isDataype indicates whether resource should be added to sergen/slangcheck
                          val isDatatype: B
                         ) extends InternalResource

@sig trait ExternalResource extends FileResource {
  def srcPath: String
  def symLink: B
}

// External Resource
@datatype class EResource(val srcPath: String,
                          val dstPath: String,
                          val symLink: B) extends ExternalResource

@sig trait ConfigResource extends Resource

@datatype class SireumSlangTranspilersCOption(val help: String,
                                              val args: ISZ[String],
                                              val sourcepath: ISZ[String],
                                              val output: Option[String],
                                              val verbose: B,
                                              val apps: ISZ[String],
                                              val bitWidth: Z,
                                              val projectName: Option[String],
                                              val stackSize: Option[String],
                                              val customArraySizes: ISZ[String],
                                              val maxArraySize: Z,
                                              val maxStringSize: Z,
                                              val cmakeIncludes: ISZ[String],
                                              val exts: ISZ[String],
                                              val libOnly: B,
                                              val excludeBuild: ISZ[String],
                                              val plugins: ISZ[String],
                                              val fingerprint: Z,
                                              val stableTypeId: B,
                                              val unroll: B,
                                              val save: Option[String],
                                              val load: Option[String],
                                              val customConstants: ISZ[String],
                                              val forwarding: ISZ[String]) extends ConfigResource

@enum object SireumProyekIveEdition {
  "Community"
  "Ultimate"
  "Server"
}

@datatype class SireumProyekIveOption(val help: String,
                                      val args: ISZ[String],
                                      val force: B,
                                      val edition: SireumProyekIveEdition.Type,
                                      val javac: ISZ[String],
                                      val scalac: ISZ[String],
                                      val ignoreRuntime: B,
                                      val json: Option[String],
                                      val name: Option[String],
                                      val outputDirName: Option[String],
                                      val project: Option[String],
                                      val slice: ISZ[String],
                                      val symlink: B,
                                      val versions: ISZ[String],
                                      val cache: Option[String],
                                      val docs: B,
                                      val sources: B,
                                      val repositories: ISZ[String]) extends ConfigResource

@enum object SireumToolsSergenSerializerMode {
  'Json
  'Msgpack
}

@datatype class SireumToolsSergenOption(val help: String,
                                        val args: ISZ[String],
                                        val modes: ISZ[SireumToolsSergenSerializerMode.Type],
                                        val packageName: ISZ[String],
                                        val name: Option[String],
                                        val license: Option[String],
                                        val outputDir: Option[String]) extends ConfigResource

@datatype class SireumToolsSlangcheckGeneratorOption(val help: String,
                                                     val args: ISZ[String],
                                                     val license: Option[String],
                                                     val packageName: ISZ[String],
                                                     val outputDir: Option[String],
                                                     val testDir: Option[String]) extends ConfigResource
