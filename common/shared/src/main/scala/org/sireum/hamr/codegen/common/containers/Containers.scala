// #Sireum

package org.sireum.hamr.codegen.common.containers

import org.sireum._

@sig trait Resource {
  def dstPath: String
}

@datatype class Marker(val beginMarker: String,
                       val endMarker: String)

// Internal Resource
@datatype class IResource(val dstPath: String,
                          val content: ST,
                          val markers: ISZ[Marker],
                          val overwrite: B,
                          val makeExecutable: B,
                          val makeCRLF: B,

                          // isDataype indicates whether resource should be added to sergen/slangcheck
                          val isDatatype: B
                         ) extends Resource

// External Resource
@datatype class EResource(val srcPath: String,
                          val dstPath: String,
                          val symlink: B) extends Resource

@datatype class TranspilerConfig(val help: String,
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
                                 val forwarding: ISZ[String])

@enum object ProyekIveEdition {
  "Community"
  "Ultimate"
  "Server"
}

@datatype class ProyekIveConfig(val help: String,
                                val args: ISZ[String],
                                val force: B,
                                val edition: ProyekIveEdition.Type,
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
                                val repositories: ISZ[String])


@datatype class SireumToolsSlangcheckOption(val datatypeFiles: ISZ[Resource],
                                            val outputDir: ISZ[String],
                                            val testDir: ISZ[String])
