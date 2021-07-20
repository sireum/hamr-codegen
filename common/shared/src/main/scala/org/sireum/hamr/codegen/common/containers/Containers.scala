// #Sireum

package org.sireum.hamr.codegen.common.containers

import org.sireum._

@datatype class Resource(path: String,
                         content: ST,
                         overwrite: B,
                         makeExecutable: B,
                         makeCRLF: B)

@datatype class TranspilerConfig(help: String,
                                 args: ISZ[String],
                                 sourcepath: ISZ[String],
                                 output: Option[String],
                                 verbose: B,
                                 apps: ISZ[String],
                                 bitWidth: Z,
                                 projectName: Option[String],
                                 stackSize: Option[String],
                                 customArraySizes: ISZ[String],
                                 maxArraySize: Z,
                                 maxStringSize: Z,
                                 cmakeIncludes: ISZ[String],
                                 exts: ISZ[String],
                                 libOnly: B,
                                 excludeBuild: ISZ[String],
                                 plugins: ISZ[String],
                                 fingerprint: Z,
                                 stableTypeId: B,
                                 unroll: B,
                                 save: Option[String],
                                 load: Option[String],
                                 customConstants: ISZ[String],
                                 forwarding: ISZ[String])

@datatype class ProyekIveConfig(val help: String,
                                val args: ISZ[String],
                                val force: B,
                                val ultimate: B,
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