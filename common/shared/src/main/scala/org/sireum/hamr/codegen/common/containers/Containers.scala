// #Sireum

package org.sireum.hamr.codegen.common.containers

import org.sireum._

@datatype class Resource(path: String,
                         content: ST,
                         overwrite: B,
                         makeExecutable: B)

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
