// #Sireum

package org.sireum.hamr.codegen

import org.sireum._

@enum object CodeGenPlatform {
  'JVM
  'Linux
  'Cygwin
  'MacOS
  'SeL4
  'SeL4_Only
  'SeL4_TB
}

@enum object CodeGenIpcMechanism {
  'SharedMemory
}

@datatype class CodeGenConfig(verbose: B,
                              writeOutResources: B,
                             
                              platform: CodeGenPlatform.Type,

                              // arsit options
                              slangOutputDir: Option[String],
                              packageName: Option[String],
                              embedArt: B,
                              devicesAsThreads: B,
                              ipc: CodeGenIpcMechanism.Type,
                              slangAuxCodeDirs: ISZ[String],
                              slangOutputCDir: Option[String],
                              excludeComponentImpl: B,
                              bitWidth: Z,
                              maxStringSize: Z,
                              maxArraySize: Z,
                              
                              // act options
                              camkesOutputDir: Option[String],
                              camkesAuxCodeDirs: ISZ[String],
                              aadlRootDir: Option[String])
                                   

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

@datatype class CodeGenResults(resources: ISZ[Resource],
                               transpilerConfig: Option[TranspilerConfig])
