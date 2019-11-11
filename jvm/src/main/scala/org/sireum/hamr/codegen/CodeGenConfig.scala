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
  'MessageQueue
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

@datatype class TranspilerConfig(sourcepath: ISZ[String],
                                 output: Option[String],
                                 verbose: B,
                                 projectName: Option[String],
                                 apps: ISZ[String],
                                 unroll: B,
                                 fingerprint: Z,
                                 bitWidth: Z,
                                 maxStringSize: Z,
                                 maxArraySize: Z,
                                 customArraySizes: ISZ[String],
                                 customConstants: ISZ[String],
                                 plugins: ISZ[String],
                                 exts: ISZ[String],
                                 forwarding: ISZ[String],
                                 stackSize: Option[String],
                                 excludeBuild: ISZ[String],
                                 libOnly: B,
                                 stableTypeId: B,
                                 save: Option[String],
                                 load: Option[String])

@datatype class CodeGenResults(resources: ISZ[Resource],
                               transpilerConfig: Option[TranspilerConfig])
