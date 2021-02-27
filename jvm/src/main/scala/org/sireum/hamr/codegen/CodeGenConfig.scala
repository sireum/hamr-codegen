// #Sireum

package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}

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

@datatype class CodeGenConfig(writeOutResources: B,
                              ipc: CodeGenIpcMechanism.Type,

                              // the following is effectively a copy of the globals in
                              // org.sireum.Cli.HamrCodeGenOption

                              verbose: B,
                              platform: CodeGenPlatform.Type,

                              // arsit options
                              slangOutputDir: Option[String],
                              packageName: Option[String],
                              noEmbedArt: B,
                              devicesAsThreads: B,

                              //
                              slangAuxCodeDirs: ISZ[String],
                              slangOutputCDir: Option[String],
                              excludeComponentImpl: B,
                              bitWidth: Z,
                              maxStringSize: Z,
                              maxArraySize: Z,
                              runTranspiler: B,

                              // act options
                              camkesOutputDir: Option[String],
                              camkesAuxCodeDirs: ISZ[String],
                              aadlRootDir: Option[String],

                              experimentalOptions: ISZ[String])


@datatype class CodeGenResults(resources: ISZ[Resource],
                               transpilerConfigs: ISZ[TranspilerConfig])
