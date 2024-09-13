// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{FileResource, Resource, SireumSlangTranspilersCOption}

@enum object CodeGenPlatform {
  'JVM
  'Linux
  'Cygwin
  'MacOS
  'SeL4
  'SeL4_Only
  'SeL4_TB
  'Ros2
}

@enum object CodeGenIpcMechanism {
  'SharedMemory
}

@enum object CodegenNodesCodeLanguage {
  'Python
  'Cpp
}

@enum object CodegenLaunchCodeLanguage {
  'Python
  'Xml
}

@datatype class CodeGenConfig(writeOutResources: B,
                              ipc: CodeGenIpcMechanism.Type,

                              // the following is effectively a copy of the globals in
                              // org.sireum.Cli.SireumHamrCodeGenOption

                              verbose: B,
                              runtimeMonitoring: B,
                              platform: CodeGenPlatform.Type,

                              // arsit options
                              slangOutputDir: Option[String],
                              packageName: Option[String],
                              noProyekIve: B,
                              noEmbedArt: B,
                              devicesAsThreads: B,
                              genSbtMill: B,

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
                              workspaceRootDir: Option[String],

                              // ros options
                              strictAadlMode: B,
                              ros2OutputWorkspaceDir: Option[String],
                              ros2Dir: Option[String],
                              ros2NodesLanguage: CodegenNodesCodeLanguage.Type,
                              ros2LaunchLanguage: CodegenLaunchCodeLanguage.Type,

                              //
                              experimentalOptions: ISZ[String])


@datatype class CodeGenResults(resources: ISZ[FileResource],
                               auxResources: ISZ[Resource])
