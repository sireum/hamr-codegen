// #Sireum

package org.sireum.hamr.arsit.util

import org.sireum._

@enum object IpcMechanism {
  'SharedMemory
}

@enum object ArsitPlatform {
  'JVM
  'Linux
  'Cygwin
  'MacOS
  'SeL4
}

@datatype class ArsitOptions(slangOutputDir: Os.Path,
                             packageName: String,
                             noEmbedArt: B,
                             bless: B,
                             verbose: B,
                             runtimeMonitoring: B,
                             devicesAsThreads: B,
                             genSbtMill: B,
                             ipc: IpcMechanism.Type,
                             auxCodeDirs: ISZ[String],
                             outputSharedCDir: Os.Path,
                             outputPlatformCDir: Os.Path,
                             excludeImpl: B,
                             platform: ArsitPlatform.Type,
                             bitWidth: Z,
                             maxStringSize: Z,
                             maxArraySize: Z,
                             pathSeparator: C,

                             runSlangCheck: B, // TODO: remove once slang check is part of kekinian

                             experimentalOptions: ISZ[String])
