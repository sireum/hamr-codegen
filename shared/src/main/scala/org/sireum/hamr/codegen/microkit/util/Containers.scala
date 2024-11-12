// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._

@sig trait SharedMemoryRegion {
  def regionName: String
  def dataSizeInKiBytes: Z
}

@datatype class PortSharedMemoryRegion(val outgoingPortPath: ISZ[String],
                                       val queueSize: Z,
                                       val varAddr: String,
                                       val perms: ISZ[Perm.Type],
                                       val dataSizeInKiBytes: Z) extends SharedMemoryRegion{

  def regionName: String = {
    return st"${(outgoingPortPath, "_")}_${queueSize}_Memory_Region".render
  }
}
