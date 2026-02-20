// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.CommonUtil.ISZValue
import org.sireum.hamr.codegen.microkit.util._

object StoreUtil {

  val KEY_Channels: String = "KEY_Channels"

  @strictpure def getChannels(store: Store): ISZ[Channel] =
    store.getOrElse(KEY_Channels, ISZValue[Channel](ISZ())).asInstanceOf[ISZValue[Channel]].elements

  @strictpure def addChannels(s: ISZ[Channel], store: Store): Store =
    store + KEY_Channels ~> ISZValue(getChannels(store) ++ s)



  val KEY_MemoryRegions: String = "KEY_MemoryRegions"

  @strictpure def getMemoryRegions(store: Store): ISZ[MemoryRegion] =
    store.getOrElse(KEY_MemoryRegions, ISZValue[MemoryRegion](ISZ())).asInstanceOf[ISZValue[MemoryRegion]].elements

  @strictpure def addMemoryRegions(s: ISZ[MemoryRegion], store: Store): Store =
    store + KEY_MemoryRegions ~> ISZValue(getMemoryRegions(store) ++ s)



  val KEY_ProtectionDomains: String = "KEY_ProtectionDomains"

  @strictpure def getProtectionDomains(store: Store): ISZ[ProtectionDomain] =
    store.getOrElse(KEY_ProtectionDomains, ISZValue[ProtectionDomain](ISZ())).asInstanceOf[ISZValue[ProtectionDomain]].elements

  @strictpure def addProtectionDomains(s: ISZ[ProtectionDomain], store: Store): Store =
    store + KEY_ProtectionDomains ~> ISZValue(getProtectionDomains(store) ++ s)



  val KEY_SchedulingDomains: String = "KEY_SchedulingDomains"

  @strictpure def getSchedulingDomains(store: Store): ISZ[SchedulingDomain] =
    store.getOrElse(KEY_SchedulingDomains, ISZValue[SchedulingDomain](ISZ())).asInstanceOf[ISZValue[SchedulingDomain]].elements

  @strictpure def addSchedulingDomains(s: ISZ[SchedulingDomain], store: Store): Store =
    store + KEY_SchedulingDomains ~> ISZValue(getSchedulingDomains(store) ++ s)



  val KEY_MakefileContainers: String = "KEY_MakefileContainers"

  @strictpure def getMakefileContainers(store: Store): ISZ[MakefileContainer] =
    store.getOrElse(KEY_MakefileContainers, ISZValue[MakefileContainer](ISZ())).asInstanceOf[ISZValue[MakefileContainer]].elements

  @strictpure def addMakefileContainers(s: ISZ[MakefileContainer], store: Store): Store =
    store + KEY_MakefileContainers ~> ISZValue(getMakefileContainers(store) ++ s)
}
