// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.microkit.util.SystemDescription

object SystemDescriptionProviderPlugin {

  type MSD = MapValue[String, SystemDescription]

  val SystemDescriptionProviderPlugin_KEY: String = "SystemDescriptionProviderPlugin_KEY"

  val systemDescriptionProviderPlugins: ISZ[Plugin] = ISZ(
    SystemDescriptionProvider_DomainScheduler(),
    SystemDescriptionProvider_MCS()
  )

  @strictpure def getMSDs(store: Store): Map[String, SystemDescription] = store.getOrElse(SystemDescriptionProviderPlugin_KEY, MapValue[String, SystemDescription](Map.empty)).asInstanceOf[MSD].map

  @strictpure def getMSD(name: String, store: Store): SystemDescription = getMSDOpt(name, store).get

  @strictpure def getMSDOpt(name: String, store: Store): Option[SystemDescription] =
    store.get(SystemDescriptionProviderPlugin_KEY) match {
      case Some(msd) => msd.asInstanceOf[MSD].map.get(name)
      case _ => None()
    }

  @strictpure def putMSD(name: String, sd: SystemDescription, store: Store): Store = {
    val map = getMSDs(store)
    store + SystemDescriptionProviderPlugin_KEY ~> (MapValue(map + name ~> sd))
  }
}


