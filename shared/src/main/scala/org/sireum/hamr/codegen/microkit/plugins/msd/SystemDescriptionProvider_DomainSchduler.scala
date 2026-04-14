// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.MicrokitFinalizePlugin
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.util.SystemDescription
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@datatype class SystemDescriptionProvider_DomainScheduler extends MicrokitFinalizePlugin {

  val name: String = "SystemDescriptionProvider_DomainScheduler"

  @strictpure def hasHandled(store: Store): B = store.contains(name)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return !reporter.hasError &&
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.Domain_Scheduling &&
      SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
      !hasHandled(store)
  }

  override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + name ~> BoolValue(T)
    var resources = ISZ[Resource]()

    val msds: Map[String, SystemDescription] = SystemDescriptionProviderPlugin.getMSDs(localStore)
    for (msd <- msds.values) {

      val xmlProtectionDomains = msd.protectionDomains

      val markers: ISZ[Marker] = ((for (p <- xmlProtectionDomains) yield p.getMarkers)).flatMap((s: ISZ[Marker]) => s)

      val sdXmlPath = s"${options.sel4OutputDir.get}/${msd.systemName}"
      resources = resources :+ ResourceUtil.createResourceWithMarkers(
        path = sdXmlPath,
        content = msd.prettyST,
        markers = markers ++ msd.getMarkers,
        invertMarkers = T,
        overwrite = T)

      val sdScheduleXmlPath = s"${options.sel4OutputDir.get}/${msd.scheduleName}"
      resources = resources :+ ResourceUtil.createResource(sdScheduleXmlPath, msd.scheduleText, F)

      val sysDot = msd.toDot
      val dotPath = s"${options.sel4OutputDir.get}/${msd.dotName}"
      resources = resources :+ ResourceUtil.createResource(path = dotPath, content = sysDot, overwrite = T)
    }

    return (localStore, resources)
  }
}
