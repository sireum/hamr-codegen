// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.util.SystemDescription
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@datatype class SystemDescriptionProvider_DomainScheduler extends SystemDescriptionProviderPlugin {

  val name: String = "SystemDescriptionProvider_DomainScheduler"

  @strictpure override def hasHandled(store: Store): B = store.contains(name)

  override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.Domain_Scheduling
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + name ~> BoolValue(T)
    var resources = ISZ[Resource]()

    val xmlProtectionDomains = StoreUtil.getProtectionDomains(localStore)

    val markers: ISZ[Marker] = ((for (p <- xmlProtectionDomains) yield p.getMarkers)).flatMap((s: ISZ[Marker]) => s)

    val sd = SystemDescription(
      schedulingDomains = StoreUtil.getSchedulingDomains(localStore),// xmlScheds,
      protectionDomains = xmlProtectionDomains,
      memoryRegions = StoreUtil.getMemoryRegions(localStore),
      channels = StoreUtil.getChannels(localStore))

    val sdXmlPath = s"${options.sel4OutputDir.get}/${MicrokitCodegen.microkitSystemXmlFilename}"
    resources = resources :+ ResourceUtil.createResourceWithMarkers(
      path = sdXmlPath,
      content = sd.prettyST,
      markers = markers ++ sd.getMarkers,
      invertMarkers = T,
      overwrite = F)

    val sdScheduleXmlPath = s"${options.sel4OutputDir.get}/${MicrokitCodegen.microkitScheduleXmlFilename}"
    resources = resources :+ ResourceUtil.createResource(sdScheduleXmlPath, sd.scheduleText, F)

    val sysDot = sd.toDot
    val dotPath = s"${options.sel4OutputDir.get}/microkit.dot"
    resources = resources :+ ResourceUtil.createResource(path = dotPath, content = sysDot, overwrite = T)

    return (localStore, resources)
  }
}