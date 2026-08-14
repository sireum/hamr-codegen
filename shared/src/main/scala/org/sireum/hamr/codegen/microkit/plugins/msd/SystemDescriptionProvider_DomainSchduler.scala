// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.MicrokitFinalizePlugin
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, SystemDescription}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@datatype class SystemDescriptionProvider_DomainScheduler extends MicrokitFinalizePlugin {

  val name: String = "SystemDescriptionProvider_DomainScheduler"

  @strictpure def hasHandled(store: Store): B = store.contains(name)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return !reporter.hasError &&
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !MicrokitUtil.isMCS(options, symbolTable.rootSystem) &&
      SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
      !hasHandled(store)
  }

  /** Checks the MSD's domain schedule against the constraints Microkit 2.3.0 enforces.
    * Violations would otherwise surface as errors from the microkit tool at build time,
    * well after codegen has finished.
    */
  def checkDomainSchedule(msd: SystemDescription, reporter: Reporter): Unit = {
    if (msd.schedulingDomains.isEmpty) {
      return
    }

    // NOTE: the KernelNumDomains bound on domain ids is not checked here. Both it and
    // KernelNumDomainSchedules are properties of the kernel the SDK was built with, and
    // codegen is given neither the SDK path nor the target board, so the values below are
    // only the stock Microkit defaults -- an SDK rebuilt with larger values would make an
    // error here a false rejection. The microkit tool checks both against the actual
    // kernel config at build time; the linter additionally warns on model-declared domain
    // ids that exceed the default (see MicrokitLinterPlugin).
    //
    // The schedule length is warned about rather than rejected for the same reason. The
    // explicit <schedule_end_marker /> occupies an entry, and the microkit tool requires
    // the total number of entries to be strictly less than KernelNumDomainSchedules.
    val entryCount = msd.schedulingDomains.size + 1
    if (entryCount >= MicrokitUtil.KernelNumDomainSchedules) {
      reporter.warn(None(), name,
        st"""The domain schedule of '${msd.systemName}' has $entryCount entries, which is not less than the default KernelNumDomainSchedules of ${MicrokitUtil.KernelNumDomainSchedules}.
            |Building this system requires an SDK whose kernel was built with a larger KernelNumDomainSchedules.""".render)
    }

    for (sd <- msd.schedulingDomains if sd.length <= 0) {
      reporter.error(None(), name,
        s"Scheduling domain ${sd.id} (${sd.componentName}) has a duration of ${sd.length}ms; Microkit requires schedule entry durations to be non-zero")
    }

    val declaredIds = msd.distinctSchedulingDomainIds
    for (pd <- msd.allProtectionDomains) {
      pd.schedulingDomain match {
        case Some(id) =>
          if (!ops.ISZOps(declaredIds).contains(id)) {
            reporter.error(None(), name,
              s"Protection domain '${pd.name}' is assigned to scheduling domain $id, which has no entry in the domain schedule")
          }
        case _ =>
          reporter.error(None(), name,
            s"Protection domain '${pd.name}' does not have a scheduling domain; Microkit requires a 'domain' attribute on every protection domain when a domain schedule is present")
      }
    }
  }

  override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + name ~> BoolValue(T)
    var resources = ISZ[Resource]()

    val msds: Map[String, SystemDescription] = SystemDescriptionProviderPlugin.getMSDs(localStore)
    for (msd <- msds.values) {
      checkDomainSchedule(msd, reporter)

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
