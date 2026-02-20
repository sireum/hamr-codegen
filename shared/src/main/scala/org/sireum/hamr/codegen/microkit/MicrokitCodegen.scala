// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitInitPlugin, MicrokitLintPlugin, MicrokitPlugin, PluginUtil, StoreUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object MicrokitCodegen {
  val toolName: String = "Mircokit Codegen"

  val microkitSystemXmlFilename: String = "microkit.system"
  val microkitScheduleXmlFilename: String = "microkit.schedule.xml"
  val systemMakeFilename: String = "system.mk"

  val dirComponents: String = "components"
  val dirInclude: String = "include"
  val dirSrc: String = "src"
}

@record class MicrokitCodegen {
  var resources: ISZ[Resource] = ISZ()

  def run(model: Aadl, options: CodegenOption, types: AadlTypes, symbolTable: SymbolTable, plugins: ISZ[Plugin], store: Store, reporter: Reporter): (CodeGenResults, Store) = {
    var localStore = store

    if (!reporter.hasError) { // linting phase (one pass)
      var validModel = T
      for (p <- plugins) {
        p match {
          case p: MicrokitLintPlugin =>
            val results = p.validModel(model, options, types, symbolTable, localStore, reporter)
            validModel = validModel && results._2
            localStore = results._1
          case _ =>
        }
      }

      if (!validModel) {
        return (CodeGenResults.empty, localStore)
      }
    }

    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // plugin initialization phase (one pass)
      localStore = MakefileUtil.addMakefileTargets(ISZ("Makefile"), ISZ(), localStore)
      localStore = MakefileUtil.addMakefileTargets(ISZ("system.mk"), ISZ(), localStore)
      // TODO: move to an init plugin
      val modelIsRusty: B = ops.ISZOps(symbolTable.getThreads()).exists(p => MicrokitUtil.isRusty(p))
      if (modelIsRusty) {
        localStore = localStore + MicrokitPlugin.KEY_MODEL_IS_RUSTY ~> BoolValue(T)
      }

      for (p <- plugins) {
        p match {
          case p: MicrokitInitPlugin =>
            localStore = p.init(model, options, types, symbolTable, localStore, reporter)
          case _ =>
        }
      }
    }

    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // main phase plugins (multi pass)
      val microkitPlugins = PluginUtil.getMicrokitPlugins(plugins)
      var continue = T
      // keep iterating until plugins stop offering to handle things
      while (continue) {
        var somethingHandled = F
        for (plugin <- microkitPlugins if continue) {
          if (plugin.canHandle(model, options, types, symbolTable, localStore, reporter)) {
            val results = plugin.handle(model, options, types, symbolTable, localStore, reporter)
            localStore = results._1
            resources = resources ++ results._2
            somethingHandled = T
          }
        }
        continue = somethingHandled
      }
    }

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

    val makefileContents = MakefileTemplate.mainMakefile(MakefileUtil.getMakefileTargets(ISZ("Makefile"), localStore), MicrokitPlugin.modelIsRusty(localStore))
    val makefilePath = s"${options.sel4OutputDir.get}/Makefile"
    resources = resources :+ ResourceUtil.createResource(makefilePath, makefileContents, T)

    val makefileContainers = StoreUtil.getMakefileContainers(localStore)

    val buildEntries: ISZ[ST] =
      CConnectionProviderPlugin.getMakeFileEntries(localStore) ++
        (for (mk <- makefileContainers) yield mk.buildEntry)

    var elfFiles: ISZ[String] = ISZ()
    var oFiles: ISZ[String] = ISZ()
    for (mk <- makefileContainers) {
      elfFiles = elfFiles ++ mk.getElfNames
      oFiles = oFiles ++ mk.getObjNames
    }

    val elfEntries: ISZ[ST] = for (mk <- makefileContainers) yield mk.elfEntry

    val systemmkContents = MakefileTemplate.systemMakefile(
      elfFiles = elfFiles,
      typeObjectNames = CConnectionProviderPlugin.getTypeObjectNames(localStore),
      buildEntries = buildEntries,
      elfEntries = elfEntries,
      miscTargets = MakefileUtil.getMakefileTargets(ISZ("system.mk"), localStore))

    val systemmkPath = s"${options.sel4OutputDir.get}/${MicrokitCodegen.systemMakeFilename}"
    resources = resources :+ ResourceUtil.createResource(systemmkPath, systemmkContents, T)


    val utilIncludePath = s"${options.sel4OutputDir.get}/${MicrokitUtil.utilDir}/${MicrokitCodegen.dirInclude}"
    val utilSrcPath = s"${options.sel4OutputDir.get}/${MicrokitUtil.utilDir}/${MicrokitCodegen.dirSrc}"
    resources = resources :+ ResourceUtil.createResource(s"${utilIncludePath}/printf.h", MicrokitUtil.printfh, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilSrcPath}/printf.c", MicrokitUtil.printfc, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilIncludePath}/util.h", MicrokitUtil.utilh, T)
    resources = resources :+ ResourceUtil.createResource(s"${utilSrcPath}/util.c", MicrokitUtil.utilc, T)


    if (reporter.hasError) {
      return (CodeGenResults.empty, localStore)
    } else {
      // finalizing plugins
      val microkitFinalizePlugins = PluginUtil.getMicrokitFinalizePlugins(plugins)
      var continue = T
      // keep iterating until plugins stop offering to handle things
      while (continue) {
        var somethingHandled = F
        for (plugin <- microkitFinalizePlugins if continue) {
          if (plugin.canFinalizeMicrokit(model, options, types, symbolTable, localStore, reporter)) {
            val results = plugin.finalizeMicrokit(model, options, types, symbolTable, localStore, reporter)
            localStore = results._1
            resources = resources ++ results._2
            somethingHandled = T
          }
        }
        continue = somethingHandled
      }
    }

    return (CodeGenResults(resources = resources), localStore)
  }
}
