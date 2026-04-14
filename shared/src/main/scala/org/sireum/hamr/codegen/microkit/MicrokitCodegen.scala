// #Sireum
package org.sireum.hamr.codegen.microkit

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ModelUtil, MonitorInjector, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins._
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

  def run(modelP: Aadl, options: CodegenOption, aadlTypesP: AadlTypes, symbolTableP: SymbolTable, plugins: ISZ[Plugin], store: Store, reporter: Reporter): (CodeGenResults, Store) = {
    var localStore = store

    var model = modelP
    var aadlTypes = aadlTypesP
    var symbolTable = symbolTableP

    if (!reporter.hasError) { // linting phase (one pass)
      var validModel = T
      for (p <- plugins) {
        p match {
          case p: MicrokitLintPlugin =>
            val results = p.validModel(model, options, aadlTypes, symbolTable, localStore, reporter)
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
            localStore = p.init(model, options, aadlTypes, symbolTable, localStore, reporter)
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
          if (plugin.canHandle(model, options, aadlTypes, symbolTable, localStore, reporter)) {
            val results = plugin.handle(model, options, aadlTypes, symbolTable, localStore, reporter)
            localStore = results._1
            resources = resources ++ results._2
            somethingHandled = T
          }
        }
        continue = somethingHandled
      }
    }

    val makeFileTargetes = MakefileUtil.getMakefileTargets(ISZ("Makefile"), localStore)
    val modelIsRusty = MicrokitPlugin.modelIsRusty(localStore)
    val makefileContents = MakefileTemplate.mainMakefile(makeFileTargetes, modelIsRusty)

    val makefilePath = s"${options.sel4OutputDir.get}/Makefile"
    resources = resources :+ ResourceUtil.createResource(makefilePath, makefileContents, T)

    val makefileContainers = StoreUtil.getMakefileContainers(localStore)

    var elfFiles: Set[String] = Set.empty[String]
    for (mk <- makefileContainers) {
      elfFiles = elfFiles ++ mk.getElfNames
    }

    val elfEntries: Set[String] = Set.empty[String] ++ (for (mk <- makefileContainers) yield mk.elfEntry.render)

    val isMCS = CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.MCS



    val systemmkContents: ST =
      if (isMCS) {
        val includesPaths: ISZ[String] = for (mk <- makefileContainers) yield s"-I$$(TOP_DIR)/${mk.relativePathIncludeDir}"

        val sourcePaths: ISZ[String] = for (mk <- makefileContainers) yield s"$$(TOP_DIR)/${mk.relativePathSrcDir}"

        var rustBuildEntries: ISZ[ST] = ISZ()
        for (mk <- makefileContainers if (mk.isRustic && mk.hasUserContent)) {
          rustBuildEntries = rustBuildEntries :+ mk.rustBuildEntry
        }

        val typeObjectNames = Set.empty[String] ++ CConnectionProviderPlugin.getTypeSimpleObjectNames(localStore)

        MakefileTemplate.systemMakefileMCS(
          includePaths = includesPaths,
          sourcePaths = sourcePaths,
          elfFiles = elfFiles.elements,
          typeObjectNames = typeObjectNames.elements,
          buildEntries = rustBuildEntries,
          elfEntries = elfEntries.elements,
          miscTargets = MakefileUtil.getMakefileTargets(ISZ("system.mk"), localStore))
      } else {

        val typeObjectNames = Set.empty[String] ++ CConnectionProviderPlugin.getTypeObjectNames(localStore)

        val buildEntries: ISZ[ST] =
          CConnectionProviderPlugin.getMakeFileEntries(localStore) ++
            (for (mk <- makefileContainers) yield mk.buildEntry)

        MakefileTemplate.systemMakefileDomainScheduler(
          elfFiles = elfFiles.elements,
          typeObjectNames = typeObjectNames.elements,
          buildEntries = buildEntries,
          elfEntries = elfEntries.elements,
          miscTargets = MakefileUtil.getMakefileTargets(ISZ("system.mk"), localStore))
      }

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
          if (plugin.canFinalizeMicrokit(model, options, aadlTypes, symbolTable, localStore, reporter)) {
            val results = plugin.finalizeMicrokit(model, options, aadlTypes, symbolTable, localStore, reporter)
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
