// #Sireum
package org.sireum.hamr.codegen.arsit

import org.sireum._
import org.sireum.hamr.codegen.arsit.plugin.{ArsitConfigurationPlugin, ArsitFinalizePlugin, ArsitInitializePlugin, PlatformProviderPlugin}
import org.sireum.hamr.codegen.arsit.templates._
import org.sireum.hamr.codegen.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, ReporterUtil}
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers.{FileResource, IResource, Resource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType}
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.message._

object Arsit {

  //=================================================================
  //  A r s i t    C o d e    G e n e r a t i o n   P i p e l i n e
  //
  //   Primary methods for invoke pipeline phases.
  //   Phase results accumulated and held in memory using the PhaseResult structure,
  //   which is threaded through phases (intermedidate versions are
  //   named according to the associated phase).
  //=================================================================

  def run(model: ir.Aadl, o: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: ISZ[Plugin], store: Store, reporter: Reporter): (ArsitResult, Store) = {
    ReporterUtil.resetReporter()
    val ret = runInternal(model, o, aadlTypes, symbolTable, plugins, store)
    ReporterUtil.addReports(reporter)
    return ret
  }

  def runInternal(model: ir.Aadl, arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: ISZ[Plugin], store: Store): (ArsitResult, Store) = {
    if (model.components.isEmpty) {
      ReporterUtil.reporter.error(None(), Util.toolName, "Model is empty")
      return (ArsitResult(ISZ(), 0, 0, 0), store)
    }

    assert(model.components.size == 1, "Expecting a single root component")

    val projectDirectories = ProjectDirectories(arsitOptions)

    var localStore = store
    var fileResources: ISZ[Resource] = ISZ()

    for (p <- plugins if p.isInstanceOf[ArsitInitializePlugin] && p.asInstanceOf[ArsitInitializePlugin].canHandleArsitInitializePlugin(arsitOptions, aadlTypes, symbolTable)) {
      val t = p.asInstanceOf[ArsitInitializePlugin].handleArsitInitializePlugin(projectDirectories, arsitOptions, aadlTypes, symbolTable, localStore, ReporterUtil.reporter)
      localStore = t._2
      fileResources = fileResources ++ t._1
    }

    val archPhase = ArchitectureGenerator(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes).generate(plugins, localStore)
    localStore = archPhase._2

    val stubPhase = StubGenerator(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes, archPhase._1).generate(plugins, localStore)
    localStore = stubPhase._2

    val nixPhase = nix.NixGenDispatch.generate(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes, stubPhase._1)

    fileResources = fileResources ++ nixPhase.resources

    var plaformContributions: ISZ[(String, PlatformProviderPlugin.PlatformContributions)] = ISZ()
    for (p <- plugins if p.isInstanceOf[PlatformProviderPlugin] && p.asInstanceOf[PlatformProviderPlugin].canHandlePlatformProviderPlugin(arsitOptions, symbolTable, aadlTypes)) {
      plaformContributions = plaformContributions ++
        (for (pc <- p.asInstanceOf[PlatformProviderPlugin].handlePlatformProviderPlugin(projectDirectories, arsitOptions, symbolTable, aadlTypes, localStore, ReporterUtil.reporter)) yield (p.name, pc))
    }
    for(pc <- plaformContributions) {
      fileResources = fileResources ++ pc._2.resources
    }
    fileResources = fileResources :+ ResourceUtil.createResourceWithMarkers(
      path = Util.pathAppend(projectDirectories.architectureDir, ISZ(arsitOptions.packageName, "Platform.scala")),
      content =  PlatformTemplate.createPlatform(arsitOptions.packageName, plaformContributions),
      markers = PlatformTemplate.markers,
      overwrite = F)

    val maxPortId: Z = nixPhase.maxPort + ArsitConfigurationPlugin.getAdditionalPortIds(ExperimentalOptions.addPortIds(arsitOptions.experimentalOptions), plugins, ReporterUtil.reporter)
    val maxComponentId: Z = nixPhase.maxComponent + ArsitConfigurationPlugin.getAdditionalComponentIds(ExperimentalOptions.addComponentIds(arsitOptions.experimentalOptions), plugins, ReporterUtil.reporter)
    val maxConnectionId: Z = nixPhase.maxConnection + ArsitConfigurationPlugin.getAdditionalConnectionIds(ExperimentalOptions.addConnectionIds(arsitOptions.experimentalOptions), plugins, ReporterUtil.reporter)


    fileResources = fileResources ++ createBuildArtifacts(
      CommonUtil.getLastName(model.components(0).identifier), arsitOptions, projectDirectories, fileResources, ReporterUtil.reporter)

    for (p <- plugins if p.isInstanceOf[ArsitFinalizePlugin] && p.asInstanceOf[ArsitFinalizePlugin].canHandleArsitFinalizePlugin(localStore)) {
      fileResources = fileResources ++ p.asInstanceOf[ArsitFinalizePlugin].handleArsitFinalizePlugin(projectDirectories, arsitOptions, symbolTable, aadlTypes, localStore, ReporterUtil.reporter)
    }

    if (!arsitOptions.noEmbedArt) { // sergen requires art.DataContent so only generate the script when art is being embedded

      val outDataDir = s"${projectDirectories.dataDir}/${arsitOptions.packageName}"
      val outUtilDir = s"${projectDirectories.utilDir}/${arsitOptions.packageName}"

      // embed art
      fileResources = fileResources ++ copyArtFiles(maxPortId, maxComponentId, maxConnectionId, s"${projectDirectories.mainDir}/art") :+
        ResourceUtil.createResourceH(
          path = s"$outDataDir/Aux_Types.scala",
          content =
            st"""// #Sireum
                |
                |package ${arsitOptions.packageName}
                |
                |import org.sireum._
                |
                |${CommentTemplate.safeToEditComment_scala}
                |
                |// Any datatype definitions placed in this file will be processed by sergen and SlangCheck
                |""",
          overwrite = F, isDatatype = T)

      val datatypeResources: ISZ[IResource] = fileResources.filter(f => f.isInstanceOf[IResource] && f.asInstanceOf[IResource].isDatatype).asInstanceOf[ISZ[IResource]]

      val (sergenCmd, sergenConfig) = ToolsTemplate.genSerGen(arsitOptions.packageName, outUtilDir, projectDirectories.slangBinDir, datatypeResources)
      fileResources = fileResources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(projectDirectories.slangBinDir, ISZ("sergen.cmd")), sergenCmd, T)
      fileResources = fileResources :+ sergenConfig

      val (slangCheckCmd, slangCheckConfig) = ToolsTemplate.slangCheck(datatypeResources, arsitOptions.packageName, outUtilDir, projectDirectories.slangBinDir)
      fileResources = fileResources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(projectDirectories.slangBinDir, ISZ("slangcheck.cmd")), slangCheckCmd, T)
      fileResources = fileResources :+ slangCheckConfig
    }


    return (ArsitResult(
      fileResources,
      maxPortId,
      maxComponentId,
      maxConnectionId), localStore)
  }

  def copyArtFiles(maxPort: Z, maxComponent: Z, maxConnections: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for (entry <- ArsitLibrary.getFiles if ops.StringOps(entry._1).contains("art")) {
      val _c: String =
        if (ops.StringOps(entry._1).contains("Art.scala")) {
          @strictpure def atLeast0(i: Z): Z = if (i < 0) 0 else i

          val subs: ISZ[(String, String)] = ISZ(
            ("@range(min = 0, index = T) class BridgeId", s"  @range(min = 0, max = ${atLeast0(maxComponent - 1)}, index = T) class BridgeId"),
            ("@range(min = 0, index = T) class PortId", s"  @range(min = 0, max = ${atLeast0(maxPort - 1)}, index = T) class PortId"),
            ("@range(min = 0, index = T) class ConnectionId", s"  @range(min = 0, max = ${atLeast0(maxConnections - 1)}, index = T) class ConnectionId"),
            ("val numComponents", s"  val numComponents: Z = $maxComponent"),
            ("val numPorts:", s"  val numPorts: Z = $maxPort"),
            ("val numConnections:", s"  val numConnections: Z = $maxConnections")
          )

          def sub(str: String): String = {
            for (s <- subs if ops.StringOps(str).contains(s._1)) {
              return s._2
            }
            return str
          }

          val lines = StringUtil.split_PreserveEmptySegments(entry._2, (c: C) => c == '\n').map((s: String) => sub(s))
          st"${(lines, "\n")}".render
        } else {
          entry._2
        }

      resources = resources :+ ResourceUtil.createStringResource(Util.pathAppend(outputDir, ISZ(entry._1)), _c, T)
    }

    for (i <- 0 until resources.size) {
      resources(i) match {
        case r : IResource if r.name == "DataContent.scala" =>
          var resource = (resources(i).asInstanceOf[IResource])
          resource = resource(isDatatype = T)
          val o = ops.ISZOps(resources)
          val ret = (o.slice(0, i) :+ resource) ++ o.slice(i + 1, resources.size)
          return ret
        case _ =>
      }
    }

    halt("Infeasible")
  }

  def createBuildArtifacts(projectName: String,
                           options: ArsitOptions,
                           projDirs: ProjectDirectories,
                           resources: ISZ[Resource],
                           reporter: Reporter): ISZ[Resource] = {

    var ret: ISZ[Resource] = ISZ()
    val root = options.slangOutputDir

    val demoScalaPath: String = {
      val candidate: ISZ[Resource] = resources.filter(p =>
        p match {
          case p: FileResource => ops.StringOps(p.dstPath).endsWith("Demo.scala")
          case _ => F
        })
      if (candidate.nonEmpty) root.relativize(Os.path(candidate(0).asInstanceOf[FileResource].dstPath)).value
      else "??"
    }

    val bridgeTestPath: String = root.relativize(Os.path(projDirs.testBridgeDir)).value

    def dewindowfy(s: String): String = {
      // want path seps in build.sbt comments to be nix so they don't break reg tests
      return ops.StringOps.replaceAllLiterally(conversions.String.toCis(s), "\\", "/")
    }

    val proyekBuildDest = options.slangOutputDir / "bin" / "project.cmd"
    val proyekBuildContent = ProjectTemplate.proyekBuild(projectName, options.packageName, !options.noEmbedArt,
      dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
    ret = ret :+ ResourceUtil.createExeCrlfResource(proyekBuildDest.value, proyekBuildContent, F)

    val versionPropDest = options.slangOutputDir / "versions.properties"
    val versionPropBuildContent = ProjectTemplate.proyekVersionProperties()
    ret = ret :+ ResourceUtil.createResource(versionPropDest.value, versionPropBuildContent, F)

    if (options.genSbtMill) {
      val millBuildDest = options.slangOutputDir / "build.sc"
      val outputDirSimpleName = millBuildDest.up.name
      val millBuildContent = ProjectTemplate.millBuild(options.packageName, outputDirSimpleName, !options.noEmbedArt)
      ret = ret :+ ResourceUtil.createResource(millBuildDest.value, millBuildContent, F)

      val sbtBuildDest = options.slangOutputDir / "build.sbt"
      val sbtBuildContent = ProjectTemplate.sbtBuild(projectName, options.packageName, !options.noEmbedArt,
        dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
      ret = ret :+ ResourceUtil.createResource(sbtBuildDest.value, sbtBuildContent, F)

      val buildPropertiesDest = options.slangOutputDir / "project" / "build.properties"
      ret = ret :+ ResourceUtil.createResource(buildPropertiesDest.value, ProjectTemplate.sbtBuildPropertiesContents(), F)

      val pluginsSbtDest = options.slangOutputDir / "project" / "plugins.sbt"
      ret = ret :+ ResourceUtil.createResource(pluginsSbtDest.value, ProjectTemplate.sbtPluginsSbtContents(), F)
    }

    reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
      ProjectTemplate.arsitSlangInstructionsMessage(options.slangOutputDir.value, options.genSbtMill).render)

    if (isNixProject(options.platform)) {
      val cmakeDir: String = projDirs.cNixDir

      val devDir = projDirs.cExt_c_Dir

      val transpile: String = {
        val x = resources.filter(p =>
          p match {
            case p: IResource => ops.StringOps(p.dstPath).endsWith("bin/transpile.cmd")
            case _ => F
          }).asInstanceOf[ISZ[IResource]]
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val compile: String = {
        val x = resources.filter(p =>
          p match {
            case p: IResource => ops.StringOps(p.dstPath).contains("bin/compile.cmd")
            case _ => F
          }).asInstanceOf[ISZ[IResource]]
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val run: String = {
        val x = resources.filter(p =>
          p match {
            case p: IResource => ops.StringOps(p.dstPath).contains("bin/run.sh")
            case _ => F
          }).asInstanceOf[ISZ[IResource]]
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val stop: String = {
        val x = resources.filter(p =>
          p match {
            case p: IResource => ops.StringOps(p.dstPath).endsWith("bin/stop.sh")
            case _ => F
          }).asInstanceOf[ISZ[IResource]]
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
        ProjectTemplate.arsitCInstructionsMessage(cmakeDir, devDir, transpile, compile, run, stop).render)
    }

    if (options.platform == ArsitPlatform.SeL4) {
      val transpile: String = {
        val x = resources.filter(p =>
          p match {
            case p: IResource => ops.StringOps(p.dstPath).endsWith("bin/transpile-sel4.cmd")
            case _ => F
          }).asInstanceOf[ISZ[IResource]]
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
        ProjectTemplate.arsitCAmkESInstructionsMessage(projDirs.cExt_c_Dir, transpile).render)
    }

    return ret
  }

  def isNixProject(value: ArsitPlatform.Type): B = {
    val ret: B = value match {
      case ArsitPlatform.Linux => T
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Cygwin => T
      case _ => F
    }
    return ret
  }
}
