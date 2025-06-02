// #Sireum
package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.Os.Path
import org.sireum.hamr.codegen.act.util.Util.ACT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.codegen.arsit.Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.codegen.common.containers._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.{CodegenHamrPlatform, CodegenOption}
import org.sireum.hamr.codegen.common.util.ModelUtil.ModelElements
import org.sireum.hamr.codegen.common.util.{CodeGenResults, ExperimentalOptions, ModelUtil}
import org.sireum.hamr.codegen.common.{DirectoryUtil, StringUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.ops.StringOps
import org.sireum.hamr.codegen.ros2.Ros2Codegen

object CodeGen {

  val toolName: String = "HAMR CodeGen"

  def codeGen(model: Aadl,
              shouldWriteOutResources: B,
              options: CodegenOption,
              plugins: ISZ[Plugin],
              store: Store,
              reporter: Reporter,
              transpilerCallback: (SireumSlangTranspilersCOption, Reporter) => Z,
              proyekIveCallback: SireumProyekIveOption => Z,
              sergenCallback: (SireumToolsSergenOption, Reporter) => Z,
              slangCheckCallback: (SireumToolsSlangcheckGeneratorOption, Reporter) => Z): (CodeGenResults, Store) = {

    @pure def finalizePlugins(aadlTypes: Option[AadlTypes], symbolTable: Option[SymbolTable],
                              codegenResults: CodeGenResults, currentStore: Store): (CodeGenResults, Store) = {
      var localStore = currentStore
      for(p <- plugins) {
        localStore = p.finalizePlugin(model, aadlTypes, symbolTable, codegenResults, localStore, options, reporter)
      }
      return (codegenResults, localStore)
    }

    var localStore = store
    if (model.components.size != 1) {
      reporter.error(None(), toolName, s"Expecting exactly one root component but found ${model.components.size}")
      return (CodeGenResults.empty, localStore)
    }

    var modOptions = options

    val systemRootDirectory: Os.Path = getSystemRoot(model, modOptions.workspaceRootDir)

    def getPath(path: String): Os.Path = {
      val p = Os.path(path)
      return (
        if (p.isAbs) p // use the abs path the user requested
        else systemRootDirectory / path) // make the user's request relative to the system's root directory
    }

    val outputDir: Path = getPath(modOptions.outputDir.getOrElse("hamr"))

    val slangOutputDir: Path = getPath(modOptions.slangOutputDir.getOrElse((outputDir / "slang").value))

    val output_shared_C_Directory: Path = getPath(modOptions.slangOutputCDir.getOrElse((outputDir / "c").value))

    val sel4Type: String = if (modOptions.platform == CodegenHamrPlatform.Microkit) "microkit" else "camkes"
    val sel4OutputDir: Path = getPath(modOptions.sel4OutputDir.getOrElse((outputDir / sel4Type).value))

    val output_platform_C_Directory: Path =
      if (modOptions.platform == CodegenHamrPlatform.SeL4) sel4OutputDir / DirectoryUtil.DIR_SLANG_LIBRARIES
      else output_shared_C_Directory

    val packageName: String = if (modOptions.packageName.nonEmpty) {
      cleanupPackageName(modOptions.packageName.get)
    } else {
      cleanupPackageName("base" )
    }

    modOptions = modOptions(
      packageName = Some(packageName),
      outputDir = Some(outputDir.canon.value),
      slangOutputDir = Some(slangOutputDir.canon.value),
      sel4OutputDir = Some(sel4OutputDir.canon.value))

    val (runArsit, runACT, runMicrokit, runRos2, isTranspilerProject, isSlangProject): (B, B, B, B, B, B) =
      modOptions.platform match {
        case CodegenHamrPlatform.JVM => (T, F, F, F, F, T)

        case CodegenHamrPlatform.Linux => (T, F, F, F, T, T)
        case CodegenHamrPlatform.Cygwin => (T, F, F, F, T, T)
        case CodegenHamrPlatform.MacOS => (T, F, F, F, T, T)

        case CodegenHamrPlatform.SeL4 => (T, T, F, F, T, T)

        case CodegenHamrPlatform.SeL4_Only => (F, T, F, F, F, F)
        case CodegenHamrPlatform.SeL4_TB => (F, T, F, F, F, F)

        case CodegenHamrPlatform.Microkit => (F, F, T, F, F, F)

        case CodegenHamrPlatform.Ros2 => (F, F, F, T, F, F)
    }

    var reporterIndex = z"0"

    if (modOptions.runtimeMonitoring && isTranspilerProject) {
      reporter.error(None(), toolName, "Runtime monitoring support for transpiled projects has not been added yet. Disable runtime-monitoring before transpiling")
      reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())
      return finalizePlugins(None(), None(), CodeGenResults.empty, localStore)
    }

    var arsitResources: ISZ[Resource] = ISZ()

    var wroteOutArsitResources: B = F

    val result: (Option[ModelElements], Store) = ModelUtil.resolve(model, model.components(0).identifier.pos, packageName, modOptions, localStore, reporter)
    localStore = result._2
    reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())
    if (result._1.isEmpty) {
      return finalizePlugins(None(), None(), CodeGenResults.empty, localStore)
    }

    val (rmodel, aadlTypes, symbolTable): (Aadl, AadlTypes, SymbolTable) = (result._1.get.model, result._1.get.types, result._1.get.symbolTable)

    if (modOptions.runtimeMonitoring && symbolTable.getThreads().isEmpty) {
      reporter.error(None(), toolName, "Model must contain threads in order to enable runtime monitoring")
      return finalizePlugins(Some(aadlTypes), Some(symbolTable), CodeGenResults.empty, localStore)
    }

    if (~reporter.hasError && runRos2) {
      val results = Ros2Codegen().run(rmodel, modOptions, aadlTypes, symbolTable, plugins, localStore, reporter)
      localStore = results._2
      if (!reporter.hasError) {
        writeOutResources(results._1.resources, T, reporter)
      }
      if (!modOptions.parseableMessages) {
        reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())
      }
      return finalizePlugins(Some(aadlTypes), Some(symbolTable), CodeGenResults(resources = results._1.resources), localStore)
    }

    if (!reporter.hasError && runMicrokit) {
      reporter.info(None(), toolName, "Generating Microkit artifacts...")
      val results = MicrokitCodegen().run(rmodel, modOptions, aadlTypes, symbolTable, plugins, localStore, reporter)
      localStore = results._2
      if (!reporter.hasError) {
        writeOutResources(results._1.resources, F, reporter)
      }
      if (!modOptions.parseableMessages) {
        reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())
      }
      return finalizePlugins(Some(aadlTypes), Some(symbolTable), results._1, localStore)
    }

    if (!reporter.hasError && runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.util.IpcMechanism.SharedMemory
      val platform = arsit.util.ArsitPlatform.byName(modOptions.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      val opt = arsit.util.ArsitOptions(
        slangOutputDir = slangOutputDir,
        packageName = packageName,
        noEmbedArt = modOptions.noEmbedArt,
        bless = genBlessEntryPoints,
        verbose = modOptions.verbose,
        runtimeMonitoring = modOptions.runtimeMonitoring,
        devicesAsThreads = modOptions.devicesAsThreads,
        genSbtMill = modOptions.genSbtMill,
        ipc = ipc,
        auxCodeDirs = modOptions.slangAuxCodeDirs,
        outputSharedCDir = output_shared_C_Directory,
        outputPlatformCDir = output_platform_C_Directory,
        excludeImpl = modOptions.excludeComponentImpl,
        platform = platform,
        bitWidth = modOptions.bitWidth,
        maxStringSize = modOptions.maxStringSize,
        maxArraySize = modOptions.maxArraySize,
        pathSeparator = fileSep,
        experimentalOptions = modOptions.experimentalOptions,

        runSlangCheck = !ExperimentalOptions.disableSlangCheck(modOptions.experimentalOptions)
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")
      reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

      val results = arsit.Arsit.run(rmodel, opt, aadlTypes, symbolTable, plugins, localStore, reporter)
      localStore = results._2

      arsitResources = arsitResources ++ results._1.resources

      reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ(ARSIT_INSTRUCTIONS_MESSAGE_KIND))

      arsitResources = removeDuplicates(arsitResources, reporter)

      val sergenConfigs = Resource.projectSergenConfigs(arsitResources)
      if (!reporter.hasError && isSlangProject && sergenConfigs.nonEmpty &&
        !ExperimentalOptions.disableSergen(modOptions.experimentalOptions) &&
        !modOptions.noEmbedArt // only run sergen and slangcheck when art is embedded
      ) {
        // doesn't matter what 'o.writeOutResources' is, sergen/slangcheck needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, F, reporter)
          wroteOutArsitResources = T
        }

        if (!reporter.hasError) {
          reporter.info(None(), toolName, "Generating serializer/deserializers for data types via sergen ...")
          reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

          for (sc <- sergenConfigs if !reporter.hasError) {
            sergenCallback(sc, reporter)
          }
        }
      }

      val slangCheckConfigs = Resource.projectSlangCheckConfigs(arsitResources)
      if (!reporter.hasError && isSlangProject && slangCheckConfigs.nonEmpty &&
        !ExperimentalOptions.disableSlangCheck(modOptions.experimentalOptions) &&
        !modOptions.noEmbedArt // only run sergen and slangcheck when art is embedded
      ) {
        // doesn't matter what 'o.writeOutResources' is, sergen/slangcheck needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, F, reporter)
          wroteOutArsitResources = T
        }

        if (!reporter.hasError) {
          reporter.info(None(), toolName, "Generating SlangCheck artifacts ...")
          reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

          for (sc <- slangCheckConfigs if !reporter.hasError) {
            slangCheckCallback(sc, reporter)
          }
        }
      }

      if (!reporter.hasError && !modOptions.noProyekIve && isSlangProject) {
        // doesn't matter what 'o.writeOutResources' is, proyek ive needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, F, reporter)
          wroteOutArsitResources = T
        }

        if (!reporter.hasError) {
          val proyekConfig = SireumProyekIveOption(
            help = "",
            args = ISZ(slangOutputDir.canon.value),
            force = F,
            edition = SireumProyekIveEdition.Community,
            javac = ISZ(),
            scalac = ISZ(),
            ignoreRuntime = F,
            json = None(),
            name = None(),
            outputDirName = Some("out"),
            project = None(),
            slice = ISZ(),
            symlink = F,
            versions = ISZ(),
            cache = None(),
            docs = T,
            sources = T,
            repositories = ISZ()
          )

          reporter.info(None(), toolName, "Generating IVE and Scala Metals project via Proyek ...")
          reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

          if (proyekIveCallback(proyekConfig) != 0) {
            reporter.error(None(), toolName, "Proyek did not complete successfully")
          }
        }
      }

      if (!reporter.hasError && modOptions.runTranspiler && isTranspilerProject) {

        // doesn't matter what 'o.writeOutResources' is, transpiler needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, F, reporter)
          wroteOutArsitResources = T
        }

        if (!reporter.hasError) {
          reporter.info(None(), toolName, "Transpiling project ...")
          reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

          for (transpilerConfig <- Resource.projectTranspilerConfigs(results._1.resources) if !reporter.hasError) {
            // CTranspiler prints all the messages in the passed in reporter so
            // create a new one for each config
            val transpilerReporter = Reporter.create
            if (transpilerCallback(transpilerConfig, transpilerReporter) != 0) {
              reporter.error(None(), toolName, s"Transpiler did not complete successfully")
            }
          }
        }
      }
    }

    var actResources: ISZ[Resource] = ISZ()

    if (!reporter.hasError && runACT) {

      val platform = org.sireum.hamr.codegen.act.util.ActPlatform.byName(modOptions.platform.name).get
      reporter.info(None(), toolName, "Generating CAmkES artifacts...")

      val actOptions = org.sireum.hamr.codegen.act.util.ActOptions(
        sel4OutputDir = sel4OutputDir.value,
        auxFiles = getAuxFiles(modOptions.sel4AuxCodeDirs, F, reporter),
        workspaceRootDir = modOptions.workspaceRootDir,
        platform = platform,
        hamrBasePackageName = Some(packageName),
        experimentalOptions = modOptions.experimentalOptions
      )

      val results = org.sireum.hamr.codegen.act.Act.run(rmodel, actOptions, aadlTypes, symbolTable, reporter)
      actResources = actResources ++ results.resources

      reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ(ACT_INSTRUCTIONS_MESSAGE_KIND))
    }

    actResources = removeDuplicates(actResources, reporter)

    if (!reporter.hasError && shouldWriteOutResources) {
      if (!wroteOutArsitResources) {
        writeOutResources(arsitResources, F, reporter)
        wroteOutArsitResources = T
      }
      writeOutResources(actResources, F, reporter)
    }

    reporterIndex = printMessages(reporter.messages, modOptions.verbose, reporterIndex, ISZ())

    if (!reporter.hasError && shouldWriteOutResources) {
      // always print out any instructional messages
      val instructions = reporter.messages.filter(p =>
        p.kind == ARSIT_INSTRUCTIONS_MESSAGE_KIND || p.kind == ACT_INSTRUCTIONS_MESSAGE_KIND)
      for (i <- instructions) {
        cprintln(F, "")
        cprintln(F, i.text)
      }
    }

    if (reporter.hasError && !modOptions.verbose) { // at least need to print out the error messages
      printMessages(reporter.errors, T, 0, ISZ())
    }

    return finalizePlugins(
      Some(aadlTypes), Some(symbolTable),
      CodeGenResults(resources = arsitResources ++ actResources), localStore)
  }

  def printMessages(reporterMessages: ISZ[Message], verbose: B, messageIndex: Z, kindsToFilterOut: ISZ[String]): Z = {
    var messages = ops.ISZOps(reporterMessages).slice(messageIndex, reporterMessages.size)
    for (key <- kindsToFilterOut) {
      messages = messages.filter(p => p.kind != key)
    }

    var infoWarnings: ISZ[Message] = ISZ()
    var errors: ISZ[Message] = ISZ()
    for (m <- messages) {
      if (m.isError) {
        errors = errors :+ m
      } else {
        infoWarnings = infoWarnings :+ m
      }
    }

    if (verbose) {
      ReporterImpl(infoWarnings).printMessages()
    }

    if (errors.nonEmpty) {
      cprintln(T, "")
        cprintln(T, "Errors:")
      ReporterImpl(errors).printMessages()
    }

    return reporterMessages.size
  }

  def toOption(f: Path): Option[String] = {
    return Some(f.value)
  }

  def cleanupPackageName(s: String): String = {
    val o = ops.StringOps(ops.StringOps(ops.StringOps(s).replaceAllChars('-', '_')).replaceAllChars('.', '_'))
    if (o.startsWith("_")) {
      return "base"
    } else {
      return o.s
    }
  }

  def getHeaderFiles(root: Os.Path): ISZ[Os.Path] = {
    assert(root.isDir)

    var ret: ISZ[Os.Path] = ISZ()

    def processDir(dir: Os.Path): Unit = {
      ret = ret ++ dir.list.filter(p => p.ext == "h")

      for (d <- dir.list if d.isDir) {
        processDir(d)
      }
    }

    processDir(root)
    return ret
  }

  def getAuxFiles(directories: ISZ[String], includeParentDir: B, reporter: Reporter): Map[String, String] = {
    var ret: Map[String, String] = Map.empty

    for (rootDir <- directories) {
      val rootDirPath = Os.path(rootDir)
      if (!rootDirPath.exists) {
        reporter.warn(None(), toolName, s"Directory '${rootDirPath}' does not exist")
      }

      def processDir(dir: Os.Path): Unit = {
        for (f <- dir.list.filter(p => p.ext == "c" || p.ext == "h")) {
          // make subdir paths relative to the root dir
          val root: Os.Path = if (includeParentDir) rootDirPath.up else rootDirPath
          val rel = root.relativize(f).value

          ret = ret + (rel ~> f.read)
        }

        for (f <- dir.list if f.isDir) {
          processDir(f)
        }
      }

      if (rootDirPath.isDir) {
        processDir(rootDirPath)
      }
    }

    return ret
  }

  // With invertMarkers enabled, edits made to the existing file within the markers will be carried over into the
  // freshly generated file.  With it disabled, edits made outside of the markers will be carried over.
  def writeOutResources(resources: ISZ[Resource], invertMarkers: B, reporter: Reporter): Unit = {
    def render(i: InternalResource): String = {
      val ret: String = {
        val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
        val replace: String = if (i.makeCRLF) "\r\n" else "\n"
        ops.StringOps(i.content.render).replaceAllLiterally(lineSep, replace)
      }
      return ret
    }

    for (r <- resources) {
      r match {
        case r: FileResource =>
          val _p = Os.path(r.dstPath).canon
          val p = _p.canon
          assert(!p.exists || p.isFile)
          p.up.mkdirAll()
          r match {
            case i: IResource =>
              if (i.overwrite || !p.exists) {
                val content = render(i)
                p.writeOver(content)
                reporter.info(None(), toolName, s"Wrote: ${p}")
                if (i.makeExecutable) {
                  p.chmodAll("700")
                  reporter.info(None(), toolName, s"Made ${p} executable")
                }
              } else if (p.exists && i.markers.nonEmpty) {
                val newContent = render(i)
                val oldSections = StringUtil.collectSections(p.read, toolName, i.markers, reporter)
                val newSections = StringUtil.collectSections(newContent, toolName, i.markers, reporter)

                val missingMarkers = i.markers.filter((m: Marker) => !ops.ISZOps(oldSections.keys).contains(m))
                if (missingMarkers.nonEmpty) {
                  val fixme = p.up / s"${p.name}_fixme"
                  fixme.writeOver(newContent)
                  val msg =
                    st"""Existing file did not contain the following markers. Copy the markers/content found in ${fixme.toUri}
                        |to the corresponding locations in ${p.toUri}
                        |
                        |  ${(missingMarkers, "\n")}"""
                  reporter.error(None(), toolName, msg.render)
                } else {
                  if (invertMarkers) {
                    val replacements: ISZ[(Z, Z, String)] = newSections.entries.map((newEntry: (Marker, (Z, Z, String))) =>
                      ((newEntry._2._1, newEntry._2._2, oldSections.get(newEntry._1).get._3)))
                    val content: String = StringUtil.replaceSections(newContent, replacements, reporter)
                    p.writeOver(content)
                  } else {
                    val replacements: ISZ[(Z, Z, String)] = oldSections.entries.map((oldEntry: (Marker, (Z, Z, String))) =>
                      ((oldEntry._2._1, oldEntry._2._2, newSections.get(oldEntry._1).get._3)))
                    val content: String = StringUtil.replaceSections(p.read, replacements, reporter)
                    p.writeOver(content)
                  }
                  reporter.info(None(), toolName, s"Wrote and preserved existing content: ${p}")
                }

              } else {
                reporter.info(None(), toolName, s"File exists, will not overwrite: ${p}")
              }
            case e: EResource =>
              if (e.symLink) {
                halt("sym linking not yet supported")
              } else {
                Os.path(e.srcPath).copyOverTo(p)
                reporter.info(None(), toolName, s"Copied: ${e.srcPath} to ${p}")
              }
          }
        case _ =>
      }
    }
  }

  def removeDuplicates(resources: ISZ[Resource], reporter: Reporter): ISZ[Resource] = {
    var m: HashSMap[String, Resource] = HashSMap.empty[String, Resource]
    var others: ISZ[Resource] = ISZ()
    for (r <- resources) {
      r match {
        case r: FileResource =>if (m.contains(r.dstPath)) {
          val entry = m.get(r.dstPath).get

          // sanity checks
          (r, entry) match {
            case ((ei: ExternalResource, ci: ExternalResource)) =>
              if (ei.srcPath != ci.srcPath) {
                reporter.warn(None(), toolName, s"srcPath for ${r.dstPath} not the same for duplicate entries")
              }
              if (ei.symLink != ci.symLink) {
                reporter.warn(None(), toolName, s"symLink flag for ${r.dstPath} not the same for duplicate entries")
              }
            case ((ri: IResource, ci: IResource)) =>
              if (ri.content.render != ci.content.render) {
                reporter.warn(None(), toolName, s"content of ${r.dstPath} not the same for duplicate entries")
              }
              if (ri.overwrite != ci.overwrite) {
                reporter.warn(None(), toolName, s"overwrite flag for ${r.dstPath} not the same for duplicate entries")
              }
              if (ri.makeExecutable != ci.makeExecutable) {
                reporter.warn(None(), toolName, s"makeExecutable flag for ${r.dstPath} not the same for duplicate entries")
              }

            case _ => halt(s"Infeasible: resource types not the same")
          }
        } else {
          m = m + (r.dstPath ~> r)
        }
        case _ =>
          others = others :+ r
      }
    }
    return m.values ++ others
  }

  def getSystemRoot(model: Aadl, workspaceRootDir: Option[String]): Os.Path = {
    model.components(0).identifier.pos match {
      case Some(p) => p.uriOpt match {
        case Some(uri) =>
          if (ops.StringOps(uri).startsWith("file")) {
            // probably from hamr sysml which emits absolute uri's
            return Os.Path.fromUri(uri).up
          } else {
            // probably from osate which emits relative uri's
            workspaceRootDir match {
              case Some(wroot) =>
                var cand = Os.path(wroot) / uri
                if (cand.exists) {
                  return cand
                }
                else {
                  cand = Os.path(".") / uri
                  if (cand.exists) {
                    return cand
                  }
                }
              case _ =>
            }
          }
        case _ =>
      }
      case _ =>
    }
    return Os.path(".")
  }
}

