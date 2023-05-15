// #Sireum
package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.Os.Path
import org.sireum.hamr.act.util.Util.{ACT_INSTRUCTIONS_MESSAGE_KIND}
import org.sireum.hamr.arsit
import org.sireum.hamr.arsit.{ProjectDirectories, Util}
import org.sireum.hamr.arsit.Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.arsit.templates.ToolsTemplate
import org.sireum.hamr.codegen.common.containers._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType}
import org.sireum.hamr.codegen.common.util.CodeGenPlatform._
import org.sireum.hamr.codegen.common.util.ModelUtil.ModelElements
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, CodeGenPlatform, CodeGenResults, ExperimentalOptions, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.common.{DirectoryUtil, StringUtil}
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.ops.StringOps

object CodeGen {

  val toolName: String = "HAMR CodeGen"

  def codeGen(model: Aadl,
              options: CodeGenConfig,
              plugins: MSZ[Plugin],
              reporter: Reporter,
              transpilerCallback: (TranspilerConfig, Reporter) => Z,
              proyekIveCallback: (ProyekIveConfig) => Z): CodeGenResults = {

    val targetingSel4 = options.platform == CodeGenPlatform.SeL4

    val slangOutputDir: Path = Os.path(options.slangOutputDir.getOrElse("."))

    val output_shared_C_Directory: Path =
      if (options.slangOutputCDir.nonEmpty) Os.path(options.slangOutputCDir.get)
      else slangOutputDir / "src" / "c"

    val camkesOutputDir: Path =
      if (options.camkesOutputDir.nonEmpty) Os.path(options.camkesOutputDir.get)
      else output_shared_C_Directory / "camkes"

    val output_platform_C_Directory: Path =
      if (targetingSel4) camkesOutputDir / DirectoryUtil.DIR_SLANG_LIBRARIES
      else output_shared_C_Directory

    val packageName: String = if (options.packageName.nonEmpty) {
      cleanupPackageName(options.packageName.get)
    } else {
      cleanupPackageName(slangOutputDir.name)
    }

    val (runArsit, runACT, hamrIntegration, isTranspilerProject, isSlangProject): (B, B, B, B, B) = options.platform match {
      case JVM => (T, F, F, F, T)

      case Linux => (T, F, F, T, T)
      case Cygwin => (T, F, F, T, T)
      case MacOS => (T, F, F, T, T)

      case SeL4 => (T, T, T, T, T)

      case SeL4_Only => (F, T, F, F, F)
      case SeL4_TB => (F, T, F, F, F)
    }

    var reporterIndex = z"0"

    var transpilerConfigs: ISZ[TranspilerConfig] = ISZ()

    var arsitResources: ISZ[Resource] = ISZ()

    var wroteOutArsitResources: B = F

    val result: Option[ModelElements] = ModelUtil.resolve(model, packageName, options, reporter)
    reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())
    if (result.isEmpty) {
      return CodeGenResults(ISZ(), ISZ())
    }

    val (rmodel, aadlTypes, symbolTable): (Aadl, AadlTypes, SymbolTable) = (result.get.model, result.get.types, result.get.symbolTable)
    if (!reporter.hasError && runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.util.IpcMechanism.byName(options.ipc.name).get
      val platform = arsit.util.ArsitPlatform.byName(options.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      // TODO: remove once slang check is part of kekinian
      val slangCheckJar: Option[Os.Path] = {
        Os.env("SLANG_CHECK_JAR") match {
          case Some(p) =>
            val cand = Os.path(p)
            if (!cand.exists) {
              reporter.error(None(), toolName, s"SLANG_CHECK_JAR is not a file: $p")
              None()
            } else {
              println("SlangCheck found")
              Some(cand)
            }
          case _ => None()
        }
      }

      val opt = arsit.util.ArsitOptions(
        outputDir = slangOutputDir,
        packageName = packageName,
        noEmbedArt = options.noEmbedArt,
        bless = genBlessEntryPoints,
        verbose = options.verbose,
        devicesAsThreads = options.devicesAsThreads,
        genSbtMill = options.genSbtMill,
        ipc = ipc,
        auxCodeDirs = options.slangAuxCodeDirs,
        outputSharedCDir = output_shared_C_Directory,
        outputPlatformCDir = output_platform_C_Directory,
        excludeImpl = options.excludeComponentImpl,
        platform = platform,
        bitWidth = options.bitWidth,
        maxStringSize = options.maxStringSize,
        maxArraySize = options.maxArraySize,
        pathSeparator = fileSep,
        experimentalOptions = options.experimentalOptions,

        runSlangCheck = slangCheckJar.nonEmpty && !ExperimentalOptions.disableSlangCheck(options.experimentalOptions)
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")
      reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

      val results = arsit.Arsit.run(rmodel, opt, aadlTypes, symbolTable, plugins, reporter)

      arsitResources = arsitResources ++ results.resources
      transpilerConfigs = transpilerConfigs ++ results.transpilerOptions

      reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ(ARSIT_INSTRUCTIONS_MESSAGE_KIND))

      arsitResources = removeDuplicates(arsitResources, reporter)

      val r = !reporter.hasError && isSlangProject && slangCheckJar.nonEmpty && !ExperimentalOptions.disableSlangCheck(options.experimentalOptions)
      println("-----")
      println(s"r = $r")
      println(!reporter.hasError)
      println(isSlangProject)
      println(slangCheckJar.nonEmpty)
      println(!ExperimentalOptions.disableSlangCheck(options.experimentalOptions))
      println("-----")
      if (!reporter.hasError && isSlangProject && slangCheckJar.nonEmpty && !ExperimentalOptions.disableSlangCheck(options.experimentalOptions)) {
        val noArrayTypes: B = !ops.ISZOps(aadlTypes.typeMap.values).exists(t => t.isInstanceOf[ArrayType])

        println(s"noArrayTypes = $noArrayTypes")
        if (noArrayTypes) {
          val datatypeResources: ISZ[Resource] = for (r <- arsitResources.filter(f => f.isInstanceOf[IResource] && f.asInstanceOf[IResource].isDatatype)) yield r.asInstanceOf[IResource]

          val projectDirectories: ProjectDirectories = ProjectDirectories(opt)

          // TODO: include slang check containers once slang check supports traits
          val datatypesMinusContainers = datatypeResources.filter(d => !ops.StringOps(d.name).endsWith("SlangCheckContainer.scala"))

          val slangCheck = ToolsTemplate.slangCheck(datatypesMinusContainers, packageName, projectDirectories.dataDir, projectDirectories.slangBinDir)
          val slangCheckCmd = ResourceUtil.createExeCrlfResource(Util.pathAppend(projectDirectories.slangBinDir, ISZ("slangcheck.cmd")), slangCheck, T)
          arsitResources = arsitResources :+ slangCheckCmd

          // doesn't matter what 'o.writeOutResources' is, slang check needs the
          // resources to be written out
          if (!wroteOutArsitResources) {
            writeOutResources(arsitResources, reporter)
            wroteOutArsitResources = T
          }

          // TODO: add sergen option and pass in callback
          val sergen = slangOutputDir / "bin" / "sergen.cmd"
          val sergenRes = proc"$sergen".console.echo.redirectErr.run()
          if (options.verbose) {
            println(sergenRes.out)
          }
          if (!sergenRes.ok) {
            reporter.error(None(), toolName, "sergen generation failed")
          }

          // TODO: add slang check option and pass in callback
          val slangCheckP = Os.path(slangCheckCmd.dstPath)
          val slangCheckRes = proc"$slangCheckP".console.echo.redirectErr.run()
          if (options.verbose) {
            println(slangCheckRes.out)
          }
          if (!slangCheckRes.ok) {
            reporter.error(None(), toolName, s"SlangCheck generation failed: ${slangCheckRes.err}")
          }
        } else {
          println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          println("SlangCheck disabled as model contains array types")
          println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        }
      }

      if (!reporter.hasError && !options.noProyekIve && isSlangProject) {
        // doesn't matter what 'o.writeOutResources' is, proyek ive needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, reporter)
          wroteOutArsitResources = T
        }

        if (!reporter.hasError) {
          val proyekConfig = ProyekIveConfig(
            help = "",
            args = ISZ(slangOutputDir.canon.value),
            force = F,
            edition = ProyekIveEdition.Community,
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

          reporter.info(None(), toolName, "Generating IVE project via Proyek IVE ...")
          reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

          if (proyekIveCallback(proyekConfig) != 0) {
            reporter.error(None(), toolName, "Proyek IVE did not complete successfully")
          }
        }
      }

      if (!reporter.hasError && options.runTranspiler && isTranspilerProject) {

        // doesn't matter what 'o.writeOutResources' is, transpiler needs the
        // resources to be written out
        if (!wroteOutArsitResources) {
          writeOutResources(arsitResources, reporter)
          wroteOutArsitResources = T
        }

        reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

        if (!reporter.hasError) {
          for (transpilerConfig <- results.transpilerOptions if !reporter.hasError) {
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

      val platform = org.sireum.hamr.act.util.ActPlatform.byName(options.platform.name).get
      reporter.info(None(), toolName, "Generating CAmkES artifacts...")

      val actOptions = org.sireum.hamr.act.util.ActOptions(
        outputDir = camkesOutputDir.value,
        auxFiles = getAuxFiles(options.camkesAuxCodeDirs, F, reporter),
        aadlRootDirectory = options.aadlRootDir,
        platform = platform,
        hamrBasePackageName = Some(packageName),
        experimentalOptions = options.experimentalOptions
      )

      val results = org.sireum.hamr.act.Act.run(rmodel, actOptions, aadlTypes, symbolTable, reporter)
      actResources = actResources ++ results.resources

      reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ(ACT_INSTRUCTIONS_MESSAGE_KIND))
    }

    actResources = removeDuplicates(actResources, reporter)

    if (!reporter.hasError && options.writeOutResources) {
      if (!wroteOutArsitResources) {
        writeOutResources(arsitResources, reporter)
        wroteOutArsitResources = T
      }
      writeOutResources(actResources, reporter)
    }

    reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

    if (!reporter.hasError && options.writeOutResources) {
      // always print out any instructional messages
      val instructions = reporter.messages.filter(p =>
        p.kind == ARSIT_INSTRUCTIONS_MESSAGE_KIND || p.kind == ACT_INSTRUCTIONS_MESSAGE_KIND)
      for (i <- instructions) {
        cprintln(F, "")
        cprintln(F, i.text)
      }
    }

    if (reporter.hasError && !options.verbose) { // at least need to print out the error messages
      printMessages(reporter.errors, T, 0, ISZ())
    }

    return CodeGenResults(arsitResources ++ actResources, transpilerConfigs)
  }

  def printMessages(reporterMessages: ISZ[Message], verbose: B, messageIndex: Z, kindsToFilterOut: ISZ[String]): Z = {

    if (verbose) {
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

      ReporterImpl(infoWarnings).printMessages()

      if (errors.nonEmpty) {
        cprintln(T, "")
        cprintln(T, "Errors:")
        ReporterImpl(errors).printMessages()
      }
    }
    return reporterMessages.size
  }

  def toOption(f: Path): Option[String] = {
    return Some(f.value)
  }

  def cleanupPackageName(s: String): String = {
    return ops.StringOps(ops.StringOps(s).replaceAllChars('-', '_')).replaceAllChars('.', '_')
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

  def writeOutResources(resources: IS[Z, Resource], reporter: Reporter): Unit = {
    def render(i: IResource): String = {
      val ret: String = {
        val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
        val replace: String = if (i.makeCRLF) "\r\n" else "\n"
        ops.StringOps(i.content.render).replaceAllLiterally(lineSep, replace)
      }
      return ret
    }

    for (r <- resources) {
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
              val replacements: ISZ[(Z, Z, String)] = oldSections.entries.map((oldEntry: (Marker, (Z, Z, String))) =>
                ((oldEntry._2._1, oldEntry._2._2, newSections.get(oldEntry._1).get._3)))
              val content: String = StringUtil.replaceSections(p.read, replacements, reporter)
              p.writeOver(content)
              reporter.info(None(), toolName, s"Wrote and preserved existing content: ${p}")
            }

          } else {
            reporter.info(None(), toolName, s"File exists, will not overwrite: ${p}")
          }
        case e: EResource =>
          if (e.symlink) {
            halt("sym linking not yet supported")
          } else {
            Os.path(e.srcPath).copyOverTo(p)
            reporter.info(None(), toolName, s"Copied: ${e.srcPath} to ${p}")
          }
      }
    }
  }

  def removeDuplicates(resources: ISZ[Resource], reporter: Reporter): ISZ[Resource] = {
    var m: HashSMap[String, Resource] = HashSMap.empty[String, Resource]
    for (r <- resources) {
      if (m.contains(r.dstPath)) {
        val entry = m.get(r.dstPath).get

        // sanity checks
        (r, entry) match {
          case ((ei: EResource, ci: EResource)) =>
            if (ei.srcPath != ci.srcPath) {
              reporter.warn(None(), toolName, s"srcPath for ${r.dstPath} not the same for duplicate entries")
            }
            if (ei.symlink != ci.symlink) {
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
    }
    return m.values
  }
}

