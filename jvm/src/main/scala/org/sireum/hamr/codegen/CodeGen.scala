package org.sireum.hamr.codegen

import org.sireum.Os.Path
import org.sireum._
import org.sireum.hamr.act.util.Util.ACT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.arsit.Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.{act, arsit}
import org.sireum.hamr.codegen.common.util.CodeGenPlatform._
import org.sireum.hamr.codegen.common.DirectoryUtil
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.ModelUtil.ModelElements
import org.sireum.hamr.codegen.common.util.{CodeGenConfig, CodeGenPlatform, CodeGenResults, ModelUtil}
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.ops.StringOps

object CodeGen {

  val toolName: String = "HAMR CodeGen"

  def codeGen(model: Aadl,
              options: CodeGenConfig,
              reporter: Reporter,
              transpilerCallback: (TranspilerConfig) => Z): CodeGenResults = {

    val targetingSel4 = options.platform == CodeGenPlatform.SeL4

    val slangOutputDir: Path = Os.path(options.slangOutputDir.getOrElse("."))

    val output_shared_C_Directory: Path =
      if(options.slangOutputCDir.nonEmpty) Os.path(options.slangOutputCDir.get)
      else slangOutputDir / "src" / "c"

    val camkesOutputDir: Path =
      if(options.camkesOutputDir.nonEmpty) Os.path(options.camkesOutputDir.get)
      else output_shared_C_Directory / "camkes"

    val output_platform_C_Directory: Path =
      if (targetingSel4) camkesOutputDir / DirectoryUtil.DIR_SLANG_LIBRARIES
      else output_shared_C_Directory

    val packageName: String = if (options.packageName.nonEmpty) {
      cleanupPackageName(options.packageName.get)
    } else {
      cleanupPackageName(slangOutputDir.name)
    }

    val (runArsit, runACT, hamrIntegration, isTranspilerProject) = options.platform match {
      case JVM => (T, F, F, F)
      case Linux | Cygwin | MacOS => (T, F, F, T)
      case SeL4 => (T, T, T, T)
      case SeL4_Only | SeL4_TB => (F, T, F, F)
    }

    var reporterIndex = z"0"

    var transpilerConfigs: ISZ[TranspilerConfig] = ISZ()

    var arsitResources: ISZ[Resource] = ISZ()

    var wroteOutArsitResources: B = F

    val result: Option[ModelElements] = ModelUtil.resolve(model, packageName, options, reporter)
    reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())
    if(result.isEmpty) {
      return CodeGenResults(ISZ(), ISZ())
    }

    val (rmodel, aadlTypes, symbolTable) : (Aadl, AadlTypes, SymbolTable) = (result.get.model, result.get.types, result.get.symbolTable)
    if (!reporter.hasError && runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.util.IpcMechanism.byName(options.ipc.name).get
      val platform = arsit.util.ArsitPlatform.byName(options.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      val opt = arsit.util.ArsitOptions(
        outputDir = slangOutputDir,
        packageName = packageName,
        noEmbedArt = options.noEmbedArt,
        bless = genBlessEntryPoints,
        verbose = options.verbose,
        devicesAsThreads = options.devicesAsThreads,
        ipc = ipc,
        auxCodeDirs = options.slangAuxCodeDirs,
        outputSharedCDir = Some(output_shared_C_Directory),
        outputPlatformCDir = Some(output_platform_C_Directory),
        excludeImpl = options.excludeComponentImpl,
        platform = platform,
        bitWidth = options.bitWidth,
        maxStringSize = options.maxStringSize,
        maxArraySize = options.maxArraySize,
        pathSeparator = fileSep,
        experimentalOptions = options.experimentalOptions
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")
      reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

      val results = arsit.Arsit.run(rmodel, opt, aadlTypes, symbolTable, reporter)

      arsitResources = arsitResources ++ results.resources
      transpilerConfigs = transpilerConfigs ++ results.transpilerOptions

      reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ(ARSIT_INSTRUCTIONS_MESSAGE_KIND))

      arsitResources = removeDuplicates(arsitResources, reporter)

      if (!reporter.hasError && options.runTranspiler && isTranspilerProject) {

        // doesn't matter what 'o.writeOutResources' is, transpiler needs the
        // resources to be written out
        writeOutResources(arsitResources, reporter)
        wroteOutArsitResources = T

        reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

        for (transpilerConfig <- results.transpilerOptions) {
          if (transpilerCallback(transpilerConfig) != 0) {
            reporter.error(None(), toolName, s"Transpiler did not complete successfully")
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

    if(!reporter.hasError && options.writeOutResources) {
      if(!wroteOutArsitResources) {
        writeOutResources(arsitResources, reporter)
      }
      writeOutResources(actResources, reporter)
    }

    reporterIndex = printMessages(reporter.messages, options.verbose, reporterIndex, ISZ())

    if(!reporter.hasError && options.writeOutResources) {
      // always print out any instructional messages
      val instructions = reporter.messages.filter(p =>
        p.kind == ARSIT_INSTRUCTIONS_MESSAGE_KIND || p.kind == ACT_INSTRUCTIONS_MESSAGE_KIND)
      for (i <- instructions) {
        cprintln(F, "")
        cprintln(F, i.text)
      }
    }

    if(reporter.hasError && !options.verbose) { // at least need to print out the error messages
      printMessages(reporter.errors, T, 0, ISZ())
    }

    return CodeGenResults(arsitResources ++ actResources, transpilerConfigs)
  }

  def printMessages(reporterMessages: ISZ[Message], verbose: B, messageIndex: Z, kindsToFilterOut: ISZ[String]): Z = {

    var messages = ops.ISZOps(reporterMessages).slice(messageIndex, reporterMessages.size)
    for(key <- kindsToFilterOut) {
      messages = messages.filter(p => p.kind != key)
    }

    for (m <- messages) {
      val t = if (m.level != org.sireum.message.Level.Info) s"${m.level.name}: " else ""
      val err = m.level == org.sireum.message.Level.Error
      val mText: String = m.posOpt match {
        case Some(pos) =>
          val uri: String = if (pos.uriOpt.nonEmpty) s" ${pos.uriOpt.get}" else ""
          s"${m.kind} - ${t}[${pos.beginLine}, ${pos.beginColumn}] ${m.text}. ${uri}"
        case _ => s"${m.kind} - ${t}${m.text}"
      }
      if(verbose) {
        cprintln(err, mText)
      }
    }

    return reporterMessages.size
  }

  def toOption(f: Path): Option[String] = {
    return Some(f.value)
  }

  def cleanupPackageName(s: String): String = {
    return s.native.replaceAll("[\\-|\\.]", "_")
  }

  def getHeaderFiles(root: Os.Path): ISZ[Os.Path] = {
    assert(root.isDir)

    var ret: ISZ[Os.Path] = ISZ()

    def processDir(dir: Os.Path): Unit = {
      ret = ret ++ dir.list.filter(p => p.ext == string"h")

      dir.list.foreach(d => if (d.isDir) processDir(d))
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
        dir.list.filter(p => p.ext == string"c" || p.ext == string"h").foreach((f: Os.Path) => {
          // make subdir paths relative to the root dir
          val root = if (includeParentDir) rootDirPath.up else rootDirPath
          val rel = root.relativize(f).value

          ret = ret + (rel ~> f.read)
        })

        dir.list.foreach(f => if (f.isDir) processDir(f))
      }

      if (rootDirPath.isDir) processDir(rootDirPath)
    }

    return ret
  }

  def writeOutResources(resources: IS[Z, Resource], reporter: Reporter) = {
    for (r <- resources) {
      val _p = Os.path(r.path)
      val p = _p.canon
      assert(!p.exists || p.isFile)
      p.up.mkdirAll()
      if (r.overwrite || !p.exists) {
        p.writeOver(r.content.render)
        reporter.info(None(), toolName, s"Wrote: ${p}")
        if (r.makeExecutable) {
          p.chmodAll("700")
          reporter.info(None(), toolName, s"Made ${p} executable")
        }
      } else {
        reporter.info(None(), toolName, s"File exists, will not overwrite: ${p}")
      }
    }
  }

  def removeDuplicates(resources: ISZ[Resource], reporter: Reporter): ISZ[Resource] = {
    var m: HashSMap[String, Resource] = HashSMap.empty[String, Resource]()
    for(r <- resources) {
      if(m.contains(r.path)) {
        val entry = m.get(r.path).get

        // sanity checks
        if(r.content.render != entry.content.render) {
          reporter.warn(None(), toolName, s"content of ${r.path} not the same for duplicate entries")
        }
        if(r.overwrite != entry.overwrite) {
          reporter.warn(None(), toolName, s"overwrite flag for ${r.path} not the same for duplicate entries")
        }
        if(r.makeExecutable != entry.makeExecutable) {
          reporter.warn(None(), toolName, s"makeExecutable flag for ${r.path} not the same for duplicate entries")
        }
      } else {
        m = m + (r.path ~> r)
      }
    }
    return m.values
  }
}

