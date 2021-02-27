package org.sireum.hamr.codegen

import org.sireum.Os.Path
import org.sireum._
import org.sireum.hamr.act.util.Util.ACT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.arsit.Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND
import org.sireum.hamr.{act, arsit}
import org.sireum.hamr.codegen.CodeGenPlatform._
import org.sireum.hamr.codegen.common.DirectoryUtil
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.ops.StringOps

object CodeGen {

  val toolName: String = "Hamr CodeGen"

  def codeGen(model: Aadl,
              o: CodeGenConfig,
              reporter: Reporter,
              transpilerCallback: (TranspilerConfig) => Z): CodeGenResults = {

    val targetingSel4 = o.platform == CodeGenPlatform.SeL4

    val slangOutputDir: Path = Os.path(o.slangOutputDir.getOrElse("."))

    val output_shared_C_Directory: Path =
      if(o.slangOutputCDir.nonEmpty) Os.path(o.slangOutputCDir.get)
      else slangOutputDir / "src" / "c"

    val camkesOutputDir: Path =
      if(o.camkesOutputDir.nonEmpty) Os.path(o.camkesOutputDir.get)
      else output_shared_C_Directory / "camkes"

    val output_platform_C_Directory: Path =
      if (targetingSel4) camkesOutputDir / DirectoryUtil.DIR_SLANG_LIBRARIES
      else output_shared_C_Directory

    val packageName: String = if (o.packageName.nonEmpty) {
      cleanupPackageName(o.packageName.get)
    } else {
      cleanupPackageName(slangOutputDir.name)
    }

    val (runArsit, runACT, hamrIntegration, isTranspilerProject) = o.platform match {
      case JVM => (T, F, F, F)
      case Linux | Cygwin | MacOS => (T, F, F, T)
      case SeL4 => (T, T, T, T)
      case SeL4_Only | SeL4_TB => (F, T, F, F)
    }

    var reporterIndex = z"0"

    var transpilerConfigs: ISZ[TranspilerConfig] = ISZ()

    var arsitResources: ISZ[Resource] = ISZ()

    if (runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.util.IpcMechanism.byName(o.ipc.name).get
      val platform = arsit.util.ArsitPlatform.byName(o.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      val opt = arsit.util.ArsitOptions(
        outputDir = slangOutputDir,
        packageName = packageName,
        noEmbedArt = o.noEmbedArt,
        bless = genBlessEntryPoints,
        verbose = o.verbose,
        devicesAsThreads = o.devicesAsThreads,
        ipc = ipc,
        auxCodeDirs = o.slangAuxCodeDirs,
        outputSharedCDir = Some(output_shared_C_Directory),
        outputPlatformCDir = Some(output_platform_C_Directory),
        excludeImpl = o.excludeComponentImpl,
        platform = platform,
        bitWidth = o.bitWidth,
        maxStringSize = o.maxStringSize,
        maxArraySize = o.maxArraySize,
        pathSeparator = fileSep,
        experimentalOptions = o.experimentalOptions
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")
      reporterIndex = printMessages(reporter, reporterIndex, ISZ())

      val results = arsit.Arsit.run(model, opt, reporter)

      arsitResources = arsitResources ++ results.resources
      transpilerConfigs = transpilerConfigs ++ results.transpilerOptions

      reporterIndex = printMessages(reporter, reporterIndex, ISZ(ARSIT_INSTRUCTIONS_MESSAGE_KIND))

      arsitResources = removeDuplicates(arsitResources, reporter)

      if (!reporter.hasError && o.runTranspiler && isTranspilerProject) {

        // doesn't matter what 'o.writeOutResources' is, transpiler needs the
        // resources to be written out
        writeOutResources(arsitResources, reporter)

        reporterIndex = printMessages(reporter, reporterIndex, ISZ())

        for (transpilerConfig <- results.transpilerOptions) {
          if (transpilerCallback(transpilerConfig) != 0) {
            reporter.error(None(), toolName, s"Transpiler did not complete successfully")
          }
        }
      }
    }

    var actResources: ISZ[Resource] = ISZ()
    if (!reporter.hasError && runACT) {

      val platform = org.sireum.hamr.act.util.ActPlatform.byName(o.platform.name).get
      reporter.info(None(), toolName, "Generating CAmkES artifacts...")

      val actOptions = org.sireum.hamr.act.util.ActOptions(
        outputDir = camkesOutputDir.value,
        auxFiles = getAuxFiles(o.camkesAuxCodeDirs, F, reporter),
        aadlRootDirectory = o.aadlRootDir,
        platform = platform,
        hamrBasePackageName = Some(packageName),
        experimentalOptions = o.experimentalOptions
      )

      val results = org.sireum.hamr.act.Act.run(model, actOptions, reporter)
      actResources = actResources ++ results.resources

      reporterIndex = printMessages(reporter, reporterIndex, ISZ(ACT_INSTRUCTIONS_MESSAGE_KIND))
    }

    actResources = removeDuplicates(actResources, reporter)

    if(!reporter.hasError && o.writeOutResources) {
      writeOutResources(actResources, reporter)
    }

    reporterIndex = printMessages(reporter, reporterIndex, ISZ())

    if(!reporter.hasError && o.writeOutResources) {
      // print out any instructional messages
      val instructions = reporter.messages.filter(p =>
        p.kind == ARSIT_INSTRUCTIONS_MESSAGE_KIND || p.kind == ACT_INSTRUCTIONS_MESSAGE_KIND)
      for (i <- instructions) {
        cprintln(F, "")
        cprintln(F, i.text)
      }
    }

    return CodeGenResults(arsitResources ++ actResources, transpilerConfigs)
  }

  def printMessages(reporter: Reporter, reporterIndex: Z, kindsToFilterOut: ISZ[String]): Z = {

    var messages = ops.ISZOps(reporter.messages).slice(reporterIndex, reporter.messages.size)
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
      cprintln(err, mText)
    }

    return reporter.messages.size
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

