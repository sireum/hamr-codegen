package org.sireum.hamr.codegen

import org.sireum.Os.Path
import org.sireum._
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

    val camkesOutputDir: Path = Os.path(o.camkesOutputDir.getOrElse("."))
    val slangOutputDir: Path = Os.path(o.slangOutputDir.getOrElse("."))

    val outputSlang_C_Directory = if (targetingSel4) {
      camkesOutputDir / DirectoryUtil.DIR_SLANG_LIBRARIES
    } else {
      slangOutputDir / "src" / "c" / "nix"
    }

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

    var reporterIndex = 0

    var resources: ISZ[Resource] = ISZ()
    var transpilerConfigs: ISZ[TranspilerConfig] = ISZ()

    if (runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.util.IpcMechanism.byName(o.ipc.name).get
      val platform = arsit.util.ArsitPlatform.byName(o.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      val opt = arsit.util.ArsitOptions(
        outputDir = slangOutputDir.value,
        packageName = packageName,
        embedArt = o.embedArt,
        bless = genBlessEntryPoints,
        verbose = o.verbose,
        devicesAsThreads = o.devicesAsThreads,
        ipc = ipc,
        auxCodeDir = o.slangAuxCodeDirs,
        outputCDir = toOption(outputSlang_C_Directory),
        excludeImpl = o.excludeComponentImpl,
        platform = platform,
        bitWidth = o.bitWidth,
        maxStringSize = o.maxStringSize,
        maxArraySize = o.maxArraySize,
        pathSeparator = fileSep,
        experimentalOptions = o.experimentalOptions
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")

      val results = arsit.Arsit.run(model, opt, reporter)

      resources = resources ++ results.resources
      transpilerConfigs = transpilerConfigs ++ results.transpilerOptions

      reporterIndex = printMessages(reporter.messages, reporterIndex)

      if (!reporter.hasError && o.runTranspiler && isTranspilerProject) {

        for (transpilerConfig <- results.transpilerOptions) {
          if (transpilerCallback(transpilerConfig) != 0) {
            reporter.error(None(), toolName, s"Transpiler did not complete successfully")
          }
        }
      }
    }

    if (!reporter.hasError && runACT) {

      val platform = org.sireum.hamr.act.ActPlatform.byName(o.platform.name).get
      reporter.info(None(), toolName, "Generating CAmkES artifacts...")

      val actOptions = act.ActOptions(
        outputDir = camkesOutputDir.value,
        auxFiles = getAuxFiles(o.camkesAuxCodeDirs, F, reporter),
        aadlRootDirectory = o.aadlRootDir,
        platform = platform,
        hamrBasePackageName = Some(packageName),
        experimentalOptions = o.experimentalOptions
      )

      val results = org.sireum.hamr.act.Act.run(model, actOptions, reporter)
      resources = resources ++ results.resources

      reporterIndex = printMessages(reporter.messages, reporterIndex)
    }

    if(!reporter.hasError && o.writeOutResources) {
      writeOutResources(resources, reporter)
    }

    reporterIndex = printMessages(reporter.messages, reporterIndex)

    return CodeGenResults(resources, transpilerConfigs)
  }

  def printMessages(messages: ISZ[Message], index: Int): Int = {
    for (i <- index until messages.size) {
      val m = messages(i)
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
    return messages.size.toInt
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
}

