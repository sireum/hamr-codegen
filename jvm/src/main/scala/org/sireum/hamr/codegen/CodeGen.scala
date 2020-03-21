package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.message._
import org.sireum.ops.StringOps
import org.sireum.Os.Path
import org.sireum.hamr.act
import org.sireum.hamr.arsit
import org.sireum.hamr.ir.Aadl
import org.sireum.hamr.codegen.CodeGenPlatform._

object CodeGen {

  val toolName: String = "Hamr CodeGen"

  def codeGen(model: Aadl,
              o: CodeGenConfig,
              reporter: Reporter,
              transpilerCallback: (TranspilerConfig) => Z): CodeGenResults = {

    val targetingSel4 = o.platform == CodeGenPlatform.SeL4

    val camkesOutputDir: Path = Os.path(o.camkesOutputDir.getOrElse("."))
    val slangOutputDir: Path = Os.path(o.slangOutputDir.getOrElse("."))

    val outputSlang_C_Directory = if(targetingSel4) {
      camkesOutputDir / "hamr"
    }  else {
      slangOutputDir / "src" / "c" / "nix"
    }

    val packageName: String = if(o.packageName.nonEmpty) {
      cleanupPackageName(o.packageName.get)
    } else {
      cleanupPackageName(slangOutputDir.name)
    }

    val (runArsit, runACT, hamrIntegration, shouldCallTranspile) = o.platform match {
      case JVM => (T, F, F, F)
      case Linux | Cygwin | MacOS => (T, F, F, T)
      case SeL4 => (T, T, T, T)
      case SeL4_Only | SeL4_TB => (F, T, F, F)
    }

    var reporterIndex = 0
    
    var resources: ISZ[Resource] = ISZ()
    var transpilerConfig: Option[TranspilerConfig] = None()
    
    if(runArsit) {

      val genBlessEntryPoints = false
      val ipc = arsit.Cli.IpcMechanism.byName(o.ipc.name).get
      val platform = arsit.Cli.ArsitPlatform.byName(o.platform.name).get
      val fileSep = StringOps(org.sireum.Os.fileSep).first

      val opt = arsit.Cli.ArsitOption(
        slangOutputDir.value,
        packageName,
        o.embedArt,
        genBlessEntryPoints,
        o.verbose,
        o.devicesAsThreads,
        ipc,
        o.slangAuxCodeDirs,
        toOption(outputSlang_C_Directory),
        o.excludeComponentImpl,
        platform,
        o.bitWidth,
        o.maxStringSize,
        o.maxArraySize,
        fileSep
      )

      reporter.info(None(), toolName, "Generating Slang artifacts...")

      val results = arsit.Arsit.run(model, opt, reporter)

      reporterIndex = printMessages(reporter.messages, reporterIndex)
      
      if(!reporter.hasError){
        val arsitResources = results.resources.map(r => Resource(r.path, r.content, r.overwrite, r.makeExecutable))
        resources = resources ++ arsitResources

        if(o.writeOutResources) {
          writeOutResources(arsitResources, reporter)
        }
      }

      reporterIndex = printMessages(reporter.messages, reporterIndex)
      
      if(!reporter.hasError && shouldCallTranspile) {
        
        for(t <- results.transpilerOptions) {
          val transpilerConfig = TranspilerConfig(
            t.sourcepath,
            t.output,
            t.verbose,
            t.projectName,
            t.apps,
            t.unroll,
            t.fingerprint,
            t.bitWidth,
            t.maxStringSize,
            t.maxArraySize,
            t.customArraySizes,
            t.customConstants,
            t.plugins,
            t.exts,
            t.forwarding,
            t.stackSize,
            t.excludeBuild,
            t.libOnly,
            t.stableTypeId,
            t.save,
            t.load
          )

          if (transpilerCallback(transpilerConfig) != 0) {
            reporter.error(None(), toolName, s"Transpiler did not complete successfully")
          }
        }
      }
    }

    if(!reporter.hasError && runACT) {

      var hamrLibs: Map[String, act.HamrLib] = Map.empty
      if(hamrIntegration) {
        val root: Os.Path = outputSlang_C_Directory.canon
        for(instanceDir <- root.list){
          assert(instanceDir.isDir)
          val instanceName = instanceDir.name
          
          val hamrDirsWithHeaders: Set[String] = Set.empty ++ getHeaderFiles(instanceDir)
            .map(m => camkesOutputDir.relativize(m.up).value)

          val instanceLib = instanceDir / "sel4-build" / "libmain.a"
          val path = camkesOutputDir.relativize(instanceLib)
          val staticLib: String = s"$${CMAKE_CURRENT_LIST_DIR}/${path.value}"
          
          val hamrLib = act.HamrLib(instanceName, hamrDirsWithHeaders.elements, staticLib)
          hamrLibs = hamrLibs + (instanceName ~> hamrLib)
        }
      } 
      
      
      val platform = org.sireum.hamr.act.ActPlatform.byName(o.platform.name).get
      reporter.info(None(), toolName, "Generating CAmkES artifacts...")

      val actOptions = act.ActOptions(
        outputDir = camkesOutputDir.value,
        auxFiles = getAuxFiles(o.camkesAuxCodeDirs, F, reporter),
        aadlRootDirectory = o.aadlRootDir,
        platform = platform,
        hamrLibs = hamrLibs,
        Some(packageName)
      )
      
      val results = org.sireum.hamr.act.Act.run(model, actOptions, reporter)

      reporterIndex = printMessages(reporter.messages, reporterIndex)
      
      if(!reporter.hasError) {
        val actResources = results.resources.map(r => Resource(r.path, r.content, r.overwrite, r.makeExecutable))
        resources = resources ++ actResources
        
        if(o.writeOutResources) {
          writeOutResources(actResources, reporter)
        }
      }

      reporterIndex = printMessages(reporter.messages, reporterIndex)
    }

    return CodeGenResults(resources, transpilerConfig)
  }

  def printMessages(messages: ISZ[Message], index: Int): Int = {
    for(i <- index until messages.size) {
      val m = messages(i)
      val t = if(m.level != org.sireum.message.Level.Info) s"${m.level.name}: " else ""
      val err = m.level == org.sireum.message.Level.Error
      val mText: String = m.posOpt match {
        case Some(pos) =>
          val uri: String = if(pos.uriOpt.nonEmpty) s" ${pos.uriOpt.get}" else "" 
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
      
      dir.list.foreach(d => if(d.isDir) processDir(d))
    }
    
    processDir(root)
    return ret
  }
  
  def getAuxFiles(directories: ISZ[String], includeParentDir: B, reporter: Reporter): Map[String, String] = {
    var ret: Map[String, String] = Map.empty
    
    for (rootDir <- directories) {
      val rootDirPath = Os.path(rootDir)
      if(!rootDirPath.exists) {
        reporter.warn(None(), toolName, s"Directory '${rootDirPath}' does not exist")
      }
      
      def processDir(dir: Os.Path): Unit = {
        dir.list.filter(p => p.ext == string"c" || p.ext == string"h").foreach((f : Os.Path) => {
          // make subdir paths relative to the root dir
         val root = if(includeParentDir) rootDirPath.up else rootDirPath
          val rel = root.relativize(f).value

          ret = ret + (rel ~> f.read)
        })

        dir.list.foreach(f => if(f.isDir) processDir(f))
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
        if(r.makeExecutable) {
          p.chmod("700")
          reporter.info(None(), toolName, s"Made ${p} executable")
        }
      } else {
        reporter.info(None(), toolName, s"File exists, will not overwrite: ${p}")
      }
    }
  }
}

