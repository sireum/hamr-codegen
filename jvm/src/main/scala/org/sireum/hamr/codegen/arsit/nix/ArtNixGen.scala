// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit.Util.nameProvider
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{SchedulerTemplate, SeL4NixTemplate, TranspilerTemplate}
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.arsit.util.{ArsitOptions, IpcMechanism}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{FileResource, Resource, SireumSlangTranspilersCOption}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.TemplateUtil
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.Direction

@record class ArtNixGen(val dirs: ProjectDirectories,
                        val root: AadlSystem,
                        val arsitOptions: ArsitOptions,
                        val symbolTable: SymbolTable,
                        val types: AadlTypes,
                        val previousPhase: Result
                       ) extends NixGen {

  val basePackage: String = arsitOptions.packageName

  var resources: ISZ[FileResource] = ISZ()

  var transpilerOptions: ISZ[SireumSlangTranspilersCOption] = ISZ()

  var maxPortsForComponents: Z = 0
  var numConnections: Z = 0
  var maxStackSize: Z = -1

  var portId: Z = previousPhase.maxPort

  def getPortId(): Z = {
    val r = portId
    portId = portId + 1
    return r
  }

  def generate(): ArsitResult = {
    assert(Util.isNix(arsitOptions.platform))

    gen()

    return ArsitResult(
      resources = previousPhase.resources() ++ resources,
      auxResources = previousPhase.auxResources ++ transpilerOptions.asInstanceOf[ISZ[Resource]],
      maxPort = portId,
      maxComponent = previousPhase.maxComponent,
      maxConnection = previousPhase.maxConnection)
  }

  def addExeResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createExeResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def addResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def gen(): Unit = {

    var platformPorts: ISZ[ST] = ISZ()
    var mainSends: ISZ[ST] = ISZ()
    var inPorts: ISZ[Port] = ISZ()
    var appNames: ISZ[String] = ISZ()
    var ext_h_entries: ISZ[ST] = ISZ()
    var ext_c_entries: ISZ[ST] = ISZ()

    val components = symbolTable.componentMap.values.filter(p =>
      p.isInstanceOf[AadlThread] || (p.isInstanceOf[AadlDevice] && arsitOptions.devicesAsThreads))
      .map((m: AadlComponent) => m.asInstanceOf[AadlThreadOrDevice])

    for (threadOrDevice <- components) {
      val component = threadOrDevice.component

      val names = nameProvider(component, basePackage)
      val ports: ISZ[Port] = Util.getPorts(threadOrDevice, types, basePackage, z"0")

      val dispatchProtocol: Dispatch_Protocol.Type = threadOrDevice.dispatchProtocol

      val isPeriodic: B = dispatchProtocol == Dispatch_Protocol.Periodic

      val bridgeInstanceVarName: String = CommonUtil.getName(component.identifier)
      val App_Id: String = s"${names.componentSingletonType}_App"

      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portIdOpts: ISZ[ST] = ISZ()

      var portOptResets: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      PropertyUtil.getStackSizeInBytes(component) match {
        case Some(bytes) => if (bytes > maxStackSize) {
          maxStackSize = bytes
        }
        case _ =>
      }

      val aadlPorts: ISZ[AadlPort] = threadOrDevice.getPorts()
      if (aadlPorts.size > maxPortsForComponents) {
        maxPortsForComponents = aadlPorts.size
      }

      val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)

      for (p <- aadlPorts if CommonUtil.isInFeature(p.feature)) {

        val portName = p.identifier
        val isTrigger: B =
          if (dispatchTriggers.isEmpty) T
          else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty

        val port = Util.getPort(p, p.feature, component, types, basePackage, isTrigger, z"-1000")

        val portIdName: String = s"${port.name}PortId"
        val portOptName: String = s"${port.name}Opt"
        val portType: String = port.getPortTypeNames.qualifiedReferencedTypeName
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${port.name}"

        portDefs = portDefs :+ ArtNixTemplate.portDef(portOptName, portType)
        portOpts = portOpts :+ ArtNixTemplate.portOpt("", portOptName, portType, T)

        portIds = portIds :+ ArtNixTemplate.portId(portIdName, archPortInstanceName)
        portIdOpts = portIdOpts :+ ArtNixTemplate.portOpt(portIdName, portOptName, "Art.PortId", F)

        portOptResets = portOptResets :+ ArtNixTemplate.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ ArtNixTemplate.appCases(portOptName, portIdName, port.getPortTypeNames)

        inPorts = inPorts :+ port
      }

      platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl(App_Id, getPortId())
      mainSends = mainSends :+ ArtNixTemplate.mainSend(App_Id)
      appNames = appNames :+ App_Id

      val transpilerToucher = SeL4NixTemplate.transpilerToucher(basePackage)

      addResource(
        dirs.componentDir,
        ISZ(basePackage, s"${SeL4NixTemplate.TRANSPILER_TOUCHER_OBJECT_NAME}.scala"),
        transpilerToucher,
        F) // don't overwrite since user may add contents to this file

      val apiTouches = SeL4NixTemplate.apiTouches(names, ports)
      val touchMethod = SeL4NixTemplate.genTouchMethod(NixGen.genTypeTouches(types), apiTouches, ISZ())

      val stApp = ArtNixTemplate.app(
        packageName = basePackage,
        objectName = App_Id,
        IPCPort_Id = App_Id,
        period = CommonUtil.getPeriod(threadOrDevice),
        bridge = bridgeInstanceVarName,
        component = threadOrDevice,
        isPeriodic = isPeriodic,
        types = types,
        touchMethod = touchMethod,
        basePackage = basePackage
      )

      addResource(dirs.slangNixDir, ISZ(basePackage, s"${App_Id}.scala"), stApp, T)

      // don't care about paths since the root directory containing the 'ext-c'
      // dir will be passed to the transpiler rather than the individual resources
      val (paths, extResources) = genExtensionFiles(threadOrDevice, names, ports)

      resources = resources ++ extResources
    }

    val (_ext_h_entries, _ext_c_entries) = genExtensionEntries(basePackage, components)
    ext_h_entries = ext_h_entries ++ _ext_h_entries
    ext_c_entries = ext_c_entries ++ _ext_c_entries

    val archBridgeInstanceNames: ISZ[String] = components.map((c: AadlThreadOrDevice) =>
      nameProvider(c.component, basePackage).cArchInstanceName)
    resources = resources ++ genSchedulerFiles(basePackage, archBridgeInstanceNames)

    {
      val extC = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_C
      val extH = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_H

      val uc = TemplateUtil.uniqueSTs(ext_c_entries)
      val uh = TemplateUtil.uniqueSTs(ext_h_entries)

      addResource(extC.up.value, ISZ(extC.name), SeL4NixTemplate.ext_c(uc), F)
      addResource(extH.up.value, ISZ(extH.name), SeL4NixTemplate.ext_h(uh), F)
    }

    var artNixCasesM: HashSMap[String, ISZ[ST]] = HashSMap.empty
    for (c <- symbolTable.connections) {
      val dstComp = symbolTable.airComponentMap.get(c.dst.component.name).get
      val srcComp = symbolTable.airComponentMap.get(c.src.component.name).get

      if ((CommonUtil.isDevice(srcComp) || CommonUtil.isThread(srcComp)) &&
        (CommonUtil.isDevice(dstComp) || CommonUtil.isThread(dstComp))) {
        val dstPath = CommonUtil.getName(c.dst.feature.get)
        val dstArchPortInstanceName = s"${CommonUtil.getName(dstComp.identifier)}.${CommonUtil.getLastName(c.dst.feature.get)}"
        val name = nameProvider(dstComp, basePackage)

        val srcArchPortInstanceName =
          s"${CommonUtil.getName(srcComp.identifier)}.${CommonUtil.getLastName(c.src.feature.get)}"

        if (ops.ISZOps(inPorts.map((m: Port) => m.path)).contains(dstPath) &&
          (CommonUtil.isThread(srcComp) || CommonUtil.isDevice(srcComp)) && (CommonUtil.isThread(dstComp) || CommonUtil.isDevice(dstComp))) {
          val dstCompApp = s"${name.componentSingletonType}_App"
          var cases: ISZ[ST] = {
            if (artNixCasesM.contains(srcArchPortInstanceName)) {
              artNixCasesM.get(srcArchPortInstanceName).get
            } else {
              ISZ()
            }
          }
          cases = cases :+ ArtNixTemplate.artNixCase(dstCompApp, dstArchPortInstanceName)
          artNixCasesM = artNixCasesM + (srcArchPortInstanceName ~> cases)
        }
        numConnections = numConnections + 1
      } else {
        reporter.info(None(), Util.toolName, s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${CommonUtil.getName(c.name)}")
      }
    }

    platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case IpcMechanism.SharedMemory =>
        addResource(dirs.slangNixDir, ISZ(basePackage, "SharedMemory.scala"), ArtNixTemplate.SharedMemory(basePackage), T)
        addResource(dirs.slangNixDir, ISZ(basePackage, "SharedMemory_Ext.scala"), ArtNixTemplate.SharedMemory_Ext(basePackage), T)
      case x => halt(s"Unexpected IPC ${x}")
    }

    val stIPC = ArtNixTemplate.ipc(basePackage, platformPorts)
    addResource(dirs.slangNixDir, ISZ(basePackage, "IPC.scala"), stIPC, T)

    val artNixCases: ISZ[ST] = artNixCasesM.entries.map(k => ArtNixTemplate.artNixCases(k._1, k._2))
    val stArtNix = ArtNixTemplate.artNix(
      basePackage,
      artNixCases,
      inPorts.filter(p => CommonUtil.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id")
    )

    addResource(dirs.slangNixDir, ISZ(basePackage, "ArtNix.scala"), stArtNix, T)

    val stMain = ArtNixTemplate.main(basePackage, mainSends)
    addResource(dirs.slangNixDir, ISZ(basePackage, "LegacyDemo.scala"), stMain, T)

    addResource(dirs.slangNixDir, ISZ(basePackage, s"${ArtNixTemplate.PlatformComm}.scala"), ArtNixTemplate.platformComm(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, s"${ArtNixTemplate.PlatformComm}_Ext.scala"), ArtNixTemplate.PlatformCommExt(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, s"${ArtNixTemplate.PlatformComm}Nix.scala"), ArtNixTemplate.PlatformCommNix(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Process.scala"), ArtNixTemplate.Process(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Process_Ext.scala"), ArtNixTemplate.ProcessExt(basePackage), T)

    resources = resources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(dirs.cBinDir.value, ISZ("compile.cmd")), ArtNixTemplate.compileSlash(dirs), T)

    addExeResource(dirs.cBinDir.value, ISZ(s"run.sh"), ArtNixTemplate.runS(appNames), T)

    //for(plat <- ISZ(ArsitPlatform.Linux, ArsitPlatform.MacOS, ArsitPlatform.Cygwin)) {
    //  val platName = ops.StringOps(plat.name).firstToLower
    //  addExeResource(dirs.cBinDir.value, ISZ(s"run-${platName}.sh"), ArtNixTemplate.run(appNames, plat), T)
    //}

    addExeResource(dirs.cBinDir.value, ISZ("stop.sh"), ArtNixTemplate.stop(appNames), T)

    addResource(dirs.cEtcDir, ISZ(NixGen.IPC_C), Util.getIpc(arsitOptions.ipc, basePackage), T)

    var outputPaths: ISZ[String] = ISZ(dirs.mainDir)
    if (!org.sireum.ops.StringOps(dirs.slangNixDir).contains(dirs.mainDir)) {
      outputPaths = outputPaths :+ dirs.slangNixDir
    }

    val excludes: ISZ[String] = if (arsitOptions.excludeImpl) {
      components.map((m: AadlThreadOrDevice) => {
        val name = nameProvider(m.component, basePackage)
        s"${name.packageName}.${name.componentSingletonType}"
      })
    } else {
      ISZ()
    }

    val buildApps: B = T

    val numPorts: Z = portId
    val numComponents: Z = previousPhase.maxComponent

    val (t, s, z, hasSporadic) = ArtNixGen.V().v(symbolTable.rootSystem)

    var customSequenceSizes = ISZ[(String, String)](
      (s"MS[Z,Option[art.Bridge]]=$numComponents", "Needed for Art.bridges"),
      ("IS[Z,String]=3", "Needed for the CLI arguments to the Demo Slang app"),
      (s"IS[Z,art.Art.PortId]=$z", "Needed for the sending and receiving of messages in ART and the bridges"),
      (s"IS[Z,art.UPort]=$z", s"Needed for ${t.identifier}'s ${s} ports")
    )

    genBitArraySequenceSizes() match {
      case Some((z, typeName)) => customSequenceSizes = customSequenceSizes :+ ((s"IS[Z,B]=$z", s"Needed for the max bit size specified in the model -- see $typeName"))
      case _ =>
    }

    val customConstants: ISZ[String] = ISZ(
      s"art.Art.numComponents=${numComponents}",
      s"art.Art.numPorts=${numPorts}",
      s"art.Art.numConnections=${numConnections}"
    )

    val _legacyextensions: ISZ[String] = ISZ(dirs.cExt_c_Dir, dirs.cEtcDir) ++ arsitOptions.auxCodeDirs

    val legacyTranspiler: (ST, SireumSlangTranspilersCOption) = TranspilerTemplate.transpiler(
      verbose = F,
      libraryName = "main",
      sourcepaths = ISZ(dirs.mainDir),
      outputDir = Os.path(dirs.cNixDir),
      binDir = dirs.slangBinDir,
      apps = (appNames :+ "LegacyDemo").map(s => s"${basePackage}.${s}"),
      forwards = ISZ(s"art.ArtNative=${basePackage}.ArtNix", s"${basePackage}.${ArtNixTemplate.PlatformComm}=${basePackage}.${ArtNixTemplate.PlatformComm}Nix"),
      numBits = arsitOptions.bitWidth,
      maxArraySize = arsitOptions.maxArraySize,
      maxStringSize = arsitOptions.maxStringSize,
      customArraySizes = customSequenceSizes.map(m => m._1),
      customConstants = customConstants,
      stackSizeInBytes = maxStackSize,
      extensions = _legacyextensions,
      excludes = excludes,
      buildApps = buildApps,
      cmakeIncludes = ISZ()
    )

    if (hasSporadic) {
      // sporadic threads call ArtNative.dispatchStatus to get any received incoming events.
      // These will be sorted using ISZOps when ArtNativeSlang is used.  ISZOps internally uses
      // an MSZ[Uport]
      customSequenceSizes = customSequenceSizes :+
        ((s"MS[Z,art.UPort]=$z", "Needed for the ops.ISZOps(sorted).tail call used by ArtNativeSlang"))
    }

    customSequenceSizes = customSequenceSizes ++ ISZ(
      (s"IS[Z,(Z,art.ArtSlangMessage)]=$numPorts", "Needed for the backing store of Map[Z, ArgSlangMessage] in ArtNativeSlang"),
      (s"IS[Z,art.Art.BridgeId]=$numComponents", "Needed for Schedulers.roundRobinSchedule"),
      (s"IS[Z,art.scheduling.static.Schedule.Slot]=$numComponents", "Needed for Schedulers.staticSchedule"),
      (s"IS[Z,(String,art.Art.BridgeId)]=$numComponents", "Needed for the backing store of Scheduler.threadNickNames")
    )

    val custSeqSizeComment =
      st"""// Origin of custom sequence sizes
          |${(customSequenceSizes.map((m: (String, String)) => st"//   ${m._1} - ${m._2}"), "\n")}"""

    // don't have arsit automatically run transpiler on legacy scheduler as
    // legacy and slang schedulers projects are saved to the same nix directory
    // so there can be only one
    //transpilerOptions = transpilerOptions :+ legacyTranspiler._2

    val _extensions: ISZ[String] = ISZ(dirs.cExt_schedule_Dir, dirs.cExt_c_Dir, dirs.cEtcDir) ++ arsitOptions.auxCodeDirs

    val transpiler: (ST, SireumSlangTranspilersCOption) = TranspilerTemplate.transpiler(
      verbose = F,
      libraryName = "main",
      sourcepaths = ISZ(dirs.mainDir),
      outputDir = Os.path(dirs.cNixDir),
      binDir = dirs.slangBinDir,
      apps = ISZ(s"${basePackage}.Demo"),
      forwards = ISZ(s"art.ArtNative=art.ArtNativeSlang"),
      numBits = arsitOptions.bitWidth,
      maxArraySize = arsitOptions.maxArraySize,
      maxStringSize = arsitOptions.maxStringSize,
      customArraySizes = customSequenceSizes.map(m => m._1),
      customConstants = customConstants,
      stackSizeInBytes = maxStackSize,
      extensions = _extensions,
      excludes = excludes,
      buildApps = buildApps,
      cmakeIncludes = ISZ()
    )

    transpilerOptions = transpilerOptions :+ transpiler._2

    val comments = ISZ(st"// If you want to make changes to this script, make a copy of it and edit that version", custSeqSizeComment)

    val slashTranspileScript = TranspilerTemplate.transpilerSlashScriptPreamble(legacyTranspiler._1, transpiler._1, comments)
    resources = resources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(dirs.slangBinDir, ISZ("transpile.cmd")), slashTranspileScript, T)
  }

  def genSchedulerFiles(packageName: String, archBridgeInstanceNames: ISZ[String]): ISZ[FileResource] = {
    val roundRobinFile = "round_robin.c"
    val staticSchedulerFile = "static_scheduler.c"
    val ret: ISZ[FileResource] =
      ISZ(ResourceUtil.createResource(Util.pathAppend(dirs.cExt_schedule_Dir, ISZ("legacy.c")), SchedulerTemplate.c_legacy(), F),
        ResourceUtil.createResource(Util.pathAppend(dirs.cExt_schedule_Dir, ISZ(roundRobinFile)), SchedulerTemplate.c_roundRobin(packageName, archBridgeInstanceNames, roundRobinFile), F),
        ResourceUtil.createResource(Util.pathAppend(dirs.cExt_schedule_Dir, ISZ(staticSchedulerFile)), SchedulerTemplate.c_static_schedule(packageName, archBridgeInstanceNames, staticSchedulerFile), F),
        ResourceUtil.createResource(Util.pathAppend(dirs.cExt_schedule_Dir, ISZ("process.c")), SchedulerTemplate.c_process(), T))
    return ret
  }
}

object ArtNixGen {

  @record class V extends MTransformer {
    var thread: Option[AadlThread] = None() // the thread which has the largest port partition
    var partition: String = "" // port partition id
    var maxPorts: Z = -1 // the number of ports in the thread's partition
    var hasSporadic: B = F // does the model have sporadic threads?

    def v(r: AadlSystem): (AadlThread, String, Z, B) = {
      this.transformAadlSystem(r)
      return (thread.get, partition, maxPorts, hasSporadic)
    }

    override def preAadlThread(o: AadlThread): MTransformer.PreResult[AadlThread] = {
      hasSporadic = hasSporadic || o.dispatchProtocol == Dispatch_Protocol.Sporadic
      var (inData, inEvent, outData, outEvent) = ((0, 0, 0, 0))
      for (p <- o.features.filter((p => p.isInstanceOf[AadlPort]))) {
        if (p.isInstanceOf[AadlFeatureEvent]) {
          if (p.asInstanceOf[AadlPort].direction == Direction.In) {
            inEvent = inEvent + 1
          }
          else {
            outEvent = outEvent + 1
          }
        }
        if (p.isInstanceOf[AadlFeatureData]) {
          if (p.asInstanceOf[AadlPort].direction == Direction.In) {
            inData = inData + 1
          }
          else {
            outData = outData + 1
          }
        }
      }

      def s(z: Z, id: String): Unit = {
        if (z > maxPorts) {
          maxPorts = z
          thread = Some(o)
          partition = id
        }
      }

      s(inData, "dataIns")
      s(outData, "dataOuts")
      s(inEvent, "eventIns")
      s(outEvent, "eventOuts")

      return MTransformer.PreResultAadlThread(F, MNone())
    }
  }
}