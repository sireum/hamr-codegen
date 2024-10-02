// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitPlatform}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThreadOrDevice}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeNameProvider}

object ArtNixTemplate {

  @pure def portDef(portName: String,
                    portType: String): ST = {
    return st"""$portName: Option[$portType]"""
  }

  @pure def portOpt(portId: String,
                    portIdOpt: String,
                    portType: String,
                    mutable: B): ST = {
    if (mutable) {
      return st"""var ${portDef(portIdOpt, portType)} = None[$portType]()"""
    } else {
      return st"""val ${portDef(portIdOpt, portType)} = Some(${portId})"""
    }
  }

  @pure def portId(portIdName: String,
                   archPortInstanceName: String): ST = {
    return st"""val $portIdName = Arch.${archPortInstanceName}.id"""
  }

  @pure def portOptReset(portOptName: String,
                         portType: String): ST = {
    return st"""$portOptName = None[$portType]()"""
  }

  @pure def appCases(portOptName: String,
                     portId: String,
                     payloadType: TypeNameProvider): ST = {
    val ret: ST =
      st"""${portOptName} match {
          |  case Some(v) => ArtNix.updateData(${portId}, ${if (payloadType.isEmptyType) "v" else s"${payloadType.qualifiedPayloadName}(v)"})
          |  case _ =>
          |}"""
    return ret
  }

  @pure def platformPortDecl(portName: String,
                             portId: Z): ST = {
    return st"""val $portName: Art.PortId = Art.PortId.fromZ($portId)"""
  }

  @pure def artNixCases(srcArchPortId: String,
                        cases: ISZ[ST]): ST = {
    val ret: ST =
      st"""r(Arch.$srcArchPortId.id) = IS(
          |  ${(cases, ",\n")}
          |)"""
    return ret
  }

  @pure def artNixCase(portId: String,
                       dstArchPortId: String): ST = {
    return st"(IPCPorts.$portId, Arch.$dstArchPortId.id)"
  }

  @pure def mainSend(portId: String): ST = {
    return st"""${PlatformComm}.sendAsync(IPCPorts.${portId}, IPCPorts.${s"$portId"}, empty)"""
  }

  @pure def app(packageName: String,
                objectName: String,
                IPCPort_Id: String,
                period: Z,
                bridge: String,
                component: AadlThreadOrDevice,
                isPeriodic: B,
                types: AadlTypes,
                touchMethod: ST,
                basePackage: String
               ): ST = {

    def localPortId(p: Port): String = {
      return s"${p.name}PortId"
    }

    def localPortIdOpt(p: Port): String = {
      return s"${localPortId(p)}Opt"
    }

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component.component)
    val inPorts: ISZ[Port] = component.getPorts().filter((p: AadlPort) => CommonUtil.isInPort(p.feature)).map((f: AadlPort) => {
      val portName = f.identifier
      val isTrigger: B =
        if (dispatchTriggers.isEmpty) T
        else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
      Util.getPort(f, f.feature, component.component, types, basePackage, isTrigger, z"-1000")
    })

    var globals: ISZ[ST] = ISZ(
      st"""val entryPoints: Bridge.EntryPoints = Arch.${bridge}.entryPoints
          |val appPortId: Art.PortId = IPCPorts.${IPCPort_Id}
          |val appPortIdOpt: Option[Art.PortId] = Some(appPortId)""")

    var inits: ISZ[ST] = ISZ(st"${PlatformComm}.initialise(seed, appPortIdOpt)")

    globals = globals :+ st"""
                             |// incoming ports"""
    for (p <- inPorts) {
      globals = globals :+ st"val ${localPortId(p)}: Art.PortId = Arch.${bridge}.${p.name}.id"

      globals = globals :+ st"val ${localPortIdOpt(p)}: Option[Art.PortId] = Some(${localPortId(p)})"
      inits = inits :+ st"${PlatformComm}.initialise(seed, ${localPortIdOpt(p)})"
    }

    var computeBody: ST = st""

    val body: ST = {

      val loopBody: ST = {

        val receiveOnInPorts: ISZ[ST] =
          inPorts.map((p: Port) => {
            val dispatch: String = if (CommonUtil.isAadlDataPort(p.feature)) {
              "F"
            } else {
              "T"
            }
            st"""{
                |  val out = IPCPorts.emptyReceiveAsyncOut
                |  ${PlatformComm}.receiveAsync(${localPortIdOpt(p)}, out)
                |  out.value2 match {
                |    case Some(v: ${p.getPortTypeNames.qualifiedPayloadName}) => ArtNix.updateData(${localPortId(p)}, v)${if (!isPeriodic) s"; dispatch = ${dispatch}" else ""}
                |    case Some(v) => halt(s"Unexpected payload on port ${p.name}.  Expecting something of type ${p.getPortTypeNames.qualifiedPayloadName} but received $${v}")
                |    case None() => // do nothing
                |  }
                |}"""
          })

        if (isPeriodic) {
          st"""
              |${(receiveOnInPorts, "\n")}
              |entryPoints.compute()
              |${basePackage}.Process.sleep($period)"""
        } else {

          st"""var dispatch = F
              |
              |${(receiveOnInPorts, "\n")}
              |if (dispatch) {
              |  entryPoints.compute()
              |  ${basePackage}.Process.sleep($period)
              |} else {
              |  ${basePackage}.Process.sleep(10)
              |}"""
        }
      }

      computeBody = loopBody

      st"""var terminated = F
          |while (!terminated) {
          |  val out = IPCPorts.emptyReceiveAsyncOut
          |  ${PlatformComm}.receiveAsync(appPortIdOpt, out)
          |  if (out.value2.isEmpty) {
          |    compute()
          |  } else {
          |    terminated = T
          |  }
          |}
          |exit()"""
    }

    val ret: ST =
      st"""// #Sireum

          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import art.Art.PortId._
          |import art.scheduling.nop.NopScheduler
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${objectName} extends App {
          |
          |  ${(globals, "\n")}
          |
          |  def initialiseArchitecture(seed: Z): Unit = {
          |    ${(inits, "\n")}
          |
          |    Art.run(Arch.ad, NopScheduler())
          |  }
          |
          |  def initialise(): Unit = {
          |    entryPoints.initialise()
          |  }
          |
          |  def compute(): Unit = {
          |    ${computeBody}
          |  }
          |
          |  def finalise(): Unit = {
          |    entryPoints.finalise()
          |  }
          |
          |  def main(args: ISZ[String]): Z = {
          |
          |    val seed: Z = if (args.size == z"1") {
          |      val n = Z(args(0)).get
          |      if (n == z"0") 1 else n
          |    } else {
          |      1
          |    }
          |
          |    initialiseArchitecture(seed)
          |
          |    ${PlatformComm}.receive(appPortIdOpt, IPCPorts.emptyReceiveOut) // pause after setting up component
          |
          |    initialise()
          |
          |    ${PlatformComm}.receive(appPortIdOpt, IPCPorts.emptyReceiveOut) // pause after component init
          |
          |    println("${objectName} starting ...")
          |
          |    ${if (isPeriodic) "ArtNix.timeDispatch()" else "ArtNix.eventDispatch()"}
          |
          |    ${body}
          |
          |    touch()
          |
          |    return 0
          |  }
          |
          |  ${touchMethod}
          |
          |  def exit(): Unit = {
          |    finalise()
          |    ${PlatformComm}.finalise()
          |  }
          |
          |  override def atExit(): Unit = {
          |    exit()
          |  }
          |}"""
    return ret
  }

  @pure def ipc(packageName: String,
                ports: ISZ[ST]): ST = {

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import art.Art.PortId._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object IPCPorts {
          |  ${(ports, "\n")}
          |
          |  def emptyReceiveOut: MBox2[Art.PortId, DataContent] = {
          |    return MBox2(portId"0", art.Empty())
          |  }
          |
          |  def emptyReceiveAsyncOut: MBox2[Art.PortId, Option[DataContent]] = {
          |    return MBox2(portId"0", None())
          |  }
          |}
          |"""
    return ret
  }

  @pure def artNix(packageName: String,
                   cases: ISZ[ST],
                   eventInPorts: ISZ[String]): ST = {

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ArtNix {
          |
          |  val maxPortIds: Z = IPCPorts.Main.toZ + 1
          |  val timeTriggered: TimeTriggered = TimeTriggered()
          |  val noData: Option[DataContent] = None()
          |  val data: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  val connection: MS[Art.PortId, IS[Art.ConnectionId, (Art.PortId, Art.PortId)]] = {
          |    // mapping from src ports to pairs holding the destination app port ids and component port ids
          |    val r = MS.create[Art.PortId, IS[Art.ConnectionId, (Art.PortId, Art.PortId)]](maxPortIds, IS())
          |
          |    ${(cases, "\n")}
          |
          |    r
          |  }
          |  val eventInPorts: MS[Art.PortId, Art.PortId] = MS[Art.PortId, Art.PortId] (
          |    ${(eventInPorts, ",\n")}
          |  )
          |  var frozen: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  var outgoing: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
          |  var isTimeDispatch: B = F
          |
          |  def updateData(port: Art.PortId, d: DataContent): Unit = {
          |    data(port) = Some(d)
          |  }
          |
          |  def timeDispatch(): Unit = {
          |    isTimeDispatch = T
          |  }
          |
          |  def eventDispatch(): Unit = {
          |    isTimeDispatch = F
          |  }
          |
          |  def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
          |    if (isTimeDispatch) {
          |      return timeTriggered
          |    } else {
          |      var r = ISZ[Art.PortId]()
          |      for (i <- eventInPorts if data(i).nonEmpty) {
          |        r = r :+ i
          |      }
          |      return EventTriggered(r)
          |    }
          |  }
          |
          |  def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |    frozen = data
          |    for (i <- eventPortIds) {
          |      data(i) = noData
          |    }
          |  }
          |
          |  def putValue(portId: Art.PortId, data: DataContent): Unit = {
          |    outgoing(portId) = Some(data)
          |  }
          |
          |  def getValue(portId: Art.PortId): Option[DataContent] = {
          |    return frozen(portId)
          |  }
          |
          |  def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |    for (p <- dataPortIds) {
          |      outgoing(p) match {
          |        case Some(d) =>
          |          outgoing(p) = noData
          |          for(e <- connection(p)){
          |            ${PlatformComm}.sendAsync(e._1, e._2, d)
          |          }
          |        case _ =>
          |      }
          |    }
          |
          |    for (p <- eventPortIds) {
          |      outgoing(p) match {
          |        case Some(d) =>
          |          outgoing(p) = noData
          |          for(e <- connection(p)){
          |            ${PlatformComm}.sendAsync(e._1, e._2, d)
          |          }
          |        case _ =>
          |      }
          |    }
          |  }
          |
          |  def logInfo(title: String, msg: String): Unit = {
          |    print(title)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def logError(title: String, msg: String): Unit = {
          |    eprint(title)
          |    eprint(": ")
          |    eprintln(msg)
          |  }
          |
          |  def logDebug(title: String, msg: String): Unit = {
          |    print(title)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def time(): Art.Time = {
          |    return Process.time()
          |  }
          |
          |  def run(): Unit = {}
          |
          |  def tearDownSystemState(): Unit = {}
          |
          |  def setUpSystemState(): Unit = {}
          |
          |  def initializePhase(): Unit = {}
          |
          |  def computePhase(): Unit = {}
          |
          |  def finalizePhase(): Unit = {}
          |}
          |"""
    return ret
  }

  @pure def main(packageName: String,
                 sends: ISZ[ST]): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object LegacyDemo extends App {
          |  def main(args: ISZ[String]): Z = {
          |
          |    val seed: Z = if (args.size == z"1") {
          |      val n = Z(args(0)).get
          |      if (n <= z"0") 1 else n
          |    } else {
          |      1
          |    }
          |
          |    ${PlatformComm}.initialise(seed, None())
          |
          |    val empty = art.Empty()
          |
          |    ${(sends, "\n")}
          |
          |    ${PlatformComm}.finalise()
          |    return 0
          |  }
          |}
          | """
    return ret
  }

  @pure def SharedMemory(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@ext object SharedMemory {
          |  def create(id: Z): Z = $$
          |  def get(id: Z): Z = $$
          |  def send(appId: Z, portId: Z, d: DataContent): Unit = $$
          |  def sendAsync(appId: Z, portId: Z, d: DataContent): B = $$
          |  def receive(portId: Z, out: MBox2[Art.PortId, DataContent]): Unit = $$
          |  def receiveAsync(portId: Z, out: MBox2[Art.PortId, Option[DataContent]]): Unit = $$
          |  def remove(id: Z): Unit = $$
          |}"""
    return ret
  }

  @pure def SharedMemory_Ext(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object SharedMemory_Ext {
          |  def create(id: Z): Z = halt("stub")
          |  def get(id: Z): Z = halt("stub")
          |  def send(appId: Z, portId: Z, d: DataContent): Unit = halt("stub")
          |  def sendAsync(appId: Z, portId: Z, d: DataContent): B = halt("stub")
          |  def receive(portId: Z, out: MBox2[Art.PortId, DataContent]): Unit = halt("stub")
          |  def receiveAsync(portId: Z, out: MBox2[Art.PortId, Option[DataContent]]): Unit = halt("stub")
          |  def remove(id: Z): Unit = halt("stub")
          |}"""
    return ret
  }

  val PlatformComm: String = "PlatformComm"

  @pure def platformComm(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@ext object $PlatformComm {
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = $$
          |  def receive(portOpt: Option[Art.PortId],  out: MBox2[Art.PortId, DataContent]): Unit = $$
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = $$
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = $$
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = $$
          |  def finalise(): Unit = $$
          |}
          |"""
    return ret
  }

  @pure def PlatformCommExt(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${PlatformComm}_Ext {
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = halt("stub")
          |  def receive(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, DataContent]) = halt("stub")
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = halt("stub")
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = halt("stub")
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = halt("stub")
          |  def finalise(): Unit = halt("stub")
          |}
          |"""
    return ret
  }

  @pure def PlatformCommNix(packageName: String): ST = {

    val init: ST =
      st"""val id = seed + port.toZ
          |SharedMemory.create(id)
          |ids = ids :+ id"""

    val receive: ST =
      st"""portOpt match {
          |  case Some(port) =>
          |    out.value1 = port
          |    SharedMemory.receive(seed + port.toZ, out)
          |  case _ => halt("Unsupported receive operation without port.")
          |}"""

    val send: String = "SharedMemory.send(app.toZ, seed + port.toZ, data)"

    val sendAsync: String = "SharedMemory.sendAsync(app.toZ, seed + port.toZ, data)"

    val finalise: ST =
      st"""for (id <- ids) {
          |  SharedMemory.remove(id)
          |}"""

    val receiveAsync: ST =
      st"""portOpt match {
          |  case Some(port) => SharedMemory.receiveAsync(seed + port.toZ, out)
          |  case _ => halt("Unsupported receive operation without port.")
          |}"""

    val ret: ST =
      st"""// #Sireum
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${PlatformComm}Nix {
          |
          |  var seed: Z = 0
          |  var ids: IS[Art.PortId, Z] = IS()
          |
          |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = {
          |    ${PlatformComm}Nix.seed = seed
          |    portOpt match {
          |      case Some(port) =>
          |        $init
          |      case _ =>
          |    }
          |  }
          |
          |  def receive(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, DataContent]): Unit = {
          |    ${receive}
          |  }
          |
          |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = {
          |    ${send}
          |  }
          |
          |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = {
          |    val r = ${sendAsync}
          |    return r
          |  }
          |
          |  def receiveAsync(portOpt: Option[Art.PortId], out: MBox2[Art.PortId, Option[DataContent]]): Unit = {
          |    ${receiveAsync}
          |  }
          |
          |  def finalise(): Unit = {
          |    ${finalise}
          |  }
          |}
          |"""
    return ret
  }

  @pure def Process(packageName: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |package $packageName
          |
          |import org.sireum._
          |import art.Art
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@ext object Process {
          |  def sleep(n: Z): Unit = $$
          |
          |  def time(): Art.Time = $$
          |}
          |"""
    return ret
  }

  @pure def ProcessExt(packageName: String): ST = {
    val ret: ST =
      st"""package $packageName
          |
          |import org.sireum._
          |import art.Art
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object Process_Ext {
          |  def sleep(millis: Z): Unit = halt("stub")
          |
          |  def time(): Art.Time = halt("stub")
          |}"""
    return ret
  }

  @pure def compileSlash(dirs: ProjectDirectories): ST = {
    val cOutputDirRel = Util.relativizePaths(dirs.cBinDir.value, dirs.cNixDir, "")

    val ret =
      st"""::/*#! 2> /dev/null                                   #
          |@ 2>/dev/null # 2>nul & echo off & goto BOF           #
          |if [ -z "$${SIREUM_HOME}" ]; then                     #
          |  echo "Please set SIREUM_HOME env var"               #
          |  exit -1                                             #
          |fi                                                    #
          |exec "$${SIREUM_HOME}/bin/sireum" slang run "$$0" "$$@" #
          |:BOF
          |setlocal
          |if not defined SIREUM_HOME (
          |  echo Please set SIREUM_HOME env var
          |  exit /B -1
          |)
          |"%SIREUM_HOME%\bin\sireum.bat" slang run %0 %*
          |exit /B %errorlevel%
          |::!#*/
          |// #Sireum
          |
          |import org.sireum._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |val home = Os.slashDir
          |
          |Cli(Os.pathSepChar).parseCompile(Os.cliArgs, 0) match {
          |  case Some(o: Cli.CompileOption) if o.args.size == 0 =>
          |    val nixDir = home / "${cOutputDirRel}" / "slang-build"
          |    if(!nixDir.up.exists){
          |      eprintln(s"Directory does not exist, have your run the transpiler? $${nixDir.up}")
          |      Os.exit(-1)
          |    }
          |    nixDir.mkdir()
          |
          |    if((nixDir / "CMakeCache.txt").exists) {
          |      // remove cached transpiler variables
          |      proc"cmake -U BOUND_CHECK -U NO_PRINT -U RANGE_CHECK -U WITH_LOC ..".at(nixDir).console.runCheck()
          |    }
          |
          |    var cmake: ISZ[String] = ISZ("cmake")
          |    if(o.boundCheck) { cmake = cmake :+ "-D" :+ "BOUND_CHECK=ON" }
          |    if(o.noPrint) { cmake = cmake :+ "-D" :+ "NO_PRINT=ON" }
          |    if(o.rangeCheck) { cmake = cmake :+ "-D" :+ "RANGE_CHECK=ON" }
          |    if(o.withLoc) { cmake = cmake :+ "-D" :+ "WITH_LOC=ON" }
          |    cmake = (cmake :+ "-D" :+ s"CMAKE_BUILD_TYPE=$${o.build}") :+ ".."
          |
          |    if(o.verbose) { println(st"$${(cmake, " ")}".render) }
          |
          |    Os.proc(cmake).at(nixDir).console.runCheck()
          |
          |    val MAKE_ARGS: String = Os.env("MAKE_ARGS") match {
          |      case Some(o) => o
          |      case _ => ""
          |    }
          |
          |    proc"make --jobs $${o.jobs} $${MAKE_ARGS}".at(nixDir).console.runCheck()
          |
          |    val binDir = home / "slang-build"
          |    binDir.removeAll()
          |    binDir.mkdir()
          |
          |    if(Os.isWin) {
          |      nixDir.list.filter(p => p.ext == "exe").foreach((f: Os.Path) => f.moveTo(binDir / f.name))
          |    } else {
          |      nixDir.list.filter(p => ops.StringOps(p.name).endsWith("_App")).foreach((f: Os.Path) => f.moveTo(binDir / f.name))
          |      val candidates: ISZ[Os.Path] = ISZ[String]("Demo", "LegacyDemo").map((m: String) => nixDir / m)
          |      val main: ISZ[Os.Path] = candidates.filter((p: Os.Path) => p.exists)
          |      if(main.isEmpty || main.size > 1) {
          |        eprintln(s"Found $${main.size} possible main programs.  There should be only one of the following: $${candidates}")
          |        Os.exit(1)
          |      }
          |      main(0).moveTo(binDir / main(0).name)
          |    }
          |
          |    Os.exit(0)
          |
          |  case Some(o: Cli.CompileOption) =>
          |    println(o.help)
          |    Os.exit(0)
          |  case Some(o: Cli.HelpOption) => Os.exit(0)
          |  case _ =>
          |    eprintln("Could not recognize arguments")
          |}
          |
          |Os.exit(-1)
          |
          |${ArsitLibrary.getCompileCli}
          |"""

    return ret
  }

  @pure def compile(arch: ArsitPlatform.Type,
                    dirs: ProjectDirectories): ST = {

    val script_home = "${SCRIPT_HOME}"
    val cOutputDirRel = Util.relativizePaths(dirs.cBinDir.value, dirs.cNixDir, script_home)

    val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
    val mv: ST = if (arch == ArsitPlatform.Cygwin) {
      st"mv *.exe ${script_home}/${buildDir}/"
    } else {
      st"""mv *_App ${script_home}/${buildDir}/
          |mv Main ${script_home}/${buildDir}/"""
    }
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |${CommentTemplate.doNotEditComment_bash}
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd ${script_home}
          |mkdir -p ${buildDir}
          |mkdir -p ${cOutputDirRel}/${buildDir}
          |cd ${cOutputDirRel}/${buildDir}
          |BOUND_CHECK=$${BOUND_CHECK:-OFF}
          |NO_PRINT=$${NO_PRINT:-OFF}
          |RANGE_CHECK=$${RANGE_CHECK:-OFF}
          |WITH_LOC=$${WITH_LOC:-OFF}
          |cmake -DBOUND_CHECK=$$BOUND_CHECK -DNO_PRINT=$$NO_PRINT -DRANGE_CHECK=$$RANGE_CHECK -DWITH_LOC=$$WITH_LOC -DCMAKE_BUILD_TYPE=Release ..
          |make $$MAKE_ARGS
          |$mv"""
    return ret
  }

  @pure def runS(apps: ISZ[String]): ST = {
    val buildDir = st"slang-build"

    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |${CommentTemplate.doNotEditComment_bash}
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |
          |# Uncomment the following to prevent terminal from closing when the app shuts down or crashes
          |#PREVENT_CLOSE="; bash -i"
          |
          |# check if getopt supports long options
          |getopt -T > /dev/null || ret=$$?
          |[[ $$ret -eq 4 ]] && GNU_GETOPT=0 || GNU_GETOPT=1
          |
          |OPTIONS=s:h
          |LONGOPTS=scheduler:,help
          |
          |function usage {
          |  echo ""
          |  echo "Usage: <option>*"
          |  echo ""
          |  echo "Available Options:"
          |  if [[ $$GNU_GETOPT -eq 0 ]]; then
          |    echo "-s, --scheduler        The scheduler to use (expects one of"
          |    echo "                         { default, roundRobin, static, legacy};"
          |    echo "                         default: default)"
          |    echo "-h, --help             Display this information"
          |  else
          |    echo "-s                     The scheduler to use (expects one of"
          |    echo "                         { default, roundRobin, static, legacy};"
          |    echo "                         default: default)"
          |    echo "-h                     Display this information"
          |  fi
          |}
          |
          |if [[ $$GNU_GETOPT -eq 0 ]]; then
          |  ! PARSED=$$(getopt --options=$$OPTIONS --longoptions=$$LONGOPTS --name "$$0" -- "$$@")
          |else
          |  ! PARSED=$$(getopt $$OPTIONS "$$@")
          |fi
          |
          |if [[ $${PIPESTATUS[0]} -ne 0 ]]; then
          |  usage
          |  exit 1
          |fi
          |
          |eval set -- "$$PARSED"
          |
          |SCHEDULER="default"
          |while true; do
          |  case "$$1" in
          |    -h|--help) usage; exit 0 ;;
          |    -s|--scheduler)
          |      case "$$2" in
          |        default|roundRobin|static|legacy)
          |          SCHEDULER="$$2" ;;
          |        *)
          |          echo "Invalid scheduler: $${2}"
          |          exit 2 ;;
          |      esac
          |      shift 2 ;;
          |    --) shift; break ;;
          |  esac
          |done
          |
          |# handle non-option arguments
          |if [[ $$# -ne 0 ]]; then
          |  echo "$$0: Unexpected non-option arguments"
          |  usage
          |  exit 3
          |fi
          |
          |function launch() {
          |  if [ "$$2" ]; then SCHEDULER_ARG=" -s $${2}"; fi
          |  if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then
          |    for APP in $$1; do
          |      cygstart mintty /bin/bash -c "slang-build/$${APP}$${SCHEDULER_ARG}$${PREVENT_CLOSE}" &
          |    done
          |  elif [[ "$$(uname)" == "Darwin" ]]; then
          |    for APP in $$1; do
          |      # workaround to launch the applications via separate Terminals. Create a shell script in the
          |      # /tmp directory that launches the application. Then delete the shell script when the
          |      # application exits
          |      echo "$${SCRIPT_HOME}/slang-build/$${APP}$${SCHEDULER_ARG}$${PREVENT_CLOSE} ; rm /tmp/$${APP}.sh" > /tmp/$${APP}.sh ; chmod +x /tmp/$${APP}.sh ; open -a Terminal /tmp/$${APP}.sh &
          |    done
          |  elif [[ "$$(expr substr $$(uname -s) 1 5)" == "Linux" ]]; then
          |    for APP in $$1; do
          |      x-terminal-emulator -T $${APP} -e sh -i -c "slang-build/$${APP}$${SCHEDULER_ARG}$${PREVENT_CLOSE}" &
          |    done
          |  else
          |    >&2 echo "Platform not supported: $$(uname)."
          |    exit 1
          |  fi
          |}
          |
          |EXT=""
          |if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then EXT=".exe"; fi
          |
          |case "$${SCHEDULER}" in
          |  legacy)
          |    if [ ! -f ./slang-build/LegacyDemo$${EXT} ]; then
          |      if [ -f ./slang-build/Demo$${EXT} ]; then
          |        echo "Error: Found program for Slang based schedulers.  Pass '--legacy' to the"
          |        echo "transpiler script in order to use the legacy scheduler"
          |      else
          |        echo "Expected program not found, have you compiled? $${SCRIPT_HOME}/slang-build/LegacyDemo$${EXT}"
          |      fi
          |      exit 1
          |    fi
          |
          |    launch "${(apps.map((m: String) => s"${m}$${EXT}"), " ")}";
          |
          |    read -p "Press enter to initialise components ..."
          |    slang-build/LegacyDemo$${EXT}
          |    read -p "Press enter again to start ..."
          |    slang-build/LegacyDemo$${EXT}
          |    ;;
          |  *)
          |    if [ ! -f ./slang-build/Demo$${EXT} ]; then
          |      if [ -f ./slang-build/LegacyDemo$${EXT} ]; then
          |        echo "Error: Found program for the legacy scheduler. Either pass '-s legacy' to the"
          |        echo "run script if you want to use the legacy scheduler, or, do not pass"
          |        echo "'--legacy' to the transpiler script if you want to use a Slang based scheduler"
          |      else
          |        echo "Expected program not found, have you compiled? $${SCRIPT_HOME}/slang-build/Demo$${EXT}"
          |      fi
          |      exit 1
          |    fi
          |
          |    launch "Demo" $${SCHEDULER};
          |    ;;
          |esac
          |"""
    return ret
  }

  @pure def run(apps: ISZ[String],
                arch: ArsitPlatform.Type): ST = {

    val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
    val ext: String = if (arch == ArsitPlatform.Cygwin) ".exe" else ""
    val stapp: ISZ[ST] = apps.map(st => {
      val prefix: String = arch match {
        case ArsitPlatform.Cygwin => "cygstart mintty /bin/bash"
        case ArsitPlatform.Linux => s"x-terminal-emulator -T ${st} -e sh -c"
        case ArsitPlatform.MacOS => "open -a Terminal"
        case _ => halt(s"Unexpected platform ${arch}")
      }
      st"""$prefix "${buildDir}/${st}$ext$${PREVENT_CLOSE}" &"""
    })
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |${CommentTemplate.doNotEditComment_bash}
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |
          |# Uncomment the following to prevent terminal from closing if app crashes
          |#PREVENT_CLOSE="; bash -i"
          |
          |${(stapp, "\n")}
          |read -p "Press enter to initialise components ..."
          |${buildDir}/Main$ext
          |read -p "Press enter again to start ..."
          |${buildDir}/Main$ext"""
    return ret
  }

  @pure def stop(apps: ISZ[String]): ST = {
    val ret: ST =
      st"""#!/usr/bin/env bash
          |#
          |${CommentTemplate.doNotEditComment_bash}
          |#
          |APPS="Demo ${(apps, " ")}"
          |for APP in $${APPS}; do
          |  pkill -SIGTERM -f $$APP
          |done
          |ME=`whoami`
          |
          |# message queue
          |IPCS_Q=`ipcs -q | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -q $$id;
          |done
          |
          |# shared memory
          |IPCS_Q=`ipcs -m | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -m $$id;
          |done
          |
          |# semaphores
          |IPCS_Q=`ipcs -s | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -s $$id;
          |done
          |"""
    return ret
  }
}
