// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.codegen.common.containers.{FileResource, Resource, SireumSlangTranspilersCOption}
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlFeature, AadlThreadOrDevice}
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.{NameUtil, PathUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.ops._

object Util {
  var pathSep: C = '/'

  val toolName: String = "Arsit"

  // special message 'kind' so instruction messages can be filtered
  val ARSIT_INSTRUCTIONS_MESSAGE_KIND: String = "Arsit - Instructions"

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  def nameProvider(c: ir.Component,
                   basePackage: String): NameProvider = {
    return NameUtil.getAirNameProvider(c, basePackage)
  }

  def pathAppend(outputDir: String, s: ISZ[String]): String = {
    if (s.isEmpty) {
      return outputDir
    } else {
      return ISZOps(s).foldLeft((r: String, s: String) => s"${r}${pathSep}${s}", s"${outputDir}")
    }
  }

  // currently all Arsit targets are nix based so returned
  // path will only contain nix style path separators
  def relativizePaths(anchorDir: String, toRel: String, anchorResource: String): String = {
    val o1 = Os.path(anchorDir)
    val o2 = Os.path(toRel)
    val rel = o1.relativize(o2)
    return PathUtil.convertWinPathSepToNix(s"${anchorResource}/${rel}")
  }

  def isNix(platform: ArsitPlatform.Type): B = {
    val ret: B = platform match {
      case ArsitPlatform.JVM => F
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Linux => T
      case ArsitPlatform.Cygwin => T
      case ArsitPlatform.SeL4 => F
    }
    return ret
  }

  @pure def getLibraryFile(fileName: String): ST = {
    val e = ArsitLibrary.getFiles.filter(p => p._1 == fileName)
    assert(e.size == 1)
    return st"${e(0)._2}"
  }

  @pure def getDispatchTriggers(c: ir.Component): Option[ISZ[String]] = {
    if (!PropertyUtil.hasProperty(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)) {
      return None()
    } else {
      var ret: ISZ[String] = ISZ()
      for (p <- PropertyUtil.getPropertyValues(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)) {
        p match {
          case ir.ReferenceProp(v) => ret = ret :+ CommonUtil.getLastName(v)
          case _ => halt(s"Unhandled ${p}")
        }
      }
      return Some(ret)
    }
  }

  // FIXME: add symbol resolver support for subprograms and then remove this method
  @pure def getFeatureEnds_DEPRECATED(is: ISZ[ir.Feature]): ISZ[ir.FeatureEnd] = {
    return is.filter(f => f.isInstanceOf[ir.FeatureEnd]).map(m => m.asInstanceOf[ir.FeatureEnd])
  }

  @pure def getFeatureEndType(f: ir.FeatureEnd, types: AadlTypes): AadlType = {
    val ret: AadlType = f.classifier match {
      case Some(c) => types.typeMap.get(c.name).get
      case _ => TypeUtil.EmptyType
    }
    return ret
  }


  def getPort(aadlFeature: AadlFeature,
              feature: ir.FeatureEnd,
              parent: ir.Component,
              types: AadlTypes,
              basePackage: String,
              isTrigger: B,
              counter: Z): Port = {

    val pType = getFeatureEndType(feature, types)
    return Port(aadlFeature, feature, parent, pType, basePackage, counter)
  }

  def getPorts(m: AadlThreadOrDevice, types: AadlTypes, basePackage: String, counter: Z): ISZ[Port] = {
    var _counter = counter

    val component = m.component
    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)

    var ports: ISZ[Port] = ISZ()
    for (f <- m.getPorts()) {
      val portName = f.identifier
      val isTrigger: B =
        if (dispatchTriggers.isEmpty) T
        else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty

      ports = ports :+ getPort(f, f.feature, component, types, basePackage, isTrigger, _counter)

      _counter = _counter + 1
    }
    return ports
  }

  @pure def getIpc(ipcmech: IpcMechanism.Type, packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r: String = ipcmech match {
      case IpcMechanism.SharedMemory => "ipc_shared_memory.c"
      case x => halt("Unexpected IPC mechanism ${x}")
    }
    val lib = Util.getLibraryFile(r).render
    val c = StringUtil.replaceAll(lib, PACKAGE_PLACEHOLDER, packageName)
    return st"${c}"
  }
}

@enum object EntryPoints {
  "activate"
  "compute"
  "deactivate"
  "finalise"
  "initialise"
  "recover"
  "testInitialise"
  "testCompute"
}

// see property set in HAMR.aadl
object HAMR {

  @enum object OS {
    'Linux
    'macOS
    'Cygwin
    'CAmkES
  }

  @enum object HW {
    'ODROID
    'QEMU
    'x86
    'amd64
  }

}

@datatype class Port(aadlFeature: AadlFeature,
                     feature: ir.FeatureEnd,
                     parent: ir.Component,
                     _portType: AadlType,
                     basePackageName: String,
                     portId: Z) {

  def name: String = {
    return aadlFeature.identifier
  }

  def nameWithPortId: String = {
    return s"${name}_${portId}"
  }

  def nameId: String = {
    return s"${name}_id"
  }

  def sel4PortVarable: String = {
    return s"${name}_port"
  }

  def path: String = {
    return CommonUtil.getName(feature.identifier)
  }

  def parentName: String = {
    return CommonUtil.getLastName(parent.identifier)
  }

  def parentPath: String = {
    return CommonUtil.getName(parent.identifier)
  }

  def getPortTypeNames: TypeNameProvider = {
    return _portType.nameProvider
  }

  def urgency: Option[Z] = {
    return PropertyUtil.getUnitPropZ(feature.properties, OsateProperties.THREAD_PROPERTIES__URGENCY)
  }
}

@datatype class ProjectDirectories(options: ArsitOptions) {

  val slangOutputDir: String = options.slangOutputDir.value

  val slangSrcDir: String = Util.pathAppend(slangOutputDir, ISZ("src"))

  /* Slang dirs */
  val mainDir: String = Util.pathAppend(slangSrcDir, ISZ("main"))

  val architectureDir: String = Util.pathAppend(mainDir, ISZ("architecture"))

  val bridgeDir: String = Util.pathAppend(mainDir, ISZ("bridge"))

  val dataDir: String = Util.pathAppend(mainDir, ISZ("data"))

  val componentDir: String = Util.pathAppend(mainDir, ISZ("component"))

  val inspectorDir: String = Util.pathAppend(mainDir, ISZ("inspector"))

  val utilDir: String = Util.pathAppend(mainDir, ISZ("util"))

  /* Testing dirs */
  val testDir: String = Util.pathAppend(slangSrcDir, ISZ("test"))

  val testBridgeDir: String = Util.pathAppend(testDir, ISZ("bridge"))

  val testSystemDir: String = Util.pathAppend(testDir, ISZ("system"))

  val testUtilDir: String = Util.pathAppend(testDir, ISZ("util"))

  val auxCodeDir: ISZ[String] = options.auxCodeDirs

  val slangBinDir: String = Util.pathAppend(slangOutputDir, ISZ("bin"))

  val slangNixDir: String = Util.pathAppend(mainDir, ISZ("nix"))

  val seL4NixDir: String = Util.pathAppend(mainDir, ISZ("seL4Nix"))


  /* C dirs */
  val cOutputSharedDir: Os.Path = options.outputSharedCDir

  val cBinDir: Os.Path = cOutputSharedDir / "bin"

  val cNixDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("nix"))

  val cExt_c_Dir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("ext-c"))

  val cExt_schedule_Dir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("ext-schedule"))

  val cEtcDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("etc"))


  /* Camkes specific dirs */
  val cOutputPlatformDir: Os.Path = options.outputPlatformCDir

  val seL4CDir: String = Util.pathAppend(cOutputPlatformDir.value, ISZ("CAmkES_seL4"))

  val sel4EtcDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("etc_seL4"))
}

@sig trait Result {
  def resources: ISZ[FileResource]

  def auxResources: ISZ[Resource]

  def maxPort: Z

  def maxComponent: Z

  def maxConnection: Z
}

@datatype class ArsitResult(val resources: ISZ[FileResource],
                            val auxResources: ISZ[Resource],
                            val maxPort: Z,
                            val maxComponent: Z,
                            val maxConnection: Z) extends Result