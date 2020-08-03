// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._
import org.sireum.hamr.codegen.common.properties._
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, Dispatch_Protocol}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, Feature, FeatureEnd}

object CommonUtil {

  val toolName: String = "HAMR Codegen"

  def getLastName(n: ir.Name): String = {
    return n.name(n.name.size - 1)
  }

  def getName(n: ir.Name): String = {
    return st"${(n.name, "_")}".render
  }

  def isSystem(f: ir.Component): B = {
    return f.category == ir.ComponentCategory.System
  }

  def isDevice(f: ir.Component): B = {
    return f.category == ir.ComponentCategory.Device
  }

  def isThread(f: ir.Component): B = {
    return f.category == ir.ComponentCategory.Thread
  }

  def isData(f: ir.Component): B = {
    return f.category == ir.ComponentCategory.Data
  }

  def isPeriodic(c: ir.Component): B = {
    return PropertyUtil.getDispatchProtocol(c) == Some(Dispatch_Protocol.Periodic)
  }

  def isSporadic(c: ir.Component): B = {
    return PropertyUtil.getDispatchProtocol(c) == Some(Dispatch_Protocol.Sporadic)
  }

  def isAadlEventPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.EventPort
  }

  def isAadlEventDataPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.EventDataPort
  }

  def isAadlDataPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.DataPort
  }

  def isEventPort(f: ir.Feature): B = {
    return isAadlEventPort(f) || isAadlEventDataPort(f)
  }

  def isDataPort(f: ir.Feature): B = {
    return isAadlDataPort(f) || isAadlEventDataPort(f)
  }

  def isDataAccesPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.DataAccess
  }

  def isSubprogramAccess(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.SubprogramAccess
  }

  def isSubprogramAccessGroup(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.SubprogramAccessGroup
  }

  def isPort(f: ir.Feature): B = {
    return isEventPort(f) || isDataPort(f)
  }

  def isInFeature(f: Feature): B = {
    val ret: B = f match {
      case fe: FeatureEnd => fe.direction == Direction.In
      case _ => F
    }
    return ret
  }

  def isOutFeature(f: Feature): B = {
    val ret: B = f match {
      case fe: FeatureEnd => fe.direction == Direction.Out
      case _ => F
    }
    return ret
  }

  def isInPort(f: ir.Feature): B = {
    return isPort(f) && isInFeature(f)
  }

  def isOutPort(f: ir.Feature): B = {
    return isPort(f) && isOutFeature(f)
  }


  def getInPorts(c: ir.Component): ISZ[ir.FeatureEnd] = {
    return c.features.filter(f => f.isInstanceOf[ir.FeatureEnd] && isInPort(f)).map(f => f.asInstanceOf[FeatureEnd])
  }

  def getOutPorts(c: ir.Component): ISZ[ir.FeatureEnd] = {
    return c.features.filter(f => f.isInstanceOf[ir.FeatureEnd] && isOutPort(f)).map(f => f.asInstanceOf[FeatureEnd])
  }

  def findMaxZ(zs: ISZ[Z]): Z = {
    assert(zs.nonEmpty)
    return ops.ISZOps(zs).foldLeft((a: Z, b: Z) => if (a > b) a else b, zs(0))
  }

  def getPeriod(m: AadlThreadOrDevice): Z = {
    val ret: Z = m.period match {
      case Some(p) => p
      case _ => z"1"
    }
    return ret
  }
}


@datatype class Names(c: ir.Component,
                      basePackage: String) {
  val split: ISZ[String] = {
    val san = StringUtil.replaceAll(c.classifier.get.name, "::", ":")
    ops.StringOps(san).split(char => char == ':')
  }

  def componentType: String = {
    return StringUtil.sanitizeName(split(1))
  }

  def componentImpl: String = {
    return s"${componentType}_Impl"
  }

  def bridge: String = {
    return s"${componentType}_Bridge"
  }

  def bridgeIdentifier: String = {
    return s"${identifier}Bridge"
  }

  def bridgeTypeName: String = {
    return s"${packageName}.${bridge}"
  }

  def aadlPackage: String = {
    return split(0)
  }

  def packageName: String = {
    return s"${basePackage}.${aadlPackage}"
  }

  def packagePath: String = {
    return s"${basePackage}/${aadlPackage}"
  }

  def path: ISZ[String] = {
    return ISZ(basePackage, aadlPackage)
  }

  def identifier: String = {
    return CommonUtil.getLastName(c.identifier)
  }

  def instanceName: String = {
    return CommonUtil.getName(c.identifier)
  }

  def testName: String = {
    return s"${instanceName}_Test"
  }


  def cPackageName: String = {
    return st"${(path, "_")}".render
  }

  def cComponentImpl: String = {
    return s"${cPackageName}_${componentImpl}"
  }

  def cEntryPointAdapterName: String = {
    return s"${componentType}_adapter"
  }

  def cEntryPointAdapterQualifiedName: String = {
    return s"${cPackageName}_${cEntryPointAdapterName}"
  }

  def cThisApi: String = {
    return s"${cComponentImpl}_api_"
  }

  def cBridgeApi: String = {
    return s"${cPackageName}_${componentType}_Bridge_Api"
  }

  def cBridgeEntryPoints: String = {
    return s"${cPackageName}_${componentType}_Bridge_EntryPoints"
  }


  def sel4AppName: String = {
    return s"${componentImpl}_App"
  }

  def sel4SlangExtensionName: String = {
    return s"${componentType}_seL4Nix"
  }

  def sel4SlangExtensionStubName: String = {
    return s"${sel4SlangExtensionName}_Ext"
  }

  def sel4SlangExtensionQualifiedNameC: String = {
    return s"${cPackageName}_${sel4SlangExtensionName}"
  }
}
