// #Sireum

package org.sireum.hamr.codegen.common

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, Dispatch_Protocol}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, Feature, FeatureEnd}

object CommonUtil {

  @sig trait StoreValue

  @datatype class BoolValue(val value: B) extends StoreValue

  @datatype class ISZValue[T](val elements: ISZ[T]) extends StoreValue

  type Store = Map[String, StoreValue]

  type IdPath = ISZ[String]

  type DataIdPath = ISZ[String]

  type ThreadIdPath = ISZ[String]

  type PortIdPath = ISZ[String]

  type TypeIdPath = ISZ[String]

  val toolName: String = "HAMR Codegen"

  def splitClassifier(c: ir.Classifier): ISZ[String] = {
    val san = StringUtil.replaceAll(c.name, "::", ":")
    return ops.StringOps(san).split((char: C) => char == ':')
  }

  def getLastName(n: ir.Name): String = {
    return n.name(n.name.lastIndex)
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

  def isPeriodic(a: AadlThreadOrDevice): B = {
    return a.dispatchProtocol == Dispatch_Protocol.Periodic
  }

  def isSporadic(a: AadlThreadOrDevice): B = {
    return a.dispatchProtocol == Dispatch_Protocol.Sporadic
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

@enum object ModuleType {
  "jvm"
  "js"
  "shared"
}
