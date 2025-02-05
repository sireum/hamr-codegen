//# Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType

@sig trait TypeStore {
  def getCTypeName(aadl: AadlType): String

  def getRustTypeName(aadl: AadlType): String

  def getRepType(aadlType: AadlType): AadlType
}

@datatype class DefaultTypeStore (val m: Map[AadlType, (String, String)],
                                  val optStringType: Option[AadlType]) extends TypeStore {

  def getRepType(aadlType: AadlType): AadlType = {
    if (aadlType.name == "Base_Types::String") {
      return optStringType.get
    } else {
      return aadlType
    }
  }

  def getCTypeName(aadlType: AadlType): String = {
    m.get(aadlType) match {
      case Some((c, _)) => return c
      case _ if (aadlType.name == "Base_Types::String") => return "String"
      case _ => halt(s"Infeasible: $aadlType")
    }
  }

  def getRustTypeName(aadlType: AadlType): String = {
    m.get(aadlType) match {
      case Some((_, rust)) => return rust
      case _ => halt(s"Infeasible: $aadlType")
    }
  }
}
