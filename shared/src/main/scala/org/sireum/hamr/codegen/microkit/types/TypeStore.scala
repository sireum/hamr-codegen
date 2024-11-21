//# Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType

@sig trait TypeStore {
  def typeName: String
  def aadlType: AadlType
}

@datatype class DefaultTypeStore (val typeName: String,
                                  val aadlType: AadlType) extends TypeStore
