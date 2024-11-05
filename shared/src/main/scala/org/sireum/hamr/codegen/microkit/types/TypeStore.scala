//# Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._

@sig trait TypeStore {
  def typeName: String
}

@datatype class DefaultTypeStore (val typeName: String) extends TypeStore
