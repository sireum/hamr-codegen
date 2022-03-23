// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.hamr.ir.GclSpec

@datatype class GclSymbolTable(val rexprs: HashMap[AST.Exp, AST.Exp],
                               val integrationPort: Map[GclSpec, AadlPort])