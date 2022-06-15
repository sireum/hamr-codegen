// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.lang.{ast => AST}
import org.sireum.hamr.ir.GclSpec

@datatype class GclSymbolTable(val rexprs: HashMap[AST.Exp, AST.Exp], // tipe resolved exprs
                               val apiReferences: ISZ[AadlPort], // all ports referenced in exprs
                               val integrationMap: Map[AadlPort, GclSpec],
                               val computeHandlerPortMap: Map[AST.Exp, AadlPort] // compute handle port ref map
                              )