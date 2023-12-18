// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.ir.GclSpec
import org.sireum.lang.{ast => AST}
import org.sireum.message.Position

@datatype class SymTableKey(val exp: AST.Exp,
                            val posOpt: Option[Position])

@datatype class GclSymbolTable(val rexprs: HashMap[SymTableKey, AST.Exp], // tipe resolved exprs

                               val apiReferences: ISZ[AadlPort], // all ports referenced in exprs
                               val integrationMap: Map[AadlPort, GclSpec],
                               val computeHandlerPortMap: Map[AST.Exp, AadlPort] // compute handle port ref map
                              )