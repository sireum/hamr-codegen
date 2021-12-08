// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.ir.GclExp

@sig trait  GclSymbols extends AadlSymbol

@sig trait GclKey
@datatype class GclExpKey(exp: GclExp) extends GclKey

@datatype class GclSymbolTable()