// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.ir.{GclExp, GclSpec}

@sig trait GclSymbols extends AadlSymbol

@sig trait GclKey

@datatype class GclExpKey(exp: GclExp) extends GclKey

@datatype class GclSymbolTable(symbols: Map[GclKey, AadlSymbol],
                               expTypes: HashMap[GclExp, AadlType],
                               integrationPort: Map[GclSpec, AadlPort]) {

  def lookup(e: GclExp): Option[AadlSymbol] = {
    return symbols.get(GclExpKey(e))
  }

}