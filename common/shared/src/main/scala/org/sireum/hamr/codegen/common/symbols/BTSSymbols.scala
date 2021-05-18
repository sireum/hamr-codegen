//# Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.ir.{BTSBLESSAnnexClause, BTSExp, BTSStateDeclaration, BTSVariableDeclaration}

@sig trait BTSSymbol extends AadlSymbol

@datatype class BTSState(id: String,
                         state: BTSStateDeclaration) extends BTSSymbol

@datatype class BTSVariable(id: String,
                            typ: AadlType,

                            decl: BTSVariableDeclaration) extends BTSSymbol


@sig trait BTSKey
@datatype class BTSExpKey (exp: BTSExp) extends BTSKey

@datatype class BTSSymbolTable(annex: BTSBLESSAnnexClause,
                               symbols: Map[BTSKey, AadlSymbol],
                               expTypes: HashMap[BTSExp, AadlType],
                               states: Map[String, BTSState],
                               variables: Map[String, BTSVariable]) {

  def lookupExp(k: BTSExp): Option[AadlSymbol] = { return symbols.get(BTSExpKey(k)) }
}
