// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlSymbol, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir.{BTSAssignmentAction, BTSBLESSAnnexClause, BTSExp, BTSNameExp, Transformer}

@sig trait BTSKey
@datatype class BTSExpKey (exp: BTSExp) extends BTSKey

@datatype class BTSSymbolTable(map: Map[BTSKey, AadlSymbol]) {
  def lookupExp(k: BTSExp): Option[AadlSymbol] = { return map.get(BTSExpKey(k)) }
}

@record class BTSExpResolver(symbolTable: SymbolTable,
                             aadlTypes: AadlTypes) {

  def resolve(annex: BTSBLESSAnnexClause,
              reporter: org.sireum.message.Reporter): BTSSymbolTable = {

    var m : Map[BTSKey, AadlSymbol] = Map.empty

    @datatype class BTSVisitor() extends Transformer.PrePost[B] {

    }

    BTSSymbolTable(m)
  }
}