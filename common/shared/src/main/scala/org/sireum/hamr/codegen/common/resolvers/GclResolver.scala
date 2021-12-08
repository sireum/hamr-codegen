// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir.GclAnnex
import org.sireum.message.Reporter

@record class GclResolver () {
  def processGclAnnex(b: GclAnnex, symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): Option[GclSymbolTable] = {
    return None()
  }

}
