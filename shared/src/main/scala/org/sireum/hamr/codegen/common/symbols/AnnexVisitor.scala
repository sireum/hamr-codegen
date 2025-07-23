// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir.{Annex, AnnexLib}
import org.sireum.message.Reporter

@msig trait AnnexVisitor {
  // calling context is a singleton so allow visitor to reset their state if needed b/w invocations
  def reset: B

  def offerLibraries(annexLibs: ISZ[AnnexLib],
                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     store: Store,
                     reporter: Reporter): (ISZ[AnnexLibInfo], Store)

  def offer(context: AadlComponent,
            annex: Annex,
            annexLibs: ISZ[AnnexLibInfo],
            symbolTable: SymbolTable,
            aadlTypes: AadlTypes,
            store: Store,
            reporter: Reporter): (Option[AnnexClauseInfo], Store)
}