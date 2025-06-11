// #Sireum

package org.sireum.hamr.codegen.arsit.util

import org.sireum._
import org.sireum.hamr.codegen.arsit.Util
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object ArsitLinter {

  @pure def lint(model: Aadl, o: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable,
           plugins: ISZ[Plugin], store: Store, reporter: Reporter): B = {

    for (t <- aadlTypes.typeMap.values) {
      t match {
        case at: ArrayType if at.dimensions.size > 1 =>
          // ISSUE: sergen does not support nested sequences like ISZ[ISZ[Z]]
          reporter.error(None(), Util.toolName, s"Invalid array definition '${at.name}': Multi-dimensional array are not currently supported. Consider using nested array data components instead.")
        case _ =>
      }
    }
    return !reporter.hasError
  }
}
