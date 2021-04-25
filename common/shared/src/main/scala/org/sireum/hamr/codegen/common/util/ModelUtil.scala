// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{SymbolResolver, SymbolTable}
import org.sireum.hamr.codegen.common.transformers.Transformers
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeResolver}
import org.sireum.hamr.ir.{Aadl, Transformer}
import org.sireum.message.Reporter

object ModelUtil {
  def resolve(model: Aadl,
              packageName: String,
              maxStringSize: Z,
              unboundedZRBitWidth: Z,
              useCaseConnectors: B, reporter: Reporter): (Aadl, AadlTypes, SymbolTable) = {

    val result = Transformer(Transformers.MissingTypeRewriter(reporter)).transformAadl(Transformers.CTX(F, F), model)
    if(result.ctx.hasErrors) {
      reporter.error(None(), "HAMR CodeGen", "AST rewrite encountered errors")
    }

    val rmodel: Aadl = if (result.resultOpt.nonEmpty) result.resultOpt.get else model

    val rawConnections: B = PropertyUtil.getUseRawConnection(rmodel.components(0).properties)
    val aadlTypes = TypeResolver.processDataTypes(rmodel, rawConnections, maxStringSize, unboundedZRBitWidth, packageName)

    val symbolTable = SymbolResolver.resolve(
      model = rmodel,
      useCaseConnectors = useCaseConnectors,
      aadlTypes = aadlTypes,
      reporter = reporter)

    return (rmodel, aadlTypes, symbolTable)
  }
}
