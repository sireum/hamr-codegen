// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{SymbolResolver, SymbolTable}
import org.sireum.hamr.codegen.common.transformers.Transformers
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeResolver}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.ir.{Transformer => AirTransformer}

object ModelUtil {
  @datatype class ModelElements(model: Aadl,
                                types: AadlTypes,
                                symbolTable: SymbolTable)

  def resolve(origModel: Aadl,
              packageName: String,
              options: CodeGenConfig,
              reporter: Reporter): Option[ModelElements] = {

    var transModel = origModel

    val aadlMaps = SymbolResolver.buildAadlMaps(transModel)

    val tResult = AirTransformer(Transformers.MissingTypeRewriter(reporter)).transformAadl(Transformers.CTX(F), transModel)
    if(reporter.hasError) {
      return None()
    }
    transModel = if(tResult.resultOpt.nonEmpty) tResult.resultOpt.get else transModel

    // transform BTS nodes -- e.g. out data port assignments -> port output
    val btxResults = Transformers.BTSMTransform(aadlMaps, reporter).transformAadl(transModel)
    if(reporter.hasError) {
      return None()
    }

    transModel = if(btxResults.nonEmpty) btxResults.get else transModel

    val rawConnections: B = PropertyUtil.getUseRawConnection(transModel.components(0).properties)
    val aadlTypes = TypeResolver.processDataTypes(transModel, rawConnections, options.maxStringSize, options.bitWidth, packageName)

    val symbolTable = SymbolResolver.resolve(
      model = transModel,
      aadlTypes = aadlTypes,
      options = options,
      reporter = reporter)

    return Some(ModelElements(transModel, aadlTypes, symbolTable))
  }
}
