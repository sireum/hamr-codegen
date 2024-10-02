// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.{SymbolResolver, SymbolTable, SymbolUtil}
import org.sireum.hamr.codegen.common.transformers.Transformers
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeResolver}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenOption
import org.sireum.hamr.ir.{Aadl, Transformer => AirTransformer}
import org.sireum.message.{Position, Reporter}

object ModelUtil {
  @datatype class ModelElements(model: Aadl,
                                modelPosOpt: Option[Position],
                                types: AadlTypes,
                                symbolTable: SymbolTable)

  def resolve(origModel: Aadl,
              modelPosOpt: Option[Position],
              packageName: String,
              options: CodegenOption,
              reporter: Reporter): Option[ModelElements] = {

    var transModel = origModel

    val aadlMaps = SymbolUtil.buildAadlMaps(transModel, reporter)

    val tResult = AirTransformer(Transformers.MissingTypeRewriter()).transformAadl(Transformers.CTX(F, ISZ()), transModel)
    reporter.reports(tResult.ctx.messages)
    if (reporter.hasError) {
      return None()
    }
    transModel = if (tResult.resultOpt.nonEmpty) tResult.resultOpt.get else transModel

    // transform BTS nodes -- e.g. out data port assignments -> port output
    val btsmt = Transformers.BTSMTransform(aadlMaps, Reporter.create)
    val btxResults = btsmt.transformAadl(transModel)
    reporter.reports(btsmt.reporter.messages)
    if (reporter.hasError) {
      return None()
    }

    transModel = if (btxResults.nonEmpty) btxResults.get else transModel

    val rawConnections: B = PropertyUtil.getUseRawConnection(transModel.components(0).properties)
    val aadlTypes = TypeResolver.processDataTypes(transModel, rawConnections, options.maxStringSize, options.bitWidth, packageName, reporter)

    SymbolResolver.resolve(
      model = transModel,
      aadlTypes = aadlTypes,
      aadlMaps = aadlMaps,
      options = options,
      reporter = reporter) match {
      case Some(symbolTable) => return Some(ModelElements(transModel, modelPosOpt, aadlTypes, symbolTable))
      case _ => return None()
    }
  }
}
