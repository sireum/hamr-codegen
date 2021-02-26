// #Sireum
package org.sireum.hamr.codegen.common.transformers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.OsateProperties
import org.sireum.hamr.codegen.common.types.TypeUtil
import org.sireum.hamr.ir

object Transformers {

  val toolName: String = CommonUtil.toolName

  @datatype class CTX(requiresMissingType: B,
                      hasErrors: B)

  @datatype class MissingTypeRewriter(reporter: org.sireum.message.Reporter) extends ir.Transformer.PrePost[CTX] {

    val missingType: ir.Component = ir.Component(
      identifier = ir.Name(ISZ(), None()),
      category = ir.ComponentCategory.Data,
      classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)),
      features = ISZ(),
      subComponents = ISZ(),
      connections = ISZ(),
      connectionInstances = ISZ(),
      properties = ISZ(),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = ""
    )

    val missingArrayBaseType: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.DATA_MODEL__BASE_TYPE), None()),
      propertyValues = ISZ(ir.ClassifierProp(TypeUtil.MISSING_AADL_TYPE)),
      appliesTo = ISZ())

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
      propertyValues = ISZ(ir.ValueProp("Sporadic")),
      appliesTo = ISZ())


    override def postAadl(ctx: CTX, o: ir.Aadl): ir.Transformer.TPostResult[CTX, ir.Aadl] = {
      if (ctx.requiresMissingType) {
        ir.Transformer.TPostResult(ctx, Some(o(dataComponents = o.dataComponents :+ missingType)))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.Aadl]())
      }
    }

    override def postComponent(ctx: CTX, o: ir.Component): ir.Transformer.TPostResult[CTX, ir.Component] = {

      o.category match {
        case ir.ComponentCategory.Data =>
          if (o.classifier.isEmpty) {
            reporter.warn(o.identifier.pos, toolName, s"Classifier not specified for '${CommonUtil.getName(o.identifier)}'.  Substituting ${TypeUtil.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T),
              Some(o(classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)))))
          }
          else {
            ir.Transformer.TPostResult(ctx, None[ir.Component]())
          }
        case _ => ir.Transformer.TPostResult(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: ir.FeatureEnd): ir.Transformer.TPostResult[CTX, ir.FeatureEnd] = {
      if ((CommonUtil.isDataPort(o)) && o.classifier.isEmpty) {
        reporter.warn(o.identifier.pos, toolName, s"No datatype specified for data port ${CommonUtil.getName(o.identifier)}.  Substituting ${TypeUtil.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.FeatureEnd]())
      }
    }
  }

  @datatype class UnboundedIntegerRewriter(reporter: org.sireum.message.Reporter) extends ir.Transformer.PrePost[B] {
    val unboundInt: String = "Base_Types::Integer"
    val int32: String = "Base_Types::Integer_32"

    override def postClassifier(ctx: B, o: ir.Classifier): ir.Transformer.TPostResult[B, ir.Classifier] = {
      if(o.name == unboundInt) {
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }

    override def postClassifierProp(ctx: B, o: ir.ClassifierProp): ir.Transformer.TPostResult[B, ir.PropertyValue] = {
      if(o.name == unboundInt) {
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(T, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }
  }
}