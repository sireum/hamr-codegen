// #Sireum
package org.sireum.hamr.codegen.common.transformers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.OsateProperties
import org.sireum.hamr.codegen.common.symbols.AadlMaps
import org.sireum.hamr.codegen.common.types.TypeUtil
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{BTSAction, BTSAssignmentAction, BTSNameExp, BTSPortOutAction, BTSTransitionLabel, Name, MTransformer => MAirTransformer}

object Transformers {

  val toolName: String = CommonUtil.toolName

  @datatype class CTX(val requiresMissingType: B, val messages: ISZ[message.Message])

  @datatype class MissingTypeRewriter extends ir.Transformer.PrePost[CTX] {

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
            val reporter = message.Reporter.create
            reporter.warn(o.identifier.pos, toolName, s"Classifier not specified for '${CommonUtil.getName(o.identifier)}'.  Substituting ${TypeUtil.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T, messages = ctx.messages ++ reporter.messages),
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
        val reporter = message.Reporter.create
        reporter.warn(o.identifier.pos, toolName, s"No datatype specified for data port ${CommonUtil.getName(o.identifier)}.  Substituting ${TypeUtil.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T, messages = ctx.messages ++ reporter.messages), Some(o(classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.FeatureEnd]())
      }
    }
  }

  @datatype class UnboundedIntegerRewriter extends ir.Transformer.PrePost[ISZ[message.Message]] {
    val unboundInt: String = "Base_Types::Integer"
    val int32: String = "Base_Types::Integer_32"

    override def postClassifier(ctx: ISZ[message.Message], o: ir.Classifier): ir.Transformer.TPostResult[ISZ[message.Message], ir.Classifier] = {
      if (o.name == unboundInt) {
        val reporter = message.Reporter.create
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(ctx ++ reporter.messages, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }

    override def postClassifierProp(ctx: ISZ[message.Message], o: ir.ClassifierProp): ir.Transformer.TPostResult[ISZ[message.Message], ir.PropertyValue] = {
      if (o.name == unboundInt) {
        val reporter = message.Reporter.create
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(ctx ++ reporter.messages, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }
  }

  @datatype class VContainer(origProcess: ir.Component,
                             newProcess: ir.Component,
                             origThreads: ISZ[ir.Component],
                             newThread: ir.Component)

  @record class BTSMTransform(val aadlMaps: AadlMaps,
                              val reporter: org.sireum.message.Reporter) extends MAirTransformer {
    var unlabeledCount: Z = 0

    override def postBTSTransitionLabel(o: BTSTransitionLabel): MOption[BTSTransitionLabel] = {
      if (o.id.name.isEmpty) {
        val id = s"_unlabeled_transition_${unlabeledCount}"
        unlabeledCount = unlabeledCount + 1
        return MSome(BTSTransitionLabel(Name(ISZ(id), o.id.pos), o.priority))
      } else {
        return MNone()
      }
    }

    override def postBTSAction(o: BTSAction): MOption[BTSAction] = {
      o match {
        case a@BTSAssignmentAction(lhs@BTSNameExp(name, pos), rhs) =>
          val id = CommonUtil.getName(name)
          aadlMaps.airFeatureMap.get(id) match {
            case Some(f) if CommonUtil.isDataPort(f) =>
              assert(CommonUtil.isOutPort(f), s"Data port is on lhs of an assignment exp but it isn't outgoing: $id")

              return MSome(BTSPortOutAction(f.identifier, Some(rhs)))
            case _ =>
              // TODO
              return MNone[BTSAction]()
          }

        case _ => return MNone[BTSAction]()
      }
    }
  }
}