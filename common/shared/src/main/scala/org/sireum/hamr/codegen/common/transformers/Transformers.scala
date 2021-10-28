// #Sireum
package org.sireum.hamr.codegen.common.transformers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.SymbolResolver.AadlMaps
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
      if(o.name == unboundInt) {
        val reporter = message.Reporter.create
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(ctx ++ reporter.messages, Some(ir.Classifier(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }

    override def postClassifierProp(ctx: ISZ[message.Message], o: ir.ClassifierProp): ir.Transformer.TPostResult[ISZ[message.Message], ir.PropertyValue] = {
      if(o.name == unboundInt) {
        val reporter = message.Reporter.create
        reporter.warn(None(), toolName, s"Replacing classifier ${unboundInt} with ${int32}")
        return ir.Transformer.TPostResult(ctx ++ reporter.messages, Some(ir.ClassifierProp(int32)))
      } else {
        return ir.Transformer.TPostResult(ctx, None())
      }
    }
  }

  @datatype class VContainer (origProcess: ir.Component,
                              newProcess: ir.Component,
                              origThreads : ISZ[ir.Component],
                              newThread: ir.Component)

  @record class VirtProcessRewriter(aadlMap: AadlMaps,
                                    reporter: org.sireum.message.Reporter) extends MAirTransformer {
    var replaced: Map[ISZ[String], VContainer] = Map.empty
    val REPLACEMENT_THREAD_NAME: String = "HAMR_INSERTED_VM_THREAD"

    override def postComponent(o: ir.Component): MOption[ir.Component] = {
      o.category match {
        case ir.ComponentCategory.System if replaced.nonEmpty =>
          val (replacedProcesses, other) : (ISZ[VContainer], ISZ[ir.Component]) = {
            var r: ISZ[VContainer] = ISZ()
            var other: ISZ[ir.Component] = ISZ()
            for(s <- o.subComponents) {
              if(replaced.contains(s.identifier.name)) { r = r :+ replaced.get(s.identifier.name).get }
              else { other = other :+ s }
            }
            (r, other)
          }

          if(replacedProcesses.nonEmpty) {
            val connectionInstances:ISZ[ir.ConnectionInstance] = {
              o.connectionInstances.map((ci: ir.ConnectionInstance) => {
                val src = ci.src.component.name
                val dst = ci.dst.component.name

                def isOldThread(name: ISZ[String]): Option[VContainer] = {
                  for(vc <- replacedProcesses;
                      t <- vc.origThreads) {
                    if(t.identifier.name == name) { return Some(vc)}
                  }
                  return None()
                }

                var xci: ir.ConnectionInstance = ci
                var mod: B = F

                isOldThread(src) match {
                  // connection leaving a replaced thread
                  case Some(x) =>
                    val firstcrn = ci.connectionRefs(0).name.name
                    val oldThread2ProcessConn: ir.Connection = aadlMap.connectionsMap.get(firstcrn).get
                    if (oldThread2ProcessConn.dst.size > 1) {
                      halt(s"Connection Instance ${ci.name} has more destination connections than expected. Please report this")
                    }
                    val processPort = oldThread2ProcessConn.dst(0).feature.get
                    val opp = ops.ISZOps(processPort.name)
                    val fname = opp.slice(0, opp.s.size - 1) :+ REPLACEMENT_THREAD_NAME :+ opp.last
                    xci = xci(
                      src = ci.src(
                        component = x.newThread.identifier,
                        feature = Some(ir.Name(fname, None()))
                      )
                    )
                    mod = T
                  case _ =>
                }

                isOldThread(dst) match {
                  // connection entering a replaced thread
                  case Some(x) =>
                    val lastcrn = ci.connectionRefs(ci.connectionRefs.size - 1).name.name
                    val lastc = aadlMap.connectionsMap.get(lastcrn).get
                    if(lastc.src.size > 1){
                      halt(s"Connection Instance ${ci.name} has more destination connections than expected. Please report this")
                    }
                    val processPort = lastc.src(0).feature.get
                    val opp = ops.ISZOps(processPort.name)
                    val fname = opp.slice(0, opp.s.size - 1) :+ REPLACEMENT_THREAD_NAME :+ opp.last
                    xci = xci(
                      dst = ci.dst(component = x.newThread.identifier,
                        feature = Some(ir.Name(fname, None())))
                    )
                    mod = T
                  case _ =>
                }

                if(mod) { xci }
                else { ci }
              })
            }

            val rSystem = o(connectionInstances = connectionInstances)
            return MSome(rSystem)
          } else {
            return MNone()
          }

        case ir.ComponentCategory.Process =>
          var virtProcessPeriod: Option[ir.Property] = None()
          val toVM: B = PropertyUtil.getDiscreetPropertyValue(o.properties, OsateProperties.DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING) match {
            case Some(v: ir.ReferenceProp) =>
              val virtx = aadlMap.airComponentMap.get(CommonUtil.getName(v.value)).get
              virtx.category match {
                case ir.ComponentCategory.VirtualProcessor =>
                  virtProcessPeriod = PropertyUtil.getProperty(virtx.properties, OsateProperties.TIMING_PROPERTIES__PERIOD)
                  T
                case _ => F
              }
            case _ => F
          }

          if(toVM) {
            val threads = o.subComponents.filter(p => p.category == ir.ComponentCategory.Thread)
            if(threads.size > 1) {
              var subComponents = o.subComponents.filter(p => p.category != ir.ComponentCategory.Thread)

              val vFeatures: ISZ[ir.Feature] = {
                (CommonUtil.getInPorts(o) ++ CommonUtil.getOutPorts(o)).map((f: ir.FeatureEnd) => {
                  val x = ops.ISZOps(f.identifier.name)
                  f(identifier = ir.Name((x.slice(0, x.s.size - 1) :+ REPLACEMENT_THREAD_NAME) :+ x.last, f.identifier.pos))
                })
              }

              val classifier: Option[ir.Classifier] = {
                val x = ops.StringOps(threads(0).classifier.get.name)
                val p = x.lastIndexOf(':')
                Some(ir.Classifier(s"${x.substring(0, p + 1)}${REPLACEMENT_THREAD_NAME}"))
              }

              var props = ISZ(ir.Property(
                name = ir.Name(name = ISZ("Thread_Properties::Dispatch_Protocol"), pos = None()),
                propertyValues = ISZ(ir.ValueProp("Periodic")),
                appliesTo = ISZ()))

              if(virtProcessPeriod.nonEmpty) { props = props :+ virtProcessPeriod.get}

              val vid = ir.Name(o.identifier.name :+ REPLACEMENT_THREAD_NAME, o.identifier.pos)
              val newThread = ir.Component(
                identifier = vid,
                category = ir.ComponentCategory.Thread,
                classifier = classifier,
                features = vFeatures,
                subComponents = ISZ(),
                connections = ISZ(),
                connectionInstances = ISZ(),
                properties = props,
                flows = ISZ(),
                modes = ISZ(),
                annexes = ISZ(),
                uriFrag = "")

              val rProcess = o(
                subComponents = subComponents :+ newThread,
                connectionInstances = ISZ())

              replaced = replaced + (o.identifier.name ~> VContainer(origProcess = o, newProcess = rProcess, origThreads = threads, newThread = newThread))

              return MSome(rProcess)
            }
          }
          return MNone()
        case _ => return MNone()
      }
    }
  }

  @record class BTSMTransform(val aadlMaps: AadlMaps,
                              val reporter: org.sireum.message.Reporter) extends MAirTransformer {
    var unlabeledCount: Z = 0

    override def postBTSTransitionLabel(o: BTSTransitionLabel): MOption[BTSTransitionLabel] = {
      if(o.id.name.isEmpty){
        val id = s"_unlabeled_transition_${unlabeledCount}"
        unlabeledCount = unlabeledCount + 1
        return MSome(BTSTransitionLabel(Name(ISZ(id), o.id.pos) , o.priority))
      } else {
        return MNone()
      }
    }
    override def postBTSAction(o: BTSAction): MOption[BTSAction] = {
      o match {
        case a @ BTSAssignmentAction(lhs @ BTSNameExp(name, pos), rhs) =>
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