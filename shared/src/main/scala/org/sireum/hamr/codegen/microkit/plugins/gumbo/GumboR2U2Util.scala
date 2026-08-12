// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.{Context, TargetLanguage}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiUtil, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboR2U2Util {

  @datatype class R2U2MonitorInput(val exp: RAST.Expr,
                                   val expType: GumboC2POUtil.C2POType.Type,
                                   val enumTypeOpt: Option[GumboC2POUtil.C2POEnum],
                                   val referencedPorts: Set[String])

  // The C transport exposes peek for every port; only R2U2 components add it
  // to their generated Rust application API.
  @pure def peekApiContributions(thread: AadlThread,
                                 tp: CRustTypeProvider,
                                 store: Store): ComponentApiContributions = {
    var contributions = ComponentApiContributions.empty
    for (port <- thread.getPorts() if !StoreUtil.isSynthetic(port.path, store)) {
      contributions = contributions.combine(CRustApiUtil.processPeekPort(port, tp))
    }
    return contributions
  }

  // Lower a resolved Slang expression to an executable R2U2 signal expression.
  // Port reads become dispatch-local observations. An absent event-data 
  // payload uses its Rust default.
  @pure def lowerR2U2Input(exp: SAST.Exp,
                          portIds: Set[String],
                          component: AadlComponent,
                          context: Context.Type,
                          isAssumeRequires: B,
                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          store: Store,
                          reporter: Reporter): R2U2MonitorInput = {
    val ports: Map[String, AadlPort] = Map.empty ++
      component.getPorts()
        .filter(p => portIds.contains(p.identifier))
        .map(p => p.identifier ~> p)

    // Rewrite api.port references and collect the ports that must be observed
    // in the pre- or post-dispatch hook.
    val portRewriter = R2U2PortRewriter(ports)
    val snapshotExp: SAST.Exp = portRewriter.transform_langastExp(exp) match {
      case MSome(e) => e
      case _ => exp
    }
    val rustExp = SlangExpUtil.rewriteExpH(
      rexp = snapshotExp,
      owner = component.classifier,
      optComponent = Some(component),
      context = context,
      inRequires = isAssumeRequires,
      target = TargetLanguage.rust,
      substitutions = Map.empty,
      aadlTypes = types,
      tp = tp,
      store = store,
      reporter = reporter)
    val monitorExp: ST = snapshotExp.typedOpt match {
      // R2U2 loads a concrete payload value. Presence is loaded as a separate
      // Boolean signal (e.g., HasEvent(...)), so an absent optional payload can
      // safely use a default. The rewritten expression carries the executable
      // peek type rather than GCL's ghost-oriented port type.
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, _)) =>
        st"${rustExp}.unwrap_or_default()"
      case _ =>
        rustExp
    }
    val expType: GumboC2POUtil.C2POType.Type = GumboC2POUtil.getExprType(exp)
    val enumTypeOpt: Option[GumboC2POUtil.C2POEnum] =
      if (expType == GumboC2POUtil.C2POType.enumeration) Some(GumboC2POUtil.getEnumType(exp, types))
      else None()
    return R2U2MonitorInput(
      exp = RAST.ExprST(monitorExp),
      expType = expType,
      enumTypeOpt = enumTypeOpt,
      referencedPorts = portRewriter.referencedPorts)
  }

  @record class R2U2PortRewriter(val ports: Map[String, AadlPort]) extends org.sireum.hamr.ir.MTransformer {
    var referencedPorts: Set[String] = Set.empty

    // GCL resolves HasEvent(port) to api.port.nonEmpty. Verus models event and
    // event-data ports as Option, but their executable Rust peek methods differ:
    //
    //   event port       -> B
    //   data port        -> payload
    //   event-data port  -> Option[payload]
    //
    // Rewrite api.port in pre. In post, its parent has become port.nonEmpty or
    // port.isEmpty and can be normalized using the executable getter type.
    override def pre_langastExpSelect(o: SAST.Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o match {
        case SAST.Exp.Select(Some(SAST.Exp.Ident(SAST.Id("api"))), id, _) =>
          ports.get(id.value) match {
            case Some(port) =>
              // Convert the GCL ghost type to the executable peek type.
              val snapshotTypedOpt: Option[SAST.Typed] = port match {
                case _: AadlEventPort =>
                  SAST.Typed.bOpt
                case _: AadlDataPort =>
                  o.typedOpt match {
                    case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => Some(payload)
                    case _ => o.typedOpt
                  }
                case _: AadlEventDataPort =>
                  o.typedOpt
              }

              // Record the port so the caller emits a dispatch-local peek.
              referencedPorts = referencedPorts + id.value
              val snapshot = SAST.Exp.Ident(id = id, attr = o.attr(typedOpt = snapshotTypedOpt))
              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(snapshot))
            case _ =>
          }
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    // Children are already rewritten, so api.port.status arrives as port.status;
    // the retained Id identifies its AADL port kind.
    @pure override def post_langastExpSelect(o: SAST.Exp.Select): MOption[SAST.Exp] = {
      o match {
        case SAST.Exp.Select(Some(snapshot@SAST.Exp.Ident(id)), statusId, _)
          if statusId.value == "nonEmpty" || statusId.value == "isEmpty" =>
          ports.get(id.value) match {
            // Event-data remains Option[payload]; SlangExpUtil will translate
            // nonEmpty/isEmpty to is_some()/is_none().
            case Some(_: AadlEventDataPort) =>
              return MNone()

            // A plain event getter returns B, so its snapshot already represents
            // presence: nonEmpty is the value and isEmpty is its negation.
            case Some(_) =>
              if (statusId.value == "isEmpty") {
                return MSome(SAST.Exp.Unary(
                  op = SAST.Exp.UnaryOp.Not,
                  exp = snapshot,
                  attr = o.attr,
                  opPosOpt = statusId.attr.posOpt))
              } else {
                return MSome(snapshot)
              }
            case _ =>
          }
        case _ =>
      }
      return MNone()
    }
  }
}
