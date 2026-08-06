// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.{Context, TargetLanguage}
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboR2U2Util {

  @datatype class R2U2MonitorInput(val exp: RAST.Expr,
                                   val expType: GumboC2POUtil.C2POType.Type,
                                   val referencedInputPorts: Set[String])

  // Lower a resolved Slang expression to an executable R2U2 signal expression.
  // Port reads become dispatch-local snapshots so each referenced getter is
  // called once. An absent event-data payload uses its Rust default.
  @pure def lowerR2U2Input(exp: SAST.Exp,
                          inputPortIds: Set[String],
                          component: AadlComponent,
                          context: Context.Type,
                          isAssumeRequires: B,
                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          store: Store,
                          reporter: Reporter): R2U2MonitorInput = {
    val inputPorts: Map[String, AadlPort] = Map.empty ++
      component.getPorts()
        .filter(p => inputPortIds.contains(p.identifier))
        .map(p => p.identifier ~> p)

    // Rewrite api.port references and collect the ports whose getter snapshots
    // must be declared in timeTriggered.
    val inputRewriter = R2U2InputRewriter(inputPorts)
    val snapshotExp: SAST.Exp = inputRewriter.transform_langastExp(exp) match {
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
      // getter type rather than GCL's ghost-oriented port type.
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, _)) =>
        st"${rustExp}.unwrap_or_default()"
      case _ =>
        rustExp
    }
    return R2U2MonitorInput(
      exp = RAST.ExprST(monitorExp),
      expType = GumboC2POUtil.getExprType(exp),
      referencedInputPorts = inputRewriter.referencedInputPorts)
  }

  @record class R2U2InputRewriter(val inputPorts: Map[String, AadlPort]) extends org.sireum.hamr.ir.MTransformer {
    var referencedInputPorts: Set[String] = Set.empty

    // GCL resolves HasEvent(port) to api.port.nonEmpty. Verus models event and
    // event-data ports as Option, but their executable Rust getters differ:
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
          inputPorts.get(id.value) match {
            case Some(port) =>
              // Convert the GCL ghost type to the executable getter type.
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

              // Record the port so the caller emits `let port = api.get_port();`.
              referencedInputPorts = referencedInputPorts + id.value
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
          inputPorts.get(id.value) match {
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
