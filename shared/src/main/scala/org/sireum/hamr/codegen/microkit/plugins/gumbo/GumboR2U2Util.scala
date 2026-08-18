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
import org.sireum.hamr.ir.GclStateVar
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboR2U2Util {

  @datatype class R2U2MonitorInput(val exp: RAST.Expr,
                                   val expType: GumboC2POUtil.C2POType.Type,
                                   val enumTypeOpt: Option[GumboC2POUtil.C2POEnum],
                                   val arrayTypeOpt: Option[GumboC2POUtil.C2POArray],
                                   val structTypeOpt: Option[GumboC2POUtil.C2POStruct],
                                   val referencedPorts: Set[String],
                                   val isPostStateVar: B)

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
                          stateVars: ISZ[GclStateVar],
                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          store: Store,
                          reporter: Reporter): R2U2MonitorInput = {
    // In(state) is sampled before dispatch; current state is sampled afterward.
    val stateVarOpt: Option[SAST.Exp.Ident] = exp match {
      case SAST.Exp.Input(id: SAST.Exp.Ident) => Some(id)
      case id: SAST.Exp.Ident if ops.ISZOps(stateVars).exists((stateVar: GclStateVar) => stateVar.name == id.id.value) =>
        Some(id)
      case _ => None()
    }
    val valueExp: SAST.Exp = stateVarOpt match {
      case Some(stateVar) => stateVar
      case _ => exp
    }
    val isPostStateVar: B = stateVarOpt.nonEmpty && !exp.isInstanceOf[SAST.Exp.Input]
    val ports: Map[String, AadlPort] = Map.empty ++
      component.getPorts()
        .filter(p => portIds.contains(p.identifier))
        .map(p => p.identifier ~> p)

    // Rewrite api.port references and collect the ports that must be observed
    // in the pre- or post-dispatch hook.
    val portRewriter = R2U2PortRewriter(ports)
    val snapshotExp: SAST.Exp = portRewriter.transform_langastExp(valueExp) match {
      case MSome(e) => e
      case _ => valueExp
    }
    val substitutions: Map[String, String] = stateVarOpt match {
      case Some(stateVar) => Map.empty[String, String] + stateVar.id.value ~> s"self.${stateVar.id.value}"
      case _ => Map.empty
    }
    var rustExp = SlangExpUtil.rewriteExpH(
      rexp = snapshotExp,
      owner = component.classifier,
      optComponent = Some(component),
      context = context,
      inRequires = isAssumeRequires,
      target = TargetLanguage.rust,
      substitutions = substitutions,
      aadlTypes = types,
      tp = tp,
      store = store,
      reporter = reporter)
    // Local subclause functions are emitted in the component's GUMBOX module.
    if (GumboC2POUtil.isGumboFunctionCall(valueExp, component.classifier)) {
      rustExp = st"GUMBOX::$rustExp"
    }
    snapshotExp.typedOpt match {
      // R2U2 loads a concrete payload value. Presence is loaded as a separate
      // Boolean signal (e.g., HasEvent(...)), so an absent optional payload can
      // safely use a default. The rewritten expression carries the executable
      // peek type rather than GCL's ghost-oriented port type.
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, _)) =>
        rustExp = st"${rustExp}.unwrap_or_default()"
      case _ =>
    }
    val expType: GumboC2POUtil.C2POType.Type = GumboC2POUtil.getExprType(valueExp)
    val arrayTypeOpt: Option[GumboC2POUtil.C2POArray] =
      if (expType == GumboC2POUtil.C2POType.array) GumboC2POUtil.getArrayType(valueExp, types, store)
      else None()

    val enumTypeOpt: Option[GumboC2POUtil.C2POEnum] =
      if (expType == GumboC2POUtil.C2POType.enumeration) Some(GumboC2POUtil.getEnumType(valueExp, types))
      else None()
    val structTypeOpt: Option[GumboC2POUtil.C2POStruct] =
      if (expType == GumboC2POUtil.C2POType.struct) Some(GumboC2POUtil.getStructType(valueExp, types))
      else None()
    return R2U2MonitorInput(
      exp = RAST.ExprST(rustExp),
      expType = expType,
      enumTypeOpt = enumTypeOpt,
      arrayTypeOpt = arrayTypeOpt,
      structTypeOpt = structTypeOpt,
      referencedPorts = portRewriter.referencedPorts,
      isPostStateVar = isPostStateVar)
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

    // Children are already rewritten, so api.port.operation arrives as
    // port.operation; the retained Id identifies its AADL port kind.
    @pure override def post_langastExpSelect(o: SAST.Exp.Select): MOption[SAST.Exp] = {
      o match {
        case SAST.Exp.Select(Some(snapshot@SAST.Exp.Ident(id)), operationId, _) =>
          ports.get(id.value) match {
            case Some(_: AadlEventDataPort) =>
              operationId.value match {
                // Event-data remains Option[payload]; SlangExpUtil will translate
                // nonEmpty/isEmpty to is_some()/is_none().
                case "nonEmpty" | "isEmpty" =>
                  return MNone()
                case "get" =>
                  // R2U2 loads presence separately, so use a default payload when
                  // the event-data port is empty instead of panicking on unwrap().
                  o.typedOpt match {
                    case Some(m: SAST.Typed.Method) if m.owner == SAST.Typed.optionName && m.name == "get" =>
                      return MSome(o(id = operationId(value = "unwrap_or_default()")))
                    case _ =>
                  }
                case _ =>
              }

            // A plain event getter returns B, so its snapshot already represents
            // presence: nonEmpty is the value and isEmpty is its negation.
            case Some(_: AadlEventPort) if operationId.value == "nonEmpty" || operationId.value == "isEmpty" =>
              if (operationId.value == "isEmpty") {
                return MSome(SAST.Exp.Unary(
                  op = SAST.Exp.UnaryOp.Not,
                  exp = snapshot,
                  attr = o.attr,
                  opPosOpt = operationId.attr.posOpt))
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
