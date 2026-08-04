// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.AadlComponent
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

  // Lowers monitor input signal from its resolved Slang expression to executable
  // Rust. Input-port selections are first replaced with dispatch-local snapshots
  // and recorded so the caller can emit one getter per referenced port. Optional
  // event-data payloads receive a safe default when absent because their presence
  // is represented by a separate R2U2 input signal (i.e., HasEvent(...)).
  @pure def lowerR2U2Input(exp: SAST.Exp,
                          inputPortIds: Set[String],
                          component: AadlComponent,
                          context: Context.Type,
                          isAssumeRequires: B,
                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          store: Store,
                          reporter: Reporter): R2U2MonitorInput = {
    val snapshotRewriter = MonitorPortSnapshotRewriter(inputPortIds)
    val snapshotExp: SAST.Exp = snapshotRewriter.transform_langastExp(exp) match {
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
    val monitorExp: ST = if (GumboC2POUtil.isOptional(exp)) {
      st"${rustExp}.unwrap_or_default()"
    } else {
      rustExp
    }
    return R2U2MonitorInput(
      exp = RAST.ExprST(monitorExp),
      expType = GumboC2POUtil.getExprType(exp),
      referencedInputPorts = snapshotRewriter.referencedInputPorts)
  }

  @record class MonitorPortSnapshotRewriter(val inputPortIds: Set[String]) extends org.sireum.hamr.ir.MTransformer {
    var referencedInputPorts: Set[String] = Set.empty

    // Replace api.<input-port> selections with the dispatch-local port value and
    // record the port so the caller can emit exactly one corresponding getter.
    override def pre_langastExpSelect(o: SAST.Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o match {
        case SAST.Exp.Select(Some(SAST.Exp.Ident(SAST.Id("api"))), id, _)
          if inputPortIds.contains(id.value) =>
          referencedInputPorts = referencedInputPorts + id.value
          return org.sireum.hamr.ir.MTransformer.PreResult(
            F,
            MSome(SAST.Exp.Ident(id = id, attr = o.attr)))
        case _ =>
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
      }
    }
  }
}
