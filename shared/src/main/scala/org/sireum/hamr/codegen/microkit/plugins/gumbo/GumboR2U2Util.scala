// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.lang.{ast => SAST}

object GumboR2U2Util {

  @datatype class R2U2MonitorInput(val exp: RAST.Expr,
                                   val expType: GumboC2POUtil.C2POType.Type,
                                   val referencedInputPorts: Set[String])

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
