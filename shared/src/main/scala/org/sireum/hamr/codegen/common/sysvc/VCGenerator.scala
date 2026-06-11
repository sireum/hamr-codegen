// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.{AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.sysvc.ScheduleNextRel._
import org.sireum.hamr.ir.{GclAssume, GclCaseStatement, GclGuarantee, GclSchedule, GclScheduleComponentRef}

object VCGenerator {

  @pure def hasSystemSchedule(symbolTable: SymbolTable): B = {
    return getSystemSchedule(symbolTable).nonEmpty
  }

  @pure def getSystemSchedule(symbolTable: SymbolTable): Option[GclSchedule] = {
    symbolTable.annexClauseInfos.get(symbolTable.rootSystem.path) match {
      case Some(infos) =>
        for (info <- infos) {
          info match {
            case gclInfo: GclAnnexClauseInfo =>
              gclInfo.annex.schedule match {
                case Some(_) => return gclInfo.annex.schedule
                case _ =>
              }
            case _ =>
          }
        }
      case _ =>
    }
    return None()
  }

  @pure def resolveCompPath(compRef: GclScheduleComponentRef,
                            resolvedAliasMap: Map[String, IdPath]): IdPath = {
    if (compRef.component.name.isEmpty) {
      return compRef.component.name
    }
    val aliasName = compRef.component.name(compRef.component.name.lastIndex)
    resolvedAliasMap.get(aliasName) match {
      case Some(fullPath) => return fullPath
      case _ => return compRef.component.name
    }
  }

  @pure def generate(schedule: GclSchedule,
                     nextRel: NextRelResult,
                     resolvedComponentAliasMap: Map[String, IdPath],
                     symbolTable: SymbolTable): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()

    val threads = symbolTable.getThreads()
    val writeSets = WriteFrameBuilder.buildAll(
      threads = for (t <- threads) yield t,
      symbolTable = symbolTable)

    val placeMap: Map[PlaceId, PlaceInfo] = {
      var m: Map[PlaceId, PlaceInfo] = Map.empty
      for (p <- nextRel.places) {
        m = m + p.placeId ~> p
      }
      m
    }

    vcs = vcs :+ generateInitStateVC(nextRel, symbolTable)

    for (i <- z"0" until nextRel.transitions.size) {
      val t = nextRel.transitions(i)
      nextRel.activationMap.get(t.inPlaces(0)) match {
        case Some(compRef) =>
          val compPath = resolveCompPath(compRef, resolvedComponentAliasMap)
          vcs = vcs :+ generatePreAssertVC(i, t, compRef, placeMap, compPath, symbolTable)
          vcs = vcs :+ generateNextAssertTaskVC(i, t, compRef, placeMap, compPath, writeSets, symbolTable)
        case _ =>
          vcs = vcs :+ generateNextAssertSkipVC(i, t, placeMap)
      }
    }

    vcs = vcs :+ generatePostPreVC(nextRel, placeMap)

    val mhipPairs = MHIPComputer.computeMHIP(nextRel)
    for (pair <- mhipPairs) {
      vcs = vcs ++ generateIndependenceVCs(pair._1, pair._2, placeMap, nextRel, writeSets, resolvedComponentAliasMap)
    }

    return vcs
  }

  @pure def getAssertExp(placeId: PlaceId, placeMap: Map[PlaceId, PlaceInfo]): Option[org.sireum.lang.ast.Exp] = {
    placeMap.get(placeId) match {
      case Some(info) =>
        info.assertOpt match {
          case Some(a) => return Some(a.exp)
          case _ => return None()
        }
      case _ => return None()
    }
  }

  @pure def collectPlaceAsserts(placeIds: ISZ[PlaceId],
                                placeMap: Map[PlaceId, PlaceInfo]): ISZ[org.sireum.lang.ast.Exp] = {
    var exps: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    for (pid <- placeIds) {
      getAssertExp(pid, placeMap) match {
        case Some(e) => exps = exps :+ e
        case _ =>
      }
    }
    return exps
  }

  @pure def getGclInfoOpt(compPath: IdPath, symbolTable: SymbolTable): Option[GclAnnexClauseInfo] = {
    symbolTable.annexClauseInfos.get(compPath) match {
      case Some(clauses) =>
        return Some(clauses(0).asInstanceOf[GclAnnexClauseInfo])
      case _ => return None()
    }
  }

  // Integration constraints are port invariants the component-level verification
  // already discharges: guarantees on out ports are checked at every entrypoint
  // exit (GUMBOX I-Guar, initialize included) and assumes on in ports are relied
  // upon at every dispatch (I-Assm). They therefore contribute to the system-level
  // task contracts -- guarantees join the component's postcondition (Next-Assert
  // premises, Init-State premises) and assumes join its precondition (Pre-Assert
  // obligations) -- without restating them as compute clauses in the model.
  // Iterates the thread's ports against the symbol table's integrationMap (the
  // resolved clauses; the resolver enforces in port => assume, out port =>
  // guarantee) so the serializer can reconstruct the same clauses in the same
  // order.
  @pure def integrationExps(thread: AadlThread, info: GclAnnexClauseInfo, wantAssumes: B): ISZ[org.sireum.lang.ast.Exp] = {
    var exps: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    for (port <- thread.getPorts()) {
      info.gclSymbolTable.integrationMap.get(port) match {
        case Some(a: GclAssume) =>
          if (wantAssumes) {
            exps = exps :+ a.exp
          }
        case Some(g: GclGuarantee) =>
          if (!wantAssumes) {
            exps = exps :+ g.exp
          }
        case _ =>
      }
    }
    return exps
  }

  @pure def generateInitStateVC(nextRel: NextRelResult,
                                symbolTable: SymbolTable): VC = {
    var initGuarantees: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    for (thread <- symbolTable.getThreads()) {
      getGclInfoOpt(thread.path, symbolTable) match {
        case Some(info) =>
          info.annex.initializes match {
            case Some(init) =>
              for (g <- init.guarantees) {
                initGuarantees = initGuarantees :+ g.exp
              }
            case _ =>
          }
          initGuarantees = initGuarantees ++ integrationExps(thread, info, F)
        case _ =>
      }
    }

    val placeMap: Map[PlaceId, PlaceInfo] = {
      var m: Map[PlaceId, PlaceInfo] = Map.empty
      for (p <- nextRel.places) {
        m = m + p.placeId ~> p
      }
      m
    }
    val startAsserts = collectPlaceAsserts(ISZ(nextRel.startPlace), placeMap)

    return VC(
      kind = VCKind.InitState,
      premises = initGuarantees,
      conclusion = startAsserts,
      writeSetOpt = None(),
      source = VCSource(
        transitionIdx = None(),
        componentOpt = None(),
        mhipPairOpt = None()))
  }

  @pure def generatePreAssertVC(transIdx: Z,
                                t: Transition,
                                compRef: GclScheduleComponentRef,
                                placeMap: Map[PlaceId, PlaceInfo],
                                compPath: IdPath,
                                symbolTable: SymbolTable): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, placeMap)

    var assumes: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    getGclInfoOpt(compPath, symbolTable) match {
      case Some(info) =>
        info.annex.compute match {
          case Some(compute) =>
            for (a <- compute.assumes) {
              assumes = assumes :+ a.exp
            }
          case _ =>
        }
        assumes = assumes ++ integrationExps(symbolTable.getThreadById(compPath), info, T)
      case _ =>
    }

    return VC(
      kind = VCKind.PreAssert,
      premises = preAsserts,
      conclusion = assumes,
      writeSetOpt = None(),
      source = VCSource(
        transitionIdx = Some(transIdx),
        componentOpt = Some(compRef.component),
        mhipPairOpt = None()))
  }

  // A compute case's contribution to the component's verified postcondition is the
  // implication `assume ==> guarantee` -- the component only establishes the guarantee
  // when the case's assume held in the pre-state. Using the bare guarantee as a VC
  // premise would assume more than component contract conformance establishes.
  @strictpure def caseToExp(c: GclCaseStatement): org.sireum.lang.ast.Exp = c.assumes match {
    case Some(a) => org.sireum.lang.ast.Exp.Binary(
      left = a,
      op = org.sireum.lang.ast.Exp.BinaryOp.CondImply,
      right = c.guarantees,
      attr = org.sireum.lang.ast.ResolvedAttr(
        posOpt = c.posOpt,
        resOpt = Some(org.sireum.lang.ast.ResolvedInfo.BuiltIn(
          kind = org.sireum.lang.ast.ResolvedInfo.BuiltIn.Kind.BinaryCondImply,
          defPosOpt = None())),
        typedOpt = Some(org.sireum.lang.ast.Typed.b)),
      opPosOpt = c.posOpt)
    case _ => c.guarantees
  }

  @pure def generateNextAssertTaskVC(transIdx: Z,
                                     t: Transition,
                                     compRef: GclScheduleComponentRef,
                                     placeMap: Map[PlaceId, PlaceInfo],
                                     compPath: IdPath,
                                     writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                     symbolTable: SymbolTable): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, placeMap)

    var postConditions: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    getGclInfoOpt(compPath, symbolTable) match {
      case Some(info) =>
        info.annex.compute match {
          case Some(compute) =>
            for (g <- compute.guarantees) {
              postConditions = postConditions :+ g.exp
            }
            for (c <- compute.cases) {
              postConditions = postConditions :+ caseToExp(c)
            }
          case _ =>
        }
        postConditions = postConditions ++ integrationExps(symbolTable.getThreadById(compPath), info, F)
      case _ =>
    }

    val postAsserts = collectPlaceAsserts(t.outPlaces, placeMap)

    return VC(
      kind = VCKind.NextAssertTask,
      premises = preAsserts ++ postConditions,
      conclusion = postAsserts,
      writeSetOpt = writeSets.get(compPath),
      source = VCSource(
        transitionIdx = Some(transIdx),
        componentOpt = Some(compRef.component),
        mhipPairOpt = None()))
  }

  @pure def generateNextAssertSkipVC(transIdx: Z,
                                     t: Transition,
                                     placeMap: Map[PlaceId, PlaceInfo]): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, placeMap)
    val postAsserts = collectPlaceAsserts(t.outPlaces, placeMap)

    return VC(
      kind = VCKind.NextAssertSkip,
      premises = preAsserts,
      conclusion = postAsserts,
      writeSetOpt = None(),
      source = VCSource(
        transitionIdx = Some(transIdx),
        componentOpt = None(),
        mhipPairOpt = None()))
  }

  @pure def generatePostPreVC(nextRel: NextRelResult,
                               placeMap: Map[PlaceId, PlaceInfo]): VC = {
    val endAsserts = collectPlaceAsserts(ISZ(nextRel.endPlace), placeMap)
    val startAsserts = collectPlaceAsserts(ISZ(nextRel.startPlace), placeMap)

    return VC(
      kind = VCKind.PostPre,
      premises = endAsserts,
      conclusion = startAsserts,
      writeSetOpt = None(),
      source = VCSource(
        transitionIdx = None(),
        componentOpt = None(),
        mhipPairOpt = None()))
  }

  // For a component transition, returns its component ref and write set; None for a
  // control-point transition (which has no component contract or write frame).
  @pure def transitionComponentInfo(t: Transition,
                                    nextRel: NextRelResult,
                                    writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                    resolvedAliasMap: Map[String, IdPath]):
                                    Option[(GclScheduleComponentRef, Option[WriteFrameBuilder.ComponentWriteSet])] = {
    if (t.kind != TransitionKind.Component || t.inPlaces.isEmpty) {
      return None()
    }
    nextRel.activationMap.get(t.inPlaces(0)) match {
      case Some(compRef) =>
        return Some((compRef, writeSets.get(resolveCompPath(compRef, resolvedAliasMap))))
      case _ => return None()
    }
  }

  // Generates the independence VCs for one MHIP pair of transitions (identified by their
  // indices into `nextRel.transitions`), matching the Isabelle `independent` =
  // `execIndependent ∧ nonBlocking ∧ nonContradictPost`. Each sub-property carries the
  // activation guards from the formalization, so only the non-trivial obligations are
  // emitted:
  //   - NonBlocking / NonContradictPost (Preservation): one directed VC per pair member
  //     that is a component transition (the direction in which that component fires).
  //   - Commutativity (execIndependent): a single VC, only when BOTH members are
  //     component transitions (symmetric, so one suffices).
  // Consequently a (component, component) pair yields 5 VCs (2 + 2 + 1), a
  // (component, control-point) pair yields 2 (1 + 1), and a (control-point, control-point)
  // pair yields 0 (trivially independent).
  @pure def generateIndependenceVCs(i1: Z,
                                    i2: Z,
                                    placeMap: Map[PlaceId, PlaceInfo],
                                    nextRel: NextRelResult,
                                    writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                    resolvedAliasMap: Map[String, IdPath]): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()
    val t1 = nextRel.transitions(i1)
    val t2 = nextRel.transitions(i2)
    val mhipSource = (i1, i2)

    val t1Pre = collectPlaceAsserts(t1.inPlaces, placeMap)
    val t1Post = collectPlaceAsserts(t1.outPlaces, placeMap)
    val t2Pre = collectPlaceAsserts(t2.inPlaces, placeMap)
    val t2Post = collectPlaceAsserts(t2.outPlaces, placeMap)

    val info1 = transitionComponentInfo(t1, nextRel, writeSets, resolvedAliasMap)
    val info2 = transitionComponentInfo(t2, nextRel, writeSets, resolvedAliasMap)

    // NonBlocking: firing a component must preserve the other transition's pre-assertions.
    // Direction guarded by "the firing transition is a component" (Isabelle nonBlocking).
    info1 match {
      case Some((c1, ws1)) =>
        vcs = vcs :+ VC(
          kind = VCKind.NonBlocking,
          premises = t1Pre ++ t2Pre,
          conclusion = t2Pre,
          writeSetOpt = ws1,
          source = VCSource(transitionIdx = None(), componentOpt = Some(c1.component), mhipPairOpt = Some(mhipSource)))
      case _ =>
    }
    info2 match {
      case Some((c2, ws2)) =>
        vcs = vcs :+ VC(
          kind = VCKind.NonBlocking,
          premises = t1Pre ++ t2Pre,
          conclusion = t1Pre,
          writeSetOpt = ws2,
          source = VCSource(transitionIdx = None(), componentOpt = Some(c2.component), mhipPairOpt = Some(mhipSource)))
      case _ =>
    }

    // Preservation (NonContradictPost): firing a component must not contradict the other
    // transition's post-assertions. Same per-direction component guard.
    info1 match {
      case Some((c1, ws1)) =>
        vcs = vcs :+ VC(
          kind = VCKind.Preservation,
          premises = t1Pre ++ t2Post,
          conclusion = t2Post,
          writeSetOpt = ws1,
          source = VCSource(transitionIdx = None(), componentOpt = Some(c1.component), mhipPairOpt = Some(mhipSource)))
      case _ =>
    }
    info2 match {
      case Some((c2, ws2)) =>
        vcs = vcs :+ VC(
          kind = VCKind.Preservation,
          premises = t1Post ++ t2Pre,
          conclusion = t1Post,
          writeSetOpt = ws2,
          source = VCSource(transitionIdx = None(), componentOpt = Some(c2.component), mhipPairOpt = Some(mhipSource)))
      case _ =>
    }

    // Execution independence / commutativity (Isabelle execIndependent): under both
    // transitions' pre-assertions, firing t1 then t2 yields the same system state as firing
    // t2 then t1. The obligation is symmetric, so a single VC per pair suffices, and its
    // activation guard requires BOTH members to be component transitions. The conclusion is
    // a state-equality rather than a GUMBO assertion, so `conclusion` is empty: the
    // serializer synthesizes the goal by abstracting each component's action as
    // uninterpreted functions over its read scope. That encoding discharges automatically
    // only when each component's write set is disjoint from the other's write set AND read
    // scope (Bernstein's conditions -- write-set disjointness alone is insufficient, and a
    // frame-only encoding is unprovable even for disjoint pairs; see FormalizationIssues.md
    // in the hamr-system-reasoning-prototype repo). `writeSetOpt` carries t1's frame; t2's
    // frame is resolved from `mhipPairOpt`.
    (info1, info2) match {
      case (Some((c1, ws1)), Some((_, _))) =>
        vcs = vcs :+ VC(
          kind = VCKind.Commutativity,
          premises = t1Pre ++ t2Pre,
          conclusion = ISZ(),
          writeSetOpt = ws1,
          source = VCSource(transitionIdx = None(), componentOpt = Some(c1.component), mhipPairOpt = Some(mhipSource)))
      case _ =>
    }

    return vcs
  }
}
