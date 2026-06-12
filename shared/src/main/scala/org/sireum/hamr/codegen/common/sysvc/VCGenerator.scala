// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.{AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.sysvc.ScheduleNextRel._
import org.sireum.hamr.ir.{GclAssume, GclCaseStatement, GclComposition, GclCompositionProperty,
  GclGuarantee, GclPropertyBinding, GclSchemaComponentRef}

// Generates the system-level VCs of one composition (design D8 of
// VCGenerationDesign.md in the hamr-system-reasoning-prototype repo). The net
// and the MHIP relation derive from the schema alone and are shared; the
// sequential VCs and the Non-Blocking/Preservation VCs are generated per
// property against that property's decoration (place -> binding, from
// `ScheduleNextRel.decorate`); the Commutativity VCs are assertion-free state
// equalities generated once per composition.
object VCGenerator {

  @pure def hasCompositions(symbolTable: SymbolTable): B = {
    return getCompositions(symbolTable).nonEmpty
  }

  @pure def getCompositions(symbolTable: SymbolTable): ISZ[GclComposition] = {
    var result: ISZ[GclComposition] = ISZ()
    symbolTable.annexClauseInfos.get(symbolTable.rootSystem.path) match {
      case Some(infos) =>
        for (info <- infos) {
          info match {
            case gclInfo: GclAnnexClauseInfo =>
              result = result ++ gclInfo.annex.compositions
            case _ =>
          }
        }
      case _ =>
    }
    return result
  }

  @pure def resolveCompPath(compRef: GclSchemaComponentRef,
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

  // Generates one property's VC set: Init-State, Pre-Assert, Next-Assert
  // (task and control point), Post-Pre, and the per-pair Non-Blocking and
  // Preservation VCs (whose conclusions mention the decoration's assertions).
  @pure def generateForProperty(decoration: Map[PlaceId, GclPropertyBinding],
                                nextRel: NextRelResult,
                                mhipPairs: ISZ[(Z, Z)],
                                resolvedComponentAliasMap: Map[String, IdPath],
                                symbolTable: SymbolTable): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()

    val threads = symbolTable.getThreads()
    val writeSets = WriteFrameBuilder.buildAll(
      threads = for (t <- threads) yield t,
      symbolTable = symbolTable)

    vcs = vcs :+ generateInitStateVC(decoration, nextRel, symbolTable)

    for (i <- z"0" until nextRel.transitions.size) {
      val t = nextRel.transitions(i)
      nextRel.activationMap.get(t.inPlaces(0)) match {
        case Some(compRef) =>
          val compPath = resolveCompPath(compRef, resolvedComponentAliasMap)
          // Contract projection: the property uses this component's contract iff
          // it binds one of the component's out-places (otherwise its Next-Assert
          // conclusion is trivial and only the write frames are needed). An unused
          // contract is instantiated as (taskPre, taskPost) := (true, true), which
          // trivially satisfies taskConVC and preTaskVC -- so a sparse property
          // need not carry the upstream plumbing that establishes the assumes of
          // components outside its story.
          val covered = coversTransition(t, decoration)
          vcs = vcs :+ generatePreAssertVC(i, t, compRef, decoration, compPath, covered, symbolTable)
          vcs = vcs :+ generateNextAssertTaskVC(i, t, compRef, decoration, compPath, covered, writeSets, symbolTable)
        case _ =>
          vcs = vcs :+ generateNextAssertSkipVC(i, t, decoration)
      }
    }

    vcs = vcs :+ generatePostPreVC(nextRel, decoration)

    for (pair <- mhipPairs) {
      vcs = vcs ++ generateIndependenceVCs(pair._1, pair._2, decoration, nextRel, writeSets, resolvedComponentAliasMap)
    }

    return vcs
  }

  // Commutativity (execIndependent) is a state-equality over the components'
  // frames -- it mentions no assertions, so it is generated once per
  // composition and shared by all of its properties. Premises are empty: the
  // uninterpreted-action encoding discharges by congruence when the pair
  // satisfies Bernstein's conditions, independently of any decoration.
  @pure def generateCommutativityVCs(nextRel: NextRelResult,
                                     mhipPairs: ISZ[(Z, Z)],
                                     resolvedComponentAliasMap: Map[String, IdPath],
                                     symbolTable: SymbolTable): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()
    val threads = symbolTable.getThreads()
    val writeSets = WriteFrameBuilder.buildAll(
      threads = for (t <- threads) yield t,
      symbolTable = symbolTable)
    for (pair <- mhipPairs) {
      val t1 = nextRel.transitions(pair._1)
      val t2 = nextRel.transitions(pair._2)
      val info1 = transitionComponentInfo(t1, nextRel, writeSets, resolvedComponentAliasMap)
      val info2 = transitionComponentInfo(t2, nextRel, writeSets, resolvedComponentAliasMap)
      (info1, info2) match {
        case (Some((c1, ws1)), Some((_, _))) =>
          vcs = vcs :+ VC(
            kind = VCKind.Commutativity,
            premises = ISZ(),
            conclusion = ISZ(),
            writeSetOpt = ws1,
            source = VCSource(transitionIdx = None(), componentOpt = Some(c1.component), mhipPairOpt = Some(pair)))
        case _ =>
      }
    }
    return vcs
  }

  // A property covers a component transition iff it binds one of the
  // transition's out-places, i.e., its Next-Assert conclusion is non-trivial.
  @pure def coversTransition(t: Transition, decoration: Map[PlaceId, GclPropertyBinding]): B = {
    for (p <- t.outPlaces) {
      if (decoration.contains(p)) {
        return T
      }
    }
    return F
  }

  @pure def getAssertExp(placeId: PlaceId,
                         decoration: Map[PlaceId, GclPropertyBinding]): Option[org.sireum.lang.ast.Exp] = {
    decoration.get(placeId) match {
      case Some(b) => return Some(b.exp)
      case _ => return None()
    }
  }

  @pure def collectPlaceAsserts(placeIds: ISZ[PlaceId],
                                decoration: Map[PlaceId, GclPropertyBinding]): ISZ[org.sireum.lang.ast.Exp] = {
    var exps: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    for (pid <- placeIds) {
      getAssertExp(pid, decoration) match {
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

  @pure def generateInitStateVC(decoration: Map[PlaceId, GclPropertyBinding],
                                nextRel: NextRelResult,
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

    val startAsserts = collectPlaceAsserts(ISZ(nextRel.startPlace), decoration)

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

  // `covered = F` instantiates the component's taskPre as `true` (contract
  // projection): the obligation is emitted trivially for uniformity (D5).
  @pure def generatePreAssertVC(transIdx: Z,
                                t: Transition,
                                compRef: GclSchemaComponentRef,
                                decoration: Map[PlaceId, GclPropertyBinding],
                                compPath: IdPath,
                                covered: B,
                                symbolTable: SymbolTable): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, decoration)

    var assumes: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    if (covered) {
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

  // `covered = F` instantiates the component's taskPost as `true` (contract
  // projection): the Next-Assert relies on the write frames alone -- sound
  // without the component's Pre-Assert, since frames hold unconditionally.
  @pure def generateNextAssertTaskVC(transIdx: Z,
                                     t: Transition,
                                     compRef: GclSchemaComponentRef,
                                     decoration: Map[PlaceId, GclPropertyBinding],
                                     compPath: IdPath,
                                     covered: B,
                                     writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                     symbolTable: SymbolTable): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, decoration)

    var postConditions: ISZ[org.sireum.lang.ast.Exp] = ISZ()
    if (covered) {
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
    }

    val postAsserts = collectPlaceAsserts(t.outPlaces, decoration)

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
                                     decoration: Map[PlaceId, GclPropertyBinding]): VC = {
    val preAsserts = collectPlaceAsserts(t.inPlaces, decoration)
    val postAsserts = collectPlaceAsserts(t.outPlaces, decoration)

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
                               decoration: Map[PlaceId, GclPropertyBinding]): VC = {
    val endAsserts = collectPlaceAsserts(ISZ(nextRel.endPlace), decoration)
    val startAsserts = collectPlaceAsserts(ISZ(nextRel.startPlace), decoration)

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
                                    Option[(GclSchemaComponentRef, Option[WriteFrameBuilder.ComponentWriteSet])] = {
    if (t.kind != TransitionKind.Component || t.inPlaces.isEmpty) {
      return None()
    }
    nextRel.activationMap.get(t.inPlaces(0)) match {
      case Some(compRef) =>
        return Some((compRef, writeSets.get(resolveCompPath(compRef, resolvedAliasMap))))
      case _ => return None()
    }
  }

  // Generates the per-property independence VCs for one MHIP pair of transitions
  // (identified by their indices into `nextRel.transitions`), matching the Isabelle
  // `independent` = `execIndependent ∧ nonBlocking ∧ nonContradictPost`. Each
  // sub-property carries the activation guards from the formalization, so only the
  // non-trivial obligations are emitted:
  //   - NonBlocking / NonContradictPost (Preservation): one directed VC per pair member
  //     that is a component transition (the direction in which that component fires).
  // Commutativity (execIndependent) mentions no assertions and is generated once per
  // composition by `generateCommutativityVCs`, not here. Consequently, per property, a
  // (component, component) pair yields 4 VCs (2 + 2), a (component, control-point) pair
  // yields 2 (1 + 1), and a (control-point, control-point) pair yields 0.
  @pure def generateIndependenceVCs(i1: Z,
                                    i2: Z,
                                    decoration: Map[PlaceId, GclPropertyBinding],
                                    nextRel: NextRelResult,
                                    writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                    resolvedAliasMap: Map[String, IdPath]): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()
    val t1 = nextRel.transitions(i1)
    val t2 = nextRel.transitions(i2)
    val mhipSource = (i1, i2)

    val t1Pre = collectPlaceAsserts(t1.inPlaces, decoration)
    val t1Post = collectPlaceAsserts(t1.outPlaces, decoration)
    val t2Pre = collectPlaceAsserts(t2.inPlaces, decoration)
    val t2Post = collectPlaceAsserts(t2.outPlaces, decoration)

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

    return vcs
  }
}
