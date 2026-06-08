// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.{GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.sysvc.ScheduleNextRel._
import org.sireum.hamr.ir.{GclSchedule, GclScheduleComponentRef}

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

    val mhipPairs = MHIPComputer.compute(schedule)
    for (pair <- mhipPairs) {
      vcs = vcs ++ generateIndependenceVCs(pair, placeMap, nextRel, writeSets, resolvedComponentAliasMap, symbolTable)
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
              postConditions = postConditions :+ c.guarantees
            }
          case _ =>
        }
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

  @pure def generateIndependenceVCs(pair: (GclScheduleComponentRef, GclScheduleComponentRef),
                                    placeMap: Map[PlaceId, PlaceInfo],
                                    nextRel: NextRelResult,
                                    writeSets: Map[IdPath, WriteFrameBuilder.ComponentWriteSet],
                                    resolvedAliasMap: Map[String, IdPath],
                                    symbolTable: SymbolTable): ISZ[VC] = {
    var vcs: ISZ[VC] = ISZ()
    val c1 = pair._1
    val c2 = pair._2
    val mhipSource = (c1.component, c2.component)

    val c1PreOpt = findComponentPreAsserts(c1, nextRel, placeMap)
    val c2PreOpt = findComponentPreAsserts(c2, nextRel, placeMap)
    val c1PostOpt = findComponentPostAsserts(c1, nextRel, placeMap)
    val c2PostOpt = findComponentPostAsserts(c2, nextRel, placeMap)

    val c1Pre: ISZ[org.sireum.lang.ast.Exp] = c1PreOpt.getOrElse(ISZ())
    val c2Pre: ISZ[org.sireum.lang.ast.Exp] = c2PreOpt.getOrElse(ISZ())
    val c1Post: ISZ[org.sireum.lang.ast.Exp] = c1PostOpt.getOrElse(ISZ())
    val c2Post: ISZ[org.sireum.lang.ast.Exp] = c2PostOpt.getOrElse(ISZ())

    val ws1 = writeSets.get(resolveCompPath(c1, resolvedAliasMap))
    val ws2 = writeSets.get(resolveCompPath(c2, resolvedAliasMap))

    vcs = vcs :+ VC(
      kind = VCKind.NonBlocking,
      premises = c1Pre ++ c2Pre,
      conclusion = c2Pre,
      writeSetOpt = ws1,
      source = VCSource(
        transitionIdx = None(),
        componentOpt = Some(c1.component),
        mhipPairOpt = Some(mhipSource)))

    vcs = vcs :+ VC(
      kind = VCKind.NonBlocking,
      premises = c1Pre ++ c2Pre,
      conclusion = c1Pre,
      writeSetOpt = ws2,
      source = VCSource(
        transitionIdx = None(),
        componentOpt = Some(c2.component),
        mhipPairOpt = Some(mhipSource)))

    vcs = vcs :+ VC(
      kind = VCKind.Preservation,
      premises = c1Pre ++ c2Post,
      conclusion = c2Post,
      writeSetOpt = ws1,
      source = VCSource(
        transitionIdx = None(),
        componentOpt = Some(c1.component),
        mhipPairOpt = Some(mhipSource)))

    vcs = vcs :+ VC(
      kind = VCKind.Preservation,
      premises = c1Post ++ c2Pre,
      conclusion = c1Post,
      writeSetOpt = ws2,
      source = VCSource(
        transitionIdx = None(),
        componentOpt = Some(c2.component),
        mhipPairOpt = Some(mhipSource)))

    return vcs
  }

  @pure def findComponentPreAsserts(compRef: GclScheduleComponentRef,
                                    nextRel: NextRelResult,
                                    placeMap: Map[PlaceId, PlaceInfo]): Option[ISZ[org.sireum.lang.ast.Exp]] = {
    for (entry <- nextRel.activationMap.entries) {
      if (entry._2.component == compRef.component) {
        val exps = collectPlaceAsserts(ISZ(entry._1), placeMap)
        return Some(exps)
      }
    }
    return None()
  }

  @pure def findComponentPostAsserts(compRef: GclScheduleComponentRef,
                                     nextRel: NextRelResult,
                                     placeMap: Map[PlaceId, PlaceInfo]): Option[ISZ[org.sireum.lang.ast.Exp]] = {
    for (i <- z"0" until nextRel.transitions.size) {
      val t = nextRel.transitions(i)
      nextRel.activationMap.get(t.inPlaces(0)) match {
        case Some(ref) =>
          if (ref.component == compRef.component) {
            val exps = collectPlaceAsserts(t.outPlaces, placeMap)
            return Some(exps)
          }
        case _ =>
      }
    }
    return None()
  }
}
