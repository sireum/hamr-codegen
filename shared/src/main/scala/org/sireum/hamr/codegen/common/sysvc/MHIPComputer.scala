// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.sysvc.ScheduleNextRel._
import org.sireum.hamr.ir.{GclSchedule, GclScheduleComponentRef, GclScheduleElement,
  GclScheduleSequence, GclScheduleSplitJoin}

object MHIPComputer {

  // Transition-level MHIP, matching the Isabelle `MHIP_assumption`: two distinct
  // transitions may happen in parallel iff there is a reachable schedule marking in
  // which both are fireable. Returns unordered transition-index pairs (i < j) into
  // `nextRel.transitions`.
  //
  // Unlike `compute` (which only relates component transitions from the schedule AST),
  // this includes control-point transitions, so the generator can emit the non-trivial
  // (component, control-point) independence obligations the soundness proof requires.
  // The reachable marking graph is explored by a BFS over the Petri net; for the schedule
  // sizes seen in practice the state space is small (tens of markings).
  @pure def computeMHIP(nextRel: NextRelResult): ISZ[(Z, Z)] = {
    val transitions = nextRel.transitions
    val initial: Set[PlaceId] = Set.empty[PlaceId] + nextRel.startPlace
    var visited: Set[Set[PlaceId]] = Set.empty
    var worklist: ISZ[Set[PlaceId]] = ISZ(initial)
    var head: Z = 0
    var pairs: HashSSet[(Z, Z)] = HashSSet.empty
    while (head < worklist.size) {
      val marking = worklist(head)
      head = head + 1
      if (!visited.contains(marking)) {
        visited = visited + marking
        var enabledIdx: ISZ[Z] = ISZ()
        for (i <- z"0" until transitions.size) {
          if (isEnabled(marking, transitions(i))) {
            enabledIdx = enabledIdx :+ i
          }
        }
        for (a <- z"0" until enabledIdx.size) {
          for (b <- a + 1 until enabledIdx.size) {
            pairs = pairs + ((enabledIdx(a), enabledIdx(b)))
          }
        }
        for (i <- enabledIdx) {
          val next = fireTransition(marking, transitions(i))
          if (!visited.contains(next)) {
            worklist = worklist :+ next
          }
        }
      }
    }
    return pairs.elements
  }

  @pure def compute(schedule: GclSchedule): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    return collectFromElements(schedule.elements)
  }

  @pure def collectFromElements(elements: ISZ[GclScheduleElement]): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    var pairs: ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = ISZ()
    for (e <- elements) {
      e match {
        case sj: GclScheduleSplitJoin =>
          pairs = pairs ++ collectFromSplitJoin(sj)
        case _ =>
      }
    }
    return pairs
  }

  @pure def collectFromSplitJoin(sj: GclScheduleSplitJoin): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    var pairs: ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = ISZ()

    val branchComponents: ISZ[ISZ[GclScheduleComponentRef]] =
      for (seq <- sj.sequences) yield collectComponents(seq)

    for (i <- z"0" until branchComponents.size) {
      for (j <- i + 1 until branchComponents.size) {
        for (c1 <- branchComponents(i)) {
          for (c2 <- branchComponents(j)) {
            pairs = pairs :+ ((c1, c2))
          }
        }
      }
    }

    for (seq <- sj.sequences) {
      pairs = pairs ++ collectFromElements(seq.elements)
    }

    return pairs
  }

  @pure def collectComponents(seq: GclScheduleSequence): ISZ[GclScheduleComponentRef] = {
    var result: ISZ[GclScheduleComponentRef] = ISZ()
    for (e <- seq.elements) {
      e match {
        case c: GclScheduleComponentRef =>
          result = result :+ c
        case sj: GclScheduleSplitJoin =>
          for (inner <- sj.sequences) {
            result = result ++ collectComponents(inner)
          }
        case _ =>
      }
    }
    return result
  }
}
