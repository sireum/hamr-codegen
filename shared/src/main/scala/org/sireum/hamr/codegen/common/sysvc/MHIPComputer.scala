// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.sysvc.ScheduleNextRel._

object MHIPComputer {

  // Transition-level MHIP, matching the Isabelle `MHIP_assumption`: two distinct
  // transitions may happen in parallel iff there is a reachable schedule marking in
  // which both are fireable. Returns unordered transition-index pairs (i < j) into
  // `nextRel.transitions`. The relation includes control-point transitions, so the
  // generator can emit the non-trivial (component, control-point) independence
  // obligations the soundness proof requires. The reachable marking graph is explored
  // by a BFS over the Petri net; for the schema sizes seen in practice the state
  // space is small (tens of markings). The relation derives from the schema alone --
  // it is shared by all of a composition's properties.
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
}
