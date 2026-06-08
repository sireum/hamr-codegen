// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.ir.{GclSchedule, GclScheduleAssert, GclScheduleComponentRef,
  GclScheduleElement, GclScheduleSplitJoin}

object ScheduleNextRel {

  @datatype class PlaceId(val name: String)

  @datatype class Transition(val inPlaces: ISZ[PlaceId],
                              val outPlaces: ISZ[PlaceId],
                              val kind: TransitionKind.Type)

  @enum object TransitionKind {
    "Component"
    "ControlPoint"
  }

  @datatype class PlaceInfo(val placeId: PlaceId,
                            val assertOpt: Option[GclScheduleAssert])

  @datatype class NextRelResult(val transitions: ISZ[Transition],
                                val places: ISZ[PlaceInfo],
                                val activationMap: Map[PlaceId, GclScheduleComponentRef],
                                val startPlace: PlaceId,
                                val endPlace: PlaceId)

  @pure def build(schedule: GclSchedule): NextRelResult = {
    var transitions: ISZ[Transition] = ISZ()
    var places: ISZ[PlaceInfo] = ISZ()
    var activationMap: Map[PlaceId, GclScheduleComponentRef] = Map.empty
    var counter: Z = 0

    def freshPlace(prefix: String): PlaceId = {
      counter = counter + 1
      return PlaceId(s"${prefix}_$counter")
    }

    def addPlace(id: PlaceId, assertOpt: Option[GclScheduleAssert]): Unit = {
      places = places :+ PlaceInfo(placeId = id, assertOpt = assertOpt)
    }

    // Determines the out-place for a component or split-join transition.
    // If the next element is an assert, use its named place directly (the
    // thesis model puts one place between every two transitions). Otherwise
    // create a fresh implicit place named after the preceding element.
    def resolvePostPlace(elements: ISZ[GclScheduleElement], nextIdx: Z,
                         contextName: String): (PlaceId, B) = {
      if (nextIdx < elements.size) {
        elements(nextIdx) match {
          case a: GclScheduleAssert =>
            val p = PlaceId(a.id)
            addPlace(p, Some(a))
            return (p, T)
          case _ =>
        }
      }
      val p = freshPlace(s"post_$contextName")
      addPlace(p, None())
      return (p, F)
    }

    def firstElementLabel(elements: ISZ[GclScheduleElement]): String = {
      if (elements.nonEmpty) {
        elements(0) match {
          case c: GclScheduleComponentRef => return getComponentName(c)
          case a: GclScheduleAssert => return a.id
          case _: GclScheduleSplitJoin => return "split"
          case _ =>
        }
      }
      return "seq"
    }

    def processElements(elements: ISZ[GclScheduleElement], entryPlace: PlaceId): PlaceId = {
      var currentPlace = entryPlace
      var i: Z = 0
      while (i < elements.size) {
        val element = elements(i)
        element match {
          case a: GclScheduleAssert =>
            // If this assert was already consumed as a component's post-place,
            // currentPlace already points to it. Otherwise we need a
            // pass-through control-point transition.
            if (currentPlace.name != a.id) {
              val assertPlace = PlaceId(a.id)
              addPlace(assertPlace, Some(a))
              transitions = transitions :+ Transition(
                inPlaces = ISZ(currentPlace),
                outPlaces = ISZ(assertPlace),
                kind = TransitionKind.ControlPoint)
              currentPlace = assertPlace
            }

          case c: GclScheduleComponentRef =>
            val componentName = getComponentName(c)
            val prePlace = currentPlace
            val pair = resolvePostPlace(elements, i + 1, componentName)
            val postPlace = pair._1
            val consumed = pair._2
            transitions = transitions :+ Transition(
              inPlaces = ISZ(prePlace),
              outPlaces = ISZ(postPlace),
              kind = TransitionKind.Component)
            activationMap = activationMap + prePlace ~> c
            currentPlace = postPlace
            if (consumed) {
              i = i + 1
            }

          case sj: GclScheduleSplitJoin =>
            val splitInPlace = currentPlace
            var branchEndPlaces: ISZ[PlaceId] = ISZ()
            var branchStartPlaces: ISZ[PlaceId] = ISZ()

            for (seq <- sj.sequences) {
              val label = firstElementLabel(seq.elements)
              val branchStart = freshPlace(s"pre_$label")
              addPlace(branchStart, None())
              branchStartPlaces = branchStartPlaces :+ branchStart

              val branchEnd = processElements(seq.elements, branchStart)
              branchEndPlaces = branchEndPlaces :+ branchEnd
            }

            transitions = transitions :+ Transition(
              inPlaces = ISZ(splitInPlace),
              outPlaces = branchStartPlaces,
              kind = TransitionKind.ControlPoint)

            // If a single assert follows the split, use it as the join
            // out-place directly
            val joinPair = resolvePostPlace(elements, i + 1, "join")
            val joinOutPlace = joinPair._1
            val joinConsumed = joinPair._2

            transitions = transitions :+ Transition(
              inPlaces = branchEndPlaces,
              outPlaces = ISZ(joinOutPlace),
              kind = TransitionKind.ControlPoint)

            currentPlace = joinOutPlace
            if (joinConsumed) {
              i = i + 1
            }

          case _ =>
        }
        i = i + 1
      }
      return currentPlace
    }

    val startPlace = PlaceId("START")
    addPlace(startPlace, None())

    val lastPlace = processElements(schedule.elements, startPlace)

    val endPlace = PlaceId("END")
    if (lastPlace.name != "END") {
      addPlace(endPlace, None())
      transitions = transitions :+ Transition(
        inPlaces = ISZ(lastPlace),
        outPlaces = ISZ(endPlace),
        kind = TransitionKind.ControlPoint)
    }

    return NextRelResult(
      transitions = transitions,
      places = places,
      activationMap = activationMap,
      startPlace = startPlace,
      endPlace = endPlace)
  }

  @pure def getComponentName(c: GclScheduleComponentRef): String = {
    val names = c.component.name
    if (names.nonEmpty) {
      return names(names.lastIndex)
    }
    return "unknown"
  }

  @pure def fireTransition(ready: Set[PlaceId], t: Transition): Set[PlaceId] = {
    var result = ready
    for (p <- t.inPlaces) {
      result = result - p
    }
    for (p <- t.outPlaces) {
      result = result + p
    }
    return result
  }

  @pure def isEnabled(ready: Set[PlaceId], t: Transition): B = {
    for (p <- t.inPlaces) {
      if (!ready.contains(p)) {
        return F
      }
    }
    return T
  }

  // Fires all enabled control-point transitions (splits, joins,
  // pass-throughs) until no more are enabled. Returns the accumulated
  // set of all places that appeared in any intermediate marking —
  // the runtime monitor checks assertions for all of these, matching
  // the "collapsed walk" from the hand-written monitor.
  @pure def cascadeControlPoints(ready: Set[PlaceId],
                                  transitions: ISZ[Transition]): (Set[PlaceId], Set[PlaceId]) = {
    var current = ready
    var accumulated = ready
    var changed: B = T
    while (changed) {
      changed = F
      for (t <- transitions) {
        if (t.kind == TransitionKind.ControlPoint && isEnabled(current, t)) {
          current = fireTransition(current, t)
          for (p <- current.elements) {
            accumulated = accumulated + p
          }
          changed = T
        }
      }
    }
    return (current, accumulated)
  }

  @pure def findComponentTransition(ready: Set[PlaceId],
                                     componentRef: GclScheduleComponentRef,
                                     nextRel: NextRelResult): Option[Transition] = {
    for (t <- nextRel.transitions) {
      if (t.kind == TransitionKind.Component && isEnabled(ready, t)) {
        t.inPlaces match {
          case ISZ(inPlace) =>
            nextRel.activationMap.get(inPlace) match {
              case Some(ref) if getComponentName(ref) == getComponentName(componentRef) =>
                return Some(t)
              case _ =>
            }
          case _ =>
        }
      }
    }
    return None()
  }

  @pure def getActiveAssertions(places: Set[PlaceId],
                                 nextRel: NextRelResult): ISZ[GclScheduleAssert] = {
    var result: ISZ[GclScheduleAssert] = ISZ()
    for (pi <- nextRel.places) {
      if (places.contains(pi.placeId)) {
        pi.assertOpt match {
          case Some(a) => result = result :+ a
          case _ =>
        }
      }
    }
    return result
  }
}
