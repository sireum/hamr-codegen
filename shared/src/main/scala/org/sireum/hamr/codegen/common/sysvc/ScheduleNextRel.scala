// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.ir.{GclComposition, GclCompositionProperty, GclPointAfter, GclPointAt, GclPointBefore,
  GclPointEnd, GclPointStart, GclPropertyBinding, GclSchemaComponentRef, GclSchemaElement, GclSchemaLabel,
  GclSchemaPoint, GclSchemaSplitJoin}
import org.sireum.message.Reporter

// Builds the Petri net of a composition's *schema* (design D8 of
// VCGenerationDesign.md in the hamr-system-reasoning-prototype repo). The net
// carries no assertions: a composition's properties are separate decorations of
// the net's places, resolved per property by `decorate`. Point names are
// stable: a place is reachable through any of its synonyms (`before`/`after` a
// component occurrence, a `label`, START/END), and consecutive elements share a
// place, so e.g. `after mrm` and `before mhs` resolve to the same PlaceId.
object ScheduleNextRel {

  @datatype class PlaceId(val name: String)

  @datatype class Transition(val inPlaces: ISZ[PlaceId],
                              val outPlaces: ISZ[PlaceId],
                              val kind: TransitionKind.Type)

  @enum object TransitionKind {
    "Component"
    "ControlPoint"
  }

  @datatype class NextRelResult(val transitions: ISZ[Transition],
                                val places: ISZ[PlaceId],
                                val activationMap: Map[PlaceId, GclSchemaComponentRef],
                                val startPlace: PlaceId,
                                val endPlace: PlaceId,
                                // point-name resolution: place labels (`label <id>;`)
                                val labelMap: Map[String, PlaceId],
                                // `before <occ>` / `after <occ>` where <occ> is an
                                // occurrence label or the alias of a unique occurrence
                                val beforeMap: Map[String, PlaceId],
                                val afterMap: Map[String, PlaceId])

  @pure def getComponentName(c: GclSchemaComponentRef): String = {
    val names = c.component.name
    if (names.nonEmpty) {
      return names(names.lastIndex)
    }
    return "unknown"
  }

  // The occurrence's identity: its occurrence label when present (required by
  // the resolver for multi-firing components), otherwise its alias. Used for
  // point names and for naming the occurrence's generated proof fns.
  @pure def occurrenceId(c: GclSchemaComponentRef): String = {
    c.occurrenceLabelOpt match {
      case Some(l) => return l
      case _ => return getComponentName(c)
    }
  }

  @pure def countOccurrences(elements: ISZ[GclSchemaElement], acc: Map[String, Z]): Map[String, Z] = {
    var m = acc
    for (e <- elements) {
      e match {
        case c: GclSchemaComponentRef =>
          val key = getComponentName(c)
          m = m + key ~> (m.getOrElse(key, z"0") + 1)
        case sj: GclSchemaSplitJoin =>
          for (branch <- sj.branches) {
            m = countOccurrences(branch.elements, m)
          }
        case _ =>
      }
    }
    return m
  }

  @pure def build(composition: GclComposition): NextRelResult = {
    var transitions: ISZ[Transition] = ISZ()
    var places: ISZ[PlaceId] = ISZ()
    var activationMap: Map[PlaceId, GclSchemaComponentRef] = Map.empty
    var labelMap: Map[String, PlaceId] = Map.empty
    var beforeMap: Map[String, PlaceId] = Map.empty
    var afterMap: Map[String, PlaceId] = Map.empty
    var counter: Z = 0

    // a component alias may serve as an occurrence id only when the component
    // fires once per hyperperiod (the resolver errors on unlabeled multi-firing
    // occurrences, so this is belt-and-suspenders)
    val occurrenceCounts = countOccurrences(composition.schema, Map.empty)

    def addPlace(id: PlaceId): PlaceId = {
      places = places :+ id
      return id
    }

    def freshPlace(prefix: String): PlaceId = {
      counter = counter + 1
      return addPlace(PlaceId(s"${prefix}_$counter"))
    }

    def registerOccurrencePlaces(c: GclSchemaComponentRef, prePlace: PlaceId, postPlace: PlaceId): Unit = {
      c.occurrenceLabelOpt match {
        case Some(l) =>
          beforeMap = beforeMap + l ~> prePlace
          afterMap = afterMap + l ~> postPlace
        case _ =>
      }
      val alias = getComponentName(c)
      if (occurrenceCounts.getOrElse(alias, z"0") == 1) {
        beforeMap = beforeMap + alias ~> prePlace
        afterMap = afterMap + alias ~> postPlace
      }
    }

    // Canonical name for a branch-entry place: `before_<occId>` of the first
    // component past any leading labels, else a fresh anonymous name. (The
    // leading labels and the first component's before-name register as
    // synonyms of this place during processing.)
    def branchEntryPlace(branchElements: ISZ[GclSchemaElement]): PlaceId = {
      for (e <- branchElements) {
        e match {
          case c: GclSchemaComponentRef => return addPlace(PlaceId(s"before_${occurrenceId(c)}"))
          case _: GclSchemaLabel =>
          case _ => return freshPlace("pre_split")
        }
      }
      return freshPlace("pre_seq")
    }

    def processElements(elements: ISZ[GclSchemaElement], entryPlace: PlaceId): PlaceId = {
      var currentPlace = entryPlace
      for (element <- elements) {
        element match {
          case l: GclSchemaLabel =>
            // purely naming: the label is a synonym of the place at this
            // position -- no place or transition is created
            labelMap = labelMap + l.id ~> currentPlace

          case c: GclSchemaComponentRef =>
            val prePlace = currentPlace
            val postPlace = addPlace(PlaceId(s"after_${occurrenceId(c)}"))
            registerOccurrencePlaces(c, prePlace, postPlace)
            transitions = transitions :+ Transition(
              inPlaces = ISZ(prePlace),
              outPlaces = ISZ(postPlace),
              kind = TransitionKind.Component)
            activationMap = activationMap + prePlace ~> c
            currentPlace = postPlace

          case sj: GclSchemaSplitJoin =>
            val splitInPlace = currentPlace
            var branchStartPlaces: ISZ[PlaceId] = ISZ()
            var branchEndPlaces: ISZ[PlaceId] = ISZ()

            // each branch's entry place IS the split transition's out-place for
            // that branch -- no synthetic pass-through places (finding F1 of
            // InitialVerusRunFindings.md in the hamr-system-reasoning-prototype
            // repo: an unannotatable hop would drop carried invariants)
            for (branch <- sj.branches) {
              val branchStart = branchEntryPlace(branch.elements)
              branchStartPlaces = branchStartPlaces :+ branchStart
              val branchEnd = processElements(branch.elements, branchStart)
              branchEndPlaces = branchEndPlaces :+ branchEnd
            }

            transitions = transitions :+ Transition(
              inPlaces = ISZ(splitInPlace),
              outPlaces = branchStartPlaces,
              kind = TransitionKind.ControlPoint)

            val joinOutPlace = freshPlace("post_join")
            transitions = transitions :+ Transition(
              inPlaces = branchEndPlaces,
              outPlaces = ISZ(joinOutPlace),
              kind = TransitionKind.ControlPoint)

            currentPlace = joinOutPlace

          case _ => halt(s"Unexpected schema element: $element")
        }
      }
      return currentPlace
    }

    val startPlace = addPlace(PlaceId("START"))

    val lastPlace = processElements(composition.schema, startPlace)

    // the hyperperiod's END place, fed by a final control point so the Post-Pre
    // VC (END |- START) is anchored on a dedicated place
    val endPlace = addPlace(PlaceId("END"))
    transitions = transitions :+ Transition(
      inPlaces = ISZ(lastPlace),
      outPlaces = ISZ(endPlace),
      kind = TransitionKind.ControlPoint)

    return NextRelResult(
      transitions = transitions,
      places = places,
      activationMap = activationMap,
      startPlace = startPlace,
      endPlace = endPlace,
      labelMap = labelMap,
      beforeMap = beforeMap,
      afterMap = afterMap)
  }

  // Resolves a property's point reference to a place of the net.
  @pure def resolvePoint(point: GclSchemaPoint, nextRel: NextRelResult): Option[PlaceId] = {
    point match {
      case _: GclPointStart => return Some(nextRel.startPlace)
      case _: GclPointEnd => return Some(nextRel.endPlace)
      case p: GclPointAt => return nextRel.labelMap.get(p.label)
      case p: GclPointBefore => return nextRel.beforeMap.get(p.occurrence)
      case p: GclPointAfter => return nextRel.afterMap.get(p.occurrence)
      case _ => halt(s"Unexpected schema point: $point")
    }
  }

  // Resolves one property's bindings against the net: place -> binding. A
  // place may be bound through only one of its names (consecutive elements
  // share a place, so `after mrm`, `before mhs`, and a label there are
  // synonyms); binding it twice within one property is an error.
  @pure def decorate(nextRel: NextRelResult,
               property: GclCompositionProperty,
               reporter: Reporter): Map[PlaceId, GclPropertyBinding] = {
    var result: Map[PlaceId, GclPropertyBinding] = Map.empty
    for (b <- property.bindings) {
      resolvePoint(b.point, nextRel) match {
        case Some(pid) =>
          if (result.contains(pid)) {
            reporter.error(b.posOpt, "ScheduleNextRel",
              s"Property '${property.id}' binds place '${pid.name}' more than once (the point '${b.point.prettyST.render}' is a synonym of an already-bound point)")
          } else {
            result = result + pid ~> b
          }
        case _ =>
          reporter.error(b.posOpt, "ScheduleNextRel",
            s"Property '${property.id}': point '${b.point.prettyST.render}' does not name a point of the schema (unknown label or occurrence, or an ambiguous multi-firing alias)")
      }
    }
    return result
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
                                     componentRef: GclSchemaComponentRef,
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

  // The bindings of a decoration whose places are in the given marking (used by
  // the runtime monitor: one observation, per-property checks).
  @pure def getActiveBindings(placeSet: Set[PlaceId],
                              decoration: Map[PlaceId, GclPropertyBinding]): ISZ[(PlaceId, GclPropertyBinding)] = {
    var result: ISZ[(PlaceId, GclPropertyBinding)] = ISZ()
    for (e <- decoration.entries) {
      if (placeSet.contains(e._1)) {
        result = result :+ e
      }
    }
    return result
  }
}
