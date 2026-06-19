// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB

@sig trait MakefileItem

object MakefileUtil {
  val KEY_MainMakefileTarget: String = "KEY_RustMainMakefileTarget"

  type tt = Map[ISZ[String], ISZ[MakefileTarget]]
  type t = MapValue[ISZ[String], ISZ[MakefileTarget]]

  @strictpure def addMakefileTargets(makefilePath: ISZ[String], targets: ISZ[MakefileTarget], store: Store): Store = {
    val existingMakefiles: Map[ISZ[String], ISZ[MakefileTarget]] =
      store.getOrElse(KEY_MainMakefileTarget, MapValue[ISZ[String], ISZ[MakefileTarget]](Map.empty)).asInstanceOf[t].map
    val existingTargets = existingMakefiles.getOrElse(makefilePath, ISZ())
    store + (KEY_MainMakefileTarget ~> MapValue(existingMakefiles + (makefilePath ~> (existingTargets ++ targets))))
  }

  // Returns the makefile's targets with same-named targets merged into one rule.
  // Multiple plugins may contribute to a shared target (e.g. `verus`): merging unions
  // their dependencies (dedup, order-preserving) and concatenates their bodies so the
  // rendered makefile has exactly one rule per target name. Targets with distinct
  // names pass through unchanged, in first-seen order.
  @pure def getMakefileTargets(makefilePath: ISZ[String], store: Store): ISZ[MakefileTarget] = {
    val raw = store.get(KEY_MainMakefileTarget).get.asInstanceOf[t].map.get(makefilePath).get
    var order: ISZ[String] = ISZ()
    var byName: Map[String, MakefileTarget] = Map.empty
    for (target <- raw) {
      byName.get(target.name) match {
        case Some(existing) =>
          var deps = existing.dependencies
          var seenDeps = Set.empty[String] ++ (for (d <- existing.dependencies) yield d.render)
          for (d <- target.dependencies if !seenDeps.contains(d.render)) {
            deps = deps :+ d
            seenDeps = seenDeps + d.render
          }
          byName = byName + target.name ~> existing(
            allowMultiple = existing.allowMultiple || target.allowMultiple,
            dependencies = deps,
            body = existing.body ++ target.body)
        case _ =>
          order = order :+ target.name
          byName = byName + target.name ~> target
      }
    }
    return for (n <- order) yield byName.get(n).get
  }
}


@datatype class MakefileTarget(val name: String,
                               val allowMultiple: B,
                               val dependencies: ISZ[ST],
                               val body: ISZ[ST]) extends MakefileItem {
  @pure def prettyST: ST = {
    return (
    st"""$name:${if (allowMultiple) ":" else ""} ${(dependencies, " ")}
        |${(for(b <- body) yield st"${TAB}$b", "\n")}""")
  }
}