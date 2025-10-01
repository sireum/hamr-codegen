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

  @strictpure def getMakefileTargets(makefilePath: ISZ[String], store: Store): ISZ[MakefileTarget] =
    store.get(KEY_MainMakefileTarget).get.asInstanceOf[t].map.get(makefilePath).get
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