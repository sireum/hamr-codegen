// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{ISZValue, Store}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.TAB

@sig trait MakefileItem

object MakefileUtil {
  val KEY_MainMakefileTarget: String = "KEY_MainMakefileTarget"

  @strictpure def getMainMakefileTarget(store: Store): ISZValue[MakefileTarget] =
    store.getOrElse(KEY_MainMakefileTarget, ISZValue[MakefileTarget](ISZ())).asInstanceOf[ISZValue[MakefileTarget]]

  @strictpure def addMainMakefileTarget(target: MakefileTarget, store: Store): Store =
    store + KEY_MainMakefileTarget ~> ISZValue(getMainMakefileTarget(store).elements :+ target)
}


@datatype class MakefileTarget(val name: String,
                               val dependencies: ISZ[ST],
                               val body: ISZ[ST]) extends MakefileItem {
  @pure def prettyST: ST = {
    return (
    st"""$name: ${(dependencies, " ")}
        |${(for(b <- body) yield st"$TAB$b", "\n")}""")
  }
}