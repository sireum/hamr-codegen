// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{ISZValue, Store}
import org.sireum.hamr.codegen.microkit.util._

object StoreUtil {

  val KEY_MakefileContainers: String = "KEY_MakefileContainers"

  @strictpure def getMakefileContainers(store: Store): ISZ[MakefileContainer] =
    store.getOrElse(KEY_MakefileContainers, ISZValue[MakefileContainer](ISZ())).asInstanceOf[ISZValue[MakefileContainer]].elements

  @strictpure def addMakefileContainers(s: ISZ[MakefileContainer], store: Store): Store =
    store + KEY_MakefileContainers ~> ISZValue(getMakefileContainers(store) ++ s)
}
