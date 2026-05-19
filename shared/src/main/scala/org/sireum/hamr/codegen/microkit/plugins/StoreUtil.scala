// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{ISZValue, IdPath, Store}
import org.sireum.hamr.codegen.microkit.util._

object StoreUtil {

  val KEY_MakefileContainers: String = "KEY_MakefileContainers"
  @strictpure def getMakefileContainers(store: Store): ISZ[MakefileContainer] =
    store.getOrElse(KEY_MakefileContainers, ISZValue[MakefileContainer](ISZ())).asInstanceOf[ISZValue[MakefileContainer]].elements

  @strictpure def addMakefileContainers(s: ISZ[MakefileContainer], store: Store): Store =
    store + KEY_MakefileContainers ~> ISZValue(getMakefileContainers(store) ++ s)


  val KEY_NonModelElement: String = "KEY_NoneModelElement"

  @strictpure def getNonModelElements(store: Store): ISZ[IdPath] =
    store.getOrElse(KEY_NonModelElement, ISZValue[IdPath](ISZ())).asInstanceOf[ISZValue[IdPath]].elements

  @strictpure def isNonModelElement(id: IdPath, store: Store): B =
    ops.ISZOps(getNonModelElements(store)).contains(id)

  @strictpure def addNonModelElement(id: IdPath, store: Store): Store = {
    val pluginGenerated: ISZ[IdPath] = store.getOrElse(KEY_NonModelElement, ISZValue[IdPath](ISZ())).asInstanceOf[ISZValue[IdPath]].elements
    store + KEY_NonModelElement ~> ISZValue(pluginGenerated :+ id)
  }
}
