// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{ISZValue, IdPath, MapValue, Store}
import org.sireum.hamr.codegen.microkit.util._

// Per-component code-generation policy (orthogonal to element provenance, which
// is tracked by StoreUtil.isSynthetic). The axes are independent:
//   - verusVerified : emit the component's app code inside verus (verus! wrap +
//                     #[verus_verify]); narrowable to F by any contributor that
//                     emits exec-only code (e.g. the implies!/impliesL! macros).
//   - userEditable  : preserve user edits across regen (marker regions +
//                     overwrite=F + "safe to edit" header); F => fully generated
//                     (no markers, overwrite=T, "do not edit" header).
//   - emitTestHarness : generate the per-component test/ infrastructure.
// Defaults are seeded from provenance (model thread => all T; synthetic => all F)
// and may be set explicitly by the injector that creates a component.
@datatype class ComponentGenProfile(val verusVerified: B,
                                    val userEditable: B,
                                    val emitTestHarness: B) {
  // Compose two policy wishes for the same component by narrowing toward "more
  // generated / plainer" -- F is absorbing on every axis. Locking down is the
  // monotonic, safe direction when several plugins co-own a synthetic component.
  @strictpure def merge(other: ComponentGenProfile): ComponentGenProfile =
    ComponentGenProfile(
      verusVerified = verusVerified & other.verusVerified,
      userEditable = userEditable & other.userEditable,
      emitTestHarness = emitTestHarness & other.emitTestHarness)
}

object StoreUtil {

  // Provenance-seeded defaults.
  val modelComponentProfile: ComponentGenProfile = ComponentGenProfile(verusVerified = T, userEditable = T, emitTestHarness = T)
  val syntheticComponentProfile: ComponentGenProfile = ComponentGenProfile(verusVerified = F, userEditable = F, emitTestHarness = F)

  val KEY_MakefileContainers: String = "KEY_MakefileContainers"
  @strictpure def getMakefileContainers(store: Store): ISZ[MakefileContainer] =
    store.getOrElse(KEY_MakefileContainers, ISZValue[MakefileContainer](ISZ())).asInstanceOf[ISZValue[MakefileContainer]].elements

  @strictpure def addMakefileContainers(s: ISZ[MakefileContainer], store: Store): Store =
    store + KEY_MakefileContainers ~> ISZValue(getMakefileContainers(store) ++ s)


  val KEY_SyntheticElement: String = "KEY_SyntheticElement"

  @strictpure def getSyntheticElements(store: Store): ISZ[IdPath] =
    store.getOrElse(KEY_SyntheticElement, ISZValue[IdPath](ISZ())).asInstanceOf[ISZValue[IdPath]].elements

  @strictpure def isSynthetic(id: IdPath, store: Store): B =
    ops.ISZOps(getSyntheticElements(store)).contains(id)

  @strictpure def addSyntheticElement(id: IdPath, store: Store): Store = {
    val pluginGenerated: ISZ[IdPath] = store.getOrElse(KEY_SyntheticElement, ISZValue[IdPath](ISZ())).asInstanceOf[ISZValue[IdPath]].elements
    store + KEY_SyntheticElement ~> ISZValue(pluginGenerated :+ id)
  }


  val KEY_ComponentGenProfiles: String = "KEY_ComponentGenProfiles"

  @strictpure def getComponentGenProfiles(store: Store): Map[IdPath, ComponentGenProfile] =
    store.getOrElse(KEY_ComponentGenProfiles, MapValue[IdPath, ComponentGenProfile](Map.empty)).asInstanceOf[MapValue[IdPath, ComponentGenProfile]].map

  // Resolved policy for a component: the explicit entry if one was set by its
  // injector, otherwise a provenance-seeded default (synthetic => all F, model => all T).
  @strictpure def getComponentGenProfile(id: IdPath, store: Store): ComponentGenProfile =
    getComponentGenProfiles(store).get(id) match {
      case Some(p) => p
      case _ => if (isSynthetic(id, store)) syntheticComponentProfile else modelComponentProfile
    }

  // Set/override a component's policy (used by an injector at component-creation time).
  @strictpure def putComponentGenProfile(id: IdPath, profile: ComponentGenProfile, store: Store): Store =
    store + KEY_ComponentGenProfiles ~> MapValue(getComponentGenProfiles(store) + id ~> profile)

  // Narrow a component's policy toward "more generated" (see ComponentGenProfile.merge);
  // for a contributing plugin that must force a constraint (e.g. verusVerified=F)
  // without discarding the creator's other choices.
  @strictpure def narrowComponentGenProfile(id: IdPath, constraint: ComponentGenProfile, store: Store): Store =
    putComponentGenProfile(id, getComponentGenProfile(id, store).merge(constraint), store)
}
