// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.rust.component

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.{rust => RAST}

object CRustComponentStoreUtil {
  @strictpure def addBridgeModule(thread: IdPath, mod: RAST.Mod, store: Store): Store = {
    val allExisting = CRustApiPlugin.getCRustApiContributions(store).get
    val existing = allExisting.apiContributions.get(thread).get
    CRustApiPlugin.putCRustApiContributions(
      allExisting.addApiContributions(
        threadId = thread,
        contributions = existing(bridgeModuleContributions = existing.bridgeModuleContributions :+ mod)),
      store)
  }
}
