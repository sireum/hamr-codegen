// #Sireum
package org.sireum.hamr.codegen.microkit.plugins

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{ISZValue, IdPath, MapValue, Store}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir
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

  // The symbol table as the model was written: synthetic components (injected monitors,
  // the test controller), synthetic ports (e.g. the sv_ state-variable mirrors) and every
  // connection touching one are removed. For consumers that reason about the model rather
  // than the built image, e.g. the system VC generator.
  @pure def modelSymbolTable(symbolTable: SymbolTable, store: Store): SymbolTable = {
    if (getSyntheticElements(store).isEmpty) {
      return symbolTable
    }
    // A synthetic thread port (e.g. an sv_ mirror) reaches the process boundary through a
    // same-named process port and a delegation; only the thread port is registered, so its
    // process twin is derived: <process> :+ <port name>.
    var processTwins: Set[IdPath] = Set.empty
    for (sp <- getSyntheticElements(store) if sp.size >= 3) {
      // only a port of a thread has a process twin (a synthetic thread itself has none)
      symbolTable.componentMap.get(ops.ISZOps(sp).dropRight(1)) match {
        case Some(_: AadlThread) =>
          processTwins = processTwins + (ops.ISZOps(sp).dropRight(2) :+ sp(sp.size - 1))
        case _ =>
      }
    }
    @strictpure def isSyntheticFeature(path: IdPath): B =
      isSynthetic(path, store) || processTwins.contains(path)
    @strictpure def keepEnd(e: ir.EndPoint): B =
      !isSynthetic(e.component.name, store) && (e.feature.isEmpty || !isSyntheticFeature(e.feature.get.name))
    @strictpure def keepConn(c: ir.ConnectionInstance): B = keepEnd(c.src) && keepEnd(c.dst)
    @strictpure def keepFeature(path: IdPath): B =
      !isSyntheticFeature(path) && !isSynthetic(ops.ISZOps(path).dropRight(1), store)
    @strictpure def keepComp(c: AadlComponent): B = !isSynthetic(c.path, store)
    // the whole tree below c, so a walk from the root sees the same model as componentMap
    def prune(c: AadlComponent): AadlComponent = {
      val subs: ISZ[AadlComponent] = for (sc <- c.subComponents if keepComp(sc)) yield prune(sc)
      val features: ISZ[AadlFeature] = c.features.filter((f: AadlFeature) => keepFeature(f.path))
      c match {
        case t: AadlThread =>
          return t(features = features, subComponents = subs,
            connectionInstances = t.connectionInstances.filter(keepConn _))
        case p: AadlProcess =>
          return p(features = features, subComponents = subs,
            connectionInstances = p.connectionInstances.filter(keepConn _))
        case s: AadlSystem =>
          return s(features = features, subComponents = subs,
            connectionInstances = s.connectionInstances.filter(keepConn _))
        case _ => return c
      }
    }
    @strictpure def keepPortConn(c: AadlConnection): B =
      c match {
        case pc: AadlPortConnection => keepConn(pc.connectionInstance)
        case _ => T
      }
    val rootSystem: AadlSystem = prune(symbolTable.rootSystem).asInstanceOf[AadlSystem]
    val componentMap: HashSMap[IdPath, AadlComponent] = HashSMap.empty[IdPath, AadlComponent] ++
      (for (e <- symbolTable.componentMap.entries if !isSynthetic(e._1, store)) yield (e._1, prune(e._2)))
    val featureMap: HashSMap[IdPath, AadlFeature] = HashSMap.empty[IdPath, AadlFeature] ++
      symbolTable.featureMap.entries.filter((e: (IdPath, AadlFeature)) => keepFeature(e._1))
    val inConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty[IdPath, ISZ[ir.ConnectionInstance]] ++
      (for (e <- symbolTable.inConnections.entries if keepFeature(e._1)) yield (e._1, e._2.filter(keepConn _)))
    val outConnections: HashSMap[IdPath, ISZ[ir.ConnectionInstance]] = HashSMap.empty[IdPath, ISZ[ir.ConnectionInstance]] ++
      (for (e <- symbolTable.outConnections.entries if keepFeature(e._1)) yield (e._1, e._2.filter(keepConn _)))
    return symbolTable(
      rootSystem = rootSystem,
      componentMap = componentMap,
      featureMap = featureMap,
      aadlConnections = symbolTable.aadlConnections.filter(keepPortConn _),
      connections = symbolTable.connections.filter(keepConn _),
      inConnections = inConnections,
      outConnections = outConnections)
  }


  // Overrides the name (and thus directory, Cargo package, and staticlib name) of the
  // Rust crate generated for a thread. By default a thread's crate is named by its id
  // path (e.g. sys_nominal_monitor_process_sys_nominal_monitor_thread); an injector may
  // register a shorter unique name (e.g. sys_nominal_monitor) here. Only the crate-level
  // names are affected -- the thread's id path (PD name, component dir, module names
  // inside the crate) is unchanged.
  val KEY_CrateNameOverrides: String = "KEY_CrateNameOverrides"

  @strictpure def getCrateNameOverrides(store: Store): Map[IdPath, String] =
    store.getOrElse(KEY_CrateNameOverrides, MapValue[IdPath, String](Map.empty)).asInstanceOf[MapValue[IdPath, String]].map

  @strictpure def getCrateNameOverride(id: IdPath, store: Store): Option[String] =
    getCrateNameOverrides(store).get(id)

  @strictpure def putCrateNameOverride(id: IdPath, crateName: String, store: Store): Store =
    store + KEY_CrateNameOverrides ~> MapValue(getCrateNameOverrides(store) + id ~> crateName)


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
