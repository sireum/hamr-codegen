// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.testing

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, HamrCli, HamrTimeUnit, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.microkit.MicrokitCodegen.toolName
import org.sireum.hamr.codegen.microkit.plugins.{ComponentGenProfile, MicrokitFinalizePlugin, MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{ComponentApiContributions, CRustApiPlugin}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.connections.{ConnectionStore, DefaultConnectionStore, DefaultSystemContributions, UberConnectionContributions, cConnectionContributions}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.types.{MicrokitLayout, MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.codegen.microkit.plugins.monitors.{SDValue, UserLandMonitorPlugin}
import org.sireum.hamr.codegen.microkit.plugins.msd.SystemDescriptionProviderPlugin
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.codegen.common.symbols.GclAnnexClauseInfo
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.AadlDirectedFeature
import org.sireum.hamr.codegen.common.symbols.{AadlEventDataPort, AadlEventPort, AadlPortConnection}
import org.sireum.hamr.codegen.common.sysvc.VCGenerator
import org.sireum.hamr.codegen.microkit.plugins.gumbo.{ContractObserverInfo, ContractObserverPlugin}
import org.sireum.hamr.ir.{Aadl, Direction, GclStateVar}
import org.sireum.message.Reporter

object TestSchedulerPlugin {

  /** Name of the MSD variant, and hence the prefix of every file in the bundle:
    * test_scheduler.meta.py, test_scheduler.scheduler.c, test_scheduler.mk, ...
    */
  val variantName: String = "test_scheduler"

  val KEY_handled: String = "KEY_TestSchedulerPlugin_handled"
  val KEY_modelTransformed: String = "KEY_TestSchedulerPlugin_modelTransformed"
  val KEY_contributed: String = "KEY_TestSchedulerPlugin_contributed"
  val KEY_observable: String = "KEY_TestSchedulerPlugin_observable"
  val KEY_injectable: String = "KEY_TestSchedulerPlugin_injectable"
  val KEY_observersWired: String = "KEY_TestSchedulerPlugin_observersWired"

  @strictpure def getInjectable(store: Store): ISZ[InjectableStateVar] =
    store.get(KEY_injectable) match {
      case Some(v) => v.asInstanceOf[InjectableStateVars].vars
      case _ => ISZ()
    }

  @strictpure def getObservable(store: Store): ISZ[ObservableRegion] =
    store.get(KEY_observable) match {
      case Some(v) => v.asInstanceOf[ObservableRegions].regions
      case _ => ISZ()
    }

  @strictpure def hasHandled(store: Store): B = store.contains(KEY_handled)

  @strictpure def hasTransformed(store: Store): B = store.contains(KEY_modelTransformed)

  /** The controller's name, and hence its crate, protection domain and channel names. */
  val controllerName: String = "test_controller"

  @strictpure def controllerProcessPath(root: IdPath): IdPath = root :+ s"${controllerName}_process"

  @strictpure def controllerThreadPath(root: IdPath): IdPath =
    controllerProcessPath(root) :+ s"${controllerName}_thread"

  /** The protection domain name CComponentPlugin_MCS derives from the thread path. */
  @strictpure def controllerPdName(root: IdPath): String = {
    val tp = controllerThreadPath(root)
    s"${tp(tp.lastIndex - 1)}_${tp(tp.lastIndex)}"
  }

  // Wall-clock bound for a QEMU run.  Generous by design: DONE is the real terminator, and
  // this only exists to bound the silent-failure case -- a hang, a panic, or a thread that
  // spins forever and so starves the lowest-priority controller.
  val qemuTimeoutSeconds: Z = 300

  // The controller must be the lowest-priority protection domain in the system so that its
  // busy-wait on ack_seq cannot starve the threads whose execution is what changes ack_seq
  // (TestScheduler-design.md D4).  Component threads sit at 140 and their _MON wrappers at
  // 150; the controller's own _MON only forwards a notification and does not spin, so it
  // stays just above the controller.
  val controllerPriority: Z = 100
  val controllerMonPriority: Z = 101

  /** Every port-backed shared memory region in the system, paired with the type information
    * the generated accessors need.  Built from the connection store: a PortSharedMemoryRegion
    * is named after its outgoing port path, and processOutPort / processInPort key their
    * contributions by that same path, so the two join cleanly.
    */
  @pure def observableRegions(symbolTable: SymbolTable, store: Store): ISZ[ObservableRegion] = {
    val cTypeProvider = CTypePlugin.getCTypeProvider(store).get

    // Threads that read a port: its producer (an output port's own guarantees) and each
    // thread it is connected to -- not the injected monitors, which are stripped from the
    // controller's variant.  An unconnected input's region is named after the reader, so
    // there the "producer" is the reader itself.
    def readersOf(portPath: IdPath): ISZ[String] = {
      var ret: ISZ[String] = ISZ(st"${(ops.ISZOps(ops.ISZOps(portPath).dropRight(1)).drop(1), "_")}".render)
      for (c <- symbolTable.aadlConnections) {
        c match {
          case pc: AadlPortConnection
            if pc.srcFeature.path == portPath && !StoreUtil.isSynthetic(pc.dstComponent.path, store) =>
            val r = MicrokitUtil.getComponentIdPath(pc.dstComponent)
            if (!ops.ISZOps(ret).contains(r)) {
              ret = ret :+ r
            }
          case _ =>
        }
      }
      return ret
    }

    val rustTypeProvider = CRustTypePlugin.getCRustTypeProvider(store).get

    var ret = ISZ[ObservableRegion]()
    var seen = Set.empty[String]
    for (entry <- CConnectionProviderPlugin.getCConnectionStore(store);
         cc <- entry.codeContributions.values;
         mr <- cc.sharedMemoryMapping) {
      mr match {
        // Skip ports belonging to injected protection domains -- the monitors' observation
        // ports and their sched_state/sched_schedule.  Those PDs are stripped from this
        // variant, so their regions may not be mapped here at all; generating accessors for
        // them would hand the controller addresses that point at nothing.  A thread's own
        // sv_ ports are synthetic but the *thread* is not, which is what this tests.
        case p: PortSharedMemoryRegion
          if !seen.contains(p.name) &&
             !StoreUtil.isSynthetic(ops.ISZOps(p.outgoingPortPath).dropRight(1), store) =>
          seen = seen + p.name
          var isEventPort: B = F
          var isEventDataPort: B = F
          symbolTable.featureMap.get(p.outgoingPortPath) match {
            case Some(_: AadlEventPort) => isEventPort = T
            case Some(_: AadlEventDataPort) => isEventDataPort = T
            case _ =>
          }
          // an unconnected input's region is named after the input itself
          val isProducerOutput: B = symbolTable.featureMap.get(p.outgoingPortPath) match {
            case Some(f: AadlDirectedFeature) => f.direction == Direction.Out
            case _ => F
          }
          // Drop the system instance prefix: the accessor reads tcp_tct_currentTemp,
          // not TempControlSystem_Instance_tcp_tct_currentTemp.
          val acc = st"${(ops.ISZOps(p.outgoingPortPath).drop(1), "_")}".render
          ret = ret :+ ObservableRegion(
            accessor = acc,
            regionName = p.name,
            cTypeName = cTypeProvider.getTypeNameProvider(
              cTypeProvider.getRepresentativeType(cc.aadlType)).mangledName,
            rustTypeName = rustTypeProvider.getTypeNameProvider(
              rustTypeProvider.getRepresentativeType(cc.aadlType)).qualifiedRustName,
            queueSize = p.queueSize,
            sizeInKiBytes = p.sizeInKiBytes,
            // a GUMBO state variable's sv_ port is synthetic (StateVarPortsPlugin); a model
            // port that merely happens to be named sv_* is not
            isStateVar = StoreUtil.isSynthetic(p.outgoingPortPath, store),
            isEvent = isEventPort || isEventDataPort,
            isPureEvent = isEventPort,
            isProducerOutput = isProducerOutput,
            readers = readersOf(p.outgoingPortPath))
        case _ =>
      }
    }
    return ret
  }

  /** Base virtual address, in KiB, for the controller's view of the observable regions.
    * Chosen clear of the command regions at 0x4_00x_000 and of the port regions' own
    * mappings, which start at 0x10_000_000 in their owning protection domains.
    */
  val observableBaseVaddrKiB: Z = 524288 // 0x20_000_000

  /** The controller's vaddr for the i-th observable region.  Regions are placed back to back
    * by their real size, each followed by a guard page (SharedMemorySafety-design.md, D4, D7);
    * a fixed 4 KiB stride assumed every region fit in one page, which a large element
    * outgrows.  Sizes are whole pages, since KiBytesToHex rounds up and anything finer
    * silently aliases.
    */
  @strictpure def observableVaddrKiB(regions: ISZ[ObservableRegion], i: Z): Z =
    MicrokitUtil.packedVaddrKiB(observableBaseVaddrKiB, for (r <- regions) yield r.sizeInKiBytes, i)

  /** Prefix for the synthetic *input* ports that carry injected state var values, mirroring
    * GumboMonitorPlugin's `sv_` outputs.  See TestScheduler-design.md D16: the queue's
    * emptiness is the dirty flag, so a thread that was sent nothing keeps its own state.
    */
  val injectStateVarPortPrefix: String = "inj_sv_"

  @strictpure def injectStateVarPortName(stateVarName: String): String =
    s"${injectStateVarPortPrefix}${stateVarName}"

  @pure def getStateVars(threadPath: ISZ[String], symbolTable: SymbolTable): ISZ[GclStateVar] = {
    symbolTable.annexClauseInfos.get(threadPath) match {
      case Some(clauses) =>
        for (clause <- clauses) {
          clause match {
            case gclInfo: GclAnnexClauseInfo => return gclInfo.annex.state
            case _ =>
          }
        }
        return ISZ()
      case _ => return ISZ()
    }
  }

  /** One injectable GUMBO state variable: the thread that owns it, its type, and the names
    * of the plugin-declared region that carries an injected value to that thread.
    */
  @pure def injectableStateVars(symbolTable: SymbolTable, types: AadlTypes, store: Store): ISZ[InjectableStateVar] = {
    val cTypeProvider = CTypePlugin.getCTypeProvider(store).get
    val rustTypeProvider = CRustTypePlugin.getCRustTypeProvider(store).get

    var ret = ISZ[InjectableStateVar]()
    // Only a Rust thread ingests an injected value (CRustComponentPlugin's generated ingest);
    // a C thread would keep its own state while the checks assumed the injected one.
    for (thread <- symbolTable.getThreads() if !StoreUtil.isSynthetic(thread.path, store) && MicrokitUtil.isRusty(thread)) {
      val threadId = MicrokitUtil.getComponentIdPath(thread)
      for (sv <- getStateVars(thread.path, symbolTable)) {
        types.typeMap.get(sv.classifier) match {
          case Some(aadlType) =>
            val rep = cTypeProvider.getRepresentativeType(aadlType)
            ret = ret :+ InjectableStateVar(
              threadPath = thread.path,
              threadId = threadId,
              varName = sv.name,
              regionName = s"inj_${threadId}_sv_${sv.name}",
              cTypeName = cTypeProvider.getTypeNameProvider(rep).mangledName,
              rustTypeName = rustTypeProvider.getTypeNameProvider(
                rustTypeProvider.getRepresentativeType(aadlType)).qualifiedRustName,
              // the whole injection queue (queue size 1), from HAMR's layout -- not a fixed
              // page, which a large state variable outgrew (SharedMemorySafety-design.md, D4)
              sizeInKiBytes = MicrokitLayout.queueRegionKiBytes(aadlType, cTypeProvider.substitutions, 1))
          case _ =>
        }
      }
    }
    return ret
  }

  /** Every thread's whole-component pre-state (design D14): its input ports and its GUMBO
    * state variables, in one container per thread.
    *
    * Ports are named as the *component* sees them (`currentTemp`), not by the producer the
    * region is named after (`tsp_tst_currentTemp`), because the point of the container is to
    * describe one component's inputs. State vars carry the `sv_` prefix they already have on
    * the accessors, which also keeps them clear of a port of the same name.
    *
    * `outgoingPortPath` cannot tell input from output here: for an *unconnected* input the
    * region is named after the reader, so it equals the thread's own port path. The AADL
    * feature's direction is asked instead.
    */
  @pure def threadPreStates(regions: ISZ[ObservableRegion],
                            injectables: ISZ[InjectableStateVar],
                            symbolTable: SymbolTable,
                            store: Store): ISZ[ThreadPreState] = {
    var byRegionName = Map.empty[String, ObservableRegion]
    for (r <- regions) {
      byRegionName = byRegionName + r.regionName ~> r
    }

    var inputsByThread = Map.empty[IdPath, ISZ[PreStateField]]
    var seen = Set.empty[String]
    for (entry <- CConnectionProviderPlugin.getCConnectionStore(store);
         e <- entry.codeContributions.entries) {
      val threadPath = e._1
      val cc = e._2
      val isInput: B = symbolTable.featureMap.get(cc.portName) match {
        case Some(f: AadlDirectedFeature) => f.direction == Direction.In
        case _ => F
      }
      if (isInput && !StoreUtil.isSynthetic(threadPath, store)) {
        for (mr <- cc.sharedMemoryMapping) {
          mr match {
            case p: PortSharedMemoryRegion =>
              val fieldName = cc.portName(cc.portName.lastIndex)
              val key = st"${(threadPath, "_")}_$fieldName".render
              byRegionName.get(p.name) match {
                case Some(r) if !seen.contains(key) =>
                  seen = seen + key
                  val existing: ISZ[PreStateField] =
                    inputsByThread.get(threadPath) match {
                      case Some(fs) => fs
                      case _ => ISZ()
                    }
                  inputsByThread = inputsByThread + threadPath ~>
                    (existing :+ PreStateField(
                      fieldName = fieldName,
                      rustTypeName = r.rustTypeName,
                      setter = s"put_${r.accessor}"))
                case _ =>
              }
            case _ =>
          }
        }
      }
    }

    var ret = ISZ[ThreadPreState]()
    for (thread <- symbolTable.getThreads() if !StoreUtil.isSynthetic(thread.path, store)) {
      val threadId = MicrokitUtil.getComponentIdPath(thread)
      val ports: ISZ[PreStateField] =
        inputsByThread.get(thread.path) match {
          case Some(fs) => fs
          case _ => ISZ()
        }
      val svs: ISZ[PreStateField] =
        for (v <- injectables if v.threadPath == thread.path) yield PreStateField(
          fieldName = s"sv_${v.varName}",
          rustTypeName = v.rustTypeName,
          setter = s"put_${v.threadId}_sv_${v.varName}")
      // A thread with neither is not worth a container: an empty struct and an empty
      // setter would only be noise in the generated API.
      if (ports.nonEmpty || svs.nonEmpty) {
        ret = ret :+ ThreadPreState(threadId = threadId, fields = ports ++ svs)
      }
    }
    return ret
  }

  /** Base virtual address, in KiB, for the injection regions.  A separate block from the
    * observable regions at 0x20_000_000 so the two allocations cannot collide.
    */
  val injectBaseVaddrKiB: Z = 528384 // 0x20_400_000

  @strictpure def injectVaddrKiB(vars: ISZ[InjectableStateVar], i: Z): Z =
    MicrokitUtil.packedVaddrKiB(injectBaseVaddrKiB, for (v <- vars) yield v.sizeInKiBytes, i)

  /** Where the owning thread maps its own injection region.  Distinct from the controller's
    * view, and clear of the port regions the thread already maps from 0x10_000_000.
    */
  val injectThreadBaseVaddrKiB: Z = 532480 // 0x20_800_000

  @strictpure def injectThreadVaddrKiB(vars: ISZ[InjectableStateVar], i: Z): Z =
    MicrokitUtil.packedVaddrKiB(injectThreadBaseVaddrKiB, for (v <- vars) yield v.sizeInKiBytes, i)

  /** Whether the model has anything for the controller to check (stage 7): a thread with a
    * GUMBO subclause, or a system composition.  Decided from the symbol table because the
    * controller's observation cursors are C bridge code, which must be contributed in the
    * first handle pass -- before the observers crate that says exactly what is read exists.
    */
  @pure def checksRequested(symbolTable: SymbolTable): B = {
    if (VCGenerator.getCompositions(symbolTable).nonEmpty) {
      return T
    }
    for (thread <- symbolTable.getThreads()) {
      symbolTable.annexClauseInfos.get(thread.path) match {
        case Some(clauses) =>
          for (clause <- clauses) {
            clause match {
              case _: GclAnnexClauseInfo => return T
              case _ =>
            }
          }
        case _ =>
      }
    }
    return F
  }

  /** The observers crate is ready and has something for the controller to host. */
  @pure def observersReady(store: Store): B = {
    ContractObserverPlugin.getInfo(store) match {
      case Some(info) => return info.hasLayers
      case _ => return F
    }
  }

  @pure def hasThreadsWithStateVars(symbolTable: SymbolTable): B = {
    for (thread <- symbolTable.getThreads()) {
      symbolTable.annexClauseInfos.get(thread.path) match {
        case Some(clauses) =>
          for (clause <- clauses) {
            clause match {
              case gclInfo: GclAnnexClauseInfo =>
                if (gclInfo.annex.state.nonEmpty) {
                  return T
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
    return F
  }
}

/** One shared memory region the controller can see: a component port, or a GUMBO state var
  * published through a synthetic `sv_` port.  Stage 5 maps each of these into the controller
  * and generates a typed accessor pair for it.
  *
  * `accessor` is what the generated API is named after -- the region's port path with the
  * system prefix dropped, e.g. `tcp_tct_currentTemp`.
  */
@datatype class ObservableRegions(val regions: ISZ[ObservableRegion]) extends StoreValue

@datatype class InjectableStateVars(val vars: ISZ[InjectableStateVar]) extends StoreValue

/** A GUMBO state variable the controller can set.  The value travels through a region this
  * plugin declares -- deliberately NOT an AADL port, which would put test plumbing into the
  * component test harness and the system verification model (design D16a).
  */
@datatype class InjectableStateVar(val threadPath: ISZ[String],
                                   val threadId: String,
                                   val varName: String,
                                   val regionName: String,
                                   val cTypeName: String,
                                   val rustTypeName: String,
                                   val sizeInKiBytes: Z) {
  /** The C global microkit patches with this region's address via setvar_vaddr. */
  @strictpure def queueVar: String = s"inj_sv_${varName}_queue"
}

@datatype class ObservableRegion(val accessor: String,
                                 val regionName: String,
                                 val cTypeName: String,
                                 val rustTypeName: String,
                                 val queueSize: Z,
                                 val sizeInKiBytes: Z,
                                 val isStateVar: B,
                                 // an event or event-data port: the contract checks read it with
                                 // event semantics, through one cursor per reading thread
                                 val isEvent: B,
                                 val isPureEvent: B,
                                 // the region of a thread's output port -- not of an
                                 // unconnected input, which is named after its reader
                                 val isProducerOutput: B,
                                 // thread ids whose contracts can read it: the producer and
                                 // every consumer
                                 val readers: ISZ[String]) {
  /** A test's `put_` into this region is not the producer's own output: the producer's
    * check must not read it back as such (see `observe::injected_port_*`). */
  @strictpure def injectionHidesFromProducer: B = isEvent && isProducerOutput
  /** Name of the controller's observation cursor on this region read for `reader`: one per
    * region for data ports and state variables (a last-value cache serves every reader),
    * one per (region, reader) for event ports, plus `sys` for the system assertions.
    */
  @strictpure def obsCursor(reader: String): String =
    if (isEvent) s"${accessor}__$reader" else accessor

  @strictpure def obsReaders: ISZ[String] = if (isEvent) readers :+ "sys" else ISZ("")
}

/** One field of a thread's whole-component pre-state (design D14): an input port or a GUMBO
  * state variable, named as the *component* sees it rather than by the region's producer.
  *
  * `setter` is the existing per-field function the container delegates to, so D14 adds no
  * new way to reach shared memory -- only a way to be sure nothing was left out.
  */
@datatype class PreStateField(val fieldName: String,
                              val rustTypeName: String,
                              val setter: String)

/** Every input port and GUMBO state variable of one thread (design D14). */
@datatype class ThreadPreState(val threadId: String,
                               val fields: ISZ[PreStateField])

/** System testing on seL4: the test-scheduler variant bundle (test_scheduler.meta.py, .mk,
  * the command-driven scheduler), the test controller protection domain that drives it, and
  * the controller's generated test support -- api, harness, inspect, selection and, when the
  * model has contracts, observe.  See hamr/codegen/doc/TestScheduler-design.md.
  *
  * It works in three phases:
  *  - model transform: injects the controller thread and process into the model;
  *  - handle: contributes the controller's C bridge (inspection, injection and observation
  *    accessors) and the thread-side state-variable ingest, while the component plugins can
  *    still take them; a second handle stage adds the observers crate dependency once
  *    ContractObserverPlugin has run;
  *  - finalize: builds the variant's system description and writes the bundle and the
  *    controller's system_tests sources.  Finalize runs after every handle plugin has
  *    settled, so the "normal" system description the variant derives from is final (in
  *    particular, the monitor plugins have stripped their own protection domains from it).
  */
@datatype class TestSchedulerPlugin extends ModelTransformerPlugin with MicrokitPlugin with MicrokitFinalizePlugin {

  val name: String = "TestSchedulerPlugin"

  @pure def enabled(options: HamrCli.CodegenOption, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        ExperimentalOptions.enableTestScheduler(options.experimentalOptions) &&
        MicrokitUtil.isMCS(options, symbolTable.rootSystem))
  }

  @pure override def canHandleModelTransform(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                             symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return enabled(options, symbolTable, store, reporter) && !TestSchedulerPlugin.hasTransformed(store)
  }

  override def handleModelTransform(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                    symbolTable: SymbolTable, store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore = store + TestSchedulerPlugin.KEY_modelTransformed ~> BoolValue(T)

    val sysPath = model.components(0).identifier.name
    val processPath = TestSchedulerPlugin.controllerProcessPath(sysPath)
    val threadPath = TestSchedulerPlugin.controllerThreadPath(sysPath)

    // There is one controller, so its crate drops the <..>_process_<..>_thread suffix.
    localStore = StoreUtil.putCrateNameOverride(threadPath, TestSchedulerPlugin.controllerName, localStore)

    TestControllerInjector().inject(model, processPath, threadPath, symbolTable, reporter) match {
      case Some(injected) =>
        val reResult = ModelUtil.resolve(injected, injected.components(0).identifier.pos, "", options, localStore, reporter)
        localStore = reResult._2
        if (reResult._1.isEmpty || reporter.hasError) {
          return None()
        }

        localStore = StoreUtil.addSyntheticElement(processPath, StoreUtil.addSyntheticElement(threadPath, localStore))

        // Not Verus-verified (it is test code, and its assertions are exec-only), and fully
        // generated: the hand-written test script is system_tests/tests.rs (and the files it
        // names), which this plugin writes once and never overwrites, so nothing the user
        // edits lives in the crate's app module or manifest.  Overwriting the manifest is what
        // lets a dependency codegen adds later (crates/observers, stage 7) reach a tree
        // regenerated in place.  No component-level test harness: the controller IS the test
        // harness, and its tests compile into the protection domain rather than running under
        // cargo test.
        localStore = StoreUtil.putComponentGenProfile(threadPath,
          ComponentGenProfile(verusVerified = F, userEditable = F, emitTestHarness = F), localStore)

        return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
      case _ =>
        return None()
    }
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      enabled(options, symbolTable, store, reporter) &&
        TestSchedulerPlugin.hasTransformed(store) &&
        CRustComponentPlugin.hasCRustComponentContributions(store) &&
        CRustApiPlugin.getCRustApiContributions(store).nonEmpty &&
        (!store.contains(TestSchedulerPlugin.KEY_contributed) ||
          // second stage: the observers crate appeared in a later pass
          (!store.contains(TestSchedulerPlugin.KEY_observersWired) && TestSchedulerPlugin.observersReady(store))))
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                      symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    val sysPath = model.components(0).identifier.name
    val threadPath = TestSchedulerPlugin.controllerThreadPath(sysPath)

    if (store.contains(TestSchedulerPlugin.KEY_contributed)) {
      // Stage 7: the controller hosts the contract checks, so its crate depends on
      // crates/observers.  Its C side -- the obs_get_* cursors -- went in with the first
      // stage, since the C bridges are written before the observers crate exists.
      val contributions = CRustComponentPlugin.getCRustComponentContributions(store)
      val contrib = contributions.componentContributions.get(threadPath).get
      val wired = contributions.replaceComponentContributions(
        contributions.componentContributions + threadPath ~> contrib(
          crateDependencies = contrib.crateDependencies :+ ContractObserverPlugin.crateDependency))
      return (CRustComponentPlugin.putComponentContributions(wired,
        store + TestSchedulerPlugin.KEY_observersWired ~> BoolValue(T)), ISZ())
    }

    var localStore = store + TestSchedulerPlugin.KEY_contributed ~> BoolValue(T)

    // microkit_notify is a static inline in microkit.h, so it cannot be linked against from
    // Rust.  Give the controller C bridge a wrapper that Rust declares as an extern.
    // PORT_FROM_MON is the controller channel to its own _MON, which forwards to the
    // scheduler -- the same path the readiness handshake already takes.
    val notifySig: ST = st"void ${TestSchedulerPlugin.controllerName}_notify_scheduler(void)"
    val notifyImpl: ST =
      st"""$notifySig {
          |  microkit_notify(PORT_FROM_MON);
          |}"""

    // Verdict lines must be emitted verbatim: the host driver anchors on the "TEST | "
    // prefix, and the log crate's formatter would prepend a level and target.  printf is
    // #defined to sddf_dprintf by the generated header, which reaches QEMU stdio through
    // the seL4 debug syscall in a debug build.
    val printSig: ST = st"void ${TestSchedulerPlugin.controllerName}_print(const char *s)"
    val printImpl: ST =
      st"""$printSig {
          |  printf("%s", s);
          |}"""

    // Stage 5: a typed accessor pair per observable region.  These call the generated
    // sb_queue API rather than reimplementing the wire protocol in Rust -- the queue has
    // an atomic numSent, a ring buffer, and per-receiver cursors, and getting any of that
    // subtly wrong would corrupt the very data a test is trying to read.
    //
    // Reading is non-destructive: the controller owns its own Recv_t, so its cursor is
    // independent of the real consumer's.  Writing enqueues exactly as the producer would;
    // that makes the controller a second sender, which the queue's single-sender contract
    // permits only because D4 guarantees the producer is not running concurrently.
    val regions = TestSchedulerPlugin.observableRegions(symbolTable, localStore)
    // An output feeding consumers with different queue sizes has one region per size, all
    // for the same port.  The controller names its accessors after the port, so it cannot
    // tell them apart.
    var accessorSizes: Map[String, ISZ[Z]] = Map.empty
    for (r <- regions) {
      accessorSizes = accessorSizes + r.accessor ~> (accessorSizes.getOrElse(r.accessor, ISZ()) :+ r.queueSize)
    }
    for (e <- accessorSizes.entries if e._2.size > 1) {
      reporter.error(None(), toolName,
        st"The test scheduler does not support '${e._1}': its consumers have different queue sizes (${(e._2, ", ")}). Give them the same Queue_Size.".render)
    }
    if (reporter.hasError) {
      return (localStore, ISZ())
    }
    localStore = localStore + TestSchedulerPlugin.KEY_observable ~> ObservableRegions(regions)

    val checks: B = TestSchedulerPlugin.checksRequested(symbolTable)
    var accessorSigs: ISZ[ST] = ISZ()
    var accessorImpls: ISZ[ST] = ISZ()
    var accessorInits: ISZ[ST] = ISZ()
    var i: Z = 0
    for (r <- regions) {
      val qt = QueueTemplate.getTypeQueueTypeName(r.cTypeName, r.queueSize)
      val rt = QueueTemplate.getClientRecvQueueTypeName(r.cTypeName, r.queueSize)
      val qn = QueueTemplate.getTypeQueueName(r.cTypeName, r.queueSize)
      val vaddr = MicrokitUtil.KiBytesToHexH(TestSchedulerPlugin.observableVaddrKiB(regions, i), F)
      val getSig = st"bool test_get_${r.accessor}(${r.cTypeName} *value)"
      val putSig = st"void test_put_${r.accessor}(${r.cTypeName} *value)"

      val getImpl =
        st"""volatile $qt *tc_${r.accessor}_queue = (volatile $qt *) $vaddr;
            |$rt tc_${r.accessor}_recv;
            |
            |$getSig {
            |  sb_event_counter_t numDropped;
            |  return ${qn}_dequeue(&tc_${r.accessor}_recv, &numDropped, value);
            |}"""

      // A state var's sv_ region is the thread's output: it publishes there and never reads
      // back, so a writer would be a no-op that looks like it works. Setting a state var goes
      // through its injection region instead (D16), whose accessor is emitted below and would
      // otherwise collide with this one by name.
      accessorSigs = accessorSigs :+ getSig
      if (!r.isStateVar) {
        accessorSigs = accessorSigs :+ putSig
      }
      accessorImpls = accessorImpls :+
        (if (r.isStateVar) getImpl
         else
           st"""$getImpl
               |
               |$putSig {
               |  ${qn}_enqueue(($qt *) tc_${r.accessor}_queue, value);
               |}""")
      accessorInits = accessorInits :+
        st"${qn}_Recv_init(&tc_${r.accessor}_recv, ($qt *) tc_${r.accessor}_queue);"

      // Stage 7 (D25): the contract checks' own receive cursors, so a check never consumes
      // what a test is about to read through inspect::, nor the reverse.
      if (checks) {
        for (reader <- r.obsReaders) {
          val cursor = r.obsCursor(reader)
          val obsSig = st"bool test_obs_get_$cursor(${r.cTypeName} *value)"
          accessorSigs = accessorSigs :+ obsSig
          accessorImpls = accessorImpls :+
            st"""$rt tc_obs_${cursor}_recv;
                |
                |$obsSig {
                |  sb_event_counter_t numDropped;
                |  return ${qn}_dequeue(&tc_obs_${cursor}_recv, &numDropped, value);
                |}"""
          accessorInits = accessorInits :+
            st"${qn}_Recv_init(&tc_obs_${cursor}_recv, ($qt *) tc_${r.accessor}_queue);"
        }
      }
      i = i + 1
    }

    val injectables = TestSchedulerPlugin.injectableStateVars(symbolTable, types, localStore)
    for (thread <- symbolTable.getThreads()
         if !StoreUtil.isSynthetic(thread.path, localStore) && !MicrokitUtil.isRusty(thread) &&
           TestSchedulerPlugin.getStateVars(thread.path, symbolTable).nonEmpty) {
      reporter.warn(thread.component.identifier.pos, toolName,
        s"${MicrokitUtil.getComponentIdPath(thread)} is a C thread: a system test cannot set its GUMBO state variables (only Rust threads take an injected value); its checks still read them")
    }
    localStore = localStore + TestSchedulerPlugin.KEY_injectable ~> InjectableStateVars(injectables)

    // Controller side of D16: one enqueue per injectable state var, into the plugin-declared
    // region the owning thread ingests from.  Write-only by construction -- reading a state
    // var goes through its sv_ region, which is a different region with a different cursor.
    var injSigs: ISZ[ST] = ISZ()
    var injImpls: ISZ[ST] = ISZ()
    var ki: Z = 0
    for (v <- injectables) {
      val qt = QueueTemplate.getTypeQueueTypeName(v.cTypeName, 1)
      val qn = QueueTemplate.getTypeQueueName(v.cTypeName, 1)
      val vaddr = MicrokitUtil.KiBytesToHexH(TestSchedulerPlugin.injectVaddrKiB(injectables, ki), F)
      val sig = st"void test_put_${v.threadId}_sv_${v.varName}(${v.cTypeName} *value)"
      injSigs = injSigs :+ sig
      injImpls = injImpls :+
        st"""volatile $qt *tc_inj_${v.threadId}_sv_${v.varName}_queue = (volatile $qt *) $vaddr;
            |
            |$sig {
            |  ${qn}_enqueue(($qt *) tc_inj_${v.threadId}_sv_${v.varName}_queue, value);
            |}"""
      ki = ki + 1
    }

    val cContribs = cConnectionContributions(
      cPortApiMethodSigs = ISZ(notifySig, printSig) ++ accessorSigs ++ injSigs,
      cBridge_EntrypointMethodSignatures = ISZ(),
      cBridge_GlobalVarContributions = ISZ(),
      cBridge_PortApiMethods = ISZ(notifyImpl, printImpl) ++ accessorImpls ++ injImpls,
      cBridge_InitContributions = accessorInits,
      cBridge_ComputeContributions = ISZ(),
      cUser_MethodDefaultImpls = ISZ())

    // D16: the thread side of state var injection.  The regions are declared by this plugin
    // (D16a), so both the queue pointer and the accessor are generated here rather than
    // falling out of an AADL port -- which is the whole point: nothing outside this plugin
    // sees them, so they cannot reach the component test harness or the verification model.
    var byThread: Map[ISZ[String], ISZ[(InjectableStateVar, Z)]] = Map.empty
    var ii: Z = 0
    for (v <- injectables) {
      byThread = byThread + v.threadPath ~> (byThread.getOrElse(v.threadPath, ISZ()) :+ ((v, ii)))
      ii = ii + 1
    }

    var injectEntries: ISZ[ConnectionStore] = ISZ()
    for (e <- byThread.entries) {
      val owner = e._1
      var sigs: ISZ[ST] = ISZ()
      var impls: ISZ[ST] = ISZ()
      var inits: ISZ[ST] = ISZ()
      var nullChecks: ISZ[ST] = ISZ()
      for (p <- e._2) {
        val v = p._1
        val qt = QueueTemplate.getTypeQueueTypeName(v.cTypeName, 1)
        val rt = QueueTemplate.getClientRecvQueueTypeName(v.cTypeName, 1)
        val qn = QueueTemplate.getTypeQueueName(v.cTypeName, 1)
        val sig = st"bool get_inj_sv_${v.varName}(${v.cTypeName} *value)"
        sigs = sigs :+ sig
        // Left uninitialized on purpose: microkit patches it through setvar_vaddr in the
        // variants that map the region, and it stays NULL everywhere else.  A hardcoded
        // address would make is_injection_enabled() true in the default variant and send
        // the thread dequeuing from unmapped memory.
        impls = impls :+
          st"""volatile $qt *${v.queueVar};
              |$rt inj_sv_${v.varName}_recv;
              |
              |$sig {
              |  if (${v.queueVar} == NULL) {
              |    return false;
              |  }
              |  sb_event_counter_t numDropped;
              |  return ${qn}_dequeue(&inj_sv_${v.varName}_recv, &numDropped, value);
              |}"""
        inits = inits :+
          st"""if (${v.queueVar} != NULL) {
              |  ${qn}_Recv_init(&inj_sv_${v.varName}_recv, ($qt *) ${v.queueVar});
              |}"""
        nullChecks = nullChecks :+ st"${v.queueVar} != NULL"
      }

      // Mirrors is_monitoring_enabled: a NULL check on the queue pointers, so a variant that
      // does not map these regions compiles the same code and simply never ingests.
      val injSig: ST = st"bool is_injection_enabled(void)"
      sigs = sigs :+ injSig
      impls = impls :+
        st"""$injSig {
            |  return ${(nullChecks, " && ")};
            |}"""

      injectEntries = injectEntries :+ DefaultConnectionStore(
        systemContributions = DefaultSystemContributions(
          sharedMemoryRegionContributions = ISZ(), channelContributions = ISZ()),
        typeApiContributions = ISZ(),
        senderName = owner,
        codeContributions = Map.empty[ISZ[String], UberConnectionContributions] +
          owner ~> UberConnectionContributions(
            portName = ISZ(), portPriority = None(), aadlType = TypeUtil.EmptyType,
            queueSize = 0, sharedMemoryMapping = ISZ(),
            cContributions = cConnectionContributions(
              cPortApiMethodSigs = sigs,
              cBridge_EntrypointMethodSignatures = ISZ(),
              cBridge_GlobalVarContributions = ISZ(),
              cBridge_PortApiMethods = impls,
              cBridge_InitContributions = inits,
              cBridge_ComputeContributions = ISZ(),
              cUser_MethodDefaultImpls = ISZ())))
    }

    val entry: ConnectionStore = DefaultConnectionStore(
      systemContributions = DefaultSystemContributions(
        sharedMemoryRegionContributions = ISZ(), channelContributions = ISZ()),
      typeApiContributions = ISZ(),
      senderName = threadPath,
      codeContributions = Map.empty[ISZ[String], UberConnectionContributions] +
        threadPath ~> UberConnectionContributions(
          portName = ISZ(),
          portPriority = None(),
          aadlType = TypeUtil.EmptyType,
          queueSize = 0,
          sharedMemoryMapping = ISZ(),
          cContributions = cContribs))

    localStore = CConnectionProviderPlugin.putCConnectionStore(
      (CConnectionProviderPlugin.getCConnectionStore(localStore) :+ entry) ++ injectEntries, localStore)

    // Weave the generated test module into the controller crate lib.rs.  CRustComponentPlugin
    // owns that file skeleton, so the module declaration and the call that runs the suite are
    // contributed rather than emitted by re-generating lib.rs at the same path.
    var contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)

    // D16 thread side, Rust half: ingest injected state vars before the app computes, so a
    // test's setup is visible to the very dispatch it is setting up.  An empty queue means
    // nothing was injected, and the thread keeps its own state -- that is the dirty flag.
    //
    // The C functions are declared through the thread's extern_c_api.rs (CRustApiPlugin),
    // as is_monitoring_enabled is, rather than by a raw extern block in lib.rs: the C bridge
    // only exists in the seL4 image, so a host `cargo test` needs the #[cfg(test)] stubs
    // extern_c_api.rs provides, which report that nothing is ever injected.
    var crustApiContribs = CRustApiPlugin.getCRustApiContributions(localStore).get
    for (e <- byThread.entries) {
      val owner = e._1
      (contributions.componentContributions.get(owner), crustApiContribs.apiContributions.get(owner)) match {
        case (Some(contrib), Some(apiContrib)) =>
          var externCApis: ISZ[RAST.Item] = ISZ(
            RAST.FnSig(
              verusHeader = None(), fnHeader = RAST.FnHeader(F),
              ident = RAST.IdentString("is_injection_enabled"),
              generics = None(),
              fnDecl = RAST.FnDecl(
                inputs = ISZ(),
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType))))
          var wrappers: ISZ[RAST.Item] = ISZ(
            RAST.FnImpl(
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString("unsafe_is_injection_enabled"),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(),
                  outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
              comments = ISZ(), attributes = ISZ(), meta = ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  return is_injection_enabled();
                    |}"""))))))
          var testMockVars: ISZ[RAST.Item] = ISZ(
            RAST.ItemStatic(
              ident = RAST.IdentString("INJECTION_ENABLED"),
              visibility = RAST.Visibility.Public,
              ty = RAST.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), ISZ("bool")), None()),
              mutability = RAST.Mutability.Not,
              expr = RAST.ExprST(st"Mutex::new(None);")))
          var testingApis: ISZ[RAST.Item] = ISZ(
            RAST.FnImpl(
              attributes = ISZ(RAST.AttributeST(F, st"cfg(test)")),
              sig = RAST.FnSig(
                ident = RAST.IdentString("is_injection_enabled"),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(),
                  outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
              comments = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match *INJECTION_ENABLED.lock().unwrap_or_else(|e| e.into_inner()) {
                    |    Some(v) => return v,
                    |    None => return false,
                    |  }
                    |}"""))))))
          var ingest: ISZ[ST] = ISZ()
          for (p <- e._2) {
            val v = p._1
            val getName = s"get_inj_sv_${v.varName}"
            val mockVar = s"INJ_SV_${v.varName}"
            val valueTy = RAST.TyPath(ISZ(ISZ(v.rustTypeName)), None())
            val valueParam: ISZ[RAST.Param] = ISZ(RAST.ParamImpl(
              ident = RAST.IdentString("value"),
              kind = RAST.TyPtr(mutty = RAST.MutTy(ty = valueTy, mutbl = RAST.Mutability.Mut))))
            externCApis = externCApis :+ RAST.FnSig(
              verusHeader = None(), fnHeader = RAST.FnHeader(F),
              ident = RAST.IdentString(getName),
              generics = None(),
              fnDecl = RAST.FnDecl(
                inputs = valueParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
            wrappers = wrappers :+ RAST.FnImpl(
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString(s"unsafe_$getName"),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(),
                  outputs = RAST.FnRetTyImpl(RAST.TyPath(ISZ(ISZ("Option"), ISZ(v.rustTypeName)), None()))),
                verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
              comments = ISZ(), attributes = ISZ(), meta = ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  let mut value: ${v.rustTypeName} = ${v.rustTypeName}::default();
                    |  if $getName(&mut value) {
                    |    return Some(value);
                    |  } else {
                    |    return None;
                    |  }
                    |}""")))))
            testMockVars = testMockVars :+ RAST.ItemStatic(
              ident = RAST.IdentString(mockVar),
              visibility = RAST.Visibility.Public,
              ty = RAST.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), ISZ(v.rustTypeName)), None()),
              mutability = RAST.Mutability.Not,
              expr = RAST.ExprST(st"Mutex::new(None);"))
            testingApis = testingApis :+ RAST.FnImpl(
              attributes = ISZ(RAST.AttributeST(F, st"cfg(test)")),
              sig = RAST.FnSig(
                ident = RAST.IdentString(getName),
                fnDecl = RAST.FnDecl(
                  inputs = valueParam,
                  outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
              comments = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match *$mockVar.lock().unwrap_or_else(|e| e.into_inner()) {
                    |    Some(v) => {
                    |      *value = v;
                    |      return true;
                    |    },
                    |    None => return false,
                    |  }
                    |}""")))))
            ingest = ingest :+
              st"""if let Some(v) = crate::bridge::extern_c_api::unsafe_$getName() {
                  |  _app.${v.varName} = v;
                  |}"""
          }

          crustApiContribs = crustApiContribs.addApiContributions(owner, apiContrib.combine(
            ComponentApiContributions.empty(
              externCApis = externCApis,
              unsafeExternCApiWrappers = wrappers,
              externApiTestMockVariables = testMockVars,
              externApiTestingApis = testingApis)))

          contributions = contributions.replaceComponentContributions(
            contributions.componentContributions + owner ~> contrib(
              libComputePre = contrib.libComputePre :+ RAST.BodyItemST(
                st"""// Injected GUMBO state variables, if the test controller set any.
                    |if crate::bridge::extern_c_api::unsafe_is_injection_enabled() {
                    |  ${(ingest, "\n")}
                    |}""")))
        case _ =>
      }
    }
    localStore = CRustApiPlugin.putCRustApiContributions(crustApiContribs, localStore)
    localStore = CRustComponentPlugin.putComponentContributions(contributions, localStore)

    contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
    contributions.componentContributions.get(threadPath) match {
      case Some(contrib) =>
        val versions = MicrokitUtil.getMicrokitVersions(localStore)
        val updated = contrib(
          // Property-based system tests: proptest runs on target without std, given alloc,
          // which the heap in the generated harness provides (see run_property there).
          crateDependencies = contrib.crateDependencies :+
            st"""proptest = { version = "${versions.get("proptest").get}", default-features = false, features = ["alloc", "no_std"] }
                |linked_list_allocator = "${versions.get("linked_list_allocator").get}"""",
          libModDecls = contrib.libModDecls :+ RAST.ItemST(st"mod system_tests;"),
          libComputePost = contrib.libComputePost :+ RAST.BodyItemST(
            st"""// Run the system test suite.  The controller is dispatched once: by the
                |// scheduler kick sent after every partition has reported ready.
                |crate::system_tests::run_all();"""))
        localStore = CRustComponentPlugin.putComponentContributions(
          contributions.replaceComponentContributions(
            contributions.componentContributions + threadPath ~> updated),
          localStore)
      case _ =>
        reporter.error(None(), toolName, "Test controller component contributions were not found")
    }

    return (localStore, ISZ())
  }

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                         symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        ExperimentalOptions.enableTestScheduler(options.experimentalOptions) &&
        MicrokitUtil.isMCS(options, symbolTable.rootSystem) &&
        SystemDescriptionProviderPlugin.getMSDOpt("normal", store).nonEmpty &&
        !TestSchedulerPlugin.hasHandled(store))
  }

  override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + TestSchedulerPlugin.KEY_handled ~> BoolValue(T)

    // Stage 7: the controller hosts the contract checks when the observers crate has any.
    val observeInfo: Option[ContractObserverInfo] =
      ContractObserverPlugin.getInfo(localStore) match {
        case Some(info) if info.hasLayers && localStore.contains(TestSchedulerPlugin.KEY_observersWired) &&
          TestSchedulerPlugin.checksRequested(symbolTable) => Some(info)
        case _ => None()
      }
    val hasObserve: B = observeInfo.nonEmpty
    // the layers the controller hosts, for the build-level switches (D23)
    val hasGumbo: B = observeInfo.nonEmpty && observeInfo.get.hasComponentLayer
    val hasSysverif: B = observeInfo.nonEmpty && observeInfo.get.compositionIds.nonEmpty
    var resources: ISZ[Resource] = ISZ()

    // State var inspection reads the sv_ ports, their regions and is_monitoring_enabled.
    // StateVarPortsPlugin creates them for system testing as well as for --runtime-monitoring
    // (design D22), so --runtime-monitoring is no longer required here (it was, under D5).

    // Base the variant on the pre-monitor snapshot when one exists.  Each monitor plugin
    // rewrites "normal" with every non-model protection domain stripped -- including this
    // plugin's controller -- so reading "normal" after they have run would yield a system
    // with no controller in it.  With monitoring off no monitor ran, nothing was stripped,
    // and "normal" is already the original.
    val base: SystemDescription =
      store.get(UserLandMonitorPlugin.MONITOR_ORIG_MSD_KEY) match {
        case Some(v) => v.asInstanceOf[SDValue].sd
        case _ => SystemDescriptionProviderPlugin.getMSD("normal", localStore)
      }

    val sysPath = model.components(0).identifier.name
    val ctrlPd = TestSchedulerPlugin.controllerPdName(sysPath)
    val ctrlMonPd = s"${ctrlPd}_MON"

    // Strip every other injected protection domain (the monitors) and its channels, keeping
    // only this variant's controller -- one injected PD per variant, as the monitor variants
    // do.  The monitor is deliberately absent: the controller does the checking, and leaving
    // the monitor in would extend the frame with slots the system under test does not have.
    var otherInjectedPds: Set[String] = Set.empty
    for (id <- StoreUtil.getSyntheticElements(localStore)) {
      val pdName = st"${(ops.ISZOps(id).drop(1), "_")}".render
      if (pdName != ctrlPd) {
        otherInjectedPds = otherInjectedPds + pdName + s"${pdName}_MON"
      }
    }

    // Stage 5: give the controller its own view of every port and sv_ region.  Read-write
    // throughout -- inspection needs read, injection needs write, and which ports a given
    // test treats as inputs is not knowable here.  The addresses must agree with the
    // constants compiled into the controller's accessors, which is why both come from
    // TestSchedulerPlugin.observableVaddrKiB over the same stored inventory.
    val observable = TestSchedulerPlugin.getObservable(localStore)
    var observableMaps: ISZ[MemoryMap] = ISZ()
    var oi: Z = 0
    for (r <- observable) {
      observableMaps = observableMaps :+ MemoryMap(
        memoryRegion = r.regionName,
        vaddrInKiBytes = TestSchedulerPlugin.observableVaddrKiB(observable, oi),
        perms = ISZ(Perm.READ, Perm.WRITE),
        varAddr = None(), cached = None())
      oi = oi + 1
    }
    // The observable, injection and thread-side injection blocks sit at fixed bases; now
    // that regions are sized by their contents, check none runs into the next.
    val injectablesForCheck = TestSchedulerPlugin.getInjectable(localStore)
    // (block, base, end, limit) in KiB
    val blocks: ISZ[(String, Z, Z, Z)] = ISZ(
      ("observable", TestSchedulerPlugin.observableBaseVaddrKiB,
        TestSchedulerPlugin.observableVaddrKiB(observable, observable.size), TestSchedulerPlugin.injectBaseVaddrKiB),
      ("injection", TestSchedulerPlugin.injectBaseVaddrKiB,
        TestSchedulerPlugin.injectVaddrKiB(injectablesForCheck, injectablesForCheck.size), TestSchedulerPlugin.injectThreadBaseVaddrKiB))
    for (b <- blocks if b._3 > b._4) {
      reporter.error(None(), toolName,
        s"The test controller's ${b._1} regions need ${b._3 - b._2} KiB of address space, more than the ${b._4 - b._2} KiB reserved for them")
    }

    // D16a: the injection regions are declared here rather than by any AADL port, so the
    // template creates them and both the owning thread and the controller get a map.
    val injectables = TestSchedulerPlugin.getInjectable(localStore)
    var injectRegions: ISZ[MemoryRegion] = ISZ()
    var injectPy: ISZ[ST] = ISZ()
    var injectThreadMaps: Map[String, ISZ[MemoryMap]] = Map.empty
    var injectControllerMaps: ISZ[MemoryMap] = ISZ()
    var ji: Z = 0
    for (v <- injectables) {
      injectRegions = injectRegions :+ GenericMemoryRegion(name = v.regionName, sizeInKiBytes = v.sizeInKiBytes)
      injectPy = injectPy :+
        st"""${v.regionName} = MemoryRegion(sdf, "${v.regionName}", ${MicrokitUtil.KiBytesToHexH(v.sizeInKiBytes, F)})
            |sdf.add_mr(${v.regionName})"""
      injectThreadMaps = injectThreadMaps + v.threadId ~> (
        injectThreadMaps.getOrElse(v.threadId, ISZ()) :+ MemoryMap(
          memoryRegion = v.regionName,
          vaddrInKiBytes = TestSchedulerPlugin.injectThreadVaddrKiB(injectables, ji),
          perms = ISZ(Perm.READ), varAddr = Some(v.queueVar), cached = None()))
      injectControllerMaps = injectControllerMaps :+ MemoryMap(
        memoryRegion = v.regionName,
        vaddrInKiBytes = TestSchedulerPlugin.injectVaddrKiB(injectables, ji),
        perms = ISZ(Perm.READ, Perm.WRITE), varAddr = None(), cached = None())
      ji = ji + 1
    }

    // Channel per thread: SystemDescriptionProvider_MCS renders `channel_<pd> = <domain>`
    // for every _MON, so the scheduling domain id is the channel id.  The controller's own
    // is omitted -- it holds no slot and is never a run_to_thread target.
    var observableChannels: ISZ[(String, Z)] = ISZ()
    for (sd <- base.schedulingDomains) {
      if (ops.StringOps(sd.componentName).endsWith("_MON") && sd.componentName != ctrlMonPd &&
          !otherInjectedPds.contains(sd.componentName)) {
        observableChannels = observableChannels :+ ((sd.componentName, sd.id))
      }
    }

    // Demote the controller so it is the lowest-priority protection domain (D4).
    def retargetChild(c: MicrokitDomain): MicrokitDomain = {
      c match {
        case childPd: ProtectionDomain => return retarget(childPd)
        case other => return other
      }
    }

    def retarget(pd: ProtectionDomain): ProtectionDomain = {
      val repriced: ProtectionDomain =
        if (pd.name == ctrlPd) pd(priority = Some(TestSchedulerPlugin.controllerPriority),
          memMaps = pd.memMaps ++ StaticContent.controllerMemMaps ++ observableMaps ++ injectControllerMaps)
        else if (pd.name == ctrlMonPd) pd(priority = Some(TestSchedulerPlugin.controllerMonPriority))
        else if (injectThreadMaps.contains(pd.name)) pd(memMaps = pd.memMaps ++ injectThreadMaps.get(pd.name).get)
        else pd
      val kids: ISZ[MicrokitDomain] = for (c <- repriced.children) yield retargetChild(c)
      return repriced(children = kids)
    }

    val keptPds: ISZ[ProtectionDomain] =
      for (pd <- base.protectionDomains.filter((pd: ProtectionDomain) => !otherInjectedPds.contains(pd.name))) yield retarget(pd)

    val keptChannels = base.channels.filter((c: Channel) =>
      !otherInjectedPds.contains(c.firstPD) && !otherInjectedPds.contains(c.secondPD))

    // The controller holds no timeslice (D4), so drop its scheduling slot and any other
    // injected PD's, then recompute the pad so the frame still sums to the frame period.
    val framePeriodNano: Z = MicrokitUtil.frameTimeNs(symbolTable, reporter)

    def isPad(sd: SchedulingDomain): B = {
      return sd.componentName == "pad" || sd.componentName == "padding"
    }

    // The frame's thread slots, padded out to the frame period.  The pad goes where the
    // production schedule has it (first when a monitor rebuilt "normal", last otherwise), so a
    // slot index means the same slot in the test variant as in the image that ships.
    def padded(slots: ISZ[SchedulingDomain], padFirst: B): ISZ[SchedulingDomain] = {
      var usedNano: Z = 0
      for (sd <- slots) {
        usedNano = usedNano + sd.length
      }
      val remainder: Z = framePeriodNano - usedNano
      if (remainder <= 0) {
        return slots
      }
      val pad = SchedulingDomain(id = 0, componentName = "pad", length = remainder, unit = HamrTimeUnit.ns, isUserPartition = F)
      return if (padFirst) pad +: slots else slots :+ pad
    }

    val threadSlots: ISZ[SchedulingDomain] = base.schedulingDomains.filter((sd: SchedulingDomain) =>
      !isPad(sd) && sd.componentName != ctrlMonPd && !otherInjectedPds.contains(sd.componentName))
    // Where the image that ships has its pad: a monitor plugin that ran rebuilt "normal" (pad
    // first); otherwise "normal" is CComponentPlugin_MCS's (pad last).  The snapshot `base`
    // predates any rebuild, so it cannot say.
    val shipped = SystemDescriptionProviderPlugin.getMSD("normal", localStore)
    val shippedPadFirst: B = shipped.schedulingDomains.nonEmpty && isPad(shipped.schedulingDomains(0))
    val scheds: ISZ[SchedulingDomain] = padded(threadSlots, shippedPadFirst)

    val testSd = SystemDescription(
      name = TestSchedulerPlugin.variantName,
      schedulingDomains = scheds,
      protectionDomains = keptPds,
      memoryRegions = base.memoryRegions ++ StaticContent.controllerMemoryRegions ++ injectRegions,
      channels = keptChannels,
      templateContributions = ISZ(StaticContent.regions_py) ++
        (if (injectPy.isEmpty) ISZ[ST]()
         else ISZ(st"""#######################################
                      |# STATE VARIABLE INJECTION
                      |# One region per GUMBO state variable, carrying a value from the test
                      |# controller to the owning thread.  Declared here rather than as AADL
                      |# ports so they stay out of the component test harness and the system
                      |# verification model (design D16a).
                      |#######################################
                      |${(injectPy, "\n\n")}""")),
      templateTailContributions = ISZ(StaticContent.testSelection_py(ctrlPd, hasGumbo, hasSysverif)))

    localStore = SystemDescriptionProviderPlugin.putMSD(TestSchedulerPlugin.variantName, testSd, localStore)

    // The image that ships must not be changed by system testing.  With no monitor plugin run,
    // "normal" still holds the controller, its channels and a slot, and the state variables'
    // sv_ regions -- mapped into their threads, which would then publish their state in
    // production (is_monitoring_enabled is a NULL check on those maps).  A monitor plugin that
    // ran rebuilt "normal" without its injected protection domains, but whether that rebuild
    // also dropped the sv_ regions depends on which monitor plugin wrote it last, so the
    // decision is keyed on what actually survives in "normal", not on whether a monitor ran.
    // It is rebuilt here the same way.  The maps' addresses are left as they are: a gap is
    // harmless.
    val normal = SystemDescriptionProviderPlugin.getMSD("normal", localStore)
    val ctrlPds: Set[String] = Set.empty[String] + ctrlPd + ctrlMonPd
    val svRegions: Set[String] = Set.empty[String] ++
      (for (r <- TestSchedulerPlugin.getObservable(localStore) if r.isStateVar) yield r.regionName)
    val normalHasTestOnly: B =
      ops.ISZOps(normal.protectionDomains).exists((pd: ProtectionDomain) => ctrlPds.contains(pd.name)) ||
        ops.ISZOps(normal.memoryRegions).exists((mr: MemoryRegion) => svRegions.contains(mr.name))
    if (normalHasTestOnly) {
      def withoutSv(d: MicrokitDomain): MicrokitDomain = {
        d match {
          case pd: ProtectionDomain =>
            return pd(
              memMaps = pd.memMaps.filter((m: MemoryMap) => !svRegions.contains(m.memoryRegion)),
              children = for (c <- pd.children) yield withoutSv(c))
          case other => return other
        }
      }
      val normalSlots = normal.schedulingDomains.filter((sd: SchedulingDomain) => !isPad(sd) && !ctrlPds.contains(sd.componentName))
      localStore = SystemDescriptionProviderPlugin.putMSD("normal", normal(
        schedulingDomains = padded(normalSlots, normal.schedulingDomains.nonEmpty && isPad(normal.schedulingDomains(0))),
        protectionDomains = for (pd <- normal.protectionDomains if !ctrlPds.contains(pd.name))
          yield withoutSv(pd).asInstanceOf[ProtectionDomain],
        memoryRegions = normal.memoryRegions.filter((mr: MemoryRegion) => !svRegions.contains(mr.name)),
        channels = normal.channels.filter((c: Channel) => !ctrlPds.contains(c.firstPD) && !ctrlPds.contains(c.secondPD))),
        localStore)
    }

    // The channel the scheduler uses to reach the controller.  SystemDescriptionProvider_MCS
    // renders `channel_<pd> = <schedulingDomain>` for every _MON protection domain, so the
    // MON's scheduling domain id IS the channel id.  The controller holds no timeslice, so
    // that domain is never dispatched -- it exists only to carry this id.
    def findMonChannel(pds: ISZ[ProtectionDomain]): Option[Z] = {
      for (pd <- pds) {
        if (pd.name == ctrlMonPd) {
          return pd.schedulingDomain
        }
        var childPds = ISZ[ProtectionDomain]()
        for (c <- pd.children) {
          c match {
            case childPd: ProtectionDomain => childPds = childPds :+ childPd
            case _ =>
          }
        }
        findMonChannel(childPds) match {
          case Some(z) => return Some(z)
          case _ =>
        }
      }
      return None()
    }

    val controllerChannel: Z = findMonChannel(base.protectionDomains) match {
      case Some(z) => z
      case _ =>
        reporter.error(None(), toolName,
          s"Could not determine the test controller's channel: protection domain '$ctrlMonPd' was not found in the system description")
        return (localStore, ISZ())
    }

    val schedulerPath = s"${options.sel4OutputDir.get}/scheduler"
    val v = TestSchedulerPlugin.variantName

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$schedulerPath/src/$v.scheduler.c",
      content = StaticContent.scheduler_c,
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$schedulerPath/include/$v.scheduler_config.h",
      content = StaticContent.scheduler_config_h(controllerChannel),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$schedulerPath/include/$v.user_config.h",
      content = StaticContent.user_config_h,
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"${options.sel4OutputDir.get}/$v.mk",
      content = StaticContent.mk,
      overwrite = T, isDatatype = F)

    // The controller crate's generated test support.  tests.rs is the user's script and is
    // preserved across regeneration; everything else here is overwritten.
    val stDir = s"${options.sel4OutputDir.get}/crates/${TestSchedulerPlugin.controllerName}/src/system_tests"


    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/mod.rs", content = StaticContent.systemTests_mod_rs(hasObserve), overwrite = T, isDatatype = F)

    observeInfo match {
      case Some(info) =>
        resources = resources :+ ResourceUtil.createResourceH(
          path = s"$stDir/observe.rs",
          content = StaticContent.systemTests_observe_rs(
            info = info, regions = observable, injectables = injectables,
            channelThreads = for (c <- observableChannels) yield ops.StringOps(c._1).substring(0, c._1.size - 4),
            reporter = reporter),
          overwrite = T, isDatatype = F)
      case _ =>
    }

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/selection.rs", content = StaticContent.systemTests_selection_rs, overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/api.rs",
      content = StaticContent.systemTests_api_rs(TestSchedulerPlugin.controllerName, hasObserve),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/inspect.rs",
      content = StaticContent.systemTests_inspect_rs(
        regions = observable, injectables = injectables, channels = observableChannels,
        preStates = TestSchedulerPlugin.threadPreStates(observable, injectables, symbolTable, localStore),
        hasObserve = hasObserve),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/harness.rs",
      content = StaticContent.systemTests_harness_rs(TestSchedulerPlugin.controllerName, hasObserve),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResource(
      s"$stDir/tests.rs", StaticContent.systemTests_tests_rs, F)

    // Host-side driver (D18).  Lives beside the generated Microkit project rather than in the
    // model's sysml/bin, so it is self-contained and works for any model.
    resources = resources :+ ResourceUtil.createExeResource(
      path = s"${options.sel4OutputDir.get}/bin/run-tests.cmd",
      content = StaticContent.runTests_cmd(TestSchedulerPlugin.qemuTimeoutSeconds),
      overwrite = T)

    return (localStore, resources)
  }
}

object StaticContent {

  val v: String = TestSchedulerPlugin.variantName

  // Virtual addresses, in KiB, matching TEST_*_VADDR in the generated scheduler_config.h:
  // 0x4_002_000 / 0x4_003_000 / 0x4_004_000.  Each region is 4 KiB, so consecutive regions
  // are 4 KiB apart -- MicrokitUtil.KiBytesToHex rounds up to the memory alignment, so
  // values that are not already aligned silently collapse onto the same address.
  val testCmdVaddrKiB: Z = 65544
  val testStatusVaddrKiB: Z = 65548
  val testScheduleVaddrKiB: Z = 65552

  /** The regions are created by the template contribution below, so they are declared to the
    * system description as template-managed: the MEMORY REGIONS loop then emits only the
    * controller's add_map calls and not a second MemoryRegion(...) for each.
    */
  val controllerMemoryRegions: ISZ[MemoryRegion] = ISZ(
    GenericMemoryRegion(name = "test_cmd", sizeInKiBytes = 4),
    GenericMemoryRegion(name = "test_status", sizeInKiBytes = 4),
    GenericMemoryRegion(name = "test_schedule", sizeInKiBytes = 4))

  /** The controller's side of the three regions, mirroring the scheduler's: it writes
    * commands and reads status and the schedule.  No setvar_vaddr -- the controller
    * addresses them through the same TEST_*_VADDR constants the scheduler uses.
    */
  val controllerMemMaps: ISZ[MemoryMap] = ISZ(
    MemoryMap(memoryRegion = "test_cmd", vaddrInKiBytes = testCmdVaddrKiB,
      perms = ISZ(Perm.READ, Perm.WRITE), varAddr = None(), cached = None()),
    MemoryMap(memoryRegion = "test_status", vaddrInKiBytes = testStatusVaddrKiB,
      perms = ISZ(Perm.READ), varAddr = None(), cached = None()),
    MemoryMap(memoryRegion = "test_schedule", vaddrInKiBytes = testScheduleVaddrKiB,
      perms = ISZ(Perm.READ), varAddr = None(), cached = None()))

  /** Python injected into test_scheduler.meta.py via SystemDescription.templateContributions.
    * The three regions are mapped into the scheduler protection domain only; stage 3 adds the
    * controller's maps of the same regions.
    */
  val regions_py: ST =
    st"""#######################################
        |# TEST SCHEDULER REGIONS
        |# Command input, status output, and the published schedule.  The virtual addresses
        |# must match TEST_CMD_VADDR / TEST_STATUS_VADDR / TEST_SCHEDULE_VADDR in
        |# $v.scheduler_config.h.  Change both if there is a conflict.
        |#######################################
        |TEST_CMD_VADDR      = 0x4_002_000
        |TEST_CMD_SIZE       = 0x1000  # 4 KB
        |TEST_STATUS_VADDR   = 0x4_003_000
        |TEST_STATUS_SIZE    = 0x1000  # 4 KB
        |TEST_SCHEDULE_VADDR = 0x4_004_000
        |TEST_SCHEDULE_SIZE  = 0x1000  # 4 KB
        |
        |test_cmd = MemoryRegion(sdf, "test_cmd", TEST_CMD_SIZE)
        |sdf.add_mr(test_cmd)
        |scheduler.add_map(Map(test_cmd, TEST_CMD_VADDR, perms="r"))
        |
        |test_status = MemoryRegion(sdf, "test_status", TEST_STATUS_SIZE)
        |sdf.add_mr(test_status)
        |scheduler.add_map(Map(test_status, TEST_STATUS_VADDR, perms="rw"))
        |
        |test_schedule = MemoryRegion(sdf, "test_schedule", TEST_SCHEDULE_SIZE)
        |sdf.add_mr(test_schedule)
        |scheduler.add_map(Map(test_schedule, TEST_SCHEDULE_VADDR, perms="rw"))"""


  // ---------------------------------------------------------------------------
  // Controller crate: src/system_tests/
  // ---------------------------------------------------------------------------

  @pure def systemTests_mod_rs(hasObserve: B): ST = {
    // A separate block rather than an interpolation at the end of a line: ST indents the
    // lines of an interpolated value to the column it starts at.
    val observeDoc: ISZ[ST] =
      if (hasObserve) ISZ(st"""//!
                             |//! `observe` checks the model's contracts at every dispatch and turns a
                             |//! violation into a failure of the running test.""")
      else ISZ()
    val mods: ISZ[String] =
      ISZ[String]("api", "harness", "inspect") ++ (if (hasObserve) ISZ[String]("observe") else ISZ[String]()) ++
        ISZ[String]("selection", "tests")
    val modDecls: ISZ[ST] = for (m <- mods) yield st"pub mod $m;"
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! System test support for the test controller.
          |//!
          |//! `api` drives the test scheduler, `harness` runs the suite and records results,
          |${(st"//! and `tests` holds the hand-written test script (preserved across regeneration)." +: observeDoc, "\n")}
          |
          |${(modDecls, "\n")}
          |
          |pub fn run_all() {
          |  harness::run_all();
          |}
          |""")
  }

  val systemTests_selection_rs: ST =
    st"""${CommentTemplate.doNotEditComment_slash}
        |
        |//! Which tests to run, patched into the controller ELF at image build time.
        |//!
        |//! An all-zero filter -- the default when nothing has been patched in -- means run
        |//! everything.  The build populates it from the TESTS make variable.
        |
        |pub const FILTER_LEN: usize = 256;
        |
        |#[repr(C)]
        |pub struct TestSelection {
        |  pub filter: [u8; FILTER_LEN],
        |  pub flags: u32,
        |}
        |
        |#[no_mangle]
        |#[link_section = ".test_selection"]
        |pub static mut TEST_SELECTION: TestSelection = TestSelection {
        |  filter: [0u8; FILTER_LEN],
        |  flags: 0,
        |};
        |
        |/// Bits of `flags`, set at image build time: a contract-checking layer turned off for
        |/// the whole run (GUMBO_CHECKS=off, SYSVERIF_CHECKS=off), and list-only mode.
        |pub const FLAG_GUMBO_OFF: u32 = 0x1;
        |pub const FLAG_SYSVERIF_OFF: u32 = 0x2;
        |/// LIST_TESTS=1: print the test table and run nothing.
        |pub const FLAG_LIST_ONLY: u32 = 0x4;
        |
        |/// The flags patched in at image build time.  Read volatile: the value is patched into
        |/// the ELF after compilation, so the compiler must not assume the initializer.
        |pub fn flags() -> u32 {
        |  unsafe { core::ptr::read_volatile(core::ptr::addr_of!(TEST_SELECTION.flags)) }
        |}
        |
        |/// The filter as a string slice, empty when unset.  A filter that is not UTF-8 --
        |/// which the build does not produce -- selects nothing rather than everything.
        |pub fn filter() -> &'static str {
        |  unsafe {
        |    let bytes = &*core::ptr::addr_of!(TEST_SELECTION.filter);
        |    let mut n = 0usize;
        |    while n < FILTER_LEN && bytes[n] != 0 {
        |      n += 1;
        |    }
        |    match core::str::from_utf8(&bytes[..n]) {
        |      Ok(s) => s,
        |      Err(_) => "\0", // no test name contains NUL
        |    }
        |  }
        |}
        |
        |/// True when `name` should run under `filter`.  Substring match, so a qualified
        |/// name makes "suite::" select a whole suite and a longer prefix select one test.
        |pub fn selects(filter: &str, name: &str) -> bool {
        |  if filter.is_empty() {
        |    return true;
        |  }
        |  let (f, n) = (filter.as_bytes(), name.as_bytes());
        |  if f.len() > n.len() {
        |    return false;
        |  }
        |  let mut i = 0usize;
        |  while i + f.len() <= n.len() {
        |    if &n[i..i + f.len()] == f {
        |      return true;
        |    }
        |    i += 1;
        |  }
        |  false
        |}
        |"""


  @pure def systemTests_api_rs(controllerName: String, hasObserve: B): ST = {
    // Stage 7: with contract checking, every command parks before each user dispatch and
    // the controller checks there, and again when the command completes.
    val waitLoop: ST =
      if (hasObserve)
        st"""let status = TEST_STATUS_VADDR as *const TestStatus;
            |loop {
            |  if read_volatile(addr_of!((*status).ack_seq)) == seq {
            |    break;
            |  }
            |  let obs = read_volatile(addr_of!((*status).obs_seq));
            |  if obs != LAST_OBS_SEQ {
            |    // The scheduler is parked before a dispatch: check, then let it go ahead.
            |    fence(Ordering::Acquire);
            |    LAST_OBS_SEQ = obs;
            |    crate::system_tests::observe::at_park(&read_volatile(status));
            |    fence(Ordering::Release);
            |    write_volatile(addr_of_mut!((*cmd).obs_ack), obs);
            |    ${controllerName}_notify_scheduler();
            |  }
            |}
            |fence(Ordering::Acquire);
            |let st = read_volatile(status);
            |// The command's last dispatch has completed, and no park follows it until the next
            |// command -- which may be after the test has ended.
            |crate::system_tests::observe::at_command_end(&st);
            |st"""
      else
        st"""let status = TEST_STATUS_VADDR as *const TestStatus;
            |while read_volatile(addr_of!((*status).ack_seq)) != seq {}
            |fence(Ordering::Acquire);
            |read_volatile(status)"""
    val observeFlag: ST =
      if (hasObserve) st"if crate::system_tests::observe::any_live() { 1 } else { 0 }"
      else st"0"
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Command API over the test_cmd / test_status shared memory regions.
          |
          |use core::ptr::{addr_of, addr_of_mut, read_volatile, write_volatile};
          |use core::sync::atomic::{fence, Ordering};
          |
          |extern "C" {
          |  fn ${controllerName}_notify_scheduler();
          |}
          |
          |// Must match TEST_*_VADDR in $v.scheduler_config.h and $v.meta.py.
          |pub const TEST_CMD_VADDR: usize = 0x400_2000;
          |pub const TEST_STATUS_VADDR: usize = 0x400_3000;
          |pub const TEST_SCHEDULE_VADDR: usize = 0x400_4000;
          |
          |// Must match TEST_*_SIZE in $v.scheduler_config.h and $v.meta.py.
          |pub const TEST_REGION_SIZE: usize = 0x1000;
          |
          |pub const MAX_SCHEDULE_SLOTS: usize = 128;
          |
          |pub const CMD_SSTEP: u32 = 2;
          |pub const CMD_HSTEP: u32 = 3;
          |pub const CMD_RUN_TO_SLOT: u32 = 4;
          |pub const CMD_RUN_TO_HP: u32 = 5;
          |pub const CMD_RUN_TO_STATE: u32 = 6;
          |pub const CMD_RUN_TO_THREAD: u32 = 7;
          |pub const CMD_INFO_STATE: u32 = 8;
          |pub const CMD_INFO_SCHEDULE: u32 = 9;
          |pub const CMD_STOP: u32 = 10;
          |
          |pub const FLAG_STOPPED: u32 = 0x1;
          |pub const FLAG_BAD_COMMAND: u32 = 0x2;
          |pub const FLAG_UNREACHABLE: u32 = 0x4;
          |pub const FLAG_OVERRUN: u32 = 0x8;
          |
          |#[repr(C)]
          |struct TestCommand {
          |  seq: u32,
          |  typ: u32,
          |  count: u32,
          |  target_ch: u32,
          |  target_hp: u32,
          |  target_slot: u32,
          |  observe: u32,
          |  obs_ack: u32,
          |}
          |
          |#[repr(C)]
          |#[derive(Copy, Clone)]
          |pub struct TestStatus {
          |  pub ack_seq: u32,
          |  pub current_timeslice: u32,
          |  pub hyperperiod_num: u32,
          |  pub last_dispatched_ch: u32,
          |  pub flags: u32,
          |  /// The channel of the slot at current_timeslice.
          |  pub next_ch: u32,
          |  /// User-slot completions so far, observed or not.
          |  pub completed_seq: u32,
          |  /// Incremented at each observation park.
          |  pub obs_seq: u32,
          |}
          |
          |/// Whether commands park before each user dispatch for the contract checks: while any
          |/// of them is tracked.
          |fn observe_flag() -> u32 {
          |  $observeFlag
          |}
          |
          |#[repr(C)]
          |pub struct TestSchedule {
          |  pub num_timeslices: u32,
          |  pub timeslice_ch: [u32; MAX_SCHEDULE_SLOTS],
          |  pub timeslices: [u64; MAX_SCHEDULE_SLOTS],
          |  pub is_user_partition: [bool; MAX_SCHEDULE_SLOTS],
          |}
          |
          |// Each struct fills a fixed region; one that outgrew it would spill into the next.
          |const _: () = assert!(core::mem::size_of::<TestCommand>() <= TEST_REGION_SIZE);
          |const _: () = assert!(core::mem::size_of::<TestStatus>() <= TEST_REGION_SIZE);
          |const _: () = assert!(core::mem::size_of::<TestSchedule>() <= TEST_REGION_SIZE);
          |
          |static mut NEXT_SEQ: u32 = 0;
          |static mut LAST_OBS_SEQ: u32 = 0;
          |
          |/// Issues a command and waits for it; a command that did not do what it was asked fails
          |/// the running test (see `harness::command_outcome`).
          |fn issue(typ: u32, count: u32, target_ch: u32, target_hp: u32, target_slot: u32) -> TestStatus {
          |  let st = exchange(typ, count, target_ch, target_hp, target_slot);
          |  crate::system_tests::harness::command_outcome(command_name(typ), st.flags);
          |  st
          |}
          |
          |fn command_name(typ: u32) -> &'static str {
          |  match typ {
          |    CMD_SSTEP => "sstep",
          |    CMD_HSTEP => "hstep",
          |    CMD_RUN_TO_SLOT => "run_to_slot",
          |    CMD_RUN_TO_HP => "run_to_hp",
          |    CMD_RUN_TO_STATE => "run_to_state",
          |    CMD_RUN_TO_THREAD => "run_to_thread",
          |    CMD_INFO_STATE => "info_state",
          |    CMD_INFO_SCHEDULE => "info_schedule",
          |    CMD_STOP => "stop",
          |    _ => "command",
          |  }
          |}
          |
          |fn exchange(typ: u32, count: u32, target_ch: u32, target_hp: u32, target_slot: u32) -> TestStatus {
          |  unsafe {
          |    let cmd = TEST_CMD_VADDR as *mut TestCommand;
          |    NEXT_SEQ = NEXT_SEQ.wrapping_add(1);
          |    let seq = NEXT_SEQ;
          |
          |    write_volatile(addr_of_mut!((*cmd).typ), typ);
          |    write_volatile(addr_of_mut!((*cmd).count), count);
          |    write_volatile(addr_of_mut!((*cmd).target_ch), target_ch);
          |    write_volatile(addr_of_mut!((*cmd).target_hp), target_hp);
          |    write_volatile(addr_of_mut!((*cmd).target_slot), target_slot);
          |    write_volatile(addr_of_mut!((*cmd).observe), observe_flag());
          |
          |    // Publish the body before the sequence number that advertises it.
          |    fence(Ordering::Release);
          |    write_volatile(addr_of_mut!((*cmd).seq), seq);
          |
          |    ${controllerName}_notify_scheduler();
          |
          |    // Busy-wait for the acknowledgement.  This protection domain is the lowest
          |    // priority in the system, so every other one preempts this loop and the
          |    // schedule advances while it spins.  The load must be volatile: a plain read
          |    // is hoisted out of the loop and never observes the update.
          |    $waitLoop
          |  }
          |}
          |
          |/// Step `n` schedule slots.
          |pub fn sstep(n: u32) -> TestStatus { issue(CMD_SSTEP, n, 0, 0, 0) }
          |
          |/// Step `n` hyperperiods: finish the one in progress, then `n - 1` whole ones.
          |pub fn hstep(n: u32) -> TestStatus { issue(CMD_HSTEP, n, 0, 0, 0) }
          |
          |/// Run until slot `slot` is the next to be dispatched.
          |pub fn run_to_slot(slot: u32) -> TestStatus { issue(CMD_RUN_TO_SLOT, 0, 0, 0, slot) }
          |
          |/// Run until hyperperiod `hp`.
          |pub fn run_to_hp(hp: u32) -> TestStatus { issue(CMD_RUN_TO_HP, 0, 0, hp, 0) }
          |
          |/// Run until `(hp, slot)`.
          |pub fn run_to_state(hp: u32, slot: u32) -> TestStatus { issue(CMD_RUN_TO_STATE, 0, 0, hp, slot) }
          |
          |/// Run until the slot of the thread reached over channel `ch` is next.
          |pub fn run_to_thread(ch: u32) -> TestStatus { issue(CMD_RUN_TO_THREAD, 0, ch, 0, 0) }
          |
          |/// Current scheduler position.
          |pub fn info_state() -> TestStatus { issue(CMD_INFO_STATE, 0, 0, 0, 0) }
          |
          |/// End the session.  The scheduler idles permanently and is not resumable.
          |pub fn stop() -> TestStatus { issue(CMD_STOP, 0, 0, 0, 0) }
          |
          |/// The schedule the scheduler published at init.
          |pub fn schedule() -> &'static TestSchedule {
          |  unsafe { &*(TEST_SCHEDULE_VADDR as *const TestSchedule) }
          |}
          |""")
  }


  @pure def systemTests_harness_rs(controllerName: String, hasObserve: B): ST = {
    // Stage 7: contract checking.  The initialization checks run once, before the first
    // test; each test's body is its recording window.
    val suiteStart: ISZ[ST] = ISZ(
      st"""// The scheduler notifies this protection domain on every command completion, and
          |// those notifications accumulate as a pending bit while the suite runs without
          |// returning to the event loop.  Without this guard the pending signal re-enters
          |// the entrypoint once the suite finishes and the whole suite runs again, forever.
          |unsafe {
          |  if SUITE_DONE { return; }
          |  SUITE_DONE = true;
          |}""") ++
      (if (hasObserve) ISZ(
        st"""// Every thread has initialized and none has computed: check the initialization
            |// guarantees.  Not part of any test; a violation shows as DONE init=failed.  Not
            |// when only listing: nothing runs.
            |if selection::flags() & selection::FLAG_LIST_ONLY == 0 {
            |  crate::system_tests::observe::on_init();
            |}""")
       else ISZ[ST]())
    val testBody: ISZ[ST] =
      if (hasObserve) ISZ(
        st"crate::system_tests::observe::begin_test();",
        st"body();",
        st"crate::system_tests::observe::end_test();")
      else ISZ(st"body();")
    val initField: ST =
      if (hasObserve) st"""if crate::system_tests::observe::init_ok() { "ok" } else { "failed" }"""
      else st""""ok""""
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Test registration, assertions and the runner.
          |//!
          |//! cargo's test harness is not available here: this crate is no_std on target, so
          |//! there is no #[test] collection and no catch_unwind.  A failing assertion records
          |//! the failure and returns from the test body instead of panicking, which would
          |//! take the protection domain down and end the run.
          |//!
          |//! Property-based tests use proptest, which runs here without std given alloc:
          |//! `run_property` sets up a heap, generates cases, and shrinks a failure to a
          |//! minimal input.
          |
          |extern crate alloc;
          |
          |use crate::system_tests::api;
          |use crate::system_tests::selection;
          |use core::fmt::Write;
          |
          |extern "C" {
          |  fn ${controllerName}_print(s: *const u8);
          |}
          |
          |const LINE_LEN: usize = 256;
          |
          |/// Fixed-capacity line buffer.  Output is built in place rather than with format!,
          |/// so reporting never depends on the heap.  Overlong lines are truncated rather
          |/// than lost.
          |pub struct LineBuf {
          |  buf: [u8; LINE_LEN],
          |  len: usize,
          |}
          |
          |impl LineBuf {
          |  pub fn new() -> Self { LineBuf { buf: [0u8; LINE_LEN], len: 0 } }
          |
          |  pub fn emit(&mut self) {
          |    let end = if self.len < LINE_LEN - 2 { self.len } else { LINE_LEN - 2 };
          |    self.buf[end] = b'\n';
          |    self.buf[end + 1] = 0;
          |    unsafe { ${controllerName}_print(self.buf.as_ptr()); }
          |    self.len = 0;
          |  }
          |}
          |
          |impl Write for LineBuf {
          |  fn write_str(&mut self, s: &str) -> core::fmt::Result {
          |    for b in s.as_bytes() {
          |      if self.len + 2 < LINE_LEN {
          |        self.buf[self.len] = *b;
          |        self.len += 1;
          |      }
          |    }
          |    Ok(())
          |  }
          |}
          |
          |pub fn line(args: core::fmt::Arguments) {
          |  let mut b = LineBuf::new();
          |  let _ = b.write_fmt(args);
          |  b.emit();
          |}
          |
          |static mut CURRENT_FAILED: bool = false;
          |static mut FAIL_PRINTED: bool = false;
          |/// Whether a test body is running, so a failure has a test to be charged to.
          |static mut IN_TEST: bool = false;
          |static mut SUITE_DONE: bool = false;
          |
          |/// Record a failed assertion.  Called by the sys_assert macros, which return from
          |/// the test body immediately afterwards.
          |pub fn fail(file: &str, line_no: u32, what: &str) {
          |  fail_with(format_args!("{}:{} {}", file, line_no, what));
          |}
          |
          |/// A command that did not do what it was asked fails the running test
          |/// (TestScheduler-design.md, D10): a thread that overran its slot's watchdog, a
          |/// `run_to_*` target never reached, or a command the scheduler rejected.  A test that
          |/// carried on would be testing something other than what it says.  Between tests --
          |/// the runner's own normalization -- there is no test to fail, so it is reported as
          |/// INFO; a FAIL line there would break the host driver's count.
          |pub fn command_outcome(command: &str, flags: u32) {
          |  // Stop dispatches nothing, so nothing about it can fail: an overrun still outstanding
          |  // when it is issued was already charged to the command that met it
          |  if command == "stop" {
          |    return;
          |  }
          |  let why = if flags & api::FLAG_OVERRUN != 0 {
          |    "a thread overran its slot's watchdog, or is still running after one"
          |  } else if flags & api::FLAG_UNREACHABLE != 0 {
          |    "its target was not reached"
          |  } else if flags & api::FLAG_BAD_COMMAND != 0 {
          |    "the scheduler rejected it"
          |  } else if flags & api::FLAG_STOPPED != 0 {
          |    // STOPPED is sticky: after a test calls api::stop() nothing is dispatched again, and
          |    // every later test would run against a frozen system without noticing
          |    "the session was stopped (api::stop), so nothing runs"
          |  } else {
          |    return;
          |  };
          |  if unsafe { IN_TEST } {
          |    fail_with(format_args!("{} failed: {} (flags 0x{:x})", command, why, flags));
          |  } else {
          |    line(format_args!("TEST | INFO  between tests, {} failed: {} (flags 0x{:x})", command, why, flags));
          |  }
          |}
          |
          |/// Fail the running test.  Only its first failure prints a `TEST | FAIL` line -- the
          |/// host driver counts those against DONE's failed= -- and later ones print as INFO.
          |pub fn fail_with(args: core::fmt::Arguments) {
          |  unsafe {
          |    CURRENT_FAILED = true;
          |    if FAIL_PRINTED {
          |      line(format_args!("TEST | INFO  also failed: {}", args));
          |    } else {
          |      FAIL_PRINTED = true;
          |      line(format_args!("TEST | FAIL  {}", args));
          |    }
          |  }
          |}
          |
          |pub fn run_all() {
          |  ${(suiteStart, "\n\n")}
          |
          |  let filter = selection::filter();
          |  let list_only = selection::flags() & selection::FLAG_LIST_ONLY != 0;
          |  let mut matched: u32 = 0;
          |  let mut passed: u32 = 0;
          |  let mut failed: u32 = 0;
          |
          |  // The test table (D17): every registered test, and whether this run selects it, so
          |  // the host can discover what is runnable.
          |  for (name, _) in crate::system_tests::tests::SYSTEM_TESTS {
          |    if selection::selects(filter, name) {
          |      line(format_args!("TEST | LIST  {}", name));
          |    } else {
          |      line(format_args!("TEST | LIST  {} (not selected)", name));
          |    }
          |  }
          |
          |  for (name, body) in crate::system_tests::tests::SYSTEM_TESTS {
          |    if !selection::selects(filter, name) {
          |      continue;
          |    }
          |    matched += 1;
          |    if list_only {
          |      continue;
          |    }
          |
          |    // Normalize the schedule position so a step means the same thing in every
          |    // test.  Component state is NOT reset: tests are order-independent and
          |    // establish their own preconditions.  A thread that overruns meanwhile leaves the
          |    // position on its slot; the command is repeated (the scheduler takes the late
          |    // completion first), and a test that still would not start at the frame's start
          |    // is failed rather than left to count its steps from somewhere else.
          |    let mut st = api::run_to_slot(0);
          |    let mut tries = 1;
          |    while st.flags & api::FLAG_OVERRUN != 0 && tries < 4 {
          |      st = api::run_to_slot(0);
          |      tries += 1;
          |    }
          |    let stopped = st.flags & api::FLAG_STOPPED != 0;
          |    // still overrun: at another slot, or on slot 0 itself -- which the position does
          |    // not show, as it stays on the overran slot
          |    let not_at_start = (st.current_timeslice != 0 || st.flags & api::FLAG_OVERRUN != 0) && !stopped;
          |
          |    unsafe {
          |      CURRENT_FAILED = false;
          |      FAIL_PRINTED = false;
          |    }
          |    line(format_args!("TEST | BEGIN {}", name));
          |    unsafe { IN_TEST = true; }
          |    // A test that cannot run as written is failed without running it: after api::stop()
          |    // nothing is dispatched, so one that only injects and inspects would pass against a
          |    // frozen system; one not at the frame's start would count its steps, and leave its
          |    // injections, from somewhere else.
          |    if stopped {
          |      fail_with(format_args!("not run: the session was stopped (api::stop) by an earlier test, so nothing runs (flags 0x{:x})", st.flags));
          |    } else if not_at_start {
          |      fail_with(format_args!("not run: the test could not start at the frame's start: the schedule is at slot {}{} (flags 0x{:x})",
          |        st.current_timeslice,
          |        if st.flags & api::FLAG_OVERRUN != 0 { ", whose thread overran and has not completed" } else { "" },
          |        st.flags));
          |    } else {
          |      ${(testBody, "\n")}
          |    }
          |    unsafe { IN_TEST = false; }
          |
          |    if unsafe { CURRENT_FAILED } {
          |      failed += 1;
          |    } else {
          |      passed += 1;
          |      line(format_args!("TEST | PASS  {}", name));
          |    }
          |  }
          |
          |  // The host driver treats a run without this line as a failure, whatever preceded
          |  // it -- that is what catches a hang, a panic, or a controller that never started.
          |  // matched is what catches a filter that selected nothing, init= a violation of an
          |  // initialization guarantee, which belongs to no test, and list=1 a run that only
          |  // listed the tests.
          |  line(format_args!("TEST | DONE  matched={} passed={} failed={} init={}{}", matched, passed, failed,
          |    $initField, if list_only { " list=1" } else { "" }));
          |
          |  let _ = api::stop();
          |}
          |
          |/// Declare the system tests.  Generates the bodies plus the registration table the
          |/// runner walks; suites qualify the registered names, which is what gives the
          |/// TESTS= filter its granularity.  Each suite is a module of its own (so a suite must
          |/// not share its name with anything the file declares or imports).
          |///
          |/// A suite may switch contract checking for each of its tests, e.g.
          |/// `suite fault_injection(gumbo = off) { .. }` (TestScheduler-design.md, D23); the
          |/// keys are the model's layers, `gumbo` and `sysverif`, and the values `on` and `off`,
          |/// so a typo -- or a layer the model does not have -- is a compile error.
          |#[macro_export]
          |macro_rules! system_tests {
          |  // Suites are first normalized to `suite name [settings] { .. }`, so each suite's
          |  // settings are a single token tree the per-test expansion below can repeat.
          |  ( suite $$( $$t:tt )* ) => {
          |    $$crate::system_tests!(@norm [] suite $$( $$t )*);
          |  };
          |  (@norm [ $$( $$done:tt )* ]) => {
          |    $$crate::system_tests!(@emit $$( $$done )*);
          |  };
          |  (@norm [ $$( $$done:tt )* ] suite $$s:ident ( $$( $$set:tt )* ) { $$( $$b:tt )* } $$( $$rest:tt )*) => {
          |    $$crate::system_tests!(@norm [ $$( $$done )* suite $$s [ $$( $$set )* ] { $$( $$b )* } ] $$( $$rest )*);
          |  };
          |  (@norm [ $$( $$done:tt )* ] suite $$s:ident { $$( $$b:tt )* } $$( $$rest:tt )*) => {
          |    $$crate::system_tests!(@norm [ $$( $$done )* suite $$s [] { $$( $$b )* } ] $$( $$rest )*);
          |  };
          |  // Each suite is a module, so two suites may each have a test of the same name; it sees
          |  // everything the enclosing file declares or imports.
          |  (@emit $$( suite $$suite:ident $$settings:tt { $$( fn $$name:ident () $$body:block )* } )+ ) => {
          |    $$(
          |      #[allow(non_snake_case)]
          |      mod $$suite {
          |        #[allow(unused_imports)]
          |        use super::*;
          |        $$( pub(super) fn $$name() { $$crate::suite_settings!($$settings); $$body } )*
          |      }
          |    )+
          |    pub static SYSTEM_TESTS: &[(&str, fn())] = &[
          |      $$( $$( (concat!(stringify!($$suite), "::", stringify!($$name)), $$suite::$$name as fn()), )* )+
          |    ];
          |  };
          |  ( $$( fn $$name:ident () $$body:block )+ ) => {
          |    $$( fn $$name() $$body )+
          |    pub static SYSTEM_TESTS: &[(&str, fn())] = &[
          |      $$( (stringify!($$name), $$name as fn()), )+
          |    ];
          |  };
          |}
          |
          |/// Applies a suite's settings at the start of each of its tests; see `system_tests!`.
          |#[doc(hidden)]
          |#[macro_export]
          |macro_rules! suite_settings {
          |  ( [] ) => {};
          |  ( [ $$( $$k:ident = $$v:ident ),* $$(,)? ] ) => {
          |    $$( $$crate::system_tests::observe::suite_setting::$$k($$crate::system_tests::observe::suite_setting::$$v); )*
          |  };
          |}
          |
          |/// Split the system tests across files, one per `mod`, each holding its own
          |/// `system_tests!` block.  Used in place of `system_tests!` in tests.rs; the modules
          |/// resolve to `system_tests/tests/<name>.rs`.  Name them `<something>_tests.rs` so
          |/// the model's clean script preserves them.  The per-file tables are concatenated at
          |/// compile time, in declaration order, into the one table the runner walks.
          |#[macro_export]
          |macro_rules! system_test_files {
          |  ( $$( mod $$m:ident; )+ ) => {
          |    $$( mod $$m; )+
          |    const SYSTEM_TESTS_LEN: usize = 0 $$( + $$m::SYSTEM_TESTS.len() )+;
          |    const SYSTEM_TESTS_ALL: [(&str, fn()); SYSTEM_TESTS_LEN] =
          |      $$crate::system_tests::harness::concat_tables(&[ $$( $$m::SYSTEM_TESTS ),+ ]);
          |    pub static SYSTEM_TESTS: &[(&str, fn())] = &SYSTEM_TESTS_ALL;
          |  };
          |}
          |
          |fn no_test() {}
          |
          |/// Concatenate test tables into one array of length `N`, which must be their total
          |/// length.  Only called from `system_test_files!`, in a const context.
          |pub const fn concat_tables<const N: usize>(
          |    tables: &[&[(&'static str, fn())]]) -> [(&'static str, fn()); N] {
          |  let mut out: [(&'static str, fn()); N] = [("", no_test as fn()); N];
          |  let mut k = 0usize;
          |  let mut t = 0usize;
          |  while t < tables.len() {
          |    let table = tables[t];
          |    let mut i = 0usize;
          |    while i < table.len() {
          |      out[k] = table[i];
          |      k += 1;
          |      i += 1;
          |    }
          |    t += 1;
          |  }
          |  if k != N {
          |    panic!("concat_tables: N does not match the total table length");
          |  }
          |  out
          |}
          |
          |/// Assert a condition, recording a failure and returning instead of panicking.
          |#[macro_export]
          |macro_rules! sys_assert {
          |  ($$cond:expr) => {
          |    if !($$cond) {
          |      $$crate::system_tests::harness::fail(file!(), line!(), stringify!($$cond));
          |      return;
          |    }
          |  };
          |}
          |
          |/// Assert equality, recording a failure and returning instead of panicking.
          |#[macro_export]
          |macro_rules! sys_assert_eq {
          |  ($$l:expr, $$r:expr) => {
          |    if !($$l == $$r) {
          |      $$crate::system_tests::harness::fail(
          |        file!(), line!(), concat!(stringify!($$l), " == ", stringify!($$r)));
          |      return;
          |    }
          |  };
          |}
          |
          |// ---------------------------------------------------------------------------------
          |// Property-based tests
          |//
          |// proptest needs only alloc, so the controller gets a heap, set up on the first
          |// property test.  There is no OS entropy on target, so the generator is seeded from
          |// PROPTEST_SEED: every run explores the same cases, and a failure is reproducible.
          |// ---------------------------------------------------------------------------------
          |
          |const HEAP_SIZE: usize = 256 * 1024;
          |static mut HEAP: [u8; HEAP_SIZE] = [0u8; HEAP_SIZE];
          |
          |// Host builds (cargo test) keep std's allocator.
          |#[cfg(not(test))]
          |#[global_allocator]
          |static ALLOCATOR: linked_list_allocator::LockedHeap = linked_list_allocator::LockedHeap::empty();
          |
          |static mut HEAP_READY: bool = false;
          |
          |fn ensure_heap() {
          |  unsafe {
          |    if !HEAP_READY {
          |      #[cfg(not(test))]
          |      ALLOCATOR.lock().init(core::ptr::addr_of_mut!(HEAP) as *mut u8, HEAP_SIZE);
          |      HEAP_READY = true;
          |    }
          |  }
          |}
          |
          |pub const PROPTEST_SEED: [u8; 32] = *b"hamr-microkit-system-test-seed!!";
          |
          |/// Runs `cases` generated cases of `test` over the strategy `make_strategy` builds,
          |/// shrinking on failure.  Returns whether every case passed; a failure's message and
          |/// minimal input are printed as `TEST | INFO` lines, so the caller can simply write
          |/// `sys_assert!(run_property(..))`.
          |///
          |/// The strategy is built here, not by the caller, because building one can allocate
          |/// (`prop_flat_map` wraps its closure in an `Arc`) and the heap does not exist until
          |/// the first property test starts.
          |pub fn run_property<S: proptest::strategy::Strategy>(
          |    cases: u32,
          |    make_strategy: impl FnOnce() -> S,
          |    test: impl Fn(S::Value) -> proptest::test_runner::TestCaseResult) -> bool
          |  where S::Value: core::fmt::Debug {
          |  use proptest::test_runner::{Config, RngAlgorithm, TestError, TestRng, TestRunner};
          |  ensure_heap();
          |  let strategy = make_strategy();
          |  let rng = TestRng::from_seed(RngAlgorithm::ChaCha, &PROPTEST_SEED);
          |  let mut runner = TestRunner::new_with_rng(Config::with_cases(cases), rng);
          |  match runner.run(&strategy, test) {
          |    Ok(()) => {
          |      line(format_args!("TEST | INFO  {} generated cases passed", cases));
          |      true
          |    }
          |    Err(TestError::Fail(reason, minimal)) => {
          |      // The reason spans several lines (left/right of a failed prop_assert_eq!), and
          |      // the host driver keeps only lines carrying the TEST prefix.
          |      let reason = alloc::format!("{}", reason);
          |      for l in reason.lines() {
          |        line(format_args!("TEST | INFO  {}", l));
          |      }
          |      line(format_args!("TEST | INFO  minimal failing input: {:?}", minimal));
          |      false
          |    }
          |    Err(TestError::Abort(reason)) => {
          |      line(format_args!("TEST | INFO  aborted: {}", reason));
          |      false
          |    }
          |  }
          |}
          |""")
  }

  val systemTests_tests_rs: ST =
    st"""${CommentTemplate.safeToEditComment_slash}
        |
        |//! System tests.  This file is preserved across regeneration.
        |//!
        |//! Tests must be **order-independent** and establish their own preconditions: the
        |//! runner normalizes the schedule position between tests, but component state is
        |//! whatever the previous test left behind.  Set every input you depend on.
        |//!
        |//! Assertions are `sys_assert!` / `sys_assert_eq!`, not `assert!`: a panic would
        |//! take the protection domain down and end the run.
        |//!
        |//! Select a subset at image build time with `make CONFIG=$v.mk TESTS=<filter>`;
        |//! the filter is a substring match against the qualified `suite::test` name.
        |//!
        |//! `api::` steps the schedule; `inspect::` reads and writes this model's ports and
        |//! GUMBO state variables (see `inspect.rs` for the generated accessors), and
        |//! `inspect::channels::` names each thread for `api::run_to_thread(..)`.  The
        |//! inject / step / observe shape is:
        |//!
        |//! ```ignore
        |//! // Park immediately before the consumer.  Stepping past the producer's own slot
        |//! // first would overwrite the injected value -- the queue holds one element.
        |//! let _ = api::run_to_thread(inspect::channels::some_thread_MON);
        |//! inspect::put_some_thread_someInput(value);
        |//! let _ = api::sstep(1);
        |//! sys_assert_eq!(inspect::get_some_thread_someOutput(), Some(expected));
        |//! ```
        |//!
        |//! To split the tests across files, replace the `system_tests!` block below with
        |//!
        |//! ```ignore
        |//! system_test_files! {
        |//!   mod smoke_tests;      // system_tests/tests/smoke_tests.rs
        |//!   mod nominal_tests;    // system_tests/tests/nominal_tests.rs
        |//! }
        |//! ```
        |//!
        |//! where each file has its own `system_tests!` block and the same `use` lines as
        |//! this one.  Keep the `_tests.rs` suffix so the model's clean script preserves them.
        |//!
        |//! For random inputs, `harness::run_property` runs a proptest strategy against the
        |//! system on target and shrinks a failure to a minimal input:
        |//!
        |//! ```ignore
        |//! use proptest::prelude::Strategy;
        |//! sys_assert!(crate::system_tests::harness::run_property(64, || 0i32..100, |v| {
        |//!   // inject v, step, then check with proptest::prop_assert_eq!(..)
        |//!   Ok(())
        |//! }));
        |//! ```
        |
        |use crate::system_tests::api;
        |#[allow(unused_imports)]
        |use crate::system_tests::inspect;
        |use crate::{system_tests, sys_assert, sys_assert_eq};
        |
        |system_tests! {
        |  suite smoke {
        |    fn schedule_advances_by_hyperperiod() {
        |      let before = api::info_state();
        |      let after = api::hstep(1);
        |      sys_assert!(after.flags & api::FLAG_BAD_COMMAND == 0);
        |      sys_assert_eq!(after.hyperperiod_num, before.hyperperiod_num + 1);
        |    }
        |
        |    fn stepping_one_slot_at_a_time_wraps() {
        |      let n = api::schedule().num_timeslices;
        |      let before = api::info_state();
        |      let mut i = 0;
        |      while i < n {
        |        let _ = api::sstep(1);
        |        i += 1;
        |      }
        |      let after = api::info_state();
        |      sys_assert_eq!(after.current_timeslice, before.current_timeslice);
        |      sys_assert_eq!(after.hyperperiod_num, before.hyperperiod_num + 1);
        |    }
        |  }
        |}
        |"""


  /** Patches the TESTS= filter into the controller's .test_selection ELF section, following
    * the same objcopy route meta.py already uses for the user_schedule.  Emitted at the end
    * of generate(), because it has to name the controller's protection domain.
    *
    * Layout must match TestSelection in the controller's system_tests/selection.rs:
    * filter: [u8; 256] then flags: u32, so 260 bytes.  An all-zero filter means run
    * everything, which is what an unpatched image already contains.
    */
  @pure def testSelection_py(controllerPdName: String, hasGumbo: B, hasSysverif: B): ST = {
    @strictpure def py(b: B): String = if (b) "True" else "False"
    return (
      st"""#######################################
          |# TEST SELECTION
          |# Which system tests to run, from the TESTS make variable.  Substring match
          |# against the qualified suite::test name; empty selects all of them.
          |#
          |# And which contract checks are live for the whole run (TestScheduler-design.md,
          |# D23), from GUMBO_CHECKS and SYSVERIF_CHECKS: empty or "on" leaves a layer on,
          |# "off" turns it off -- not tracked, and no scheduler park taken for it.  They
          |# go into the flags word after the filter: bit 0 GUMBO off, bit 1 SYSVERIF off.
          |# LIST_TESTS=1 sets bit 2: list the selected tests without running them (D17).
          |#######################################
          |test_selection = bytearray(260)
          |try:
          |    _filter = tests_filter.encode()
          |except UnicodeEncodeError:
          |    raise SystemExit("TESTS must be valid text (UTF-8)")
          |# truncating would change which tests run, or split a character, silently
          |if len(_filter) > 255:
          |    raise SystemExit(f"TESTS must be at most 255 bytes (UTF-8), not {len(_filter)}")
          |test_selection[0:len(_filter)] = _filter
          |import os
          |_flags = 0
          |for _name, _bit, _generated in [("GUMBO_CHECKS", 0x1, ${py(hasGumbo)}),
          |                                ("SYSVERIF_CHECKS", 0x2, ${py(hasSysverif)})]:
          |    _value = os.environ.get(_name, "")
          |    if _value not in ("", "on", "off"):
          |        raise SystemExit(f"{_name} must be 'on' or 'off', not '{_value}'")
          |    if _value != "" and not _generated:
          |        print(f"warning: {_name}={_value} has no effect: this model has no such checks")
          |    if _value == "off":
          |        _flags |= _bit
          |# LIST_TESTS=1: print the test table and run nothing (bit 2).
          |_list = os.environ.get("LIST_TESTS", "")
          |if _list not in ("", "0", "1"):
          |    raise SystemExit(f"LIST_TESTS must be '0' or '1', not '{_list}'")
          |if _list == "1":
          |    _flags |= 0x4
          |test_selection[256:260] = _flags.to_bytes(4, "little")
          |test_selection_path = output_dir + "/test_selection.data"
          |with open(test_selection_path, "wb+") as f:
          |    f.write(bytes(test_selection))
          |update_elf_section(obj_copy, ${controllerPdName}.program_image,
          |                   "test_selection",
          |                   test_selection_path)""")
  }


  /** Host-side driver for the on-target system tests (TestScheduler-design.md D18).
    *
    * Serial is the only channel that carries a verdict off the target -- test_status lives in
    * guest memory that the host cannot read -- and every interesting failure of a bare-metal
    * QEMU run is silence: a hang, a panic, a controller that never started, a filter that
    * matched nothing.  So the rule is inverted: a run without the DONE line fails, whatever
    * preceded it.
    */
  @pure def runTests_cmd(qemuTimeoutSeconds: Z): ST = {
    val tq: String = "\"\"\""
    return (
      st"""::/*#! 2> /dev/null                                 #
          |@ 2>/dev/null # 2>nul & echo off & goto BOF         #
          |if [ -z $${SIREUM_HOME} ]; then                      #
          |  echo "Please set SIREUM_HOME env var"             #
          |  exit -1                                           #
          |fi                                                  #
          |exec $${SIREUM_HOME}/bin/sireum slang run "$$0" "$$@"  #
          |:BOF
          |setlocal
          |if not defined SIREUM_HOME (
          |  echo Please set SIREUM_HOME env var
          |  exit /B -1
          |)
          |%SIREUM_HOME%\bin\sireum.bat slang run "%0" %*
          |exit /B %errorlevel%
          |::!#*/
          |// #Sireum
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |import org.sireum._
          |
          |// Runs the system tests under QEMU and turns the serial output into an exit code.
          |//
          |// Usage:  run-tests.cmd [--list] [<test filter>]
          |//
          |// --list builds an image that prints the test table and runs nothing (LIST_TESTS=1).
          |//
          |// The filter is a substring match against the qualified suite::test name, so
          |// "nominal::" selects a suite and "fan_turns" selects a single test.
          |
          |val microkitDir: Os.Path = Os.slashDir.up
          |val listOnly: B = Os.cliArgs.nonEmpty && Os.cliArgs(0) == "--list"
          |val filterArgs: ISZ[String] = if (listOnly) ops.ISZOps(Os.cliArgs).drop(1) else Os.cliArgs
          |val filter: String = if (filterArgs.nonEmpty) filterArgs(0) else ""
          |val listArg: String = if (listOnly) "LIST_TESTS=1" else "LIST_TESTS=0"
          |
          |// make expands a `$$` in a variable's value, in the rebuild hash and in what it hands the
          |// build, and the hash is echoed, which reads `\`: a filter holding either would select
          |// something other than what was typed.  No test name can contain them.
          |if (ops.StringOps(filter).contains("$$") || ops.StringOps(filter).contains("\\")) {
          |  eprintln(s"FAILED: a test filter cannot contain '$$$$' or '\\' (got '$$filter')")
          |  Os.exit(1)
          |}
          |
          |// sddf_dprintf is compiled out unless CONFIG_DEBUG_BUILD is set, which would remove
          |// every TEST line and make a perfectly good run look like a failure.  Force it.
          |val commonArgs: ISZ[String] = ISZ(
          |  "make", "-C", microkitDir.string,
          |  "CONFIG=$v.mk",
          |  "MICROKIT_CONFIG=debug",
          |  "RUST_MAKE_TARGET=build-release",
          |  s"TESTS=$$filter",
          |  listArg)
          |
          |println(s"Building $$microkitDir (TESTS='$$filter', $$listArg) ...")
          |val build = Os.proc(commonArgs).console.run()
          |if (!build.ok) {
          |  eprintln("Build failed")
          |  Os.exit(1)
          |}
          |
          |println("Running under QEMU ...")
          |
          |// QEMU never exits on its own: once the suite is done the scheduler idles and the
          |// guest goes quiet.  Waiting for the timeout would make every successful run cost
          |// the full bound, so watch the log and stop QEMU as soon as DONE appears.  Piping
          |// into `sed /DONE/q` does not work here -- with the guest idle there is no further
          |// write to raise SIGPIPE -- so the process group has to be killed explicitly.  The
          |// console writes a line a character at a time, so DONE can be seen before the rest of
          |// its line is out: QEMU is given another second before it is stopped.
          |val logFile: Os.Path = Os.temp()
          |
          |val driver: String =
          |  st${tq}set -u
          |      |"$$$$@" > "$$$$LOG" 2>&1 &
          |      |mpid=$$$$!
          |      |waited=0
          |      |while kill -0 $$$$mpid 2>/dev/null; do
          |      |  grep -q 'TEST | DONE' "$$$$LOG" && { sleep 1; break; }
          |      |  [ $$$$waited -ge $$$$TIMEOUT ] && break
          |      |  sleep 1
          |      |  waited=$$$$((waited + 1))
          |      |done
          |      |pkill -P $$$$mpid 2>/dev/null
          |      |kill $$$$mpid 2>/dev/null
          |      |wait $$$$mpid 2>/dev/null
          |      |exit 0$tq.render
          |
          |Os.proc(ISZ[String]("sh", "-c", driver, "sh") ++ (commonArgs :+ "qemu"))
          |  .env(ISZ(("LOG", logFile.string), ("TIMEOUT", "$qemuTimeoutSeconds")))
          |  .timeout(($qemuTimeoutSeconds + 30) * 1000).run()
          |
          |val out: String = if (logFile.exists) logFile.read else ""
          |logFile.removeAll()
          |
          |var matched: Z = -1
          |var passed: Z = -1
          |var failed: Z = -1
          |// an initialization guarantee is taken as met only on DONE's own word (init=ok): a
          |// DONE line cut short must not pass a run whose initialization failed
          |var initOk: B = F
          |var listed: B = F
          |var sawDone: B = F
          |var seenPass: Z = 0
          |var seenFail: Z = 0
          |
          |// The QEMU serial console emits CRLF, so every line arrives with a trailing \r.
          |// Left in place it makes Z("0\r") a None and the parse below blows up.
          |val normalized: String = ops.StringOps(out).replaceAllLiterally("\r", "")
          |
          |for (line <- ops.StringOps(normalized).split((c: C) => c == '\n')) {
          |  val l = ops.StringOps(line)
          |  if (l.startsWith("TEST | ")) {
          |    println(line)
          |    if (l.startsWith("TEST | PASS")) {
          |      seenPass = seenPass + 1
          |    } else if (l.startsWith("TEST | FAIL")) {
          |      seenFail = seenFail + 1
          |    } else if (l.startsWith("TEST | DONE")) {
          |      sawDone = T
          |      for (tok <- ops.StringOps(line).split((c: C) => c == ' ')) {
          |        val t = ops.StringOps(tok)
          |        if (t.startsWith("matched=")) {
          |          matched = Z(t.substring(8, tok.size)).getOrElse(-1)
          |        } else if (t.startsWith("passed=")) {
          |          passed = Z(t.substring(7, tok.size)).getOrElse(-1)
          |        } else if (t.startsWith("failed=")) {
          |          failed = Z(t.substring(7, tok.size)).getOrElse(-1)
          |        } else if (t.startsWith("init=")) {
          |          initOk = t.substring(5, tok.size) == "ok"
          |        } else if (t.startsWith("list=")) {
          |          listed = t.substring(5, tok.size) == "1"
          |        }
          |      }
          |    }
          |  }
          |}
          |
          |// Absence of evidence is failure.  A hang, a panicking protection domain, or a
          |// controller that never ran all produce a well-formed-looking log with no DONE.
          |if (!sawDone) {
          |  eprintln("FAILED: the run produced no 'TEST | DONE' line (hang, panic, or the test controller never ran)")
          |  eprintln("---- captured output ----")
          |  eprintln(out)
          |  Os.exit(1)
          |}
          |
          |// A DONE line cut short before its counts: output was lost, whatever the filter was.
          |if (matched < 0 || passed < 0 || failed < 0) {
          |  eprintln("FAILED: the 'TEST | DONE' line is incomplete (matched=, passed= or failed= missing); output was lost")
          |  Os.exit(1)
          |}
          |
          |// A filter that selects nothing must not pass: that is how a typo turns a job green.
          |if (matched == 0) {
          |  eprintln(s"FAILED: the filter '$$filter' matched no tests")
          |  Os.exit(1)
          |}
          |
          |// A listing runs nothing; the table above is its output.
          |if (listed) {
          |  println(s"OK: listed $$matched test(s) matching '$$filter'; none were run")
          |  Os.exit(0)
          |}
          |
          |// The counts and the per-test lines have to agree, or output was lost -- and every
          |// matched test was run, or DONE was cut short (a listing's list=1 among what was lost).
          |if (passed != seenPass || failed != seenFail || passed + failed != matched) {
          |  eprintln(s"FAILED: DONE reports matched=$$matched passed=$$passed failed=$$failed but $$seenPass PASS and $$seenFail FAIL lines were seen; output was lost")
          |  Os.exit(1)
          |}
          |
          |if (failed != 0) {
          |  eprintln(s"FAILED: $$failed of $$matched tests failed")
          |  Os.exit(1)
          |}
          |
          |// An initialization guarantee was violated before any test ran (see the VIOLATION
          |// lines above).  No test is charged with it, so it has to fail the run here.
          |if (!initOk) {
          |  eprintln("FAILED: an initialization guarantee was violated, or DONE's init= was lost (DONE init=failed or missing)")
          |  Os.exit(1)
          |}
          |
          |println(s"OK: $$passed of $$matched tests passed")
          |Os.exit(0)
          |""")
  }


  /** The typed inspect/inject API over the component ports and GUMBO state vars.
    *
    * Reads are non-destructive: each accessor dequeues through the controller's own receiver
    * cursor, so observing a port does not consume data the real consumer has not seen.
    *
    * Writes enqueue exactly as the producing thread would.  They are safe only while the
    * schedule is paused (D4) -- the queue is single-sender, and the controller is a second
    * one.  Where a port has a real producer, injecting and then stepping past that producer's
    * own slot will lose the injected value: the queue holds a single element.  Use
    * `run_to_thread(..)` to park immediately before the consumer, then inject, then `sstep(1)`.
    */
  @pure def systemTests_inspect_rs(regions: ISZ[ObservableRegion],
                                  injectables: ISZ[InjectableStateVar],
                                  channels: ISZ[(String, Z)],
                                  preStates: ISZ[ThreadPreState],
                                  hasObserve: B): ST = {
    var externs: ISZ[ST] = ISZ()
    var wrappers: ISZ[ST] = ISZ()
    for (r <- regions) {
      externs = externs :+ st"fn test_get_${r.accessor}(value: *mut ${r.rustTypeName}) -> bool;"
      if (!r.isStateVar) {
        externs = externs :+ st"fn test_put_${r.accessor}(value: *mut ${r.rustTypeName});"
      }
      val kind: String = if (r.isStateVar) "GUMBO state variable" else "port"
      wrappers = wrappers :+
        st"""/// Current value of the $kind `${r.accessor}`, or None when nothing is queued.
            |pub fn get_${r.accessor}() -> Option<${r.rustTypeName}> {
            |  unsafe {
            |    let mut v: ${r.rustTypeName} = ${r.rustTypeName}::default();
            |    if test_get_${r.accessor}(&mut v) { Some(v) } else { None }
            |  }
            |}"""
      // A state var's sv_ region is the thread's *output*: writing it would be seen by
      // observers but never by the thread. Setting one goes through its injection region
      // instead, emitted below, so no writer is offered here.
      if (!r.isStateVar) {
        val body: ISZ[ST] =
          if (hasObserve && r.injectionHidesFromProducer)
            ISZ(
              st"let mut v = value;",
              st"unsafe { test_put_${r.accessor}(&mut v); }",
              st"crate::system_tests::observe::injected_port_${r.accessor}(&v);")
          else
            ISZ(
              st"""unsafe {
                  |  let mut v = value;
                  |  test_put_${r.accessor}(&mut v);
                  |}""")
        wrappers = wrappers :+
          st"""/// Publish `value` to the port `${r.accessor}`, as its producer would.
              |pub fn put_${r.accessor}(value: ${r.rustTypeName}) {
              |  ${(body, "\n")}
              |}"""
      }
    }

    for (v <- injectables) {
      externs = externs :+
        st"fn test_put_${v.threadId}_sv_${v.varName}(value: *mut ${v.rustTypeName});"
      // The contract checks must see the injected value as the thread's pre-state, though
      // its sv_ region still holds the old one until the thread dispatches.
      val putBody: ISZ[ST] =
        (if (hasObserve) ISZ(st"crate::system_tests::observe::injected_${v.threadId}_sv_${v.varName}(value.clone());")
         else ISZ[ST]()) :+
          st"""unsafe {
              |  let mut v = value;
              |  test_put_${v.threadId}_sv_${v.varName}(&mut v);
              |}"""
      wrappers = wrappers :+
        st"""/// Set the GUMBO state variable `${v.varName}` on `${v.threadId}`.  The thread adopts
            |/// it at the start of its next dispatch, before computing; if nothing is set it keeps
            |/// the value it already had.
            |pub fn put_${v.threadId}_sv_${v.varName}(value: ${v.rustTypeName}) {
            |  ${(putBody, "\n")}
            |}"""
    }
    // D14: one container per thread, with no Default, so rustc refuses a test that leaves a
    // field out. Adding a state variable to the model then breaks stale tests at compile time
    // rather than silently defaulting them and producing a green run against a state nobody
    // meant to set up.
    var containers: ISZ[ST] = ISZ()
    for (ps <- preStates) {
      val decls: ISZ[ST] =
        for (f <- ps.fields) yield st"pub ${f.fieldName}: ${f.rustTypeName},"
      val sets: ISZ[ST] =
        for (f <- ps.fields) yield st"${f.setter}(s.${f.fieldName});"
      containers = containers :+
        st"""/// Complete pre-state for `${ps.threadId}`: every input port and GUMBO state
            |/// variable it has.  There is deliberately no `Default` -- name every field.
            |pub struct ${ps.threadId}_PreState {
            |  ${(decls, "\n")}
            |}
            |
            |/// Establish the whole pre-state of `${ps.threadId}` in one call.  The thread picks
            |/// it up on its next dispatch, so the usual ordering applies: park immediately
            |/// before it with `api::run_to_thread`, set, then `api::sstep(1)`.
            |pub fn set_${ps.threadId}(s: ${ps.threadId}_PreState) {
            |  ${(sets, "\n")}
            |}"""
    }

    val chans: ISZ[ST] = for (c <- channels) yield st"pub const ${c._1}: u32 = ${c._2};"

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Inspection and injection over the component ports and GUMBO state variables.
          |
          |use data::*;
          |
          |/// Scheduler channel per thread, for `api::run_to_thread(..)`.  Parking immediately
          |/// before a thread is what makes injection into a port with a real producer
          |/// reliable: step past that producer's own slot and it overwrites the value.
          |pub mod channels {
          |  ${(chans, "\n")}
          |}
          |
          |extern "C" {
          |  ${(externs, "\n")}
          |}
          |
          |${(wrappers, "\n\n")}
          |
          |${(containers, "\n\n")}
          |""")
  }

  /** The controller's contract checking (TestScheduler-design.md, stage 7): hosts the layers
    * of crates/observers over a SystemView that reads through the controller's own obs_get_*
    * cursors (D25), and a ViolationSink that turns a violation into a test verdict (D21).
    *
    * @param channelThreads the thread ids that have a scheduler channel (`<id>_MON`)
    */
  @pure def systemTests_observe_rs(info: ContractObserverInfo,
                                   regions: ISZ[ObservableRegion],
                                   injectables: ISZ[InjectableStateVar],
                                   channelThreads: ISZ[String],
                                   reporter: Reporter): ST = {
    val crate = ContractObserverPlugin.crateName
    var byAccessor = Map.empty[String, ObservableRegion]
    for (r <- regions) {
      byAccessor = byAccessor + r.accessor ~> r
    }
    val isThread: Set[String] = Set.empty[String] ++ info.threads

    var externs: ISZ[ST] = ISZ()
    var fields: ISZ[ST] = ISZ()
    var fieldInits: ISZ[ST] = ISZ()
    var getters: ISZ[ST] = ISZ()
    var seenResets: ISZ[ST] = ISZ()
    var externed: Set[String] = Set.empty

    def extern(r: ObservableRegion, cursor: String): Unit = {
      if (!externed.contains(cursor)) {
        externed = externed + cursor
        externs = externs :+ st"fn test_obs_get_$cursor(value: *mut ${r.rustTypeName}) -> bool;"
      }
    }

    // A state variable the controller can inject is read from its pending value until the
    // thread has dispatched and adopted it -- by that thread's own checks only.  Its pending
    // value is the pre-state the thread will start from; everyone else (the system
    // assertions) sees the thread's actual state, which is the sv_ region until then.
    var pendingOwners: Map[String, String] = Map.empty
    var injectedFns: ISZ[ST] = ISZ()
    var clearArms: Map[String, ISZ[ST]] = Map.empty
    for (v <- injectables) {
      val acc = s"${v.threadId}_sv_${v.varName}"
      pendingOwners = pendingOwners + acc ~> v.threadId
      fields = fields :+ st"pending_$acc: Option<${v.rustTypeName}>,"
      fieldInits = fieldInits :+ st"pending_$acc: None,"
      injectedFns = injectedFns :+
        st"""/// Called by `inspect::put_$acc`: the value `${v.threadId}` will start its next dispatch from.
            |pub(crate) fn injected_$acc(value: ${v.rustTypeName}) {
            |  unsafe { VIEW.pending_$acc = Some(value); }
            |}"""
      if (isThread.contains(v.threadId)) {
        clearArms = clearArms + v.threadId ~> (clearArms.getOrElse(v.threadId, ISZ()) :+ st"self.pending_$acc = None;")
      }
    }

    // A test's put_ into a producer's event region enqueues where the producer's own cursor
    // reads its output.  Left there, the producer's next completion check would take the
    // injected value for something it sent.  Consume it from the producer's view as it goes in.
    // The consumers do receive it, so for the system assertions it is what the producer's
    // port carries in this frame (frame_*, below) until the producer sends again.
    for (r <- regions if r.injectionHidesFromProducer) {
      val producerCursor = r.obsCursor(r.readers(0))
      extern(r, producerCursor)
      extern(r, r.obsCursor("sys"))
      val setFrame: ISZ[ST] =
        if (isThread.contains(r.readers(0)))
          ISZ(st"unsafe { VIEW.frame_${r.accessor} = Stamped { at: tick(), value: Some(value.clone()), injected: true }; }")
        else ISZ()
      // An injection is in the frame it is made in.  A consumer that already ran in this
      // hyperperiod dequeues it in the next, where the producer's frame value no longer shows
      // it -- so an assertion relating the two sees something received that was not sent.
      // Say so where it happens (TestScheduler-design.md, "Frames and event values"): only for
      // a consumer that ran in this hyperperiod and has no slot left in it (one with a later
      // slot, or the one that just overran and is re-dispatched, receives it now), and only
      // while the system assertions are live and switched on (SYSVERIF_LIVE, SYSVERIF_ON) --
      // the mismatch is theirs to see.
      val lateConsumers: ISZ[ST] =
        for (c <- ops.ISZOps(r.readers).drop(1) if isThread.contains(c) && ops.ISZOps(channelThreads).contains(c)) yield
          st"""if { let last = DISPATCHED_HP[thread_index(Thread::$c)]; last } == Some(hp) && !slot_left_for(inspect::channels::${c}_MON) {
              |  harness::line(format_args!("TEST | INFO  {} injected after {} ran in hp={}: {} receives it in the next hyperperiod, where {} did not send it", "${r.accessor}", "$c", hp, "$c", "${r.readers(0)}"));
              |}"""
      val lateNote: ISZ[ST] =
        if (lateConsumers.isEmpty || info.compositionIds.isEmpty) ISZ()
        else ISZ(
          st"""unsafe {
              |  if let (Some(hp), true, true) = (FRAME_HP, SYSVERIF_LIVE && SYSVERIF_ON, dispatch_left_in_hp()) {
              |    ${(lateConsumers, "\n")}
              |  }
              |}""")
      injectedFns = injectedFns :+
        st"""/// Called by `inspect::put_${r.accessor}`: the injected element is the test's, not
            |/// `${r.readers(0)}`'s output, so its own check must not see it; its consumers and
            |/// the system assertions do.
            |pub(crate) fn injected_port_${r.accessor}(value: &${r.rustTypeName}) {
            |  let mut v: ${r.rustTypeName} = Default::default();
            |  while unsafe { test_obs_get_$producerCursor(&mut v) } {}
            |  while unsafe { test_obs_get_${r.obsCursor("sys")}(&mut v) } {}
            |  ${(setFrame, "\n")}
            |  ${(lateNote, "\n")}
            |}"""
    }

    // The system assertions read a producer's event port as what that producer's latest
    // dispatch sent -- nothing, if it sent nothing -- or what a test injected into it that no
    // send has replaced; the consumers receive the injection.  Taken from its `sys` cursor
    // as the producer completes, and stamped with the logical time (Stamped), so each
    // composition sees only what arrived in its own current frame (focus_system): what an
    // assertion sees does not depend on which assertions read the port before it, on a
    // suspension, or on another composition ending its frame.  An unconnected input's region
    // has no producer: its readers(0) is the reading thread, and the value is what that thread
    // received, taken at its completion the same way.
    var frameRegions: Set[String] = Set.empty
    var producerArms: Map[String, ISZ[ST]] = Map.empty
    for (r <- regions if r.isEvent && isThread.contains(r.readers(0))) {
      val sysCursor = r.obsCursor("sys")
      extern(r, sysCursor)
      frameRegions = frameRegions + r.accessor
      fields = fields :+ st"frame_${r.accessor}: Stamped<${r.rustTypeName}>,"
      fieldInits = fieldInits :+ st"frame_${r.accessor}: Stamped::empty(),"
      val producer = r.readers(0)
      producerArms = producerArms + producer ~> (producerArms.getOrElse(producer, ISZ()) :+
        st"""{
            |  let mut v: ${r.rustTypeName} = Default::default();
            |  // what it sent last, if it sent more than once (a queue deeper than 1)
            |  let mut got = false;
            |  while unsafe { test_obs_get_$sysCursor(&mut v) } {
            |    got = true;
            |  }
            |  if got {
            |    self.frame_${r.accessor} = Stamped { at: tick(), value: Some(v), injected: false };
            |  } else if !self.frame_${r.accessor}.injected {
            |    // sent nothing: the port carries nothing now -- unless a test injected into it,
            |    // which its consumers still receive (an injection from an earlier frame is
            |    // outside the current one by its time anyway)
            |    self.frame_${r.accessor} = Stamped { at: tick(), value: None, injected: false };
            |  }
            |}""")
    }
    // An overran dispatch ran unobserved: it dequeued its inputs and sent its outputs, but no
    // completion check moved the controller's cursors past them.  Left there, the thread's
    // next check would see those events again.  Drain every cursor read on the thread's
    // behalf: its inputs, its own outputs, and the system layer's view of its outputs.
    var drainArms: Map[String, ISZ[ST]] = Map.empty
    for (r <- regions if r.isEvent; reader <- r.readers if isThread.contains(reader)) {
      var cursors: ISZ[String] = ISZ(r.obsCursor(reader))
      if (reader == r.readers(0)) {
        cursors = cursors :+ r.obsCursor("sys")
      }
      for (c <- cursors) {
        extern(r, c)
        drainArms = drainArms + reader ~> (drainArms.getOrElse(reader, ISZ()) :+
          st"""{
              |  let mut v: ${r.rustTypeName} = Default::default();
              |  while unsafe { test_obs_get_$c(&mut v) } {}
              |}""")
      }
    }
    val drainCursorArms: ISZ[ST] =
      for (e <- drainArms.entries) yield
        st"""Thread::${e._1} => {
            |  ${(e._2, "\n")}
            |}"""

    // What producers sent while initializing is the first frame's value, as it is for the
    // monitors, until they send again -- taken before the START checks, which may read it.
    val initFrameCalls: ISZ[ST] =
      for (p <- producerArms.keys) yield st"(&mut *addr_of_mut!(VIEW)).producer_completed(Thread::$p);"
    // What a thread sent while initializing is not its first dispatch's output: once the
    // initialization checks have read what they read, the rest is skipped on its own cursor.
    var initDrains: ISZ[ST] = ISZ()
    for (r <- regions if r.isEvent && r.isProducerOutput && isThread.contains(r.readers(0))) {
      val c = r.obsCursor(r.readers(0))
      extern(r, c)
      initDrains = initDrains :+
        st"""{
            |  let mut v: ${r.rustTypeName} = Default::default();
            |  while test_obs_get_$c(&mut v) {}
            |}"""
    }
    val producerCompletedArms: ISZ[ST] =
      for (e <- producerArms.entries) yield
        st"""Thread::${e._1} => {
            |  ${(e._2, "\n")}
            |}"""

    // Aliases of connected inputs (ContractObserverPlugin.ReceivedGetter): what the reader
    // received, latched at its dispatch through its own cursor.  That one dequeue is also what
    // the reader's own checks read for the rest of the dispatch, so nothing is dequeued twice.
    var aliasedPairs: Set[String] = Set.empty // "<acc>__<reader>"
    var latchArms: Map[String, ISZ[ST]] = Map.empty
    for (rg <- info.receivedGetters) {
      val ty: String = info.getters.filter((g: (String, String)) => g._1 == rg.name)(0)._2
      val acc = ops.StringOps(rg.underlying).substring(4, rg.underlying.size)
      byAccessor.get(acc) match {
        case Some(r) if isThread.contains(rg.reader) =>
          val cell = s"recv_${acc}__${rg.reader}"
          if (!aliasedPairs.contains(s"${acc}__${rg.reader}")) {
            aliasedPairs = aliasedPairs + s"${acc}__${rg.reader}"
            if (r.isEvent) {
              fields = fields :+ st"$cell: Stamped<${r.rustTypeName}>,"
              fieldInits = fieldInits :+ st"$cell: Stamped::empty(),"
              val c = r.obsCursor(rg.reader)
              extern(r, c)
              latchArms = latchArms + rg.reader ~> (latchArms.getOrElse(rg.reader, ISZ()) :+
                st"""{
                    |  let mut v: ${r.rustTypeName} = Default::default();
                    |  let got = unsafe { test_obs_get_$c(&mut v) };
                    |  self.$cell = Stamped { at: tick(), value: if got { Some(v) } else { None }, injected: false };
                    |}""")
            } else {
              fields = fields :+ st"$cell: Option<${r.rustTypeName}>,"
              fieldInits = fieldInits :+ st"$cell: None,"
              latchArms = latchArms + rg.reader ~> (latchArms.getOrElse(rg.reader, ISZ()) :+
                st"self.$cell = Some(self.${rg.underlying}());")
            }
          }
          val result: ST =
            if (!r.isEvent)
              st"""match &self.$cell {
                  |  Some(v) => v.clone(),
                  |  None => Default::default(),
                  |}"""
            else st"self.$cell.visible(self.sys_start)"
          getters = getters :+
            st"""fn ${rg.name}(&mut self) -> $ty {
                |  $result
                |}"""
        case _ =>
          reporter.warn(None(), toolName,
            s"The test controller has no region to read '${rg.name}' from; the system assertions that read it are skipped")
          getters = getters :+
            st"""fn ${rg.name}(&mut self) -> ${info.getters.filter((g: (String, String)) => g._1 == rg.name)(0)._2} {
                |  self.missing = true;
                |  Default::default()
                |}"""
      }
    }
    val latchReceivedArms: ISZ[ST] =
      for (e <- latchArms.entries) yield
        st"""Thread::${e._1} => {
            |  ${(e._2, "\n")}
            |}"""

    val receivedNames: ISZ[String] = info.receivedNames
    for (g <- info.getters if !ops.ISZOps(receivedNames).contains(g._1)) {
      val name = g._1
      val ty = g._2
      val acc = ops.StringOps(name).substring(4, name.size)
      val isOption: B = ops.StringOps(ty).startsWith("Option<")
      byAccessor.get(acc) match {
        case Some(r) if !r.isEvent =>
          // data port or state variable: last-value cache over one cursor
          val c = r.obsCursor("")
          extern(r, c)
          fields = fields :+ st"last_$acc: Option<${r.rustTypeName}>,"
          fieldInits = fieldInits :+ st"last_$acc: None,"
          val pending: ISZ[ST] = pendingOwners.get(acc) match {
            case Some(owner) if isThread.contains(owner) =>
              ISZ(st"""if self.focus == Some(Thread::$owner) {
                      |  if let Some(v) = &self.pending_$acc {
                      |    return ${if (isOption) "Some(v.clone())" else "v.clone()"};
                      |  }
                      |}""")
            case _ => ISZ()
          }
          // Never written: a data port reads as its default, as the thread's own getter
          // does (it returns a zero-initialized last value), so the check sees what the
          // thread saw.  A state variable is published at initialization, so a sv_ region
          // never written means there is nothing to check against.
          val result: ST =
            if (isOption) st"self.last_$acc.clone()"
            else if (r.isStateVar)
              st"""match &self.last_$acc {
                  |  Some(v) => v.clone(),
                  |  None => {
                  |    self.missing = true;
                  |    Default::default()
                  |  }
                  |}"""
            else
              st"""match &self.last_$acc {
                  |  Some(v) => v.clone(),
                  |  None => Default::default(),
                  |}"""
          val read: ST =
            st"""let mut v: ${r.rustTypeName} = Default::default();
                |if unsafe { test_obs_get_$c(&mut v) } {
                |  self.last_$acc = Some(v);
                |}"""
          val body: ISZ[ST] = pending :+ read :+ result
          getters = getters :+
            st"""fn $name(&mut self) -> $ty {
                |  ${(body, "\n")}
                |}"""
        case Some(r) =>
          // event or event-data port: present only if it arrived since this reader's
          // previous check, as the reading thread's own dequeue sees it
          var arms: ISZ[ST] = ISZ()
          for (reader <- r.readers if isThread.contains(reader)) {
            extern(r, r.obsCursor(reader))
            arms = arms :+ (
              if (aliasedPairs.contains(s"${r.accessor}__$reader"))
                // what the reader received at this dispatch, latched by latch_received
                st"""Some(Thread::$reader) => match &self.recv_${r.accessor}__$reader.value {
                    |  Some(x) => {
                    |    v = x.clone();
                    |    true
                    |  }
                    |  None => false,
                    |},"""
              else if (r.isProducerOutput && reader == r.readers(0))
                // the producer's own output: what it put last, as its post-state records
                st"""Some(Thread::$reader) => {
                    |  let mut got = false;
                    |  while test_obs_get_${r.obsCursor(reader)}(&mut v) {
                    |    got = true;
                    |  }
                    |  got
                    |}"""
              else st"Some(Thread::$reader) => test_obs_get_${r.obsCursor(reader)}(&mut v),")
          }
          extern(r, r.obsCursor("sys"))
          val sysArm: ST =
            if (frameRegions.contains(r.accessor))
              st"""_ => match self.frame_${r.accessor}.visible(self.sys_start) {
                  |  Some(x) => {
                  |    v = x;
                  |    true
                  |  }
                  |  None => false,
                  |},"""
            else st"_ => test_obs_get_${r.obsCursor("sys")}(&mut v),"
          // an event getter is Option<T> (a pure event port's T is its empty payload)
          val result: ST =
            if (ty == s"Option<${r.rustTypeName}>") st"if got { Some(v) } else { None }"
            else if (isOption) st"if got { Some(Default::default()) } else { None }"
            else
              st"""if !got {
                  |  self.missing = true;
                  |}
                  |Default::default()"""
          // A check may read a port more than once (`x.is_some() && x.unwrap()..`); only
          // the first read of a focus dequeues.
          fields = fields :+ st"seen_$acc: Option<(bool, ${r.rustTypeName})>,"
          fieldInits = fieldInits :+ st"seen_$acc: None,"
          seenResets = seenResets :+ st"self.seen_$acc = None;"
          getters = getters :+
            st"""fn $name(&mut self) -> $ty {
                |  let (got, v) = match &self.seen_$acc {
                |    Some((got, v)) => (*got, v.clone()),
                |    None => {
                |      let mut v: ${r.rustTypeName} = Default::default();
                |      let got = unsafe {
                |        match self.focus {
                |          ${(arms :+ sysArm, "\n")}
                |        }
                |      };
                |      self.seen_$acc = Some((got, v.clone()));
                |      (got, v)
                |    }
                |  };
                |  $result
                |}"""
        case _ =>
          reporter.warn(None(), toolName,
            s"The test controller has no region to read '$acc' from; the contract checks that read it are skipped")
          // as the warning says: the checks that read it are skipped (missing), whatever its
          // type -- a made-up false or None could pass or fail them
          val result: ST =
            st"""self.missing = true;
                |Default::default()"""
          getters = getters :+
            st"""fn $name(&mut self) -> $ty {
                |  $result
                |}"""
      }
    }

    val threadOfArms: ISZ[ST] =
      for (t <- info.threads if ops.ISZOps(channelThreads).contains(t))
        yield st"inspect::channels::${t}_MON => Some(Thread::$t),"
    val threadNamedArms: ISZ[ST] = for (t <- info.threads) yield st""""$t" => Some(Thread::$t),"""
    var threadIndexArms: ISZ[ST] = ISZ()
    for (i <- 0 until info.threads.size) {
      threadIndexArms = threadIndexArms :+ st"Thread::${info.threads(i)} => $i,"
    }
    val clears: ISZ[ST] =
      for (e <- clearArms.entries) yield
        st"""Thread::${e._1} => {
            |  ${(e._2, "\n")}
            |}"""

    // the layers this model has
    var layerStatics: ISZ[ST] = ISZ()
    var initCalls: ISZ[ST] = ISZ()
    var completeCalls: ISZ[ST] = ISZ()
    var dispatchCalls: ISZ[ST] = ISZ()
    var forgetCalls: ISZ[ST] = ISZ()
    var resumeCalls: ISZ[ST] = ISZ()
    if (info.hasComponentLayer) {
      forgetCalls = forgetCalls :+ st"(&mut *addr_of_mut!(COMPONENTS)).forget();"
      layerStatics = layerStatics :+
        st"static mut COMPONENTS: $crate::components::ComponentContracts = $crate::components::ComponentContracts::new();"
      initCalls = initCalls :+
        st"""if GUMBO_LIVE {
            |  REC.layer = Layer::Gumbo;
            |  let c = &mut *addr_of_mut!(COMPONENTS);
            |  // GUMBO is assume-guarantee: a dispatch whose assumption failed owes no guarantee (D24)
            |  c.excuse_post_on_failed_pre = true;
            |  c.on_init(&mut *addr_of_mut!(VIEW), &mut TestSink);
            |}"""
      completeCalls = completeCalls :+
        st"""if GUMBO_LIVE {
            |  REC.layer = Layer::Gumbo;
            |  (&mut *addr_of_mut!(COMPONENTS)).on_complete(t, &mut *addr_of_mut!(VIEW), &mut TestSink);
            |}"""
      dispatchCalls = dispatchCalls :+
        st"""if GUMBO_LIVE {
            |  REC.layer = Layer::Gumbo;
            |  (&mut *addr_of_mut!(COMPONENTS)).on_dispatch(t, &mut *addr_of_mut!(VIEW), &mut TestSink);
            |}"""
    }
    var sysMods: ISZ[ST] = ISZ()
    // Each composition completes on its own: its frame values are its own (focus_system,
    // frame_ended), so one composition ending its frame does not touch another's.
    for (i <- info.compositionIds.indices) {
      val id = info.compositionIds(i)
      val modName = ContractObserverPlugin.sysAssertModuleName(id)
      val typeName = ContractObserverPlugin.sysAssertTypeName(id)
      layerStatics = layerStatics :+
        st"static mut SYS_$id: $crate::$modName::$typeName = $crate::$modName::$typeName::new();"
      initCalls = initCalls :+
        st"""if SYSVERIF_LIVE {
            |  REC.layer = Layer::Sysverif;
            |  REC.composition = "$id";
            |  // the published schedule; its is_user_partition bits are the production schedule's
            |  let t = api::schedule();
            |  $crate::$modName::$typeName::validate_schedule(t.num_timeslices as usize, &t.timeslice_ch,
            |    &t.is_user_partition, thread_of, &mut TestSink);
            |  (&mut *addr_of_mut!(SYS_$id)).on_init(&mut *addr_of_mut!(VIEW), &mut TestSink);
            |}"""
      completeCalls = completeCalls :+
        st"""if SYSVERIF_LIVE && SYS_SUSPENDED.is_none() {
            |  REC.layer = Layer::Sysverif;
            |  REC.composition = "$id";
            |  (&mut *addr_of_mut!(SYS_$id)).on_complete(t, &mut *addr_of_mut!(VIEW), &mut TestSink);
            |}"""
      resumeCalls = resumeCalls :+
        st"""REC.layer = Layer::Sysverif;
            |REC.composition = "$id";
            |(&mut *addr_of_mut!(SYS_$id)).on_init(&mut *addr_of_mut!(VIEW), &mut TestSink);"""
      val props: ISZ[ST] = for (pId <- info.compositionProperties(i)) yield st"""pub const $pId: &str = "$pId";"""
      sysMods = sysMods :+
        st"""pub mod $id {
            |  pub const COMPOSITION: &str = "$id";
            |  ${(props, "\n")}
            |}"""
    }

    // D23: a switch per generated layer.  A layer the model does not have gets no function,
    // so asking for it is a compile error.
    def switchFns(layer: String, fnName: String, live: String, on: String, makeVar: String): ST = {
      return (
        st"""/// Turns the $layer checks on or off for the rest of this test.  Off, a violation is
            |/// not reported, but the layer keeps tracking, so turning it back on is immediately
            |/// correct.  It cannot turn on checks that $makeVar=off took out of the build.
            |pub fn $fnName(on: bool) {
            |  unsafe {
            |    if on && !$live {
            |      harness::fail_with(format_args!(
            |        "the $layer checks are off for this build ($makeVar=off); a test cannot turn them back on"));
            |      return;
            |    }
            |    $on = on;
            |  }
            |}""")
    }
    var switchApi: ISZ[ST] = ISZ()
    var suiteSettings: ISZ[ST] = ISZ()
    if (info.hasComponentLayer) {
      switchApi = switchApi :+ switchFns("GUMBO", "set_gumbo", "GUMBO_LIVE", "GUMBO_ON", "GUMBO_CHECKS")
      suiteSettings = suiteSettings :+ st"pub fn gumbo(value: bool) { super::set_gumbo(value) }"
    }
    if (info.compositionIds.nonEmpty) {
      switchApi = switchApi :+ switchFns("system-verification", "set_sysverif", "SYSVERIF_LIVE", "SYSVERIF_ON", "SYSVERIF_CHECKS")
      suiteSettings = suiteSettings :+ st"pub fn sysverif(value: bool) { super::set_sysverif(value) }"
    }
    val gumboBuilt: String = if (info.hasComponentLayer) "true" else "false"
    val sysverifBuilt: String = if (info.compositionIds.nonEmpty) "true" else "false"

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Contract checking in the test controller (TestScheduler-design.md, stage 7).
          |//!
          |//! The checks are crates/$crate, shared with the monitor protection domains.  This
          |//! module hosts them: `ControllerView` reads ports and state variables through the
          |//! controller's own `obs_get_*` receive cursors -- never through `inspect::get_*`,
          |//! whose reads are destructive -- and `TestSink` turns a violation into a failure of
          |//! the running test.
          |//!
          |//! A test that breaks a guarantee on purpose declares it, and still has every other
          |//! contract checked:
          |//!
          |//! ```ignore
          |//! observe::expect(observe::Expect::CepPost(observe::Thread::some_thread));
          |//! ```
          |//!
          |//! `observe::take()` hands the test the violations recorded so far instead, for it to
          |//! assert on; taken violations do not fail the test.
          |//!
          |//! Each layer -- GUMBO (the threads' contracts) and system verification (the
          |//! compositions' assertions) -- can be switched off (D23): for the whole run with
          |//! `make GUMBO_CHECKS=off` / `SYSVERIF_CHECKS=off`, which also stops tracking it; for
          |//! a suite with `suite name(gumbo = off) { .. }`; or for the rest of a test with
          |//! `observe::set_gumbo(false)`.  Every test starts from the build's setting.
          |
          |use core::ptr::addr_of_mut;
          |
          |use data::*;
          |use $crate::{Event, SystemView, ViolationSink};
          |pub use $crate::Thread;
          |use crate::system_tests::{api, harness, inspect, selection};
          |
          |extern "C" {
          |  ${(externs, "\n")}
          |}
          |
          |/// Maps a schedule channel to the thread it dispatches.
          |pub fn thread_of(ch: u32) -> Option<Thread> {
          |  match ch {
          |    ${(threadOfArms, "\n")}
          |    _ => None,
          |  }
          |}
          |
          |fn thread_named(name: &str) -> Option<Thread> {
          |  match name {
          |    ${(threadNamedArms, "\n")}
          |    _ => None,
          |  }
          |}
          |
          |// ---------------------------------------------------------------------------------
          |// Reading
          |// ---------------------------------------------------------------------------------
          |
          |/// Logical time: advanced whenever a value for the system layer is taken, so each
          |/// composition can tell what arrived in its own current frame.
          |static mut NOW: u32 = 0;
          |
          |fn tick() -> u32 {
          |  unsafe {
          |    NOW = NOW.wrapping_add(1);
          |    NOW
          |  }
          |}
          |
          |/// Each composition's frame start: the time its marking last reached END (or a frame was
          |/// started for all, see roll_frame).  A value taken after it is in its current frame.
          |static mut COMP_START: [u32; ${info.compositionIds.size}] = [0; ${info.compositionIds.size}];

          |
          |/// A value for the system layer, with the time it was taken; `injected` if a test put it.
          |#[derive(Clone)]
          |struct Stamped<T: Clone> {
          |  at: u32,
          |  value: Option<T>,
          |  injected: bool,
          |}
          |
          |#[allow(dead_code)]
          |impl<T: Clone> Stamped<T> {
          |  const fn empty() -> Self {
          |    Stamped { at: 0, value: None, injected: false }
          |  }
          |
          |  /// The value, if it was taken in the frame that started at `start` (wrap-safe).
          |  fn visible(&self, start: u32) -> Option<T> {
          |    if (self.at.wrapping_sub(start) as i32) > 0 { self.value.clone() } else { None }
          |  }
          |}
          |
          |/// Reads ports and state variables for the checks.  Data ports and state variables keep
          |/// the last value read -- what a thread sees -- so a check reads them as often as it
          |/// likes.  A data port never written reads as its default, as it does to the thread; a
          |/// state variable never written makes the check skip (`missing`).  Event ports are
          |/// read through one cursor per reading thread, so an event is seen once, at the check
          |/// of the thread that consumes it, however often that check reads it; the system layer
          |/// reads them as per-frame values (Stamped), for the composition in focus.
          |pub struct ControllerView {
          |  focus: Option<Thread>,
          |  missing: bool,
          |  /// the frame start of the composition whose assertion is reading
          |  sys_start: u32,
          |  ${(fields, "\n")}
          |}
          |
          |impl ControllerView {
          |  pub const fn new() -> Self {
          |    ControllerView {
          |      focus: None,
          |      missing: false,
          |      sys_start: 0,
          |      ${(fieldInits, "\n")}
          |    }
          |  }
          |
          |  /// `t` has completed a dispatch: what it sent on each of its event ports, and what
          |  /// it received on each unconnected event input, if anything, is this frame's value
          |  /// for the system assertions.
          |  fn producer_completed(&mut self, t: Thread) {
          |    match t {
          |      ${(producerCompletedArms :+ st"_ => {}", "\n")}
          |    }
          |  }
          |
          |  /// `t` overran: skip every event its unobserved dispatch consumed or sent.
          |  fn drain_cursors(&mut self, t: Thread) {
          |    match t {
          |      ${(drainCursorArms :+ st"_ => {}", "\n")}
          |    }
          |  }
          |
          |  /// `t` is about to be dispatched: latch what it receives on each connected input a
          |  /// composition aliases (see ContractObserverPlugin.ReceivedGetter).
          |  fn latch_received(&mut self, t: Thread) {
          |    match t {
          |      ${(latchReceivedArms :+ st"_ => {}", "\n")}
          |    }
          |  }
          |
          |  /// A new frame for every composition, starting at logical time `at`: nothing taken
          |  /// before it is in the frame.
          |  fn new_frame_at(&mut self, at: u32) {
          |    unsafe {
          |      for start in (&mut *addr_of_mut!(COMP_START)).iter_mut() {
          |        *start = at;
          |      }
          |    }
          |  }
          |
          |  /// `t` has dispatched: it adopted any injected state variables, and its sv_ regions
          |  /// carry them from here on.
          |  fn clear_pending(&mut self, t: Thread) {
          |    match t {
          |      ${(clears, "\n")}
          |      _ => {}
          |    }
          |  }
          |}
          |
          |impl SystemView for ControllerView {
          |  fn focus(&mut self, t: Option<Thread>) {
          |    ${(ISZ[ST](st"self.focus = t;", st"self.missing = false;") ++ seenResets, "\n")}
          |  }
          |
          |  fn missing(&self) -> bool {
          |    self.missing
          |  }
          |
          |  fn focus_system(&mut self, composition: usize) {
          |    self.sys_start = unsafe { COMP_START[composition] };
          |  }
          |
          |  fn frame_ended(&mut self, composition: usize) {
          |    unsafe { COMP_START[composition] = NOW; }
          |  }
          |
          |  ${(getters, "\n\n")}
          |}
          |
          |${(injectedFns, "\n\n")}
          |
          |// ---------------------------------------------------------------------------------
          |// Reporting
          |// ---------------------------------------------------------------------------------
          |
          |/// What a violation broke.
          |#[derive(Clone, Copy, PartialEq, Eq, Debug)]
          |pub enum Kind {
          |  IepPost,
          |  CepPost,
          |  SysAssert,
          |  Schedule,
          |}
          |
          |/// One violation recorded in the running test.
          |#[derive(Clone, Copy, Debug)]
          |pub struct Violation {
          |  pub kind: Kind,
          |  /// The thread whose guarantee was broken (IepPost, CepPost).
          |  pub thread: Option<Thread>,
          |  /// The composition, property and point of a broken system assertion (SysAssert).
          |  pub composition: &'static str,
          |  pub property: &'static str,
          |  pub point: &'static str,
          |  /// Where the schedule was: the hyperperiod and slot of the check.
          |  pub hp: u32,
          |  pub slot: u32,
          |}
          |
          |/// A violation a test causes on purpose; see [`expect`].  (An initialization guarantee
          |/// is checked before any test runs, so it cannot be expected: it fails the run instead,
          |/// through DONE's init=.)
          |#[derive(Clone, Copy, Debug)]
          |pub enum Expect {
          |  CepPost(Thread),
          |  /// (composition, property), named by the constants in [`sys`].
          |  SysAssert(&'static str, &'static str),
          |}
          |
          |impl Expect {
          |  /// Whether the layer that reports this violation is on right now; while it is off, its
          |  /// violations are not reported, so the expectation cannot be met.
          |  fn layer_on(&self) -> bool {
          |    unsafe {
          |      match *self {
          |        Expect::CepPost(_) => GUMBO_ON,
          |        Expect::SysAssert(_, _) => SYSVERIF_ON,
          |      }
          |    }
          |  }
          |
          |  fn layer(&self) -> &'static str {
          |    match *self {
          |      Expect::CepPost(_) => "GUMBO",
          |      Expect::SysAssert(_, _) => "system-verification",
          |    }
          |  }
          |
          |  fn matches(&self, v: &Violation) -> bool {
          |    match *self {
          |      Expect::CepPost(t) => v.kind == Kind::CepPost && v.thread == Some(t),
          |      Expect::SysAssert(c, p) => v.kind == Kind::SysAssert && v.composition == c && v.property == p,
          |    }
          |  }
          |}
          |
          |// ---------------------------------------------------------------------------------
          |// Switches (D23)
          |// ---------------------------------------------------------------------------------
          |
          |/// Live: the layer is generated and not turned off at build level, so it is tracked
          |/// and the scheduler parks for it.  On: its violations are reported right now.
          |static mut GUMBO_LIVE: bool = $gumboBuilt;
          |static mut SYSVERIF_LIVE: bool = $sysverifBuilt;
          |static mut GUMBO_ON: bool = $gumboBuilt;
          |static mut SYSVERIF_ON: bool = $sysverifBuilt;
          |
          |fn load_build_switches() {
          |  let f = selection::flags();
          |  unsafe {
          |    GUMBO_LIVE = $gumboBuilt && f & selection::FLAG_GUMBO_OFF == 0;
          |    SYSVERIF_LIVE = $sysverifBuilt && f & selection::FLAG_SYSVERIF_OFF == 0;
          |    GUMBO_ON = GUMBO_LIVE;
          |    SYSVERIF_ON = SYSVERIF_LIVE;
          |  }
          |}
          |
          |/// Whether any layer is tracked: without one, commands take no parks.
          |pub(crate) fn any_live() -> bool {
          |  unsafe { GUMBO_LIVE || SYSVERIF_LIVE }
          |}
          |
          |${(switchApi, "\n\n")}
          |
          |/// The settings `suite name(gumbo = off, ..)` in `system_tests!` applies at the start of
          |/// each of the suite's tests.
          |pub mod suite_setting {
          |  #![allow(non_upper_case_globals)]
          |  pub const on: bool = true;
          |  pub const off: bool = false;
          |  ${(suiteSettings, "\n")}
          |}
          |
          |/// Composition and property names for [`Expect::SysAssert`], so a typo is a compile error.
          |pub mod sys {
          |  ${(sysMods, "\n")}
          |}
          |
          |pub const MAX_RECORDED: usize = 16;
          |pub const MAX_EXPECTED: usize = 8;
          |
          |/// The layer whose check is running, for the switches.
          |#[derive(Clone, Copy, PartialEq, Eq)]
          |enum Layer {
          |  Gumbo,
          |  Sysverif,
          |}
          |
          |#[derive(Clone, Copy, PartialEq, Eq)]
          |enum Phase {
          |  /// the initialization checks, before the first test
          |  Init,
          |  /// a test's recording window: from its BEGIN to the end of its body
          |  InTest,
          |  /// everything else, e.g. the runner's normalization between tests: checked and
          |  /// tracked, but not anyone's verdict
          |  Between,
          |}
          |
          |struct Recorder {
          |  phase: Phase,
          |  layer: Layer,
          |  init_failed: bool,
          |  at_init: bool,
          |  hp: u32,
          |  slot: u32,
          |  /// the composition whose system layer is running
          |  composition: &'static str,
          |  recorded: [Option<Violation>; MAX_RECORDED],
          |  n_recorded: usize,
          |  /// The running test's violations that no expectation matched and no take() claimed,
          |  /// counting those past MAX_RECORDED too.
          |  unhandled: u32,
          |  expected: [Option<(Expect, bool)>; MAX_EXPECTED],
          |}
          |
          |static mut REC: Recorder = Recorder {
          |  phase: Phase::Between,
          |  layer: Layer::Gumbo,
          |  init_failed: false,
          |  at_init: false,
          |  hp: 0,
          |  slot: 0,
          |  composition: "",
          |  recorded: [None; MAX_RECORDED],
          |  n_recorded: 0,
          |  unhandled: 0,
          |  expected: [None; MAX_EXPECTED],
          |};
          |
          |static mut VIEW: ControllerView = ControllerView::new();
          |${(layerStatics, "\n")}
          |
          |/// Where the schedule is, for a VIOLATION line.
          |struct Position;
          |
          |impl core::fmt::Display for Position {
          |  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
          |    unsafe {
          |      if REC.at_init {
          |        write!(f, "init")
          |      } else {
          |        write!(f, "hp={} slot={}", REC.hp, REC.slot)
          |      }
          |    }
          |  }
          |}
          |
          |/// Decides what becomes of a violation, prints its line, and returns whether its
          |/// details (the pre/post values) should follow.
          |fn violation(v: Violation, what: core::fmt::Arguments) -> bool {
          |  unsafe {
          |    match REC.phase {
          |      Phase::Between => return false,
          |      Phase::Init => REC.init_failed = true,
          |      Phase::InTest => {
          |        for e in REC.expected.iter_mut() {
          |          if let Some((x, met)) = e {
          |            if x.matches(&v) {
          |              *met = true;
          |              harness::line(format_args!("TEST | INFO  expected violation: {}", what));
          |              return false;
          |            }
          |          }
          |        }
          |        if REC.n_recorded < MAX_RECORDED {
          |          REC.recorded[REC.n_recorded] = Some(v);
          |          REC.n_recorded += 1;
          |        }
          |        REC.unhandled += 1;
          |      }
          |    }
          |  }
          |  harness::line(format_args!("TEST | VIOLATION {}", what));
          |  true
          |}
          |
          |/// Whether what the running check finds is reported: inside a recording window, and
          |/// from a layer that is switched on.
          |fn in_window() -> bool {
          |  unsafe {
          |    let on = match REC.layer {
          |      Layer::Gumbo => GUMBO_ON,
          |      Layer::Sysverif => SYSVERIF_ON,
          |    };
          |    REC.phase != Phase::Between && on
          |  }
          |}
          |
          |fn new_violation(kind: Kind, thread: Option<Thread>, property: &'static str, point: &'static str) -> Violation {
          |  unsafe {
          |    Violation { kind: kind, thread: thread, composition: REC.composition, property: property,
          |                point: point, hp: REC.hp, slot: REC.slot }
          |  }
          |}
          |
          |/// Reports what the checks find as test output.  IEP_Post, CEP_Post and system
          |/// assertions are violations; a failed CEP_Pre is information -- the dispatch's
          |/// assumption did not hold, so it owes no guarantee (D24).
          |pub struct TestSink;
          |
          |impl ViolationSink for TestSink {
          |  fn report(&mut self, e: Event) {
          |    if !in_window() {
          |      return;
          |    }
          |    match e {
          |      Event::IepPostViolation { thread, post } => {
          |        let v = new_violation(Kind::IepPost, thread_named(thread), "", "");
          |        if violation(v, format_args!("IEP_Post {} {}", thread, Position)) {
          |          harness::line(format_args!("TEST | INFO  {} post: {:?}", thread, post));
          |        }
          |      }
          |      Event::CepPreViolation { thread, pre } => {
          |        harness::line(format_args!("TEST | INFO  {} assumption not met (CEP_Pre) {}", thread, Position));
          |        harness::line(format_args!("TEST | INFO  {} pre: {:?}", thread, pre));
          |      }
          |      Event::CepPostViolation { thread, pre, post } => {
          |        let v = new_violation(Kind::CepPost, thread_named(thread), "", "");
          |        if violation(v, format_args!("CEP_Post {} {}", thread, Position)) {
          |          harness::line(format_args!("TEST | INFO  {} pre: {:?}", thread, pre));
          |          harness::line(format_args!("TEST | INFO  {} post: {:?}", thread, post));
          |        }
          |      }
          |      // no saved pre-state: the thread's first completion observed, or after a resync
          |      Event::CepPostSkipped { .. } => {}
          |      Event::CepPostExcused { thread } => {
          |        harness::line(format_args!("TEST | INFO  {} CEP_Post not checked: its assumption was not met", thread));
          |      }
          |      Event::CheckSkipped { thread, check } => {
          |        harness::line(format_args!("TEST | INFO  {} {} not checked: a value it reads was never written", thread, check));
          |      }
          |      Event::SysAssertViolation { property, point } => {
          |        let v = new_violation(Kind::SysAssert, None, property, point);
          |        let _ = violation(v, format_args!("SysAssert {}/{} at {} {}", v.composition, property, point, Position));
          |      }
          |      Event::ScheduleNoTransition { ch, timeslice } => {
          |        let v = new_violation(Kind::Schedule, None, "", "");
          |        let _ = violation(v, format_args!("Schedule {}: no enabled transition for channel {} at slot {}", v.composition, ch, timeslice));
          |      }
          |      Event::ScheduleNoEnd { ready } => {
          |        let v = new_violation(Kind::Schedule, None, "", "");
          |        let _ = violation(v, format_args!("Schedule {}: the walk did not reach END (marking 0x{:x})", v.composition, ready));
          |      }
          |      Event::ScheduleConformance { .. } => {}
          |    }
          |  }
          |}
          |
          |// ---------------------------------------------------------------------------------
          |// Checking points
          |// ---------------------------------------------------------------------------------
          |
          |/// Every thread has initialized and none has computed.  Checks the initialization
          |/// guarantees, and for each composition validates the schedule and puts the marking at
          |/// its start.  Its violations belong to no test; they make DONE report init=failed.
          |pub(crate) fn on_init() {
          |  load_build_switches();
          |  unsafe {
          |    REC.phase = Phase::Init;
          |    REC.at_init = true;
          |    ${(initFrameCalls, "\n")}
          |    ${(initCalls, "\n")}
          |    ${(initDrains, "\n")}
          |    REC.at_init = false;
          |    REC.phase = Phase::Between;
          |  }
          |}
          |
          |/// The thread over channel `ch` has completed the dispatch it began at (`hp`, `slot`).
          |pub(crate) fn on_complete(ch: u32, hp: u32, slot: u32) {
          |  if let Some(t) = thread_of(ch) {
          |    unsafe {
          |      REC.hp = hp;
          |      REC.slot = slot;
          |      (&mut *addr_of_mut!(VIEW)).clear_pending(t);
          |      (&mut *addr_of_mut!(VIEW)).producer_completed(t);
          |      ${(completeCalls, "\n")}
          |    }
          |  }
          |}
          |
          |/// The slot the schedule was at when the controller last looked (roll_frame).
          |static mut POS_SLOT: u32 = 0;
          |
          |/// Whether a user dispatch is still to come in the current hyperperiod.  Only then is
          |/// what a test injects now in this hyperperiod's frame: at a stop after its last
          |/// dispatch (e.g. on a trailing pad) it is already the next frame's.
          |fn dispatch_left_in_hp() -> bool {
          |  let s = api::schedule();
          |  let n = (s.num_timeslices as usize).min(api::MAX_SCHEDULE_SLOTS);
          |  let mut i = unsafe { POS_SLOT } as usize;
          |  while i < n {
          |    if s.is_user_partition[i] {
          |      return true;
          |    }
          |    i += 1;
          |  }
          |  false
          |}
          |
          |/// Whether the thread reached over channel `ch` has a user slot at or after the current
          |/// position in this hyperperiod -- it would then receive what is queued for it now.
          |fn slot_left_for(ch: u32) -> bool {
          |  let s = api::schedule();
          |  let n = (s.num_timeslices as usize).min(api::MAX_SCHEDULE_SLOTS);
          |  let mut i = unsafe { POS_SLOT } as usize;
          |  while i < n {
          |    if s.is_user_partition[i] && s.timeslice_ch[i] == ch {
          |      return true;
          |    }
          |    i += 1;
          |  }
          |  false
          |}
          |
          |/// The hyperperiod each thread was last dispatched in (by thread_index).
          |static mut DISPATCHED_HP: [Option<u32>; ${info.threads.size}] = [None; ${info.threads.size}];
          |
          |fn thread_index(t: Thread) -> usize {
          |  match t {
          |    ${(threadIndexArms, "\n")}
          |  }
          |}
          |
          |/// The thread over channel `ch` is about to be dispatched at (`hp`, `slot`).
          |pub(crate) fn on_dispatch(ch: u32, hp: u32, slot: u32) {
          |  if let Some(t) = thread_of(ch) {
          |    unsafe {
          |      DISPATCHED_HP[thread_index(t)] = Some(hp);
          |      REC.hp = hp;
          |      REC.slot = slot;
          |      (&mut *addr_of_mut!(VIEW)).latch_received(t);
          |      ${(dispatchCalls, "\n")}
          |    }
          |  }
          |}
          |
          |/// The hyperperiod in which the system layers lost their place in the workflow net --
          |/// a dispatch completed unobserved, or overran -- if they have not yet resumed, and
          |/// whether they may resume within it.  Their marking describes nothing until a frame
          |/// starts afresh.
          |static mut SYS_SUSPENDED: Option<(u32, bool)> = None;
          |
          |/// The hyperperiod of the frame the system layers' event values belong to.
          |static mut FRAME_HP: Option<u32> = None;
          |
          |/// The logical time the current hyperperiod's frame began: where the system layers'
          |/// frames start again when they resume in it (so an injection a test made at the stop
          |/// before the resuming park is in the frame).
          |static mut HP_FRAME_AT: u32 = 0;
          |
          |/// The logical time the latest completion was accounted for.  A hyperperiod's frame
          |/// begins here, not where the new hyperperiod is first observed: a command can stop on a
          |/// pad after the last dispatch of a hyperperiod, and what a test injects there is the
          |/// next frame's.
          |static mut LAST_DONE_AT: u32 = 0;
          |
          |/// Stops trusting what was saved about dispatches in flight (TestScheduler-design.md,
          |/// "After a watchdog trip"): every saved pre-state is dropped, so each thread's next
          |/// completion is skipped rather than checked against a dispatch it no longer
          |/// describes, and the system layers are suspended until the next frame.  One INFO
          |/// line instead of a run of false violations.
          |///
          |/// `dispatched` is the channel of a dispatch known to have run -- the one that overran:
          |/// its thread has adopted any state variable injected into it, so the pending value is
          |/// dropped too.  Others' pending injections stay: their threads may not have run yet.
          |/// (When completions went unobserved, the caller clears the threads that ran.)
          |///
          |/// `same_frame`: whether the system layers may resume at the first user slot of this
          |/// hyperperiod.  Not after an overrun: the aborted dispatch's output still reaches its
          |/// consumers in this frame, which then mixes two dispatches of the thread, so the
          |/// layers resume only in the next hyperperiod.
          |fn lose_track(hp: u32, dispatched: Option<u32>, same_frame: bool, why: core::fmt::Arguments) {
          |  unsafe {
          |    if let Some(t) = dispatched.and_then(thread_of) {
          |      (&mut *addr_of_mut!(VIEW)).clear_pending(t);
          |    }
          |    ${(forgetCalls, "\n")}
          |    let suspend = ${if (info.compositionIds.nonEmpty) "SYSVERIF_LIVE" else "false"};
          |    if suspend {
          |      SYS_SUSPENDED = Some((hp, same_frame));
          |    }
          |    harness::line(format_args!("TEST | INFO  {}; saved pre-states dropped{}", why,
          |      if suspend { ", system assertions suspended until the next frame" } else { "" }));
          |  }
          |}
          |
          |/// The dispatch the scheduler last let go ahead, which the next completion finishes:
          |/// (channel, hyperperiod, slot).
          |static mut IN_FLIGHT: Option<(u32, u32, u32)> = None;
          |/// The scheduler's completed_seq when a completion was last accounted for.
          |static mut COMPLETED: u32 = 0;
          |
          |/// Runs the completion check if the scheduler has completed a dispatch since the last
          |/// one accounted for.  Exactly one: the scheduler parks before every user dispatch, so
          |/// completed_seq moves by at most one between checks.  More means a dispatch went
          |/// unobserved, whose pre-states and marking can no longer be trusted.
          |fn account_completion(st: &api::TestStatus) {
          |  unsafe {
          |    let moved = st.completed_seq.wrapping_sub(COMPLETED);
          |    if moved == 0 {
          |      return; // nothing completed: e.g. a command that dispatched nothing
          |    }
          |    COMPLETED = st.completed_seq;
          |    match IN_FLIGHT {
          |      Some((ch, hp, slot)) if moved == 1 && ch == st.last_dispatched_ch => on_complete(ch, hp, slot),
          |      _ => {
          |        forget_ran(st, moved);
          |        lose_track(st.hyperperiod_num, None, true,
          |          format_args!("{} dispatch(es) completed unobserved", moved));
          |      }
          |    }
          |    IN_FLIGHT = None;
          |    LAST_DONE_AT = NOW;
          |  }
          |}
          |
          |/// The `moved` dispatches before `st`'s position ran unobserved: their threads adopted any
          |/// state variable injected into them, and consumed and sent events no check saw.  Drops
          |/// both, as after an overrun.  Walks the published schedule back from the position over
          |/// that many user slots -- the ones completed_seq counts; the schedule repeats, so one
          |/// full frame covers every thread however many dispatches went by.
          |fn forget_ran(st: &api::TestStatus, moved: u32) {
          |  let s = api::schedule();
          |  let n = s.num_timeslices as usize;
          |  if n == 0 || n > api::MAX_SCHEDULE_SLOTS {
          |    return;
          |  }
          |  let mut i = (st.current_timeslice as usize) % n;
          |  let mut left = moved;
          |  let mut steps = 0usize;
          |  while left > 0 && steps < n {
          |    i = if i == 0 { n - 1 } else { i - 1 };
          |    steps += 1;
          |    if s.is_user_partition[i] {
          |      left -= 1;
          |      if let Some(t) = thread_of(s.timeslice_ch[i]) {
          |        unsafe {
          |          (&mut *addr_of_mut!(VIEW)).clear_pending(t);
          |          (&mut *addr_of_mut!(VIEW)).drain_cursors(t);
          |        }
          |      }
          |    }
          |  }
          |}
          |
          |/// The index of the frame's first user slot in the published schedule.
          |fn first_user_slot() -> u32 {
          |  let s = api::schedule();
          |  let mut i = 0usize;
          |  while i < (s.num_timeslices as usize) && i < api::MAX_SCHEDULE_SLOTS {
          |    if s.is_user_partition[i] {
          |      return i as u32;
          |    }
          |    i += 1;
          |  }
          |  0
          |}
          |
          |/// The position is now in `st`'s hyperperiod: if that is a new one, so is the frame the
          |/// system assertions' event values belong to -- unless a system layer is running, which
          |/// ends its frames itself, at END (frame_ended); clearing here too would lose a value a
          |/// test injected after END.  Called once completions are accounted for -- they belong
          |/// to the frame they ran in -- at every park and at the end of every command, so a
          |/// value a test injects afterwards belongs to the frame of the next dispatch.  The very
          |/// first frame keeps what producers sent while initializing.
          |fn roll_frame(st: &api::TestStatus) {
          |  unsafe {
          |    POS_SLOT = st.current_timeslice;
          |    if FRAME_HP != Some(st.hyperperiod_num) {
          |      let system_running = ${if (info.compositionIds.nonEmpty) "SYSVERIF_LIVE && SYS_SUSPENDED.is_none()" else "false"};
          |      if FRAME_HP.is_some() && !system_running {
          |        (&mut *addr_of_mut!(VIEW)).new_frame_at(LAST_DONE_AT);
          |      }
          |      FRAME_HP = Some(st.hyperperiod_num);
          |      HP_FRAME_AT = LAST_DONE_AT;
          |    }
          |  }
          |}
          |
          |/// The scheduler is parked before dispatching `st.next_ch` at (hyperperiod, slot): the
          |/// dispatch before it, if any, has completed.
          |pub(crate) fn at_park(st: &api::TestStatus) {
          |  account_completion(st);
          |  roll_frame(st);
          |  unsafe {
          |    // The system layers resume at the first dispatch of a frame, starting it afresh as
          |    // at initialization.  Every user dispatch parks, so that is the first park in a later
          |    // hyperperiod -- or this one, if tracking was lost at the frame's first park and not
          |    // by an overrun (see lose_track).
          |    if let Some((hp, same_frame)) = SYS_SUSPENDED {
          |      if st.hyperperiod_num != hp || (same_frame && st.current_timeslice == first_user_slot()) {
          |        SYS_SUSPENDED = None;
          |        harness::line(format_args!("TEST | INFO  system assertions resumed at hp={}", st.hyperperiod_num));
          |        REC.hp = st.hyperperiod_num;
          |        REC.slot = st.current_timeslice;
          |        // the frames start afresh with this hyperperiod: START sees only what arrived since
          |        // it began -- including what a test injected at the stop before this park
          |        (&mut *addr_of_mut!(VIEW)).new_frame_at(HP_FRAME_AT);
          |        ${(resumeCalls, "\n")}
          |      }
          |    }
          |  }
          |  on_dispatch(st.next_ch, st.hyperperiod_num, st.current_timeslice);
          |  unsafe { IN_FLIGHT = Some((st.next_ch, st.hyperperiod_num, st.current_timeslice)); }
          |}
          |
          |/// A command has completed, so its last dispatch has too -- unless it overran, in which
          |/// case that dispatch never finished and nothing saved for it describes anything.
          |pub(crate) fn at_command_end(st: &api::TestStatus) {
          |  if !any_live() {
          |    return; // nothing tracked, nothing parked for
          |  }
          |  if st.flags & api::FLAG_OVERRUN != 0 {
          |    unsafe {
          |      // A report of an earlier trip (the position is still at the overran slot) dispatched
          |      // nothing: IN_FLIGHT is already None.
          |      if IN_FLIGHT.is_some() {
          |        lose_track(st.hyperperiod_num, Some(st.last_dispatched_ch), false,
          |          format_args!("channel {} overran its slot", st.last_dispatched_ch));
          |        // what the dispatch consumed and sent by now; a dispatch still running when
          |        // the controller gets here is not caught up with
          |        if let Some(t) = thread_of(st.last_dispatched_ch) {
          |          (&mut *addr_of_mut!(VIEW)).drain_cursors(t);
          |        }
          |        IN_FLIGHT = None;
          |      }
          |      COMPLETED = st.completed_seq;
          |    }
          |    roll_frame(st);
          |    return;
          |  }
          |  account_completion(st);
          |  roll_frame(st);
          |}
          |
          |/// Whether the initialization checks passed.
          |pub(crate) fn init_ok() -> bool {
          |  unsafe { !REC.init_failed }
          |}
          |
          |/// Opens a test's recording window.
          |pub(crate) fn begin_test() {
          |  unsafe {
          |    // every test starts from the build's setting, whatever the last one switched
          |    GUMBO_ON = GUMBO_LIVE;
          |    SYSVERIF_ON = SYSVERIF_LIVE;
          |    REC.phase = Phase::InTest;
          |    REC.recorded = [None; MAX_RECORDED];
          |    REC.n_recorded = 0;
          |    REC.unhandled = 0;
          |    REC.expected = [None; MAX_EXPECTED];
          |  }
          |}
          |
          |/// Closes the test's recording window, and fails the test for violations it did not
          |/// expect or take, and for expectations never met.
          |pub(crate) fn end_test() {
          |  unsafe {
          |    REC.phase = Phase::Between;
          |    for e in REC.expected.iter() {
          |      if let Some((x, false)) = e {
          |        if x.layer_on() {
          |          harness::fail_with(format_args!("expected violation did not occur: {:?}", x));
          |        } else {
          |          harness::fail_with(format_args!("expected violation {:?} cannot be reported: the {} checks are off", x, x.layer()));
          |        }
          |      }
          |    }
          |    if REC.unhandled > 0 {
          |      harness::fail_with(format_args!("contract violation ({} in this test)", REC.unhandled));
          |    }
          |  }
          |}
          |
          |// ---------------------------------------------------------------------------------
          |// The test-facing API
          |// ---------------------------------------------------------------------------------
          |
          |/// Declares a violation the rest of this test causes on purpose: a matching violation
          |/// is reported as INFO instead of failing the test, and the test fails if none occurs.
          |/// Its layer must be on while the violation happens: a switched-off layer reports
          |/// nothing, and the test fails saying so.
          |pub fn expect(e: Expect) {
          |  if !e.layer_on() {
          |    harness::fail_with(format_args!("expected violation {:?} cannot be reported: the {} checks are off", e, e.layer()));
          |    return;
          |  }
          |  unsafe {
          |    for slot in REC.expected.iter_mut() {
          |      if slot.is_none() {
          |        *slot = Some((e, false));
          |        return;
          |      }
          |    }
          |  }
          |  harness::fail_with(format_args!("more than {} expectations in one test", MAX_EXPECTED));
          |}
          |
          |/// The violations recorded so far in this test, which no longer fail it.
          |pub struct Taken {
          |  items: [Option<Violation>; MAX_RECORDED],
          |  n: usize,
          |  total: u32,
          |}
          |
          |impl Taken {
          |  /// How many there were, including any past MAX_RECORDED that were not kept.
          |  pub fn count(&self) -> u32 {
          |    self.total
          |  }
          |
          |  pub fn is_empty(&self) -> bool {
          |    self.total == 0
          |  }
          |
          |  pub fn iter(&self) -> impl Iterator<Item = &Violation> {
          |    self.items[..self.n].iter().flatten()
          |  }
          |
          |  /// Whether one of them is what `e` describes.
          |  pub fn contains(&self, e: Expect) -> bool {
          |    self.iter().any(|v| e.matches(v))
          |  }
          |}
          |
          |/// Takes the violations recorded so far in this test, for the test to assert on.
          |pub fn take() -> Taken {
          |  unsafe {
          |    let t = Taken { items: REC.recorded, n: REC.n_recorded, total: REC.unhandled };
          |    REC.recorded = [None; MAX_RECORDED];
          |    REC.n_recorded = 0;
          |    REC.unhandled = 0;
          |    t
          |  }
          |}
          |""")
  }

  val mk: ST =
    st"""${CommentTemplate.doNotEditComment_hash}
        |
        |# Test scheduler configuration.
        |# Usage: make CONFIG=$v.mk
        |export MSD := $$(TOP_DIR)/$v.meta.py
        |export SCHEDULER_C := $$(TOP_DIR)/scheduler/src/$v.scheduler.c
        |export SCHEDULER_CONFIG_HEADERS := $$(TOP_DIR)/scheduler/include/$v.user_config.h
        |# the test controller is built only for this variant
        |export EXTRA_IMAGES := ${TestSchedulerPlugin.controllerName}_process_${TestSchedulerPlugin.controllerName}_thread.elf ${TestSchedulerPlugin.controllerName}_process_${TestSchedulerPlugin.controllerName}_thread_MON.elf"""

  val user_config_h: ST =
    st"""#pragma once
        |
        |#include <stdbool.h>
        |#include <stdint.h>
        |#include <$v.scheduler_config.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// The metaprogram emits a binary with the same format as this struct, which is
        |// patched into the scheduler's ELF at system build time.  sdfgen_helper.py parses
        |// this file (and only this file) to build the matching Python serializer, so any
        |// struct it must serialize has to be declared here rather than in
        |// $v.scheduler_config.h.
        |
        |typedef struct user_schedule {
        |    uint64_t timeslices[MAX_SCHEDULE_SLOTS];
        |    uint32_t timeslice_ch[MAX_SCHEDULE_SLOTS];
        |    bool is_user_partition[MAX_SCHEDULE_SLOTS];
        |    uint32_t num_timeslices;
        |} user_schedule_t;"""

  @pure def scheduler_config_h(controllerChannel: Z): ST = {
    return (st"""#pragma once
        |
        |#include <microkit.h>
        |#include <stdbool.h>
        |#include <stdint.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// The max partitions is limited by the number of channels that can be established
        |// between the scheduler and a partition's initial process in microkit.
        |// One channel is taken by the sDDF timer subsystem.
        |#define MAX_PARTITIONS (MICROKIT_MAX_CHANNELS - 1)
        |
        |// Maximum number of timeslice slots in a schedule.  A thread may appear in multiple
        |// slots per frame period, so this can exceed MAX_PARTITIONS.
        |#define MAX_SCHEDULE_SLOTS 128
        |
        |// ---------------------------------------------------------------------------
        |// Shared memory regions.  These must match $v.meta.py.
        |// ---------------------------------------------------------------------------
        |#define TEST_CMD_VADDR       0x4002000UL
        |#define TEST_CMD_SIZE        0x1000UL
        |#define TEST_STATUS_VADDR    0x4003000UL
        |#define TEST_STATUS_SIZE     0x1000UL
        |#define TEST_SCHEDULE_VADDR  0x4004000UL
        |#define TEST_SCHEDULE_SIZE   0x1000UL
        |
        |// Channel to the test controller's _MON, which forwards to the controller itself.
        |// Assigned by the system description; the scheduler both kicks the controller here
        |// once every partition is ready and is woken here when a command arrives.
        |#define TEST_CONTROLLER_CH ${controllerChannel}
        |
        |// Command types, mirroring art.scheduling.static.Command.
        |#define TEST_CMD_NONE          0
        |#define TEST_CMD_RUN_FOREVER   1
        |#define TEST_CMD_SSTEP         2
        |#define TEST_CMD_HSTEP         3
        |#define TEST_CMD_RUN_TO_SLOT   4
        |#define TEST_CMD_RUN_TO_HP     5
        |#define TEST_CMD_RUN_TO_STATE  6
        |#define TEST_CMD_RUN_TO_THREAD 7
        |#define TEST_CMD_INFO_STATE    8
        |#define TEST_CMD_INFO_SCHEDULE 9
        |#define TEST_CMD_STOP          10
        |
        |// Watchdog bound for a dispatched slot, derived from that slot's configured budget.
        |// Dispatch is completion-driven, so the timer no longer paces the schedule; it exists
        |// only so a thread that never reports completion fails the run instead of wedging it.
        |#define TEST_WATCHDOG_FACTOR 10
        |#define TEST_WATCHDOG_MIN_NS (1000000000ULL)
        |
        |// test_status.flags bits.  STOPPED is sticky; the others are per-command.
        |#define TEST_FLAG_STOPPED      0x1u
        |#define TEST_FLAG_BAD_COMMAND  0x2u
        |#define TEST_FLAG_UNREACHABLE  0x4u
        |#define TEST_FLAG_OVERRUN      0x8u
        |
        |// Written by the controller, read by the scheduler.  The controller fills the body,
        |// then stores seq last; the scheduler acts on a command only when seq changes.
        |typedef struct test_command {
        |    uint32_t seq;
        |    uint32_t type;
        |    uint32_t count;       // Sstep / Hstep
        |    uint32_t target_ch;   // RunToThread
        |    uint32_t target_hp;   // RunToHP / RunToState
        |    uint32_t target_slot; // RunToSlot / RunToState
        |    // Non-zero: park before every user dispatch this command makes, so the controller
        |    // can check contracts at the boundary (TestScheduler-design.md, stage 7).
        |    uint32_t observe;
        |    // Echoes test_status.obs_seq once the controller has checked a park; that is
        |    // what lets the parked dispatch go ahead.
        |    uint32_t obs_ack;
        |} test_command_t;
        |
        |// Written by the scheduler, read by the controller.  ack_seq is stored last and
        |// echoes test_command.seq once the command has completed; obs_seq is stored last
        |// when the scheduler parks for observation.
        |typedef struct test_status {
        |    uint32_t ack_seq;
        |    uint32_t current_timeslice;
        |    uint32_t hyperperiod_num;
        |    uint32_t last_dispatched_ch;
        |    uint32_t flags;
        |    // The channel of the slot at current_timeslice: at a park, the dispatch waiting.
        |    uint32_t next_ch;
        |    // User-slot completions so far, observed or not.  Identifies a dispatch, which a
        |    // channel cannot: the same channel completes once per frame.
        |    uint32_t completed_seq;
        |    // Incremented at each observation park.
        |    uint32_t obs_seq;
        |} test_status_t;
        |
        |// Published once at init so a controller can map slot indices to channels.  A plain
        |// struct rather than a HAMR queue: the sb_queue wrappers for hamr::Schedule are only
        |// generated when runtime monitoring forces that type to be touched, and this variant
        |// must also work for models built without any monitoring.
        |typedef struct test_schedule {
        |    uint32_t num_timeslices;
        |    uint32_t timeslice_ch[MAX_SCHEDULE_SLOTS];
        |    uint64_t timeslices[MAX_SCHEDULE_SLOTS];
        |    bool is_user_partition[MAX_SCHEDULE_SLOTS];
        |} test_schedule_t;
        |
        |// Each struct fills a fixed region; one that outgrew it would spill into the next.
        |_Static_assert(sizeof(test_command_t) <= TEST_CMD_SIZE, "test_command_t outgrows its shared memory region");
        |_Static_assert(sizeof(test_status_t) <= TEST_STATUS_SIZE, "test_status_t outgrows its shared memory region");
        |_Static_assert(sizeof(test_schedule_t) <= TEST_SCHEDULE_SIZE, "test_schedule_t outgrows its shared memory region");""")
  }

  val scheduler_c: ST =
    st"""#include <stdint.h>
        |#include <stdbool.h>
        |
        |#include <microkit.h>
        |#include <sel4/sel4.h>
        |#include <os/sddf.h>
        |#include <sddf/timer/client.h>
        |#include <sddf/timer/config.h>
        |#include <sddf/util/printf.h>
        |
        |#include <$v.scheduler_config.h>
        |#include <$v.user_config.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// Command-driven variant of the MCS user-land scheduler.  See
        |// hamr/codegen/doc/TestScheduler-design.md.
        |//
        |// Unlike the default scheduler this is a state machine: a command establishes what
        |// still has to happen, and each slot completion advances it.  A passive protection
        |// domain cannot run while this one is inside notified(), so a slot is dispatched by
        |// returning to the event loop, never by blocking here.
        |//
        |// The scheduler is always positioned AT A SLOT THAT HAS NOT YET RUN.  A command that
        |// has reached its stop point leaves it parked there, so a controller can inspect the
        |// inputs of the component that is about to run.
        |
        |/* Number of nanoseconds in a second */
        |#define NS_IN_S  1000000000ULL
        |
        |__attribute__((__section__(".timer_client_config"))) timer_client_config_t config;
        |__attribute__((__section__(".user_schedule"))) user_schedule_t user_schedule;
        |
        |volatile test_command_t  *test_cmd      = (volatile test_command_t *)  TEST_CMD_VADDR;
        |volatile test_status_t   *test_status   = (volatile test_status_t *)   TEST_STATUS_VADDR;
        |volatile test_schedule_t *test_schedule = (volatile test_schedule_t *) TEST_SCHEDULE_VADDR;
        |
        |uint32_t current_timeslice;
        |uint32_t hyperperiod_num;
        |uint32_t last_dispatched_ch;
        |
        |// Bitstring for partition ready status. 0 = not ready, 1 = ready.
        |uint64_t part_ready;
        |uint64_t part_ready_check;
        |
        |bool scheduler_running;
        |
        |// Active command state.
        |static uint32_t active_cmd;
        |static uint32_t accepted_seq;
        |static uint32_t status_flags;
        |static uint32_t slots_remaining;  // Sstep / Hstep budget
        |static uint32_t target_ch;
        |static uint32_t target_hp;
        |static uint32_t target_slot;
        |
        |// Slots a RunTo* command may dispatch before its predicate is declared unreachable.
        |// Without it a target that never occurs -- a channel absent from the schedule, a
        |// hyperperiod already passed -- would dispatch forever.
        |static uint32_t runto_budget;
        |// A run-to command ended UNREACHABLE while advancing, so it still owes its acknowledgement.
        |static bool ended_unreachable;
        |
        |// Whether a dispatched slot is in flight, with its watchdog armed.
        |// sddf_timer_set_timeout cannot be cancelled: arming the next slot replaces the
        |// timeout, but an expiry already signalled for the previous slot is still delivered --
        |// possibly after that next slot was armed.  The deadline tells the two apart: an
        |// expiry before the slot in flight's deadline is not its own.
        |static bool armed;
        |static uint64_t armed_deadline;
        |
        |// Observation (stage 7): whether the active command parks before each user dispatch,
        |// the completions so far, and the park in progress, if any.
        |static bool cmd_observe;
        |static uint32_t completed_seq;
        |static uint32_t obs_seq;
        |static bool obs_pending;
        |
        |// A thread that overran its watchdog is still running that dispatch.  Its completion,
        |// when it comes, belongs to the aborted dispatch, not to any later one; until it
        |// arrives the thread cannot be dispatched again.  One at most: the position stays at
        |// the overran slot, and every command reports the overrun again from there without
        |// dispatching anything, until the late completion arrives.
        |static bool overrun_pending;
        |static microkit_channel overrun_ch;
        |
        |static bool is_runto(uint32_t cmd) {
        |    return cmd == TEST_CMD_RUN_TO_SLOT || cmd == TEST_CMD_RUN_TO_HP ||
        |           cmd == TEST_CMD_RUN_TO_STATE || cmd == TEST_CMD_RUN_TO_THREAD;
        |}
        |
        |static bool scheduled_channel(uint32_t ch) {
        |    for (uint32_t i = 0; i < user_schedule.num_timeslices; i++) {
        |        if (user_schedule.timeslice_ch[i] == ch) {
        |            return true;
        |        }
        |    }
        |    return false;
        |}
        |
        |static void publish_position(void) {
        |    test_status->current_timeslice  = current_timeslice;
        |    test_status->hyperperiod_num    = hyperperiod_num;
        |    test_status->last_dispatched_ch = last_dispatched_ch;
        |    test_status->next_ch            = user_schedule.timeslice_ch[current_timeslice];
        |    test_status->completed_seq      = completed_seq;
        |}
        |
        |static void publish_status(void) {
        |    publish_position();
        |    test_status->flags              = status_flags;
        |    // ack_seq is stored last, and only after everything it describes is visible.
        |    __atomic_thread_fence(__ATOMIC_RELEASE);
        |    test_status->ack_seq            = accepted_seq;
        |}
        |
        |// Does the current position satisfy the active command's stop condition?
        |static bool at_stop_point(void) {
        |    if ((status_flags & TEST_FLAG_STOPPED) != 0) {
        |        return true;
        |    }
        |    switch (active_cmd) {
        |        case TEST_CMD_RUN_FOREVER:
        |            return false;
        |        case TEST_CMD_SSTEP:
        |        case TEST_CMD_HSTEP:
        |            return slots_remaining == 0;
        |        case TEST_CMD_RUN_TO_SLOT:
        |            return current_timeslice == target_slot;
        |        case TEST_CMD_RUN_TO_HP:
        |            return hyperperiod_num >= target_hp;
        |        case TEST_CMD_RUN_TO_STATE:
        |            return hyperperiod_num == target_hp && current_timeslice == target_slot;
        |        case TEST_CMD_RUN_TO_THREAD:
        |            return user_schedule.timeslice_ch[current_timeslice] == target_ch;
        |        default:
        |            // NONE, and the Info commands, complete without dispatching anything.
        |            return true;
        |    }
        |}
        |
        |// The slots a run-to that ends in hyperperiod `t_hp` may take, plus one; saturated, as a
        |// far target's product would wrap to a small budget and end the command UNREACHABLE.
        |static uint32_t runto_budget_to(uint32_t t_hp, uint32_t n_slots) {
        |    uint64_t b = ((uint64_t) (t_hp - hyperperiod_num) + 1) * n_slots + 1;
        |    return b > UINT32_MAX ? UINT32_MAX : (uint32_t) b;
        |}
        |
        |static void accept_command(uint32_t seq) {
        |    uint32_t type    = test_cmd->type;
        |    uint32_t count   = test_cmd->count;
        |    uint32_t t_ch    = test_cmd->target_ch;
        |    uint32_t t_hp    = test_cmd->target_hp;
        |    uint32_t t_slot  = test_cmd->target_slot;
        |    uint32_t n_slots = user_schedule.num_timeslices;
        |
        |    accepted_seq = seq;
        |    cmd_observe = test_cmd->observe != 0;
        |    obs_pending = false;
        |    // STOPPED is sticky for the rest of the session; the rest are per-command.
        |    status_flags &= TEST_FLAG_STOPPED;
        |
        |    if ((status_flags & TEST_FLAG_STOPPED) != 0) {
        |        // The session has ended.  Acknowledge so the caller is not left waiting, but
        |        // dispatch nothing: a resumable stop would let the schedule restart after a
        |        // verdict had already been published.
        |        active_cmd = TEST_CMD_NONE;
        |        return;
        |    }
        |
        |    active_cmd = TEST_CMD_NONE;
        |
        |    switch (type) {
        |        case TEST_CMD_RUN_FOREVER:
        |            active_cmd = type;
        |            break;
        |
        |        case TEST_CMD_SSTEP:
        |            slots_remaining = count;
        |            active_cmd = type;
        |            break;
        |
        |        case TEST_CMD_HSTEP:
        |            // Finish the hyperperiod in progress, then count - 1 whole ones.
        |            if (count == 0) {
        |                slots_remaining = 0;
        |            } else {
        |                // saturated: a huge count would wrap to a small one and stop early
        |                uint64_t n = (uint64_t) (n_slots - current_timeslice) + (uint64_t) (count - 1) * n_slots;
        |                slots_remaining = n > UINT32_MAX ? UINT32_MAX : (uint32_t) n;
        |            }
        |            active_cmd = type;
        |            break;
        |
        |        case TEST_CMD_RUN_TO_SLOT:
        |            if (t_slot >= n_slots) {
        |                status_flags |= TEST_FLAG_BAD_COMMAND;
        |            } else {
        |                target_slot = t_slot;
        |                runto_budget = n_slots + 1;
        |                active_cmd = type;
        |            }
        |            break;
        |
        |        case TEST_CMD_RUN_TO_THREAD:
        |            // channel 0 is padding, which is never dispatched
        |            if (t_ch == 0 || !scheduled_channel(t_ch)) {
        |                status_flags |= TEST_FLAG_BAD_COMMAND;
        |            } else {
        |                target_ch = t_ch;
        |                runto_budget = n_slots + 1;
        |                active_cmd = type;
        |            }
        |            break;
        |
        |        case TEST_CMD_RUN_TO_HP:
        |            if (t_hp <= hyperperiod_num) {
        |                status_flags |= TEST_FLAG_BAD_COMMAND;
        |            } else {
        |                target_hp = t_hp;
        |                runto_budget = runto_budget_to(t_hp, n_slots);
        |                active_cmd = type;
        |            }
        |            break;
        |
        |        case TEST_CMD_RUN_TO_STATE:
        |            if (t_slot >= n_slots || t_hp < hyperperiod_num ||
        |                (t_hp == hyperperiod_num && t_slot < current_timeslice)) {
        |                status_flags |= TEST_FLAG_BAD_COMMAND;
        |            } else {
        |                target_hp = t_hp;
        |                target_slot = t_slot;
        |                runto_budget = runto_budget_to(t_hp, n_slots);
        |                active_cmd = type;
        |            }
        |            break;
        |
        |        case TEST_CMD_INFO_STATE:
        |        case TEST_CMD_INFO_SCHEDULE:
        |            // Status is published on every command completion and the schedule is
        |            // published once at init, so these need only be acknowledged.  They are
        |            // kept for parity with the JVM vocabulary and for the interactive CLI.
        |            break;
        |
        |        case TEST_CMD_STOP:
        |            status_flags |= TEST_FLAG_STOPPED;
        |            break;
        |
        |        default:
        |            status_flags |= TEST_FLAG_BAD_COMMAND;
        |            break;
        |    }
        |}
        |
        |// Returns whether a new command was accepted.
        |static bool poll_command(void) {
        |    uint32_t seq = test_cmd->seq;
        |    if (seq == accepted_seq) {
        |        return false;
        |    }
        |    // Pairs with the controller's release store of seq: everything it wrote before
        |    // publishing seq is visible here.
        |    __atomic_thread_fence(__ATOMIC_ACQUIRE);
        |    accept_command(seq);
        |    return true;
        |}
        |
        |// Move to the next slot and charge the active command for the one just finished.
        |static void advance_position(void) {
        |    if (slots_remaining > 0) {
        |        slots_remaining--;
        |    }
        |    if (runto_budget > 0) {
        |        runto_budget--;
        |    }
        |
        |    current_timeslice++;
        |    if (current_timeslice >= user_schedule.num_timeslices) {
        |        current_timeslice = 0;
        |        hyperperiod_num++;
        |    }
        |
        |    if (is_runto(active_cmd) && runto_budget == 0 && !at_stop_point()) {
        |        status_flags |= TEST_FLAG_UNREACHABLE;
        |        active_cmd = TEST_CMD_NONE;
        |        ended_unreachable = true;
        |    }
        |}
        |
        |static void try_advance(void) {
        |    bool accepted = poll_command();
        |
        |    // Padding slots are skipped in this loop rather than dispatched.  Iteratively,
        |    // not by recursing through on_slot_complete: a schedule can be mostly padding,
        |    // and this runs on a 4 KB protection domain stack.
        |    while (true) {
        |        if (active_cmd == TEST_CMD_NONE || at_stop_point()) {
        |            // Acknowledge only a command that is completing now: one just accepted,
        |            // one that was running, or one that just ended UNREACHABLE.  Every
        |            // controller dispatch ends by signalling this channel; answering a signal
        |            // that carried no command would notify the controller back, and the two
        |            // would signal each other forever -- after the suite's Stop, with nothing
        |            // left to do.
        |            bool completing = accepted || active_cmd != TEST_CMD_NONE || ended_unreachable;
        |            active_cmd = TEST_CMD_NONE;
        |            ended_unreachable = false;
        |            if (completing) {
        |                // A command that stops on the slot whose dispatch overran -- e.g. the
        |                // runner's run_to_slot(0) when slot 0 overran -- reports it again, as
        |                // one that would dispatch it does: that thread may still be running.
        |                if (overrun_pending && user_schedule.timeslice_ch[current_timeslice] == overrun_ch) {
        |                    status_flags |= TEST_FLAG_OVERRUN;
        |                }
        |                publish_status();
        |                microkit_notify(TEST_CONTROLLER_CH);
        |            }
        |            return; // parked: nothing dispatched, no watchdog armed
        |        }
        |
        |        microkit_channel ch = user_schedule.timeslice_ch[current_timeslice];
        |
        |        if (ch == 0) {
        |            // Channel 0 pads out a schedule.  Nothing runs, so there is nothing to
        |            // wait for and the slot completes immediately -- padding only means
        |            // elapsed time, and dispatch is no longer paced by the clock.  It is
        |            // still counted, so slot indices stay aligned with the published schedule.
        |            advance_position();
        |            continue;
        |        }
        |
        |        if (overrun_pending && ch == overrun_ch) {
        |            // Still running the dispatch that overran: it cannot be dispatched again,
        |            // and its late completion would be taken for this dispatch's.  The command
        |            // ends here, reporting the overrun again.
        |            status_flags |= TEST_FLAG_OVERRUN;
        |            active_cmd = TEST_CMD_NONE;
        |            publish_status();
        |            microkit_notify(TEST_CONTROLLER_CH);
        |            return;
        |        }
        |
        |        // Observation park: before the dispatch -- so what the controller saves as the
        |        // pre-state includes anything a test injected while parked -- and before the
        |        // watchdog is armed, so time spent checking is never charged to the thread.
        |        if (cmd_observe && user_schedule.is_user_partition[current_timeslice]) {
        |            if (!obs_pending) {
        |                obs_pending = true;
        |                obs_seq++;
        |                publish_position();
        |                // obs_seq is stored last, and only after everything it describes.
        |                __atomic_thread_fence(__ATOMIC_RELEASE);
        |                test_status->obs_seq = obs_seq;
        |                microkit_notify(TEST_CONTROLLER_CH);
        |                return; // parked: dispatched once the controller acknowledges
        |            }
        |            if (test_cmd->obs_ack != obs_seq) {
        |                return; // still parked
        |            }
        |            __atomic_thread_fence(__ATOMIC_ACQUIRE);
        |            obs_pending = false;
        |        }
        |
        |        last_dispatched_ch = ch;
        |        armed = true;
        |
        |        // Arm the watchdog before dispatching, so a thread that never reports back
        |        // cannot leave the scheduler waiting forever.
        |        uint64_t bound = user_schedule.timeslices[current_timeslice] * TEST_WATCHDOG_FACTOR;
        |        if (bound < TEST_WATCHDOG_MIN_NS) {
        |            bound = TEST_WATCHDOG_MIN_NS;
        |        }
        |        armed_deadline = sddf_timer_time_now(config.driver_id) + bound;
        |        sddf_timer_set_timeout(config.driver_id, bound);
        |
        |        microkit_notify(ch);
        |        return;
        |    }
        |}
        |
        |static void on_slot_complete(void) {
        |    armed = false;
        |    // User slots only: those are the dispatches the controller parks before, and so
        |    // the ones it accounts for.
        |    if (user_schedule.is_user_partition[current_timeslice]) {
        |        completed_seq++;
        |    }
        |    advance_position();
        |    try_advance();
        |}
        |
        |void notified(microkit_channel ch)
        |{
        |    if (ch == config.driver_id) {
        |        if (armed && sddf_timer_time_now(config.driver_id) >= armed_deadline) {
        |            // The slot in flight never reported completion.  Fail the command rather
        |            // than wait forever; the controller sees OVERRUN and the run fails.
        |            sddf_dprintf("TEST SCHEDULER | slot %u (channel %u) did not complete within its watchdog bound\n",
        |                         current_timeslice, last_dispatched_ch);
        |            armed = false;
        |            overrun_pending = true;
        |            overrun_ch = last_dispatched_ch;
        |            status_flags |= TEST_FLAG_OVERRUN;
        |            active_cmd = TEST_CMD_NONE;
        |            publish_status();
        |            microkit_notify(TEST_CONTROLLER_CH);
        |        }
        |        // Otherwise a stale expiry: signalled for a slot that has since completed --
        |        // nothing is armed, or the slot in flight's deadline is still ahead.
        |    } else if (ch == TEST_CONTROLLER_CH) {
        |        // A command arrived.  If the scheduler is parked this is what restarts it.
        |        // It may instead acknowledge an observation park, which try_advance tells
        |        // apart by obs_ack.  Ignored before the schedule is live: the controller
        |        // signals this same channel from its own init(), by way of its _MON, and that
        |        // is not a command.  Ignored while a slot is in flight too: nothing the
        |        // controller sends then can change what happens before that slot completes.
        |        if (scheduler_running && !armed) {
        |            try_advance();
        |        }
        |    } else if ((part_ready_check & (1ULL << ch)) != 0) {
        |        if ((part_ready & (1ULL << ch)) == 0) {
        |            // First notification from this partition: the initialisation handshake.
        |            sddf_dprintf("TEST SCHEDULER | Marking partition %d as ready\n", ch);
        |            part_ready |= (1ULL << ch);
        |            if (part_ready == part_ready_check) {
        |                sddf_dprintf("TEST SCHEDULER | All partitions ready, handing over to the test controller\n");
        |                // The schedule is live from here, but parked: nothing is dispatched
        |                // until the controller issues a command.  The default scheduler's
        |                // settling timeout is not needed, because the first dispatch cannot
        |                // happen until the controller has run and asked for one.
        |                scheduler_running = true;
        |                microkit_notify(TEST_CONTROLLER_CH);
        |            }
        |        }
        |        else if (overrun_pending && ch == overrun_ch) {
        |            // The late completion of the dispatch that overran: absorbed, and the
        |            // thread can be dispatched again.  It counts toward nothing -- that
        |            // dispatch was aborted, and completed_seq never included it.
        |            overrun_pending = false;
        |        }
        |        else if (scheduler_running && armed && ch == last_dispatched_ch) {
        |            // The dispatched thread has finished its slot.  This is the event that
        |            // paces the schedule; the clock no longer does.
        |            on_slot_complete();
        |        }
        |    } else {
        |        sddf_dprintf("TEST SCHEDULER | received unknown notification on channel: %d\n", ch);
        |    }
        |}
        |
        |void init(void)
        |{
        |    current_timeslice = 0;
        |    hyperperiod_num = 0;
        |    last_dispatched_ch = 0;
        |    scheduler_running = false;
        |
        |    accepted_seq = 0;
        |    status_flags = 0;
        |    slots_remaining = 0;
        |    runto_budget = 0;
        |    ended_unreachable = false;
        |    armed = false;
        |    armed_deadline = 0;
        |    cmd_observe = false;
        |    completed_seq = 0;
        |    obs_seq = 0;
        |    obs_pending = false;
        |    overrun_pending = false;
        |    overrun_ch = 0;
        |
        |    // Park until the controller issues a command.  RUN_FOREVER remains the behaviour
        |    // for a command that asks for it, and is what the interactive CLI will use for
        |    // "run freely until I interrupt".
        |    active_cmd = TEST_CMD_NONE;
        |
        |    part_ready |= (1ULL << 0); // ch 0 is always 'ready'
        |    part_ready_check |= (1ULL << 0); // ch 0 is always 'ready' -- keeps padding optional
        |
        |    // Build a bitmask of the channels that must report ready before the schedule can
        |    // start.  1ULL rather than 1: channel ids reach MICROKIT_MAX_CHANNELS - 1 = 61,
        |    // and shifting an int by more than 31 is undefined.
        |    for (uint32_t i = 0; i < user_schedule.num_timeslices; i++) {
        |        part_ready_check |= (1ULL << user_schedule.timeslice_ch[i]);
        |    }
        |
        |    // Publish the schedule so a controller can correlate slot indices with channels.
        |    test_schedule->num_timeslices = user_schedule.num_timeslices;
        |    for (uint32_t i = 0; i < user_schedule.num_timeslices; i++) {
        |        test_schedule->timeslices[i] = user_schedule.timeslices[i];
        |        test_schedule->timeslice_ch[i] = user_schedule.timeslice_ch[i];
        |        test_schedule->is_user_partition[i] = user_schedule.is_user_partition[i];
        |    }
        |
        |    publish_status();
        |}
        |"""
}
