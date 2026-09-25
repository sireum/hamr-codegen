// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.testing

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, HamrCli, ModelUtil, ResourceUtil}
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

  // The controller must be the lowest-priority protection domain in the system so that its
  // busy-wait on ack_seq cannot starve the threads whose execution is what changes ack_seq
  // (TestScheduler-design.md D4).  Component threads sit at 140 and their _MON wrappers at
  // 150; the controller's own _MON only forwards a notification and does not spin, so only
  // the controller thread itself has to be demoted.
  // Wall-clock bound for a QEMU run.  Generous by design: DONE is the real terminator, and
  // this only exists to bound the silent-failure case.  Timer-gated dispatch runs a
  // hyperperiod in roughly a frame period, so a suite of any size needs room.
  val qemuTimeoutSeconds: Z = 300

  val controllerPriority: Z = 100
  val controllerMonPriority: Z = 101

  /** Whether any thread declares GUMBO state variables.  Mirrors
    * GumboMonitorPlugin.hasThreadsWithStateVars, which is a trait method and so is not
    * reachable from here.  Used only for the D5 diagnostic: --runtime-monitoring is
    * required alongside ENABLE_TEST_SCHEDULER exactly when there are state vars to
    * publish, since that is what creates the sv_ ports and regions.
    */
  /** Every port-backed shared memory region in the system, paired with the type information
    * the generated accessors need.  Built from the connection store: a PortSharedMemoryRegion
    * is named after its outgoing port path, and processOutPort / processInPort key their
    * contributions by that same path, so the two join cleanly.
    */
  @pure def observableRegions(store: Store): ISZ[ObservableRegion] = {
    val cTypeProvider = CTypePlugin.getCTypeProvider(store).get
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
            isStateVar = ops.StringOps(
              p.outgoingPortPath(p.outgoingPortPath.lastIndex)).startsWith("sv_"))
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
    for (thread <- symbolTable.getThreads() if !StoreUtil.isSynthetic(thread.path, store)) {
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
                                 val isStateVar: B)

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

/** Emits the Microkit test-scheduler variant bundle.  See
  * hamr/codegen/doc/TestScheduler-design.md.
  *
  * This is stage 2 of that design: the bundle and a command-driven scheduler, but no test
  * controller protection domain.  With nothing to issue commands the scheduler starts in
  * run-forever mode, so the image is observationally equivalent to the default variant --
  * which is what makes the rewritten control flow verifiable before anything drives it.
  *
  * Implemented as a finalize plugin rather than a MicrokitPlugin: finalize runs after every
  * handle plugin has settled, so the "normal" system description this variant derives from
  * is already final (in particular, the monitor plugins have already stripped their own
  * protection domains from it).  A handle plugin would have to win a pass-ordering race
  * against those plugins instead.
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

        // Not Verus-verified (it is test code, and its assertions are exec-only), but
        // user-editable so the hand-written test script survives regeneration.  No
        // component-level test harness: the controller IS the test harness, and its tests
        // compile into the protection domain rather than running under cargo test.
        localStore = StoreUtil.putComponentGenProfile(threadPath,
          ComponentGenProfile(verusVerified = F, userEditable = T, emitTestHarness = F), localStore)

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
        !store.contains(TestSchedulerPlugin.KEY_contributed))
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                      symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + TestSchedulerPlugin.KEY_contributed ~> BoolValue(T)

    val sysPath = model.components(0).identifier.name
    val threadPath = TestSchedulerPlugin.controllerThreadPath(sysPath)

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
    val regions = TestSchedulerPlugin.observableRegions(localStore)
    localStore = localStore + TestSchedulerPlugin.KEY_observable ~> ObservableRegions(regions)

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
      i = i + 1
    }

    val injectables = TestSchedulerPlugin.injectableStateVars(symbolTable, types, localStore)
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
    var resources: ISZ[Resource] = ISZ()

    // D5: --runtime-monitoring is required alongside ENABLE_TEST_SCHEDULER only when the
    // model has GUMBO state variables.  It is not needed because the monitor runs -- the
    // monitor protection domains are stripped from this variant -- but because it is what
    // makes GumboMonitorPlugin create the sv_ ports, their memory regions, and the
    // is_monitoring_enabled plumbing that state var inspection reads.  A model with no
    // state vars gets none of that either way, so the flag buys nothing and is not demanded.
    if (TestSchedulerPlugin.hasThreadsWithStateVars(symbolTable) && !options.runtimeMonitoring) {
      reporter.error(None(), toolName,
        st"""${ExperimentalOptions.ENABLE_TEST_SCHEDULER} requires --runtime-monitoring for this model.
            |
            |The model declares GUMBO state variables, and --runtime-monitoring is what causes the
            |sv_ state variable ports, their shared memory regions, and is_monitoring_enabled() to be
            |generated.  The test scheduler needs those to inspect component state; without them the
            |threads never publish their state variables.  The runtime monitor itself is not included
            |in the test scheduler variant.""".render)
      return (localStore, ISZ())
    }

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
    val boundProcessors = symbolTable.getAllActualBoundProcessors()
    assert(boundProcessors.size == 1, "Linter should have ensured there is exactly one bound processor")
    var framePeriodNano: Z = 0
    boundProcessors(0).getFramePeriod() match {
      case Some(ms) => framePeriodNano = ms * 1_000_000
      case _ => halt("Infeasible: linter should have ensured bound processor has frame period")
    }

    val threadSlots: ISZ[SchedulingDomain] = base.schedulingDomains.filter((sd: SchedulingDomain) =>
      sd.componentName != "pad" && sd.componentName != "padding" &&
        sd.componentName != ctrlMonPd && !otherInjectedPds.contains(sd.componentName))

    var usedNano: Z = 0
    for (sd <- threadSlots) {
      usedNano = usedNano + sd.length
    }
    val remainder: Z = framePeriodNano - usedNano
    val scheds: ISZ[SchedulingDomain] =
      if (remainder > 0) SchedulingDomain(id = 0, componentName = "pad", length = remainder, isUserPartition = F) +: threadSlots
      else threadSlots

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
      templateTailContributions = ISZ(StaticContent.testSelection_py(ctrlPd)))

    localStore = SystemDescriptionProviderPlugin.putMSD(TestSchedulerPlugin.variantName, testSd, localStore)

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
      path = s"$stDir/mod.rs", content = StaticContent.systemTests_mod_rs, overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/selection.rs", content = StaticContent.systemTests_selection_rs, overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/api.rs",
      content = StaticContent.systemTests_api_rs(TestSchedulerPlugin.controllerName),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/inspect.rs",
      content = StaticContent.systemTests_inspect_rs(
        regions = observable, injectables = injectables, channels = observableChannels,
        preStates = TestSchedulerPlugin.threadPreStates(observable, injectables, symbolTable, localStore)),
      overwrite = T, isDatatype = F)

    resources = resources :+ ResourceUtil.createResourceH(
      path = s"$stDir/harness.rs",
      content = StaticContent.systemTests_harness_rs(TestSchedulerPlugin.controllerName),
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

  val systemTests_mod_rs: ST =
    st"""${CommentTemplate.doNotEditComment_slash}
        |
        |//! System test support for the test controller.
        |//!
        |//! `api` drives the test scheduler, `harness` runs the suite and records results,
        |//! and `tests` holds the hand-written test script (preserved across regeneration).
        |
        |pub mod api;
        |pub mod harness;
        |pub mod inspect;
        |pub mod selection;
        |pub mod tests;
        |
        |pub fn run_all() {
        |  harness::run_all();
        |}
        |"""

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
        |/// The filter as a string slice, empty when unset.
        |pub fn filter() -> &'static str {
        |  unsafe {
        |    let bytes = &*core::ptr::addr_of!(TEST_SELECTION.filter);
        |    let mut n = 0usize;
        |    while n < FILTER_LEN && bytes[n] != 0 {
        |      n += 1;
        |    }
        |    match core::str::from_utf8(&bytes[..n]) {
        |      Ok(s) => s,
        |      Err(_) => "",
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


  @pure def systemTests_api_rs(controllerName: String): ST = {
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
          |
          |fn issue(typ: u32, count: u32, target_ch: u32, target_hp: u32, target_slot: u32) -> TestStatus {
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
          |    let status = TEST_STATUS_VADDR as *const TestStatus;
          |    while read_volatile(addr_of!((*status).ack_seq)) != seq {}
          |    fence(Ordering::Acquire);
          |    read_volatile(status)
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


  @pure def systemTests_harness_rs(controllerName: String): ST = {
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
          |static mut SUITE_DONE: bool = false;
          |
          |/// Record a failed assertion.  Called by the sys_assert macros, which return from
          |/// the test body immediately afterwards.
          |pub fn fail(file: &str, line_no: u32, what: &str) {
          |  unsafe { CURRENT_FAILED = true; }
          |  line(format_args!("TEST | FAIL  {}:{} {}", file, line_no, what));
          |}
          |
          |pub fn run_all() {
          |  // The scheduler notifies this protection domain on every command completion, and
          |  // those notifications accumulate as a pending bit while the suite runs without
          |  // returning to the event loop.  Without this guard the pending signal re-enters
          |  // the entrypoint once the suite finishes and the whole suite runs again, forever.
          |  unsafe {
          |    if SUITE_DONE { return; }
          |    SUITE_DONE = true;
          |  }
          |
          |  let filter = selection::filter();
          |  let mut matched: u32 = 0;
          |  let mut passed: u32 = 0;
          |  let mut failed: u32 = 0;
          |
          |  for (name, body) in crate::system_tests::tests::SYSTEM_TESTS {
          |    if !selection::selects(filter, name) {
          |      continue;
          |    }
          |    matched += 1;
          |
          |    // Normalize the schedule position so a step means the same thing in every
          |    // test.  Component state is NOT reset: tests are order-independent and
          |    // establish their own preconditions.
          |    let _ = api::run_to_slot(0);
          |
          |    unsafe { CURRENT_FAILED = false; }
          |    line(format_args!("TEST | BEGIN {}", name));
          |    body();
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
          |  // matched is what catches a filter that selected nothing.
          |  line(format_args!("TEST | DONE  matched={} passed={} failed={}", matched, passed, failed));
          |
          |  let _ = api::stop();
          |}
          |
          |/// Declare the system tests.  Generates the bodies plus the registration table the
          |/// runner walks; suites qualify the registered names, which is what gives the
          |/// TESTS= filter its granularity.
          |#[macro_export]
          |macro_rules! system_tests {
          |  ( $$( suite $$suite:ident { $$( fn $$name:ident () $$body:block )* } )+ ) => {
          |    $$( $$( fn $$name() $$body )* )+
          |    pub static SYSTEM_TESTS: &[(&str, fn())] = &[
          |      $$( $$( (concat!(stringify!($$suite), "::", stringify!($$name)), $$name as fn()), )* )+
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
  @pure def testSelection_py(controllerPdName: String): ST = {
    return (
      st"""#######################################
          |# TEST SELECTION
          |# Which system tests to run, from the TESTS make variable.  Substring match
          |# against the qualified suite::test name; empty selects all of them.
          |#######################################
          |test_selection = bytearray(260)
          |_filter = tests_filter.encode()[:255]
          |test_selection[0:len(_filter)] = _filter
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
          |// Usage:  run-tests.cmd [<test filter>]
          |//
          |// The filter is a substring match against the qualified suite::test name, so
          |// "nominal::" selects a suite and "fan_turns" selects a single test.
          |
          |val microkitDir: Os.Path = Os.slashDir.up
          |val filter: String = if (Os.cliArgs.nonEmpty) Os.cliArgs(0) else ""
          |
          |// sddf_dprintf is compiled out unless CONFIG_DEBUG_BUILD is set, which would remove
          |// every TEST line and make a perfectly good run look like a failure.  Force it.
          |val commonArgs: ISZ[String] = ISZ(
          |  "make", "-C", microkitDir.string,
          |  "CONFIG=$v.mk",
          |  "MICROKIT_CONFIG=debug",
          |  "RUST_MAKE_TARGET=build-release",
          |  s"TESTS=$$filter")
          |
          |println(s"Building $$microkitDir (TESTS='$$filter') ...")
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
          |// write to raise SIGPIPE -- so the process group has to be killed explicitly.
          |val logFile: Os.Path = Os.temp()
          |
          |val driver: String =
          |  st${tq}set -u
          |      |"$$$$@" > "$$$$LOG" 2>&1 &
          |      |mpid=$$$$!
          |      |waited=0
          |      |while kill -0 $$$$mpid 2>/dev/null; do
          |      |  grep -q 'TEST | DONE' "$$$$LOG" && break
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
          |    if (l.contains("TEST | PASS")) {
          |      seenPass = seenPass + 1
          |    } else if (l.contains("TEST | FAIL")) {
          |      seenFail = seenFail + 1
          |    } else if (l.contains("TEST | DONE")) {
          |      sawDone = T
          |      for (tok <- ops.StringOps(line).split((c: C) => c == ' ')) {
          |        val t = ops.StringOps(tok)
          |        if (t.startsWith("matched=")) {
          |          matched = Z(t.substring(8, tok.size)).getOrElse(-1)
          |        } else if (t.startsWith("passed=")) {
          |          passed = Z(t.substring(7, tok.size)).getOrElse(-1)
          |        } else if (t.startsWith("failed=")) {
          |          failed = Z(t.substring(7, tok.size)).getOrElse(-1)
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
          |// A filter that selects nothing must not pass: that is how a typo turns a job green.
          |if (matched <= 0) {
          |  eprintln(s"FAILED: the filter '$$filter' matched no tests")
          |  Os.exit(1)
          |}
          |
          |// The counts and the per-test lines have to agree, or output was lost.
          |if (passed != seenPass || failed != seenFail) {
          |  eprintln(s"FAILED: DONE reports passed=$$passed failed=$$failed but $$seenPass PASS and $$seenFail FAIL lines were seen; output was lost")
          |  Os.exit(1)
          |}
          |
          |if (failed != 0) {
          |  eprintln(s"FAILED: $$failed of $$matched tests failed")
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
                                  preStates: ISZ[ThreadPreState]): ST = {
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
        wrappers = wrappers :+
          st"""/// Publish `value` to the port `${r.accessor}`, as its producer would.
              |pub fn put_${r.accessor}(value: ${r.rustTypeName}) {
              |  unsafe {
              |    let mut v = value;
              |    test_put_${r.accessor}(&mut v);
              |  }
              |}"""
      }
    }

    for (v <- injectables) {
      externs = externs :+
        st"fn test_put_${v.threadId}_sv_${v.varName}(value: *mut ${v.rustTypeName});"
      wrappers = wrappers :+
        st"""/// Set the GUMBO state variable `${v.varName}` on `${v.threadId}`.  The thread adopts
            |/// it at the start of its next dispatch, before computing; if nothing is set it keeps
            |/// the value it already had.
            |pub fn put_${v.threadId}_sv_${v.varName}(value: ${v.rustTypeName}) {
            |  unsafe {
            |    let mut v = value;
            |    test_put_${v.threadId}_sv_${v.varName}(&mut v);
            |  }
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

  val mk: ST =
    st"""${CommentTemplate.doNotEditComment_hash}
        |
        |# Test scheduler configuration.
        |# Usage: make CONFIG=$v.mk
        |export MSD := $$(TOP_DIR)/$v.meta.py
        |export SCHEDULER_C := $$(TOP_DIR)/scheduler/src/$v.scheduler.c
        |export SCHEDULER_CONFIG_HEADERS := $$(TOP_DIR)/scheduler/include/$v.user_config.h"""

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
        |} test_command_t;
        |
        |// Written by the scheduler, read by the controller.  ack_seq is stored last and
        |// echoes test_command.seq once the command has completed.
        |typedef struct test_status {
        |    uint32_t ack_seq;
        |    uint32_t current_timeslice;
        |    uint32_t hyperperiod_num;
        |    uint32_t last_dispatched_ch;
        |    uint32_t flags;
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
        |
        |// Generation of the slot currently in flight.  sddf_timer_set_timeout cannot be
        |// cancelled, so a timeout armed for a slot that has already ended may still fire;
        |// expiries whose generation does not match the slot in flight are stale and dropped.
        |// armed_generation == 0 means nothing is armed.
        |static uint32_t slot_generation;
        |static uint32_t armed_generation;
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
        |static void publish_status(void) {
        |    test_status->current_timeslice  = current_timeslice;
        |    test_status->hyperperiod_num    = hyperperiod_num;
        |    test_status->last_dispatched_ch = last_dispatched_ch;
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
        |static void accept_command(uint32_t seq) {
        |    uint32_t type    = test_cmd->type;
        |    uint32_t count   = test_cmd->count;
        |    uint32_t t_ch    = test_cmd->target_ch;
        |    uint32_t t_hp    = test_cmd->target_hp;
        |    uint32_t t_slot  = test_cmd->target_slot;
        |    uint32_t n_slots = user_schedule.num_timeslices;
        |
        |    accepted_seq = seq;
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
        |            slots_remaining = (count == 0) ? 0
        |                : (n_slots - current_timeslice) + (count - 1) * n_slots;
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
        |            if (!scheduled_channel(t_ch)) {
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
        |                runto_budget = (t_hp - hyperperiod_num + 1) * n_slots + 1;
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
        |                runto_budget = (t_hp - hyperperiod_num + 1) * n_slots + 1;
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
        |static void poll_command(void) {
        |    uint32_t seq = test_cmd->seq;
        |    if (seq == accepted_seq) {
        |        return;
        |    }
        |    // Pairs with the controller's release store of seq: everything it wrote before
        |    // publishing seq is visible here.
        |    __atomic_thread_fence(__ATOMIC_ACQUIRE);
        |    accept_command(seq);
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
        |    }
        |}
        |
        |static void try_advance(void) {
        |    poll_command();
        |
        |    // Padding slots are skipped in this loop rather than dispatched.  Iteratively,
        |    // not by recursing through on_slot_complete: a schedule can be mostly padding,
        |    // and this runs on a 4 KB protection domain stack.
        |    while (true) {
        |        if (active_cmd == TEST_CMD_NONE || at_stop_point()) {
        |            active_cmd = TEST_CMD_NONE;
        |            publish_status();
        |            microkit_notify(TEST_CONTROLLER_CH);
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
        |        slot_generation++;
        |        last_dispatched_ch = ch;
        |        armed_generation = slot_generation;
        |
        |        // Arm the watchdog before dispatching, so a thread that never reports back
        |        // cannot leave the scheduler waiting forever.
        |        uint64_t bound = user_schedule.timeslices[current_timeslice] * TEST_WATCHDOG_FACTOR;
        |        if (bound < TEST_WATCHDOG_MIN_NS) {
        |            bound = TEST_WATCHDOG_MIN_NS;
        |        }
        |        sddf_timer_set_timeout(config.driver_id, bound);
        |
        |        microkit_notify(ch);
        |        return;
        |    }
        |}
        |
        |static void on_slot_complete(void) {
        |    armed_generation = 0;
        |    advance_position();
        |    try_advance();
        |}
        |
        |void notified(microkit_channel ch)
        |{
        |    if (ch == config.driver_id) {
        |        if (armed_generation != 0 && armed_generation == slot_generation) {
        |            // The slot in flight never reported completion.  Fail the command rather
        |            // than wait forever; the controller sees OVERRUN and the run fails.
        |            sddf_dprintf("TEST SCHEDULER | slot %u (channel %u) did not complete within its watchdog bound\n",
        |                         current_timeslice, last_dispatched_ch);
        |            armed_generation = 0;
        |            status_flags |= TEST_FLAG_OVERRUN;
        |            active_cmd = TEST_CMD_NONE;
        |            publish_status();
        |            microkit_notify(TEST_CONTROLLER_CH);
        |        }
        |        // Otherwise a stale expiry: sddf_timer_set_timeout cannot be cancelled, so a
        |        // slot that completed normally leaves its bound armed to fire later.  The
        |        // generation check is what tells the two apart.
        |    } else if (ch == TEST_CONTROLLER_CH) {
        |        // A command arrived.  If the scheduler is parked this is what restarts it.
        |        // Ignored before the schedule is live: the controller signals this same
        |        // channel from its own init(), by way of its _MON, and that is not a command.
        |        if (scheduler_running) {
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
        |        else if (scheduler_running && armed_generation != 0 && ch == last_dispatched_ch) {
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
        |    slot_generation = 0;
        |    armed_generation = 0;
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
