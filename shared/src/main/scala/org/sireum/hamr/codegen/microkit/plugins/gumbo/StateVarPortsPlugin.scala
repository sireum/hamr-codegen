// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, HamrCli, ModelUtil}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiPlugin, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclStateVar}
import org.sireum.message.Reporter

// The thread side of GUMBO state-variable observation: each state variable's sv_ output port
// (thread, process and the delegation between them), is_monitoring_enabled(), the guarded
// put_sv_* methods, and the Rust hooks that publish the state variables after initialize and
// after each dispatch.  Its readers are the gumbo / sys-assert monitor PDs (--runtime-monitoring)
// and the test controller (ENABLE_TEST_SCHEDULER), so it runs for either (TestScheduler-design.md
// D22); the monitor plugins only wire their own PD to these ports.  With no monitor attached the
// sv_ ports are unconnected outputs, and CConnectionProviderPlugin gives them their regions.
// Both readers need the MCS user-land scheduler, so a domain-scheduled model gets none of this.
object StateVarPortsPlugin {

  val KEY_ModelTransformed: String = "KEY_StateVarPortsPlugin_Model_Transformed"

  val KEY_CBackend: String = "KEY_StateVarPortsPlugin_CBackend"

  @pure def isRequested(options: HamrCli.CodegenOption, symbolTable: SymbolTable): B = {
    return (
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        (options.runtimeMonitoring || ExperimentalOptions.enableTestScheduler(options.experimentalOptions)) &&
        MicrokitUtil.isMCS(options, symbolTable.rootSystem) &&
        hasThreadsWithStateVars(symbolTable))
  }

  @pure def hasThreadsWithStateVars(symbolTable: SymbolTable): B = {
    for (thread <- symbolTable.getThreads()) {
      if (ContractObserverPlugin.stateVarsOf(thread.path, symbolTable).nonEmpty) {
        return T
      }
    }
    return F
  }

  // The thread's sv_ output port, the matching port on its process, and the delegation
  // between them.  Shared with GumboMonitorPlugin.wireMonitorStateVars, which creates them
  // itself when it runs first, so both produce the same ports.
  @pure def sourcePortElements(threadPath: IdPath, processPath: IdPath, sv: GclStateVar): (ir.Feature, ir.Feature, ir.Connection) = {
    val svPortName = GumboMonitorPlugin.stateVarPortName(sv.name)
    val classifier = Some(ir.Classifier(sv.classifier))
    val threadPortPath: ISZ[String] = threadPath :+ svPortName
    val processPortPath: ISZ[String] = processPath :+ svPortName

    // Output data port on source thread
    val threadFeature = ir.FeatureEnd(
      identifier = ir.Name(name = threadPortPath, pos = None()),
      direction = ir.Direction.Out,
      category = ir.FeatureCategory.DataPort,
      classifier = classifier,
      properties = ISZ(),
      uriFrag = "")

    // Output data port on source process
    val processFeature = ir.FeatureEnd(
      identifier = ir.Name(name = processPortPath, pos = None()),
      direction = ir.Direction.Out,
      category = ir.FeatureCategory.DataPort,
      classifier = classifier,
      properties = ISZ(),
      uriFrag = "")

    // Delegation: srcThread.svPort → srcProcess.svPort (Out-to-Out going up)
    val delegation = ir.Connection(
      name = ir.Name(name = processPath :+ s"deleg_${svPortName}", pos = None()),
      src = ISZ(ir.EndPoint(
        component = ir.Name(name = threadPath, pos = None()),
        feature = Some(ir.Name(name = threadPortPath, pos = None())),
        direction = Some(ir.Direction.Out))),
      dst = ISZ(ir.EndPoint(
        component = ir.Name(name = processPath, pos = None()),
        feature = Some(ir.Name(name = processPortPath, pos = None())),
        direction = Some(ir.Direction.Out))),
      kind = ir.ConnectionKind.Port,
      isBiDirectional = F,
      connectionInstances = ISZ(),
      properties = ISZ(),
      uriFrag = "")

    return (threadFeature, processFeature, delegation)
  }

  @pure def updateThreadInModel(subComponents: ISZ[ir.Component],
                                processPath: ISZ[String],
                                threadPath: ISZ[String],
                                additionalThreadFeatures: ISZ[ir.Feature],
                                additionalProcessFeatures: ISZ[ir.Feature],
                                additionalProcessConnections: ISZ[ir.Connection]): ISZ[ir.Component] = {
    var result: ISZ[ir.Component] = ISZ()
    for (comp <- subComponents) {
      if (comp.identifier.name == processPath) {
        var updatedSubs: ISZ[ir.Component] = ISZ()
        for (sub <- comp.subComponents) {
          if (sub.identifier.name == threadPath) {
            updatedSubs = updatedSubs :+ sub(features = sub.features ++ additionalThreadFeatures)
          } else {
            updatedSubs = updatedSubs :+ sub
          }
        }
        result = result :+ comp(
          features = comp.features ++ additionalProcessFeatures,
          subComponents = updatedSubs,
          connections = comp.connections ++ additionalProcessConnections)
      } else if (comp.subComponents.nonEmpty) {
        result = result :+ comp(subComponents = updateThreadInModel(
          subComponents = comp.subComponents,
          processPath = processPath,
          threadPath = threadPath,
          additionalThreadFeatures = additionalThreadFeatures,
          additionalProcessFeatures = additionalProcessFeatures,
          additionalProcessConnections = additionalProcessConnections))
      } else {
        result = result :+ comp
      }
    }
    return result
  }
}

@sig trait StateVarPortsPlugin extends ModelTransformerPlugin with MicrokitPlugin {

  @pure override def canHandleModelTransform(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                             symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !isDisabled(store) &&
        !reporter.hasError &&
        !store.contains(StateVarPortsPlugin.KEY_ModelTransformed) &&
        StateVarPortsPlugin.isRequested(options, symbolTable))
  }

  // Adds each state variable's sv_ output port to its thread and process, skipping any a
  // monitor plugin already created.
  override def handleModelTransform(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                    symbolTable: SymbolTable, store: Store, reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore: Store = store + StateVarPortsPlugin.KEY_ModelTransformed ~> BoolValue(T)

    val system = model.components(0)
    var updatedSubComponents: ISZ[ir.Component] = system.subComponents
    var changed = F

    for (thread <- symbolTable.getThreads()) {
      val stateVars = ContractObserverPlugin.stateVarsOf(thread.path, symbolTable)
      if (stateVars.nonEmpty) {
        val srcProcess = thread.getParent(symbolTable)
        var existing: Set[String] = Set.empty
        for (f <- thread.component.features) {
          existing = existing + CommonUtil.getLastName(f.identifier)
        }

        var threadFeatures: ISZ[ir.Feature] = ISZ()
        var processFeatures: ISZ[ir.Feature] = ISZ()
        var processConnections: ISZ[ir.Connection] = ISZ()
        for (sv <- stateVars) {
          if (!existing.contains(GumboMonitorPlugin.stateVarPortName(sv.name))) {
            val (tf, pf, deleg) = StateVarPortsPlugin.sourcePortElements(thread.path, srcProcess.path, sv)
            localStore = StoreUtil.addSyntheticElement(tf.identifier.name, localStore)
            threadFeatures = threadFeatures :+ tf
            processFeatures = processFeatures :+ pf
            processConnections = processConnections :+ deleg
          }
        }

        if (threadFeatures.nonEmpty) {
          changed = T
          updatedSubComponents = StateVarPortsPlugin.updateThreadInModel(
            subComponents = updatedSubComponents,
            processPath = srcProcess.path,
            threadPath = thread.path,
            additionalThreadFeatures = threadFeatures,
            additionalProcessFeatures = processFeatures,
            additionalProcessConnections = processConnections)
        }
      }
    }

    if (!changed) {
      return Some((localStore, model, types, symbolTable))
    }

    val updatedModel = model(components = ISZ(system(subComponents = updatedSubComponents)))
    if (!reporter.hasError) {
      val reResult = ModelUtil.resolve(updatedModel, updatedModel.components(0).identifier.pos, "", options, localStore, reporter)
      localStore = reResult._2
      if (reResult._1.nonEmpty) {
        return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
      }
    }
    return None()
  }

  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !isDisabled(store) &&
        !reporter.hasError &&
        store.contains(StateVarPortsPlugin.KEY_ModelTransformed) &&
        !store.contains(StateVarPortsPlugin.KEY_CBackend) &&
        CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
        CTypePlugin.getCTypeProvider(store).nonEmpty)
  }

  // Post-processes the C connection layer. For each thread with GUMBO state variables, wraps
  // the state variable put methods with an is_monitoring_enabled() guard so values are only
  // published when an sv_ region is mapped, and adds is_monitoring_enabled() itself (true when
  // all the thread's sv_ regions are mapped). On the Rust side, registers the
  // is_monitoring_enabled extern C API and its unsafe wrapper, and contributes the publishing
  // of the state variables to each monitored component's crate root.
  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                            symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store + StateVarPortsPlugin.KEY_CBackend ~> BoolValue(T)

    val cTypeProvider = CTypePlugin.getCTypeProvider(localStore).get
    val existingConnectionStore = CConnectionProviderPlugin.getCConnectionStore(localStore)

    var threadSvPorts: Map[ISZ[String], ISZ[GclStateVar]] = Map.empty
    for (thread <- symbolTable.getThreads()) {
      val stateVars = ContractObserverPlugin.stateVarsOf(thread.path, symbolTable)
      if (stateVars.nonEmpty) {
        threadSvPorts = threadSvPorts + thread.path ~> stateVars
      }
    }

    // Post-process: wrap sv_ put methods with if (is_monitoring_enabled()) guard
    var updatedConnectionStore: ISZ[ConnectionStore] = ISZ()
    for (entry <- existingConnectionStore) {
      val senderPath = entry.senderName
      entry.codeContributions.get(senderPath) match {
        case Some(senderCC) =>
          val pn = senderCC.portName
          if (pn.nonEmpty &&
            ops.StringOps(pn(pn.size - 1)).startsWith(GumboMonitorPlugin.stateVarPortPrefix) &&
            threadSvPorts.contains(senderPath)) {

            val portIdentifier = pn(pn.size - 1)
            val cTypeName = cTypeProvider.getTypeNameProvider(senderCC.aadlType).mangledName
            val queueSize: Z = 1

            val queueTypeName = QueueTemplate.getTypeQueueTypeName(cTypeName, queueSize)
            val enqueueName = QueueTemplate.getQueueEnqueueMethodName(cTypeName, queueSize)
            val sharedVarName = QueueTemplate.getClientEnqueueSharedVarName(portIdentifier, queueSize)
            val methodSig = QueueTemplate.getClientPut_C_MethodSig(portIdentifier, cTypeName, F)

            val guardedMethod: ST =
              st"""$methodSig {
                  |  if (is_monitoring_enabled()) {
                  |    $enqueueName(($queueTypeName *) $sharedVarName, ($cTypeName *) data);
                  |  }
                  |
                  |  return true;
                  |}"""

            val oldCContribs = senderCC.cContributions.asInstanceOf[cConnectionContributions]
            val newCContribs = oldCContribs(cBridge_PortApiMethods = ISZ(guardedMethod))
            val newSenderCC = senderCC(cContributions = newCContribs)
            val newCodeContribs = entry.codeContributions + senderPath ~> newSenderCC

            updatedConnectionStore = updatedConnectionStore :+
              entry.asInstanceOf[DefaultConnectionStore](codeContributions = newCodeContribs)
          } else {
            updatedConnectionStore = updatedConnectionStore :+ entry
          }
        case _ =>
          updatedConnectionStore = updatedConnectionStore :+ entry
      }
    }

    // Add is_monitoring_enabled() for each thread with state vars
    var additionalEntries: ISZ[ConnectionStore] = ISZ()
    for (threadEntry <- threadSvPorts.entries) {
      val threadPath = threadEntry._1
      val stateVars = threadEntry._2

      val svChecks: ISZ[ST] = for (sv <- stateVars) yield
        st"${GumboMonitorPlugin.stateVarPortName(sv.name)}_queue_1 != NULL"

      val headerSig: ST = st"bool is_monitoring_enabled(void)"
      val impl: ST =
        st"""bool is_monitoring_enabled(void) {
            |  return ${(svChecks, " && ")};
            |}"""

      val cContribs = cConnectionContributions(
        cPortApiMethodSigs = ISZ(headerSig),
        cBridge_EntrypointMethodSignatures = ISZ(),
        cBridge_GlobalVarContributions = ISZ(),
        cBridge_PortApiMethods = ISZ(impl),
        cBridge_InitContributions = ISZ(),
        cBridge_ComputeContributions = ISZ(),
        cUser_MethodDefaultImpls = ISZ())

      additionalEntries = additionalEntries :+
        DefaultConnectionStore(
          systemContributions = DefaultSystemContributions(
            sharedMemoryRegionContributions = ISZ(),
            channelContributions = ISZ()),
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
    }

    localStore = CConnectionProviderPlugin.putCConnectionStore(
      updatedConnectionStore ++ additionalEntries, localStore)

    // Rust backend: add is_monitoring_enabled to extern_c_api.rs and record which
    // threads have state vars so finalizeMicrokit can generate monitoring lib.rs
    val crustApiContribsOpt = CRustApiPlugin.getCRustApiContributions(localStore)

    if (crustApiContribsOpt.nonEmpty) {
      var crustApiContribs = crustApiContribsOpt.get
      var monitoringEntries: HashSMap[ISZ[String], ISZ[RustMonitoringStateVarInfo]] = HashSMap.empty

      for (threadEntry <- threadSvPorts.entries) {
        val threadPath = threadEntry._1
        val stateVars = threadEntry._2
        val thread = symbolTable.componentMap.get(threadPath).get.asInstanceOf[AadlThread]

        if (MicrokitUtil.isRusty(thread)) {
          crustApiContribs.apiContributions.get(threadPath) match {
            case Some(existing) =>
              val monExternCApis: ISZ[RAST.Item] = ISZ(
                RAST.FnSig(
                  verusHeader = None(), fnHeader = RAST.FnHeader(F),
                  ident = RAST.IdentString("is_monitoring_enabled"),
                  generics = None(),
                  fnDecl = RAST.FnDecl(
                    inputs = ISZ(),
                    outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType))))

              val monWrappers: ISZ[RAST.Item] = ISZ(
                RAST.FnImpl(
                  visibility = RAST.Visibility.Public,
                  sig = RAST.FnSig(
                    ident = RAST.IdentString("unsafe_is_monitoring_enabled"),
                    fnDecl = RAST.FnDecl(
                      inputs = ISZ(),
                      outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                    verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
                  comments = ISZ(), attributes = ISZ(), meta = ISZ(),
                  verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
                  body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                    st"""unsafe {
                        |  return is_monitoring_enabled();
                        |}"""))))))

              val monTestMockVars: ISZ[RAST.Item] = ISZ(
                RAST.ItemStatic(
                  ident = RAST.IdentString("MONITORING_ENABLED"),
                  visibility = RAST.Visibility.Public,
                  ty = RAST.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), ISZ("bool")), None()),
                  mutability = RAST.Mutability.Not,
                  expr = RAST.ExprST(st"Mutex::new(None);")))

              val monTestingApis: ISZ[RAST.Item] = ISZ(
                RAST.FnImpl(
                  attributes = ISZ(RAST.AttributeST(F, st"cfg(test)")),
                  sig = RAST.FnSig(
                    ident = RAST.IdentString("is_monitoring_enabled"),
                    fnDecl = RAST.FnDecl(
                      inputs = ISZ(),
                      outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                    verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
                  comments = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
                  verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
                  body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                    st"""unsafe {
                        |  match *MONITORING_ENABLED.lock().unwrap_or_else(|e| e.into_inner()) {
                        |    Some(v) => return v,
                        |    None => return false,
                        |  }
                        |}"""))))))

              val svInfos: ISZ[RustMonitoringStateVarInfo] = for (sv <- stateVars) yield
                RustMonitoringStateVarInfo(name = sv.name)

              val combined = existing.combine(ComponentApiContributions.empty(
                externCApis = monExternCApis,
                unsafeExternCApiWrappers = monWrappers,
                externApiTestMockVariables = monTestMockVars,
                externApiTestingApis = monTestingApis))

              crustApiContribs = crustApiContribs.addApiContributions(threadPath, combined)
              monitoringEntries = monitoringEntries + threadPath ~> svInfos

            case _ =>
          }
        }
      }

      localStore = CRustApiPlugin.putCRustApiContributions(crustApiContribs, localStore)
      if (monitoringEntries.nonEmpty) {
        localStore = localStore + GumboMonitorPlugin.KEY_RUST_MONITORING ~> RustMonitoringStore(monitoringEntries)

        // Contribute the monitoring observation points into each monitored component's
        // crate root.  This used to be done by re-emitting crates/<component>/src/lib.rs
        // wholesale from finalizeMicrokit, which meant maintaining a second near-copy of
        // CRustComponentPlugin's ~80-line template and winning by running last.  The
        // guarded blocks are contributed whole, so CRustComponentPlugin needs to know
        // nothing about monitoring.
        if (CRustComponentPlugin.hasCRustComponentContributions(localStore)) {
          val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
          var updated = contributions.componentContributions
          for (entry <- monitoringEntries.entries) {
            val threadPath = entry._1
            val svInfos = entry._2
            updated.get(threadPath) match {
              case Some(contrib) =>
                val puts: ISZ[ST] = for (sv <- svInfos) yield
                  st"extern_c_api::unsafe_put_${GumboMonitorPlugin.stateVarPortName(sv.name)}(&_app.${sv.name});"
                updated = updated + threadPath ~> contrib(
                  libUses = contrib.libUses :+ RAST.ItemST(st"use crate::bridge::extern_c_api;"),
                  libModuleLevelEntries = contrib.libModuleLevelEntries :+
                    RAST.ItemST(st"static mut monitoring_enabled: bool = false;"),
                  libInitializePre = contrib.libInitializePre :+
                    RAST.BodyItemST(st"monitoring_enabled = extern_c_api::unsafe_is_monitoring_enabled();"),
                  libInitializePost = contrib.libInitializePost :+
                    RAST.BodyItemST(
                      st"""if monitoring_enabled {
                          |  ${(puts, "\n")}
                          |}"""),
                  libComputePost = contrib.libComputePost :+
                    RAST.BodyItemST(
                      st"""if monitoring_enabled {
                          |  ${(puts, "\n")}
                          |}"""))
              case _ =>
            }
          }
          localStore = CRustComponentPlugin.putComponentContributions(
            contributions.replaceComponentContributions(updated), localStore)
        }
      }
    }

    return (localStore, ISZ())
  }
}

@datatype class DefaultStateVarPortsPlugin extends StateVarPortsPlugin {

  val name: String = "DefaultStateVarPortsPlugin"
}
