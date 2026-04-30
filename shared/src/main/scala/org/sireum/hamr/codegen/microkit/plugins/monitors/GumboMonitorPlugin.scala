// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.ModelTransformerPlugin
import org.sireum.hamr.codegen.common.symbols.{AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiPlugin, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclStateVar}
import org.sireum.message.Reporter

object GumboMonitorPlugin {

  val KEY_GumboMonitorPlugin: String = "KEY_GumboMonitorPlugin"

  val KEY_GumboMonitorPlugin_CBackend: String = "KEY_GumboMonitorPlugin_CBackend"

  val stateVarPortPrefix: String = "sv_"

  @strictpure def haveHandledModelTransform(store: Store): B =
    store.contains(KEY_GumboMonitorPlugin)

  @strictpure def haveHandledCBackend(store: Store): B =
    store.contains(KEY_GumboMonitorPlugin_CBackend)

  @strictpure def stateVarPortName(stateVarName: String): String =
    s"${stateVarPortPrefix}${stateVarName}"

  @strictpure def monitorStateVarPortName(srcProcessId: String, srcThreadId: String, stateVarName: String): String =
    s"${srcProcessId}_${srcThreadId}_${stateVarPortPrefix}${stateVarName}"

  val KEY_RUST_MONITORING: String = "KEY_RUST_MONITORING"

  val KEY_GumboMonitorPlugin_RustFinalized: String = "KEY_GumboMonitorPlugin_RustFinalized"

  @strictpure def getRustMonitoringStore(store: Store): Option[RustMonitoringStore] =
    store.get(KEY_RUST_MONITORING).asInstanceOf[Option[RustMonitoringStore]]

  @strictpure def haveRustFinalized(store: Store): B =
    store.contains(KEY_GumboMonitorPlugin_RustFinalized)
}

@datatype class RustMonitoringStateVarInfo(val name: String)

@datatype class RustMonitoringStore(val entries: HashSMap[ISZ[String], ISZ[RustMonitoringStateVarInfo]]) extends StoreValue

@sig trait GumboMonitorPlugin
  extends ModelTransformerPlugin with MicrokitPlugin with MicrokitFinalizePlugin {

  @pure override def canHandleModelTransform(model: Aadl,
                                             options: HamrCli.CodegenOption,
                                             types: AadlTypes,
                                             symbolTable: SymbolTable,
                                             store: Store,
                                             reporter: Reporter): B = {
    return options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !GumboMonitorPlugin.haveHandledModelTransform(store) &&
      !reporter.hasError &&
      options.runtimeMonitoring &&
      RustScheduleAwareMonitorPlugin.haveHandledModelTransform(store) &&
      hasThreadsWithStateVars(symbolTable)
  }

  override def handleModelTransform(model: Aadl,
                                    options: HamrCli.CodegenOption,
                                    types: AadlTypes,
                                    symbolTable: SymbolTable,
                                    store: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore: Store = store + GumboMonitorPlugin.KEY_GumboMonitorPlugin ~> BoolValue(T)

    val system = model.components(0)
    val systemPath: ISZ[String] = system.identifier.name

    val monitorProcessPath: ISZ[String] = systemPath :+ MonitorInjector.monitorProcessId
    val monitorThreadPath: ISZ[String] = monitorProcessPath :+ MonitorInjector.monitorThreadId

    var updatedSubComponents: ISZ[ir.Component] = system.subComponents
    var additionalMonitorThreadFeatures: ISZ[ir.Feature] = ISZ()
    var additionalMonitorProcessFeatures: ISZ[ir.Feature] = ISZ()
    var additionalMonitorProcessConnections: ISZ[ir.Connection] = ISZ()
    var systemFanOutConnections: ISZ[ir.Connection] = ISZ()
    var systemConnectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

    for (thread <- symbolTable.getThreads()) {
      val stateVars = getStateVars(thread.path, symbolTable)
      if (stateVars.nonEmpty) {
        val srcProcess = thread.getParent(symbolTable)

        var additionalThreadFeatures: ISZ[ir.Feature] = ISZ()
        var additionalProcessFeatures: ISZ[ir.Feature] = ISZ()
        var additionalProcessConnections: ISZ[ir.Connection] = ISZ()

        for (sv <- stateVars) {
          val svPortName = GumboMonitorPlugin.stateVarPortName(sv.name)
          val monPortName = GumboMonitorPlugin.monitorStateVarPortName(srcProcess.identifier, thread.identifier, sv.name)
          val classifier = Some(ir.Classifier(sv.classifier))

          // Output data port on source thread
          val threadPortPath: ISZ[String] = thread.path :+ svPortName
          localStore = StoreUtil.addNonModelElement(threadPortPath, localStore)
          additionalThreadFeatures = additionalThreadFeatures :+ ir.FeatureEnd(
            identifier = ir.Name(name = threadPortPath, pos = None()),
            direction = ir.Direction.Out,
            category = ir.FeatureCategory.DataPort,
            classifier = classifier,
            properties = ISZ(),
            uriFrag = "")

          // Output data port on source process
          val processPortPath: ISZ[String] = srcProcess.path :+ svPortName
          additionalProcessFeatures = additionalProcessFeatures :+ ir.FeatureEnd(
            identifier = ir.Name(name = processPortPath, pos = None()),
            direction = ir.Direction.Out,
            category = ir.FeatureCategory.DataPort,
            classifier = classifier,
            properties = ISZ(),
            uriFrag = "")

          // Delegation: srcThread.svPort → srcProcess.svPort (Out-to-Out going up)
          val srcDelegConnName: ISZ[String] = srcProcess.path :+ s"deleg_${svPortName}"
          additionalProcessConnections = additionalProcessConnections :+
            ir.Connection(
              name = ir.Name(name = srcDelegConnName, pos = None()),
              src = ISZ(ir.EndPoint(
                component = ir.Name(name = thread.path, pos = None()),
                feature = Some(ir.Name(name = threadPortPath, pos = None())),
                direction = Some(ir.Direction.Out))),
              dst = ISZ(ir.EndPoint(
                component = ir.Name(name = srcProcess.path, pos = None()),
                feature = Some(ir.Name(name = processPortPath, pos = None())),
                direction = Some(ir.Direction.Out))),
              kind = ir.ConnectionKind.Port,
              isBiDirectional = F,
              connectionInstances = ISZ(),
              properties = ISZ(),
              uriFrag = "")

          // Input data port on monitor thread
          val monitorThreadPortPath: ISZ[String] = monitorThreadPath :+ monPortName
          additionalMonitorThreadFeatures = additionalMonitorThreadFeatures :+ ir.FeatureEnd(
            identifier = ir.Name(name = monitorThreadPortPath, pos = None()),
            direction = ir.Direction.In,
            category = ir.FeatureCategory.DataPort,
            classifier = classifier,
            properties = ISZ(),
            uriFrag = "")

          // Input data port on monitor process
          val monitorProcessPortPath: ISZ[String] = monitorProcessPath :+ monPortName
          additionalMonitorProcessFeatures = additionalMonitorProcessFeatures :+ ir.FeatureEnd(
            identifier = ir.Name(name = monitorProcessPortPath, pos = None()),
            direction = ir.Direction.In,
            category = ir.FeatureCategory.DataPort,
            classifier = classifier,
            properties = ISZ(),
            uriFrag = "")

          // Delegation: monitorProcess.port → monitorThread.port (In-to-In going down)
          val monitorDelegConnName: ISZ[String] = monitorProcessPath :+ s"deleg_${monPortName}"
          additionalMonitorProcessConnections = additionalMonitorProcessConnections :+
            ir.Connection(
              name = ir.Name(name = monitorDelegConnName, pos = None()),
              src = ISZ(ir.EndPoint(
                component = ir.Name(name = monitorProcessPath, pos = None()),
                feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                direction = Some(ir.Direction.In))),
              dst = ISZ(ir.EndPoint(
                component = ir.Name(name = monitorThreadPath, pos = None()),
                feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                direction = Some(ir.Direction.In))),
              kind = ir.ConnectionKind.Port,
              isBiDirectional = F,
              connectionInstances = ISZ(),
              properties = ISZ(),
              uriFrag = "")

          // System-level fan-out: srcProcess.svPort → monitorProcess.port
          val systemFanOutConnName: ISZ[String] = systemPath :+ s"sv_mon_${monPortName}"
          systemFanOutConnections = systemFanOutConnections :+
            ir.Connection(
              name = ir.Name(name = systemFanOutConnName, pos = None()),
              src = ISZ(ir.EndPoint(
                component = ir.Name(name = srcProcess.path, pos = None()),
                feature = Some(ir.Name(name = processPortPath, pos = None())),
                direction = Some(ir.Direction.Out))),
              dst = ISZ(ir.EndPoint(
                component = ir.Name(name = monitorProcessPath, pos = None()),
                feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                direction = Some(ir.Direction.In))),
              kind = ir.ConnectionKind.Port,
              isBiDirectional = F,
              connectionInstances = ISZ(),
              properties = ISZ(),
              uriFrag = "")

          // ConnectionInstance: srcThread.svPort → monitorThread.port
          val connInstNameStr: String =
            st"${(threadPortPath, ".")} -> ${(monitorThreadPortPath, ".")}".render
          systemConnectionInstances = systemConnectionInstances :+
            ir.ConnectionInstance(
              name = ir.Name(name = ISZ(connInstNameStr), pos = None()),
              src = ir.EndPoint(
                component = ir.Name(name = thread.path, pos = None()),
                feature = Some(ir.Name(name = threadPortPath, pos = None())),
                direction = Some(ir.Direction.Out)),
              dst = ir.EndPoint(
                component = ir.Name(name = monitorThreadPath, pos = None()),
                feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                direction = Some(ir.Direction.In)),
              kind = ir.ConnectionKind.Port,
              connectionRefs = ISZ(
                ir.ConnectionReference(
                  name = ir.Name(name = systemFanOutConnName, pos = None()),
                  context = ir.Name(name = systemPath, pos = None()),
                  isParent = T),
                ir.ConnectionReference(
                  name = ir.Name(name = monitorDelegConnName, pos = None()),
                  context = ir.Name(name = monitorProcessPath, pos = None()),
                  isParent = F)),
              properties = ISZ())
        }

        // Update the source thread and process with the new state var ports
        updatedSubComponents = updateThreadInModel(
          subComponents = updatedSubComponents,
          processPath = srcProcess.path,
          threadPath = thread.path,
          additionalThreadFeatures = additionalThreadFeatures,
          additionalProcessFeatures = additionalProcessFeatures,
          additionalProcessConnections = additionalProcessConnections)
      }
    }

    // Update the monitor process with the new state var ports
    updatedSubComponents = updateMonitorProcess(
      subComponents = updatedSubComponents,
      monitorProcessPath = monitorProcessPath,
      monitorThreadPath = monitorThreadPath,
      additionalMonitorThreadFeatures = additionalMonitorThreadFeatures,
      additionalMonitorProcessFeatures = additionalMonitorProcessFeatures,
      additionalMonitorProcessConnections = additionalMonitorProcessConnections)

    val updatedSystem = system(
      subComponents = updatedSubComponents,
      connections = system.connections ++ systemFanOutConnections,
      connectionInstances = system.connectionInstances ++ systemConnectionInstances)
    val updatedModel = model(components = ISZ(updatedSystem))

    if (!reporter.hasError) {
      val reResult = ModelUtil.resolve(updatedModel, updatedModel.components(0).identifier.pos, "", options, localStore, reporter)
      localStore = reResult._2
      if (reResult._1.nonEmpty) {
        return Some((localStore, reResult._1.get.model, reResult._1.get.types, reResult._1.get.symbolTable))
      }
    }
    return None()
  }

  // MicrokitPlugin phase: post-process ConnectionStore to add monitoring guards
  // to sv_ put methods and add is_monitoring_enabled()
  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !reporter.hasError &&
      options.runtimeMonitoring &&
      GumboMonitorPlugin.haveHandledModelTransform(store) &&
      CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
      CTypePlugin.getCTypeProvider(store).nonEmpty &&
      !GumboMonitorPlugin.haveHandledCBackend(store) &&
      hasThreadsWithStateVars(symbolTable)
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                             symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store + GumboMonitorPlugin.KEY_GumboMonitorPlugin_CBackend ~> BoolValue(T)

    val cTypeProvider = CTypePlugin.getCTypeProvider(localStore).get
    val existingConnectionStore = CConnectionProviderPlugin.getCConnectionStore(localStore)

    var threadSvPorts: Map[ISZ[String], ISZ[GclStateVar]] = Map.empty
    for (thread <- symbolTable.getThreads()) {
      val stateVars = getStateVars(thread.path, symbolTable)
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
                  comments = ISZ(), attributes = ISZ(), contract = None(), meta = ISZ(),
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
                  comments = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
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
      }
    }

    return (localStore, ISZ())
  }

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                          symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !isDisabled(store) &&
        GumboMonitorPlugin.getRustMonitoringStore(store).nonEmpty &&
        CRustComponentPlugin.hasCRustComponentContributions(store) &&
        store.contains(s"FINALIZED_DefaultCRustComponentPlugin") &&
        !GumboMonitorPlugin.haveRustFinalized(store))
  }

  // TODO: This replaces CRustComponentPlugin's lib.rs with a custom version that posts state vars
  //       to shared memory regions for runtime monitoring. lib.rs should be refactored to use a
  //       contribution-based approach (like the component app modules) so that plugins can modify
  //       it directly rather than regenerating the entire file.
  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                       symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var resources: ISZ[Resource] = ISZ()

    val monitoringStore = GumboMonitorPlugin.getRustMonitoringStore(store).get
    val componentContributions = CRustComponentPlugin.getCRustComponentContributions(store).componentContributions

    for (e <- componentContributions.entries) {
      monitoringStore.entries.get(e._1) match {
        case Some(svInfos) =>
          val thread = symbolTable.componentMap.get(e._1).get.asInstanceOf[AadlThread]
          val threadId = MicrokitUtil.getComponentIdPath(thread)
          val componentCrateDir = CRustComponentPlugin.componentCrateDirectory(thread, options)
          val componentSrcDir = s"$componentCrateDir/src"

          val initPuts: ISZ[ST] = for (sv <- svInfos) yield
            st"extern_c_api::unsafe_put_${GumboMonitorPlugin.stateVarPortName(sv.name)}(&_app.${sv.name});"

          val postPuts: ISZ[ST] = for (sv <- svInfos) yield
            st"extern_c_api::unsafe_put_${GumboMonitorPlugin.stateVarPortName(sv.name)}(&_app.${sv.name});"

          val entrypoints: ISZ[ST] =
            if (thread.isPeriodic())
              ISZ(
                st"""#[no_mangle]
                    |pub extern "C" fn ${threadId}_timeTriggered() {
                    |  unsafe {
                    |    if let Some(_app) = app.as_mut() {
                    |      _app.timeTriggered(&mut compute_api);
                    |      if monitoring_enabled {
                    |        ${(postPuts, "\n")}
                    |      }
                    |    } else {
                    |      panic!("Unexpected: app is None");
                    |    }
                    |  }
                    |}""")
            else ISZ(st"NOT YET")

          val content =
            st"""#![cfg_attr(not(test), no_std)]
                |
                |${RustUtil.defaultCrateLevelAttributes}
                |
                |${CommentTemplate.doNotEditComment_slash}
                |
                |mod bridge;
                |mod component;
                |mod logging;
                |
                |#[cfg(test)]
                |mod test;
                |
                |use crate::bridge::${CRustApiPlugin.apiModuleName(thread)}::{self as api, *};
                |use crate::bridge::extern_c_api;
                |use crate::component::${CRustComponentPlugin.appModuleName(thread)}::*;
                |use data::*;
                |
                |static mut app: Option<$threadId> = None;
                |static mut init_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.initializationApiType(thread)}> = api::init_api();
                |static mut compute_api: ${CRustApiPlugin.applicationApiType(thread)}<${CRustApiPlugin.computeApiType(thread)}> = api::compute_api();
                |static mut monitoring_enabled: bool = false;
                |
                |#[no_mangle]
                |pub extern "C" fn ${threadId}_initialize() {
                |  logging::init_logging();
                |
                |  unsafe {
                |    #[cfg(test)]
                |    crate::bridge::extern_c_api::initialize_test_globals();
                |
                |    monitoring_enabled = extern_c_api::unsafe_is_monitoring_enabled();
                |
                |    let mut _app = $threadId::new();
                |    _app.initialize(&mut init_api);
                |
                |    if monitoring_enabled {
                |      ${(initPuts, "\n")}
                |    }
                |
                |    app = Some(_app);
                |  }
                |}
                |
                |${(entrypoints, "\n\n")}
                |
                |#[no_mangle]
                |pub extern "C" fn ${threadId}_notify(channel: microkit_channel) {
                |  unsafe {
                |    if let Some(_app) = app.as_mut() {
                |      _app.notify(channel);
                |    } else {
                |      panic!("Unexpected: app is None");
                |    }
                |  }
                |}
                |
                |// Need a Panic handler in a no_std environment
                |#[panic_handler]
                |#[cfg(not(test))]
                |fn panic(info: &core::panic::PanicInfo) -> ! {
                |  log::error!("PANIC: {info:#?}");
                |  loop {}
                |}
                |"""
          resources = resources :+ ResourceUtil.createResource(s"$componentSrcDir/lib.rs", content, T)

        case _ =>
      }
    }

    return (store + GumboMonitorPlugin.KEY_GumboMonitorPlugin_RustFinalized ~> BoolValue(T), resources)
  }

  @pure def hasThreadsWithStateVars(symbolTable: SymbolTable): B = {
    for (thread <- symbolTable.getThreads()) {
      if (getStateVars(thread.path, symbolTable).nonEmpty) {
        return T
      }
    }
    return F
  }

  @pure def getStateVars(threadPath: ISZ[String], symbolTable: SymbolTable): ISZ[GclStateVar] = {
    symbolTable.annexClauseInfos.get(threadPath) match {
      case Some(clauses) =>
        for (clause <- clauses) {
          clause match {
            case gclInfo: GclAnnexClauseInfo =>
              return gclInfo.annex.state
            case _ =>
          }
        }
        return ISZ()
      case _ => return ISZ()
    }
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

  @pure def updateMonitorProcess(subComponents: ISZ[ir.Component],
                                 monitorProcessPath: ISZ[String],
                                 monitorThreadPath: ISZ[String],
                                 additionalMonitorThreadFeatures: ISZ[ir.Feature],
                                 additionalMonitorProcessFeatures: ISZ[ir.Feature],
                                 additionalMonitorProcessConnections: ISZ[ir.Connection]): ISZ[ir.Component] = {
    var result: ISZ[ir.Component] = ISZ()
    for (comp <- subComponents) {
      if (comp.identifier.name == monitorProcessPath) {
        var updatedSubs: ISZ[ir.Component] = ISZ()
        for (sub <- comp.subComponents) {
          if (sub.identifier.name == monitorThreadPath) {
            updatedSubs = updatedSubs :+ sub(features = sub.features ++ additionalMonitorThreadFeatures)
          } else {
            updatedSubs = updatedSubs :+ sub
          }
        }
        result = result :+ comp(
          features = comp.features ++ additionalMonitorProcessFeatures,
          subComponents = updatedSubs,
          connections = comp.connections ++ additionalMonitorProcessConnections)
      } else {
        result = result :+ comp
      }
    }
    return result
  }
}

@datatype class DefaultGumboMonitorPlugin extends GumboMonitorPlugin {

  val name: String = "DefaultGumboMonitorPlugin"
}
