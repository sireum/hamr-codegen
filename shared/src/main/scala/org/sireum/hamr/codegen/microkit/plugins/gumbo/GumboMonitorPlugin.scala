// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlPortConnection, AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.{HamrCli, ModelUtil, ResourceUtil}
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.monitors.{MonitorInjector, UserLandMonitorPlugin}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiPlugin, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.component.{CRustComponentPlugin, ComponentContributions}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.util.{MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Aadl, GclStateVar}
import org.sireum.message.Reporter

object GumboMonitorPlugin {

  val KEY_GumboMonitorPlugin_Model_Transformed: String = "KEY_GumboMonitorPlugin_Model_Transformed"

  val KEY_GumboMonitorPlugin_Model_handled: String = "KEY_GumboMonitorPlugin_Model_handled"

  val KEY_GumboMonitorPlugin_CBackend: String = "KEY_GumboMonitorPlugin_CBackend"

  val KEY_GumboMonitorPlugin_RustFinalized: String = "KEY_GumboMonitorPlugin_RustFinalized"

  val KEY_GumboMonitorPlugin_MonitorMethod: String = "KEY_GumboMonitorPlugin_MonitorMethod"

  @strictpure def haveHandledModelTransform(store: Store): B = store.contains(KEY_GumboMonitorPlugin_Model_Transformed)

  @strictpure def hasHandled(store: Store): B = store.contains(KEY_GumboMonitorPlugin_Model_handled)

  @strictpure def haveHandledCBackend(store: Store): B = store.contains(KEY_GumboMonitorPlugin_CBackend)

  @strictpure def haveRustFinalized(store: Store): B = store.contains(KEY_GumboMonitorPlugin_RustFinalized)

  @strictpure def haveHandledMonitorMethod(store: Store): B = store.contains(KEY_GumboMonitorPlugin_MonitorMethod)




  val stateVarPortPrefix: String = "sv_"

  @strictpure def stateVarPortName(stateVarName: String): String =
    s"${stateVarPortPrefix}${stateVarName}"

  @strictpure def monitorStateVarPortName(srcProcessId: String, srcThreadId: String, stateVarName: String): String =
    s"${srcProcessId}_${srcThreadId}_${stateVarPortPrefix}${stateVarName}"

  val KEY_RUST_MONITORING: String = "KEY_RUST_MONITORING"


  @strictpure def getRustMonitoringStore(store: Store): Option[RustMonitoringStore] =
    store.get(KEY_RUST_MONITORING).asInstanceOf[Option[RustMonitoringStore]]


  @pure def computeMonitorGetterCall(param: GumboXRustUtil.GGParam,
                                      processId: String,
                                      threadSimpleId: String,
                                      dstPortToMonitorPortName: Map[ISZ[String], String]): ST = {
    param match {
      case sv: GumboXRustUtil.GGStateVarParam =>
        return st"api.get_${processId}_${threadSimpleId}_sv_${sv.originName}()"
      case pp: GumboXRustUtil.GGPortParam =>
        if (pp.isIn) {
          dstPortToMonitorPortName.get(pp.port.path) match {
            case Some(mn) => return st"api.get_${mn}()"
            case _ => return st"api.get_UNKNOWN_${pp.originName}()"
          }
        } else {
          return st"api.get_${processId}_${threadSimpleId}_${pp.originName}()"
        }
      case _ => return st"api.get_UNKNOWN()"
    }
  }
}

@datatype class RustMonitoringStateVarInfo(val name: String)

@datatype class RustMonitoringStore(val entries: HashSMap[ISZ[String], ISZ[RustMonitoringStateVarInfo]]) extends StoreValue

@sig trait GumboMonitorPlugin
  extends UserLandMonitorPlugin with MicrokitFinalizePlugin {

  @strictpure override def getMonitorName: String = "gumbo_monitor"

  @strictpure override def getRetainedNonModelPorts(store: Store): ISZ[IdPath] =
    for (id <- StoreUtil.getNonModelElements(store) if id.nonEmpty && ops.StringOps(id(id.lastIndex)).startsWith(GumboMonitorPlugin.stateVarPortPrefix)) yield id

  @pure override def canHandleModelTransform(model: Aadl,
                                             options: HamrCli.CodegenOption,
                                             types: AadlTypes,
                                             symbolTable: SymbolTable,
                                             store: Store,
                                             reporter: Reporter): B = {
    return (
      canHandleModelTransformHelper(model, options, types, symbolTable, store, reporter) &&
        hasThreadsWithStateVars(symbolTable) &&
        !GumboMonitorPlugin.haveHandledModelTransform(store))
  }

  override def handleModelTransform(origModel: Aadl,
                                    options: HamrCli.CodegenOption,
                                    origTypes: AadlTypes,
                                    origSymbolTable: SymbolTable,
                                    origStore: Store,
                                    reporter: Reporter): Option[(Store, Aadl, AadlTypes, SymbolTable)] = {
    var localStore: Store = origStore + GumboMonitorPlugin.KEY_GumboMonitorPlugin_Model_Transformed ~> BoolValue(T)

    super[UserLandMonitorPlugin].handleModelTransform(origModel, options, origTypes, origSymbolTable, localStore, reporter) match {
      case Some((uStore, model, types, symbolTable)) => {
        localStore = uStore

        val system = model.components(0)
        val systemPath = system.identifier.name
        val monitorProcessPath = getMonitorProcessPath(systemPath)
        val monitorThreadPath = getMonitorThreadPath(systemPath)

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
      case _ =>
        return None()
    }
  }



  // MicrokitPlugin phase: post-process ConnectionStore to add monitoring guards
  // to sv_ put methods and add is_monitoring_enabled()
  @pure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                               symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {

    val commonBase =
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !isDisabled(store) &&
        !reporter.hasError &&
        options.runtimeMonitoring &&
        GumboMonitorPlugin.haveHandledModelTransform(store)

    if (!commonBase) {
      return F
    } else {
      val canDoBackend: B =
        CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
          CTypePlugin.getCTypeProvider(store).nonEmpty &&
          !GumboMonitorPlugin.haveHandledCBackend(store)

      val canDoUserLandHandle: B =
        !GumboMonitorPlugin.hasHandled(store) &&
          canHandleHelper(model, options, types, symbolTable, store, reporter)

      val canDoMonitorMethod: B =
        !GumboMonitorPlugin.haveHandledMonitorMethod(store) &&
          GumboXRustPlugin.getGumboXContributions(store).nonEmpty &&
          CRustComponentPlugin.hasCRustComponentContributions(store)

      return canDoBackend || canDoUserLandHandle || canDoMonitorMethod
    }
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                             symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store
    var resources: ISZ[Resource] = ISZ()

    if (!GumboMonitorPlugin.haveHandledCBackend(localStore)) {
      localStore = localStore + GumboMonitorPlugin.KEY_GumboMonitorPlugin_CBackend ~> BoolValue(T)

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
        }
      }
    } // end C backend guard

    if (!GumboMonitorPlugin.hasHandled(localStore) &&
      canHandleHelper(model, options, types, symbolTable, localStore, reporter)) {
      localStore = localStore + GumboMonitorPlugin.KEY_GumboMonitorPlugin_Model_handled ~> BoolValue(T)
      val s = super[UserLandMonitorPlugin].handle(model, options, types, symbolTable, localStore, reporter)
      localStore = s._1
      resources = resources ++ s._2
    }

    // Monitor method generation pass: modify ComponentContributions to add
    // state vars, monitor method, and buildUserChannelTables to the monitor
    // thread's app file so the initial codegen includes runtime monitoring behavior.
    if (!GumboMonitorPlugin.haveHandledMonitorMethod(localStore) &&
      GumboXRustPlugin.getGumboXContributions(localStore).nonEmpty &&
      CRustComponentPlugin.hasCRustComponentContributions(localStore)) {

      localStore = localStore + GumboMonitorPlugin.KEY_GumboMonitorPlugin_MonitorMethod ~> BoolValue(T)

      val gumboxContribs = GumboXRustPlugin.getGumboXContributions(localStore).get

      if (gumboxContribs.componentContributions.nonEmpty) {
        val monitorThreadPath: ISZ[String] = getMonitorThreadPath(symbolTable.rootSystem.path)

        // Build destination port path → monitor port name mapping
        var dstPortToMonitorPortName: Map[ISZ[String], String] = Map.empty
        for (conn <- symbolTable.aadlConnections) {
          conn match {
            case pc: AadlPortConnection =>
              val srcThread = pc.srcComponent.asInstanceOf[AadlThread]
              val srcProcess = srcThread.getParent(symbolTable)
              val portName = CommonUtil.getLastName(pc.srcFeature.feature.identifier)
              val monPortName: String = s"${srcProcess.identifier}_${srcThread.identifier}_${portName}"
              dstPortToMonitorPortName = dstPortToMonitorPortName + pc.dstFeature.path ~> monPortName
            case _ =>
          }
        }

        val contributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
        contributions.componentContributions.get(monitorThreadPath) match {
          case Some(monitorContrib) =>
            var preStateStructFields: ISZ[RAST.Item] = ISZ()
            var preStateInitValues: ISZ[ST] = ISZ()
            var initChecks: ISZ[ST] = ISZ()
            var postCheckArms: ISZ[ST] = ISZ()
            var preCheckArms: ISZ[ST] = ISZ()
            var gumboxUses: ISZ[RAST.Item] = ISZ()

            for (entry <- gumboxContribs.componentContributions.entries) {
              val thread = symbolTable.componentMap.get(entry._1).get.asInstanceOf[AadlThread]
              val threadId = MicrokitUtil.getComponentIdPath(thread)
              val process = thread.getParent(symbolTable)
              val processId = process.identifier
              val threadSimpleId = thread.identifier
              val contribs = entry._2

              gumboxUses = gumboxUses :+ RAST.Use(ISZ(), RAST.IdentString(s"crate::gumbox::${threadId}_containers::*"))

              // Pre-state struct field and init (needed for either CEP_Pre or CEP_Post since
              // post-check references pre-state)
              val hasPreOrPost: B = contribs.computeContributions.CEP_Pre.nonEmpty ||
                contribs.computeContributions.CEP_Post.nonEmpty
              if (hasPreOrPost) {
                preStateStructFields = preStateStructFields :+
                  RAST.StructField(
                    visibility = RAST.Visibility.Private, isGhost = F,
                    ident = RAST.IdentString(s"pre_${threadId}"),
                    fieldType = RAST.TyPath(ISZ(ISZ("Option"), ISZ(s"PreState_${threadId}")), None()))
                preStateInitValues = preStateInitValues :+ st"pre_${threadId}: None,"
              }

              // Init check (IEP_Post)
              val iepPostParams = GumboXRustUtil.sortParams(contribs.initializeContributions.IEP_Post_Params)
              if (contribs.initializeContributions.IEP_Guarantee.nonEmpty) {
                val postFieldInits: ISZ[ST] = for (p <- iepPostParams) yield
                  st"${p.name}: ${GumboMonitorPlugin.computeMonitorGetterCall(p, processId, threadSimpleId, dstPortToMonitorPortName)},"
                val postArgs: ISZ[ST] = for (p <- iepPostParams) yield
                  st"post_${threadId}.${p.name}"

                initChecks = initChecks :+
                  st"""{
                      |  let post_${threadId} = PostState_${threadId} {
                      |    ${(postFieldInits, "\n")}
                      |  };
                      |  if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getInitialize_IEP_Post_MethodName}(
                      |    ${(postArgs, ", ")}) {
                      |    log::warn!("*** CONTRACT VIOLATION: ${threadId} IEP_Post not satisfied ***");
                      |    log::warn!("${threadId} post: {:?}", post_${threadId});
                      |  }
                      |}"""
              }

              // Post-state check match arm (CEP_Post)
              if (contribs.computeContributions.CEP_Post.nonEmpty) {
                val cepPostParams = GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Post_Params)
                val postOnlyParams = cepPostParams.filter(p =>
                  p.kind == GumboXRustUtil.SymbolKind.StateVar || p.isOutPort)

                val postFieldInits: ISZ[ST] = for (p <- postOnlyParams) yield
                  st"${p.name}: ${GumboMonitorPlugin.computeMonitorGetterCall(p, processId, threadSimpleId, dstPortToMonitorPortName)},"

                val cepPostArgs: ISZ[ST] = for (p <- cepPostParams) yield
                  st"${if (p.kind == GumboXRustUtil.SymbolKind.StateVarPre || p.isInPort) "pre" else "post"}.${p.name}"

                postCheckArms = postCheckArms :+
                  st"""${threadId}_MON => {
                      |  let post = PostState_${threadId} {
                      |    ${(postFieldInits, "\n")}
                      |  };
                      |  if let Some(pre) = &self.pre_${threadId} {
                      |    if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getCompute_CEP_Post_MethodName}(
                      |      ${(cepPostArgs, ", ")}) {
                      |      log::warn!("*** CONTRACT VIOLATION: ${threadId} CEP_Post not satisfied ***");
                      |      log::warn!("${threadId} pre: {:?}", pre);
                      |      log::warn!("${threadId} post: {:?}", post);
                      |    }
                      |  } else {
                      |    log::warn!("${threadId} post check skipped: no saved pre-state");
                      |  }
                      |}"""
              }

              // Pre-state capture match arm (needed for either CEP_Pre or CEP_Post)
              if (hasPreOrPost) {
                val hasCepPre: B = contribs.computeContributions.CEP_Pre.nonEmpty
                val preParams: ISZ[GumboXRustUtil.GGParam] =
                  if (hasCepPre) GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Pre_Params)
                  else GumboXRustUtil.sortParams(contribs.computeContributions.CEP_Post_Params).filter(p =>
                    p.kind == GumboXRustUtil.SymbolKind.StateVarPre || p.isInPort)
                val preFieldInits: ISZ[ST] = for (p <- preParams) yield
                  st"${p.name}: ${GumboMonitorPlugin.computeMonitorGetterCall(p, processId, threadSimpleId, dstPortToMonitorPortName)},"

                val preCheckBody: ST =
                  if (hasCepPre) {
                    val preArgs: ISZ[ST] = for (p <- preParams) yield st"pre.${p.name}"
                    st"""let pre = PreState_${threadId} {
                        |  ${(preFieldInits, "\n")}
                        |};
                        |if !crate::gumbox::${threadId}_GUMBOX::${GumboXRustUtil.getCompute_CEP_Pre_MethodName}(
                        |  ${(preArgs, ", ")}) {
                        |  log::warn!("*** CONTRACT VIOLATION: ${threadId} CEP_Pre not satisfied ***");
                        |  log::warn!("${threadId} pre: {:?}", pre);
                        |}
                        |self.pre_${threadId} = Some(pre);"""
                  } else {
                    st"""let pre = PreState_${threadId} {
                        |  ${(preFieldInits, "\n")}
                        |};
                        |self.pre_${threadId} = Some(pre);"""
                  }

                preCheckArms = preCheckArms :+
                  st"""${threadId}_MON => {
                      |  $preCheckBody
                      |}"""
              }
            }

            val externalBodyAttr: String =
              if (options.verusAttributeSyntax) "#[verus_verify(external_body)]"
              else "#[verifier::external_body]"

            val monitorThread = symbolTable.componentMap.get(monitorThreadPath).get.asInstanceOf[AadlThread]
            val monitorThreadId = MicrokitUtil.getComponentIdPath(monitorThread)
            val appApiType = CRustApiPlugin.applicationApiType(monitorThread)

            val monitorMethod = RAST.ItemST(
              st"""$externalBodyAttr
                  |pub fn monitor<API: ${monitorThreadId}_Full_Api>(
                  |  &mut self,
                  |  api: &mut ${appApiType}<API>)
                  |{
                  |  let state = api.get_sched_state();
                  |
                  |  if self.last_index == u32::MAX {
                  |    let schedule = api.get_sched_schedule();
                  |    buildUserChannelTables(
                  |      &schedule, &mut self.prev_user_ch, &mut self.next_user_ch);
                  |  }
                  |
                  |  // Detect schedule wraparound: if the current timeslice index is not
                  |  // greater than the last one seen, the schedule has started a new frame
                  |  if state.current_timeslice <= self.last_index {
                  |    self.frame_period = self.frame_period + 1;
                  |  }
                  |
                  |  let idx = state.current_timeslice as usize;
                  |
                  |  if self.last_index == u32::MAX {
                  |    // First compute phase, check initialization guarantees
                  |    ${(initChecks, "\n")}
                  |  } else {
                  |    let prev_ch = self.prev_user_ch[idx];
                  |    match prev_ch {
                  |      ${(postCheckArms, "\n")}
                  |      _ => {}
                  |    }
                  |  }
                  |
                  |  let next_ch = self.next_user_ch[idx];
                  |  match next_ch {
                  |    ${(preCheckArms, "\n")}
                  |    _ => {}
                  |  }
                  |
                  |  self.last_index = state.current_timeslice;
                  |}""")

            // Scheduling state struct fields
            val schedFields: ISZ[RAST.Item] = ISZ(
              RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
                ident = RAST.IdentString("frame_period"),
                fieldType = RAST.TyPath(ISZ(ISZ("i32")), None())),
              RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
                ident = RAST.IdentString("last_index"),
                fieldType = RAST.TyPath(ISZ(ISZ("u32")), None())),
              RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
                ident = RAST.IdentString("prev_user_ch"),
                fieldType = RAST.TyPath(ISZ(ISZ("hamr", "ScheduleChannels")), None())),
              RAST.StructField(visibility = RAST.Visibility.Private, isGhost = F,
                ident = RAST.IdentString("next_user_ch"),
                fieldType = RAST.TyPath(ISZ(ISZ("hamr", "ScheduleChannels")), None())))

            val updatedStruct = monitorContrib.appStructDef(
              items = monitorContrib.appStructDef.items ++ schedFields ++ preStateStructFields)

            // Initializer values for new()
            val schedInits: ISZ[ST] = ISZ(
              st"frame_period: 0,",
              st"last_index: u32::MAX,",
              st"prev_user_ch: [0; hamr::hamr_ScheduleChannels_DIM_0],",
              st"next_user_ch: [0; hamr::hamr_ScheduleChannels_DIM_0],")
            val allInits = schedInits ++ preStateInitValues

            // Update impl: replace new() and timeTriggered bodies, append monitor method
            val existingImpl = monitorContrib.appStructImpl.asInstanceOf[RAST.ImplBase]
            var updatedImplItems: ISZ[RAST.Item] = ISZ()
            for (item <- existingImpl.items) {
              item match {
                case fn: RAST.FnImpl =>
                  if (fn.sig.ident.prettyST.render == "new") {
                    updatedImplItems = updatedImplItems :+ fn(
                      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemSelf(allInits)))))
                  } else if (fn.sig.ident.prettyST.render == "timeTriggered") {
                    updatedImplItems = updatedImplItems :+ fn(
                      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                        st"self.monitor(api);")))))
                  } else {
                    updatedImplItems = updatedImplItems :+ item
                  }
                case _ =>
                  updatedImplItems = updatedImplItems :+ item
              }
            }
            updatedImplItems = updatedImplItems :+ monitorMethod

            val updatedImpl = existingImpl(items = updatedImplItems)

            // buildUserChannelTables standalone function
            val buildUserChannelTablesFn = RAST.ItemST(
              st"""// For each timeslice index, finds the nearest preceding and following user
                  |// partition channels. This lets the monitor know which thread just yielded
                  |// (prev) and which will run next (next) so it can check post-conditions
                  |// and capture pre-state at the right time.
                  |$externalBodyAttr
                  |pub fn buildUserChannelTables(
                  |  sched: &hamr::Schedule,
                  |  prev: &mut hamr::ScheduleChannels,
                  |  next: &mut hamr::ScheduleChannels)
                  |{
                  |  let n = sched.num_timeslices as usize;
                  |  for i in 0..n {
                  |    let mut found_prev = false;
                  |    let mut found_next = false;
                  |    for offset in 1..n {
                  |      if !found_prev {
                  |        let backward = (i + n - offset) % n;
                  |        if sched.is_user_partition[backward] {
                  |          prev[i] = sched.timeslice_ch[backward];
                  |          found_prev = true;
                  |        }
                  |      }
                  |      if !found_next {
                  |        let forward = (i + offset) % n;
                  |        if sched.is_user_partition[forward] {
                  |          next[i] = sched.timeslice_ch[forward];
                  |          found_next = true;
                  |        }
                  |      }
                  |      if found_prev && found_next {
                  |        break;
                  |      }
                  |    }
                  |  }
                  |}""")

            val additionalUses: ISZ[RAST.Item] = gumboxUses

            val updatedContrib = monitorContrib(
              appStructDef = updatedStruct,
              appStructImpl = updatedImpl,
              moduleLevelEntries = monitorContrib.moduleLevelEntries :+ buildUserChannelTablesFn,
              appUses = monitorContrib.appUses ++ additionalUses)

            localStore = CRustComponentPlugin.putComponentContributions(
              contributions.replaceComponentContributions(
                contributions.componentContributions + monitorThreadPath ~> updatedContrib),
              localStore)

          case _ =>
        }
      }
    }

    return (localStore, resources)
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

    val monitorThreadPath: ISZ[String] = getMonitorThreadPath(symbolTable.rootSystem.path)

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

    // Generate GUMBOX and container modules in the monitor crate under src/gumbox
    val gumboxContribsOpt = GumboXRustPlugin.getGumboXContributions(store)
    if (gumboxContribsOpt.nonEmpty) {
      symbolTable.componentMap.get(monitorThreadPath) match {
        case Some(monitorComp) =>
          val monitorThread = monitorComp.asInstanceOf[AadlThread]
          val monitorCrateDir = CRustComponentPlugin.componentCrateDirectory(monitorThread, options)
          val gumboxDir = s"$monitorCrateDir/src/gumbox"

          var modDecls: ISZ[String] = ISZ()

          for (entry <- gumboxContribsOpt.get.componentContributions.entries) {
            val thread = symbolTable.componentMap.get(entry._1).get.asInstanceOf[AadlThread]
            val threadId = MicrokitUtil.getComponentIdPath(thread)

            // Generate GUMBOX module (copy of the thread's contract functions)
            val gumboxContent = GumboXRustPlugin.generateGumboxModuleContent(entry._2)
            val gumboxPath = s"$gumboxDir/${threadId}_GUMBOX.rs"
            resources = resources :+ ResourceUtil.createResource(gumboxPath, gumboxContent, T)
            modDecls = modDecls :+ s"${threadId}_GUMBOX"

            // Generate PreState/PostState container structs
            val containerContent = GumboXRustPlugin.generateMonitorContainerContent(threadId, entry._2)
            val containerPath = s"$gumboxDir/${threadId}_containers.rs"
            resources = resources :+ ResourceUtil.createResource(containerPath, containerContent, T)
            modDecls = modDecls :+ s"${threadId}_containers"
          }

          // Generate gumbox/mod.rs
          val modEntries: ISZ[ST] = for (m <- modDecls) yield st"pub mod $m;"
          val modContent =
            st"""${CommentTemplate.doNotEditComment_slash}
                |
                |${(modEntries, "\n")}
                |"""
          resources = resources :+ ResourceUtil.createResource(s"$gumboxDir/mod.rs", modContent, T)

          // Regenerate the monitor crate's lib.rs to include 'mod gumbox;'
          val monitorThreadId = MicrokitUtil.getComponentIdPath(monitorThread)
          val monitorSrcDir = s"$monitorCrateDir/src"
          val monitorEntrypoints: ISZ[ST] =
            if (monitorThread.isPeriodic())
              ISZ(
                st"""#[no_mangle]
                    |pub extern "C" fn ${monitorThreadId}_timeTriggered() {
                    |  unsafe {
                    |    if let Some(_app) = app.as_mut() {
                    |      _app.timeTriggered(&mut compute_api);
                    |    } else {
                    |      panic!("Unexpected: app is None");
                    |    }
                    |  }
                    |}""")
            else ISZ(st"NOT YET")

          val monitorLibContent =
            st"""#![cfg_attr(not(test), no_std)]
                |
                |${RustUtil.defaultCrateLevelAttributes}
                |
                |${CommentTemplate.doNotEditComment_slash}
                |
                |mod bridge;
                |mod component;
                |mod gumbox;
                |mod logging;
                |
                |#[cfg(test)]
                |mod test;
                |
                |use crate::bridge::${CRustApiPlugin.apiModuleName(monitorThread)}::{self as api, *};
                |use crate::component::${CRustComponentPlugin.appModuleName(monitorThread)}::*;
                |use data::*;
                |
                |static mut app: Option<$monitorThreadId> = None;
                |static mut init_api: ${CRustApiPlugin.applicationApiType(monitorThread)}<${CRustApiPlugin.initializationApiType(monitorThread)}> = api::init_api();
                |static mut compute_api: ${CRustApiPlugin.applicationApiType(monitorThread)}<${CRustApiPlugin.computeApiType(monitorThread)}> = api::compute_api();
                |
                |#[no_mangle]
                |pub extern "C" fn ${monitorThreadId}_initialize() {
                |  logging::init_logging();
                |
                |  unsafe {
                |    #[cfg(test)]
                |    crate::bridge::extern_c_api::initialize_test_globals();
                |
                |    let mut _app = $monitorThreadId::new();
                |    _app.initialize(&mut init_api);
                |    app = Some(_app);
                |  }
                |}
                |
                |${(monitorEntrypoints, "\n\n")}
                |
                |#[no_mangle]
                |pub extern "C" fn ${monitorThreadId}_notify(channel: microkit_channel) {
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
          resources = resources :+ ResourceUtil.createResource(s"$monitorSrcDir/lib.rs", monitorLibContent, T)

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
