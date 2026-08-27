// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections.QueueVaddr
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.components.{CComponentPlugin, CComponentR2U2Contributions}
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.{CTypePlugin, CTypeProvider}
import org.sireum.hamr.codegen.microkit.plugins.gumbo.GumboR2U2Util.R2U2MonitorInput
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.{Context, TargetLanguage}
import org.sireum.hamr.codegen.microkit.plugins.rust.types.{CRustTypeNameProvider, CRustTypeProvider, DefaultCRustTypeNameProvider}
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir._
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

// Generates native-C R2U2 support for GUMBO monitors and their states/functions.
object GumboCPlugin {

  val KEY_GumboCPlugin: String = "KEY_GumboCPlugin"

  // Pure-C models do not run CRustTypePlugin, so this adapter supplies the type
  // operations required by the shared C2PO and expression lowering paths.
  @datatype class C2POTypeAdapter(val cTypeProvider: CTypeProvider) extends CRustTypeProvider {
    @pure override def getTypeNameProvider(aadlType: AadlType): CRustTypeNameProvider = {
      return DefaultCRustTypeNameProvider(ISZ(cTypeProvider.getTypeNameProvider(aadlType).mangledName))
    }

    @pure override def getRepresentativeType(aadlType: AadlType): AadlType = {
      return cTypeProvider.getRepresentativeType(aadlType)
    }

    @pure override def rustTypeDefs: HashSMap[String, ISZ[RAST.Item]] = {
      return HashSMap.empty[String, ISZ[RAST.Item]]
    }
  }
}

@sig trait GumboCPlugin extends MicrokitPlugin {

  // Generate the native-C contributions for a GUMBO R2U2 monitor.
  @pure def handleComputeMonitor(component: AadlThread,
                                 subclauseInfo: GclAnnexClauseInfo,
                                 types: AadlTypes,
                                 cTypeProvider: CTypeProvider,
                                 store: Store,
                                 reporter: Reporter): CComponentR2U2Contributions = {
    val monitor: GclMonitor = subclauseInfo.annex.monitor.get
    if (monitor.guarantees.isEmpty) {
      reporter.error(component.component.identifier.pos, MicrokitCodegen.toolName,
        "A C R2U2 monitor must contain at least one guarantee")
      return CComponentR2U2Contributions.empty
    }

    @pure def getCTypeName(aadlType: AadlType): String = {
      return cTypeProvider.getTypeNameProvider(aadlType).mangledName
    }

    @pure def snapshotPort(port: AadlPort): ST = {
      port match {
        case _: AadlEventPort =>
          return st"bool r2u2_port_${port.identifier}_present = peek_${port.identifier}();"
        case _: AadlDataPort =>
          val cTypeName: String = getCTypeName(MicrokitTypeUtil.getPortType(port))
          return st"""$cTypeName r2u2_port_${port.identifier};
                     |peek_${port.identifier}(&r2u2_port_${port.identifier});"""
        case _: AadlEventDataPort =>
          val cTypeName: String = getCTypeName(MicrokitTypeUtil.getPortType(port))
          return st"""$cTypeName r2u2_port_${port.identifier} = {0};
                     |bool r2u2_port_${port.identifier}_present = peek_${port.identifier}(&r2u2_port_${port.identifier});"""
      }
    }

    val c2poTypeAdapter: GumboCPlugin.C2POTypeAdapter =
      GumboCPlugin.C2POTypeAdapter(cTypeProvider)
    var specs: RAST.R2U2SpecDef = RAST.R2U2SpecDef(
      structs = ISZ(), enums = ISZ(), inputs = ISZ(), defines = ISZ(), ftspecs = ISZ(), ptspecs = ISZ())
    var monitorInputs: Map[String, R2U2MonitorInput] = Map.empty
    var referencedMethods: Set[String] = Set.empty

    for (guarantee <- monitor.guarantees) {
      val (formula, tense, variablesInSpec) =
        GumboC2POUtil.processGumboSpec(
          guarantee, component, Context.monitor_clause, F, types, c2poTypeAdapter, store, reporter)
      tense match {
        case GumboC2POUtil.SpecTense.Future => specs = specs(ftspecs = specs.ftspecs :+ formula)
        case GumboC2POUtil.SpecTense.Past => specs = specs(ptspecs = specs.ptspecs :+ formula)
      }
      for (entry <- variablesInSpec.entries) {
        val (monitorInput, methods) = GumboR2U2Util.lowerCR2U2Input(
          entry._2, component, subclauseInfo.annex.state, types, c2poTypeAdapter, store, reporter)
        monitorInputs = monitorInputs + entry._1 ~> monitorInput
        referencedMethods = referencedMethods ++ methods.elements
      }
    }

    val expandedInputs: (RAST.R2U2SpecDef, Map[String, R2U2MonitorInput]) =
      GumboR2U2Util.expandStructInputs(specs, monitorInputs)
    specs = expandedInputs._1
    monitorInputs = expandedInputs._2

    var preLoads: ISZ[ST] = ISZ()
    var postLoads: ISZ[ST] = ISZ()
    var prePortIds: Set[String] = Set.empty
    var postPortIds: Set[String] = Set.empty
    val outputPortIds: Set[String] = Set.empty[String] ++
      component.getPorts().filter((port: AadlPort) => port.direction == Direction.Out).map((port: AadlPort) => port.identifier)
    var signalIndex: Z = 0
    for (entry <- monitorInputs.entries) {
      val name: String = entry._1
      val input: R2U2MonitorInput = entry._2
      val typeName: String = input.enumTypeOpt match {
        case Some(enumType) => // Check if enum type and adjust type name to enum type name
          if (!ops.ISZOps(specs.enums).exists(existing => existing.name == enumType.name)) {
            specs = specs(enums = specs.enums :+ enumType)
          }
          enumType.name
        case _ => input.arrayTypeOpt match {
          // Check if array type and adjust type name to array syntax
          case Some(arrayType) => s"${arrayType.elementType.string}[${arrayType.size}]"
          case _ => input.expType.string
        }
      }
      specs = specs(inputs = specs.inputs :+ RAST.R2U2InputDef(
        name, typeName, signalIndex, input.arrayTypeOpt.map(t => t.size)))
      val load: ST = input.expType match {
        case GumboC2POUtil.C2POType.bool =>
          st"r2u2_load_bool_signal(&r2u2_monitor.monitor, $signalIndex, ${input.exp.prettyST}); // Loading signal $name into index $signalIndex"
        case GumboC2POUtil.C2POType.int =>
          st"r2u2_load_int_signal(&r2u2_monitor.monitor, $signalIndex, (int32_t) (${input.exp.prettyST})); // Loading signal $name into index $signalIndex"
        case GumboC2POUtil.C2POType.float =>
          st"r2u2_load_float_signal(&r2u2_monitor.monitor, $signalIndex, (double) (${input.exp.prettyST})); // Loading signal $name into index $signalIndex"
        case GumboC2POUtil.C2POType.enumeration =>
          st"r2u2_load_int_signal(&r2u2_monitor.monitor, $signalIndex, (int32_t) (${input.exp.prettyST})); // Loading enum signal $name into index $signalIndex"
        case GumboC2POUtil.C2POType.array =>
          val arrayType: GumboC2POUtil.C2POArray = input.arrayTypeOpt.get
          val arrayLoad: ST = arrayType.elementType match {
            case GumboC2POUtil.C2POType.bool =>
              st"r2u2_load_bool_signal(&r2u2_monitor.monitor, $signalIndex + r2u2_i, (${input.exp.prettyST})[r2u2_i]);"
            case GumboC2POUtil.C2POType.int =>
              st"r2u2_load_int_signal(&r2u2_monitor.monitor, $signalIndex + r2u2_i, (int32_t) ((${input.exp.prettyST})[r2u2_i]));"
            case GumboC2POUtil.C2POType.float =>
              st"r2u2_load_float_signal(&r2u2_monitor.monitor, $signalIndex + r2u2_i, (double) ((${input.exp.prettyST})[r2u2_i]));"
            case _ => halt("Unsupported C R2U2 array element type")
          }
          st"""{
             |  // Loading array signal $name into indices $signalIndex..${signalIndex + arrayType.size - 1}
             |  for (size_t r2u2_i = 0; r2u2_i < ${arrayType.size}; ++r2u2_i) {
             |    $arrayLoad
             |  }
             |}"""
        case GumboC2POUtil.C2POType.struct => halt("C R2U2 struct input was not expanded")
      }
      val referencesOutput: B = ops.ISZOps(input.referencedPorts.elements).exists(
        (portId: String) => outputPortIds.contains(portId))
      if (referencesOutput || input.isPostStateVar) {
        postLoads = postLoads :+ load
        postPortIds = postPortIds ++ input.referencedPorts.elements
      } else {
        preLoads = preLoads :+ load
        prePortIds = prePortIds ++ input.referencedPorts.elements
      }
      input.arrayTypeOpt match {
        case Some(arrayType) => signalIndex = signalIndex + arrayType.size
        case _ => signalIndex = signalIndex + 1
      }
    }

    var preSnapshots: ISZ[ST] = ISZ()
    var postSnapshots: ISZ[ST] = ISZ()
    var inputGets: ISZ[ST] = ISZ()
    var outputEventQueueDeclarations: ISZ[ST] = ISZ()
    var outputEventCountDeclarations: ISZ[ST] = ISZ()
    for (port <- component.getPorts() if !StoreUtil.isSynthetic(port.path, store)) {
      if (port.direction == Direction.In &&
        (prePortIds.contains(port.identifier) || postPortIds.contains(port.identifier))) {
        port match {
          case _: AadlEventPort =>
            inputGets = inputGets :+ st"get_${port.identifier}();"
          case _ =>
            val cTypeName: String = getCTypeName(MicrokitTypeUtil.getPortType(port))
            inputGets = inputGets :+ st"""$cTypeName ${port.identifier};
                                             |get_${port.identifier}(&${port.identifier});"""
        }
      }
      if (prePortIds.contains(port.identifier)) {
        preSnapshots = preSnapshots :+ snapshotPort(port)
      }
      if (postPortIds.contains(port.identifier)) {
        if (port.direction == Direction.Out &&
          (port.isInstanceOf[AadlEventPort] || port.isInstanceOf[AadlEventDataPort])) {
          var queueOpt: Option[QueueVaddr] = None()
          for (connectionStore <- CConnectionProviderPlugin.getCConnectionStore(store)
               if connectionStore.senderName == component.path) {
            connectionStore.codeContributions.get(component.path) match {
              case Some(contribution) if contribution.portName == port.path =>
                for (global <- contribution.cContributions.cBridge_GlobalVarContributions if queueOpt.isEmpty) {
                  global match {
                    case queue: QueueVaddr => queueOpt = Some(queue)
                    case _ =>
                  }
                }
              case _ =>
            }
          }
          assert(queueOpt.nonEmpty, s"Could not find the C output queue for ${port.identifier}")
          val queue: QueueVaddr = queueOpt.get
          val queueNameOps: ops.StringOps = ops.StringOps(queue.varName)
          assert(queueNameOps.startsWith("*"), s"Unexpected C output queue name ${queue.varName}")
          val queueName: String = queueNameOps.substring(1, queue.varName.size)
          outputEventQueueDeclarations = outputEventQueueDeclarations :+ st"extern ${queue.pretty};"
          outputEventCountDeclarations = outputEventCountDeclarations :+
            st"static sb_event_counter_t r2u2_port_${port.identifier}_count;"
          // Output queues persist, so compare enqueue counts to detect an event
          // emitted during the current dispatch.
          preSnapshots = preSnapshots :+
            st"r2u2_port_${port.identifier}_count = $queueName->numSent;"
          port match {
            case _: AadlEventPort =>
              postSnapshots = postSnapshots :+
                st"bool r2u2_port_${port.identifier}_present = $queueName->numSent != r2u2_port_${port.identifier}_count;"
            case _: AadlEventDataPort =>
              val cTypeName: String = getCTypeName(MicrokitTypeUtil.getPortType(port))
              postSnapshots = postSnapshots :+ st"""$cTypeName r2u2_port_${port.identifier} = {0};
                                                       |bool r2u2_port_${port.identifier}_present = $queueName->numSent != r2u2_port_${port.identifier}_count;
                                                       |if (r2u2_port_${port.identifier}_present) {
                                                       |  peek_${port.identifier}(&r2u2_port_${port.identifier});
                                                       |}"""
            case _ => halt("Expected an output event port")
          }
        } else {
          postSnapshots = postSnapshots :+ snapshotPort(port)
        }
      }
    }

    // Render the executable GUMBO functions referenced by monitor inputs.
    var pending: ISZ[String] = referencedMethods.elements
    var seen: Set[String] = Set.empty
    var prototypes: ISZ[ST] = ISZ()
    var definitions: ISZ[ST] = ISZ()
    while (pending.nonEmpty) {
      val methodName: String = pending(0)
      pending = ops.ISZOps(pending).drop(1)
      if (!seen.contains(methodName)) {
        seen = seen + methodName
        var found: B = F
        for (method <- subclauseInfo.annex.methods if method.sig.id.value == methodName) {
          found = T
          method match {
            case bodyMethod: GclBodyMethod =>
              var params: ISZ[ST] = ISZ()
              for (param <- bodyMethod.sig.params) {
                param.tipe match {
                  case named: SAST.Type.Named =>
                    val ids: ISZ[String] = for (id <- named.name.ids) yield id.value
                    params = params :+ st"${getCTypeName(types.getTypeByPath(ids))} ${param.id.value}"
                  case _ =>
                    reporter.error(param.id.attr.posOpt, MicrokitCodegen.toolName,
                      s"C R2U2 GUMBO function '$methodName' has an unsupported parameter type")
                }
              }

              var returnType: String = "void"
              bodyMethod.sig.returnType match {
                case named: SAST.Type.Named =>
                  val ids: ISZ[String] = for (id <- named.name.ids) yield id.value
                  cTypeProvider.getRepresentativeType(types.getTypeByPath(ids)) match {
                    case _: ArrayType =>
                      reporter.error(bodyMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                        s"C R2U2 GUMBO function '$methodName' cannot return an array")
                    case aadlType =>
                      returnType = cTypeProvider.getTypeNameProvider(aadlType).mangledName
                  }
                case _ =>
                  reporter.error(bodyMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                    s"C R2U2 GUMBO function '$methodName' has an unsupported return type")
              }

              bodyMethod.method.bodyOpt match {
                case Some(body) if body.stmts.size == 1 =>
                  body.stmts(0) match {
                    case ret: SAST.Stmt.Return if ret.expOpt.nonEmpty =>
                      val expRewriter: GumboR2U2Util.R2U2ExpRewriter = GumboR2U2Util.R2U2ExpRewriter(
                        TargetLanguage.C, component, subclauseInfo.annex.state, types, store, reporter)
                      val snapshotExp: SAST.Exp =
                        expRewriter.transform_langastExp(ret.expOpt.get).getOrElse(ret.expOpt.get)
                      val cExp: ST = SlangExpUtil.rewriteExpH(
                        rexp = snapshotExp,
                        owner = component.classifier,
                        optComponent = Some(component),
                        context = Context.subclause_function,
                        substitutions = Map.empty[String, String],
                        inRequires = F,
                        inEnsures = F,
                        target = TargetLanguage.C,
                        tp = c2poTypeAdapter,
                        aadlTypes = types,
                        store = store,
                        reporter = reporter)
                      if (expRewriter.referencedPorts.nonEmpty || expRewriter.referencesPreStateVar ||
                        expRewriter.referencesPostStateVar) {
                        reporter.error(bodyMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                          s"C R2U2 GUMBO function '$methodName' must receive ports and state variables as parameters")
                      }
                      for (called <- expRewriter.referencedMethods.elements if !seen.contains(called)) {
                        pending = pending :+ called
                      }
                      val renderedParams: ST =
                        if (params.isEmpty) st"void"
                        else st"${(params, ", ")}"
                      val sig: ST = st"static $returnType r2u2_gumbo_$methodName($renderedParams)"
                      prototypes = prototypes :+ st"$sig;"
                      definitions = definitions :+ st"""$sig {
                                                           |  return $cExp;
                                                           |}"""
                    case _ =>
                      reporter.error(bodyMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                        s"C R2U2 GUMBO function '$methodName' must contain one return expression")
                  }
                case _ =>
                  reporter.error(bodyMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                    s"C R2U2 GUMBO function '$methodName' must contain one return expression")
              }

            case specMethod: GclSpecMethod =>
              reporter.error(specMethod.sig.id.attr.posOpt, MicrokitCodegen.toolName,
                s"C R2U2 monitor calls bodyless GUMBO function '$methodName'; a C executable body is required")
          }
        }
        if (!found) {
          reporter.error(component.component.identifier.pos, MicrokitCodegen.toolName,
            s"Could not resolve C R2U2 GUMBO function '$methodName'")
        }
      }
    }
    if (reporter.hasError) {
      return CComponentR2U2Contributions.empty
    }

    val (loggedSpecDefinitions, outputItems) = GumboR2U2Util.processCOutputs(
      thread = component,
      orderedSpecs = specs.ftspecs ++ specs.ptspecs,
      alerts = monitor.alerts)

    val stateDeclarations: ISZ[ST] = for (state <- subclauseInfo.annex.state) yield
      st"extern ${getCTypeName(types.typeMap.get(state.classifier).get)} r2u2_state_${state.name};"
    val stateDefinitions: ISZ[ST] = for (state <- subclauseInfo.annex.state) yield
      st"${getCTypeName(types.typeMap.get(state.classifier).get)} r2u2_state_${state.name} = {0};"

    var monitorItems: ISZ[ST] = ISZ()
    val outputEventDeclarations: ISZ[ST] = outputEventQueueDeclarations ++ outputEventCountDeclarations
    if (outputEventDeclarations.nonEmpty) {
      monitorItems = monitorItems :+ st"${(outputEventDeclarations, "\n")}"
    }
    if (stateDefinitions.nonEmpty) {
      monitorItems = monitorItems :+ st"${(stateDefinitions, "\n")}"
    }
    if (prototypes.nonEmpty) {
      monitorItems = monitorItems :+ st"${(prototypes, "\n")}"
    }
    if (definitions.nonEmpty) {
      monitorItems = monitorItems :+ st"${(definitions, "\n\n")}"
    }
    if (loggedSpecDefinitions.nonEmpty) {
      monitorItems = monitorItems :+ loggedSpecDefinitions.get
    }

    var preItems: ISZ[ST] = ISZ()
    if (preSnapshots.nonEmpty) {
      preItems = preItems :+ st"${(preSnapshots, "\n\n")}"
    }
    if (preLoads.nonEmpty) {
      preItems = preItems :+ st"${(preLoads, "\n")}"
    }

    var postItems: ISZ[ST] = ISZ()
    if (postSnapshots.nonEmpty) {
      postItems = postItems :+ st"${(postSnapshots, "\n\n")}"
    }
    if (postLoads.nonEmpty) {
      postItems = postItems :+ st"${(postLoads, "\n")}"
    }

    return CComponentR2U2Contributions(
      requiresR2U2 = T,
      r2u2SpecDef = Some(specs),
      inputGets = inputGets,
      r2u2HeaderItems = stateDeclarations,
      r2u2MonitorItems = monitorItems,
      r2u2PreItems = preItems,
      r2u2PostItems = postItems,
      r2u2OutputItems = outputItems)
  }

  @pure override def canHandle(model: Aadl,
                               options: HamrCli.CodegenOption,
                               types: AadlTypes,
                               symbolTable: SymbolTable,
                               store: Store,
                               reporter: Reporter): B = {
    val hasCMonitor: B = ops.ISZOps(symbolTable.getThreads()).exists((thread: AadlThread) =>
      !MicrokitUtil.isRusty(thread) && !StoreUtil.isSynthetic(thread.path, store) &&
        GumboRustUtil.getGumboSubclauseOpt(thread.path, symbolTable).
          exists((info: GclAnnexClauseInfo) => info.annex.monitor.nonEmpty))
    return options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      hasCMonitor &&
      !isDisabled(store) &&
      !reporter.hasError &&
      !store.contains(GumboCPlugin.KEY_GumboCPlugin) &&
      CTypePlugin.getCTypeProvider(store).nonEmpty &&
      CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
      !CComponentPlugin.processedThreads(store)
  }

  @pure override def handle(model: Aadl,
                            options: HamrCli.CodegenOption,
                            types: AadlTypes,
                            symbolTable: SymbolTable,
                            store: Store,
                            reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore: Store = store + GumboCPlugin.KEY_GumboCPlugin ~> BoolValue(T)
    val cTypeProvider: CTypeProvider = CTypePlugin.getCTypeProvider(localStore).get

    for (thread <- symbolTable.getThreads() if !MicrokitUtil.isRusty(thread) &&
      !StoreUtil.isSynthetic(thread.path, localStore)) {
      GumboRustUtil.getGumboSubclauseOpt(thread.path, symbolTable) match {
        case Some(subclauseInfo) if subclauseInfo.annex.monitor.nonEmpty =>
          if (!thread.isPeriodic()) {
            reporter.error(thread.component.identifier.pos, MicrokitCodegen.toolName,
              "C R2U2 monitors currently require a periodic component")
          } else {
            val contributions: CComponentR2U2Contributions = handleComputeMonitor(
              thread, subclauseInfo, types, cTypeProvider, localStore, reporter)
            if (!reporter.hasError) {
              localStore = CComponentPlugin.putCComponentR2U2Contributions(thread.path, contributions, localStore)
            }
          }
        case _ =>
      }
    }
    return (localStore, ISZ())
  }
}

@datatype class DefaultGumboCPlugin extends GumboCPlugin {
  @strictpure override def name: String = "DefaultGumboCPlugin"
}
