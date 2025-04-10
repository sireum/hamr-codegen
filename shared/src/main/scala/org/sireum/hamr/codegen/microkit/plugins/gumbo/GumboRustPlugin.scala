// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, DataIdPath, ISZValue, Store, StoreValue, ThreadIdPath}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitInitPlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.plugins.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.linters.MicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.plugins.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.ir.{Aadl, Direction, GclAssume, GclGuarantee, GclSubclause}
import org.sireum.message.{Level, Message, Reporter}
import org.sireum.hamr.codegen.microkit.{MicrokitCodegen, rust => RAST}

object GumboRustPlugin {
  val KEY_GumboRustPlugin: String = "KEY_GumboRustPlugin"
  val KEY_twc: String = s"${KEY_GumboRustPlugin}_threads_with_contracts"
  val KEY_dwc: String = s"${KEY_GumboRustPlugin}_datatypes_with_contracts"

  @strictpure def getGumboRustContributions(store: Store): Option[GumboRustContributions] = store.get(KEY_GumboRustPlugin).asInstanceOf[Option[GumboRustContributions]]
  @strictpure def getThreadsWithContracts(store: Store): ISZ[ThreadIdPath] = store.getOrElse(KEY_twc, ISZValue[ThreadIdPath](ISZ())).asInstanceOf[ISZValue[ThreadIdPath]].elements
  @strictpure def getDatatypesWithContracts(store: Store): ISZ[DataIdPath] = store.getOrElse(KEY_dwc, ISZValue[DataIdPath](ISZ())).asInstanceOf[ISZValue[DataIdPath]].elements

  @strictpure def putGumboRustContributions(contributions: GumboRustContributions, store: Store): Store = store + KEY_GumboRustPlugin ~> contributions
  @strictpure def putThreadsWithContracts(i: ISZ[ThreadIdPath], store: Store): Store = store + KEY_twc ~> ISZValue(i)
  @strictpure def putDatatypesWithContracts(i: ISZ[DataIdPath], store: Store): Store = store + KEY_dwc ~> ISZValue(i)
}

@sig trait GumboRustContributions extends StoreValue {
  @pure def datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]]
}

@datatype class DefaultGumboRustContributions(val datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]]) extends GumboRustContributions

@sig trait GumboRustPlugin extends MicrokitInitPlugin with MicrokitPlugin {

  @pure override def init(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): Store = {
    var localStore = store
    if (options.platform != HamrCli.CodegenHamrPlatform.Microkit ||
      !ops.ISZOps(symbolTable.getThreads()).exists(p => Util.isRusty(p))) {
      return localStore
    }
    var threadsWithContracts: ISZ[ThreadIdPath] = ISZ()
    var datatypesWithContracts: ISZ[DataIdPath] = ISZ()
    for (t <- symbolTable.getThreads() if Util.isRusty(t)) {
      if(ops.ISZOps(t.annexes()).exists(p => p.clause.isInstanceOf[GclSubclause])) {
        threadsWithContracts = threadsWithContracts :+ t.path
      }
    }

    val touchedTypes = MicrokitLinterPlugin.getTouchedTypes(localStore)
    var errorMsgs: ISZ[Message] = ISZ()
    for (typeName <- touchedTypes.orderedDependencies) {
      val v = types.typeMap.get(typeName).get
      if (typeName == "Base_Types::Float" || typeName == "Base_Types::Float_32" || typeName == "BaseTypes::Float_64") {
        errorMsgs = errorMsgs :+ Message(Level.Error, v.container.get.identifier.pos, MicrokitCodegen.toolName, "Model contains contract and uses floats/reals that are not supported by Verus")
      }
      if (ops.ISZOps(v.container.get.annexes).exists(a => a.clause.isInstanceOf[GclSubclause])) {
        datatypesWithContracts = datatypesWithContracts :+ v.classifier
      }
    }

    // if gumbo contracts will be integrated then disallow models that touch floats/reals
    // (probably more efficient to lint gumbo contracts here rather than implementing a gumbo lint plugin)
    if (threadsWithContracts.nonEmpty || datatypesWithContracts.nonEmpty) {
      reporter.reports(errorMsgs)
    }

    return (
      GumboRustPlugin.putThreadsWithContracts(threadsWithContracts,
        GumboRustPlugin.putDatatypesWithContracts(datatypesWithContracts, localStore)))
  }

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !store.contains(GumboRustPlugin.KEY_GumboRustPlugin) &&
      // may need the app contributions if we need to insert method-level contracts so
      // wait until that phase is done.  The component plugin depends on the typing and
      // api plugins so we can also add datatype invariants and/or integration constraints
      // if needed
      CRustComponentPlugin.hasCRustComponentContributions(store) &&
      //
      (GumboRustPlugin.getThreadsWithContracts(store).nonEmpty ||
        GumboRustPlugin.getDatatypesWithContracts(store).nonEmpty)

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store

    var datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]] = Map.empty
    val datatypesWithContracts = GumboRustPlugin.getDatatypesWithContracts(localStore)
    if (datatypesWithContracts.nonEmpty) {
      for (d <- datatypesWithContracts) {
        val aadlType = types.getTypeByPath(d)
        reporter.warn(aadlType.container.get.identifier.pos, MicrokitCodegen.toolName,
          s"TODO: Need to handle datatype invariants for ${aadlType.name}")
      }
    }

    for (threadId <- GumboRustPlugin.getThreadsWithContracts(localStore)) {
      var markers: ISZ[Marker] = ISZ()
      val thread = symbolTable.componentMap.get(threadId).get.asInstanceOf[AadlThread]
      val subclauseInfo = GumboRustUtil.getGumboSubclause(threadId, symbolTable)
      val componentContributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
      val threadContributions = componentContributions.componentContributions.get(threadId).get
      var structDef = threadContributions.appStructDef
      val structImpl = threadContributions.appStructImpl.asInstanceOf[RAST.ImplBase]

      var optStateVarInits: ISZ[ST] = ISZ()
      if (subclauseInfo.annex.state.nonEmpty) {
        var stateVars: ISZ[RAST.StructField] = ISZ()
        for(sv <- subclauseInfo.annex.state) {
          val i = GumboRustUtil.processStateVariable(sv, types, CRustTypePlugin.getCRustTypeProvider(localStore).get)
          stateVars = stateVars :+ i._1
          optStateVarInits = optStateVarInits :+ i._2
        }
        val m = Marker.createMarker("STATE VARS")
        markers = markers :+ m
        structDef = structDef(items = structDef.items :+ RAST.StructFieldsMarker(m, stateVars))
      }

      if (subclauseInfo.annex.integration.nonEmpty) {
        val (requires, ensures) = handleIntegrationConstraints(thread, subclauseInfo, types, reporter)

        val crustApiContributions = CRustApiPlugin.getCRustApiContributions(localStore).get
        val componentApiContributions = crustApiContributions.apiContributions.get(threadId).get

        var updatedPutApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.appApiDefaultPutters) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("put_", "")
              if (requires.contains(portId)) {
                val require = requires.get(portId).get
                updatedPutApis = updatedPutApis :+
                  fn(contract = Some(fn.contract.get(requires = fn.contract.get.requires :+ require)))
              } else {
                updatedPutApis = updatedPutApis :+ fn
              }
            case _ => updatedPutApis = updatedPutApis :+ u
          }
        }

        var updatedGetApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.appApiDefaultGetters) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("get_", "")
              if (ensures.contains(portId)) {
                val ensure = ensures.get(portId).get
                updatedGetApis = updatedGetApis :+
                  fn(contract = Some(fn.contract.get(ensures = fn.contract.get.ensures :+ ensure)))
              } else {
                updatedGetApis = updatedGetApis :+ fn
              }
            case _ => updatedGetApis = updatedGetApis :+ u
          }
        }

        val con = crustApiContributions.addApiContributions(threadId,
          componentApiContributions(
            appApiDefaultPutters = updatedPutApis,
            appApiDefaultGetters = updatedGetApis))

        localStore = CRustApiPlugin.putCRustApiContributions(con, localStore)
      }

      var updatedImplItems: ISZ[RAST.Item] = ISZ()
      for (i <- structImpl.items) {
        i match {
          case f: RAST.FnImpl =>
            if (f.ident.string == "new" && optStateVarInits.nonEmpty) {
              val b: Option[RAST.MethodBody] = f.body match {
                case Some(RAST.MethodBody(ISZ(self: RAST.BodyItemSelf))) =>
                  val m = Marker.createMarker("STATE VAR INIT")
                  markers = markers :+ m
                  val wrapper =
                    st"""${m.beginMarker}
                        |${(optStateVarInits, ",\n")}
                        |${m.endMarker}"""
                  Some(RAST.MethodBody(ISZ(self(items = self.items :+ wrapper))))
                case Some(x) => halt("Not expecting new to contain anything other than Self {...}")
                case _ => Some(RAST.MethodBody(ISZ(RAST.BodyItemSelf(optStateVarInits))))
              }
              updatedImplItems = updatedImplItems :+ f(body = b)
            }
            else if (f.ident.string == "initialize" && subclauseInfo.annex.initializes.nonEmpty) {
              val init = handleInitialize(f, thread, subclauseInfo, types, symbolTable, reporter)
              markers = markers :+ init._1
              updatedImplItems = updatedImplItems :+ init._2
            } else if (f.ident.string == "timeTriggered" && subclauseInfo.annex.compute.nonEmpty) {
              val tt = handleCompute(f, thread, subclauseInfo, types, symbolTable, reporter)
              markers = markers ++ tt._1
              updatedImplItems = updatedImplItems :+ tt._2
            } else {
              updatedImplItems = updatedImplItems :+ i
            }
          case _ => updatedImplItems = updatedImplItems :+ i
        }
      }

      localStore = CRustComponentPlugin.putComponentContributions(localStore,
        componentContributions.replaceComponentContributions(
          componentContributions.componentContributions + threadId ~>
            threadContributions(
              markers = markers,
              requiresVerus = T,
              appStructDef = structDef,
              appStructImpl = structImpl(items = updatedImplItems))))
    }

    return (
      GumboRustPlugin.putGumboRustContributions(DefaultGumboRustContributions(datatypeInvariants), localStore),
      ISZ())
  }

  @pure def handleIntegrationConstraints(thread: AadlThread, subclauseInfo: GclAnnexClauseInfo, types: AadlTypes, reporter: Reporter): (Map[String, RAST.Expr], Map[String, RAST.Expr]) = {
    var requires: Map[String, RAST.Expr] = Map.empty
    var ensures: Map[String, RAST.Expr] = Map.empty

    for (p <- thread.getPorts()) {
      val (aadlType, isEvent, isData): (AadlType, B, B) = p match {
        case i: AadlEventDataPort => (i.aadlType, T, T)
        case i: AadlDataPort => (i.aadlType, F, T)
        case i: AadlEventPort => halt("Need to handle event ports")
        case x => halt("Unexpected port type: $x")
      }

      subclauseInfo.gclSymbolTable.integrationMap.get(p) match {
        case Some(spec) =>
          spec match {
            case a: GclAssume =>
              // assume integration clauses can only be applied to incoming ports.  The api therefore
              // ensures that the getter's return value will satisfy the assume clause.

              if (isEvent) {
                halt("Need to handle event ports")
              } else {
                val subs = Map.empty[String, String] + p.identifier ~> s"${CRustApiPlugin.apiResultName}"
                ensures = ensures + p.identifier ~> GumboRustUtil.processGumboSpecH(a, subs, F, types, subclauseInfo.gclSymbolTable, reporter)
                reporter.warn(p.feature.identifier.pos, MicrokitCodegen.toolName, s"TODO: Need to add type invariant for ${thread.identifier}'s ${p.identifier} ghost variable")
              }

            case g: GclGuarantee =>
            // guarantee integration clauses can only be applied to outgoing ports.  They become
            // requirements on the param value passed to the api -- the param's name will always be 'value'

              val subs = Map.empty[String, String] + p.identifier ~> s"${CRustApiPlugin.apiParameterName}"
              requires = requires + p.identifier ~> GumboRustUtil.processGumboSpecH(g, subs, F, types, subclauseInfo.gclSymbolTable, reporter)
              reporter.warn(p.feature.identifier.pos, MicrokitCodegen.toolName, s"TODO: Need to add type invariant for ${thread.identifier}'s ${p.identifier} ghost variable")
          }
        case _ =>
      }
    }
    return (requires, ensures)
  }

  @pure def handleInitialize(fn: RAST.FnImpl, thread: AadlThread, subclauseInfo: GclAnnexClauseInfo,
                             types: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): (Marker, RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    val ensures: ISZ[RAST.Expr] = for (g <- subclauseInfo.annex.initializes.get.guarantees) yield
      GumboRustUtil.processGumboSpec(g, F, types, subclauseInfo.gclSymbolTable, reporter)

    val ensuresMarker = Marker.createMarker("INITIALIZATION ENSURES")
    return (ensuresMarker, fn(contract = Some(RAST.FnContract(
      optRequiresMarker = None(),
      requires = ISZ(),
      optEnsuresMarker = Some(ensuresMarker),
      ensures = ensures))))
  }

  @pure def handleCompute(fn: RAST.FnImpl, thread: AadlThread, subclauseInfo: GclAnnexClauseInfo,
                          types: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): (ISZ[Marker], RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    // general assumes clauses
    var requires: ISZ[RAST.Expr] =
      for (r <- subclauseInfo.annex.compute.get.specs.filter(p => p.isInstanceOf[GclAssume])) yield
        GumboRustUtil.processGumboSpec(r, T, types, subclauseInfo.gclSymbolTable, reporter)

    var aadlReq: ISZ[ST] = ISZ()
    for (p <- thread.getPorts() if !p.isInstanceOf[AadlDataPort] && p.direction == Direction.Out) {
      aadlReq = aadlReq :+ (st"old(api).${p.identifier}.is_none()")
    }
    if (aadlReq.nonEmpty) {
      requires = RAST.ExprST(st"""// assume AADL_Requirement
                                 |//   All outgoing event ports must be empty
                                 |${(aadlReq, ",\n")}""") +: requires
    }

    val ensures: ISZ[RAST.Expr] = {
      // general ensures clauses
      (for (r <- subclauseInfo.annex.compute.get.specs.filter(p => p.isInstanceOf[GclGuarantee])) yield
        GumboRustUtil.processGumboSpec(r, F, types, subclauseInfo.gclSymbolTable, reporter)) ++
        // gumbo compute cases clauses
        (for (c <- subclauseInfo.annex.compute.get.cases) yield
          GumboRustUtil.processGumboCase(c, types, subclauseInfo.gclSymbolTable, reporter))
    }

    var optEnsuresMarker: Option[Marker] = None()
    var optRequiresMarker: Option[Marker] = None()
    var markers: ISZ[Marker] = ISZ()
    if (requires.nonEmpty) {
      val m = Marker.createMarker("TIME TRIGGERED REQUIRES")
      markers = markers :+ m
      optRequiresMarker = Some(m)
    }
    if (ensures.nonEmpty) {
      val m = Marker.createMarker("TIME TRIGGERED ENSURES")
      markers = markers :+ m
      optEnsuresMarker = Some(m)
    }
    return (markers,
      fn(contract = Some(RAST.FnContract(
        optRequiresMarker = optRequiresMarker,
        requires = requires,
        optEnsuresMarker = optEnsuresMarker,
        ensures = ensures))))
  }
}

@datatype class DefaultGumboRustPlugin extends GumboRustPlugin {
  @strictpure override def name: String = "DefaultGumboRustPlugin"
}

