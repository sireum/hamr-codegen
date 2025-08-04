// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, DataIdPath, IdPath, PortIdPath, Store, StoreValue, ThreadIdPath}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlThread, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.microkit.plugins.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.GumboXRustUtil.{GGParam, GGPortParam, SymbolKind, paramsToComment, sortParams}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypePlugin, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.ir.{Aadl, GclAssume, GclGuarantee, GclSubclause}
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RAST}

object GumboXRustPlugin {

  val KEY_GumboXPlugin: String = "KEY_GUMBOX_PLUGIN"

  @strictpure def getGumboXContributions(store: Store): Option[GumboXContributions] = store.get(KEY_GumboXPlugin).asInstanceOf[Option[GumboXContributions]]

  @strictpure def putGumboXContributions(contributions: GumboXContributions, store: Store): Store = store + KEY_GumboXPlugin ~> contributions
}

@sig trait GumboXContributions extends StoreValue {
  @pure def componentContributions: Map[ThreadIdPath, GumboXComponentContributions]
}

@datatype class DefaultGumboXContributions(val componentContributions: Map[ThreadIdPath, GumboXComponentContributions]) extends GumboXContributions

object GumboXComponentContributions {
  @strictpure def empty: GumboXComponentContributions = GumboXComponentContributions(
    Map.empty,
    InitializeContributions.empty,
    ComputeContributions.empty,
    ISZ())
}

object InitializeContributions {
  @strictpure def empty: InitializeContributions = InitializeContributions(ISZ(), ISZ())
}

@datatype class InitializeContributions(val IEP_Post_Params: ISZ[GGParam],
                                        val IEP_Guarantee: ISZ[RAST.Fn])

object ComputeContributions {
  @strictpure def empty: ComputeContributions = ComputeContributions(ISZ(), ISZ(), ISZ(), ISZ(), None(), ISZ(), None())
}

@datatype class ComputeContributions(val CEP_T_Assum__methods: ISZ[RAST.Fn],
                                     val CEP_T_Guar__methods: ISZ[RAST.Fn],
                                     val CEP_T_Case__methods: ISZ[RAST.Fn],

                                     val CEP_Pre_Params: ISZ[GGParam],
                                     val CEP_Pre: Option[RAST.Fn],

                                     val CEP_Post_Params: ISZ[GGParam],
                                     val CEP_Post: Option[RAST.Fn])

@datatype class GumboXComponentContributions(val integrationConstraints: Map[PortIdPath, ISZ[RAST.Fn]],

                                             // init contributions
                                             val initializeContributions: InitializeContributions,

                                             // compute contributions
                                             val computeContributions: ComputeContributions,

                                             //
                                             val gumboMethods: ISZ[RAST.Fn])


@sig trait GumboXRustPlugin extends MicrokitPlugin with MicrokitFinalizePlugin {

  @strictpure def haveHandled(store: Store): B = store.contains(GumboXRustPlugin.KEY_GumboXPlugin)

  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_${name}")

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !haveHandled(store) &&
      CRustTypePlugin.hasCRustTypeProvider(store) &&
      // gumbo rust plugin provides datatype invariant rust methods
      GumboRustPlugin.getGumboRustContributions(store).nonEmpty &&
      (GumboRustPlugin.getThreadsWithContracts(store).nonEmpty ||
        GumboRustPlugin.getDatatypesWithContracts(store).nonEmpty)

  @strictpure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !alreadyFinalized(store) &&
      GumboXRustPlugin.getGumboXContributions(store).nonEmpty &&
      // will need to add the gumboX module to the bridge's mod.rs file
      CRustApiPlugin.getCRustApiContributions(store).nonEmpty

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption,
                            types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var resources: ISZ[Resource] = ISZ()
    var localStore = store
    val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

    var componentContributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
    var apiContributions = CRustApiPlugin.getCRustApiContributions(localStore).get

    val datatypesWithContracts = GumboRustPlugin.getDatatypesWithContracts(localStore)
    val componentsWithContracts = GumboRustPlugin.getThreadsWithContracts(localStore)

    val datatypeInvariants = GumboRustPlugin.getGumboRustContributions(localStore).get.datatypeInvariants
    var items: Map[ThreadIdPath, GumboXComponentContributions] = Map.empty

    for (thread <- symbolTable.getThreads() if Util.isRusty(thread)) {
      assert(!items.contains(thread.path), "Not expecting anyone else to have made gumbox contributions up to this point")

      if (datatypesWithContracts.nonEmpty || ops.ISZOps(componentsWithContracts).contains(thread.path)) {
        val subclauseInfoOpt = GumboRustUtil.getGumboSubclauseOpt(thread.path, symbolTable)

        val integrationConstraints = processIntegrationConstraints(thread, subclauseInfoOpt, crustTypeProvider, types, localStore, reporter)

        val initializeContributions = processInitialize(thread, datatypeInvariants, integrationConstraints, subclauseInfoOpt, crustTypeProvider, types, localStore, reporter)

        val computeContributions = processCompute(thread, datatypeInvariants, integrationConstraints, subclauseInfoOpt, crustTypeProvider, types, localStore, reporter)

        val gumboMethods = processGumboMethods(thread, subclauseInfoOpt, crustTypeProvider, types, localStore, reporter)

        { // update the generated apis to include GUMBOX artifacts

          val testingApiContributions = buildGumboxTestMethods(thread, initializeContributions, computeContributions, subclauseInfoOpt, crustTypeProvider, types, localStore, reporter)

          val apiComponentContributions = apiContributions.apiContributions.get(thread.path).get

          val updated = apiComponentContributions(
            testingApis =
              RAST.Use(attributes = ISZ(),
                ident = RAST.IdentString(s"crate::bridge::${Util.getThreadIdPath(thread)}_GUMBOX as GUMBOX")) +:
                (apiComponentContributions.testingApis ++ testingApiContributions._1),

            // modify bridge/mod.rs to add the GUMBOX module
            bridgeModuleContributions =
              apiComponentContributions.bridgeModuleContributions :+
                RAST.ItemST(st"pub mod ${Util.getThreadIdPath(thread)}_GUMBOX;")
          )

          apiContributions = apiContributions.addApiContributions(thread.path, updated)

          // add gumbox test module to tests.rs
          val existingComponentContributions = componentContributions.componentContributions.get(thread.path).get
          componentContributions = componentContributions.replaceComponentContributions(
            componentContributions.componentContributions + thread.path ~>
              existingComponentContributions(testEntries =
              existingComponentContributions.testEntries ++ testingApiContributions._2)
          )
        }

        items = items + thread.path ~> GumboXComponentContributions(
          integrationConstraints = integrationConstraints,
          initializeContributions = initializeContributions,
          computeContributions = computeContributions,
          gumboMethods = gumboMethods)
      }
    }

    localStore = CRustApiPlugin.putCRustApiContributions(apiContributions, localStore)
    localStore = CRustComponentPlugin.putComponentContributions(componentContributions, localStore)

    return (GumboXRustPlugin.putGumboXContributions(DefaultGumboXContributions(items), localStore), resources)
  }

  @pure def processGumboMethods(thread: AadlThread,
                                subclauseInfoOpt: Option[GclAnnexClauseInfo],
                                crustTypeProvider: CRustTypeProvider,
                                types: AadlTypes,
                                store: Store,
                                reporter: Reporter): ISZ[RAST.Fn] = {
    subclauseInfoOpt match {
      case Some(c) =>
        var ret: ISZ[RAST.Fn] = ISZ()
        for (m <- c.annex.methods) {
          ret = ret :+ GumboRustUtil.processGumboMethod(
            m = m,
            context = thread,
            inVerus = F,
            aadlTypes = types,
            tp = crustTypeProvider,
            gclSymbolTable = c.gclSymbolTable,
            store = store,
            reporter = reporter)
        }
        return ret
      case _ => return ISZ()
    }
  }

  @pure def processIntegrationConstraints(thread: AadlThread,
                                          subclauseInfoOpt: Option[GclAnnexClauseInfo],
                                          crustTypeProvider: CRustTypeProvider,
                                          types: AadlTypes,
                                          store: Store,
                                          reporter: Reporter): Map[PortIdPath, ISZ[RAST.Fn]] = {
    var ret: Map[PortIdPath, ISZ[RAST.Fn]] = Map.empty

    subclauseInfoOpt match {
      case Some(GclAnnexClauseInfo(_, gclSymbolTable)) =>
        for (port <- thread.getPorts() if gclSymbolTable.integrationMap.contains(port)) {
          val (aadlType, portDescription) = GumboXRustUtil.getPortInfo(port)
          val typeNameProvider = crustTypeProvider.getTypeNameProvider(aadlType)

          val clause = gclSymbolTable.integrationMap.get(port).get
          val rewrittenExp = SlangExpUtil.rewriteExp(
            rexp = SlangExpUtil.getRexp(clause.exp, gclSymbolTable),
            context = thread,

            // integration GclGuarantees become verus requires clauses and
            // integration GclAssumes become verus ensures clauses
            inRequires = clause.isInstanceOf[GclAssume],

            inVerus = F,
            tp = crustTypeProvider,
            aadlTypes = types,
            store = store,
            reporter = reporter)

          val (i_name, i_assum_guard_name) = GumboXRustUtil.createIntegrationMethodName(port)
          clause match {
            case i: GclAssume =>
              val I_Assum = RAST.FnImpl(
                comments = ISZ(RAST.CommentST(
                  st"""/** I-Assm: Integration constraint on ${thread.identifier}'s incoming $portDescription port ${port.identifier}
                      |  *
                      |  * assume ${i.id}
                      |  ${GumboRustUtil.processDescriptor(i.descriptor, "*   ")}
                      |  */""")),
                sig = RAST.FnSig(
                  ident = RAST.IdentString(i_name),
                  fnDecl = RAST.FnDecl(
                    inputs = ISZ(RAST.ParamImpl(
                      ident = RAST.IdentString(port.identifier),
                      kind = RAST.TyPath(ISZ(typeNameProvider.qualifiedRustNameS), Some(aadlType.classifier)))),
                    outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                  verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
                attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
                body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(rewrittenExp)))),
                meta = ISZ(RAST.MetaOrigin(port.path)))

              if (port.isInstanceOf[AadlDataPort]) {
                ret = ret + port.path ~> ISZ(I_Assum)
              } else {
                halt("Need to handle event ports")
              }
            case i: GclGuarantee =>
              val I_Assum = RAST.FnImpl(
                comments = ISZ(RAST.CommentST(
                  st"""/** I-Guar: Integration constraint on ${thread.identifier}'s outgoing $portDescription port ${port.identifier}
                      |  *
                      |  * guarantee ${i.id}
                      |  ${GumboRustUtil.processDescriptor(i.descriptor, "*  ")}
                      |  */""")),
                sig = RAST.FnSig(
                  ident = RAST.IdentString(i_name),
                  fnDecl = RAST.FnDecl(
                    inputs = ISZ(RAST.ParamImpl(
                      ident = RAST.IdentString(port.identifier),
                      kind = RAST.TyPath(ISZ(typeNameProvider.qualifiedRustNameS), Some(aadlType.classifier)))),
                    outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                  verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
                attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
                body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(rewrittenExp)))),
                meta = ISZ(RAST.MetaOrigin(port.path)))

              if (port.isInstanceOf[AadlDataPort]) {
                ret = ret + port.path ~> ISZ(I_Assum)
              } else {
                halt("Need to handle event ports")
              }
            case _ => halt("Infeasible")
          }
        }
      case _ =>
    }
    return ret
  }

  @pure def processInitialize(thread: AadlThread,
                              datatypeInvariants: Map[PortIdPath, ISZ[RAST.Fn]],
                              integrationConstraints: Map[PortIdPath, ISZ[RAST.Fn]],
                              subclauseInfoOpt: Option[GclAnnexClauseInfo],
                              crustTypeProvider: CRustTypeProvider,
                              types: AadlTypes,
                              store: Store,
                              reporter: Reporter): InitializeContributions = {

    var IEP_Guarantee: ISZ[RAST.Fn] = ISZ()
    var IEP_Guarantee_Params: Set[GGParam] = Set.empty[GGParam] ++
      GumboXRustUtil.outPortsToParams(thread, crustTypeProvider) ++
      GumboXRustUtil.stateVarsToParams(subclauseInfoOpt, F, types, crustTypeProvider)

    subclauseInfoOpt match {
      case Some(GclAnnexClauseInfo(GclSubclause(stateVars, _, _, Some(initializes), _, _), gclSymbolTable)) if initializes.guarantees.nonEmpty =>

        var combinedSpecCalls: ISZ[ST] = ISZ()

        for (guarantee <- initializes.guarantees) {

          val gg = GumboXRustUtil.rewriteToExpX(SlangExpUtil.getRexp(guarantee.exp, gclSymbolTable), thread, types, stateVars, crustTypeProvider)
          val rewrittenExp = SlangExpUtil.rewriteExp(
            rexp = gg.exp,
            context = thread,
            inRequires = F,
            inVerus = F,
            tp = crustTypeProvider,
            aadlTypes = types,
            store = store,
            reporter = reporter)

          IEP_Guarantee_Params = IEP_Guarantee_Params ++ gg.params.elements
          val sortedParams = GumboXRustUtil.sortParams(gg.params.elements)

          val methodName = GumboXRustUtil.getInitializeGuaranteeMethodName(guarantee.id)

          combinedSpecCalls = combinedSpecCalls :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          IEP_Guarantee = IEP_Guarantee :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** Initialize EntryPointContract
                  |  *
                  |  * guarantee ${guarantee.id}
                  |  ${GumboRustUtil.processDescriptor(guarantee.descriptor, "*   ")}
                  |  ${(GumboXRustUtil.paramsToComment(sortedParams), "\n")}
                  |  */""")),
            sig = RAST.FnSig(
              ident = RAST.IdentString(methodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sortedParams) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(rewrittenExp)))),
            meta = ISZ())
        }

        val sorted_IEP_Guar_Params = GumboXRustUtil.sortParams(IEP_Guarantee_Params.elements)

        // create IEP-Guar that calls the individual guarantee clauses
        val combinedInitializeMethodName = GumboXRustUtil.getInitialize_IEP_Guar_MethodName

        IEP_Guarantee = IEP_Guarantee :+ RAST.FnImpl(
          comments = ISZ(RAST.CommentST(
            st"""/** IEP-Guar: Initialize Entrypoint for ${thread.identifier}
                |  *
                |  ${(GumboXRustUtil.paramsToComment(sorted_IEP_Guar_Params), "\n")}
                |  */""")),
          sig = RAST.FnSig(
            ident = RAST.IdentString(combinedInitializeMethodName),
            fnDecl = RAST.FnDecl(
              inputs = for (p <- sorted_IEP_Guar_Params) yield p.toRustParam,
              outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
            verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
          attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
          body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(st"${(combinedSpecCalls, " &&\n")}")))),
          meta = ISZ())
      case _ =>
    }

    var I_Guar_Guar_Params: Set[GGParam] = Set.empty
    var I_Guar_Guard: ISZ[ST] = ISZ()

    { // integration constraints
      for (entry <- integrationConstraints.entries) {
        val port = thread.getPortByPath(entry._1).get
        // initializes clause so only care about outgoing ports
        if (GumboXRustUtil.isOutPort(port)) {
          val isEvent = !port.isInstanceOf[AadlDataPort]

          val method = entry._2(entry._2.lastIndex)

          val typ = MicrokitTypeUtil.getPortType(port)
          val param = GGPortParam(
            port = port,
            aadlType = typ.classifier,
            typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))

          I_Guar_Guar_Params = I_Guar_Guar_Params + param
          I_Guar_Guard = I_Guar_Guard :+ st"${method.ident.prettyST}(${param.name})"
        }
      }
    }


    val IEP_Post_Params: Set[GGParam] = I_Guar_Guar_Params ++ IEP_Guarantee_Params.elements
    val sorted_IEP_Post_Params = GumboXRustUtil.sortParams(IEP_Post_Params.elements)

    var D_Inv_Guards: ISZ[ST] = ISZ()
    if (datatypeInvariants.nonEmpty) {
      for (sortedParam <- sorted_IEP_Post_Params if datatypeInvariants.contains(sortedParam.aadlType)) {
        halt("Need to handle datatype invariants for gumbox init clauses")
      }
    }

    // create IEP-Post that:
    //   - calls I-Guar-Guard for each of the output ports of the component
    //   - calls D-Inv-Guard for each datatype associated with the output ports
    //   - calls IEP-Guard

    var bodySegments: ISZ[ST] = ISZ()
    if (D_Inv_Guards.nonEmpty) {
      bodySegments = bodySegments :+
        st"""// D-Inv-Guard: Datatype invariants for the types associated with ${thread.identifier}'s state variables and outgoing ports
            |${(D_Inv_Guards, " &\n")}"""
    }
    if (I_Guar_Guard.nonEmpty) {
      bodySegments = bodySegments :+
        st"""// I-Guar-Guard: Integration constraints for ${thread.identifier}'s outgoing ports"
            |${(I_Guar_Guard, " &\n")}"""
    }
    if (IEP_Guarantee.nonEmpty) {
      // call the iep_guar method
      assert(IEP_Guarantee.size >= 2)
      val iep_guar_method = IEP_Guarantee(IEP_Guarantee.lastIndex)

      bodySegments = bodySegments :+
        st"${iep_guar_method.ident.prettyST}(${(for (p <- iep_guar_method.sig.fnDecl.inputs) yield p.ident.prettyST, ", ")})"
    }

    if (bodySegments.nonEmpty) {
      val iepPostMethodName = GumboXRustUtil.getInitialize_IEP_Post_MethodName
      IEP_Guarantee = IEP_Guarantee :+ RAST.FnImpl(
        comments = ISZ(RAST.CommentST(
          st"""/** IEP-Post: Initialize Entrypoint Post-Condition
              |  *
              |  ${(paramsToComment(sorted_IEP_Post_Params), "\n")}
              |  */""")),
        sig = RAST.FnSig(
          ident = RAST.IdentString(iepPostMethodName),
          fnDecl = RAST.FnDecl(
            inputs = for (p <- sorted_IEP_Post_Params) yield p.toRustParam,
            outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(st"""${(bodySegments, "&& \n\n")}""")))),
        meta = ISZ())
    }
    return InitializeContributions(sorted_IEP_Post_Params, IEP_Guarantee)
  }


  @pure def processCompute(thread: AadlThread,
                           datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]],
                           integrationConstraints: Map[PortIdPath, ISZ[RAST.Fn]],
                           subclauseInfoOpt: Option[GclAnnexClauseInfo],
                           crustTypeProvider: CRustTypeProvider,
                           types: AadlTypes,
                           store: Store,
                           reporter: Reporter): ComputeContributions = {

    var CEP_T_Assm__methods: ISZ[RAST.Fn] = ISZ()
    var CEP_T_Guar__methods: ISZ[RAST.Fn] = ISZ()
    var CEP_T_Case__methods: ISZ[RAST.Fn] = ISZ()
    var CEP_Pre: Option[RAST.Fn] = None()
    var CEP_Post: Option[RAST.Fn] = None()


    var CEP_Pre_Params: Set[GGParam] = Set.empty[GGParam] ++
      GumboXRustUtil.inPortsToParams(thread, crustTypeProvider) ++
      GumboXRustUtil.stateVarsToParams(subclauseInfoOpt, T, types, crustTypeProvider)

    var CEP_Post_Params: Set[GGParam] = Set.empty[GGParam] ++
      CEP_Pre_Params.elements ++
      GumboXRustUtil.outPortsToParams(thread, crustTypeProvider) ++
      GumboXRustUtil.stateVarsToParams(subclauseInfoOpt, F, types, crustTypeProvider)

    subclauseInfoOpt match {
      case Some(GclAnnexClauseInfo(GclSubclause(stateVars, _, _, _, _, Some(compute)), gclSymbolTable)) => { // process top level assume/guarantees

        var CEP_T_Assum_Params: Set[GGParam] = Set.empty

        var topLevelAssumeCallsCombined: ISZ[ST] = ISZ()

        var CEP_T_Guar_Params: Set[GGParam] = Set.empty

        var topLevelGuaranteesCombined: ISZ[ST] = ISZ()

        for (g <- compute.assumes) {
          val gg = GumboXRustUtil.rewriteToExpX(
            SlangExpUtil.getRexp(g.exp, gclSymbolTable),
            thread,
            types,
            stateVars,
            crustTypeProvider)

          val rexp = SlangExpUtil.rewriteExp(
            rexp = gg.exp,
            context = thread,
            inRequires = T,
            inVerus = F,
            tp = crustTypeProvider,
            aadlTypes = types,
            store = store,
            reporter = reporter)

          val methodName = s"compute_spec_${g.id}_assume"

          CEP_T_Assum_Params = CEP_T_Assum_Params ++ gg.params.elements

          val sortedParams = GumboXRustUtil.sortParams(gg.params.elements)

          topLevelAssumeCallsCombined = topLevelAssumeCallsCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          CEP_T_Assm__methods = CEP_T_Assm__methods :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** Compute Entrypoint Contract
                  |  *
                  |  * assumes ${g.id}
                  |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                  |  ${(paramsToComment(sortedParams), "\n")}
                  |  */""")),
            sig = RAST.FnSig(
              ident = RAST.IdentString(methodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sortedParams) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(rexp)))),
            meta = ISZ())
        }

        for (g <- compute.guarantees) {
          val gg = GumboXRustUtil.rewriteToExpX(SlangExpUtil.getRexp(g.exp, gclSymbolTable), thread, types, stateVars, crustTypeProvider)
          val rexp = SlangExpUtil.rewriteExp(
            rexp = gg.exp,
            context = thread,
            inRequires = F,
            inVerus = F,
            tp = crustTypeProvider,
            aadlTypes = types,
            store = store,
            reporter = reporter)

          val methodName = s"compute_spec_${g.id}_guarantee"

          CEP_T_Guar_Params = CEP_T_Guar_Params ++ gg.params.elements

          val sortedParams = GumboXRustUtil.sortParams(gg.params.elements)

          topLevelGuaranteesCombined = topLevelGuaranteesCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

          CEP_T_Guar__methods = CEP_T_Guar__methods :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** Compute Entrypoint Contract
                  |  *
                  |  * guarantee ${g.id}
                  |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                  |  ${(paramsToComment(sortedParams), "\n")}
                  |  */"""
            )),
            sig = RAST.FnSig(
              ident = RAST.IdentString(methodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sortedParams) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(rexp)))),
            meta = ISZ())
        }

        if (CEP_T_Assm__methods.nonEmpty) {

          CEP_Pre_Params = CEP_Pre_Params ++ CEP_T_Assum_Params.elements

          val sorted_CEP_T_Assm_Params = sortParams(CEP_T_Assum_Params.elements)
          val CEP_T_Assm_MethodName = GumboXRustUtil.getCompute_CEP_T_Assm_MethodName
          CEP_T_Assm__methods = CEP_T_Assm__methods :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** CEP-T-Assm: Top-level assume contracts for ${thread.identifier}'s compute entrypoint
                  |  *
                  |  ${(paramsToComment(sorted_CEP_T_Assm_Params), "\n")}
                  |  */""")),
            sig = RAST.FnSig(
              ident = RAST.IdentString(CEP_T_Assm_MethodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sorted_CEP_T_Assm_Params) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""${(for (i <- 0 until topLevelAssumeCallsCombined.size) yield st"let r$i: bool = ${topLevelAssumeCallsCombined(i)};", "\n")}
                  |
                  |return ${(for (i <- 0 until topLevelAssumeCallsCombined.size) yield s"r$i", " && ")};""")))),
            meta = ISZ())
        }

        if (CEP_T_Guar__methods.nonEmpty) {
          val sorted_CEP_T_Guar_Params = sortParams(CEP_T_Guar_Params.elements)
          val CEP_T_Guar_MethodName = GumboXRustUtil.getCompute_CEP_T_Guar_MethodName
          CEP_T_Guar__methods = CEP_T_Guar__methods :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** CEP-T-Guar: Top-level guarantee contracts for ${thread.identifier}'s compute entrypoint
                  |  *
                  |  ${(paramsToComment(sorted_CEP_T_Guar_Params), "\n")}
                  |  */"""
            )),
            sig = RAST.FnSig(
              ident = RAST.IdentString(CEP_T_Guar_MethodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sorted_CEP_T_Guar_Params) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""${(for (i <- 0 until topLevelGuaranteesCombined.size) yield st"let r$i: bool = ${topLevelGuaranteesCombined(i)};", "\n")}
                  |
                  |return ${(for (i <- 0 until topLevelGuaranteesCombined.size) yield s"r$i", " && ")};"""))))
          )
        }
      }

        if (compute.cases.nonEmpty) {
          var CEP_T_Case_Params: Set[GGParam] = Set.empty
          var caseCallsCombined: ISZ[ST] = ISZ()

          for (ccase <- compute.cases) {
            var combinedAssumGuarParams: Set[GGParam] = Set.empty
            val rAssm: Option[ST] =
              ccase.assumes match {
                case Some(assumes) =>
                  val ggAssm = GumboXRustUtil.rewriteToExpX(SlangExpUtil.getRexp(assumes, gclSymbolTable), thread, types, stateVars, crustTypeProvider)
                  combinedAssumGuarParams = combinedAssumGuarParams ++ ggAssm.params.elements
                  Some(SlangExpUtil.rewriteExp(
                    rexp = ggAssm.exp,
                    context = thread,
                    inRequires = T,
                    inVerus = F,
                    tp = crustTypeProvider,
                    aadlTypes = types,
                    store = store,
                    reporter = reporter))
                case _ => None()
              }

            val ggGuar = GumboXRustUtil.rewriteToExpX(SlangExpUtil.getRexp(ccase.guarantees, gclSymbolTable), thread, types, stateVars, crustTypeProvider)
            val rGuar = SlangExpUtil.rewriteExp(
              rexp = ggGuar.exp,
              context = thread,
              inRequires = F,
              inVerus = F,
              tp = crustTypeProvider,
              aadlTypes = types,
              store = store,
              reporter = reporter)

            val methodName = s"compute_case_${ccase.id}"

            val combinedAssmGuarParams = combinedAssumGuarParams ++ ggGuar.params.elements
            CEP_T_Case_Params = CEP_T_Case_Params ++ combinedAssmGuarParams.elements

            val sortedParams = GumboXRustUtil.sortParams(combinedAssmGuarParams.elements)

            caseCallsCombined = caseCallsCombined :+ st"$methodName(${(for (p <- sortedParams) yield p.name, ", ")})"

            val pred: ST =
              if (rAssm.nonEmpty)
                st"""implies!(
                    |  $rAssm,
                    |  $rGuar)"""
              else rGuar
            CEP_T_Case__methods = CEP_T_Case__methods :+ RAST.FnImpl(
              comments = ISZ(RAST.CommentST(
                st"""/** guarantee ${ccase.id}
                    |  ${GumboRustUtil.processDescriptor(ccase.descriptor, "*   ")}
                    |  ${(paramsToComment(sortedParams), "\n")}
                    |  */"""
              )),
              sig = RAST.FnSig(
                ident = RAST.IdentString(methodName),
                fnDecl = RAST.FnDecl(
                  inputs = for (p <- sortedParams) yield p.toRustParam,
                  outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
                verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
              attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(pred)))),
              meta = ISZ())
          } // end for

          val sorted_CEP_T_Case_Params = sortParams(CEP_T_Case_Params.elements)
          val CEP_T_Case_MethodName = GumboXRustUtil.getCompute_CEP_T_Case_MethodName
          CEP_T_Case__methods = CEP_T_Case__methods :+ RAST.FnImpl(
            comments = ISZ(RAST.CommentST(
              st"""/** CEP-T-Case: Top-Level case contracts for ${thread.identifier}'s compute entrypoint
                  |  *
                  |  ${(paramsToComment(sorted_CEP_T_Case_Params), "\n")}
                  |  */"""
            )),
            sig = RAST.FnSig(
              ident = RAST.IdentString(CEP_T_Case_MethodName),
              fnDecl = RAST.FnDecl(
                inputs = for (p <- sorted_CEP_T_Case_Params) yield p.toRustParam,
                outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
              verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
            attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""${(for (i <- 0 until caseCallsCombined.size) yield st"let r$i: bool = ${caseCallsCombined(i)};", "\n")}
                  |
                  |return ${(for (i <- 0 until caseCallsCombined.size) yield s"r$i", " && ")};""")))),
            meta = ISZ())
        } // end processing general cases

        if (compute.handlers.nonEmpty) {
          halt("Need to handle compute handlers")
        }

      case _ =>

    } // done processing compute clauses


    { // CEP-Pre

      var I_Assm_Guar_Params: Set[GGParam] = Set.empty
      var I_Assm_Guar: ISZ[ST] = ISZ()
      for (entry <- integrationConstraints.entries) {
        val port = thread.getPortByPath(entry._1).get
        if (GumboXRustUtil.isInPort(port)) {
          for (fn <- entry._2) {
            val aadlType = MicrokitTypeUtil.getPortType(port)
            val param = GGPortParam(port, aadlType.classifier, crustTypeProvider.getTypeNameProvider(aadlType))
            I_Assm_Guar_Params = I_Assm_Guar_Params + param
            I_Assm_Guar = I_Assm_Guar :+ st"${fn.ident.prettyST}(${param.name})"
          }
        }
      }

      var D_Inv_Guards: ISZ[ST] = ISZ()
      for (sortedParam <- sortParams(CEP_Pre_Params.elements) if datatypeInvariants.contains(sortedParam.aadlType)) {
        halt("Need to handle data data invariants")
      }

      if (I_Assm_Guar.nonEmpty || CEP_T_Assm__methods.nonEmpty || D_Inv_Guards.nonEmpty) {
        // create CEP-Pre

        if (D_Inv_Guards.nonEmpty) {
          halt("Need to handle data invariants")
        }

        val CEP_Assm_call: ISZ[ST] =
          if (CEP_T_Assm__methods.nonEmpty) {
            val lst = CEP_T_Assm__methods(CEP_T_Assm__methods.lastIndex)
            val args: ISZ[ST] = for (p <- lst.sig.fnDecl.inputs) yield p.ident.prettyST
            ISZ(st"${lst.ident.prettyST}(${(args, ", ")})")
          } else {
            ISZ()
          }

        // create CEP-Pre that:
        //   - calls I-Assm-Guar for each input port
        //   - calls D-Inv-Guar for each type used by an input port
        //   - call CEP-Assm

        var count = 0

        def opt(sts: ISZ[ST], desc: String): ST = {
          val ret = (st"""// $desc
                         |${(for (i <- 0 until sts.size) yield st"let r${i + count}: bool = ${sts(i)};", "\n")}""")
          count = count + sts.size
          return ret
        }

        val sorted_Cep_Pre_Params = sortParams(CEP_Pre_Params.elements)

        var bodySegments: ISZ[ST] = ISZ()
        if (D_Inv_Guards.nonEmpty) {
          bodySegments = bodySegments :+ opt(D_Inv_Guards, s"D-Inv-Guard: Datatype invariants for the types associated with ${thread.identifier}'s state variables and incoming ports")
        }
        if (I_Assm_Guar.nonEmpty) {
          bodySegments = bodySegments :+ opt(I_Assm_Guar, s"I-Assm-Guard: Integration constraints for ${thread.identifier}'s incoming ports")
        }
        if (CEP_Assm_call.nonEmpty) {
          bodySegments = bodySegments :+ opt(CEP_Assm_call, s"CEP-Assm: assume clauses of ${thread.identifier}'s compute entrypoint")
        }

        CEP_Pre = Some(RAST.FnImpl(
          comments = ISZ(RAST.CommentST(
            st"""/** CEP-Pre: Compute Entrypoint Pre-Condition for ${thread.identifier}
                |  *
                |  ${(paramsToComment(sorted_Cep_Pre_Params), "\n")}
                |  */"""
          )),
          sig = RAST.FnSig(
            ident = RAST.IdentString(GumboXRustUtil.getCompute_CEP_Pre_MethodName),
            fnDecl = RAST.FnDecl(
              inputs = for (p <- sorted_Cep_Pre_Params) yield p.toRustParam,
              outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
            verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
          attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
          body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
            st"""${(bodySegments, " & \n\n")}
                |
                |return ${(for (i <- 0 until count) yield s"r$i", " && ")};"""))))))
      }
    }

    { // CEP_Post
      var I_Guar_Guard_Params: Set[GGParam] = Set.empty
      var I_Guar_Guard: ISZ[ST] = ISZ()
      for (entry <- integrationConstraints.entries) {
        val port = thread.getPortByPath(entry._1).get
        val isOptional = !port.isInstanceOf[AadlDataPort]
        if (isOptional) {
          halt("Need to handle event ports")
        }
        val aadlType = MicrokitTypeUtil.getPortType(port)
        val param = GGPortParam(port, aadlType.classifier, crustTypeProvider.getTypeNameProvider(aadlType))
        I_Guar_Guard_Params = I_Guar_Guard_Params + param

        val fn = entry._2(entry._2.lastIndex)
        I_Guar_Guard = I_Guar_Guard :+ st"${fn.ident.prettyST}(${param.name})"
      }

      var D_Inv_Guards: ISZ[ST] = ISZ()
      for (sortedParam <- sortParams(CEP_Post_Params.elements) if datatypeInvariants.contains(sortedParam.aadlType)) {
        halt("Need to handle datatype invariants")
      }

      if (I_Guar_Guard.nonEmpty || CEP_T_Guar__methods.nonEmpty || CEP_T_Case__methods.nonEmpty || D_Inv_Guards.nonEmpty) {
        // Create CEP-Post

        // create call to the top level guarantee statements
        val CEP_Guar_call: Option[ST] =
          if (CEP_T_Guar__methods.nonEmpty) {
            val cep_t_guar_fn = CEP_T_Guar__methods(CEP_T_Guar__methods.lastIndex)
            val args: ISZ[ST] = for (a <- cep_t_guar_fn.sig.fnDecl.inputs) yield a.ident.prettyST
            Some(st"${cep_t_guar_fn.ident.prettyST}(${(args, ", ")})")
          } else {
            None()
          }

        // create call to the top level case contracts
        // TODO
        val CEP_T_Case_call: Option[ST] =
          if (CEP_T_Case__methods.nonEmpty) {
            val cep_t_case_fn = CEP_T_Case__methods(CEP_T_Case__methods.lastIndex)
            val args: ISZ[ST] = for (a <- cep_t_case_fn.sig.fnDecl.inputs) yield a.ident.prettyST
            Some(st"${cep_t_case_fn.ident.prettyST}(${(args, ", ")})")
          } else {
            None()
          }

        // create call to the top level handler contracts
        // TODO
        val CEP_T_Handler_calls: ISZ[ST] = ISZ()
        if (CEP_T_Handler_calls.nonEmpty) {
          halt("Need to handle compute handlers")
        }

        // create CEP-Post that:
        //  - calls I-Guar-Guard for each output port
        //  - calls D-Inv-Guar for each type used by an output port
        //  - calls CEP-Guar
        //  - calls CEP-T-Case
        //  - calls CEP-T-Handlers

        var count = 0

        def opts(sts: ISZ[ST], desc: String): ST = {
          val ret = (st"""// $desc
                         |${(for (i <- 0 until sts.size) yield st"let r${i + count}: bool = ${sts(i)};", "\n")}""")
          count = count + sts.size
          return ret
        }

        val sorted_Cep_Post_Params = GumboXRustUtil.sortParams(CEP_Post_Params.elements)

        var segments: ISZ[ST] = ISZ()
        if (D_Inv_Guards.nonEmpty) {
          segments = segments :+ opts(D_Inv_Guards, s"D-Inv-Guard: Datatype invariants for the types associated with ${thread.identifier}'s state variables and outgoing ports")
        }
        if (I_Guar_Guard.nonEmpty) {
          segments = segments :+ opts(I_Guar_Guard, s"I-Guar-Guard: Integration constraints for ${thread.identifier}'s outgoing ports")
        }
        if (CEP_Guar_call.nonEmpty) {
          segments = segments :+ opts(ISZ(CEP_Guar_call.get), s"CEP-Guar: guarantee clauses of ${thread.identifier}'s compute entrypoint")
        }
        if (CEP_T_Case_call.nonEmpty) {
          segments = segments :+ opts(ISZ(CEP_T_Case_call.get), s"CEP-T-Case: case clauses of ${thread.identifier}'s compute entrypoint")
        }
        if (CEP_T_Handler_calls.nonEmpty) {
          segments = segments :+ opts(CEP_T_Handler_calls, s"CEP-T-Handlers: handler clauses of ${thread.identifier}'s compute entrypoint")
        }

        CEP_Post = Some(RAST.FnImpl(
          comments = ISZ(RAST.CommentST(
            st"""/** CEP-Post: Compute Entrypoint Post-Condition for ${thread.identifier}
                |  *
                |  ${(paramsToComment(sorted_Cep_Post_Params), "\n")}
                |  */""")),
          sig = RAST.FnSig(
            ident = RAST.IdentString(GumboXRustUtil.getCompute_CEP_Post_MethodName),
            fnDecl = RAST.FnDecl(
              inputs = for (p <- sorted_Cep_Post_Params) yield p.toRustParam,
              outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
            verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
          attributes = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
          body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
            st"""${(segments, "\n\n")}
                |
                |return ${(for (i <- 0 until count) yield s"r$i", " && ")};"""))))))
      }
    }

    return ComputeContributions(
      CEP_T_Assum__methods = CEP_T_Assm__methods,
      CEP_T_Guar__methods = CEP_T_Guar__methods,
      CEP_T_Case__methods = CEP_T_Case__methods,

      CEP_Pre_Params = CEP_Pre_Params.elements,
      CEP_Pre = CEP_Pre,

      CEP_Post_Params = CEP_Post_Params.elements,
      CEP_Post = CEP_Post)
  }

  @pure def buildGumboxTestMethods(thread: AadlThread,
                                   initializeContributions: InitializeContributions,
                                   computeContributions: ComputeContributions,
                                   subclauseInfoOpt: Option[GclAnnexClauseInfo], crustTypeProvider: CRustTypeProvider, types: AadlTypes, localStore: Store, reporter: Reporter
                                  ): (ISZ[RAST.Item], ISZ[RAST.Item]) = {
    assert(thread.isPeriodic(), s"Need to handle sporadic threads: ${thread.classifierAsString}")

    var test_api_rs_Items: ISZ[RAST.Item] = ISZ()
    var tests_rs_Items: ISZ[ST] = ISZ()


    val testInitializeCB = "testInitializeCB"
    val testComputeCB = "testComputeCB"
    val testComputeCBwLV = "testComputeCBwLV"

    { // testInitialiseCB
      val sorted_iep_post_param = GumboXRustUtil.sortParams(initializeContributions.IEP_Post_Params)

      val postStateFetchOpt: Option[ST] =
        if (initializeContributions.IEP_Guarantee.nonEmpty)
          Some(
            st"""// [RetrieveOutState]: retrieve values of the output ports via get operations and GUMBO declared local state variable
                |${(for (p <- sorted_iep_post_param) yield st"let ${p.name} = get_${p.originName}();", "\n")}
                |""")
        else None()

      val postOpt: Option[ST] =
        if (initializeContributions.IEP_Guarantee.nonEmpty) {
          val iep_post = initializeContributions.IEP_Guarantee(initializeContributions.IEP_Guarantee.lastIndex)
          assert(iep_post.sig.ident.string == GumboXRustUtil.getInitialize_IEP_Post_MethodName)
          Some(
            st"""// [CheckPost]: invoke the oracle function
                |prop_assert!(
                |  GUMBOX::${iep_post.sig.ident.string}(
                |    ${(for (p <- sorted_iep_post_param) yield p.name, ",\n")}
                |  ),
                |  "Postcondition failed: incorrect output behavior"
                |);
                |""")
        } else {
          None()
        }

      val initBody: ST =
        st"""// [InvokeEntryPoint]: Invoke the entry point
            |crate::${Util.getThreadIdPath(thread)}_initialize();
            |
            |$postStateFetchOpt
            |$postOpt
            |// Return Ok(()) if all assertions pass
            |Ok(())"""
      val testInitialize = RAST.FnImpl(
        visibility = RAST.Visibility.Public,
        sig = RAST.FnSig(
          ident = RAST.IdentString(testInitializeCB),
          fnHeader = RAST.FnHeader(F),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(),
            outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"Result<(), TestCaseError>"))),
          verusHeader = None(), generics = None()),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(initBody)))),
        comments = ISZ(RAST.CommentST(
          st"""/** Contract-based test harness for the initialize entry point
              |  */""")),
        attributes = ISZ(), contract = None(), meta = ISZ())

      test_api_rs_Items = test_api_rs_Items :+ testInitialize
    }

    { // testInitialize_macro
      var inputs: ISZ[RAST.Param] = ISZ()
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("$test_name"), RAST.TyPath(ISZ(ISZ("ident")), None()))
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("config"), RAST.TyPath(ISZ(ISZ("$config:expr")), None()))

      val body =
        st"""proptest!{
            |  #![proptest_config($$config)]
            |  #[test]
            |  #[serial]
            |  fn $$test_name(empty in ::proptest::strategy::Just(())) {
            |    $$crate::bridge::test_api::${testInitializeCB}()?;
            |  }
            |}"""
      val testInitialize_CB_macro = RAST.MacroImpl(
        attributes = ISZ(RAST.AttributeST(inner = F, content = st"macro_export")),
        ident = RAST.IdentString(s"${testInitializeCB}_macro"),
        inputs = inputs,
        comments = ISZ(),
        body = Some(body))

      test_api_rs_Items = test_api_rs_Items :+ testInitialize_CB_macro


      tests_rs_Items = tests_rs_Items :+
        st"""${testInitializeCB}_macro! {
            |  prop_${testInitializeCB}_macro, // test name
            |  config: ProptestConfig { // proptest configuration, built by overriding fields from default config
            |    cases: numValidComputeTestCases,
            |    max_global_rejects: numValidComputeTestCases * computeRejectRatio,
            |    verbose: verbosity,
            |    ..ProptestConfig::default()
            |  }
            |}"""
    }


    val sorted_Pre_State_Params = GumboXRustUtil.sortParams(computeContributions.CEP_Pre_Params)
    val sorted_Post_State_Params = GumboXRustUtil.sortParams(computeContributions.CEP_Post_Params)

    val preOpt: Option[ST] =
      if (computeContributions.CEP_Pre.nonEmpty)
        Some(
          st"""// [CheckPre]: check/filter based on pre-condition.
              |prop_assume! {
              |  GUMBOX::${computeContributions.CEP_Pre.get.sig.ident.prettyST} (
              |    ${(for (p <- sorted_Pre_State_Params) yield p.name, ",\n")}
              |  ),
              |   "Precondition failed: invalid input combination"
              |}
              |""")
      else None()

    val inPorts = sorted_Pre_State_Params.filter(p => p.isInPort)
    val putInPortOpts: Option[ST] =
      if (inPorts.nonEmpty)
        Some(
          st"""// [PutInPorts]: Set values on the input ports
              |${(for (p <- inPorts) yield st"put_${p.originName}(${p.name});", "\n")}
              |""")
      else None()

    val onlyOutState = sorted_Post_State_Params.filter(p => p.isOutPort || p.kind == SymbolKind.StateVar)
    val postStateFetchOpt: Option[ST] =
      if (computeContributions.CEP_Post.nonEmpty) Some(
        st"""// [RetrieveOutState]: retrieve values of the output ports via get operations and GUMBO declared local state variable
            |${(for (p <- onlyOutState) yield st"let ${p.name} = get_${p.originName}();", "\n")}
            |""")
      else None()

    val postOpt: Option[ST] =
      if (computeContributions.CEP_Post.nonEmpty)
        Some(
          st"""// [CheckPost]: invoke the oracle function
              |prop_assert!(
              |  GUMBOX::${computeContributions.CEP_Post.get.sig.ident.prettyST}(
              |    ${(for (p <- sorted_Post_State_Params) yield p.name, ",\n")}
              |  ),
              |  "Postcondition failed: incorrect output behavior"
              |);
              |""")
      else None()

    { // testComputeCB
      val sorted_pre_without_state_vars = sorted_Pre_State_Params.filter(p => p.isInPort)

      val state_vars = sorted_Pre_State_Params.filter(p => p.isStateVar)
      var svs: ISZ[ST] = ISZ()
      for (s <- state_vars) {
        svs = svs :+ st"let ${s.name}: ${s.typeNameProvider.qualifiedRustName} = get_${s.originName}();"
      }
      val saveInLocalOpt: Option[ST] =
        if (state_vars.nonEmpty)
          Some(
            st"""// [SaveInLocal]: retrieve and save the current (input) values of GUMBO-declared local state variables as retrieved
                |//                from the component state
                |${(svs, "\n")}
                |""")
        else None()

      val computeCBBody: ST =
        st"""// Initialize the app
            |crate::${Util.getThreadIdPath(thread)}_initialize();
            |
            |$saveInLocalOpt
            |$preOpt
            |$putInPortOpts
            |// [InvokeEntryPoint]: Invoke the entry point
            |crate::${Util.getThreadIdPath(thread)}_timeTriggered();
            |
            |$postStateFetchOpt
            |$postOpt
            |// Return Ok(()) if all assertions pass
            |Ok(())"""
      val testComputeWithoutStateVars = RAST.FnImpl(
        visibility = RAST.Visibility.Public,
        sig = RAST.FnSig(
          ident = RAST.IdentString(testComputeCB),
          fnHeader = RAST.FnHeader(F),
          fnDecl = RAST.FnDecl(
            inputs = for (p <- sorted_pre_without_state_vars) yield p.toRustParam,
            outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"Result<(), TestCaseError>"))),
          verusHeader = None(), generics = None()),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(computeCBBody)))),
        comments = ISZ(RAST.CommentST(
          st"""/** Contract-based test harness for the compute entry point
              |  *
              |  ${(paramsToComment(sorted_pre_without_state_vars), "\n")}
              |  */""")),
        attributes = ISZ(), contract = None(), meta = ISZ())

      test_api_rs_Items = test_api_rs_Items :+ testComputeWithoutStateVars
    }

    { // test_compute_CB_macro
      var inputs: ISZ[RAST.Param] = ISZ()
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("$test_name"), RAST.TyPath(ISZ(ISZ("ident")), None()))
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("config"), RAST.TyPath(ISZ(ISZ("$config:expr")), None()))

      val sorted_pre_without_state_vars = sorted_Pre_State_Params.filter(p => p.isInPort)

      for (p <- sorted_pre_without_state_vars) {
        inputs = inputs :+ RAST.ParamImpl(
          ident = RAST.IdentString(p.name),
          kind = RAST.TyPath(ISZ(ISZ(s"$$${p.name}_strat:expr")), None())
        )
      }
      val body =
        st"""proptest!{
            |  #![proptest_config($$config)]
            |  #[test]
            |  #[serial]
            |  fn $$test_name(
            |    (${(for (p <- sorted_pre_without_state_vars) yield p.name, ", ")})
            |    in (${(for (p <- sorted_pre_without_state_vars) yield s"$$${p.name}_strat", ", ")})
            |  ) {
            |    $$crate::bridge::test_api::${testComputeCB}(
            |      ${(for (p <- sorted_pre_without_state_vars) yield p.name, ",\n")}
            |    )?;
            |  }
            |}"""
      val test_compute_CB_macro = RAST.MacroImpl(
        attributes = ISZ(RAST.AttributeST(inner = F, content = st"macro_export")),
        ident = RAST.IdentString(s"${testComputeCB}_macro"),
        inputs = inputs,
        comments = ISZ(),
        body = Some(body))

      test_api_rs_Items = test_api_rs_Items :+ test_compute_CB_macro

      val strategiesOpt: Option[ST] =
       if(sorted_pre_without_state_vars.nonEmpty) {
         var strategies: ISZ[ST] = ISZ()
         for (p <- sorted_pre_without_state_vars) {
           var d = st"test_api::${(p.typeNameProvider.qualifiedRustNameS, "_")}_strategy_default()"
           if (p.isOptional) {
             d = st"test_api::option_strategy_default($d)"
           }
           strategies = strategies :+ st"${p.name}: $d"
         }
         Some(
           st"""// strategies for generating each component input
               |${(strategies, ",\n")}""")
       } else {
         None()
       }
      tests_rs_Items = tests_rs_Items :+
        st"""${testComputeCB}_macro! {
            |  prop_${testComputeCB}_macro, // test name
            |  config: ProptestConfig { // proptest configuration, built by overriding fields from default config
            |    cases: numValidComputeTestCases,
            |    max_global_rejects: numValidComputeTestCases * computeRejectRatio,
            |    verbose: verbosity,
            |    ..ProptestConfig::default()
            |  }${if(strategiesOpt.nonEmpty) "," else ""}
            |  $strategiesOpt
            |}"""
    }

    { // test_compute_CBwL
      val stateVars = sorted_Pre_State_Params.filter(p => p.isStateVar)
      val putStateVarOpts: Option[ST] =
        if (stateVars.nonEmpty)
          Some(
            st"""// [SetInStateVars]: set the pre-state values of state variables
                |${(for (p <- stateVars) yield st"put_${p.originName}(${p.name});", "\n")}
                |""")
        else None()

      val computeCBwLBody: ST =
        st"""// Initialize the app
            |crate::${Util.getThreadIdPath(thread)}_initialize();
            |
            |$preOpt
            |$putInPortOpts
            |$putStateVarOpts
            |// [InvokeEntryPoint]: Invoke the entry point
            |crate::${Util.getThreadIdPath(thread)}_timeTriggered();
            |
            |$postStateFetchOpt
            |$postOpt
            |// Return Ok(()) if all assertions pass
            |Ok(())"""

      val testComputeWithStateVars = RAST.FnImpl(
        visibility = RAST.Visibility.Public,
        sig = RAST.FnSig(
          ident = RAST.IdentString(testComputeCBwLV),
          fnHeader = RAST.FnHeader(F),
          fnDecl = RAST.FnDecl(
            inputs = for (p <- sorted_Pre_State_Params) yield p.toRustParam,
            outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"Result<(), TestCaseError>"))),
          verusHeader = None(), generics = None()),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(computeCBwLBody)))),
        comments = ISZ(RAST.CommentST(
          st"""/** Contract-based test harness for the compute entry point
              |  *
              |  ${(paramsToComment(sorted_Pre_State_Params), "\n")}
              |  */""")),
        attributes = ISZ(), contract = None(), meta = ISZ())

      test_api_rs_Items = test_api_rs_Items :+ testComputeWithStateVars
    }

    { // test_compute_CBwL_macro
      var inputs: ISZ[RAST.Param] = ISZ()
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("$test_name"), RAST.TyPath(ISZ(ISZ("ident")), None()))
      inputs = inputs :+ RAST.ParamImpl(RAST.IdentString("config"), RAST.TyPath(ISZ(ISZ("$config:expr")), None()))

      for (p <- sorted_Pre_State_Params) {
        inputs = inputs :+ RAST.ParamImpl(
          ident = RAST.IdentString(p.name),
          kind = RAST.TyPath(ISZ(ISZ(s"$$${p.name}_strat:expr")), None())
        )
      }
      val body =
        st"""proptest!{
            |  #![proptest_config($$config)]
            |  #[test]
            |  #[serial]
            |  fn $$test_name(
            |    (${(for (p <- sorted_Pre_State_Params) yield p.name, ", ")})
            |    in (${(for (p <- sorted_Pre_State_Params) yield s"$$${p.name}_strat", ", ")})
            |  ) {
            |    $$crate::bridge::test_api::${testComputeCBwLV}(
            |      ${(for (p <- sorted_Pre_State_Params) yield p.name, ",\n")}
            |    )?;
            |  }
            |}"""
      val test_compute_CBwL_macro = RAST.MacroImpl(
        attributes = ISZ(RAST.AttributeST(inner = F, content = st"macro_export")),
        ident = RAST.IdentString(s"${testComputeCBwLV}_macro"),
        inputs = inputs,
        comments = ISZ(),
        body = Some(body))

      test_api_rs_Items = test_api_rs_Items :+ test_compute_CBwL_macro


      val strategiesOpt: Option[ST] =
        if(sorted_Pre_State_Params.nonEmpty) {
          var strategies: ISZ[ST] = ISZ()
          for (p <- sorted_Pre_State_Params) {
            var d = st"test_api::${(p.typeNameProvider.qualifiedRustNameS, "_")}_strategy_default()"
            if (p.isOptional) {
              d = st"test_api::option_strategy_default($d)"
            }
            strategies = strategies :+ st"${p.name}: $d"
          }
          Some(
            st"""// strategies for generating each component input
                |${(strategies, ",\n")}""")
        } else {
          None()
        }
      tests_rs_Items = tests_rs_Items :+
        st"""${testComputeCBwLV}_macro! {
            |  prop_${testComputeCBwLV}_macro, // test name
            |  config: ProptestConfig { // proptest configuration, built by overriding fields from default config
            |    cases: numValidComputeTestCases,
            |    max_global_rejects: numValidComputeTestCases * computeRejectRatio,
            |    verbose: verbosity,
            |    ..ProptestConfig::default()
            |  }${if (strategiesOpt.nonEmpty) "," else ""}
            |  $strategiesOpt
            |}"""
    }

    val testrsMod = st"""mod GUMBOX_tests {
                        |  use serial_test::serial;
                        |  use proptest::prelude::*;
                        |
                        |  use crate::bridge::test_api;
                        |  use crate::testInitializeCB_macro;
                        |  use crate::testComputeCB_macro;
                        |  use crate::testComputeCBwLV_macro;
                        |
                        |  // number of valid (i.e., non-rejected) test cases that must be executed for the compute method.
                        |  const numValidComputeTestCases: u32 = 100;
                        |
                        |  // how many total test cases (valid + rejected) that may be attempted.
                        |  //   0 means all inputs must satisfy the precondition (if present),
                        |  //   5 means at most 5 rejected inputs are allowed per valid test case
                        |  const computeRejectRatio: u32 = 5;
                        |
                        |  const verbosity: u32 = 2;
                        |
                        |  ${(tests_rs_Items, "\n\n")}
                        |}"""

    return (test_api_rs_Items, ISZ(RAST.ItemST(testrsMod)))
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption,
                                     types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    for (entry <- GumboXRustPlugin.getGumboXContributions(localStore).get.componentContributions.entries) {
      val thread = symbolTable.componentMap.get(entry._1).get.asInstanceOf[AadlThread]
      val threadId = Util.getThreadIdPath(thread)

      { // the gumbox module
        val apiDirectory = CRustApiPlugin.apiDirectory(thread, options)

        var entries: ISZ[ST] = ISZ()

        entries = entries ++ (for (m <- entry._2.gumboMethods) yield m.prettyST)

        entries = entries ++ (for (values <- entry._2.integrationConstraints.values;
                                   v <- values) yield v.prettyST)

        entries = entries ++ (for (i <- entry._2.initializeContributions.IEP_Guarantee) yield i.prettyST)

        entries = entries ++ (for (a <- entry._2.computeContributions.CEP_T_Assum__methods) yield a.prettyST)
        if (entry._2.computeContributions.CEP_Pre.nonEmpty) {
          entries = entries :+ entry._2.computeContributions.CEP_Pre.get.prettyST
        }

        entries = entries ++ (for (g <- entry._2.computeContributions.CEP_T_Guar__methods) yield g.prettyST)
        entries = entries ++ (for (c <- entry._2.computeContributions.CEP_T_Case__methods) yield c.prettyST)

        if (entry._2.computeContributions.CEP_Post.nonEmpty) {
          entries = entries :+ entry._2.computeContributions.CEP_Post.get.prettyST
        }

        val content =
          st"""${Util.doNotEdit}
              |
              |use ${CRustTypePlugin.usePath};
              |
              |macro_rules! implies {
              |  ($$lhs: expr, $$rhs: expr) => {
              |    !$$lhs || $$rhs
              |  };
              |}
              |
              |macro_rules! impliesL {
              |  ($$lhs: expr, $$rhs: expr) => {
              |    !$$lhs | $$rhs
              |  };
              |}
              |
              |${(entries, "\n\n")}
              |"""
        val path = s"$apiDirectory/${threadId}_GUMBOX.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }
    }
    return (localStore + s"FINALIZED_$name" ~> BoolValue(T), resources)
  }
}

@datatype class DefaultGumboXPlugin extends GumboXRustPlugin {
  @strictpure override def name: String = "DefaultGumboXPlugin"
}