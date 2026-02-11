// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.{BlockMarker, Marker, Resource}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.Context
import org.sireum.hamr.codegen.microkit.plugins.linters.MicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.plugins.testing.CRustTestingPlugin
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypePlugin, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitInitPlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.rust.Item
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{MicrokitCodegen, rust => RAST}
import org.sireum.hamr.ir._
import org.sireum.message.{Level, Message, Reporter}

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

  @strictpure def getGclLibraryAnnexes(symbolTable: SymbolTable): ISZ[GclAnnexLibInfo] =
    symbolTable.annexLibInfos.filter(p => p.isInstanceOf[GclAnnexLibInfo]).asInstanceOf[ISZ[GclAnnexLibInfo]]


  @datatype class LibraryAnnex(val rustItems: ISZ[RAST.Item],

                               val verusItems: ISZ[RAST.Item],
                               val verusDeveloperItems: ISZ[RAST.Item])
}

@sig trait GumboRustContributions extends StoreValue {
  @pure def datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]]

  @pure def getLibraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex]
  @pure def setLibraryAnnexes(m: Map[String, GumboRustPlugin.LibraryAnnex]): GumboRustContributions
}

@datatype class DefaultGumboRustContributions(val datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]],
                                              val libraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex]) extends GumboRustContributions {

  @strictpure override def setLibraryAnnexes(m: Map[String, GumboRustPlugin.LibraryAnnex]): GumboRustContributions = DefaultGumboRustContributions(datatypeInvariants, m)
}

@sig trait GumboRustPlugin extends MicrokitInitPlugin with MicrokitPlugin with MicrokitFinalizePlugin {

  @pure override def init(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): Store = {
    if (isDisabled(store)) {
      return store
    }
    var localStore = store
    if (options.platform != HamrCli.CodegenHamrPlatform.Microkit ||
      !ops.ISZOps(symbolTable.getThreads()).exists(p => MicrokitUtil.isRusty(p))) {
      return localStore
    }
    var threadsWithContracts: ISZ[ThreadIdPath] = ISZ()
    var datatypesWithContracts: ISZ[DataIdPath] = ISZ()
    for (t <- symbolTable.getThreads() if MicrokitUtil.isRusty(t)) {
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
      !isDisabled(store) &&
      !store.contains(GumboRustPlugin.KEY_GumboRustPlugin) &&
      // may need the app contributions if we need to insert method-level contracts so
      // wait until that phase is done.  The component plugin depends on the typing and
      // api plugins so we can also add datatype invariants and/or integration constraints
      // if needed
      CRustComponentPlugin.hasCRustComponentContributions(store) /* &&

      NOTE: gumbo placeholder markers are now emitted for any missing GUMBO contract artifact

      (GumboRustPlugin.getThreadsWithContracts(store).nonEmpty ||
       GumboRustPlugin.getDatatypesWithContracts(store).nonEmpty ||
       GumboRustPlugin.getGclLibraryAnnexes(symbolTable).nonEmpty)
      */


  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_$name")

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    val hasLibraryAnnexes: B = GumboRustPlugin.getGumboRustContributions(store) match {
      case Some(c) => c.getLibraryAnnexes.nonEmpty
      case _ => F
    }

    return (options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !reporter.hasError &&
      !isDisabled(store) &&
      !alreadyFinalized(store) &&
      hasLibraryAnnexes)
  }


  @pure override def handle(model: Aadl,
                            options: HamrCli.CodegenOption,
                            types: AadlTypes,
                            symbolTable: SymbolTable,
                            store: Store,
                            reporter: Reporter): (Store, ISZ[Resource]) = {
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

    var makefileVerusItems: ISZ[ST] = ISZ()

    var libraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex] = Map.empty
    for(gclLib <-  GumboRustPlugin.getGclLibraryAnnexes(symbolTable)) {
      // add dependencies to gumbo library annexes
      assert(gclLib.name.size == 2 && gclLib.name(1) == "GUMBO__Library", gclLib.name.string)
      val name = gclLib.name(0)

      val (verusLibFuns, developerVerusUifs): (ISZ[RAST.Item], ISZ[RAST.Item]) = handleGclLibrary(gclLib, symbolTable, types, store, reporter)


      libraryAnnexes = libraryAnnexes + name ~> GumboRustPlugin.LibraryAnnex(
        rustItems = ISZ(),

        verusItems = verusLibFuns,
        verusDeveloperItems = developerVerusUifs)
    }

    val crateDeps: ISZ[ST] = for (k <- libraryAnnexes.keys) yield st"""$k = { path = "../$k" }"""

    for (thread <- symbolTable.getThreads() if MicrokitUtil.isRusty(thread)) {
      var markers: ISZ[Marker] = ISZ()
      val threadPath = thread.path
      val subclauseInfo = GumboRustUtil.getGumboSubclauseOrDummy(threadPath, symbolTable)
      val componentContributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
      val threadContributions = componentContributions.componentContributions.get(threadPath).get
      var structDef = threadContributions.appStructDef
      val structImpl = threadContributions.appStructImpl.asInstanceOf[RAST.ImplBase]
      var crateLevelEntries = threadContributions.crateLevelEntries
      val crateDependencies = threadContributions.crateDependencies ++ crateDeps

      var optStateVarInits: ISZ[RAST.Item] = ISZ()
      if (subclauseInfo.annex.state.nonEmpty) {
        val typeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

        { // add testing apis to get/set the state variables
          var testingApis: ISZ[RAST.Item] = ISZ()
          for (sv <- subclauseInfo.annex.state) {
            val aadlType = typeProvider.getRepresentativeType(types.typeMap.get(sv.classifier).get)
            val np = typeProvider.getTypeNameProvider(aadlType)
            testingApis = testingApis :+ RAST.FnImpl(
              comments = ISZ(RAST.CommentRustDoc(ISZ(st"getter for GUMBO State Variable"))),
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString(s"get_${sv.name}"),
                fnHeader = RAST.FnHeader(F),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(),
                  outputs = RAST.FnRetTyImpl(RAST.TyPath(items = ISZ(np.qualifiedRustNameS), aadlType = Some(aadlType.classifier)))
                ),
                verusHeader = None(), generics = None()),
              attributes = ISZ(), contract = None(), meta =  ISZ(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match &crate::app {
                    |    Some(inner) => inner.${sv.name},
                    |    None => panic!("The app is None")
                    |  }
                    |}""" )))))

            val typ = RAST.TyPath(items = ISZ(np.qualifiedRustNameS), aadlType = Some(aadlType.classifier))
            testingApis = testingApis :+ RAST.FnImpl(
              comments = ISZ(RAST.CommentRustDoc(ISZ(st"setter for GUMBO State Variable"))),
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString(s"put_${sv.name}"),
                fnHeader = RAST.FnHeader(F),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(RAST.ParamImpl(ident = RAST.IdentString("value"), kind = typ)),
                  outputs = RAST.FnRetTyDefault()),
                verusHeader = None(), generics = None()),
              attributes = ISZ(), contract = None(), meta =  ISZ(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match &mut crate::app {
                    |    Some(inner) => inner.${sv.name} = value,
                    |    None => panic!("The app is None")
                    |  }
                    |}""" )))))
          }

          val testingContributions = CRustTestingPlugin.getCRustTestingContributions(localStore).get.testingContributions
          val existing = testingContributions.get(thread.path).get
          localStore = CRustTestingPlugin.putCRustTestingContributions(
            CRustTestingPlugin.CRustTestingContributions(
              testingContributions + thread.path ~> existing(testApiEntries = existing.testApiEntries ++ testingApis)),
            localStore
          )
        }

        { // add a Rust field to the struct definition for each state variable
          var stateVars: ISZ[RAST.StructField] = ISZ()
          for (sv <- subclauseInfo.annex.state) {
            val i = GumboRustUtil.processStateVariable(sv, types, typeProvider)
            stateVars = stateVars :+ i._1
            optStateVarInits = optStateVarInits :+ RAST.ItemST(i._2)
          }
          val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.stateVar)
          markers = markers :+ m
          structDef = structDef(items = structDef.items :+ RAST.MarkerWrap(m, stateVars.asInstanceOf[ISZ[RAST.Item]], ",\n", Some(",")))
        }
      } else {
        val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.stateVar)
        markers = markers :+ m
        structDef = structDef(items = structDef.items :+ RAST.MarkerPlaceholder(m))
      }

      if (subclauseInfo.annex.methods.nonEmpty) {

        var verusFuns: ISZ[RAST.Item] = ISZ()
        var developerUifFuns: ISZ[RAST.Item] = ISZ()
        for (m <- subclauseInfo.annex.methods) {
          m match {
            case g: GclSpecMethod =>
              val (gumboFunction, developerFunction) = GumboRustUtil.processGumboSpecMethod(
                m = g,

                owner = thread.classifier,
                optComponent = Some(thread),
                isLibraryMethod = F,

                inVerus = T,

                aadlTypes = types,
                tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = localStore,
                reporter = reporter
              )

              verusFuns = verusFuns :+ gumboFunction
              developerUifFuns = developerUifFuns :+ developerFunction

            case g: GclBodyMethod =>
              verusFuns = verusFuns :+ GumboRustUtil.processGumboBodyMethod(
                m = g,

                owner = thread.classifier,
                optComponent = Some(thread),
                isLibraryMethod = F,

                inVerus = T,

                aadlTypes = types,
                tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = localStore,
                reporter = reporter)
          }
        }

        val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboMethods)
        markers = markers :+ m
        crateLevelEntries = crateLevelEntries :+ RAST.MarkerWrap(m, verusFuns, "\n\n", None())

        crateLevelEntries = crateLevelEntries ++ developerUifFuns

      } else {
        val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.gumboMethods)
        markers = markers :+ m
        crateLevelEntries = crateLevelEntries :+ RAST.MarkerPlaceholder(m)
      }

      if (subclauseInfo.annex.integration.nonEmpty) {
        val (requires, ensures) = handleIntegrationConstraints(
          thread = thread,
          subclauseInfo = subclauseInfo,

          types = types,
          tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
          store = localStore,
          reporter = reporter)

        val crustApiContributions = CRustApiPlugin.getCRustApiContributions(localStore).get
        val componentApiContributions = crustApiContributions.apiContributions.get(threadPath).get

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

        var updatedUnverifiedGetApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.unverifiedGetApis) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("unverified_get_", "")
              if (ensures.contains(portId)) {
                val ensure = ensures.get(portId).get
                updatedUnverifiedGetApis = updatedUnverifiedGetApis :+
                  fn(contract = Some(fn.contract.get(ensures = fn.contract.get.ensures :+ ensure)))
              } else {
                updatedUnverifiedGetApis = updatedUnverifiedGetApis :+ fn
              }
            case _ => updatedUnverifiedGetApis = updatedUnverifiedGetApis :+ u
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

        val con = crustApiContributions.addApiContributions(threadPath,
          componentApiContributions(
            unverifiedGetApis = updatedUnverifiedGetApis,
            appApiDefaultPutters = updatedPutApis,
            appApiDefaultGetters = updatedGetApis))

        localStore = CRustApiPlugin.putCRustApiContributions(con, localStore)
      }

      var updatedImplItems: ISZ[RAST.Item] = ISZ()
      for (i <- structImpl.items) {
        i match {
          case f: RAST.FnImpl =>
            if (f.ident.string == "new") {
              if (optStateVarInits.nonEmpty) {
                val b: Option[RAST.MethodBody] = f.body match {
                  case Some(RAST.MethodBody(ISZ(self: RAST.BodyItemSelf))) =>
                    val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.stateVarInit)
                    markers = markers :+ m
                    val wrapper = RAST.MarkerWrap(m, optStateVarInits, ",\n", Some(","))
                    Some(RAST.MethodBody(ISZ(self(items = self.items :+ wrapper.prettyST))))
                  case _ => halt("Not expecting new to contain anything other than Self {...}")
                }
                updatedImplItems = updatedImplItems :+ f(body = b)
              } else{
                val b: Option[RAST.MethodBody] = f.body match {
                  case Some(RAST.MethodBody(ISZ(self: RAST.BodyItemSelf))) =>
                    val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.stateVarInit)
                    markers = markers :+ m
                    Some(RAST.MethodBody(ISZ(self(items = self.items :+ RAST.MarkerPlaceholder(m).prettyST))))
                  case _ => halt("Not expecting new to contain anything other than Self {...}")
                }
                updatedImplItems = updatedImplItems :+ f(body = b)
              }
            }
            else if (f.ident.string == "initialize") {
              if (subclauseInfo.annex.initializes.nonEmpty) {
                val init: (Marker, RAST.FnImpl) = handleInitialize(
                  fn = f,
                  thread = thread,
                  subclauseInfo = subclauseInfo,
                  types = types,
                  tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                  symbolTable = symbolTable,
                  store = localStore,
                  reporter = reporter)
                markers = markers :+ init._1
                updatedImplItems = updatedImplItems :+ init._2
              } else {
                val init = handleInitializePlaceholder(f)
                markers = markers :+ init._1
                updatedImplItems = updatedImplItems :+ init._2
              }
            } else if (f.ident.string == "timeTriggered") {
              if (subclauseInfo.annex.compute.nonEmpty) {
                val tt: (ISZ[Marker], RAST.FnImpl) = handleCompute(
                  fn = f,
                  thread = thread,
                  subclauseInfo = subclauseInfo,
                  types = types,
                  tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                  symbolTable = symbolTable,
                  store = localStore,
                  reporter = reporter)
                markers = markers ++ tt._1
                updatedImplItems = updatedImplItems :+ tt._2
              } else {
                val tt = handleComputePlaceholder(f)
                markers = markers ++ tt._1
                updatedImplItems = updatedImplItems :+ tt._2
              }
            } else {
              updatedImplItems = updatedImplItems :+ i
            }
          case _ => updatedImplItems = updatedImplItems :+ i
        }
      }

      var annotatedCrateLevelItems: ISZ[RAST.Item] = ISZ()
      for (f <- crateLevelEntries) {
        f match {
          case (fi: RAST.FnImpl) if fi.ident.string == "log_info" || fi.ident.string == "log_warn_channel" =>
            val attrs = fi.attributes
            annotatedCrateLevelItems = annotatedCrateLevelItems :+ fi(
              attributes =  attrs :+ RAST.AttributeST(F, st"verifier::external_body"))
          case _ =>
            annotatedCrateLevelItems = annotatedCrateLevelItems :+ f
        }
      }
      crateLevelEntries = annotatedCrateLevelItems

      localStore = CRustComponentPlugin.putComponentContributions(
        componentContributions.replaceComponentContributions(
          componentContributions.componentContributions + threadPath ~>
            threadContributions(
              markers = markers,
              requiresVerus = T,
              appStructDef = structDef,
              appStructImpl = structImpl(items = updatedImplItems),
              crateLevelEntries = annotatedCrateLevelItems,
              crateDependencies = crateDependencies)),
        localStore)

      makefileVerusItems = makefileVerusItems :+ st"make -C $${CRATES_DIR}/${MicrokitUtil.getComponentIdPath(thread)} verus"
    } // end processing thread's contracts

    localStore = MakefileUtil.addMakefileTargets(ISZ("system.mk"), ISZ(MakefileTarget(name = "verus", allowMultiple = F, dependencies = ISZ(), body = makefileVerusItems)), localStore)
    localStore = MakefileUtil.addMakefileTargets(ISZ("Makefile"), ISZ(MakefileTarget(name = "verus", allowMultiple = F, dependencies = ISZ(st"$${TOP_BUILD_DIR}/Makefile"), body = ISZ(st"$${MAKE} -C $${TOP_BUILD_DIR} verus"))), localStore)

    return (GumboRustPlugin.putGumboRustContributions(
      DefaultGumboRustContributions(datatypeInvariants, libraryAnnexes), localStore), ISZ())
  }

  @pure def handleGclLibrary(gclLib: GclAnnexLibInfo,
                             symbolTable: SymbolTable,
                             types: AadlTypes,
                             store: Store,
                             reporter: Reporter): (ISZ[RAST.Item], ISZ[RAST.Item]) = {
    val GclAnnexLibInfo(annex, name, gclSymbolTable) = gclLib
    var gumboFuns: ISZ[RAST.Fn] = ISZ()
    var developerUifFuns: ISZ[RAST.Fn] = ISZ()

    for(m <- annex.methods) {
      m match {
        case g: GclSpecMethod =>
          val (libFun, developerLibFun) = GumboRustUtil.processGumboSpecMethod(
            m = g,

            owner = gclLib.name,
            optComponent = None(),
            isLibraryMethod = T,

            inVerus = T,
            aadlTypes = types,
            tp = CRustTypePlugin.getCRustTypeProvider(store).get,
            gclSymbolTable = gclSymbolTable,
            store = store,
            reporter = reporter)

          gumboFuns = gumboFuns :+ libFun
          developerUifFuns = developerUifFuns :+ developerLibFun

        case g: GclBodyMethod =>
          gumboFuns = gumboFuns :+ GumboRustUtil.processGumboBodyMethod(
            m = g,

            owner = gclLib.name,
            optComponent = None(),
            isLibraryMethod = T,

            inVerus = T,
            aadlTypes = types,
            tp = CRustTypePlugin.getCRustTypeProvider(store).get,
            gclSymbolTable = gclSymbolTable,
            store = store,
            reporter = reporter)
      }
    }

    return (gumboFuns.asInstanceOf[ISZ[RAST.Item]], developerUifFuns.asInstanceOf[ISZ[RAST.Item]])
  }

  @pure def handleIntegrationConstraints(thread: AadlThread,
                                         subclauseInfo: GclAnnexClauseInfo,

                                         types: AadlTypes,
                                         tp: CRustTypeProvider,
                                         store: Store,
                                         reporter: Reporter): (Map[String, RAST.Expr], Map[String, RAST.Expr]) = {
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

              val subs = Map.empty[String, String] + p.identifier ~>
                s"${CRustApiPlugin.apiResultName}${if (isEvent) ".unwrap()" else ""}"

              val rspec: RAST.Expr = GumboRustUtil.processGumboSpecH(
                spec = a,
                component = thread,
                context = Context.integration_constraint,
                substitutions = subs,
                isAssumeRequires = F,
                types = types,
                tp = tp,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)

              if (isEvent) {
                ensures = ensures + p.identifier ~> RAST.ExprST(
                  st"""(res.is_none() ||
                      |  ${rspec.prettyST})""")
              } else {
                ensures = ensures + p.identifier ~> rspec
              }

            case g: GclGuarantee =>
              // guarantee integration clauses can only be applied to outgoing ports.  They become
              // requirements on the param value passed to the api -- the param's name will always be 'value'

              val subs = Map.empty[String, String] + p.identifier ~> s"${CRustApiPlugin.apiParameterName}"
              requires = requires + p.identifier ~> GumboRustUtil.processGumboSpecH(
                spec = g,
                component = thread,
                context = Context.integration_constraint,

                substitutions = subs,

                isAssumeRequires = F,

                types = types,
                tp = tp,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)
          }
        case _ =>
      }
    }
    return (requires, ensures)
  }

  @pure def handleInitialize(fn: RAST.FnImpl,
                             thread: AadlThread,
                             subclauseInfo: GclAnnexClauseInfo,

                             types: AadlTypes,
                             tp: CRustTypeProvider,
                             symbolTable: SymbolTable,
                             store: Store,
                             reporter: Reporter): (Marker, RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    val ensures: ISZ[RAST.Expr] = for (g <- subclauseInfo.annex.initializes.get.guarantees) yield
      GumboRustUtil.processGumboSpec(
        spec = g,
        component = thread,
        context = Context.initialize_clause,

        isAssumeRequires = F,

        types = types,
        tp = tp,
        gclSymbolTable = subclauseInfo.gclSymbolTable,
        store = store,
        reporter = reporter)


    val ensuresMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.initializationEnsures)
    return (ensuresMarker, fn(contract = Some(RAST.FnContract(
      optRequiresMarker = None(),
      requires = ISZ(),
      optEnsuresMarker = Some(ensuresMarker),
      ensures = ensures))))
  }

  @pure def handleInitializePlaceholder(fn: RAST.FnImpl): (Marker, RAST.FnImpl) = {
    val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.initializationEnsures)
    return (m, fn(contract = Some(RAST.FnContract(
      optRequiresMarker = None(),
      requires = ISZ(),
      optEnsuresMarker = Some(m),
      ensures = ISZ()))))
  }

  @pure def handleCompute(fn: RAST.FnImpl,
                          thread: AadlThread,
                          subclauseInfo: GclAnnexClauseInfo,

                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          symbolTable: SymbolTable,
                          store: Store,
                          reporter: Reporter): (ISZ[Marker], RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    // general assumes clauses
    var requires: ISZ[RAST.Expr] =
      for (r <- subclauseInfo.annex.compute.get.assumes) yield
        GumboRustUtil.processGumboSpec(
          spec = r,
          component = thread,
          context = Context.compute_clause,
          isAssumeRequires = T,
          types = types,
          tp = tp,
          gclSymbolTable = subclauseInfo.gclSymbolTable,
          store = store,
          reporter = reporter)

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
      (for (r <- subclauseInfo.annex.compute.get.guarantees) yield
        GumboRustUtil.processGumboSpec(
          spec = r,
          component = thread,
          context = Context.compute_clause,
          isAssumeRequires = F,
          types = types,
          tp = tp,
          gclSymbolTable = subclauseInfo.gclSymbolTable,
          store = store,
          reporter = reporter)) ++
        // gumbo compute cases clauses
        (for (c <- subclauseInfo.annex.compute.get.cases) yield
          GumboRustUtil.processGumboCase(
            c = c,
            component = thread,
            store = store,
            aadlTypes = types ,
            tp = tp,
            gclSymbolTable = subclauseInfo.gclSymbolTable,
            reporter = reporter))
    }

    var optEnsuresMarker: Option[Marker] = None()
    var optRequiresMarker: Option[Marker] = None()
    var markers: ISZ[Marker] = ISZ()

    if (requires.nonEmpty) {
      val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
      markers = markers :+ m
      optRequiresMarker = Some(m)
    } else {
      val p = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
      markers = markers :+ p
      optRequiresMarker = Some(p)
    }

    if (ensures.nonEmpty) {
      val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
      markers = markers :+ m
      optEnsuresMarker = Some(m)
    } else {
      val p = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
      markers = markers :+ p
      optEnsuresMarker = Some(p)
    }

    return (markers,
      fn(contract = Some(RAST.FnContract(
        optRequiresMarker = optRequiresMarker,
        requires = requires,
        optEnsuresMarker = optEnsuresMarker,
        ensures = ensures))))
  }

  @pure def handleComputePlaceholder(fn: RAST.FnImpl): (ISZ[Marker], RAST.FnImpl) = {
    val requiresPlaceholder = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
    val ensuresPlaceholder = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
    val markers: ISZ[Marker] = ISZ(requiresPlaceholder, ensuresPlaceholder)
    return (markers,
      fn(contract = Some(RAST.FnContract(
        optRequiresMarker = Some(requiresPlaceholder),
        requires = ISZ(),
        optEnsuresMarker = Some(ensuresPlaceholder),
        ensures = ISZ()))))
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    GumboRustPlugin.getGumboRustContributions(store) match {
      case Some(c) =>
        // write out any gumbo library annex crates

        assert (c.getLibraryAnnexes.nonEmpty)
        var localStore = store
        var resources: ISZ[Resource] = ISZ()

        for (e <- c.getLibraryAnnexes.entries) {

          val rootDir = s"${options.sel4OutputDir.get}/crates/${e._1}"

          { // crates/<aadl-package-name>/src/lib.rs
            var markers: ISZ[Marker] = ISZ()

            val path = s"$rootDir/src/lib.rs"

            val rustMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboLibRust)
            markers = markers :+ rustMarker

            val rustSection = RAST.MarkerWrap(
              marker = rustMarker,
              items = e._2.rustItems,
              sep = "\n\n",
              optLastItemSep = None())

            val verusMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboLibVerus)
            markers = markers :+ verusMarker

            val verusSection: ISZ[RAST.Item] = ISZ[RAST.Item] (
              RAST.MarkerWrap(
                marker = verusMarker,
                items = e._2.verusItems,
                sep = "\n\n",
                optLastItemSep = None())) ++ e._2.verusDeveloperItems

            val verusMacro = RAST.MacCall(
              macName = "verus",
              items = verusSection)

            val content =
              st"""#![cfg_attr(not(test), no_std)]
                  |
                  |${RustUtil.defaultCrateLevelAttributes}
                  |
                  |${MicrokitUtil.safeToEdit}
                  |
                  |use data::*;
                  |use vstd::prelude::*;
                  |
                  |${GumboRustUtil.RustImplicationMacros}
                  |
                  |${rustSection.prettyST}
                  |
                  |${verusMacro.prettyST}
                  |"""

            resources = resources :+ ResourceUtil.createResourceWithMarkers(
              path = path, content = content, markers = markers, invertMarkers = F, overwrite = F)
          }

          { // crates/<aadl-package-name>/Cargo.toml
            val content = st"""${MicrokitUtil.safeToEditMakefile}
                              |
                              |[package]
                              |name = "${e._1}"
                              |version = "0.1.0"
                              |edition = "2021"
                              |
                              |[dependencies]
                              |data = { path="../data" }
                              |${RustUtil.verusCargoDependencies(store)}
                              |
                              |${RustUtil.commonCargoTomlEntries}
                              |"""
            val cargoTomlPath = s"${rootDir}/Cargo.toml"
            resources = resources :+ ResourceUtil.createResourceH(path = cargoTomlPath, content = content, overwrite = F, isDatatype = T)
          }

          { // crates/<aadl-package-name>/rust-toolchain.toml
            val content = RustUtil.defaultRustToolChainToml

            val rusttoolchain = s"${rootDir}/rust-toolchain.toml"
            resources = resources :+ ResourceUtil.createResourceH(path = rusttoolchain, content = content, overwrite = F, isDatatype = T)

          }
        }

        return (localStore + s"FINALIZED_$name" ~> BoolValue(T), resources)

      case _ => halt("Infeasible")
    }
  }
}

@datatype class DefaultGumboRustPlugin extends GumboRustPlugin {
  @strictpure override def name: String = "DefaultGumboRustPlugin"
}

