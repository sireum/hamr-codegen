// #Sireum

package org.sireum.hamr.codegen.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.arsit.gcl.GumboXGen.{ComputeEntryPointHolder, DataInvariantHolder, InitializeEntryPointHolder, IntegrationHolder}
import org.sireum.hamr.codegen.arsit.gcl.GumboXRuntimeMonitoring.RM_Container
import org.sireum.hamr.codegen.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.codegen.arsit.plugin._
import org.sireum.hamr.codegen.arsit.templates.{BridgeEntryPointTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.arsit.util.{ArsitOptions, ReporterUtil}
import org.sireum.hamr.codegen.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

object GumboXPluginStore {
  val key: String = "GumboXPluginStore"

  @pure def empty: GumboXPluginStore = {
    return GumboXPluginStore(
      F, Set.empty, Set.empty,
      ISZ(), Set.empty, Map.empty, Set.empty, Set.empty, ISZ(),
      Map.empty, Map.empty, Map.empty, Map.empty,
      RM_Container.empty)
  }

  @pure def getGumboStore(store: Store): GumboXPluginStore = {
    return store.getOrElse(GumboXPluginStore.key, GumboXPluginStore.empty).asInstanceOf[GumboXPluginStore]
  }
}

@datatype class GumboXPluginStore(val handledAnnexLibraries: B,
                                  val handledStateVars: Set[AadlComponent],
                                  val handledSubClauseFunctions: Set[AadlComponent],

                                  val imports: ISZ[String],

                                  val handledComponents: Set[IdPath],
                                  val prePostContainerMap: Map[IdPath, GumboXGenUtil.Container],
                                  val datatypesWithInvariants: Set[IdPath],
                                  val componentsWithGclSubclauses: Set[IdPath],
                                  val systemTestSuiteRenamings: ISZ[ST],

                                  val dataInvariants: Map[String, DataInvariantHolder],
                                  val integrationClauses: Map[IdPath, ISZ[(IdPath, IntegrationHolder)]],
                                  val initializeEntryPointHolder: Map[IdPath, InitializeEntryPointHolder],
                                  val computeEntryPointHolder: Map[IdPath, ComputeEntryPointHolder],

                                  val runtimeMonitoringContainer: GumboXRuntimeMonitoring.RM_Container
                                 ) extends StoreValue
@datatype class GumboXPlugin
  extends ArsitInitializePlugin with DatatypeProviderPlugin
    with EntryPointProviderPlugin
    with BehaviorEntryPointProviderPlugin
    with PlatformProviderPlugin
    with ArsitFinalizePlugin {

  val name: String = "GumboX Plugin"

  val gumboXGen: GumboXGen = GumboXGen()

  @pure def modelHasGcl(gumboStore: GumboXPluginStore): B = {
    return gumboStore.datatypesWithInvariants.nonEmpty || gumboStore.componentsWithGclSubclauses.nonEmpty
  }

  def getContainer(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes, gumboStore: GumboXPluginStore): GumboXGenUtil.Container = {
    return gumboStore.prePostContainerMap.getOrElse(component.path, GumboXGenUtil.generateContainer(component, componentNames, annexInfo, aadlTypes))
  }

  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      store: GumboXPluginStore): B = {

    if (aadlTypes.rawConnections) {
      return F
    }

    val datatypeInvariantsExist: B = store.datatypesWithInvariants.nonEmpty

    val componentHasGumboSubclauseInfo: B =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        // GCL's symbol resolver ensures there's at most one GCL clause per component
        case ISZ(GclAnnexClauseInfo(annex, _)) =>
          val hasInitContracts: B = annex.initializes.nonEmpty && annex.initializes.get.guarantees.nonEmpty
          val hasComputeContracts: B = annex.compute match {
            case Some(c) => c.cases.nonEmpty || c.assumes.nonEmpty || c.guarantees.nonEmpty || c.handlers.nonEmpty
            case _ => F
          }
          annex.integration.nonEmpty || hasInitContracts || hasComputeContracts
        case _ => F
      }

    return datatypeInvariantsExist || componentHasGumboSubclauseInfo
  }

  /** Common method for entrypoint and behavior provider plugin -- i.e. only needs to be called once
    * per component
    */
  def handle(component: AadlThreadOrDevice,
             componentNames: NameProvider,

             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,
             projectDirectories: ProjectDirectories,

             gstore: GumboXPluginStore,

             store: Store,

             reporter: Reporter): GumboXPluginStore = {

    val gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)] =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) => Some((annex, gclSymbolTable))
        case _ => None()
      }

    var gumboStore = gstore

    gumboStore = gumboXGen.processAnnex(component, componentNames,
      gclSubclauseInfo,
      componentNames.basePackage, symbolTable, aadlTypes, projectDirectories,
      gumboStore, store, reporter)

    gumboStore = gumboStore(handledComponents = gumboStore.handledComponents + component.path)

    return gumboStore
  }

  /******************************************************************************************
   * ArsitInitializePlugin
   ******************************************************************************************/

  @strictpure def canHandleArsitInitializePlugin(arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable): B =
    !aadlTypes.rawConnections

  override def handleArsitInitializePlugin(projectDirectories: ProjectDirectories,
                                           arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable,
                                           store: Store, reporter: Reporter): (ISZ[Resource], Store) = {
    var gumboStore = GumboXPluginStore.getGumboStore(store)
    for (aadlType <- aadlTypes.typeMap.entries if symbolTable.annexClauseInfos.contains(ISZ(aadlType._1));
         annex <- symbolTable.annexClauseInfos.get(ISZ(aadlType._1)).get) {
      if (annex.isInstanceOf[GclAnnexClauseInfo]) {
        gumboStore =
          gumboStore(datatypesWithInvariants =
            gumboStore.datatypesWithInvariants + aadlType._2.nameProvider.classifier)
      }
    }

    for (thread <- symbolTable.getThreads() if symbolTable.annexClauseInfos.contains(thread.path);
         annex <- symbolTable.annexClauseInfos.get(thread.path).get) {
      if (annex.isInstanceOf[GclAnnexClauseInfo]) {
        gumboStore = gumboStore(componentsWithGclSubclauses = gumboStore.componentsWithGclSubclauses + thread.path)
      }
    }
    return (ISZ(), store + (GumboXPluginStore.key ~> gumboStore))
  }


  /******************************************************************************************
   * DatatypeProvider
   *
   * Adds executable version of a datatype's GCL invariants to the datatype's
   * companion object
   ******************************************************************************************/

  @pure def canHandleDatatypeProvider(aadlType: AadlType, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo], aadlTypes: AadlTypes, symbolTable: SymbolTable, store: Store): B = {
    return GumboXPluginStore.getGumboStore(store).datatypesWithInvariants.contains(aadlType.nameProvider.classifier)
  }

  override def handleDatatypeProvider(basePackageName: String,
                                      aadlType: AadlType,
                                      datatypeTemplate: IDatatypeTemplate,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                      suggestedFilename: String,
                                      projectDirectories: ProjectDirectories,
                                      aadlTypes: AadlTypes,
                                      symbolTable: SymbolTable,
                                      store: Store,
                                      reporter: Reporter): (DatatypeProviderPlugin.DatatypeContribution, Store) = {
    for (a <- resolvedAnnexSubclauses if a.isInstanceOf[GclAnnexClauseInfo]) {
      // there should only be one Gumbo Annex Clause per component so just return once we found it
      val ret = gumboXGen.processDatatype(aadlType, a.asInstanceOf[GclAnnexClauseInfo], basePackageName, symbolTable, aadlTypes, projectDirectories, GumboXPluginStore.getGumboStore(store), store, ReporterUtil.reporter)
      return (ret._1, store + GumboXPluginStore.key ~> ret._2)
    }
    halt(s"Infeasible: why did ${name} offer to handle ${aadlType.name} if it doesn't have a GclSubclause attached to it?")
  }


  /******************************************************************************************
   * PlatformProviderPlugin - only called once by codegen
   *
   * Adds GumboX runtime monitoring artifacts
   ******************************************************************************************/

  override def canHandlePlatformProviderPlugin(arsitOptions: ArsitOptions,
                                               symbolTable: SymbolTable,
                                               aadlTypes: AadlTypes): B = {
    return arsitOptions.runtimeMonitoring // && modelHasGcl
  }

  override def handlePlatformProviderPlugin(projectDirectories: ProjectDirectories,
                                            arsitOptions: ArsitOptions,
                                            symbolTable: SymbolTable,
                                            aadlTypes: AadlTypes,
                                            store: Store,
                                            reporter: Reporter): ISZ[PlatformProviderPlugin.PlatformContributions] = {

    return GumboXRuntimeMonitoring.handlePlatformProviderPlugin(
      rmContainer = GumboXPluginStore.getGumboStore(store).runtimeMonitoringContainer,
      hasGcl = modelHasGcl(GumboXPluginStore.getGumboStore(store)),
      basePackageName = arsitOptions.packageName,
      projectDirectories = projectDirectories)
  }

  /******************************************************************************************
   * EntryPoint provider
   *
   * Adds runtime monitoring hooks into the component's initialize and compute methods
   ******************************************************************************************/

  override def canHandleEntryPointProvider(component: AadlThreadOrDevice,
                                           resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                           arsitOptions: ArsitOptions,
                                           symbolTable: SymbolTable,
                                           aadlTypes: AadlTypes): B = {
    return arsitOptions.runtimeMonitoring
  }

  override def handleEntryPointProvider(component: AadlThreadOrDevice,
                                        componentNames: NameProvider,
                                        ports: ISZ[Port],

                                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                        entryPointTemplate: BridgeEntryPointTemplate,

                                        arsitOptions: ArsitOptions,
                                        symbolTable: SymbolTable,
                                        aadlTypes: AadlTypes,
                                        projectDirectories: ProjectDirectories,
                                        store: Store,
                                        reporter: Reporter): (EntryPointProviderPlugin.EntryPointContributions, Store) = {

    var hasStateVariables: B = F

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) =>
        hasStateVariables = annex_.state.nonEmpty
        Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    var gumboStore = GumboXPluginStore.getGumboStore(store)

    val containers = getContainer(component, componentNames, annexInfo, aadlTypes, gumboStore)

    gumboStore = handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, gumboStore, store, reporter)

    if (arsitOptions.runtimeMonitoring) {
      gumboStore = gumboStore(systemTestSuiteRenamings = gumboStore.systemTestSuiteRenamings :+
        st"// import ${componentNames.packageName}.{${componentNames.componentSingletonType}_SystemTestAPI => nickname}")
    }

    val r = GumboXRuntimeMonitoring.handleEntryPointProvider(
      component, componentNames, entryPointTemplate, gumboXGen, containers, hasStateVariables, annexInfo,
      aadlTypes, projectDirectories, gumboStore)
    gumboStore = r._2

    return (r._1,
      store + GumboXPluginStore.key ~> gumboStore)
  }

  /******************************************************************************************
  * Behavior EntryPoint provider
  ******************************************************************************************/

  override def canHandleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                                   optInEventPort: Option[AadlPort],
                                                   component: AadlThreadOrDevice,
                                                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                   arsitOptions: ArsitOptions,
                                                   symbolTable: SymbolTable,
                                                   aadlTypes: AadlTypes,
                                                   store: Store): B = {
    val gumboStore = GumboXPluginStore.getGumboStore(store)
    return !gumboStore.handledComponents.contains(component.path) &&
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes, gumboStore)
  }

  override def handleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                                optInEventPort: Option[AadlPort],
                                                component: AadlThreadOrDevice,
                                                componentNames: NameProvider,
                                                excludeComponentImplementation: B,

                                                methodSignature: String,
                                                defaultMethodBody: ST,

                                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                symbolTable: SymbolTable,
                                                aadlTypes: AadlTypes,
                                                projectDirectories: ProjectDirectories,
                                                arsitOptions: ArsitOptions,

                                                store: Store,
                                                reporter: Reporter): (BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions, Store) = {

    val gumboStore = handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, GumboXPluginStore.getGumboStore(store), store, reporter)

    return (BehaviorEntryPointProviderPlugin.emptyPartialContributions, store + GumboXPluginStore.key ~> gumboStore)
  }

  override def canFinaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                     resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                     arsitOptions: ArsitOptions,
                                                     symbolTable: SymbolTable,
                                                     aadlTypes: AadlTypes,
                                                     store: Store): B = {
    return arsitOptions.runtimeMonitoring ||
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes, GumboXPluginStore.getGumboStore(store))
  }

  override def finaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                  componentNames: NameProvider,
                                                  resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                  symbolTable: SymbolTable,
                                                  aadlTypes: AadlTypes,
                                                  projectDirectories: ProjectDirectories,
                                                  arsitOptions: ArsitOptions,
                                                  store: Store,
                                                  reporter: Reporter): Option[ObjectContributions] = {

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) => Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    var resources = ISZ[Resource]()

    val gumbox = gumboXGen.finalise(component, componentNames, projectDirectories, GumboXPluginStore.getGumboStore(store))

    val containers = getContainer(component, componentNames, annexInfo, aadlTypes, GumboXPluginStore.getGumboStore(store))
    val containersPath = s"${projectDirectories.dataDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}_Containers.scala"
    resources = resources :+ ResourceUtil.createResourceH(containersPath, containers.genContainers(), T, T)

    if (canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes, GumboXPluginStore.getGumboStore(store))) {
      val testHarness = gumboXGen.createTestHarness(component, componentNames, containers, annexInfo, arsitOptions.runSlangCheck, symbolTable, aadlTypes, projectDirectories, GumboXPluginStore.getGumboStore(store))

      val profilePath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}_Profiles.scala"
      val profilesR = ResourceUtil.createResource(profilePath, containers.genProfiles(), T)

      resources = (resources ++ testHarness.resources) :+ profilesR
    }

    return Some(gumbox(resources = gumbox.resources ++ resources))
  }

  override def canHandleArsitFinalizePlugin(store: Store): B = {
    val gumboStore = GumboXPluginStore.getGumboStore(store)
    return gumboStore.handledComponents.nonEmpty || gumboStore.systemTestSuiteRenamings.nonEmpty
  }

  override def handleArsitFinalizePlugin(projectDirectories: ProjectDirectories, arsitOptions: ArsitOptions, symbolTable: SymbolTable, aadlTypes: AadlTypes, store: Store, reporter: Reporter): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    val gumboStore = GumboXPluginStore.getGumboStore(store)
    if (gumboStore.handledComponents.nonEmpty) {
      val container: ST = GumboXGenUtil.getContainerSig(arsitOptions.packageName)
      val containerPath = s"${projectDirectories.dataDir}/${arsitOptions.packageName}/util/Container.scala"
      resources = resources :+ ResourceUtil.createResourceH(containerPath, container, T, T)

      val unitTestConfig: ST = GumboXGenUtil.genUnitTestConfiguration(arsitOptions.packageName)
      val unitTextConfigPath = s"${projectDirectories.testUtilDir}/${arsitOptions.packageName}/util/UnitTestConfiguration.scala"
      resources = resources :+ ResourceUtil.createResource(unitTextConfigPath, unitTestConfig, T)

      val mutableBase: ST = GumboXGenUtil.genMutableBase(arsitOptions.packageName)
      val mutableBasePath = s"${projectDirectories.testUtilDir}/${arsitOptions.packageName}/util/MutableBase.scala"
      resources = resources :+ ResourceUtil.createResource(mutableBasePath, mutableBase, T)

      val gumboXUtil: ST = GumboXGenUtil.genGumboXUtil(arsitOptions.packageName)
      val gumboXUtilPath = s"${projectDirectories.testUtilDir}/${arsitOptions.packageName}/GumboXUtil.scala"
      resources = resources :+ ResourceUtil.createResource(gumboXUtilPath, gumboXUtil, T)
    }
    if (gumboStore.systemTestSuiteRenamings.nonEmpty) {
      resources = resources :+ GumboXRuntimeMonitoring.genSystemTest(
        arsitOptions.packageName, gumboStore.systemTestSuiteRenamings, projectDirectories)
    }
    return resources
  }
}
