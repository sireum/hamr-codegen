// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.plugin._
import org.sireum.hamr.arsit.templates.{EntryPointTemplate, IDatatypeTemplate}
import org.sireum.hamr.arsit.util.{ArsitOptions, ReporterUtil}
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

@record class GumboXPlugin
  extends ArsitInitializePlugin with DatatypeProviderPlugin
    with EntryPointProviderPlugin
    with BehaviorEntryPointProviderPlugin
    with PlatformProviderPlugin
    with ArsitFinalizePlugin {

  val name: String = "GumboX Plugin"

  val gumboXGen: GumboXGen = GumboXGen()

  var handledComponents: Set[IdPath] = Set.empty

  var prePostContainerMap: Map[IdPath, GumboXGenUtil.Container] = Map.empty

  val runtimeMonitoringContainer: GumboXRuntimeMonitoring.RM_Container = GumboXRuntimeMonitoring.RM_Container(ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())

  var datatypesWithInvariants: Set[IdPath] = Set.empty
  var componentsWithGclSubclauses: Set[IdPath] = Set.empty

  var systemTestSuiteRenamings: ISZ[ST] = ISZ()

  @pure def modelHasGcl: B = {
    return datatypesWithInvariants.nonEmpty || componentsWithGclSubclauses.nonEmpty
  }

  def getContainer(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): GumboXGenUtil.Container = {
    return prePostContainerMap.getOrElse(component.path, GumboXGenUtil.generateContainer(component, componentNames, annexInfo, aadlTypes))
  }

  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes): B = {

    if (aadlTypes.rawConnections) {
      return F
    }

    val datatypeInvariantsExist: B = datatypesWithInvariants.nonEmpty

    val componentHasGumboSubclauseInfo: B =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        // GCL's symbol resolver ensures there's at most one GCL clause per component
        case ISZ(GclAnnexClauseInfo(annex, _)) =>
          val hasInitContracts: B = annex.initializes.nonEmpty && annex.initializes.get.guarantees.nonEmpty
          val hasComputeContracts: B = annex.compute match {
            case Some(c) => c.cases.nonEmpty || c.specs.nonEmpty || c.handlers.nonEmpty
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
             reporter: Reporter): Unit = {

    val gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)] =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) => Some((annex, gclSymbolTable))
        case _ => None()
      }

    gumboXGen.processAnnex(component, componentNames,
      gclSubclauseInfo,
      componentNames.basePackage, symbolTable, aadlTypes, projectDirectories, reporter)

    handledComponents = handledComponents + component.path
  }

  /******************************************************************************************
   * ArsitInitializePlugin
   ******************************************************************************************/

  @strictpure def canHandleArsitInitializePlugin(arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable): B =
    !aadlTypes.rawConnections

  override def handleArsitInitializePlugin(projectDirectories: ProjectDirectories, arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ISZ[FileResource] = {
    for (aadlType <- aadlTypes.typeMap.entries if symbolTable.annexClauseInfos.contains(ISZ(aadlType._1));
         annex <- symbolTable.annexClauseInfos.get(ISZ(aadlType._1)).get) {
      if (annex.isInstanceOf[GclAnnexClauseInfo]) {
        datatypesWithInvariants = datatypesWithInvariants + aadlType._2.nameProvider.classifier
      }
    }

    for (thread <- symbolTable.getThreads() if symbolTable.annexClauseInfos.contains(thread.path);
      annex <- symbolTable.annexClauseInfos.get(thread.path).get) {
      if (annex.isInstanceOf[GclAnnexClauseInfo]) {
        componentsWithGclSubclauses = componentsWithGclSubclauses + thread.path
      }
    }
    return ISZ()
  }


  /******************************************************************************************
   * DatatypeProvider
   *
   * Adds executable version of a datatype's GCL invariants to the datatype's
   * companion object
   ******************************************************************************************/

  @pure def canHandleDatatypeProvider(aadlType: AadlType, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo], aadlTypes: AadlTypes, symbolTable: SymbolTable): B = {
    return datatypesWithInvariants.contains(aadlType.nameProvider.classifier)
  }

  override def handleDatatypeProvider(basePackageName: String,
                                      aadlType: AadlType,
                                      datatypeTemplate: IDatatypeTemplate,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                      suggestedFilename: String,
                                      projectDirectories: ProjectDirectories,
                                      aadlTypes: AadlTypes,
                                      symbolTable: SymbolTable,
                                      reporter: Reporter): DatatypeProviderPlugin.DatatypeContribution = {
    for (a <- resolvedAnnexSubclauses if a.isInstanceOf[GclAnnexClauseInfo]) {
      return gumboXGen.processDatatype(aadlType, a.asInstanceOf[GclAnnexClauseInfo], basePackageName, symbolTable, aadlTypes, projectDirectories, ReporterUtil.reporter)
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
                                            reporter: Reporter): ISZ[PlatformProviderPlugin.PlatformContributions] = {

    return GumboXRuntimeMonitoring.handlePlatformProviderPlugin(
      rmContainer = runtimeMonitoringContainer,
      hasGcl = modelHasGcl,
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
    return arsitOptions.runtimeMonitoring //&&
      //!handledComponents.contains(component.path) &&
      //canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
  }

  override def handleEntryPointProvider(component: AadlThreadOrDevice,
                                        componentNames: NameProvider,
                                        ports: ISZ[Port],

                                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                        entryPointTemplate: EntryPointTemplate,

                                        arsitOptions: ArsitOptions,
                                        symbolTable: SymbolTable,
                                        aadlTypes: AadlTypes,
                                        projectDirectories: ProjectDirectories,
                                        reporter: Reporter): EntryPointProviderPlugin.EntryPointContributions = {

    var hasStateVariables: B = F

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) =>
        hasStateVariables = annex_.state.nonEmpty
        Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    val containers = getContainer(component, componentNames, annexInfo, aadlTypes)

    handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, reporter)

    if (arsitOptions.runtimeMonitoring) {
      systemTestSuiteRenamings = systemTestSuiteRenamings :+
        st"// import ${componentNames.packageName}.{${componentNames.componentSingletonType}_SystemTestAPI => nickname}"
    }

    return GumboXRuntimeMonitoring.handleEntryPointProvider(
      component, componentNames, entryPointTemplate, gumboXGen, containers, hasStateVariables, annexInfo, runtimeMonitoringContainer, aadlTypes, projectDirectories
    )
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
                                                   aadlTypes: AadlTypes): B = {
    return !handledComponents.contains(component.path) &&
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
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
                                                reporter: Reporter): BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions = {

    handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, reporter)

    return BehaviorEntryPointProviderPlugin.emptyPartialContributions
  }

  override def canFinaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                     resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                     arsitOptions: ArsitOptions,
                                                     symbolTable: SymbolTable,
                                                     aadlTypes: AadlTypes): B = {
    return arsitOptions.runtimeMonitoring ||
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
  }

  override def finaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                  componentNames: NameProvider,
                                                  resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                  symbolTable: SymbolTable,
                                                  aadlTypes: AadlTypes,
                                                  projectDirectories: ProjectDirectories,
                                                  arsitOptions: ArsitOptions,
                                                  reporter: Reporter): Option[ObjectContributions] = {

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) => Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    var resources = ISZ[FileResource]()

    val gumbox = gumboXGen.finalise(component, componentNames, projectDirectories)

    val containers = getContainer(component, componentNames, annexInfo, aadlTypes)
    val containersPath = s"${projectDirectories.dataDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}_Containers.scala"
    resources = resources :+ ResourceUtil.createResourceH(containersPath, containers.genContainers(), T, T)

    if (canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)) {
      val testHarness = gumboXGen.createTestHarness(component, componentNames, containers, annexInfo, arsitOptions.runSlangCheck, symbolTable, aadlTypes, projectDirectories)

      val profilePath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}_Profiles.scala"
      val profilesR = ResourceUtil.createResource(profilePath, containers.genProfiles(), T)

      resources = (resources ++ testHarness.resources) :+ profilesR
    }

    return Some(gumbox(resources = gumbox.resources ++ resources))
  }

  override def canHandleArsitFinalizePlugin(): B = {
    return handledComponents.nonEmpty || systemTestSuiteRenamings.nonEmpty
  }

  override def handleArsitFinalizePlugin(projectDirectories: ProjectDirectories, arsitOptions: ArsitOptions, symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): ISZ[FileResource] = {
    var resources: ISZ[FileResource] = ISZ()
    if (handledComponents.nonEmpty) {
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
    if (systemTestSuiteRenamings.nonEmpty) {
      resources = resources :+ GumboXRuntimeMonitoring.genSystemTest(
        arsitOptions.packageName, systemTestSuiteRenamings, projectDirectories)
    }
    return resources
  }
}
