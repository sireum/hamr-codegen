// #Sireum

package org.sireum.hamr.codegen.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.codegen.arsit.plugin.BehaviorEntryPointProviderPlugin.{BehaviorEntryPointContributions, ContractBlock, NonCaseContractBlock}
import org.sireum.hamr.codegen.arsit.util.ArsitOptions
import org.sireum.hamr.codegen.arsit.{EntryPoints, ProjectDirectories, Util}
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.message.Reporter

@datatype class GumboPlugin extends BehaviorEntryPointProviderPlugin {

  val name: String = "Gumbo Plugin"

  @strictpure def getAnnexLibraries(symbolTable: SymbolTable): ISZ[GclAnnexLibInfo] =
    symbolTable.annexLibInfos.filter(f => f.isInstanceOf[GclAnnexLibInfo]).map(m => m.asInstanceOf[GclAnnexLibInfo])

  def canHandleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                          optInEventPort: Option[AadlPort],
                                          component: AadlThreadOrDevice,
                                          resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                          arsitOptions: ArsitOptions,
                                          symbolTable: SymbolTable,
                                          aadlTypes: AadlTypes,

                                          store: Store): B = {
    val localGumboStore = GumboXPluginStore.getGumboStore(store)
    val needToProcessGclAnnexLibraries = !localGumboStore.handledAnnexLibraries && getAnnexLibraries(symbolTable).nonEmpty

    val needToProcessComponentsGclSubclause: B = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      // GCL's symbol resolver ensures there's at most one GCL clause per component
      case ISZ(GclAnnexClauseInfo(annex, _)) =>
        (annex.state.nonEmpty && !localGumboStore.handledStateVars.contains(component)) ||
          (annex.methods.nonEmpty && !localGumboStore.handledSubClauseFunctions.contains(component)) ||
          (entryPoint == EntryPoints.initialise && annex.initializes.nonEmpty) ||
          (entryPoint == EntryPoints.compute && annex.compute.nonEmpty)
      case _ => F
    }

    return needToProcessGclAnnexLibraries || needToProcessComponentsGclSubclause
  }

  def handleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                       optInEventPort: Option[AadlPort], // will be populated if processing the event handler for a sporadic component
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
                                       reporter: Reporter): (BehaviorEntryPointContributions, Store) = {
    var localGumboStore = GumboXPluginStore.getGumboStore(store)

    var imports: ISZ[String] = ISZ()
    var preMethodBlocks: ISZ[ST] = ISZ()
    var postMethodBlocks: ISZ[ST] = ISZ()
    var markers: ISZ[Marker] = ISZ()
    var reads: ISZ[ST] = ISZ()
    var requires: ISZ[ST] = ISZ()
    var modifies: ISZ[ST] = ISZ()
    var ensures: ISZ[ST] = ISZ()
    var flows: ISZ[ST] = ISZ()
    var resources: ISZ[Resource] = ISZ()

    if (!localGumboStore.handledAnnexLibraries) {
      for (gclLib <- getAnnexLibraries(symbolTable)) {
        val (content, filename) = GumboGen.processGclLibrary(gclLib, symbolTable, aadlTypes, componentNames.basePackage, store)
        // TODO: treat libraries as datatype files since datatype invariants may use the libraries functions
        //       (i.e. the file containing the library will need to be given to slangcheck)
        resources = resources :+ ResourceUtil.createResourceH(
          Util.pathAppend(projectDirectories.componentDir, filename), content, T, T)
      }
      localGumboStore = localGumboStore(handledAnnexLibraries = T)
    }

    var optSubclauseContractBlock: Option[ContractBlock] = None()

    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>

        if (annex.state.nonEmpty && !localGumboStore.handledStateVars.contains(component)) {
          val p = GumboGen(gclSymbolTable, symbolTable, aadlTypes, componentNames.basePackage).processStateVars(annex.state)
          preMethodBlocks = preMethodBlocks :+ p._1
          markers = markers :+ p._2
          localGumboStore = localGumboStore(handledStateVars = localGumboStore.handledStateVars + component)
        }

        if (annex.methods.nonEmpty && !localGumboStore.handledSubClauseFunctions.contains(component)) {
          val (content, marker, methodImports) = GumboGen.processSubclauseFunctions(annex.methods, gclSymbolTable, symbolTable, aadlTypes, componentNames.basePackage, store)
          preMethodBlocks = preMethodBlocks :+ content
          markers = markers :+ marker
          imports = imports ++ methodImports
          localGumboStore = localGumboStore(handledSubClauseFunctions = localGumboStore.handledSubClauseFunctions + component)
        }

        entryPoint match {
          case EntryPoints.initialise if annex.initializes.nonEmpty =>
            val r = GumboGen.processInitializes(component, symbolTable, aadlTypes, componentNames.basePackage, store).get
            requires = requires ++ r.requires
            modifies = modifies ++ r.modifies
            ensures = ensures ++ r.ensures
            flows = flows ++ r.flows
            markers = markers ++ r.markers
            imports = imports ++ r.imports

          case EntryPoints.compute if annex.compute.nonEmpty =>
            GumboGen(gclSymbolTable, symbolTable, aadlTypes, componentNames.basePackage).processCompute(
              annex.compute.get, optInEventPort, component, store) match {
              case (n: NonCaseContractBlock, mmarkers) =>
                markers = markers ++ mmarkers
                reads = reads ++ n.contractReads
                requires = requires ++ n.contractRequires
                modifies = modifies ++ n.contractModifies
                ensures = ensures ++ n.contractEnsures
                flows = flows ++ n.contractFlows
                imports = imports ++ n.imports

              case _ => halt("Not handling Contract cases yet")
            }
          case _ => // gumbo contracts cannot currently be placed on the other entrypoints
        }

        if (imports.nonEmpty || reads.nonEmpty || requires.nonEmpty || modifies.nonEmpty || ensures.nonEmpty || flows.nonEmpty) {
          optSubclauseContractBlock = Some(NonCaseContractBlock(imports, reads, requires, modifies, ensures, flows))
        }

      case _ =>
        if (!getAnnexLibraries(symbolTable).nonEmpty) {
          halt(s"Infeasible: the system doesn't have GCL annex libraries and ${component.identifier} does not have a GCL subclause so why did the ${this.name} offer agree to handle the component?")
        }
    }

    return (
      BehaviorEntryPointProviderPlugin.PartialMethodContributions(
        imports = imports,
        preMethodBlocks = preMethodBlocks,
        postMethodBlocks = postMethodBlocks,
        markers = markers,
        contractBlock = optSubclauseContractBlock,
        resources = resources,

        tags = ISZ(),
        preObjectBlocks = ISZ(),
        optBody = None(),

        postObjectBlocks = ISZ()),
      store + GumboXPluginStore.key ~> localGumboStore)
  }
}
