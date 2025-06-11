// #Sireum
package org.sireum.hamr.codegen.arsit.plugin

import org.sireum._
import org.sireum.hamr.codegen.arsit.bts.BlessBehaviorProviderPlugin
import org.sireum.hamr.codegen.arsit.gcl.{GumboDatatypeProviderPlugin, GumboPlugin, GumboXPlugin}
import org.sireum.hamr.codegen.arsit.plugin.BehaviorEntryPointProviderPlugin.{BehaviorEntryPointContributions, ObjectContributions}
import org.sireum.hamr.codegen.arsit.templates.{BridgeEntryPointTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.arsit.util.ArsitOptions
import org.sireum.hamr.codegen.arsit.{EntryPoints, Port, ProjectDirectories, plugin}
import org.sireum.hamr.codegen.common.CommonUtil.{Store, toolName}
import org.sireum.hamr.codegen.common.containers.{FileResource, Marker, Resource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.message.Reporter

object ArsitPlugin {

  @strictpure def defaultPlugins: ISZ[Plugin] = ISZ(
    SingletonBridgeCodeProviderPlugin(),
    SingletonEntryPointProviderPlugin(),
    DefaultArsitConfigurationPlugin()
  )

  @strictpure def blessPlugins: ISZ[Plugin] = ISZ[Plugin](BlessBehaviorProviderPlugin())

  @strictpure def gumboPlugins: ISZ[Plugin] = ISZ[Plugin](
    GumboDatatypeProviderPlugin(),
    GumboPlugin(),
    GumboXPlugin())

  @strictpure def gumboEnhancedPlugins: ISZ[Plugin] = gumboPlugins ++ defaultPlugins


  @memoize def getBridgeCodeProvidersIndices(plugins: ISZ[Plugin]): ISZ[Z] = {
    var ret: ISZ[Z] = ISZ()
    for (i <- 0 until plugins.size if plugins(i).isInstanceOf[BridgeCodeProviderPlugin]) {
      ret = ret :+ i
    }
    if (ret.size != 1) {
      halt("Only the default bridge code provider is currently allowed")
    }
    return ret
  }

  def getEntryPointProviderIndices(plugins: ISZ[Plugin],
                                   component: AadlThreadOrDevice,
                                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                   arsitOptions: ArsitOptions,
                                   symbolTable: SymbolTable,
                                   aadlTypes: AadlTypes): ISZ[Z] = {
    var ret: ISZ[Z] = ISZ()
    for (i <- 0 until plugins.size
         if plugins(i).isInstanceOf[EntryPointProviderPlugin] &&
           plugins(i).asInstanceOf[EntryPointProviderPlugin]
             .canHandleEntryPointProvider(component, resolvedAnnexSubclauses, arsitOptions, symbolTable, aadlTypes)) {
      ret = ret :+ i
    }
    if (ret.isEmpty) {
      halt("Infeasible as there is a built in entry point provider")
    }
    return ret
  }

  @strictpure def canHandleBP(p: Plugin, component: AadlThreadOrDevice, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B =
    p.isInstanceOf[BehaviorProviderPlugin] &&
      p.asInstanceOf[BehaviorProviderPlugin].canHandleBehaviorProvider(component, resolvedAnnexSubclauses)

  def canHandleBehaviorProviders(plugins: ISZ[Plugin],
                                 component: AadlThreadOrDevice,
                                 resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = {
    for (p <- plugins if canHandleBP(p, component, resolvedAnnexSubclauses)) {
      return T
    }
    return F
  }
}

@sig trait ArsitPlugin extends Plugin

@sig trait ArsitInitializePlugin extends ArsitPlugin {
  @pure def canHandleArsitInitializePlugin (arsitOptions: ArsitOptions,
                                            aadlTypes: AadlTypes,
                                            symbolTable: SymbolTable): B

  def handleArsitInitializePlugin(projectDirectories: ProjectDirectories,
                                  arsitOptions: ArsitOptions,
                                  aadlTypes: AadlTypes,
                                  symbolTable: SymbolTable,
                                  store: Store,
                                  reporter: Reporter): (ISZ[Resource], Store)
}

object PlatformProviderPlugin {

  @sig trait PlatformContributions {

    def imports: ISZ[ST]

    def blocks: ISZ[ST]

    def resources: ISZ[Resource]
  }

  @datatype class PlatformSetupContributions(val imports: ISZ[ST],
                                             val blocks: ISZ[ST],
                                             val resources: ISZ[Resource]) extends PlatformContributions

  @datatype class PlatformTearDownContributions(val imports: ISZ[ST],
                                                val blocks: ISZ[ST],
                                                val resources: ISZ[Resource]) extends PlatformContributions
}

@sig trait PlatformProviderPlugin extends ArsitPlugin {
  @pure def canHandlePlatformProviderPlugin(arsitOptions: ArsitOptions,
                                            symbolTable: SymbolTable,
                                            aadlTypes: AadlTypes): B

  @pure def handlePlatformProviderPlugin(projectDirectories: ProjectDirectories,
                                         arsitOptions: ArsitOptions,
                                         symbolTable: SymbolTable,
                                         aadlTypes: AadlTypes,

                                         store: Store,
                                         reporter: Reporter): ISZ[PlatformProviderPlugin.PlatformContributions]
}

@sig trait BehaviorProviderPlugin extends ArsitPlugin {

  @pure def canHandleBehaviorProvider(component: AadlThreadOrDevice,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  // allows a plugin to provide its own behavior code implementation for component
  def handleBehaviorProvider(component: AadlThreadOrDevice,
                             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                             suggestedFilename: String,
                             componentDirectory: ISZ[String],

                             symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,

                             reporter: Reporter): ISZ[Resource]
}

object BehaviorEntryPointProviderPlugin {

  @strictpure def emptyObjectContributions: ObjectContributions =
    ObjectContributions(ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())

  @strictpure def emptyPartialContributions: PartialMethodContributions =
    PartialMethodContributions(ISZ(), ISZ(), ISZ(), ISZ(), None(), None(), ISZ(), ISZ(), ISZ(), ISZ())

  @strictpure def emptyFullContributions: FullMethodContributions =
    FullMethodContributions(ISZ(), ISZ(), ISZ(), ISZ(), st"", ISZ(), ISZ(), ISZ(), ISZ())

  @sig trait BehaviorEntryPointContributions

  @sig trait BehaviorEntryPointObjectContributions extends BehaviorEntryPointContributions {
    def tags: ISZ[String]

    def imports: ISZ[String]

    def preObjectBlocks: ISZ[ST]

    def preMethodBlocks: ISZ[ST]

    def postMethodBlocks: ISZ[ST]

    def postObjectBlocks: ISZ[ST]

    def resources: ISZ[Resource]
  }

  @datatype class ObjectContributions(val tags: ISZ[String],
                                      val imports: ISZ[String],
                                      val preObjectBlocks: ISZ[ST],
                                      val preMethodBlocks: ISZ[ST],
                                      val postMethodBlocks: ISZ[ST],
                                      val postObjectBlocks: ISZ[ST],
                                      val resources: ISZ[Resource]) extends BehaviorEntryPointObjectContributions

  @sig trait BehaviorEntryPointMethodContributions extends BehaviorEntryPointObjectContributions {
    def markers: ISZ[Marker]
  }

  // allows plugin to fully provide the behavior code for a method.  An error will be thrown
  // if there are more than one of these plugins for a pipeline, or if BehaviorEntryPointPartialContributions
  // contributes a body or contract block
  @datatype class FullMethodContributions(val tags: ISZ[String],
                                          val imports: ISZ[String],
                                          val preObjectBlocks: ISZ[ST],

                                          val preMethodBlocks: ISZ[ST],
                                          val method: ST, // includes the method sig, optional contract, and body
                                          val postMethodBlocks: ISZ[ST],

                                          val postObjectBlocks: ISZ[ST],

                                          val markers: ISZ[Marker],
                                          val resources: ISZ[Resource]) extends BehaviorEntryPointMethodContributions

  // allows plugin to provide parts of the behavior code for a method that will be
  // combined with those from other plugins in the same pipeline.
  // ContractBlock types must be the same for all plugins (all return cases or general)
  // an error will be thrown if more than one plugin provides an optBody
  @datatype class PartialMethodContributions(val tags: ISZ[String],
                                             val imports: ISZ[String],
                                             val preObjectBlocks: ISZ[ST],

                                             val preMethodBlocks: ISZ[ST],

                                             val contractBlock: Option[ContractBlock],

                                             val optBody: Option[ST],

                                             val postMethodBlocks: ISZ[ST],

                                             val postObjectBlocks: ISZ[ST],

                                             val markers: ISZ[Marker],
                                             val resources: ISZ[Resource]) extends BehaviorEntryPointMethodContributions

  @sig trait ContractBlock

  @datatype class CaseContractBlock(val imports: ISZ[String],
                                    val cases: ISZ[ST]) extends ContractBlock

  @datatype class NonCaseContractBlock(val imports: ISZ[String],
                                       val contractReads: ISZ[ST],
                                       val contractRequires: ISZ[ST],
                                       val contractModifies: ISZ[ST],
                                       val contractEnsures: ISZ[ST],
                                       val contractFlows: ISZ[ST]) extends ContractBlock
}

@sig trait BehaviorEntryPointProviderPlugin extends ArsitPlugin {

  @pure def canHandleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type, // the entry point
                                                optInEventPort: Option[AadlPort], // handler's in event port

                                                component: AadlThreadOrDevice,
                                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                arsitOptions: ArsitOptions,
                                                symbolTable: SymbolTable,
                                                aadlTypes: AadlTypes,

                                                store: Store): B

  // allows a plugin to provide contributions to the generated code for
  // an entrypoint (ie. where developers will add behavior code).
  // BehaviorEntryPointProviderPlugins will not be called
  // for a component if a BehaviorProviderPlugin has already handled the
  // component
  def handleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type, // the entry point
                                       optInEventPort: Option[AadlPort], // handler's in event port
                                       component: AadlThreadOrDevice,
                                       componentNames: NameProvider,
                                       excludeComponentImplementation: B,

                                       methodSignature: String, // e.g. def handlePortName(value: PortType, api: ApiType): Unit
                                       defaultMethodBody: ST,

                                       resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                       symbolTable: SymbolTable,
                                       aadlTypes: AadlTypes,
                                       projectDirectories: ProjectDirectories,
                                       arsitOptions: ArsitOptions,

                                       store: Store,
                                       reporter: Reporter): (BehaviorEntryPointContributions, Store)


  @pure def canFinaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                  resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                  arsitOptions: ArsitOptions,
                                                  symbolTable: SymbolTable,
                                                  aadlTypes: AadlTypes,

                                                  store: Store): B = {
    return F
  }

  // Called prior to codegen writing out the behavior code for the component.
  // This allows plugins the ability, for e.g., to write out blocks they've been
  // collecting to an external resource.
  def finaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                         nameProvider: NameProvider,
                                         resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                         symbolTable: SymbolTable,
                                         aadlTypes: AadlTypes,
                                         projectDirectories: ProjectDirectories,
                                         arsitOptions: ArsitOptions,

                                         store: Store,
                                         reporter: Reporter): Option[ObjectContributions] = {
    return None()
  }
}

object BridgeCodeProviderPlugin {
  @datatype class BridgeCodeContributions(val entryPointTemplate: BridgeEntryPointTemplate,
                                          val e: EntryPointProviderPlugin.EntryPointContributions => ST,
                                          val resources: ISZ[Resource])
}

@sig trait BridgeCodeProviderPlugin extends ArsitPlugin {
  def generate(nameProvider: NameProvider,
               component: AadlThreadOrDevice,
               ports: ISZ[Port],

               symbolTable: SymbolTable,
               aadlTypes: AadlTypes,

               reporter: Reporter): BridgeCodeProviderPlugin.BridgeCodeContributions
}

object EntryPointProviderPlugin {

  @datatype class EntryPointContributions(val imports: ISZ[String],
                                          val bridgeCompanionBlocks: ISZ[String],
                                          val entryPoint: ST,
                                          val resources: ISZ[Resource])
}

@sig trait EntryPointProviderPlugin extends ArsitPlugin {
  @pure def canHandleEntryPointProvider(component: AadlThreadOrDevice,
                                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                        arsitOptions: ArsitOptions,
                                        symbolTable: SymbolTable,
                                        aadlTypes: AadlTypes): B

  def handleEntryPointProvider(component: AadlThreadOrDevice,
                               nameProvider: NameProvider,
                               ports: ISZ[Port],

                               resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                               entryPointTemplate: BridgeEntryPointTemplate,

                               arsitOptions: ArsitOptions,
                               symbolTable: SymbolTable,
                               aadlTypes: AadlTypes,
                               projectDirectories: ProjectDirectories,

                               store: Store,
                               reporter: Reporter): (EntryPointProviderPlugin.EntryPointContributions, Store)
}

object DatatypeProviderPlugin {
  @datatype trait DatatypeContribution

  @datatype class FullDatatypeContribution(val datatype: FileResource,
                                           val resources: ISZ[Resource]) extends DatatypeContribution

  @datatype class PartialDatatypeContribution(val slangSwitches: ISZ[ST],
                                              val imports: ISZ[ST],
                                              val datatypeSingletonBlocks: ISZ[ST],
                                              val datatypeBlocks: ISZ[ST],
                                              val payloadSingletonBlocks: ISZ[ST],
                                              val preBlocks: ISZ[ST],
                                              val postBlocks: ISZ[ST],
                                              val resources: ISZ[Resource]) extends DatatypeContribution

  @strictpure def emptyPartialDatatypeContributions: PartialDatatypeContribution =
    PartialDatatypeContribution(ISZ(),ISZ(),ISZ(),ISZ(),ISZ(),ISZ(),ISZ(),ISZ())
}

@sig trait DatatypeProviderPlugin extends ArsitPlugin {
  @pure def canHandleDatatypeProvider(aadlType: AadlType,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                      aadlTypes: AadlTypes,
                                      symbolTable: SymbolTable,
                                      store: Store): B

  def handleDatatypeProvider(basePackageName: String,
                             aadlType: AadlType,
                             datatypeTemplate: IDatatypeTemplate,
                             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                             suggestedFilename: String,
                             projectDirectories: ProjectDirectories,
                             aadlTypes: AadlTypes,
                             symbolTable: SymbolTable,

                             store: Store,
                             reporter: Reporter): (DatatypeProviderPlugin.DatatypeContribution, Store)
}


object ArsitConfigurationPlugin {

  @pure def getAdditionalPortIds(cliOpt: Z, plugins: ISZ[Plugin], reporter: Reporter): Z = {
    var maxId: Z = cliOpt
    var contributor: String = ""
    for (p <- plugins if p.isInstanceOf[ArsitConfigurationPlugin] &&
      p.asInstanceOf[plugin.ArsitConfigurationPlugin].addPortIds > maxId) {
      maxId = p.asInstanceOf[plugin.ArsitConfigurationPlugin].addPortIds
      contributor = p.name
    }
    if (maxId > cliOpt) {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max port id increment value of $maxId")
    }
    return maxId
  }

  @pure def getAdditionalComponentIds(cliOpt: Z, plugins: ISZ[Plugin], reporter: Reporter): Z = {
    var maxId: Z = cliOpt
    var contributor: String = ""
    for (p <- plugins if p.isInstanceOf[ArsitConfigurationPlugin] &&
      p.asInstanceOf[ArsitConfigurationPlugin].addComponentIds > maxId) {
      maxId = p.asInstanceOf[ArsitConfigurationPlugin].addComponentIds
      contributor = p.name
    }
    if (maxId > cliOpt) {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max component id increment value of $maxId")
    }
    return maxId
  }

  @pure def getAdditionalConnectionIds(cliOpt: Z, plugins: ISZ[Plugin], reporter: Reporter): Z = {
    var maxId: Z = cliOpt
    var contributor: String = ""
    for (p <- plugins if p.isInstanceOf[plugin.ArsitConfigurationPlugin] &&
      p.asInstanceOf[ArsitConfigurationPlugin].addConnectionIds > maxId) {
      maxId = p.asInstanceOf[plugin.ArsitConfigurationPlugin].addConnectionIds
      contributor = p.name
    }
    if (maxId > cliOpt) {
      reporter.info(None(), toolName, s"Plugin $contributor contributed the max connection id increment value of $maxId")
    }
    return maxId
  }
}

@sig trait ArsitConfigurationPlugin extends ArsitPlugin {

  @strictpure def addPortIds: Z = z"0"

  @strictpure def addComponentIds: Z = z"0"

  @strictpure def addConnectionIds: Z = z"0"
}



@sig trait ArsitFinalizePlugin extends ArsitPlugin {
  @pure def canHandleArsitFinalizePlugin (store: Store): B

  def handleArsitFinalizePlugin(projectDirectories: ProjectDirectories,
                                arsitOptions: ArsitOptions,
                                symbolTable: SymbolTable,
                                aadlTypes: AadlTypes,
                                store: Store,
                                reporter: Reporter): ISZ[Resource]
}
