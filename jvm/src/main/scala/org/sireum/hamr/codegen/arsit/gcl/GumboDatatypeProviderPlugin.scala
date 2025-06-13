// #Sireum
package org.sireum.hamr.codegen.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.arsit.ProjectDirectories
import org.sireum.hamr.codegen.arsit.plugin.DatatypeProviderPlugin
import org.sireum.hamr.codegen.arsit.templates.{DatatypeTemplate, EnumTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.{AnnexClauseInfo, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArraySizeKind, ArrayType}
import org.sireum.message.Reporter

@datatype class GumboDatatypeProviderPlugin extends DatatypeProviderPlugin {
  @strictpure def name: String = "GUMBO Datatype Provider Plugin"

  @strictpure def canHandleDatatypeProvider(aadlType: AadlType,
                                            resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                            aadlTypes: AadlTypes,
                                            symbolTable: SymbolTable,
                                            store: Store): B =
    resolvedAnnexSubclauses.filter((f: AnnexClauseInfo) => f.isInstanceOf[GclAnnexClauseInfo]).nonEmpty ||
      (aadlType.isInstanceOf[ArrayType] && aadlType.asInstanceOf[ArrayType].kind == ArraySizeKind.Fixed)

  @pure def handleDatatypeProvider(basePackageName: String,
                                   aadlType: AadlType,
                                   datatypeTemplate: IDatatypeTemplate,
                                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                   suggestedFilename: String,
                                   projectDirectories: ProjectDirectories,
                                   aadlTypes: AadlTypes,
                                   symbolTable: SymbolTable,
                                   store: Store,
                                   reporter: Reporter): (DatatypeProviderPlugin.DatatypeContribution, Store) = {

    val subclauses = resolvedAnnexSubclauses.filter((f: AnnexClauseInfo) => f.isInstanceOf[GclAnnexClauseInfo])
    if (subclauses.size > 1) {
      // should be infeasible as sym resolution should have rejected this already
      halt(s"A data component can have at most one GUMBO subclause but ${aadlType.name} has ${subclauses.size}")
    }

    datatypeTemplate match {
      case dt: DatatypeTemplate =>
        val invariants = GumboGen.processInvariants(aadlType, symbolTable, aadlTypes, aadlType.nameProvider.basePackageName, store)

        val imports: ISZ[ST] = for (str <- (Set.empty[String] ++ GumboGen.imports).elements) yield st"$str"

        return (DatatypeProviderPlugin.PartialDatatypeContribution(
          slangSwitches = ISZ(),
          imports = imports,
          datatypeSingletonBlocks = ISZ(),
          datatypeBlocks = invariants,
          payloadSingletonBlocks = ISZ(),
          preBlocks = ISZ(),
          postBlocks = ISZ(),
          resources = ISZ()), store)

      case en: EnumTemplate =>
        halt(s"Not expecting GUMBO contracts on enum types: ${aadlType.name}")
    }
  }
}
