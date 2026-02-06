// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.apis

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object CRustApiPlugin {

  val KEY_CrustApiPlugin: String = "KEY_CRUST_API_PLUGIN"

  @strictpure def getCRustApiContributions(store: Store): Option[CRustApiContributions] = store.get(KEY_CrustApiPlugin).asInstanceOf[Option[CRustApiContributions]]

  @strictpure def putCRustApiContributions(contributions: CRustApiContributions, store: Store): Store = store + KEY_CrustApiPlugin ~> contributions


  // TODO: maybe move everything below into the Store
  @strictpure def apiDirectory(thread: AadlThread, options: HamrCli.CodegenOption): String =
    s"${CRustComponentPlugin.componentCrateDirectory(thread, options)}/src/bridge"

  val apiParameterName: String = "value"

  val apiResultName: String = "res"

  @strictpure def apiModuleName(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_api"

  @strictpure def applicationApiType(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_Application_Api"

  @strictpure def initializationApiType(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_Initialization_Api"

  @strictpure def computeApiType(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_Compute_Api"

  @strictpure def putApiType(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_Put_Api"

  @strictpure def fullApiType(thread: AadlThread): String = s"${MicrokitUtil.getComponentIdPath(thread)}_Full_Api"
}

object ComponentApiContributions {
  @strictpure def empty: ComponentApiContributions = ComponentApiContributions(
    ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())
}

@datatype class ComponentApiContributions( // items for bridge/mod.rs
                                           val bridgeModuleContributions: ISZ[RAST.Item],

                                           // items for bridge/extern_c_api.rs
                                           val externCApis: ISZ[RAST.Item],
                                           val unsafeExternCApiWrappers: ISZ[RAST.Item],
                                           val externApiTestMockVariables: ISZ[RAST.Item],
                                           val externApiTestingApis: ISZ[RAST.Item],

                                           // items for bridge api
                                           val unverifiedPutApis: ISZ[RAST.Item],
                                           val unverifiedGetApis: ISZ[RAST.Item],

                                           val appApiDefaultPutters: ISZ[RAST.Item],
                                           val appApiDefaultGetters: ISZ[RAST.Item],

                                           val ghostVariables: ISZ[RAST.Item],
                                           val ghostInitializations: ISZ[RAST.Item]) {
  @pure def combine(other: ComponentApiContributions): ComponentApiContributions = {
    val ret = this
    return ret(
      bridgeModuleContributions = this.bridgeModuleContributions ++ other.bridgeModuleContributions,

      externCApis = this.externCApis ++ other.externCApis,
      unsafeExternCApiWrappers = this.unsafeExternCApiWrappers ++ other.unsafeExternCApiWrappers,
      externApiTestMockVariables = this.externApiTestMockVariables ++ other.externApiTestMockVariables,
      externApiTestingApis = this.externApiTestingApis ++ other.externApiTestingApis,

      unverifiedPutApis = this.unverifiedPutApis ++ other.unverifiedPutApis,
      unverifiedGetApis = this.unverifiedGetApis ++ other.unverifiedGetApis,

      appApiDefaultPutters = this.appApiDefaultPutters ++ other.appApiDefaultPutters,
      appApiDefaultGetters = this.appApiDefaultGetters ++ other.appApiDefaultGetters,

      ghostVariables = this.ghostVariables ++ other.ghostVariables,
      ghostInitializations = this.ghostInitializations ++ other.ghostInitializations
    )
  }
}

@sig trait CRustApiContributions extends StoreValue {
  @pure def apiContributions: HashSMap[IdPath, ComponentApiContributions]

  @pure def addApiContributions(threadId: IdPath,
                                contributions: ComponentApiContributions): CRustApiContributions
}

@datatype class DefaultCRustApiContributions (val apiContributions: HashSMap[IdPath, ComponentApiContributions]) extends CRustApiContributions {

  @pure override def addApiContributions(threadId: IdPath, contributions: ComponentApiContributions): CRustApiContributions = {
    return this(this.apiContributions + threadId ~> contributions)
  }
}

@sig trait CRustApiPlugin extends MicrokitPlugin with MicrokitFinalizePlugin {

  @strictpure def haveCreatedApis(store: Store): B = CRustApiPlugin.getCRustApiContributions(store).nonEmpty

  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_$name")

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CRustTypePlugin.hasCRustTypeProvider(store) &&
      !haveCreatedApis(store)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !isDisabled(store) &&
        haveCreatedApis(store) &&
        !alreadyFinalized(store))
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

    var ret: HashSMap[IdPath, ComponentApiContributions] = HashSMap.empty

    for (srcThread <- symbolTable.getThreads()) {
      if (MicrokitUtil.isRusty(srcThread)) {
        var contributions = ComponentApiContributions.empty

        for (inPort <- srcThread.getPorts().filter(p => p.direction == Direction.In)) {
          contributions = contributions.combine(CRustApiUtil.processInPort(srcThread, inPort, crustTypeProvider))
        }
        for (outPort <- srcThread.getPorts().filter(p => p.direction == Direction.Out)) {
          contributions = contributions.combine(CRustApiUtil.processOutPort(srcThread, outPort, crustTypeProvider))
        }

        ret = ret + srcThread.path ~> contributions
      }
    } // end processing connections/ports for threads

    return (localStore + CRustApiPlugin.KEY_CrustApiPlugin ~>
      DefaultCRustApiContributions(ret), resources)
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    val contributions = CRustApiPlugin.getCRustApiContributions(store).get
    var resources: ISZ[Resource] = ISZ()

    for (c <- contributions.apiContributions.entries) {
      val thread = symbolTable.componentMap.get(c._1).get.asInstanceOf[AadlThread]
      val threadId = MicrokitUtil.getComponentIdPath(thread)
      val bridgeDir = CRustApiPlugin.apiDirectory(thread, options)

      val reset_test_globals: ISZ[ST] = for(v <- c._2.externApiTestMockVariables) yield
        st"*${v.asInstanceOf[RAST.ItemStatic].ident.string}.lock().unwrap_or_else(|e| e.into_inner()) = None;"

      { // extern_c_api.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |//! C-interface for the component.
              |//! This code must be unsafe.
              |//! Assumptions about correctness are introduced and need to be verified by other means.
              |
              |use ${CRustTypePlugin.usePath};
              |
              |#[cfg(test)]
              |use std::sync::Mutex;
              |
              |#[cfg(not(test))]
              |extern "C" {
              |  ${(for (e <- c._2.externCApis) yield st"${e.prettyST};", "\n")}
              |}
              |
              |${(for (e <- c._2.unsafeExternCApiWrappers) yield e.prettyST, "\n\n")}
              |
              |//////////////////////////////////////////////////////////////////////////////////
              |// Testing Versions
              |//////////////////////////////////////////////////////////////////////////////////
              |
              |#[cfg(test)]
              |lazy_static::lazy_static! {
              |  // simulate the global C variables that point to the microkit shared memory regions.  In a full
              |  // microkit system we would be able to mutate the shared memory for out ports since they're r/w,
              |  // but we couldn't do that for in ports since they are read-only
              |  ${(for (v <- c._2.externApiTestMockVariables) yield v.prettyST, "\n")}
              |}
              |
              |#[cfg(test)]
              |pub fn initialize_test_globals() {
              |  unsafe {
              |    ${(reset_test_globals, "\n")}
              |  }
              |}
              |
              |${(for (a <- c._2.externApiTestingApis) yield a.prettyST, "\n\n")}
              |"""
        val path = s"$bridgeDir/extern_c_api.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // bridge api
        val apiTraitName = s"${threadId}_Api"
        val apiPutTraitName = CRustApiPlugin.putApiType(thread)
        val apiGetTraitName = s"${threadId}_Get_Api"
        val apiFullTraitName = CRustApiPlugin.fullApiType(thread)
        val appInterfaceName = CRustApiPlugin.applicationApiType(thread)
        val initApiTypeName = CRustApiPlugin.initializationApiType(thread)
        val computeApiTypeName = CRustApiPlugin.computeApiType(thread)
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |use vstd::prelude::*;
              |use ${CRustTypePlugin.usePath};
              |use super::extern_c_api as extern_api;
              |
              |verus! {
              |  pub trait $apiTraitName {}
              |
              |  pub trait $apiPutTraitName: $apiTraitName {
              |    ${(for(f <- c._2.unverifiedPutApis) yield f.prettyST, "\n\n")}
              |  }
              |
              |  pub trait $apiGetTraitName: $apiTraitName {
              |    ${(for(f <- c._2.unverifiedGetApis) yield f.prettyST, "\n\n")}
              |  }
              |
              |  pub trait $apiFullTraitName: $apiPutTraitName + $apiGetTraitName {}
              |
              |  pub struct $appInterfaceName<API: $apiTraitName> {
              |    pub api: API,
              |
              |    ${(for (g <- c._2.ghostVariables) yield g.prettyST, ",\n")}
              |  }
              |
              |  impl<API: $apiPutTraitName> $appInterfaceName<API> {
              |    ${(for(f <- c._2.appApiDefaultPutters) yield f.prettyST)}
              |  }
              |
              |  impl<API: $apiGetTraitName> $appInterfaceName<API> {
              |    ${(for(f <- c._2.appApiDefaultGetters) yield f.prettyST)}
              |  }
              |
              |  pub struct $initApiTypeName;
              |  impl $apiTraitName for $initApiTypeName {}
              |  impl $apiPutTraitName for $initApiTypeName {}
              |
              |  pub const fn init_api() -> $appInterfaceName<$initApiTypeName> {
              |    return $appInterfaceName {
              |      api: $initApiTypeName {},
              |
              |      ${(for (g <- c._2.ghostInitializations) yield g.prettyST, ",\n")}
              |    }
              |  }
              |
              |  pub struct $computeApiTypeName;
              |  impl $apiTraitName for $computeApiTypeName {}
              |  impl $apiPutTraitName for $computeApiTypeName {}
              |  impl $apiGetTraitName for $computeApiTypeName {}
              |  impl $apiFullTraitName for $computeApiTypeName {}
              |
              |  pub const fn compute_api() -> $appInterfaceName<$computeApiTypeName> {
              |    return $appInterfaceName {
              |      api: $computeApiTypeName {},

              |      ${(for (g <- c._2.ghostInitializations) yield g.prettyST, ",\n")}
              |    }
              |  }
              |}"""
        val path = s"$bridgeDir/${CRustApiPlugin.apiModuleName(thread)}.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // bridge/mod.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |pub mod extern_c_api;
              |pub mod ${threadId}_api;
              |${(for(c <- c._2.bridgeModuleContributions) yield st"${c.prettyST}", "\n")}"""
        val path = s"$bridgeDir/mod.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }
    }
    return (store + s"FINALIZED_$name" ~> BoolValue(T), resources)
  }
}

@datatype class DefaultCRustApiPlugin extends CRustApiPlugin {

  @strictpure override def name: String = "DefaultCRustApiPlugin"

}