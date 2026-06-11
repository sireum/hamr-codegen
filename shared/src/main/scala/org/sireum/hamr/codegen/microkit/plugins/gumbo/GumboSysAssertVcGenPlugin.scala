// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.sysvc.{ScheduleNextRel, VCGenerator}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.MicrokitFinalizePlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.util.RustUtil
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@sig trait GumboSysAssertVcGenPlugin extends MicrokitFinalizePlugin {

  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_${name}")

  @pure override def canFinalizeMicrokit(model: Aadl,
                                          options: HamrCli.CodegenOption,
                                          types: AadlTypes,
                                          symbolTable: SymbolTable,
                                          store: Store,
                                          reporter: Reporter): B = {
    return (options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !reporter.hasError &&
      !isDisabled(store) &&
      !alreadyFinalized(store) &&
      VCGenerator.hasSystemSchedule(symbolTable))
  }

  @pure override def finalizeMicrokit(model: Aadl,
                                       options: HamrCli.CodegenOption,
                                       types: AadlTypes,
                                       symbolTable: SymbolTable,
                                       store: Store,
                                       reporter: Reporter): (Store, ISZ[Resource]) = {
    // Mark finalized so the Microkit finalize fixpoint loop does not re-invoke this
    // plugin every pass (canFinalizeMicrokit gates on !alreadyFinalized). Without this
    // the loop never terminates because the plugin otherwise always reports it can
    // finalize.
    val finalizedStore = store + s"FINALIZED_${name}" ~> BoolValue(T)

    val scheduleOpt = VCGenerator.getSystemSchedule(symbolTable)
    if (scheduleOpt.isEmpty) {
      return (finalizedStore, ISZ())
    }
    val schedule = scheduleOpt.get

    val tpOpt = CRustTypePlugin.getCRustTypeProvider(store)
    if (tpOpt.isEmpty) {
      reporter.warn(None(), name,
        "CRustTypeProvider is not available (model has no Rust components?); skipping system VC serialization")
      return (finalizedStore, ISZ())
    }
    val tp = tpOpt.get

    val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)
    val nextRel = ScheduleNextRel.build(schedule)
    val vcs = VCGenerator.generate(schedule, nextRel, resolvedAliasMap, symbolTable)

    // serialize the VCs to Verus proof fns in the sys_proof crate
    val ssm = VerusVCSerializer.buildSystemStateMap(schedule, resolvedAliasMap, symbolTable, types, reporter)
    val contracts = VerusVCSerializer.buildComponentContracts(symbolTable, types, resolvedAliasMap, tp, options, store, reporter)
    val assertionsPair = VerusVCSerializer.buildAssertions(nextRel, ssm, symbolTable, types, options, tp, store, reporter)
    val assertionFns = assertionsPair._1
    val sysFns = assertionsPair._2
    val frames = VerusVCSerializer.buildWriteFrames(ssm, resolvedAliasMap, symbolTable)
    val actions = VerusVCSerializer.buildActions(ssm, frames, symbolTable, tp, reporter)
    val seqVCs = VerusVCSerializer.genSequentialVCs(vcs, nextRel, ssm, contracts, frames, assertionFns, resolvedAliasMap, reporter)
    val indVCs = VerusVCSerializer.genIndependenceVCs(vcs, nextRel, frames, assertionFns, actions, resolvedAliasMap, reporter)

    if (reporter.hasError) {
      return (finalizedStore, ISZ())
    }

    // the GUMBO library annex crates hold the Verus library spec fns the copied
    // contracts and system assertions call
    val libCrates: ISZ[String] =
      for (lib <- GumboRustPlugin.getGclLibraryAnnexes(symbolTable)) yield lib.name(0)

    val rootDir = s"${options.sel4OutputDir.get}/crates/sys_proof"
    var resources: ISZ[Resource] = ISZ()

    def add(path: String, content: ST): Unit = {
      resources = resources :+ ResourceUtil.createResource(
        path = s"$rootDir/$path", content = content, overwrite = T)
    }

    add("Cargo.toml", VerusVCSerializer.genSysProofCargoToml(libCrates, store))
    // the toolchain file has no do-not-edit header (it is a shared template), so it
    // is emitted overwrite = F like the other generated crates' toolchain files
    resources = resources :+ ResourceUtil.createResource(
      path = s"$rootDir/rust-toolchain.toml",
      content = RustUtil.defaultRustToolChainToml(store),
      overwrite = F)
    add("src/lib.rs", VerusVCSerializer.genLibRs())
    add("src/system_state.rs", VerusVCSerializer.genSystemStateRs(ssm, tp))
    add("src/contracts.rs", VerusVCSerializer.genContractsRs(contracts))
    add("src/assertions.rs", VerusVCSerializer.genAssertionsRs(assertionFns, sysFns))
    add("src/write_frames.rs", VerusVCSerializer.genWriteFramesRs(frames))
    add("src/actions.rs", VerusVCSerializer.genActionsRs(actions))
    add("src/vc_init.rs", seqVCs.vcInitRs)
    add("src/vc_sequential.rs", seqVCs.vcSequentialRs)
    add("src/vc_post_pre.rs", seqVCs.vcPostPreRs)
    add("src/vc_independence.rs", indVCs)

    reporter.info(None(), name, s"Generated ${vcs.size} system VCs in $rootDir")

    return (finalizedStore, resources)
  }
}

@datatype class DefaultGumboSysAssertVcGenPlugin extends GumboSysAssertVcGenPlugin {

  val name: String = "DefaultGumboSysAssertVcGenPlugin"
}
