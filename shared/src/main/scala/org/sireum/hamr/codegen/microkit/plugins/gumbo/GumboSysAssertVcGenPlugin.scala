// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.sysvc.{MHIPComputer, ScheduleNextRel, VCGenerator}
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
      VCGenerator.hasCompositions(symbolTable))
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

    val compositions = VCGenerator.getCompositions(symbolTable)
    if (compositions.isEmpty) {
      return (finalizedStore, ISZ())
    }

    val tpOpt = CRustTypePlugin.getCRustTypeProvider(store)
    if (tpOpt.isEmpty) {
      reporter.warn(None(), name,
        "CRustTypeProvider is not available (model has no Rust components?); skipping system VC serialization")
      return (finalizedStore, ISZ())
    }
    val tp = tpOpt.get

    val resolvedAliasMap = GclResolver.getResolvedComponentAliasMap(store)

    // the GUMBO library annex crates hold the Verus library spec fns the copied
    // contracts and system assertions call
    val libCrates: ISZ[String] =
      for (lib <- GumboRustPlugin.getGclLibraryAnnexes(symbolTable)) yield lib.name(0)

    var resources: ISZ[Resource] = ISZ()

    def add(rootDir: String, path: String, content: ST): Unit = {
      resources = resources :+ ResourceUtil.createResource(
        path = s"$rootDir/$path", content = content, overwrite = T)
    }

    // one independently buildable proof crate per composition (design D8)
    for (composition <- compositions) {
      val crateName = s"sys_proof_${composition.id}"
      val rootDir = s"${options.sel4OutputDir.get}/crates/$crateName"

      // schema-derived, property-independent artifacts -- built once per composition
      val nextRel = ScheduleNextRel.build(composition)
      val mhipPairs = MHIPComputer.computeMHIP(nextRel)
      val ssm = VerusVCSerializer.buildSystemStateMap(composition, resolvedAliasMap, symbolTable, types, reporter)
      val contracts = VerusVCSerializer.buildComponentContracts(symbolTable, types, resolvedAliasMap, tp, options, store, reporter)
      val sysFns = VerusVCSerializer.buildSysFns(symbolTable, types, options, tp, store, reporter)
      val frames = VerusVCSerializer.buildWriteFrames(ssm, resolvedAliasMap, symbolTable)
      val actions = VerusVCSerializer.buildActions(ssm, frames, symbolTable, tp, reporter)
      val commutativityVCs = VCGenerator.generateCommutativityVCs(nextRel, mhipPairs, resolvedAliasMap, symbolTable)
      val commVCsRs = VerusVCSerializer.genCommutativityVCs(commutativityVCs, nextRel, actions, resolvedAliasMap, reporter)

      var totalVCs: Z = commutativityVCs.size
      var propertyModIds: ISZ[String] = ISZ()

      // per-property VC sets over the shared net
      for (property <- composition.properties) {
        val propModId = ops.StringOps(property.id).toLower // Rust module ids: lowercased property id
        propertyModIds = propertyModIds :+ propModId

        val decoration = ScheduleNextRel.decorate(nextRel, property, reporter)
        val vcs = VCGenerator.generateForProperty(decoration, nextRel, mhipPairs, resolvedAliasMap, symbolTable)
        totalVCs = totalVCs + vcs.size

        val assertionFns = VerusVCSerializer.buildPropertyAssertions(
          propModId, decoration, ssm, symbolTable, types, tp, store, reporter)
        val seqVCs = VerusVCSerializer.genSequentialVCs(
          propModId, vcs, nextRel, ssm, contracts, frames, assertionFns, resolvedAliasMap, reporter)
        val indVCs = VerusVCSerializer.genPropertyIndependenceVCs(
          propModId, vcs, nextRel, frames, assertionFns, resolvedAliasMap, reporter)

        add(rootDir, s"src/assertions_$propModId.rs", VerusVCSerializer.genPropertyAssertionsRs(propModId, assertionFns))
        add(rootDir, s"src/vc_${propModId}_init.rs", seqVCs.vcInitRs)
        add(rootDir, s"src/vc_${propModId}_sequential.rs", seqVCs.vcSequentialRs)
        add(rootDir, s"src/vc_${propModId}_post_pre.rs", seqVCs.vcPostPreRs)
        add(rootDir, s"src/vc_${propModId}_independence.rs", indVCs)
      }

      if (reporter.hasError) {
        return (finalizedStore, ISZ())
      }

      add(rootDir, "Cargo.toml", VerusVCSerializer.genSysProofCargoToml(crateName, libCrates, store))
      // the toolchain file has no do-not-edit header (it is a shared template), so it
      // is emitted overwrite = F like the other generated crates' toolchain files
      resources = resources :+ ResourceUtil.createResource(
        path = s"$rootDir/rust-toolchain.toml",
        content = RustUtil.defaultRustToolChainToml(store),
        overwrite = F)
      add(rootDir, "src/lib.rs", VerusVCSerializer.genLibRs(composition.id, propertyModIds))
      add(rootDir, "src/system_state.rs", VerusVCSerializer.genSystemStateRs(ssm, tp))
      add(rootDir, "src/contracts.rs", VerusVCSerializer.genContractsRs(contracts))
      add(rootDir, "src/assertions.rs", VerusVCSerializer.genSysFnsRs(sysFns))
      add(rootDir, "src/write_frames.rs", VerusVCSerializer.genWriteFramesRs(frames))
      add(rootDir, "src/actions.rs", VerusVCSerializer.genActionsRs(actions))
      add(rootDir, "src/vc_commutativity.rs", commVCsRs)

      reporter.info(None(), name,
        s"Generated $totalVCs system VCs (${composition.properties.size} properties) in $rootDir")
    }

    return (finalizedStore, resources)
  }
}

@datatype class DefaultGumboSysAssertVcGenPlugin extends GumboSysAssertVcGenPlugin {

  val name: String = "DefaultGumboSysAssertVcGenPlugin"
}
