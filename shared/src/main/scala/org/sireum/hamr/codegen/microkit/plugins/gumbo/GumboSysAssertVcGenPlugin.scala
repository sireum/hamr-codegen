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
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypePlugin
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, RustUtil}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

@sig trait GumboSysAssertVcGenPlugin extends MicrokitPlugin {

  @strictpure def alreadyHandled(store: Store): B = store.contains(s"HANDLED_${name}")

  @pure override def canHandle(model: Aadl,
                               options: HamrCli.CodegenOption,
                               types: AadlTypes,
                               symbolTable: SymbolTable,
                               store: Store,
                               reporter: Reporter): B = {
    return (options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !reporter.hasError &&
      !isDisabled(store) &&
      !alreadyHandled(store) &&
      VCGenerator.hasCompositions(symbolTable) &&
      // For a rusty model, run after GumboRustPlugin so its component `verus` makefile
      // target is registered before this plugin appends the `verus-sys-proof` call to
      // it (keeping `verus` ahead of `verus-sys-proof` in the merged target); waiting on
      // GumboRustPlugin also guarantees the Rust type provider it depends on is ready. A
      // non-rusty model has no proof crates to emit, but still runs once to surface the
      // "no Rust components" warning below.
      (!MicrokitPlugin.modelIsRusty(store) || GumboRustPlugin.getGumboRustContributions(store).nonEmpty))
  }

  @pure override def handle(model: Aadl,
                            options: HamrCli.CodegenOption,
                            types: AadlTypes,
                            symbolTable: SymbolTable,
                            store: Store,
                            reporter: Reporter): (Store, ISZ[Resource]) = {
    // Mark handled so the Microkit handle fixpoint loop does not re-invoke this plugin
    // every pass (canHandle gates on !alreadyHandled). Without this the loop never
    // terminates because the plugin otherwise always reports it can handle.
    val handledStore = store + s"HANDLED_${name}" ~> BoolValue(T)

    val compositions = VCGenerator.getCompositions(symbolTable)
    if (compositions.isEmpty) {
      return (handledStore, ISZ())
    }

    val tpOpt = CRustTypePlugin.getCRustTypeProvider(store)
    if (tpOpt.isEmpty) {
      reporter.warn(None(), name,
        "CRustTypeProvider is not available (model has no Rust components?); skipping system VC serialization")
      return (handledStore, ISZ())
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
      val crateName = VerusVCSerializer.sysProofCrateName(composition.id)
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

      // Schema components with a GUMBO contract that are NOT implemented in Rust: the
      // system proof uses their guarantees as premises but Verus cannot discharge those
      // contracts, so they are TRUSTED. Alert via the reporter (can be overlooked) AND
      // emit durable, greppable trust records (trusted_assumptions.rs/.md/.json + a
      // Makefile notice on every verify run).
      val trusted = VerusVCSerializer.findTrustedComponents(contracts, frames, symbolTable)
      if (trusted.nonEmpty) {
        val names: ISZ[String] = for (tc <- trusted) yield tc.alias
        reporter.warn(None(), name,
          st"System proof for composition '${composition.id}' TRUSTS the GUMBO contract(s) of ${trusted.size} non-Rust component(s) (${(names, ", ")}): Verus does not discharge these, so they must be verified by other means (e.g., testing). See $rootDir/TRUSTED_ASSUMPTIONS.md".render)
        add(rootDir, "src/trusted_assumptions.rs", VerusVCSerializer.genTrustedAssumptionsRs(composition.id, trusted))
        add(rootDir, "TRUSTED_ASSUMPTIONS.md", VerusVCSerializer.genTrustedAssumptionsMd(composition.id, trusted))
        add(rootDir, "TRUSTED_ASSUMPTIONS.json", VerusVCSerializer.genTrustedAssumptionsJson(composition.id, trusted))
      }

      // integration-constraint VCs: per connected port pair whose destination
      // in-port has an integration assume, prove the sender's out-port guarantee
      // (or `true`) implies it. Static -- shared by the composition, like commutativity.
      val integrationVCs = VCGenerator.generateIntegrationVCs(symbolTable)
      val integrationVCsRs = VerusVCSerializer.genIntegrationVCs(integrationVCs, resolvedAliasMap, symbolTable, tp)

      var totalVCs: Z = commutativityVCs.size + integrationVCs.size
      var propertyModIds: ISZ[String] = ISZ()

      // per-property VC sets over the shared net (D9: abstract bases are not
      // instantiated -- no VC set is generated for them; their bindings have
      // already been folded into the concrete properties that specialize them)
      for (property <- composition.properties if !property.isAbstract) {
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

        // one folder per property (design D8): the property's bound assertions and
        // its four VC modules live together under src/<propModId>/
        val propDir = s"src/$propModId"
        add(rootDir, s"$propDir/mod.rs", VerusVCSerializer.genPropertyModRs())
        add(rootDir, s"$propDir/assertions.rs", VerusVCSerializer.genPropertyAssertionsRs(propModId, assertionFns))
        add(rootDir, s"$propDir/vc_init.rs", seqVCs.vcInitRs)
        add(rootDir, s"$propDir/vc_sequential.rs", seqVCs.vcSequentialRs)
        add(rootDir, s"$propDir/vc_post_pre.rs", seqVCs.vcPostPreRs)
        add(rootDir, s"$propDir/vc_non_disabling.rs", indVCs)
      }

      if (reporter.hasError) {
        return (handledStore, ISZ())
      }

      add(rootDir, "Cargo.toml", VerusVCSerializer.genSysProofCargoToml(crateName, libCrates, store))
      // the toolchain file has no do-not-edit header (it is a shared template), so it
      // is emitted overwrite = F like the other generated crates' toolchain files
      resources = resources :+ ResourceUtil.createResource(
        path = s"$rootDir/rust-toolchain.toml",
        content = RustUtil.defaultRustToolChainToml(store),
        overwrite = F)
      add(rootDir, "src/lib.rs", VerusVCSerializer.genLibRs(composition.id, propertyModIds, trusted.nonEmpty))
      add(rootDir, "src/system_state.rs", VerusVCSerializer.genSystemStateRs(ssm, tp))
      add(rootDir, "src/contracts.rs", VerusVCSerializer.genContractsRs(contracts))
      add(rootDir, "src/assertions.rs", VerusVCSerializer.genSysFnsRs(sysFns))
      add(rootDir, "src/write_frames.rs", VerusVCSerializer.genWriteFramesRs(frames))
      add(rootDir, "src/actions.rs", VerusVCSerializer.genActionsRs(actions))
      add(rootDir, "src/vc_commutativity.rs", commVCsRs)
      add(rootDir, "src/vc_integration.rs", integrationVCsRs)

      // per-property `make <property>` targets that verify only that property's VCs
      add(rootDir, "Makefile", VerusVCSerializer.genSysProofMakefile(propertyModIds, trusted))

      reporter.info(None(), name,
        s"Generated $totalVCs system VCs (${propertyModIds.size} properties) in $rootDir")
    }

    // Register the makefile targets that drive the generated proof crates. Add a
    // `verus-sys-proof` target to system.mk that runs each composition's proof-crate
    // `all` target (cargo-verus verify over every property), and chain it into the
    // main Makefile's `verus` target (contributed by GumboRustPlugin) -- same-named
    // targets are merged by MakefileUtil.getMakefileTargets when the makefiles render.
    var sysProofVerusItems: ISZ[ST] = ISZ()
    for (composition <- compositions) {
      sysProofVerusItems = sysProofVerusItems :+
        st"make -C $${CRATES_DIR}/${VerusVCSerializer.sysProofCrateName(composition.id)} all"
    }
    var localStore = handledStore
    localStore = MakefileUtil.addMakefileTargets(ISZ("system.mk"),
      ISZ(MakefileTarget(name = "verus-sys-proof", allowMultiple = F, dependencies = ISZ(), body = sysProofVerusItems)), localStore)
    localStore = MakefileUtil.addMakefileTargets(ISZ("Makefile"),
      ISZ(MakefileTarget(name = "verus", allowMultiple = F, dependencies = ISZ(st"$${TOP_BUILD_DIR}/Makefile"),
        body = ISZ(st"$${MAKE} -C $${TOP_BUILD_DIR} verus-sys-proof"))), localStore)

    return (localStore, resources)
  }
}

@datatype class DefaultGumboSysAssertVcGenPlugin extends GumboSysAssertVcGenPlugin {

  val name: String = "DefaultGumboSysAssertVcGenPlugin"
}
