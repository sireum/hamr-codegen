// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.sysvc.{ScheduleNextRel, VC, VCKind, WriteFrameBuilder}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.RustUtil
import org.sireum.hamr.ir
import org.sireum.message.Reporter

// Serializes the proof-language-agnostic system VCs (common/sysvc) to Verus proof
// functions in a dedicated `sys_proof` crate. The generated code structure follows
// VerusProofSketch.md in the hamr-system-reasoning-prototype repo: a flat
// `SystemState` struct, copied component contracts, system assertion spec fns,
// write frames, and one empty-body `proof fn` per VC.
object VerusVCSerializer {

  val toolName: String = "VerusVCSerializer"

  @enum object StateFieldKind {
    "Channel" // an AADL connection's shared substrate storage, owned by the source
              // out port (an unconnected port gets its own channel field)
    "StateVar" // a component GUMBO state variable
  }

  // One field of the flat `SystemState` struct. `ownerPath`/`entryId` identify the
  // canonical model element backing the field: for a Channel, the out port that
  // writes it (or an unconnected port); for a StateVar, the declaring component.
  @datatype class StateField(val fieldName: String,
                             val ownerPath: IdPath,
                             val entryId: String,
                             val kind: StateFieldKind.Type,
                             val aadlType: AadlType,
                             val aliasOpt: Option[String])

  // `byComponentEntry` is keyed by (thread path, port-or-state-var name) and maps
  // BOTH endpoints of a connection to the same field -- AADL connections are the
  // shared channels of the Isabelle formalization (wf_Model_InHasUniqueOut), so an
  // in port reads the same storage its source out port writes. `byAlias` maps every
  // schedule-declared port/state-var alias to its field.
  @datatype class SystemStateMap(val fields: ISZ[StateField],
                                 val byAlias: Map[String, StateField],
                                 val byComponentEntry: Map[(IdPath, String), StateField])

  @pure def getPortType(p: AadlPort): AadlType = {
    p match {
      case dp: AadlDataPort => return dp.aadlType
      case edp: AadlEventDataPort => return edp.aadlType
      case _ => return MicrokitTypeUtil.eventPortType
    }
  }

  // Builds the flat system-state field map of one composition:
  //   1. canonicalize: each connected in port shares its source out port's channel
  //   2. enumerate: one field per channel (in thread/port declaration order) plus
  //      one per GUMBO state variable
  //   3. name: composition alias when declared; otherwise the bare port/state-var
  //      name when globally unambiguous; otherwise prefixed with the component's
  //      alias (e.g. MA's `lastCmd` becomes `ma_lastCmd` when MHS's is aliased)
  @pure def buildSystemStateMap(composition: ir.GclComposition,
                                resolvedComponentAliasMap: Map[String, IdPath],
                                symbolTable: SymbolTable,
                                aadlTypes: AadlTypes,
                                reporter: Reporter): SystemStateMap = {
    val threads = symbolTable.getThreads()

    // feature path -> (owning thread path, port), for canonicalization
    var portByFeaturePath: Map[IdPath, (IdPath, AadlPort)] = Map.empty
    for (t <- threads) {
      for (p <- t.getPorts()) {
        portByFeaturePath = portByFeaturePath + p.path ~> ((t.path, p))
      }
    }

    // canonical (thread path, entry id) for a port: a connected in port resolves to
    // its unique source out port (AADL data ports have no fan-in)
    def canonicalOf(threadPath: IdPath, p: AadlPort): (IdPath, String) = {
      if (p.direction == ir.Direction.In) {
        for (ci <- symbolTable.getInConnections(p.path)) {
          ci.src.feature match {
            case Some(srcFeature) =>
              portByFeaturePath.get(srcFeature.name) match {
                case Some((srcThread, srcPort)) => return (srcThread, srcPort.identifier)
                case _ =>
              }
            case _ =>
          }
        }
      }
      return (threadPath, p.identifier)
    }

    // enumerate canonical entries in deterministic (thread, declaration) order
    var canonicalOrder: ISZ[(IdPath, String, StateFieldKind.Type, AadlType)] = ISZ()
    var seen: Set[(IdPath, String)] = Set.empty
    for (t <- threads) {
      for (p <- t.getPorts()) {
        val key = canonicalOf(t.path, p)
        // connected in ports are registered when their source thread is visited
        if (key == ((t.path, p.identifier)) && !seen.contains(key)) {
          seen = seen + key
          canonicalOrder = canonicalOrder :+ ((t.path, p.identifier, StateFieldKind.Channel, getPortType(p)))
        }
      }
      WriteFrameBuilder.getGclInfoOpt(t.path, symbolTable) match {
        case Some(info) =>
          for (sv <- info.annex.state) {
            val key = (t.path, sv.name)
            if (seen.contains(key)) {
              reporter.error(sv.posOpt, toolName,
                st"State variable '${sv.name}' of ${(t.path, ".")} clashes with a port of the same name".render)
            } else {
              aadlTypes.typeMap.get(sv.classifier) match {
                case Some(svType) =>
                  seen = seen + key
                  canonicalOrder = canonicalOrder :+ ((t.path, sv.name, StateFieldKind.StateVar, svType))
                case _ =>
                  reporter.error(sv.posOpt, toolName,
                    s"Could not resolve the type '${sv.classifier}' of state variable '${sv.name}'")
              }
            }
          }
        case _ =>
      }
    }

    // resolve schedule port/state-var aliases to canonical entries
    var aliasTargets: ISZ[(String, (IdPath, String))] = ISZ()

    def resolveAliasComponent(name: ir.Name, what: String): Option[AadlThread] = {
      val segs = name.name
      if (segs.size < 2) {
        reporter.error(name.pos, toolName, st"Expecting '<component alias>.<name>' for $what '${(segs, ".")}'".render)
        return None()
      }
      resolvedComponentAliasMap.get(segs(0)) match {
        case Some(compPath) =>
          symbolTable.componentMap.get(compPath) match {
            case Some(t: AadlThread) => return Some(t)
            case _ =>
              reporter.error(name.pos, toolName, st"'${segs(0)}' of $what '${(segs, ".")}' does not resolve to a thread".render)
              return None()
          }
        case _ =>
          reporter.error(name.pos, toolName, st"Could not resolve component alias '${segs(0)}' of $what '${(segs, ".")}'".render)
          return None()
      }
    }

    for (pa <- composition.portAliases) {
      resolveAliasComponent(pa.portPath, s"port alias '${pa.name}'") match {
        case Some(t) =>
          val portId = pa.portPath.name(pa.portPath.name.lastIndex)
          t.getPorts().filter(p => p.identifier == portId) match {
            case ISZ(port) => aliasTargets = aliasTargets :+ ((pa.name, canonicalOf(t.path, port)))
            case _ =>
              reporter.error(pa.posOpt, toolName,
                st"Could not resolve port '$portId' of port alias '${pa.name}' on ${(t.path, ".")}".render)
          }
        case _ =>
      }
    }
    for (sva <- composition.stateVarAliases) {
      resolveAliasComponent(sva.stateVarPath, s"state var alias '${sva.name}'") match {
        case Some(t) =>
          val svName = sva.stateVarPath.name(sva.stateVarPath.name.lastIndex)
          if (seen.contains((t.path, svName))) {
            aliasTargets = aliasTargets :+ ((sva.name, (t.path, svName)))
          } else {
            reporter.error(sva.posOpt, toolName,
              st"Could not resolve state variable '$svName' of state var alias '${sva.name}' on ${(t.path, ".")}".render)
          }
        case _ =>
      }
    }

    // the first alias declared for a canonical entry names its field; all aliases
    // resolve to the field via byAlias. Schedule alias uniqueness is enforced by a
    // GclResolver linting rule, so the duplicate check below is purely defensive.
    var aliasNameOf: Map[(IdPath, String), String] = Map.empty
    var used: Set[String] = Set.empty
    for (at <- aliasTargets) {
      if (!aliasNameOf.contains(at._2)) {
        if (used.contains(at._1)) {
          reporter.error(None(), toolName, s"Duplicate schedule alias name '${at._1}' (should have been rejected by GclResolver linting)")
        } else {
          aliasNameOf = aliasNameOf + at._2 ~> at._1
          used = used + at._1
        }
      }
    }

    // occurrences of each bare entry id among unaliased entries (a unique bare id
    // can be used as-is; an ambiguous one gets a component prefix)
    var bareCount: Map[String, Z] = Map.empty
    for (e <- canonicalOrder) {
      if (!aliasNameOf.contains((e._1, e._2))) {
        bareCount = bareCount + e._2 ~> (bareCount.getOrElse(e._2, z"0") + 1)
      }
    }

    // component path -> schedule component alias, for prefixing
    val compAliasRev = componentAliasRev(resolvedComponentAliasMap)

    var fields: ISZ[StateField] = ISZ()
    var fieldByKey: Map[(IdPath, String), StateField] = Map.empty
    for (e <- canonicalOrder) {
      val key = (e._1, e._2)
      val aliasOpt = aliasNameOf.get(key)
      val fieldName: String = aliasOpt match {
        case Some(a) => a
        case _ =>
          if (bareCount.get(e._2) == Some(z"1") && !used.contains(e._2)) {
            e._2
          } else {
            val prefix = compAliasRev.getOrElse(e._1, e._1(e._1.lastIndex))
            var cand = s"${prefix}_${e._2}"
            var i = 2
            while (used.contains(cand)) {
              cand = s"${prefix}_${e._2}_$i"
              i = i + 1
            }
            cand
          }
      }
      used = used + fieldName
      val field = StateField(
        fieldName = fieldName,
        ownerPath = e._1,
        entryId = e._2,
        kind = e._3,
        aadlType = e._4,
        aliasOpt = aliasOpt)
      fields = fields :+ field
      fieldByKey = fieldByKey + key ~> field
    }

    // every (thread, port/state var) resolves to its (possibly shared) field
    var byComponentEntry: Map[(IdPath, String), StateField] = Map.empty
    for (t <- threads) {
      for (p <- t.getPorts()) {
        fieldByKey.get(canonicalOf(t.path, p)) match {
          case Some(field) => byComponentEntry = byComponentEntry + (t.path, p.identifier) ~> field
          case _ =>
        }
      }
      WriteFrameBuilder.getGclInfoOpt(t.path, symbolTable) match {
        case Some(info) =>
          for (sv <- info.annex.state) {
            fieldByKey.get((t.path, sv.name)) match {
              case Some(field) => byComponentEntry = byComponentEntry + (t.path, sv.name) ~> field
              case _ =>
            }
          }
        case _ =>
      }
    }

    var byAlias: Map[String, StateField] = Map.empty
    for (at <- aliasTargets) {
      fieldByKey.get(at._2) match {
        case Some(field) => byAlias = byAlias + at._1 ~> field
        case _ =>
      }
    }

    return SystemStateMap(fields = fields, byAlias = byAlias, byComponentEntry = byComponentEntry)
  }

  // Renders crates/sys_proof/src/system_state.rs -- the flat struct holding every
  // channel and state variable in the system (VerusProofSketch.md "System State
  // Struct").
  @pure def genSystemStateRs(m: SystemStateMap, tp: CRustTypeProvider): ST = {
    var sections: ISZ[ST] = ISZ()
    var currentOwner: IdPath = ISZ()
    for (f <- m.fields) {
      if (f.ownerPath != currentOwner) {
        if (currentOwner.nonEmpty) {
          sections = sections :+ st""
        }
        currentOwner = f.ownerPath
        sections = sections :+ st"// -- ${(f.ownerPath, ".")} --"
      }
      val kindComment: String = f.kind match {
        case StateFieldKind.Channel => "channel"
        case StateFieldKind.StateVar => "state variable"
      }
      sections = sections :+
        st"pub ${f.fieldName}: ${tp.getTypeNameProvider(f.aadlType).qualifiedRustName}, // $kindComment"
    }

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Flat system state for the system-level verification conditions.
          |//! One field per AADL connection channel (owned by its source out port),
          |//! unconnected port, and GUMBO state variable across all components.
          |
          |use data::*;
          |use vstd::prelude::*;
          |
          |verus! {
          |
          |pub struct SystemState {
          |  ${(sections, "\n")}
          |}
          |
          |} // verus!
          |""")
  }

  @enum object ContractFnKind {
    "InitializeGuarantee"
    "ComputeAssume"
    "ComputeGuarantee"
    "ComputeCase"
    "IntegrationAssume"
    "IntegrationGuarantee"
  }

  // A copied component contract clause rendered as a Verus `open spec fn`
  // (VerusProofSketch.md "Copied Component Contracts"). Naming and parameter
  // collection reuse the GumboX machinery that produces the component-side
  // contract functions, so the copies match the component-side contracts by
  // construction (TCB item T3). `params` records, for each parameter, whether it
  // is an in port, out port, pre-state (`In_`) or post-state state variable --
  // the VC proof fns use this to pass `pre.<field>` / `post.<field>` arguments.
  @datatype class ContractFn(val fnName: String,
                             val kind: ContractFnKind.Type,
                             val clauseId: String,
                             val params: ISZ[GumboXRustUtil.GGParam],
                             val fnST: ST)

  // All copied contract fns of one component, emitted as `pub mod <moduleName>`
  // so clause ids only need to be unique per component (as on the component side).
  // `methodFns` holds the component's GUMBO subclause functions referenced by the
  // contract clauses: body methods are copied as spec fns; spec methods (developer-
  // defined UIFs) are declared uninterpreted -- the system proof must hold for all
  // interpretations, which includes the developer's actual definition.
  @datatype class ComponentContracts(val componentPath: IdPath,
                                     val moduleName: String,
                                     val methodFns: ISZ[ST],
                                     val fns: ISZ[ContractFn])

  // Renders a component GUMBO spec method (developer-defined UIF) as an
  // uninterpreted spec fn for the proof crate.
  @pure def renderUninterpSpecMethod(m: ir.GclSpecMethod,
                                     aadlTypes: AadlTypes,
                                     crustTypeProvider: CRustTypeProvider): ST = {
    def typeNameOf(t: org.sireum.lang.ast.Type.Named): String = {
      val ids: ISZ[String] = for (id <- t.name.ids) yield id.value
      val aadlType = crustTypeProvider.getRepresentativeType(aadlTypes.getTypeByPath(ids))
      return crustTypeProvider.getTypeNameProvider(aadlType).qualifiedRustName
    }
    val params: ISZ[ST] =
      for (p <- m.sig.params) yield st"${p.id.value}: ${typeNameOf(p.tipe.asInstanceOf[org.sireum.lang.ast.Type.Named])}"
    return (
      st"""/** GUMBO spec method ${m.sig.id.value}: its semantics are defined by a
          |  * developer-supplied function in the component crate; the system proof
          |  * treats it as uninterpreted (sound for any interpretation, including
          |  * the developer's actual definition).
          |  */
          |pub uninterp spec fn ${m.sig.id.value}(${(params, ", ")}) -> ${typeNameOf(m.sig.returnType.asInstanceOf[org.sireum.lang.ast.Type.Named])};""")
  }

  @strictpure def paramDecl(p: GumboXRustUtil.GGParam): ST =
    if (p.isOptional) st"${p.name}: Option<${p.langType}>" else st"${p.name}: ${p.langType}"

  @pure def renderSpecFn(doc: ST, fnName: String, params: ISZ[GumboXRustUtil.GGParam], body: ST): ST = {
    val sig: ST =
      if (params.isEmpty) st"pub open spec fn $fnName() -> bool"
      else
        st"""pub open spec fn $fnName(
            |  ${(for (p <- params) yield paramDecl(p), ",\n")}) -> bool"""
    return (
      st"""$doc
          |$sig
          |{
          |  $body
          |}""")
  }

  // Rewrites one GUMBO clause expression for the proof crate: GumboX param
  // collection (ports/state vars become typed parameters) + Verus rendering.
  @pure def renderContractClause(exp: org.sireum.lang.ast.Exp,
                                 thread: AadlThread,
                                 stateVars: ISZ[ir.GclStateVar],
                                 context: SlangExpUtil.Context.Type,
                                 aadlTypes: AadlTypes,
                                 crustTypeProvider: CRustTypeProvider,
                                 store: Store,
                                 reporter: Reporter): (ISZ[GumboXRustUtil.GGParam], ST) = {
    val gg = GumboXRustUtil.rewriteToExpX(exp, thread, aadlTypes, stateVars,
      GclResolver.getSlangTypeToAadlType(store), crustTypeProvider)
    // identity substitutions so port/state-var references render as the spec fn's
    // parameter names rather than the component-method style `self.x`/`old(self).x`.
    // The rendered expression is "timeless": the pre/post distinction is encoded in
    // the parameter identity (`api_` in-ports and `In_` state vars carry pre-state
    // values; out-ports and bare state vars carry post-state values), so
    // `inRequires` is always F -- the VC proof fns select `pre.<field>` /
    // `post.<field>` arguments per parameter kind at the call site.
    var substitutions: Map[String, String] = Map.empty
    for (p <- gg.params.elements) {
      substitutions = substitutions + p.name ~> p.name
    }
    val rexp = SlangExpUtil.rewriteExpH(
      rexp = gg.exp,
      owner = thread.classifier,
      optComponent = Some(thread),
      context = context,
      substitutions = substitutions,
      inRequires = F,
      inVerus = T,
      tp = crustTypeProvider,
      aadlTypes = aadlTypes,
      store = store,
      reporter = reporter)
    return (GumboXRustUtil.sortParams(gg.params.elements), rexp)
  }

  // Builds the copied contract spec fns for every thread with a GUMBO subclause:
  // initialize guarantees, compute assumes/guarantees, compute cases (a case
  // with an assume renders as the implication `assume ==> guarantee`, matching the
  // component's verified contract), and integration clauses (port invariants the
  // component-level verification discharges at every entrypoint exit / relies on
  // at every dispatch -- I-Guar/I-Assm): guarantees join the component's
  // postcondition and the initial state, assumes join its precondition.
  @pure def buildComponentContracts(symbolTable: SymbolTable,
                                    aadlTypes: AadlTypes,
                                    resolvedComponentAliasMap: Map[String, IdPath],
                                    crustTypeProvider: CRustTypeProvider,
                                    options: HamrCli.CodegenOption,
                                    store: Store,
                                    reporter: Reporter): ISZ[ComponentContracts] = {
    val compAliasRev = componentAliasRev(resolvedComponentAliasMap)

    var result: ISZ[ComponentContracts] = ISZ()
    for (t <- symbolTable.getThreads()) {
      WriteFrameBuilder.getGclInfoOpt(t.path, symbolTable) match {
        case Some(info) =>
          val stateVars = info.annex.state
          var fns: ISZ[ContractFn] = ISZ()

          // the component's GUMBO subclause functions, referenced by its clauses
          var methodFns: ISZ[ST] = ISZ()
          for (m <- info.annex.methods) {
            m match {
              case g: ir.GclBodyMethod =>
                methodFns = methodFns :+ GumboRustUtil.processGumboBodyMethod(
                  m = g,
                  owner = t.classifier,
                  optComponent = Some(t),
                  isLibraryMethod = F,
                  inVerus = T,
                  options = options,
                  aadlTypes = aadlTypes,
                  tp = crustTypeProvider,
                  gclSymbolTable = info.gclSymbolTable,
                  store = store,
                  reporter = reporter).prettyST
              case g: ir.GclSpecMethod =>
                methodFns = methodFns :+ renderUninterpSpecMethod(g, aadlTypes, crustTypeProvider)
            }
          }

          info.annex.initializes match {
            case Some(init) =>
              for (g <- init.guarantees) {
                val (params, body) = renderContractClause(g.exp, t, stateVars, SlangExpUtil.Context.initialize_clause, aadlTypes, crustTypeProvider, store, reporter)
                val fnName = GumboXRustUtil.getInitializeGuaranteeMethodName(g.id)
                fns = fns :+ ContractFn(
                  fnName = fnName,
                  kind = ContractFnKind.InitializeGuarantee,
                  clauseId = g.id,
                  params = params,
                  fnST = renderSpecFn(
                    doc = st"""/** initialize guarantee ${g.id}
                              |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                              |  */""",
                    fnName = fnName, params = params, body = body))
              }
            case _ =>
          }

          info.annex.compute match {
            case Some(compute) =>
              for (g <- compute.assumes) {
                val (params, body) = renderContractClause(g.exp, t, stateVars, SlangExpUtil.Context.compute_clause, aadlTypes, crustTypeProvider, store, reporter)
                val fnName = s"compute_spec_${g.id}_assume"
                fns = fns :+ ContractFn(
                  fnName = fnName,
                  kind = ContractFnKind.ComputeAssume,
                  clauseId = g.id,
                  params = params,
                  fnST = renderSpecFn(
                    doc = st"""/** compute assume ${g.id}
                              |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                              |  */""",
                    fnName = fnName, params = params, body = body))
              }
              for (g <- compute.guarantees) {
                val (params, body) = renderContractClause(g.exp, t, stateVars, SlangExpUtil.Context.compute_clause, aadlTypes, crustTypeProvider, store, reporter)
                val fnName = s"compute_spec_${g.id}_guarantee"
                fns = fns :+ ContractFn(
                  fnName = fnName,
                  kind = ContractFnKind.ComputeGuarantee,
                  clauseId = g.id,
                  params = params,
                  fnST = renderSpecFn(
                    doc = st"""/** compute guarantee ${g.id}
                              |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                              |  */""",
                    fnName = fnName, params = params, body = body))
              }
              for (c <- compute.cases) {
                val (guarParams, guarBody) = renderContractClause(c.guarantees, t, stateVars, SlangExpUtil.Context.compute_clause, aadlTypes, crustTypeProvider, store, reporter)
                val fnName = s"compute_case_${c.id}"
                val (params, body): (ISZ[GumboXRustUtil.GGParam], ST) = c.assumes match {
                  case Some(assm) =>
                    val (assmParams, assmBody) = renderContractClause(assm, t, stateVars, SlangExpUtil.Context.compute_clause, aadlTypes, crustTypeProvider, store, reporter)
                    val combined = GumboXRustUtil.sortParams(
                      (Set.empty[GumboXRustUtil.GGParam] ++ assmParams ++ guarParams).elements)
                    (combined, st"""($assmBody)
                                   |==> ($guarBody)""")
                  case _ => (guarParams, guarBody)
                }
                fns = fns :+ ContractFn(
                  fnName = fnName,
                  kind = ContractFnKind.ComputeCase,
                  clauseId = c.id,
                  params = params,
                  fnST = renderSpecFn(
                    doc = st"""/** compute case ${c.id}
                              |  ${GumboRustUtil.processDescriptor(c.descriptor, "*   ")}
                              |  */""",
                    fnName = fnName, params = params, body = body))
              }
            case _ =>
          }

          // integration clauses: each refers to exactly one port (resolver-enforced;
          // in port => assume, out port => guarantee), so the port itself is the spec
          // fn's single parameter and the clause comes resolved from integrationMap.
          // Rendering uses the compute_clause context rather than
          // integration_constraint: the component-side integration fns live in the
          // bridge/api crate so subclause function calls there are
          // crate::component-qualified, whereas the proof crate copies subclause
          // functions into the component's module, so component-local rendering is
          // wanted. The bare port reference renders as the parameter name via the
          // substitution map (it is not an `api.<port>` access, so the GumboX param
          // collection of renderContractClause does not apply).
          for (port <- t.getPorts()) {
            info.gclSymbolTable.integrationMap.get(port) match {
              case Some(spec) =>
                val param = GumboXRustUtil.portToParam(port, crustTypeProvider)
                val substitutions: Map[String, String] = Map.empty[String, String] + port.identifier ~> param.name
                val body = SlangExpUtil.rewriteExpH(
                  rexp = spec.exp,
                  owner = t.classifier,
                  optComponent = Some(t),
                  context = SlangExpUtil.Context.compute_clause,
                  substitutions = substitutions,
                  inRequires = F,
                  inVerus = T,
                  tp = crustTypeProvider,
                  aadlTypes = aadlTypes,
                  store = store,
                  reporter = reporter)
                val params = ISZ[GumboXRustUtil.GGParam](param)
                spec match {
                  case a: ir.GclAssume =>
                    val fnName = s"integration_spec_${a.id}_assume"
                    fns = fns :+ ContractFn(
                      fnName = fnName,
                      kind = ContractFnKind.IntegrationAssume,
                      clauseId = a.id,
                      params = params,
                      fnST = renderSpecFn(
                        doc = st"""/** integration assume ${a.id} (I-Assm, incoming port ${port.identifier})
                                  |  ${GumboRustUtil.processDescriptor(a.descriptor, "*   ")}
                                  |  */""",
                        fnName = fnName, params = params, body = body))
                  case g: ir.GclGuarantee =>
                    val fnName = s"integration_spec_${g.id}_guarantee"
                    fns = fns :+ ContractFn(
                      fnName = fnName,
                      kind = ContractFnKind.IntegrationGuarantee,
                      clauseId = g.id,
                      params = params,
                      fnST = renderSpecFn(
                        doc = st"""/** integration guarantee ${g.id} (I-Guar, outgoing port ${port.identifier})
                                  |  ${GumboRustUtil.processDescriptor(g.descriptor, "*   ")}
                                  |  */""",
                        fnName = fnName, params = params, body = body))
                  case _ =>
                }
              case _ =>
            }
          }

          if (fns.nonEmpty) {
            result = result :+ ComponentContracts(
              componentPath = t.path,
              moduleName = compAliasRev.getOrElse(t.path, t.identifier),
              methodFns = methodFns,
              fns = fns)
          }
        case _ =>
      }
    }
    return result
  }

  // Renders crates/sys_proof/src/contracts.rs -- per-component modules holding the
  // copied contract spec fns (VerusProofSketch.md "Copied Component Contracts").
  @pure def genContractsRs(contracts: ISZ[ComponentContracts]): ST = {
    var modules: ISZ[ST] = ISZ()
    for (cc <- contracts) {
      val methodsOpt: Option[ST] =
        if (cc.methodFns.isEmpty) None()
        else Some(
          st"""// ---- GUMBO subclause functions ----
              |
              |${(cc.methodFns, "\n\n")}
              |""")
      modules = modules :+
        st"""// -- ${(cc.componentPath, ".")} --
            |pub mod ${cc.moduleName} {
            |  use super::*;
            |
            |  $methodsOpt
            |  ${(for (f <- cc.fns) yield f.fnST, "\n\n")}
            |}"""
    }

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Copies of the component GUMBO contracts for system-level verification.
          |//! The originals live in the component crates, which this crate cannot
          |//! depend on (they are staticlibs); both are generated from the same
          |//! GUMBO source, so they are identical by construction (TCB item T3).
          |
          |#![allow(non_snake_case)]
          |
          |use data::*;
          |use vstd::prelude::*;
          |
          |verus! {
          |
          |${(modules, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // One bound place assertion of one property, rendered as a spec fn over
  // `SystemState` (VerusProofSketch.md "System Assertions"). `placeName` keys the
  // fn for the VC proof fns (an unbound place has no fn; its premises/conclusions
  // render as literal `true`, design D5).
  @datatype class AssertionFn(val placeName: String,
                              val pointName: String,
                              val fnName: String,
                              val fnST: ST)

  // Builds the system-level GUMBO fns (e.g. sysProp_*) from the root system's
  // subclause, rendered as Verus spec fns by the same machinery the component
  // crates use. Property-independent: shared by all of a composition's properties.
  @pure def buildSysFns(symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        options: HamrCli.CodegenOption,
                        crustTypeProvider: CRustTypeProvider,
                        store: Store,
                        reporter: Reporter): ISZ[ST] = {
    val rootSystem = symbolTable.rootSystem

    var sysFns: ISZ[ST] = ISZ()
    GumboRustUtil.getGumboSubclauseOpt(rootSystem.path, symbolTable) match {
      case Some(subclauseInfo) =>
        for (m <- subclauseInfo.annex.methods) {
          m match {
            case g: ir.GclBodyMethod =>
              sysFns = sysFns :+ GumboRustUtil.processGumboBodyMethod(
                m = g,
                owner = rootSystem.classifier,
                optComponent = None(),
                isLibraryMethod = F,
                inVerus = T,
                options = options,
                aadlTypes = aadlTypes,
                tp = crustTypeProvider,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter).prettyST
            case _ =>
              reporter.warn(None(), toolName,
                "Spec methods in system-level GUMBO subclauses are not yet supported")
          }
        }
      case _ =>
    }
    return sysFns
  }

  // Builds one property's place-assertion spec fns from its resolved decoration
  // (place -> binding, from ScheduleNextRel.decorate). The binding expressions
  // reference port/state-var aliases, substituted with `st.<field>` via the
  // SystemStateMap. Fn names are property-qualified
  // (`sys_assert_<property>_<place>`) so each property's assertions live in
  // their own module without collisions.
  @pure def buildPropertyAssertions(propertyId: String,
                                    decoration: Map[ScheduleNextRel.PlaceId, ir.GclPropertyBinding],
                                    ssm: SystemStateMap,
                                    symbolTable: SymbolTable,
                                    aadlTypes: AadlTypes,
                                    crustTypeProvider: CRustTypeProvider,
                                    store: Store,
                                    reporter: Reporter): ISZ[AssertionFn] = {
    val rootSystem = symbolTable.rootSystem

    // composition alias -> st.<field>
    var substitutions: Map[String, String] = Map.empty
    for (e <- ssm.byAlias.entries) {
      substitutions = substitutions + e._1 ~> s"st.${e._2.fieldName}"
    }

    var assertionFns: ISZ[AssertionFn] = ISZ()
    for (e <- decoration.entries) {
      val placeId = e._1
      val b = e._2
      val body = SlangExpUtil.rewriteExpH(
        rexp = b.exp,
        owner = rootSystem.classifier,
        optComponent = Some(rootSystem),
        context = SlangExpUtil.Context.compute_clause,
        substitutions = substitutions,
        inRequires = F,
        inVerus = T,
        tp = crustTypeProvider,
        aadlTypes = aadlTypes,
        store = store,
        reporter = reporter)
      val pointName = b.point.prettyST.render
      val fnName = s"sys_assert_${propertyId}_${placeId.name}"
      assertionFns = assertionFns :+ AssertionFn(
        placeName = placeId.name,
        pointName = pointName,
        fnName = fnName,
        fnST =
          st"""/** property $propertyId, bound '$pointName' (place ${placeId.name})
              |  ${GumboRustUtil.processDescriptor(b.descriptor, "*   ")}
              |  */
              |pub open spec fn $fnName(st: SystemState) -> bool
              |{
              |  $body
              |}""")
    }
    return assertionFns
  }

  // One component's write frames (VerusProofSketch.md "Write Frames"). The global
  // frame asserts `pre.f == post.f` for every SystemState field NOT in the
  // component's write set (out ports + state variables, resolved through the field
  // map so a written out port frames nothing but its own channel). The local frame
  // is `true` -- the component may modify anything in its own scope -- matching the
  // Isabelle instantiation `lFrame := (λ_ _. True)`, which trivially satisfies
  // `wf_WriteFrame_LocalExec`.
  @datatype class WriteFrameFn(val componentPath: IdPath,
                               val componentAlias: String,
                               val globalFnName: String,
                               val localFnName: String,
                               val writeFields: ISZ[String],
                               val framedFields: ISZ[String],
                               val fnST: ST)

  @pure def componentAliasRev(resolvedComponentAliasMap: Map[String, IdPath]): Map[IdPath, String] = {
    var compAliasRev: Map[IdPath, String] = Map.empty
    for (e <- resolvedComponentAliasMap.entries) {
      if (!compAliasRev.contains(e._2)) {
        compAliasRev = compAliasRev + e._2 ~> e._1
      }
    }
    return compAliasRev
  }

  @pure def buildWriteFrames(ssm: SystemStateMap,
                             resolvedComponentAliasMap: Map[String, IdPath],
                             symbolTable: SymbolTable): ISZ[WriteFrameFn] = {
    val compAliasRev = componentAliasRev(resolvedComponentAliasMap)

    var result: ISZ[WriteFrameFn] = ISZ()
    for (t <- symbolTable.getThreads()) {
      val ws = WriteFrameBuilder.computeWriteSet(t, symbolTable)
      var writeFieldNames: Set[String] = Set.empty
      var writeFields: ISZ[String] = ISZ()
      for (p <- ws.outPorts) {
        ssm.byComponentEntry.get((t.path, p.identifier)) match {
          case Some(f) =>
            writeFieldNames = writeFieldNames + f.fieldName
            writeFields = writeFields :+ f.fieldName
          case _ =>
        }
      }
      for (sv <- ws.stateVars) {
        ssm.byComponentEntry.get((t.path, sv.name)) match {
          case Some(f) =>
            writeFieldNames = writeFieldNames + f.fieldName
            writeFields = writeFields :+ f.fieldName
          case _ =>
        }
      }

      val framed = ssm.fields.filter(f => !writeFieldNames.contains(f.fieldName))
      val alias = compAliasRev.getOrElse(t.path, t.identifier)
      val globalFnName = s"${alias}_global_write_frame"
      val localFnName = s"${alias}_local_write_frame"

      val clauses: ISZ[ST] = for (f <- framed) yield st"pre.${f.fieldName} == post.${f.fieldName}"
      val globalBody: ST = if (clauses.isEmpty) st"true" else st"${(clauses, "\n&& ")}"

      val fnST: ST =
        st"""/** ${ops.StringOps(alias).toUpper} writes: ${if (writeFields.isEmpty) "nothing" else st"${(writeFields, ", ")}".render}.
            |  * Everything else must be unchanged.
            |  */
            |pub open spec fn $globalFnName(pre: SystemState, post: SystemState) -> bool
            |{
            |  $globalBody
            |}
            |
            |/** ${ops.StringOps(alias).toUpper} local write frame: the component may modify anything in its
            |  * own scope (Isabelle `lFrame := (λ_ _. True)`).
            |  */
            |pub open spec fn $localFnName(pre: SystemState, post: SystemState) -> bool
            |{
            |  true
            |}"""

      result = result :+ WriteFrameFn(
        componentPath = t.path,
        componentAlias = alias,
        globalFnName = globalFnName,
        localFnName = localFnName,
        writeFields = writeFields,
        framedFields = for (f <- framed) yield f.fieldName,
        fnST = fnST)
    }
    return result
  }

  // Renders crates/sys_proof/src/write_frames.rs.
  @pure def genWriteFramesRs(frames: ISZ[WriteFrameFn]): ST = {
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Write frames -- per component, the global frame constrains every
          |//! SystemState field outside the component's write set to be unchanged
          |//! across its firing; the local frame is `true`.
          |
          |#![allow(non_snake_case)]
          |
          |use vstd::prelude::*;
          |
          |use crate::system_state::SystemState;
          |
          |verus! {
          |
          |${(for (f <- frames) yield f.fnST, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Renders src/assertions.rs: the system-level GUMBO functions (sysProp_*)
  // shared by every property's assertion fns.
  @pure def genSysFnsRs(sysFns: ISZ[ST]): ST = {
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! System-level GUMBO functions (e.g. sysProp_*) called by the
          |//! properties' place-assertion spec functions.
          |
          |#![allow(non_snake_case)]
          |
          |use data::*;
          |use vstd::prelude::*;
          |
          |use crate::system_state::SystemState;
          |
          |verus! {
          |
          |${(sysFns, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Renders src/assertions_<property>.rs: one property's bound place assertions.
  @pure def genPropertyAssertionsRs(propertyId: String, assertionFns: ISZ[AssertionFn]): ST = {
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Place-assertion spec functions of property `$propertyId` -- one per
          |//! point its decoration binds (unbound points carry `true`, design D5).
          |
          |#![allow(non_snake_case)]
          |#![allow(unused_imports)]
          |
          |use data::*;
          |use vstd::prelude::*;
          |
          |use crate::assertions::*;
          |use crate::system_state::SystemState;
          |
          |verus! {
          |
          |${(for (a <- assertionFns) yield a.fnST, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // The three sequential VC files (VerusProofSketch.md "Proof Functions by VC Kind").
  @datatype class SequentialVCFns(val vcInitRs: ST,
                                  val vcSequentialRs: ST,
                                  val vcPostPreRs: ST)

  // Renders the assertion calls for a set of places on the given state variable; a
  // place without an assertion contributes a literal `true` (trivial VCs are
  // emitted for uniformity and auditability, design D5).
  @pure def assertCalls(placeIds: ISZ[ScheduleNextRel.PlaceId],
                        assertByPlace: Map[String, AssertionFn],
                        stName: String): ISZ[ST] = {
    var calls: ISZ[ST] = ISZ()
    for (pid <- placeIds) {
      assertByPlace.get(pid.name) match {
        case Some(a) => calls = calls :+ st"${a.fnName}($stName)"
        case _ => calls = calls :+ st"true /* ${pid.name} has no assertion */"
      }
    }
    return calls
  }

  // Renders a call of a copied contract spec fn, selecting the pre- or post-state
  // argument per parameter kind: `api_` in-ports and `In_` state vars carry
  // pre-state values; out-ports and bare state vars carry post-state values.
  @pure def contractCall(moduleName: String,
                         f: ContractFn,
                         componentPath: IdPath,
                         ssm: SystemStateMap,
                         preName: String,
                         postName: String,
                         reporter: Reporter): ST = {
    var args: ISZ[ST] = ISZ()
    for (p <- f.params) {
      if (p.isOptional) {
        reporter.error(None(), toolName,
          s"Event ports are not yet supported in system VCs: '${p.originName}' of ${f.fnName}")
      }
      val stateName: String =
        if (p.isInPort || p.kind == GumboXRustUtil.SymbolKind.StateVarPre) preName
        else postName
      ssm.byComponentEntry.get((componentPath, p.originName)) match {
        case Some(field) => args = args :+ st"$stateName.${field.fieldName}"
        case _ =>
          reporter.error(None(), toolName,
            st"Could not resolve '${p.originName}' of ${(componentPath, ".")} to a SystemState field".render)
      }
    }
    return st"$moduleName::${f.fnName}(${(args, ", ")})"
  }

  @pure def renderProofFn(doc: ST,
                          fnName: String,
                          paramsDecl: String,
                          requiresClauses: ISZ[ST],
                          ensuresClauses: ISZ[ST]): ST = {
    val ens: ISZ[ST] = if (ensuresClauses.isEmpty) ISZ(st"true /* no assertions at out-places */") else ensuresClauses
    val body: ST =
      if (requiresClauses.isEmpty)
        st"""pub proof fn $fnName($paramsDecl)
            |  ensures
            |    ${(ens, ",\n")},
            |{}"""
      else
        st"""pub proof fn $fnName($paramsDecl)
            |  requires
            |    ${(requiresClauses, ",\n")},
            |  ensures
            |    ${(ens, ",\n")},
            |{}"""
    return st"""$doc
               |$body"""
  }

  @strictpure def placeNames(placeIds: ISZ[ScheduleNextRel.PlaceId]): ST =
    st"${(for (p <- placeIds) yield p.name, ", ")}"

  // Serializes one property's sequential VCs (Init-State, Pre-Assert, Next-Assert
  // task/skip, Post-Pre) to empty-body proof fns; Verus checks
  // `requires ==> ensures` via SMT. Premises/conclusions are reconstructed from
  // the same model elements the generator used (same iteration order), keyed by
  // `VC.source`; independence VCs (NonBlocking/Preservation/Commutativity) are
  // serialized separately. Proof fns are named by the firing occurrence's id
  // (its occurrence label, or its alias when it fires once) -- unique by the
  // resolver's multi-firing label lint, so no disambiguation suffix is needed.
  @pure def genSequentialVCs(propertyId: String,
                             vcs: ISZ[VC],
                             nextRel: ScheduleNextRel.NextRelResult,
                             ssm: SystemStateMap,
                             contracts: ISZ[ComponentContracts],
                             frames: ISZ[WriteFrameFn],
                             assertionFns: ISZ[AssertionFn],
                             resolvedComponentAliasMap: Map[String, IdPath],
                             reporter: Reporter): SequentialVCFns = {
    var assertByPlace: Map[String, AssertionFn] = Map.empty
    for (a <- assertionFns) {
      assertByPlace = assertByPlace + a.placeName ~> a
    }
    var contractsByPath: Map[IdPath, ComponentContracts] = Map.empty
    for (cc <- contracts) {
      contractsByPath = contractsByPath + cc.componentPath ~> cc
    }
    var framesByPath: Map[IdPath, WriteFrameFn] = Map.empty
    for (fr <- frames) {
      framesByPath = framesByPath + fr.componentPath ~> fr
    }

    def occurrenceOf(t: ScheduleNextRel.Transition, transIdx: Z): String = {
      if (t.inPlaces.nonEmpty) {
        nextRel.activationMap.get(t.inPlaces(0)) match {
          case Some(compRef) => return ScheduleNextRel.occurrenceId(compRef)
          case _ =>
        }
      }
      return s"t$transIdx"
    }

    // Contract projection (mirrors VCGenerator.coversTransition): the property
    // uses a component's contract iff it binds one of the component's
    // out-places; otherwise (taskPre, taskPost) := (true, true) -- the
    // Pre-Assert is trivial and the Next-Assert is frame-only.
    def coversTransition(t: ScheduleNextRel.Transition): B = {
      for (p <- t.outPlaces) {
        if (assertByPlace.contains(p.name)) {
          return T
        }
      }
      return F
    }

    def componentPathOf(vc: VC): IdPath = {
      return vcComponentPath(vc, resolvedComponentAliasMap)
    }

    var initFns: ISZ[ST] = ISZ()
    var seqFns: ISZ[ST] = ISZ()
    var postPreFns: ISZ[ST] = ISZ()

    for (i <- z"0" until vcs.size) {
      val vc = vcs(i)
      vc.kind match {

        case VCKind.InitState =>
          var req: ISZ[ST] = ISZ()
          for (cc <- contracts) {
            for (f <- cc.fns) {
              if (f.kind == ContractFnKind.InitializeGuarantee || f.kind == ContractFnKind.IntegrationGuarantee) {
                req = req :+ contractCall(cc.moduleName, f, cc.componentPath, ssm, "st", "st", reporter)
              }
            }
          }
          val ens = assertCalls(ISZ(nextRel.startPlace), assertByPlace, "st")
          initFns = initFns :+ renderProofFn(
            doc = st"/** VC[$i]: Init-State -- all initialize + integration guarantees |- ${nextRel.startPlace.name} */",
            fnName = "vc_init_state",
            paramsDecl = "st: SystemState",
            requiresClauses = req,
            ensuresClauses = ens)

        case VCKind.PreAssert =>
          val transIdx = vc.source.transitionIdx.get
          val t = nextRel.transitions(transIdx)
          val compPath = componentPathOf(vc)
          val alias: String = framesByPath.get(compPath) match {
            case Some(fr) => fr.componentAlias
            case _ => "unknown"
          }
          val preCovered = coversTransition(t)
          var ens: ISZ[ST] = ISZ()
          if (preCovered) {
            contractsByPath.get(compPath) match {
              case Some(cc) =>
                for (f <- cc.fns) {
                  if (f.kind == ContractFnKind.ComputeAssume || f.kind == ContractFnKind.IntegrationAssume) {
                    ens = ens :+ contractCall(cc.moduleName, f, compPath, ssm, "st", "st", reporter)
                  }
                }
              case _ =>
            }
          }
          val preDoc: ST =
            if (preCovered) st"/** VC[$i]: Pre-Assert -- ${placeNames(t.inPlaces)} |- ${ops.StringOps(alias).toUpper} compute + integration assumes */"
            else st"/** VC[$i]: Pre-Assert -- trivial: this property does not use ${ops.StringOps(alias).toUpper}'s contract (no bound out-place; contract projection) */"
          seqFns = seqFns :+ renderProofFn(
            doc = preDoc,
            fnName = s"vc_pre_assert_${occurrenceOf(t, transIdx)}",
            paramsDecl = "st: SystemState",
            requiresClauses = assertCalls(t.inPlaces, assertByPlace, "st"),
            ensuresClauses = ens)

        case VCKind.NextAssertTask =>
          val transIdx = vc.source.transitionIdx.get
          val t = nextRel.transitions(transIdx)
          val compPath = componentPathOf(vc)
          val alias: String = framesByPath.get(compPath) match {
            case Some(fr) => fr.componentAlias
            case _ => "unknown"
          }
          var req = assertCalls(t.inPlaces, assertByPlace, "pre")
          framesByPath.get(compPath) match {
            case Some(fr) =>
              req = req :+ st"${fr.localFnName}(pre, post)"
              req = req :+ st"${fr.globalFnName}(pre, post)"
            case _ =>
              reporter.error(None(), toolName,
                st"No write frame for component ${(compPath, ".")}".render)
          }
          if (coversTransition(t)) {
            contractsByPath.get(compPath) match {
              case Some(cc) =>
                for (f <- cc.fns) {
                  if (f.kind == ContractFnKind.ComputeGuarantee || f.kind == ContractFnKind.ComputeCase ||
                    f.kind == ContractFnKind.IntegrationGuarantee) {
                    req = req :+ contractCall(cc.moduleName, f, compPath, ssm, "pre", "post", reporter)
                  }
                }
              case _ =>
            }
          }
          seqFns = seqFns :+ renderProofFn(
            doc = st"/** VC[$i]: Next-Assert (task) -- ${placeNames(t.inPlaces)} + frames + ${ops.StringOps(alias).toUpper} postcondition |- ${placeNames(t.outPlaces)} */",
            fnName = s"vc_next_assert_task_${occurrenceOf(t, transIdx)}",
            paramsDecl = "pre: SystemState, post: SystemState",
            requiresClauses = req,
            ensuresClauses = assertCalls(t.outPlaces, assertByPlace, "post"))

        case VCKind.NextAssertSkip =>
          val transIdx = vc.source.transitionIdx.get
          val t = nextRel.transitions(transIdx)
          seqFns = seqFns :+ renderProofFn(
            doc = st"/** VC[$i]: Next-Assert (control point) -- ${placeNames(t.inPlaces)} |- ${placeNames(t.outPlaces)} (state unchanged) */",
            fnName = s"vc_next_assert_skip_t$transIdx",
            paramsDecl = "st: SystemState",
            requiresClauses = assertCalls(t.inPlaces, assertByPlace, "st"),
            ensuresClauses = assertCalls(t.outPlaces, assertByPlace, "st"))

        case VCKind.PostPre =>
          postPreFns = postPreFns :+ renderProofFn(
            doc = st"/** VC[$i]: Post-Pre -- ${nextRel.endPlace.name} |- ${nextRel.startPlace.name} (hyperperiod loop invariant) */",
            fnName = "vc_post_pre",
            paramsDecl = "st: SystemState",
            requiresClauses = assertCalls(ISZ(nextRel.endPlace), assertByPlace, "st"),
            ensuresClauses = assertCalls(ISZ(nextRel.startPlace), assertByPlace, "st"))

        case _ => // NonBlocking / Preservation / Commutativity are serialized separately
      }
    }

    return SequentialVCFns(
      vcInitRs = vcFile(s"Property $propertyId -- Init-State VC: the initial state (all initialize + integration guarantees) satisfies the START assertion.", propertyId, initFns),
      vcSequentialRs = vcFile(s"Property $propertyId -- Pre-Assert and Next-Assert VCs for every schema transition.", propertyId, seqFns),
      vcPostPreRs = vcFile(s"Property $propertyId -- Post-Pre VC: the END assertion implies the START assertion across hyperperiod boundaries.", propertyId, postPreFns))
  }

  @pure def vcFile(desc: String, propertyId: String, fns: ISZ[ST]): ST = {
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! $desc
          |//! Each proof fn has an empty body: Verus discharges `requires ==> ensures`
          |//! via SMT; add proof hints in the body if a VC does not discharge.
          |
          |#![allow(non_snake_case)]
          |#![allow(unused_variables)]
          |#![allow(unused_imports)]
          |
          |use vstd::prelude::*;
          |
          |use crate::assertions::*;
          |use crate::assertions_$propertyId::*;
          |use crate::contracts::*;
          |use crate::system_state::SystemState;
          |use crate::write_frames::*;
          |
          |verus! {
          |
          |${(fns, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Resolves a VC's firing component to its thread path via the schedule alias map.
  @pure def vcComponentPath(vc: VC, resolvedComponentAliasMap: Map[String, IdPath]): IdPath = {
    vc.source.componentOpt match {
      case Some(name) =>
        if (name.name.isEmpty) {
          return name.name
        }
        return resolvedComponentAliasMap.getOrElse(name.name(name.name.lastIndex), name.name)
      case _ => return ISZ()
    }
  }

  // One component's uninterpreted action abstraction (VerusProofSketch.md
  // "Commutativity VC"): one uninterpreted spec fn per written field, taking the
  // component's read scope (its state variables, in ports, and own out ports'
  // pass-through values, all pre-state), plus a `<alias>_fire(pre, post)` predicate
  // stating that every written field is determined by the read scope and everything
  // else is framed. Used ONLY by Commutativity VCs: `execIndependent` is a state
  // equality, which relational contracts and frames can never establish -- but for
  // any interpretation of the action fns, two interleavings agree by congruence
  // when the pair's write sets are disjoint from each other's write sets and read
  // scopes (Bernstein's conditions). Soundness rests on the real actions being one
  // such interpretation (wf_System_ChState_In/Out plus the task-state analogue --
  // see FormalizationIssues.md SS1 in the hamr-system-reasoning-prototype repo).
  @datatype class ActionFns(val componentPath: IdPath,
                            val componentAlias: String,
                            val fireFnName: String,
                            val fnST: ST)

  @pure def buildActions(ssm: SystemStateMap,
                         frames: ISZ[WriteFrameFn],
                         symbolTable: SymbolTable,
                         crustTypeProvider: CRustTypeProvider,
                         reporter: Reporter): ISZ[ActionFns] = {
    var framesByPath: Map[IdPath, WriteFrameFn] = Map.empty
    for (fr <- frames) {
      framesByPath = framesByPath + fr.componentPath ~> fr
    }

    var result: ISZ[ActionFns] = ISZ()
    for (t <- symbolTable.getThreads()) {
      val frameOpt = framesByPath.get(t.path)
      if (frameOpt.nonEmpty) {
        val frame = frameOpt.get
        val alias = frame.componentAlias
        val ws = WriteFrameBuilder.computeWriteSet(t, symbolTable)

        // read scope, deterministic order: state vars, in ports, own out ports
        var readScope: ISZ[(String, StateField)] = ISZ()
        for (sv <- ws.stateVars) {
          ssm.byComponentEntry.get((t.path, sv.name)) match {
            case Some(f) => readScope = readScope :+ ((sv.name, f))
            case _ =>
          }
        }
        for (p <- t.getPorts()) {
          if (p.direction == ir.Direction.In) {
            ssm.byComponentEntry.get((t.path, p.identifier)) match {
              case Some(f) => readScope = readScope :+ ((p.identifier, f))
              case _ =>
            }
          }
        }
        for (p <- ws.outPorts) {
          ssm.byComponentEntry.get((t.path, p.identifier)) match {
            case Some(f) => readScope = readScope :+ ((p.identifier, f))
            case _ =>
          }
        }

        var writes: ISZ[(String, StateField)] = ISZ()
        for (p <- ws.outPorts) {
          ssm.byComponentEntry.get((t.path, p.identifier)) match {
            case Some(f) => writes = writes :+ ((p.identifier, f))
            case _ =>
          }
        }
        for (sv <- ws.stateVars) {
          ssm.byComponentEntry.get((t.path, sv.name)) match {
            case Some(f) => writes = writes :+ ((sv.name, f))
            case _ =>
          }
        }

        var uninterpFns: ISZ[ST] = ISZ()
        var fireClauses: ISZ[ST] = ISZ()
        for (w <- writes) {
          val actionFnName = s"${alias}_action_${w._1}"
          val paramDecls: ISZ[ST] =
            for (r <- readScope) yield st"${r._1}: ${crustTypeProvider.getTypeNameProvider(r._2.aadlType).qualifiedRustName}"
          uninterpFns = uninterpFns :+
            st"pub uninterp spec fn $actionFnName(${(paramDecls, ", ")}) -> ${crustTypeProvider.getTypeNameProvider(w._2.aadlType).qualifiedRustName};"
          fireClauses = fireClauses :+
            st"post.${w._2.fieldName} == $actionFnName(${(for (r <- readScope) yield st"pre.${r._2.fieldName}", ", ")})"
        }
        fireClauses = fireClauses :+ st"${frame.globalFnName}(pre, post)"

        val fireFnName = s"${alias}_fire"
        val fnST: ST =
          st"""// -- ${(t.path, ".")} --
              |
              |${(uninterpFns, "\n")}
              |
              |/** "${ops.StringOps(alias).toUpper} fires": every written field is determined by the read scope;
              |  * everything else is framed.
              |  */
              |pub open spec fn $fireFnName(pre: SystemState, post: SystemState) -> bool
              |{
              |  ${(fireClauses, "\n&& ")}
              |}"""

        result = result :+ ActionFns(
          componentPath = t.path,
          componentAlias = alias,
          fireFnName = fireFnName,
          fnST = fnST)
      }
    }
    return result
  }

  // Renders crates/sys_proof/src/actions.rs.
  @pure def genActionsRs(actions: ISZ[ActionFns]): ST = {
    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Uninterpreted component action abstractions, used ONLY by the
          |//! Commutativity VCs. Verus proves those VCs for ALL interpretations of
          |//! the action fns; the real component behaviors are one interpretation
          |//! (components only write their declared out ports/state, and outputs
          |//! depend only on the read scope), so a discharged Commutativity VC
          |//! establishes the Isabelle `execIndependent` obligation.
          |
          |#![allow(non_snake_case)]
          |#![allow(unused_variables)]
          |
          |use data::*;
          |use vstd::prelude::*;
          |
          |use crate::system_state::SystemState;
          |use crate::write_frames::*;
          |
          |verus! {
          |
          |${(for (a <- actions) yield a.fnST, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Serializes one property's Non-Blocking and Preservation VCs. Both
  // over-approximate the firing component's actual action by its global write
  // frame (sound: the real action satisfies the frame). Commutativity is
  // property-independent and serialized once per composition by
  // `genCommutativityVCs`.
  @pure def genPropertyIndependenceVCs(propertyId: String,
                                       vcs: ISZ[VC],
                                       nextRel: ScheduleNextRel.NextRelResult,
                                       frames: ISZ[WriteFrameFn],
                                       assertionFns: ISZ[AssertionFn],
                                       resolvedComponentAliasMap: Map[String, IdPath],
                                       reporter: Reporter): ST = {
    var assertByPlace: Map[String, AssertionFn] = Map.empty
    for (a <- assertionFns) {
      assertByPlace = assertByPlace + a.placeName ~> a
    }
    var framesByPath: Map[IdPath, WriteFrameFn] = Map.empty
    for (fr <- frames) {
      framesByPath = framesByPath + fr.componentPath ~> fr
    }

    def transitionPathOpt(t: ScheduleNextRel.Transition): Option[IdPath] = {
      if (t.inPlaces.isEmpty) {
        return None()
      }
      nextRel.activationMap.get(t.inPlaces(0)) match {
        case Some(compRef) =>
          if (compRef.component.name.isEmpty) {
            return Some(compRef.component.name)
          }
          return Some(resolvedComponentAliasMap.getOrElse(
            compRef.component.name(compRef.component.name.lastIndex), compRef.component.name))
        case _ => return None()
      }
    }

    def aliasOf(path: IdPath): String = {
      framesByPath.get(path) match {
        case Some(fr) => return fr.componentAlias
        case _ => return "unknown"
      }
    }

    def transLabel(t: ScheduleNextRel.Transition, idx: Z): String = {
      transitionPathOpt(t) match {
        case Some(p) => return aliasOf(p)
        case _ => return s"t$idx"
      }
    }

    var usedNames: Set[String] = Set.empty
    def uniqueName(base: String, vcIdx: Z): String = {
      var cand = base
      if (usedNames.contains(cand)) {
        cand = s"${base}_vc$vcIdx"
      }
      usedNames = usedNames + cand
      return cand
    }

    // identifies the firing member of a directed NonBlocking/Preservation VC; when
    // both members belong to the same component, the generator's emission order
    // (t1-fires first) breaks the tie
    var dirCount: Map[(Z, Z, VCKind.Type), Z] = Map.empty
    def firingIsFirst(vc: VC, pair: (Z, Z), p1Opt: Option[IdPath], p2Opt: Option[IdPath]): B = {
      val firingPath = vcComponentPath(vc, resolvedComponentAliasMap)
      if (p1Opt == Some(firingPath) && p2Opt == Some(firingPath)) {
        val k = (pair._1, pair._2, vc.kind)
        val n = dirCount.getOrElse(k, z"0")
        dirCount = dirCount + k ~> (n + 1)
        return n == z"0"
      }
      return p1Opt == Some(firingPath)
    }

    var fns: ISZ[ST] = ISZ()
    for (i <- z"0" until vcs.size) {
      val vc = vcs(i)
      vc.kind match {

        case VCKind.NonBlocking =>
          val pair = vc.source.mhipPairOpt.get
          val t1 = nextRel.transitions(pair._1)
          val t2 = nextRel.transitions(pair._2)
          val p1Opt = transitionPathOpt(t1)
          val p2Opt = transitionPathOpt(t2)
          val isFirst = firingIsFirst(vc, pair, p1Opt, p2Opt)
          val firingPath: IdPath = if (isFirst) p1Opt.get else p2Opt.get
          val other: ScheduleNextRel.Transition = if (isFirst) t2 else t1
          val otherIdx: Z = if (isFirst) pair._2 else pair._1
          val firingAlias = aliasOf(firingPath)
          val otherLabel = transLabel(other, otherIdx)
          var req = assertCalls(t1.inPlaces, assertByPlace, "pre") ++ assertCalls(t2.inPlaces, assertByPlace, "pre")
          req = req :+ st"${framesByPath.get(firingPath).get.globalFnName}(pre, post)"
          fns = fns :+ renderProofFn(
            doc = st"/** VC[$i]: Non-Blocking -- ${ops.StringOps(firingAlias).toUpper} firing does not block ${ops.StringOps(otherLabel).toUpper} (MHIP pair t${pair._1}/t${pair._2}) */",
            fnName = uniqueName(s"vc_non_blocking_${firingAlias}_$otherLabel", i),
            paramsDecl = "pre: SystemState, post: SystemState",
            requiresClauses = req,
            ensuresClauses = assertCalls(other.inPlaces, assertByPlace, "post"))

        case VCKind.Preservation =>
          val pair = vc.source.mhipPairOpt.get
          val t1 = nextRel.transitions(pair._1)
          val t2 = nextRel.transitions(pair._2)
          val p1Opt = transitionPathOpt(t1)
          val p2Opt = transitionPathOpt(t2)
          val isFirst = firingIsFirst(vc, pair, p1Opt, p2Opt)
          val firingPath: IdPath = if (isFirst) p1Opt.get else p2Opt.get
          val firing: ScheduleNextRel.Transition = if (isFirst) t1 else t2
          val other: ScheduleNextRel.Transition = if (isFirst) t2 else t1
          val otherIdx: Z = if (isFirst) pair._2 else pair._1
          val firingAlias = aliasOf(firingPath)
          val otherLabel = transLabel(other, otherIdx)
          var req = assertCalls(firing.inPlaces, assertByPlace, "pre") ++ assertCalls(other.outPlaces, assertByPlace, "pre")
          req = req :+ st"${framesByPath.get(firingPath).get.globalFnName}(pre, post)"
          fns = fns :+ renderProofFn(
            doc = st"/** VC[$i]: Preservation -- ${ops.StringOps(firingAlias).toUpper} firing preserves ${ops.StringOps(otherLabel).toUpper}'s post-assertions (MHIP pair t${pair._1}/t${pair._2}) */",
            fnName = uniqueName(s"vc_preservation_${firingAlias}_$otherLabel", i),
            paramsDecl = "pre: SystemState, post: SystemState",
            requiresClauses = req,
            ensuresClauses = assertCalls(other.outPlaces, assertByPlace, "post"))

        case _ => // sequential VCs are serialized by genSequentialVCs; Commutativity by genCommutativityVCs
      }
    }

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Property $propertyId -- Non-Blocking and Preservation VCs for every
          |//! MHIP transition pair (per direction whose firing member is a component).
          |//! Each proof fn has an empty body: Verus discharges `requires ==> ensures`
          |//! via SMT; add proof hints in the body if a VC does not discharge.
          |
          |#![allow(non_snake_case)]
          |#![allow(unused_variables)]
          |#![allow(unused_imports)]
          |
          |use vstd::prelude::*;
          |
          |use crate::assertions::*;
          |use crate::assertions_$propertyId::*;
          |use crate::system_state::SystemState;
          |use crate::write_frames::*;
          |
          |verus! {
          |
          |${(fns, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Serializes the Commutativity VCs (execIndependent), once per composition --
  // they mention no assertions, so they are shared by all properties.
  // Commutativity quantifies both interleavings via the `_fire` predicates and
  // concludes state equality.
  @pure def genCommutativityVCs(vcs: ISZ[VC],
                                nextRel: ScheduleNextRel.NextRelResult,
                                actions: ISZ[ActionFns],
                                resolvedComponentAliasMap: Map[String, IdPath],
                                reporter: Reporter): ST = {
    var actionsByPath: Map[IdPath, ActionFns] = Map.empty
    for (a <- actions) {
      actionsByPath = actionsByPath + a.componentPath ~> a
    }

    def transitionPathOpt(t: ScheduleNextRel.Transition): Option[IdPath] = {
      if (t.inPlaces.isEmpty) {
        return None()
      }
      nextRel.activationMap.get(t.inPlaces(0)) match {
        case Some(compRef) =>
          if (compRef.component.name.isEmpty) {
            return Some(compRef.component.name)
          }
          return Some(resolvedComponentAliasMap.getOrElse(
            compRef.component.name(compRef.component.name.lastIndex), compRef.component.name))
        case _ => return None()
      }
    }

    var usedNames: Set[String] = Set.empty
    def uniqueName(base: String, vcIdx: Z): String = {
      var cand = base
      if (usedNames.contains(cand)) {
        cand = s"${base}_vc$vcIdx"
      }
      usedNames = usedNames + cand
      return cand
    }

    var fns: ISZ[ST] = ISZ()
    for (i <- z"0" until vcs.size) {
      val vc = vcs(i)
      vc.kind match {
        case VCKind.Commutativity =>
          val pair = vc.source.mhipPairOpt.get
          val t1 = nextRel.transitions(pair._1)
          val t2 = nextRel.transitions(pair._2)
          (transitionPathOpt(t1), transitionPathOpt(t2)) match {
            case (Some(path1), Some(path2)) =>
              (actionsByPath.get(path1), actionsByPath.get(path2)) match {
                case (Some(a1), Some(a2)) =>
                  var req: ISZ[ST] = ISZ()
                  req = req :+ st"${a1.fireFnName}(st, mid_a)"
                  req = req :+ st"${a2.fireFnName}(mid_a, post_a)"
                  req = req :+ st"${a2.fireFnName}(st, mid_b)"
                  req = req :+ st"${a1.fireFnName}(mid_b, post_b)"
                  fns = fns :+ renderProofFn(
                    doc =
                      st"""/** VC[$i]: Commutativity (execIndependent) -- firing ${ops.StringOps(a1.componentAlias).toUpper} then ${ops.StringOps(a2.componentAlias).toUpper}
                          |  * yields the same state as ${ops.StringOps(a2.componentAlias).toUpper} then ${ops.StringOps(a1.componentAlias).toUpper} (MHIP pair t${pair._1}/t${pair._2}).
                          |  * Discharges by congruence when the write sets are disjoint from each
                          |  * other's write sets and read scopes. */""",
                    fnName = uniqueName(s"vc_commutativity_${a1.componentAlias}_${a2.componentAlias}", i),
                    paramsDecl = "st: SystemState, mid_a: SystemState, post_a: SystemState, mid_b: SystemState, post_b: SystemState",
                    requiresClauses = req,
                    ensuresClauses = ISZ(st"post_a == post_b"))
                case _ =>
                  reporter.error(None(), toolName,
                    s"Missing action abstraction for Commutativity VC[$i]")
              }
            case _ =>
              reporter.error(None(), toolName,
                s"Commutativity VC[$i] members must both be component transitions")
          }
        case _ =>
      }
    }

    return (
      st"""${CommentTemplate.doNotEditComment_slash}
          |
          |//! Commutativity VCs (execIndependent), one per component-component MHIP
          |//! pair. Assertion-free state equalities over the action abstractions --
          |//! shared by all of the composition's properties.
          |//! Each proof fn has an empty body: Verus discharges `requires ==> ensures`
          |//! via SMT; add proof hints in the body if a VC does not discharge.
          |
          |#![allow(non_snake_case)]
          |#![allow(unused_variables)]
          |
          |use vstd::prelude::*;
          |
          |use crate::actions::*;
          |use crate::system_state::SystemState;
          |
          |verus! {
          |
          |${(fns, "\n\n")}
          |
          |} // verus!
          |""")
  }

  // Renders crates/sys_proof_<composition>/src/lib.rs. The proof crate is
  // verification-only -- every fn is a spec or proof fn -- but it follows the
  // workspace's no_std conventions so it builds alongside the other crates.
  // Shared modules first, then per-property modules in declaration order.
  @pure def genLibRs(compositionId: String, propertyModIds: ISZ[String]): ST = {
    var propertyMods: ISZ[ST] = ISZ()
    for (p <- propertyModIds) {
      propertyMods = propertyMods :+
        st"""pub mod assertions_$p;
            |pub mod vc_${p}_independence;
            |pub mod vc_${p}_init;
            |pub mod vc_${p}_post_pre;
            |pub mod vc_${p}_sequential;"""
    }
    return (
      st"""#![cfg_attr(not(test), no_std)]
          |
          |${RustUtil.defaultCrateLevelAttributes}
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |//! System-level verification conditions of composition `$compositionId`,
          |//! discharged by Verus -- shared modules (state, contracts, frames,
          |//! actions, commutativity) plus one module group per property. See the
          |//! proof-fn doc comments for the VC indices tying each obligation back
          |//! to the generator output.
          |
          |pub mod actions;
          |pub mod assertions;
          |pub mod contracts;
          |pub mod system_state;
          |pub mod vc_commutativity;
          |pub mod write_frames;
          |
          |${(propertyMods, "\n")}
          |""")
  }

  // Renders crates/sys_proof_<composition>/Cargo.toml. The proof crate depends on
  // `data` (shared AADL types) and the GUMBO library annex crates (ordinary rlibs
  // holding the Verus library spec fns the contracts/assertions call) -- but NOT
  // on any component staticlib crate, which is what enforces the modular
  // abstraction boundary (the system proof sees contract copies, never
  // implementations).
  @pure def genSysProofCargoToml(crateName: String, libraryCrateNames: ISZ[String], store: Store): ST = {
    val libDeps: ISZ[ST] = for (k <- libraryCrateNames) yield st"""$k = { path = "../$k" }"""
    return (
      st"""${CommentTemplate.doNotEditComment_hash}
          |
          |[package]
          |name = "$crateName"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[dependencies]
          |data = { path = "../data" }
          |${(libDeps, "\n")}
          |
          |${RustUtil.verusCargoDependencies(store)}
          |
          |${RustUtil.commonCargoTomlEntries}
          |""")
  }
}
