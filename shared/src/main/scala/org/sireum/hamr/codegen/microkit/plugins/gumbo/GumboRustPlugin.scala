// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.microkit.plugins.testing.TestSchedulerPlugin
import org.sireum.hamr.codegen.common.CommonUtil._
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import SlangExpUtil.{Context, TargetLanguage}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.microkit.plugins.linters.MicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.CRustApiPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.testing.CRustTestingPlugin
import org.sireum.hamr.codegen.microkit.plugins.rust.types.{CRustTypePlugin, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitInitPlugin, MicrokitPlugin, StoreUtil}
import org.sireum.hamr.codegen.microkit.util.{MakefileTarget, MakefileUtil, MicrokitUtil, RustUtil}
import org.sireum.hamr.codegen.microkit.{MicrokitCodegen, rust => RAST}
import org.sireum.hamr.ir._
import org.sireum.lang.{ast => SAST}
import org.sireum.message.{Level, Message, Reporter}

object GumboRustPlugin {
  val KEY_GumboRustPlugin: String = "KEY_GumboRustPlugin"
  val KEY_twc: String = s"${KEY_GumboRustPlugin}_threads_with_contracts"
  val KEY_dwc: String = s"${KEY_GumboRustPlugin}_datatypes_with_contracts"
  val KEY_ctwc: String = s"${KEY_GumboRustPlugin}_c_threads_with_contracts"

  @strictpure def getGumboRustContributions(store: Store): Option[GumboRustContributions] = store.get(KEY_GumboRustPlugin).asInstanceOf[Option[GumboRustContributions]]
  @strictpure def getThreadsWithContracts(store: Store): ISZ[ThreadIdPath] = store.getOrElse(KEY_twc, ISZValue[ThreadIdPath](ISZ())).asInstanceOf[ISZValue[ThreadIdPath]].elements
  @strictpure def getDatatypesWithContracts(store: Store): ISZ[DataIdPath] = store.getOrElse(KEY_dwc, ISZValue[DataIdPath](ISZ())).asInstanceOf[ISZValue[DataIdPath]].elements
  /** Threads implemented in C that have a GUMBO subclause.  They get no Rust crate, so no
    * Verus contracts or component test harness, but their contracts are still checked at
    * run time -- by the monitors and the test controller, through crates/observers --
    * from the executable GUMBOX predicates GumboXRustPlugin generates for them. */
  @strictpure def getCThreadsWithContracts(store: Store): ISZ[ThreadIdPath] = store.getOrElse(KEY_ctwc, ISZValue[ThreadIdPath](ISZ())).asInstanceOf[ISZValue[ThreadIdPath]].elements

  @strictpure def putGumboRustContributions(contributions: GumboRustContributions, store: Store): Store = store + KEY_GumboRustPlugin ~> contributions
  @strictpure def putThreadsWithContracts(i: ISZ[ThreadIdPath], store: Store): Store = store + KEY_twc ~> ISZValue(i)
  @strictpure def putDatatypesWithContracts(i: ISZ[DataIdPath], store: Store): Store = store + KEY_dwc ~> ISZValue(i)

  @strictpure def getGclLibraryAnnexes(symbolTable: SymbolTable): ISZ[GclAnnexLibInfo] =
    symbolTable.annexLibInfos.filter(p => p.isInstanceOf[GclAnnexLibInfo]).asInstanceOf[ISZ[GclAnnexLibInfo]]


  @datatype class LibraryAnnex(val rustItems: ISZ[RAST.Item],

                               val verusItems: ISZ[RAST.Item],
                               val verusDeveloperItems: ISZ[RAST.Item],

                               // the other GUMBO library crates this library's functions call
                               // (libraryDependencies), each a path dependency in its Cargo.toml
                               val crateDeps: ISZ[String])

  /** Collects the other GUMBO libraries that the code it scans references: every resolved
    * method of an object `<package>::GUMBO__Library` other than `selfOwner`, by package. A
    * call is rendered as `<package>::<fn>(..)` in the calling library's crate
    * (`SlangExpUtil`), so each such package is a crate it depends on. */
  @record class LibraryDepScan(val selfOwner: ISZ[String]) extends org.sireum.hamr.ir.MTransformer {
    var deps: Set[String] = Set.empty

    override def pre_langastResolvedInfoMethod(o: SAST.ResolvedInfo.Method): org.sireum.hamr.ir.MTransformer.PreResult[SAST.ResolvedInfo] = {
      o.owner match {
        case ISZ(pkg, obj) if obj == GclResolver.GUMBO__Library && o.owner != selfOwner =>
          deps = deps + pkg
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.ResolvedInfo]())
    }
  }

  /** The packages of the other GUMBO libraries whose functions `lib`'s functions (bodies and
    * contracts) reference: the crates `lib`'s crate depends on. */
  @pure def libraryDependencies(lib: GclLib): Set[String] = {
    val scan = LibraryDepScan(lib.containingPackage.name :+ GclResolver.GUMBO__Library)
    for (m <- lib.methods) {
      m match {
        case s: GclSpecMethod => scan.transform_langastStmt(s.method)
        case b: GclBodyMethod => scan.transform_langastStmt(b.method)
        case _ =>
      }
    }
    return scan.deps
  }

  /** A cycle in `deps` (library -> libraries it depends on), as the libraries along it with
    * the first repeated at the end, or None. Cargo rejects cyclic crate dependencies. */
  @pure def libraryDependencyCycle(libs: ISZ[String], deps: Map[String, Set[String]]): Option[ISZ[String]] = {
    var done: Set[String] = Set.empty
    var found: Option[ISZ[String]] = None()
    def visit(lib: String, path: ISZ[String]): Unit = {
      if (found.nonEmpty || done.contains(lib)) {
        return
      }
      val i = ops.ISZOps(path).indexOf(lib)
      if (i < path.size) {
        found = Some(ops.ISZOps(path).slice(i, path.size) :+ lib)
        return
      }
      for (d <- deps.get(lib).getOrElse(Set.empty[String]).elements) {
        visit(d, path :+ lib)
      }
      done = done + lib
    }
    for (l <- libs) {
      visit(l, ISZ())
    }
    return found
  }

  /** Lets verified code call a GUMBO library function's executable version.
    *
    * A library crate's executable functions (`rustItems`, from GUMBOX) are plain Rust
    * outside `verus!`, so Verus rejects a call to one from verified component code ("is
    * not supported"). For each executable function whose spec twin `<name>_spec` is among
    * `verusItems`, this emits, for the crate's `verus!` block,
    *
    * {{{
    * pub assume_specification[ name ](a: A, ..) -> (res: R)
    *   ensures res == name_spec(a, ..);
    * }}}
    *
    * (the result is `res`, with underscores appended while a parameter has that name), so
    * Verus reasons about a call through the spec. Only the functions in `withSpecs`
    * (`libraryFnsWithExecSpecs`) get one; the others are listed in a comment.
    *
    * For a GUMBO function with a body this is trusted, not proved: the executable body is
    * GUMBOX's rendering of the GUMBO expression (non-short-circuit `&`/`|`, quantifiers as
    * loops), which Verus could not verify against the spec in general. The trust is that
    * the two renderings of the same expression agree, which GUMBOX's run-time checks already
    * rely on. Arithmetic is where they could not be relied on to agree (machine integers
    * against mathematical ones, e.g. on overflow), so a function using it gets no
    * specification. For a GUMBO spec
    * function the executable wrapper only delegates to the developer's
    * `<name>__developer_gumbox`, which is itself verified to equal
    * `<name>__developer_verus`, the spec twin's definition. The executable functions stay
    * outside `verus!`, where the crate's existing marker layout puts them, so a project
    * regenerated in place keeps a valid layout.
    */
  @pure def libraryExecSpecs(rustItems: ISZ[RAST.Item], verusItems: ISZ[RAST.Item], withSpecs: Set[String]): ISZ[RAST.Item] = {
    var specNames: Set[String] = Set.empty
    for (i <- verusItems) {
      i match {
        case f: RAST.Fn => specNames = specNames + f.ident.string
        case _ =>
      }
    }
    var ret: ISZ[RAST.Item] = ISZ()
    var without: ISZ[String] = ISZ()
    for (i <- rustItems) {
      i match {
        case f: RAST.FnImpl if specNames.contains(s"${f.ident.string}_spec") && !withSpecs.contains(f.ident.string) =>
          without = without :+ f.ident.string
        case f: RAST.FnImpl if specNames.contains(s"${f.ident.string}_spec") =>
          f.sig.fnDecl.outputs match {
            case RAST.FnRetTyImpl(retTy) =>
              val paramNames: ISZ[String] = for (p <- f.sig.fnDecl.inputs) yield p.ident.string
              // the result's name must differ from every parameter's (Verus rejects a
              // collision), so append underscores to `res` until it does
              var resName: String = "res"
              while (ops.ISZOps(paramNames).contains(resName)) {
                resName = s"${resName}_"
              }
              ret = ret :+ RAST.ItemST(
                st"""pub assume_specification[ ${f.ident.string} ](${(for (p <- f.sig.fnDecl.inputs) yield p.prettyST, ", ")}) -> ($resName: ${retTy.prettyST})
                    |  ensures $resName == ${f.ident.string}_spec(${(paramNames, ", ")});""")
            case _ =>
          }
        case _ =>
      }
    }
    if (without.nonEmpty) {
      ret = ret :+ RAST.ItemST(
        st"""// No assume_specification, so not callable from verified code: ${(without, ", ")}.
            |// Each uses arithmetic or calls something outside this library (such as a conversion
            |// or another library's function), directly or through a function of this library it
            |// calls, where the executable version (machine integers) and the spec version
            |// (mathematical integers) can disagree.""")
    }
    return ret
  }

  val libraryArithmeticOps: ISZ[String] = ISZ("+", "-", "*", "/", "%", "<<", ">>", ">>>")

  /** What in a GUMBO library function's body could make its executable and spec versions
    * disagree: arithmetic (`libraryArithmeticOps`, or negating anything but a literal),
    * which GUMBOX computes in machine integers and Verus in mathematical ones, and calls to
    * anything other than the library's own functions, such as conversions or another
    * library's functions. A callee is identified by its resolved method, owner and name:
    * a function of this library resolves to a method of `libOwner`
    * (`<package>::GUMBO__Library`) named in `libraryFns`, so a same-named function of
    * another library does not pass for it. Calls are found wherever a method is referenced:
    * a positional or named invocation, or a parameterless reference (`Ident` / `Select`).
    * An invocation whose callee did not resolve to a method counts as another call. The
    * library functions it calls are collected in `calls`. */
  @record class LibraryBodyScan(val libOwner: ISZ[String], val libraryFns: Set[String]) extends org.sireum.hamr.ir.MTransformer {
    var hasArithmetic: B = F
    var hasOtherCall: B = F
    var calls: Set[String] = Set.empty

    override def pre_langastExpBinary(o: SAST.Exp.Binary): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      if (ops.ISZOps(libraryArithmeticOps).contains(o.op)) {
        hasArithmetic = T
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    override def pre_langastExpUnary(o: SAST.Exp.Unary): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      if (o.op == SAST.Exp.UnaryOp.Minus) {
        o.exp match {
          case _: SAST.Exp.LitZ => // a negative constant
          case _: SAST.Exp.StringInterpolate => // a negative typed constant, e.g. s32"5"
          case _ => hasArithmetic = T
        }
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    /** Records a call of the method `res` resolved to: one of this library's functions, or
      * anything else (including an unresolved callee, `None`). */
    def noteCallee(res: Option[SAST.ResolvedInfo]): Unit = {
      res match {
        case Some(m: SAST.ResolvedInfo.Method) if m.owner == libOwner && libraryFns.contains(m.id) =>
          calls = calls + m.id
        case _ =>
          hasOtherCall = T
      }
    }

    /** The resolved callee of an invocation: the invocation's own, else its name's. */
    @strictpure def calleeOf(attr: SAST.ResolvedAttr, ident: SAST.Exp.Ident): Option[SAST.ResolvedInfo] =
      if (attr.resOpt.nonEmpty) attr.resOpt else ident.attr.resOpt

    override def pre_langastExpInvoke(o: SAST.Exp.Invoke): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      noteCallee(calleeOf(o.attr, o.ident))
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    override def pre_langastExpInvokeNamed(o: SAST.Exp.InvokeNamed): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      noteCallee(calleeOf(o.attr, o.ident))
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    // a method referenced without an argument list, and the name of an invocation (which
    // resolves to the same method as the invocation, so it adds nothing new there)
    override def pre_langastExpIdent(o: SAST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.attr.resOpt match {
        case Some(_: SAST.ResolvedInfo.Method) => noteCallee(o.attr.resOpt)
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    override def pre_langastExpSelect(o: SAST.Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.attr.resOpt match {
        case Some(_: SAST.ResolvedInfo.Method) => noteCallee(o.attr.resOpt)
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }
  }

  /** The functions of GUMBO library `lib` whose executable versions get an
    * `assume_specification` (`libraryExecSpecs`). A spec function always does: its
    * executable wrapper delegates to the developer's verified `__developer_gumbox`. A
    * function with a body does when `LibraryBodyScan` finds neither arithmetic nor a call
    * of anything outside `lib` in it (callees are matched by resolved owner and name, so a
    * same-named function of another library does not count as `lib`'s), and every function
    * of `lib` it calls also qualifies, so that its executable and spec versions compute the
    * same value. */
  @pure def libraryFnsWithExecSpecs(lib: GclLib): Set[String] = {
    var libraryFns: Set[String] = Set.empty
    for (m <- lib.methods) {
      libraryFns = libraryFns + m.id
    }
    // the object a call of one of this library's own functions resolves to
    val libOwner: ISZ[String] = lib.containingPackage.name :+ GclResolver.GUMBO__Library
    var ok: Set[String] = Set.empty
    var candidates: ISZ[(String, ISZ[String])] = ISZ() // body functions that pass the scan, with their library calls
    for (m <- lib.methods) {
      m match {
        case s: GclSpecMethod => ok = ok + s.id
        case b: GclBodyMethod =>
          val scan = LibraryBodyScan(libOwner, libraryFns)
          b.method.bodyOpt match {
            case Some(body) =>
              for (stmt <- body.stmts) {
                scan.transform_langastStmt(stmt)
              }
            case _ =>
          }
          if (!scan.hasArithmetic && !scan.hasOtherCall) {
            candidates = candidates :+ ((b.id, scan.calls.elements))
          }
        case _ =>
      }
    }
    // a candidate qualifies once every library function it calls has (a fixpoint, since
    // library functions may call each other in any order)
    var changed: B = T
    while (changed) {
      changed = F
      for (c <- candidates if !ok.contains(c._1)) {
        var allOk: B = T
        for (callee <- c._2 if !ok.contains(callee)) {
          allOk = F
        }
        if (allOk) {
          ok = ok + c._1
          changed = T
        }
      }
    }
    return ok
  }
}

@sig trait GumboRustContributions extends StoreValue {
  @pure def datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]]

  @pure def getTheLibraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex]

  @pure def setTheLibraryAnnexes(m: Map[String, GumboRustPlugin.LibraryAnnex]): GumboRustContributions
}

@datatype class DefaultGumboRustContributions(val datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]],
                                              val libraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex]) extends GumboRustContributions {

  @strictpure override def getTheLibraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex] = libraryAnnexes

  @strictpure override def setTheLibraryAnnexes(m: Map[String, GumboRustPlugin.LibraryAnnex]): GumboRustContributions =
    DefaultGumboRustContributions(datatypeInvariants, m)
}

@sig trait GumboRustPlugin extends MicrokitInitPlugin with MicrokitPlugin with MicrokitFinalizePlugin {

  @pure override def init(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): Store = {
    if (isDisabled(store)) {
      return store
    }
    var localStore = store
    if (options.platform != HamrCli.CodegenHamrPlatform.Microkit ||
      !ops.ISZOps(symbolTable.getThreads()).exists(p => MicrokitUtil.isRusty(p))) {
      return localStore
    }
    var threadsWithContracts: ISZ[ThreadIdPath] = ISZ()
    var cThreadsWithContracts: ISZ[ThreadIdPath] = ISZ()
    var datatypesWithContracts: ISZ[DataIdPath] = ISZ()
    for (t <- symbolTable.getThreads()) {
      if(ops.ISZOps(t.annexes()).exists(p => p.clause.isInstanceOf[GclSubclause])) {
        if (MicrokitUtil.isRusty(t)) {
          threadsWithContracts = threadsWithContracts :+ t.path
        } else if (!t.toVirtualMachine(symbolTable)) {
          cThreadsWithContracts = cThreadsWithContracts :+ t.path
        }
      }
    }

    val touchedTypes = MicrokitLinterPlugin.getTouchedTypes(localStore)
    var errorMsgs: ISZ[Message] = ISZ()
    for (typeName <- touchedTypes.orderedDependencies) {
      val v = types.typeMap.get(typeName).get
      if (typeName == "Base_Types::Float" || typeName == "Base_Types::Float_32" || typeName == "BaseTypes::Float_64") {
        errorMsgs = errorMsgs :+ Message(Level.Error, v.container.get.identifier.pos, MicrokitCodegen.toolName, "Model contains contract and uses floats/reals that are not supported by Verus")
      }
      if (ops.ISZOps(v.container.get.annexes).exists(a => a.clause.isInstanceOf[GclSubclause])) {
        datatypesWithContracts = datatypesWithContracts :+ v.classifier
      }
    }

    // if gumbo contracts will be integrated then disallow models that touch floats/reals
    // (probably more efficient to lint gumbo contracts here rather than implementing a gumbo lint plugin)
    if (threadsWithContracts.nonEmpty || datatypesWithContracts.nonEmpty) {
      reporter.reports(errorMsgs)
    }

    // C threads' contracts are checked through Rust predicates, which the float restriction
    // above applies to as well; rather than fail a model whose Rust side is fine, leave them
    // unchecked when it bites.
    if (errorMsgs.nonEmpty) {
      cThreadsWithContracts = ISZ()
    }
    localStore = localStore + GumboRustPlugin.KEY_ctwc ~> ISZValue(cThreadsWithContracts)

    return (
      GumboRustPlugin.putThreadsWithContracts(threadsWithContracts,
        GumboRustPlugin.putDatatypesWithContracts(datatypesWithContracts, localStore)))
  }

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      !store.contains(GumboRustPlugin.KEY_GumboRustPlugin) &&
      // may need the app contributions if we need to insert method-level contracts so
      // wait until that phase is done.  The component plugin depends on the typing and
      // api plugins so we can also add datatype invariants and/or integration constraints
      // if needed
      CRustComponentPlugin.hasCRustComponentContributions(store) /* &&

      NOTE: gumbo placeholder markers are now emitted for any missing GUMBO contract artifact

      (GumboRustPlugin.getThreadsWithContracts(store).nonEmpty ||
       GumboRustPlugin.getDatatypesWithContracts(store).nonEmpty ||
       GumboRustPlugin.getGclLibraryAnnexes(symbolTable).nonEmpty)
      */


  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_$name")

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    val hasLibraryAnnexes: B = GumboRustPlugin.getGumboRustContributions(store) match {
      case Some(c) => c.getTheLibraryAnnexes.nonEmpty
      case _ => F
    }

    return (options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !reporter.hasError &&
      !isDisabled(store) &&
      !alreadyFinalized(store) &&
      hasLibraryAnnexes)
  }


  @pure override def handle(model: Aadl,
                            options: HamrCli.CodegenOption,
                            types: AadlTypes,
                            symbolTable: SymbolTable,
                            store: Store,
                            reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store

    var datatypeInvariants: Map[DataIdPath, ISZ[RAST.Fn]] = Map.empty
    val datatypesWithContracts = GumboRustPlugin.getDatatypesWithContracts(localStore)
    if (datatypesWithContracts.nonEmpty) {
      for (d <- datatypesWithContracts) {
        val aadlType = types.getTypeByPath(d)
        reporter.warn(aadlType.container.get.identifier.pos, MicrokitCodegen.toolName,
          s"TODO: Need to handle datatype invariants for ${aadlType.name}")
      }
    }

    var makefileVerusItems: ISZ[ST] = ISZ()

    var libraryAnnexes: Map[String, GumboRustPlugin.LibraryAnnex] = Map.empty
    for(gclLib <-  GumboRustPlugin.getGclLibraryAnnexes(symbolTable)) {
      // add dependencies to gumbo library annexes
      assert(gclLib.name.size == 2 && gclLib.name(1) == "GUMBO__Library", gclLib.name.string)
      val name = gclLib.name(0)

      val (verusLibFuns, developerVerusUifs): (ISZ[RAST.Item], ISZ[RAST.Item]) = handleGclLibrary(gclLib, options, symbolTable, types, store, reporter)


      libraryAnnexes = libraryAnnexes + name ~> GumboRustPlugin.LibraryAnnex(
        rustItems = ISZ(),

        verusItems = verusLibFuns,
        verusDeveloperItems = developerVerusUifs,
        crateDeps = ISZ())
    }

    // A library whose functions call another library's needs that library's crate as a
    // dependency; listed in the order the libraries' crates are, so the output is stable.
    var libraryDeps: Map[String, Set[String]] = Map.empty
    var libraryPos: Map[String, Option[org.sireum.message.Position]] = Map.empty
    for (gclLib <- GumboRustPlugin.getGclLibraryAnnexes(symbolTable)) {
      libraryDeps = libraryDeps + gclLib.name(0) ~> GumboRustPlugin.libraryDependencies(gclLib.annex)
      libraryPos = libraryPos + gclLib.name(0) ~> gclLib.annex.posOpt
    }
    GumboRustPlugin.libraryDependencyCycle(libraryAnnexes.keys, libraryDeps) match {
      case Some(cycle) =>
        reporter.error(libraryPos.get(cycle(0)).get, MicrokitCodegen.toolName,
          st"GUMBO libraries call each other in a cycle: ${(cycle, " -> ")}. Each library is a Rust crate, and Cargo does not allow crates that depend on each other; move the functions they share into one library".render)
      case _ =>
    }
    for (k <- libraryAnnexes.keys) {
      val deps: Set[String] = libraryDeps.get(k).getOrElse(Set.empty[String])
      libraryAnnexes = libraryAnnexes + k ~> libraryAnnexes.get(k).get(
        crateDeps = for (d <- libraryAnnexes.keys if deps.contains(d)) yield d)
    }

    val crateDeps: ISZ[ST] = for (k <- libraryAnnexes.keys) yield st"""$k = { path = "../$k" }"""

    for (thread <- symbolTable.getThreads() if MicrokitUtil.isRusty(thread)) {
      var markers: ISZ[Marker] = ISZ()
      val threadPath = thread.path
      // Components whose policy is non-Verus (e.g. fully-generated monitors) skip
      // all Verus weaving here -- no struct/impl #[verus_verify], no entry-point
      // requires/ensures contracts -- so the emitted app is plain Rust.
      val genVerus: B = StoreUtil.getComponentGenProfile(threadPath, localStore).verusVerified
      val subclauseInfo = GumboRustUtil.getGumboSubclauseOrDummy(threadPath, symbolTable)
      val componentContributions = CRustComponentPlugin.getCRustComponentContributions(localStore)
      val threadContributions = componentContributions.componentContributions.get(threadPath).get
      var appUses = threadContributions.appUses
      var structDef = threadContributions.appStructDef
      var structImpl = threadContributions.appStructImpl.asInstanceOf[RAST.ImplBase]
      var r2u2SpecDef = threadContributions.appR2U2SpecDef
      var r2u2MonitorMethods = threadContributions.appR2U2MonitorMethods
      var moduleLevelEntries = threadContributions.moduleLevelEntries
      val crateDependencies = threadContributions.crateDependencies ++ crateDeps

      appUses = appUses :+ RAST.Use(ISZ(), RAST.IdentString("vstd::prelude::*"))

      // Only Verus-decorate the struct/impl for components whose policy says so;
      // fully-generated monitors (verusVerified=F) stay plain Rust.
      if (options.verusAttributeSyntax && genVerus) {
        val verusVerify = RAST.AttributeST(inner = F, content = st"verus_verify")
        structDef = structDef(attributes = structDef.attributes :+ verusVerify)
        structImpl = structImpl(attributes = structImpl.attributes :+ verusVerify)
      }

      var optStateVarInits: ISZ[RAST.Item] = ISZ()
      if (subclauseInfo.annex.state.nonEmpty) {
        val typeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

        { // add testing apis to get/set the state variables
          var testingApis: ISZ[RAST.Item] = ISZ()
          for (sv <- subclauseInfo.annex.state) {
            val aadlType = typeProvider.getRepresentativeType(types.typeMap.get(sv.classifier).get)
            val np = typeProvider.getTypeNameProvider(aadlType)
            testingApis = testingApis :+ RAST.FnImpl(
              comments = ISZ(RAST.CommentRustDoc(ISZ(st"getter for GUMBO State Variable"))),
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString(s"get_${sv.name}"),
                fnHeader = RAST.FnHeader(F),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(),
                  outputs = RAST.FnRetTyImpl(RAST.TyPath(items = ISZ(np.qualifiedRustNameS), aadlType = Some(aadlType.classifier)))
                ),
                verusHeader = None(), generics = None()),
              attributes = ISZ(), meta =  ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match &crate::app {
                    |    Some(inner) => inner.${sv.name},
                    |    None => panic!("The app is None")
                    |  }
                    |}""" )))))

            val typ = RAST.TyPath(items = ISZ(np.qualifiedRustNameS), aadlType = Some(aadlType.classifier))
            testingApis = testingApis :+ RAST.FnImpl(
              comments = ISZ(RAST.CommentRustDoc(ISZ(st"setter for GUMBO State Variable"))),
              visibility = RAST.Visibility.Public,
              sig = RAST.FnSig(
                ident = RAST.IdentString(s"put_${sv.name}"),
                fnHeader = RAST.FnHeader(F),
                fnDecl = RAST.FnDecl(
                  inputs = ISZ(RAST.ParamImpl(ident = RAST.IdentString("value"), kind = typ)),
                  outputs = RAST.FnRetTyDefault()),
                verusHeader = None(), generics = None()),
              attributes = ISZ(), meta =  ISZ(),
              verusAttributeSyntax = options.verusAttributeSyntax, contract = None(),
              body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
                st"""unsafe {
                    |  match &mut crate::app {
                    |    Some(inner) => inner.${sv.name} = value,
                    |    None => panic!("The app is None")
                    |  }
                    |}""" )))))
          }

          val testingContributions = CRustTestingPlugin.getCRustTestingContributions(localStore).get.testingContributions
          val existing = testingContributions.get(thread.path).get
          localStore = CRustTestingPlugin.putCRustTestingContributions(
            CRustTestingPlugin.CRustTestingContributions(
              testingContributions + thread.path ~> existing(testApiEntries = existing.testApiEntries ++ testingApis)),
            localStore
          )
        }

        { // add a Rust field to the struct definition for each state variable
          var stateVars: ISZ[RAST.StructField] = ISZ()
          for (sv <- subclauseInfo.annex.state) {
            val i = GumboRustUtil.processStateVariable(sv, types, typeProvider)
            stateVars = stateVars :+ i._1
            optStateVarInits = optStateVarInits :+ RAST.ItemST(i._2)
          }
          val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.stateVar)
          markers = markers :+ m
          structDef = structDef(items = structDef.items :+ RAST.MarkerWrap(m, stateVars.asInstanceOf[ISZ[RAST.Item]], "\n", None()))
        }
      } else {
        val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.stateVar)
        markers = markers :+ m
        structDef = structDef(items = structDef.items :+ RAST.MarkerPlaceholder(m))
      }

      if (subclauseInfo.annex.methods.nonEmpty) {

        var verusFuns: ISZ[RAST.Item] = ISZ()
        var developerUifFuns: ISZ[RAST.Item] = ISZ()
        for (m <- subclauseInfo.annex.methods) {
          m match {
            case g: GclSpecMethod =>
              val (gumboFunction, developerFunction) = GumboRustUtil.processGumboSpecMethod(
                m = g,

                owner = thread.classifier,
                optComponent = Some(thread),
                isLibraryMethod = F,

                target = TargetLanguage.verus,
                options = options,

                aadlTypes = types,
                tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = localStore,
                reporter = reporter
              )

              verusFuns = verusFuns :+ gumboFunction
              developerUifFuns = developerUifFuns :+ developerFunction

            case g: GclBodyMethod =>
              verusFuns = verusFuns :+ GumboRustUtil.processGumboBodyMethod(
                m = g,

                owner = thread.classifier,
                optComponent = Some(thread),
                isLibraryMethod = F,

                target = TargetLanguage.verus,
                options = options,

                aadlTypes = types,
                tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = localStore,
                reporter = reporter)
          }
        }

        val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboMethods)
        markers = markers :+ m
        val marked = RAST.MarkerWrap(m, verusFuns, "\n\n", None())


        val items: ISZ[RAST.Item] = if (options.verusAttributeSyntax) {
          // gumbo and uif functions use 'open', 'spec', etc so need to be wrapped in verus macro
          ISZ(RAST.MacCall(macName = "verus", items = ISZ[RAST.Item](marked) ++ developerUifFuns))
        } else {
          marked +: developerUifFuns
        }

        moduleLevelEntries = moduleLevelEntries ++ items

      } else {
        val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.gumboMethods)
        markers = markers :+ m
        moduleLevelEntries = moduleLevelEntries :+ RAST.MarkerPlaceholder(m)
      }

      if (subclauseInfo.annex.integration.nonEmpty) {
        val (requires, ensures) = handleIntegrationConstraints(
          thread = thread,
          subclauseInfo = subclauseInfo,

          types = types,
          tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
          store = localStore,
          reporter = reporter)

        val crustApiContributions = CRustApiPlugin.getCRustApiContributions(localStore).get
        val componentApiContributions = crustApiContributions.apiContributions.get(threadPath).get

        var updatedPutApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.appApiDefaultPutters) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("put_", "")
              if (requires.contains(portId)) {
                val require = requires.get(portId).get
                updatedPutApis = updatedPutApis :+
                  fn(contract = Some(fn.contract.get(requires = fn.contract.get.requires :+ require)))
              } else {
                updatedPutApis = updatedPutApis :+ fn
              }
            case _ => updatedPutApis = updatedPutApis :+ u
          }
        }

        var updatedUnverifiedGetApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.unverifiedGetApis) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("unverified_get_", "")
              if (ensures.contains(portId)) {
                val ensure = ensures.get(portId).get
                updatedUnverifiedGetApis = updatedUnverifiedGetApis :+
                  fn(contract = Some(fn.contract.get(ensures = fn.contract.get.ensures :+ ensure)))
              } else {
                updatedUnverifiedGetApis = updatedUnverifiedGetApis :+ fn
              }
            case _ => updatedUnverifiedGetApis = updatedUnverifiedGetApis :+ u
          }
        }

        var updatedGetApis: ISZ[RAST.Item] = ISZ()
        for (u <- componentApiContributions.appApiDefaultGetters) {
          u match {
            case fn: RAST.FnImpl =>
              val portId = ops.StringOps(fn.ident.string).replaceAllLiterally("get_", "")
              if (ensures.contains(portId)) {
                val ensure = ensures.get(portId).get
                updatedGetApis = updatedGetApis :+
                  fn(contract = Some(fn.contract.get(ensures = fn.contract.get.ensures :+ ensure)))
              } else {
                updatedGetApis = updatedGetApis :+ fn
              }
            case _ => updatedGetApis = updatedGetApis :+ u
          }
        }

        val con = crustApiContributions.addApiContributions(threadPath,
          componentApiContributions(
            unverifiedGetApis = updatedUnverifiedGetApis,
            appApiDefaultPutters = updatedPutApis,
            appApiDefaultGetters = updatedGetApis))

        localStore = CRustApiPlugin.putCRustApiContributions(con, localStore)
      }

      if (subclauseInfo.annex.monitor.nonEmpty) {
        val crustApiContributions = CRustApiPlugin.getCRustApiContributions(localStore).get
        val componentApiContributions = crustApiContributions.apiContributions.get(threadPath).get
        val peekContributions = GumboR2U2Util.peekApiContributions(
          thread = thread,
          tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
          store = localStore)
        val updated = crustApiContributions.addApiContributions(
          threadPath,
          componentApiContributions.combine(peekContributions))
        localStore = CRustApiPlugin.putCRustApiContributions(updated, localStore)
      }

      var updatedImplItems: ISZ[RAST.Item] = ISZ()
      for (i <- structImpl.items) {
        i match {
          case f: RAST.FnImpl =>
            if (f.ident.string == "new") {
              var b: Option[RAST.MethodBody] = None()
              if (optStateVarInits.nonEmpty) {
                b = f.body match {
                  case Some(RAST.MethodBody(ISZ(self: RAST.BodyItemSelf))) =>
                    val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.stateVarInit)
                    markers = markers :+ m
                    val wrapper = RAST.MarkerWrap(m, optStateVarInits, ",\n", Some(","))
                    Some(RAST.MethodBody(ISZ(self(items = self.items :+ wrapper.prettyST))))
                  case _ => halt("Not expecting new to contain anything other than Self {...}")
                }
              } else{
                b = f.body match {
                  case Some(RAST.MethodBody(ISZ(self: RAST.BodyItemSelf))) =>
                    val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.stateVarInit)
                    markers = markers :+ m
                    Some(RAST.MethodBody(ISZ(self(items = self.items :+ RAST.MarkerPlaceholder(m).prettyST))))
                  case _ => halt("Not expecting new to contain anything other than Self {...}")
                }
              }
              updatedImplItems = updatedImplItems :+ f(body = b)
            }
            else if (f.ident.string == "initialize" && genVerus) {
              val init: (Marker, RAST.FnImpl) = if (subclauseInfo.annex.initializes.nonEmpty) {
                handleInitialize(
                  fn = f,
                  thread = thread,
                  subclauseInfo = subclauseInfo,
                  types = types,
                  tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                  symbolTable = symbolTable,
                  store = localStore,
                  reporter = reporter)
              } else {
                handleInitializePlaceholder(f)
              }
              markers = markers :+ init._1
              updatedImplItems = updatedImplItems :+ init._2
            } else if (f.ident.string == "timeTriggered" && genVerus) {
              val compute: (ISZ[Marker], RAST.FnImpl) = if (subclauseInfo.annex.compute.nonEmpty) {
                handleCompute(
                  fn = f,
                  thread = thread,
                  subclauseInfo = subclauseInfo,
                  types = types,
                  tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                  symbolTable = symbolTable,
                  store = localStore,
                  reporter = reporter)
              } else {
                handleComputePlaceholder(f, thread, subclauseInfo)
              }
              markers = markers ++ compute._1

              var timeTriggered: RAST.FnImpl = compute._2
              if (subclauseInfo.annex.monitor.nonEmpty) {
                val (monitorMethods, spec, monitorTimeTriggered) = handleComputeMonitor(
                  fn = compute._2,
                  thread = thread,
                  subclauseInfo = subclauseInfo,
                  types = types,
                  tp = CRustTypePlugin.getCRustTypeProvider(localStore).get,
                  store = localStore,
                  reporter = reporter)
                r2u2SpecDef = Some(spec)
                r2u2MonitorMethods = monitorMethods
                timeTriggered = monitorTimeTriggered
              } else {
                r2u2MonitorMethods = ISZ()
              }
              updatedImplItems = updatedImplItems :+ timeTriggered
            } else {
              updatedImplItems = updatedImplItems :+ i
            }
          case _ => updatedImplItems = updatedImplItems :+ i
        }
      }

      var annotatedModuleLevelItems: ISZ[RAST.Item] = ISZ()
      val externalBodyAttr: ST =
        if (options.verusAttributeSyntax) st"verus_verify(external_body)"
        else st"verifier::external_body"
      for (f <- moduleLevelEntries) {
        f match {
          case (fi: RAST.FnImpl) if genVerus && (fi.ident.string == "log_info" || fi.ident.string == "log_warn_channel") =>
            val attrs = fi.attributes
            annotatedModuleLevelItems = annotatedModuleLevelItems :+ fi(
              attributes =  attrs :+ RAST.AttributeST(F, externalBodyAttr))
          case _ =>
            annotatedModuleLevelItems = annotatedModuleLevelItems :+ f
        }
      }
      moduleLevelEntries = annotatedModuleLevelItems

      localStore = CRustComponentPlugin.putComponentContributions(
        componentContributions.replaceComponentContributions(
          componentContributions.componentContributions + threadPath ~>
            threadContributions(
              markers = markers,
              requiresVerus = genVerus,
              requiresR2U2 = subclauseInfo.annex.monitor.nonEmpty,
              appUses = appUses,
              appStructDef = structDef,
              appStructImpl = structImpl(items = updatedImplItems),
              appR2U2SpecDef = r2u2SpecDef,
              appR2U2MonitorMethods = r2u2MonitorMethods,
              moduleLevelEntries = annotatedModuleLevelItems,
              crateDependencies = crateDependencies)),
        localStore)

      // the test controller has no contracts to verify, and is not part of the shipped build
      if (thread.path != TestSchedulerPlugin.controllerThreadPath(symbolTable.rootSystem.path)) {
        makefileVerusItems = makefileVerusItems :+ st"make -C $${CRATES_DIR}/${CRustComponentPlugin.componentCrateName(thread, localStore)} verus"
      }
    } // end processing thread's contracts

    localStore = MakefileUtil.addMakefileTargets(ISZ("system.mk"), ISZ(MakefileTarget(name = "verus", allowMultiple = F, dependencies = ISZ(), body = makefileVerusItems)), localStore)
    localStore = MakefileUtil.addMakefileTargets(ISZ("Makefile"), ISZ(MakefileTarget(name = "verus", allowMultiple = F, dependencies = ISZ(st"$${TOP_BUILD_DIR}/Makefile"), body = ISZ(st"$${MAKE} -C $${TOP_BUILD_DIR} verus"))), localStore)

    return (GumboRustPlugin.putGumboRustContributions(
      DefaultGumboRustContributions(datatypeInvariants, libraryAnnexes), localStore), ISZ())
  }

  @pure def handleGclLibrary(gclLib: GclAnnexLibInfo,
                             options: HamrCli.CodegenOption,
                             symbolTable: SymbolTable,
                             types: AadlTypes,
                             store: Store,
                             reporter: Reporter): (ISZ[RAST.Item], ISZ[RAST.Item]) = {
    val GclAnnexLibInfo(annex, name, gclSymbolTable) = gclLib
    var gumboFuns: ISZ[RAST.Fn] = ISZ()
    var developerUifFuns: ISZ[RAST.Fn] = ISZ()

    for(m <- annex.methods) {
      m match {
        case g: GclSpecMethod =>
          val (libFun, developerLibFun) = GumboRustUtil.processGumboSpecMethod(
            m = g,

            owner = gclLib.name,
            optComponent = None(),
            isLibraryMethod = T,

            target = TargetLanguage.verus,
            options = options,

            aadlTypes = types,
            tp = CRustTypePlugin.getCRustTypeProvider(store).get,
            gclSymbolTable = gclSymbolTable,
            store = store,
            reporter = reporter)

          gumboFuns = gumboFuns :+ libFun
          developerUifFuns = developerUifFuns :+ developerLibFun

        case g: GclBodyMethod =>
          gumboFuns = gumboFuns :+ GumboRustUtil.processGumboBodyMethod(
            m = g,

            owner = gclLib.name,
            optComponent = None(),
            isLibraryMethod = T,

            target = TargetLanguage.verus,
            options = options,

            aadlTypes = types,
            tp = CRustTypePlugin.getCRustTypeProvider(store).get,
            gclSymbolTable = gclSymbolTable,
            store = store,
            reporter = reporter)
      }
    }

    return (gumboFuns.asInstanceOf[ISZ[RAST.Item]], developerUifFuns.asInstanceOf[ISZ[RAST.Item]])
  }

  @pure def handleIntegrationConstraints(thread: AadlThread,
                                         subclauseInfo: GclAnnexClauseInfo,

                                         types: AadlTypes,
                                         tp: CRustTypeProvider,
                                         store: Store,
                                         reporter: Reporter): (Map[String, RAST.Expr], Map[String, RAST.Expr]) = {
    var requires: Map[String, RAST.Expr] = Map.empty
    var ensures: Map[String, RAST.Expr] = Map.empty

    for (p <- thread.getPorts()) {
      subclauseInfo.gclSymbolTable.integrationMap.get(p) match {
        case Some(spec) =>
          // integration constraints restrict a port's payload, so GclResolver only
          // allows them on payload-carrying ports; pure event ports the thread owns
          // never appear in integrationMap and are simply skipped by this loop
          val isEvent: B = p match {
            case i: AadlEventDataPort => T
            case i: AadlDataPort => F
            case x => halt(s"Infeasible: integration constraints cannot be applied to ${x.identifier} which is not a data or event data port")
          }
          spec match {
            case a: GclAssume =>
              // assume integration clauses can only be applied to incoming ports.  The api therefore
              // ensures that the getter's return value will satisfy the assume clause.

              val subs = Map.empty[String, String] + p.identifier ~>
                s"${CRustApiPlugin.apiResultName}${if (isEvent) ".unwrap()" else ""}"

              val rspec: RAST.Expr = GumboRustUtil.processGumboSpecH(
                spec = a,
                component = thread,
                context = Context.integration_constraint,
                substitutions = subs,
                isAssumeRequires = F,
                // becomes a postcondition of the port's `&mut self` getter
                isGuaranteeEnsures = T,
                types = types,
                tp = tp,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)

              if (isEvent) {
                ensures = ensures + p.identifier ~> RAST.ExprST(
                  st"""(res.is_none() ||
                      |  ${rspec.prettyST})""")
              } else {
                ensures = ensures + p.identifier ~> rspec
              }

            case g: GclGuarantee =>
              // guarantee integration clauses can only be applied to outgoing ports.  They become
              // requirements on the param value passed to the api -- the param's name will always be 'value'

              val subs = Map.empty[String, String] + p.identifier ~> s"${CRustApiPlugin.apiParameterName}"
              requires = requires + p.identifier ~> GumboRustUtil.processGumboSpecH(
                spec = g,
                component = thread,
                context = Context.integration_constraint,

                substitutions = subs,

                isAssumeRequires = F,

                isGuaranteeEnsures = F,

                types = types,
                tp = tp,
                gclSymbolTable = subclauseInfo.gclSymbolTable,
                store = store,
                reporter = reporter)
          }
        case _ =>
      }
    }
    return (requires, ensures)
  }

  @pure def handleInitialize(fn: RAST.FnImpl,
                             thread: AadlThread,
                             subclauseInfo: GclAnnexClauseInfo,

                             types: AadlTypes,
                             tp: CRustTypeProvider,
                             symbolTable: SymbolTable,
                             store: Store,
                             reporter: Reporter): (Marker, RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    val ensures: ISZ[RAST.Expr] = for (g <- subclauseInfo.annex.initializes.get.guarantees) yield
      GumboRustUtil.processGumboSpec(
        spec = g,
        component = thread,
        context = Context.initialize_clause,

        isAssumeRequires = F,
        isGuaranteeEnsures = T,

        types = types,
        tp = tp,
        gclSymbolTable = subclauseInfo.gclSymbolTable,
        store = store,
        reporter = reporter)


    val ensuresMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.initializationEnsures)
    return (ensuresMarker, fn(contract = Some(RAST.FnContract(
      optRequiresMarker = None(),
      requires = ISZ(),
      optEnsuresMarker = Some(ensuresMarker),
      ensures = ensures))))
  }

  @pure def handleInitializePlaceholder(fn: RAST.FnImpl): (Marker, RAST.FnImpl) = {
    val m = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.initializationEnsures)
    return (m, fn(contract = Some(RAST.FnContract(
      optRequiresMarker = None(),
      requires = ISZ(),
      optEnsuresMarker = Some(m),
      ensures = ISZ()))))
  }

  @pure def handleCompute(fn: RAST.FnImpl,
                          thread: AadlThread,
                          subclauseInfo: GclAnnexClauseInfo,

                          types: AadlTypes,
                          tp: CRustTypeProvider,
                          symbolTable: SymbolTable,
                          store: Store,
                          reporter: Reporter): (ISZ[Marker], RAST.FnImpl) = {
    assert (fn.contract.isEmpty, "who filled this in already?")

    // general assumes clauses
    val requires: ISZ[RAST.Expr] = getAadlRequires(thread) ++
      (for (r <- subclauseInfo.annex.compute.get.assumes) yield
        GumboRustUtil.processGumboSpec(
          spec = r,
          component = thread,
          context = Context.compute_clause,
          isAssumeRequires = T,
          isGuaranteeEnsures = F,
          types = types,
          tp = tp,
          gclSymbolTable = subclauseInfo.gclSymbolTable,
          store = store,
          reporter = reporter))

    val ensures: ISZ[RAST.Expr] = {
      // general ensures clauses
      (for (r <- subclauseInfo.annex.compute.get.guarantees) yield
        GumboRustUtil.processGumboSpec(
          spec = r,
          component = thread,
          context = Context.compute_clause,
          isAssumeRequires = F,
          isGuaranteeEnsures = T,
          types = types,
          tp = tp,
          gclSymbolTable = subclauseInfo.gclSymbolTable,
          store = store,
          reporter = reporter)) ++
        // gumbo compute cases clauses
        (for (c <- subclauseInfo.annex.compute.get.cases) yield
          GumboRustUtil.processGumboCase(
            c = c,
            component = thread,
            store = store,
            aadlTypes = types ,
            tp = tp,
            gclSymbolTable = subclauseInfo.gclSymbolTable,
            reporter = reporter)) ++ getAlertEnsures(subclauseInfo)
    }

    var optEnsuresMarker: Option[Marker] = None()
    var optRequiresMarker: Option[Marker] = None()
    var markers: ISZ[Marker] = ISZ()

    if (requires.nonEmpty) {
      val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
      markers = markers :+ m
      optRequiresMarker = Some(m)
    } else {
      val p = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
      markers = markers :+ p
      optRequiresMarker = Some(p)
    }

    if (ensures.nonEmpty) {
      val m = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
      markers = markers :+ m
      optEnsuresMarker = Some(m)
    } else {
      val p = Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
      markers = markers :+ p
      optEnsuresMarker = Some(p)
    }

    return (markers,
      fn(contract = Some(RAST.FnContract(
        optRequiresMarker = optRequiresMarker,
        requires = requires,
        optEnsuresMarker = optEnsuresMarker,
        ensures = ensures))))
  }

  // Require outgoing event ports to be empty before user code runs.
  @pure def getAadlRequires(thread: AadlThread): ISZ[RAST.Expr] = {
    val outputPorts: ISZ[ST] = for (port <- thread.getPorts()
                                    if !port.isInstanceOf[AadlDataPort] &&
                                      port.direction == Direction.Out) yield
      st"old(api).${port.identifier}.is_none()"
    if (outputPorts.nonEmpty) {
      return ISZ(RAST.ExprST(st"""// assume AADL_Requirement
                                    |//   All outgoing event ports must be empty
                                    |${(outputPorts, ",\n")}"""))
    }
    return ISZ()
  }

  // Reserve alert ports for verdicts published by the R2U2 monitor.
  @pure def getAlertEnsures(subclauseInfo: GclAnnexClauseInfo): ISZ[RAST.Expr] = {
    subclauseInfo.annex.monitor match {
      case Some(monitor) if monitor.alerts.nonEmpty =>
        val alertPorts: ISZ[ST] = for (alert <- monitor.alerts) yield
          st"old(api).${alert.portId} == final(api).${alert.portId}"
        return ISZ(RAST.ExprST(st"""// guarantee Monitor_Requirement
                                   |//   Alert ports are reserved for the monitor
                                   |${(alertPorts, ",\n")}"""))
      case _ => return ISZ()
    }
  }

  @pure def handleComputePlaceholder(fn: RAST.FnImpl,
                                     thread: AadlThread,
                                     subclauseInfo: GclAnnexClauseInfo): (ISZ[Marker], RAST.FnImpl) = {
    // don't add the AADL requirements (i.e. outgoing ports are empty) when there is no monitor or gumbo
    // compute clause in the model
    val requires: ISZ[RAST.Expr] = if (subclauseInfo.annex.monitor.nonEmpty) getAadlRequires(thread) else ISZ()
    val requiresMarker: Marker = if (requires.nonEmpty) {
      Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
    } else {
      Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredRequires)
    }
    val ensures: ISZ[RAST.Expr] = getAlertEnsures(subclauseInfo)
    val ensuresMarker: Marker = if (ensures.nonEmpty) {
      Marker.createSlashMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
    } else {
      Marker.createSlashPlaceholderMarker(GumboRustUtil.GumboMarkers.timeTriggeredEnsures)
    }
    val markers: ISZ[Marker] = ISZ(requiresMarker, ensuresMarker)
    return (markers,
      fn(contract = Some(RAST.FnContract(
        optRequiresMarker = Some(requiresMarker),
        requires = requires,
        optEnsuresMarker = Some(ensuresMarker),
        ensures = ensures))))
  }

  @pure def handleComputeMonitor(fn: RAST.FnImpl,
                                 thread: AadlThread,
                                 subclauseInfo: GclAnnexClauseInfo,
                                 types: AadlTypes,
                                 tp: CRustTypeProvider,
                                 store: Store,
                                 reporter: Reporter): (ISZ[RAST.Item], RAST.R2U2SpecDef, RAST.FnImpl) = {
    var specs = RAST.R2U2SpecDef(structs = ISZ(), enums = ISZ(), inputs = ISZ(), defines = ISZ(), ftspecs = ISZ(), ptspecs = ISZ())
    var monitorInputs: Map[String, GumboR2U2Util.R2U2MonitorInput] = Map.empty

    for (guarantee <- subclauseInfo.annex.monitor.get.guarantees) {
      val (c2poSpec, tense, inputs) = GumboRustUtil.processGumboSpecR2U2(
        spec = guarantee,
        component = thread,
        context = Context.monitor_clause,
        isAssumeRequires = F,
        types = types,
        tp = tp,
        stateVars = subclauseInfo.annex.state,
        store = store,
        reporter = reporter)
      tense match {
        case GumboC2POUtil.SpecTense.Future => specs = specs(ftspecs = specs.ftspecs :+ c2poSpec)
        case GumboC2POUtil.SpecTense.Past => specs = specs(ptspecs = specs.ptspecs :+ c2poSpec)
      }
      monitorInputs = monitorInputs ++ inputs.entries
    }

    val expandedInputs: (RAST.R2U2SpecDef, Map[String, GumboR2U2Util.R2U2MonitorInput]) =
      GumboR2U2Util.expandStructInputs(specs, monitorInputs)
    specs = expandedInputs._1
    monitorInputs = expandedInputs._2

    val ports: Map[String, AadlPort] = Map.empty[String, AadlPort] ++
      (thread.getPorts().map((port: AadlPort) => port.identifier ~> port))
    var referencedInputPorts: Set[String] = Set.empty
    var referencedOutputPorts: Set[String] = Set.empty
    for (monitorInput <- monitorInputs.values; portId <- monitorInput.referencedPorts.elements) {
      ports.get(portId) match {
        case Some(port) if port.direction == Direction.In =>
          referencedInputPorts = referencedInputPorts + portId
        case Some(port) if port.direction == Direction.Out =>
          referencedOutputPorts = referencedOutputPorts + portId
        case _ =>
      }
    }

    var preItems: ISZ[RAST.BodyItem] = ISZ()
    var postItems: ISZ[RAST.BodyItem] = ISZ()
    var inputGets: ISZ[RAST.BodyItem] = ISZ()
    for (port <- thread.getPorts().filter(p => p.direction == Direction.In)
         if referencedInputPorts.contains(port.identifier) &&
           !StoreUtil.isSynthetic(port.path, store)) {
      preItems = preItems :+ RAST.BodyItemST(
        st"let ${port.identifier} = api.peek_${port.identifier}();")
      inputGets = inputGets :+ RAST.BodyItemST(
        st"let ${port.identifier} = api.get_${port.identifier}();")
    }
    for (port <- thread.getPorts().filter(p => p.direction == Direction.Out)
         if referencedOutputPorts.contains(port.identifier)) {
      postItems = postItems :+ RAST.BodyItemST(
        st"let ${port.identifier} = api.peek_${port.identifier}();")
    }
    if (preItems.nonEmpty) { preItems = preItems :+ RAST.BodyItemST(st"") }
    if (postItems.nonEmpty) { postItems = postItems :+ RAST.BodyItemST(st"") }
    val monitorInstance = RAST.BodyItemST(
      st"""let r2u2_monitor = unsafe {
          |  R2U2_MONITOR.as_mut().expect("R2U2 monitor used before initialization")
          |};""")
    preItems = preItems :+ monitorInstance :+ RAST.BodyItemST(st"")
    postItems = postItems :+ monitorInstance :+ RAST.BodyItemST(st"")

    var index = 0
    for (entry <- monitorInputs.entries) {
      val name: String = entry._1
      val monitorInput: GumboR2U2Util.R2U2MonitorInput = entry._2
      val typeName: String = monitorInput.enumTypeOpt match {
        case Some(enumType) => // Check if enum type and adjust type name to enum type name
          if (!ops.ISZOps(specs.enums).exists(existing => existing.name == enumType.name)) {
            specs = specs(enums = specs.enums :+ enumType)
          }
          enumType.name
        case _ => monitorInput.arrayTypeOpt match {
          // Check if array type and adjust type name to array syntax
          case Some(arrayType) => s"${arrayType.elementType.string}[${arrayType.size}]"
          case _ => monitorInput.expType.string
        }
      }
      specs = specs(inputs = specs.inputs :+ RAST.R2U2InputDef(
        name, typeName, index, monitorInput.arrayTypeOpt.map(t => t.size)))
      val loadSignal: RAST.BodyItem = monitorInput.expType match {
        case GumboC2POUtil.C2POType.bool =>
          RAST.BodyItemST(st"""r2u2_core::load_bool_signal(&mut r2u2_monitor.monitor, $index, ${monitorInput.exp.prettyST}); // Loading signal $name into index $index""")
        case GumboC2POUtil.C2POType.int =>
          RAST.BodyItemST(st"""r2u2_core::load_int_signal(&mut r2u2_monitor.monitor, $index, (${monitorInput.exp.prettyST}) as i32); // Loading signal $name into index $index""")
        case GumboC2POUtil.C2POType.float =>
          RAST.BodyItemST(st"""r2u2_core::load_float_signal(&mut r2u2_monitor.monitor, $index, (${monitorInput.exp.prettyST}) as f64); // Loading signal $name into index $index""")
        case GumboC2POUtil.C2POType.enumeration =>
          RAST.BodyItemST(st"""r2u2_core::load_int_signal(&mut r2u2_monitor.monitor, $index, ${monitorInput.exp.prettyST} as i32); // Loading enum signal $name into index $index""")
        case GumboC2POUtil.C2POType.array =>
          val arrayType: GumboC2POUtil.C2POArray = monitorInput.arrayTypeOpt.get
          arrayType.elementType match {
            case GumboC2POUtil.C2POType.bool =>
              RAST.BodyItemST(st"""r2u2_core::load_bool_array(&mut r2u2_monitor.monitor, $index, &${monitorInput.exp.prettyST}); // Loading array signal $name into indices $index..${index + arrayType.size - 1}""")
            case GumboC2POUtil.C2POType.int =>
              RAST.BodyItemST(st"""r2u2_core::load_int_array(&mut r2u2_monitor.monitor, $index, &(${monitorInput.exp.prettyST}).map(|value| value as i32)); // Loading array signal $name into indices $index..${index + arrayType.size - 1}""")
            case GumboC2POUtil.C2POType.float =>
              RAST.BodyItemST(st"""r2u2_core::load_float_array(&mut r2u2_monitor.monitor, $index, &(${monitorInput.exp.prettyST}).map(|value| value as f64)); // Loading array signal $name into indices $index..${index + arrayType.size - 1}""")
            case _ => halt("Unsupported R2U2 array element type")
          }
        case GumboC2POUtil.C2POType.struct => halt("R2U2 struct input was not expanded")
      }
      val referencesOutput = ops.ISZOps(monitorInput.referencedPorts.elements).exists(
        portId => referencedOutputPorts.contains(portId))
      // Output ports and current state variables are observed after dispatch.
      if (referencesOutput || monitorInput.isPostStateVar) {
        postItems = postItems :+ loadSignal
      } else {
        preItems = preItems :+ loadSignal
      }
      monitorInput.arrayTypeOpt match {
        case Some(arrayType) => index = index + arrayType.size // Increment mapping index by array size
        case _ => index = index + 1
      }
    }

    if (postItems.nonEmpty) { postItems = postItems :+ RAST.BodyItemST(st"") }
    val orderedSpecs = specs.ftspecs ++ specs.ptspecs
    val outputItems = GumboR2U2Util.processRustOutputs(
      thread = thread,
      orderedSpecs = orderedSpecs,
      alerts = subclauseInfo.annex.monitor.get.alerts)
    postItems = postItems ++ outputItems

    val propertyVariants: ISZ[ST] = for (spec <- orderedSpecs) yield st"${spec.id},"
    val alertSpecNumbers = GumboR2U2Util.getAlertSpecNumbers(
      orderedSpecs, subclauseInfo.annex.monitor.get.alerts)
    var nonAlertHandlerCases: ISZ[ST] = ISZ()
    for (i <- 0 until orderedSpecs.size
         if !ops.ISZOps(alertSpecNumbers.values).contains(i)) {
        val specId = orderedSpecs(i).id
        nonAlertHandlerCases = nonAlertHandlerCases :+ st"""R2U2Property::$specId => {
            |  match verdict {
            |    Some(true) => log_info("$specId is currently True"),
            |    Some(false) => log_info("$specId is currently False"),
            |    None => log_info("$specId is currently unknown"),
            |  }
            |},"""
    }
    val threadId = MicrokitUtil.getComponentIdPath(thread)
    val verdictHandlerMarker = Marker.createSlashMarker(CRustComponentPlugin.MarkerR2U2VerdictHandler)
    val verdictHandler = fn(
      sig = fn.sig(
        ident = RAST.IdentString("handle_r2u2_verdict"),
        fnDecl = fn.sig.fnDecl(
          inputs = fn.sig.fnDecl.inputs ++ ISZ[RAST.Param](
            RAST.ParamFixMe(st"property: R2U2Property"),
            RAST.ParamFixMe(st"verdict: Option<bool>")),
          outputs = RAST.FnRetTyDefault())),
      comments = ISZ(),
      attributes = ISZ(),
      visibility = RAST.Visibility.Public,
      meta = ISZ(),
      verusAttributeSyntax = F,
      contract = Some(RAST.FnContract(
        optRequiresMarker = None(),
        requires = ISZ(),
        optEnsuresMarker = None(),
        ensures = getAlertEnsures(subclauseInfo))),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
        st"""${verdictHandlerMarker.beginMarker}
            |// Optional implementation placeholder for handling R2U2 verdicts.
            |match property {
            |  ${(nonAlertHandlerCases, "\n")}
            |  _ => {}
            |}
            |${verdictHandlerMarker.endMarker}""")))))
    val monitorApiItems = RAST.ItemST(
      st"""verus! {
          |  #[allow(non_camel_case_types)]
          |  pub enum R2U2Property {
          |    ${(propertyVariants, "\n")}
          |  }
          |
          |  impl $threadId {
          |    ${verdictHandler.prettyST}
          |  }
          |}""")

    var preInputs: ISZ[RAST.Param] = ISZ()
    for (input <- fn.sig.fnDecl.inputs) {
      input match {
        case param: RAST.ParamImpl if param.ident.string == "api" =>
          param.kind match {
            case ty: RAST.TyRef =>
              preInputs = preInputs :+ param(kind = ty(mutty = ty.mutty(mutbl = RAST.Mutability.Not)))
            case _ =>
              preInputs = preInputs :+ param
          }
        case _ =>
          preInputs = preInputs :+ input
      }
    }
    val initializeFn = fn(
      sig = fn.sig(
        ident = RAST.IdentString("r2u2_monitor_initialize"),
        generics = None(),
        fnDecl = fn.sig.fnDecl(inputs = ISZ(RAST.ParamFixMe(st"&mut self")))),
      attributes = ISZ(),
      contract = None(),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
        st"""let mut r2u2_monitor = R2U2Monitor::new();
            |r2u2_core::update_binary_file(include_bytes!("spec.bin"), &mut r2u2_monitor.monitor);
            |unsafe { R2U2_MONITOR = Some(r2u2_monitor); }""")))))
    val preFn = fn(
      sig = fn.sig(
        ident = RAST.IdentString("r2u2_monitor_pre_timeTriggered"),
        fnDecl = fn.sig.fnDecl(inputs = preInputs)),
      attributes = ISZ(),
      contract = None(),
      body = Some(RAST.MethodBody(preItems)))
    val postFn = fn(
      sig = fn.sig(ident = RAST.IdentString("r2u2_monitor_post_timeTriggered")),
      attributes = ISZ(),
      contract = None(),
      body = Some(RAST.MethodBody(postItems)))

    val monitorMethods: ISZ[RAST.Item] = ISZ(monitorApiItems, initializeFn, preFn, postFn)
      
    var timeTriggered: RAST.FnImpl = fn
    if (inputGets.nonEmpty) {
      fn.body match {
        case Some(RAST.MethodBody(items)) =>
          timeTriggered = fn(body = Some(RAST.MethodBody(inputGets ++ items)))
        case _ =>
      }
    }
    return (monitorMethods, specs, timeTriggered)
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    GumboRustPlugin.getGumboRustContributions(store) match {
      case Some(c) =>
        // write out any gumbo library annex crates

        assert (c.getTheLibraryAnnexes.nonEmpty)
        var localStore = store
        var resources: ISZ[Resource] = ISZ()

        for (e <- c.getTheLibraryAnnexes.entries) {

          val rootDir = s"${options.sel4OutputDir.get}/crates/${e._1}"

          { // crates/<aadl-package-name>/src/lib.rs
            var markers: ISZ[Marker] = ISZ()

            val path = s"$rootDir/src/lib.rs"

            val rustMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboLibRust)
            markers = markers :+ rustMarker

            val rustSection = RAST.MarkerWrap(
              marker = rustMarker,
              items = e._2.rustItems,
              sep = "\n\n",
              optLastItemSep = None())

            val verusMarker = Marker.createSlashMarker(GumboRustUtil.GumboMarkers.gumboLibVerus)
            markers = markers :+ verusMarker

            var withExecSpecs: Set[String] = Set.empty
            for (lib <- GumboRustPlugin.getGclLibraryAnnexes(symbolTable) if lib.name(0) == e._1) {
              withExecSpecs = withExecSpecs ++ GumboRustPlugin.libraryFnsWithExecSpecs(lib.annex).elements
            }

            val verusSection: ISZ[RAST.Item] = ISZ[RAST.Item] (
              RAST.MarkerWrap(
                marker = verusMarker,
                items = e._2.verusItems ++ GumboRustPlugin.libraryExecSpecs(e._2.rustItems, e._2.verusItems, withExecSpecs),
                sep = "\n\n",
                optLastItemSep = None())) ++ e._2.verusDeveloperItems

            val verusMacro = RAST.MacCall(
              macName = "verus",
              items = verusSection)

            val content =
              st"""#![cfg_attr(not(test), no_std)]
                  |
                  |${RustUtil.defaultCrateLevelAttributes}
                  |
                  |${CommentTemplate.safeToEditComment_slash}
                  |
                  |use data::*;
                  |use vstd::prelude::*;
                  |
                  |${GumboRustUtil.RustImplicationMacros}
                  |
                  |${rustSection.prettyST}
                  |
                  |${verusMacro.prettyST}
                  |"""

            resources = resources :+ ResourceUtil.createResourceWithMarkers(
              path = path, content = content, markers = markers, invertMarkers = F, overwrite = F)
          }

          { // crates/<aadl-package-name>/Cargo.toml
            // data, then the other library crates this library's functions call
            val dependencies: ISZ[ST] = st"""data = { path="../data" }""" +:
              (for (d <- e._2.crateDeps) yield st"""$d = { path = "../$d" }""")
            val content = st"""${CommentTemplate.safeToEditComment_hash}
                              |
                              |[package]
                              |name = "${e._1}"
                              |version = "0.1.0"
                              |edition = "2021"
                              |
                              |[dependencies]
                              |${(dependencies, "\n")}
                              |
                              |${RustUtil.verusCargoDependencies(store)}
                              |
                              |${RustUtil.commonCargoTomlEntries}
                              |"""
            val cargoTomlPath = s"${rootDir}/Cargo.toml"
            resources = resources :+ ResourceUtil.createResourceH(path = cargoTomlPath, content = content, overwrite = F, isDatatype = T)
          }

          { // crates/<aadl-package-name>/rust-toolchain.toml
            val content = RustUtil.defaultRustToolChainToml(store)

            val rusttoolchain = s"${rootDir}/rust-toolchain.toml"
            resources = resources :+ ResourceUtil.createResourceH(path = rusttoolchain, content = content, overwrite = F, isDatatype = T)

          }
        }

        return (localStore + s"FINALIZED_$name" ~> BoolValue(T), resources)

      case _ => halt("Infeasible")
    }
  }
}

@datatype class DefaultGumboRustPlugin extends GumboRustPlugin {
  @strictpure override def name: String = "DefaultGumboRustPlugin"
}
