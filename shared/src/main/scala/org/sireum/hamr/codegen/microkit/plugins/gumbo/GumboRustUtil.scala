// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, GclAnnexClauseInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.Context
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.rust.FnVerusHeader
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir._
import org.sireum.lang.ast.Stmt.Return
import org.sireum.lang.tipe.TypeHierarchy
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboRustUtil {

  object GumboMarkers {
    val stateVar: String = "MARKER STATE VARS"
    val stateVarInit: String = "MARKER STATE VAR INIT"

    val initializationEnsures: String = "MARKER INITIALIZATION ENSURES"

    val timeTriggeredRequires: String = "MARKER TIME TRIGGERED REQUIRES"
    val timeTriggeredEnsures: String = "MARKER TIME TRIGGERED ENSURES"

    val gumboMethods: String = "MARKER GUMBO METHODS"
  }

  val RustImplicationMacros: ST = st"""macro_rules! implies {
                                      |  ($$lhs: expr, $$rhs: expr) => {
                                      |    !$$lhs || $$rhs
                                      |  };
                                      |}
                                      |
                                      |macro_rules! impliesL {
                                      |  ($$lhs: expr, $$rhs: expr) => {
                                      |    !$$lhs | $$rhs
                                      |  };
                                      |}"""

  @pure def getGumboSubclauseOpt(threadId: IdPath, symbolTable: SymbolTable): Option[GclAnnexClauseInfo] = {
    symbolTable.annexClauseInfos.get(threadId) match {
      case Some(clauses) =>
        assert (clauses.size < 2, "gcl's symbol resolver should have only allowed one gcl clause per component")
        return Some(clauses(0).asInstanceOf[GclAnnexClauseInfo])
      case _ => return None()
    }
  }

  @pure def getGumboSubclause(threadId: IdPath, symbolTable: SymbolTable): GclAnnexClauseInfo = {
    return getGumboSubclauseOpt(threadId, symbolTable).get
  }

  @pure def getGumboSubclauseOrDummy(threadId: IdPath, symbolTable: SymbolTable): GclAnnexClauseInfo = {
    getGumboSubclauseOpt(threadId, symbolTable) match {
      case Some(c) => return c
      case _ =>
        return GclAnnexClauseInfo(
          annex = GclSubclause(
            state = ISZ(),
            methods = ISZ(),
            invariants = ISZ(),
            initializes = None(),
            integration = None(),
            compute = None(),
            attr = Attr(None())),
          gclSymbolTable = GclSymbolTable(
            rexprs = HashMap.empty,
            slangTypeHierarchy = TypeHierarchy.empty,
            apiReferences = ISZ(),
            integrationMap = Map.empty,
            computeHandlerPortMap = Map.empty))
    }
  }
  def processDescriptor(descriptor: Option[String], pad: String): Option[ST] = {
    def getPipeLoc(cis: ISZ[C]): Z = {
      var firstNonSpace: Z = 0
      while (cis(firstNonSpace) == ' ' || cis(firstNonSpace) == '\t') {
        firstNonSpace = firstNonSpace + 1
      }
      if (firstNonSpace < cis.size - 1 && cis(firstNonSpace) == '|') {
        return firstNonSpace + 1
      } else {
        return 0
      }
    }

    val ret: Option[ST] = descriptor match {
      case Some(s) =>
        val lines = StringUtil.split_PreserveEmptySegments(s, (c: C) => c == '\n')
        var mLines: ISZ[ST] = ISZ(st"${pad}${lines(0)}")
        if (lines.size > 1) {
          for (i <- 1 until lines.size) {
            val lop = conversions.String.toCis(lines(i))
            val pipeLoc = getPipeLoc(lop)
            val x = conversions.String.fromCis(ops.ISZOps(lop).slice(pipeLoc, lop.size))
            mLines = mLines :+ st"${pad}${x}"
          }
        }
        Some(st"${(mLines, "\n")}")
      case _ => None()
    }
    return ret
  }

  @pure def processGumboSpec(spec: GclSpec,

                             component: AadlComponent,
                             context: Context.Type,

                             isAssumeRequires: B,

                             types: AadlTypes,
                             tp: CRustTypeProvider,
                             gclSymbolTable: GclSymbolTable,
                             store: Store,
                             reporter: Reporter): RAST.Expr = {
    return processGumboSpecH(spec, component, context, isAssumeRequires, Map.empty, types, tp, gclSymbolTable, store, reporter)
  }

  @pure def processGumboSpecH(spec: GclSpec,

                              component: AadlComponent,
                              context: Context.Type,

                              isAssumeRequires: B,

                              substitutions: Map[String, String],

                              types: AadlTypes,
                              tp: CRustTypeProvider,
                              gclSymbolTable: GclSymbolTable,
                              store: Store,
                              reporter: Reporter): RAST.Expr = {
    val typ: String =
      spec match {
        case g: GclInvariant => "invariant"
        case g: GclAssume => "assume"
        case g: GclGuarantee => "guarantee"
        case _ => halt(s"Infeasible: $spec")
      }
    val verusExp =
      SlangExpUtil.rewriteExpH(
        rexp = SlangExpUtil.getRexp(spec.exp, gclSymbolTable),

        owner = component.classifier,
        optComponent = Some(component),
        context = context,

        inRequires = isAssumeRequires,
        inVerus = T,
        substitutions = substitutions,
        aadlTypes = types,
        tp = tp,
        store = store,
        reporter = reporter)
    return RAST.ExprST(
      st"""// $typ ${spec.id}
          |${GumboRustUtil.processDescriptor(spec.descriptor, "//   ")}
          |$verusExp""")
  }

  @pure def processGumboCase(c: GclCaseStatement,
                             component: AadlComponent,

                             aadlTypes: AadlTypes,
                             tp: CRustTypeProvider,
                             gclSymbolTable: GclSymbolTable,
                             store: Store,
                             reporter: Reporter): RAST.Expr = {
    val requires: Option[ST] =
      if (c.assumes.nonEmpty)
        Some(SlangExpUtil.rewriteExpH(
          rexp = SlangExpUtil.getRexp(c.assumes.get, gclSymbolTable),

          owner = component.classifier,
          optComponent = Some(component),
          context = Context.compute_clause,

          substitutions = Map.empty,
          inRequires = T,
          inVerus = T,
          aadlTypes = aadlTypes,
          tp = tp,
          store = store,
          reporter = reporter))
      else None()
    val ensures =
      SlangExpUtil.rewriteExpH(
        rexp = SlangExpUtil.getRexp(c.guarantees, gclSymbolTable),

        owner = component.classifier,
        optComponent = Some(component),
        context = Context.compute_clause,

        substitutions = Map.empty,
        inRequires = F,
        inVerus = T,
        aadlTypes = aadlTypes,
        tp = tp,
        store = store,
        reporter = reporter)
    val e: ST =
      if (requires.nonEmpty)
        st"""($requires) ==>
            |  ($ensures)"""
      else ensures
    return RAST.ExprST(
      st"""// case ${c.id}
          |${GumboRustUtil.processDescriptor(c.descriptor, "//   ")}
          |$e""")
  }


  @pure def processStateVariable(sv: GclStateVar, types: AadlTypes, tp: CRustTypeProvider): (RAST.StructField, ST) = {
    val aadlType = tp.getRepresentativeType(types.typeMap.get(sv.classifier).get)
    val np = tp.getTypeNameProvider(aadlType)
    return (RAST.StructField(
      visibility = RAST.Visibility.Public,
        isGhost = F,
        ident = RAST.IdentString(sv.name),
      fieldType = RAST.TyPath(
        items = ISZ(np.qualifiedRustNameS),
        aadlType = Some(aadlType.classifier))),
      st"${sv.name}: ${MicrokitTypeUtil.getCRustTypeDefaultValue(aadlType, tp)}"
    )
  }

  def processGumboMethod(m: GclMethod,

                         owner: IdPath,
                         optComponent: Option[AadlComponent],
                         isLibraryMethod: B,

                         inVerus: B,

                         aadlTypes: AadlTypes,
                         tp: CRustTypeProvider,
                         gclSymbolTable: GclSymbolTable,
                         store: Store,
                         reporter: Reporter): RAST.Fn = {

    def r(t: SAST.Type.Named): CRustTypeNameProvider = {
      val retType: ISZ[String] = for(id <- t.name.ids) yield id.value
      val retAadlType = tp.getRepresentativeType(aadlTypes.getTypeByPath(retType))
      return tp.getTypeNameProvider(retAadlType)
    }

    var inputs: ISZ[RAST.Param] = ISZ()
    for(p <- m.method.sig.params) {
      inputs = inputs :+ RAST.ParamImpl(
        ident = RAST.IdentString(p.id.value),
        kind = RAST.TyPath(ISZ(r(p.tipe.asInstanceOf[SAST.Type.Named]).qualifiedRustNameS), None()))
    }

    val contractOpt: Option[RAST.FnContract] =
      if (m.method.mcontract.isEmpty) {
        None()
      } else {
        halt("TODO: need to handle method contracts for RUST/Verus")
      }

    val optBody: Option[RAST.MethodBody] = m.method.bodyOpt match {
      case Some(body) =>
        assert (body.stmts.size == 1, s"Currently expecting GUMBO methods to have a single statement")
        val exp: ST = body.stmts(0) match {
          case r: Return =>
            assert (r.expOpt.nonEmpty, "Currently expecting GUMBO methods to return values")

            val retExp = SlangExpUtil.rewriteExp(
              rexp = SlangExpUtil.getRexp(r.expOpt.get, gclSymbolTable),

              owner = owner,
              optComponent = optComponent,
              context = if (isLibraryMethod) Context.library_function else Context.subclause_function,

              inRequires = F,
              inVerus = inVerus,
              tp = tp,
              aadlTypes = aadlTypes,
              store = store,
              reporter = reporter)

            // could also walk the expression and look for the use of arith operators (e.g. + - * ...)
            val mayNeedCast: B = r.expOpt.get match {
              case e: SAST.Exp.Binary => T // e.g. a + b
              case e: SAST.Exp.If => T // e.g. if (T) a + b else b + c
              case _ => F
            }

            if (inVerus && MicrokitTypeUtil.isNumericType(m.method.sig.returnType.typedOpt.get) && mayNeedCast) {
              // For the GUMBO method
              //  def add(a: Base_Types::Integer_32, b: Base_Types::Integer_32): Base_Types::Integer_32 := a + b;
              // verus will evaluate a + b in mathematical integers (int), not in the machine type i32. Therefore,
              // we need to cast the return value back to the declared return type

              val rType = m.method.sig.returnType.typedOpt.get.asInstanceOf[SAST.Typed.Name]
              val aadlType = MicrokitTypeUtil.getAadlTypeFromSlangTypeH(rType, aadlTypes)
              val np = tp.getTypeNameProvider(aadlType)

              st"($retExp) as ${(np.qualifiedRustNameS, "::")}"
            } else {
              retExp
            }

          case x =>
            halt(s"Currently expecting GUMBO methods to only have return statements: ${x.prettyST.render}")
        }

        Some(RAST.MethodBody(ISZ(RAST.BodyItemST(exp))))
      case _ => None()
    }

    val id: String = s"${m.method.sig.id.value}${if (isLibraryMethod && inVerus) "_spec" else ""}"

    return RAST.FnImpl(
      comments = ISZ(),
      attributes = ISZ(),
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString(id),
        fnDecl = RAST.FnDecl(
          inputs = inputs,
          outputs = RAST.FnRetTyImpl(RAST.TyPath(ISZ(r(m.method.sig.returnType.asInstanceOf[SAST.Type.Named]).qualifiedRustNameS), None()))
        ),
        verusHeader =
          if (inVerus) Some(FnVerusHeader(isOpen = T, isSpec = T))
          else None(),
        fnHeader = RAST.FnHeader(F), generics = None()),
      contract = contractOpt,
      body = optBody,
      meta = ISZ())
  }
}
