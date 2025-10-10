// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, GclAnnexClauseInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.rust.FnVerusHeader
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir._
import org.sireum.lang.ast.Stmt.Return
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboRustUtil {

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
                             context: AadlComponent,
                             isAssumeRequires: B,

                             types: AadlTypes,
                             tp: CRustTypeProvider,
                             gclSymbolTable: GclSymbolTable,
                             store: Store,
                             reporter: Reporter): RAST.Expr = {
    return processGumboSpecH(spec, context, Map.empty, isAssumeRequires, types, tp, gclSymbolTable, store, reporter)
  }

  @pure def processGumboSpecH(spec: GclSpec,
                              context: AadlComponent,
                              substitutions: Map[String, String],
                              isAssumeRequires: B,

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
                             context: AadlComponent,

                             aadlTypes: AadlTypes,
                             tp: CRustTypeProvider,
                             gclSymbolTable: GclSymbolTable,
                             store: Store,
                             reporter: Reporter): RAST.Expr = {
    val requires: Option[ST] =
      if (c.assumes.nonEmpty)
        Some(SlangExpUtil.rewriteExpH(
          rexp = SlangExpUtil.getRexp(c.assumes.get, gclSymbolTable),
          context = context,
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
        context = context,
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
                         context: AadlComponent,

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
            SlangExpUtil.rewriteExp(
              rexp = SlangExpUtil.getRexp(r.expOpt.get, gclSymbolTable),
              context = context,
              inRequires = F,
              inVerus = inVerus,
              tp = tp,
              aadlTypes = aadlTypes,
              store = store,
              reporter = reporter)
          case x =>
            halt(s"Currently expecting GUMBO methods to only have return statements: ${x.prettyST.render}")
        }

        Some(RAST.MethodBody(ISZ(RAST.BodyItemST(exp))))
      case _ => None()
    }

    return RAST.FnImpl(
      comments = ISZ(),
      attributes = ISZ(),
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString(m.method.sig.id.value),
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
