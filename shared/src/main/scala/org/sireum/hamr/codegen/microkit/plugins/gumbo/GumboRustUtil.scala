// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols.{GclAnnexClauseInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.plugins.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{GclAssume, GclCaseStatement, GclGuarantee, GclInvariant, GclSpec, GclStateVar}
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

  @pure def processGumboSpec(spec: GclSpec, isAssumeRequires: B, types: AadlTypes, gclSymbolTable: GclSymbolTable, reporter: Reporter): RAST.Expr = {
    return processGumboSpecH(spec, Map.empty, isAssumeRequires, types, gclSymbolTable, reporter)
  }

  @pure def processGumboSpecH(spec: GclSpec, substitutions: Map[String, String],
                              isAssumeRequires: B, types: AadlTypes, gclSymbolTable: GclSymbolTable, reporter: Reporter): RAST.Expr = {
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
        inRequires = isAssumeRequires,
        inVerus = T,
        substitutions = substitutions, reporter = reporter)
    return RAST.ExprST(
      st"""// $typ ${spec.id}
          |${GumboRustUtil.processDescriptor(spec.descriptor, "//   ")}
          |$verusExp""")
  }

  @pure def processGumboCase(c: GclCaseStatement, types: AadlTypes, gclSymbolTable: GclSymbolTable, reporter: Reporter): RAST.Expr = {
    val requires = SlangExpUtil.rewriteExp(
      rexp = SlangExpUtil.getRexp(c.assumes, gclSymbolTable),
      inRequires = T,
      inVerus = T,
      reporter = reporter)
    val ensures = SlangExpUtil.rewriteExp(
      rexp = SlangExpUtil.getRexp(c.guarantees, gclSymbolTable),
      inRequires = F,
      inVerus = T,
      reporter = reporter)
    return RAST.ExprST(
      st"""// case ${c.id}
          |${GumboRustUtil.processDescriptor(c.descriptor, "//   ")}
          |($requires) ==>
          |  ($ensures)""")
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
}
