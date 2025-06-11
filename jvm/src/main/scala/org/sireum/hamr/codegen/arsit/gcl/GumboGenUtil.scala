// #Sireum
package org.sireum.hamr.codegen.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.codegen.common.util.GclUtil
import org.sireum.lang.{ast => AST}

object GumboGenUtil {


  @pure def convertSelects(exp: Option[AST.Exp]): String = {
    val ret: String = exp match {
      case Some(s: AST.Exp.Select) =>
        if (s.receiverOpt.isEmpty) s.id.value
        else s"${convertSelects(s.receiverOpt)}::${s.id.value}"
      case Some(AST.Exp.Ident(id)) => id.value
      case Some(x) => halt(s"Unexpected Exp.Select form: '${x}''")
      case _ => ""
    }
    return ret
  }

  @pure def convertToSelect(value: ISZ[String]): Option[AST.Exp] = {
    return (
      if (value.isEmpty) None()
      else if (value.size == 1) Some(
        AST.Exp.Ident(id = AST.Id(value = value(0), attr = AST.Attr(None())), attr = AST.ResolvedAttr(None(), None(), None())))
      else Some(AST.Exp.Select(
        receiverOpt = convertToSelect(ops.ISZOps(value).dropRight(1)),
        id = AST.Id(value = value(value.size - 1), attr = AST.Attr(None())),
        targs = ISZ(), attr = AST.ResolvedAttr(None(), None(), None())))
      )
  }

  @record class ImportResolver(basePackageName: String, indexingTypeFingerprints: Map[String, TypeIdPath]) extends org.sireum.hamr.ir.MTransformer {
    var imports: ISZ[String] = ISZ()

    override def post_langastId(o: AST.Id): MOption[AST.Id] = {
      indexingTypeFingerprints.get(o.value) match {
        case Some(v) => imports = imports :+ st"$basePackageName.${(ops.ISZOps(v).dropRight(1), ".")}.${o.value}".render
        case _ =>
      }
      return MNone()
    }

    override def post_langastExpStringInterpolate(o: AST.Exp.StringInterpolate): MOption[AST.Exp] = {
      GclUtil.interpolatorLookup.get(o.prefix) match {
        case Some(e) => imports = imports :+ e
        case _ =>
      }
      return MNone()
    }
  }

  def resolveLitInterpolateImports(exp: AST.Exp, basePackageName: String, indexingTypeFingerprints: Map[String, TypeIdPath]): ISZ[String] = {
    val r = ImportResolver(basePackageName, indexingTypeFingerprints)
    r.transform_langastExp(exp)
    return r.imports
  }
}
