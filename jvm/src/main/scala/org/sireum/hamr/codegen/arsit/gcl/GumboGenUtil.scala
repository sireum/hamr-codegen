// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.util.GclUtil
import org.sireum.lang.{ast => AST}

object GumboGenUtil {

  @record class LitImportResolver() extends org.sireum.hamr.ir.MTransformer {
    var imports: ISZ[String] = ISZ()

    override def post_langastExpStringInterpolate(o: AST.Exp.StringInterpolate): MOption[AST.Exp] = {
      GclUtil.interpolatorLookup.get(o.prefix) match {
        case Some(e) => imports = imports :+ e
        case _ =>
      }
      return MNone()
    }
  }

  def resolveLitInterpolateImports(exp: AST.Exp): ISZ[String] = {
    val r = LitImportResolver()
    r.transform_langastExp(exp)
    return r.imports
  }
}
