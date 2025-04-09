// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.lang.ast.Exp
import org.sireum.lang.ast.Exp.LitString
import org.sireum.lang.{ast => AST}
import org.sireum.message.{FlatPos, Position, Reporter}
import org.sireum.parser.ParseTree
import org.sireum.parser.ParseTree.BinaryPrecedenceOps
import org.sireum.U32._

object GclUtil {

  var ureporter: Reporter = Reporter.create

  val interpolatorLookup: Map[String, String] = Map.empty[String, String] ++ (ISZ[String](
    "z8", "z16", "z32", "z64",
    "n", "n8", "n16", "n32", "n64",
    "s8", "s16", "s32", "s64"
  ) ++ (for (i <- 1 to 64) yield s"u${i}")).map((s: String) => {
    ((s, s"org.sireum.${ops.StringOps(s).firstToUpper}._"))
  })

  @datatype class BinaryBuilder

  @datatype class BinaryExpPrecedenceOps extends BinaryPrecedenceOps[BinaryBuilder, Exp, Exp] {
    override def messageKind: String = {
      return "BinaryExpPrecedenceOps"
    }

    override def isBinary(e: Exp): B = {
      return e.isInstanceOf[AST.Exp.Binary]
    }

    override def isRightAssoc(op: Exp): B = {
      op match {
        case AST.Exp.LitString(value) =>
          return ops.StringOps(value).endsWith(":")
        case _ =>
          ureporter.error(op.posOpt, messageKind, s"isRightAssoc: Was expecting a LitString holding a binary operator but received: ${op.string}")
          return F
      }
    }

    override def isHigherPrecedence(n1: Z, n2: Z): B = {
      return n1 < n2
    }

    override def lowestPrecedence: Z = {
      return 10
    }

    override def shouldParenthesizeOperands(t: Exp): B = {
      return F
    }

    override def precedence(e: Exp): Option[Z] = {
      e match {
        case AST.Exp.LitString(op) =>
          return Some(AST.Exp.BinaryOp.precendenceLevel(op))
        case _ =>
          return None()
      }
    }

    override def posOpt(e: Exp): Option[Position] = {
      return e.posOpt
    }

    override def parenthesize(builder: BinaryBuilder, e: Exp): Exp = {
      return e
    }

    def mergePos(a: Option[Position], b: Option[Position]): Option[Position] = {
      if (a.isEmpty) {
        return b
      } else if (b.isEmpty) {
        return a
      } else {
        (a, b) match {
          case (Some(afp: FlatPos), Some(bfp: FlatPos)) =>
            if (!(afp.offset32 <= bfp.offset32)) {
              ureporter.error(a, messageKind, s"Offsets must increase but received: ${afp.offset32} vs ${bfp.offset32}")
            }
            val length = (bfp.offset32 - afp.offset32) + bfp.length32
            return Some(
              FlatPos(
                uriOpt = afp.uriOpt,
                beginLine32 = afp.beginLine32,
                beginColumn32 = afp.beginColumn32,
                endLine32 = bfp.endLine32,
                endColumn32 = bfp.endColumn32,
                offset32 = afp.offset32,
                length32 = length)
            )
          case _ =>
            ureporter.error(a, messageKind, "Only handing merging FlatPos")
            return a
        }
      }
    }

    override def binary(builder: BinaryBuilder, left: Exp, op: Exp, right: Exp): Exp = {
      val o: String = op match {
        case AST.Exp.LitString(op) => op
        case _ =>
          ureporter.error(op.posOpt, messageKind, s"binary: Was expecting a LitString holding a binary operator but received $op")
          "???"
      }
      val posOpt = mergePos(left.fullPosOpt, right.fullPosOpt)
      var opPosOpt: Option[Position] =
        if (op.posOpt.nonEmpty) op.posOpt
        else if (left.posOpt.nonEmpty) left.posOpt
        else right.posOpt
      (left.posOpt, right.posOpt) match {
        case (Some(leftPos: FlatPos), Some(rightPos: FlatPos)) if op.posOpt.isEmpty =>
          val length32 = (rightPos.offset32 - u32"1") - (leftPos.offset32 + leftPos.length32 + u32"1");
          opPosOpt = Some(FlatPos(
            uriOpt = leftPos.uriOpt,
            beginLine32 = leftPos.endLine32,
            beginColumn32 = leftPos.endColumn32 + leftPos.length32 + u32"1",
            endLine32 = rightPos.beginLine32,
            endColumn32 = rightPos.beginColumn32 - u32"1",
            offset32 = leftPos.offset32 + leftPos.length32 + u32"1",
            length32 = length32
          ));
        case _ =>
      }
      return AST.Exp.Binary(left, o, right, AST.ResolvedAttr(posOpt = posOpt, resOpt = None(), typedOpt = None()), opPosOpt)
    }

    override def transform(builder: BinaryBuilder, tree: Exp): Exp = {
      return tree
    }

    override def toString(e: Exp): String = {
      e match {
        case LitString(op) => return op
        case _ =>
          ureporter.error(e.posOpt, messageKind, s"toString: Was expecting a LitString holding a binary operator but received ${e.string}")
          return e.string
      }
    }
  }

  def rewriteBinary(exps: ISZ[AST.Exp], reporter: Reporter): AST.Exp = {
    ureporter = Reporter.create

    val e = ParseTree.rewriteBinary[BinaryBuilder, AST.Exp, AST.Exp](
      builder = BinaryBuilder(),
      bp = BinaryExpPrecedenceOps(),
      trees = exps,
      reporter = ureporter
    )

    reporter.reports(ureporter.messages)

    return e
  }
}
