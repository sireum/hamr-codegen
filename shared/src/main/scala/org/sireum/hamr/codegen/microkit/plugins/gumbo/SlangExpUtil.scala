// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.codegen.common.symbols.{GclSymbolTable, SymTableKey}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.lang.{ast => SAST}
import org.sireum.lang.ast.Exp
import org.sireum.message.{Position, Reporter}

object SlangExpUtil {

  object BinaryOpCust {
    val CondImpl: String = "uif_short_circuit_implication"
    val Impl: String = "uif_logical_implication"
  }
  @strictpure def toKey(e: SAST.Exp): SymTableKey = SymTableKey(e, e.fullPosOpt)

  @pure def getRexp(exp: SAST.Exp, gclSymbolTable: GclSymbolTable): SAST.Exp = {
    return gclSymbolTable.rexprs.get(toKey(exp)).get
  }

  @pure def rewriteExp(rexp: Exp,
                       inRequires: B, inVerus: B,
                       reporter: Reporter): ST = {
    return rewriteExpH(rexp, Map.empty, inRequires, inVerus, reporter)
  }

  @pure def rewriteExpH(rexp: Exp, substitutions: Map[String, String],
                        inRequires: B,
                        inVerus: B,
                        reporter: Reporter): ST = {

    @pure def receiverOptST(receiverOpt: Option[Exp], sep: String): Option[ST] = {
      if (receiverOpt.isEmpty) {
        return None()
      }
      receiverOpt.get match {
        case exp: Exp.This => halt("Not expecting 'this' in a gumbo contract")
        case exp =>
          return (
            if (Exp.shouldParenthesize(exp)) Some(st"x(${nestedRewriteExp(exp, Some(sep))})$sep")
            else Some(st"${nestedRewriteExp(exp, Some(sep))}$sep"))
      }
    }

    @pure def nestedRewriteExp(d: Exp, sep: Option[String]): ST = {
      val dd: Exp = d
      dd match {
        case exp: Exp.StringInterpolate =>
          exp.prefix match {
            case "u8" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}u8"
            case "u16" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}u16"
            case "u32" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}u32"
            case "u64" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}u64"

            case "s8" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}i8"
            case "s16" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}i16"
            case "s32" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}i32"
            case "s64" => return st"${(unquoteLits(exp.lits), "_INFEASIBLE")}i64"

            case x => halt(s"Need to handle $x")
          }
        case exp: Exp.Tuple => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Select =>
          val sepS: String = exp.typedOpt match {
            case Some(s: SAST.Typed.Name) =>
              exp.resOpt match {
                case Some(e: SAST.ResolvedInfo.EnumElement) => "::"
                case _ => "."
              }
            case Some(s: SAST.Typed.Enum) => "::"
            case Some(x) => halt(s"Only expecting Typed.Name, Typed.Enum: $x")
            case _ if sep.nonEmpty => sep.get
            case _ => halt(s"Infeasible: this should have been typed by now $exp")
          }
          exp.id.value match {
            case "get" =>
              return st"${receiverOptST(exp.receiverOpt, sepS)}unwrap()"
            case "isEmpty" =>
              return st"${receiverOptST(exp.receiverOpt, sepS)}is_none()"
            case "nonEmpty" =>
              return st"${receiverOptST(exp.receiverOpt, sepS)}is_some()"
            case _ =>
              return st"${receiverOptST(exp.receiverOpt, sepS)}${exp.id.value}"
          }
        case exp: Exp.Old => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Binary =>
          val leftOpOpt: Option[String] = exp.left match {
            case left: Exp.Binary => Some(left.op)
            case _ => None()
          }
          val rightOpOpt: Option[String] = exp.right match {
            case right: Exp.Binary => Some(right.op)
            case _ => None()
          }
          val isRightAssoc = exp.op == Exp.BinaryOp.CondImply || exp.op == Exp.BinaryOp.Imply

          return BinaryPrettyST(inVerus,
            convertBinaryOp(exp.op, inVerus, exp.posOpt, reporter), isRightAssoc,
            nestedRewriteExp(exp.left, sep), leftOpOpt, exp.left.isInstanceOf[Exp.If],
            nestedRewriteExp(exp.right, sep), rightOpOpt, exp.right.isInstanceOf[Exp.If])

        case exp: Exp.Invoke =>
          reporter.error(exp.posOpt, MicrokitCodegen.toolName,
            "Gumbo definition invocations are not currently supported for Rust/Verus")
          return st"??"
        case exp: Exp.InvokeNamed => halt(s"$exp : ${exp.posOpt}")

        case exp: Exp.Ident =>
          exp.resOpt match {
            case Some(x: SAST.ResolvedInfo.Var) =>
              if (!inVerus) {
                return st"${exp.id.prettyST}"
              } else {
                if (inRequires) {
                  return st"old(self).${exp.id.prettyST}"
                }
                else {
                  substitutions.get(exp.id.value) match {
                    case Some(sub) => return st"$sub"
                    case _ => return st"self.${exp.id.prettyST}"
                  }
                }
              }
            case _ =>
              if (exp.id.value == "api" && inRequires) {
                return st"old(${exp.id.prettyST})"
              } else {
                return exp.id.prettyST
              }
          }
        case exp: Exp.Input =>
          exp.exp match {
            case id: Exp.Ident =>
              return st"old(self).${exp.exp.prettyST}"
            case _ =>
              halt(s"Only expecting Ident to be wrapped in In(): ${exp.exp}")
          }
        case exp: Exp.Result => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.If => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitC => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitZ => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitF32 => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitF64 => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitR => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitB => return if (exp.value) st"true" else st"false"
        case exp: Exp.LitString => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Unary =>
          val paren: B = exp.exp match {
            case _: Exp.Ident => F
            // TODO rust primitives?
            case _ => T
          }
          val op = convertUnaryOp(exp.op)
          val rrexp = nestedRewriteExp(exp.exp, sep)
          return if (paren) st"$op($rrexp)" else st"$op$rrexp"

        // currently not expecting the following expressions in gumbo
        case exp: Exp.At => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.ForYield => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Super => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Quant => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.AssumeAgree => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LoopIndex => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Labeled => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.InfoFlowInvariant => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.StateSeq => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.This => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Eta => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Sym => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.StrictPureBlock => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.RS => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Fun => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.AssertAgree => halt(s"$exp : ${exp.posOpt}")
        case exp: SAST.ProofAst.StepId => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.TypeCond => halt(s"$exp : ${exp.posOpt}")
      }
    }

    return nestedRewriteExp(rexp, None())
  }

  @pure def unquoteLit(lit: Exp.LitString): String = {
    return ops.StringOps(lit.value).replaceAllLiterally("\"", "")
  }

  @pure def unquoteLits(lits: ISZ[Exp.LitString]): ISZ[String] = {
    return for(l <- lits) yield unquoteLit(l)
  }

  @pure def convertUnaryOp(op: Exp.UnaryOp.Type): String = {
    op match {
      case Exp.UnaryOp.Not => return "!"
      case Exp.UnaryOp.Minus => return "-"

      case Exp.UnaryOp.Plus => halt(s"what is the rust equiv of $op")
      case Exp.UnaryOp.Complement => halt(s"what is the rust equiv of $op")
    }
  }

  @pure def convertBinaryOp(op: String, inVerus: B, posOpt: Option[Position], reporter: Reporter) : String = {
    op match {
      case Exp.BinaryOp.Add => return op
      case Exp.BinaryOp.Sub => return op
      case Exp.BinaryOp.Mul => return op
      case Exp.BinaryOp.Div => return op
      case Exp.BinaryOp.Rem => return op
      case Exp.BinaryOp.Eq => return op
      case Exp.BinaryOp.Ne => return op
      case Exp.BinaryOp.Shl => return op
      case Exp.BinaryOp.Shr => return op
      case Exp.BinaryOp.Lt => return op
      case Exp.BinaryOp.Le => return op
      case Exp.BinaryOp.Gt => return op
      case Exp.BinaryOp.Ge => return op
      case Exp.BinaryOp.Xor => return "^"

      case Exp.BinaryOp.And =>
        return (
          if (inVerus) "&&"
          else Exp.BinaryOp.And)

      case Exp.BinaryOp.CondAnd => return op

      case Exp.BinaryOp.Or =>
        return (
          if (inVerus) "||"
          else Exp.BinaryOp.Or)

      case Exp.BinaryOp.CondOr => return op

      case Exp.BinaryOp.Imply =>
        return (
          if (inVerus) "==>"
          else SlangExpUtil.BinaryOpCust.Impl)

      case "->:" =>
        return (
          if (inVerus) "==>"
          else SlangExpUtil.BinaryOpCust.Impl)

      case Exp.BinaryOp.CondImply =>
        return (
          if (inVerus)  "==>"
          else BinaryOpCust.CondImpl)

      case "-->:" =>
        return (
          if (inVerus) "==>"
          else BinaryOpCust.CondImpl)

      // will the following ever appear in a gumbo expression?
      case Exp.BinaryOp.Equiv => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.EquivUni => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.Inequiv => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.InequivUni => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.FpEq => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.FpNe => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.Ushr => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.Append => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.Prepend => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.AppendAll => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.RemoveAll => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.MapsTo => halt(s"what is the rust equiv of $op")
      case Exp.BinaryOp.Arrow => halt(s"what is the rust equiv of $op")

      case _ => halt(s"Wasn't expecting $op")
    }
  }

  @pure def isPrimitive(path: TypeIdPath): B = {
    path match {
      case ISZ("org", "sireum", _) => return T
      case _ => return F
    }
  }

  @pure def shouldParenthesize(level: Z, cop: String, isRight: B, isRightAssoc: B): B = {
    val c = precendenceLevel(cop)
    if (c > level) {
      return T
    }
    if (c == level) {
      return isRight != isRightAssoc
    }
    return F
  }

  def precendenceLevel(op: String): Z = {
    val c = conversions.String.toCis(op)(0)
    c match {
      case '*' => return 2
      case '/' => return 2
      case '%' => return 2
      case '+' => return 3
      case '-' => return 3
      case ':' => return 4
      case '=' => return 5
      case '!' => return 5
      case '<' => return 6
      case '>' => return 6
      case '&' => return 7
      case '^' => return 8
      case '|' => return 9
      case '$' => return 10
      case '_' => return 10
      case _ =>
        ops.COps(c).category match {
          case ops.COps.Category.Ll => return 10
          case ops.COps.Category.Lu => return 10
          case ops.COps.Category.Lt => return 10
          case ops.COps.Category.Lo => return 10
          case ops.COps.Category.Nl => return 10
          case _ => return 1
        }
    }
  }

  @pure def BinaryPrettyST(inVerus: B,
                           op: String, isOpRightAssoc: B,
                           left: ST, leftOpOpt: Option[String], isLeftIf: B,
                           right: ST, rightOpOpt: Option[String], isRightIf: B): ST = {
    val l = precendenceLevel(op)
    var singleLine = T
    val leftST: ST = leftOpOpt match {
      case Some(leftOp) =>
        if (l > 6 || op == "___>:" || op == "-->:" || op == BinaryOpCust.CondImpl) {
          singleLine = F
        }
        if (shouldParenthesize(l, leftOp, F, isOpRightAssoc)) st"($left)" else left
      case _ if isLeftIf =>
        singleLine = F
        st"($left)"
      case _ =>
        left
    }
    val rightST: ST = rightOpOpt match {
      case Some(rightOp) =>
        if (l > 6 || op == "___>:" || op == "-->:" || op == BinaryOpCust.CondImpl) {
          singleLine = F
        }
        if (shouldParenthesize(l, rightOp, T, isOpRightAssoc)) st"($right)" else right
      case _ if isRightIf =>
        singleLine = F
        st"($right)"
      case _ =>
        right
    }
    if (!inVerus && (op == BinaryOpCust.Impl || op == BinaryOpCust.CondImpl)) {
      val functionName: String = if (op == BinaryOpCust.Impl) "impliesL" else "implies"
      return (
        st"""$functionName(
            |  $leftST,
            |  $rightST)""")
    } else {
      return (
        if (singleLine) st"$leftST $op $rightST"
        else st"""$leftST $op
                 |  $rightST""")
    }
  }
}
