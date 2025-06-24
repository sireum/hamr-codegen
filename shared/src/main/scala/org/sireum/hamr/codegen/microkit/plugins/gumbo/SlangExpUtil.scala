// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{Store, TypeIdPath}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, GclSymbolTable, SymTableKey}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.lang.{ast => SAST}
import org.sireum.lang.ast.Exp
import org.sireum.message.{Position, Reporter}

object SlangExpUtil {

  object BinaryOpCust {

    val CondImplLeft: String = "<=="

    val BiImplication: String = "<==>"
  }

  @strictpure def toKey(e: SAST.Exp): SymTableKey = SymTableKey(e, e.fullPosOpt)

  @pure def getRexp(exp: SAST.Exp, gclSymbolTable: GclSymbolTable): SAST.Exp = {
    return gclSymbolTable.rexprs.get(toKey(exp)).get
  }

  @pure def rewriteExp(rexp: Exp,
                       context: AadlComponent,

                       inRequires: B,
                       inVerus: B,

                       tp: CRustTypeProvider,
                       aadlTypes: AadlTypes,
                       store: Store,
                       reporter: Reporter): ST = {
    return rewriteExpH(rexp, context, Map.empty, inRequires, inVerus, tp, aadlTypes, store, reporter)
  }

  @pure def rewriteExpH(rexp: Exp,
                        context: AadlComponent,

                        substitutions: Map[String, String],

                        inRequires: B,
                        inVerus: B,

                        tp: CRustTypeProvider,
                        aadlTypes: AadlTypes,
                        store: Store,
                        reporter: Reporter): ST = {
    return rewriteExpHL(rexp, context, substitutions, inRequires, inVerus, F, F, tp, aadlTypes, store, reporter)
  }

  @pure def rewriteExpHL(rexp: Exp,
                         context: AadlComponent,

                         substitutions: Map[String, String],

                         inRequires: B,
                         inVerus: B,

                         alwaysOneLine: B, // don't add newlines, useful when testing
                         isTesting: B, // true when invoking from testing context

                         tp: CRustTypeProvider,
                         aadlTypes: AadlTypes,
                         store: Store,
                         reporter: Reporter): ST = {

    var optCastType: Option[TypeIdPath] = None()
    @pure def pushCast(t: TypeIdPath): Unit = {
      if (optCastType.isEmpty) {
        optCastType = Some(t)
      } else {
        assert(optCastType.get == t)
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

            // TODO: verus also has u128 and s128 which slang doesn't

            // TODO: how to handle Slang's finer grained s1"0", s2"0", etc.

            case x => reporter.error(d.posOpt, MicrokitCodegen.toolName,
              s"There is not a direct translation of the Slang interpolate $x for Rust/Verus")
            return st"TODO"
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
            case Some(m: SAST.Typed.Method) =>
              if (m.owner == ISZ("org", "sireum", "IS")) {
                if (m.name == "size") {
                  "."
                } else {
                  halt(s"TODO: ${m.name}")
                }
              } else {
                halt(s"TODO: ${m.owner}")
              }
            case Some(o: SAST.Typed.Object) =>
              o.owner match {
                case ISZ("org", "sireum", "conversions") =>
                  "NA"
                case x =>
                  halt(s"Need to handle ${exp.prettyST.render}")
              }

            case Some(x) =>
              halt(s"Only expecting Typed.Name, Typed.Enum but found: $x")
            case _ if sep.nonEmpty => sep.get
            case _ =>
              halt(s"Infeasible: this should have been typed by now $exp")
          }

            exp.id.value match {
              case "get" =>
                return st"${receiverOptST(exp.receiverOpt, sepS)}unwrap()"
              case "isEmpty" =>
                return st"${receiverOptST(exp.receiverOpt, sepS)}is_none()"
              case "nonEmpty" =>
                return st"${receiverOptST(exp.receiverOpt, sepS)}is_some()"
              case "size" =>
                return st"${receiverOptST(exp.receiverOpt, sepS)}len()"
              case _ =>
                return st"${receiverOptST(exp.receiverOpt, sepS)}${exp.id.value}"
            }

        case exp: Exp.Binary =>
          val (leftOpOpt, leftPosOpt): (Option[String], Option[Position]) = exp.left match {
            case left: Exp.Binary => (Some(left.op), left.posOpt)
            case _ => (None(), None())
          }
          val (rightOpOpt, rightPosOpt): (Option[String], Option[Position]) = exp.right match {
            case right: Exp.Binary => (Some(right.op), right.posOpt)
            case _ => (None(), None())
          }

          return BinaryPrettyST(
            slangParentOp = exp.op, parentPos = exp.posOpt,
            left = nestedRewriteExp(exp.left, sep), leftSlangOp = leftOpOpt, isLeftIf = exp.left.isInstanceOf[Exp.If], leftPos = leftPosOpt,
            right = nestedRewriteExp(exp.right, sep), rightSlangOp = rightOpOpt, isRightIf = exp.right.isInstanceOf[Exp.If], rightPos = rightPosOpt)

        case exp: Exp.Invoke =>
          GclResolver.getIndexingTypeFingerprints(store).get(exp.ident.id.value) match {
            case Some(indexingType) =>
              // bounded array index. The Z literals are wrapped in calls to the indexing type's fingerprint method
              // for Slang.  For Rust, use the raw Z literal to index into the array
              assert (exp.args.size == 1 && exp.args(0).typedOpt.get == SAST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ()))
              return nestedRewriteExp(exp.args(0), None())
            case _ =>
              val args: ISZ[ST] = for(arg <- exp.args) yield nestedRewriteExp(arg, None())

              if (isUnitConversionOperation(exp)) {
                assert(args.size == 1)

                // unit type conversion (e.g. conversions.U8.toU16(v))

                //val fromType = getUnitConversionFromType(exp)
                val toType: SAST.Typed.Name = getUnitConversionToType(exp).get
                val aadlType = MicrokitTypeUtil.getAadlTypeFromSlangTypeH(toType, aadlTypes)
                val np = tp.getTypeNameProvider(aadlType)

                pushCast(np.qualifiedRustNameS)

                return st"((${args(0)}) as ${(np.qualifiedRustNameS, "::")})"

              } else {

                val receiverOpt: Option[ST] =
                  exp.receiverOpt match {
                    case Some(ro) if isThreadSingleton(ro) =>
                      // drop the fully qualified reference to the Slang singleton object
                      None()
                    case Some(ro) => Some(nestedRewriteExp(ro, None()))
                    case  _ => None()
                  }

                val fname: ST =
                  if (exp.ident.id.value == "apply") {
                  assert(receiverOpt.nonEmpty, "What is being applied?")
                  receiverOpt.get
                } else {
                  st"${if( receiverOpt.nonEmpty) st"${receiverOpt.get}." else st""}${exp.ident.id.value}"
                }

                exp.attr.resOpt match {
                  case Some(m: SAST.ResolvedInfo.Method) if m.owner == ISZ("org", "sireum") && m.id == "IS" =>
                    if (exp.ident.id.value == "IS") {
                      // array construction
                      m.tpeOpt.get.ret.asInstanceOf[SAST.Typed.Name].args(0).asInstanceOf[SAST.Typed.Name].ids match {
                        case ISZ("org", "sireum", "Z") =>
                          // unbounded array construction
                          halt("Need to handle unbounded array constructions")
                        case ids =>
                          // bounded array construction

                          return st"[${(args, ", ")}]"
                      }
                    } else {
                      // array indexing expression
                      return st"$fname[${(args, ", ")}]"
                    }
                  case _ =>
                    // normal method invocation
                    return st"${if (inVerus) "Self::" else ""}$fname(${(args, ",")})"
                }
              }
          }

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
        case exp: Exp.If =>
          return st"""if (${nestedRewriteExp(exp.cond, None())}) {
                     |  ${nestedRewriteExp(exp.thenExp, None())}
                     |} else {
                     |  ${nestedRewriteExp(exp.elseExp, None())}
                     |}"""
        case exp: Exp.LitZ => return exp.prettyST
        case exp: Exp.LitB => return if (exp.value) st"true" else st"false"

        case exp: Exp.Unary =>
          val paren: B = exp.exp match {
            case _: Exp.Ident => F
            // TODO rust primitives?
            case _ => T
          }
          val op = convertUnaryOp(exp.op)
          val rrexp = nestedRewriteExp(exp.exp, sep)
          return if (paren) st"$op($rrexp)" else st"$op$rrexp"

        case exp: Exp.QuantRange =>
          assert (exp.fun.params.size == 1 && exp.fun.params(0).idOpt.nonEmpty, "only expecting a single named quantified variable")
          assert (exp.fun.exp.isInstanceOf[SAST.Stmt.Expr], s"Unexpected quantified expression: ${exp.fun.exp.prettyST.render}")

          val param = exp.fun.params(0).idOpt.get.value

          val lo = nestedRewriteExp(exp.lo, None())
          val hi = st"${nestedRewriteExp(exp.hi, None())}${if (exp.hiExact) "" else " - 1"}"

          val body = nestedRewriteExp(exp.fun.exp.asInstanceOf[SAST.Stmt.Expr].exp, None())

          if (inVerus) {
            val quantType: String = if (exp.isForall) "forall" else "exists"

            val range = st"$lo <= $param <= $hi"

            return st"$quantType|$param:int| $range && $body"
          } else {
            val b: ST =
              if (exp.isForall)
                st"""if not($body) {
                    |  return false;
                    |}
                    |"""
              else
                st"""if $body {
                    |  return true;
                    |}"""

            return st"""for $param in $lo .. $hi {
                       |  $b
                       |}
                       |return ${if (exp.isForall) "true" else "false"};"""
          }


        case exp: Exp.InvokeNamed => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Old => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.Result => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitC => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitF32 => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitF64 => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitR => halt(s"$exp : ${exp.posOpt}")
        case exp: Exp.LitString => halt(s"$exp : ${exp.posOpt}")


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
    } // end nestedRewriteExp

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

    @pure def isThreadSingleton(e: Exp): B = {
      e match {
        case Exp.Select(None(), id, _) =>
          val classifier = StringUtil.sanitizeName(context.classifier(context.classifier.lastIndex))
          val path = st"${(ops.ISZOps(context.path).drop(1), "_")}".render

          return s"${classifier}_$path" == id.value
        case _ => return F
      }
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

    @pure def shouldParenthesize(slangParentOp: String, parentPosOpt: Option[Position],
                                 slangChildOp: String, childPosOpt: Option[Position], isRightChild: B): B = {
      val slangParentPrecedence = Exp.BinaryOp.precendenceLevel(slangParentOp)
      val verusRustParentOp = convertBinaryOp(inVerus, slangParentOp, parentPosOpt)
      val verusRustParentPrecedence = rustPrecendenceLevel(verusRustParentOp)

      val slangChildPrecedence = Exp.BinaryOp.precendenceLevel(slangChildOp)
      val rustChildOp = convertBinaryOp(F, slangChildOp, childPosOpt)
      val verusRustChildOp = convertBinaryOp(inVerus, slangChildOp, childPosOpt)
      val verusRustChildPrecedence = rustPrecendenceLevel(verusRustChildOp)

      // rust requires comparison expressions to be explicitly parenthesized
      rustChildOp match {
        case Exp.BinaryOp.Eq => return T
        case Exp.BinaryOp.Ne => return T
        case Exp.BinaryOp.Le => return T
        case Exp.BinaryOp.Lt => return T
        case Exp.BinaryOp.Ge => return T
        case Exp.BinaryOp.Gt => return T
        case BinaryOpCust.BiImplication => return T
        case "!==" => return T
        case _ =>
      }

      // Verus doesn't support & and | so those are converted to && and ||. The non-short
      // circuit version have higher precedence than their short circuit counter-parts in
      // rust so we need to parenthesize the child
      (rustChildOp, verusRustChildOp) match {
        case (string"&", string"&&") => return T
        case (string"|", string"||") => return T
        case _ =>
      }

      if (slangChildPrecedence >= slangParentPrecedence) {
        // in Slang, either
        //   a) the child's op either binds looser than the parent op
        //      so would need parens in Slang, or
        //   b) the child and parent ops have the same precedence
        //      in which case we only potentially need parens in Slang
        //      due to right associativity

        if (verusRustChildPrecedence > verusRustParentPrecedence) {
          // in Rust the child's Rust op also binds looser than the
          // parent's Rust op so need parens in Rust
          return T
        } else if (verusRustChildPrecedence == verusRustParentPrecedence) {
          // same precedence in Rust so may need parens due to
          // right associativity
          val isParentRightAssoc: B = verusRustParentOp match {
            case Exp.BinaryOp.Imply => T
            case Exp.BinaryOp.CondImply => T
            case _ => F
          }
          return isRightChild != isParentRightAssoc
        } else {
          // in Rust the child's Rust op binds tighter than the
          // parent Rust op so don't need parens
        }
      } else {
        // in Slang the child's op binds tighter than the parent so
        // wouldn't need parens in Slang
      }

      return F
    }

    @pure def convertBinaryOp(convertingToVerus: B, op: String, posOpt: Option[Position]) : String = {
      op match {
        case Exp.BinaryOp.Add => return op // +
        case Exp.BinaryOp.Sub => return op // -
        case Exp.BinaryOp.Mul => return op // *
        case Exp.BinaryOp.Div => return op // /
        case Exp.BinaryOp.Rem => return op // %
        case Exp.BinaryOp.Eq => return op // ==
        case Exp.BinaryOp.Ne => return op // !=
        case Exp.BinaryOp.Shl => return op // <<
        case Exp.BinaryOp.Shr => return op // >>
        case Exp.BinaryOp.Lt => return op // <
        case Exp.BinaryOp.Le => return op // >=
        case Exp.BinaryOp.Gt => return op // >
        case Exp.BinaryOp.Ge => return op // >=
        case Exp.BinaryOp.Xor => return op // |^ in Slang, ^ in Rust/Verus
        case Exp.BinaryOp.CondAnd => return op // &&
        case Exp.BinaryOp.CondOr => return op // ||

        case Exp.BinaryOp.And => // &
          return (
            if (convertingToVerus) Exp.BinaryOp.CondAnd
            else op)

        case Exp.BinaryOp.Or => // |
          return (
            if (convertingToVerus) Exp.BinaryOp.CondOr
            else op)

        case Exp.BinaryOp.Imply => // __>:
          return (
            if (convertingToVerus) Exp.BinaryOp.CondImply
            else op)

        case Exp.BinaryOp.CondImply => return op // ___>:

        case "->:" => halt(s"Not expecting '->:', it should have been converted to ${Exp.BinaryOp.Imply} at the AIR level")
        case "-->:" => halt(s"Not expecting '-->:', it should have been converted to ${Exp.BinaryOp.CondImply} at the AIR level")

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

    // https://doc.rust-lang.org/reference/expressions.html#r-expr.precedence
    // https://verus-lang.github.io/verus/guide/spec-operator-precedence.html
    def rustPrecendenceLevel(op: String): Z = {
      op match {
        //                                               Verus Op | Associativity
        case Exp.BinaryOp.Mul => return 3 //             *          left
        case Exp.BinaryOp.Div => return 3 //             /          left
        case Exp.BinaryOp.Rem => return 3 //             %          left

        case Exp.BinaryOp.Add => return 4 //             +          left
        case Exp.BinaryOp.Sub => return 4 //             -          left

        case Exp.BinaryOp.Shl => return 5 //             <<         left
        case Exp.BinaryOp.Shr => return 5 //             >>         left

        case Exp.BinaryOp.And => return 6 //             &          left

        case Exp.BinaryOp.Xor => return 7 //             ^          left

        case Exp.BinaryOp.Or => return 8 //              |          left

        case Exp.BinaryOp.Eq => return 9 //              ==         requires parens
        case Exp.BinaryOp.Ne => return 9 //              !=         requires parens
        case Exp.BinaryOp.Lt => return 9 //              <          requires parens
        case Exp.BinaryOp.Le => return 9 //              <=         requires parens
        case Exp.BinaryOp.Gt => return 9 //              >          requires parens
        case Exp.BinaryOp.Ge => return 9 //              >=         requires parens

        case Exp.BinaryOp.CondAnd => return 10 //        &&         left

        case Exp.BinaryOp.CondOr => return 11 //         ||         left

        case Exp.BinaryOp.CondImply => return 12 //      ==>        right
        case Exp.BinaryOp.Imply => return 12 //          NA         right

        case BinaryOpCust.CondImplLeft => return 13 //   <==        left

        case BinaryOpCust.BiImplication => return 14 //  <==>       requires parens

        case string"-->:" => halt(s"Not expecting '-->:', it should have been converted to ${Exp.BinaryOp.CondImply} at the AIR level")
        case string"->:" => halt(s"Not expecting '->:', it should have been converted to ${Exp.BinaryOp.Imply} at the AIR level")

        case _ => halt(s"Infeasible binary operator for GUMBO: $op")
      }
    }

    @pure def BinaryPrettyST(slangParentOp: String, parentPos: Option[Position],
                             left: ST, leftSlangOp: Option[String], isLeftIf: B, leftPos: Option[Position],
                             right: ST, rightSlangOp: Option[String], isRightIf: B, rightPos: Option[Position]): ST = {
      val slangParentPrecedence = Exp.BinaryOp.precendenceLevel(slangParentOp)

      var singleLine = T
      val leftST: ST = leftSlangOp match {
        case Some(leftOp) =>

          if (slangParentPrecedence > 6 || slangParentOp == Exp.BinaryOp.CondImply) {
            singleLine = F
          }

          if (shouldParenthesize(slangParentOp, parentPos, leftOp, leftPos, F)) st"($left)"
          else left
        case _ if isLeftIf =>
          singleLine = F
          st"($left)"
        case _ =>
          left
      }
      val rightST: ST = rightSlangOp match {
        case Some(rightOp) =>
          if (slangParentPrecedence > 6 || slangParentOp == Exp.BinaryOp.CondImply) {
            singleLine = F
          }

          if (shouldParenthesize(slangParentOp, parentPos, rightOp, rightPos, T)) st"($right)"
          else right
        case _ if isRightIf =>
          singleLine = F
          st"($right)"
        case _ =>
          right
      }

      // now convert logical operators if in verus (e.g. & becomes &&)
      val verusRustParentOp: String = convertBinaryOp(inVerus, slangParentOp, parentPos) match {
        case Exp.BinaryOp.Xor => "^"
        case Exp.BinaryOp.CondImply => "==>"
        case op => op
      }

      if ((!inVerus || isTesting) && (verusRustParentOp == "==>" || verusRustParentOp == Exp.BinaryOp.Imply)) {
        val functionName: String =
          if (verusRustParentOp == "==>") "implies!"
          else "impliesL!"
        return (
          st"""$functionName(
              |  $leftST,
              |  $rightST)""")
      } else {
        return (
          if (alwaysOneLine || singleLine) st"$leftST $verusRustParentOp $rightST"
          else st"""$leftST $verusRustParentOp
                   |  $rightST""")
      }
    }

    val ret = nestedRewriteExp(rexp, None())

    if (inVerus && optCastType.nonEmpty) {
      return st"($ret) as ${(optCastType.get, "::")}"
    } else {
      return ret
    }
  }

  @pure def isUnitConversionOperation(e: Exp.Invoke): B = {
    return getUnitConversionFromType(e).nonEmpty
  }

  @pure def getUnitConversionToType(e: Exp.Invoke): Option[SAST.Typed.Name] = {
    assert(isUnitConversionOperation(e))
    e.typedOpt match {
      case Some(t: SAST.Typed.Name) => return Some(t)
      case x => halt(s"Unexpected: $x")
    }
  }

  @pure def getUnitConversionFromType(e: Exp.Invoke): Option[SAST.Typed.Name] = {
    e.receiverOpt match {
      case Some(Exp.Select(Some(Exp.Select(None(), SAST.Id("conversions"), _)), fromType, _)) =>
        assert(e.args.size == 1)
        e.args(0).typedOpt match {
          case Some(t: SAST.Typed.Name) => return Some(t)
          case x => halt(s"Unexpected: $x")
        }
      case  _ => return None()
    }
  }
}
