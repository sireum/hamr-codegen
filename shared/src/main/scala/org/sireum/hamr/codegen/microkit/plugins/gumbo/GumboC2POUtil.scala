// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType, BaseType, EnumType, RecordType, SlangType, TypeUtil}
import org.sireum.lang.{ast => SAST}
import org.sireum.lang.symbol.Info

object GumboC2POUtil {

  @enum object C2POType {
    "bool"
    "int"
    "float"
    "enumeration"
    "array"
    "struct"
  }

  // Mirrors C2PO's lexer-reserved identifiers.
  val C2POReservedWords: Set[String] = Set.empty[String] ++ ISZ(
    "STRUCT", "ENUM", "INPUT", "DEFINE", "FTSPEC", "PTSPEC",
    "foreach", "forsome", "forexactly", "foratleast", "foratmost",
    "TAU", "pow", "sqrt", "abs", "xor", "prev",
    "G", "F", "H", "O", "U", "R", "S", "T", "M", "true", "false")

  // C2PO's prev token also matches the beginning of longer identifiers.
  @pure def checkC2POIdentifier(identifier: String): Unit = {
    if (C2POReservedWords.contains(identifier) || ops.StringOps(identifier).startsWith("prev")) {
      halt(s"C2PO identifier '$identifier' conflicts with a reserved word")
    }
  }

  // Checks the C2PO identifiers emitted for an AADL enum.
  @pure def checkC2POEnum(enumType: EnumType): Unit = {
    checkC2POIdentifier(enumType.simpleName)
    for (value <- enumType.values) {
      checkC2POIdentifier(value)
    }
  }

  @datatype class C2POEnum(val name: String,
                           val values: ISZ[String])

  @datatype class C2POArray(val elementType: C2POType.Type,
                            val size: Z)

  @datatype class C2POStructField(val name: String,
                                  val fieldType: C2POType.Type,
                                  val enumTypeOpt: Option[C2POEnum],
                                  val arrayTypeOpt: Option[C2POArray])

  @datatype class C2POStruct(val name: String,
                             val fields: ISZ[C2POStructField])

  // Removes a generated bounded-index conversion, e.g., I8FE679(i) becomes i.
  @pure def getIndexingExpr(exp: SAST.Exp, store: Store): SAST.Exp = {
    exp match {
      case invoke: SAST.Exp.Invoke if GclResolver.getIndexingTypeFingerprints(store).contains(invoke.ident.id.value) =>
        invoke.args match {
          case ISZ(index) => return index
          case _ => halt("Unexpected array indexing expression")
        }
      case _ => return exp
    }
  }

  // Returns the generated C2PO path for a supported array expression.
  @pure def getC2POArrayPath(exp: SAST.Exp): Option[String] = {
    exp match {
      case id: SAST.Exp.Ident => return Some(id.id.value)
      case select: SAST.Exp.Select if select.receiverOpt.nonEmpty =>
        getC2POArrayPath(select.receiverOpt.get) match {
          case Some(receiver) =>
            select.attr.typedOpt match {
              case Some(m: SAST.Typed.Method) if m.owner == SAST.Typed.optionName && m.name == "get" =>
                return Some(receiver)
              case _ =>
                val separator: String = select.receiverOpt.get match {
                  case id: SAST.Exp.Ident if id.id.value == "api" => "_"
                  case _ => "."
                }
                return Some(s"$receiver$separator${select.id.value}")
            }
          case _ => return None()
        }
      case _ => return None()
    }
  }

  // Determines whether two expressions reference the same C2PO array.
  @pure def isSameArray(left: SAST.Exp, right: SAST.Exp): B = {
    (getC2POArrayPath(left), getC2POArrayPath(right)) match {
      case (Some(l), Some(r)) => return l == r
      case _ => return F
    }
  }

  // Evaluates static range bounds and indices, e.g., samples.size - 2 or i + 1.
  @pure def getStaticValue(exp: SAST.Exp,
                           quantifierValues: Map[String, Z],
                           aadlTypes: AadlTypes,
                           store: Store): Option[Z] = {
    @pure def getValue(e: SAST.Exp): Option[Z] = e match {
      case lit: SAST.Exp.LitZ => return Some(lit.value)
      case id: SAST.Exp.Ident =>
        id.resOpt match {
          case Some(local: SAST.ResolvedInfo.LocalVar) => return quantifierValues.get(local.id)
          case _ => return None()
        }
      case binary: SAST.Exp.Binary =>
        (getValue(binary.left), binary.op, getValue(binary.right)) match {
          case (Some(left), SAST.Exp.BinaryOp.Add, Some(right)) => return Some(left + right)
          case (Some(left), SAST.Exp.BinaryOp.Sub, Some(right)) => return Some(left - right)
          case (Some(left), SAST.Exp.BinaryOp.Mul, Some(right)) => return Some(left * right)
          case (Some(left), SAST.Exp.BinaryOp.Div, Some(right)) if right != 0 => return Some(left / right)
          case (Some(left), SAST.Exp.BinaryOp.Rem, Some(right)) if right != 0 => return Some(left % right)
          case _ => return None()
        }
      case select: SAST.Exp.Select if select.id.value == "size" && select.receiverOpt.nonEmpty =>
        val array: SAST.Exp = select.receiverOpt.get match {
          case get: SAST.Exp.Select if get.receiverOpt.nonEmpty =>
            get.attr.typedOpt match {
              case Some(m: SAST.Typed.Method) if m.owner == SAST.Typed.optionName && m.name == "get" =>
                get.receiverOpt.get
              case _ => get
            }
          case receiver => receiver
        }
        getArrayType(array, aadlTypes, store) match {
          case Some(arrayType) => return Some(arrayType.size)
          case _ => return None()
        }
      case invoke: SAST.Exp.Invoke if GclResolver.getIndexingTypeFingerprints(store).contains(invoke.ident.id.value) =>
        return getValue(getIndexingExpr(invoke, store))
      case _ => return None()
    }
    return getValue(exp)
  }

  // Resolves a quantified range and its direct C2PO array representation.
  @pure def getQuantRange(exp: SAST.Exp.QuantRange,
                          param: String,
                          binder: String,
                          bodyExp: SAST.Exp,
                          quantifierValues: Map[String, Z],
                          aadlTypes: AadlTypes,
                          store: Store): (Z, Option[Z], SAST.Exp, Option[SAST.Exp]) = {
    checkC2POIdentifier(param)
    val lo: Z = getStaticValue(exp.lo, quantifierValues, aadlTypes, store) match {
      case Some(value) => value
      case _ => halt("R2U2 monitors require statically resolvable quantified ranges")
    }
    val hi: Z = getStaticValue(exp.hi, quantifierValues, aadlTypes, store) match {
      case Some(value) => value
      case _ => halt("R2U2 monitors require statically resolvable quantified ranges")
    }
    val lastIndex: Z = if (exp.hiExact) hi else hi - 1
    if (!(lo < lastIndex && lo >= 0)) {
      halt(s"R2U2 monitor quantifier range has lower bound $lo and upper bound $lastIndex; the lower bound must be nonnegative and less than the upper bound")
    }

    // Determines whether the predicate can bind a C2PO array element directly.
    val rewriter = C2POQuantifierRewriter(param, binder, store)
    // Predicate with indexed uses of the local quantifier variable replaced by the C2PO binder.
    val rewrittenBody: SAST.Exp = rewriter.transform_langastExp(bodyExp).getOrElse(bodyExp)
    // Array to aggregate directly when every use of the quantifier variable is local.
    val directArrayOpt: Option[SAST.Exp] = if (rewriter.isQuantifierVarLocal) rewriter.arrayOpt else None()
    // Recognizes full ranges: 0 until samples.size or 0 to samples.size - 1.
    val fullRangeSizeExpOpt: Option[SAST.Exp] =
      if (!exp.hiExact) Some(exp.hi)
      else exp.hi match {
        case binary: SAST.Exp.Binary if binary.op == SAST.Exp.BinaryOp.Sub &&
          getStaticValue(binary.right, quantifierValues, aadlTypes, store) == Some(z"1") => Some(binary.left)
        case _ => None()
      }
    // True when the range spans the same array used by the predicate.
    val isFullArray: B = (fullRangeSizeExpOpt, directArrayOpt) match {
      case (Some(select: SAST.Exp.Select), Some(array))
        if lo == 0 && select.id.value == "size" && select.receiverOpt.nonEmpty =>
        isSameArray(select.receiverOpt.get, array)
      case _ => F
    }
    // None omits the slice for a full array; otherwise this is the inclusive final index.
    val lastIndexOpt: Option[Z] =
      if (isFullArray) None()
      else Some(lastIndex)
    return (lo, lastIndexOpt, rewrittenBody, directArrayOpt)
  }

  @record class C2POQuantifierRewriter(val param: String, val binder: String, val store: Store) extends org.sireum.hamr.ir.MTransformer {
    var arrayOpt: Option[SAST.Exp] = None()
    var isQuantifierVarLocal: B = T

    // Replaces array indexing on the local quantifier variable with the C2PO element binder.
    override def pre_langastExpInvoke(o: SAST.Exp.Invoke): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.attr.resOpt match {
        case Some(m: SAST.ResolvedInfo.Method)
          if m.owner == ISZ("org", "sireum") && m.id == "IS" && o.ident.id.value != "IS" && o.args.size == 1 =>
          val indexedArrayOpt: Option[SAST.Exp] =
            if (o.ident.id.value == "apply") o.receiverOpt
            else Some(o.ident)
          (indexedArrayOpt, getIndexingExpr(o.args(0), store)) match {
            case (Some(array), index: SAST.Exp.Ident) =>
              (index.resOpt, getC2POArrayPath(array)) match {
                case (Some(local: SAST.ResolvedInfo.LocalVar), Some(_)) if local.id == param =>
                  arrayOpt match {
                    case Some(a) if !isSameArray(a, array) => isQuantifierVarLocal = F
                    case _ => arrayOpt = Some(array)
                  }
                  return org.sireum.hamr.ir.MTransformer.PreResult(
                    F, MSome(index(id = index.id(value = binder), attr = o.attr(resOpt = None()))))
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    // Detects uses of the quantified variable outside a supported array index.
    override def pre_langastExpIdent(o: SAST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.resOpt match {
        case Some(local: SAST.ResolvedInfo.LocalVar) if local.id == param => isQuantifierVarLocal = F
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(F, MNone[SAST.Exp]())
    }

    // Disables direct element rewriting when a nested quantifier shadows the binder.
    override def pre_langastExpQuantRange(o: SAST.Exp.QuantRange): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp.Quant] = {
      if (ops.ISZOps(o.fun.params).exists((p: SAST.Exp.Fun.Param) =>
        p.idOpt.exists((id: SAST.Id) => id.value == param))) {
        isQuantifierVarLocal = F
        return org.sireum.hamr.ir.MTransformer.PreResult(F, MNone[SAST.Exp.Quant]())
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp.Quant]())
    }
  }

  // Maps a resolved Slang type to its C2PO type.
  @pure def getTypedExprType(typed: SAST.Typed): C2POType.Type = {
    typed match {
      // Event-data input ports are Option[payload].  C2PO receives the payload
      // and its presence as separate input signals, so classify the payload.
      case SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload)) =>
        return getTypedExprType(payload)
      case n: SAST.Typed.Name =>
        n.ids match {
          case ISZ("org", "sireum", "IS") => return C2POType.array
          case ISZ("org", "sireum", "ISZ") => return C2POType.array
          case ISZ("org", "sireum", "B") => return C2POType.bool
          case ISZ("org", "sireum", "C") => return C2POType.int
          case ISZ("org", "sireum", "S8") => return C2POType.int
          case ISZ("org", "sireum", "S16") => return C2POType.int
          case ISZ("org", "sireum", "S32") => return C2POType.int
          case ISZ("org", "sireum", "U8") => return C2POType.int
          case ISZ("org", "sireum", "U16") => return C2POType.int
          case ISZ("org", "sireum", "F32") => return C2POType.float
          case ISZ("org", "sireum", "F64") => return C2POType.float
          // Enum element types end in "Type".
          case _ if n.ids.nonEmpty && n.ids(n.ids.lastIndex) == Info.Enum.elementTypeSuffix => return C2POType.enumeration
          // Other named AADL types are validated as records by getStructType.
          case _ if n.ids.size < 2 || n.ids(0) != "org" || n.ids(1) != "sireum" => return C2POType.struct
          case _ => halt(s"Type ${n.ids} is not supported by R2U2 monitors")
        }
      case _ => halt(s"Type ${typed} is not supported by R2U2 monitors")
    }
  }

  // Determines the C2PO result type of a GUMBO expression.
  @pure def getExprType(exp: org.sireum.lang.ast.Exp): C2POType.Type = {
    exp match {
      // Literals have fixed concrete types.
      case _: org.sireum.lang.ast.Exp.LitB => return C2POType.bool
      case _: org.sireum.lang.ast.Exp.LitC => return C2POType.int // R2U2 supports i32 by default, char (or u8) can be cast to i32 within R2U2
      case _: org.sireum.lang.ast.Exp.LitZ => halt("Unbounded Integer is not supported by R2U2 monitors")
      case _: org.sireum.lang.ast.Exp.LitF32 => return C2POType.float // R2U2 supports f64 by default, f32 can be cast up to f64 within R2U2
      case _: org.sireum.lang.ast.Exp.LitF64 => return C2POType.float
      case _: org.sireum.lang.ast.Exp.LitR => halt("Unbounded Float is not supported by R2U2 monitors")
      case _: org.sireum.lang.ast.Exp.LitString => halt("Strings are not supported by R2U2 monitors")

      case bin: org.sireum.lang.ast.Exp.Binary =>
        if ( // Boolean and comparison operations
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Lt ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Le ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Gt ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Ge ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondAnd ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondOr ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Imply ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondImply ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Eq ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Ne
        ) {
          // Boolean and comparison operations always return Booleans.
          return C2POType.bool
        } else if ( // Bitwise operations
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.And ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Or ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Xor ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shl ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shr
        ){
          // Bitwise operations preserve their matching int or bool operand type.
          val return_type_left = getExprType(bin.left)
          val return_type_right = getExprType(bin.right)
          if (return_type_left == return_type_right && (return_type_left == C2POType.int || return_type_left == C2POType.bool)){
            return return_type_left
          } else {
            halt("Expression type is not supported by R2U2 monitors")
          }
        }
        else if ( // Arithmetic operations
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Add ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Sub ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Mul ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Div ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Rem
        ){
          // Arithmetic operations preserve their matching int or float operand type.
          val return_type_left = getExprType(bin.left)
          val return_type_right = getExprType(bin.right)
          if (return_type_left == return_type_right && (return_type_left == C2POType.int || return_type_left == C2POType.float)){
            return return_type_left
          } else {
            halt("Expression type is not supported by R2U2 monitors")
          }
        } else {
          halt("Expression type is not supported by R2U2 monitors")
        }

      case un: org.sireum.lang.ast.Exp.Unary =>
        if ( // Logical negation
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Not
        ) {
          return C2POType.bool
        } else if (
            un.op == org.sireum.lang.ast.Exp.UnaryOp.Complement
        ) { // Bitwise operations
          // Bitwise complement preserves its int or bool operand type.
          val return_type = getExprType(un.exp)
          if (return_type == C2POType.int || return_type == C2POType.bool){
            return return_type
          } else {
            halt("Expression type is not supported by R2U2 monitors")
          }
        } else if ( // Arithmetic signs
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Plus ||
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Minus
        ) {
          // Arithmetic signs preserve their int or float operand type.
          val return_type = getExprType(un.exp)
          if (return_type == C2POType.int || return_type == C2POType.float){
            return return_type
          } else {
            halt("Expression type is not supported by R2U2 monitors")
          }
        } else {
          halt("Expression type is not supported by R2U2 monitors")
        }
      case ifExp: org.sireum.lang.ast.Exp.If =>
          // Both branches of a conditional must have the same result type and boolean.
          val thenType = getExprType(ifExp.thenExp)
          val elseType = getExprType(ifExp.elseExp)
          if (thenType == elseType && thenType == C2POType.bool) {
               return thenType
          } else {
          halt("Expression type is not supported by R2U2 monitors")
      }
      // Classify status, size, port, and member selections.
      case sel: org.sireum.lang.ast.Exp.Select => 
          if (sel.id.value == "nonEmpty" || sel.id.value == "isEmpty") {
               return C2POType.bool
          } else if (sel.id.value == "size") {
               return C2POType.int
          } else {
               // Port values and other typed selections carry enough resolved type
               // information to classify them without access to the surrounding scope.
               sel.attr.typedOpt match {
                    case Some(typed) => return getTypedExprType(typed)
                    case _ => halt("Expression type is not supported by R2U2 monitors")
               }
          }

      // Resolved identifiers carry their expression type.
      case id: org.sireum.lang.ast.Exp.Ident =>
        id.attr.typedOpt match {
          case Some(typed) => return getTypedExprType(typed)
          case _ => halt("Expression type is not supported by R2U2 monitors")
        }
      // Resolved GUMBO function calls carry their return type.
      case invoke: org.sireum.lang.ast.Exp.Invoke =>
        invoke.attr.typedOpt match {
          case Some(typed) => return getTypedExprType(typed)
          case _ => halt("Expression type is not supported by R2U2 monitors")
        }
      case _ => halt("Expression type is not supported by R2U2 monitors")
    }
  }

  // Maps an AADL primitive type to its C2PO type.
  @pure def getBaseType(baseType: BaseType): C2POType.Type = {
    baseType.slangType match {
      case SlangType.B => return C2POType.bool
      case SlangType.S8 | SlangType.S16 | SlangType.S32 |
           SlangType.U8 | SlangType.U16 => return C2POType.int
      case SlangType.F32 | SlangType.F64 => return C2POType.float
      case _ => halt(s"Type ${baseType.slangType} is not supported by R2U2 monitors")
    }
  }

  // Resolves an expression's AADL enum declaration.
  @pure def getEnumType(exp: SAST.Exp, aadlTypes: AadlTypes): C2POEnum = {
    val typed: SAST.Typed = exp.typedOpt match {
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => payload
      case Some(t) => t
      case _ => halt("Enum expression is missing its resolved type")
    }
    typed match {
      case n: SAST.Typed.Name =>
        aadlTypes.getTypeByPathOpt(n.ids) match {
          case Some(e: EnumType) =>
            checkC2POEnum(e)
            return C2POEnum(e.simpleName, e.values)
          case _ => halt(s"Type ${n.ids} is not an AADL enum")
        }
      case _ => halt(s"Type $typed is not an AADL enum")
    }
  }

  // Resolves a supported one-dimensional AADL array type.
  @pure def getArrayType(exp: SAST.Exp,
                         aadlTypes: AadlTypes,
                         store: Store): Option[C2POArray] = {
    val typed: SAST.Typed = exp.typedOpt match {
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => payload
      case Some(t) => t
      case _ => halt("Array expression is missing its resolved type")
    }
    typed match {
      case n: SAST.Typed.Name =>
        val aadlTypePath: ISZ[String] = GclResolver.getSlangTypeToAadlType(store).get(n) match {
          case Some(path) => path
          case _ => halt(s"Could not resolve Slang array type $n to an AADL type")
        }
        aadlTypes.getTypeByPathOpt(aadlTypePath) match {
          case Some(arrayType: ArrayType) =>
            if (arrayType.dimensions.size != 1) halt("Only one-dimensional arrays are supported by R2U2 monitors")
            arrayType.baseType match {
              case b: BaseType => return Some(C2POArray(getBaseType(b), arrayType.dimensions(0)))
              case _ => halt("Only arrays of primitive values are supported by R2U2 monitors")
            }
          case _ => return None()
        }
      case _ => halt(s"Type $typed is not an AADL array")
    }
  }

  // Resolves an expression's supported AADL struct declaration.
  @pure def getStructType(exp: SAST.Exp, aadlTypes: AadlTypes): C2POStruct = {
    val typed: SAST.Typed = exp.typedOpt match {
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => payload
      case Some(t) => t
      case _ => halt("Struct expression is missing its resolved type")
    }
    typed match {
      case n: SAST.Typed.Name =>
        aadlTypes.getTypeByPathOpt(n.ids) match {
          case Some(recordType: RecordType) =>
            checkC2POIdentifier(recordType.nameProvider.typeName)
            val fields: ISZ[C2POStructField] = for (field <- recordType.fields.entries) yield {
              checkC2POIdentifier(field._1)
              field._2 match {
                case b: BaseType =>
                  C2POStructField(field._1, getBaseType(b), None(), None())
                case e: EnumType =>
                  checkC2POEnum(e)
                  C2POStructField(field._1, C2POType.enumeration, Some(C2POEnum(e.simpleName, e.values)), None())
                case a: ArrayType =>
                  if (a.dimensions.size != 1) halt("Only one-dimensional arrays are supported by R2U2 monitors")
                  a.baseType match {
                    case b: BaseType => C2POStructField(field._1, C2POType.array, None(), Some(C2POArray(getBaseType(b), a.dimensions(0))))
                    case _ => halt("Only arrays of primitive values are supported by R2U2 monitors")
                  }
                case _ => halt("Nested structs are not supported by R2U2 monitors")
              }
            }
            return C2POStruct(recordType.nameProvider.typeName, fields)
          case _ => halt(s"Type ${n.ids} is not an AADL struct")
        }
      case _ => halt(s"Type $typed is not an AADL struct")
    }
  }

  // Helper to build a clean string chain ("api_myStructArray_nonEmpty").
  // It returns None() if the selection path does not originate from the 'api' object.
  @pure def getFlatPathString(subExp: org.sireum.lang.ast.Exp): Option[String] = {
    subExp match {
      case id: org.sireum.lang.ast.Exp.Ident if id.id.value == "api" => return Some("api")
      case sel: org.sireum.lang.ast.Exp.Select =>
        sel.receiverOpt match {
          case Some(recv) =>
            getFlatPathString(recv) match {
              case Some(prefix) => return Some(if (sel.id.value == "get") prefix else s"${prefix}_${sel.id.value}")
              case _ => return None()
            }
          case _ => return None()
        }
      case _ => return None()
    }
  }

  // Function collects any identifiers, flattens them if necessary, and returns the new expr and a
  // map of identifiers to expressions
  @pure def collectIdentifiers(exp: org.sireum.lang.ast.Exp): (org.sireum.lang.ast.Exp, Map[String, org.sireum.lang.ast.Exp]) = {
    var categorized : Map[String, org.sireum.lang.ast.Exp] = Map.empty

    exp match {
      // 1. Collect simple variables, ports, or standalone identifiers.
      case id: org.sireum.lang.ast.Exp.Ident =>
        // Quantifier local variables are NOT monitor inputs.
        id.resOpt match {
          case Some(_: SAST.ResolvedInfo.LocalVar) => return (id, categorized)
          case _ =>
        }
        categorized += (id.id.value -> id)
        return (id, categorized)
      // 2. Collect and rewrite component selections (e.g., api.my_var or state.my_var).
      case sel: org.sireum.lang.ast.Exp.Select =>
        val isEnumMember: B = sel.resOpt match {
          case Some(_: SAST.ResolvedInfo.EnumElement) => T
          case _ => F
        }
        val isOptionGet: B = sel.attr.typedOpt match {
          case Some(m: SAST.Typed.Method) =>
            m.owner == ISZ("org", "sireum", "Option") && m.name == "get"
          case _ => F
        }
        val isStructMember: B = sel.id.value != "size" && sel.receiverOpt.exists((receiver: SAST.Exp) => receiver.typedOpt match {
          case Some(m: SAST.Typed.Method) => m.owner == SAST.Typed.optionName && m.name == "get"
          case Some(SAST.Typed.Name(ids, _, _)) => ids.size < 2 || ids(0) != "org" || ids(1) != "sireum"
          case _ => F
        })
        if (isEnumMember) {
          // Enum members are constants, NOT monitor inputs. Preserve the
          // selection so SlangExpUtil can lower it to its C2PO member name.
          return (sel, categorized)
        } else if (isOptionGet) {
          // GCL resolution inserts Option.get when an event-data port is used as
          // a value.  R2U2 models that value and its presence as separate input
          // signals, so collect the optional port itself and omit the accessor.
          sel.receiverOpt match {
            case Some(receiver) => return collectIdentifiers(receiver)
            case _ => halt("Option.get is missing its receiver")
          }
        } else if (isStructMember) {
          // Preserve record member access while collecting the record input.
          val res = collectIdentifiers(sel.receiverOpt.get)
          for (e <- res._2.entries) { categorized += e }
          return (sel(receiverOpt = Some(res._1)), categorized)
        } else {
          getFlatPathString(sel) match {
            case Some(collapsedString) =>
              categorized += (collapsedString -> sel)

              val freshId = org.sireum.lang.ast.Id(value = collapsedString, attr = sel.id.attr)
              val rewrittenNode = org.sireum.lang.ast.Exp.Ident(id = freshId, attr = sel.attr)
              return (rewrittenNode, categorized)

            case _ =>
              val (updatedRecv, innerMapping): (Option[org.sireum.lang.ast.Exp], Map[String, org.sireum.lang.ast.Exp]) = sel.receiverOpt match {
                case Some(r) =>
                  val res = collectIdentifiers(r)
                  (Some(res._1), res._2)
                case _ => (None[org.sireum.lang.ast.Exp](), Map.empty)
              }
              for (e <- innerMapping.entries) { categorized += e }
              return (sel(receiverOpt = updatedRecv), categorized)
          }
        }
      // 3. Collect In(state) under a distinct pre-state input name.
      case input @ org.sireum.lang.ast.Exp.Input(id: org.sireum.lang.ast.Exp.Ident) =>
        val name = s"in_${id.id.value}"
        categorized += (name -> input)
        return (id(id = id.id(value = name)), categorized)
      // 4. Drill down into conditional expressions.
      case ifExp: org.sireum.lang.ast.Exp.If =>
        val cond = collectIdentifiers(ifExp.cond)
        val thenExp = collectIdentifiers(ifExp.thenExp)
        val elseExp = collectIdentifiers(ifExp.elseExp)
        for (res <- ISZ(cond, thenExp, elseExp); e <- res._2.entries) { categorized += e }
        return (ifExp(cond._1, thenExp._1, elseExp._1), categorized)
      // 5. Drill down into binary operators (e.g., x > 5, a AND b).
      case bin: org.sireum.lang.ast.Exp.Binary =>
        val res_left = collectIdentifiers(bin.left)
        val res_right = collectIdentifiers(bin.right)
        for (e <- res_left._2.entries) { categorized += e }
        for (e <- res_right._2.entries) { categorized += e }
        return (bin(res_left._1, bin.op, res_right._1), categorized)
      // 6. Drill down into unary operators (e.g., !x).
      case un: org.sireum.lang.ast.Exp.Unary =>
        val res = collectIdentifiers(un.exp)
        for (e <- res._2.entries) { categorized += e }
        return (un(exp = res._1), categorized)
      case un: org.sireum.lang.ast.Exp.UnaryTemporal =>
        val res = collectIdentifiers(un.exp)
        for (e <- res._2.entries) { categorized += e }
        return (un(exp = res._1), categorized)
      case bin: org.sireum.lang.ast.Exp.BinaryTemporal =>
        val resLeft = collectIdentifiers(bin.left)
        val resRight = collectIdentifiers(bin.right)
        for (e <- resLeft._2.entries) { categorized += e }
        for (e <- resRight._2.entries) { categorized += e }
        return (bin(left = resLeft._1, right = resRight._1), categorized)
      // 7. Range bounds are static; only the predicate contributes monitor inputs.
      case quant: org.sireum.lang.ast.Exp.QuantRange =>
        quant.fun.exp match {
          case stmt: SAST.Stmt.Expr =>
            val res: (SAST.Exp, Map[String, SAST.Exp]) = collectIdentifiers(stmt.exp)
            for (e <- res._2.entries) { categorized += e }
            return (quant(fun = quant.fun(exp = stmt(exp = res._1))), categorized)
          case _ => halt(s"Unexpected quantified expression: ${quant.fun.exp.prettyST.render}")
        }
      // 8. Drill down into function or method invocations.
      case invoke: org.sireum.lang.ast.Exp.Invoke =>
        // GCL combines Option.get and array indexing for an event-data array into
        // api.port.get(index). Preserve the index while omitting the Option access.
        val ident = invoke.receiverOpt match {
          case Some(receiver) if invoke.ident.id.value == "get" =>
            receiver.typedOpt match {
              case Some(SAST.Typed.Name(SAST.Typed.optionName, _, _)) =>
                invoke.ident(id = invoke.ident.id(value = "apply"))
              case _ => invoke.ident
            }
          case _ => invoke.ident
        }
        val functionName: String = invoke.receiverOpt match {
          case Some(fId: org.sireum.lang.ast.Exp.Ident) => fId.id.value
          case _ => ""
        }
        if (functionName != "") {
          categorized += (functionName -> invoke)
        }
        // Collect the indexed array as an input.
        invoke.attr.resOpt match {
          case Some(m: SAST.ResolvedInfo.Method)
            if m.owner == ISZ("org", "sireum") && m.id == "IS" && invoke.receiverOpt.isEmpty =>
            invoke.ident.resOpt match {
              case Some(_: SAST.ResolvedInfo.Var) => categorized += (invoke.ident.id.value -> invoke.ident)
              case _ =>
            }
          case _ =>
        }
        var updatedArgs = ISZ[org.sireum.lang.ast.Exp]()

        for (arg <- invoke.args.elements) {
          val (newArg, argMapping) = collectIdentifiers(arg)
          updatedArgs = updatedArgs :+ newArg
          for (e <- argMapping.entries) { categorized += e }
        }

        val res: (Option[org.sireum.lang.ast.Exp], Map[String, org.sireum.lang.ast.Exp]) = invoke.receiverOpt match {
          case Some(r) =>
            val res_inner = collectIdentifiers(r)
            (Some(res_inner._1), res_inner._2)
          case _ => (None[org.sireum.lang.ast.Exp](), Map.empty)
        }
        for (e <- res._2.entries) { categorized += e }
        return (invoke(receiverOpt = res._1, ident = ident, args = updatedArgs), categorized)
      // Fallback for literals, constants, or unsupported expressions
      case leaf =>
        return (leaf, categorized)
    }
  }

  // Checks whether an expression calls a local GUMBO function.
  @pure def isGumboFunctionCall(exp: SAST.Exp, owner: ISZ[String]): B = {
    exp match {
      case invoke: SAST.Exp.Invoke =>
        invoke.attr.resOpt match {
          case Some(method: SAST.ResolvedInfo.Method) => return method.owner == owner
          case _ => return F
        }
      case _ => return F
    }
  }

  // Replaces local GUMBO calls with named C2PO input identifiers.
  @record class C2POFunctionRewriter(val owner: ISZ[String]) extends org.sireum.hamr.ir.MTransformer {
    var functionInputs: Map[String, SAST.Exp] = Map.empty

    // Names simple function arguments directly and fingerprints complex expressions.
    def getFunctionArgumentName(arg: SAST.Exp): String = {
      getFlatPathString(arg) match {
        case Some(name) if ops.StringOps(name).startsWith("api_") =>
          return ops.StringOps(name).substring(4, name.size)
        case Some(name) => return name
        case _ => return s"arg_${TypeUtil.stableTypeSig(arg.prettyST.render, 3)}"
      }
    }

    override def pre_langastExpInvoke(invoke: SAST.Exp.Invoke): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      if (isGumboFunctionCall(invoke, owner)) {
        val argumentNames: ISZ[String] = for (arg <- invoke.args) yield getFunctionArgumentName(arg)
        val suffix: String = if (argumentNames.nonEmpty) st"_${(argumentNames, "_")}".render else ""
        val name = s"fn_${invoke.ident.id.value}$suffix"
        functionInputs = functionInputs + name ~> invoke
        return org.sireum.hamr.ir.MTransformer.PreResult(
          F, MSome(SAST.Exp.Ident(SAST.Id(name, invoke.ident.id.attr), invoke.attr)))
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }
  }

  // Collects standard monitor inputs after replacing local function calls.
  @pure def collectMonitorInputs(exp: SAST.Exp,
                                 owner: ISZ[String]): (SAST.Exp, Map[String, SAST.Exp]) = {
    val rewriter = C2POFunctionRewriter(owner)
    val rewrittenExp: SAST.Exp = rewriter.transform_langastExp(exp).getOrElse(exp)
    val result = collectIdentifiers(rewrittenExp)
    return (result._1, result._2 ++ rewriter.functionInputs.entries)
  }

  @enum object SpecTense {
    "Future"
    "Past"
  }

  @record class TenseCollector extends org.sireum.hamr.ir.MTransformer {
    var hasFuture: B = F
    var hasPast: B = F

    // Records future-time or past-time unary temporal operators.
    override def pre_langastExpUnaryTemporal(o: SAST.Exp.UnaryTemporal): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.op match {
        case SAST.Exp.UnaryTemporalOp.Future | SAST.Exp.UnaryTemporalOp.Globally => hasFuture = T
        case SAST.Exp.UnaryTemporalOp.Once | SAST.Exp.UnaryTemporalOp.Historically => hasPast = T
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    // Records future-time or past-time binary temporal operators.
    override def pre_langastExpBinaryTemporal(o: SAST.Exp.BinaryTemporal): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.op match {
        case SAST.Exp.BinaryTemporalOp.Until | SAST.Exp.BinaryTemporalOp.Release => hasFuture = T
        case SAST.Exp.BinaryTemporalOp.Since | SAST.Exp.BinaryTemporalOp.Trigger => hasPast = T
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }
  }

  // Determines whether a monitor specification uses future or past time.
  @pure def getSpecTense(exp: org.sireum.lang.ast.Exp): SpecTense.Type = {
    val collector = TenseCollector()
    collector.transform_langastExp(exp)

    if (collector.hasFuture && collector.hasPast) {
      halt(s"Monitor guarantee cannot combine future-time and past-time temporal operators")
    }

    if (collector.hasPast) {
      return SpecTense.Past
    } else { // Expressions without temporal operators are also future-time specifications.
      return SpecTense.Future
    }
  }
}
