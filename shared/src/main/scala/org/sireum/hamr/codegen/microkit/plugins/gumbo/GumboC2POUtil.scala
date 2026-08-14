// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType, BaseType, EnumType, RecordType, SlangType}
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

  @pure def getExprType(exp: org.sireum.lang.ast.Exp): C2POType.Type = {
    exp match {
      // 1. Literal Nodes have fixed concrete types
      case _: org.sireum.lang.ast.Exp.LitB => return C2POType.bool
      case _: org.sireum.lang.ast.Exp.LitC => return C2POType.int // R2U2 supports i32 by default, char (or u8) can be cast to i32 within R2U2
      case _: org.sireum.lang.ast.Exp.LitZ => halt("Unbounded Integer is not supported by R2U2 monitors")
      case _: org.sireum.lang.ast.Exp.LitF32 => return C2POType.float // R2U2 supports f64 by default, f32 can be cast up to f64 within R2U2
      case _: org.sireum.lang.ast.Exp.LitF64 => return C2POType.float
      case _: org.sireum.lang.ast.Exp.LitR => halt("Unbounded Float is not supported by R2U2 monitors")
      case _: org.sireum.lang.ast.Exp.LitString => halt("Strings are not supported by R2U2 monitors")


      // 2. Logic and comparison operations always return standard Booleans
      case bin: org.sireum.lang.ast.Exp.Binary =>
        if ( // Relational operations
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
          return C2POType.bool
        } else if ( // Bitwise operations
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.And ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Or ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Xor ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shl ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shr
        ){
          // Need to infer int or bool math based on left side and right side
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
          // Need to infer int or float math based on left side and right side
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
        if ( // Relational operations
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Not
        ) {
          return C2POType.bool
        } else if (
            un.op == org.sireum.lang.ast.Exp.UnaryOp.Complement
        ) { // Bitwise operations
          // Need to infer type based on right side (int or bool)
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
          // Need to infer type based on right side (int or float)
          val return_type = getExprType(un.exp)
          if (return_type == C2POType.int || return_type == C2POType.float){
            return return_type
          } else {
            halt("Expression type is not supported by R2U2 monitors")
          }
        } else {
          halt("Expression type is not supported by R2U2 monitors")
        }
      // 3. Status checks on nested properties are structurally Booleans
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

      // 4. Identifiers must be checked against your scope context
      case id: org.sireum.lang.ast.Exp.Ident =>
        id.attr.typedOpt match {
          case Some(typed) => return getTypedExprType(typed)
          case _ => halt("Expression type is not supported by R2U2 monitors")
        }
      case _ => halt("Expression type is not supported by R2U2 monitors")
    }
  }

  @pure def getBaseType(baseType: BaseType): C2POType.Type = {
    baseType.slangType match {
      case SlangType.B => return C2POType.bool
      case SlangType.S8 | SlangType.S16 | SlangType.S32 |
           SlangType.U8 | SlangType.U16 => return C2POType.int
      case SlangType.F32 | SlangType.F64 => return C2POType.float
      case _ => halt(s"Type ${baseType.slangType} is not supported by R2U2 monitors")
    }
  }

  @pure def getEnumType(exp: SAST.Exp, aadlTypes: AadlTypes): C2POEnum = {
    val typed: SAST.Typed = exp.typedOpt match {
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => payload
      case Some(t) => t
      case _ => halt("Enum expression is missing its resolved type")
    }
    typed match {
      case n: SAST.Typed.Name =>
        aadlTypes.getTypeByPathOpt(n.ids) match {
          case Some(e: EnumType) => return C2POEnum(e.simpleName, e.values)
          case _ => halt(s"Type ${n.ids} is not an AADL enum")
        }
      case _ => halt(s"Type $typed is not an AADL enum")
    }
  }

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
            val fields: ISZ[C2POStructField] = for (field <- recordType.fields.entries) yield {
              field._2 match {
                case b: BaseType =>
                  C2POStructField(field._1, getBaseType(b), None(), None())
                case e: EnumType =>
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

  // Function collects any identifiers, flattens them if necessary, and returns the new expr and a
  // map of identifiers to expressions
  @pure def collectIdentifiers(exp: org.sireum.lang.ast.Exp): (org.sireum.lang.ast.Exp, Map[String, org.sireum.lang.ast.Exp]) = {
    var categorized : Map[String, org.sireum.lang.ast.Exp] = Map.empty

    // Helper closure to build a clean string chain ("api_myStructArray_nonEmpty")
    // It returns None() if the selection path does not originate from the 'api' object
    def getFlatPathString(subExp: org.sireum.lang.ast.Exp): Option[String] = {
      subExp match {
        case id: org.sireum.lang.ast.Exp.Ident if id.id.value == "api" =>
          return Some("api")

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

    exp match {
      // 1. Matches simple variables, ports, or standalone identifiers
      case id: org.sireum.lang.ast.Exp.Ident =>
        categorized += (id.id.value -> id)
        return (id, categorized)
      // 2. Matches component dot-selections (e.g., api.my_var or state.my_var)
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
          // Enum members are constants, not monitor inputs. Preserve the
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
      // 3. Drill down into Binary Operators (e.g., x > 5, a AND b)
      case bin: org.sireum.lang.ast.Exp.Binary =>
        val res_left = collectIdentifiers(bin.left)
        val res_right = collectIdentifiers(bin.right)
        for (e <- res_left._2.entries) { categorized += e }
        for (e <- res_right._2.entries) { categorized += e }
        return (bin(res_left._1, bin.op, res_right._1), categorized)
      // 4. Drill down into Unary Operators (e.g., !x)
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
      // 5. Drill down into Function/Method invocations (e.g., compute(x, y))
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

  @enum object SpecTense {
    "Future"
    "Past"
  }

  @record class TenseCollector extends org.sireum.hamr.ir.MTransformer {
    var hasFuture: B = F
    var hasPast: B = F

    override def pre_langastExpUnaryTemporal(o: SAST.Exp.UnaryTemporal): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.op match {
        case SAST.Exp.UnaryTemporalOp.Future | SAST.Exp.UnaryTemporalOp.Globally => hasFuture = T
        case SAST.Exp.UnaryTemporalOp.Once | SAST.Exp.UnaryTemporalOp.Historically => hasPast = T
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    override def pre_langastExpBinaryTemporal(o: SAST.Exp.BinaryTemporal): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.op match {
        case SAST.Exp.BinaryTemporalOp.Until | SAST.Exp.BinaryTemporalOp.Release => hasFuture = T
        case SAST.Exp.BinaryTemporalOp.Since | SAST.Exp.BinaryTemporalOp.Trigger => hasPast = T
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }
  }

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
