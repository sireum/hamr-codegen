// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, BaseType, EnumType, RecordType, TypeResolver, TypeUtil}
import org.sireum.hamr.ir.{GclMethod, GclStateVar, MTransformer, MTransformer => irMTransformer}
import org.sireum.message.Reporter
import org.sireum.lang.{ast => AST}
import org.sireum.lang.ast.Exp
import org.sireum.lang.symbol.{Info, Scope, TypeInfo}
import org.sireum.lang.tipe.TypeHierarchy

object GclResolverUtil {

  def getSlangName(aadlType: AadlType, reporter: Reporter): ISZ[String] = {
    val qualifiedName = aadlType.classifier
    val simpleName = qualifiedName(qualifiedName.lastIndex)
    aadlType match {
      case b: BaseType =>
        val simpleSireumName: String = simpleName match {
          case "Integer_8" => "S8"
          case "Integer_16" => "S16"
          case "Integer_32" => "S32"
          case "Integer_64" => "S64"
          case "Integer" => "Z"

          case "Unsigned_8" => "U8"
          case "Unsigned_16" => "U16"
          case "Unsigned_32" => "U32"
          case "Unsigned_64" => "U64"

          case "Float_32" => "F32"
          case "Float_64" => "F64"
          case "Float" => "R"

          case "Boolean" => "B"
          case "Character" => "C"
          case "String" => "String"


          // the 2025.11 omg library adds prefixes to the following
          case "Data_Float_32" => "F32"
          case "Data_Float_64" => "F64"
          case "Data_Float" => "R"
          case "Data_Boolean" => "B"

          case x =>
            reporter.error(b.container.get.identifier.pos, GclResolver.toolName, s"Wasn't expecting $simpleName while trying to get Slang type name")
            "String"
        }
        return ISZ("org", "sireum", simpleSireumName)
      case e: EnumType =>
        return qualifiedName :+ "Type"
      case _ =>
        return qualifiedName
    }
  }

  val baseTypeConvertersMap: Map[String, ISZ[String]] = {
    @strictpure def s(dest: String): ISZ[String] =
      ISZ(s"to$dest")

    @strictpure def m(dest: String): ISZ[String] =
      for (i <- ISZ[Z](8, 16, 32, 64)) yield s"to$dest$i"

    @pure def p(includeR: B): ISZ[String] = {
      var ret: ISZ[String] = ISZ()
      ret = ret ++ s("B")

      ret = ret ++ s("Z")

      //ret = ret ++ s("N")
      //ret = ret ++ m("N")

      ret = ret ++ m("S")

      ret = ret ++ m("U")

      if (includeR) {
        ret = ret ++ s("R")
      }
      return ret
    }

    var ret: Map[String, ISZ[String]] = Map.empty

    @pure def add(origin: String, dests: ISZ[String]): Unit = {
      ret = ret + (origin ~> (ret.getOrElse(origin, ISZ()) ++ dests))
    }

    add("B", p(T))
    add("B", s("F32"))
    add("B", s("F64"))

    add("C", s("U32"))

    add("Z", p(T))

    /*
    add("N", p(T))
    add("N8", p(T))
    add("N16", p(T))
    add("N32", p(T))
    add("N64", p(T))
    */

    add("S8", p(F))
    add("S16", p(F))
    add("S32", p(F))
    add("S64", p(F))
    add("U8", p(F))
    add("U16", p(F))

    add("U32", p(F))
    add("U32", s("F32"))
    add("U32", s("C"))

    add("U64", p(F))
    add("U64", s("F64"))

    add("F32", s("B"))
    add("F32", s("F32"))
    add("F32", s("F64"))
    add("F32", s("R"))

    add("F64", s("B"))
    add("F64", s("F64"))
    add("F64", s("R"))

    add("R", s("B"))
    add("R", s("Z"))
    //add("R", s("N"))
    add("R", s("R"))

    ret
  }


  @pure def isBaseTypeConverter(baseType: BaseType, s: String, reporter: Reporter): B = {
    val slangName = getSlangName(baseType, reporter)
    return ops.ISZOps(baseTypeConvertersMap.get(slangName(slangName.lastIndex)).get).contains(s)
  }
}


@record class ArrayIndexRewriter(val context: Option[AadlComponent],
                                 val params: ISZ[AST.Param],
                                 val stateVars: ISZ[GclStateVar],
                                 val specFuncs: Map[ISZ[String], GclMethod],
                                 val scope: Scope,
                                 val typeHierarchy: TypeHierarchy,
                                 val symbolTable: SymbolTable,
                                 val aadlTypes: AadlTypes) extends org.sireum.hamr.ir.MTransformer {
  var currType: Option[AadlType] = None()
  val emptyAttr: AST.Attr = AST.Attr(None())
  val emptyRAttr: AST.ResolvedAttr = AST.ResolvedAttr(None(), None(), None())

  var quantifierParams: ISZ[Exp.Fun.Param] = ISZ()

  val reporter: Reporter = Reporter.create

  def popType: AadlType = {
    val ret: AadlType = currType.get
    currType = None()
    return ret
  }

  def pushType(a: AadlType): Unit = {
    assert(currType.isEmpty, s"${currType.get.name}")
    currType = Some(a)
  }


  override def pre_langastExpInvoke(o: Exp.Invoke): irMTransformer.PreResult[Exp] = {
    if (GclResolver.uifs.contains(o.ident.id.value)) {
      return irMTransformer.PreResult(F, MNone())
    }

    var constructingArray = F
    var isFunctionCall = F
    var arrayType: Option[ArrayType] = None()
    var receiverOpt: MOption[Option[Exp]] = MNone()
    var ident: MOption[Exp.Ident] = MNone()

    if (o.ident.id.value == "apply") {
      o.receiverOpt match {
        case Some(i: Exp.Invoke) =>
          assert(i.receiverOpt.isEmpty, "Need to handle non-empty receivers")
          assert(i.args.isEmpty, "Need to rewrite invoke args for apply")
          val identv = i.ident.id.value
          scope.resolveName(typeHierarchy.nameMap, ISZ(identv)) match {
            case Some(info: Info.Method) =>
              // e.g. gumboMethodInvocation()(0)
              val retType: ISZ[String] = for (i <- info.ast.sig.returnType.asInstanceOf[AST.Type.Named].name.ids) yield i.value
              aadlTypes.getTypeByPathOpt(retType) match {
                case Some(a: ArrayType) =>
                  arrayType = Some(a)
                  isFunctionCall = T
                case _ => halt("Unexpected")
              }
            case _ => halt("Unexpected")
          }
        case _ => halt("Unexpected")
      }
    }
    else {
      val receiver: ISZ[String] =
        if (o.receiverOpt.nonEmpty) ops.StringOps(o.receiverOpt.get.prettyST.render).split(c => c == '.')
        else ISZ()

      val potentialTypeName = st"${(receiver :+ o.ident.id.value, "::")}".render
      aadlTypes.typeMap.get(potentialTypeName) match {
        case Some(t: ArrayType) =>
          arrayType = Some(t)
          constructingArray = T
        case Some(x) => halt(s"Unexpected: ${x.name}")
        case _ =>
          currType = None()
          receiverOpt = irMTransformer.transformOption(o.receiverOpt, transform_langastExp _)
          currType match {
            case Some(r: RecordType) =>
              r.fields.get(o.ident.id.value).get match {
                case a: ArrayType =>
                  arrayType = Some(a)
                case _ =>
              }
            case x =>
              ident = transform_langastExpIdent(o.ident)
              popType match {
                case a: ArrayType =>
                  arrayType = Some(a)
                case _ =>
              }
          }
          currType = None()
      }
    }

    arrayType match {
      case Some(a) if a.dimensions.nonEmpty =>
        val qualifiedName = a.classifier
        val indexingTypeFingerprint = TypeUtil.getIndexingTypeFingerprintMethodName(qualifiedName :+ "I")

        val typeAlias = typeHierarchy.typeMap.get(a.classifier).get.asInstanceOf[TypeInfo.TypeAlias].ast.tipe.asInstanceOf[AST.Type.Named]

        var args: ISZ[Exp] = ISZ()
        for (arg <- o.args) {
          val e = transform_langastExp(arg)
          if (currType.nonEmpty) {
            //println(s"No one consumed '${currType.get.name}' at arg '${arg.prettyST.render}' in '${o.prettyST.render}''")
            currType = None()
          }
          if (constructingArray) {
            args = args :+ e.getOrElse(arg)
          } else {
            // need to wrap indices in call to the indexing type's Z->I helper method
            args = args :+ Exp.Invoke(
              receiverOpt = None(),
              ident = Exp.Ident(AST.Id(indexingTypeFingerprint, emptyAttr), emptyRAttr),
              targs = ISZ(),
              args = ISZ(e.getOrElse(arg)),
              attr = emptyRAttr)
          }
        }

        val mod: Exp.Invoke =
          if (constructingArray)
            o(
              receiverOpt = None(),
              ident = AST.Exp.Ident(AST.Id("IS", emptyAttr), emptyRAttr),
              args = args,
              targs = typeAlias.typeArgs)
          else if (isFunctionCall)
            o(args = args)
          else
            o(
              receiverOpt = receiverOpt.getOrElse(o.receiverOpt),
              ident = ident.getOrElse(o.ident),
              args = args)

        return irMTransformer.PreResult(F, MSome(mod))
      case _ =>
        return irMTransformer.PreResult(T, MNone())
    }
  }

  override def pre_langastExpQuantRange(o: Exp.QuantRange): MTransformer.PreResult[Exp.Quant] = {
    quantifierParams = quantifierParams ++ o.fun.params
    return irMTransformer.PreResult(T, MNone())
  }

  override def pre_langastExpSelect(o: Exp.Select): irMTransformer.PreResult[Exp] = {
    assert(o.receiverOpt.nonEmpty)

    val receiver = irMTransformer.transformOption(o.receiverOpt, transform_langastExp _)

    currType match {
      case Some(a: ArrayType) =>
        assert(o.id.value == "size")
        currType = None()
        return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(o(receiverOpt = receiver.getOrElse(o.receiverOpt))))

      case Some(b: BaseType) if GclResolverUtil.isBaseTypeConverter(b, o.id.value, reporter) =>
        val fromType: ISZ[String] = GclResolverUtil.getSlangName(b, Reporter.create)

        // e.g. u8"0".toU16 --> conversions.U8.toU16(u8"0")
        val invoke = AST.Exp.Invoke(
          receiverOpt = Some(
            Exp.Select(
              receiverOpt = Some(Exp.Select(receiverOpt = None(), id = AST.Id("conversions", o.id.attr), targs = ISZ(), attr = o.attr)),
              id = AST.Id(fromType(fromType.lastIndex), o.id.attr), targs = ISZ(), attr = o.attr)
          ),
          ident = Exp.Ident(id = o.id, attr = o.attr),
          targs = ISZ(),
          args = ISZ(receiver.getOrElse(o.receiverOpt).get),
          attr = o.attr)

        currType = None()

        return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(invoke))
      case Some(r: RecordType) =>
        val fieldType = r.fields.get(o.id.value).get
        currType = None()
        pushType(fieldType)
        return org.sireum.hamr.ir.MTransformer.PreResult(F, MNone())
      case _ =>
        //halt(
        //  st"""TODO: ${o.prettyST.render}
        //      |      $x""".render)
        return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
    }
  }

  override def pre_langastExpIdent(o: Exp.Ident): irMTransformer.PreResult[Exp] = {
    if (currType.nonEmpty) {
      //println(s"No one consumed '${currType.get.name}' at '${o.prettyST.render}'")
      currType = None()
    }

    // TODO: should use resolveName when possible (currently quantifier params are not resolved)

    /*
    scope.resolveName(typeHierarchy.nameMap, ISZ(o.id.value)) match {
      case Some(n) => println(s"Resolved ${o.prettyST.render}")
      case _ => println(s"Unable to resolve ${o.prettyST.render}")
    }
    */

    context match {
      case Some(component) =>
        // is it a port
        component.getPorts().filter(p => p.identifier == o.id.value) match {
          case ISZ(port) =>
            port match {
              case port: AadlDataPort =>
                pushType(port.aadlType)
                return irMTransformer.PreResult(F, MNone())
              case port: AadlEventDataPort =>
                pushType(port.aadlType)
                return irMTransformer.PreResult(F, MNone())
              case _ => halt("Need to handle user accessing event ports")
            }
          case ISZ() =>
          case _ => halt("Infeasible")
        }

        // is it a state var
        stateVars.filter(s => s.name == o.id.value) match {
          case ISZ(a) =>
            aadlTypes.typeMap.get(a.classifier) match {
              case Some(t) =>
                pushType(t)
                return irMTransformer.PreResult(F, MNone())
              case _ => halt(s"Couldn't resolve ${a.classifier}")
            }
          case ISZ() =>
          case _ => halt("Infeasible")
        }

        // is it a gumbo method invocation
        specFuncs.get(component.classifier :+ o.id.value) match {
          case Some(m) =>
            m.method.sig.returnType match {
              case t: AST.Type.Named =>
                val name = st"${(for (i <- t.name.ids) yield i.value, "::")}".render
                aadlTypes.typeMap.get(name) match {
                  case Some(tpe) =>
                    pushType(tpe)
                    return irMTransformer.PreResult(F, MNone())
                  case _ =>
                    halt(name)
                }
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }

    // is it a param
    ops.ISZOps(params).filter(p => p.id.value == o.id.value) match {
      case ISZ(p) =>
        p.tipe match {
          case n: AST.Type.Named =>
            val typeName: ISZ[String] = for (id <- n.name.ids) yield id.value
            pushType(aadlTypes.getTypeByPath(typeName))
            return irMTransformer.PreResult(F, MNone())
          case _ =>
        }
      case ISZ() =>
      case _ => halt("Infeasible")
    }

    // is it a quantifier param
    ops.ISZOps(quantifierParams).filter(p => p.idOpt.get.value == o.id.value) match {
      case ISZ(p) =>
        assert(p.tipeOpt.isEmpty)
        pushType(aadlTypes.typeMap.get("Base_Types::Integer").get)
        return irMTransformer.PreResult(F, MNone())
      case ISZ() =>
      case _ => halt(st"Infeasible: ${(for (q <- quantifierParams) yield q.idOpt.get.value, ",")}".render)
    }

    scope.resolveName(typeHierarchy.nameMap, ISZ(o.id.value)) match {
      case Some(e: Info.Method) =>
        val n: ISZ[String] = for (id <- e.ast.sig.returnType.asInstanceOf[AST.Type.Named].name.ids) yield id.value
        pushType(aadlTypes.getTypeByPath(n))
        return irMTransformer.PreResult(F, MNone())
      case Some(e) =>
        return irMTransformer.PreResult(F, MNone())
      case _ =>
        halt(s"Could not resolve ${o.id.value}${if(o.posOpt.nonEmpty) s" at ${o.posOpt.get}" else ""}")
    }
  }
}