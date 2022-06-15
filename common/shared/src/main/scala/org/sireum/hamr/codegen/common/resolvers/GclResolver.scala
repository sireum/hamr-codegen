// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.resolvers.GclResolver.{AadlSymbolHolder, toolName}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.lang.ast.{Exp, ResolvedAttr, ResolvedInfo, TypeParam}
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{Info, Resolver, Scope, TypeInfo}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.lang.{ast => AST}
import org.sireum.message.{Position, Reporter, ReporterImpl}

object GclResolver {

  @sig trait SymbolHolder

  @datatype class AadlSymbolHolder(symbol: AadlSymbol) extends SymbolHolder

  @datatype class GclSymbolHolder(symbol: GclStateVar) extends SymbolHolder

  val toolName: String = "GCL-Resolver"

  val libraryReporter: TypeChecker = org.sireum.lang.FrontEnd.libraryReporter._1

  @record class SymbolFinder(val rewriteApiCalls: B, val context: AadlComponent, stateVars: ISZ[GclStateVar], val symbolTable: SymbolTable) extends org.sireum.hamr.ir.MTransformer {
    var symbols: Set[SymbolHolder] = Set.empty
    var reporter: Reporter = ReporterImpl(ISZ())
    var apiReferences: Set[AadlPort] = Set.empty

    def lookup(name: String, optPos: Option[Position]): Option[SymbolHolder] = {
      context match {
        case a: AadlData =>
          val cands = a.subComponents.filter((p: AadlComponent) => p.identifier == name)
          if (cands.isEmpty) {
            reporter.error(optPos, toolName, s"Could not find ${name} in data component ${a.identifier}")
            return None()
          } else if (cands.size > 1) {
            reporter.error(optPos, toolName, s"Found ${cands.size} number of ${name} in data component ${a.identifier}")
            return None()
          } else {
            return Some(AadlSymbolHolder(cands(0)))
          }
        case a: AadlThread =>
          var cands: ISZ[SymbolHolder] = a.getPorts().filter((p: AadlPort) => p.identifier == name)
            .map((p: AadlPort) => AadlSymbolHolder(p))
            .map((f: AadlSymbolHolder) => f.asInstanceOf[SymbolHolder]) // make tipe happy

          if (cands.isEmpty) {
            cands = stateVars.filter((p: GclStateVar) => p.name == name)
              .map((p: GclStateVar) => GclSymbolHolder(p))
              .map((m: GclSymbolHolder) => m.asInstanceOf[SymbolHolder]) // make tipe happy
          }

          if (cands.isEmpty) {
            reporter.error(optPos, toolName, s"Could not find ${name} in thread component ${a.identifier}")
            return None()
          } else if (cands.size > 1) {
            reporter.error(optPos, toolName, s"Found ${cands.size} number of ${name} in thread component ${a.identifier}")
            return None()
          } else {
            return Some(cands(0))
          }
        case x =>
          halt(s"todo ${x}")
      }
    }

    def processIdent(o: Exp.Ident): Option[SymbolHolder] = {
      o.attr.resOpt match {
        case Some(e: AST.ResolvedInfo.Enum) => // ignore
        case Some(e: AST.ResolvedInfo.Package) => // ignore
        case Some(_) =>
          return lookup(o.id.value, o.posOpt)
        case _ => reporter.error(o.posOpt, toolName, s"Ident '$o' did not resolve")
      }
      return None()
    }

    override def pre_langastExpSelect(o: Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[Exp] = {
      if(rewriteApiCalls) {
        o.receiverOpt match {
          case Some(i@Exp.Ident(featureId)) =>
            processIdent(i) match {
              case Some(s) =>
                symbols = symbols + s

                s match {
                  case AadlSymbolHolder(p: AadlPort) if rewriteApiCalls =>
                    apiReferences = apiReferences + p

                      val emptyAttr = AST.Attr(posOpt = o.posOpt)
                      val emptyRAttr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None())

                      val apiIdent: Exp = Exp.Ident(id = AST.Id(value = "api", attr = emptyAttr), attr = emptyRAttr)
                      val apiSelect = Exp.Select(
                        receiverOpt = Some(apiIdent),
                        id = featureId, targs = ISZ(), attr = emptyRAttr)

                      val featureSelect = Exp.Select(receiverOpt = Some(apiSelect),
                        id = o.id, targs = o.targs, attr = o.attr)

                      // don't visit sub children
                      return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(featureSelect))

                  case _ =>
                }
              case _ =>
            }
          case _ =>
        }
      }

      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
    }

    override def post_langastExpIdent(o: Exp.Ident): MOption[Exp] = {
      processIdent(o) match {
        case Some(s) =>
          symbols = symbols + s

          s match {
            case AadlSymbolHolder(p: AadlPort) if rewriteApiCalls =>
              apiReferences = apiReferences + p

              val emptyAttr = AST.Attr(posOpt = o.posOpt)
              val emptyRAttr = AST.ResolvedAttr(posOpt = o.posOpt, resOpt = None(), typedOpt = None())

              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = "api", attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(
                receiverOpt = Some(apiIdent),
                id = o.id, targs = ISZ(), attr = emptyRAttr)

              return MSome(apiSelect)
            case _ =>
          }

        case _ =>
      }
      return MNone()
    }
  }

  def collectSymbols(exp: Exp,
                     rewriteApiCalls: B,
                     context: AadlComponent,
                     stateVars: ISZ[GclStateVar],
                     symbolTable: SymbolTable,
                     reporter: Reporter): (MOption[Exp], ISZ[SymbolHolder], ISZ[AadlPort]) = {
    val sf = SymbolFinder(rewriteApiCalls, context, stateVars, symbolTable)
    val rexp = sf.transform_langastExp(exp)
    reporter.reports(sf.reporter.messages)
    return (rexp, sf.symbols.elements, sf.apiReferences.elements)
  }
}

@record class GclResolver() extends AnnexVisitor {

  var rexprs: HashMap[AST.Exp, AST.Exp] = HashMap.empty
  var apiReferences: Set[AadlPort] = Set.empty
  var computeHandlerPortMap: Map[AST.Exp, AadlPort] = Map.empty
  var integrationMap: Map[AadlPort, GclSpec] = Map.empty

  @memoize def globalImports(symbolTable: SymbolTable): ISZ[AST.Stmt.Import] = {
    // import all AADL package names and org.sireum

    val emptyAttr = AST.Attr(None())

    var set: Set[AST.Stmt.Import.Importer] = Set.empty
    for (a <- symbolTable.componentMap.values if !a.isInstanceOf[AadlSystem]) {
      val classifier = ops.ISZOps(getPathFromClassifier(a.component.classifier.get.name)).dropRight(1)
      val packageName = AST.Name(classifier.map((m: String) => AST.Id(m, emptyAttr)), emptyAttr)
      set = set + AST.Stmt.Import.Importer(packageName, Some(AST.Stmt.Import.WildcardSelector()))
    }

    val sireumImporters: ISZ[AST.Stmt.Import.Importer] = {
      ISZ[String]("S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64").map((m: String) =>
        AST.Stmt.Import.Importer(
          name = AST.Name(
            ids = ISZ[AST.Id](AST.Id("org", emptyAttr), AST.Id("sireum", emptyAttr), AST.Id(m, emptyAttr)),
            attr = emptyAttr),
          selectorOpt = Some(AST.Stmt.Import.WildcardSelector())
        )
      ) :+ AST.Stmt.Import.Importer(
        name = AST.Name(
          ids = ISZ[AST.Id](AST.Id("org", emptyAttr), AST.Id("sireum", emptyAttr)),
          attr = emptyAttr),
        selectorOpt = Some(AST.Stmt.Import.WildcardSelector())
      )
    }

    return (set.elements ++ sireumImporters).map((m: AST.Stmt.Import.Importer) =>
      AST.Stmt.Import(importers = ISZ(m), attr = emptyAttr))
  }

  def fetchSubcomponent(name: Name, context: AadlComponent): Option[AadlComponent] = {
    val n = CommonUtil.getName(name)
    for (sc <- context.subComponents if sc.identifier == n) {
      return Some(sc)
    }
    return None()
  }

  def isSubcomponent(name: Name, context: AadlComponent): B = {
    return !fetchSubcomponent(name, context).isEmpty
  }

  def fetchPort(name: Name, context: AadlComponent): Option[AadlPort] = {
    val n = CommonUtil.getName(name)
    for (p <- context.getPorts() if p.identifier == n) {
      return Some(p)
    }
    return None()
  }

  def isPort(name: Name, context: AadlComponent): B = {
    return !fetchPort(name, context).isEmpty
  }


  def processGclAnnex(context: AadlComponent,
                      annex: GclAnnex,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      typeHierarchy: TypeHierarchy,
                      scope: Scope,
                      reporter: Reporter): Option[GclSymbolTable] = {

    val baseTypeBoolean = aadlTypes.typeMap.get("Base_Types::Boolean").get
    val baseTypeInteger = aadlTypes.typeMap.get("Base_Types::Integer").get
    val baseTypeString = aadlTypes.typeMap.get("Base_Types::String").get
    val baseTypeFloat = aadlTypes.typeMap.get("Base_Types::Float").get
    val baseTypeFloat32 = aadlTypes.typeMap.get("Base_Types::Float_32").get
    val baseTypeFloat64 = aadlTypes.typeMap.get("Base_Types::Float_64").get

    val componentPos = context.component.identifier.pos

    def visitGclSubclause(s: GclSubclause): Unit = {
      var seenInvariantIds: Set[String] = Set.empty

      def typeCheckBoolExp(exp: Exp): Exp = {
        val rexp: AST.Exp = visitSlangExp(exp) match {
          case Some((rexp, roptType)) =>
            roptType match {
              case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
                //rexprs = rexprs + (exp ~> rexp)
                rexp
              case Some(x) =>
                reporter.error(exp.posOpt, GclResolver.toolName, s"Expecting B but found ${x}")
                exp
              case _ =>
                assert(reporter.hasError, "Expression is untyped so Tipe should have reported errors already") // sanity check
                exp
            }
          case _ =>
            reporter.error(exp.posOpt, GclResolver.toolName, "Unexpected: type checking returned none")
            exp
        }
        return rexp
      }

      for (i <- s.invariants) {
        if (seenInvariantIds.contains(i.id)) {
          reporter.error(i.exp.posOpt, GclResolver.toolName, s"Duplicate invariant id: ${i.id}")
        }
        seenInvariantIds = seenInvariantIds + i.id
        visitInvariant(i)
      }

      if (s.integration.nonEmpty) {
        var seenSpecNames: Set[String] = Set.empty
        val gclIntegration = s.integration.get

        for (s <- gclIntegration.specs) {

          if (seenSpecNames.contains(s.id)) {
            reporter.error(s.exp.posOpt, GclResolver.toolName, s"Duplicate spec name: ${s.id}")
          }
          seenSpecNames = seenSpecNames + s.id

          val rexp: AST.Exp = typeCheckBoolExp(s.exp)

          if (!reporter.hasError) {
            val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, F, context, ISZ(), symbolTable, reporter)
            rexprs = rexprs + s.exp ~> rexp
            apiReferences = apiReferences ++ apiRefs

            if (!reporter.hasError) {
              if (symbols.size != 1) {
                reporter.error(s.exp.posOpt, GclResolver.toolName, s"An integration clause must refer to exactly one port")
              } else {
                if(rexp2.nonEmpty) {
                  rexprs = rexprs + s.exp ~> rexp2.get
                }
                symbols(0) match {
                  case AadlSymbolHolder(sym: AadlPort) =>
                    integrationMap = integrationMap + sym ~> s

                    sym.direction match {
                      case Direction.Out =>
                        if (!s.isInstanceOf[GclGuarantee]) {
                          reporter.error(s.exp.posOpt, GclResolver.toolName, s"Integration contracts for outgoing ports must be Guarantee statements")
                        }
                      case Direction.In =>
                        if (!s.isInstanceOf[GclAssume]) {
                          reporter.error(s.exp.posOpt, GclResolver.toolName, s"Integration contracts for incoming ports must be Assume statements")
                        }
                      case x => halt(s"Other phase rejects this case: ${x}")
                    }

                  case x => halt(s"Not expecting ${x}")
                }
              }
            }
          }
        }
      }

      if (s.initializes.nonEmpty) {
        for (modifies <- s.initializes.get.modifies) {
          modifies match {
            case e: Exp.Ident =>
              visitSlangExp(e) match {
                case Some((rexp, roptType)) =>
                  val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, T, context, s.state, symbolTable, reporter)
                  apiReferences = apiReferences ++ apiRefs

                  if (symbols.size != 1) {
                    reporter.error(e.posOpt, GclResolver.toolName, s"Modifies should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                  }

                  if(rexp2.isEmpty) {
                    rexprs = rexprs + e ~> rexp
                  }
                  else {
                    rexprs = rexprs + e ~> rexp2.get
                  }
                case _ => halt("TODO")
              }
            case _ =>
              reporter.error(modifies.posOpt, GclResolver.toolName, s"Expecting modifies to be Idents, found ${modifies}")
          }
        }

        var seenGuaranteeIds: Set[String] = Set.empty
        for (guarantees <- s.initializes.get.guarantees) {
          if (seenGuaranteeIds.contains(guarantees.id)) {
            reporter.error(guarantees.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantees.id}")
          }
          seenGuaranteeIds = seenGuaranteeIds + guarantees.id

          val rexp = typeCheckBoolExp(guarantees.exp)
          val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, T, context, s.state, symbolTable, reporter)
          apiReferences = apiReferences ++ apiRefs

          if(rexp2.isEmpty) {
            rexprs = rexprs + guarantees.exp ~> rexp
          } else {
            rexprs = rexprs + guarantees.exp ~> rexp2.get
          }
        }
      }

      s.compute match {
        case Some(GclCompute(modifies, cases, handlers)) => {
          for (modify <- modifies) {
            modify match {
              case e: Exp.Ident =>
                visitSlangExp(e) match {
                  case Some((rexp, roptType)) =>
                    val (rexp2, symbols, _) = GclResolver.collectSymbols(rexp, F, context, s.state, symbolTable, reporter)
                    if (symbols.size != 1) {
                      reporter.error(e.posOpt, GclResolver.toolName, s"Modifies should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                    }
                    if (rexp2.isEmpty) {
                      rexprs = rexprs + e ~> rexp
                    }
                    else {
                      rexprs = rexprs + e ~> rexp2.get
                    }
                  case _ => halt("TODO")
                }
              case _ =>
                reporter.error(modify.posOpt, GclResolver.toolName, s"Expecting modifies to be Idents, found ${modifies}")
            }
          }

          var seenCaseStmtIds: Set[String] = Set.empty
          for (caase <- cases) {
            if (seenCaseStmtIds.contains(caase.id)) {
              reporter.error(caase.posOpt, GclResolver.toolName, s"Duplicate spec name: ${caase.id}")
            }
            seenCaseStmtIds = seenCaseStmtIds + caase.id

            {
              val rexp = typeCheckBoolExp(caase.assumes)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, T, context, s.state, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              rexprs = rexprs + (caase.assumes ~> rexp)
              if (rexp2.nonEmpty) {
                rexprs = rexprs + (caase.assumes ~> rexp2.get)
              }
            }

            {
              val rexp = typeCheckBoolExp(caase.guarantees)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, T, context, s.state, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              rexprs = rexprs + (caase.guarantees ~> rexp)
              if (rexp2.nonEmpty) {
                rexprs = rexprs + (caase.guarantees ~> rexp2.get)
              }
            }
          }

          for (handler <- handlers) {
            visitSlangExp(handler.port) match {
              case Some((rexp, roptType)) =>
                val (_, symbols, _) = GclResolver.collectSymbols(rexp, F, context, s.state, symbolTable, reporter)
                if (symbols.size != 1) {
                  reporter.error(handler.port.posOpt, GclResolver.toolName, s"Handler should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                }
                symbols(0) match {
                  case AadlSymbolHolder(p: AadlPort) =>
                    computeHandlerPortMap = computeHandlerPortMap + handler.port ~> p
                  case x => reporter.error(handler.port.posOpt, GclResolver.toolName, s"Handler should resolve to an AADL port but received $x")
                }
              case _ => halt(s"TODO: ${handler.port} failed to type check")
            }

            for (modify <- handler.modifies) {
              modify match {
                case e: Exp.Ident =>
                  visitSlangExp(e) match {
                    case Some((rexp, roptType)) =>
                      val (rexp2, symbols, _) = GclResolver.collectSymbols(rexp, F, context, s.state, symbolTable, reporter)
                      if (symbols.size != 1) {
                        reporter.error(e.posOpt, GclResolver.toolName, s"Modifies should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                      }
                      if (rexp2.isEmpty) {
                        rexprs = rexprs + e ~> rexp
                      }
                      else {
                        rexprs = rexprs + e ~> rexp2.get
                      }
                    case _ => halt("TODO")
                  }
                case _ =>
                  reporter.error(modify.posOpt, GclResolver.toolName, s"Expecting modifies to be Idents, found ${modifies}")
              }
            }

            var seenHanlderGuaranteeIds: Set[String] = Set.empty
            for (guarantees <- handler.guarantees) {
              if (seenHanlderGuaranteeIds.contains(guarantees.id)) {
                reporter.error(guarantees.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantees.id}")
              }
              seenHanlderGuaranteeIds = seenHanlderGuaranteeIds + guarantees.id

              val rexp = typeCheckBoolExp(guarantees.exp)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, T, context, s.state, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              if(rexp2.isEmpty) {
                rexprs = rexprs + guarantees.exp ~> rexp
              } else {
                rexprs = rexprs + guarantees.exp ~> rexp2.get
              }
            }
          }
        }
        case Some(x) => reporter.error(componentPos, toolName, s"Expecting GclCompute but received ${x}")
        case _ =>
      }
    }

    def visitInvariant(i: GclInvariant): Unit = {
      visitSlangExp(i.exp) match {
        case Some((rexp, roptType)) =>
          roptType match {
            case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
              val (rexp2, _, _) = GclResolver.collectSymbols(rexp, F, context, ISZ(), symbolTable, reporter)
              if(rexp2.isEmpty) {
                rexprs = rexprs + (i.exp ~> rexp)
              } else {
                rexprs = rexprs + (i.exp ~> rexp2.get)
              }
            case Some(x) => reporter.error(i.exp.posOpt, GclResolver.toolName, s"Expecting B but found ${x}")
            case _ =>
              assert(reporter.hasError, "Invariant expression is untyped so Tipe should have reported errors already")
            //reporter.error(i.exp.posOpt, GclResolver.toolName, "Invariant expression is untyped")
          }
        case _ => reporter.error(i.exp.posOpt, GclResolver.toolName, "Unexpected: type checking returned none")
      }
    }

    def visitSlangExp(exp: AST.Exp): Option[(AST.Exp, Option[AST.Typed])] = {
      val scontext: QName = ISZ("???")
      val mode = TypeChecker.ModeContext.Code
      val typeChecker: TypeChecker = TypeChecker(typeHierarchy, scontext, F, mode, F)

      return Some(typeChecker.checkExp(None(), scope, exp, reporter))
    }

    annex match {
      case g: GclSubclause =>
        visitGclSubclause(g)
        return Some(GclSymbolTable(rexprs, apiReferences.elements, integrationMap, computeHandlerPortMap))
      case x =>
        halt(s"TODO: need to handle gcl annex type: ${x}")
    }
  }

  var seenAnnexes: Set[Annex] = Set.empty
  var typeMap: TypeMap = HashMap.empty
  var globalNameMap: NameMap = HashMap.empty

  @memoize def scope(packageName: IdPath, imports: ISZ[AST.Stmt.Import], enclosingName: IdPath): Scope.Global = {
    return Scope.Global(packageName, imports, enclosingName)
  }

  def getPathFromClassifier(s: String): ISZ[String] = {
    return ops.StringOps(ops.StringOps(s).replaceAllLiterally("::", "|")).split((c: C) => c == '|')
  }

  def buildTypeMap(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): Unit = {

    def getSimpleNameFromClassifier(c: String): String = {
      return ops.ISZOps(getPathFromClassifier(c)).last
    }

    def buildTypeInfo(aadlType: AadlType): TypeInfo = {

      val qualifiedName = getPathFromClassifier(aadlType.name)
      if (typeMap.contains(qualifiedName)) {
        return typeMap.get(qualifiedName).get
      }

      aadlType match {
        case e: EnumType =>

          var elements: Map[String, AST.ResolvedInfo] = Map.empty
          var ordinal: Z = 0
          for (value <- e.values) {
            val ri: ResolvedInfo.EnumElement = ResolvedInfo.EnumElement(qualifiedName, value, ordinal)
            ordinal = ordinal + 1
            elements = elements + (value ~> ri)
          }

          val posOpt: Option[Position] = None()

          val enumx: TypeInfo.Enum = TypeInfo.Enum(qualifiedName, elements, posOpt)

          typeMap = typeMap + (qualifiedName ~> enumx)

          if (!globalNameMap.contains(qualifiedName)) {
            val elementTypedOpt = AST.Typed.Name(ids = qualifiedName :+ "Type", args = ISZ())

            for (elem <- enumx.elements.entries) {
              val ee = elem._2.asInstanceOf[AST.ResolvedInfo.EnumElement]
              globalNameMap = globalNameMap + (qualifiedName :+ ee.name) ~> Info.EnumElement(
                owner = ee.owner,
                id = ee.name,
                typedOpt = Some(elementTypedOpt),
                resOpt = Some(AST.ResolvedInfo.EnumElement(owner = qualifiedName, name = ee.name, ordinal = ee.ordinal)),
                posOpt = None())
            }

            globalNameMap = globalNameMap + qualifiedName ~> Info.Enum(
              name = qualifiedName,
              elements = enumx.elements,
              typedOpt = Some(AST.Typed.Enum(name = qualifiedName)),
              resOpt = Some(AST.ResolvedInfo.Enum(name = qualifiedName)),
              elementTypedOpt = Some(elementTypedOpt),
              posOpt = None())
          }

          return enumx

        case r: RecordType =>
          val component = r.container.get

          val aadlData = symbolTable.componentMap.get(ISZ(component.classifier.get.name)).get

          return buildAdtTypeInfo(aadlData)

        case b: BaseType =>

          val simpleName = getSimpleNameFromClassifier(b.name)
          val packageName = ops.ISZOps(qualifiedName).dropRight(1)
          buildPackageInfo(packageName)

          val imports: ISZ[AST.Stmt.Import] = ISZ()

          val scope: Scope.Global = Scope.Global(packageName, imports, packageName)

          val ast: AST.Stmt.TypeAlias = {
            val _id = AST.Id(simpleName, AST.Attr(None()))
            val typeParams: ISZ[TypeParam] = ISZ()
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

              case x => halt(s"Fix ${x}")
            }
            val tipe: AST.Type = AST.Type.Named(
              name = AST.Name(
                ids = ISZ(AST.Id(simpleSireumName, AST.Attr(None()))),
                attr = AST.Attr(None())
              ),
              typeArgs = ISZ(),
              attr = AST.TypedAttr(
                posOpt = None(),
                typedOpt = Some(AST.Typed.Name(
                  ids = ISZ[String]("org", "sireum") :+ simpleSireumName,
                  args = ISZ()
                ))
              )
            )
            val attr = AST.Attr(None())

            AST.Stmt.TypeAlias(
              id = _id,
              typeParams = typeParams,
              tipe = tipe,
              attr = attr
            )
          }

          val ta = TypeInfo.TypeAlias(
            name = qualifiedName,
            scope = scope,
            ast = ast)

          typeMap = typeMap + (qualifiedName ~> ta)

          return ta

        case TODOType("art::Empty", _, _) =>
          val adtAst: AST.Stmt.Adt =
            AST.Stmt.Adt(
              isRoot = F,
              isDatatype = T,
              id = AST.Id("Empty", AST.Attr(None())),
              typeParams = ISZ(),
              params = ISZ(),
              parents = ISZ(),
              stmts = ISZ(),
              attr = AST.Attr(None()))

          val typeInfoAdt = TypeInfo.Adt(
            owner = ISZ("art"),
            outlined = F,
            contractOutlined = F,
            typeChecked = F,
            tpe = AST.Typed.Name(ISZ("art", "Empty"), ISZ()),
            constructorTypeOpt = None(),
            constructorResOpt = None(),
            extractorTypeMap = Map.empty,
            extractorResOpt = None(),
            ancestors = ISZ(),
            specVars = HashSMap.empty,
            vars = HashSMap.empty,
            specMethods = HashSMap.empty,
            methods = HashSMap.empty,
            refinements = HashSMap.empty,
            invariants = HashSMap.empty,
            dataRefinements = ISZ(),
            scope = scope(ISZ("art"), globalImports(symbolTable), ISZ("art")),
            ast = adtAst)

          typeMap = typeMap + (ISZ("art", "Empty") ~> typeInfoAdt)

          return typeInfoAdt

        case a: ArrayType => halt(s"Not yet handling: $a")
        case b: BitType => halt(s"Not yet handling: $b")
        case x => halt(s"Not yet handling: $x")
      }
    }

    def buildPackageInfo(packageName: ISZ[String]): Info.Package = {
      assert(packageName.size == 1, s"TODO: package name has more than one segment ${packageName}")

      if (!globalNameMap.contains(packageName)) {
        globalNameMap = globalNameMap + packageName ~> Info.Package(
          name = packageName,
          typedOpt = Some(AST.Typed.Package(name = packageName)),
          resOpt = Some(ResolvedInfo.Package(name = packageName))
        )
      }

      return globalNameMap.get(packageName).get.asInstanceOf[Info.Package]
    }

    def buildAdtTypeInfo(a: AadlComponent): TypeInfo.Adt = {
      assert(a.isInstanceOf[AadlThread] || a.isInstanceOf[AadlData])

      val component = a.component
      val adtQualifiedName = getPathFromClassifier(component.classifier.get.name)

      if (typeMap.contains(adtQualifiedName)) {
        return typeMap.get(adtQualifiedName).get.asInstanceOf[TypeInfo.Adt]
      }

      val simpleName = getSimpleNameFromClassifier(component.classifier.get.name)
      val packageName = ops.ISZOps(adtQualifiedName).dropRight(1)
      assert(packageName.nonEmpty)

      buildPackageInfo(packageName)

      // enclosingName is the same as the packageName
      val sc = scope(packageName, globalImports(symbolTable), packageName)

      var paramVars = HashSMap.empty[String, Info.Var]
      var constructorParamVars = ISZ[String]()
      val extractParamVars = ISZ[String]() // always empty as there are no @hidden params in AADL

      val gclAnnexes = component.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])
      assert(gclAnnexes.size <= 1, s"There can be only one")

      // treat stateVars as if they were features for Tipe
      val stateVars: ISZ[AadlPort] = gclAnnexes.flatMap((g: GclSubclause) => g.state.map((sv: GclStateVar) => {
        val aadlType = aadlTypes.typeMap.get(sv.classifier).get
        AadlDataPort(
          feature = FeatureEnd(
            identifier = Name(adtQualifiedName :+ sv.name, sv.posOpt),
            direction = Direction.Out,
            category = FeatureCategory.DataPort,
            classifier = Some(Classifier(sv.classifier)),
            properties = ISZ(),
            uriFrag = ""),
          featureGroupIds = ISZ(),
          direction = Direction.In,
          aadlType = aadlType)
      }))

      val features: ISZ[AadlPort] = {
        a match {
          case at: AadlThread => at.getPorts()
          case ad: AadlData =>
            if (stateVars.nonEmpty) {
              reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have state vars")
            }
            // treat aadl data subcomponents as if they were features for Tipe
            ad.subComponents.map((sc: AadlComponent) => {
              val aadlType = aadlTypes.typeMap.get(sc.component.classifier.get.name).get
              AadlDataPort(feature = FeatureEnd(
                identifier = Name(sc.path, sc.component.identifier.pos),
                direction = Direction.Out,
                category = FeatureCategory.DataPort,
                classifier = sc.component.classifier,
                properties = ISZ(),
                uriFrag = ""),
                featureGroupIds = ISZ(),
                direction = Direction.Out,
                aadlType = aadlType)
            })
          case _ =>
            reporter.error(a.component.identifier.pos, toolName, s"Expecting an AadlThread or AadlData but recevied ${a}")
            ISZ[AadlPort]()
        }
      }

      var adtParams: ISZ[AST.AdtParam] = ISZ()
      for (param <- features ++ stateVars) {
        val paramId: String = param.identifier
        val paramType: AadlType = param match {
          case afd: AadlFeatureData => afd.aadlType
          case _ => TypeUtil.EmptyType
        }

        constructorParamVars = constructorParamVars :+ paramId

        if (paramVars.contains(paramId)) {
          val prev: String = paramVars.get(paramId).get.posOpt match {
            case Some(p) => s". Previously declared at [${p.beginLine},${p.beginColumn}]"
            case _ => ""
          }
          val msg = s"Cannot redeclare feature '$paramId'${prev}"
          reporter.error(param.feature.identifier.pos, toolName, msg)
        }

        val paramResInfoOpt = Some[AST.ResolvedInfo](
          AST.ResolvedInfo.Var(isInObject = F, isSpec = F, isVal = F, owner = adtQualifiedName, id = paramId))

        val paramResolvedType: TypeInfo = buildTypeInfo(paramType)
        val typedOpt: Option[AST.Typed] = paramResolvedType match {
          case ta: TypeInfo.TypeAlias => ta.ast.tipe.typedOpt
          case tadt: TypeInfo.Adt => Some(tadt.tpe)
          case te: TypeInfo.Enum => Some(AST.Typed.Name(ids = te.name, args = ISZ()))
          case x =>
            halt(s"TODO ${x}")
        }

        val varTypeId = AST.Id(value = paramType.name, attr = AST.Attr(None()))
        val varTypeName = AST.Type.Named(
          name = AST.Name(ids = ISZ(varTypeId), attr = AST.Attr(None())),
          typeArgs = ISZ(),
          attr = AST.TypedAttr(posOpt = None(), typedOpt = None())
        )

        paramVars = paramVars + paramId ~> Info.Var(
          owner = adtQualifiedName,
          isInObject = F,
          scope = sc,
          ast = AST.Stmt.Var(
            isSpec = F,
            isVal = F,
            id = AST.Id(value = paramId, attr = AST.Attr(None())),
            tipeOpt = Some(varTypeName),
            initOpt = None(),
            attr = ResolvedAttr(posOpt = param.feature.identifier.pos, resOpt = paramResInfoOpt, typedOpt = typedOpt)
          )
        )

        { // build adt param
          val tipeName: AST.Name = AST.Name(
            ids = paramResolvedType.name.map((m: String) => AST.Id(m, AST.Attr(None()))),
            attr = AST.Attr(None()))

          val typedAttr = AST.TypedAttr(
            posOpt = None(),
            typedOpt = Some(AST.Typed.Name(
              ids = paramResolvedType.name,
              args = ISZ()
            )))

          adtParams = adtParams :+ AST.AdtParam(
            isHidden = F,
            isVal = F,
            id = AST.Id(value = paramId, attr = AST.Attr(None())),
            tipe = AST.Type.Named(
              name = tipeName,
              typeArgs = ISZ(),
              attr = typedAttr
            )
          )
        }
      }

      val constructorTypeOpt: Option[AST.Typed] = None()
      val constructorResOpt: Option[AST.ResolvedInfo] = None()
      val extractorTypeMap: Map[String, AST.Typed] = Map.empty
      val extractorResOpt: Option[AST.ResolvedInfo] = None()
      val ancestors: ISZ[AST.Typed.Name] = ISZ()
      val specVars: HashSMap[String, Info.SpecVar] = HashSMap.empty

      val specMethods: HashSMap[String, Info.SpecMethod] = HashSMap.empty
      val methods: HashSMap[String, Info.Method] = HashSMap.empty
      val refinements: HashSMap[String, TypeInfo.Name] = HashSMap.empty
      val invariants: HashSMap[String, Info.Inv] = HashSMap.empty
      val dataRefinements: ISZ[AST.Stmt.DataRefinement] = ISZ()

      val tpe: AST.Typed.Name = {
        val ids: ISZ[String] = adtQualifiedName
        val args: ISZ[AST.Typed] = ISZ()
        AST.Typed.Name(ids, args)
      }

      val adtAst: AST.Stmt.Adt =
        AST.Stmt.Adt(
          isRoot = F,
          isDatatype = T,
          id = AST.Id(simpleName, AST.Attr(None())),
          typeParams = ISZ(),
          params = adtParams,
          parents = ISZ(),
          stmts = ISZ(),
          attr = AST.Attr(None()))

      val typeInfoAdt = TypeInfo.Adt(
        owner = packageName,
        outlined = F,
        contractOutlined = F,
        typeChecked = F,
        tpe = tpe,
        constructorTypeOpt = constructorTypeOpt,
        constructorResOpt = constructorResOpt,
        extractorTypeMap = extractorTypeMap,
        extractorResOpt = extractorResOpt,
        ancestors = ancestors,
        specVars = specVars,
        vars = paramVars,
        specMethods = specMethods,
        methods = methods,
        refinements = refinements,
        invariants = invariants,
        dataRefinements = dataRefinements,
        scope = sc,
        ast = adtAst)

      typeMap = typeMap + (adtQualifiedName ~> typeInfoAdt)

      return typeInfoAdt
    }

    { // build type info aadl types
      for (aadlType <- aadlTypes.typeMap.values) {
        buildTypeInfo(aadlType)
      }
    }

    { // build type info for aadl threads
      val threadComponents = symbolTable.componentMap.values.filter((m: AadlComponent) => m.isInstanceOf[AadlThread]).map((m: AadlComponent) => m.asInstanceOf[AadlThread])

      for (component <- threadComponents) {
        buildAdtTypeInfo(component)
      }
    }
  }

  def offer(context: AadlComponent, annex: Annex, symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): Option[AnnexInfo] = {
    if (!seenAnnexes.contains(annex)) {
      seenAnnexes = seenAnnexes + annex

      annex.clause match {
        case b: GclSubclause =>
          buildTypeMap(aadlTypes, symbolTable, reporter)

          val qualifiedName: IdPath = context match {
            case ad: AadlData => getPathFromClassifier(ad.component.classifier.get.name)
            case ac: AadlThread => getPathFromClassifier(ac.component.classifier.get.name)
            case _ =>
              reporter.error(None(), toolName, s"Expecting AadlData or AadlThread but passed ${context}")
              return None()
          }

          val scope: Scope = typeMap.get(qualifiedName) match {
            case Some(o) =>
              o match {
                case info: TypeInfo.Adt =>
                  val typeParams = Resolver.typeParamMap(info.ast.typeParams, reporter)
                  var scope = Scope.Local.create(typeParams.map, info.scope)
                  scope = scope(localThisOpt = Some(info.tpe))
                  scope
                case x =>
                  reporter.error(None(), toolName, s"Expecting ${qualifiedName} to resolve to an ADT but found ${x}")
                  return None()
              }
            case _ =>
              reporter.error(None(), toolName, s"Could not resolve type info for: ${qualifiedName}")
              return None()
          }

          val libReporter = GclResolver.libraryReporter

          val typeHierarchy: TypeHierarchy = TypeHierarchy(
            nameMap = globalNameMap ++ libReporter.nameMap.entries,
            typeMap = typeMap ++ libReporter.typeMap.entries,
            poset = Poset.empty,
            aliases = HashMap.empty)

          val gclSymbolTable: GclSymbolTable = processGclAnnex(context, b, symbolTable, aadlTypes, typeHierarchy, scope, reporter).get
          return Some(GclAnnexInfo(b, gclSymbolTable))
        case _ =>
      }
    }
    return None()
  }
}
