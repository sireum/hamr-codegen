// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.resolvers.GclResolver.{AadlSymbolHolder, toolName}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlData, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureData, AadlPort, AadlSymbol, AadlSystem, AadlThread, AnnexInfo, AnnexVisitor, GclAnnexInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, BaseType, BitType, EnumType, RecordType, TypeUtil}
import org.sireum.hamr.ir.{Annex, Classifier, Direction, FeatureCategory, FeatureEnd, GclAnnex, GclAssume, GclGuarantee, GclInvariant, GclSpec, GclStateVar, GclSubclause, Name}
import org.sireum.lang.ast.{AdtParam, Exp, ResolvedAttr, ResolvedInfo, TypeParam}
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{Info, Resolver, Scope, TypeInfo}
import org.sireum.lang.symbol.Scope.Local
import org.sireum.lang.{ast => AST}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.message.{Position, Reporter, ReporterImpl}

object GclResolver {

  @sig trait SymbolHolder

  @datatype class AadlSymbolHolder(symbol: AadlSymbol) extends SymbolHolder

  @datatype class GclSymbolHolder(symbol: GclStateVar) extends SymbolHolder

  val toolName: String = "GCL-Resolver"

  val libraryReporter: TypeChecker = org.sireum.lang.FrontEnd.libraryReporter._1

  @record class SymbolFinder(val context: AadlComponent, stateVars: ISZ[GclStateVar], val symbolTable: SymbolTable) extends org.sireum.hamr.ir.MTransformer {
    var symbols: Set[SymbolHolder] = Set.empty
    var reporter: Reporter = ReporterImpl(ISZ())

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

    override def post_langastExpIdent(o: Exp.Ident): MOption[Exp] = {
      o.attr.resOpt match {
        case Some(e: AST.ResolvedInfo.Enum) => // ignore
        case Some(_) =>
          lookup(o.id.value, o.posOpt) match {
            case Some(s) => symbols = symbols + s
            case _ =>
          }
        case _ => reporter.error(o.posOpt, toolName, s"Ident '$o' did not resolve")
      }
      return MNone()
    }
  }

  def collectSymbols(exp: Exp,
                     context: AadlComponent,
                     stateVars: ISZ[GclStateVar],
                     symbolTable: SymbolTable,
                     reporter: Reporter): ISZ[SymbolHolder] = {
    val sf = SymbolFinder(context, stateVars, symbolTable)
    sf.transform_langastExp(exp)
    reporter.reports(sf.reporter.messages)
    return sf.symbols.elements
  }
}

@record class GclResolver() extends AnnexVisitor {

  var rexprs: HashMap[AST.Exp, AST.Exp] = HashMap.empty
  var specPort: Map[GclSpec, AadlPort] = Map.empty

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
      AST.Stmt.Import(importers = ISZ(m), emptyAttr))
  }

  var symbolRecorder: Stack[Set[AadlPort]] = Stack.empty

  def symbolRecorderPush(a: AadlPort): Unit = {
    if (symbolRecorder.nonEmpty) {
      val (set, stack) = symbolRecorder.pop.get
      symbolRecorder = stack.push(set + a)
    }
  }

  def symbolRecorderStart(): Z = {
    symbolRecorder = symbolRecorder.push(Set.empty)
    return symbolRecorder.size
  }

  def symbolRecorderStop(): Set[AadlPort] = {
    val (set, stack) = symbolRecorder.pop.get
    symbolRecorder = stack
    return set
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

    def visitGclSubclause(s: GclSubclause): Unit = {
      var seenInvariantIds: Set[String] = Set.empty

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

          val rexp: AST.Exp = visitSlangExp(s.exp) match {
            case Some((rexp, roptType)) =>
              roptType match {
                case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
                  rexprs = rexprs + (s.exp ~> rexp)
                  rexp
                case Some(x) =>
                  reporter.error(s.exp.posOpt, GclResolver.toolName, s"Expecting B but found ${x}")
                  s.exp
                case _ =>
                  assert(reporter.hasError, "Integration expression is untyped so Tipe should have reported errors already") // sanity check
                  //reporter.error(s.exp.posOpt, GclResolver.toolName, "Integration expression is untyped")
                  s.exp
              }
            case _ =>
              reporter.error(s.exp.posOpt, GclResolver.toolName, "Unexpected: type checking returned none")
              s.exp
          }

          if (!reporter.hasError) {
            val symbols = GclResolver.collectSymbols(rexp, context, ISZ(), symbolTable, reporter)

            if (!reporter.hasError) {
              if (symbols.size != 1) {
                reporter.error(s.exp.posOpt, GclResolver.toolName, s"An integration clause must refer to exactly one port")
              } else {
                symbols(0) match {
                  case AadlSymbolHolder(sym: AadlPort) =>
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

                    specPort = specPort + (s ~> sym)

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
                  val symbols = GclResolver.collectSymbols(rexp, context, s.state, symbolTable, reporter)
                  if (symbols.size != 1) {
                    reporter.error(e.posOpt, GclResolver.toolName, s"Modifies should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                  }
                case _ => halt("TODO")
              }
            case _ =>
              reporter.error(modifies.posOpt, GclResolver.toolName, s"Expecting modifies to be Idents, found ${modifies}")
          }
        }

        for (guarantees <- s.initializes.get.guarantees) {
          val exp = guarantees.exp
          visitSlangExp(exp) match {
            case Some((rexp, roptType)) =>
              roptType match {
                case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
                  rexprs = rexprs + (exp ~> rexp)
                  val symbols = GclResolver.collectSymbols(rexp, context, s.state, symbolTable, reporter)
                case Some(x) => reporter.error(exp.posOpt, GclResolver.toolName, s"Expecting B but found ${x}")
                case _ =>
                  assert(reporter.hasError, "Guarantee express is untyped so Tipe should have reported errors already") // sanity check
                //reporter.error(exp.posOpt, GclResolver.toolName, "Guarantee expression is untyped")
              }
            case _ => reporter.error(exp.posOpt, GclResolver.toolName, "Unexpected: type checking returned none")
          }
        }
      }

      assert(s.compute.isEmpty, "not yet")
    }

    def visitInvariant(i: GclInvariant): Unit = {
      visitSlangExp(i.exp) match {
        case Some((rexp, roptType)) =>
          roptType match {
            case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
              rexprs = rexprs + (i.exp ~> rexp)
              val symbols = GclResolver.collectSymbols(rexp, context, ISZ(), symbolTable, reporter)
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
        return Some(GclSymbolTable(rexprs, specPort))
      case x =>
        halt(s"TODO: need to handle gcl annex type: ${x}")
    }
  }

  var seenAnnexes: Set[Annex] = Set.empty
  var typeMap: TypeMap = HashMap.empty
  var globalNameMap: NameMap = HashMap.empty

  def getPathFromClassifier(s: String): ISZ[String] = {
    return ops.StringOps(ops.StringOps(s).replaceAllLiterally("::", "|")).split((c: C) => c == '|')
  }

  def getSimpleNameFromClassifier(c: String): String = {
    return ops.ISZOps(getPathFromClassifier(c)).last
  }

  @memoize def scope(packageName: IdPath, imports: ISZ[AST.Stmt.Import], enclosingName: IdPath): Scope.Global = {
    return Scope.Global(packageName, imports, enclosingName)
  }

  def addEnumToGlobalNameMap(qualifiedName: ISZ[String], tienum: TypeInfo.Enum): Unit = {
    if (globalNameMap.contains(qualifiedName)) {
      return
    }
    val elementTypedOpt = AST.Typed.Name(ids = qualifiedName :+ "Type", args = ISZ())

    for (elem <- tienum.elements.entries) {
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
      elements = tienum.elements,
      typedOpt = Some(AST.Typed.Enum(name = qualifiedName)),
      resOpt = Some(AST.ResolvedInfo.Enum(name = qualifiedName)),
      elementTypedOpt = Some(elementTypedOpt),
      posOpt = None())
  }

  def buildTypeMap(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): Unit = {
    var seenTypes: Map[AadlType, TypeInfo] = Map.empty

    def resolveType(aadlType: AadlType): TypeInfo = {

      if (seenTypes.contains(aadlType)) {
        return seenTypes.get(aadlType).get
      }

      aadlType match {
        case e: EnumType =>
          val qualifiedName = getPathFromClassifier(e.name)
          assert(qualifiedName.nonEmpty)

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
          seenTypes = seenTypes + (aadlType ~> enumx)

          addEnumToGlobalNameMap(qualifiedName, enumx)

          return enumx

        case r: RecordType =>
          val component = r.container.get

          val adtQualifiedName = getPathFromClassifier(component.classifier.get.name)
          val simpleName = getSimpleNameFromClassifier(component.classifier.get.name)
          val packageName = ops.ISZOps(adtQualifiedName).dropRight(1)
          assert(packageName.nonEmpty)

          // enclosingName is the same as the packageName
          val sc = scope(packageName, globalImports(symbolTable), packageName)

          var paramVars = HashSMap.empty[String, Info.Var]
          var constructorParamVars = ISZ[String]()
          val extractParamVars = ISZ[String]() // always empty as there are no @hidden params in AADL

          var adtParams: ISZ[AST.AdtParam] = ISZ()
          for (param <- r.fields.entries) {
            val paramId: String = param._1
            val paramType: AadlType = param._2

            constructorParamVars = constructorParamVars :+ paramId

            if (paramVars.contains(paramId)) {
              reporter.error(None(), toolName, s"Cannot redeclare parameter '$paramId'.")
            }

            val paramResInfoOpt = Some[AST.ResolvedInfo](
              AST.ResolvedInfo.Var(isInObject = F, isSpec = F, isVal = F, owner = adtQualifiedName, id = paramId))

            val paramResolvedType: TypeInfo = resolveType(paramType)
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
                attr = ResolvedAttr(posOpt = None(), resOpt = paramResInfoOpt, typedOpt = typedOpt)
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
          seenTypes = seenTypes + (aadlType ~> typeInfoAdt)

          return typeInfoAdt

        case b: BaseType =>
          val qualifiedName = getPathFromClassifier(b.name)
          val simpleName = getSimpleNameFromClassifier(b.name)

          val imports: ISZ[AST.Stmt.Import] = ISZ()
          val enclosingName: ISZ[String] = qualifiedName

          val scope: Scope.Global = Scope.Global(qualifiedName, imports, enclosingName)

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
          seenTypes = seenTypes + (aadlType ~> ta)

          return ta

        case a: ArrayType => halt(s"Not yet handling: $a")
        case b: BitType => halt(s"Not yet handling: $b")
        case x => halt(s"Not yet handling: $x")

      }
    }

    def buildAdtTypeInfo(a: AadlComponent): TypeInfo.Adt = {
      assert(a.isInstanceOf[AadlThread] || a.isInstanceOf[AadlData])

      val component = a.component

      val adtQualifiedName = getPathFromClassifier(component.classifier.get.name)
      val simpleName = getSimpleNameFromClassifier(component.classifier.get.name)
      val packageName = ops.ISZOps(adtQualifiedName).dropRight(1)
      assert(packageName.nonEmpty)

      // enclosingName is the same as the packageName
      val sc = scope(packageName, globalImports(symbolTable), packageName)

      var paramVars = HashSMap.empty[String, Info.Var]
      var constructorParamVars = ISZ[String]()
      val extractParamVars = ISZ[String]() // always empty as there are no @hidden params in AADL

      val gclAnnexes = component.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])
      assert(gclAnnexes.size <= 1, s"There can be only one")

      // convert stateVars into ports
      val stateVars: ISZ[AadlPort] = gclAnnexes.flatMap((g: GclSubclause) => g.state.map((sv: GclStateVar) => {
        val aadlType = aadlTypes.typeMap.get(sv.classifier).get
        AadlDataPort(feature = FeatureEnd(
          identifier = Name(adtQualifiedName :+ sv.name, None()),
          direction = Direction.In,
          category = FeatureCategory.DataPort,
          classifier = Some(Classifier(sv.classifier)),
          properties = ISZ(),
          uriFrag = ""),
          featureGroupIds = ISZ(),
          direction = Direction.In,
          aadlType = aadlType)
      }))

      var adtParams: ISZ[AST.AdtParam] = ISZ()
      for (param <- a.getPorts() ++ stateVars) {
        val paramId: String = param.identifier
        val paramType: AadlType = param match {
          case afd: AadlFeatureData => afd.aadlType
          case _ => halt("not handling event ports yet")
        }

        constructorParamVars = constructorParamVars :+ paramId

        if (paramVars.contains(paramId)) {
          reporter.error(None(), toolName, s"Cannot redeclare parameter '$paramId'.")
        }

        val paramResInfoOpt = Some[AST.ResolvedInfo](
          AST.ResolvedInfo.Var(isInObject = F, isSpec = F, isVal = F, owner = adtQualifiedName, id = paramId))

        val paramResolvedType: TypeInfo = resolveType(paramType)
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
            attr = ResolvedAttr(posOpt = None(), resOpt = paramResInfoOpt, typedOpt = typedOpt)
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

    def processTypes(): Unit = {

      for (entry <- aadlTypes.typeMap.entries) {
        val name: String = entry._1
        val aadlType: AadlType = entry._2

        resolveType(aadlType)
      }

      typeMap = typeMap + (ISZ("art", "Empty") ~> typeMap.get(ISZ("Base_Types", "Boolean")).get)
    }

    processTypes()

    def processThreads(): Unit = {
      var seenComponentType: Set[ISZ[String]] = Set.empty

      val threadComponents = symbolTable.componentMap.values.filter((m: AadlComponent) => m.isInstanceOf[AadlThread]).map((m: AadlComponent) => m.asInstanceOf[AadlThread])

      for (component <- threadComponents) {
        val typeName = getPathFromClassifier(component.component.classifier.get.name)
        if (!seenComponentType.contains(typeName)) {
          seenComponentType = seenComponentType + typeName

          buildAdtTypeInfo(component)
        }
      }
    }

    processThreads()
  }

  def buildNameMap(context: AadlComponent): (HashMap[String, Info]) = {
    var localNameMap: HashMap[String, Info] = HashMap.empty

    val emptyLocalScope: Local = Local(
      nameMap = HashMap.empty,
      typeMap = HashMap.empty,
      localThisOpt = None(),
      methodReturnOpt = None(),
      indexMap = HashMap.empty,
      outerOpt = None()
    )

    context match {
      case a: AadlData =>
        val componentQualifiedName = getPathFromClassifier(a.typ.name)
        assert(componentQualifiedName.nonEmpty)

        for (sc <- a.subComponents) {
          sc match {
            case scA: AadlData =>
              val qTypeName = getPathFromClassifier(scA.typ.name)
              val resolvedType = typeMap.get(qTypeName).get

              val tipe = AST.Type.Named(
                name = AST.Name(
                  ids = ISZ(AST.Id(value = scA.identifier, attr = AST.Attr(None()))),
                  attr = AST.Attr(None())),
                typeArgs = ISZ(),
                attr = AST.TypedAttr(
                  posOpt = None(),
                  typedOpt = Some(AST.Typed.Name(
                    ids = qTypeName,
                    args = ISZ()
                  ))
                )
              )

              val astResAttrTyped = AST.Typed.Name(ids = ISZ(scA.identifier), args = ISZ())

              val ast = AST.Stmt.Var(
                isSpec = F,
                isVal = T,
                id = AST.Id(value = sc.identifier, attr = AST.Attr(None())),
                tipeOpt = Some(tipe),
                initOpt = None(),
                attr = AST.ResolvedAttr(posOpt = None(), resOpt = None(), typedOpt = Some(astResAttrTyped))
              )

              val infoVar = Info.Var(
                owner = componentQualifiedName,
                isInObject = T,
                scope = emptyLocalScope,
                ast = ast
              )

              localNameMap = localNameMap + (scA.identifier ~> infoVar)

            case x => halt(s"Not yet handling data subcomponents of type ${x}")
          }
        }
      case a: AadlThread =>

        for (aadlPort <- context.getPorts()) {
          val portName = aadlPort.identifier
          val qualifiedName = aadlPort.path

          val (qualifiedTypeName, resolvedType): (ISZ[String], TypeInfo) = aadlPort match {
            case a: AadlDataPort =>
              val portType = a.aadlType
              val _qualifiedTypeName = getPathFromClassifier(portType.name)
              val typeInfo = typeMap.get(_qualifiedTypeName).get

              (_qualifiedTypeName, typeInfo)
            case a: AadlEventDataPort =>
              val portType = a.aadlType
              val _qualifiedTypeName = getPathFromClassifier(portType.name)
              val typeInfo = typeMap.get(_qualifiedTypeName).get

              (_qualifiedTypeName, typeInfo)

            case a: AadlEventPort =>
              val emptyType = TypeUtil.EmptyType
              val _qualifiedTypeName = getPathFromClassifier(emptyType.name)
              val typeInfo = typeMap.get(_qualifiedTypeName).get

              (_qualifiedTypeName, typeInfo)

            case x => halt(s"Not currently supported ${x}")

          }

          val infoVar = buildGlobalVar(aadlPort.identifier, qualifiedTypeName, a.path)

          localNameMap = localNameMap + (aadlPort.identifier ~> infoVar)
        }
      case x => halt(s"Not expecting ${x}")
    }

    return (localNameMap)
  }

  def buildGlobalVar(identifier: String, qualifiedTypeName: ISZ[String], owner: ISZ[String]): Info.Var = {
    assert(qualifiedTypeName.nonEmpty, s"qualifiedTypeName is empty for $identifier")
    assert(owner.nonEmpty, s"owner is empty for $identifier")

    val emptyLocalScope: Local = Local(
      nameMap = HashMap.empty,
      typeMap = HashMap.empty,
      localThisOpt = None(),
      methodReturnOpt = None(),
      indexMap = HashMap.empty,
      outerOpt = None()
    )

    val tipe = AST.Type.Named(
      name = AST.Name(
        ids = ISZ(AST.Id(value = identifier, attr = AST.Attr(None()))),
        attr = AST.Attr(None())),
      typeArgs = ISZ(),
      attr = AST.TypedAttr(
        posOpt = None(),
        typedOpt = Some(AST.Typed.Name(
          ids = qualifiedTypeName,
          args = ISZ()
        ))
      )
    )

    val astResAttrTyped = AST.Typed.Name(ids = qualifiedTypeName, args = ISZ())
    val astAttrResOpt = ResolvedInfo.Var(
      isInObject = T,
      isSpec = F,
      isVal = F,
      owner = owner,
      id = identifier
    )

    val ast = AST.Stmt.Var(
      isSpec = F,
      isVal = T,
      id = AST.Id(value = identifier, attr = AST.Attr(None())),
      tipeOpt = Some(tipe),
      initOpt = None(),
      attr = AST.ResolvedAttr(posOpt = None(), resOpt = Some(astAttrResOpt), typedOpt = Some(astResAttrTyped))
    )

    val infoVar = Info.Var(
      owner = owner,
      isInObject = T,
      scope = emptyLocalScope,
      ast = ast
    )

    return infoVar
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
                  reporter.error(None(), toolName, s"Expecting component to resolve to an ADT but found ${x}")
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
