// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.resolvers.GclResolver.AadlSymbolHolder
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlData, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureData, AadlPort, AadlSymbol, AadlThread, AnnexInfo, AnnexVisitor, GclAnnexInfo, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, BaseType, BitType, EnumType, RecordType, TypeUtil}
import org.sireum.hamr.ir.{Annex, Direction, GclAnnex, GclAssume, GclGuarantee, GclInvariant, GclSpec, GclStateVar, GclSubclause, Name}
import org.sireum.lang.FrontEnd.libraryReporter
import org.sireum.lang.ast.{AdtParam, Exp, ResolvedAttr, ResolvedInfo, TypeParam}
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{Info, Scope, TypeInfo}
import org.sireum.lang.symbol.Scope.Local
import org.sireum.lang.{ast => AST}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.message.{Position, Reporter, ReporterImpl}

object GclResolver {

  @sig trait SymbolHolder
  @datatype class AadlSymbolHolder(symbol: AadlSymbol) extends SymbolHolder
  @datatype class GclSymbolHolder(symbol: GclStateVar) extends SymbolHolder

  val toolName: String = "GCL-Resolver"

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
          } else  if(cands.size > 1) {
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
      lookup(o.id.value, o.posOpt) match {
        case Some(s) => symbols = symbols + s
        case _ =>
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

  var symbolRecorder: Stack[Set[AadlPort]] = Stack.empty
  def symbolRecorderPush(a: AadlPort): Unit = {
    if(symbolRecorder.nonEmpty) {
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
      var seenInvariantNames: Set[String] = Set.empty

      for (i <- s.invariants) {
        if (seenInvariantNames.contains(i.name)) {
          reporter.error(i.exp.posOpt, GclResolver.toolName, s"Duplicate invariant name: ${i.name}")
        }
        seenInvariantNames = seenInvariantNames + i.name
        visitInvariant(i)
      }

      if (s.integration.nonEmpty) {
        var seenSpecNames: Set[String] = Set.empty
        val gclIntegration = s.integration.get

        for (s <- gclIntegration.specs) {

          if (seenSpecNames.contains(s.name)) {
            reporter.error(s.exp.posOpt, GclResolver.toolName, s"Duplicate spec name: ${s.name}")
          }
          seenSpecNames = seenSpecNames + s.name

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

            if(!reporter.hasError) {
              if (symbols.size != 1) {
                reporter.error(s.exp.posOpt, GclResolver.toolName, s"An integration clause must refer to exactly one port")
              } else {
                symbols(0) match {
                  case AadlSymbolHolder(sym : AadlPort) =>
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

      for(initClause <- s.initializes){
        val exp = initClause.exp
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
  var seenTypes: Map[AadlType, TypeInfo] = Map.empty
  var typeMap: TypeMap = HashMap.empty


  def getPathFromClassifier(s: String): ISZ[String]= {
    return ops.StringOps(ops.StringOps(s).replaceAllLiterally("::", "|")).split((c: C) => c == '|')
  }

  def getSimpleNameFromClassifier(c: String): String = {
    return ops.ISZOps(getPathFromClassifier(c)).last
  }

  def buildTypeMap(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): TypeMap = {

    def resolveType(aadlType: AadlType): TypeInfo = {

      if (seenTypes.contains(aadlType)) {
        return seenTypes.get(aadlType).get
      }

      aadlType match {
        case e: EnumType =>
          val qualifiedName = getPathFromClassifier(e.name)

          var elements: Map[String, AST.ResolvedInfo] = Map.empty
          var ordinal: Z = 0
          for(value <- e.values){
            val ri: ResolvedInfo.EnumElement = ResolvedInfo.EnumElement(qualifiedName, value, ordinal)
            ordinal = ordinal + 1
            elements = elements + (value ~> ri)
          }

          val posOpt: Option[Position] = None()

          val enumx: TypeInfo.Enum = TypeInfo.Enum(qualifiedName, elements, posOpt)

          typeMap = typeMap + (qualifiedName ~> enumx)
          seenTypes = seenTypes + (aadlType ~> enumx)

          return enumx

        case r: RecordType =>
          val component = r.container.get
          val qualifiedName = getPathFromClassifier(component.classifier.get.name)

          val owner = ops.ISZOps(qualifiedName).drop(1)
          val simpleName = getSimpleNameFromClassifier(component.classifier.get.name)

          val outlined = T
          val contractOutlined = T
          val typeChecked = T

          val tpe: AST.Typed.Name = {
            val ids: ISZ[String] = qualifiedName
            val args: ISZ[AST.Typed] = ISZ()
            AST.Typed.Name(ids, args)
          }

          val constructorTypeOpt: Option[AST.Typed] = None()
          val constructorResOpt:Option[AST.ResolvedInfo] = None()
          val extractorTypeMap: Map[String, AST.Typed] = Map.empty
          val extractorResOpt: Option[AST.ResolvedInfo] = None()
          val ancestors: ISZ[AST.Typed.Name] = ISZ()
          val specVars: HashSMap[String, Info.SpecVar] = HashSMap.empty

          var adtParams: ISZ[AST.AdtParam] = ISZ()
          val vars: HashSMap[String, Info.Var] = {
            var map: HashSMap[String, Info.Var] = HashSMap.empty
            for(field <- r.fields.entries) {
              val fieldName = field._1
              val fieldType = field._2

              val fResolvedType: TypeInfo = resolveType(fieldType)
              val typedOpt: Option[AST.Typed] = fResolvedType match {
                case ta: TypeInfo.TypeAlias => ta.ast.tipe.typedOpt
                case tadt: TypeInfo.Adt => Some(tadt.tpe)
                case x =>
                  halt(s"TODO ${x}")
              }

              val varTypeId = AST.Id(value = fieldType.name, attr = AST.Attr(None()))
              val varTypeName = AST.Type.Named(
                name = AST.Name(ids = ISZ(varTypeId), attr = AST.Attr(None())),
                typeArgs = ISZ(),
                attr = AST.TypedAttr(posOpt = None(), typedOpt = None())
              )

              val varId = AST.Id(value = fieldName, attr = AST.Attr(None()))
              val varAst = AST.Stmt.Var(
                isSpec = F,
                isVal = T,
                id = varId,
                tipeOpt = Some(varTypeName),
                initOpt = None(),
                attr = ResolvedAttr(posOpt = None(), resOpt = None(), typedOpt = typedOpt)
              )

              val varScope = Scope.Global(
                packageName = owner,
                imports = ISZ(),
                enclosingName = owner
              )

              val v = Info.Var(
                owner = qualifiedName,
                isInObject = F,
                scope = varScope,
                ast = varAst
              )

              { // build adt param
                val fieldTypeId = fieldType.container.get.identifier
                val resolvedTypesName = fResolvedType.name

                val tipeName: AST.Name = AST.Name(
                  ids = fieldTypeId.name.map(m => AST.Id(m, AST.Attr(None()))),
                  attr = AST.Attr(None()))

                val typedAttr = AST.TypedAttr(
                  posOpt = None(),
                  typedOpt = Some(AST.Typed.Name(
                    ids = resolvedTypesName,
                    args = ISZ()
                  )))

                adtParams = adtParams :+ AST.AdtParam(
                  isHidden = F,
                  isVal = T,
                  id = varId,
                  tipe = AST.Type.Named(
                    name = tipeName,
                    typeArgs = ISZ(),
                    attr = typedAttr
                  )
                )
              }
              map = map + (fieldName ~> v)
            }
            map
          }

          val specMethods: HashSMap[String, Info.SpecMethod] = HashSMap.empty
          val methods: HashSMap[String, Info.Method] = HashSMap.empty
          val refinements: HashSMap[String, TypeInfo.Name] = HashSMap.empty
          val invariants: HashSMap[String, Info.Inv] = HashSMap.empty
          val dataRefinements: ISZ[AST.Stmt.DataRefinement] = ISZ()

          val adtScope: Scope.Global = {
            val packageName: ISZ[String] = owner
            val imports: ISZ[AST.Stmt.Import] = ISZ()
            val enclosingName: ISZ[String] = owner

            Scope.Global(packageName, imports, enclosingName)
          }

          val adtAst: AST.Stmt.Adt = {
            val isRoot: B = F
            val isDatatype: B = T
            val id = AST.Id(simpleName, AST.Attr(None()))
            val typeParams: ISZ[TypeParam] = ISZ()
            val params: ISZ[AdtParam] = ISZ()
            val parents: ISZ[AST.Type.Named] = ISZ()
            val stmts: ISZ[AST.Stmt] = ISZ()
            val attr: AST.Attr = AST.Attr(None())

            AST.Stmt.Adt(isRoot, isDatatype, id, typeParams, params, parents, stmts, attr)
          }

          val adt = TypeInfo.Adt(owner, outlined, contractOutlined, typeChecked, tpe, constructorTypeOpt,
            constructorResOpt, extractorTypeMap, extractorResOpt, ancestors, specVars,
            vars, specMethods, methods, refinements, invariants, dataRefinements, adtScope, adtAst)

          typeMap = typeMap + (qualifiedName ~> adt)
          seenTypes = seenTypes + (aadlType ~> adt)

          return adt

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

    for(entry <- aadlTypes.typeMap.entries) {
      val name: String = entry._1
      val aadlType: AadlType = entry._2

      resolveType(aadlType)
    }

    typeMap = typeMap + (ISZ("art", "Empty") ~> typeMap.get(ISZ("Base_Types", "Boolean")).get)

    return typeMap
  }

  def buildNameMap(context: AadlComponent): (HashMap[String,Info], NameMap) = {
    var globalNameMap: HashMap[ISZ[String], Info] = HashMap.empty
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
        val componentQualifiedName = a.component.identifier.name

        for(sc <- a.subComponents) {
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
              globalNameMap = globalNameMap + (ISZ(scA.identifier) ~> infoVar)

            case x => halt(s"Not yet handling data subcomponents of type ${x}")
          }
        }
      case a: AadlThread =>

        for(aadlPort <- context.getPorts()) {
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

          val infoVar = buildGlobalVar(aadlPort.identifier,  qualifiedTypeName, a.path)

          localNameMap = localNameMap + (aadlPort.identifier ~> infoVar)
          globalNameMap = globalNameMap + (aadlPort.path ~> infoVar)
        }
      case x => halt(s"Not expecting ${x}")
    }

    return (localNameMap, globalNameMap)
  }


  def buildGlobalVar(identifier: String, qualifiedTypeName: ISZ[String], owner: ISZ[String]): Info.Var = {
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
      isInObject =  T,
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
  def buildSlangTypeInfo(context: AadlComponent,
                         stateVars: ISZ[GclStateVar],
                         symbolTable: SymbolTable,
                         aadlTypes: AadlTypes,
                         reporter: Reporter): (TypeHierarchy, Scope) = {

    val poset: Poset[QName] = Poset.empty
    val aliases: HashMap[QName, AST.Typed] = HashMap.empty

    val _typeMap = buildTypeMap(aadlTypes, symbolTable, reporter)
    var (localNameMap, globalNameMap) = buildNameMap(context)

    val methodReturnOpt: Option[AST.Typed] = None()
    val indexMap: HashMap[String, AST.Typed] = HashMap.empty
    val scopeTypeMap: HashMap[String, TypeInfo] = HashMap.empty

    val emptyAttr = AST.Attr(None())

    val importers: ISZ[AST.Stmt.Import.Importer] = {
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

    val imports= AST.Stmt.Import(importers = importers, attr = emptyAttr)

    val outerOpt: Scope = Scope.Global(
      packageName = ISZ[String]("placeholder"),
      imports = ISZ(imports),
      enclosingName = ISZ[String]("placeholder")
    )

    val scope: Scope = context match {
      case a: AadlData =>
        // "this" is the datatype itself
        val qTypeName = getPathFromClassifier(a.typ.name)
        val localThisOp: Option[AST.Typed] = Some(AST.Typed.Name(ids = qTypeName, args = ISZ()))

        Local(localNameMap, scopeTypeMap, localThisOp, methodReturnOpt, indexMap, Some(outerOpt))
      case a: AadlThread =>
        val localThisOp: Option[AST.Typed] = Some(AST.Typed.Object(owner = a.parent, id = a.identifier))

        for(svd <- stateVars) {
          val name = a.path :+ svd.name
          val typeName = getPathFromClassifier(svd.classifier)
          val gv = buildGlobalVar(svd.name, typeName, a.path)

          localNameMap = localNameMap + (svd.name ~> gv)
          globalNameMap = globalNameMap + (name ~> gv)
        }

        Local(localNameMap, scopeTypeMap, localThisOp, methodReturnOpt, indexMap, Some(outerOpt))
      case x => halt(s"Not expected ${x}")
    }

    val th = libraryReporter._1.typeHierarchy


    val typeHierarchy: TypeHierarchy = TypeHierarchy(globalNameMap ++ th.nameMap.entries, _typeMap ++ th.typeMap.entries,
      poset, aliases)

    return (typeHierarchy, scope)
  }


  def offer(context: AadlComponent, annex: Annex, symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): Option[AnnexInfo] = {
    if (!seenAnnexes.contains(annex)) {
      seenAnnexes = seenAnnexes + annex

      annex.clause match {
        case b: GclSubclause =>

          val (th, scope): (TypeHierarchy, Scope) = buildSlangTypeInfo(context, b.state, symbolTable, aadlTypes, reporter)
          val gclSymbolTable: GclSymbolTable = processGclAnnex(context, b, symbolTable, aadlTypes, th, scope, reporter).get
          return Some(GclAnnexInfo(b, gclSymbolTable))
        case _ =>
      }
    }
    return None()
  }
}
