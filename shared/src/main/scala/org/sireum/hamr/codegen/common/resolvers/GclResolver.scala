// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.TypeUtil.EmptyType
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.{GclUtil, NameUtil}
import org.sireum.hamr.ir._
import org.sireum.lang.ast.MethodContract.Simple
import org.sireum.lang.ast.{Exp, ResolvedAttr, ResolvedInfo, TypeParam}
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap}
import org.sireum.lang.symbol.{Info, Resolver, Scope, TypeInfo}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.lang.{ast => AST}
import org.sireum.message.{Position, Reporter, ReporterImpl}

object GclResolver {

  @sig trait SymbolHolder

  @datatype class AadlSymbolHolder(symbol: AadlSymbol) extends SymbolHolder

  @datatype class GclSymbolHolder(symbol: GclSymbol) extends SymbolHolder

  val toolName: String = "GCL-Resolver"

  val GUMBO__Library: String = "GUMBO__Library"

  val uif__HasEvent: String = "uif__HasEvent"
  val uif__MaySend: String = "uif__MaySend"
  val uif__MustSend: String = "uif__MustSend"
  val uif__MustSendWithExpectedValue: String = "uif__MustSendWithExpectedValue"
  val uif__NoSend: String = "uif__NoSend"

  val apiName: String = "api"

  @enum object RewriteMode {
    "Normal" // no rewrites of api calls
    "Api" // add api calls
    "ApiGet" // add get to api calls
  }

  @pure def libraryReporter: TypeChecker = {
    return org.sireum.lang.FrontEnd.checkedLibraryReporter._1
  }

  @record class SymbolFinder(val mode: RewriteMode.Type,
                             val context: AadlComponent,
                             val isContextGeneralAssumeClause: B,
                             val optHandledPort: Option[AadlPort],
                             val stateVars: ISZ[GclStateVar],
                             val specFuncs: Map[ISZ[String], GclMethod],
                             val symbolTable: SymbolTable) extends org.sireum.hamr.ir.MTransformer {
    var symbols: Set[SymbolHolder] = Set.empty
    var reporter: Reporter = ReporterImpl(ISZ())
    var apiReferences: Set[AadlPort] = Set.empty

    val rewriteApiCalls: B = mode match {
      case RewriteMode.ApiGet => T
      case RewriteMode.Api => T
      case _ => F
    }
    val addGetToApiCalls: B = mode == RewriteMode.ApiGet

    def lookup(name: String, fqn: ISZ[String], resOpt: Option[ResolvedInfo], optPos: Option[Position]): Option[SymbolHolder] = {
      context match {
        case a: AadlData =>
          var cands: ISZ[SymbolHolder] = a.subComponents.filter((p: AadlComponent) => p.identifier == name)
            .map((s: AadlComponent) => AadlSymbolHolder(s))
            .map((s: AadlSymbolHolder) => s.asInstanceOf[SymbolHolder]) // make tipe happy

          if (cands.isEmpty) {
            if (specFuncs.contains(fqn)) {
              cands = cands :+ GclSymbolHolder(specFuncs.get(fqn).get)
            }
          }

          if (cands.isEmpty) {
            resOpt match {
              case Some(ario: AST.ResolvedInfo.Object) =>
                // must be a call to a data type constructor
                return None()
              case _ =>
                reporter.error(optPos, toolName, s"Could not find ${name} in data component ${a.identifier}")
                return None()
            }
          }

          if (cands.isEmpty) {
            reporter.error(optPos, toolName, s"Could not find ${name} in data component ${a.identifier}")
            return None()
          } else if (cands.size > 1) {
            reporter.error(optPos, toolName, s"Found ${cands.size} instances of ${name} in data component ${a.identifier}")
            return None()
          } else {
            return Some(cands(0))
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
            if (specFuncs.contains(fqn)) {
              cands = cands :+ GclSymbolHolder(specFuncs.get(fqn).get)
            }
          }

          if (cands.isEmpty) {
            resOpt match {
              case Some(ario: AST.ResolvedInfo.Object) =>
                // must be a call to a data type constructor
                return None()
              case _ =>
                reporter.error(optPos, toolName, s"Could not find ${name} in thread component ${a.identifier}")
                return None()
            }
          }

          if (cands.size > 1) {
            reporter.error(optPos, toolName, s"Found ${cands.size} instances of ${name} in thread component ${a.identifier}")
            return None()
          } else {
            return Some(cands(0))
          }
        case x =>
          reporter.error(optPos, GclResolver.toolName, "Was expecting either a Thread or Data component")
          return None()
      }
    }

    def getPortAttr(p: AadlPort): ResolvedAttr = {
      val aadlType: AadlType =
        if (p.isInstanceOf[AadlEventPort]) TypeUtil.EmptyType
        else p.asInstanceOf[AadlFeatureData].aadlType

      val ids = aadlType.nameProvider.classifier
      val typedName = AST.Typed.Name(ids = ids, args = ISZ())

      var typedOpt: Option[AST.Typed] = Some(typedName)
      if (p.isInstanceOf[AadlFeatureEvent]) {
        typedOpt = Some(AST.Typed.Name(ids = AST.Typed.optionName, args = ISZ(typedName)))
      }

      return ResolvedAttr(posOpt = None(), resOpt = None(), typedOpt = typedOpt)
    }

    def processIdent(o: Exp.Ident): Option[SymbolHolder] = {
      o.attr.resOpt match {
        case Some(e: AST.ResolvedInfo.Enum) => // ignore
        case Some(e: AST.ResolvedInfo.Package) => // ignore
        case Some(x) =>
          val fqName: ISZ[String] = x match {
            case e: AST.ResolvedInfo.Var => e.owner :+ e.id
            case e: AST.ResolvedInfo.Method => e.owner :+ e.id
            case e: AST.ResolvedInfo.Object => e.name
            case x =>
              reporter.error(o.fullPosOpt, GclResolver.toolName, s"Wasn't expecting $x")
              ISZ()
          }
          return lookup(o.id.value, fqName, o.resOpt, o.fullPosOpt)
        case _ => reporter.error(o.fullPosOpt, toolName, s"Ident '$o' did not resolve")
      }
      return None()
    }

    override def pre_langastExpInvoke(o: Exp.Invoke): org.sireum.hamr.ir.MTransformer.PreResult[Exp] = {

      val emptyAttr = AST.Attr(posOpt = o.fullPosOpt)
      val emptyRAttr = AST.ResolvedAttr(posOpt = o.fullPosOpt, resOpt = None(), typedOpt = None())

      if (o.ident.id.value == uif__MustSend || o.ident.id.value == uif__MustSendWithExpectedValue) {

        if (o.args.isEmpty) {
          reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. First argument must be outgoing event port")
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
        }

        val onlyIdent: Exp.Ident = o.args(0) match {
          case Exp.Select(Some(t: Exp.This), id, _) =>
            // strip 'this' off that's added by tipe
            Exp.Ident(id, o.attr)
          case i: Exp.Ident => i
          case _ =>
            reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. First argument must (currently) be the simple name of an outgoing event port")
            return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
        }

        processIdent(onlyIdent) match {
          case Some(ash@AadlSymbolHolder(aadlFeatureEvent: AadlFeatureEvent)) if aadlFeatureEvent.direction == Direction.Out =>

            if (!aadlFeatureEvent.isInstanceOf[AadlEventPort] && !aadlFeatureEvent.isInstanceOf[AadlEventDataPort]) {
              reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. First argument must an an outgoing event port")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
            }

            val p = aadlFeatureEvent.asInstanceOf[AadlPort]

            symbols = symbols + ash
            apiReferences = apiReferences + p

            if (o.args.size == 1) { // MustSend(portIdent)
              // api.portid
              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = onlyIdent.id, targs = ISZ(), attr = getPortAttr(p))

              // api.portid.nonEmpty
              val nonEmpty = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("nonEmpty", emptyAttr), targs = o.targs, attr = o.attr)

              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(nonEmpty))

            } else if (o.args.size == 2) { // MustSend(portIdent, expectedValue)

              if (aadlFeatureEvent.isInstanceOf[AadlEventPort]) {
                reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. Expected value not supported for event ports")
                return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
              }

              val expectedValue: Exp = o.args(1) match {
                case s@Exp.Select(Some(t: Exp.This), id, _) => Exp.Ident(id, s.attr) // strip off 'this'
                case x => x
              }

              expectedValue match {
                case valueIdent: Exp.Ident =>
                  processIdent(valueIdent) match {
                    case Some(g: GclSymbolHolder) => symbols = symbols + g
                    case _ => // expected value can be any legal Ident
                  }
                case _ => // expected value can be any legal Ident
              }

              // api.portid
              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = onlyIdent.id, targs = ISZ(), attr = getPortAttr(p))

              // api.portid.get
              val getSelect = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("get", emptyAttr), targs = o.targs, attr = o.attr)

              // api.portid.get == expectedValue
              val be = Exp.Binary(getSelect, "==", expectedValue, o.attr, o.attr.posOpt)

              // api.portid.nonempty
              val nonEmptySel: Exp = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("nonEmpty", emptyAttr), targs = o.targs, attr = o.attr)

              // (api.portid.nonEmpty && (api.portid.get == expectedValue)
              val rexp = Exp.Binary(nonEmptySel, "&&", be, o.attr, o.attr.posOpt)

              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(rexp))

            } else {
              reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. Too many arguments")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
            }

          case _ =>
            reporter.error(o.fullPosOpt, toolName, "Invalid MustSend expression. First argument must be an outgoing event port")
            return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
        }

      } else if (o.ident.id.value == uif__NoSend) {
        if (o.args.size == 1) {

          val onlyIdent: Exp.Ident = o.args(0) match {
            case Exp.Select(Some(t: Exp.This), id, _) =>
              // strip 'this' off that's added by tipe
              Exp.Ident(id, o.attr)
            case i: Exp.Ident => i
            case _ =>
              reporter.error(o.fullPosOpt, toolName, "Invalid NoSend expression. Argument must (currently) be the simple name of an outgoing event port")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
          }

          processIdent(onlyIdent) match {
            case Some(ash@AadlSymbolHolder(aadlFeatureEvent: AadlFeatureEvent)) if aadlFeatureEvent.direction == Direction.Out && aadlFeatureEvent.isInstanceOf[AadlPort] =>
              val p = aadlFeatureEvent.asInstanceOf[AadlPort]

              symbols = symbols + ash
              apiReferences = apiReferences + p

              // api.portid
              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = onlyIdent.id, targs = ISZ(), attr = getPortAttr(p))

              // api.portid.isEmpty
              val isEmpty = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("isEmpty", emptyAttr), targs = o.targs, attr = o.attr)

              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(isEmpty))
            case _ =>
              reporter.error(o.fullPosOpt, toolName, "Invalid NoSend expression. Can only be applied to outgoing event ports")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
          }
        }
        else {
          reporter.error(o.fullPosOpt, toolName, "Invalid NoSend expression. Requires an outgoing event port")
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
        }
      } else if (o.ident.id.value == uif__HasEvent) {
        if (o.args.size == 1) {

          val onlyIdent: Exp.Ident = o.args(0) match {
            case Exp.Select(Some(t: Exp.This), id, _) =>
              // strip 'this' off that's added by tipe
              Exp.Ident(id, o.attr)
            case i: Exp.Ident => i
            case _ =>
              reporter.error(o.fullPosOpt, toolName, "Invalid HasEvent expression. Argument must (currently) be the simple name of an incoming event port")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
          }

          processIdent(onlyIdent) match {
            case Some(ash@AadlSymbolHolder(aadlFeatureEvent: AadlFeatureEvent)) if aadlFeatureEvent.isInstanceOf[AadlPort] =>
              val p = aadlFeatureEvent.asInstanceOf[AadlPort]

              symbols = symbols + ash
              apiReferences = apiReferences + p

              // api.portid
              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = onlyIdent.id, targs = ISZ(), attr = getPortAttr(p))

              // api.portid.nonEmpty
              val isEmpty = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("nonEmpty", emptyAttr), targs = o.targs, attr = o.attr)

              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(isEmpty))
            case _ =>
              reporter.error(o.fullPosOpt, toolName, "Invalid HasEvent expression. Can only be applied to incoming event ports")
              return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
          }
        }
        else {
          reporter.error(o.fullPosOpt, toolName, "Invalid HasEvent expression. Requires an incoming event port")
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
        }
      }
      else {
        return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
      }
    }

    override def pre_langastExpSelect(o: Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[Exp] = {
      o.receiverOpt match {
        case Some(i: Exp.This) =>
          lookup(o.id.value, ISZ(), o.resOpt, o.fullPosOpt) match {
            case Some(s) => symbols = symbols + s
            case _ =>
          }
          // strip 'this' off that's added by tipe
          val ident = Exp.Ident(o.id, o.attr)
          return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(ident))

        case Some(i@Exp.Ident(featureId)) if (rewriteApiCalls) =>
          processIdent(i) match {
            case Some(s) =>
              symbols = symbols + s

              s match {
                case AadlSymbolHolder(p: AadlPort) =>
                  apiReferences = apiReferences + p

                  val emptyAttr = AST.Attr(posOpt = o.fullPosOpt)
                  val emptyRAttr = AST.ResolvedAttr(posOpt = o.fullPosOpt, resOpt = None(), typedOpt = None())

                  // api.portid
                  val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
                  val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = featureId, targs = ISZ(), attr = getPortAttr(p))

                  val sel: Exp =
                    if (p.isInstanceOf[AadlEventDataPort]) {
                      // api.portid.get
                      val getSelect = Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("get", emptyAttr), targs = o.targs, attr = o.attr)

                      // api.portid.get.fieldName
                      Exp.Select(receiverOpt = Some(getSelect), id = o.id, targs = o.targs, attr = o.attr)
                    } else {
                      // api.portid.fieldName
                      Exp.Select(receiverOpt = Some(apiSelect), id = o.id, targs = o.targs, attr = o.attr)
                    }

                  // don't visit sub children
                  return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(sel))

                case _ =>
              }
            case _ =>
          }
        case _ =>

      }

      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
    }

    override def post_langastExpInvoke(o: Exp.Invoke): MOption[Exp] = {
      val emptyAttr = AST.Attr(posOpt = o.fullPosOpt)
      val emptyRAttr = AST.ResolvedAttr(posOpt = o.fullPosOpt, resOpt = None(), typedOpt = None())

      val receiverOpt: Option[Exp] = {
        o.attr.resOpt.get match {
          case rim: ResolvedInfo.Method if (rim.mode == AST.MethodMode.Constructor) =>
            // datatype constructor, no rewrites needed
            return MNone()
          case _ =>
            o.receiverOpt match {
              case s@Some(AST.Exp.Select(_, rid, _)) =>
                if (rid.value != GUMBO__Library) {
                  reporter.error(o.fullPosOpt, toolName, s"Expecting the method ${rid.value} to be in a synthetic package called ${GUMBO__Library}")
                }
                s
              case _ =>
                // receiver is empty so must be a call to a subclause specdef

                val owner: ISZ[String] = {
                  o.attr.resOpt match {
                    case Some(ri: ResolvedInfo.Method) => ri.owner
                    case _ =>
                      reporter.error(o.fullPosOpt, toolName, s"Couldn't resolve owner of ${o}")
                      ISZ("<INVALID>")
                  }
                }
                val components: ISZ[AadlComponent] = symbolTable.classifierMap.get(owner).get
                if (components.size > 1) {
                  reporter.error(None(), toolName, s"There are multiple instances of ${owner}, currently only handling single instances")
                  None()
                } else if (components.isEmpty) {
                  reporter.error(None(), toolName, s"Couldn't find any instances of ${owner}")
                  None()
                } else {
                  val component = components(0)
                  val objectName = NameUtil.getAirNameProvider(component.component, "").componentSingletonType

                  Some(Exp.Select(
                    //rreceiverOpt = Some(Exp.Ident(id = AST.Id(owner(0), emptyAttr), attr = emptyRAttr)),
                    receiverOpt = None(),
                    id = AST.Id(value = objectName, attr = AST.Attr(None())),
                    targs = ISZ(),
                    attr = emptyRAttr))
                }
            }
        }
      }
      return MSome(o(receiverOpt = receiverOpt))
    }

    override def post_langastExpInput(o: Exp.Input): MOption[Exp] = {
      o.exp match {
        case o: Exp.Ident =>
          processIdent(o) match {
            case Some(GclSymbolHolder(g: GclStateVar)) =>
            case _ =>
              reporter.error(o.fullPosOpt, toolName, s"Only state vars can be used in In expressions")
          }
        case _ =>
          reporter.error(o.fullPosOpt, toolName, s"Currently only allowing the simple name of state vars to be used in In expressions")
      }

      return MNone()
    }

    override def post_langastExpIdent(o: Exp.Ident): MOption[Exp] = {
      processIdent(o) match {
        case Some(s) =>
          symbols = symbols + s

          s match {
            case AadlSymbolHolder(p: AadlPort) if rewriteApiCalls =>
              apiReferences = apiReferences + p

              val emptyAttr = AST.Attr(posOpt = o.fullPosOpt)
              val emptyRAttr = AST.ResolvedAttr(posOpt = o.fullPosOpt, resOpt = None(), typedOpt = None())

              // api.portId
              val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = emptyAttr), attr = emptyRAttr)
              val apiSelect = Exp.Select(receiverOpt = Some(apiIdent), id = o.id, targs = ISZ(), attr = getPortAttr(p))

              val sel: Exp =
                if (addGetToApiCalls && p.isInstanceOf[AadlEventDataPort]) {
                  // api.portId.get
                  Exp.Select(receiverOpt = Some(apiSelect), id = AST.Id("get", emptyAttr), targs = o.targs, attr = o.attr)
                } else {
                  apiSelect
                }

              return MSome(sel)

            case GclSymbolHolder(s: GclStateVar) if isContextGeneralAssumeClause =>
              // In(s)
              return MSome(Exp.Input(o, AST.Attr(o.fullPosOpt)))
            case _ =>
          }

        case _ =>
      }
      return MNone()
    }
  }

  def collectSymbols(exp: Exp,
                     mode: RewriteMode.Type,
                     context: AadlComponent,
                     isContextGeneralAssumeClause: B,
                     stateVars: ISZ[GclStateVar],
                     methods: Map[ISZ[String], GclMethod],
                     symbolTable: SymbolTable,
                     reporter: Reporter): (MOption[Exp], ISZ[SymbolHolder], ISZ[AadlPort]) = {
    return collectSymbolsH(exp, mode, context, isContextGeneralAssumeClause, None(), stateVars, methods, symbolTable, reporter)
  }

  def collectSymbolsH(exp: Exp,
                      mode: RewriteMode.Type,
                      context: AadlComponent,
                      isContextGeneralAssumeClause: B,
                      optHandledPort: Option[AadlPort],
                      stateVars: ISZ[GclStateVar],
                      methods: Map[ISZ[String], GclMethod],
                      symbolTable: SymbolTable,
                      reporter: Reporter): (MOption[Exp], ISZ[SymbolHolder], ISZ[AadlPort]) = {
    if (reporter.hasError) {
      // already in an inconsistent state
      return (MNone(), ISZ(), ISZ())
    } else {
      val sf = SymbolFinder(mode, context, isContextGeneralAssumeClause, optHandledPort, stateVars, methods, symbolTable)
      val rexp = sf.transform_langastExp(exp)
      reporter.reports(sf.reporter.messages)
      return (rexp, sf.symbols.elements, sf.apiReferences.elements)
    }
  }

  def getAadlType(s: String, aadlTypes: AadlTypes, posOpt: Option[Position], reporter: Reporter): AadlType = {
    aadlTypes.typeMap.get(s) match {
      case Some(t) => return t
      case _ =>
        reporter.error(posOpt, toolName, s"$s did not resolve to an AADL type")
        return EmptyType
    }
  }
}

import org.sireum.hamr.codegen.common.resolvers.GclResolver._

@record class GclResolver() extends AnnexVisitor {

  var rexprs: HashMap[SymTableKey, AST.Exp] = HashMap.empty
  var apiReferences: Set[AadlPort] = Set.empty
  var computeHandlerPortMap: Map[AST.Exp, AadlPort] = Map.empty
  var integrationMap: Map[AadlPort, GclSpec] = Map.empty

  var processedAnnexes: Map[Annex, Option[AnnexClauseInfo]] = Map.empty
  var processedLibs: Map[GclLib, AnnexLibInfo] = Map.empty

  var globalTypeMap: TypeMap = HashSMap.empty
  var globalNameMap: NameMap = HashSMap.empty
  var resolvedMethods: HashSMap[ISZ[String], Info.Method] = HashSMap.empty
  var builtTypeInfo: B = F

  def reset: B = {
    rexprs = HashMap.empty
    apiReferences = Set.empty
    computeHandlerPortMap = Map.empty
    integrationMap = Map.empty
    processedAnnexes = Map.empty
    processedLibs = Map.empty
    globalTypeMap = HashSMap.empty[QName, TypeInfo] ++ GclResolver.libraryReporter.typeMap.entries
    globalNameMap = HashSMap.empty[QName, Info] ++ GclResolver.libraryReporter.nameMap.entries
    resolvedMethods = HashSMap.empty
    builtTypeInfo = F

    return T
  }

  def globalImports(symbolTable: SymbolTable): ISZ[AST.Stmt.Import] = {

    val emptyAttr = AST.Attr(None())

    val sireumImporters: ISZ[AST.Stmt.Import.Importer] = {
      val keys = GclUtil.interpolatorLookup.keys.map((m: String) => ops.StringOps(m).firstToUpper)
      keys.map((m: String) =>
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

    return sireumImporters.map((m: AST.Stmt.Import.Importer) =>
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

  def typeMethod(fqn: ISZ[String], m: AST.Stmt.Method, reporter: Reporter): AST.Stmt.Method = {
    resolvedMethods.get(fqn) match {
      case Some(resolvedMethod) =>
        return m(sig = resolvedMethod.ast.sig, attr = resolvedMethod.ast.attr)
      case _ =>
        reporter.error(m.posOpt, toolName, st"Could not resolve method '${(fqn, "::")}'".render)
        return m
    }
  }

  def visitMethodContract(m: AST.Stmt.Method,
                          typeHierarchy: TypeHierarchy,
                          scope: Scope,
                          reporter: Reporter): AST.Stmt.Method = {
    val tc = TypeChecker(typeHierarchy, ISZ(m.sig.id.value), F, TypeChecker.ModeContext.Spec, F)

    val newStmt = TypeChecker.checkMethodContractSequent(F, typeHierarchy, ISZ(m.sig.id.value), scope, F, m, reporter)

    return newStmt
  }

  @pure def toKey(e: AST.Exp): SymTableKey = {
    //assert(e.fullPosOpt.nonEmpty, e.string)
    return SymTableKey(e, e.fullPosOpt)
  }

  def visitGclMethod(context: ISZ[String],
                     gclMethod: GclMethod,
                     typeHierarchy: TypeHierarchy,
                     scope: Scope,
                     reporter: Reporter): AST.Stmt.Method = {
    val fqn = context :+ gclMethod.method.sig.id.value
    val tc = TypeChecker(typeHierarchy, fqn, F, TypeChecker.ModeContext.Spec, F)
    val typedMethod: AST.Stmt.Method = typeMethod(fqn, gclMethod.method, reporter)
    var rMethod: AST.Stmt.Method = tc.checkMethod(scope, typedMethod, reporter)

    if (gclMethod.method.mcontract.nonEmpty) {
      val scontract = gclMethod.method.mcontract.asInstanceOf[Simple]

      rMethod = visitMethodContract(rMethod, typeHierarchy, scope, reporter)

      val rscontract = rMethod.mcontract.asInstanceOf[Simple]

      for (i <- 0 until scontract.reads.size) {
        rexprs = rexprs + (toKey(scontract.reads(i).asInstanceOf[AST.Exp.Ident]) ~> rscontract.reads(i).asInstanceOf[AST.Exp.Ident])
      }

      for (i <- 0 until scontract.requires.size) {
        rexprs = rexprs + (toKey(scontract.requires(i)) ~> rscontract.requires(i))
      }

      for (i <- 0 until scontract.modifies.size) {
        rexprs = rexprs + (toKey(scontract.modifies(i).asInstanceOf[AST.Exp.Ident]) ~> rscontract.modifies(i).asInstanceOf[AST.Exp.Ident])
      }

      for (i <- 0 until scontract.ensures.size) {
        rexprs = rexprs + (toKey(scontract.ensures(i)) ~> rscontract.ensures(i))
      }
    }

    (gclMethod.method.bodyOpt, rMethod.bodyOpt) match {
      case (Some(AST.Body(ISZ(AST.Stmt.Return(Some(exp))))),
      Some(AST.Body(ISZ(AST.Stmt.Return(Some(rexp)))))) =>

        rexprs = rexprs + (toKey(exp) ~> rexp)

      case _ =>
        reporter.error(gclMethod.method.posOpt, GclResolver.toolName, "Unexpected: method does not have a body")
    }

    return rMethod

  }

  def processGclLib(context: ISZ[String],
                    gclLib: GclLib,
                    symbolTable: SymbolTable,
                    aadlTypes: AadlTypes,
                    typeHierarchy: TypeHierarchy,
                    scope: Scope,
                    reporter: Reporter): Option[GclSymbolTable] = {

    for (gclMethod <- gclLib.methods) {
      val rMethod = visitGclMethod(context, gclMethod, typeHierarchy, scope, reporter)
      val methodQualifiedName = context :+ gclMethod.method.sig.id.value
      replaceName(methodQualifiedName, Info.Method(
        owner = context,
        isInObject = T,
        scope = scope,
        hasBody = rMethod.bodyOpt.nonEmpty,
        ast = rMethod))
    }

    val gclSymTable = GclSymbolTable(
      rexprs = rexprs,
      slangTypeHierarchy = typeHierarchy(
          nameMap = globalNameMap,
          typeMap = globalTypeMap),
      apiReferences = ISZ(),
      integrationMap = Map.empty,
      computeHandlerPortMap = Map.empty)

    return Some(gclSymTable)
  }

  def processGclAnnex(component: AadlComponent,
                      annex: GclSubclause,
                      libInfos: ISZ[GclAnnexLibInfo],
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      typeHierarchy: TypeHierarchy,
                      scope: Scope.Local,
                      reporter: Reporter): Option[GclSymbolTable] = {

    val baseTypeBoolean = aadlTypes.typeMap.get("Base_Types::Boolean").get
    val baseTypeInteger = aadlTypes.typeMap.get("Base_Types::Integer").get
    val baseTypeString = aadlTypes.typeMap.get("Base_Types::String").get
    val baseTypeFloat = aadlTypes.typeMap.get("Base_Types::Float").get
    val baseTypeFloat32 = aadlTypes.typeMap.get("Base_Types::Float_32").get
    val baseTypeFloat64 = aadlTypes.typeMap.get("Base_Types::Float_64").get

    val componentPos = component.component.identifier.pos

    var libMethods: Map[ISZ[String], GclMethod] = Map.empty
    for (lib <- libInfos) {
      libMethods = libMethods ++ (for (m <- lib.annex.methods) yield (lib.name :+ m.method.sig.id.value) ~> m)
    }

    def visitGclSubclause(s: GclSubclause): Unit = {
      var seenInvariantIds: Set[String] = Set.empty

      val context = component.classifier
      if (reporter.hasError) {
        return
      }

      val threadMethods: Map[ISZ[String], GclMethod] = Map.empty[ISZ[String], GclMethod] ++ (for(m <- s.methods) yield (context :+ m.method.sig.id.value) ~> m)

      // hmm, lost imports with AIR translation so assume all glc lib annexes have been imported
      val gclMethods = threadMethods ++ libMethods.entries

      def typeCheckBoolExp(exp: Exp): Exp = {
        val rexp: AST.Exp = visitSlangExp(exp) match {
          case Some((rexp, roptType)) =>
            roptType match {
              case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
                //rexprs = rexprs + (exp ~> rexp)
                rexp
              case Some(x) =>
                reporter.error(exp.fullPosOpt, GclResolver.toolName, s"Expecting B but found ${x}")
                exp
              case _ =>
                assert(reporter.hasError, "Expression is untyped so Tipe should have reported errors already") // sanity check
                exp
            }
          case _ =>
            reporter.error(exp.fullPosOpt, GclResolver.toolName, "Unexpected: type checking returned none")
            exp
        }
        return rexp
      }

      for (gclMethod <- s.methods) {
        visitGclMethod(context, gclMethod, typeHierarchy, scope, reporter)
      }

      for (i <- s.invariants) {
        if (seenInvariantIds.contains(i.id)) {
          reporter.error(i.exp.fullPosOpt, GclResolver.toolName, s"Duplicate invariant id: ${i.id}")
        }
        seenInvariantIds = seenInvariantIds + i.id
        visitInvariant(i)
      }

      if (s.integration.nonEmpty) {
        var seenSpecNames: Set[String] = Set.empty
        val gclIntegration = s.integration.get

        for (glcIntegSpec <- gclIntegration.specs) {

          if (seenSpecNames.contains(glcIntegSpec.id)) {
            reporter.error(glcIntegSpec.exp.fullPosOpt, GclResolver.toolName, s"Duplicate spec name: ${glcIntegSpec.id}")
          }
          seenSpecNames = seenSpecNames + glcIntegSpec.id

          val expTipe: AST.Exp = typeCheckBoolExp(glcIntegSpec.exp)

          if (!reporter.hasError) {
            val (expTrans, symbols, apiRefs) =
              GclResolver.collectSymbols(expTipe, RewriteMode.Normal, component, F, ISZ(), gclMethods, symbolTable, reporter)

            val resolvedExpr: AST.Exp = expTrans match {
              case MSome(et2) => et2
              case _ => expTipe
            }
            rexprs = rexprs + toKey(glcIntegSpec.exp) ~> resolvedExpr
            apiReferences = apiReferences ++ apiRefs

            if (!reporter.hasError) {
              val portRef: Option[AadlPort] = {
                var portRefs: Set[AadlPort] = Set.empty
                for (s <- symbols) {
                  s match {
                    case AadlSymbolHolder(sym: AadlPort) => portRefs = portRefs + sym
                    case GclSymbolHolder(sym: GclMethod) => // TODO: collect port refs in method args
                    case x =>
                      reporter.error(glcIntegSpec.exp.fullPosOpt, toolName, s"Error in integration spec.  Not expecting to encounter ${x}")
                  }
                }
                if (portRefs.size != 1) {
                  reporter.error(glcIntegSpec.exp.fullPosOpt, GclResolver.toolName, s"An integration clause must refer to exactly one port")
                  None()
                } else {
                  Some(portRefs.elements(0))
                }
              }
              if (portRef.nonEmpty) {
                val sym = portRef.get
                // update the spec here as well as its expression will be handed off
                // to tipe for integration constraint checking
                glcIntegSpec match {
                  case a: GclAssume =>
                    integrationMap = integrationMap + sym ~> a(exp = resolvedExpr)
                  case g: GclGuarantee =>
                    integrationMap = integrationMap + sym ~> g(exp = resolvedExpr)
                }
                rexprs = rexprs - toKey(glcIntegSpec.exp) ~> resolvedExpr // remove the old entry

                rexprs = rexprs + toKey(resolvedExpr) ~> resolvedExpr

                sym.direction match {
                  case Direction.Out =>
                    if (!glcIntegSpec.isInstanceOf[GclGuarantee]) {
                      reporter.error(glcIntegSpec.exp.fullPosOpt, GclResolver.toolName, s"Integration contracts for outgoing ports must be Guarantee statements")
                    }
                  case Direction.In =>
                    if (!glcIntegSpec.isInstanceOf[GclAssume]) {
                      reporter.error(glcIntegSpec.exp.fullPosOpt, GclResolver.toolName, s"Integration contracts for incoming ports must be Assume statements")
                    }
                  case x =>
                    reporter.error(resolvedExpr.fullPosOpt, GclResolver.toolName, s"Previous phase should have rejected this case: ${x}")
                }
              }
            }
          }
        }
      }


      def checkFlow(exp: AST.Exp, isFrom: B, posOpt: Option[Position]): Unit = {
        exp match {
          case e: Exp.Ident =>
            visitSlangExp(e) match {
              case Some((rexp, roptType)) =>
                val (rexp2, symbols, _) = GclResolver.collectSymbols(rexp, RewriteMode.Api, component, F, s.state, gclMethods, symbolTable, reporter)
                if (!reporter.hasError) {
                  if (symbols.size != 1) {
                    reporter.error(e.fullPosOpt, GclResolver.toolName, s"From/To expressions should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                  }
                  symbols(0) match {
                    case AadlSymbolHolder(sym) =>
                      sym match {
                        case p: AadlPort =>
                          if (isFrom && p.direction != Direction.In) {
                            reporter.error(e.fullPosOpt, GclResolver.toolName, s"Only in ports are allowed in From flow clauses")
                          }
                          if (!isFrom && p.direction != Direction.Out) {
                            reporter.error(e.fullPosOpt, GclResolver.toolName, s"Only out ports are allowed in To flow clauses")
                          }
                        case _ =>
                          reporter.error(e.fullPosOpt, GclResolver.toolName, s"From/To flow clauses can only contain ports and state vars")
                      }
                    case GclSymbolHolder(sym) =>
                      if (!sym.isInstanceOf[GclStateVar]) {
                        reporter.error(e.fullPosOpt, GclResolver.toolName, s"From/To flow clauses can only contain ports and state vars")
                      }
                  }
                  if (rexp2.isEmpty) {
                    rexprs = rexprs + toKey(e) ~> rexp
                  }
                  else {
                    rexprs = rexprs + toKey(e) ~> rexp2.get
                  }
                }
              case _ =>
                reporter.error(e.fullPosOpt, GclResolver.toolName, s"Unable to resolve $e")
            }
          case _ =>
            reporter.error(posOpt, GclResolver.toolName, s"Expecting from/to expressions to be Idents, found ${exp}")
        }
      }

      def processModifiesClause(lmodifies: ISZ[Exp]): Unit = {
        for (modifies <- lmodifies) {
          modifies match {
            case e: Exp.Ident =>
              visitSlangExp(e) match {
                case Some((rexp, roptType)) =>
                  val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, s.state, gclMethods, symbolTable, reporter)
                  apiReferences = apiReferences ++ apiRefs

                  if (!reporter.hasError) {
                    if (symbols.size != 1) {
                      reporter.error(e.fullPosOpt, GclResolver.toolName, s"Modifies should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                    } else {
                      symbols(0) match {
                        case AadlSymbolHolder(p: AadlPort) if p.direction == Direction.Out =>
                          // modifies clause should just be "api" rather than "portName"
                          val apiIdent: Exp = Exp.Ident(id = AST.Id(value = apiName, attr = AST.Attr(posOpt = e.fullPosOpt)),
                            attr = AST.ResolvedAttr(posOpt = e.fullPosOpt, resOpt = None(), typedOpt = None()))
                          rexprs = rexprs + toKey(e) ~> apiIdent
                        case GclSymbolHolder(GclStateVar(_, _)) =>
                          rexp2 match {
                            case MSome(re2) => rexprs = rexprs + toKey(e) ~> re2
                            case _ => rexprs = rexprs + toKey(e) ~> rexp
                          }
                        case x =>
                          reporter.error(modifies.fullPosOpt, GclResolver.toolName, s"Modifies expressions must be the simple name of an outgoing port or a state variable, found ${x}.")
                      }
                    }
                  }
                case _ => reporter.error(modifies.fullPosOpt, GclResolver.toolName, s"Unable to resolve $e")
              }
            case _ =>
              reporter.error(modifies.fullPosOpt, GclResolver.toolName, s"Modifies expressions must be the simple name of an outgoing port or a state variable, found ${modifies}")
          }
        }
      }

      if (s.initializes.nonEmpty) {

        processModifiesClause(s.initializes.get.modifies)

        var seenGuaranteeIds: Set[String] = Set.empty
        for (guarantees <- s.initializes.get.guarantees) {
          if (seenGuaranteeIds.contains(guarantees.id)) {
            reporter.error(guarantees.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantees.id}")
          }
          seenGuaranteeIds = seenGuaranteeIds + guarantees.id

          val rexp = typeCheckBoolExp(guarantees.exp)
          val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, s.state, gclMethods, symbolTable, reporter)
          apiReferences = apiReferences ++ apiRefs

          if (rexp2.isEmpty) {
            rexprs = rexprs + toKey(guarantees.exp) ~> rexp
          } else {
            rexprs = rexprs + toKey(guarantees.exp) ~> rexp2.get
          }
        }

        for (flow <- s.initializes.get.flows) {
          if (flow.from.nonEmpty) {
            reporter.error(flow.posOpt, GclResolver.toolName, s"Initialize from clauses must be empty")
          }
          if (flow.to.isEmpty) {
            reporter.error(flow.posOpt, GclResolver.toolName, s"Initialize to clauses cannot be empty")
          }
          for (toExp <- flow.to) {
            checkFlow(toExp, F, flow.posOpt)
          }
        }
      }

      s.compute match {
        case Some(GclCompute(modifies, specs, cases, handlers, flows)) => {

          processModifiesClause(modifies)

          var seenSpecIds: Set[String] = Set.empty

          for (spec <- specs) {
            if (seenSpecIds.contains(spec.id)) {
              reporter.error(spec.posOpt, GclResolver.toolName, s"Duplicate spec name: ${spec.id}")
            }
            seenSpecIds = seenSpecIds + spec.id

            {
              val rexp = typeCheckBoolExp(spec.exp)
              val isContextGeneralAssumeClause = spec.isInstanceOf[GclAssume]
              val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, isContextGeneralAssumeClause, s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              spec match {
                case g: GclAssume =>
                  for (sym <- symbols) {
                    sym match {
                      case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                        reporter.error(spec.exp.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                      case _ =>
                    }
                  }
                case _ =>
              }

              if (rexp2.isEmpty) {
                rexprs = rexprs + (toKey(spec.exp) ~> rexp)
              } else {
                rexprs = rexprs + (toKey(spec.exp) ~> rexp2.get)
              }
            }
          }


          for (caase <- cases) {
            if (seenSpecIds.contains(caase.id)) {
              reporter.error(caase.posOpt, GclResolver.toolName, s"Duplicate spec name: ${caase.id}")
            }
            seenSpecIds = seenSpecIds + caase.id

            {
              val rexp = typeCheckBoolExp(caase.assumes)
              val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              for (sym <- symbols) {
                sym match {
                  case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                    reporter.error(caase.assumes.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                  case _ =>
                }
              }
              if (rexp2.isEmpty) {
                rexprs = rexprs + (toKey(caase.assumes) ~> rexp)
              } else {
                rexprs = rexprs + (toKey(caase.assumes) ~> rexp2.get)
              }
            }

            {
              val rexp = typeCheckBoolExp(caase.guarantees)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              if (rexp2.isEmpty) {
                rexprs = rexprs + (toKey(caase.guarantees) ~> rexp)
              } else {
                rexprs = rexprs + (toKey(caase.guarantees) ~> rexp2.get)
              }
            }
          }

          for (handler <- handlers) {

            visitSlangExp(handler.port) match {
              case Some((rexp, roptType)) =>
                val (hexp, symbols, _) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, s.state, gclMethods, symbolTable, reporter)
                if (!reporter.hasError) {
                  if (symbols.size != 1) {
                    reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Handler should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                  }
                  symbols(0) match {
                    case AadlSymbolHolder(p: AadlPort) =>
                      computeHandlerPortMap = computeHandlerPortMap + handler.port ~> p

                      if (p.direction != Direction.In || p.isInstanceOf[AadlDataPort]) {
                        reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Compute handlers can only be applied to incoming event or event data ports")
                      }

                      hexp match {
                        case MSome(hexpresolved) => rexprs = rexprs + toKey(handler.port) ~> hexpresolved
                        case _ => rexprs = rexprs + toKey(handler.port) ~> rexp
                      }
                    case x => reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Handler should resolve to an AADL port but received $x")
                  }
                }
              case _ => reporter.error(handler.posOpt, GclResolver.toolName, s"Unable to resolve ${handler.port}")
            }

            processModifiesClause(handler.modifies)

            var handlerSpecIds: Set[String] = Set.empty
            for (guarantees <- handler.guarantees) {
              if (seenSpecIds.contains(guarantees.id) || handlerSpecIds.contains(guarantees.id)) {
                reporter.error(guarantees.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantees.id}")
              }
              handlerSpecIds = handlerSpecIds + guarantees.id

              val rexp = typeCheckBoolExp(guarantees.exp)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbolsH(rexp, RewriteMode.ApiGet, component, F,
                computeHandlerPortMap.get(handler.port), // rewrite handled port references to the passed in param
                s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              if (rexp2.isEmpty) {
                rexprs = rexprs + toKey(guarantees.exp) ~> rexp
              } else {
                rexprs = rexprs + toKey(guarantees.exp) ~> rexp2.get
              }
            }

            component match {
              case a: AadlDispatchableComponent =>
                if (!a.isSporadic()) {
                  reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Compute handlers can only be used with sporadic components")
                }
              case _ =>
                reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Unexpected: Compute handlers can only be used with dispatchable components")
            }
          }

          for (flow <- flows) {
            if (flow.from.isEmpty && flow.to.isEmpty) {
              reporter.error(flow.posOpt, GclResolver.toolName, s"At least one of the from/to clauses must be non-empty")
            }
            for (fromExp <- flow.from) {
              checkFlow(fromExp, T, flow.posOpt)
            }
            for (toExp <- flow.to) {
              checkFlow(toExp, F, flow.posOpt)
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
              val (rexp2, _, _) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, ISZ(), libMethods, symbolTable, reporter)
              if (rexp2.isEmpty) {
                rexprs = rexprs + (toKey(i.exp) ~> rexp)
              } else {
                rexprs = rexprs + (toKey(i.exp) ~> rexp2.get)
              }
            case Some(x) => reporter.error(i.exp.fullPosOpt, GclResolver.toolName, s"Expecting B but found ${x}")
            case _ =>
              assert(reporter.hasError, "Invariant expression is untyped so Tipe should have reported errors already")
          }
        case _ => reporter.error(i.exp.fullPosOpt, GclResolver.toolName, "Unexpected: type checking returned none")
      }
    }

    def visitSlangExp(exp: AST.Exp): Option[(AST.Exp, Option[AST.Typed])] = {
      val scontext: QName = component.classifier

      val mode = TypeChecker.ModeContext.Spec
      val typeChecker: TypeChecker = TypeChecker(typeHierarchy, scontext, F, mode, F)

      val typeChecked = typeChecker.checkExp(None(), scope, exp, reporter)

      if (typeChecked._2.isEmpty) {
        reporter.error(exp.posOpt, toolName, s"Could not resolve expression's type")
      }

      return Some(typeChecked)
    }

    visitGclSubclause(annex)

    return Some(GclSymbolTable(
      rexprs = rexprs,
      slangTypeHierarchy = typeHierarchy(
        nameMap = globalNameMap,
        typeMap = globalTypeMap),
      apiReferences = apiReferences.elements,
      integrationMap = integrationMap,
      computeHandlerPortMap = computeHandlerPortMap))
  }

  def scope(packageName: IdPath, imports: ISZ[AST.Stmt.Import], enclosingName: IdPath): Scope.Global = {
    return Scope.Global(packageName, imports, enclosingName)
  }

  def replaceName(name: QName, info: Info): Unit = {
    globalNameMap = globalNameMap + name ~> info
  }

  def declareName(entity: String, name: QName, info: Info, posOpt: Option[Position], reporter: Reporter): Unit = {
    globalNameMap.get(name) match {
      case Some(_) =>
        reporter
          .error(posOpt, GclResolver.toolName, s"Cannot declare $entity because the name has already been declared previously.")
      case _ => globalNameMap = globalNameMap + name ~> info
    }
  }

  def buildTypeMap(gclLibs: ISZ[GclLib], aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): Unit = {
    if (builtTypeInfo) {
      return
    }
    builtTypeInfo = T

    def fromTypetoTyped(value: AST.Type): AST.Typed = {
      val ret: AST.Typed = value match {
        case atn: AST.Type.Named =>
          AST.Typed.Name(ids = atn.name.ids.map((m: AST.Id) => m.value), args = ISZ())
        case x =>
          reporter.error(value.posOpt, GclResolver.toolName, s"Expected a Type.Named but received $x")
          AST.Typed.Name(ids = ISZ(), args = ISZ())
      }
      return ret
    }

    def resolveType(value: AST.Type, posOpt: Option[Position]): AST.Type = {
      val ret: AST.Type = {
        value match {
          case atn: AST.Type.Named =>
            val fqn = atn.name.ids.map((i: AST.Id) => i.value)
            val s = st"${(fqn, "::")}".render

            val aadlType: AadlType = getAadlType(s, aadlTypes, if(posOpt.nonEmpty) posOpt else value.posOpt, reporter)
            val typeInfo = buildTypeInfo(aadlType)

            val name: AST.Name = typeInfo match {
              case ta: TypeInfo.TypeAlias =>
                val tn = ta.ast.tipe.asInstanceOf[AST.Type.Named].attr.typedOpt.get.asInstanceOf[AST.Typed.Name]
                AST.Name(ids = tn.ids.map((s: String) => AST.Id(value = s, attr = AST.Attr(None()))),
                  attr = AST.Attr(None()))

              case tadt: TypeInfo.Adt =>
                AST.Name(ids = tadt.tpe.ids.map((s: String) => AST.Id(value = s, attr = AST.Attr(None()))), attr = AST.Attr(None()))

              case te: TypeInfo.Enum =>
                AST.Name(ids = te.name.map((s: String) => AST.Id(value = s, attr = AST.Attr(None()))), attr = AST.Attr(None()))

              case x =>
                reporter.error(typeInfo.posOpt, GclResolver.toolName, s"Wasn't expecting a $x")
                AST.Name(ids = ISZ(), attr = AST.Attr(None()))
            }

            val typedName = AST.Typed.Name(ids = name.ids.map((i: AST.Id) => i.value), args = ISZ())

            AST.Type.Named(
              name = name,
              typeArgs = ISZ(),
              attr = AST.TypedAttr(posOpt = None(), typedOpt = Some(typedName))
            )
          case x =>
            reporter.error(value.posOpt, GclResolver.toolName, s"Wasn't expecting $x")
            AST.Type.Named(name = AST.Name(ids = ISZ(), attr = AST.Attr(None())), typeArgs = ISZ(), attr = AST.TypedAttr(None(), None()))
        }
      }
      return ret
    }

    def typeToTyped(t: AST.Type): AST.Typed = {
      val ret: AST.Typed = t match {
        case atn: AST.Type.Named => AST.Typed.Name(ids = atn.name.ids.map((i: AST.Id) => i.value), args = ISZ())
        case x =>
          reporter.error(t.posOpt, GclResolver.toolName, s"Wasn't expecting $x")
          AST.Typed.Name(ids = ISZ(), args = ISZ())
      }
      return ret
    }

    def paramToTypedName(p: AST.Param): AST.Typed = {
      val ids = p.tipe.asInstanceOf[AST.Type.Named].name.ids.map((i: AST.Id) => i.value)
      return AST.Typed.Name(ids = ids, args = ISZ())
    }

    def getTypedFromAadlType(aadlType: AadlType): Option[AST.Typed] = {
      val paramResolvedType: TypeInfo = buildTypeInfo(aadlType)
      val typedOpt: Option[AST.Typed] = paramResolvedType match {
        case ta: TypeInfo.TypeAlias => ta.ast.tipe.typedOpt
        case tadt: TypeInfo.Adt => Some(tadt.tpe)
        case te: TypeInfo.Enum => Some(AST.Typed.Name(ids = te.name, args = ISZ()))
        case x =>
          reporter.error(aadlType.container.get.identifier.pos, GclResolver.toolName, s"Wasn't expecting $x")
          None()
      }
      return typedOpt
    }

    def buildTypeInfo(aadlType: AadlType): TypeInfo = {
      val TODO_TYPE: TypeInfo.Adt = {
        val adtAst: AST.Stmt.Adt =
          AST.Stmt.Adt(
            isRoot = F,
            isDatatype = T,
            id = AST.Id("TODO", AST.Attr(None())),
            typeParams = ISZ(),
            params = ISZ(),
            parents = ISZ(),
            stmts = ISZ(),
            attr = AST.Attr(None()))

        TypeInfo.Adt(
          owner = ISZ("TODO"),
          outlined = F,
          contractOutlined = F,
          typeChecked = F,
          tpe = AST.Typed.Name(ISZ(s"TODO_${aadlType.name}"), ISZ()),
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
          scope = scope(ISZ("TODO"), globalImports(symbolTable), ISZ("TODO")),
          ast = adtAst)
      }

      val qualifiedName = aadlType.classifier

      val qualifiedTypeName: ISZ[String] =
        aadlType match {
          case e: EnumType => qualifiedName :+ Info.Enum.elementTypeSuffix
          case _ => qualifiedName
        }

      if (globalTypeMap.contains(qualifiedTypeName)) {
        return globalTypeMap.get(qualifiedTypeName).get
      }

      aadlType match {
        case e: EnumType =>

          var elements: Map[String, AST.ResolvedInfo] = Map.empty
          var ordinal: Z = 0
          for (value <- e.values) {
            val ri: ResolvedInfo.EnumElement = ResolvedInfo.EnumElement(qualifiedTypeName, value, ordinal)
            ordinal = ordinal + 1
            elements = elements + (value ~> ri)
          }

          val posOpt: Option[Position] = None()

          val enumx: TypeInfo.Enum = TypeInfo.Enum(qualifiedName, elements, posOpt)

          globalTypeMap = globalTypeMap + (qualifiedTypeName ~> enumx)

          if (!globalNameMap.contains(qualifiedName)) {
            val elementTypedOpt = AST.Typed.Name(ids = qualifiedTypeName, args = ISZ())

            for (elem <- enumx.elements.entries) {
              val ee = elem._2.asInstanceOf[AST.ResolvedInfo.EnumElement]
              declareName(ee.name, qualifiedName :+ ee.name, Info.EnumElement(
                owner = ee.owner,
                id = ee.name,
                typedOpt = Some(elementTypedOpt),
                resOpt = Some(AST.ResolvedInfo.EnumElement(owner = qualifiedName, name = ee.name, ordinal = ee.ordinal)),
                posOpt = None()), None(), reporter)
            }

            declareName(e.name, qualifiedName,
              Info.Enum(
                name = qualifiedName,
                elements = enumx.elements,
                typedOpt = Some(AST.Typed.Enum(name = qualifiedName)),
                resOpt = Some(AST.ResolvedInfo.Enum(name = qualifiedName)),
                elementTypedOpt = Some(elementTypedOpt),
                posOpt = None()), None(), reporter)
          }

          val gclAnnexes = e.container.get.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])
          if (gclAnnexes.nonEmpty) {
            reporter.error(e.container.get.identifier.pos, toolName, "GCL subclauses cannot be attached to enum definitions")
          }

          return enumx

        case r: RecordType =>
          val component = r.container.get

          val aadlData = symbolTable.componentMap.get(ISZ(component.classifier.get.name)).get.asInstanceOf[AadlData]

          return buildAdtTypeInfo(aadlData)

        case b: BaseType =>

          val simpleName = b.simpleName
          val packageName = ops.ISZOps(qualifiedTypeName).dropRight(1)
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

              case x =>
                reporter.error(b.container.get.identifier.pos, GclResolver.toolName, s"Wasn't expecting $simpleName")
                "String"
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
            name = qualifiedTypeName,
            scope = scope,
            ast = ast)

          globalTypeMap = globalTypeMap + (qualifiedTypeName ~> ta)

          val gclAnnexes = b.container.get.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])
          if (gclAnnexes.nonEmpty) {
            reporter.error(b.container.get.identifier.pos, toolName, "GCL subclauses cannot be attached to Base Type definitions")
          }

          return ta

        case TODOType(ISZ("art", "Empty"), _, _) =>
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

          globalTypeMap = globalTypeMap + (ISZ("art", "Empty") ~> typeInfoAdt)

          return typeInfoAdt

        case a: ArrayType => return TODO_TYPE
        case b: BitType => return TODO_TYPE
        case x => return TODO_TYPE
      }
    }

    def buildPackageInfo(packageName: ISZ[String]): Info.Package = {
      assert(packageName.size == 1, s"TODO: only expecting a single package name segment: ${packageName}")

      if (!globalNameMap.contains(packageName)) {
        declareName(packageName(packageName.lastIndex), packageName, Info.Package(
          name = packageName,
          typedOpt = Some(AST.Typed.Package(name = packageName)),
          resOpt = Some(ResolvedInfo.Package(name = packageName))
        ), None(), reporter)
      }

      return globalNameMap.get(packageName).get.asInstanceOf[Info.Package]
    }

    def buildGumboLibrary(g: GclLib): Info.Object = {

      val packageName: ISZ[String] = g.containingPackage.name
      buildPackageInfo(packageName)

      val adtQualifiedName: ISZ[String] = packageName :+ GUMBO__Library

      globalNameMap.get(adtQualifiedName) match {
        case Some(i: Info.Object) => return i
        case _ =>
      }

      val adtScope = scope(packageName, globalImports(symbolTable), packageName)

      for(gclMethod <- g.methods) {
        val fqMethodName = (adtQualifiedName :+ gclMethod.method.sig.id.value)

        if (!globalNameMap.contains(fqMethodName)) {
          val infoMethod = buildInfoMethod(gclMethod, adtQualifiedName, adtScope)
          resolvedMethods = resolvedMethods + (fqMethodName ~> infoMethod)
          declareName(gclMethod.method.sig.id.value, fqMethodName, infoMethod, None(), reporter)
        }
      }

      val objectInfo = Info.Object(
        owner = packageName,
        isSynthetic = F,
        scope = adtScope,
        outlined = F,
        contractOutlined = F,
        typeChecked = F,
        ast = AST.Stmt.Object(
          isApp = F,
          extNameOpt = None(),
          id = AST.Id(value = GUMBO__Library, attr = AST.Attr(None())),
          stmts = ISZ(),
          attr = AST.Attr(None())
        ),
        typedOpt = Some(AST.Typed.Object(owner = packageName, id = GUMBO__Library)),
        resOpt = Some(AST.ResolvedInfo.Object(name = adtQualifiedName)),
        constructorRes = AST.ResolvedInfo.Method(
          isInObject = T,
          mode = AST.MethodMode.ObjectConstructor,
          typeParams = ISZ(),
          owner = packageName,
          id = GUMBO__Library,
          paramNames = ISZ(),
          tpeOpt = None(),
          reads = ISZ(),
          writes = ISZ()
        )
      )

      declareName(adtQualifiedName(adtQualifiedName.lastIndex), adtQualifiedName,
        objectInfo, None(), reporter)

      return objectInfo
    }

    def buildInfoMethod(gclMethod: GclMethod, adtQualifiedName: ISZ[String], adtScope: Scope): Info.Method = {
      val m = gclMethod.method

      val methodName = m.sig.id.value

      val qualifiedMethodName = adtQualifiedName :+ methodName

      val resolvedParams: ISZ[AST.Param] = m.sig.params.map((p: AST.Param) => {
        val resolvedTipe = resolveType(p.tipe, p.id.attr.posOpt)
        p(tipe = resolvedTipe)
      })

      val resolvedReturnType: AST.Type = resolveType(m.sig.returnType, m.sig.id.attr.posOpt)

      val resolvedReturnTyped: AST.Typed = fromTypetoTyped(resolvedReturnType)

      val resolvedTypeParams = m.sig.typeParams
      assert(resolvedTypeParams.isEmpty, "Not handling type params yet")

      val resolvedMsig = AST.MethodSig(
        purity = m.sig.purity,
        id = m.sig.id,
        typeParams = m.sig.typeParams,
        hasParams = resolvedParams.nonEmpty,
        params = resolvedParams,
        returnType = resolvedReturnType)

      val resolvedTypedFun = AST.Typed.Fun(
        purity = m.sig.purity,
        isByName = F,
        args = resolvedParams.map((p: AST.Param) => paramToTypedName(p)),
        ret = resolvedReturnTyped)

      val resolvedInfoMethod = AST.ResolvedInfo.Method(
        isInObject = T,
        mode = AST.MethodMode.Method,
        typeParams = resolvedTypeParams.map((t: AST.TypeParam) => t.id.value),
        owner = adtQualifiedName,
        id = methodName,
        paramNames = resolvedParams.map((p: AST.Param) => p.id.value),
        tpeOpt = Some(resolvedTypedFun),
        reads = ISZ(),
        writes = ISZ())

      val resolvedTypedMethod = AST.Typed.Method(
        isInObject = T,
        mode = AST.MethodMode.Method,
        typeParams = ISZ(),
        owner = adtQualifiedName,
        name = methodName,
        paramNames = resolvedParams.map((p: AST.Param) => p.id.value),
        tpe = resolvedTypedFun)

      val resolvedAttr = AST.ResolvedAttr(
        posOpt = m.attr.posOpt,
        resOpt = Some(resolvedInfoMethod),
        typedOpt = Some(resolvedTypedMethod))

      val resolvedAstMethod = AST.Stmt.Method(
        typeChecked = m.typeChecked,
        purity = m.purity,
        modifiers = m.modifiers,
        sig = resolvedMsig,
        mcontract = m.mcontract,
        bodyOpt = m.bodyOpt,
        attr = resolvedAttr)

      val scopeNameMap: HashMap[String, Info] = {
        val entries: ISZ[(String, Info)] = resolvedParams.map((p: AST.Param) => {
          val lv = Info.LocalVar(
            name = qualifiedMethodName :+ p.id.value,
            isVal = T,
            ast = AST.Id(value = p.id.value, attr = AST.Attr(posOpt = None())),
            typedOpt = Some(paramToTypedName(p)),
            initOpt = None(),
            resOpt = Some(AST.ResolvedInfo.LocalVar(
              context = qualifiedMethodName,
              scope = ResolvedInfo.LocalVar.Scope.Current,
              isSpec = F,
              isVal = T,
              id = p.id.value)
            )
          )
          (p.id.value, lv)
        })

        HashMap.empty[String, Info] ++ entries
      }

      val methodScope = Scope.Local(
        nameMap = scopeNameMap,
        typeMap = HashMap.empty,
        localThisOpt = None(),
        methodReturnOpt = Some(resolvedReturnTyped),
        indexMap = HashMap.empty,
        outerOpt = Some(adtScope)
      )

      val infoMethod = Info.Method(
        owner = adtQualifiedName,
        isInObject = T,
        scope = methodScope,
        hasBody = T,
        ast = resolvedAstMethod
      )

      return infoMethod
    }

    def buildInfoObject(a: AadlThread): Info.Object = {

      val component = a.component
      val threadsName = a.classifier

      if (globalNameMap.contains(threadsName)) {
        return globalNameMap.get(threadsName).get.asInstanceOf[Info.Object]
      }

      val threadsOwner = ops.ISZOps(threadsName).dropRight(1)
      val currentName = threadsOwner
      val threadsPackageName = threadsOwner

      val threadsSimpleName = a.classifier(a.classifier.lastIndex)

      val threadsId = AST.Id(
        value = threadsSimpleName,
        attr = AST.Attr(component.identifier.pos))

      // build Slang package from AADL package
      buildPackageInfo(threadsOwner)

      // enclosingName is the same as the packageName
      val threadsScope = scope(threadsOwner, globalImports(symbolTable), threadsOwner)

      val gclAnnexes = component.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])

      if (gclAnnexes.size > 1) {
        reporter.error(a.component.identifier.pos, toolName, "Only a single GCL subclause is allowed per component type/implementation")
      }

      // treat stateVars as if they were features for Tipe
      val stateVars: ISZ[AadlPort] = gclAnnexes.flatMap((g: GclSubclause) => g.state.map((sv: GclStateVar) => {
        val aadlType = getAadlType(sv.classifier, aadlTypes, sv.posOpt, reporter)
        AadlDataPort(
          feature = FeatureEnd(
            identifier = Name(threadsName :+ sv.name, sv.posOpt),
            direction = Direction.Out,
            category = FeatureCategory.DataPort,
            classifier = Some(Classifier(sv.classifier)),
            properties = ISZ(),
            uriFrag = ""),
          featureGroupIds = ISZ(),
          direction = Direction.In,
          aadlType = aadlType)
      }))

      val features: ISZ[AadlPort] = a.getPorts()

      for (field <- features ++ stateVars) {
        val fieldId: String = field.identifier
        val fieldType: AadlType = field match {
          case afd: AadlFeatureData => afd.aadlType
          case _ => TypeUtil.EmptyType
        }

        val qualifiedFieldName = threadsName :+ fieldId

        val fieldResInfoOpt = Some[AST.ResolvedInfo](
          AST.ResolvedInfo.Var(isInObject = T, isSpec = F, isVal = F, owner = threadsName, id = fieldId))

        val typedOpt: Option[AST.Typed] = getTypedFromAadlType(fieldType)

        val varTypeId = AST.Id(value = fieldType.name, attr = AST.Attr(None()))
        val varTypeName = AST.Type.Named(
          name = AST.Name(ids = ISZ(varTypeId), attr = AST.Attr(None())),
          typeArgs = ISZ(),
          attr = AST.TypedAttr(posOpt = None(), typedOpt = None())
        )

        val infoVar = Info.Var(
          owner = threadsName,
          isInObject = T,
          scope = scope(
            packageName = threadsPackageName,
            imports = globalImports(symbolTable),
            enclosingName = threadsName),
          ast = AST.Stmt.Var(
            isSpec = F,
            isVal = F,
            id = AST.Id(value = fieldId, attr = AST.Attr(None())),
            tipeOpt = Some(varTypeName),
            initOpt = None(),
            attr = ResolvedAttr(posOpt = field.feature.identifier.pos, resOpt = fieldResInfoOpt, typedOpt = typedOpt)
          )
        )

        declareName(fieldId, qualifiedFieldName, infoVar, field.feature.identifier.pos, reporter)
      }

      val methods: HashSMap[String, Info.Method] = {

        val TYPE_VAR__STATE_VAR = "TYPE_VAR__STATE_VAR"

        def createInfoMethod(methodName: String, typeParams: ISZ[AST.TypeParam], params: ISZ[AST.Param], returnType: AST.Type.Named, isInObject: B): Info.Method = {

          val methodAst: AST.Stmt.Method = {
            val methodSig = AST.MethodSig(
              purity = AST.Purity.Pure,
              id = AST.Id(methodName, AST.Attr(None())),
              typeParams = typeParams,
              hasParams = params.nonEmpty,
              params = params,
              returnType = returnType
            )

            val typedFun = AST.Typed.Fun(
              purity = AST.Purity.Pure,
              isByName = F,
              args = params.map((m: AST.Param) => {
                val ids = m.tipe.asInstanceOf[AST.Type.Named].name.ids.map((i: AST.Id) => i.value)

                if (ids == ISZ(TYPE_VAR__STATE_VAR))
                  AST.Typed.TypeVar(id = TYPE_VAR__STATE_VAR, kind = AST.Typed.VarKind.Immutable)
                else
                  AST.Typed.Name(ids = ids, args = ISZ())
              }),
              ret = AST.Typed.Name(ids = returnType.name.ids.map((m: AST.Id) => m.value), args = ISZ())
            )

            val rInfoMethod = AST.ResolvedInfo.Method(
              isInObject = isInObject,
              mode = AST.MethodMode.Method,
              typeParams = typeParams.map((t: AST.TypeParam) => t.id.value),
              owner = threadsName,
              id = methodName,
              paramNames = params.map((m: AST.Param) => m.id.value),
              tpeOpt = Some(typedFun),
              reads = ISZ(),
              writes = ISZ()
            )

            val typedMethod = AST.Typed.Method(
              isInObject = isInObject,
              mode = AST.MethodMode.Method,
              typeParams = typeParams.map((t: AST.TypeParam) => t.id.value),
              owner = threadsName,
              name = methodName,
              paramNames = params.map((m: AST.Param) => m.id.value),
              tpe = typedFun
            )

            AST.Stmt.Method(
              typeChecked = F,
              purity = AST.Purity.Pure,
              modifiers = ISZ(),
              sig = methodSig,
              mcontract = AST.MethodContract.Simple.empty,
              bodyOpt = None(),
              attr = AST.ResolvedAttr(posOpt = None(), resOpt = Some(rInfoMethod), typedOpt = Some(typedMethod))
            )
          }

          return Info.Method(
            owner = threadsName,
            isInObject = isInObject,
            scope = scope(
              packageName = threadsPackageName,
              imports = threadsScope.imports,
              enclosingName = threadsName),
            hasBody = F,
            ast = methodAst
          )
        }

        def toName(x: ISZ[String]): AST.Name = {
          return AST.Name(
            ids = x.map((m: String) => AST.Id(value = m, attr = AST.Attr(None()))),
            attr = AST.Attr(None()))
        }

        val expType = AST.Type.Named(
          name = toName(ISZ("org", "sireum", "lang", "ast", "Exp")),
          typeArgs = ISZ(),
          attr = AST.TypedAttr(
            posOpt = None(),
            typedOpt = Some(AST.Typed.Name(ids = ISZ("org", "sireum", "lang", "ast", "Exp"), args = ISZ()))
          )
        )

        val boolType = AST.Type.Named(
          name = toName(ISZ("org", "sireum", "B")),
          typeArgs = ISZ(),
          attr = AST.TypedAttr(
            posOpt = None(),
            typedOpt = Some(AST.Typed.Name(ids = ISZ("org", "sireum", "B"), args = ISZ()))
          ))

        val portParam = AST.Param(
          isHidden = F,
          id = AST.Id("port", AST.Attr(None())),
          tipe = expType
        )


        val genericType = AST.TypeParam(AST.Id(TYPE_VAR__STATE_VAR, AST.Attr(None())), AST.Typed.VarKind.Immutable)
        val genericPortParam = AST.Param(
          isHidden = F,
          id = AST.Id("port", AST.Attr(None())),
          tipe = AST.Type.Named(
            name = toName(ISZ(TYPE_VAR__STATE_VAR)),
            typeArgs = ISZ(),
            attr = AST.TypedAttr(
              posOpt = None(),
              typedOpt = Some(AST.Typed.TypeVar(id = TYPE_VAR__STATE_VAR, kind = AST.Typed.VarKind.Immutable))
            )
          )
        )
        val genericValueParam = AST.Param(
          isHidden = F,
          id = AST.Id("value", AST.Attr(None())),
          tipe = AST.Type.Named(
            name = toName(ISZ(TYPE_VAR__STATE_VAR)),
            typeArgs = ISZ(),
            attr = AST.TypedAttr(
              posOpt = None(),
              typedOpt = Some(AST.Typed.TypeVar(id = TYPE_VAR__STATE_VAR, kind = AST.Typed.VarKind.Immutable))
            )
          )
        )

        var _methods: HashSMap[String, Info.Method] = HashSMap.empty
        val sigs = ISZ[(String, ISZ[AST.TypeParam], ISZ[AST.Param], AST.Type.Named)](
          (uif__HasEvent, ISZ(genericType), ISZ[AST.Param](genericPortParam), boolType),
          (uif__MaySend, ISZ(), ISZ[AST.Param](portParam), boolType),
          (uif__NoSend, ISZ(genericType), ISZ[AST.Param](genericPortParam), boolType),
          (uif__MustSend, ISZ(genericType), ISZ[AST.Param](genericPortParam), boolType),
          (uif__MustSendWithExpectedValue, ISZ(genericType), ISZ[AST.Param](genericPortParam, genericValueParam), boolType),
        )

        for (sig <- sigs) {
          _methods = _methods + sig._1 ~> createInfoMethod(
            methodName = sig._1,
            typeParams = sig._2,
            params = sig._3,
            returnType = sig._4,
            isInObject = T)
        }

        val specDefs: ISZ[(String, Info.Method)] = gclAnnexes.flatMap((g: GclSubclause) =>
          g.methods.map((gclMethod: GclMethod) => {

            val infoMethod = buildInfoMethod(gclMethod, threadsName, threadsScope)

            val fqMethodName = threadsName :+ gclMethod.method.sig.id.value
            resolvedMethods = resolvedMethods + (fqMethodName ~> infoMethod)

            (gclMethod.method.sig.id.value, infoMethod)
          }))

        _methods ++ specDefs
      }

      for (m <- methods.values) {
        declareName(
          entity = "method",
          name = m.name,
          info = m,
          posOpt = m.posOpt,
          reporter = reporter)
      }

      val infoObject = Info.Object(
        owner = threadsOwner,
        isSynthetic = F,
        scope = threadsScope,
        outlined = T,
        contractOutlined = T,
        typeChecked = T,
        ast = AST.Stmt.Object(
          isApp = F,
          extNameOpt = None(),
          id = threadsId,
          stmts = ISZ(), // TODO add fields and methods?
          attr = AST.Attr(component.identifier.pos)),
        typedOpt = Some(AST.Typed.Object(currentName, threadsSimpleName)),
        resOpt = Some(AST.ResolvedInfo.Object(threadsName)),
        constructorRes = AST.ResolvedInfo.Method(F, AST.MethodMode.ObjectConstructor, ISZ(), threadsName, threadsSimpleName, ISZ(), None(), ISZ(), ISZ())
      )

      declareName(
        entity = "object",
        name = threadsName,
        info = infoObject,
        posOpt = infoObject.posOpt,
        reporter = reporter)

      return infoObject
    }

    def buildAdtTypeInfo(a: AadlData): TypeInfo.Adt = {

      val adtQualifiedName = a.classifier

      if (globalTypeMap.contains(adtQualifiedName)) {
        return globalTypeMap.get(adtQualifiedName).get.asInstanceOf[TypeInfo.Adt]
      }

      val simpleName = a.classifier(a.classifier.lastIndex)
      val packageName = ops.ISZOps(adtQualifiedName).dropRight(1)
      assert(packageName.nonEmpty)

      // build Slang package from AADL package
      buildPackageInfo(packageName)

      // enclosingName is the same as the packageName
      val adtScope = scope(packageName, globalImports(symbolTable), packageName)

      var paramVars = HashSMap.empty[String, Info.Var]
      var constructorParamVars = ISZ[String]()
      val extractParamVars = ISZ[String]() // always empty as there are no @hidden params in AADL

      val gclAnnexes = a.component.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])

      if (gclAnnexes.size > 1) {
        reporter.error(a.component.identifier.pos, toolName, "Only a single GCL subclause is allowed per component type/implementation")
      }

      // treat stateVars as if they were features for Tipe
      val stateVars: ISZ[AadlPort] = gclAnnexes.flatMap((g: GclSubclause) => g.state.map((sv: GclStateVar) => {
        val aadlType = getAadlType(sv.classifier, aadlTypes, sv.posOpt, reporter)
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
        if (stateVars.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have state vars")
        }
        // treat aadl data subcomponents as if they were features for Tipe
        a.subComponents.map((sc: AadlComponent) => {
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

        val typedOpt: Option[AST.Typed] = getTypedFromAadlType(paramType)

        val varTypeId = AST.Id(value = paramType.name, attr = AST.Attr(None()))
        val varTypeName = AST.Type.Named(
          name = AST.Name(ids = ISZ(varTypeId), attr = AST.Attr(None())),
          typeArgs = ISZ(),
          attr = AST.TypedAttr(posOpt = None(), typedOpt = None())
        )

        paramVars = paramVars + paramId ~> Info.Var(
          owner = adtQualifiedName,
          isInObject = F,
          scope = adtScope,
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
          val paramResolvedType: TypeInfo = buildTypeInfo(paramType)

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

      val methods: HashSMap[String, Info.Method] = HashSMap.empty

      val constructorInfo: (Option[AST.Typed], Option[AST.ResolvedInfo]) = {

        val tpeFun = AST.Typed.Fun(
          purity = AST.Purity.Pure,
          isByName = F,
          args = a.subComponents.map((sc: AadlComponent) => {
            val aadlType = aadlTypes.typeMap.get(sc.component.classifier.get.name).get
            getTypedFromAadlType(aadlType).get
          }),
          ret = AST.Typed.Name(ids = adtQualifiedName, args = ISZ())
        )
        val constructorTypeOpt: Option[AST.Typed] = Some(
          AST.Typed.Method(
            isInObject = T,
            mode = AST.MethodMode.Constructor,
            typeParams = ISZ(),
            owner = packageName,
            name = simpleName,
            paramNames = a.subComponents.map((s: AadlComponent) => s.identifier),
            tpe = tpeFun)
        )
        val constructorResOpt: Option[AST.ResolvedInfo] = Some(
          AST.ResolvedInfo.Method(
            isInObject = T,
            mode = AST.MethodMode.Constructor,
            typeParams = ISZ(),
            owner = packageName,
            id = simpleName,
            paramNames = a.subComponents.map((s: AadlComponent) => s.identifier),
            tpeOpt = Some(tpeFun),
            reads = ISZ(),
            writes = ISZ())
        )
        (constructorTypeOpt, constructorResOpt)
      }

      val extractorTypeMap: Map[String, AST.Typed] = Map.empty
      val extractorResOpt: Option[AST.ResolvedInfo] = None()
      val ancestors: ISZ[AST.Typed.Name] = ISZ()
      val specVars: HashSMap[String, Info.SpecVar] = HashSMap.empty

      val specMethods: HashSMap[String, Info.SpecMethod] = HashSMap.empty
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
        constructorTypeOpt = constructorInfo._1,
        constructorResOpt = constructorInfo._2,
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
        scope = adtScope,
        ast = adtAst)

      globalTypeMap = globalTypeMap + (adtQualifiedName ~> typeInfoAdt)

      // build companion object for data type def
      val constructorRes = AST.ResolvedInfo.Method(
        isInObject = T,
        mode = AST.MethodMode.ObjectConstructor,
        typeParams = ISZ(),
        owner = packageName,
        id = simpleName,
        paramNames = ISZ(),
        tpeOpt = None(),
        reads = ISZ(),
        writes = ISZ()
      )
      declareName(adtQualifiedName(adtQualifiedName.lastIndex), adtQualifiedName,
        Info.Object(
          owner = packageName,
          isSynthetic = F,
          scope = adtScope,
          outlined = F,
          contractOutlined = F,
          typeChecked = F,
          ast = AST.Stmt.Object(
            isApp = F,
            extNameOpt = None(),
            id = AST.Id(value = simpleName, attr = AST.Attr(None())),
            stmts = ISZ(),
            attr = AST.Attr(posOpt = None())
          ),
          typedOpt = Some(
            AST.Typed.Object(owner = packageName, id = simpleName)
          ),
          resOpt = Some(
            AST.ResolvedInfo.Object(name = adtQualifiedName)
          ),
          constructorRes = constructorRes), None(), reporter)

      return typeInfoAdt
    }

    { // build type info aadl types
      for (aadlType <- aadlTypes.typeMap.values) {
        buildTypeInfo(aadlType)
      }
    }

    { // build type info for the GCL Libraries
      for (gclLib <- gclLibs) {
        buildGumboLibrary(gclLib)
      }
    }

    { // build type info for aadl threads
      val threadComponents = symbolTable.componentMap.values.filter((m: AadlComponent) => m.isInstanceOf[AadlThread]).map((m: AadlComponent) => m.asInstanceOf[AadlThread])

      for (component <- threadComponents) {
        buildInfoObject(component)
      }
    }
  }

  def offer(component: AadlComponent, annex: Annex, annexLibs: ISZ[AnnexLibInfo], symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): Option[AnnexClauseInfo] = {
    if (processedAnnexes.contains(annex)) {
      return processedAnnexes.get(annex).get
    } else {
      val result: Option[AnnexClauseInfo] = annex.clause match {
        case gclSubclause: GclSubclause =>
          val gclLibs: ISZ[GclAnnexLibInfo] = annexLibs.filter((a: AnnexLibInfo) => a.isInstanceOf[GclAnnexLibInfo]).map((a: AnnexLibInfo) => a.asInstanceOf[GclAnnexLibInfo])
          buildTypeMap(gclLibs.map((g: GclAnnexLibInfo) => g.annex), aadlTypes, symbolTable, reporter)

          val qualifiedName: IdPath = component.classifier
          if (reporter.hasError) {
            return None()
          }

          val scope: Scope.Local = component match {
            case ad: AadlData =>
              globalTypeMap.get(qualifiedName) match {
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
                  reporter.error(None(), toolName, s"Could not resolve type info for GCl Subclause: ${qualifiedName}")
                  return None()
              }
            case ac: AadlThread =>
              globalNameMap.get(qualifiedName) match {
                case Some(o: Info.Object) =>
                  val packageName = ops.ISZOps(o.name).dropRight(1)
                  val global = Scope.Global(
                    packageName = packageName,
                    imports = globalImports(symbolTable),
                    enclosingName = o.name
                  )
                  Scope.Local.create(HashMap.empty, global)

                case _ =>
                  reporter.error(None(), toolName, s"Could not resolve name for GCL Subclause: ${qualifiedName}")
                  return None()
              }
            case c =>
              reporter.error(c.component.identifier.pos, toolName, s"GUMBO subclause contracts can only be attached to threads and data components")
              return None()
          }

          val typeHierarchy: TypeHierarchy = TypeHierarchy(
            nameMap = globalNameMap,
            typeMap = globalTypeMap,
            poset = Poset.empty,
            aliases = HashSMap.empty)

          val gclSymbolTable: GclSymbolTable =
            processGclAnnex(component, gclSubclause, gclLibs, symbolTable, aadlTypes, typeHierarchy, scope, reporter).get
          Some(GclAnnexClauseInfo(gclSubclause, gclSymbolTable))
        case _ => None()
      }
      return result
    }
  }

  def offerLibraries(annexLibs: ISZ[AnnexLib], symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): ISZ[AnnexLibInfo] = {
    var ret: ISZ[AnnexLibInfo] = ISZ()
    val gclLibs: ISZ[GclLib] = annexLibs.filter((al: AnnexLib) => al.isInstanceOf[GclLib]).map((al: AnnexLib) => al.asInstanceOf[GclLib])

    if (gclLibs.nonEmpty) {
      buildTypeMap(gclLibs, aadlTypes, symbolTable, reporter)

      for (gclLib <- gclLibs) {
        if (processedLibs.contains(gclLib)) {
          ret = ret :+ processedLibs.get(gclLib).get
        } else {
          val qualifiedName = gclLib.containingPackage.name :+ GUMBO__Library
          globalNameMap.get(qualifiedName) match {
            case Some(o: Info.Object) =>
              val global = Scope.Global(
                packageName = qualifiedName,
                imports = globalImports(symbolTable),
                enclosingName = o.name
              )
              val localScope = Scope.Local.create(HashMap.empty, global)

              val typeHierarchy: TypeHierarchy = TypeHierarchy(
                nameMap = globalNameMap,
                typeMap = globalTypeMap,
                poset = Poset.empty,
                aliases = HashSMap.empty)

              val gclSymTable: Option[GclSymbolTable] = processGclLib(qualifiedName, gclLib, symbolTable, aadlTypes, typeHierarchy, localScope, reporter)
              val gali = GclAnnexLibInfo(
                annex = gclLib,
                name = qualifiedName,
                gclSymbolTable = gclSymTable.get)

              processedLibs = processedLibs + (gclLib ~> gali)

              ret = ret :+ gali

            case _ =>
              reporter.error(None(), toolName, st"Could not resolve GCL Library: ${(qualifiedName, "::")}".render)
          }
        }
      }
    }
    return ret
  }

}
