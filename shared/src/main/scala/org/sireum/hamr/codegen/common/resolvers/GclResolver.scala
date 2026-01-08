// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, MapValue, Store, TypeIdPath, isInFeature}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.TypeUtil.EmptyType
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.{GclUtil, NameUtil}
import org.sireum.hamr.ir._
import org.sireum.lang.ast.MethodContract.Simple
import org.sireum.lang.ast.{Exp, Purity, ResolvedAttr, ResolvedInfo, TypeParam}
import org.sireum.lang.symbol.Resolver.{NameMap, QName, TypeMap, resolverKind, typeName, typeParamMap}
import org.sireum.lang.symbol.{Info, Resolver, Scope, TypeInfo}
import org.sireum.lang.tipe.{TypeChecker, TypeHierarchy}
import org.sireum.lang.{ast => AST}
import org.sireum.message.{Position, Reporter, ReporterImpl}

object GclResolver {

  val KEY_IndexingTypeFingerprints: String = "KEY_IndexingTypeFingerprints"
  @strictpure def getIndexingTypeFingerprints(store: Store): Map[String, TypeIdPath] =
    store.getOrElse(KEY_IndexingTypeFingerprints, MapValue[String, TypeIdPath](Map.empty))
      .asInstanceOf[MapValue[String, TypeIdPath]].map

  @strictpure def putIndexingTypeFingerprints(m: Map[String, TypeIdPath], store: Store): Store =
    store + KEY_IndexingTypeFingerprints ~> MapValue(m)

  val KEY_SlangTypeToAadlType: String = "KEY_SlangTypeToAadlType"
  @strictpure def getSlangTypeToAadlType(store: Store): Map[AST.Typed, TypeIdPath] =
    store.getOrElse(KEY_SlangTypeToAadlType, MapValue[AST.Typed, TypeIdPath](Map.empty))
      .asInstanceOf[MapValue[AST.Typed, TypeIdPath]].map

  @strictpure def putSlangTypeToAadlType(m: Map[AST.Typed, TypeIdPath], store: Store): Store =
    store + KEY_SlangTypeToAadlType ~> MapValue(m)

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

  val uifs: ops.ISZOps[String] = ops.ISZOps(ISZ(uif__HasEvent, uif__MaySend, uif__MustSend, uif__MustSendWithExpectedValue, uif__NoSend))

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
                             val indexingTypeFingerprints: Map[String, TypeIdPath],

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
                //reporter.error(optPos, toolName, s"Could not find ${name} in thread component ${a.identifier}")
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

      val ids: TypeIdPath = aadlType match {
        case e: EnumType => aadlType.nameProvider.classifier :+ "Type"
        case _ => aadlType.nameProvider.classifier
      }

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
            case e: AST.ResolvedInfo.LocalVar =>
              // must be quantifier variable
              return None()
            case e: AST.ResolvedInfo.Var => e.owner :+ e.id
            case e: AST.ResolvedInfo.Method => e.owner :+ e.id
            case e: AST.ResolvedInfo.Object => e.name
            case x =>
              reporter.error(o.fullPosOpt, GclResolver.toolName, s"Wasn't expecting $x while resolving Ident")
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

      if (o.receiverOpt.isEmpty) {
        if (!uifs.contains(o.ident.id.value)) {
          // e.g. assuming RxIn is an incoming event data port with an array payload
          //      RxIn(0)
          post_langastExpIdent(o.ident) match {
            case MSome(s@Exp.Select(receiverOpt, id@AST.Id("get"), _)) =>
              // port(..) --> api.port.get(..)
              return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(
                o(receiverOpt = receiverOpt, ident = Exp.Ident(id, emptyRAttr))))

            case MSome(e) =>
              halt(s"Unexpected: ${e}")

            case _ => return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
          }
        } else {
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
          } else {
            halt(s"Infeasible uif: ${o.ident.id.value}")
          }
        }
      } // end receiverOpt isEmpty
      else {
        // receiverOpt is nonEmpty
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
                  // can now be an array indexing exp

                  //reporter.error(o.fullPosOpt, toolName, s"Expecting the method ${rid.value} to be in a synthetic package called ${GUMBO__Library}")
                }
                s
              case _ if indexingTypeFingerprints.contains(o.ident.id.value) =>
                // rewritten indexing into an array
                return MNone()

              case _ =>
                // receiver is empty so must be a call to a subclause gumbo function so
                // use the fully qualified name of the generated Slang function

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

    // TODO: why isn't this a pre order call?
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

            case GclSymbolHolder(_: GclStateVar) if isContextGeneralAssumeClause =>
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
                     indexingTypeFingerprints: Map[String, TypeIdPath],
                     stateVars: ISZ[GclStateVar],
                     methods: Map[ISZ[String], GclMethod],
                     symbolTable: SymbolTable,
                     reporter: Reporter): (MOption[Exp], ISZ[SymbolHolder], ISZ[AadlPort]) = {
    return collectSymbolsH(exp, mode, context, isContextGeneralAssumeClause, None(), indexingTypeFingerprints,
      stateVars, methods, symbolTable, reporter)
  }

  def collectSymbolsH(exp: Exp,
                      mode: RewriteMode.Type,
                      context: AadlComponent,
                      isContextGeneralAssumeClause: B,
                      optHandledPort: Option[AadlPort],
                      indexingTypeFingerprints: Map[String, TypeIdPath],
                      stateVars: ISZ[GclStateVar],
                      methods: Map[ISZ[String], GclMethod],
                      symbolTable: SymbolTable,
                      reporter: Reporter): (MOption[Exp], ISZ[SymbolHolder], ISZ[AadlPort]) = {
    if (reporter.hasError) {
      // already in an inconsistent state
      return (MNone(), ISZ(), ISZ())
    } else {
      val sf = SymbolFinder(mode, context, isContextGeneralAssumeClause, optHandledPort,
        indexingTypeFingerprints, stateVars, methods, symbolTable)
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

  def getActualSlangTypedName(typeName: ISZ[String], typeHierarchy: TypeHierarchy): AST.Typed.Name = {
    typeHierarchy.typeMap.get(typeName) match {
      case Some(ta: TypeInfo.TypeAlias) =>
        return ta.ast.tipe.typedOpt.get.asInstanceOf[AST.Typed.Name]
      case _ => return AST.Typed.Name(ids = typeName, args = ISZ())
    }
  }
}

import org.sireum.hamr.codegen.common.resolvers.GclResolver._

@record class GclResolver() extends AnnexVisitor {

  var apiReferences: Set[AadlPort] = Set.empty
  var computeHandlerPortMap: Map[AST.Exp, AadlPort] = Map.empty
  var integrationMap: Map[AadlPort, GclSpec] = Map.empty

  var processedAnnexes: Map[Annex, Option[AnnexClauseInfo]] = Map.empty
  var processedLibs: Map[GclLib, AnnexLibInfo] = Map.empty

  var globalTypeMap: TypeMap = HashSMap.empty
  var globalNameMap: NameMap = HashSMap.empty
  var resolvedMethods: HashSMap[ISZ[String], Info.Method] = HashSMap.empty
  var builtTypeInfo: B = F
  var modelContainsBoundArrays: B = F

  var arrayIndexInterpolateImports: ISZ[AST.Stmt.Import] = ISZ()
  var indexingTypeFingerprints: Map[String, TypeIdPath] = Map.empty
  var slangTypeToAadlType: Map[AST.Typed, TypeIdPath] = Map.empty

  def reset: B = {
    apiReferences = Set.empty
    computeHandlerPortMap = Map.empty
    integrationMap = Map.empty
    processedAnnexes = Map.empty
    processedLibs = Map.empty
    globalTypeMap = HashSMap.empty[QName, TypeInfo] ++ GclResolver.libraryReporter.typeMap.entries
    globalNameMap = HashSMap.empty[QName, Info] ++ GclResolver.libraryReporter.nameMap.entries
    resolvedMethods = HashSMap.empty
    builtTypeInfo = F
    modelContainsBoundArrays = F
    arrayIndexInterpolateImports = ISZ()
    indexingTypeFingerprints = Map.empty

    return T
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

  @pure def toKey(e: AST.Exp): SymTableKey = {
    //assert(e.fullPosOpt.nonEmpty, e.string)
    return SymTableKey(e, e.fullPosOpt)
  }

  def visitSlangExp(exp: AST.Exp,
                    context: ISZ[String],
                    mode: TypeChecker.ModeContext.Type,
                    scope: Scope.Local, typeHierarchy: TypeHierarchy, reporter: Reporter): Option[(AST.Exp, Option[AST.Typed])] = {

    val typeChecker: TypeChecker = TypeChecker(typeHierarchy, context, F, mode, F)

    val typeChecked = typeChecker.checkExp(None(), scope, exp, reporter)

    if (typeChecked._2.isEmpty) {
      reporter.error(exp.posOpt, toolName, s"Could not resolve expression's type: ${exp.prettyST.render}")
    }

    return Some(typeChecked)
  }

  def typeCheckBoolExp(exp: Exp,
                       context: ISZ[String],
                       mode: TypeChecker.ModeContext.Type,
                       component: Option[AadlComponent],
                       params: ISZ[AST.Param],
                       stateVars: ISZ[GclStateVar],
                       specFuns: Map[ISZ[String], GclMethod],
                       symbolTable: SymbolTable, aadlTypes: AadlTypes,
                       scope: Scope.Local,
                       typeHierarchy: TypeHierarchy,
                       reporter: Reporter): Exp = {
    return typeCheckExp(
      exp,
      AST.Typed.Name(ids = ISZ("org", "sireum", "B"), args = ISZ()),
      context,
      mode, component, params, stateVars, specFuns, symbolTable, aadlTypes, scope, typeHierarchy, reporter)
  }

  def typeCheckExp(exp: Exp,
                   expectedType: AST.Typed.Name,
                   context: ISZ[String],
                   mode: TypeChecker.ModeContext.Type,
                   component: Option[AadlComponent],
                   params: ISZ[AST.Param],
                   stateVars: ISZ[GclStateVar],
                   specFuns: Map[ISZ[String], GclMethod],
                   symbolTable: SymbolTable,
                   aadlTypes: AadlTypes,
                   scope: Scope.Local,
                   typeHierarchy: TypeHierarchy,
                   reporter: Reporter): Exp = {
    val _exp: AST.Exp = {
      if (modelContainsBoundArrays) {
        val r = ArrayIndexRewriter(component, params, stateVars, specFuns, scope, typeHierarchy, symbolTable, aadlTypes).transform_langastExp(exp)
        if (r.nonEmpty) r.get
        else exp
      }
      else {
        exp
      }
    }

    val rexp: AST.Exp = visitSlangExp(_exp, context, mode, scope, typeHierarchy, reporter) match {
      case Some((rexp, roptType)) =>
        roptType match {
          case Some(t: AST.Typed.Name) if t == expectedType =>
            (_exp, expectedType.ids) match {
              case (i: AST.Exp.Ident, ISZ("org", "sireum", "B")) if i.id.value == "T" || i.id.value == "F" =>
                // sysml toolchain may emit these, which type check correctly.  However, downstream tools
                // likely expect LitB's
                AST.Exp.LitB(i.id.value == "T", i.id.attr)
              case _ =>
                rexp
            }
          case Some(x) =>
            reporter.error(_exp.fullPosOpt, GclResolver.toolName, st"Expecting ${(expectedType, ".")} but found $x".render)
            _exp
          case _ =>
            assert(reporter.hasError, "Expression is untyped so Tipe should have reported errors already") // sanity check
            _exp
        }
      case _ =>
        reporter.error(_exp.fullPosOpt, GclResolver.toolName, "Unexpected: type checking returned none")
        _exp
    }
    return rexp
  }

  def visitGclMethod(context: ISZ[String],
                     component: Option[AadlComponent],
                     gclMethod: GclMethod,
                     specMethods: Map[ISZ[String], GclMethod],
                     typeHierarchy: TypeHierarchy,
                     scope: Scope,
                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     reporter: Reporter): Option[GclMethod] = {

    val fqn = context :+ gclMethod.method.sig.id.value

    var rMethod: AST.Stmt.Method =
      resolvedMethods.get(fqn) match {
        case Some(resolvedMethod) =>
          gclMethod.method(sig = resolvedMethod.ast.sig, attr = resolvedMethod.ast.attr)
        case _ =>
          reporter.error(gclMethod.posOpt, toolName, st"Could not resolve method '${(fqn, "::")}'".render)
          return None()
      }

    val (ok, methodScope) = TypeChecker.methodScope(typeHierarchy, context, scope, rMethod.sig, reporter)
    if (ok) {

      // need to rewrite any array accesses for bound arrays (e.g. array(0) -> array(I(0))

      @pure def tce(e: Exp, mode: TypeChecker.ModeContext.Type): Exp = {
        val ret = typeCheckBoolExp(
          exp = e,
          context = context,
          mode = mode,
          component = component,
          params = rMethod.sig.params,
          stateVars = ISZ(), specFuns = specMethods,
          scope = methodScope, typeHierarchy = typeHierarchy,
          symbolTable = symbolTable, aadlTypes = aadlTypes, reporter = reporter)
        return ret
      }

      var mc = gclMethod.method.mcontract.asInstanceOf[Simple]

      val readsClause = mc.readsClause(refs = for (r <- mc.reads) yield tce(r.asExp, TypeChecker.ModeContext.Spec).asInstanceOf[Exp.Ident])
      val requiresClause = mc.requiresClause(claims = for (r <- mc.requires) yield tce(r, TypeChecker.ModeContext.Spec))
      val modifiesClause = mc.modifiesClause(refs = for (m <- mc.modifies) yield tce(m.asExp, TypeChecker.ModeContext.Spec).asInstanceOf[AST.Exp.Ident])
      val ensuresClause = mc.ensuresClause(claims = for (e <- mc.ensures) yield tce(e, TypeChecker.ModeContext.SpecPost))

      mc = mc(
        readsClause = readsClause,
        requiresClause = requiresClause,
        modifiesClause = modifiesClause,
        ensuresClause = ensuresClause)

      val body: Option[AST.Body] = (gclMethod.method.bodyOpt) match {
        case Some(body@AST.Body(ISZ(ret @ AST.Stmt.Return(Some(exp))))) =>
          val retType: ISZ[String] = for (id <- gclMethod.method.sig.returnType.asInstanceOf[AST.Type.Named].name.ids) yield id.value
          val retAadlType = aadlTypes.getTypeByPath(retType)
          val retSlangTypeName = GclResolverUtil.getSlangName(retAadlType, reporter)

          val rexp = typeCheckExp(exp = exp,
            expectedType = GclResolver.getActualSlangTypedName(retSlangTypeName, typeHierarchy),
            context = context,
            mode = TypeChecker.ModeContext.Code,
            component = component,
            params = rMethod.sig.params,
            stateVars = ISZ(), specFuns = specMethods,
            scope = methodScope, typeHierarchy = typeHierarchy,
            symbolTable = symbolTable, aadlTypes = aadlTypes, reporter = reporter)

          Some(body(stmts=ISZ(ret(expOpt=Some(rexp)))))
        case _ =>
          reporter.error(gclMethod.method.posOpt, GclResolver.toolName, "Unexpected: method does not have a body")
          None()
      }

      rMethod = rMethod(bodyOpt = body, mcontract = mc)

      // now do a full tipe check one the method using the rewritten expressions (will recheck the expressions)

      rMethod = if (rMethod.hasContract) TypeChecker.checkMethodContractSequent(
        // don't use methodScope here
        F, typeHierarchy, ISZ(rMethod.sig.id.value), scope, F, rMethod, reporter)
      else rMethod

      val tc = TypeChecker(typeHierarchy, fqn, F, TypeChecker.ModeContext.Spec, F)
      val resolvedMethod: AST.Stmt.Method = tc.checkMethod(scope, rMethod(bodyOpt = body, mcontract = mc), reporter)

      // update the GclMethod, except keep the original param and return type typing
      // since tipe will replace type aliases with their actual type
      // (e.g. changes Base_Types.Boolean to B).
      val resolvedGclMethod = gclMethod(method = resolvedMethod(
        sig = resolvedMethod.sig(
          params = gclMethod.method.sig.params,
          returnType = gclMethod.method.sig.returnType)))

      // update the global name map with the resolved method info
      val methodQualifiedName = context :+ resolvedMethod.sig.id.value
      assert (globalNameMap.contains(methodQualifiedName), s"Global name map did not contain $methodQualifiedName")
      replaceName(methodQualifiedName, Info.Method(
        owner = context,
        isInObject = T,
        scope = scope,
        hasBody = resolvedMethod.bodyOpt.nonEmpty,
        ast = resolvedMethod))

      return Some(resolvedGclMethod)
    } else {
      assert(reporter.hasError)
      return None()
    }
  }

  def processGclLib(context: ISZ[String],
                    gclLib: GclLib,
                    symbolTable: SymbolTable,
                    aadlTypes: AadlTypes,
                    typeHierarchy: TypeHierarchy,
                    scope: Scope,
                    reporter: Reporter): Option[(GclLib, GclSymbolTable)] = {

    var resolvedGclMethods: ISZ[GclMethod] = ISZ()

    for (gclMethod <- gclLib.methods) {
      visitGclMethod(context, None(), gclMethod, Map.empty, typeHierarchy, scope, symbolTable, aadlTypes, reporter) match {
        case Some(resolvedGclMethod) =>
          resolvedGclMethods = resolvedGclMethods :+ resolvedGclMethod
        case _ =>
      }
    }

    if (reporter.hasError) {
      return None()
    } else {
      return Some((
        gclLib(methods = resolvedGclMethods),
        GclSymbolTable(
          slangTypeHierarchy = typeHierarchy(
            nameMap = globalNameMap,
            typeMap = globalTypeMap),
          apiReferences = ISZ(),
          integrationMap = Map.empty,
          computeHandlerPortMap = Map.empty)))
    }
  }

  def processGclSubclause(component: AadlComponent,
                          subclause: GclSubclause,
                          libInfos: ISZ[GclAnnexLibInfo],
                          symbolTable: SymbolTable,
                          aadlTypes: AadlTypes,
                          typeHierarchy: TypeHierarchy,
                          scope: Scope.Local,
                          reporter: Reporter): Option[(GclSubclause, GclSymbolTable)] = {

    val componentPos = component.component.identifier.pos

    val context = component.classifier

    var libMethods: Map[ISZ[String], GclMethod] = Map.empty
    for (lib <- libInfos) {
      libMethods = libMethods ++ (for (m <- lib.annex.methods) yield (lib.name :+ m.method.sig.id.value) ~> m)
    }

    def visitInvariant(i: GclInvariant): Option[GclInvariant] = {
      visitSlangExp(exp = i.exp, context = context, scope = scope, mode = TypeChecker.ModeContext.Spec, typeHierarchy = typeHierarchy, reporter = reporter) match {
        case Some((rexp, roptType)) =>
          roptType match {
            case Some(AST.Typed.Name(ISZ("org", "sireum", "B"), _)) =>
              val (rexp2, _, _) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, indexingTypeFingerprints,
                ISZ(), libMethods, symbolTable, reporter)
              val resolvedExp: AST.Exp =
                if (rexp2.isEmpty) rexp
                else rexp2.get

              return Some(i(exp = resolvedExp))

            case Some(x) => reporter.error(i.exp.fullPosOpt, GclResolver.toolName, s"Expecting B but found ${x}")
            case _ => assert(reporter.hasError, "Invariant expression is untyped so Tipe should have reported errors already")
          }
        case _ => reporter.error(i.exp.fullPosOpt, GclResolver.toolName, "Unexpected: type checking returned none")
      }
      return None()
    }

    def visitGclSubclause(s: GclSubclause): Option[GclSubclause] = {
      var seenInvariantIds: Set[String] = Set.empty

      if (reporter.hasError) {
        return None()
      }

      val threadMethods: Map[ISZ[String], GclMethod] = Map.empty[ISZ[String], GclMethod] ++ (for (m <- s.methods) yield (context :+ m.method.sig.id.value) ~> m)

      // hmm, lost imports with AIR translation so assume all glc lib annexes have been imported
      val gclMethods = threadMethods ++ libMethods.entries

      var resolvedGclMethods: ISZ[GclMethod] = ISZ()
      for (gclMethod <- s.methods) {
        visitGclMethod(context, Some(component), gclMethod, gclMethods, typeHierarchy, scope, symbolTable, aadlTypes, reporter) match {
          case Some(resolvedGclMethod)=>
            resolvedGclMethods = resolvedGclMethods :+ resolvedGclMethod
          case _ =>
        }
      }

      var resolvedInvariants: ISZ[GclInvariant] = ISZ()
      for (i <- s.invariants) {
        if (seenInvariantIds.contains(i.id)) {
          reporter.error(i.exp.fullPosOpt, GclResolver.toolName, s"Duplicate invariant id: ${i.id}")
        }
        seenInvariantIds = seenInvariantIds + i.id
        visitInvariant(i) match {
          case Some(ri) => resolvedInvariants = resolvedInvariants :+ ri
          case _ =>
        }
      }

      var resolvedIntegration: Option[GclIntegration] = None()
      if (s.integration.nonEmpty) {
        var seenSpecNames: Set[String] = Set.empty
        val gclIntegration = s.integration.get

        var resolvedGclSpecs: ISZ[GclSpec] = ISZ()
        for (glcIntegSpec <- gclIntegration.specs) {

          if (seenSpecNames.contains(glcIntegSpec.id)) {
            reporter.error(glcIntegSpec.exp.fullPosOpt, GclResolver.toolName, s"Duplicate spec name: ${glcIntegSpec.id}")
          }
          seenSpecNames = seenSpecNames + glcIntegSpec.id

          val expTipe: AST.Exp = typeCheckBoolExp(
            exp = glcIntegSpec.exp, context = context,
            mode = TypeChecker.ModeContext.Spec,
            component = Some(component),
            params = ISZ(),
            stateVars = s.state, specFuns = gclMethods,
            symbolTable = symbolTable, aadlTypes = aadlTypes,
            scope = scope, typeHierarchy = typeHierarchy,
            reporter = reporter)

          if (!reporter.hasError) {
            val (expTrans, symbols, apiRefs) =
              GclResolver.collectSymbols(expTipe, RewriteMode.Normal, component, F, indexingTypeFingerprints,
            ISZ(), gclMethods, symbolTable, reporter)

            val resolvedExpr: AST.Exp = expTrans match {
              case MSome(et2) => et2
              case _ => expTipe
            }
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
                    val resolvedAssume = a(exp = resolvedExpr)
                    integrationMap = integrationMap + sym ~> resolvedAssume
                    resolvedGclSpecs = resolvedGclSpecs :+ resolvedAssume
                  case g: GclGuarantee =>
                    val resolvedGurarantee = g(exp = resolvedExpr)
                    integrationMap = integrationMap + sym ~> resolvedGurarantee
                    resolvedGclSpecs = resolvedGclSpecs :+ resolvedGurarantee
                }

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
        resolvedIntegration = Some(s.integration.get(specs = resolvedGclSpecs))
      }


      def checkFlow(exp: AST.Exp, isFrom: B, posOpt: Option[Position]): Option[Exp] = {
        exp match {
          case e: Exp.Ident =>
            visitSlangExp(exp = e, context = context, mode = TypeChecker.ModeContext.Spec, scope = scope, typeHierarchy = typeHierarchy, reporter = reporter) match {
              case Some((rexp, roptType)) =>
                val (rexp2, symbols, _) = GclResolver.collectSymbols(rexp, RewriteMode.Api, component, F, indexingTypeFingerprints,
                  s.state, gclMethods, symbolTable, reporter)
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
                  val resolvedExp: AST.Exp = rexp2 match {
                    case MSome(r) => r
                    case _ => rexp
                  }
                  return Some(resolvedExp)
                }
              case _ =>
                reporter.error(e.fullPosOpt, GclResolver.toolName, s"Unable to resolve $e")
            }
          case _ =>
            reporter.error(posOpt, GclResolver.toolName, s"Expecting from/to expressions to be Idents, found ${exp}")
        }
        return None()
      }

      def processModifiesClause(lmodifies: ISZ[Exp]): ISZ[Exp] = {
        var resolvedExpressions: ISZ[Exp] = ISZ()
        for (modifies <- lmodifies) {
          modifies match {
            case e: Exp.Ident =>
              visitSlangExp(exp = e, context = context, mode = TypeChecker.ModeContext.Spec, scope = scope, typeHierarchy = typeHierarchy, reporter = reporter) match {
                case Some((rexp, roptType)) =>
                  val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, indexingTypeFingerprints,
                    s.state, gclMethods, symbolTable, reporter)

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

                        case GclSymbolHolder(GclStateVar(_, _)) =>
                          val resolvedExpression: AST.Exp = rexp2 match {
                            case MSome(re2) => re2
                            case _ => rexp
                          }
                          resolvedExpressions = resolvedExpressions :+ resolvedExpression
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
        return resolvedExpressions
      }

      var resolvedInitializes: Option[GclInitialize] = None()
      if (s.initializes.nonEmpty) {

        val resolvedModifiesClause = processModifiesClause(s.initializes.get.modifies)

        var resolvedGuarantees: ISZ[GclGuarantee] = ISZ()
        var seenGuaranteeIds: Set[String] = Set.empty
        for (guarantees <- s.initializes.get.guarantees) {
          if (seenGuaranteeIds.contains(guarantees.id)) {
            reporter.error(guarantees.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantees.id}")
          }
          seenGuaranteeIds = seenGuaranteeIds + guarantees.id

          val rexp = typeCheckBoolExp(exp = guarantees.exp, context = context, mode = TypeChecker.ModeContext.SpecPost, component = Some(component),
            params = ISZ(),
            stateVars = s.state, specFuns = gclMethods,
            symbolTable = symbolTable, aadlTypes = aadlTypes,
            scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
          val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
            s.state, gclMethods, symbolTable, reporter)
          apiReferences = apiReferences ++ apiRefs

          val resolvedExp: AST.Exp = rexp2 match {
            case MSome(r) => r
            case _ => rexp
          }
          resolvedGuarantees = resolvedGuarantees :+ guarantees(exp = resolvedExp)
        }

        var resolvedFlows: ISZ[InfoFlowClause] = ISZ()
        for (flow <- s.initializes.get.flows) {
          if (flow.from.nonEmpty) {
            reporter.error(flow.posOpt, GclResolver.toolName, s"Initialize from clauses must be empty")
          }
          if (flow.to.isEmpty) {
            reporter.error(flow.posOpt, GclResolver.toolName, s"Initialize to clauses cannot be empty")
          }
          var resolvedToExprs: ISZ[Exp] = ISZ()
          for (toExp <- flow.to) {
            checkFlow(toExp, F, flow.posOpt) match {
              case Some(rtoExp) => resolvedToExprs = resolvedToExprs :+ rtoExp
              case _ =>
            }
          }
          resolvedFlows = resolvedFlows :+ flow(to = resolvedToExprs)
        }
        resolvedInitializes = Some(s.initializes.get(
          modifies = resolvedModifiesClause,
          guarantees = resolvedGuarantees,
          flows = resolvedFlows))
      }

      var resolvedCompute: Option[GclCompute] = None()
      s.compute match {
        case Some(gclCompute @ GclCompute(modifies, assumes, guarantees, cases, handlers, flows)) => {

          val resolvedModifies = processModifiesClause(modifies)

          var resolvedAssume: ISZ[GclAssume] = ISZ()
          var seenSpecIds: Set[String] = Set.empty
          for (assumee <- assumes) {
            if (seenSpecIds.contains(assumee.id)) {
              reporter.error(assumee.posOpt, GclResolver.toolName, s"Duplicate spec name: ${assumee.id}")
            }
            seenSpecIds = seenSpecIds + assumee.id

            {
              val rexp = typeCheckBoolExp(exp = assumee.exp, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                params = ISZ(),
                stateVars = s.state, specFuns = gclMethods,
                symbolTable = symbolTable, aadlTypes = aadlTypes,
                scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
              val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, T, indexingTypeFingerprints,
                s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              for (sym <- symbols) {
                sym match {
                  case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                    reporter.error(assumee.exp.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                  case _ =>
                }
              }
              val resolvedExp: AST.Exp = rexp2 match {
                case MSome(r) => r
                case _ => rexp
              }
              resolvedAssume = resolvedAssume :+ assumee(exp = resolvedExp)
            }
          }

          var resolvedGuarantee: ISZ[GclGuarantee] = ISZ()
          for (guarantee <- guarantees) {
            if (seenSpecIds.contains(guarantee.id)) {
              reporter.error(guarantee.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guarantee.id}")
            }
            seenSpecIds = seenSpecIds + guarantee.id

            val rexp = typeCheckBoolExp(exp = guarantee.exp, context = context, mode = TypeChecker.ModeContext.SpecPost, component = Some(component),
              params = ISZ(),
              stateVars = s.state, specFuns = gclMethods,
              symbolTable = symbolTable, aadlTypes = aadlTypes,
              scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
            val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
              s.state, gclMethods, symbolTable, reporter)
            apiReferences = apiReferences ++ apiRefs

            val resolvedExpr: AST.Exp = rexp2 match {
              case MSome(r) => r
              case _ => rexp
            }
            resolvedGuarantee = resolvedGuarantee :+ guarantee(exp = resolvedExpr)
          }

          var resolvedCases: ISZ[GclCaseStatement] = ISZ()
          for (caase <- cases) {
            if (seenSpecIds.contains(caase.id)) {
              reporter.error(caase.posOpt, GclResolver.toolName, s"Duplicate spec name: ${caase.id}")
            }
            seenSpecIds = seenSpecIds + caase.id

            var resolvedCaseAssumes: Option[AST.Exp] = None()
            caase.assumes match {
              case Some(assumes2) =>
                val rexp = typeCheckBoolExp(exp = assumes2, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                  params = ISZ(),
                  stateVars = s.state, specFuns = gclMethods,
                  symbolTable = symbolTable, aadlTypes = aadlTypes,
                  scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
                val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
                  s.state, gclMethods, symbolTable, reporter)
                apiReferences = apiReferences ++ apiRefs

                for (sym <- symbols) {
                  sym match {
                    case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                      reporter.error(assumes2.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                    case _ =>
                  }
                }
                val resolvedExp: AST.Exp = rexp2 match {
                  case MSome(r) => r
                  case _ => rexp
                }
                resolvedCaseAssumes = Some(resolvedExp)
              case _ =>
            }

            var resolvedCaseGuarantee: AST.Exp = caase.guarantees

            {
              val rexp = typeCheckBoolExp(exp = caase.guarantees, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                params = ISZ(),
                stateVars = s.state, specFuns = gclMethods,
                symbolTable = symbolTable, aadlTypes = aadlTypes,
                scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
                s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              val resolvedExp: AST.Exp = rexp2 match {
                case MSome(r) => r
                case _ => rexp
              }
              resolvedCaseGuarantee = resolvedExp
            }

            resolvedCases = resolvedCases :+ caase(
              assumes = resolvedCaseAssumes,
              guarantees = resolvedCaseGuarantee)
          }

          var resolvedHandlers: ISZ[GclHandle] = ISZ()
          for (handler <- handlers) {

            var resolvedHandlerPort = handler.port
            visitSlangExp(exp = handler.port, context = context, mode = TypeChecker.ModeContext.Spec, scope = scope, typeHierarchy = typeHierarchy, reporter = reporter) match {
              case Some((rexp, roptType)) =>
                val (hexp, symbols, _) = GclResolver.collectSymbols(rexp, RewriteMode.Normal, component, F, indexingTypeFingerprints,
                  s.state, gclMethods, symbolTable, reporter)
                if (!reporter.hasError) {
                  if (symbols.size != 1) {
                    reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Handler should resolve to exactly one symbol, instead resolved to ${symbols.size}")
                  }
                  symbols(0) match {
                    case AadlSymbolHolder(p: AadlPort) =>
                      if (computeHandlerPortMap.contains(handler.port)) {
                        reporter.error(handler.port.fullPosOpt, GclResolver.toolName,
                          s"Only a single handler is allowed per port")
                      }
                      computeHandlerPortMap = computeHandlerPortMap + handler.port ~> p

                      if (p.direction != Direction.In || p.isInstanceOf[AadlDataPort]) {
                        reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Compute handlers can only be applied to incoming event or event data ports")
                      }

                      val resolvedExp: AST.Exp = hexp match {
                        case MSome(r) => r
                        case _ => rexp
                      }
                      resolvedHandlerPort = resolvedExp
                    case x => reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Handler should resolve to an AADL port but received $x")
                  }
                }
              case _ => reporter.error(handler.posOpt, GclResolver.toolName, s"Unable to resolve ${handler.port}")
            }

            val resolvedHandlerModifiesClause = processModifiesClause(handler.modifies)

            var resolvedHandlerAssumes: ISZ[GclAssume] = ISZ()
            var handlerSpecIds: Set[String] = Set.empty
            for (assm <- handler.assumes) {
              if (seenSpecIds.contains(assm.id) || handlerSpecIds.contains(assm.id)) {
                reporter.error(assm.posOpt, GclResolver.toolName, s"Duplicate spec name: ${assm.id}")
              }
              handlerSpecIds = handlerSpecIds + assm.id

              val rexp = typeCheckBoolExp(exp = assm.exp, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                params = ISZ(),
                stateVars = s.state, specFuns = gclMethods,
                symbolTable = symbolTable, aadlTypes = aadlTypes,
                scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
              val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, T, indexingTypeFingerprints,
                s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              for (sym <- symbols) {
                sym match {
                  case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                    reporter.error(assm.exp.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                  case _ =>
                }
              }

              val resolvedExp: AST.Exp = rexp2 match {
                case MSome(r) => r
                case _ => rexp
              }
              resolvedHandlerAssumes = resolvedHandlerAssumes :+ assm(exp = resolvedExp)
            }

            var resolvedHandlerGuarantees: ISZ[GclGuarantee] = ISZ()
            for (guar <- handler.guarantees) {
              if (seenSpecIds.contains(guar.id) || handlerSpecIds.contains(guar.id)) {
                reporter.error(guar.posOpt, GclResolver.toolName, s"Duplicate spec name: ${guar.id}")
              }
              handlerSpecIds = handlerSpecIds + guar.id

              val rexp = typeCheckBoolExp(exp = guar.exp, context = context, mode = TypeChecker.ModeContext.SpecPost, component = Some(component),
                params = ISZ(),
                stateVars = s.state, specFuns = gclMethods,
                symbolTable = symbolTable, aadlTypes = aadlTypes,
                scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)

              val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
                s.state, gclMethods, symbolTable, reporter)
              apiReferences = apiReferences ++ apiRefs

              val resolvedExp: AST.Exp = rexp2 match {
                case MSome(r) => r
                case _ => rexp
              }
              resolvedHandlerGuarantees = resolvedHandlerGuarantees :+ guar(exp = resolvedExp)
            }

            var resolvedHandlerCases: ISZ[GclCaseStatement] = ISZ()
            for (caase <- handler.cases) {
              if (seenSpecIds.contains(caase.id)) {
                reporter.error(caase.posOpt, GclResolver.toolName, s"Duplicate spec name: ${caase.id}")
              }
              seenSpecIds = seenSpecIds + caase.id

              var resolvedHandlerCaseAssumes: Option[AST.Exp] = None()
              caase.assumes match {
                case Some(assumes2) =>
                  val rexp = typeCheckBoolExp(exp = assumes2, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                    params = ISZ(),
                    stateVars = s.state, specFuns = gclMethods,
                    symbolTable = symbolTable, aadlTypes = aadlTypes,
                    scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
                  val (rexp2, symbols, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
                    s.state, gclMethods, symbolTable, reporter)
                  apiReferences = apiReferences ++ apiRefs

                  for (sym <- symbols) {
                    sym match {
                      case AadlSymbolHolder(i: AadlPort) if i.direction == Direction.Out =>
                        reporter.error(assumes2.fullPosOpt, toolName, "Assume clauses cannot refer to outgoing ports")
                      case _ =>
                    }
                  }
                  val resolvedExp: AST.Exp = rexp2 match {
                    case MSome(r) => r
                    case _ => rexp
                  }
                  resolvedHandlerCaseAssumes = Some(resolvedExp)
                case _ =>
              }

              var resolvedHandlerCaseGuarantees = caase.guarantees

              {
                val rexp = typeCheckBoolExp(exp = caase.guarantees, context = context, mode = TypeChecker.ModeContext.Spec, component = Some(component),
                  params = ISZ(),
                  stateVars = s.state, specFuns = gclMethods,
                  symbolTable = symbolTable, aadlTypes = aadlTypes,
                  scope = scope, typeHierarchy = typeHierarchy, reporter = reporter)
                val (rexp2, _, apiRefs) = GclResolver.collectSymbols(rexp, RewriteMode.ApiGet, component, F, indexingTypeFingerprints,
                  s.state, gclMethods, symbolTable, reporter)
                apiReferences = apiReferences ++ apiRefs

                val resolvedExp: AST.Exp = rexp2 match {
                  case MSome(r) => r
                  case _ => rexp
                }
                resolvedHandlerCaseGuarantees = resolvedExp
              }

              resolvedHandlerCases = resolvedHandlerCases :+ caase(
                assumes = resolvedHandlerCaseAssumes,
                guarantees = resolvedHandlerCaseGuarantees
              )
            }

            component match {
              case a: AadlDispatchableComponent =>
                if (!a.isSporadic()) {
                  reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Compute handlers can only be used with sporadic components")
                }
              case _ =>
                reporter.error(handler.port.fullPosOpt, GclResolver.toolName, s"Unexpected: Compute handlers can only be used with dispatchable components")
            }

            resolvedHandlers = resolvedHandlers :+ handler(
              port = resolvedHandlerPort,
              modifies = resolvedHandlerModifiesClause,
              assumes = resolvedHandlerAssumes,
              guarantees = resolvedHandlerGuarantees,
              cases = resolvedHandlerCases)
          }

          var resolvedFlows: ISZ[InfoFlowClause] = ISZ()
          for (flow <- flows) {
            if (flow.from.isEmpty && flow.to.isEmpty) {
              reporter.error(flow.posOpt, GclResolver.toolName, s"At least one of the from/to clauses must be non-empty")
            }
            var resolvedFromExps: ISZ[Exp] = ISZ()
            for (fromExp <- flow.from) {
              checkFlow(fromExp, T, flow.posOpt) match {
                case Some(ff) => resolvedFromExps = resolvedFromExps :+ ff
                case _ =>
              }
            }
            var resolvedToExps: ISZ[Exp] = ISZ()
            for (toExp <- flow.to) {
              checkFlow(toExp, F, flow.posOpt) match {
                case Some(tf) => resolvedToExps = resolvedToExps :+ tf
                case _ =>
              }
            }
            resolvedFlows = resolvedFlows :+ flow(from = resolvedFromExps, to = resolvedToExps)
          }


          resolvedCompute = Some(gclCompute(
            modifies = resolvedModifies,
            assumes = resolvedAssume,
            guarantees = resolvedGuarantee,
            cases = resolvedCases,
            handlers = resolvedHandlers,
            flows = resolvedFlows))
        }
        case Some(x) => reporter.error(componentPos, toolName, s"Expecting GclCompute but received ${x}")
        case _ =>
      }

      return Some(s(
        methods = resolvedGclMethods,
        invariants = resolvedInvariants,
        initializes = resolvedInitializes,
        integration = resolvedIntegration,
        compute = resolvedCompute))

    }

    visitGclSubclause(subclause) match {
      case Some(resolvedGclSubclause) =>
        return Some((
          resolvedGclSubclause,
          GclSymbolTable(
            slangTypeHierarchy = typeHierarchy(
              nameMap = globalNameMap,
              typeMap = globalTypeMap),
            apiReferences = apiReferences.elements,
            integrationMap = integrationMap,
            computeHandlerPortMap = computeHandlerPortMap)))
      case _ => return None()
    }
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

    def globalImports(packageName: ISZ[String]): ISZ[AST.Stmt.Import] = {

      val emptyAttr = AST.Attr(None())

      val libImport: ISZ[AST.Stmt.Import.Importer] = {
        for(l <- gclLibs.filter(f => f.containingPackage.name == packageName)) yield 
          AST.Stmt.Import.Importer(
            name = AST.Name(
              ids = (for(n <- l.containingPackage.name) yield AST.Id(n, emptyAttr)) :+ AST.Id("GUMBO__Library", emptyAttr), attr = emptyAttr),
            selectorOpt = Some(AST.Stmt.Import.WildcardSelector()))
      }

      assert (libImport.size <= 1, libImport.size.string)

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

      return (
        arrayIndexInterpolateImports ++
          (for (m <- libImport ++ sireumImporters) yield AST.Stmt.Import(importers = ISZ(m), attr = emptyAttr)))
    }

    if (builtTypeInfo) {
      return
    }
    builtTypeInfo = T

    def resolveType(value: AST.Type, posOpt: Option[Position]): AST.Type.Named = {
      val pos: Option[Position] = if (value.posOpt.nonEmpty) value.posOpt else posOpt
      value match {
        case atn: AST.Type.Named =>
          val typeIds: ISZ[String] = for (id <- atn.name.ids) yield id.value
          val aadlName = TypeUtil.getAadlTypeFromSlangType(typeIds)

          return resolveTypeH(getAadlType(aadlName, aadlTypes, pos, reporter), posOpt)

        case x =>
          reporter.error(value.posOpt, GclResolver.toolName, s"Wasn't expecting $x")
          return AST.Type.Named(name = AST.Name(ids = ISZ(), attr = AST.Attr(None())), typeArgs = ISZ(), attr = AST.TypedAttr(None(), None()))
      }
    }

    def resolveTypeH(aadlType: AadlType, posOpt: Option[Position]): AST.Type.Named = {
      val typeInfo = buildTypeInfo(aadlType)
      val pos: Option[Position] = if (typeInfo.posOpt.nonEmpty) typeInfo.posOpt else posOpt

      val (typeName, typedName): (ISZ[String], AST.Typed) = typeInfo match {
        case ta: TypeInfo.TypeAlias =>
          // use the actual type for the typed opt
          (ta.name, ta.ast.tipe.asInstanceOf[AST.Type.Named].attr.typedOpt.get.asInstanceOf[AST.Typed.Name])

        case tadt: TypeInfo.Adt =>
          (tadt.tpe.ids, AST.Typed.Name(tadt.tpe.ids, ISZ()))

        case te: TypeInfo.Enum =>
          (te.name, AST.Typed.Name(te.name, ISZ()))

        case x =>
          reporter.error(pos, GclResolver.toolName, s"Wasn't expecting a $x")
          (ISZ(), AST.Typed.Name(ISZ(), ISZ()))
      }

      return AST.Type.Named(
        name = AST.Name(for (t <- typeName) yield AST.Id(t, AST.Attr(None())), AST.Attr(None())),
        typeArgs = ISZ(),
        attr = AST.TypedAttr(posOpt = None(), typedOpt = Some(typedName))
      )
    }

    @pure def TODO_TYPE(t: AadlType): TypeInfo.Adt = {
      val adtAst: AST.Stmt.Adt =
        AST.Stmt.Adt(
          isRoot = F,
          isDatatype = T,
          isUnclonable = F,
          id = AST.Id("TODO", AST.Attr(None())),
          typeParams = ISZ(),
          params = ISZ(),
          parents = ISZ(),
          stmts = ISZ(),
          attr = AST.Attr(None()))

      return TypeInfo.Adt(
        owner = ISZ("TODO"),
        outlined = F,
        contractOutlined = F,
        typeChecked = F,
        tpe = AST.Typed.Name(ISZ(s"TODO_${t.name}"), ISZ()),
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
        scope = scope(ISZ("TODO"), globalImports(ISZ("TODO")), ISZ("TODO", s"TODO_${t.name}")),
        ast = adtAst)
    }

    def buildTypeInfo(aadlType: AadlType): TypeInfo = {

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
            val ri: ResolvedInfo.EnumElement = ResolvedInfo.EnumElement(
              owner = qualifiedName,
              name = value,
              ordinal = ordinal)
            ordinal = ordinal + 1
            elements = elements + (value ~> ri)
          }

          val posOpt: Option[Position] = None()

          val enumx: TypeInfo.Enum = TypeInfo.Enum(qualifiedName, elements, posOpt)

          slangTypeToAadlType = slangTypeToAadlType + enumx.tpe ~> qualifiedTypeName
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
          val aadlData = symbolTable.componentMap.get(ISZ(r.name)).get.asInstanceOf[AadlData]

          return buildAdtTypeInfo(aadlData)

        case a: ArrayType =>
          if (a.dimensions.size > 1) {
            reporter.error(None(), GclResolver.toolName, s"Only single dimension arrays are supported.  ${a.name} has ${a.dimensions.size}")
          }

          if (globalTypeMap.contains(qualifiedName)) {
            return globalTypeMap.get(qualifiedName).get
          }

          val packageName = ops.ISZOps(qualifiedName).dropRight(1)
          val simpleName = qualifiedName(qualifiedName.lastIndex)

          buildPackageInfo(packageName)

          val emptyAttr = AST.Attr(None())

          val baseTypeFQN: ISZ[String] = GclResolverUtil.getSlangName(a.baseType, reporter)

          val adtScope = scope(packageName, globalImports(packageName), qualifiedTypeName)

          modelContainsBoundArrays = modelContainsBoundArrays || a.dimensions.nonEmpty

          val (seqName, indexingType): (String, AST.Typed.Name) =
            if (a.dimensions.isEmpty)
              ("ISZ", AST.Typed.Name(ids = ISZ("org", "sireum", "Z"), args = ISZ()))
            else
              ("IS", AST.Typed.Name(ids = qualifiedName :+ "I", args = ISZ()))

          // type ta = IS[IndexingType, baseType]
          val ta = TypeInfo.TypeAlias(
            name = qualifiedName,
            scope = adtScope,
            ast = AST.Stmt.TypeAlias(
              // original name
              id = AST.Id(simpleName, emptyAttr),
              typeParams = ISZ(),
              // IS[IndexingType, baseType]
              tipe = AST.Type.Named(
                // IS
                name = AST.Name(ids = ISZ(AST.Id(seqName, emptyAttr)), attr = emptyAttr),
                // [IndexingType, baseType]
                typeArgs = ISZ(
                  // IndexingType
                  AST.Type.Named(
                    name = AST.Name(ids = for (id <- indexingType.ids) yield AST.Id(id, emptyAttr), attr = emptyAttr),
                    typeArgs = ISZ(),
                    attr = AST.TypedAttr(
                      posOpt = None(),
                      typedOpt = Some(AST.Typed.Name(ids = indexingType.ids, args = ISZ())))),
                  // baseType
                  AST.Type.Named(
                    name = AST.Name(ids = for (b <- baseTypeFQN) yield AST.Id(b, emptyAttr), attr = emptyAttr),
                    typeArgs = ISZ(),
                    attr = AST.TypedAttr(
                      posOpt = None(),
                      typedOpt = Some(AST.Typed.Name(ids = baseTypeFQN, args = ISZ()))))),
                attr = AST.TypedAttr(
                  posOpt = None(),
                  typedOpt = Some(AST.Typed.Name(
                    ids = ISZ("org", "sireum", "IS"),
                    args = ISZ(
                      indexingType,
                      AST.Typed.Name(ids = baseTypeFQN, args = ISZ())
                    ))))),
              attr = emptyAttr))

          slangTypeToAadlType = slangTypeToAadlType + AST.Typed.Name(ids = qualifiedTypeName, args = ISZ()) ~> qualifiedTypeName
          slangTypeToAadlType = slangTypeToAadlType + ta.tpe ~> qualifiedTypeName
          globalTypeMap = globalTypeMap + (qualifiedTypeName ~> ta)


          if (a.dimensions.nonEmpty) {
            // introduce companion object that contains
            //  - the indexing type definition, "@range(min=x, max=y, index=T) class I"
            //  - the type alias, "type <aadlTypeName> = IS[I, <baseType>]"
            //  - a method whose name is the indexing type's fingerprint.  It takes a Z and returns an I
            val indexTypeFingerprint = TypeUtil.getIndexingTypeFingerprintMethodName(indexingType.ids)
            indexingTypeFingerprints = indexingTypeFingerprints + indexTypeFingerprint ~> indexingType.ids

            val fingerMethodAST =
              AST.Stmt.Method(
                typeChecked = F,
                purity = Purity.StrictPure,
                modifiers = ISZ("@strictpure"),
                sig = AST.MethodSig(
                  purity = Purity.StrictPure,
                  annotations = ISZ(),
                  id = AST.Id(indexTypeFingerprint, emptyAttr),
                  typeParams = ISZ(),
                  hasParams = T,
                  params = ISZ(AST.Param(
                    isHidden = F,
                    id = AST.Id("i", emptyAttr),
                    tipe = AST.Type.Named(
                      name = AST.Name(ISZ(AST.Id("Z", emptyAttr)), emptyAttr),
                      typeArgs = ISZ(),
                      attr = AST.TypedAttr(typedOpt = Some(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())), posOpt = None())))),
                  returnType = AST.Type.Named(
                    name = AST.Name(ISZ(AST.Id("I", emptyAttr)), emptyAttr),
                    typeArgs = ISZ(),
                    attr = AST.TypedAttr(typedOpt = Some(AST.Typed.Name(ids = qualifiedName :+ "I", args = ISZ())), posOpt = None()))),
                mcontract = AST.MethodContract.Simple.empty,
                bodyOpt = Some(AST.Body(
                  stmts = ISZ(
                    // val _r_: I = I.fromZ(i)
                    AST.Stmt.Var(isSpec = F, isVal = T, id = AST.Id("_r_", emptyAttr),
                      tipeOpt = Some(AST.Type.Named(
                        name = AST.Name(ISZ(AST.Id("I", emptyAttr)), emptyAttr), typeArgs = ISZ(),
                        attr = AST.TypedAttr(typedOpt = Some(AST.Typed.Name(qualifiedName :+ "I", ISZ())), posOpt = None()))),

                      // ... = I.fromZ(i)
                      initOpt = Some(AST.Stmt.Expr(
                        exp = AST.Exp.Invoke(
                          receiverOpt = Some(AST.Exp.Ident(AST.Id("I", emptyAttr), AST.ResolvedAttr(
                            posOpt = None(),
                            resOpt = Some(AST.ResolvedInfo.Object(qualifiedName :+ "I")),
                            typedOpt = Some(AST.Typed.Object(owner = qualifiedName, id = "I"))))),
                          ident = AST.Exp.Ident(
                            id = AST.Id("fromZ", emptyAttr),
                            attr = AST.ResolvedAttr(posOpt = None(),
                              resOpt = Some(AST.ResolvedInfo.Method(isInObject = T, mode = AST.MethodMode.Method, typeParams = ISZ(),
                                owner = qualifiedName :+ "I", id = "fromZ", paramNames = ISZ("n"),
                                tpeOpt = Some(AST.Typed.Fun(
                                  purity = AST.Purity.Pure, isByName = F,
                                  args = ISZ(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())),
                                  ret = AST.Typed.Name(qualifiedName :+ "I", ISZ()))),
                                reads = ISZ(), writes = ISZ())),
                              typedOpt = Some(AST.Typed.Method(
                                isInObject = T, mode = AST.MethodMode.Ext, typeParams = ISZ(),
                                owner = qualifiedName :+ "I", name = "fromZ", paramNames = ISZ("n"),
                                tpe = AST.Typed.Fun(purity = AST.Purity.Pure, isByName = F,
                                  args = ISZ(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())),
                                  ret = AST.Typed.Name(qualifiedName :+ "I", ISZ())))))),
                          targs = ISZ(),
                          args = ISZ(
                            AST.Exp.Ident(AST.Id("i", emptyAttr), AST.ResolvedAttr(
                              posOpt = None(),
                              resOpt = Some(AST.ResolvedInfo.LocalVar(context = qualifiedName :+ indexTypeFingerprint, scope = AST.ResolvedInfo.LocalVar.Scope.Current,
                                isSpec = F, isVal = T, id = "i")),
                              typedOpt = Some(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ()))
                            ))),
                          attr = AST.ResolvedAttr(
                            posOpt = None(),
                            resOpt = Some(AST.ResolvedInfo.Method(
                              isInObject = T, mode = AST.MethodMode.Ext, typeParams = ISZ(),
                              owner = qualifiedName :+ "I", id = "fromZ", paramNames = ISZ("n"),
                              tpeOpt = Some(AST.Typed.Fun(
                                purity = AST.Purity.Pure, isByName = F, args = ISZ(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())),
                                ret = AST.Typed.Name(qualifiedName :+ "I", ISZ()))),
                              reads = ISZ(), writes = ISZ())),
                            typedOpt = Some(AST.Typed.Name(qualifiedName :+ "I", ISZ())))),
                        attr = AST.TypedAttr(posOpt = None(), typedOpt = Some(AST.Typed.Name(qualifiedName :+ "I", ISZ())))
                      )),
                      attr = AST.ResolvedAttr(posOpt = None(),
                        resOpt = Some(AST.ResolvedInfo.LocalVar(context = qualifiedName :+ indexTypeFingerprint, scope = AST.ResolvedInfo.LocalVar.Scope.Current,
                          isSpec = F, isVal = T, id = "_r_")),
                        typedOpt = Some(AST.Typed.Name(qualifiedName :+ "I", ISZ())))),

                    // return _r_
                    AST.Stmt.Return(
                      expOpt = Some(AST.Exp.Ident(AST.Id("_r_", emptyAttr),
                        AST.ResolvedAttr(posOpt = None(),
                          resOpt = Some(ResolvedInfo.LocalVar(
                            context = qualifiedName :+ indexTypeFingerprint, scope = AST.ResolvedInfo.LocalVar.Scope.Current,
                            isSpec = F, isVal = T, id = "_r")),
                          typedOpt = Some(AST.Typed.Name(qualifiedName :+ "I", ISZ()))))),
                      attr = AST.TypedAttr(posOpt = None(), typedOpt = Some(
                        AST.Typed.Name(qualifiedName :+ "I", ISZ()))))
                  ),
                  undecls = ISZ(
                    AST.ResolvedInfo.LocalVar(
                      id = "i", context = qualifiedName :+ indexTypeFingerprint, scope = AST.ResolvedInfo.LocalVar.Scope.Current, isSpec = F, isVal = T),
                    AST.ResolvedInfo.LocalVar(
                      id = "_r", context = qualifiedName :+ indexTypeFingerprint, scope = AST.ResolvedInfo.LocalVar.Scope.Current, isSpec = F, isVal = T)))),
                attr = AST.ResolvedAttr(posOpt = None(),
                  resOpt = Some(AST.ResolvedInfo.Method(
                    isInObject = T, mode = AST.MethodMode.Method, typeParams = ISZ(),
                    owner = qualifiedName,
                    id = indexTypeFingerprint,
                    paramNames = ISZ("i"),
                    tpeOpt = Some(AST.Typed.Fun(
                      purity = Purity.StrictPure, isByName = F,
                      args = ISZ(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())),
                      ret = AST.Typed.Name(qualifiedName :+ "I", ISZ()))),
                    reads = ISZ(),
                    writes = ISZ())),
                  typedOpt = Some(AST.Typed.Method(isInObject = T, mode = AST.MethodMode.Method, typeParams = ISZ(),
                    owner = qualifiedName,
                    name = indexTypeFingerprint,
                    paramNames = ISZ("i"),
                    tpe = AST.Typed.Fun(purity = Purity.StrictPure, isByName = F,
                      args = ISZ(AST.Typed.Name(ISZ("org", "sireum", "Z"), ISZ())),
                      ret = AST.Typed.Name(qualifiedName :+ "I", ISZ()))))
                ))

            val fingerMethodObj = Info.Method(
              owner = qualifiedName,
              isInObject = T,
              scope = adtScope,
              hasBody = T,
              ast = fingerMethodAST)

            declareName(indexTypeFingerprint, qualifiedName :+ indexTypeFingerprint, fingerMethodObj, None(), reporter)

            val indexingTypeTypeInfo = TypeInfo.SubZ(
              owner = packageName :+ simpleName,
              ast = // @range(min=x max=x, index=T) class I
                AST.Stmt.SubZ(
                  id = AST.Id("I", emptyAttr),
                  min = 0, max = a.dimensions(0) - 1, isIndex = T,
                  isSigned = F, isBitVector = F, isWrapped = F, hasMin = T, hasMax = T, bitWidth = 0, index = 0, attr = emptyAttr))

            globalTypeMap = globalTypeMap + ((packageName :+ simpleName :+ "I") ~> indexingTypeTypeInfo)

            val obj = Info.Object(
              owner = packageName,
              isSynthetic = F,
              scope = adtScope,
              outlined = F,
              contractOutlined = F,
              typeChecked = F,
              ast = AST.Stmt.Object(
                isApp = F,
                extNameOpt = None(),
                id = AST.Id(simpleName, emptyAttr),
                stmts = ISZ(

                  // @range(min=x max=x, index=T) class I
                  indexingTypeTypeInfo.ast,

                  // type <aadlTypeName> = IS[I, <baseType>]
                  ta.ast,

                  // @strictpure def <indexTypeFingerprint>(i: Z): I = {
                  //   val _r_: I = I.fromZ(i)
                  //   return _r_
                  // }
                  fingerMethodAST
                ),
                attr = emptyAttr
              ),
              typedOpt = Some(AST.Typed.Object(owner = packageName, id = simpleName)),
              resOpt = Some(AST.ResolvedInfo.Object(name = qualifiedName)),
              constructorRes = AST.ResolvedInfo.Method(
                isInObject = F,
                mode = AST.MethodMode.ObjectConstructor,
                typeParams = ISZ(),
                owner = packageName,
                id = simpleName,
                paramNames = ISZ(),
                tpeOpt = None(), // this is null in the debugger
                reads = ISZ(),
                writes = ISZ()))

            declareName(simpleName, qualifiedName, obj, None(), reporter)

            // import the subz fingerprint method
            val fname: ISZ[AST.Id] = for (n <- qualifiedName :+ indexTypeFingerprint) yield AST.Id(n, emptyAttr)
            arrayIndexInterpolateImports = arrayIndexInterpolateImports :+
              AST.Stmt.Import(importers = ISZ(AST.Stmt.Import.Importer(
                name = AST.Name(fname, emptyAttr), selectorOpt = None())), attr = emptyAttr)
          }

          val gclAnnexes = a.container.get.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])
          if (gclAnnexes.nonEmpty) {
            halt("TODO: invariant added to an ISZ()")
          }

          return ta

        case b: BaseType =>
          val simpleName = b.simpleName
          val packageName = ops.ISZOps(qualifiedTypeName).dropRight(1)
          buildPackageInfo(packageName)

          val imports: ISZ[AST.Stmt.Import] = ISZ()

          val scope: Scope.Global = Scope.Global(packageName, imports, qualifiedTypeName)

          val slangName = GclResolverUtil.getSlangName(b, reporter)

          val ast: AST.Stmt.TypeAlias = {
            val _id = AST.Id(simpleName, AST.Attr(None()))
            val typeParams: ISZ[TypeParam] = ISZ()
            val simpleSireumName: String = slangName(slangName.lastIndex)
            val tipe: AST.Type = AST.Type.Named(
              name = AST.Name(
                ids = ISZ(AST.Id(simpleSireumName, AST.Attr(None()))),
                attr = AST.Attr(None())
              ),
              typeArgs = ISZ(),
              attr = AST.TypedAttr(
                posOpt = None(),
                typedOpt = Some(AST.Typed.Name(
                  ids = slangName,
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

          slangTypeToAadlType = slangTypeToAadlType + AST.Typed.Name(ids = b.classifier, args = ISZ()) ~> qualifiedTypeName
          slangTypeToAadlType = slangTypeToAadlType + ta.tpe ~> qualifiedTypeName

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
              isUnclonable = F,
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
            scope = scope(ISZ("art"), globalImports(ISZ("art")), ISZ("art", "Empty")),
            ast = adtAst)

          globalTypeMap = globalTypeMap + (ISZ("art", "Empty") ~> typeInfoAdt)

          return typeInfoAdt

        case b: BitType => return TODO_TYPE(b)
        case x => return TODO_TYPE(x)
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

      val adtScope = scope(packageName, globalImports(packageName), adtQualifiedName)

      for (gclMethod <- g.methods) {
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

      val resolvedParams: ISZ[AST.Param] = for (p <- m.sig.params) yield
        p(tipe = resolveType(p.tipe, p.id.attr.posOpt))

      val resolvedReturnType = resolveType(m.sig.returnType, m.sig.id.attr.posOpt)

      val resolvedReturnTyped: AST.Typed = resolvedReturnType.attr.typedOpt.get

      val resolvedTypeParams = m.sig.typeParams
      assert(resolvedTypeParams.isEmpty, "Not handling type params yet")

      val resolvedMsig = AST.MethodSig(
        purity = m.sig.purity,
        annotations = ISZ(),
        id = m.sig.id,
        typeParams = m.sig.typeParams,
        hasParams = resolvedParams.nonEmpty,
        params = resolvedParams,
        returnType = resolvedReturnType)

      val resolvedTypedFun = AST.Typed.Fun(
        purity = m.sig.purity,
        isByName = F,
        args = resolvedParams.map((p: AST.Param) => p.tipe.asInstanceOf[AST.Type.Named].attr.typedOpt.get),
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
          val paramTypeName = resolveType(p.tipe, None())
          val typedOpt: Option[AST.Typed] = paramTypeName.attr.typedOpt

          val lv = Info.LocalVar(
            name = qualifiedMethodName :+ p.id.value,
            isVal = T,
            ast = AST.Id(value = p.id.value, attr = AST.Attr(posOpt = None())),
            typedOpt = typedOpt, //
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
      val threadsScope = scope(threadsOwner, globalImports(threadsOwner), threadsName)

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
        val fieldAadlType: AadlType = field match {
          case afd: AadlFeatureData => afd.aadlType
          case _ => TypeUtil.EmptyType
        }

        val qualifiedFieldName = threadsName :+ fieldId

        val fieldResInfoOpt = Some[AST.ResolvedInfo](
          AST.ResolvedInfo.Var(isInObject = T, isSpec = F, isVal = F, owner = threadsName, id = fieldId))

        val fieldType = resolveTypeH(fieldAadlType, None())

        val infoVar = Info.Var(
          owner = threadsName,
          isInObject = T,
          scope = scope(
            packageName = threadsPackageName,
            imports = globalImports(threadsPackageName),
            enclosingName = threadsName),
          ast = AST.Stmt.Var(
            isSpec = F,
            isVal = F,
            id = AST.Id(value = fieldId, attr = AST.Attr(None())),
            tipeOpt = Some(fieldType),
            initOpt = None(),
            attr = ResolvedAttr(posOpt = field.feature.identifier.pos, resOpt = fieldResInfoOpt,
              typedOpt = Some(fieldType.attr.typedOpt.get))
          )
        )

        declareName(fieldId, qualifiedFieldName, infoVar, field.feature.identifier.pos, reporter)
      }

      val methods: HashSMap[String, Info.Method] = {

        val TYPE_VAR__STATE_VAR = "TYPE_VAR__STATE_VAR"

        def createUIFInfoMethod(methodName: String, typeParams: ISZ[AST.TypeParam], params: ISZ[AST.Param], returnType: AST.Type.Named, isInObject: B): Info.Method = {

          val methodAst: AST.Stmt.Method = {
            val methodSig = AST.MethodSig(
              purity = AST.Purity.Pure,
              annotations = ISZ(),
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
          _methods = _methods + sig._1 ~> createUIFInfoMethod(
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
      val adtScope = scope(packageName, globalImports(packageName), packageName)

      var paramVars = HashSMap.empty[String, Info.Var]
      var constructorParamVars = ISZ[String]()
      val extractParamVars = ISZ[String]() // always empty as there are no @hidden params in AADL

      val gclAnnexes = a.component.annexes.filter((a: Annex) => a.clause.isInstanceOf[GclSubclause]).map((a: Annex) => a.clause.asInstanceOf[GclSubclause])

      if (gclAnnexes.size > 1) {
        reporter.error(a.component.identifier.pos, toolName, "Only a single GCL subclause is allowed per component type/implementation")
      } else if (gclAnnexes.nonEmpty) {
        if (gclAnnexes(0).state.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have state vars")
        }
        if (gclAnnexes(0).methods.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have GUMBO methods")
        }
        if (gclAnnexes(0).initializes.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have initialize clauses")
        }
        if (gclAnnexes(0).integration.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have integration clauses")
        }
        if (gclAnnexes(0).compute.nonEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Not expecting a data component to have compute clauses")
        }
        if (gclAnnexes(0).invariants.isEmpty) {
          reporter.error(a.component.identifier.pos, toolName, s"Expected a datatype invariant")
        }
      }

      val features: ISZ[AadlPort] = {
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
      for (param <- features) {
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

        val typedOpt: Option[AST.Typed] = resolveTypeH(paramType, None()).attr.typedOpt

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
            resolveTypeH(aadlType, None()).attr.typedOpt.get
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
          isUnclonable = F,
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

      slangTypeToAadlType = slangTypeToAadlType + typeInfoAdt.tpe ~> adtQualifiedName
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

    } // end buildAdtTypeInfo

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

  def offer(component: AadlComponent, annex: Annex, annexLibs: ISZ[AnnexLibInfo], symbolTable: SymbolTable, aadlTypes: AadlTypes, store: Store, reporter: Reporter): (Option[AnnexClauseInfo], Store) = {
    if (processedAnnexes.contains(annex)) {
      return (processedAnnexes.get(annex).get, store)
    } else {
      val result: Option[AnnexClauseInfo] = annex.clause match {
        case gclSubclause: GclSubclause =>
          val gclLibs: ISZ[GclAnnexLibInfo] = annexLibs.filter((a: AnnexLibInfo) => a.isInstanceOf[GclAnnexLibInfo]).map((a: AnnexLibInfo) => a.asInstanceOf[GclAnnexLibInfo])
          buildTypeMap(gclLibs.map((g: GclAnnexLibInfo) => g.annex), aadlTypes, symbolTable, reporter)

          val qualifiedName: IdPath = component.classifier
          if (reporter.hasError) {
            return (None(), store)
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
                      return (None(), store)
                  }
                case _ =>
                  reporter.error(None(), toolName, s"Could not resolve type info for GCl Subclause: ${qualifiedName}")
                  return (None(), store)
              }
            case ac: AadlThread =>
              globalNameMap.get(qualifiedName) match {
                case Some(o: Info.Object) =>
                  val packageName = ops.ISZOps(o.name).dropRight(1)

                  /*
                  val global = Scope.Global(
                    packageName = packageName,
                    imports = globalImports(symbolTable),
                    enclosingName = o.name
                  )
                  */

                  //Scope.Local.create(HashMap.empty, global)
                  Scope.Local.create(HashMap.empty, o.scope)

                case _ =>
                  reporter.error(None(), toolName, s"Could not resolve name for GCL Subclause: ${qualifiedName}")
                  return (None(), store)
              }
            case c =>
              reporter.error(c.component.identifier.pos, toolName, s"GUMBO subclause contracts can only be attached to threads and data components")
              return (None(), store)
          }

          val typeHierarchy: TypeHierarchy = TypeHierarchy(
            nameMap = globalNameMap,
            typeMap = globalTypeMap,
            poset = Poset.empty,
            aliases = HashSMap.empty)

          processGclSubclause(component, gclSubclause, gclLibs, symbolTable, aadlTypes, typeHierarchy, scope, reporter) match {
            case Some((resolvedGclSubclause, gclSymbolTable)) =>
              Some(GclAnnexClauseInfo(resolvedGclSubclause, gclSymbolTable))
            case _ => None()
          }
        case _ => None()
      }

      val s1 = GclResolver.putIndexingTypeFingerprints(indexingTypeFingerprints, store)
      val s2 = GclResolver.putSlangTypeToAadlType(slangTypeToAadlType, s1)
      return (result, s2)
    }
  }

  def offerLibraries(annexLibs: ISZ[AnnexLib], symbolTable: SymbolTable, aadlTypes: AadlTypes, store:Store, reporter: Reporter): (ISZ[AnnexLibInfo], Store) = {
    var ret: ISZ[AnnexLibInfo] = ISZ()
    val gclLibs: ISZ[GclLib] = annexLibs.filter((al: AnnexLib) => al.isInstanceOf[GclLib]).map((al: AnnexLib) => al.asInstanceOf[GclLib])

    if (gclLibs.nonEmpty) {
      buildTypeMap(gclLibs, aadlTypes, symbolTable, reporter)

      for (gclLib <- gclLibs) {
        val qualifiedName = gclLib.containingPackage.name :+ GUMBO__Library
        if (processedLibs.contains(gclLib)) {
          halt(s"Why is gcl library annex being processed again: $qualifiedName")
        } else {
          globalNameMap.get(qualifiedName) match {
            case Some(o: Info.Object) =>

              val localScope = Scope.Local.create(HashMap.empty, o.scope)

              val typeHierarchy: TypeHierarchy = TypeHierarchy(
                nameMap = globalNameMap,
                typeMap = globalTypeMap,
                poset = Poset.empty,
                aliases = HashSMap.empty)

              processGclLib(qualifiedName, gclLib, symbolTable, aadlTypes, typeHierarchy, localScope, reporter) match {
                case Some((resolvedGclLib, gclSymbolTable)) =>
                  val gali = GclAnnexLibInfo(
                    annex = resolvedGclLib,
                    name = qualifiedName,
                    gclSymbolTable = gclSymbolTable)

                  processedLibs = processedLibs + (gclLib ~> gali)

                  ret = ret :+ gali
                case _ =>
                  // TODO: this probably can be deleted
                  processedLibs = processedLibs + (gclLib ~> GclAnnexLibInfo(
                    annex = gclLib,
                    name = qualifiedName,
                    gclSymbolTable = GclSymbolTable(
                      slangTypeHierarchy = typeHierarchy,
                      apiReferences = ISZ(),
                      integrationMap = Map.empty,
                      computeHandlerPortMap = Map.empty
                    )
                  ))
              }

            case _ =>
              reporter.error(None(), toolName, st"Could not resolve GCL Library: ${(qualifiedName, "::")}".render)
          }
        }
      }
    }

    val s1 = GclResolver.putIndexingTypeFingerprints(indexingTypeFingerprints, store)
    val s2 = GclResolver.putSlangTypeToAadlType(slangTypeToAadlType, s1)
    return (ret, s2)
  }

}
