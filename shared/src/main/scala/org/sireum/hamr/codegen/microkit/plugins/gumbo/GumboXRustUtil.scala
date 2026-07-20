// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{Store, TypeIdPath}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.rust.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.rust.Param
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, GclAssume, GclStateVar, GclSubclause}
import org.sireum.lang.ast.Exp
import org.sireum.lang.{ast => SAST}

object GumboXRustUtil {

  def getAadlType(typ: SAST.Typed.Name, aadlTypes: AadlTypes, slangTypesToAadlTypes: Map[SAST.Typed, TypeIdPath]): (AadlType, B) = {
    typ match {
      case SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(i: SAST.Typed.Name)) =>
        val idPath = slangTypesToAadlTypes.get(i).get
        return (aadlTypes.getTypeByPath(idPath), T)
        //i.ids
      case _ =>
        val idPath = slangTypesToAadlTypes.get(typ).get
        return (aadlTypes.getTypeByPath(idPath), F)
        //typ.ids
    }

    //return (aadlTypes.getTypeByPath(ids), isOptional)
  }

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: Exp)

  @record class EE(context: AadlThread,
                   aadlTypes: AadlTypes,
                   stateVars: ISZ[GclStateVar],
                   slangTypesToAadlTypes: Map[SAST.Typed, TypeIdPath],
                   crustTypeProvider: CRustTypeProvider) extends ir.MTransformer {

    var params: Set[GGParam] = Set.empty

    override def pre_langastExpInvoke(o: Exp.Invoke): ir.MTransformer.PreResult[Exp] = {
      o match {
        case Exp.Invoke(Some(Exp.Ident(SAST.Id("api"))), ident, _, targs, args) =>
          val typed = o.ident.attr.typedOpt.get.asInstanceOf[SAST.Typed.Name]
          val (typ, _) = getAadlType(typed, aadlTypes, slangTypesToAadlTypes)
          val ports = context.getPorts().filter(p => p.identifier == ident.id.value)
          assert(ports.size == 1)
          //val param = GGPortParam(
          //  port = ports(0),
          //  aadlType = typ.classifier,
          //  typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
          val param = portToParam(ports(0), crustTypeProvider)
          //println(param)
          params = params + param
          return ir.MTransformer.PreResult(F,
            MSome(o(receiverOpt = None(), ident = o.ident(id = o.ident.id(value = param.name)))))

        case _ => return ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
      }
    }

    override def pre_langastExpSelect(o: Exp.Select): ir.MTransformer.PreResult[Exp] = {
      o match {
        case Exp.Select(Some(Exp.Ident(SAST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[SAST.Typed.Name]
          val ports: ISZ[AadlPort] = context.getPorts().filter(p => p.identifier == id.value)
          assert(ports.size == 1)
          /*
          val typ: AadlType = {
            if (ports(0).isInstanceOf[AadlEventPort]) {
              MicrokitTypeUtil.eventPortType
            } else {
              getAadlType(typed, aadlTypes, slangTypesToAadlTypes)._1
            }
          }

          val param = GGPortParam(
            port = ports(0),
            aadlType = typ.classifier,
            typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
           */
          val param = portToParam(ports(0), crustTypeProvider)
          params = params + param
          return ir.MTransformer.PreResult(F,
            MSome(Exp.Ident(id = SAST.Id(value = param.name, attr = SAST.Attr(None())), attr = o.attr)))
        case _ =>
          return ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
      }
    }

    def isStateVar(name: String, vars: ISZ[GclStateVar]): B = {
      for (i <- 0 until vars.size if vars(i).name == name) {
        return T
      }
      return F
    }

    def findStateVar(name: String, vars: ISZ[GclStateVar]): (Z, GclStateVar) = {
      for (i <- 0 until vars.size if vars(i).name == name) {
        return (i, vars(i))
      }
      halt(s"Infeasible: didn't find state var ${name}")
    }

    override def pre_langastExpInput(o: Exp.Input): ir.MTransformer.PreResult[Exp] = {
      val ret: Exp.Ident = o.exp match {
        case i: Exp.Ident =>
          val name = s"In_${i.id.value}"

          val typed: SAST.Typed.Name = i.attr.typedOpt match {
            case Some(atn: SAST.Typed.Name) => atn
            case x => halt(s"Infeasible: ${i.id.value} had the following for its typed opt ${x}")
          }

          val (typ, _) = getAadlType(typed, aadlTypes, slangTypesToAadlTypes)

          if (isStateVar(i.id.value, stateVars)) {
            val (index, stateVar) = findStateVar(i.id.value, stateVars)

            params = params +
              GGStateVarParam(
                stateVar = stateVar,
                id = index,
                isPreState = T,
                aadlType = typ.classifier,
                typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
          }

          SAST.Exp.Ident(id = SAST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    override def pre_langastExpIdent(o: Exp.Ident): ir.MTransformer.PreResult[Exp] = {
      o.attr.typedOpt match {
        case Some(typed: SAST.Typed.Name) =>
          val (typ, _) = getAadlType(typed, aadlTypes, slangTypesToAadlTypes)

          o.resOpt match {
            case Some(e: SAST.ResolvedInfo.LocalVar) =>
              // must be a quantifier variable so nothing to do
            case Some(e: SAST.ResolvedInfo.Var) =>

              if (isStateVar(o.id.value, stateVars)) {
                val (index, stateVar) = findStateVar(o.id.value, stateVars)

                params = params +
                  GGStateVarParam(
                    stateVar = stateVar,
                    id = index,
                    isPreState = F,
                    aadlType = typ.classifier,
                    typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
              }
            case Some(SAST.ResolvedInfo.BuiltIn(SAST.ResolvedInfo.BuiltIn.Kind.Apply)) =>
              // eg. In(sv)#(0)
            case x =>
              halt(s"Ident $o resolved to $x")
          }
        case _ =>
      }
      return ir.MTransformer.PreResult(F, MNone[Exp]())
    }
  }

  @enum object SymbolKind {
    "Integration"

    "StateVarPre"
    "StateVar"

    "ApiVarInEvent"
    "ApiVarInEventData"
    "ApiVarInData"

    "ApiVarOutEvent"
    "ApiVarOutEventData"
    "ApiVarOutData"

    "Parameter"
  }

  @sig trait GGParam {
    @pure def name: String

    @pure def originName: String

    @pure def langType: ST

    //def preFetch: ST

    //def postFetch: ST

    @pure def isOptional: B

    @pure def aadlType: TypeIdPath

    @pure def typeNameProvider: CRustTypeNameProvider

    @pure def kind: SymbolKind.Type

    //def setter: ST

    //def getter: ST

    @pure def getParamDef: ST = {
      return st"$name: $langType"
    }

    @pure def isStateVar: B = {
      return kind == SymbolKind.StateVarPre || kind == SymbolKind.StateVar
    }

    @pure def isInPort: B = {
      return isInEventPort || kind == SymbolKind.ApiVarInData
    }

    @pure def isInEventPort: B = {
      return kind == SymbolKind.ApiVarInEventData || kind == SymbolKind.ApiVarInEvent
    }

    @pure def isOutPort: B = {
      return isOutEventPort || kind == GumboXRustUtil.SymbolKind.ApiVarOutData
    }

    @pure def isOutEventPort: B = {
      return kind == SymbolKind.ApiVarOutEvent || kind == SymbolKind.ApiVarOutEventData
    }

    @strictpure def toRustParam: Param =
      if (isOptional)
        RAST.ParamImpl(ident = RAST.IdentString(name), kind =
          RAST.TyPath(ISZ(ISZ("Option"), typeNameProvider.qualifiedRustNameS), Some(aadlType)))
      else
        RAST.ParamImpl(ident = RAST.IdentString(name), kind =
          RAST.TyPath(ISZ(typeNameProvider.qualifiedRustNameS), Some(aadlType)))

  }

  @pure def paramsToComment(params: ISZ[GGParam]): ISZ[ST] = {
    var comments: ISZ[ST] = ISZ()
    for (p <- params) {
      val kind: String = p.kind match {
        case SymbolKind.Integration => "integration variable"

        case SymbolKind.StateVarPre => "pre-state state variable"
        case SymbolKind.StateVar => "post-state state variable"

        case SymbolKind.ApiVarInData => "incoming data port"
        case SymbolKind.ApiVarInEvent => "incoming event port"
        case SymbolKind.ApiVarInEventData => "incoming event data port"

        case SymbolKind.ApiVarOutData => "outgoing data port"
        case SymbolKind.ApiVarOutEvent => "outgoing event port"
        case SymbolKind.ApiVarOutEventData => "outgoing event data port"

        case SymbolKind.Parameter => "parameter to handler method"
      }
      comments = comments :+ st"* @param ${p.name} ${kind}"
    }
    return comments
  }


  @pure def getPortInfo(port: AadlPort): (AadlType, String) = {
    val ret: (AadlType, String) = port match {
      case i: AadlEventDataPort => (i.aadlType, "event data")
      case i: AadlDataPort => (i.aadlType, "data")
      case i: AadlEventPort => halt("Need to handle event ports")//(TypeUtil.EmptyType, "event")
      case _ => halt("Infeasible")
    }
    return ret
  }


  @pure def sortParams(params: ISZ[GGParam]): ISZ[GGParam] = {
    return (for (partition <-
                   (for (kind <- SymbolKind.elements) yield ops.ISZOps(params).filter(p => p.kind == kind))) yield
      ops.ISZOps(partition).sortWith((a,b) => a.name <= b.name)).flatMap(a => a)
  }

  @pure def rewriteToExpX(exp: Exp, thread: AadlThread, types: AadlTypes, stateVars: ISZ[GclStateVar],
                          slangTypesToAadlTypes: Map[SAST.Typed, TypeIdPath],
                          crustTypeProvider: CRustTypeProvider): GGExpParamHolder = {
    val e = EE(thread, types, stateVars, slangTypesToAadlTypes, crustTypeProvider)
    e.transform_langastExp(exp) match {
      case MSome(x) => return GGExpParamHolder(e.params, x)
      case _ => return GGExpParamHolder(e.params, exp)
    }
  }

  // Inclusive integer interval derived from GUMBO assume clauses; None means unbounded on that side
  @datatype class AssumeInterval(val lo: Option[Z], val hi: Option[Z])

  @strictpure def isRustIntPrimitive(rustName: String): B =
    rustName == "i8" || rustName == "i16" || rustName == "i32" || rustName == "i64" ||
      rustName == "u8" || rustName == "u16" || rustName == "u32" || rustName == "u64" ||
      rustName == "usize"

  /** Splits a conjunction (& / &&) into its conjuncts */
  @pure def collectConjuncts(exp: Exp): ISZ[Exp] = {
    exp match {
      case b: Exp.Binary =>
        if (b.op == Exp.BinaryOp.And || b.op == Exp.BinaryOp.CondAnd) {
          return collectConjuncts(b.left) ++ collectConjuncts(b.right)
        } else {
          return ISZ(exp)
        }
      case _ => return ISZ(exp)
    }
  }

  /** Evaluates an integer literal expression (LitZ, an integer string interpolate such as s32"...",
    * or a unary minus applied to either), returning None for anything else */
  @pure def evalIntLit(exp: Exp): Option[Z] = {
    exp match {
      case lit: Exp.LitZ => return Some(lit.value)
      case si: Exp.StringInterpolate =>
        if (si.lits.size != 1) {
          return None()
        }
        val text = ops.StringOps(si.lits(0).value).replaceAllLiterally("\"", "")
        si.prefix match {
          case "z" => return Z(text)
          case "s8" => return Z(text)
          case "s16" => return Z(text)
          case "s32" => return Z(text)
          case "s64" => return Z(text)
          case "u8" => return Z(text)
          case "u16" => return Z(text)
          case "u32" => return Z(text)
          case "u64" => return Z(text)
          case _ => return None()
        }
      case u: Exp.Unary =>
        if (u.op == SAST.Exp.UnaryOp.Minus) {
          evalIntLit(u.exp) match {
            case Some(v) => return Some(-v)
            case _ => return None()
          }
        }
        return None()
      case _ => return None()
    }
  }

  /** Tightens name's lower bound to lo if it is more restrictive than the current one */
  @pure def refineLo(intervals: HashSMap[String, AssumeInterval], name: String, lo: Z): HashSMap[String, AssumeInterval] = {
    val cur = intervals.get(name).getOrElse(AssumeInterval(lo = None(), hi = None()))
    if (cur.lo.isEmpty || lo > cur.lo.get) {
      return intervals + name ~> AssumeInterval(lo = Some(lo), hi = cur.hi)
    }
    return intervals
  }

  /** Tightens name's upper bound to hi if it is more restrictive than the current one */
  @pure def refineHi(intervals: HashSMap[String, AssumeInterval], name: String, hi: Z): HashSMap[String, AssumeInterval] = {
    val cur = intervals.get(name).getOrElse(AssumeInterval(lo = None(), hi = None()))
    if (cur.hi.isEmpty || hi < cur.hi.get) {
      return intervals + name ~> AssumeInterval(lo = cur.lo, hi = Some(hi))
    }
    return intervals
  }

  /** Name of an interval-constraint target: a plain identifier yields the identifier's name and
    * a single field selection on an identifier yields &lt;identifier&gt;.&lt;field&gt; */
  @pure def constraintTargetName(e: Exp): Option[String] = {
    e match {
      case id: Exp.Ident => return Some(id.id.value)
      case sel: Exp.Select =>
        sel.receiverOpt match {
          case Some(id: Exp.Ident) => return Some(s"${id.id.value}.${sel.id.value}")
          case _ => return None()
        }
      case _ => return None()
    }
  }

  /** Refines intervals with every conjunct of exp that is an interval constraint, i.e., a
    * comparison (<=, <, >=, >, ==) between an identifier (or a field selection on an
    * identifier) and an integer literal */
  @pure def refineIntervalsFromExp(intervals: HashSMap[String, AssumeInterval], exp: Exp): HashSMap[String, AssumeInterval] = {
    var ret = intervals
    for (conjunct <- collectConjuncts(exp)) {
      conjunct match {
        case b: Exp.Binary =>
          (constraintTargetName(b.left), evalIntLit(b.right)) match {
            case (Some(name), Some(v)) =>
              if (b.op == Exp.BinaryOp.Le) { // name <= v
                ret = refineHi(ret, name, v)
              } else if (b.op == Exp.BinaryOp.Lt) { // name < v
                ret = refineHi(ret, name, v - 1)
              } else if (b.op == Exp.BinaryOp.Ge) { // name >= v
                ret = refineLo(ret, name, v)
              } else if (b.op == Exp.BinaryOp.Gt) { // name > v
                ret = refineLo(ret, name, v + 1)
              } else if (b.op == Exp.BinaryOp.Eq) { // name == v
                ret = refineLo(refineHi(ret, name, v), name, v)
              }
            case _ =>
              (evalIntLit(b.left), constraintTargetName(b.right)) match {
                case (Some(v), Some(name)) =>
                  if (b.op == Exp.BinaryOp.Le) { // v <= name
                    ret = refineLo(ret, name, v)
                  } else if (b.op == Exp.BinaryOp.Lt) { // v < name
                    ret = refineLo(ret, name, v + 1)
                  } else if (b.op == Exp.BinaryOp.Ge) { // v >= name
                    ret = refineHi(ret, name, v)
                  } else if (b.op == Exp.BinaryOp.Gt) { // v > name
                    ret = refineHi(ret, name, v - 1)
                  } else if (b.op == Exp.BinaryOp.Eq) { // v == name
                    ret = refineLo(refineHi(ret, name, v), name, v)
                  }
                case _ =>
              }
          }
        case _ =>
      }
    }
    return ret
  }

  /** Merges iv into the interval recorded under name (intersecting with an existing one) */
  @pure def mergeInterval(intervals: HashSMap[String, AssumeInterval], name: String, iv: AssumeInterval): HashSMap[String, AssumeInterval] = {
    var ret = intervals
    iv.lo match {
      case Some(lo) => ret = refineLo(ret, name, lo)
      case _ =>
    }
    iv.hi match {
      case Some(hi) => ret = refineHi(ret, name, hi)
      case _ =>
    }
    return ret
  }

  /** Derives inclusive integer intervals for the GUMBOX test-harness parameters of thread from its
    * GUMBO integration assumes (incoming ports) and top-level compute assumes (ports and state
    * variables). The result is keyed by harness parameter name (api_&lt;port&gt;, In_&lt;statevar&gt;).
    * Only single-variable interval constraints are recognized; anything else is ignored, so the
    * result under-approximates the constraints (callers fall back to full-range generators). */
  @pure def deriveAssumeIntervals(thread: AadlThread,
                                  subclauseInfoOpt: Option[GclAnnexClauseInfo],
                                  types: AadlTypes,
                                  slangTypesToAadlTypes: Map[SAST.Typed, TypeIdPath],
                                  crustTypeProvider: CRustTypeProvider): HashSMap[String, AssumeInterval] = {
    var ret = HashSMap.empty[String, AssumeInterval]
    subclauseInfoOpt match {
      case Some(GclAnnexClauseInfo(subclause, gclSymbolTable)) =>
        // integration assumes on incoming ports: the expression references the raw port name,
        // whereas the corresponding harness parameter is named api_<port>
        for (port <- thread.getPorts() if gclSymbolTable.integrationMap.contains(port)) {
          gclSymbolTable.integrationMap.get(port).get match {
            case a: GclAssume =>
              val portIntervals = refineIntervalsFromExp(HashSMap.empty[String, AssumeInterval], a.exp)
              for (entry <- portIntervals.entries) {
                val key = entry._1
                if (key == port.identifier || ops.StringOps(key).startsWith(s"${port.identifier}.")) {
                  ret = mergeInterval(ret, s"api_$key", entry._2)
                }
              }
            case _ =>
          }
        }
        // top-level compute assumes: rewrite so port/state-var references use the harness
        // parameter names (api_<port>, In_<statevar>) before extracting intervals
        subclause.compute match {
          case Some(compute) =>
            for (g <- compute.assumes) {
              val gg = rewriteToExpX(
                exp = g.exp,
                thread = thread,
                types = types,
                stateVars = subclause.state,
                slangTypesToAadlTypes = slangTypesToAadlTypes,
                crustTypeProvider = crustTypeProvider)
              ret = refineIntervalsFromExp(ret, gg.exp)
            }
          case _ =>
        }
        // an assume may reference a state variable's pre-state without an explicit In(..) wrapper,
        // in which case the reference keeps the state variable's plain name; re-key such entries
        // (including field selections) to the harness's pre-state parameter name In_<statevar>
        for (sv <- subclause.state; entry <- ret.entries) {
          val key = entry._1
          if (key == sv.name || ops.StringOps(key).startsWith(s"${sv.name}.")) {
            ret = mergeInterval(ret, s"In_$key", entry._2)
          }
        }
      case _ =>
    }
    return ret
  }

  @datatype class GGPortParam(val port: AadlPort,
                              val aadlType: TypeIdPath,
                              @hidden val typeNameProvider: CRustTypeNameProvider) extends GGParam {
    val name: String = s"api_${port.identifier}"

    val originName: String = port.identifier

    @strictpure override def langType: ST = st"${typeNameProvider.qualifiedRustName}"

    //override def preFetch: ST = ???

    //override def postFetch: ST = ???

    val isIn: B = port.direction == Direction.In

    val isData: B = port.isInstanceOf[AadlFeatureData]
    val isEvent: B = port.isInstanceOf[AadlFeatureEvent]
    val isPureEvent: B = port.isInstanceOf[AadlEventPort]

    val kind: SymbolKind.Type =
      if (isIn) {
        port match {
          case i: AadlEventPort => SymbolKind.ApiVarInEvent
          case i: AadlDataPort => SymbolKind.ApiVarInData
          case i: AadlEventDataPort => SymbolKind.ApiVarInEventData
          case _ => halt("Infeasible")
        }
      } else {
        port match {
          case i: AadlEventPort => SymbolKind.ApiVarOutEvent
          case i: AadlDataPort => SymbolKind.ApiVarOutData
          case i: AadlEventDataPort => SymbolKind.ApiVarOutEventData
          case _ => halt("Infeasible")
        }
      }

    val isOptional: B = isEvent

    //override def setter: ST = ???

    //override def getter: ST = ???
  }

  @datatype class GGStateVarParam(val stateVar: GclStateVar,
                                  val id: Z,
                                  val isPreState: B,
                                  val aadlType: TypeIdPath,
                                  @hidden val typeNameProvider: CRustTypeNameProvider) extends GGParam {
    val name: String = s"${if (isPreState) "In_" else ""}${stateVar.name}"

    val originName: String = stateVar.name

    val langType: ST = st"${typeNameProvider.qualifiedRustName}"

    //override def preFetch: ST = ???

    //override def postFetch: ST = ???

    val isOptional: B = F

    val kind: SymbolKind.Type = if (isPreState) SymbolKind.StateVarPre else SymbolKind.StateVar

    //override def setter: ST = ???

    //override def getter: ST = ???
  }

  @pure def isOutPort(feature: AadlFeature): B = {
    return feature.isInstanceOf[AadlPort] && feature.asInstanceOf[AadlPort].direction == Direction.Out
  }

  @pure def isInPort(feature: AadlFeature): B = {
    return feature.isInstanceOf[AadlPort] && feature.asInstanceOf[AadlPort].direction == Direction.In
  }

  @strictpure def isPluginProvided(feature: AadlFeature, store: Store): B =
    StoreUtil.isSynthetic(feature.path, store)

  @pure def inPortsToParams(thread: AadlThread, typeProvider: CRustTypeProvider, store: Store): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- thread.features.filter(f => isInPort(f) && !isPluginProvided(f, store))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, typeProvider)
  }

  def outPortsToParams(thread: AadlThread, typeProvider: CRustTypeProvider, store: Store): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- thread.features.filter(f => isOutPort(f) && ~isPluginProvided(f, store))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, typeProvider)
  }

  @pure def portToParam(p: AadlPort, typeProvider: CRustTypeProvider): GGParam = {
    p match {
      case i: AadlEventPort =>
        return GGPortParam(p, ISZ(MicrokitTypeUtil.eventPortTypeName), typeProvider.getTypeNameProvider(MicrokitTypeUtil.eventPortType))
      case i: AadlEventDataPort =>
        return GGPortParam(p, i.aadlType.classifier, typeProvider.getTypeNameProvider(i.aadlType))
      case i: AadlDataPort =>
        return GGPortParam(p, i.aadlType.classifier, typeProvider.getTypeNameProvider(i.aadlType))
      case _ => halt("Infeasible")
    }
  }

  @pure def portsToParams(ports: ISZ[AadlPort], typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    return for (p <- ports) yield portToParam(p, typeProvider)
  }

  def stateVarsToParams(gclSubclauseInfo: Option[GclAnnexClauseInfo],
                        isPreState: B, types: AadlTypes, typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    gclSubclauseInfo match {
      case Some(GclAnnexClauseInfo(GclSubclause(stateVars, _, _, _, _, _, _), _)) =>
        for (i <- 0 until stateVars.size) {
          val stateVar = stateVars(i)
          val typ = types.typeMap.get(stateVar.classifier).get
          ret = ret :+ GGStateVarParam(
            stateVar = stateVar,
            id = i,
            aadlType = typ.classifier,
            isPreState = isPreState,
            typeNameProvider = typeProvider.getTypeNameProvider(typ))
        }
      case _ =>
    }
    return ret
  }

  @strictpure def getInitializeGuaranteeMethodName(id: String): String = s"initialize_${id}"

  val getInitialize_IEP_Guar_MethodName: String = "initialize_IEP_Guar"

  @strictpure def getInitialize_IEP_Post_MethodName: String = "initialize_IEP_Post"

  @pure def createIntegrationMethodName(aadlPort: AadlPort): (String, String) = {
    val kind: String = if (aadlPort.direction == Direction.In) "I_Assm" else "I_Guar"
    val name = s"${kind}_${aadlPort.identifier}"
    val guardName = s"${kind}_Guard_${aadlPort.identifier}"
    return (name, guardName)
  }

  val getCompute_CEP_T_Assm_MethodName: String = "compute_CEP_T_Assm"

  val getCompute_CEP_T_Guar_MethodName: String = "compute_CEP_T_Guar"

  val getCompute_CEP_T_Case_MethodName: String = "compute_CEP_T_Case"

  val getCompute_CEP_Pre_MethodName: String = "compute_CEP_Pre"

  val getCompute_CEP_Post_MethodName: String = "compute_CEP_Post"
}
