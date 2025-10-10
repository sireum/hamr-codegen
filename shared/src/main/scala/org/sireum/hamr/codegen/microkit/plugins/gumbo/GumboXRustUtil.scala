// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.rust.Param
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, GclStateVar, GclSubclause}
import org.sireum.lang.ast.Exp
import org.sireum.lang.{ast => SAST}

object GumboXRustUtil {

  def getAadlType(typ: SAST.Typed.Name, aadlTypes: AadlTypes): (AadlType, B) = {
    var isOptional: B = F
    val ids: ISZ[String] = typ match {
      case SAST.Typed.Name(SAST.Typed.optionName, ISZ(i: SAST.Typed.Name)) =>
        isOptional = T
        i.ids
      case _ => typ.ids
    }

    return (aadlTypes.getTypeByPath(ids), isOptional)
  }

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: Exp)

  @record class EE(context: AadlThread,
                   aadlTypes: AadlTypes,
                   stateVars: ISZ[GclStateVar],
                   crustTypeProvider: CRustTypeProvider) extends ir.MTransformer {

    var params: Set[GGParam] = Set.empty

    override def pre_langastExpSelect(o: Exp.Select): ir.MTransformer.PreResult[Exp] = {
      o match {
        case Exp.Select(Some(Exp.Ident(SAST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[SAST.Typed.Name]
          val (typ, _) = getAadlType(typed, aadlTypes)
          val ports = context.getPorts().filter(p => p.identifier == id.value)
          assert(ports.size == 1)
          val param = GGPortParam(
            port = ports(0),
            aadlType = typ.classifier,
            typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
          params = params + param
          return ir.MTransformer.PreResult(F,
            MSome(Exp.Ident(id = SAST.Id(value = param.name, attr = SAST.Attr(None())), attr = o.attr)))
        case _ =>
          return ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
      }
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

          val (typ, _) = getAadlType(typed, aadlTypes)

          val (index, stateVar) = findStateVar(i.id.value, stateVars)

          params = params +
            GGStateVarParam(
              stateVar = stateVar,
              id = index,
              isPreState = T,
              aadlType = typ.classifier,
              typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
          SAST.Exp.Ident(id = SAST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    override def pre_langastExpIdent(o: Exp.Ident): ir.MTransformer.PreResult[Exp] = {
      o.attr.typedOpt match {
        case Some(typed: SAST.Typed.Name) =>
          val (typ, _) = getAadlType(typed, aadlTypes)

          o.resOpt match {
            case Some(e: SAST.ResolvedInfo.LocalVar) =>
              // must be a quantifier variable so nothing to do
            case Some(e: SAST.ResolvedInfo.Var) =>
              val (index, stateVar) = findStateVar(o.id.value, stateVars)

              params = params +
                GGStateVarParam(
                  stateVar = stateVar,
                  id = index,
                  isPreState = F,
                  aadlType = typ.classifier,
                  typeNameProvider = crustTypeProvider.getTypeNameProvider(typ))
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

  @pure def rewriteToExpX(exp: Exp, thread: AadlThread, types: AadlTypes, stateVars: ISZ[GclStateVar], crustTypeProvider: CRustTypeProvider): GGExpParamHolder = {
    val e = EE(thread, types, stateVars, crustTypeProvider)
    e.transform_langastExp(exp) match {
      case MSome(x) => return GGExpParamHolder(e.params, x)
      case _ => return GGExpParamHolder(e.params, exp)
    }
  }

  @datatype class GGPortParam(val port: AadlPort,
                              val aadlType: TypeIdPath,
                              val typeNameProvider: CRustTypeNameProvider) extends GGParam {
    val name: String = s"api_${port.identifier}"

    val originName: String = port.identifier

    @strictpure override def langType: ST = st"${typeNameProvider.qualifiedRustName}"

    //override def preFetch: ST = ???

    //override def postFetch: ST = ???

    val isIn: B = port.direction == Direction.In

    val isData: B = port.isInstanceOf[AadlFeatureData]
    val isEvent: B = port.isInstanceOf[AadlFeatureEvent]

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
                                  val typeNameProvider: CRustTypeNameProvider) extends GGParam {
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

  @pure def inPortsToParams(thread: AadlThread, typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- thread.features.filter(f => isInPort(f))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, typeProvider)
  }

  def outPortsToParams(thread: AadlThread, typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- thread.features.filter(f => isOutPort(f))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, typeProvider)
  }

  @pure def portsToParams(ports: ISZ[AadlPort], typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    for (p <- ports) {
      p match {
        case i: AadlEventPort =>
          halt("Need to handle event ports")
        case i: AadlEventDataPort =>
          ret = ret :+ GGPortParam(p, i.aadlType.classifier, typeProvider.getTypeNameProvider(i.aadlType))
        case i: AadlDataPort =>
          ret = ret :+ GGPortParam(p, i.aadlType.classifier, typeProvider.getTypeNameProvider(i.aadlType))
        case _ => halt("Infeasible")
      }
    }
    return ret
  }

  def stateVarsToParams(gclSubclauseInfo: Option[GclAnnexClauseInfo],
                        isPreState: B, types: AadlTypes, typeProvider: CRustTypeProvider): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    gclSubclauseInfo match {
      case Some(GclAnnexClauseInfo(GclSubclause(stateVars, _, _, _, _, _), _)) =>
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
