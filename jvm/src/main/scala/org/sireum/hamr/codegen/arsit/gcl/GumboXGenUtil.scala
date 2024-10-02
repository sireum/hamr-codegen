// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Direction, GclStateVar, GclSubclause}
import org.sireum.lang.ast.Typed
import org.sireum.lang.symbol.Resolver
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter

object GumboXGenUtil {
  def genGumboXUtil(basePackage: String): ST = {
    val utilContent: ST =
      st"""// #Sireum
          |package $basePackage
          |
          |import org.sireum._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object GumboXUtil {
          |
          |  @enum object GumboXResult {
          |    "Pre_Condition_Unsat"
          |    "Post_Condition_Pass"
          |    "Post_Condition_Fail"
          |  }
          |}"""
    return utilContent
  }


  @pure def getRangenMethodName(a: AadlType): String = {
    a match {
      case i: BaseType => return i.slangType.name
      case i: EnumType =>
        val qname: ISZ[String] = i.nameProvider.basePackageName +: (i.nameProvider.qualifiedTypeNameI :+ "Type")
        return Resolver.typeName(ISZ(i.nameProvider.basePackageName), qname).render
      case i =>
        val qname: ISZ[String] =
          if (i == TypeUtil.EmptyType) i.nameProvider.qualifiedReferencedTypeNameI
          else i.nameProvider.basePackageName +: i.nameProvider.qualifiedReferencedTypeNameI
        return Resolver.typeName(ISZ(i.nameProvider.basePackageName), qname).render
    }
  }

  val portsSuffix: String = "P"
  val portStateVarSuffix: String = s"${portsSuffix}S"

  @strictpure def genContainerSigName(singletonType: String, isPre: B): String =
    s"${singletonType}_${if (isPre) "Pre" else "Post"}State_Container"

  @strictpure def genContainerName(singletonType: String, isPre: B, withL: B): String =
    s"${genContainerSigName(singletonType, isPre)}_${if (withL) portStateVarSuffix else portsSuffix}"

  @strictpure def genInitProfileTraitName(singletonType: String): String =
    s"${genInitProfileName(singletonType)}_Trait"

  @strictpure def genInitProfileName(singletonType: String): String =
    s"${singletonType}_Profile"

  @strictpure def genProfileTraitName(str: String, b: B): String =
    s"${genProfileName(str, b)}_Trait"

  @strictpure def genProfileName(singletonType: String, includeStateVars: B): String =
    s"${singletonType}_Profile_${if (includeStateVars) portStateVarSuffix else portsSuffix}"


  @pure def getContainerSig(packageName: String): ST = {
    return (
      st"""// #Sireum
          |
          |package $packageName.util
          |
          |import org.sireum._
          |
          |@sig trait Container extends art.DataContent
          |
          |@datatype class EmptyContainer extends Container
          |""")
  }



  @pure def genUnitTestConfiguration(packageName: String): ST = {
    return (
      st"""//#Sireum
          |
          |package $packageName.util
          |
          |import org.sireum._
          |import $packageName.util.Container
          |import $packageName.GumboXUtil.GumboXResult
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@msig trait UnitTestConfiguration {
          |  def name: String
          |  def description: String
          |  def profile: Profile
          |  def test(c: Container): GumboXResult.Type
          |  def genReplay: (Container, String, GumboXResult.Type) => Option[String]
          |  def verbose: B
          |}
          |
          |@msig trait UnitTestConfigurationBatch extends UnitTestConfiguration {
          |  def numTests: Z
          |  def numTestVectorGenRetries: Z
          |  def failOnUnsatPreconditions: B
          |}
          |
          |@msig trait Profile {
          |  def next: Container
          |}
          |""")
    }


  @pure def genMutableBase(packageName: String): ST = {
    return (st"""package $packageName.util
                |
                |import org.sireum._
                |import org.sireum.$$internal.MutableMarker
                |
                |trait MutableBase extends MutableMarker {
                |  def string: String = super.toString
                |
                |  override def $$clonable: Boolean = false
                |
                |  override def $$clonable_=(b: Boolean): MutableMarker = this
                |
                |  override def $$owned: Boolean = false
                |
                |  override def $$owned_=(b: Boolean): MutableMarker = this
                |
                |  override def $$clone: MutableMarker = this
                |}
                |""")
  }

  @datatype class Container(val componentSingletonType: String,
                            val packageName: String,
                            val packageNameI: ISZ[String],
                            val basePackage: String,

                            val inPorts: ISZ[GGParam],
                            val inStateVars: ISZ[GGParam],
                            val outPorts: ISZ[GGParam],
                            val outStateVars: ISZ[GGParam]) {

    @pure def jsonFrom(name: String): ST = {
      return st"JSON.from${Resolver.typeName(ISZ(basePackage), packageNameI :+ name)}"
    }

    @pure def jsonTo(name: String): ST = {
      return st"JSON.to${Resolver.typeName(ISZ(basePackage), packageNameI :+ name)}"
    }

    val preStateContainerSigName: String = genContainerSigName(componentSingletonType, T)
    val preStateContainerName_P: String = genContainerName(componentSingletonType, T, F)
    val preStateContainerName_PS: String = genContainerName(componentSingletonType, T, T)

    val postStateContainerSigName: String = genContainerSigName(componentSingletonType, F)
    val postStateContainerName_P: String = genContainerName(componentSingletonType, F, F)
    val postStateContainerName_PS: String = genContainerName(componentSingletonType, F, T)

    val fqPreStateContainerName_P: String = s"$packageName.$preStateContainerName_P"
    val fqPreStateContainerName_PS: String = s"$packageName.$preStateContainerName_PS"
    val fqPostStateContainerName_P: String = s"$packageName.$postStateContainerName_P"
    val fqPostStateContainerName_PS: String = s"$packageName.$postStateContainerName_PS"


    val preStateContainerJsonFrom_P: ST = jsonFrom(preStateContainerName_P)
    val preStateContainerJsonFrom_PS: ST = jsonFrom(preStateContainerName_PS)
    val postStateContainerJsonFrom_P: ST = jsonFrom(postStateContainerName_P)
    val postStateContainerJsonFrom_PS: ST = jsonFrom(postStateContainerName_PS)

    val preStateContainerJsonTo_P: ST = jsonTo(preStateContainerName_P)
    val preStateContainerJsonTo_PS: ST = jsonTo(preStateContainerName_PS)
    val postStateContainerJsonTo_P: ST = jsonTo(postStateContainerName_P)
    val postStateContainerJsonTo_PS: ST = jsonTo(postStateContainerName_PS)

    val profileName_P: String = genProfileName(componentSingletonType, F)
    val profileName_PS: String = genProfileName(componentSingletonType, T)

    def observePreStateH(containerName: String, params: ISZ[GGParam]): ST = {

      var entries: ISZ[ST] = ISZ()
      for (param <- sortParam(params)) {
        entries = entries :+ st"${param.name} = ${param.preFetch}"
      }
      return if (entries.isEmpty) st"$containerName()"
      else
        st"""${containerName}(
            |  ${(entries, ", \n")})"""
    }

    def observePreState(): ST = {
      return observePreStateH(preStateContainerName_P, inPorts)
    }

    def observePreState_wL(): ST = {
      return observePreStateH(preStateContainerName_PS, inPorts ++ inStateVars)
    }

    def observePostStateH(containerName: String, params: ISZ[GGParam]): ST = {
      var entries: ISZ[ST] = ISZ()
      for (outPort <- sortParam(params)) {
        entries = entries :+ st"${outPort.name} = ${outPort.postFetch}"
      }
      return if (entries.isEmpty) st"$containerName()"
      else
        st"""${containerName}(
            |  ${(entries, ",\n")})"""
    }

    def observePostState(): ST = {
      return observePostStateH(postStateContainerName_P, outPorts)
    }

    def observePostState_wL(): ST = {
      return observePostStateH(postStateContainerName_PS, outPorts ++ outStateVars)
    }

    def lastDataPortVars: Option[ST] = {
      var entries: ISZ[ST] = ISZ()
      for (outPort <- outPorts if outPort.kind == SymbolKind.ApiVarOutData) {
        entries = entries :+ st"var last_${outPort.name}: Option[${outPort.aadlType.nameProvider.qualifiedReferencedTypeName}] = None()"
        entries = entries :+ st""
        entries = entries :+ outPort.asInstanceOf[GGPortParam].dataPortFetch("last")
      }
      return (if (entries.nonEmpty) Some(st"${(entries, "\n")}")
      else None())
    }

    @pure def genSig(sigName: String,
                     params: ISZ[GGParam]): ST = {

      @strictpure def wrapOption(s: String, opt: B): String = if (opt) s"Option[$s]" else s

      val fieldDecls: ISZ[ST] = for (p <- sortParam(params)) yield
        st"def ${p.name}: ${wrapOption(getSlangTypeName(p.aadlType), p.isOptional)}"

      return (st"""@sig trait $sigName extends Container {
                  |  ${(fieldDecls, "\n")}
                  |}""")
    }

    @pure def genContainer(containerName: String,
                           params: ISZ[GGParam],
                           sigName: String,
                           isPre: B,
                           includesStateVars: B): ST = {

      @strictpure def wrapOption(s: String, opt: B): String = if (opt) s"Option[$s]" else s

      val fieldDecls: ISZ[ST] = for (p <- sortParam(params)) yield
        st"val ${p.name}: ${wrapOption(getSlangTypeName(p.aadlType), p.isOptional)}"

      return (st"""// container for ${if (isPre) "incoming" else "outgoing"} ports${if (includesStateVars) " and state variables" else ""}
                  |@datatype class $containerName (
                  |  ${(fieldDecls, ",\n")}) extends $sigName""")
    }

    def genContainers(): ST = {
      val containers: ISZ[ST] = ISZ(
        genSig(preStateContainerSigName, inPorts),
        genContainer(preStateContainerName_P, inPorts, preStateContainerSigName, T, F),
        genContainer(preStateContainerName_PS, inPorts ++ inStateVars, preStateContainerSigName, T, T),

        genSig(postStateContainerSigName, outPorts),
        genContainer(postStateContainerName_P, outPorts, postStateContainerSigName, F, F),
        genContainer(postStateContainerName_PS, outPorts ++ outStateVars, postStateContainerSigName, F, T))

      return DSCTemplate.genTestVectorContainerClass(
        packageName = packageName,
        imports = ISZ(s"$basePackage._", s"$basePackage.util.Container"),
        containers = containers)
    }

    def genProfile(params: ISZ[GGParam],
                   includeStateVars: B,
                   containerName: String): ST = {
      val baseProfileTraitName = genProfileTraitName(componentSingletonType, F)
      val profileTraitName = genProfileTraitName(componentSingletonType, includeStateVars)
      val profileName = genProfileName(componentSingletonType, includeStateVars)

      var traitFields: ISZ[ST] = ISZ()
      var fieldDecls: ISZ[ST] = ISZ()
      var nextEntries: ISZ[ST] = ISZ()
      val sps = sortParam(params)
      @pure def wrapOption(p: GGParam): String = {
        return if (p.isOptional) s"Option${p.ranGenName}" else p.ranGenName
      }
      for (i <- 0 until sps.size if !sps(i).isInstanceOf[GGStateVarParam] || includeStateVars) {
        val p = sps(i)
        traitFields = traitFields :+ st"def ${p.name}: RandomLib // random lib for generating ${p.aadlType.nameProvider.qualifiedTypeName}"
        fieldDecls = fieldDecls :+ st"var ${p.name}: RandomLib${if (i < sps.size - 1) "," else ""} // random lib for generating ${p.aadlType.nameProvider.qualifiedTypeName}"
        nextEntries = nextEntries :+ st"${p.name} = ${p.name}.next${wrapOption(p)}()"
      }

      val extend: String = if (includeStateVars) baseProfileTraitName else "Profile"
      return (
        st"""@msig trait $profileTraitName extends $extend {
            |  ${(traitFields, "\n")}
            |}
            |
            |@record class $profileName(
            |  val name: String,
            |  ${(fieldDecls, "\n")}
            |  ) extends $profileTraitName {
            |
            |  override def next: ${containerName} = {
            |    return (${containerName} (
            |      ${(nextEntries, ",\n")}))
            |  }
            |}""")
    }

    def genInitProfile(): ST = {
      return (
        st"""@msig trait ${genInitProfileTraitName(componentSingletonType)} extends Profile
            |
            |@record class ${genInitProfileName(componentSingletonType)} (
            |  val name: String,
            |) extends ${genInitProfileTraitName(componentSingletonType)} {
            |
            |  override def next: EmptyContainer = {
            |    return EmptyContainer()
            |  }
            |}""")
    }

    def genProfiles(): ST = {
      val params = inStateVars ++ inPorts
      return (
        st"""// #Sireum
            |
            |package ${packageName}
            |
            |import org.sireum._
            |import ${basePackage}.util.Profile
            |import ${basePackage}.util.EmptyContainer
            |import ${basePackage}.RandomLib
            |
            |${CommentTemplate.doNotEditComment_scala}
            |
            |// Profile for initialise entrypoint
            |${genInitProfile()}
            |
            |// Profile with generators for incoming ports
            |${genProfile(params, F, preStateContainerName_P)}
            |
            |// Profile with generators for state variables and incoming ports
            |${genProfile(params, T, preStateContainerName_PS)}
            |""")
    }

    def genGetInitialiseProfilesMethodName: String = {
      return "getInitialiseProfiles"
    }

    def defaultInitialiseProfileMethodName: String = {
      return s"getDefaultInitialiseProfile"
    }

    def genGetDefaultInitialiseProfile: ST = {
      val profileName = genInitProfileName(componentSingletonType)
      return (
        st"""def $defaultInitialiseProfileMethodName: $profileName = {
            |  return $profileName (
            |    name = "Default Initialise Profile",
            |    numTests = 100)
            |}""")
    }

    def genGetInitialiseProfilesMethodSig: ST = {
      val profileName = genInitProfileName(componentSingletonType)
      return (st"""def $genGetInitialiseProfilesMethodName: MSZ[$profileName]""")
    }

    def genGetInitialiseProfilesMethodDefault: ST = {
      return (
        st"""// profiles that will be used for the initialise tests
            |override $genGetInitialiseProfilesMethodSig = {
            |  return MSZ($defaultInitialiseProfileMethodName)
            |}""")
    }

    def defaultProfileMethodName(includeStateVars: B): String = {
      return s"getDefaultProfile_${if (includeStateVars) portStateVarSuffix else portsSuffix}"
    }

    def genGetDefaultProfile(includeStateVars: B): ST = {
      val profileName = genProfileName(componentSingletonType, includeStateVars)
      val typ: String = if (includeStateVars) "Port and State Variable" else "Port"
      var entries: ISZ[ST] = ISZ(
        st"""name = "Default $typ Profile"""",
        st"numTests = 100",
        st"numTestVectorGenRetries = 100")

      val params: ISZ[GGParam] = inPorts ++ (if (includeStateVars) inStateVars else ISZ[GGParam]())
      for (p <- sortParam(params) if !p.isInstanceOf[GGStateVarParam] || includeStateVars) {
        entries = entries :+ st"${p.name} = freshRandomLib"
      }
      return (
        st"""def ${defaultProfileMethodName(includeStateVars)}: $profileName = {
            |  return ${profileName} (
            |    ${(entries, ", \n")})
            |}""")
    }

    def genGetProfilesMethodName(includeStateVars: B): ST = {
      return st"getProfiles_${if (includeStateVars) portStateVarSuffix else portsSuffix}"
    }

    def genGetProfilesMethodSig(includeStateVars: B): ST = {
      val profileName = genProfileName(componentSingletonType, includeStateVars)
      val mname = genGetProfilesMethodName(includeStateVars)
      return (st"""def $mname: MSZ[$profileName]""")
    }

    def genGetProfilesMethodDefault(includeStateVars: B): ST = {
      var comment: ST = st"// profiles that will be used to generate the incoming port values"
      if (includeStateVars) {
        comment =
          st"""$comment
              |// and the pre-state values of the state variables"""
      }
      return (
        st"""$comment
            |override ${genGetProfilesMethodSig(includeStateVars)} = {
            |  return MSZ(${defaultProfileMethodName(includeStateVars)})
            |}""")
    }
  }

  def generateContainer(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): Container = {
    val inPorts = inPortsToParams(component, componentNames)
    val inStateVars = stateVarsToParams(componentNames, annexInfo, T, aadlTypes)
    val outPorts = outPortsToParams(component, componentNames)
    val outStateVars = stateVarsToParams(componentNames, annexInfo, F, aadlTypes)
    return Container(
      componentSingletonType = componentNames.componentSingletonType,
      packageName = componentNames.packageName,
      packageNameI = componentNames.packageNameI,
      basePackage = componentNames.basePackage,
      inPorts = inPorts,
      inStateVars = inStateVars,
      outPorts = outPorts,
      outStateVars = outStateVars)
  }

  @pure def getSlangTypeName(a: AadlType): String = {
    a match {
      case i: BaseType => return i.slangType.name
      case i: BitType => return s"ISZ[B]"
      case i: EnumType => return i.nameProvider.qualifiedReferencedTypeName
      case i => return i.nameProvider.qualifiedReferencedTypeName
    }
  }

  @pure def sortParam(params: ISZ[GGParam]): ISZ[GGParam] = {
    return (for (partition <-
                   (for (kind <- SymbolKind.elements) yield ops.ISZOps(params).filter(p => p.kind == kind))) yield
      ops.ISZOps(partition).sortWith((a, b) => a.name <= b.name)).flatMap(a => a)
  }

  @pure def inPortsToParams(component: AadlThreadOrDevice, componentNames: NameProvider): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- component.features.filter(f => isInPort(f))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, componentNames)
  }

  @pure def isInPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.In
  }

  @pure def outPortsToParams(component: AadlThreadOrDevice, componentNames: NameProvider): ISZ[GGParam] = {
    val ports: ISZ[AadlPort] = for (p <- component.features.filter(f => isOutPort(f))) yield p.asInstanceOf[AadlPort]
    return portsToParams(ports, componentNames)
  }

  @pure def isOutPort(p: AadlFeature): B = {
    return p.isInstanceOf[AadlPort] && p.asInstanceOf[AadlPort].direction == Direction.Out
  }

  @pure def portsToParams(ports: ISZ[AadlPort], componentNames: NameProvider): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    for (o <- ports) {
      o match {
        case i: AadlEventPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = TypeUtil.EmptyType)
        case i: AadlEventDataPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = i.aadlType)
        case i: AadlDataPort =>
          ret = ret :+
            GGPortParam(
              port = o,
              componentNames = componentNames,
              aadlType = i.aadlType)
        case _ => halt("Infeasible")
      }
    }
    return ret
  }

  def stateVarsToParams(componentNames: NameProvider, gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)], isPre: B, aadlTypes: AadlTypes): ISZ[GGParam] = {
    var ret: ISZ[GGParam] = ISZ()
    gclSubclauseInfo match {
      case Some((GclSubclause(stateVars, _, _, _, _, _), _)) =>

        for (i <- 0 until stateVars.size) {
          val stateVar = stateVars(i)
          ret = ret :+
            GGStateVarParam(
              stateVar = stateVar,
              id = i,
              isPreState = isPre,
              aadlType = GclResolver.getAadlType(stateVar.classifier, aadlTypes, stateVar.posOpt, Reporter.create), //aadlTypes.typeMap.get(stateVar.classifier).get,
              componentNames = componentNames)
        }
      case _ =>
    }
    return ret
  }

  @pure def filterOutPorts(params: ISZ[GGParam]): ISZ[GGParam] = {
    return params.filter(p => p.kind == SymbolKind.ApiVarOutData || p.kind == SymbolKind.ApiVarOutEvent || p.kind == SymbolKind.ApiVarOutEventData)
  }

  @pure def filterInPorts(params: ISZ[GGParam]): ISZ[GGParam] = {
    return params.filter(p => p.kind == SymbolKind.ApiVarInData || p.kind == SymbolKind.ApiVarInEvent || p.kind == SymbolKind.ApiVarInEventData)
  }

  @pure def getPort(portId: String, context: AadlThreadOrDevice): AadlPort = {
    context.getPorts().filter(p => p.identifier == portId) match {
      case ISZ(p) => return p
      case _ => halt(s"Couldn't find $portId")
    }
  }

  @strictpure def isDataPort(p: AadlFeature): B = p.isInstanceOf[AadlDataPort]

  @strictpure def isEventPort(p: AadlFeature): B = p.isInstanceOf[AadlEventPort]

  def getSlangType(typ: Typed, aadlTypes: AadlTypes): String = {
    @pure def toAadl(ids: ISZ[String]): String = {
      return st"${((if (ops.ISZOps(ids).last == "Type") ops.ISZOps(ids).dropRight(1) else ids), "::")}".render
    }

    typ match {
      case i: AST.Typed.Name =>
        if (i.ids == AST.Typed.optionName) {
          val typeKey = toAadl(i.args(0).asInstanceOf[AST.Typed.Name].ids)
          return (
            if (typeKey == "art::Empty")
              "Option[art.Empty]"
            else
              s"Option[${aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName}]")
        } else {
          val typeKey = toAadl(i.ids)
          return aadlTypes.typeMap.get(typeKey).get.nameProvider.qualifiedReferencedTypeName
        }
      case _ => halt(s"Unexpected ${typ}")
    }
  }

  def rewriteToExpX(exp: AST.Exp, context: AadlThreadOrDevice,
                    componentNames: NameProvider, aadlTypes: AadlTypes,
                    stateVars: ISZ[GclStateVar]): GGExpParamHolder = {
    val e = EE(context, componentNames, aadlTypes, stateVars)
    val ret: GGExpParamHolder = e.transform_langastExp(exp) match {
      case MSome(x) => GGExpParamHolder(e.params, x)
      case _ => GGExpParamHolder(e.params, exp)
    }
    return ret
  }

  def paramsToComment(params: ISZ[GGParam]): ISZ[ST] = {
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

  @datatype trait GGParam {
    def name: String

    def originName: String

    def slangType: ST

    def preFetch: ST

    def postFetch: ST

    def isOptional: B

    def aadlType: AadlType

    def kind: SymbolKind.Type

    def setter: ST

    def getter: ST

    //def getterViaStream(streamName: String): ST

    def ranGenName: String = {
      return GumboXGenUtil.getRangenMethodName(aadlType)
    }

    @pure def getParamDef: ST = {
      return st"$name: $slangType"
    }
  }

  @datatype class GGStateVarParam(val stateVar: GclStateVar,
                                  val id: Z,
                                  val isPreState: B,
                                  val aadlType: AadlType,

                                  val componentNames: NameProvider) extends GGParam {

    val name: String = s"${if (isPreState) "In_" else ""}${stateVar.name}"
    val originName: String = stateVar.name

    val isOptional: B = F

    val slangType: ST = st"${aadlType.nameProvider.qualifiedReferencedTypeName}"

    val preFetch: ST = st"${componentNames.componentSingletonTypeQualifiedName}.${originName}"

    val postFetch: ST = preFetch

    val kind: SymbolKind.Type = if (isPreState) SymbolKind.StateVarPre else SymbolKind.StateVar

    def setter: ST = {
      return (
        st"""// setter for state variable
            |def put_${name}(value: ${slangType}): Unit = {
            |  ${componentNames.componentSingletonType}.${originName} = value
            |}""")
    }

    def getter: ST = {
      return (
        st"""// getter for state variable
            |def get_${name}(): ${slangType} = {
            |  return ${componentNames.componentSingletonType}.$originName
            |}""")
    }
  }

  @datatype class GGPortParam(val port: AadlPort,

                              val componentNames: NameProvider,
                              val aadlType: AadlType) extends GGParam {

    val name: String = s"api_${port.identifier}"
    val originName: String = port.identifier

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

    @pure def payloadType: String = {
      return aadlType.nameProvider.qualifiedPayloadName
    }

    @pure def archPortId: ST = {
      return st"${componentNames.archInstanceName}.operational_api.${originName}_Id"
    }

    @pure def slangType: ST = {
      return (if (isEvent) st"Option[${aadlType.nameProvider.qualifiedReferencedTypeName}]"
      else st"${aadlType.nameProvider.qualifiedReferencedTypeName}")
    }

    @pure def paramTypeString: String = {
      return s"${if (isIn) "incoming" else "outgoing"} ${if (isEvent) "event " else ""}${if (isData) "data" else ""} port"
    }

    @pure def observeInPortVariable: ST = {
      return st"Art.observeInPortVariable($archPortId)"
    }

    @pure def observeOutPortVariable: ST = {
      return st"Art.observeOutPortVariable($archPortId)"
    }

    @pure def preFetch: ST = {
      if (!isEvent) {
        // incoming data port so we'll assume it was initialized in the init phase
        return st"$observeInPortVariable.get.asInstanceOf[$payloadType].value"
      } else if (isData) {
        // incoming event data port so need to unpack the payload if non-empty
        return (
          st"""
              |  if (${observeInPortVariable}.nonEmpty)
              |    Some(${observeInPortVariable}.get.asInstanceOf[${aadlType.nameProvider.qualifiedPayloadName}].value)
              |  else None()""")
      } else {
        // incoming event port so no need to unpack
        return st"$observeInPortVariable.asInstanceOf[$slangType]"
      }
    }

    @pure def postFetch: ST = {
      if (!isEvent) {
        // outgoing data port that may not have received a value on this dispatch
        return (
          st"""get_$name""")
      } else if (isData) {
        // outgoing event data port so need to unpack the payload if non-empty
        return (
          st"""
              |  if (${observeOutPortVariable}.nonEmpty)
              |    Some(${observeOutPortVariable}.get.asInstanceOf[${aadlType.nameProvider.qualifiedPayloadName}].value)
              |  else None()""")
      } else {
        // outgoing event port so no need to unpack
        return st"$observeOutPortVariable.asInstanceOf[$slangType]"
      }
    }

    @pure def dataPortFetch(prefix: String): ST = {
      val msg = s"No value found on outgoing data port $originName.\\n                  Note: values placed during the initialization phase will persist across dispatches"
      return (
        st"""/** get the value of outgoing data port $originName.  If a 'fresh' value wasn't sent
            |  * during the last dispatch then return ${prefix}_$name.get.
            |  * Note: this requires outgoing data ports to be initialized during the
            |  * initialization phase or prior to system testing.
            |  */
            |def get_${name}: ${aadlType.nameProvider.qualifiedReferencedTypeName} = {
            |  $observeOutPortVariable match {
            |    case Some(${aadlType.nameProvider.qualifiedPayloadName}(value)) =>
            |      ${prefix}_$name = Some(value)
            |      return value
            |    case _ if ${prefix}_$name.isEmpty =>
            |      assert(F, "$msg")
            |      halt("$msg")
            |    case _ => return ${prefix}_$name.get
            |  }
            |}""")
    }

    @pure def setter: ST = {
      val body: ST = if (isOptional) {
        val payload: ST =
          if (port.isInstanceOf[AadlEventDataPort]) st"${payloadType}(v)"
          else st"v"
        st"""value match {
            |  case Some(v) => Art.insertInInfrastructurePort(${archPortId}, $payload)
            |  case _ =>
            |}"""
      } else {
        st"Art.insertInInfrastructurePort(${archPortId}, ${payloadType}(value))"
      }

      return (
        st"""// setter for $paramTypeString
            |def put_${originName}(value: ${slangType}): Unit = {
            |  $body
            |}""")
    }

    @pure override def getter: ST = {
      return (
        st"""// getter for $paramTypeString
            | def get_${originName}: ${paramTypeString} = {
            |   val value: Option[$aadlType] =
            |     Art.observeOutInfrastructurePort(${archPortId}).asInstanceOf[Option[$payloadType}]] match {
            |       case Some(${payloadType}(payload)) => Some(payload)
            |       case _ => None()
            |     }
            |   return ${if (!isEvent) ".get" else ""}
            |}""")
    }
  }

  @datatype class GGExpParamHolder(val params: Set[GGParam],
                                   val exp: AST.Exp)

  @record class EE(context: AadlThreadOrDevice,
                   componentNames: NameProvider,
                   aadlTypes: AadlTypes,
                   stateVars: ISZ[GclStateVar]) extends ir.MTransformer {

    var params: Set[GGParam] = Set.empty

    override def pre_langastExpSelect(o: AST.Exp.Select): ir.MTransformer.PreResult[AST.Exp] = {
      o match {
        case AST.Exp.Select(Some(AST.Exp.Ident(AST.Id("api"))), id, attr) =>
          val typed = o.attr.typedOpt.get.asInstanceOf[AST.Typed.Name]
          val (typ, _) = getAadlType(typed, aadlTypes)
          val ports = context.getPorts().filter(p => p.identifier == id.value)
          assert(ports.size == 1)
          val param = GGPortParam(
            port = ports(0),
            componentNames = componentNames,
            aadlType = typ)
          params = params + param
          return ir.MTransformer.PreResult(F,
            MSome(AST.Exp.Ident(id = AST.Id(value = param.name, attr = AST.Attr(None())), attr = o.attr)))
        case _ =>
          return ir.MTransformer.PreResult(T, MNone[AST.Exp]())
      }
    }

    def findStateVar(name: String, vars: ISZ[GclStateVar]): (Z, GclStateVar) = {
      for (i <- 0 until vars.size if vars(i).name == name) {
        return (i, vars(i))
      }
      halt(s"Infeasible: didn't find state var ${name}")
    }

    override def pre_langastExpInput(o: AST.Exp.Input): ir.MTransformer.PreResult[AST.Exp] = {
      val ret: AST.Exp.Ident = o.exp match {
        case i: AST.Exp.Ident =>
          val name = s"In_${i.id.value}"

          val typed: AST.Typed.Name = i.attr.typedOpt match {
            case Some(atn: AST.Typed.Name) => atn
            case x => halt(s"Infeasible: ${i.id.value} had the following for its typed opt ${x}")
          }

          val (typ, _) = getAadlType(typed, aadlTypes)

          val (index, stateVar) = findStateVar(i.id.value, stateVars)

          params = params +
            GGStateVarParam(
              stateVar = stateVar,
              id = index,
              isPreState = T,
              aadlType = typ,
              componentNames = componentNames)
          AST.Exp.Ident(id = AST.Id(value = name, attr = o.attr), attr = i.attr)
        case _ => halt(s"Unexpected ${o.exp}")
      }
      return ir.MTransformer.PreResult(F, MSome(ret))
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.typedOpt match {
        case Some(typed: AST.Typed.Name) =>
          val (typ, _) = getAadlType(typed, aadlTypes)

          val (index, stateVar) = findStateVar(o.id.value, stateVars)

          params = params +
            GGStateVarParam(
              stateVar = stateVar,
              id = index,
              isPreState = F,
              aadlType = typ,
              componentNames = componentNames)
        case _ =>
      }
      return ir.MTransformer.PreResult(F, MNone[AST.Exp]())
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


  def getAadlType(typ: AST.Typed.Name, aadlTypes: AadlTypes): (AadlType, B) = {
    var isOptional: B = F
    val ids: ISZ[String] = typ match {
      case AST.Typed.Name(AST.Typed.optionName, ISZ(i: AST.Typed.Name)) =>
        isOptional = T
        i.ids
      case _ => typ.ids
    }
    val _ids: ISZ[String] =
      if (ids(ids.size - 1) == "Type") ops.ISZOps(typ.ids).dropRight(1)
      else ids

    if (_ids.size == 2 && _ids(0) == "art" && _ids(1) == "Empty") {
      return (TypeUtil.EmptyType, isOptional)
    } else if (_ids.size == 3 && _ids(0) == "org" && _ids(1) == "sireum") {
      val aadlType = TypeResolver.getAadlBaseFromSlangType(_ids)
      return (aadlTypes.typeMap.get(aadlType).get, isOptional)
    } else {
      val key = st"${(_ids, "::")}".render
      return (aadlTypes.typeMap.get(key).get, isOptional)
    }
  }
}
