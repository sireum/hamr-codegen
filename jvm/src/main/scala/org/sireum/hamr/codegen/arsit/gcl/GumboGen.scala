// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboGen._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.resolvers.GclResolver.GUMBO__Library
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.lang.ast.MethodContract.Simple
import org.sireum.lang.{ast => AST}
import org.sireum.message.Reporter

object GumboGen {

  @sig trait GclEntryPointContainer

  @datatype class GclEntryPointInitialize(val imports: ISZ[String],
                                          val markers: ISZ[Marker],
                                          val contract: ST,

                                          val modifies: ISZ[ST],
                                          val requires: ISZ[ST],
                                          val ensures: ISZ[ST],
                                          val flows: ISZ[ST]
                                         ) extends GclEntryPointContainer

  @sig trait GclHolder {
    def toST: ST

    def toSTMin: ST
  }

  @sig trait GclGeneralHolder extends GclHolder

  @datatype class GclRequiresHolder(val id: String,
                                    val descriptor: Option[ST],
                                    val requires: ST) extends GclGeneralHolder {
    def toST: ST = {
      val ret =
        st"""// assume ${id}
            |${descriptor}
            |Requires(${requires})"""
      return ret
    }

    def toSTMin: ST = {
      val ret =
        st"""// assume ${id}
            |${descriptor}
            |${requires}"""
      return ret

    }
  }

  @datatype class GclEnsuresHolder(val id: String,
                                   val descriptor: Option[ST],
                                   val ensures: ST) extends GclGeneralHolder {
    def toST: ST = {
      val ret =
        st"""// guarantee ${id}
            |${descriptor}
            |Ensures(${ensures})"""
      return ret
    }

    def toSTMin: ST = {
      val ret =
        st"""// guarantee ${id}
            |${descriptor}
            |${ensures}"""
      return ret
    }

  }

  @datatype class GclComputeEventHolder(val modifies: Option[ST],
                                        val requires: Option[ST],
                                        val ensures: Option[ST],
                                        val flows: Option[ST]) extends GclHolder {
    def toST: ST = {
      halt("stub")
    }

    def toSTMin: ST = {
      halt("stub")
    }
  }

  @datatype class GclCaseHolder(val caseId: String,
                                val descriptor: Option[ST],
                                val requires: ST,
                                val ensures: ST) extends GclHolder {
    def toST: ST = {
      val ret =
        st"""Case("${caseId}"
            |  ${descriptor}
            |  Requires(${requires}),
            |  Ensures(${ensures})
            |)"""
      return ret;
    }

    def toSTMin: ST = {
      val ret: ST =
        st"""// case ${caseId}
            |${descriptor}
            |($requires) -->: ($ensures)"""
      return ret
    }
  }

  @datatype class GclEntryPointPeriodicCompute(val markers: ISZ[Marker],
                                               val modifies: Option[ST],
                                               val requires: Option[ST],
                                               val ensures: Option[ST],
                                               val flows: Option[ST]) extends GclEntryPointContainer

  @datatype class GclEntryPointSporadicCompute(val markers: ISZ[Marker],
                                               val handlers: HashSMap[AadlPort, GclComputeEventHolder]) extends GclEntryPointContainer

  @datatype class GclApiContributions(val objectContributions: ISZ[ST],
                                      val datatypeContributions: ISZ[ST],
                                      val requiresContributions: ISZ[ST],
                                      val ensuresContributions: ISZ[ST])

  val FunctionMarker: Marker = Marker("// BEGIN FUNCTIONS", "// END FUNCTIONS")
  val StateVarMarker: Marker = Marker("// BEGIN STATE VARS", "// END STATE VARS")

  val InitializesModifiesMarker: Marker = Marker("// BEGIN INITIALIZES MODIFIES", "// END INITIALIZES MODIFIES")
  val InitializesEnsuresMarker: Marker = Marker("// BEGIN INITIALIZES ENSURES", "// END INITIALIZES ENSURES")
  val InitializesRequiresMarker: Marker = Marker("// BEGIN INITIALIZES REQUIRES", "// END INITIALIZES REQUIRES")
  val InitializesFlowsMarker: Marker = Marker("// BEGIN INITIALIZES FLOWS", "// END INITIALIZES FLOWS")

  var imports: ISZ[String] = ISZ()

  def resetImports(): Unit = {
    imports = ISZ()
  }

  def addImports(gen: GumboGen): Unit = {
    imports = imports ++ gen.imports
  }

  @record class StateVarInRewriter() extends org.sireum.hamr.ir.MTransformer {

    def wrapStateVarsInInput(o: AST.Exp): AST.Exp = {
      val ret: AST.Exp = transform_langastExp(o) match {
        case MSome(r) => r
        case _ => o
      }
      return ret
    }

    override def pre_langastExpInput(o: AST.Exp.Input): org.sireum.hamr.ir.MTransformer.PreResult[AST.Exp] = {
      // currently resolving phase ensures o.exp can only be a state vars so nothing to do
      return org.sireum.hamr.ir.MTransformer.PreResult(F, MNone())
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[AST.Exp] = {
      o.attr.resOpt match {
        case Some(v: AST.ResolvedInfo.Var) =>
          // the only vars the gumbo clause can refer to are state vars so
          // checking whether o is refering to a state var isn't needed
          return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(AST.Exp.Input(o, AST.Attr(None()))))
        case _ =>
          return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone())
      }
    }
  }

  @record class InvokeRewriter(val aadlTypes: AadlTypes, val basePackageName: String) extends org.sireum.hamr.ir.MTransformer {
    def rewriteInvokes(o: AST.Exp): AST.Exp = {
      val ret: AST.Exp = transform_langastExp(o) match {
        case MSome(r) => r
        case _ => o
      }
      return ret
    }

    def convertSelects(exp: Option[AST.Exp]): String = {
      val ret: String = exp match {
        case Some(s: AST.Exp.Select) =>
          if (s.receiverOpt.isEmpty) s.id.value
          else s"${convertSelects(s.receiverOpt)}::${s.id.value}"
        case Some(AST.Exp.Ident(id)) => id.value
        case Some(x) => halt(s"Unexpected Exp.Select form: '${x}''")
        case _ => ""
      }
      return ret
    }

    def convertToSelect(value: IS[Z, String]): Option[AST.Exp] = {
      return (
        if (value.isEmpty) None()
        else if (value.size == 1) Some(
          AST.Exp.Ident(id = AST.Id(value = value(0), attr = AST.Attr(None())), attr = AST.ResolvedAttr(None(), None(), None())))
        else Some(AST.Exp.Select(
          receiverOpt = convertToSelect(ops.ISZOps(value).dropRight(1)),
          id = AST.Id(value = value(value.size - 1), attr = AST.Attr(None())),
          targs = ISZ(), attr = AST.ResolvedAttr(None(), None(), None())))
        )
    }

    override def post_langastExpResult(o: AST.Exp.Result): MOption[AST.Exp] = {
      o.attr.typedOpt match {
        case Some(atn: AST.Typed.Name) =>
          val fqAadlTypeName = st"${(atn.ids, "::")}".render
          val aadlType = aadlTypes.typeMap.get(fqAadlTypeName).get

          val splitSlangTypeName = aadlType.nameProvider.qualifiedReferencedTypeNameI

          val name = AST.Name(ids = splitSlangTypeName.map((a: String) => AST.Id(value = a, attr = AST.Attr(None()))), attr = AST.Attr(None()))
          val slangTypedName = AST.Type.Named(name = name, typeArgs = ISZ(), attr = o.attr)

          return MSome(o(tipeOpt = Some(slangTypedName)))
        case _ => return MNone()
      }
    }

    override def post_langastExpInvoke(o: AST.Exp.Invoke): MOption[AST.Exp] = {
      val ret: MOption[AST.Exp] = o.attr.resOpt.get match {
        case arm: AST.ResolvedInfo.Method if arm.mode == AST.MethodMode.Constructor =>
          val componentName = s"${convertSelects(o.receiverOpt)}::${o.ident.id.value}"
          val path: IdPath = aadlTypes.typeMap.get(componentName) match {
            case Some(t) => t.nameProvider.qualifiedReferencedTypeNameI
            case _ => halt(s"Couldn't find an AADL data component corresponding to '${componentName}''")
          }
          val receiver = convertToSelect(ops.ISZOps(path).dropRight(1))
          val ident = AST.Exp.Ident(id = AST.Id(value = path(path.size - 1), attr = o.ident.id.attr), attr = o.ident.attr)
          MSome(o(receiverOpt = receiver, ident = ident))
        case _ => MNone()
      }
      return ret
    }
  }

  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexClauseInfo] = {
    val aadlComponent = symbolTable.componentMap.get(componentPath).get
    val annexInfos: ISZ[GclAnnexClauseInfo] = symbolTable.annexClauseInfos.get(aadlComponent.path) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexClauseInfo]).map(m => m.asInstanceOf[GclAnnexClauseInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processGclLibrary(gclLib: GclAnnexLibInfo, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackage: String): (ST, ISZ[String]) = {
    val GclAnnexLibInfo(annex, name, gclSymbolTable) = gclLib

    val gg = GumboGen(gclSymbolTable = gclSymbolTable, symbolTable = symbolTable, aadlTypes = aadlTypes, basePackageName = basePackage)
    val methods = annex.methods.map((m: GclMethod) => gg.processGclMethod(m))

    val filename: ISZ[String] = ISZ(basePackage) ++ annex.containingPackage.name :+ s"${GUMBO__Library}.scala"

    val _imports: Option[ST] =
      if (gg.imports.isEmpty) None()
      else Some(st"${(for (i <- gg.imports) yield s"import $i", "\n")}")

    return (
      st"""// #Sireum
          |
          |package ${basePackage}.${(annex.containingPackage.name, ".")}
          |
          |import org.sireum._
          |import ${basePackage}._
          |${_imports}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${GUMBO__Library} {
          |  ${(methods, "\n\n")}
          |}
          |""", filename)
  }

  @pure def toKey(e: AST.Exp): SymTableKey = {
    //assert (e.fullPosOpt.nonEmpty, e.string)
    return SymTableKey(e, e.fullPosOpt)
  }

  def getRExp(e: AST.Exp, aadlTypes: AadlTypes, gclSymbolTable: GclSymbolTable, basePackageName: String): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(toKey(e)).get)
  }

  def processInitializes(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackage: String): Option[GclEntryPointInitialize] = {
    resetImports()

    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if (ais.nonEmpty) {
      val sc = ais(0).annex
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.initializes.nonEmpty) {
        val rModifies: ISZ[ST] = sc.initializes.get.modifies.map((m: AST.Exp) => st"${gclSymbolTable.rexprs.get(toKey(m)).get}")

        var modifies: ISZ[ST] = ISZ()
        var requires: ISZ[ST] = ISZ()
        var ensures: ISZ[ST] = ISZ()
        var flows: ISZ[ST] = ISZ()
        var markers: ISZ[Marker] = ISZ()

        val inits: ISZ[ST] = sc.initializes.get.guarantees.map((m: GclGuarantee) => {
          imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(m.exp)
          st"""// guarantee ${m.id}
              |${processDescriptor(m.descriptor, "//   ")}
              |${getRExp(m.exp, aadlTypes, gclSymbolTable, basePackage)}"""
        })

        val optModifies: Option[ST] =
          if (rModifies.nonEmpty) {
            markers = markers :+ InitializesModifiesMarker

            modifies = modifies :+
              st"""${InitializesModifiesMarker.beginMarker}
                  |${(rModifies, ",\n")}
                  |${InitializesModifiesMarker.endMarker}"""

            Some(
              st"""Modifies(
                  |  ${InitializesModifiesMarker.beginMarker}
                  |  ${(rModifies, ",\n")}
                  |  ${InitializesModifiesMarker.endMarker}
                  |),""")
          } else {
            None()
          }

        val optEnsures: Option[ST] =
          if (inits.nonEmpty) {
            markers = markers :+ InitializesEnsuresMarker

            ensures = ensures :+
              st"""${InitializesEnsuresMarker.beginMarker}
                  |${(inits, ",\n")}
                  |${InitializesEnsuresMarker.endMarker}"""
            Some(
              st"""Ensures(
                  |  ${InitializesEnsuresMarker.beginMarker}
                  |  ${(inits, ",\n")}
                  |  ${InitializesEnsuresMarker.endMarker}
                  |)""")
          } else {
            None()
          }

        val generalRequires = addBuiltInOutgoingEventPortRequires(m)
        val optRequires: Option[ST] =
          if (generalRequires.nonEmpty) {
            markers = markers :+ InitializesRequiresMarker

            requires = requires :+
              st"""${InitializesRequiresMarker.beginMarker}
                  |${generalRequires.map((m: GclHolder) => m.toSTMin)}
                  |${InitializesRequiresMarker.endMarker}"""
            Some(
              st"""Requires(
                  |  ${InitializesRequiresMarker.beginMarker}
                  |  ${generalRequires.map((m: GclHolder) => m.toSTMin)}
                  |  ${InitializesRequiresMarker.endMarker}
                  |),""")
          } else {
            None()
          }

        var optFlows: Option[ST] = None()
        if (sc.initializes.get.flows.nonEmpty) {

          var initFlows: ISZ[ST] = ISZ()

          for (f <- sc.initializes.get.flows) {
            val froms: ISZ[AST.Exp] = for (e <- f.from) yield gclSymbolTable.rexprs.get(toKey(e)).get
            val tos: ISZ[AST.Exp] = for (e <- f.to) yield gclSymbolTable.rexprs.get(toKey(e)).get
            initFlows = initFlows :+
              st"""// infoflow ${f.id}
                  |${GumboGen.processDescriptor(f.descriptor, "//   ")}
                  |Flow("${f.id}",
                  |  From(${(froms, ", ")}),
                  |  To(${(tos, ", ")})
                  |)"""
          }

          flows = flows :+
            st"""${InitializesFlowsMarker.beginMarker}
                |${(initFlows, ",\n")}
                |${InitializesFlowsMarker.endMarker}"""

          optFlows = Some(
            st"""InfoFlows(
                |  ${InitializesFlowsMarker.beginMarker}
                |  ${(initFlows, ",\n")}
                |  ${InitializesFlowsMarker.endMarker}
                |),""")
        }

        val ret: ST =
          st"""Contract(
              |  ${optRequires}
              |  ${optModifies}
              |  ${optEnsures}
              |  ${optFlows}
              |)"""

        return Some(GclEntryPointInitialize(imports, markers, ret, modifies, requires, ensures, flows))
      } else {
        return None()
      }
    } else {
      return None()
    }
  }

  def processInvariants(e: AadlType, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): ISZ[ST] = {
    resetImports()
    var ret: ISZ[ST] = ISZ()

    val FIXME = ISZ(e.name)
    val ais = getGclAnnexInfos(FIXME, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for (ai <- ais) {
      val sc = ai.annex
      val gclSymTable = ai.gclSymbolTable
      val gg = GumboGen(gclSymTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(sc.invariants)
      addImports(gg)
    }

    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 symbolTable: SymbolTable,
                                 aadlTypes: AadlTypes,
                                 basePackageName: String): Map[AadlPort, GclApiContributions] = {
    resetImports()
    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if (ais.nonEmpty) {
      val sc = ais(0).annex
      val gclSymbolTable = ais(0).gclSymbolTable

      val ret: Map[AadlPort, GclApiContributions] = {
        if (gclSymbolTable.apiReferences.nonEmpty || gclSymbolTable.integrationMap.nonEmpty) {
          val gg = GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          val _contracts = gg.processIntegrationContract(m, gclSymbolTable)
          addImports(gg)
          _contracts
        } else {
          Map.empty
        }
      }
      return ret
    } else {
      return Map.empty
    }
  }

  def processSubclauseFunctions(gclMethods: ISZ[GclMethod], gclSymbolTable: GclSymbolTable, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): (ST, Marker) = {
    return GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processSubclauseFunctions(gclMethods)
  }

  def processStateVars(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String): Option[(ST, Marker)] = {
    val ais = getGclAnnexInfos(m.path, symbolTable)

    if (ais.nonEmpty) {
      val sc = ais(0).annex
      val gclSymbolTable = ais(0).gclSymbolTable

      if (sc.state.nonEmpty) {
        return Some(GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processStateVars(sc.state))
      } else {
        return None()
      }
    } else {
      return None()
    }
  }

  def addBuiltInInEventPortAssumes(context: AadlThreadOrDevice, inEventPort: AadlPort): GclRequiresHolder = {

    if (inEventPort.isInstanceOf[AadlEventDataPort]) {
      return GclRequiresHolder(
        id = "HAMR-Guarantee built-in",
        descriptor = Some(
          st"""//   The spec var corresponding to the handled event must be non-empty and
              |//   the passed in payload must be the same as the spec var's value"""),
        requires =
          st"""api.${inEventPort.identifier}.nonEmpty &&
              |api.${inEventPort.identifier}.get == value"""
      )
    } else if (inEventPort.isInstanceOf[AadlEventPort]) {
      return GclRequiresHolder(
        id = "HAMR-Guarantee built-in",
        descriptor = Some(
          st"""//   The spec var corresponding to the handled event must be non-empty"""),
        requires =
          st"""api.${inEventPort.identifier}.nonEmpty"""
      )
    } else {
      halt("Unexpected")
    }

  }

  def addBuiltInOutgoingEventPortRequires(context: AadlThreadOrDevice): ISZ[GclHolder] = {
    val outgoingEventPorts = context.getPorts().filter((p: AadlPort) => p.isInstanceOf[AadlFeatureEvent] && p.direction == Direction.Out)

    var ret: ISZ[GclHolder] = ISZ()
    if (outgoingEventPorts.nonEmpty) {
      val ensures: ISZ[ST] = outgoingEventPorts.map((m: AadlPort) => st"api.${m.identifier}.isEmpty")
      ret = ret :+ GclRequiresHolder(
        id = "AADL_Requirement",
        descriptor = Some(st"//   All outgoing event ports must be empty"),
        requires = st"${(ensures, ",\n")}"
      )
    }
    return ret
  }

  def convertToMethodName(s: String): String = {

    def isInt(c: C): B = {
      return c >= c"1" && c <= c"9"
    }

    def isChar(c: C): B = {
      return (c >= c"A" && c <= c"Z") || (c >= c"a" && c <= c"z")
    }

    var cis = ops.ISZOps(conversions.String.toCis(s)).map((c: C) => if (isInt(c) || isChar(c)) c else c"_")
    if (isInt(cis(0))) {
      cis = c"_" +: cis
    }
    return conversions.String.fromCis(cis)
  }


  def processDescriptor(descriptor: Option[String], pad: String): Option[ST] = {
    def getPipeLoc(cis: ISZ[C]): Z = {
      var firstNonSpace: Z = 0
      while (cis(firstNonSpace) == ' ' || cis(firstNonSpace) == '\t') {
        firstNonSpace = firstNonSpace + 1
      }
      if (firstNonSpace < cis.size - 1 && cis(firstNonSpace) == '|') {
        return firstNonSpace + 1
      } else {
        return 0
      }
    }

    val ret: Option[ST] = descriptor match {
      case Some(s) =>
        val lines = StringUtil.split_PreserveEmptySegments(s, (c: C) => c == '\n')
        var mLines: ISZ[ST] = ISZ(st"${pad}${lines(0)}")
        if (lines.size > 1) {
          for (i <- 1 until lines.size) {
            val lop = conversions.String.toCis(lines(i))
            val pipeLoc = getPipeLoc(lop)
            val x = conversions.String.fromCis(ops.ISZOps(lop).slice(pipeLoc, lop.size))
            mLines = mLines :+ st"${pad}${x}"
          }
        }
        Some(st"${(mLines, "\n")}")
      case _ => None()
    }
    return ret
  }
}

@record class GumboGen(gclSymbolTable: GclSymbolTable,
                       symbolTable: SymbolTable,
                       aadlTypes: AadlTypes,
                       basePackageName: String) {
  var imports: ISZ[String] = ISZ()

  def getRExp(e: AST.Exp): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(toKey(e)).get)
  }

  def getR2Exp(e: AST.Exp.Ref): AST.Exp = {
    return GumboGen.InvokeRewriter(aadlTypes, basePackageName).rewriteInvokes(gclSymbolTable.rexprs.get(toKey(e.asExp)).get)
  }

  def fetchHandler(port: AadlPort, handlers: ISZ[GclHandle]): Option[GclHandle] = {
    var ret: Option[GclHandle] = None()
    for (h <- handlers if ret.isEmpty) {
      gclSymbolTable.computeHandlerPortMap.get(h.port) match {
        case Some(p) if p == port => ret = Some(h)
        case _ =>
      }
    }
    return ret
  }

  def processCompute(compute: GclCompute, optInEvent: Option[AadlPort], context: AadlThreadOrDevice): (ContractBlock, ISZ[Marker]) = {
    resetImports()

    var markers: Set[Marker] = Set.empty
    var rreads: ISZ[ST] = ISZ()
    var rrequires: ISZ[ST] = ISZ()
    var rmodifies: ISZ[ST] = ISZ()
    var rensures: ISZ[ST] = ISZ()
    var rflows: ISZ[ST] = ISZ()

    def genComputeMarkerCreator(id: String, typ: String): Marker = {
      val m = Marker(
        s"// BEGIN COMPUTE ${typ} ${id}",
        s"// END COMPUTE ${typ} ${id}")

      markers = markers + m
      return m
    }

    val generalModifies: Set[String] = Set(compute.modifies.map((e: AST.Exp) => s"${gclSymbolTable.rexprs.get(toKey(e)).get}"))

    var generalHolder: ISZ[GclHolder] = ISZ()

    if (context.isSporadic() && optInEvent.nonEmpty) {
      generalHolder = generalHolder :+ addBuiltInInEventPortAssumes(context, optInEvent.get)
    }

    generalHolder = generalHolder ++ addBuiltInOutgoingEventPortRequires(context)


    for (spec <- compute.specs) {
      val rspec = gclSymbolTable.rexprs.get(toKey(spec.exp)).get
      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec)

      val id = spec.id
      val descriptor = GumboGen.processDescriptor(spec.descriptor, "//   ")

      val genHolder: GclGeneralHolder = spec match {
        case g: GclAssume =>
          val rassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rspec)
          GclRequiresHolder(id, descriptor, st"$rassume")
        case g: GclGuarantee => GclEnsuresHolder(id, descriptor, st"$rspec")
      }

      generalHolder = generalHolder :+ genHolder
    }

    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {

        val rexp = gclSymbolTable.rexprs.get(toKey(generalCase.assumes)).get
        val rrassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rexp)
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rrassume)

        val rguarantee = gclSymbolTable.rexprs.get(toKey(generalCase.guarantees)).get
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee)

        generalHolder = generalHolder :+ GclCaseHolder(
          caseId = generalCase.id,
          descriptor = GumboGen.processDescriptor(generalCase.descriptor, "//   "),
          requires = st"${rrassume}",
          ensures = st"${rguarantee}")
      }
    }

    var generalFlows: ISZ[ST] = ISZ()

    for (f <- compute.flows) {
      val froms: ISZ[AST.Exp] = for (e <- f.from) yield gclSymbolTable.rexprs.get(toKey(e)).get
      val tos: ISZ[AST.Exp] = for (e <- f.to) yield gclSymbolTable.rexprs.get(toKey(e)).get
      generalFlows = generalFlows :+
        st"""// infoflow ${f.id}
            |${GumboGen.processDescriptor(f.descriptor, "//   ")}
            |Flow("${f.id}",
            |  From(${(froms, ", ")}),
            |  To(${(tos, ", ")})
            |)"""
    }

    val generalRequires: ISZ[GclRequiresHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclRequiresHolder]).map((m: GclHolder) => m.asInstanceOf[GclRequiresHolder])
    val generalEnsures: ISZ[GclEnsuresHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclEnsuresHolder]).map((m: GclHolder) => m.asInstanceOf[GclEnsuresHolder])
    val generalCases: ISZ[GclCaseHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclCaseHolder]).map((m: GclHolder) => m.asInstanceOf[GclCaseHolder])

    if (context.isSporadic()) {
      if (optInEvent.nonEmpty) {

        val eventPort = optInEvent.get

        var handlerRequires = generalRequires

        if (generalFlows.nonEmpty) {
          val marker = genComputeMarkerCreator(eventPort.identifier, "FLOW")

          rflows = rflows :+
            st"""${marker.beginMarker}
                |${(generalFlows, ",\n")}
                |${marker.endMarker}"""
        }

        fetchHandler(eventPort, compute.handlers) match {
          case Some(handler) => {
            if (generalModifies.nonEmpty || handler.modifies.nonEmpty) {
              val modMarker = genComputeMarkerCreator(eventPort.identifier, "MODIFIES")
              val handlerModifies = generalModifies ++ handler.modifies.map((m: AST.Exp) => s"${gclSymbolTable.rexprs.get(toKey(m)).get}")

              rmodifies = rmodifies :+
                st"""${modMarker.beginMarker}
                    |${(handlerModifies.elements, ",\n")}
                    |${modMarker.endMarker}"""
            }

            if (handlerRequires.nonEmpty) {
              val marker = genComputeMarkerCreator(eventPort.identifier, "REQUIRES")
              val elems = handlerRequires.map((m: GclRequiresHolder) => m.toSTMin)

              rrequires = rrequires :+
                st"""${marker.beginMarker}
                    |${(elems, ",\n")}
                    |${marker.endMarker}"""
            }

            if (generalEnsures.nonEmpty || handler.guarantees.nonEmpty || generalCases.nonEmpty) {
              val generalElems = generalEnsures.map((m: GclEnsuresHolder) => m.toSTMin)
              val _cases = generalCases.map((m: GclCaseHolder) => m.toSTMin)

              val handlerEnsures = generalElems ++ _cases ++
                handler.guarantees.map((g: GclGuarantee) => {
                  val rexp = gclSymbolTable.rexprs.get(toKey(g.exp)).get
                  imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp)
                  st"""// guarantees ${g.id}
                      |${GumboGen.processDescriptor(g.descriptor, "//   ")}
                      |${rexp}"""
                })

              val marker = genComputeMarkerCreator(eventPort.identifier, "ENSURES")

              rensures = rensures :+
                st"""${marker.beginMarker}
                    |${(handlerEnsures, ",\n")}
                    |${marker.endMarker}"""
            }
          }
          case _ => {
            // use the general ones

            if (generalModifies.nonEmpty) {
              val modMarker = genComputeMarkerCreator(eventPort.identifier, "MODIFIES")
              rmodifies = rmodifies :+
                st"""${modMarker.beginMarker}
                    |${(generalModifies.elements, ",\n")}
                    |${modMarker.endMarker}"""
            }

            if (handlerRequires.nonEmpty) {
              val marker = genComputeMarkerCreator(eventPort.identifier, "REQUIRES")
              val elems = handlerRequires.map((m: GclRequiresHolder) => m.toSTMin)

              rrequires = rrequires :+
                st"""${marker.beginMarker}
                    |${(elems, ",\n")}
                    |${marker.endMarker}"""
            }

            if (generalEnsures.nonEmpty || generalCases.nonEmpty) {
              val generalElems = generalEnsures.map((m: GclEnsuresHolder) => m.toSTMin)
              val _cases = generalCases.map((m: GclCaseHolder) => m.toSTMin)

              val handlerEnsures = generalElems ++ _cases

              val marker = genComputeMarkerCreator(eventPort.identifier, "ENSURES")

              rensures = rensures :+
                st"""${marker.beginMarker}
                    |${(handlerEnsures, ",\n")}
                    |${marker.endMarker}"""
            }
          }
        } // end handler match
      }
    } else {
      // periodic component so use the general ones

      if (compute.handlers.nonEmpty) {
        halt(s"${context.identifier} is periodic but has handlers -- resolver phase should have rejected this")
      }

      val id = "timeTriggered"

      if (generalModifies.nonEmpty) {
        val modMarker = genComputeMarkerCreator(id, "MODIFIES")

        rmodifies = rmodifies :+
          st"""${modMarker.beginMarker}
              |${(generalModifies.elements, ",\n")}
              |${modMarker.endMarker}"""
      }

      if (generalRequires.nonEmpty) {
        val marker = genComputeMarkerCreator(id, "REQUIRES")
        val elems = generalRequires.map((m: GclRequiresHolder) => m.toSTMin)

        rrequires = rrequires :+
          st"""${marker.beginMarker}
              |${(elems, ",\n")}
              |${marker.endMarker}"""
      }

      if (generalEnsures.nonEmpty || generalCases.nonEmpty) {
        val generalElems = generalEnsures.map((m: GclEnsuresHolder) => m.toSTMin)
        val _cases = generalCases.map((m: GclCaseHolder) => m.toSTMin)

        val handlerEnsures = generalElems ++ _cases

        val marker = genComputeMarkerCreator(id, "ENSURES")

        rensures = rensures :+
          st"""${marker.beginMarker}
              |${(handlerEnsures, ",\n")}
              |${marker.endMarker}"""
      }

      if (generalFlows.nonEmpty) {
        val marker = genComputeMarkerCreator(context.identifier, "FLOW")

        rflows = rflows :+
          st"""${marker.beginMarker}
              |${(generalFlows, ",\n")}
              |${marker.endMarker}"""
      }
    } // end periodic branch

    return (NonCaseContractBlock(imports, rreads, rrequires, rmodifies, rensures, rflows), markers.elements)
  }

  def processGclMethod(gclMethod: GclMethod): ST = {
    val methodName = gclMethod.method.sig.id.value

    val returnType: String = {
      val retTypeName: String = gclMethod.method.sig.returnType match {
        case atn: AST.Type.Named =>
          val key = st"${(atn.name.ids.map((i: AST.Id) => i.value), "::")}".render
          val aadlType = aadlTypes.typeMap.get(key).get.nameProvider
          aadlType.qualifiedReferencedTypeName
        case _ => halt("No")
      }
      retTypeName
    }

    val params: ISZ[String] = gclMethod.method.sig.params.map((p: AST.Param) => {
      val paramTypeName: ISZ[String] = p.tipe match {
        case atn: AST.Type.Named =>
          atn.name.ids.map((i: AST.Id) => i.value)
        case _ => halt("No")
      }
      val key = st"${(paramTypeName, "::")}".render

      val aadlType = GclResolver.getAadlType(key, aadlTypes, p.id.attr.posOpt, Reporter.create).nameProvider
      //val aadlType = aadlTypes.typeMap.get(key).get.nameProvider
      s"${p.id.value}: ${aadlType.qualifiedReferencedTypeName}"
    })

    val rexp: AST.Exp = gclMethod.method.bodyOpt match {
      case Some(AST.Body(ISZ(AST.Stmt.Return(Some(exp))))) => getRExp(exp)
      case _ => halt("Unexpected: should be a return statement containing a single expression")
    }

    imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp)

    var isPure = F
    val purity: String = gclMethod.method.purity match {
      case AST.Purity.Pure =>
        isPure = T
        "pure"
      case AST.Purity.StrictPure => "strictpure"
      case AST.Purity.Abs => "abs"
      case AST.Purity.Impure => "impure"
      case AST.Purity.Memoize => "memoize"
    }

    val body: ST = if (isPure) {
      val contractOpt: Option[ST] = if (gclMethod.method.mcontract.nonEmpty) {
        val scontract: Simple = gclMethod.method.mcontract.asInstanceOf[Simple]

        val readsOpt: Option[ST] =
          if (scontract.reads.isEmpty) None()
          else Some(st"Reads(${(scontract.reads.map((i: AST.Exp.Ref) => getR2Exp(i)), ",")}),")

        val requiresOpt: Option[ST] =
          if (scontract.requires.isEmpty) None()
          else Some(st"Requires(${(scontract.requires.map((e: AST.Exp) => getRExp(e)), ",")}),")

        val modifiesOpt: Option[ST] =
          if (scontract.modifies.isEmpty) None()
          else Some(st"Modifies(${(scontract.modifies.map((i: AST.Exp.Ref) => getR2Exp(i)), ",")}),")

        val ensuresOpt: Option[ST] =
          if (scontract.ensures.isEmpty) None()
          else Some(st"Ensures(${(scontract.ensures.map((e: AST.Exp) => getRExp(e)), ",")})")

        Some(
          st"""Contract(
              |  $readsOpt
              |  $requiresOpt
              |  $modifiesOpt
              |  $ensuresOpt
              |)""")
      } else {
        None()
      }

      st"""{
          |  ${contractOpt}
          |  return ${rexp}
          |}"""
    } else {
      st"${rexp}"
    }

    return (st"""@${purity} def ${methodName}(${(params, ", ")}): ${returnType} = ${body}""")
  }

  def processSubclauseFunctions(methods: ISZ[GclMethod]): (ST, Marker) = {
    val sts: ISZ[ST] = methods.map((m: GclMethod) => processGclMethod(m))

    return (
      st"""${GumboGen.FunctionMarker.beginMarker}
          |${(sts, "\n\n")}
          |${GumboGen.FunctionMarker.endMarker}""", GumboGen.FunctionMarker)
  }

  def processStateVars(stateVars: ISZ[GclStateVar]): (ST, Marker) = {
    val svs: ISZ[ST] = stateVars.map((sv: GclStateVar) => {
      //val typ = aadlTypes.typeMap.get(sv.classifier).get
      val typ = GclResolver.getAadlType(sv.classifier, aadlTypes, sv.posOpt, Reporter.create)
      val typeNames = typ.nameProvider
      st"var ${sv.name}: ${typeNames.qualifiedReferencedTypeName} = ${typeNames.example()}"
    })

    return (
      st"""${GumboGen.StateVarMarker.beginMarker}
          |${(svs, "\n\n")}
          |${GumboGen.StateVarMarker.endMarker}""", GumboGen.StateVarMarker)
  }

  def processInvariants(invariants: ISZ[GclInvariant]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    for (i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.id)

      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(i.exp)

      // will be placed in data type def so use resolved exp
      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${getRExp(i.exp)}
            |)"""
    }
    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 gclSymTable: GclSymbolTable): Map[AadlPort, GclApiContributions] = {
    var ret: Map[AadlPort, GclApiContributions] = Map.empty

    for (port <- m.getPorts()) {
      val integration: Option[GclSpec] = gclSymTable.integrationMap.get(port)

      val (aadlType, isEvent, isData): (AadlType, B, B) = port match {
        case i: AadlEventDataPort => (i.aadlType, T, T)
        case i: AadlDataPort => (i.aadlType, F, T)
        case i: AadlEventPort => (TypeUtil.EmptyType, T, F)
        case x => halt("Unexpected port type: $x")
      }

      val isIncoming = port.direction == Direction.In

      val dataTypeNames = aadlType.nameProvider

      var objectContributions: ISZ[ST] = ISZ()
      var datatypeContributions: ISZ[ST] = ISZ()
      var requiresContributions: ISZ[ST] = ISZ()
      var ensuresContributions: ISZ[ST] = ISZ()

      integration match {
        case Some(spec) =>
          val portInvariantMethodName = GumboGen.convertToMethodName(spec.id)

          imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(spec.exp)

          var assumeOrGuar: String = "assume"
          spec match {
            case a: GclAssume =>
              // assume integration clauses can only be applied to incoming ports.  The api therefore
              // ensures that the getter's return value will satisfy the assume clause.
              ensuresContributions = ensuresContributions :+ (
                if (isEvent)
                  st"${port.identifier}.isEmpty || ${portInvariantMethodName}(${port.identifier}.get)"
                else
                  st"${portInvariantMethodName}(${port.identifier})")

            case g: GclGuarantee =>
              // guarantee integration clauses can only be applied to outgoing ports.  They become
              // requirements on the param value passed to the api -- the param's name will always be 'value'
              requiresContributions = requiresContributions :+ st"${portInvariantMethodName}(value)"

              assumeOrGuar = "guarantee"
          }

          // will be placed in api so don't use resolved expr
          objectContributions = objectContributions :+
            st"""// $assumeOrGuar ${spec.id}
                |${GumboGen.processDescriptor(spec.descriptor, "//   ")}
                |@strictpure def $portInvariantMethodName(${port.identifier}: ${dataTypeNames.qualifiedReferencedTypeName}): B =
                |  ${getRExp(spec.exp)}"""

          val body: ST =
            if (isEvent) st"${port.identifier}.isEmpty || ${portInvariantMethodName}(${port.identifier}.get)"
            else st"${portInvariantMethodName}(${port.identifier})"

          datatypeContributions = datatypeContributions :+
            st"""@spec def ${port.identifier}_Inv = Invariant(
                |  ${body}
                |)"""
        case _ =>
      }

      ret = ret + (port ~> GclApiContributions(
        objectContributions = objectContributions,
        datatypeContributions = datatypeContributions,
        requiresContributions = requiresContributions,
        ensuresContributions = ensuresContributions))
    }

    return ret
  }
}

