// #Sireum

package org.sireum.hamr.codegen.arsit.gcl

import org.sireum._
import org.sireum.hamr.codegen.arsit.gcl.GumboGen._
import org.sireum.hamr.codegen.arsit.plugin.BehaviorEntryPointProviderPlugin._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
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
                                          val flows: ISZ[ST],
                                          val gumboTables: ISZ[ST]
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
                                val requires: Option[ST],
                                val ensures: ST) extends GclHolder {
    def toST: ST = {
      val reqOpt: Option[ST] =
        if (requires.nonEmpty) Some(st"Requires($requires),")
        else None()
      val ret =
        st"""Case("${caseId}"
            |  ${descriptor}
            |  $reqOpt
            |  Ensures(${ensures})
            |)"""
      return ret;
    }

    def toSTMin: ST = {
      val pred: ST =
        if (requires.nonEmpty) st"($requires) ___>: ($ensures)"
        else ensures
      val ret: ST =
        st"""// case ${caseId}
            |${descriptor}
            |$pred"""
      return ret
    }
  }

  @datatype class GclEntryPointPeriodicCompute(val markers: ISZ[Marker],
                                               val modifies: Option[ST],
                                               val requires: Option[ST],
                                               val ensures: Option[ST],
                                               val flows: Option[ST],
                                               val gumboTables: Option[ST]) extends GclEntryPointContainer

  @datatype class GclEntryPointSporadicCompute(val markers: ISZ[Marker],
                                               val handlers: HashSMap[AadlPort, GclComputeEventHolder]) extends GclEntryPointContainer

  @datatype class GclApiContributions(val apiImportContributions: ISZ[String],
                                      val objectContributions: ISZ[ST],
                                      val datatypeContributions: ISZ[ST],
                                      val requiresContributions: ISZ[ST],
                                      val ensuresContributions: ISZ[ST])

  val FunctionMarker: Marker = Marker("// BEGIN FUNCTIONS", "// END FUNCTIONS")
  val StateVarMarker: Marker = Marker("// BEGIN STATE VARS", "// END STATE VARS")

  val InitializesModifiesMarker: Marker = Marker("// BEGIN INITIALIZES MODIFIES", "// END INITIALIZES MODIFIES")
  val InitializesEnsuresMarker: Marker = Marker("// BEGIN INITIALIZES ENSURES", "// END INITIALIZES ENSURES")
  val InitializesRequiresMarker: Marker = Marker("// BEGIN INITIALIZES REQUIRES", "// END INITIALIZES REQUIRES")
  val InitializesFlowsMarker: Marker = Marker("// BEGIN INITIALIZES FLOWS", "// END INITIALIZES FLOWS")

  var imports: ISZ[String] = ISZ() // singleton global var

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
    val emptyAttr: AST.Attr = AST.Attr(None())
    val emptyRAttr: AST.ResolvedAttr = AST.ResolvedAttr(None(), None(), None())

    def rewriteInvokes(o: AST.Exp): AST.Exp = {
      val ret: AST.Exp = transform_langastExp(o) match {
        case MSome(r) => r
        case _ => o
      }
      return ret
    }

    @pure override def post_langastExpResult(o: AST.Exp.Result): MOption[AST.Exp] = {
      o.attr.typedOpt match {
        case Some(atn: AST.Typed.Name) =>
          val aadlType = aadlTypes.getTypeByPath(atn.ids)

          val splitSlangTypeName = aadlType.nameProvider.qualifiedReferencedTypeNameI

          val name = AST.Name(ids = splitSlangTypeName.map((a: String) => AST.Id(value = a, attr = emptyAttr)), attr = emptyAttr)
          val slangTypedName = AST.Type.Named(name = name, typeArgs = ISZ(), attr = o.attr)

          return MSome(o(tipeOpt = Some(slangTypedName)))
        case _ => return MNone()
      }
    }

    @pure override def post_langastExpInvoke(o: AST.Exp.Invoke): MOption[AST.Exp] = {
      val ret: MOption[AST.Exp] = o.attr.resOpt.get match {
        case arm: AST.ResolvedInfo.Method if arm.mode == AST.MethodMode.Constructor =>
          val receiverOpt: String =
            if (o.receiverOpt.nonEmpty) s"${GumboGenUtil.convertSelects(o.receiverOpt)}::"
            else ""
          val componentName = s"$receiverOpt${o.ident.id.value}"

          if (componentName == "IS") {
            o.targs match {
              case ISZ(i: AST.Type.Named, _) =>
                val indexingTypeName: ISZ[String] = for (id <- i.name.ids) yield id.value
                if (indexingTypeName != ISZ("org", "sireum", "Z")) {

                  val arrayReceiverOpt = GumboGenUtil.convertToSelect(ops.ISZOps(indexingTypeName).dropRight(2))
                  val arrayTypeName = indexingTypeName(indexingTypeName.lastIndex - 1)

                  return MSome(AST.Exp.Invoke(
                    receiverOpt = arrayReceiverOpt,
                    ident = AST.Exp.Ident(AST.Id(arrayTypeName, emptyAttr), emptyRAttr),
                    targs = ISZ(),
                    args = ISZ(o),
                    attr = emptyRAttr))
                } else {
                  halt(s"Need to handle $indexingTypeName")
                }
              case x => halt(s"Need to handle $x")
            }
          } else {
            val path: IdPath = aadlTypes.typeMap.get(componentName) match {
              case Some(t) => t.nameProvider.qualifiedReferencedTypeNameI
              case _ => halt(s"Couldn't find an AADL data component corresponding to '${componentName}''")
            }

            val receiver = GumboGenUtil.convertToSelect(ops.ISZOps(path).dropRight(1))
            val ident = AST.Exp.Ident(id = AST.Id(value = path(path.size - 1), attr = o.ident.id.attr), attr = o.ident.attr)
            MSome(o(receiverOpt = receiver, ident = ident))
          }
        case arm: AST.ResolvedInfo.Method if arm.id == "IS" =>
          val receiverOpt: Option[AST.Exp] = {
            if (o.ident.id.value == "apply") o.receiverOpt
            else if (o.receiverOpt.nonEmpty) Some(AST.Exp.Select(o.receiverOpt, o.ident.id, ISZ(), emptyRAttr))
            else Some(o.ident)
          }
          MSome(o(receiverOpt = receiverOpt, ident = AST.Exp.Ident(AST.Id("value", emptyAttr), emptyRAttr)))
        case _ => MNone()
      }
      return ret
    }

    @pure override def post_langastExpSelect(o: AST.Exp.Select): MOption[AST.Exp] = {

      if (o.id.value == "size") {
        assert (o.receiverOpt.nonEmpty && o.receiverOpt.get.typedOpt.get.asInstanceOf[AST.Typed.Name].ids == ISZ("org", "sireum", "IS"))
        return MSome(AST.Exp.Select(
          receiverOpt = Some(AST.Exp.Select(
            receiverOpt = o.receiverOpt, id = AST.Id("value", emptyAttr), targs = ISZ(), attr = emptyRAttr)),
          id = o.id, targs = ISZ(), attr = emptyRAttr))
      }
      return MNone()
    }
  }

  @pure def getGclAnnexInfos(componentPath: IdPath, symbolTable: SymbolTable): ISZ[GclAnnexClauseInfo] = {
    val annexInfos: ISZ[GclAnnexClauseInfo] = symbolTable.annexClauseInfos.get(componentPath) match {
      case Some(annexInfos) =>
        annexInfos.filter(f => f.isInstanceOf[GclAnnexClauseInfo]).map(m => m.asInstanceOf[GclAnnexClauseInfo])
      case _ => ISZ()
    }
    return annexInfos
  }

  def processGclLibrary(gclLib: GclAnnexLibInfo, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackage: String, store: Store): (ST, ISZ[String]) = {
    val GclAnnexLibInfo(annex, name, gclSymbolTable) = gclLib

    val gg = GumboGen(gclSymbolTable = gclSymbolTable, symbolTable = symbolTable, aadlTypes = aadlTypes, basePackageName = basePackage)
    val methods = annex.methods.map((m: GclMethod) => gg.processGclMethod(m, store))

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

  def processInitializes(m: AadlThreadOrDevice, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackage: String, store: Store): Option[GclEntryPointInitialize] = {
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
        var gumboTables: ISZ[ST] = ISZ()
        var markers: ISZ[Marker] = ISZ()

        val inits: ISZ[ST] = sc.initializes.get.guarantees.map((m: GclGuarantee) => {
          imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(m.exp, basePackage, GclResolver.getIndexingTypeFingerprints(store))
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

        return Some(GclEntryPointInitialize(imports, markers, ret, modifies, requires, ensures, flows,gumboTables))
      } else {
        return None()
      }
    } else {
      return None()
    }
  }

  def processInvariants(e: AadlType, symbolTable: SymbolTable, aadlTypes: AadlTypes, basePackageName: String, store: Store): ISZ[ST] = {
    resetImports()
    var ret: ISZ[ST] = ISZ()

    val FIXME = ISZ(e.name)
    val ais = getGclAnnexInfos(FIXME, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to a data component")

    for (ai <- ais) {
      val sc = ai.annex
      val gclSymTable = ai.gclSymbolTable
      val gg = GumboGen(gclSymTable, symbolTable, aadlTypes, basePackageName)
      ret = ret ++ gg.processInvariants(sc.invariants, store)
      addImports(gg)
    }

    e match {
      case a: ArrayType if a.kind == ArraySizeKind.Fixed =>
        assert (a.dimensions.size == 1, "Only expecting single dimension arrays for Slang")
        ret = ret :+ st"""@spec def __fixedArraySizeInvariant = Invariant(value.size == ${a.dimensions(0)})"""
      case _ =>
    }

    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 symbolTable: SymbolTable,
                                 aadlTypes: AadlTypes,
                                 basePackageName: String,
                                 store: Store): Map[AadlPort, GclApiContributions] = {
    resetImports()
    val ais = getGclAnnexInfos(m.path, symbolTable)
    assert(ais.size <= 1, "Can't attach more than 1 subclause to an AADL thread")

    if (ais.nonEmpty) {
      val sc = ais(0).annex
      val gclSymbolTable = ais(0).gclSymbolTable

      val ret: Map[AadlPort, GclApiContributions] = {
        if (gclSymbolTable.apiReferences.nonEmpty || gclSymbolTable.integrationMap.nonEmpty) {
          val gg = GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          val _contracts = gg.processIntegrationContract(m, gclSymbolTable, store)
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

  def processSubclauseFunctions(gclMethods: ISZ[GclMethod],
                                gclSymbolTable: GclSymbolTable,
                                symbolTable: SymbolTable,
                                aadlTypes: AadlTypes,
                                basePackageName: String,
                                store: Store): (ST, Marker, ISZ[String]) = {
    val g = GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName)
    val r = g.processSubclauseFunctions(gclMethods, store)
    return (r._1, r._2, g.imports)
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

  def gumboTableGetGclEnsuresHolderDeterministicCheck(id:String, sets: ISZ[ISZ[AST.Exp]]): ISZ[GclEnsuresHolder] = {
    var i: Z = 0
    var ret: ISZ[GclEnsuresHolder] = ISZ()
    for(set <- sets){
      var deterministicST: ST = st""
      val gclehId: String = s"Deterministic Check: Set ${i}"
      if(set.length == 1){
        val p = set(0)
        val x = gclSymbolTable.rexprs.get(toKey(p)).get
        deterministicST = st"${deterministicST} (${x})"
      }
      else{
        for(a <- 0 until set.length){
          for(b <- (a+1) until set.length){
            val p1 = set(a)
            val p2 = set(b)
            val x = gclSymbolTable.rexprs.get(toKey(p1)).get
            val y = gclSymbolTable.rexprs.get(toKey(p2)).get
            if(!(a == set.length-2 & b == set.length-1)){
              deterministicST = st"${deterministicST}((${x}) |^ (${y})) & \n"
            }
            else{
              deterministicST = st"${deterministicST}((${x}) |^ (${y}))"
            }
          }
        }
      }
      ret = ret :+ GclEnsuresHolder(id,processDescriptor(Some(gclehId),"//   "),deterministicST)
      i = i + 1
    }
    return ret
  }

  def gumboTableGetGclEnsuresHolderDeterministicCheckCaseTable(id:String, sets: ISZ[ISZ[AST.Exp]], caseExps: ISZ[AST.Exp], caseEval: AST.Exp): ISZ[GclEnsuresHolder] = {
    var i: Z = 0
    var ret: ISZ[GclEnsuresHolder] = ISZ()
    if(caseExps.nonEmpty){
      var deterministicST: ST = st""
      val gclehId: String = s"Deterministic Check: Set ${i}"
      if(caseExps.length == 1){
        val p = caseExps(0)
        val x = gclSymbolTable.rexprs.get(toKey(p)).get
        deterministicST = st"${deterministicST} (${x})"
      }
      else{
        for(a <- 0 until caseExps.length){
          for(b <- (a+1) until caseExps.length){
            val p1 = caseExps(a)
            val p2 = caseExps(b)
            val x = gclSymbolTable.rexprs.get(toKey(p1)).get
            val y = gclSymbolTable.rexprs.get(toKey(p2)).get
            val ce = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(caseEval)).get)
            val xr: ST = st"(${ce}) == (${x})"
            val yr: ST = st"(${ce}) == (${y})"
            if(!(a == caseExps.length-2 & b == caseExps.length-1)){
              deterministicST = st"${deterministicST}((${xr}) |^ (${yr})) & \n"
            }
            else{
              deterministicST = st"${deterministicST}((${xr}) |^ (${yr}))"
            }
          }
        }
      }
      ret = ret :+ GclEnsuresHolder(id,processDescriptor(Some(gclehId),"//   "),deterministicST)
      i = i + 1
    }
    for(set <- sets){
      var deterministicST: ST = st""
      val gclehId: String = s"Deterministic Check: Set ${i}"
      if(set.length == 1){
        val p = set(0)
        val x = gclSymbolTable.rexprs.get(toKey(p)).get
        deterministicST = st"${deterministicST} (${x})"
      }
      else{
        for(a <- 0 until set.length){
          for(b <- (a+1) until set.length){
            val p1 = set(a)
            val p2 = set(b)
            val x = gclSymbolTable.rexprs.get(toKey(p1)).get
            val y = gclSymbolTable.rexprs.get(toKey(p2)).get
            if(!(a == set.length-2 & b == set.length-1)){
              deterministicST = st"${deterministicST}((${x}) |^ (${y})) & \n"
            }
            else{
              deterministicST = st"${deterministicST}((${x}) |^ (${y}))"
            }
          }
        }
      }
      ret = ret :+ GclEnsuresHolder(id,processDescriptor(Some(gclehId),"//   "),deterministicST)
      i = i + 1
    }
    return ret
  }

  def gumboTableGetGclEnsuresHolderCasesCaseTable(id: String, cases: ISZ[ISZ[AST.Exp]], resultRows: ISZ[GclResultRow], caseEval: AST.Exp): ISZ[GclEnsuresHolder] = {
    var i: Z = 0
    var ret: ISZ[GclEnsuresHolder] = ISZ()
    var x: Z = 0
    var y: Z = 0
    val cer = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(caseEval)).get)
    for (rr <- resultRows) {
      for (r <- rr.results) {
        val re = gclSymbolTable.rexprs.get(toKey(r)).get
        val c: ISZ[AST.Exp] = cases(i)
        val cr = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(c(0))).get)
        var ensures: ST = st"((${cer}) == (${cr})) & "
        for (a <- 1 until c.length) {
          val ca = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(c(a))).get)
          ensures = st"${ensures}(${ca})"
          if (a != c.length - 1) {
            ensures = st"${ensures} & "
          }
        }
        ensures = st"(${ensures}) -->: (${re})"
        ret = ret :+ GclEnsuresHolder(id, processDescriptor(Some(s"(${x},${y})"), "//   "), ensures)
        i = i + 1
        x = x + 1
      }
      x = 0
      y = y + 1
    }
    return ret
  }

  def gumboTableGetGclEnsuresHolderCases(id: String, cases: ISZ[ISZ[AST.Exp]], resultRows: ISZ[GclResultRow]): ISZ[GclEnsuresHolder] = {
    var i: Z = 0
    var ret: ISZ[GclEnsuresHolder] = ISZ()
    var x: Z = 0
    var y: Z = 0
    for(rr <- resultRows){
      for(r <- rr.results){
        val re = gclSymbolTable.rexprs.get(toKey(r)).get
        val c: ISZ[AST.Exp] = cases(i)
        var ensures: ST = st""
        for(a <- 0 until c.length){
          val ca = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(c(a))).get)
          ensures = st"${ensures}(${ca})"
          if(a != c.length-1){
            ensures = st"${ensures} & "
          }
        }
        /*
        for(a <- 0 until c.length){
          for(b <- (a+1) until c.length){
            val ca = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(c(a))).get)
            val cb = GumboGen.StateVarInRewriter().wrapStateVarsInInput(gclSymbolTable.rexprs.get(toKey(c(b))).get)
            ensures = st"${ensures}((${ca}) & (${cb})) "
            if(!(a == c.length-2 & b == c.length-1)){
              ensures = st"${ensures}& "
            }
          }
        }
         */
        ensures = st"(${ensures}) -->: (${re})"
        ret = ret :+ GclEnsuresHolder(id,processDescriptor(Some(s"(${x},${y})"),"//   "),ensures)
        i = i + 1
        x = x + 1
      }
      x = 0
      y = y + 1
    }
    return ret
  /*
  //generalHolder :+ GclEnsuresHolder(nt.id,Some(st"Deterministic"),st"(${("(",vps,") |^ ")})) ^ (${("(",hps,") |^ ")}))")

          // for each row (y position, vertical predicate (v), etc)
          var deterministicST: ST = st""
          for(ve <- vps){
            // get in the form I need.
            println(ve.posOpt)
            println(ve.prettyST.render)

            val v = gclSymbolTable.rexprs.get(toKey(ve)).get
            // results on this row.
            val row: ISZ[AST.Exp] = rrs(y).results
            // for each column (x position, horizontal predicate (h), etc)
            for(he <- hps){
              // get in the form I need.
              val h = gclSymbolTable.rexprs.get(toKey(he)).get
              // get the result at this position "x","y" (in the form I need)
              val r = gclSymbolTable.rexprs.get(toKey(row(x))).get
              // construct the ensures elem.
              //  The general holder is where this *should* be placed, it is filtered and combined
              //  with anyone
              val rv = GumboGen.StateVarInRewriter().wrapStateVarsInInput(v)
              val rh = GumboGen.StateVarInRewriter().wrapStateVarsInInput(h)
              generalHolder = generalHolder :+
                GclEnsuresHolder(nt.id,processDescriptor(Some(s"(${x},${y})"),"//   "),st"((${rv}) & (${rh})) -->: (${r})") //previously multiline, not needed.
              // we are going to the next column.
              x += 1
            }

            // go back to the first column.
            x = 0
            // we are going to the next row.
            y += 1
          }
          var a: Z = 0
          var b: Z = 0
          for(ve1 <- vps){
            val v1 = gclSymbolTable.rexprs.get(toKey(ve1)).get
            for(ve2 <- vps){
              val v2 = gclSymbolTable.rexprs.get(toKey(ve2)).get
              if(b<a){
                deterministicST = st"${deterministicST}(${v1} |^ ${v2}) ^ "
              }
              b = b + 1
            }
            b = 0
            a = a + 1
          }
          a = 0
          b = 0
          for(he1 <- hps){
            val h1 = gclSymbolTable.rexprs.get(toKey(he1)).get
            for(he2 <- hps){
              val h2 = gclSymbolTable.rexprs.get(toKey(he2)).get
              if(b<a){
                if(a < hps.length - 1 & b < hps.length - 2) {
                  deterministicST = st"${deterministicST}(${h1} |^ ${h2}) ^ "
                }
                else{
                  deterministicST = st"${deterministicST}(${h1} |^ ${h2})"
                }
              }
              b = b + 1
            }
            b = 0
            a = a + 1
          }
          //if(a < vps.length - 1 & b < vps.length - 2){
          generalHolder =
            GclEnsuresHolder(nt.id,processDescriptor(Some(s"Deterministic Check"),"//   "),deterministicST) +:
              generalHolder
   */
  }

  def processCompute(compute: GclCompute, optInEvent: Option[AadlPort], context: AadlThreadOrDevice, store: Store): (ContractBlock, ISZ[Marker]) = {
    resetImports()

    var markers: Set[Marker] = Set.empty
    var rreads: ISZ[ST] = ISZ()
    var rrequires: ISZ[ST] = ISZ()
    var rmodifies: ISZ[ST] = ISZ()
    var rensures: ISZ[ST] = ISZ()
    var rflows: ISZ[ST] = ISZ()
    var rgumboTables: ISZ[ST] = ISZ()

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

    for (assumee <- compute.assumes) {
      val rspec = gclSymbolTable.rexprs.get(toKey(assumee.exp)).get
      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec, basePackageName, GclResolver.getIndexingTypeFingerprints(store))

      val descriptor = GumboGen.processDescriptor(assumee.descriptor, "//   ")
      val rassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rspec)
      generalHolder = generalHolder :+ GclRequiresHolder(assumee.id, descriptor, st"$rassume")
    }

    for (guarantee <- compute.guarantees) {
      val rspec = gclSymbolTable.rexprs.get(toKey(guarantee.exp)).get
      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rspec, basePackageName, GclResolver.getIndexingTypeFingerprints(store))

      val descriptor = GumboGen.processDescriptor(guarantee.descriptor, "//   ")
      generalHolder = generalHolder :+ GclEnsuresHolder(guarantee.id, descriptor, st"$rspec")
    }

    if (compute.cases.nonEmpty) {
      // fill in general case
      for (generalCase <- compute.cases) {

        val rrassume: Option[ST] =
          generalCase.assumes match {
            case Some(assumes) =>
              val rexp = gclSymbolTable.rexprs.get(toKey(assumes)).get
              val rrassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rexp)
              imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rrassume, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
              Some(rrassume.prettyST)
            case _ => None()
          }

        val rguarantee = gclSymbolTable.rexprs.get(toKey(generalCase.guarantees)).get
        imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rguarantee, basePackageName, GclResolver.getIndexingTypeFingerprints(store))

        generalHolder = generalHolder :+ GclCaseHolder(
          caseId = generalCase.id,
          descriptor = GumboGen.processDescriptor(generalCase.descriptor, "//   "),
          requires = rrassume,
          ensures = rguarantee.prettyST)
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

    //SIERRA BEGIN

    //for each table
    if(compute.gumboTables.nonEmpty){
      for(t <- compute.gumboTables){
        // TODO match statement on t (Not doing it yet since we only have normal tables)
        //  normal table
        if(t.normal.nonEmpty){//NORMAL TABLE
          // get normal table
          val nt: GclNormalTable = t.normal.get
          // horizontal preds
          val hps: ISZ[AST.Exp] = nt.horizontalPredicates
          //vertical preds
          val vps: ISZ[AST.Exp] = nt.verticalPredicates
          // result rows
          val rrs: ISZ[GclResultRow] = nt.resultRows
          //
          // where I am in the table. traversal x is horizontal, y is vertical position.
          var x,y: Z = 0

          var sets: ISZ[ISZ[AST.Exp]] = ISZ(hps) :+ vps

          var cases: ISZ[ISZ[AST.Exp]] = ISZ()

          for(vp <- vps){
            for(hp <- hps){
              cases = cases :+ ISZ(vp,hp)
            }
          }

          val determinismHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderDeterministicCheck(nt.id,sets)
          val casesHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderCases(nt.id,cases,rrs)

          for ( h <- determinismHolders){
            generalHolder = generalHolder :+ h
          }
          for (h <- casesHolders){
            generalHolder = generalHolder :+ h
          }
          //  TODO end of normal table
        }
        else if(t.cases.nonEmpty){
          val ct: GclCaseTable = t.cases.get
          // horizontal preds
          val hps: ISZ[AST.Exp] = ct.horizontalPredicates
          //vertical predicate rows
          val vprs: ISZ[GclBlankRow] = ct.verticalPredicateRows
          // result rows
          val rrs: ISZ[GclResultRow] = ct.resultRows
          // vertical predicate row length
          val vrl: Z = vprs(0).results.length

          var lastRowLength: Z = -1
          var lastRowStartColumn: Z = -1

          // BUILD CASES TABLE SETS
          var sets: ISZ[ISZ[AST.Exp]] = ISZ(hps)
          var activeSet: ISZ[AST.Exp] = ISZ()
          var caseExps: ISZ[AST.Exp] = ISZ()

          for(vpr <- vprs){
            if(vpr.results.length == 2){
              val vp = vpr.results(1)
              caseExps = caseExps :+ vpr.results(0)
              if(activeSet.length > 0){
                sets = sets :+ activeSet
              }
              activeSet = ISZ(vp)
            }
            else{
              val vp = vpr.results(0)
              activeSet = activeSet :+ vp
            }
          }
          // finish building sets

          // BUILD CASES TABLE CASES
          // vpc: Vertical Predicate Cases (left hand side of implies)
          var cases: ISZ[ISZ[AST.Exp]] = ISZ()
          var active: ISZ[AST.Exp] = ISZ()

          for(vpr <- vprs){
            if(vpr.blanks.length > 0){ //if we have blanks
              for(i <- 0 until vpr.results.length){//for each non-blank
                active = ops.ISZOps(active).dropRight(1)//remove an expression to make room.
              }
              for(r <- vpr.results){//then for each result
                active = active :+ r//add it to the active case
              }
            }
            else{//if there is no blanks the case is just results
              active = vpr.results
            }
            if(active.length != vrl){//DEBUG
              print("Error Sierra")
            }
            for(hp <- hps){//for each vertical predicate
              cases = cases :+ (active:+hp)//add a case where
            }
          }
          // finished building cases

          val determinismHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderDeterministicCheckCaseTable(ct.id,sets,caseExps,ct.caseEval)
          val casesHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderCasesCaseTable(ct.id,cases,rrs,ct.caseEval)

          for ( h <- determinismHolders){
            generalHolder = generalHolder :+ h
          }
          for (h <- casesHolders){
            generalHolder = generalHolder :+ h
          }
        }
        else if(t.nested.nonEmpty){
          // get nested table
          val nt: GclNestedTable = t.nested.get
          // horizontal preds
          val hps: ISZ[AST.Exp] = nt.horizontalPredicates
          //vertical predicate rows
          val vprs: ISZ[GclBlankRow] = nt.verticalPredicateRows
          // result rows
          val rrs: ISZ[GclResultRow] = nt.resultRows
          // vertical predicate row length
          val vrl: Z = vprs(0).results.length //GCLResolver promises there are no blanks in the first row and all rows are this length (when considering blanks and exps)

          // BUILD NESTED TABLE CASES
          // vpc: Vertical Predicate Cases (left hand side of implies)
          var cases: ISZ[ISZ[AST.Exp]] = ISZ()
          var active: ISZ[AST.Exp] = ISZ()

          for(vpr <- vprs){
            if(vpr.blanks.length > 0){ //if we have blanks
              for(i <- 0 until vpr.results.length){//for each non-blank
                active = ops.ISZOps(active).dropRight(1)//remove an expression to make room.
              }
              for(r <- vpr.results){//then for each result
                active = active :+ r//add it to the active case
              }
            }
            else{//if there is no blanks the case is just results
              active = vpr.results
            }
            if(active.length != vrl){//DEBUG
              print("Error Sierra")
            }
            for(hp <- hps){//for each vertical predicate
              cases = cases :+ (active:+hp)//add a case where
            }
          }
          // finished building cases

          // BUILD NESTED TABLE SETS
          var sets: ISZ[ISZ[AST.Exp]] = ISZ(hps)
          var activeSets: MSZ[ISZ[AST.Exp]] = MSZ()
          for(i <- 0 until vrl){ // add enough sets to where there is one for each column.
            activeSets = activeSets :+ ISZ()
          }
          var lastRowLength: Z = -1
          var lastRowStartColumn: Z = -1

          for(vpr <- vprs){ // for each row
            var i: Z = 0 // reset i, this is the first pred for the row.
            for(vp <- vpr.results){ // for each pred in row
              val column: Z = vrl - vpr.results.length + i // the column
              //if(curRowStartColumn < lastRowStartColumn){ // if this row is longer than the prior, the column it starts on is smaller.
                if(i==0){ // if it is the first item in this row
                  //then its still in the active set for the column.
                  activeSets(column) = activeSets(column) :+ vp
                }
                else{// if it is not the first item in the row
                  //then this is the first item in a new set for the column.
                  if(activeSets(column).length > 0){ (sets = sets :+ activeSets(column)) } // add the completed set to the list of sets.
                  activeSets(column) = ISZ(vp) // the active set at this column is a new set with the current exp in it.
                }
              //}
              i = i + 1
            }
          }
          for(activeSet <- activeSets){
            sets = sets :+ activeSet
          }
          // finish building sets

          val determinismHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderDeterministicCheck(nt.id,sets)
          val casesHolders: ISZ[GclEnsuresHolder] = gumboTableGetGclEnsuresHolderCases(nt.id,cases,rrs)

          for ( h <- determinismHolders){
            generalHolder = generalHolder :+ h
          }
          for (h <- casesHolders){
            generalHolder = generalHolder :+ h
          }

          //while(i<vprs.length){
            //if(vprs(i).blanks.length)
          //}
        }
        // TODO end of hypothetical match statement
      }
    }
    //TODO The enuresTables GclEnsuresHolder(s) need to be placed into the genralEnsures?? Below, when the contract is made.
    //SIERRA END

    val generalRequires: ISZ[GclRequiresHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclRequiresHolder]).map((m: GclHolder) => m.asInstanceOf[GclRequiresHolder])
    val generalEnsures: ISZ[GclEnsuresHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclEnsuresHolder]).map((m: GclHolder) => m.asInstanceOf[GclEnsuresHolder])
    val generalCases: ISZ[GclCaseHolder] = generalHolder.filter((p: GclHolder) => p.isInstanceOf[GclCaseHolder]).map((m: GclHolder) => m.asInstanceOf[GclCaseHolder])

    if (context.isSporadic()) {
      if (optInEvent.nonEmpty) {

        val eventPort = optInEvent.get

        val handlerRequires: ISZ[GclRequiresHolder] = generalRequires

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

            if (handlerRequires.nonEmpty || handler.assumes.nonEmpty) {
              val marker = genComputeMarkerCreator(eventPort.identifier, "REQUIRES")
              val handlerRequiresST: ISZ[ST] =
                handlerRequires.map((m: GclRequiresHolder) => m.toSTMin) ++
                  handler.assumes.map((g: GclAssume) => {
                    val rexp = gclSymbolTable.rexprs.get(toKey(g.exp)).get
                    val rassume = GumboGen.StateVarInRewriter().wrapStateVarsInInput(rexp)
                    imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rassume, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
                    st"""// assumes ${g.id}
                        |${GumboGen.processDescriptor(g.descriptor, "//   ")}
                        |${rassume}"""
                  })

              rrequires = rrequires :+
                st"""${marker.beginMarker}
                    |${(handlerRequiresST, ",\n")}
                    |${marker.endMarker}"""
            }

            if (generalEnsures.nonEmpty || handler.guarantees.nonEmpty || generalCases.nonEmpty) {
              val generalElems = generalEnsures.map((m: GclEnsuresHolder) => m.toSTMin)
              val _cases = generalCases.map((m: GclCaseHolder) => m.toSTMin)

              val handlerEnsuresST: ISZ[ST] = generalElems ++ _cases ++
                handler.guarantees.map((g: GclGuarantee) => {
                  val rexp = gclSymbolTable.rexprs.get(toKey(g.exp)).get
                  imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
                  st"""// guarantees ${g.id}
                      |${GumboGen.processDescriptor(g.descriptor, "//   ")}
                      |${rexp}"""
                })

              val marker = genComputeMarkerCreator(eventPort.identifier, "ENSURES")

              rensures = rensures :+
                st"""${marker.beginMarker}
                    |${(handlerEnsuresST, ",\n")}
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

  def processGclMethod(gclMethod: GclMethod, store: Store): ST = {
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
      s"${p.id.value}: ${aadlType.qualifiedReferencedTypeName}"
    })

    val rexp: AST.Exp = gclMethod.method.bodyOpt match {
      case Some(AST.Body(ISZ(AST.Stmt.Return(Some(exp))))) => getRExp(exp)
      case _ => halt("Unexpected: should be a return statement containing a single expression")
    }

    imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(rexp, basePackageName, GclResolver.getIndexingTypeFingerprints(store))

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
          else Some(st"Requires(${(scontract.requires.map((e: AST.Exp) => {
            val r = getRExp(e)
            imports = GumboGen.imports ++ GumboGenUtil.resolveLitInterpolateImports(r, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
            r
          }), ",")}),")

        val modifiesOpt: Option[ST] =
          if (scontract.modifies.isEmpty) None()
          else Some(st"Modifies(${(scontract.modifies.map((i: AST.Exp.Ref) => getR2Exp(i)), ",")}),")

        val ensuresOpt: Option[ST] =
          if (scontract.ensures.isEmpty) None()
          else Some(st"Ensures(${(scontract.ensures.map((e: AST.Exp) => {
            val r = getRExp(e)
            imports = GumboGen.imports ++ GumboGenUtil.resolveLitInterpolateImports(r, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
            r
          }), ",")})")

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

  def processSubclauseFunctions(methods: ISZ[GclMethod], store: Store): (ST, Marker) = {

    val sts: ISZ[ST] = methods.map((m: GclMethod) => processGclMethod(m, store))

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

  def processInvariants(invariants: ISZ[GclInvariant], store: Store): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    for (i <- invariants) {
      val methodName = GumboGen.convertToMethodName(i.id)

      imports = imports ++ GumboGenUtil.resolveLitInterpolateImports(i.exp, basePackageName, GclResolver.getIndexingTypeFingerprints(store))

      // will be placed in data type def so use resolved exp
      ret = ret :+
        st"""@spec def ${methodName} = Invariant(
            |  ${getRExp(i.exp)}
            |)"""
    }
    return ret
  }

  def processIntegrationContract(m: AadlThreadOrDevice,
                                 gclSymTable: GclSymbolTable,
                                 store: Store): Map[AadlPort, GclApiContributions] = {
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

      var apiImports: ISZ[String] = ISZ()

      integration match {
        case Some(spec) =>
          val portInvariantMethodName = GumboGen.convertToMethodName(spec.id)

          apiImports = apiImports ++ GumboGenUtil.resolveLitInterpolateImports(spec.exp, basePackageName, GclResolver.getIndexingTypeFingerprints(store))
          imports = imports ++ apiImports

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
        apiImportContributions = apiImports,
        objectContributions = objectContributions,
        datatypeContributions = datatypeContributions,
        requiresContributions = requiresContributions,
        ensuresContributions = ensuresContributions))
    }

    return ret
  }
}

