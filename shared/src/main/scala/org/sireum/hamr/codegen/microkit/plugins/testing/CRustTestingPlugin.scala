// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.testing

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.component.CRustComponentPlugin
import org.sireum.hamr.codegen.microkit.plugins.linters.{MicrokitLinterPlugin, TouchedTypes}
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypePlugin, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object CRustTestingPlugin {
  val KEY_CrustTestingPlugin: String = "KEY_CeustTestingPlugin"

  @strictpure def getCRustTestingContributions(store: Store): Option[CRustTestingContributions] = store.get(KEY_CrustTestingPlugin).asInstanceOf[Option[CRustTestingContributions]]

  @strictpure def putCRustTestingContributions(contributions: CRustTestingContributions, store: Store): Store = store + KEY_CrustTestingPlugin ~> contributions


  @datatype class CRustTestingContributions(val testingContributions: Map[IdPath, CRustTestingContribution]) extends StoreValue

  @datatype class CRustTestingContribution(// items for src/test/mod.rs
                                           val modEntries: ISZ[RAST.Item],

                                           // items for src/test/util/mod.rs
                                           val utilModEntries: ISZ[RAST.Item],

                                           // items for src/test/tests.rs
                                           val testsEntries: ISZ[RAST.Item],

                                           // items for src/test/util/proptest_generators.rs
                                           val proptestEntries: ISZ[RAST.Item],

                                           // items for src/test/util/test_apis.rs
                                           val testApiEntries: ISZ[RAST.Item])
}

@sig trait CRustTestingPlugin extends MicrokitPlugin with MicrokitFinalizePlugin {

  @strictpure def haveHandled(store: Store): B = CRustTestingPlugin.getCRustTestingContributions(store).nonEmpty

  @strictpure def haveFinalized(store: Store): B = store.contains(s"FINALIZED_$name")

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CRustTypePlugin.hasCRustTypeProvider(store) &&
      !haveHandled(store)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        !isDisabled(store) &&
        haveHandled(store) &&
        !haveFinalized(store))
  }

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    var ret: Map[IdPath, CRustTestingPlugin.CRustTestingContribution] = Map.empty

    for (thread <- symbolTable.getThreads() if MicrokitUtil.isRusty(thread)) {
      val threadId = MicrokitUtil.getComponentIdPath(thread)

      val crustTypeProvider = CRustTypePlugin.getCRustTypeProvider(localStore).get

      val testEntries = genTestEntries(thread, crustTypeProvider)

      val testApiEntries =
        genTestApiConcreteInputsEntries(thread, crustTypeProvider) ++
          genTestApiGetterSettersEntries(thread, crustTypeProvider)

      val touchedTypes = MicrokitLinterPlugin.getTouchedTypes(localStore)
      val proptestGenerators = genProptestGenerators(touchedTypes, thread, crustTypeProvider, model, options, types, symbolTable, store, reporter)

      ret = ret + thread.path ~> CRustTestingPlugin.CRustTestingContribution(
        modEntries = ISZ(
          RAST.Mod(RAST.Visibility.Public, RAST.IdentString("util")),
          RAST.Mod(RAST.Visibility.Public, RAST.IdentString("tests"))
        ),
        utilModEntries = ISZ(
          RAST.Mod(RAST.Visibility.Public, RAST.IdentString("test_apis")),
        ),
        testsEntries = testEntries,
        proptestEntries = proptestGenerators,
        testApiEntries = testApiEntries)
    }

    return (CRustTestingPlugin.putCRustTestingContributions(CRustTestingPlugin.CRustTestingContributions(ret), localStore), resources)
  }

  @pure def genTestApiGetterSettersEntries(thread: AadlThread, crustTypeProvider: CRustTypeProvider): ISZ[RAST.Item] = {
    var ret: ISZ[RAST.Item] = ISZ()
    for (p <- thread.getPorts()) {
      val portType: AadlType = crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(p))
      val portTypeNameProvider = crustTypeProvider.getTypeNameProvider(portType)

      val paramKind: String =
        p match {
          case a:AadlDataPort => "DataPort"
          case a:AadlEventDataPort => "EventDataPort"
          case a:AadlEventPort => "EventPort"
        }

      if (p.direction == Direction.In) {
        val varName = s"IN_${p.identifier}"

        val paramType: ISZ[ISZ[String]] = if(p.isEvent) {
          ISZ(ISZ("Option"), portTypeNameProvider.qualifiedRustNameS)
        } else {
          ISZ(portTypeNameProvider.qualifiedRustNameS)
        }

        val testApiBody: ST =
          if (p.isEvent) {
            st"*extern_api::$varName.lock().unwrap_or_else(|e| e.into_inner()) = value"
          } else {
            st"*extern_api::$varName.lock().unwrap_or_else(|e| e.into_inner()) = Some(value)"
          }

        ret = ret :+ RAST.FnImpl(
          attributes = ISZ(),
          visibility = RAST.Visibility.Public,
          sig = RAST.FnSig(
            ident = RAST.IdentString(s"put_${p.identifier}"),
            fnDecl = RAST.FnDecl(
              inputs = ISZ(RAST.ParamImpl(
                ident = RAST.IdentString("value"),
                kind = RAST.TyPath(items = paramType, aadlType = Some(portType.classifier)))),
              outputs = RAST.FnRetTyDefault()),
            verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
          comments = ISZ(RAST.CommentRustDoc(ISZ(st"setter for IN $paramKind"))),
          contract = None(), meta = ISZ(),
          body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(testApiBody)))))
      }
      else {
        val varName = s"OUT_${p.identifier}"

        val (testApiBody, retType): (ST, RAST.Ty) = {
          if (p.isEvent) {
            (st"return extern_api::$varName.lock().unwrap_or_else(|e| e.into_inner()).clone()", RAST.TyPath(items = ISZ(ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), aadlType = Some(portType.classifier)))
          } else {
            (st"""return extern_api::$varName.lock().unwrap_or_else(|e| e.into_inner()).expect("Not expecting None")""", RAST.TyPath(items = ISZ(portTypeNameProvider.qualifiedRustNameS), aadlType = Some(portType.classifier)))
          }
        }

        ret = ret :+ RAST.FnImpl(
          attributes = ISZ(),
          visibility = RAST.Visibility.Public,
          sig = RAST.FnSig(
            ident = RAST.IdentString(s"get_${p.identifier}"),
            fnDecl = RAST.FnDecl(
              inputs = ISZ(),
              outputs = RAST.FnRetTyImpl(retType)),
            verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
          comments = ISZ(RAST.CommentRustDoc(ISZ(st"getter for OUT $paramKind"))),
          contract = None(), meta = ISZ(),
          body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(testApiBody)))))
      }
    }

    return ret
  }

  @pure def genTestApiConcreteInputsEntries(thread: AadlThread, crustTypeProvider: CRustTypeProvider): ISZ[RAST.Item] = {
    var params: ISZ[RAST.Param] = ISZ()
    var structItems: ISZ[RAST.Item] = ISZ()
    var bodyItems: ISZ[ST] = ISZ()
    var containerBodyItems: ISZ[ST] = ISZ()
    for (inPort <- thread.getPorts().filter(p => p.direction == Direction.In)) {
      val portType: AadlType = crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(inPort))
      val portTypeNameProvider = crustTypeProvider.getTypeNameProvider(portType)

      val paramType: ISZ[ISZ[String]] = if(inPort.isEvent) {
        ISZ(ISZ("Option"), portTypeNameProvider.qualifiedRustNameS)
      } else {
        ISZ(portTypeNameProvider.qualifiedRustNameS)
      }

      params = params :+ RAST.ParamImpl(
        ident = RAST.IdentString(inPort.identifier),
        kind = RAST.TyPath(items = paramType, aadlType = None()))

      bodyItems = bodyItems :+ st"put_${inPort.identifier}(${inPort.identifier});"
      containerBodyItems = containerBodyItems :+ st"put_${inPort.identifier}(container.api_${inPort.identifier});"

      structItems = structItems :+ RAST.StructField(
        visibility = RAST.Visibility.Public,
        isGhost = F,
        ident = RAST.IdentString(s"api_${inPort.identifier}"),
        fieldType = RAST.TyPath(items = paramType, aadlType = None()))
    }

    val struct = RAST.StructDef(
      comments = ISZ(RAST.CommentRustDoc(ISZ(st"container for component's incoming port values"))),
      attributes = ISZ(),
      visibility = RAST.Visibility.Public,
      ident = RAST.IdentString("PreStateContainer"),
      items = structItems)

    val containerPutter =  RAST.FnImpl(
      comments = ISZ(RAST.CommentRustDoc(ISZ(st"setter for component's incoming port values"))),
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString("put_concrete_inputs_container"),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(RAST.ParamImpl(
            ident = RAST.IdentString("container"),
            kind = RAST.TyPath(items = ISZ(ISZ("PreStateContainer")), aadlType = None()))),
          outputs = RAST.FnRetTyDefault()),
        fnHeader = RAST.FnHeader(F), verusHeader = None(), generics = None()),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(st"${(containerBodyItems, "\n")}")))),
      meta = ISZ(), contract = None(), attributes = ISZ())

    val putter =  RAST.FnImpl(
      comments = ISZ(RAST.CommentRustDoc(ISZ(st"setter for component's incoming port values"))),
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString("put_concrete_inputs"),
        fnDecl = RAST.FnDecl(
          inputs = params,
          outputs = RAST.FnRetTyDefault()),
        fnHeader = RAST.FnHeader(F), verusHeader = None(), generics = None()),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(st"${(bodyItems, "\n")}")))),
      meta = ISZ(), contract = None(), attributes = ISZ())

    return ISZ(struct, containerPutter, putter)
  }

  @pure def propTestOptionMethod(): ISZ[RAST.Item] = {
    var generics: ISZ[RAST.GenericParam] = ISZ()
    generics = generics :+ RAST.GenericParam(
      ident = RAST.IdentString("T"),
      attributes = ISZ(),
      bounds = RAST.GenericBoundFixMe(st"Clone + std::fmt::Debug"))
    generics = generics :+ RAST.GenericParam(
      ident = RAST.IdentString("S"),
      attributes = ISZ(),
      bounds = RAST.GenericBoundFixMe(st" Strategy<Value = T>"))

    val defaultStrategy = RAST.FnImpl(
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString("option_strategy_default"),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(
            RAST.ParamImpl(
              ident = RAST.IdentString("base"),
              kind = RAST.TyPath(ISZ(ISZ("S")), None()))),
          outputs = RAST.FnRetTyImpl(
            RAST.TyFixMe(st"impl Strategy<Value = Option<T>>"))),
        generics = Some(RAST.Generics(generics)),
        fnHeader = RAST.FnHeader(F), verusHeader = None()),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
        st"""option_strategy_bias(1, base)""")))),
      meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

    val custStrategy = RAST.FnImpl(
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString("option_strategy_bias"),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(
            RAST.ParamImpl(
              ident = RAST.IdentString("bias"),
              kind = RAST.TyPath(ISZ(ISZ("u32")), None())),
            RAST.ParamImpl(
              ident = RAST.IdentString("base"),
              kind = RAST.TyPath(ISZ(ISZ("S")), None()))),
          outputs = RAST.FnRetTyImpl(
            RAST.TyFixMe(st"impl Strategy<Value = Option<T>>"))),
        generics = Some(RAST.Generics(generics)),
        fnHeader = RAST.FnHeader(F), verusHeader = None()),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
        st"""prop_oneof![
            |  bias => base.prop_map(Some),
            |  1 => Just(None),
            |]""")))),
      meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

    return ISZ(defaultStrategy, custStrategy)
  }

  @pure def genProptestGenerators(touchedTypes: TouchedTypes,
                                  thread: AadlThread, cRustTypeProvider: CRustTypeProvider,
                                  model: Aadl, options: HamrCli.CodegenOption,
                                  types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): ISZ[RAST.Item] = {
    var ret: ISZ[RAST.Item] = propTestOptionMethod()
    for (t <- touchedTypes.orderedDependencies) {
      val o = types.typeMap.get(t).get
      val rep = cRustTypeProvider.getRepresentativeType(o)
      val np = cRustTypeProvider.getTypeNameProvider(rep)

      val defaultIdent = RAST.IdentString(st"${(np.qualifiedRustNameS, "_")}_strategy_default".render)
      val custName = st"${(np.qualifiedRustNameS, "_")}_strategy_cust"

      def getDefaultGenerator(name: String, typ: AadlType): ST = {
        cRustTypeProvider.getRepresentativeType(typ) match {
          case b: BaseType =>
            return st"any::<${MicrokitTypeUtil.translateBaseTypeToRust(typ.name)}>()"
          case _ =>
            val npx = cRustTypeProvider.getTypeNameProvider(typ)
            return st"${(npx.qualifiedRustNameS, "_")}_strategy_default()"
        }
      }

      rep match {
        case b: BaseType => // do nothing

        case a: ArrayType =>
          val baseRep = cRustTypeProvider.getRepresentativeType(a.baseType)
          val baseNp = cRustTypeProvider.getTypeNameProvider(baseRep)

          val defaultStrategy = RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = defaultIdent,
              fnDecl = RAST.FnDecl(
                inputs = ISZ(),
                outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              fnHeader = RAST.FnHeader(F), generics = None(), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""$custName(${getDefaultGenerator("base_strategy", baseRep)})""")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())



          val baseStrategyName = st"${(baseNp.qualifiedRustNameS, "_")}_strategy".render
          val baseGeneric =
            RAST.GenericParam(
              ident = RAST.IdentString(baseStrategyName),
              attributes = ISZ(),
              bounds = RAST.GenericBoundFixMe(st"Strategy<Value = ${baseNp.qualifiedRustName}>"))

          val dim = st"${(np.packageRustNames, "::")}::${CRustTypePlugin.getArrayDimName(np, 0)}"

          val paramName = "base_strategy"
          val param = RAST.ParamImpl(
            ident = RAST.IdentString(paramName),
            kind = RAST.TyPath(items = ISZ(ISZ(baseStrategyName)), aadlType = None()))

          val custStrategy = RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = RAST.IdentString(custName.render),
              fnDecl = RAST.FnDecl(
                inputs = ISZ(param),
                outputs = RAST.FnRetTyImpl(
                  RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              generics = Some(RAST.Generics(ISZ(baseGeneric))),
              fnHeader = RAST.FnHeader(F), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""proptest::collection::vec(base_strategy, $dim)
                  |  .prop_map(|v| {
                  |    let boxed: Box<[${baseNp.qualifiedRustName}; $dim]> = v.into_boxed_slice().try_into().unwrap();
                  |    *boxed
                  |})""")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

          ret = ret :+ defaultStrategy :+ custStrategy

        case r: RecordType =>
          val items: ISZ[ST] = for(f <- r.fields.entries) yield getDefaultGenerator(f._1, f._2)

          val defaultStrategy = RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = defaultIdent,
              fnDecl = RAST.FnDecl(
                inputs = ISZ(),
                outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              fnHeader = RAST.FnHeader(F), generics = None(), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""$custName(
                  |  ${(items, ",\n")}
                  |)""")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

          var strategyNames: ISZ[String] = ISZ()
          var fieldNames: ISZ[String] = ISZ()
          var params: ISZ[RAST.Param] = ISZ()
          var generics: ISZ[RAST.GenericParam] = ISZ()
          for (f <- r.fields.entries) {
            val ft = cRustTypeProvider.getTypeNameProvider(f._2)
            val strategyName = st"${f._1}_strategy".render
            strategyNames = strategyNames :+ strategyName
            fieldNames = fieldNames :+ f._1

            val genericName = st"${f._1}_${(ft.qualifiedRustNameS, "_")}_strategy".render

            generics = generics :+ RAST.GenericParam(
              ident = RAST.IdentString(genericName),
              attributes = ISZ(),
              bounds = RAST.GenericBoundFixMe(st"Strategy<Value = ${ft.qualifiedRustName}>"))
            params = params :+ RAST.ParamImpl(
              ident = RAST.IdentString(strategyName),
              kind = RAST.TyPath(items = ISZ(ISZ(genericName)), aadlType = None()))
          }

          val custStrategy = RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = RAST.IdentString(custName.render),
              fnDecl = RAST.FnDecl(
                inputs = params,
                outputs = RAST.FnRetTyImpl(
                  RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              generics = Some(RAST.Generics(generics)),
              fnHeader = RAST.FnHeader(F), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""(${(strategyNames, ", ")}).prop_map(|(${(fieldNames, ", ")})| {
                  |  ${np.qualifiedRustName} { ${(fieldNames, ", ")} }
                  |})""")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

          ret = ret :+ defaultStrategy :+ custStrategy

        case e: EnumType =>
          val bias_args: ISZ[Z] = for (v <- e.values) yield 1
          val bias_params: ISZ[RAST.Param] = for (v <- e.values) yield RAST.ParamImpl(
            ident = RAST.IdentString(s"${v}_bias"),
            kind = RAST.TyPath(items = ISZ(ISZ("u32")), aadlType = None()))
          val items: ISZ[ST] = for(v <- e.values) yield st"${v}_bias => Just(${np.qualifiedRustName}::${v})"

          ret = ret :+ RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = defaultIdent,
              fnDecl = RAST.FnDecl(
                inputs = ISZ(),
                outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              fnHeader = RAST.FnHeader(F), generics = None(), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"$custName(${(bias_args, ", ")})")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())

          ret = ret :+ RAST.FnImpl(
            visibility = RAST.Visibility.Public,
            sig = RAST.FnSig(
              ident = RAST.IdentString(custName.render),
              fnDecl = RAST.FnDecl(
                inputs = bias_params,
                outputs = RAST.FnRetTyImpl(RAST.TyFixMe(st"impl Strategy<Value = ${np.qualifiedRustName}>"))),
              fnHeader = RAST.FnHeader(F), generics = None(), verusHeader = None()),
            body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
              st"""prop_oneof![
                  |  ${(items, ",\n")}
                  |]""")))),
            meta = ISZ(), comments = ISZ(), attributes = ISZ(), contract = None())
        case x =>
          halt("Unexpected type: $x")
      }
    }

    return ret
  }

  @pure def genTestEntries(thread: AadlThread, cRustTypeProvider: CRustTypeProvider): ISZ[RAST.Item] = {
    val threadId = MicrokitUtil.getComponentIdPath(thread)
    assert(thread.isPeriodic(), s"Not yet handling sporadic threads: ${threadId}")

    val inDataPortInits: Option[ST] = {
      var ret = ISZ[ST]()
      for (p <- thread.getPorts().filter(p => p.direction == Direction.In && p.isInstanceOf[AadlDataPort])) {
        val d = p.asInstanceOf[AadlDataPort]
        ret = ret :+ st"test_apis::put_${p.identifier}(${MicrokitTypeUtil.getCRustTypeDefaultValue(d.aadlType, cRustTypeProvider)});"
      }
      if (ret.nonEmpty) Some(st"""
                                 |// populate incoming data ports
                                 |${(ret, "\n")}
                                 |""")
      else None()
    }

    return ISZ(RAST.ItemST(
      st"""mod tests {
          |  // NOTE: need to run tests sequentially to prevent race conditions
          |  //       on the app and the testing apis which are static
          |  use serial_test::serial;
          |
          |  use crate::test::util::*;
          |  use ${CRustTypePlugin.usePath};
          |
          |  #[test]
          |  #[serial]
          |  fn test_initialization() {
          |    crate::${threadId}_initialize();
          |}
          |
          |  #[test]
          |  #[serial]
          |  fn test_compute() {
          |    crate::${threadId}_initialize();
          |    ${(inDataPortInits, "\n")}
          |    crate::${threadId}_timeTriggered();
          |  }
          |}"""))
  }

  @pure override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store
    var resources: ISZ[Resource] = ISZ()

    for (e <- CRustTestingPlugin.getCRustTestingContributions(store).get.testingContributions.entries) {
      val thread = symbolTable.componentMap.get(e._1).get.asInstanceOf[AadlThread]
      val threadId = MicrokitUtil.getComponentIdPath(thread)

      val componentCrateDir = CRustComponentPlugin.componentCrateDirectory(thread, options)
      val componentSrcDir = s"$componentCrateDir/src"
      val componentTestDir = s"$componentSrcDir/test"
      val componentTestUtilDir = s"$componentTestDir/util"

      { // src/test/mod.rs
        val content =
          st"""${MicrokitUtil.safeToEdit}
              |
              |${(for(i <- e._2.modEntries) yield  i.prettyST, "\n\n")}
              |"""
        val path = s"$componentTestDir/mod.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // src/test/tests.rs
        val content =
          st"""${MicrokitUtil.safeToEdit}
              |
              |${(for(i <- e._2.testsEntries) yield i.prettyST, "\n\n")}
              |"""
        val path = s"$componentTestDir/tests.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, F)
      }

      { // src/test/util/mod.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |${(for(i <- e._2.utilModEntries) yield  i.prettyST, "\n\n")}
              |"""
        val path = s"$componentTestUtilDir/mod.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/test/util/generators.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |use data::*;
              |
              |use proptest::prelude::*;
              |
              |${(for(p <- e._2.proptestEntries) yield p.prettyST, "\n\n")}
              |"""
        val path = s"$componentTestUtilDir/generators.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }

      { // src/test/util/test_apis.rs
        val content =
          st"""${MicrokitUtil.doNotEdit}
              |
              |use crate::bridge::extern_c_api as extern_api;
              |use ${CRustTypePlugin.usePath};
              |
              |use proptest::prelude::*;
              |
              |${(for(i <- e._2.testApiEntries) yield i.prettyST, "\n\n")}
              |"""
        val path = s"$componentTestUtilDir/test_apis.rs"
        resources = resources :+ ResourceUtil.createResource(path, content, T)
      }
    }

    return (localStore + s"FINALIZED_$name" ~> BoolValue(T), resources)
  }

}

@datatype class DefaultCRustTestingPlugin extends CRustTestingPlugin {
  val name: String = "CRustTestingPlugin"
}