// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.types

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, BaseType, EnumType, RecordType, TypeUtil}
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.plugins.linters.MicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.rust.Visibility
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.{RustUtil, Util}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.hamr.codegen.microkit.{rust => RustAst}

// a type provider for C + Rust microkit components (ie. a C microkit component that
// calls out to a rust crate)
object CRustTypePlugin {

  val KEY_CRustTypeProvider: String = "KEY_MICROKIT_CRUST_TYPE_PROVIDER"

  @strictpure def hasCRustTypeProvider(store: Store): B = store.contains(KEY_CRustTypeProvider)

  @strictpure def getCRustTypeProvider(store: Store): Option[CRustTypeProvider] = store.get(KEY_CRustTypeProvider).asInstanceOf[Option[CRustTypeProvider]]

  @strictpure def putCRustTypeProvider(c: CRustTypeProvider, store: Store): Store = store + KEY_CRustTypeProvider ~> c


  // TODO: maybe move everything below into the Store
  @strictpure def getArraySizeName(arrayTypeNampeProvider: CRustTypeNameProvider): String = st"${(arrayTypeNampeProvider.qualifiedRustNameS, "_")}_BYTE_SIZE".render

  @strictpure def getArrayDimName(arrayTypeNampeProvider: CRustTypeNameProvider, dim: Z): String = st"${(arrayTypeNampeProvider.qualifiedRustNameS, "_")}_DIM_$dim".render

  @strictpure def dataDirectory(options: HamrCli.CodegenOption): String = s"${options.sel4OutputDir.get}/crates/data"
}

@sig trait CRustTypeNameProvider {
  @strictpure def qualifiedRustNameS: ISZ[String]

  @strictpure def simpleRustName: String = qualifiedRustNameS(qualifiedRustNameS.lastIndex)

  @strictpure def qualifiedRustName: String = st"${(qualifiedRustNameS, "::")}".render

  @strictpure def packageRustNames: ISZ[String] =
    if (qualifiedRustNameS.isEmpty) ISZ()
    else ops.ISZOps(qualifiedRustNameS).dropRight(1)
}

@datatype class DefaultCRustTypeNameProvider(val qualifiedRustNameS: ISZ[String]) extends CRustTypeNameProvider

@sig trait CRustTypeProvider extends StoreValue {
  @pure def getTypeNameProvider(aadlType: AadlType): CRustTypeNameProvider

  @pure def getRepresentativeType(aadlType: AadlType): AadlType

  @pure def rustTypeDefs: HashSMap[String, ISZ[RustAst.Item]]
}

@datatype class DefaultCRustTypeProvider(val rustTypeDefs: HashSMap[String, ISZ[RustAst.Item]],

                                         // use getTypeNameProvider rather than the following fields
                                         val PRIVATE_typeNameProvider: Map[String, CRustTypeNameProvider],
                                         val PRIVATE_substitutions: Map[String, AadlType]) extends CRustTypeProvider {

  @pure override def getRepresentativeType(aadlType: AadlType): AadlType = {
    return PRIVATE_substitutions.getOrElse(aadlType.name, aadlType)
  }

  @pure override def getTypeNameProvider(aadlType: AadlType): CRustTypeNameProvider = {
    PRIVATE_typeNameProvider.get(getRepresentativeType(aadlType).name) match {
      case Some(tProvider) => return tProvider
      case _ => halt(s"Infeasible: $aadlType")
    }
  }
}

@sig trait CRustTypePlugin extends MicrokitTypePlugin with MicrokitFinalizePlugin {

  @strictpure def haveProcessedTypes(store: Store): B = CRustTypePlugin.hasCRustTypeProvider(store)

  @strictpure def alreadyFinalized(store: Store): B = store.contains(s"FINALIZED_${name}")

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      // TODO this should probably be modelIsCRusty indicating there are components that will be C + Rust rather
      //      than just pure Rust
      MicrokitPlugin.modelIsRusty(store) &&
      MicrokitLinterPlugin.getTouchedTypesOpt(store).nonEmpty &&
      !haveProcessedTypes(store)

  @strictpure override def canFinalize(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      // TODO this should probably be modelIsCRusty indicating there are components that will be C + Rust rather
      //      than just pure Rust
      MicrokitPlugin.modelIsRusty(store) &&
      haveProcessedTypes(store) &&
      ~alreadyFinalized(store)

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var resources: ISZ[Resource] = ISZ()

    val touchedTypes = MicrokitLinterPlugin.getTouchedTypes(store)

    val typeNameProvider = (Map.empty[String, CRustTypeNameProvider] ++
      (for (aadlTypeName <- touchedTypes.orderedDependencies) yield
        aadlTypeName ~> getTypeNameProvider(types.typeMap.get(aadlTypeName).get, touchedTypes.substitutionTypeMap, reporter))) +
      MicrokitTypeUtil.eventPortTypeName ~> getTypeNameProvider(MicrokitTypeUtil.eventPortType, touchedTypes.substitutionTypeMap, reporter)

    val rustItems = HashSMap.empty[String, ISZ[RustAst.Item]] ++ (
      for (aadlTypeName <- touchedTypes.orderedDependencies if !TypeUtil.isBaseTypeS(aadlTypeName) || TypeUtil.isBaseTypesStringS(aadlTypeName)) yield
        aadlTypeName ~> getRustItems(types.typeMap.get(aadlTypeName).get, typeNameProvider, touchedTypes.substitutionTypeMap))

    val ret = DefaultCRustTypeProvider(rustItems, typeNameProvider, touchedTypes.substitutionTypeMap)
    return (CRustTypePlugin.putCRustTypeProvider(ret, store), resources)
  }

  @pure override def finalize(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var resources: ISZ[Resource] = ISZ()
    val typeProvider = CRustTypePlugin.getCRustTypeProvider(store).get

    // TODO verus doesn't play too well with having a common types library (ie. each create depends on ../data)
    //      A workaround until cargo+verus is improved is to nest the library into each crate

    val rootDataDir = CRustTypePlugin.dataDirectory(options)
    val rootDataSrcDir = s"$rootDataDir/src"

      var modIncludes: Map[IdPath, ISZ[ST]] = Map.empty
      for(e <- typeProvider.rustTypeDefs.entries) {
        val nameProvider = typeProvider.getTypeNameProvider(types.typeMap.get(e._1).get)
        val fname = s"${nameProvider.simpleRustName}.rs"
        val path = st"$rootDataSrcDir/${(nameProvider.packageRustNames, "/")}/$fname".render

        val content = st"${(for(i <- e._2) yield i.prettyST, "\n\n")}"

        resources = resources :+ ResourceUtil.createResourceH(
          path = path, content = content, overwrite = T, isDatatype = T)

        modIncludes = modIncludes +
          nameProvider.packageRustNames ~> (
            modIncludes.getOrElse(nameProvider.packageRustNames, ISZ()) :+ st"""include!("$fname");""")
      }

      for(p <- modIncludes.entries) { // src/data/<package>/mod.rs
        val packageMod =
          st"""${Util.doNotEdit}
              |
              |${(p._2, "\n")}
              |
              |/*
              |NOTE: the include!("xx.rs") inlines the file contents directly so a module for
              |xx is not created.  An alternative would be to do
              |
              |pub mod xx.rs;
              |pub use xx::*;
              |
              |however this would create a module out of xx.rs so to refer to the datatype
              |it contains you'd have to do something like "use data::xx::xx"
              |in lib.rs. A workaround is to rename the file, e.g. xx_STRUCT.rs but leave the
              |file contents unchanged.  Then in this file do
              |
              |pub mod xx_STRUCT;
              |pub use xx_STRUCT::*;
              |
              |then xx would be imported in lib.rs via "use data::${(p._1, "::")}::*"
              |*/
              |"""
        val packageModPath = st"${rootDataSrcDir}/${(p._1, "/")}/mod.rs".render
        resources = resources :+ ResourceUtil.createResourceH(path = packageModPath, content = packageMod, overwrite = T, isDatatype = T)
      }

      { // src/data/sb_event_counter.rs
        val sbEventCounter =
          st"""${Util.doNotEdit}
              |
              |pub type sb_event_counter_t = usize;
              |"""
        val sbEventCounterPath = s"$rootDataSrcDir/sb_event_counter.rs"
        resources = resources :+ ResourceUtil.createResourceH(path = sbEventCounterPath, content = sbEventCounter, overwrite = T, isDatatype = T)
      }

      { // src/data/sb_microkit_types.rs
        val sbMicrokitTypes =
          st"""${Util.doNotEdit}
              |
              |pub type microkit_channel = u32;
              |"""
        val sbMicrokitTypesPath = s"$rootDataSrcDir/sb_microkit_types.rs"
        resources = resources :+ ResourceUtil.createResourceH(path = sbMicrokitTypesPath, content = sbMicrokitTypes, overwrite = T, isDatatype = T)
      }

      { // src/lib.rs
        val dataMod =
          st"""#![cfg_attr(not(test), no_std)]
              |
              |${RustUtil.defaultCrateLevelAttributes}
              |
              |${Util.doNotEdit}
              |
              |${(for (k <- modIncludes.keys) yield st"pub mod ${(k, "::")};", "\n")}
              |
              |include!("sb_event_counter.rs");
              |include!("sb_microkit_types.rs");
              |"""
        val dataModPath = s"${rootDataSrcDir}/lib.rs"
        resources = resources :+ ResourceUtil.createResourceH(path = dataModPath, content = dataMod, overwrite = T, isDatatype = T)
      }

    { // Cargo.toml
      val content = st"""${Util.safeToEditMakefile}
                        |
                        |[package]
                        |name = "data"
                        |version = "0.1.0"
                        |edition = "2021"
                        |
                        |[dependencies]
                        |${RustUtil.verusCargoDependencies}
                        |
                        |[package.metadata.verus]
                        |verify = true
                        |"""
      val cargoTomlPath = s"${rootDataDir}/Cargo.toml"
      resources = resources :+ ResourceUtil.createResourceH(path = cargoTomlPath, content = content, overwrite = T, isDatatype = T)
    }

    { // rust-toolchain.toml
      val content = RustUtil.defaultRustToolChainToml

      val rusttoolchain = s"${rootDataDir}/rust-toolchain.toml"
      resources = resources :+ ResourceUtil.createResourceH(path = rusttoolchain, content = content, overwrite = T, isDatatype = T)
    }

    return (store + s"FINALIZED_$name" ~> BoolValue(true), resources)
  }

  @pure def getTypeNameProvider(aadlType: AadlType, substitutions: Map[String, AadlType], reporter: Reporter): CRustTypeNameProvider = {
    substitutions.getOrElse(aadlType.name, aadlType) match {
      case b: BaseType =>
        assert (b.name != "Base_Types::String", "This should be an AadlArray by now")
        val name = MicrokitTypeUtil.translateBaseTypeToRust(b.name)
        return DefaultCRustTypeNameProvider(qualifiedRustNameS = ISZ(name))
      case t =>
        val qualifiedNameS: ISZ[String] = for(c <- t.classifier) yield StringUtil.sanitizeName(c)
        return DefaultCRustTypeNameProvider(qualifiedRustNameS = qualifiedNameS)
    }
  }

  @pure def getRustItems(aadlType: AadlType,
                         typeNameProvider: Map[String, CRustTypeNameProvider],
                         substitutions: Map[String, AadlType]): ISZ[RustAst.Item] = {
    @pure def getTypeSimpleName(a: AadlType): String = {
      return typeNameProvider.get(substitutions.getOrElse(a.name, a).name).get.simpleRustName
    }
    @pure def getTypePackageNamesName(a: AadlType): ISZ[String] = {
      return typeNameProvider.get(substitutions.getOrElse(a.name, a).name).get.qualifiedRustNameS
    }
    @pure def getCRustTypeDefaultValue(a: AadlType): String = {
      substitutions.getOrElse(a.name, a) match {
        case b: BaseType => return MicrokitTypeUtil.getRustPrimitiveDefaultValue(a.name)
        case _ if a.name == "Base_Types::String" => return "[0; Base_Types::Base_Types_String_DIM_0]"
        case at: ArrayType =>
          assert (at.dimensions.size == 1, "Need to handle multi-dim arrays")
          val np = typeNameProvider.get(at.name).get
          val dim0 = CRustTypePlugin.getArrayDimName(np, 0)
          val baseTypeDefault = getCRustTypeDefaultValue(at.baseType)
          val dimConst = st"${(ops.ISZOps(np.qualifiedRustNameS).dropRight(1), "::")}".render
          return s"[$baseTypeDefault; $dimConst::$dim0]"
        case _ => return s"${typeNameProvider.get(a.name).get.qualifiedRustName}::default()"
      }
    }

    var ret: ISZ[RustAst.Item] = ISZ()

    ret = ret :+ RustAst.ItemST(Util.doNotEdit)
    ret = ret :+ RustAst.Use(ISZ(), RustAst.IdentString("vstd::prelude::*"))
    ret = ret :+ RustAst.Use(ISZ(), RustAst.IdentString("super::*"))

    var uses: Set[RustAst.IdentString] = Set.empty

    // TODO is it safe to always assume type defs will be in verus (ie. even if there are no contracts)
    var inVerusItems: ISZ[RustAst.Item] = ISZ()

    val substituteType = substitutions.getOrElse(aadlType.name, aadlType)
    val aadlTypePackageName = ops.ISZOps(getTypePackageNamesName(substituteType)).dropRight(1)

    @pure def addType(t: AadlType): RustAst.TypeAadl = {
      val subT = substitutions.getOrElse(t.name, t)
      val qualfiedName = getTypePackageNamesName(subT)
      val tPackageName = ops.ISZOps(qualfiedName).dropRight(1)
      if (!subT.isInstanceOf[BaseType] && aadlTypePackageName != tPackageName) {
        uses = uses + RustAst.IdentString(st"super::${(tPackageName, "::")}::*".render)
      }
      return RustAst.TypeAadl(
        qualifiedNameS = qualfiedName,
        aadlTypeName = subT.classifier)
    }

    var implBody: Option[ST] = None()
    substituteType match {
      case rt: RecordType =>
        val fields: ISZ[RustAst.StructField] = for (f <- rt.fields.entries) yield
          RustAst.StructField(
            visibility = Visibility.Public,
            isGhost = F,
            ident = RustAst.IdentString(f._1),
            fieldType = addType(f._2))
        inVerusItems = inVerusItems :+
          RustAst.StructDef(
            attributes = ISZ(
              RustAst.AttributeST(F, st"repr(C)"),
              RustAst.AttributeST(F, st"derive(Debug, Clone, Copy, PartialEq, Eq)")),
            visibility = Visibility.Public,
            ident = RustAst.IdentString(getTypeSimpleName(rt)),
            items = fields.asInstanceOf[ISZ[RustAst.Item]])

        val fieldDefaults: ISZ[ST] = for (f <- rt.fields.entries) yield
          st"${f._1}: ${getCRustTypeDefaultValue(f._2)}"
        implBody = Some(st"Self { ${(fieldDefaults, ", ")} }")

      case et: EnumType =>
        var enumValues : ISZ[RustAst.Item] = ISZ()
        for (i <- 0 until et.values.size) {
          enumValues = enumValues :+ RustAst.CEnumValue(
            visibility = Visibility.Public,
            ident = RustAst.IdentString(et.values(i)),
            value = RustAst.IdentString(i.string))
        }
        inVerusItems = inVerusItems :+
          RustAst.EnumDef(
            attributes = ISZ(
              RustAst.AttributeST(F, st"repr(C)"),
              RustAst.AttributeST(F, st"derive(Copy, Clone, Debug, PartialEq, Eq, Structural)")),
            visibility = Visibility.Public,
            ident = RustAst.IdentString(getTypeSimpleName(et)),
            items = enumValues)

        implBody = Some(st"${getTypeSimpleName(et)}::${et.values(0)}")

      case at: ArrayType =>
        val np = typeNameProvider.get(at.name).get
        var dims: ISZ[RustAst.Ident] = ISZ()
        var companions: ISZ[RustAst.Item] = ISZ()
        val byteSize = at.bitSize.get / 8
        companions = companions :+ RustAst.ItemString(s"pub const ${CRustTypePlugin.getArraySizeName(np)}: usize = $byteSize;")
        for (i <- 0 until at.dimensions.size) {
          val dimName = CRustTypePlugin.getArrayDimName(np, i)
          companions = companions :+ RustAst.ItemString(s"pub const $dimName: usize = ${at.dimensions(i)};")
          dims = dims :+ RustAst.IdentString(dimName)
        }
        inVerusItems = inVerusItems :+
          RustAst.Array(
            companions = companions,
            attributes = ISZ(),
            visibility = Visibility.Public,
            ident = RustAst.IdentString(getTypeSimpleName(at)),
            dims = dims,
            elemType = addType(at.baseType))

        //assert(at.dimensions.size == 1, "Need to handle multi-dim arrays")
        //val lastDim = CRustTypePlugin.getArrayDimName(np, at.dimensions.lastIndex)
        //implBody = Some(st"[${getCRustTypeDefaultValue(at.baseType)}; $lastDim]")

      case _ => halt(s"Infeasible ${aadlType.name}")
    }

    for (u <- uses.elements) {
      ret = ret :+ RustAst.Use(ISZ(), u)
    }
    if (implBody.nonEmpty) {
      inVerusItems = inVerusItems :+
        RustAst.ImplBase(
          implIdent = Some(RustAst.IdentString("Default")),
          forIdent = RustAst.IdentString(getTypeSimpleName(aadlType)),
          items = ISZ(RustAst.FnImpl(
            sig = RustAst.FnSig(
              verusHeader = None(),
              fnHeader = RustAst.FnHeader(F),
              ident = RustAst.IdentString("default"),
              generics = None(),
              fnDecl = RustAst.FnDecl(ISZ(), RustAst.FnRetTyImpl(RustAst.TypeRust(ISZ("Self"))))),
            comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Private, contract = None(), meta = ISZ(),
            body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(implBody.get)))))),
          comments = ISZ(),attributes = ISZ())
    }
    return ret :+ RustAst.MacCall("verus", inVerusItems)
  }
}

@datatype class DefaultCRustTypePlugin extends CRustTypePlugin {

  @strictpure override def name: String = "DefaultCRustTypePlugin"

}