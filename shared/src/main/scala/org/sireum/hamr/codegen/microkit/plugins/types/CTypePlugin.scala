// #Sireum

package org.sireum.hamr.codegen.microkit.plugins.types

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{Store, StoreValue}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.linters.MicrokitLinterPlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object CTypePlugin {
  val KEY_MicrokitCTypeProvider: String = "KEY_MICROKIT_C_TYPE_PROVIDER"

  @strictpure def getCTypeProvider(store: Store): Option[CTypeProvider] = store.get(KEY_MicrokitCTypeProvider).asInstanceOf[Option[CTypeProvider]]

  @strictpure def putCTypeProvider(c: CTypeProvider, store: Store): Store = store + KEY_MicrokitCTypeProvider ~> c

  // TODO: maybe move everything below into the Store
  @strictpure def getArrayStringByteSizeDefineName(arrayTypeNampeProvider: CTypeNameProvider): String = st"${(arrayTypeNampeProvider.mangledName, "_")}_BYTE_SIZE".render

  @strictpure def getArrayStringDimDefineName(arrayTypeNampeProvider: CTypeNameProvider, dim: Z): String = st"${(arrayTypeNampeProvider.mangledName, "_")}_DIM_$dim".render
}

@sig trait CTypeNameProvider {
  @pure def mangledName: String
}

@datatype class DefaultCTypeNameProvider(val mangledName: String) extends CTypeNameProvider

@sig trait CTypeProvider extends StoreValue {

  @pure def getRepresentativeType(aadlType: AadlType): AadlType

  @pure def getTypeNameProvider(aadlType: AadlType): CTypeNameProvider
}

@datatype class DefaultMicrokitCTypeProvider(val typeNameProvider: Map[String, CTypeNameProvider],
                                             val substitutions: Map[String, AadlType]) extends CTypeProvider {

  @pure override def getRepresentativeType(aadlType: AadlType): AadlType = {
    return substitutions.getOrElse(aadlType.name, aadlType)
  }

  @pure override def getTypeNameProvider(aadlType: AadlType): CTypeNameProvider = {
    typeNameProvider.get(getRepresentativeType(aadlType).name) match {
      case Some(tProvider) => return tProvider
      case _ => halt(s"Infeasible: $aadlType")
    }
  }
}

@sig trait CTypePlugin extends MicrokitTypePlugin

@datatype class DefaultCTypePlugin() extends CTypePlugin {

  @strictpure override def name: String = "DefaultMicrokitCTypePlugin"

  @strictpure def haveProcessedTypes(store: Store): B = CTypePlugin.getCTypeProvider(store).nonEmpty

  @strictpure override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      MicrokitLinterPlugin.getTouchedTypesOpt(store).nonEmpty &&
      !haveProcessedTypes(store)

  @pure override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var resources: ISZ[Resource] = ISZ()

    val touchedTypes = MicrokitLinterPlugin.getTouchedTypes(store)

    val typeNameProvider = (Map.empty[String, CTypeNameProvider] ++
      (for (aadlTypeName <- touchedTypes.orderedDependencies) yield
        aadlTypeName ~> getTypeNameProvider(types.typeMap.get(aadlTypeName).get, touchedTypes.substitutionTypeMap, reporter))) +
      MicrokitTypeUtil.eventPortTypeName ~> getTypeNameProvider(MicrokitTypeUtil.eventPortType, touchedTypes.substitutionTypeMap, reporter)

    val defs: ISZ[ST] = for (aadlTypeName <- touchedTypes.orderedDependencies if !TypeUtil.isNormalBaseTypeS(aadlTypeName)) yield
      getTypeDefinition(types.typeMap.get(aadlTypeName).get, typeNameProvider, touchedTypes.substitutionTypeMap, reporter)
    val aadlTypesContent =
      st"""#pragma once
          |
          |#include <stdbool.h>
          |#include <stdint.h>
          |
          |${MicrokitUtil.doNotEdit}
          |
          |${(defs, "\n\n")}
          |"""

    val typesIncludesDir = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/include"
    val path = s"$typesIncludesDir/${MicrokitTypeUtil.cAadlTypesFilename}"
    resources = resources :+ ResourceUtil.createResourceH(path, aadlTypesContent, T, T)

    val cEventCounterPath = s"$typesIncludesDir/${MicrokitTypeUtil.cEventCounterFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = cEventCounterPath, content = MicrokitTypeUtil.cEventCounterContent, overwrite = T, isDatatype = T)

    val ret = DefaultMicrokitCTypeProvider(typeNameProvider, touchedTypes.substitutionTypeMap)
    return (CTypePlugin.putCTypeProvider(ret, store), resources)
  }

  def getTypeNameProvider(aadlType: AadlType, substitutions: Map[String, AadlType], reporter: Reporter): CTypeNameProvider = {
    substitutions.getOrElse(aadlType.name, aadlType) match {
      case b: BaseType =>
        assert (b.name != "Base_Types::String", "This should be an AadlArray by now")
        return DefaultCTypeNameProvider(MicrokitTypeUtil.translateBaseTypeToC(b.name))
      case t => return DefaultCTypeNameProvider(mangle(t))
    }
  }

  def getTypeDefinition(aadlType: AadlType, nameProvider: Map[String, CTypeNameProvider], substitutions: Map[String, AadlType], reporter: Reporter): ST = {
    @pure def processType(fieldType: AadlType): String = {
      substitutions.getOrElse(fieldType.name, fieldType) match {
        case b: BaseType => return MicrokitTypeUtil.translateBaseTypeToC(fieldType.name)
        case t => return nameProvider.get(t.name).get.mangledName
      }
    }

    substitutions.getOrElse (aadlType.name, aadlType) match {
      case rt: RecordType =>
        val fields: ISZ[ST] = for (field <- rt.fields.entries) yield st"${processType(field._2)} ${field._1};"
        val name = nameProvider.get(rt.name).get.mangledName
        return (
          st"""typedef struct $name {
              |  ${(fields, "\n")}
              |} $name;""")
      case e: EnumType =>
        return (
          st"""typedef
              |  enum {${(e.values, ", ")}} ${nameProvider.get(e.name).get.mangledName};""")
      case a : ArrayType =>
        val np = nameProvider.get(a.name).get

        val baseType: String = processType(a.baseType)

        assert (a.dimensions.size == 1 && a.dimensions(0) >=0, "Linter should have disallowed other variants")
        val dim = a.dimensions(0)

        val byteSize: Z = a.bitSize.get / 8 // linter guarantees bit size will be > 0

        val byteSizeName = CTypePlugin.getArrayStringByteSizeDefineName(np)
        val dimName = CTypePlugin.getArrayStringDimDefineName(np, 0)

        return (st"""#define $byteSizeName $byteSize
                    |#define $dimName $dim
                    |
                    |typedef $baseType ${np.mangledName} [$dimName];""")

      case t => halt(s"Unexpected Type: $t")
    }
  }

  @pure def mangle(t: AadlType): String = {
    return st"${(for (seg <- t.classifier) yield StringUtil.sanitizeName(seg), "_")}".render
  }
}