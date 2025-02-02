// #Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypeNameProvider, ArrayType, BaseType, EnumType, RecordType, SlangType, TypeKind, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.codegen.microkit.util.Util.brand
import org.sireum.message.Reporter

object TypeUtil {

  @sig trait TypeProvider {
    def name: String

    def aadlType: AadlType
  }

  @sig trait cLangTypeProvider extends TypeProvider {
    def name: String = {
      return cQualifiedName
    }

    def cSimpleName: String
    def cQualifiedName: String

    def cForwardDeclaration: Option[ST]
    def cTypeDeclaration: Option[ST]

    def cDefaultValue: ST
  }

  @sig trait rustLangTypeProvider extends TypeProvider {

    def name: String = {
      return rustQualifiedName
    }

    def rustName: String
    def rustQualifiedName: String

    def rustTypeDeclaration: Option[ST]

    // future work: T if the rust type is not based off a C type
    @strictpure def pureRust: B = F
  }

  @datatype class cTypeProvider (val aadlType: AadlType,
                                 val cSimpleName: String,
                                 val cQualifiedName: String,
                                 val cForwardDeclaration: Option[ST],
                                 val cTypeDeclaration: Option[ST],
                                 val cDefaultValue: ST) extends cLangTypeProvider

  @datatype class rustTypeProvider(val aadlType: AadlType,
                                   val rustName: String,
                                   val rustQualifiedName: String,
                                   val rustTypeDeclaration: Option[ST]) extends rustLangTypeProvider

  @datatype class UberTypeProvider(val c: cTypeProvider,
                                   val rust: rustTypeProvider)

  val cratesDir: String = "crates"

  val rustTypesDir: String = s"$cratesDir/types"

  val cTypesDir: String = "types"

  val allTypesFilenamePrefix: String = "types"
  val cAllTypesFilename: String = brand(s"${allTypesFilenamePrefix}.h")
  val rustAllTypesFilename: String = brand(s"${allTypesFilenamePrefix}.rs")

  val aadlTypesFilenamePrefix: String = brand("aadl_types")
  val cAadlTypesFilename: String = s"${aadlTypesFilenamePrefix}.h"
  val rustAadlTypesFilename: String = s"${aadlTypesFilenamePrefix}.rs"

  val make_TYPE_OBJS: String = "TYPE_OBJS"

  val eventPortType: AadlType = {
    BaseType(
      classifier = ISZ("Base_Types::Unsigned_8"),
      nameProvider =
      AadlTypeNameProvider(
        basePackageName = "",
        classifier = ISZ("Base_Types::Unsigned_8"),
        enumValues = ISZ(),
        kind = TypeKind.Base),
      container = None(),
      bitSize = Some(8),
      slangType = SlangType.U8)
  }

  val cTypesFilename: String = brand(s"types.h")
  val rustTypesFilename: String = brand(s"types.rs")

  val eventCounterFilenamePrefix: String = brand("event_counter")
  val cEventCounterFilename: String = s"$eventCounterFilenamePrefix.h"
  val rustEventCounterFilename: String = s"$eventCounterFilenamePrefix.rs"

  val eventCounterTypename: String = brand("event_counter_t")

  val cEventCounterContent: ST =
   st"""#pragma once
       |
       |#include <stdint.h>
       |
       |${Util.doNotEdit}
       |
       |typedef _Atomic uintmax_t ${eventCounterTypename};
       |"""

  val rustEventCounterContent: ST =
    st"""${Util.doNotEdit}
        |
        |pub type $eventCounterTypename = cty::uintmax_t;
        |"""

  val rustMicrokitTypesPrefix: String = brand("microkit_types")
  val rustMicrokitTypesFilename: String = s"${rustMicrokitTypesPrefix}.rs"
  val rustMicrokitTypesContent: ST =
    st"""${Util.doNotEdit}
        |
        |pub type microkit_channel = cty::uint32_t;
        |"""

  val rustCargoContent: ST = st"""${Util.safeToEditMakefile}
                                 |
                                 |[package]
                                 |name = "types"
                                 |version = "0.1.0"
                                 |edition = "2021"
                                 |
                                 |[dependencies]
                                 |cty = "0.2.2"
                                 |
                                 |[lib]
                                 |path = "src/sb_types.rs""""

  def getTypeApiContributions(aadlType: AadlType, typeStore: TypeStore, queueSize: Z): TypeApiContributions = {
    val typeName = typeStore.typeName

    return DefaultTypeApiContributions(
      aadlType = aadlType,
      simpleFilename = QueueTemplate.getTypeQueueName(typeName, queueSize),
      header = QueueTemplate.header(typeName, queueSize),
      implementation = QueueTemplate.implementation(typeName, queueSize, aadlType))
  }

  def processDatatype(aadlType: AadlType, reporter: Reporter): UberTypeProvider = {

    def processField(fieldType: AadlType): (String, ST, String) = {
      fieldType match {
        case rt: RecordType =>
          val cTypeName = fieldType.nameProvider.qualifiedCTypeName
          val rustTypeName = fieldType.nameProvider.rustTypeName
          return (cTypeName, st"$cTypeName", rustTypeName)
        case b: BaseType =>
          val cName = translateBaseTypeToC(fieldType.name, reporter).get
          val rustName = translateBaseTypeToRust(fieldType.name, reporter).get
          return (cName, st"${cName}", rustName)
        case _ =>
          val cName = fieldType.nameProvider.qualifiedCTypeName
          val rustTypeName = fieldType.nameProvider.rustTypeName
          return (cName, st"$cName", rustTypeName)
      }
    }

    aadlType match {
      case rt: RecordType =>
        val cTypeName = rt.nameProvider.qualifiedCTypeName
        val cFields: ISZ[ST] = for (s <- rt.fields.entries) yield st"${processField(s._2)._1} ${s._1};"
          val ctype = cTypeProvider(
            aadlType = aadlType,
            cSimpleName = cTypeName,
            cQualifiedName = cTypeName,
            cForwardDeclaration = Some(st"typedef struct $cTypeName $cTypeName;"),
            cTypeDeclaration = Some(st"""struct $cTypeName {
                                   |  ${(cFields, "\n")}
                                   |};"""),
            cDefaultValue = st"not yet")

        val rustTypeName = rt.nameProvider.rustTypeName
        val rustQualifiedTypeName = rt.nameProvider.qualifiedRustTypeName
        val rustFields: ISZ[ST] = for (s <- rt.fields.entries) yield st"pub ${s._1}: ${processField(s._2)._3}"
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustQualifiedTypeName,
          rustTypeDeclaration = Some(
            st"""#[repr(C)]
                |#[derive(Debug, Clone, Copy)]
                |pub struct $rustTypeName {
                |  ${(rustFields, ",\n")}
                |}"""))

        return UberTypeProvider(ctype, rustType)

      case b: BaseType =>
        val cTypeName = translateBaseTypeToC(aadlType.name, reporter).get
        val ctype = cTypeProvider(
          aadlType = aadlType,
          cSimpleName = cTypeName,
          cQualifiedName = cTypeName,
          cForwardDeclaration = None(),
          cTypeDeclaration = None(),
          cDefaultValue = st"not yet")

        val rustTypeName = translateBaseTypeToRust(aadlType.name, reporter).get
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustTypeName,
          rustTypeDeclaration = None())

        return UberTypeProvider(ctype, rustType)

      case e: EnumType =>
        val cTypeName = e.nameProvider.qualifiedCTypeName
        val ctype = cTypeProvider(
          aadlType = aadlType,
          cSimpleName = cTypeName,
          cQualifiedName = cTypeName,
          cForwardDeclaration = None(),
          cTypeDeclaration = Some(st"""typedef
                                      |  enum {${(e.values, ", ")}} $cTypeName;"""),
          cDefaultValue = st"not yet")

        val rustTypeName = e.nameProvider.rustTypeName
        val rustQualifiedTypeName = e.nameProvider.qualifiedRustTypeName
        val rustEnumValues: ISZ[ST] = for(i <- 0 until e.values.size) yield st"${e.values(i)} = $i"
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustQualifiedTypeName,
          rustTypeDeclaration = Some(
            st"""#[repr(C)]
                |#[derive(Debug, Clone, Copy, PartialEq, Eq)]
                |pub enum $rustTypeName {
                |  ${(rustEnumValues, ",\n")}
                |}"""))

        return UberTypeProvider(ctype, rustType)

      case a: ArrayType =>
        val cName = aadlType.nameProvider.qualifiedCTypeName
        val container = s"${cName}_container"

        val cBaseType: String = getC_TypeName(a, reporter)

        val dim: Z = a.dimensions match {
          case ISZ() =>
            reporter.error(None(), MicrokitCodegen.toolName, s"Unbounded arrays are not currently supported: ${a.name}")
            0
          case ISZ(d) =>
            if (d <= 0) {
              reporter.error(None(), MicrokitCodegen.toolName, s"Array dimension must by >= 1: ${a.name}")
            }
            d
          case _ =>
            reporter.error(None(), MicrokitCodegen.toolName, s"Multi dimensional arrays are not currently supported: ${a.name}")
            0
        }

        val byteSize: Z = a.bitSize match {
          case Some(b) => b / 8
          case _ =>
            reporter.error(None(), MicrokitCodegen.toolName, s"Bit size must be specified for ${a.name}")
            0
        }

        val byteSizeName = getArrayByteSizeDefineName(a)
        val dimName = getArrayDimDefineName(a)

        val ctype = cTypeProvider(
          aadlType = aadlType,
          cSimpleName = cName,
          cQualifiedName = cName,
          cForwardDeclaration = None(),
          cTypeDeclaration = Some(st"""#define $byteSizeName $byteSize
                                      |#define $dimName $dim
                                      |
                                      |typedef $cBaseType $cName [$dimName];
                                      |
                                      |typedef
                                      |  struct $container{
                                      |    $cName f;
                                      |  } $container;"""),
          cDefaultValue = st"not yet")

        val rustTypeName = aadlType.nameProvider.rustTypeName
        val rustQualifiedTypeName = aadlType.nameProvider.qualifiedRustTypeName
        val rustBaseType: String = getRust_TypeName(a, reporter)

        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustQualifiedTypeName,
          rustTypeDeclaration = Some(
            st"""pub const $byteSizeName: usize = $byteSize;
                |pub const $dimName: usize = $dim;
                |
                |pub type $rustTypeName = [$rustBaseType; $dimName];"""))

        return UberTypeProvider(ctype, rustType)

      case x => halt(s"Unexpected Type: $x")
    }
  }

  @pure def getArrayByteSizeDefineName(a: ArrayType): String = {
    return s"${a.nameProvider.qualifiedCTypeName}_SIZE"
  }

  @pure def getArrayDimDefineName(a: ArrayType): String = {
    return s"${a.nameProvider.qualifiedCTypeName}_DIM"
  }

  @pure def isPrimitive(a: AadlType): B = {
    a match {
      case a: EnumType => return T
      case b: BaseType => return T
      case _ => return F
    }
  }

  def getRust_TypeName(a: AadlType, reporter: Reporter): String = {
    val t: AadlType = a match {
      case a: ArrayType => a.baseType
      case _ => a
    }
    return (
      if (isPrimitive(t)) translateBaseTypeToRust(t.name, reporter).get
      else t.nameProvider.qualifiedRustTypeName
    )
  }

  def getC_TypeName(a: AadlType, reporter: Reporter): String = {
    val t: AadlType = a match {
      case a: ArrayType => a.baseType
      case _ => a
    }
    return (
      if (isPrimitive(t)) translateBaseTypeToC(t.name, reporter).get
      else t.nameProvider.qualifiedCTypeName)
  }

  def translateBaseTypeToC(c: String, reporter: Reporter): Option[String] = {
    c match {
      case "Base_Types::Boolean" => return Some("bool")

      case "Base_Types::Integer_8" => return Some(s"int8_t")
      case "Base_Types::Integer_16" => return Some(s"int16_t")
      case "Base_Types::Integer_32" => return Some(s"int32_t")
      case "Base_Types::Integer_64" => return Some(s"int64_t")

      case "Base_Types::Unsigned_8" => return Some(s"uint8_t")
      case "Base_Types::Unsigned_16" => return Some(s"uint16_t")
      case "Base_Types::Unsigned_32" => return Some(s"uint32_t")
      case "Base_Types::Unsigned_64" => return Some(s"uint64_t")

      case "Base_Types::Float_32" => return Some("float")
      case "Base_Types::Float_64" => return Some("double")

      case "Base_Types::Character" => return Some("char")
      case "Base_Types::String" => return Some("char*")

      /*
      case "Base_Types::Float" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Float is not supported in Microkit")
        return None[String]()

      case "Base_Types::Integer" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Integer is not supported in Microkit")
        return None[String]()
     */
      case x =>
        halt(s"Unexpected base types: $x")
    }
  }

  def translateBaseTypeToRust(c: String, reporter: Reporter): Option[String] = {
    c match {
      case "Base_Types::Boolean" => return Some("bool")

      case "Base_Types::Integer_8" => return Some(s"cty::int8_t")
      case "Base_Types::Integer_16" => return Some(s"cty::int16_t")
      case "Base_Types::Integer_32" => return Some(s"cty::int32_t")
      case "Base_Types::Integer_64" => return Some(s"cty::int64_t")

      case "Base_Types::Unsigned_8" => return Some(s"cty::uint8_t")
      case "Base_Types::Unsigned_16" => return Some(s"cty::uint16_t")
      case "Base_Types::Unsigned_32" => return Some(s"cty::uint32_t")
      case "Base_Types::Unsigned_64" => return Some(s"cty::uint64_t")

      case "Base_Types::Float_32" => return Some("cty::c_float")
      case "Base_Types::Float_64" => return Some("cty::double")

      case "Base_Types::Character" => return Some("cty::c_char")
      case "Base_Types::String" => return Some("String")

      /*
      case "Base_Types::Float" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Float is not supported in Microkit")
        return None[String]()

      case "Base_Types::Integer" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Integer is not supported in Microkit")
        return None[String]()
      */

      case x =>
        halt(s"Unexpected base type: $x")
    }
  }
}
