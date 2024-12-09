// #Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypeNameProvider, ArrayType, BaseType, EnumType, RecordType, SlangType, TypeKind, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.util.Util.brand
import org.sireum.message.Reporter

object TypeUtil {

  val typesDir: String = "types"

  val allTypesFilename: String = brand("types.h")

  val aadlTypesFilename: String = brand("aadl_types.h")

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

  def typesFilename: String = {
    return brand(s"types.h")
  }

  def eventCounterFilename: String = {
    return brand("event_counter.h")
  }

  def eventCounterTypename: String = {
    return brand("event_counter_t")
  }

  def eventCounterContent: ST = {
    return (st"""#pragma once
                |
                |#include <stdint.h>
                |
                |typedef _Atomic uintmax_t ${eventCounterTypename};
                |""")
  }

  def getTypeApiContributions(aadlType: AadlType, typeStore: TypeStore, queueSize: Z): TypeApiContributions = {
    val typeName = typeStore.typeName

    return DefaultTypeApiContributions(
      aadlType = aadlType,
      simpleFilename = QueueTemplate.getTypeQueueName(typeName, queueSize),
      header = QueueTemplate.header(typeName, queueSize),
      implementation = QueueTemplate.implementation(typeName, queueSize, aadlType))
  }

  def processDatatype(t: AadlType, reporter: Reporter): (String, Option[ST], ST) = {
    return processDatatypeH(t, F, reporter)
  }

  def processDatatypeH(t: AadlType, isField: B, reporter: Reporter): (String, Option[ST], ST) = {
    if (CommonTypeUtil.isRecordTypeH(t)) {
      val r = t.asInstanceOf[RecordType]
      val cTypeName = t.nameProvider.qualifiedCTypeName
      if (isField) {
        return (cTypeName, None(), st"$cTypeName")
      } else {
        val fields: ISZ[ST] = for (s <- r.fields.entries) yield st"${processDatatypeH(s._2, T, reporter)._1} ${s._1};"
        return (
          cTypeName,
          Some(st"typedef struct $cTypeName $cTypeName;"),
          st"""struct $cTypeName {
              |  ${(fields, "\n")}
              |};""")
      }
    } else if (CommonTypeUtil.isBaseTypeH(t)) {
      val name = translateBaseType(t.name, reporter).get
      return (name, None(), st"${name}")
    } else if (isField) {
      val name = t.nameProvider.qualifiedCTypeName
      return (name, None(), st"$name")
    } else if (CommonTypeUtil.isEnumTypeH(t)) {
      val e = t.asInstanceOf[EnumType]
      val name = e.nameProvider.qualifiedCTypeName
      return (
        name,
        None(),
        st"""typedef
            |  enum {${(e.values, ", ")}} $name;""")
    } else if (CommonTypeUtil.isArrayTypeH(t)) {
      val a = t.asInstanceOf[ArrayType]

      val name = t.nameProvider.qualifiedCTypeName
      val container = s"${name}_container"

      val baseType: String = getTypeName(a, reporter)

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
      return (
        name,
        None(),
        st"""#define $byteSizeName $byteSize
            |#define $dimName $dim
            |
            |typedef $baseType $name [$dimName];
            |
            |typedef
            |  struct $container{
            |    $name f;
            |  } $container;""")
    } else {
      reporter.error(None(), MicrokitCodegen.toolName, s"Unexpected datatype ${t.name}")
      return ("", None(), st"")
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

  def getTypeName(a: AadlType, reporter: Reporter): String = {
    val t: AadlType = a match {
      case a: ArrayType => a.baseType
      case _ => a
    }
    return (
      if (isPrimitive(t)) translateBaseType(t.name, reporter).get
      else t.nameProvider.qualifiedCTypeName)
  }

  def translateBaseType(c: String, reporter: Reporter): Option[String] = {
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

      case "Base_Types::Float" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Float is not supported in Microkit")
        return None[String]()

      case "Base_Types::Integer" =>
        reporter.error(None(), MicrokitCodegen.toolName, "Unbounded Base_Types::Integer is not supported in Microkit")
        return None[String]()

      case _ => return None[String]()
    }
  }
}
