// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.ir

@datatype class AadlTypes(rawConnections: B,
                          typeMap: Map[String, AadlType])

@sig trait AadlType {
  def container: Option[ir.Component]

  def name: String
}

@datatype class EnumType(val name: String,
                         val container: Option[ir.Component],

                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val container: Option[ir.Component],

                          baseType: AadlType) extends AadlType

@datatype class RecordType(val name: String,
                           val container: Option[ir.Component],

                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val container: Option[ir.Component],

                         slangType: SlangType.Type
                        ) extends AadlType

@datatype class TODOType(val name: String,
                         val container: Option[ir.Component]
                        ) extends AadlType

@datatype class BitType(val name: String,
                        val container: Option[ir.Component]) extends AadlType

@enum object SlangType {
  'B // Base_Types::Boolean

  'Z // Base_Types::Integer

  'S8 // Base_Types::Integer_8
  'S16 // Base_Types::Integer_16
  'S32 // Base_Types::Integer_32
  'S64 // Base_Types::Integer_64

  'U8 // Base_Types::Unsigned_8
  'U16 // Base_Types::Unsigned_16
  'U32 // Base_Types::Unsigned_32
  'U64 // Base_Types::Unsigned_64

  // TODO: Base_Types::Natural

  'R // Base_Types::Float ??
  'F32 // Base_Types::Float_32
  'F64 // Base_Types::Float_64

  'C // Base_Types::Character
  'String // Base_Types::String
}

@datatype class DataTypeNames(typ: AadlType,
                              basePackage: String,
                              packageName: String,
                              typeName: String) {

  val split: ISZ[String] = ops.StringOps(ops.StringOps(typ.name).replaceAllChars(':', '|')).split(c => c == '|')

  def filePath: String = {
    return s"$basePackage/$packageName/$typeName.scala"
  }

  def qualifiedPackageName: String = {
    return s"$basePackage.$packageName"
  }

  def qualifiedTypeName: String = {
    return s"$packageName.$typeName"
  }

  def referencedTypeName: String = {
    return s"${typeName}${if (isEnum()) ".Type" else ""}"
  }

  def qualifiedReferencedTypeName: String = {
    return s"${packageName}.${referencedTypeName}"
  }

  def payloadName: String = {
    return if (typ == TypeUtil.EmptyType) typeName else s"${typeName}_Payload"
  }

  def qualifiedPayloadName: String = {
    return s"${packageName}.${payloadName}"
  }

  def qualifiedCTypeName: String = {
    val ret: String = if (typ == TypeUtil.EmptyType) {
      "art_Empty"
    } else {
      typ match {
        case b: BaseType => b.slangType.string
        case b: BitType => TypeUtil.BIT_FINGERPRINT
        case _ =>
          val enumSuffix: String = if (isEnum()) "_Type" else ""
          StringUtil.sanitizeName(s"${basePackage}_${split(0)}_${split(1)}${enumSuffix}")
      }
    }
    return ret
  }

  def isBaseType(): B = {
    return typ.isInstanceOf[BaseType]
  }

  def isEnum(): B = {
    return typ.isInstanceOf[EnumType]
  }

  def isEmptyType(): B = {
    return typ == TypeUtil.EmptyType
  }

  def isBitsTypes(): B = {
    return typ == TypeUtil.SlangEmbeddedBitType
  }

  def isAadlType(): B = {
    return !isBaseType() && !isEmptyType() && !isBitsTypes()
  }

  def empty(): String = {
    val ret: String = typ match {
      case e: EnumType => s"${qualifiedTypeName}.byOrdinal(0).get"
      case e: BaseType => s"${qualifiedTypeName}_empty()"
      case e: BitType => s"${qualifiedTypeName}_empty()"
      case e: ArrayType => s"${qualifiedTypeName}.empty()"
      case e: RecordType => s"${qualifiedTypeName}.empty()"
      case e: TODOType => s"${qualifiedTypeName}.empty()"
    }
    return ret
  }
}
