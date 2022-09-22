// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil

@enum object TypeKind {
  "Enum"
  "Base"
  "Bit"
  "Array"
  "Record"

  "Empty" // aka type passing through pure event ports

  "Unknown"
}

object TypeNameUtil {
  def getTypeNameProvider(t: AadlType, basePackageName: String): TypeNameProvider = {
    val enumValues: ISZ[String] = t match {
      case e: EnumType => e.values
      case _ => ISZ()
    }
    val kind: TypeKind.Type = t match {
      case e: EnumType => TypeKind.Enum
      case e: ArrayType => TypeKind.Array
      case e: RecordType => TypeKind.Record
      case e: BaseType => TypeKind.Base
      case e: TODOType if e == TypeUtil.EmptyType => TypeKind.Empty
      case e: TODOType => TypeKind.Unknown
      case e: BitType => TypeKind.Bit
    }
    val classifier = ops.StringOps(ops.StringOps(t.name).replaceAllLiterally("::", "|")).split(c => c == '|')

    return getAadlNameProvider(basePackageName,
      classifier,
      enumValues,
      kind)
  }

  def getAadlNameProvider(basePackageName: String,
                          classifier: ISZ[String],
                          enumValues: ISZ[String],
                          kind: TypeKind.Type): TypeNameProvider = {
    return AadlTypeNameProvider(basePackageName, classifier, enumValues, kind)
  }
}

@datatype class AadlTypeNameProvider(val basePackageName: String,
                                     val classifier: ISZ[String],
                                     val enumValues: ISZ[String],
                                     val kind: TypeKind.Type) extends TypeNameProvider

@sig trait TypeNameProvider {

  def basePackageName: String

  // the aadl fully qualified type name, ie what you'd get by
  // splitting on "::"
  def classifier: ISZ[String]

  // should be non-empty if kind is Enum
  def enumValues: ISZ[String]

  def kind: TypeKind.Type

  def packageName: String = {
    val ret: String = kind match {
      case TypeKind.Empty => "art"
      case TypeKind.Bit => "Base_Types"
      case _ =>
        val san = ops.ISZOps(classifier).dropRight(1).map((seg: String) => StringUtil.sanitizeName(seg))
        st"${(san, ".")}".render
    }
    return ret
  }

  def typeName: String = {
    val ret: String = kind match {
      case TypeKind.Empty => "Empty"
      case TypeKind.Bit => "Bits"
      case _ =>
        StringUtil.sanitizeName(ops.ISZOps(classifier).last)
    }
    return ret
  }

  def isBaseType: B = {
    return kind == TypeKind.Base
  }

  def isEnum: B = {
    return kind == TypeKind.Enum
  }

  def isEmptyType: B = {
    return kind == TypeKind.Empty
  }

  def isBitsType: B = {
    return kind == TypeKind.Bit
  }

  def isAadlType(): B = {
    return !isBaseType && !isEmptyType && !isBitsType
  }

  def filePath: String = {
    return s"$basePackageName/$packageName/$typeName.scala"
  }

  def qualifiedPackageName: String = {
    return s"$basePackageName.$packageName"
  }

  def qualifiedTypeName: String = {
    return s"$packageName.$typeName"
  }

  def referencedTypeName: String = {
    return s"${typeName}${if (isEnum) ".Type" else ""}"
  }

  def qualifiedReferencedTypeName: String = {
    return s"${packageName}.${referencedTypeName}"
  }

  def qualifiedReferencedSergenTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Bit => "ISZ[B]"
      case TypeKind.Base => TypeResolver.getSlangType(typeName).name
      case _ => qualifiedReferencedTypeName
    }
    return ret
  }

  def payloadName: String = {
    return if (isEmptyType) typeName else s"${typeName}_Payload"
  }

  def qualifiedPayloadName: String = {
    return s"${packageName}.${payloadName}"
  }

  def qualifiedCTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Empty => "art_Empty"
      case TypeKind.Base => TypeResolver.getSlangType(typeName).string
      case TypeKind.Bit => TypeUtil.BIT_FINGERPRINT
      case _ =>
        val enumSuffix: String = if (isEnum) "_Type" else ""
        val cPackageName = ops.StringOps(packageName).replaceAllChars('.', '_')
        StringUtil.sanitizeName(s"${basePackageName}_${cPackageName}_${typeName}${enumSuffix}")
    }
    return ret
  }

  def example(): String = {
    val ret: String = kind match {
      case TypeKind.Enum => s"${qualifiedTypeName}.byOrdinal(0).get"
      case TypeKind.Base => s"${qualifiedTypeName}_example()"
      case TypeKind.Bit => s"${qualifiedTypeName}_example()"
      case TypeKind.Array => s"${qualifiedTypeName}.example()"
      case TypeKind.Record => s"${qualifiedTypeName}.example()"
      case TypeKind.Unknown => s"${qualifiedTypeName}.example()"

      case TypeKind.Empty => halt("Empty is an internal type only meant for pure event ports")
    }
    return ret
  }

  def example_C_Name(): String = {
    val ret: String = kind match {
      case TypeKind.Enum => s"${qualifiedCTypeName}_${enumValues(0)}"
      case TypeKind.Base =>
        val cPackageName = ops.StringOps(packageName).replaceAllChars('.', '_')
        StringUtil.sanitizeName(s"${basePackageName}_${cPackageName}_${typeName}_example")
      case TypeKind.Bit => s"${qualifiedCTypeName}_example"
      case TypeKind.Array => s"${qualifiedCTypeName}_example"
      case TypeKind.Record => s"${qualifiedCTypeName}_example"
      case TypeKind.Unknown => s"${qualifiedCTypeName}_example"

      case TypeKind.Empty => halt("Empty is an internal type only meant for pure event ports")
    }
    return ret
  }
}

