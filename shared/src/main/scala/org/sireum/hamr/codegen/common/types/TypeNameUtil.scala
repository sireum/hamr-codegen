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
    return st"${(packageNameI, ".")}".render
  }

  def packageNameI: ISZ[String] = {
    val ret: ISZ[String] = kind match {
      case TypeKind.Empty => ISZ("art")
      case TypeKind.Bit => ISZ("Base_Types")
      case _ =>
        ops.ISZOps(classifier).dropRight(1).map((seg: String) => StringUtil.sanitizeName(seg))
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

  @pure def outputDirectory: String = {
    return st"${(outputDirectoryI, "/")}".render
  }

  @pure def outputDirectoryI: ISZ[String] = {
    return ISZ(basePackageName) ++ packageNameI
  }

  @pure def filePath: String = {
    return st"${(filePathI, "/")}".render
  }

  @pure def filePathI: ISZ[String] = {
    return outputDirectoryI :+ s"$typeName.scala"
  }

  def qualifiedPackageName: String = {
    return st"${(qualifiedPackageNameI, ".")}".render
  }

  def qualifiedPackageNameI: ISZ[String] = {
    return ISZ(basePackageName) ++ packageNameI
  }

  def qualifiedTypeName: String = {
    return st"${(qualifiedTypeNameI, ".")}".render
  }

  def qualifiedTypeNameI: ISZ[String] = {
    return packageNameI :+ typeName
  }

  def referencedTypeName: String = {
    return s"${typeName}${if (isEnum) ".Type" else ""}"
  }

  def qualifiedReferencedTypeName: String = {
    return st"${(qualifiedReferencedTypeNameI, ".")}".render
  }

  def qualifiedReferencedTypeNameI: ISZ[String] = {
    return packageNameI :+ referencedTypeName
  }

  // transpiler and sergen do not support type aliases so need to use the
  // Slang type name for Base_Type usages in datatype field decls and in
  // the generated transpiler scripts
  def referencedSergenTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Bit => "ISZ[B]"
      case TypeKind.Base => TypeUtil.getSlangType(typeName).name
      case _ => s"$packageName.$referencedTypeName"
    }
    return ret
  }

  def qualifiedReferencedSergenTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Bit => referencedSergenTypeName
      case TypeKind.Base => referencedSergenTypeName
      case _ => s"$basePackageName.$referencedSergenTypeName"
    }
    return ret
  }

  def payloadName: String = {
    return if (isEmptyType) typeName else s"${typeName}_Payload"
  }

  def qualifiedPayloadName: String = {
    return st"${(qualifiedPayloadNameI, ".")}".render
  }

  def qualifiedPayloadNameI: ISZ[String] = {
    return packageNameI :+ payloadName
  }

  def qualifiedCTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Empty => "art_Empty"
      case TypeKind.Base => TypeUtil.getSlangType(typeName).string
      case TypeKind.Bit => TypeUtil.BIT_FINGERPRINT
      case _ =>
        val enumSuffix: String = if (isEnum) "_Type" else ""
        //val cPackageName = ops.StringOps(packageName).replaceAllChars('.', '_')
        val cPackageName = st"${(packageNameI, "_")}".render
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
        //val cPackageName = ops.StringOps(packageName).replaceAllChars('.', '_')
        val cPackageName = st"${(packageNameI, "_")}".render
        StringUtil.sanitizeName(s"${basePackageName}_${cPackageName}_${typeName}_example")
      case TypeKind.Bit => s"${qualifiedCTypeName}_example"
      case TypeKind.Array => s"${qualifiedCTypeName}_example"
      case TypeKind.Record => s"${qualifiedCTypeName}_example"
      case TypeKind.Unknown => s"${qualifiedCTypeName}_example"

      case TypeKind.Empty => halt("Empty is an internal type only meant for pure event ports")
    }
    return ret
  }


  ////////////////////////////////////////////////////////////////////////
  // RUST
  ////////////////////////////////////////////////////////////////////////

  def rustTypeName: String = {
    val ret: String = kind match {
      case TypeKind.Empty => "art_Empty"
      case TypeKind.Base => TypeUtil.getSlangType(typeName).string
      case TypeKind.Bit => TypeUtil.BIT_FINGERPRINT
      case _ =>
        val enumSuffix: String = if (isEnum) "_Type" else ""
        val cPackageName = st"${(packageNameI, "_")}".render
        StringUtil.sanitizeName(s"${basePackageName}_${cPackageName}_${typeName}${enumSuffix}")
    }
    return ret
  }

  def qualifiedRustTypeName: String = {
    return s"types::${rustTypeName}"
  }
}

