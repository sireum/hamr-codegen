// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.TypeNameProvider

object TypeTemplate {

  def Base_Types(basePackage: String): ST = {
    val ret =
      st"""// #Sireum
          |
          |package ${basePackage}
          |
          |import org.sireum._
          |import org.sireum.S8._
          |import org.sireum.S16._
          |import org.sireum.S32._
          |import org.sireum.S64._
          |import org.sireum.U8._
          |import org.sireum.U16._
          |import org.sireum.U32._
          |import org.sireum.U64._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object Base_Types {
          |
          |  type Boolean = B
          |
          |  type Integer = Z
          |
          |  type Integer_8 = S8
          |  type Integer_16 = S16
          |  type Integer_32 = S32
          |  type Integer_64 = S64
          |
          |  type Unsigned_8 = U8
          |  type Unsigned_16 = U16
          |  type Unsigned_32 = U32
          |  type Unsigned_64 = U64
          |
          |  // TODO: Base_Types::Natural
          |
          |  type Float = R
          |  type Float_32 = F32
          |  type Float_64 = F64
          |
          |  type Character = C
          |  type String = org.sireum.String
          |
          |  type Bits = org.sireum.ISZ[B]
          |
          |  @datatype class Boolean_Payload(value: B) extends art.DataContent
          |
          |  @datatype class Integer_Payload(value: Z) extends art.DataContent
          |
          |  @datatype class Integer_8_Payload(value: S8) extends art.DataContent
          |  @datatype class Integer_16_Payload(value: S16) extends art.DataContent
          |  @datatype class Integer_32_Payload(value: S32) extends art.DataContent
          |  @datatype class Integer_64_Payload(value: S64) extends art.DataContent
          |
          |  @datatype class Unsigned_8_Payload(value: U8) extends art.DataContent
          |  @datatype class Unsigned_16_Payload(value: U16) extends art.DataContent
          |  @datatype class Unsigned_32_Payload(value: U32) extends art.DataContent
          |  @datatype class Unsigned_64_Payload(value: U64) extends art.DataContent
          |
          |  @datatype class Float_Payload(value: R) extends art.DataContent
          |  @datatype class Float_32_Payload(value: F32) extends art.DataContent
          |  @datatype class Float_64_Payload(value: F64) extends art.DataContent
          |
          |  @datatype class Character_Payload(value: C) extends art.DataContent
          |  @datatype class String_Payload(value: String) extends art.DataContent
          |
          |  @datatype class Bits_Payload(value: ISZ[B]) extends art.DataContent
          |
          |  def Boolean_example(): Boolean = {
          |    Contract(Ensures(Res == F))
          |    return F
          |  }
          |
          |
          |  def Integer_example(): Integer = {
          |    Contract(Ensures(Res == z"0"))
          |    return z"0"
          |  }
          |
          |  def Integer_8_example(): Integer_8 = {
          |    Contract(Ensures(Res == s8"0"))
          |    return s8"0"
          |  }
          |
          |  def Integer_16_example(): Integer_16 = {
          |    Contract(Ensures(Res == s16"0"))
          |    return s16"0"
          |  }
          |
          |  def Integer_32_example(): Integer_32 = {
          |    Contract(Ensures(Res == s32"0"))
          |    return s32"0"
          |  }
          |
          |  def Integer_64_example(): Integer_64 = {
          |    Contract(Ensures(Res == s64"0"))
          |    return s64"0"
          |  }
          |
          |
          |  def Unsigned_8_example(): Unsigned_8 = {
          |    Contract(Ensures(Res == u8"0"))
          |    return u8"0"
          |  }
          |
          |  def Unsigned_16_example(): Unsigned_16 = {
          |    Contract(Ensures(Res == u16"0"))
          |    return u16"0"
          |  }
          |
          |  def Unsigned_32_example(): Unsigned_32 = {
          |    Contract(Ensures(Res == u32"0"))
          |    return u32"0"
          |  }
          |
          |  def Unsigned_64_example(): Unsigned_64 = {
          |    Contract(Ensures(Res == u64"0"))
          |    return u64"0"
          |  }
          |
          |
          |  def Float_example(): Float = {
          |    Contract(Ensures(Res == r"0"))
          |    return r"0"
          |  }
          |
          |  def Float_32_example(): Float_32 = {
          |    Contract(Ensures(Res == f32"0"))
          |    return f32"0"
          |  }
          |
          |  def Float_64_example(): Float_64 = {
          |    Contract(Ensures(Res == f64"0"))
          |    return f64"0"
          |  }
          |
          |
          |  def Character_example(): Character = {
          |    Contract(Ensures(Res == ' '))
          |    return ' '
          |  }
          |
          |  def String_example(): String = {
          |    Contract(Ensures(Res == ""))
          |    return ""
          |  }
          |
          |
          |  def Bits_example(): Bits = {
          |    Contract(Ensures(Res == ISZ[B]()))
          |    return ISZ[B]()
          |  }
          |}"""
    return ret
  }

  @pure def enumType(typeNames: TypeNameProvider,
                     values: ISZ[String]): ST = {
    val vals = values.map((m: String) => st""""$m"""")
    val ret: ST =
      st"""@enum object ${typeNames.typeName} {
          |  ${(vals, "\n")}
          |}
          |"""
    return ret
  }

  @pure def dataType(typeNames: TypeNameProvider,
                     fields: ISZ[ST],
                     paramInits: ISZ[String],
                     invariants: ISZ[ST]): ST = {
    val optInvariants: Option[ST] = if (invariants.isEmpty) None()

    else Some(st"${(invariants, "\n\n")}")
    val ret: ST =
      st"""object ${typeNames.typeName} {
          |  def example(): ${typeNames.qualifiedTypeName} = {
          |    return ${typeNames.qualifiedTypeName}(${(paramInits, ", ")})
          |  }
          |}
          |
          |@datatype class ${typeNames.typeName}(
          |  ${(fields, ",\n")}) {
          |  $optInvariants
          |}
          |"""
    return ret
  }

  @pure def typeSkeleton(typeNames: TypeNameProvider): ST = {
    val ret: ST =
      st"""object ${typeNames.typeName} {
          |  def example(): ${typeNames.qualifiedTypeName} = {
          |    return ${typeNames.qualifiedTypeName}()
          |  }
          |}
          |
          |@datatype class ${typeNames.typeName}() // type skeleton
          |"""
    return ret
  }

  @pure def payloadType(typeNames: TypeNameProvider): ST = {
    val typeName = typeNames.qualifiedReferencedTypeName
    val payloadTypeName = typeNames.payloadName
    val examplePayload = typeNames.example()

    val ret: ST =
      st"""object $payloadTypeName {
          |  def example(): $payloadTypeName = {
          |    return $payloadTypeName($examplePayload)
          |  }
          |}
          |
          |@datatype class $payloadTypeName(value: $typeName) extends art.DataContent"""
    return ret
  }

  @pure def typeS(topLevelPackageName: String,
                  packageName: String,
                  imports: ISZ[String],
                  body: ST,
                  payload: ST,
                  canOverwrite: B): ST = {
    val overwrite: ST = if (canOverwrite) {
      st"""
          |${CommentTemplate.doNotEditComment_scala}
          |"""
    } else {
      st""
    }

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import $topLevelPackageName._
          |${StubTemplate.addImports(imports)}
          |$overwrite
          |$body
          |$payload
          |"""
    return ret
  }
}
