// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.types.{AadlType, ArrayType, BaseType, EnumType, RecordType, TypeUtil => CommonTypeUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.message.Reporter

object TypeUtil {

  def processDatatype(t: AadlType, reporter: Reporter): (Option[ST], ST) = {
    return processDatatypeH(t, F, reporter)
  }

  def processDatatypeH(t: AadlType, isField: B, reporter: Reporter): (Option[ST], ST) = {
    if (CommonTypeUtil.isRecordTypeH(t)) {
      val r = t.asInstanceOf[RecordType]
      val name = t.nameProvider.qualifiedCTypeName
      if (isField) {
        return (None(), st"$name")
      } else {
        val fields: ISZ[ST] = for (s <- r.fields.entries) yield st"${processDatatypeH(s._2, T, reporter)._2} ${s._1};"
        return (
          Some(st"typedef struct $name $name;"),
          st"""struct $name {
              |  ${(fields, "\n")}
              |};""")
      }
    } else if (CommonTypeUtil.isBaseTypeH(t)) {
      return (None(), st"${translateBaseType(t.name, reporter).get}")
    } else if (isField) {
      return (None(), st"${t.nameProvider.qualifiedCTypeName}")
    } else if (CommonTypeUtil.isEnumTypeH(t)) {
      val e = t.asInstanceOf[EnumType]
      val name = e.nameProvider.qualifiedCTypeName
      return (None(),
        st"""typedef
            |  enum {${(e.values, ", ")}} $name;""")
    } else if (CommonTypeUtil.isArrayTypeH(t)) {
      val a = t.asInstanceOf[ArrayType]

      val name = t.nameProvider.qualifiedTypeName
      val container = s"${name}_container"
      return (
        None(),
        st"""typedef ${a.baseType.name} $name [];
            |
            |typedef
            |  struct $container{
            |    $name f;
            |  } $container;""")
    } else {
      reporter.error(None(), MicrokitCodegen.toolName, s"Unexpected datatype ${t.name}")
      return (None(), st"")
    }
  }

  @pure def isPrimitive(a: AadlType): B = {
    a match {
      case a: EnumType => return T
      case b: BaseType => return T
      case _ => return F
    }
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
