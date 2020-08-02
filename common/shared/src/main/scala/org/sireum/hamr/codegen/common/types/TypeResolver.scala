// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Aadl
import org.sireum.hamr.codegen.common.symbols._

object TypeResolver {

  def getSlangType(s: String): SlangType.Type = {
    val t: SlangType.Type = s match {
      case "Boolean" => SlangType.B

      case "Integer" => SlangType.Z

      case "Integer_8" => SlangType.S8
      case "Integer_16" => SlangType.S16
      case "Integer_32" => SlangType.S32
      case "Integer_64" => SlangType.S64

      case "Unsigned_8" => SlangType.U8
      case "Unsigned_16" => SlangType.U16
      case "Unsigned_32" => SlangType.U32
      case "Unsigned_64" => SlangType.U64

      case "Float" => SlangType.R // TODO
      case "Float_32" => SlangType.F32
      case "Float_64" => SlangType.F64

      case "Character" => SlangType.C
      case "String" => SlangType.String
    }
    return t
  }

  def processDataTypes(model: Aadl,
                       rawConnections: B,
                       basePackage: String): AadlTypes = {

    var typeMap: Map[String, AadlType] = Map.empty

    for (v <- model.dataComponents) {
      typeMap = typeMap + (v.classifier.get.name ~> processType(v, basePackage, typeMap))
    }

    return AadlTypes(rawConnections, typeMap)
  }

  def processType(c: ir.Component, basePackage: String, typeMap: Map[String, AadlType]): AadlType = {
    assert(c.category == ir.ComponentCategory.Data)
    val cname = c.classifier.get.name

    val container = Some(c)

    if(TypeUtil.isEnumType(c)) {

      return EnumType(cname, container, TypeUtil.getEnumValues(c))
    }
    else if(TypeUtil.isBaseType(c)) {

      val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")

      val t: SlangType.Type = TypeResolver.getSlangType(aadlType)

      return BaseType(cname, container, t)
    }
    else if(TypeUtil.isArrayType(c)) {

      val baseTypeName = TypeUtil.getArrayBaseType(c)
      val baseType = typeMap.get(baseTypeName).get

      return ArrayType(cname, container, baseType)
    }
    else if(TypeUtil.isRecordType(c)) {
      var fields: Map[String, AadlType] = Map.empty

      for(sc <- c.subComponents){
        val fieldName = CommonUtil.getLastName(sc.identifier)
        fields = fields + (fieldName ~> processType(sc, basePackage, typeMap))
      }

      return RecordType(cname, container, fields)
    }
    else {
      return TODOType(cname, container)
    }
  }
}

