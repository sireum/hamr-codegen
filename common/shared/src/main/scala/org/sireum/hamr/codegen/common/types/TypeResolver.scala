// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Aadl

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
                       maxStringSize: Z,
                       unboundedZRBitWidth: Z,
                       basePackage: String): AadlTypes = {

    var typeMap: Map[String, AadlType] = Map.empty

    for (v <- model.dataComponents) {
      typeMap = typeMap + (v.classifier.get.name ~>
        processType(v, basePackage, maxStringSize, unboundedZRBitWidth, typeMap, rawConnections))
    }

    return AadlTypes(rawConnections, typeMap)
  }

  def processType(c: ir.Component,
                  basePackage: String,
                  maxStringSize: Z,
                  unboundedZRBitWidth: Z,
                  typeMap: Map[String, AadlType],
                  rawProtocol: B): AadlType = {

    def base(): AadlType = {
      val cname = c.classifier.get.name

      assert(c.category == ir.ComponentCategory.Data, s"Unexpected data type definition ${cname}")

      val container = Some(c)
      val bitCodecSize = PropertyUtil.getUnitPropZ(c.properties, HamrProperties.HAMR__BIT_CODEC_MAX_SIZE)

      val classifier = ops.StringOps(ops.StringOps(c.classifier.get.name).replaceAllLiterally("::", "|")).split((c: C) => c == '|')

      if (TypeUtil.isEnumType(c)) {
        val nameProvider = AadlTypeNameProvider(basePackage, classifier, TypeUtil.getEnumValues(c), TypeKind.Enum)
        return EnumType(cname, nameProvider, container, bitCodecSize, TypeUtil.getEnumValues(c))
      }
      else if (TypeUtil.isBaseType(c)) {

        val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")

        val t: SlangType.Type = TypeResolver.getSlangType(aadlType)

        val dataSize: Option[Z] =
          bitCodecSize match {
            case Some(bitCodec) => Some(bitCodec)
            case _ => {
              PropertyUtil.getUnitPropZ(c.properties, OsateProperties.MEMORY_PROPERTIES__DATA_SIZE) match {
                case Some(bits) => Some(bits)
                case _ => None()
              }
            }
          }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Base)
        return BaseType(cname, nameProvider, container, dataSize, t)
      }
      else if (TypeUtil.isArrayType(c)) {

        val baseTypeName = TypeUtil.getArrayBaseType(c)
        val baseType = typeMap.get(baseTypeName).get

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Array)
        return ArrayType(cname, nameProvider, container, bitCodecSize, TypeUtil.getArrayDimensions(c), baseType)
      }
      else if (TypeUtil.isRecordType(c)) {
        var fields: Map[String, AadlType] = Map.empty

        for (sc <- c.subComponents) {
          val fieldName = CommonUtil.getLastName(sc.identifier)
          fields = fields + (fieldName ~> processType(sc, basePackage, maxStringSize, unboundedZRBitWidth, typeMap, rawProtocol))
        }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Record)
        return RecordType(cname, nameProvider, container, bitCodecSize, fields)
      }
      else {
        val bitCodecOrDataSize: Option[Z] =
          bitCodecSize match {
            case Some(bitCodec) => Some(bitCodec)
            case _ => {
              // check to see if the component is potentially extending a bound base_type
              PropertyUtil.getUnitPropZ(c.properties, OsateProperties.MEMORY_PROPERTIES__DATA_SIZE) match {
                case Some(bits) => Some(bits)
                case _ => None()
              }
            }
          }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Unknown)
        return TODOType(cname, nameProvider, container, bitCodecOrDataSize)
      }
    }

    val _base = base()

    if (rawProtocol) {
      val np = AadlTypeNameProvider(
        basePackageName = _base.nameProvider.basePackageName,
        classifier = _base.nameProvider.classifier,
        enumValues = _base.nameProvider.enumValues,
        kind = TypeKind.Bit
      )
      return BitType(TypeUtil.SlangEmbeddedBitTypeName, np, _base.container, _base.bitSize, Some(_base))
    } else {
      return _base
    }
  }
}

