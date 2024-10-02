// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Aadl
import org.sireum.message.{Position, Reporter}

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

  def getAadlBaseFromSlangType(s: ISZ[String]): String = {
    if (s.size != 3 || s(0) != "org" || s(1) != "sireum") {
      halt(s"Infeasible: $s is not a base type")
    }
    val t: String = s(2) match {
      case  "B" => "Base_Types::Boolean"

      case "Z" => "Base_Types::Integer"

      case "S8" => "Base_Types::Integer_8"
      case "S16" => "Base_Types::Integer_16"
      case "S32" => "Base_Types::Integer_32"
      case "S64" => "Base_Types::Integer_64"

      case "U8" => "Base_Types::Unsigned_8"
      case "U16" => "Base_Types::Unsigned_16"
      case "U32" => "Base_Types::Unsigned_32"
      case "U64" => "Base_Types::Unsigned_64"

      case "R" => "Base_Types::Float"
      case "F32" => "Base_Types::Float_32"
      case "F64" => "Base_Types::Float_64"

      case "C" => "Base_Types::Character"
      case "String" => "Base_Types::String"

      case x => halt(s"Infeasible: $x is not a base type")
    }
    return t
  }

  def processDataTypes(model: Aadl,
                       rawConnections: B,
                       maxStringSize: Z,
                       unboundedZRBitWidth: Z,
                       basePackage: String,
                       reporter: Reporter): AadlTypes = {

    var typeMap: Map[String, AadlType] = Map.empty

    for (v <- model.dataComponents) {
      typeMap = typeMap + (v.classifier.get.name ~>
        processType(v, basePackage, maxStringSize, unboundedZRBitWidth, typeMap, rawConnections, reporter))
    }

    return AadlTypes(rawConnections, typeMap)
  }

  def processType(c: ir.Component,
                  basePackage: String,
                  maxStringSize: Z,
                  unboundedZRBitWidth: Z,
                  typeMap: Map[String, AadlType],
                  rawProtocol: B,
                  reporter: Reporter): AadlType = {

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

        // TODO: need to add position info to the classifier field
        val pos = None[Position]()

        val baseType: AadlType = TypeUtil.getBaseTypes(c) match {
          case notOne if notOne.size != 1 =>
            if (!rawProtocol) {
              val but: String = if (notOne.size > 1) s", but you specified ${notOne.size}" else ""
              reporter.error(pos, CommonUtil.toolName, s"Must specify exactly one base type for ${cname} via ${OsateProperties.DATA_MODEL__BASE_TYPE}$but")
            }
            TypeUtil.EmptyType
          case onlyOne =>
            if(typeMap.contains(onlyOne(0))) typeMap.get(onlyOne(0)).get
            else TypeUtil.EmptyType
        }

        val dimensions: ISZ[Z] = TypeUtil.getArrayDimensions(c)
        for(d <- dimensions if d < 1) {
          // a range type will be introduced with min = 0 so it's max (ie. dim - 1) must be at least 0
          reporter.error(pos, CommonUtil.toolName, s"Dimensions for ${cname} must be greater than 0 rather than ${d}")
        }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Array)
        return ArrayType(cname, nameProvider, container, bitCodecSize, dimensions, baseType)
      }
      else if (TypeUtil.isRecordType(c)) {
        var fields: Map[String, AadlType] = Map.empty

        for (sc <- c.subComponents) {
          if (sc.identifier.name.isEmpty) {
            assert(T)
          }
          val fieldName = CommonUtil.getLastName(sc.identifier)
          fields = fields + (fieldName ~> processType(sc, basePackage, maxStringSize, unboundedZRBitWidth, typeMap, rawProtocol, reporter))
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

