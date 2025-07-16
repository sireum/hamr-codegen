// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Aadl
import org.sireum.message.{Position, Reporter}

object TypeResolver {

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
      val cname = ops.StringOps(ops.StringOps(c.classifier.get.name).replaceAllLiterally("::", ":")).split(c => c == ':')

      assert(c.category == ir.ComponentCategory.Data, s"Unexpected data type definition ${cname}")

      val container = Some(c)
      val dataSize: Option[Z] = {
        PropertyUtil.getUnitPropZ(c.properties, HamrProperties.HAMR__BIT_CODEC_MAX_SIZE) match {
          case s: Some[Z] => s
          case _ =>
            PropertyUtil.getUnitPropZ(c.properties, OsateProperties.MEMORY_PROPERTIES__DATA_SIZE) match {
              case s: Some[Z] => s
              case n => n
            }
          }
        }

      val classifier = ops.StringOps(ops.StringOps(c.classifier.get.name).replaceAllLiterally("::", "|")).split((c: C) => c == '|')

      if (TypeUtil.isEnumType(c)) {
        val nameProvider = AadlTypeNameProvider(basePackage, classifier, TypeUtil.getEnumValues(c), TypeKind.Enum)
        return EnumType(cname, nameProvider, container, dataSize, TypeUtil.getEnumValues(c))
      }
      else if (TypeUtil.isBaseType(c)) {

        val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")

        val t: SlangType.Type = TypeUtil.getSlangType(aadlType)

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
        for(d <- dimensions if d < 0) {
          // if Data_Model::Dimension is not provided then an ISZ is introduced.  Dimensions equal to zero
          // become ISZ, otherwise an IS with a range type will be introduced with min = 0 and max = dim - 1
          reporter.error(pos, CommonUtil.toolName, s"Dimensions for ${cname} must be greater or equal to 0 rather than ${d}")
        }

        val kind: ArraySizeKind.Type = TypeUtil.getArraySizeKind(c) match {
          case Some(e) if e == ArraySizeKind.Unbounded =>
            if (dimensions.nonEmpty && dimensions.filter(d => d != 0).nonEmpty) {
              reporter.error(pos, CommonUtil.toolName, s"All dimensions for unbounded array ${cname} must be 0")
            }
            e
          case Some(e) => // bounded or fixed
            if (dimensions.isEmpty || dimensions.filter(d => d == 0).nonEmpty) {
              reporter.error(pos, CommonUtil.toolName, s"Dimensions must be provided for $e array ${cname} and all must be greater than 0")
            }
            e
          case _ =>
            if (dimensions.isEmpty || dimensions.filter(d => d == 0).nonEmpty) {
              ArraySizeKind.Unbounded
            } else if (ops.ISZOps(dimensions).forall(d => d > 0)) {
              ArraySizeKind.Bounded
            } else {
              reporter.error(pos, CommonUtil.toolName, s"Invalid array definition '${c.classifier.get.name}'. Mixed bounded and unbounded arrays are not currently supported")
              ArraySizeKind.Bounded
            }
        }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Array)
        return ArrayType(cname, nameProvider, container, dataSize, dimensions, kind, baseType)
      }
      else if (TypeUtil.isRecordType(c)) {
        var fields: Map[String, AadlType] = Map.empty

        for (sc <- c.subComponents) {
          val fieldName = CommonUtil.getLastName(sc.identifier)
          fields = fields + (fieldName ~> processType(sc, basePackage, maxStringSize, unboundedZRBitWidth, typeMap, rawProtocol, reporter))
        }

        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Record)
        return RecordType(cname, nameProvider, container, dataSize, fields)
      }
      else {
        val nameProvider = AadlTypeNameProvider(basePackage, classifier, ISZ(), TypeKind.Unknown)
        return TODOType(cname, nameProvider, container, dataSize)
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

