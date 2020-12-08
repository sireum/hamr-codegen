// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object TypeUtil {

  val FINGERPRINT_WIDTH: Z = z"3"

  val MISSING_AADL_TYPE: String = "Missing::MISSING_AADL_TYPE"

  val EmptyType: TODOType = TODOType("art::Empty", None())

  val SlangEmbeddedBitTypeName: String = "Base_Types.Bits"

  val BIT_SIG: String = "IS[Z, B]"

  val BIT_FINGERPRINT: String = TypeUtil.getTypeFingerprint("IS", BIT_SIG)

  @pure def getEnumValues(v: ir.Component): ISZ[String] = {
    var ret: ISZ[String] = ISZ()
    if (isEnum(v.properties)) {
      for (p <- PropertyUtil.getPropertyValues(v.properties, OsateProperties.DATA_MODEL__ENUMERATORS)) {
        p match {
          case ir.ValueProp(v) => ret = ret :+ v
          case _ => halt(s"Unhandled ${p}")
        }
      }
    }

    return ret
  }

  @pure def getArrayDimensions(a: ArrayType): ISZ[Z] = {
    val ret: ISZ[Z] = a.container match {
      case Some(c) =>
        PropertyUtil.getPropertyValues(c.properties, OsateProperties.DATA_MODEL__DIMENSION)
          .map((x: ir.PropertyValue) => x.asInstanceOf[ir.UnitProp])
          .map((m: ir.UnitProp) => {
            R(m.value) match {
              case Some(x) => conversions.R.toZ(x)
              case _ => z"-1"
            }
          })
      case _ => ISZ()
    }
    return ret
  }

  @pure def findMaxAadlArraySize(types: AadlTypes): Z = {
    var max: Z = 0

    def processType(t: AadlType): Unit = {
      t match {
        case a: ArrayType =>
          val dims = TypeUtil.getArrayDimensions(a)
          assert(dims.size == 1)
          if (dims(0) > max) {
            max = dims(0)
          }
          processType(a.baseType)
        case r: RecordType =>
          r.fields.values.foreach((t: AadlType) => processType(t))
        case _ =>
      }
    }

    types.typeMap.values.foreach((t: AadlType) => processType(t))

    return max
  }

  @pure def getMaxBitsSize(types: AadlTypes): Option[Z] = {
    var ret = z"-1"
    for (t <- types.typeMap.values) {
      TypeUtil.getBitCodecMaxSize(t) match {
        case Some(z) => if (z > ret) {
          ret = z
        }
        case _ =>
      }
    }
    return if (ret == -1) None() else Some(ret)
  }

  @pure def getBitCodecMaxSize(a: AadlType): Option[Z] = {
    val ret: Option[Z] = a.container match {
      case Some(c) =>
        PropertyUtil.getUnitPropZ(c.properties, HamrProperties.HAMR__BIT_CODEC_MAX_SIZE)
      case _ => None()
    }
    return ret
  }

  @pure def getArrayBaseType(c: ir.Component): String = {
    for (p <- c.properties if CommonUtil.getLastName(p.name) == OsateProperties.DATA_MODEL__BASE_TYPE) {
      return p.propertyValues(0).asInstanceOf[ir.ClassifierProp].name
    }
    halt(s"${c} isn't an array")
  }

  @pure def isEnum(props: ISZ[ir.Property]): B = {
    for (p <- props if CommonUtil.getLastName(p.name) == OsateProperties.DATA_MODEL__DATA_REPRESENTATION &&
      ops.ISZOps(p.propertyValues).contains(ir.ValueProp("Enum"))) {
      return T
    }
    return F
  }

  @pure def isEnumType(c: ir.Component): B = {
    return isEnum(c.properties)
  }

  @pure def isRecordType(c: ir.Component): B = {
    return c.subComponents.nonEmpty
  }

  @pure def isArrayType(c: ir.Component): B = {
    for (p <- c.properties if CommonUtil.getLastName(p.name) == OsateProperties.DATA_MODEL__DATA_REPRESENTATION &&
      ops.ISZOps(p.propertyValues).contains(ir.ValueProp("Array"))) {
      return T
    }
    return F
  }

  @pure def isBaseType(c: ir.Component): B = {
    return ops.StringOps(c.classifier.get.name).startsWith("Base_Types::")
  }

  @pure def verifyBitCodec(aadlTypes: AadlTypes,
                           symbolTable: SymbolTable,
                           reporter: Reporter): B = {
    var hasErrors: B = F
    if (aadlTypes.rawConnections) {

      for (featureConnections <- symbolTable.outConnections.values;
           conn <- featureConnections) {

        val src = symbolTable.airFeatureMap.get(CommonUtil.getName(conn.src.feature.get)).get

        if (CommonUtil.isDataPort(src)) {
          val fend = src.asInstanceOf[ir.FeatureEnd]
          val aadlType = aadlTypes.typeMap.get(fend.classifier.get.name).get

          val datatypeName = aadlType.name
          val connectionName = CommonUtil.getLastName(conn.name)

          TypeUtil.getBitCodecMaxSize(aadlType) match {
            case Some(z) =>
              if (z <= 0) {
                hasErrors = T
                reporter.error(None(), CommonUtil.toolName,
                  s"${HamrProperties.HAMR__BIT_CODEC_MAX_SIZE} must be greater than 0 for data type ${datatypeName}")
              }
            case _ =>
              hasErrors = T
              reporter.error(None(), CommonUtil.toolName,
                s"${HamrProperties.HAMR__BIT_CODEC_RAW_CONNECTIONS} specified but data type ${datatypeName} for connection ${connectionName} does not specify ${HamrProperties.HAMR__BIT_CODEC_MAX_SIZE}")
          }
        }
      }
    }

    return !hasErrors
  }

  @pure def getBitsFullyQualifiedTypeName(): String = {
    return "Base_Types.Bits"
  }


  @pure def stableTypeSig(t: String, width: Z): String = {
    val max: Z = if (0 < width && width <= 64) width else 64
    val bytes = ops.ISZOps(crypto.SHA3.sum512(conversions.String.toU8is(t))).take(max)
    var cs = ISZ[C]()
    for (b <- bytes) {
      val c = conversions.U32.toC(conversions.U8.toU32(b))
      cs = cs :+ ops.COps.hex2c((c >>> '\u0004') & '\u000F')
      cs = cs :+ ops.COps.hex2c(c & '\u000F')
    }
    return st"$cs".render
  }

  @pure def getTypeFingerprint(prefix: String, slangTypeName: String): String = {
    return s"${prefix}_${stableTypeSig(slangTypeName, FINGERPRINT_WIDTH)}"
  }

  @pure def getOptionTypeFingerprints(slangTypeName: String): (String, String, String) = {
    val optionType = s"Option[${slangTypeName}]"
    val someType = s"Some[${slangTypeName}]"
    val noneType = s"None[${slangTypeName}]"

    val optionSig = getTypeFingerprint("Option", optionType)
    val someSig = getTypeFingerprint("Some", someType)
    val noneSig = getTypeFingerprint("None", noneType)

    return (optionSig, someSig, noneSig)
  }
}
