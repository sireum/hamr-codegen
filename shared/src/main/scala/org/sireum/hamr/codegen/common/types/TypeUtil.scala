// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir

object TypeUtil {

  val FINGERPRINT_WIDTH: Z = z"3"

  val MISSING_AADL_TYPE: String = "Missing::MISSING_AADL_TYPE"

  val EmptyType: TODOType = TODOType(
    classifier = ISZ("art", "Empty"),
    nameProvider = AadlTypeNameProvider(
      basePackageName = "",
      classifier = ISZ("art", "Empty"),
      enumValues = ISZ(),
      kind = TypeKind.Empty),
    container = None(),
    bitSize = None())

  val SlangEmbeddedBitTypeName: ISZ[String] = ISZ("Base_Types" , "Bits")

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

  /** Returns the maximum bit size of all data components that are
    * attached to (event) data ports, even if the port is not connected
    */
  @pure def getMaxBitsSize(symbolTable: SymbolTable): Option[(Z, String)] = {
    var ret: Option[(Z, String)] = None()
    for (port <- symbolTable.getThreads().flatMap((t: AadlThread) => t.getPorts())) {
      port match {
        case afd: AadlFeatureData =>
          (afd.aadlType.bitSize, ret) match {
            case (Some(z), r) => if (r.isEmpty || z > r.get._1) {
              ret = Some((z, st"${(afd.aadlType.nameProvider.classifier, "::")}".render))
            }
            case _ =>
          }
        case _ =>
      }
    }
    return ret
  }

  @pure def getArrayDimensions(c: ir.Component): ISZ[Z] = {
    val dims = PropertyUtil.getPropertyValues(c.properties, OsateProperties.DATA_MODEL__DIMENSION)
    return (
      if (dims.isEmpty) ISZ[Z]()
      else
        dims.map((x: ir.PropertyValue) => x.asInstanceOf[ir.UnitProp])
          .map((m: ir.UnitProp) => {
            R(m.value) match {
              case Some(x) => conversions.R.toZ(x)
              case _ => z"-1"
            }
          }))
  }

  @pure def getBaseTypes(c: ir.Component): ISZ[String] = {
    val baseTypes = PropertyUtil.getPropertyValues(c.properties, OsateProperties.DATA_MODEL__BASE_TYPE)
    return (
      if (baseTypes.isEmpty) ISZ[String]()
      else baseTypes.map((x: ir.PropertyValue) => x.asInstanceOf[ir.ClassifierProp].name))
  }

  @pure def isEmptyType(t: AadlType): B = {
    return t == EmptyType
  }

  @pure def isEnumType(c: ir.Component): B = {
    return isEnum(c.properties)
  }

  @pure def isEnumTypeH(t: AadlType): B = {
    return t.isInstanceOf[EnumType]
  }

  @pure def isEnum(props: ISZ[ir.Property]): B = {
    for (p <- props if CommonUtil.getLastName(p.name) == OsateProperties.DATA_MODEL__DATA_REPRESENTATION &&
      ops.ISZOps(p.propertyValues).contains(ir.ValueProp("Enum"))) {
      return T
    }
    return F
  }

  /* TODO: the following will have have two Data_Representation's attached (array and enum) so can't use
           getDiscreetPropertyValue.  Perhaps a rewrite step introducing an array type whose base type
           is <some_enum_type> and substituting the field's type with the new array type

      data implementation SearchTask.i
        subcomponents
          id: data <some_enum_type> {Data_Model::Data_Representation => Array; Data_Model::Dimension => (8);};
          ...
   */

  /*
  @pure def isEnum(props: ISZ[ir.Property]): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(props, OsateProperties.DATA_MODEL__DATA_REPRESENTATION) match {
      case Some(ir.ValueProp("Enum")) => T
      case _ => F
    }
    return ret
  }
  */

  @pure def isRecordType(c: ir.Component): B = {
    return c.category == ir.ComponentCategory.Data && c.subComponents.nonEmpty
  }
  @pure def isRecordTypeH(r: AadlType): B = {
    return r.isInstanceOf[RecordType]
  }

  @pure def isArrayType(c: ir.Component): B = {
    for (p <- c.properties if CommonUtil.getLastName(p.name) == OsateProperties.DATA_MODEL__DATA_REPRESENTATION &&
      ops.ISZOps(p.propertyValues).contains(ir.ValueProp("Array"))) {
      return T
    }
    return F
  }

  @pure def isArrayTypeH(a: AadlType): B = {
    return a.isInstanceOf[ArrayType]
  }

  @pure def isBaseType(c: ir.Component): B = {
    return ops.StringOps(c.classifier.get.name).startsWith("Base_Types::")
  }

  @pure def isBaseTypeA(a: AadlType): B = {
    return a.isInstanceOf[BaseType]
  }

  @pure def isBaseTypeS(s: String): B = {
    return ops.StringOps(s).startsWith("Base_Types::")
  }

  @pure def isNormalBaseTypeS(s: String): B = {
    return isBaseTypeS(s) && s != "Base_Types::String"
  }

  def isMissingType(c: ir.Component): B = {
    return isMissingTypeClassifier(c.classifier.get)
  }

  def isMissingTypeClassifier(c: ir.Classifier): B = {
    return c.name == MISSING_AADL_TYPE
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
