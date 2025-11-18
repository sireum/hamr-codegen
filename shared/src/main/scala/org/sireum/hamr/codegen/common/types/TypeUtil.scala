// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.ir
import org.sireum.lang.{ast => AST}

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

  @pure def getSlangType(s: String): SlangType.Type = {
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

      // the 2025.11 omg library adds prefixes to the following
      case "Data_Float" => SlangType.R
      case "Data_Float_32" => SlangType.F32
      case "Data_Float_64" => SlangType.F64

      case "Data_Boolean" => SlangType.B

    }
    return t
  }

  @pure def getAadlTypeFromSlangType(s: ISZ[String]): String = {
    return st"${(getAadlTypePathFromSlangType(s), "::")}".render
  }

  @pure def getAadlTypePathFromSlangType(s: ISZ[String]): ISZ[String] = {
    s match {
      case ISZ("org", "sireum", _) => return getAadlBaseFromSlangType(s)
      case _ =>
        return (if (s(s.lastIndex) == "Type") ops.ISZOps(s).dropRight(1)
        else s)
    }
  }

  @pure def getAadlBaseFromSlangType(s: ISZ[String]): ISZ[String] = {
    if (s.size != 3 || s(0) != "org" || s(1) != "sireum") {
      halt(s"Infeasible: $s is not a base type")
    }
    val t: String = s(2) match {
      case  "B" => "Boolean"

      case "Z" => "Integer"

      case "S8" => "Integer_8"
      case "S16" => "Integer_16"
      case "S32" => "Integer_32"
      case "S64" => "Integer_64"

      case "U8" => "Unsigned_8"
      case "U16" => "Unsigned_16"
      case "U32" => "Unsigned_32"
      case "U64" => "Unsigned_64"

      case "R" => "Float"
      case "F32" => "Float_32"
      case "F64" => "Float_64"

      case "C" => "Character"
      case "String" => "String"

      case x => halt(s"Infeasible: $x is not a base type")
    }
    return ISZ("Base_Types", t)
  }

  @pure def getIndexingTypeFingerprintMethodName(ids: TypeIdPath): String = {
    val typed = AST.Typed.Name(ids = ids, args = ISZ())
    val fingerprint = AST.Util.stableTypeSig(typed, 3).render
    val first = ops.StringOps(fingerprint).first
    if ('0' <= first && first <= '9') {
      // make sure it's a valid Scala identifier
      return s"I$fingerprint"
    } else {
      return fingerprint
    }
  }

  @pure def getEnumValues(v: ir.Component): ISZ[String] = {
    var ret: ISZ[String] = ISZ()
    if (isEnum(v.properties)) {
      for (p <- PropertyUtil.getPropertyValues(v.properties, OsateProperties.DATA_MODEL__ENUMERATORS)) {
        p match {
          case ir.ValueProp(v2) => ret = ret :+ v2
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
      if (dims.isEmpty) ISZ[Z](0)
      else
        dims.map((x: ir.PropertyValue) => x.asInstanceOf[ir.UnitProp])
          .map((m: ir.UnitProp) => {
            R(m.value) match {
              case Some(x) => conversions.R.toZ(x)
              case _ => z"-1"
            }
          }))
  }

  @pure def getArraySizeKind(c: ir.Component): Option[ArraySizeKind.Type] = {
    PropertyUtil.getDiscreetPropertyValue(c.properties, HamrProperties.HAMR__ARRAY_SIZE_KIND) match {
      case Some(ir.ValueProp(e)) =>
        e match {
          case "Fixed" => return Some(ArraySizeKind.Fixed)
          case "Bounded" => return Some(ArraySizeKind.Bounded)
          case "Unbounded" => return Some(ArraySizeKind.Unbounded)
        }
      case _ => return None()
    }
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
    return isBaseTypeS(s) && !isBaseTypesStringS(s)
  }

  @pure def isBaseTypesStringS(s: String): B = {
    return s == "Base_Types::String"
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
