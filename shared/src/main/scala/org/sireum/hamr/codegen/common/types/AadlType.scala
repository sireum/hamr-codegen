// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.ir

@datatype class AadlTypes(val rawConnections: B,
                          val typeMap: Map[String, AadlType]) {

  @pure def getTypeByPath(path: TypeIdPath): AadlType = {
    return getTypeByPathOpt(path).get
  }

  @pure def getTypeByPathOpt(path: TypeIdPath): Option[AadlType] = {
    return typeMap.get(st"${(path, "::")}".render)
  }

}

@sig trait AadlType {

  def classifier: ISZ[String]

  def name: String = {
    return st"${(classifier, "::")}".render
  }

  def simpleName: String = {
    return classifier(classifier.lastIndex)
  }

  def container: Option[ir.Component]

  def nameProvider: TypeNameProvider

  def bitSize: Option[Z]
}

@datatype class EnumType(val classifier: ISZ[String],
                         val nameProvider: TypeNameProvider,

                         @hidden val container: Option[ir.Component],
                         val bitSize: Option[Z],

                         val values: ISZ[String]) extends AadlType

@datatype class ArrayType(val classifier: ISZ[String],
                          val nameProvider: TypeNameProvider,

                          @hidden val container: Option[ir.Component],
                          val bitSize: Option[Z],

                          val dimensions: ISZ[Z],
                          val baseType: AadlType) extends AadlType

@datatype class RecordType(val classifier: ISZ[String],
                           val nameProvider: TypeNameProvider,

                           @hidden val container: Option[ir.Component],
                           val bitSize: Option[Z],

                           val fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val classifier: ISZ[String],
                         val nameProvider: TypeNameProvider,

                         @hidden val container: Option[ir.Component],
                         val bitSize: Option[Z],

                         val slangType: SlangType.Type
                        ) extends AadlType

@datatype class TODOType(val classifier: ISZ[String],
                         val nameProvider: TypeNameProvider,

                         @hidden val container: Option[ir.Component],
                         val bitSize: Option[Z]

                        ) extends AadlType

@datatype class BitType(val classifier: ISZ[String],
                        val nameProvider: TypeNameProvider,

                        @hidden val container: Option[ir.Component],
                        val bitSize: Option[Z],

                        val originatingType: Option[AadlType]) extends AadlType

@enum object SlangType {
  "B" // Base_Types::Boolean

  "Z" // Base_Types::Integer

  "S8" // Base_Types::Integer_8
  "S16" // Base_Types::Integer_16
  "S32" // Base_Types::Integer_32
  "S64" // Base_Types::Integer_64

  "U8" // Base_Types::Unsigned_8
  "U16" // Base_Types::Unsigned_16
  "U32" // Base_Types::Unsigned_32
  "U64" // Base_Types::Unsigned_64

  // TODO: Base_Types::Natural

  "R" // Base_Types::Float
  "F32" // Base_Types::Float_32
  "F64" // Base_Types::Float_64

  "C" // Base_Types::Character
  "String" // Base_Types::String
}
