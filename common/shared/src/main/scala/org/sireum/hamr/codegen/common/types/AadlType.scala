// #Sireum

package org.sireum.hamr.codegen.common.types

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.ir

@datatype class AadlTypes(rawConnections: B,
                          typeMap: Map[String, AadlType])

@sig trait AadlType {
  def container: Option[ir.Component]

  def name: String

  def nameProvider: TypeNameProvider

  def bitSize: Option[Z]
}

@datatype class EnumType(val name: String,
                         val nameProvider: TypeNameProvider,

                         val container: Option[ir.Component],
                         val bitSize: Option[Z],

                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val nameProvider: TypeNameProvider,

                          val container: Option[ir.Component],
                          val bitSize: Option[Z],

                          baseType: AadlType) extends AadlType

@datatype class RecordType(val name: String,
                           val nameProvider: TypeNameProvider,

                           val container: Option[ir.Component],
                           val bitSize: Option[Z],

                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val nameProvider: TypeNameProvider,

                         val container: Option[ir.Component],
                         val bitSize: Option[Z],

                         slangType: SlangType.Type
                        ) extends AadlType

@datatype class TODOType(val name: String,
                         val nameProvider: TypeNameProvider,

                         val container: Option[ir.Component],
                         val bitSize: Option[Z]

                        ) extends AadlType

@datatype class BitType(val name: String,
                        val nameProvider: TypeNameProvider,

                        val container: Option[ir.Component],
                        val bitSize: Option[Z],

                        originatingType: Option[AadlType]) extends AadlType

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
