// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types._

@sig trait IDatatypeTemplate {
  def typ: AadlType

  def willBeOverwritten: B


  @pure def defaultPreBlocks: ISZ[ST] = {
    val ret: ISZ[ST] = ISZ(
      if (willBeOverwritten) st"${CommentTemplate.doNotEditComment_scala}"
      else st"${CommentTemplate.safeToEditComment_scala}")

    return if (typ.isInstanceOf[TODOType]) ret :+ st"// This is a type skeleton as HAMR doesn't know how to translate ${typ.name}"
    else ret
  }

  @pure def examplePayload: ST = {
    return st"""def example(): ${typ.nameProvider.payloadName} = {
               |  return ${typ.nameProvider.payloadName}(${typ.nameProvider.example()})
               |}"""
  }


  @strictpure def choose(user: ISZ[String], default: ISZ[ST]): ISZ[ST] =
    if (user.nonEmpty) for (u <- user) yield st"${u}"
    else default
}

@datatype class EnumTemplate(val typ: EnumType,

                             val enumValues: ISZ[String],

                             val willBeOverwritten: B) extends IDatatypeTemplate {

  @pure def generateCustom(custSlangSwitches: ISZ[String],
                           custImports: ISZ[String],
                           custDatatypeBlocks: ISZ[String],
                           custPayloadSingletonBlocks: ISZ[String],
                           custPreBlocks: ISZ[String],
                           custPostBlocks: ISZ[String]): ST = {
    return generate(
      slangSwitches = choose(custSlangSwitches, ISZ(st"#Sireum")),
      imports = choose(custImports, ISZ()),
      datatypeBlocks = choose(custDatatypeBlocks, for (e <- enumValues) yield st"$e"),
      payloadSingletonBlocks = choose(custPayloadSingletonBlocks, ISZ(examplePayload)),
      preBlocks = choose(custPreBlocks, defaultPreBlocks),
      postBlocks = choose(custPostBlocks, ISZ())
    )
  }

  @pure def generateDefault(): ST = {
    return generate(
      slangSwitches = ISZ(st"#Sireum"),
      imports = ISZ(),
      datatypeBlocks = for (e <- enumValues) yield st""""$e"""",
      payloadSingletonBlocks = ISZ(examplePayload),
      preBlocks = defaultPreBlocks,
      postBlocks = ISZ())
  }

  @pure def generate(slangSwitches: ISZ[ST],
                     imports: ISZ[ST],
                     datatypeBlocks: ISZ[ST],
                     payloadSingletonBlocks: ISZ[ST],
                     preBlocks: ISZ[ST],
                     postBlocks: ISZ[ST]): ST = {
    val importsOpt: Option[ST] =
      if (imports.isEmpty) None()
      else Some(st"${(imports.map((m: ST) => s"import ${m}"), "\n")}")


    val preBlocksOpt: Option[ST] =
      if (preBlocks.isEmpty) None()
      else Some(
        st"""
                |${(preBlocks, "\n\n")}""")

    val postBlocksOpt: Option[ST] =
      if (postBlocks.isEmpty) None()
      else Some(
        st"""
            |${(postBlocks, "\n\n")}""")

    val ret: ST =
      st"""// ${(slangSwitches, " ")}
          |
          |package ${typ.nameProvider.qualifiedPackageName}
          |
          |import org.sireum._
          |import ${typ.nameProvider.basePackageName}._
          |$importsOpt
          |${preBlocksOpt}
          |
          |@enum object ${typ.nameProvider.typeName} {
          |  ${(datatypeBlocks, "\n")}
          |}
          |
          |object ${typ.nameProvider.payloadName} {
          |  ${(payloadSingletonBlocks, "\n\n")}
          |}
          |
          |@datatype class ${typ.nameProvider.payloadName}(value: ${typ.nameProvider.qualifiedReferencedTypeName}) extends art.DataContent
          |${postBlocksOpt}"""
    return ret
  }
}

@datatype class DatatypeTemplate(val typ: AadlType,
                                 val willBeOverwritten: B) extends IDatatypeTemplate {

  @pure def params: ISZ[ST] = {
    typ match {
      case at: ArrayType =>
        if (at.dimensions.size == 1) {
          return ISZ(st"val value: IS[${at.nameProvider.referencedTypeName}.I, ${at.baseType.nameProvider.referencedSergenTypeName}]")
        } else {
          return ISZ(st"val value: ISZ[${at.baseType.nameProvider.referencedSergenTypeName}]")
        }
      case rt: RecordType =>
        return for (f <- rt.fields.entries) yield st"val ${f._1}: ${f._2.nameProvider.referencedSergenTypeName}"
      case _ => return ISZ()
    }
  }

  @pure def defaultDatatypeCompanionBlocks: ISZ[ST] = {
    var ret : ISZ[ST] = ISZ()

    val args: ISZ[ST] = typ match {
      case at: ArrayType =>
        val baseType = at.baseType.nameProvider.qualifiedReferencedTypeName
        if (at.dimensions.size == 1) {

          val min = 0 // perhaps TODO, allow for negative indexing
          val max = at.dimensions(0) - 1

          ret = ret :+ st"""// Import I's interpolator to create instances of I.  For e.g.,
                            |//   import ${at.nameProvider.qualifiedReferencedTypeName}.I._
                            |//   object Example {
                            |//     val value: ${at.nameProvider.qualifiedReferencedTypeName}.I = i"0"
                            |//     ...
                            |//
                            |// Rename I and use its fromZ method when using multiple <array-def>.I indexing types in the same context.  For e.g.
                            |//   import ${at.nameProvider.qualifiedReferencedTypeName}.{I => I0}
                            |//   import <other-array-def>.{I => I1}
                            |//   object Example {
                            |//     val value: ${at.nameProvider.qualifiedReferencedTypeName}.I = I0.fromZ(0)
                            |//     ...
                            |
                            |@range(min = 0, max = $max, index = T) class I"""

          ISZ(st"value = IS.create[I, $baseType](${at.dimensions(0)}, ${at.baseType.nameProvider.example()})")
        } else {
          ISZ(st"value = ISZ(${at.baseType.nameProvider.example()})")
        }
      case rt: RecordType =>
        for (f <- rt.fields.entries) yield st"${f._1} = ${f._2.nameProvider.example()}"
      case _ => ISZ()
    }

    ret = ret :+ (
      if (args.isEmpty)
        st"""def example(): ${typ.nameProvider.qualifiedReferencedTypeName} = {
            |  return ${typ.nameProvider.qualifiedTypeName}()
            |}"""
      else
        st"""def example(): ${typ.nameProvider.qualifiedReferencedTypeName} = {
            |  return ${typ.nameProvider.qualifiedTypeName}(
            |    ${(args, ",\n")})
            |}""")

    return ret
  }

  @strictpure def defaultDatatypeBlocks: ISZ[ST] = ISZ()

  @pure def generateCustom(custSlangSwitches: ISZ[String],
                           custImports: ISZ[String],
                           custDatatypeCompanionBlocks: ISZ[String],
                           custParams: ISZ[String],
                           custDatatypeBlocks: ISZ[String],
                           custPayloadSingletonBlocks: ISZ[String],
                           custPreBlocks: ISZ[String],
                           custPostBlocks: ISZ[String]): ST = {

    return generate(
      slangSwitches = choose(custSlangSwitches, ISZ(st"#Sireum")),
      imports = choose(custImports, ISZ()),
      datatypeCompanionBlocks = choose(custDatatypeCompanionBlocks, defaultDatatypeCompanionBlocks),
      params = choose(custParams, params),
      datatypeBlocks = choose(custDatatypeBlocks, defaultDatatypeBlocks),
      payloadSingletonBlocks = choose(custPayloadSingletonBlocks, ISZ(examplePayload)),
      preBlocks = choose(custPreBlocks, defaultPreBlocks),
      postBlocks = choose(custPostBlocks, ISZ()))
  }

  @pure def generateDefault(additionalSwitches: ISZ[ST],
                            additionalImports: ISZ[ST],
                            additionalDatatypeCompanionBlocks: ISZ[ST],
                            additionParams: ISZ[ST],
                            additionalDatatypeBlocks: ISZ[ST],
                            additionalPayloadSingletonBlocks: ISZ[ST],
                            additionalPreBlocks: ISZ[ST],
                            addtionalPostBlocks: ISZ[ST]): ST = {
    return generate(
      slangSwitches = ISZ(st"#Sireum") ++ additionalSwitches,
      imports = additionalImports,
      datatypeCompanionBlocks = defaultDatatypeCompanionBlocks ++ additionalDatatypeCompanionBlocks,
      params = params ++ additionParams,
      datatypeBlocks = defaultDatatypeBlocks ++ additionalDatatypeBlocks,
      payloadSingletonBlocks = examplePayload +: additionalPayloadSingletonBlocks,
      preBlocks = defaultPreBlocks ++ additionalPreBlocks,
      postBlocks = addtionalPostBlocks)
  }

  @pure def generate(slangSwitches: ISZ[ST],
                     imports: ISZ[ST],
                     datatypeCompanionBlocks: ISZ[ST],
                     params: ISZ[ST],
                     datatypeBlocks: ISZ[ST],
                     payloadSingletonBlocks: ISZ[ST],
                     preBlocks: ISZ[ST],
                     postBlocks: ISZ[ST]): ST = {
    val uniqueImports: ISZ[String] = (Set.empty[String] ++ (for(i <- imports) yield s"import ${i.render}")).elements

    val importsOpt: Option[ST] =
      if (imports.isEmpty) None()
      else Some(st"${(uniqueImports, "\n")}")

    val datatypeBlocksOpt: Option[ST] =
      if (datatypeBlocks.isEmpty) None()
      else Some(st"${(datatypeBlocks, "\n\n")}")

    val preBlocksOpt: Option[ST] =
      if (preBlocks.isEmpty) None()
      else Some(
        st"""
            |${(preBlocks, "\n\n")}""")

    val postBlocksOpt: Option[ST] =
      if (postBlocks.isEmpty) None()
      else Some(
        st"""
            |${(postBlocks, "\n\n")}""")

    val classDef: ST =
      if (params.isEmpty) st"@datatype class ${typ.nameProvider.typeName}()"
      else
        st"""@datatype class ${typ.nameProvider.typeName}(
            |  ${(params, ",\n")}) {
            |  $datatypeBlocksOpt
            |}"""

    val ret: ST =
      st"""// ${(slangSwitches, " ")}
          |
          |package ${typ.nameProvider.qualifiedPackageName}
          |
          |import org.sireum._
          |import ${typ.nameProvider.basePackageName}._
          |$importsOpt
          |${preBlocksOpt}
          |
          |object ${typ.nameProvider.typeName} {
          |  ${(datatypeCompanionBlocks, "\n\n")}
          |}
          |
          |$classDef
          |
          |object ${typ.nameProvider.payloadName} {
          |  ${(payloadSingletonBlocks, "\n\n")}
          |}
          |
          |@datatype class ${typ.nameProvider.payloadName}(value: ${typ.nameProvider.qualifiedReferencedTypeName}) extends art.DataContent
          |${postBlocksOpt}"""
    return ret
  }
}
