// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.gcl.GumboGen.{GclEntryPointInitialize, GclEntryPointPeriodicCompute, GclEntryPointSporadicCompute}
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlPort, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.FeatureCategory

object StubTemplate {

  def addImports(imports: ISZ[String]): Option[ST] = {
    val s: Set[String] = Set.empty[String] ++ (for(i <- imports) yield s"import $i")
    return if (s.nonEmpty) Some(st"${(s.elements, "\n")}") else None()
  }

  @pure def subprogram(methodName: String,
                       params: ISZ[String],
                       returnType: Option[String],
                       exampleValue: Option[ST]): (ST, ST) = {
    val _returnType: String = if (returnType.nonEmpty) returnType.get else "Unit"
    return (
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = ${"$"}""",
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = {
          |  ${if (exampleValue.nonEmpty) st"return ${exampleValue.get}" else ""}
          |}""")
  }

  @pure def slangPreamble(inSlang: B,
                          packageName: String,
                          topLevelPackageName: String,
                          imports: ISZ[String],
                          blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""${if (inSlang) "// #Sireum\n\n" else ""}package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |${addImports(imports)}
          |
          |${(blocks, "\n\n")}
          |"""
    return ret
  }

  @pure def slangBody(slangAnnotation: String,
                      objectName: String,
                      body: ISZ[ST]): ST = {
    val ret: ST =
      st"""${slangAnnotation}object ${objectName} {
          |
          |  ${(body, "\n\n")}
          |}"""
    return ret
  }
}

