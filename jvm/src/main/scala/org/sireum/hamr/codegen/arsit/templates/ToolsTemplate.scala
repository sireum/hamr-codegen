// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.containers
import org.sireum.hamr.codegen.common.containers.{FileResource, SireumToolsSergenOption, SireumToolsSergenSerializerMode, SireumToolsSlangcheckGeneratorOption}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.PathUtil

object ToolsTemplate {
  val header: ST =
    st"""::/*#! 2> /dev/null                                   #
      |@ 2>/dev/null # 2>nul & echo off & goto BOF           #
      |if [ -z "$${SIREUM_HOME}" ]; then                      #
      |  echo "Please set SIREUM_HOME env var"               #
      |  exit -1                                             #
      |fi                                                    #
      |exec "$${SIREUM_HOME}/bin/sireum" slang run "$$0" "$$@"    #
      |:BOF
      |setlocal
      |if not defined SIREUM_HOME (
      |  echo Please set SIREUM_HOME env var
      |  exit /B -1
      |)
      |"%SIREUM_HOME%\bin\sireum.bat" slang run %0 %*
      |exit /B %errorlevel%
      |::!#*/
      |// #Sireum
      |
      |import org.sireum._
      |
      |val sireum = Os.path(Os.env("SIREUM_HOME").get) / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")"""

  def toISString(rootDir: Os.Path, resources: ISZ[FileResource]): ST = {
    val relResources: ISZ[String] = for(r <- resources) yield s"\"${PathUtil.convertWinPathSepToNix(rootDir.relativize(Os.path(r.dstPath)).value)}\""
    val r: ST =
      st"""val files: ISZ[String] = ISZ(${(relResources, ",\n")})
          |
          |val toolargs: String = st"$${(files, " ")}".render"""
    return r
  }

  def slangCheck(resources: ISZ[containers.FileResource], basePackage: String, outputDir: String, slangBinDir: String): (ST, SireumToolsSlangcheckGeneratorOption) = {
    val ret: ST =
      st"""$header
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |// create SlangCheck artifacts for the Slang types used in the project
          |
          |${toISString(Os.path(slangBinDir), resources)}
          |
          |(Os.slashDir.up / "src" / "main" / "util" / "${basePackage}").mkdirAll()
          |
          |proc"$$sireum proyek slangcheck -p $basePackage -o $${Os.slashDir.up}/src/main/util/${basePackage} $${Os.slashDir.up} $$toolargs".at(Os.slashDir).console.runCheck()
          |
          |// call to the tools version of SlangCheck which does not invoke Tipe
          |//proc"$$sireum tools slangcheck generator -p $basePackage -o $${Os.slashDir.up}/src/main/util/${basePackage} $$toolargs".at(Os.slashDir).console.runCheck()
          |"""

    val o = SireumToolsSlangcheckGeneratorOption(
      help = "",
      args = for(r <- resources) yield r.dstPath,
      license = None(),
      packageName = ISZ(basePackage),
      outputDir = Some(outputDir),
      testDir = None())

    return (ret, o)

  }

  def genSerGen(basePackage: String, outputDir: String, slangBinDir: String, resources: ISZ[FileResource]): (ST, SireumToolsSergenOption) = {
    val ret: ST =
      st"""$header
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |// create serializers/deserializers for the Slang types used in the project
          |
          |${toISString(Os.path(slangBinDir), resources)}
          |
          |(Os.slashDir.up / "src" / "main" / "util" / "${basePackage}").mkdirAll()
          |
          |proc"$$sireum tools sergen -p ${basePackage} -m json,msgpack -o $${Os.slashDir.up}/src/main/util/${basePackage} $$toolargs".at(Os.slashDir).console.runCheck()
          |"""

    val o = SireumToolsSergenOption(
      help = "",
      args = for(r <- resources) yield r.dstPath,
      modes = ISZ(SireumToolsSergenSerializerMode.Json, SireumToolsSergenSerializerMode.Msgpack),
      packageName = ISZ(basePackage),
      name = None(),
      license = None(),
      outputDir = Some(outputDir))

    return (ret, o)
  }
}
