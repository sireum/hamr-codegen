// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.Util
import org.sireum.hamr.arsit.util.ArsitLibrary
import org.sireum.hamr.codegen.common.containers.SireumSlangTranspilersCOption
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.TypeUtil

object TranspilerTemplate {

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  @pure def transpiler(verbose: B,
                       libraryName: String,
                       sourcepaths: ISZ[String],
                       outputDir: Os.Path,
                       binDir: String,
                       apps: ISZ[String],
                       forwards: ISZ[String],
                       numBits: Z,
                       maxArraySize: Z,
                       maxStringSize: Z,
                       customArraySizes: ISZ[String],
                       customConstants: ISZ[String],
                       stackSizeInBytes: Z,
                       extensions: ISZ[String],
                       excludes: ISZ[String],
                       buildApps: B,
                       cmakeIncludes: ISZ[String]): (ST, SireumSlangTranspilersCOption) = {

    val _stackSizeInBytes: String = if (stackSizeInBytes < 0) {
      "16*1024*1024" // default set in org.sireum.transpilers.cli.cTranspiler
    } else {
      stackSizeInBytes.string
    }

    val transpilerOptions =
      SireumSlangTranspilersCOption(
        help = "",
        args = ISZ(),
        sourcepath = sourcepaths,
        output = Some(outputDir.value),
        verbose = verbose,
        projectName = Some(libraryName), // default set in org.sireum.transpilers.cli.cTranspiler
        apps = apps,
        unroll = F, // default set in org.sireum.transpilers.cli.cTranspiler
        fingerprint = TypeUtil.FINGERPRINT_WIDTH, // default set in org.sireum.transpilers.cli.cTranspiler
        bitWidth = numBits,
        maxStringSize = maxStringSize,
        maxArraySize = maxArraySize,
        customArraySizes = customArraySizes,
        customConstants = customConstants,
        plugins = ISZ(),
        exts = extensions,
        forwarding = forwards,
        stackSize = Some(_stackSizeInBytes),
        excludeBuild = excludes,
        libOnly = !buildApps,
        stableTypeId = T,
        save = None(),
        load = None(),
        cmakeIncludes = cmakeIncludes
      )
    val st = transpilerX(transpilerOptions, binDir)

    return (st, transpilerOptions)
  }

  @pure def transpilerX(opts: SireumSlangTranspilersCOption,
                        binDir: String): ST = {
    val script_home = s"$${${SCRIPT_HOME}}"

    val projHomesRel = opts.sourcepath.map((s: String) => Util.relativizePaths(binDir, s, script_home))

    val cOutputDirRel = Util.relativizePaths(binDir, opts.output.get, script_home)

    val cmakeIncludesRel: ISZ[String] = opts.cmakeIncludes.map((s: String) => {
      val opss = ops.StringOps(s)
      val (prefix, nonplussed): (String, String) = if (ops.StringOps(s).startsWith("+")) {
        ("+", opss.substring(1, opss.size))
      } else {
        ("", s)
      }

      s"${prefix}${Util.relativizePaths(binDir, nonplussed, script_home)}"
    })

    val path_sep = s"$${PATH_SEP}"

    def slashExpand(optionName: String, elems: ISZ[String]): Option[ST] = {
      return if (elems.nonEmpty) Some(st""""--${optionName}", s"${(elems, ";")}",""")
      else None()
    }

    val slash =
      st"""val ${opts.projectName.get}: ISZ[String] = ISZ(
          |  "--sourcepath", s"${(projHomesRel, path_sep)}",
          |  "--output-dir", s"${cOutputDirRel}",
          |  "--name", "${opts.projectName.get}",
          |  "--apps", "${(opts.apps, ",")}",
          |  "--fingerprint", "${opts.fingerprint}",
          |  "--bits", "${opts.bitWidth}",
          |  "--string-size", "${opts.maxStringSize}",
          |  "--sequence-size", "${opts.maxArraySize}",
          |  ${slashExpand("sequence", opts.customArraySizes)}
          |  ${slashExpand("constants", opts.customConstants)}
          |  ${slashExpand("cmake-includes", cmakeIncludesRel)}
          |  "--forward", "${(opts.forwarding, ",")}",
          |  "--stack-size", "${opts.stackSize.get}",
          |  "--stable-type-id""""

    var slashExtras: ISZ[ST] = ISZ()

    if (opts.exts.nonEmpty) {
      val extsRel = opts.exts.map((s: String) => Util.relativizePaths(binDir, s, script_home))

      slashExtras = slashExtras :+
        st""",
            |  "--exts", s"${(extsRel, path_sep)}""""
    }

    if (opts.excludeBuild.nonEmpty) {
      slashExtras = slashExtras :+
        st""",
            |  "--exclude-build", "${(opts.excludeBuild, ",")}""""
    }

    if (opts.libOnly) {
      slashExtras = slashExtras :+
        st""",
            |  "--lib-only""""
    }

    if (opts.verbose) {
      slashExtras = slashExtras :+
        st""",
            |  "--verbose""""
    }

    val slashRet = st"""${slash}${(slashExtras, "")})"""
    return slashRet
  }

  def transpilerSel4Preamble(entries: ISZ[(String, ST)]): ST = {

    val ret: ST =
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
          |${CommentTemplate.doNotEditComment_scala}
          |
          |val SCRIPT_HOME: Os.Path = Os.slashDir
          |val PATH_SEP: String = Os.pathSep
          |
          |${(entries.map((m: (String, ST)) => m._2), "\n\n")}
          |
          |val projects: ISZ[ISZ[String]] = ISZ(
          |  ${(entries.map((m: (String, ST)) => m._1), ",\n")}
          |)
          |
          |println("Initializing runtime library ...")
          |Sireum.initRuntimeLibrary()
          |
          |var result = 0
          |for(p <- projects if result == 0) {
          |  result = Sireum.run(ISZ[String]("slang", "transpilers", "c") ++ p)
          |}
          |
          |//ops.ISZOps(projects).parMap(p =>
          |//  Sireum.run(ISZ[String]("slang", "transpilers", "c") ++ p)
          |//)
          |
          |Os.exit(result)
          |"""
    return ret
  }

  def transpilerSlashScriptPreamble(legacy: ST, demo: ST, preBlocks: ISZ[ST]): ST = {
    val preBlocksOpt: Option[ST] =
      if(preBlocks.isEmpty) None()
      else Some(st"""
                    |${(preBlocks, "\n\n")}""")

    val ret: ST =
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
          |${CommentTemplate.doNotEditComment_scala}
          |$preBlocksOpt
          |
          |val SCRIPT_HOME: Os.Path = Os.slashDir
          |val PATH_SEP: String = Os.pathSep
          |
          |var project: ISZ[String] = Cli(Os.pathSepChar).parseTranspile(Os.cliArgs, 0) match {
          |  case Some(o: Cli.TranspileOption) =>
          |    if(o.legacy) {
          |      println("Using Legacy Scheduler")
          |
          |      ${legacy}
          |      main
          |    } else {
          |      ${demo}
          |      main
          |    }
          |  case Some(o: Cli.HelpOption) =>
          |    Os.exit(0);
          |    halt("")
          |  case _ =>
          |    eprintln("Could not recognize arguments")
          |    Os.exit(-1)
          |    halt("")
          |}
          |
          |println("Initializing runtime library ...")
          |Sireum.initRuntimeLibrary()
          |
          |val result = Sireum.run(ISZ[String]("slang", "transpilers", "c") ++ project)
          |
          |Os.exit(result)
          |
          |${ArsitLibrary.getTranspileSlashCli}
          |"""
    return ret
  }
}
