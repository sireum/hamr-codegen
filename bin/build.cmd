::/*#! 2> /dev/null                                                                                         #
@ 2>/dev/null # 2>nul & echo off & goto BOF                                                                 #
export SIREUM_HOME=$(cd -P $(dirname "$0")/.. && pwd -P)                                                    #
if [ ! -z ${SIREUM_PROVIDED_SCALA++} ]; then                                                                #
  SIREUM_PROVIDED_JAVA=true                                                                                 #
fi                                                                                                          #
"${SIREUM_HOME}/bin/init.sh" || exit $?                                                                     #
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then                                                                 #
  export SIREUM_HOME=$(cygpath -C OEM -w -a ${SIREUM_HOME})                                                 #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/win/java":"${SIREUM_HOME}/bin/win/z3":"$PATH"                           #
    export PATH="$(cygpath -C OEM -w -a ${JAVA_HOME}/bin)":"$(cygpath -C OEM -w -a ${Z3_HOME}/bin)":"$PATH" #
  fi                                                                                                        #
elif [ "$(uname)" = "Darwin" ]; then                                                                        #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/mac/java/bin":"${SIREUM_HOME}/bin/mac/z3/bin":"$PATH"                   #
  fi                                                                                                        #
elif [ "$(expr substr $(uname -s) 1 5)" = "Linux" ]; then                                                   #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    if [ "$(uname -m)" = "aarch64" ]; then                                                                  #
      export PATH="${SIREUM_HOME}/bin/linux/arm/java/bin":"$PATH"                                           #
    else                                                                                                    #
      export PATH="${SIREUM_HOME}/bin/linux/java/bin":"${SIREUM_HOME}/bin/linux/z3/bin":"$PATH"             #
    fi                                                                                                      #
  fi                                                                                                        #
fi                                                                                                          #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then                                                           #
  exec "$0.com" "$@"                                                                                        #
else                                                                                                        #
  rm -fR "$0.com"                                                                                           #
  exec "${SIREUM_HOME}/bin/sireum" slang run "$0" "$@"                                                      #
fi                                                                                                          #
:BOF
setlocal
set SIREUM_HOME=%~dp0../
call "%~dp0init.bat" || exit /B %errorlevel%
if defined SIREUM_PROVIDED_SCALA set SIREUM_PROVIDED_JAVA=true
if not defined SIREUM_PROVIDED_JAVA set PATH=%~dp0win\java\bin;%~dp0win\z3\bin;%PATH%
set NEWER=False
if exist %~dpnx0.com for /f %%i in ('powershell -noprofile -executionpolicy bypass -command "(Get-Item %~dpnx0.com).LastWriteTime -gt (Get-Item %~dpnx0).LastWriteTime"') do @set NEWER=%%i
if "%NEWER%" == "True" goto native
del "%~dpnx0.com" > nul 2>&1
"%~dp0sireum.bat" slang run "%0" %*
exit /B %errorlevel%
:native
%~dpnx0.com %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.cli.CliOpt
import org.sireum.cli.CliOpt.Type

val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireumHome: Os.Path = {
  if ((home.up.up / "bin" / "sireum.jar").exists) { // use kekinian's sireum.jar if it exists
    home.up.up
  } else {
    Os.sireumHomeOpt.get // this will use the environment's sireum.jar if this script is called via 'sireum slang run'
  }
}
val sireum: Os.Path = sireumHome / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")
val appDir: Os.Path = home / "bin" / (if (Os.isMac) "mac" else if (Os.isWin) "win" else "linux")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project-standalone.cmd"

if (Os.cliArgs.isEmpty) {
  usage()
  Os.exit(0)
}

def clone(repo: String, proj: String, location: Option[String]): B = {
  val loc: Os.Path = location match {
    case Some(l) => home / l
    case _ => home / proj
  }
  val ret: B = if (!loc.exists) {
    val args = ISZ[String]("git", "clone", "--recurse", s"${repo}/$proj") ++ (if (location.nonEmpty) ISZ(location.get) else ISZ[String]())
    Os.proc(args).at(home).console.timeout(10000).run().ok
  } else {
    Os.proc(ISZ("git", "pull")).at(loc).console.run().ok
  }
  return ret
}

def cloneProjects(): Unit = {
  ISZ[String]("air", "runtime", "slang", "parser").foreach((p: String) => {
    clone("https://github.com/sireum", p, None()); println()
  })
}

def tipe(): Unit = {
  println("Slang type checking ...")
  Os.proc(ISZ(sireum.string, "proyek", "tipe", "--project", project.string, "--par", "--strict-aliasing", home.string)).
    at(home).console.runCheck()
  println()
}


def compile(): Unit = {
  tipe()

  println("Compiling ...")
  proc"$sireum proyek compile --project $project -n $proyekName --par --sha3 .".at(home).console.runCheck()
  println()
}

/*
def setupGumboTesting(): B = {
  val extTestRepos: ISZ[(String, String, String)] = ISZ(
    ("git@gitlab-ext.galois.com:sirfur", "sireum-osate-tests.git", "jvm/src/test-ext/gumbo"),
    ("git@gitlab-ext.galois.com:sirfur", "sirfur_omnibus.git", "jvm/src/test-ext/gumbo/resources/models/sirfur_omnibus"),
  )

  var success: B = T
  for (c <- extTestRepos if success && !(home / c._3).exists) {
    success = clone(c._1, c._2, Some(c._3)) // may fail (e.g. when run via github actions)
  }
  if (!success) {
    cprintln(T, "Failed to clone Adventium repos via SSH")
  }

  return success
}
*/

def test(options: ISZ[String]): Unit = {

  //setupGumboTesting()

  tipe()

  val opts: String = if (options.nonEmpty) st"${(options, " ")}".render else ""

  val wildcardPackages: String = (if (ops.ISZOps(options).contains("--packages")) "" else "org.sireum.hamr.codegen")

  println(s"Testing ${if(options.nonEmpty) s"with $opts " else ""}...")

  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 $opts . $wildcardPackages".at(home).console.runCheck()
  println()
}

def regenTransformers(): Unit = {
  val symbolPackagePath = home / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "common" / "symbols"

  val asts: ISZ[String] = ISZ("AadlSymbols.scala", "BTSSymbols.scala").map(m => (symbolPackagePath / m).value)

  val license = home / "license.txt"

  Os.proc(ISZ[String](sireum.value, "tools", "trafo", "-l", license.value,
    "-m", "immutable,mutable") ++ asts).at(symbolPackagePath).console.runCheck()
}

/** Ensures SIREUM_HOME/bin/sireum.jar is being used rather than the downloaded version in
*   <hamr-dir>/bin/sireum.jar
*/
def symLinkSireumJar(): Unit = {
  val sireumjar = home.up.up / "bin" / "sireum.jar"
  val bootstrapSireumjar = homeBin / "sireum.jar"
  if (sireumjar.exists) {
    if (!bootstrapSireumjar.isSymLink) {
      bootstrapSireumjar.remove()
      bootstrapSireumjar.mklink(sireumjar)
      println(
        st"""Replaced bootstrapping sireum.jar with $sireumjar.
            |Please rerun the last task.""".render)
    }
  } else {
    println(
      st"""This task requires build.cmd be run using $sireumjar
          |rather than the bootstrapping version. Please clone the kekinian repo and try again.""".render)
    Os.exit(1)
  }
}

def regenReportArtifacts(): Unit = {
  val sharedRoot = home / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen"
  val reportContainers: ISZ[Os.Path] = ISZ(
    sharedRoot / "common" / "reporting" / "CodegenReport.scala",
    sharedRoot / "microkit" / "reporting" / "MicrokitReport.scala"
  )
  proc"$sireum tool sergen -p org.sireum.hamr.codegen.common.reporting ${st"${(reportContainers, " ")}".render}".at(sharedRoot / "common" / "reporting").console.runCheck()
}

def regenClis(): Unit = {
  symLinkSireumJar()

  val utilDir = home / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "common" / "util"
  // NOTE: cliJson.sc emits what's in $SIREUM_HOME/bin/sireum.jar's version of
  //       hamr's cli so regen that first, rebuild sireum.jar, then call this method
  proc"${sireum} tools cligen -p org.sireum.hamr.codegen.common.util -n HamrCli -o ${utilDir.value} ${(utilDir / "cliJson.sc")}".console.runCheck()


  def process(tool: CliOpt.Tool, packageName: String, outputKeyUtil: Os.Path, optionName: String): Unit = {

    var shorts: ISZ[ST] = ISZ()
    var longs: ISZ[ST] = ISZ()
    var allLongs: ISZ[ST] = ISZ()
    var allShorts: ISZ[ST] = ISZ()
    var toStrings: ISZ[ST] = ISZ()

    def addOptions(opts: ISZ[org.sireum.cli.CliOpt.Opt], optGroup: String): Unit = {
      for (o <- opts) {
        val longKey = s"--${o.longKey}"
        @strictpure def sanitize(s: String): ST =
          st"${ops.StringOps(s).replaceAllLiterally("/", "_")}"
        val id = sanitize(s"$optGroup${o.name}")
        longs = longs :+ st"""val $id: String = "$longKey""""
        allLongs = allLongs :+ id
        o.tpe match {
          case f: Type.Flag =>
            toStrings = toStrings :+
              st"""if (o.${o.name}) {
                  |  ret = ret :+ ("$longKey", None())
                  |}"""
          case n: Type.Num =>
            toStrings = toStrings :+
              st"""if (includeDefaults || o.${o.name} != ${n.default}) {
                  |  ret = ret :+ ("$longKey", Some(o.${o.name}.string))
                  |}"""
          case nf: Type.NumFlag =>
            halt(s"todo: $nf")
          case nc: Type.NumChoice =>
            val default = nc.choices(0)
            toStrings = toStrings :+
              st"""if (includeDefaults || o.${o.name} != $default) {
                  |  ret = ret :+ ("$longKey", Some(o.${o.name}.string))
                  |}"""
          case s: Type.Str =>
            val includeDefault: Option[String] =
              if (s.default.nonEmpty) Some("includeDefaults || ")
              else None()
            val extract: String =
              if (s.sep.nonEmpty) st"""st"$${(o.${o.name}, "${s.sep.get}")}".render""".render
              else s"o.${o.name}.get"
            toStrings = toStrings :+
              st"""if (${includeDefault}o.${o.name}.nonEmpty) {
                  |  ret = ret :+ ("$longKey", Some($extract))
                  |}"""
          case c: Type.Choice =>
            val default = c.elements(0)
            toStrings = toStrings :+
              st"""if (includeDefaults || o.${o.name}.name != "$default") {
                  |  ret = ret :+ ("$longKey", Some(o.${o.name}.name))
                  }"""
          case p: Type.Path =>
            if(p.default.nonEmpty) {
              halt("todo: handle path default")
            }
            toStrings = toStrings :+
              st"""if (o.${o.name}.nonEmpty) {
                  |  ret = ret :+ ("$longKey", Some(st"$${(o.${o.name}, Os.pathSep)}".render))
                  |}"""
        }
        if (o.shortKey.nonEmpty) {
          shorts = shorts :+ st"""val $id: String = "-${o.shortKey.get}""""
          allShorts = allShorts :+ id
        }
      }
    }
    addOptions(tool.opts, "")
    for (g <- tool.groups) {
      addOptions(g.opts, s"${g.name}_")
    }

    val allLongKeysEntries: ISZ[ST] = for (l <- allLongs) yield st".$$colon$$plus(new org.sireum.String($packageName.LongKeys.$l()))"
    val allShortKeysEntries: ISZ[ST] = for (l <- allShorts) yield st".$$colon$$plus(new org.sireum.String($packageName.ShortKeys.$l()))"

    val content =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |
          |// the following can be used when constructing command line arguments for ${tool.name}
          |// from an external tool (e.g. OSATE).
          |
          |object LongKeys {
          |  ${(longs, "\n")}
          |
          |  val allKeys: Set[String] = Set.empty[String] ++ ISZ(${(allLongs, ", ")})
          |
          |  @strictpure def sameKeys(keys: ISZ[String]): B = allKeys.elements == keys
          |
          |  // Paste the following into a java program if you want to ensure the known keys match.
          |  // To regenerate this, run '$$SIREUM_HOME/hamr/codegen/build.cmd regen-cli'.
          |  // If this does fail then the CLI arguments being constructed for codegen will need
          |  // to be updated (that could be delayed if only new options were added).
          |
          |  // scala.collection.Seq<org.sireum.String> seq = scala.jdk.javaapi.CollectionConverters.asScala(new java.util.ArrayList<org.sireum.String>());
          |  // scala.collection.immutable.Seq<org.sireum.String> iseq = ((scala.collection.IterableOnceOps<org.sireum.String, ?, ?>) seq).toSeq();
          |  // org.sireum.IS<org.sireum.Z, org.sireum.String> knownKeys = org.sireum.IS$$.MODULE$$.apply(iseq, org.sireum.Z$$.MODULE$$)${(allLongKeysEntries, "")};
          |  // boolean sameKeys = org.sireum.hamr.codegen.LongKeys.sameKeys(knownKeys);
          |
          |}
          |
          |object ShortKeys {
          |  ${(shorts, "\n")}
          |
          |  val allKeys: Set[String] = Set.empty[String] ++ ISZ(${(allShorts, ", ")})
          |
          |  @strictpure def sameKeys(keys: ISZ[String]): B = allKeys.elements == keys
          |
          |  // Paste the following into a java program if you want to ensure the known keys match.
          |  // To regenerate this, run '$$SIREUM_HOME/hamr/codegen/build.cmd regen-cli'.
          |  // If this does fail then the CLI arguments being constructed for codegen will need
          |  // to be updated (that could be delayed if only new options were added).
          |
          |  // scala.collection.Seq<org.sireum.String> seq = scala.jdk.javaapi.CollectionConverters.asScala(new java.util.ArrayList<org.sireum.String>());
          |  // scala.collection.immutable.Seq<org.sireum.String> iseq = ((scala.collection.IterableOnceOps<org.sireum.String, ?, ?>) seq).toSeq();
          |  // org.sireum.IS<org.sireum.Z, org.sireum.String> knownKeys = org.sireum.IS$$.MODULE$$.apply(iseq, org.sireum.Z$$.MODULE$$)${(allShortKeysEntries, "")};
          |  // boolean sameKeys = org.sireum.hamr.codegen.ShortKeys.sameKeys(knownKeys);
          |}
          |"""
    outputKeyUtil.writeOver(content.render)
    println(s"Wrote: $outputKeyUtil")

    val hamrCli = home.up.up / "cli"/ "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "cli" / "HAMR.scala"

    val cliContent =
      st"""
          |// Autogenerated via ${(home / "bin" / "build.cmd").value} --regen-cli
          |
          |object ${optionName}Util {
          |  def optionsToString(o: Cli.$optionName, includeDefaults: B): String = {
          |    val ops: ISZ[ST] = for(e <- optionsToStringH(o, includeDefaults)) yield st"$${e._1}$${if (e._2.isEmpty) "" else s" $${e._2.get}"}"
          |    return st"$${(ops, " ")}".render
          |  }
          |
          |  def optionsToStringH(o : Cli.$optionName, includeDefaults: B): ISZ[(String, Option[String])] = {
          |    var ret: ISZ[(String, Option[String])] = ISZ()
          |    ${(toStrings, "\n")}
          |    return ret
          |  }
          |}"""

    val hamrCliContent = ops.StringOps(hamrCli.read)
    val start = hamrCliContent.stringIndexOf(s"// BEGIN $optionName")
    val endText: String = s"// END $optionName"
    val end = hamrCliContent.stringIndexOf(endText)
    if (start < 0 || end < 0) {
      halt(s"Couldn't find BEGIN/END markers for $optionName")
    }
    hamrCli.writeOver(
      st"""${hamrCliContent.substring(0, start - 1)}
        |// BEGIN $optionName
        |$cliContent
        |// END $optionName
        |${hamrCliContent.substring(end + endText.size + 1, hamrCliContent.size)}""".render)
    println(s"Modified: $hamrCli")
  }

  // Ignore the Tipe error "'hamr' is not a member of package 'org.sireum'"
  process(
    tool = org.sireum.hamr.codegen.HamrCodegenCli.codeGenTool,
    packageName = "org.sireum.hamr.codegen",
    outputKeyUtil = home / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "CliKeys.scala",
    optionName =  "SireumHamrCodegenOption"
  )

  process(
    tool = org.sireum.hamr.sysml.cli.sysmlCodegen,
    packageName = "org.sireum.hamr.sysml",
    outputKeyUtil = home.up / "sysml"/ "frontend" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "sysml" / "CliKeys.scala",
    optionName = "SireumHamrSysmlCodegenOption"
  )
}

def isCI(): B = {
  return Os.env("GITLAB_CI").nonEmpty || Os.env("GITHUB_ACTIONS").nonEmpty || Os.env("BUILD_ID").nonEmpty
}

def installOsateGumbo(): B = {
  val versions = (home / "jvm" / "src" / "main" / "resources" / "phantom_versions.properties").properties

  val hamrJar = s"org.sireum.aadl.osate.hamr_${versions.get("org.sireum.aadl.osate.plugins.version_alt").get}.jar"
  val gumboJar = s"org.sireum.aadl.gumbo_${versions.get("org.sireum.aadl.gumbo.plugins.version_alt").get}.jar"

  val osateDir: Os.Path = {
    Os.env("OSATE_HOME") match {
      case Some(s) => Os.path(s)
      case _ => appDir / s"osate${if (Os.isMac) ".app" else ""}"
    }
  }

  val pluginsDir: Os.Path =
    if (Os.isMac) osateDir / "Contents" / "Eclipse" / "plugins"
    else osateDir / "plugins"

  var alreadyInstalled = F
  if (pluginsDir.exists) {
    val files = ops.ISZOps(pluginsDir.list.map((p: Os.Path) => p.name))
    alreadyInstalled = files.contains(hamrJar) && files.contains(gumboJar)
  }

  if (alreadyInstalled) {
    println(s"OSATE already up to date: $osateDir\n")
    return T
  } else {
    println("Installing Sireum plugins into OSATE, this will take a while ...")
    val result = proc"$sireum hamr phantom -u -o ${osateDir.value}".console.run()
    if (result.ok) {
      println(s"OSATE installed at ${osateDir}")
    } else {
      eprintln(result.err)
    }
    println()
    return result.ok
  }
}

def installSbtMill(): Unit = {
  val sbtBin = appDir / "sbt" / "bin" / (if (Os.isWin) "sbt.bat" else "sbt")
  if (!sbtBin.exists) {
    val versions = (home / "jvm" / "src" / "main" / "resources" / "codegen_versions.properties").properties
    val sbtV = versions.get("org.sireum.version.sbt").get
    (appDir / "sbt").removeAll()
    val sbtZip = appDir / "sbt.zip"
    println("Downloading sbt ...")
    sbtZip.downloadFrom(s"https://github.com/sbt/sbt/releases/download/v${sbtV}/sbt-${sbtV}.zip")
    sbtZip.unzipTo(appDir)
    sbtZip.remove()
    sbtBin.chmod("+x")
    println()
  }
  val millBin = appDir / (if (Os.isWin) "mill.bat" else "mill")
  if (!millBin.exists) {
    println("Downloading mill ...")
    millBin.downloadFrom("https://github.com/sireum/rolling/releases/download/mill/standalone")
    millBin.chmod("+x")
    println()
  }
}

var continue = T
for (i <- 0 until Os.cliArgs.size if continue) {
  Os.cliArgs(i) match {
    case string"compile" =>
      installOsateGumbo()
      installSbtMill()
      cloneProjects()
      compile()
    case string"test" =>
      installOsateGumbo()
      installSbtMill()
      cloneProjects()
      test(ops.ISZOps(Os.cliArgs).slice(i + 1, Os.cliArgs.size))
      continue = F
    case string"tipe" =>
      cloneProjects()
      tipe()
    case string"regen-report" => regenReportArtifacts()
    case string"regen-trans" => regenTransformers()
    case string"regen-cli" => regenClis()
    //case string"fetch-gumbo" => setupGumboTesting()
    case string"install-osate-gumbo" => installOsateGumbo()
    case string"install-sbt-mill" => installSbtMill()
    case string"-h" => usage()
    case string"--help" => usage()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}

def usage(): Unit = {
  val testModes: ISZ[String] = {
    val content = (home / "jvm" / "src" / "test" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "test" / "util" / "TestMode.scala").readLines
    var capture = F
    var modes: ISZ[String] = ISZ()
    for (l <- content) {
      if (!capture && ops.StringOps(l).contains("@enum")) {
        capture = T
      } else if (capture && !ops.StringOps(l).contains("}")) {
        modes = modes :+ ops.StringOps(l).trim
      } else {
        capture = F
      }
    }
    modes
  }
  println("HAMR Codegen /build")
  println(
    st"""Usage: ( compile | test | tipe
        |       | regen-trans | regen-cli regen-report
        |       | install-osate-gumbo | install-sbt-mill )+
        |
        |
        |Anything after 'test' will be treated as test options. Invoke 'sireum proyek test' for its options, e.g.
        |
        |   bin/build.cmd test --suffixes Base --classes org.sireum.hamr.codegen.test.gumbo.GumboTest
        |
        |
        |Set test modes via 'testmodes' environment variable, e.g.
        |
        |  testmodes=phantom,ive,logika bin/build.cmd test
        |
        |  Available Test Modes:
        |    ${(testModes, "\n")}""".render)
}
