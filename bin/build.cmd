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
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                   #
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
"%~dp0sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
:native
%~dpnx0.com %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._

val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val appDir: Os.Path = homeBin / (if (Os.isMac) "mac" else if (Os.isWin) "win" else "linux")
val sireum: Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project4testing.cmd"

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
  ISZ[String]("air", "runtime", "slang").foreach((p: String) => {
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

def test(options: ISZ[String]): Unit = {

  setupGumboTesting()

  tipe()

  val opts: String = if (options.nonEmpty) st"${(options, " ")}".render else ""

  val wildcardPackages: String = (if (ops.ISZOps(options).contains("--packages")) "" else "org.sireum.hamr.codegen")

  println(s"Testing ${if(options.nonEmpty) s"with $opts " else ""}...")

  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 $opts . $wildcardPackages".at(home).console.runCheck()
  println()
}

def regenTransformers(): Unit = {
  val commonRootPath = home / "common"
  val symbolPackagePath = commonRootPath / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "common" / "symbols"

  val asts: ISZ[String] = ISZ("AadlSymbols.scala", "BTSSymbols.scala").map(m => (symbolPackagePath / m).value)

  val license = home / "license.txt"

  Os.proc(ISZ[String](sireum.value, "tools", "trafo", "-l", license.value,
    "-m", "immutable,mutable") ++ asts).at(symbolPackagePath).console.runCheck()
}

def regenCli4Testing(): Unit = {
  val cliPackagePath = home / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen"
  val utilDir = home / "jvm" / "src" / "test" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "test" / "util"
  proc"${sireum} tools cligen -p org.sireum.hamr.codegen.test.util -o ${utilDir.value} ${(utilDir / "testingCli.sc")}".at(cliPackagePath).console.runCheck()
}

def isCI(): B = {
  return Os.env("GITLAB_CI").nonEmpty || Os.env("GITHUB_ACTIONS").nonEmpty || Os.env("BUILD_ID").nonEmpty
}

def installOsateGumbo(): B = {
  val versions = (home / "jvm" / "src" / "main" / "resources" / "phantom_versions.properties").properties

  val hamrJar = s"org.sireum.aadl.osate.hamr_${versions.get("org.sireum.aadl.osate.plugins.version_alt").get}.jar"
  val gumboJar = s"org.sireum.aadl.gumbo_${versions.get("org.sireum.aadl.gumbo.plugins.version_alt").get}.jar"

  val osateDir = appDir / s"osate${if (Os.isMac) ".app" else ""}"
  val pluginsDir: Os.Path =
    if (Os.isMac) osateDir / "Contents" / "Eclipse" / "plugins"
    else osateDir / "plugins"

  var alreadyInstalled = F
  if (pluginsDir.exists) {
    val files = ops.ISZOps(pluginsDir.list.map((p: Os.Path) => p.name))
    alreadyInstalled = files.contains(hamrJar) && files.contains(gumboJar)
  }

  if (alreadyInstalled) {
    println("OSATE already up to date.\n")
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

Os.env("GITHUB_WORKFLOW") match {
  case Some(n) if (ops.StringOps(ops.StringOps(n).toLower).contains("camkes")) =>
    val ver = (home / "versions.properties").properties.get("org.sireum.version.z3").get
    println(s"z3 4.12+ requires glibc 2.35 but the camkes container has 2.31 so installing z3 4.11.2 rather than ${ver}")
    proc"sed -i s/org.sireum.version.z3=${ver}/org.sireum.version.z3=4.11.2/g ${home / "versions.properties"}".runCheck()
    proc"cat ${home / "versions.properties"}".console.runCheck()
  case _ =>
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
    case string"regen-trans" => regenTransformers()
    case string"regen-cli" => regenCli4Testing()
    case string"fetch-gumbo" => setupGumboTesting()
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
        |       | regen-trans | fetch-gumbo
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
