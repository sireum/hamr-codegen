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

def usage(): Unit = {
  println("HAMR Codegen /build")
  println(
    st"""Usage: ( compile | test | tipe | regen-trans | fetch-gumbo
        |       | osate-gumbo)+""".render)
}


if (Os.cliArgs.isEmpty) {
  usage()
  Os.exit(0)
}


val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireum : Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")

val proyekName: String = "sireum-proyek"
val project: Os.Path = homeBin / "project4testing.cmd"

def clone(repo: String, proj: String, location: Option[String]): B = {
  val loc: Os.Path = location match {
    case Some(l) => home / l
    case _ => home / proj
  }
  val ret: B = if (!loc.exists) {
    val args = ISZ[String]("git", "clone", "--recurse", s"${repo}/$proj") ++ (if (location.nonEmpty) ISZ(location.get) else ISZ[String]())
    Os.proc(args).at(home).console.run().ok
  } else {
    Os.proc(ISZ("git", "pull")).at(loc).console.run().ok
  }
  return ret
}

def cloneProjects(): Unit = {
  ISZ[String]("air", "runtime", "slang").foreach((p: String) => { clone("https://github.com/sireum", p, None()); println() })
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
  println(home)
  proc"$sireum proyek compile --project $project -n $proyekName --par --sha3 .".at(home).console.runCheck()
  println()
}


def getIVE(): B = {

  val (suffix, os): (String, String) = {
    val ret: (String, String) = if (Os.isWin) { ("sireum-dev-win.sfx", "win") }
    else if (Os.isMac) { ("sireum-dev-mac.sfx", "mac") }
    else if (Os.isLinux) { ("sireum-dev-linux.sfx", "linux") }
    else { halt("Os not supported") }
    ret
  }

  val destDir = homeBin / os / "idea"
  val ideaDir: Os.Path =
    if (Os.envs.contains("GITHUB_ACTIONS")) homeBin / "Sireum-dev" / "bin" / os / "idea"
    else Os.home / "Applications" / "Sireum-dev" / "bin" / os / "idea"

  if(!destDir.exists) {
    if (!ideaDir.exists) {
      val repo = GitHub.repo("sireum", "kekinian")
      val latest = repo.releases.head

      val candidates = latest.assets.filter((gasset: GitHub.Asset) => ops.StringOps(gasset.url).endsWith(suffix))
      assert(candidates.count() == 1, s"hmm, so many ${candidates.count()}")

      val sfx = homeBin / suffix
      val url = candidates.head.url
      println(s"Downloading ${url} to ${sfx}")
      sfx.downloadFrom(url)

      println(s"Unzipping ${sfx} to ${ideaDir.up.up.up.up}")
      proc"7z x -y -o${ideaDir.up.up.up.up} ${sfx}".console.run()
      sfx.removeAll()
    }
    destDir.mkdirAll()
    println(s"Sym-linking ${destDir} to ${ideaDir}")
    destDir.mklink(ideaDir)
  }
  return destDir.exists
}

def isCI(): B = {
  return Os.env("GITLAB_CI").nonEmpty || Os.env("GITHUB_ACTIONS").nonEmpty || Os.env("BUILD_ID").nonEmpty
}

def installOsateGumbo(): B = {
  println("Installing Sireum plugins into OSATE ...")
  val p = proc"$sireum hamr phantom -u"
  return p.console.run().ok
}

def setupGumboTesting(): B = {

  val firstTime = !(home / "jvm/src/test-ext/gumbo").exists || isCI()
  if(firstTime) {
    installOsateGumbo()
  }

  val extTestRepos: ISZ[(String, String, String)] = ISZ(
    ("git@gitlab.adventium.com:sirfur", "sireum-osate-tests.git", "jvm/src/test-ext/gumbo"),
    ("git@gitlab.adventium.com:sirfur", "sirfur_omnibus.git", "jvm/src/test-ext/gumbo/resources/models/sirfur_omnibus"),
  )

  var success: B = T
  for(c <- extTestRepos if success && !(home / c._3).exists) {
    success = clone(c._1, c._2, Some(c._3)) // may fail (e.g. when run via github actions)
  }
  if(!success) {
    cprintln(T, "Failed to clone Adventium repos via SSH")
  }

  return success
}

def test(): Unit = {
  assert(getIVE(), "IVE doesn't exist")

  setupGumboTesting()

  tipe()

  println("Testing ...")

  val names: String = Os.env("HAMR_TEST_PACKAGE_NAMES") match {
    case Some(packages) =>
      val _packages: ISZ[String] = ops.StringOps(packages).split((c: C) => c == ',')
      st"${(_packages, " ")}".render
    case _ => "org.sireum.hamr"
  }

  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 . $names".at(home).console.runCheck()
  println()
}

def regenTransformers(): Unit = {
  val commonRootPath = home / "common"
  val symbolPackagePath = commonRootPath / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "common" / "symbols"

  val asts: ISZ[String] = ISZ("AadlSymbols.scala", "BTSSymbols.scala").map(m => (symbolPackagePath / m).value)

  val license = home / "license.txt"

  Os.proc(ISZ[String](sireum.value, "tools", "transgen", "-l", license.value,
    "-m", "immutable,mutable") ++ asts).at(symbolPackagePath).console.runCheck()
}

def regenCli4Testing(): Unit = {
  val cliPackagePath = home / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen"
  val utilDir = home /  "jvm" / "src" / "test" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "test" / "util"
  proc"${sireum} tools cligen -p org.sireum.hamr.codegen.test.util -o ${utilDir.value} ${(utilDir / "testingCli.sc")}".at(cliPackagePath).console.runCheck()
}

def installToolsViaKekinian(): Unit = {
  val builtIn = home / "runtime" / "library" / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "BuiltInTypes.slang"
  if(!builtIn.exists) {
    builtIn.write(".")
  }
  val kbuild = homeBin / "kbuild.cmd"
  kbuild.downloadFrom("https://raw.githubusercontent.com/sireum/kekinian/master/bin/build.cmd")
  proc"$sireum slang run $kbuild --help".at(homeBin).runCheck()
  kbuild.remove()
  if(builtIn.size == 1) {
    (home / "runtime").removeAll()
  }
}

def installSbtMill(): Unit = {
  val sbtBin = homeBin / "sbt" / "bin" / (if (Os.isWin) "sbt.bat" else "sbt")
  if (!sbtBin.exists) {
    val versions = (home / "arsit" / "resources" / "util" / "buildSbt.properties").properties
    val sbtV = versions.get("org.sireum.version.sbt").get
    (homeBin / "sbt").removeAll()
    val sbtZip = homeBin / "sbt.zip"
    println("Downloading sbt ...")
    sbtZip.downloadFrom(s"https://github.com/sbt/sbt/releases/download/v${sbtV}/sbt-${sbtV}.zip")
    sbtZip.unzipTo(homeBin)
    sbtZip.remove()
    sbtBin.chmod("+x")
    println()
  }
  val millBin = homeBin / "mill"
  if (!millBin.exists) {
    println("Downloading mill ...")
    millBin.downloadFrom("https://github.com/sireum/rolling/releases/download/mill/standalone")
    millBin.chmod("+x")
    println()
  }
}

installToolsViaKekinian()
installSbtMill()

for (i <- 0 until Os.cliArgs.size) {
  Os.cliArgs(i) match {
    case string"compile" =>
      cloneProjects()
      compile()
    case string"test" =>
      cloneProjects()
      test()
    case string"tipe" =>
      cloneProjects()
      tipe()
    case string"regen-trans" => regenTransformers()
    case string"regen-cli" => regenCli4Testing()
    case string"fetch-gumbo" => setupGumboTesting()
    case string"osate-gumbo" => installOsateGumbo()
    case string"-h" => usage()
    case string"--help" => usage()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}
