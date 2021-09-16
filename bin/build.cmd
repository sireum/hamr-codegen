::#! 2> /dev/null                                                                                           #
@ 2>/dev/null # 2>nul & echo off & goto BOF                                                                 #
export SIREUM_HOME=$(cd -P $(dirname "$0")/.. && pwd -P)                                                    #
if [ ! -z ${SIREUM_PROVIDED_SCALA++} ]; then                                                                #
  SIREUM_PROVIDED_JAVA=true                                                                                 #
fi                                                                                                          #
"${SIREUM_HOME}/bin/init.sh"                                                                                #
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
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                #
fi                                                                                                          #
:BOF
setlocal
set SIREUM_HOME=%~dp0../
call "%~dp0init.bat"
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
::!#
// #Sireum
import org.sireum._

def usage(): Unit = {
  println("HAMR Codegen /build")
  println("Usage: ( clean | compile | test | tipe | regen-trans )+")
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

def clone(repo: String): Unit = {
  if (!(home / repo).exists) {
    Os.proc(ISZ("git", "clone", "--depth=1", s"https://github.com/sireum/$repo")).at(home).console.runCheck()
  } else {
    Os.proc(ISZ("git", "pull")).at(home / repo).console.runCheck()
  }
  println()
}

def cloneProjects(): Unit = {
  ISZ[String]("air", "runtime").foreach((p: String) => clone(p))
}

def tipe(): Unit = {
  println("Slang type checking ...")
  val excludes = "arsit/resources,jvm/src/test/result"
  Os.proc(ISZ(sireum.string, "slang", "tipe", "--verbose", "-r", "-s", home.string, "-x", excludes)).
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
  // need the IVE if doing 'proyek ive'
  val (suffix, os): (String, String) = {
    if (Os.isWin) ("/sireum-dev-win.sfx", "win")
    else if (Os.isMac) ("/sireum-dev-mac.sfx", "mac")
    else if (Os.isLinux) ("/sireum-dev-linux.sfx", "linux")
    else halt("Os not supported")
  }

  val destDir = homeBin / os / "idea"
  val ideaDir = Os.home / "Applications" / "Sireum-dev" / "bin" / os / "idea"

  if(!destDir.exists) {
    if (!ideaDir.exists) {
      val releases = home / "releases.json"

      //releases.downloadFrom("https://api.github.com/repos/sireum/kekinian/releases")
      val o = ops.StringOps(releases.read)
      val pos = o.stringIndexOf(suffix)
      val start = o.stringIndexOfFrom("browser_download_url", pos - 150) + 23
      val url = o.substring(start, pos + suffix.size)
      releases.removeAll()

      println(s"Downloading ${url}")
      val sfx = homeBin / suffix
      sfx.downloadFrom(url)

      println(s"Unzipping ${sfx}")
      sfx.chmod("700")
      proc"${sfx.string} -y".console.runCheck()
      sfx.removeAll()
    }
    destDir.mkdirAll()
    println(s"Sym-linking ${destDir} to ${ideaDir}")
    (destDir).mklink(ideaDir)
  }
  return destDir.exists
}

def getScalac(): B = {
  // need to make lib dir has the plugin version that HAMR sticks into version.properties
  val x = home / "arsit" / "resources" / "util" / "buildSbt.properties"
  val SCALAC_PLUGIN_VER = x.properties.get("org.sireum.version.scalac-plugin").get
  val jarLoc = home / "lib" / s"scalac-plugin-${SCALAC_PLUGIN_VER}.jar"
  if(!jarLoc.exists) {
    val url = s"https://github.com/sireum/scalac-plugin/releases/download/${SCALAC_PLUGIN_VER}/scalac-plugin-${SCALAC_PLUGIN_VER}.jar"
    println(s"Downloading ${url}")
    jarLoc.downloadFrom(url)
  }
  return jarLoc.exists
}

def test(): Unit = {
  assert(getIVE(), "IVE doesn't exist")
  assert(getScalac(), "scalac plugin doesn't exist")

  tipe()

  println("Testing ...")
  //val names: String = "org.sireum.hamr.arsit"
  val names: String = "org.sireum.hamr"

  proc"$sireum proyek test --project $project -n $proyekName --par --sha3 . $names".at(home).console.runCheck()
  println()
}


def clean(): Unit = {
  println(s"Cleaning ${home}")
  val homeResources: ISZ[Os.Path] = ISZ("air", "lib", "out", "runtime", "versions.properties").map(m => home / m)
  val homeBinResources: ISZ[Os.Path] = ISZ("sireum.jar", "sireum", "scala", "linux", "win", "mac").map(m => homeBin / m)
  for(r <- (homeResources ++ homeBinResources) if r.exists ) {
    println(s"Deleting ${r}")
    r.removeAll()
  }
  ISZ[String]("act", "arsit", "art").foreach((subProj: String) => {
    val buildCmd = home / subProj / "bin" / "build.cmd"
    Os.proc(ISZ(buildCmd.canon.value, "clean")).console.run()
  })
}

def regenTransformers(): Unit = {
  val commonRootPath = home / "common"
  val symbolPackagePath = commonRootPath / "shared" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "codegen" / "common" / "symbols"

  val asts: ISZ[String] = ISZ("AadlSymbols.scala", "BTSSymbols.scala").map(m => (symbolPackagePath / m).value)

  val license = home / "license.txt"

  Os.proc(ISZ[String](sireum.value, "tools", "transgen", "-l", license.value,
    "-m", "immutable,mutable") ++ asts).at(symbolPackagePath).console.runCheck()
}

for (i <- 0 until Os.cliArgs.size) {
  Os.cliArgs(i) match {
    case string"clean" => clean()
    case string"compile" =>
      cloneProjects()
      compile()
    case string"test" =>
      cloneProjects()
      test()
    case string"tipe" =>
      cloneProjects()
      tipe()
    case string"regen-trans" =>
      regenTransformers()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}
