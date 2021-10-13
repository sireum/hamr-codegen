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
  println(
    st"""Usage: ( clean | compile | test | tipe | regen-trans
        |       | cvc4  | z3                                  )+""".render)
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

val versions = (home / "versions.properties").properties

val cache: Os.Path = Os.env("SIREUM_CACHE") match {
  case Some(p) =>
    val d = Os.path(p)
    if (!d.exists) {
      d.mkdirAll()
    }
    d
  case _ => Os.home / "Downloads" / "sireum"
}

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


def platformKind(kind: Os.Kind.Type): String = {
  kind match {
    case Os.Kind.Win => return "win"
    case Os.Kind.Linux => return "linux"
    case Os.Kind.LinuxArm => return "linux/arm"
    case Os.Kind.Mac => return "mac"
    case _ => return "unsupported"
  }
}

def installZ3(kind: Os.Kind.Type): Unit = {
  val version = versions.get("org.sireum.version.z3").get
  val dir = homeBin / platformKind(kind) / "z3"
  val ver = dir / "VER"

  if (ver.exists && ver.read == version) {
    return
  }

  val filename: String = kind match {
    case Os.Kind.Win => s"z3-$version-x64-win.zip"
    case Os.Kind.Linux => s"z3-$version-x64-glibc-2.31.zip"
    case Os.Kind.Mac => s"z3-$version-x64-osx-10.15.7.zip"
    case _ => return
  }

  val bundle = cache / filename

  if (!bundle.exists) {
    println(s"Please wait while downloading Z3 $version ...")
    bundle.up.mkdirAll()
    bundle.downloadFrom(s"https://github.com/Z3Prover/z3/releases/download/z3-$version/$filename")
  }

  println("Extracting Z3 ...")
  bundle.unzipTo(dir.up)
  println()

  for (p <- dir.up.list if ops.StringOps(p.name).startsWith("z3-")) {
    dir.removeAll()
    p.moveTo(dir)
  }

  kind match {
    case Os.Kind.Linux => (dir / "bin" / "z3").chmod("+x")
    case Os.Kind.Mac => (dir / "bin" / "z3").chmod("+x")
    case _ =>
  }

  ver.writeOver(version)
}

def installCVC4(kind: Os.Kind.Type): Unit = {
  val version = versions.get("org.sireum.version.cvc4").get
  val exe = homeBin / platformKind(kind) / (if (kind == Os.Kind.Win) "cvc4.exe" else "cvc4")
  val ver = homeBin / platformKind(kind) / ".cvc4.ver"

  if (ver.exists && ver.read == version) {
    return
  }

  val filename: String = kind match {
    case Os.Kind.Win => s"cvc4-$version-win64-opt.exe"
    case Os.Kind.Linux => s"cvc4-$version-x86_64-linux-opt"
    case Os.Kind.Mac => s"cvc4-$version-macos-opt"
    case _ => return
  }

  val drop = cache / filename

  if (!drop.exists) {
    println(s"Please wait while downloading CVC4 $version ...")
    drop.up.mkdirAll()
    drop.downloadFrom(s"https://github.com/cvc5/cvc5/releases/download/$version/$filename")
  }

  drop.copyOverTo(exe)
  println()

  kind match {
    case Os.Kind.Linux => exe.chmod("+x")
    case Os.Kind.Mac => exe.chmod("+x")
    case _ =>
  }

  ver.writeOver(version)
}

def getIVE(): B = {

  val (suffix, os): (String, String) = {
    if (Os.isWin) ("sireum-dev-win.sfx", "win")
    else if (Os.isMac) ("sireum-dev-mac.sfx", "mac")
    else if (Os.isLinux) ("sireum-dev-linux.sfx", "linux")
    else halt("Os not supported")
  }

  val destDir = homeBin / os / "idea"
  val ideaDir =
    if (Os.envs.contains("GITHUB_ACTIONS")) homeBin / "Sireum-dev" / "bin" / os / "idea"
    else Os.home / "Applications" / "Sireum-dev" / "bin" / os / "idea"

  if(!destDir.exists) {
    if (!ideaDir.exists) {
      val repo = GitHub.repo("sireum", "kekinian")
      val latest = repo.releases.head

      val candidates = latest.assets.filter(asset => ops.StringOps(asset.getUrl).endsWith(suffix))
      assert(candidates.count() == 1, s"hmm, so many ${candidates.count()}")

      val sfx = homeBin / suffix
      val url = candidates.head.getUrl
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

def test(): Unit = {
  assert(getIVE(), "IVE doesn't exist")

  installZ3(Os.kind)
  installCVC4(Os.kind)

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
    case string"cvc4" => installCVC4(Os.kind)
    case string"z3" => installZ3(Os.kind)
    case string"-h" => usage()
    case string"--help" => usage()
    case cmd =>
      usage()
      eprintln(s"Unrecognized command: $cmd")
      Os.exit(-1)
  }
}
