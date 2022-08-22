::#! 2> /dev/null                                             #
@ 2>/dev/null # 2>nul & echo off & goto BOF                   #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then             #
  exec "$0.com" "$@"                                          #
fi                                                            #
rm -f "$0.com"                                                #
if [ -z ${SIREUM_HOME} ]; then                                #
  echo "Please set SIREUM_HOME env var"                       #
  exit -1                                                     #
fi                                                            #
exec ${SIREUM_HOME}/bin/sireum slang run -n "$0" "$@"      #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._

val fast: B = T

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / "sireum"
val sireumjar = SIREUM_HOME / "bin" / "sireum.jar"

val CASE_Env_home: Os.Path = Os.home / "devel" / "sireum" / "case-env"

// TODO get these from env or arguments
val pluginDir = Os.home / "devel" / "sireum" / "osate-plugin"
val updateSiteDir = Os.home / "devel" / "sireum" / "osate-update-site"
//val updateSiteDir = Os.home / "devel" / "sireum" / "osate-plugin-update-site"
//val updateSiteHAMRDir = Os.home / "devel" / "sireum" / "hamr-plugin-update-site"


def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

def getGitRepos(d: Os.Path): ISZ[Os.Path] = {
  assert(d.isDir)
  val children: ISZ[Os.Path] = d.list.filter(p => p.isDir && !(ops.StringOps(p.name).contains("-ext"))).flatMap(f => getGitRepos(f))
  if((d / ".git").exists) { return d +: children }
  else { return children }
}

var gitStaged: B = T
getGitRepos(SIREUM_HOME / "hamr" / "codegen").foreach((repo: Os.Path) => {
  val onMaster = proc"git -C ${repo} rev-parse --abbrev-ref HEAD".at(repo).runCheck()
  val uncommitedChanges = proc"git -C ${repo} status --porcelain".at(repo).runCheck()
  val unpushedCommits = proc"git -C ${repo} log origin/master..master".at(repo).runCheck()
  var ready: B = T
  if(ops.StringOps(onMaster.out).trim != "master"){
    eprintln(s"Not on master: ${repo} -- ${ops.StringOps(onMaster.out).trim}")
    ready = F
  }
  if(uncommitedChanges.out != "") {
    var justReleaseScriptMods: B = T
    for(e <- ops.StringOps(uncommitedChanges.out).split((c: C) => c == '\n') if !ops.StringOps(e).contains("bin/plugin_release")) {
      justReleaseScriptMods = F
    }
    if(!justReleaseScriptMods) {
      eprintln(s"Uncommited changes: ${repo}")
      println(uncommitedChanges.out)
      ready = F
    }
  }
  if(unpushedCommits.out != "") {
    var onlyReleaseScripts = T
    for(sha1 <- org.sireum.ops.StringOps(proc"git cherry".at(repo).runCheck().out).split(c => c == '\n').map((m: String) => ops.StringOps(m).replaceAllLiterally("+ ", ""))){
      for(file <- ops.StringOps(proc"git diff-tree --no-commit-id --name-only -r ${sha1}".at(repo).runCheck().out).split(c => c == '\n').map((p: String) => Os.path(p))){
        // ignore commits that only contain changes to the release scripts
        onlyReleaseScripts = onlyReleaseScripts && file.up.name == "plugin_release"// && file.up.up == "bin"
      }
    }
    if(!onlyReleaseScripts) {
      eprintln(s"Unpushed changes: ${repo}: ${unpushedCommits.out}")
    }
    ready = ready && onlyReleaseScripts
  }
  gitStaged = gitStaged && ready

  gitStaged // just need to return something
})

if(!gitStaged) { Os.exit(1) }

if(!fast) {
  proc"git -C ${CASE_Env_home} pull".at(CASE_Env_home).console.runCheck()
  val case_setup = CASE_Env_home / "case-setup.sh"

  // need to download the case-env version of sireum to make
  // sure it can be used to build the rest
  val str = ops.StringOps(case_setup.read)
  val initIndex = str.stringIndexOf("SIREUM_INIT_V:=")
  val SIREUM_INIT_V = str.substring(initIndex + 15, initIndex + 28)

  for (f <- ISZ(sireum, sireumjar) if f.exists) {
    f.remove()
    println(s"Removed ${f}")
  }

  proc"${SIREUM_HOME}/bin/init.sh".at(SIREUM_HOME).env(ISZ(("SIREUM_INIT_V", SIREUM_INIT_V))).console.runCheck()

  println(
    st"""The build will use: SIREUM_INIT_V=${SIREUM_INIT_V}.  If the build fails then do the following:
        |   1. run Jenkins job: https://jenkins.cs.ksu.edu/job/Sireum-Kekinian/.  This will build the
        |      sireum.jar and then the downstream job will publish it to the Github Init rep.
        |      Fetch the new version tag from https://github.com/sireum/init/releases
        |   2. Once successful, replace SIREUM_INIT_V in ${case_setup} with the version tag from above and then
        |      rerun this script
        |""".render)

  def assertResourceExists(p: ISZ[Os.Path]): Unit = { p.foreach((x: Os.Path) => assert(x.exists, s"${x} doesn't exist")) }
  assertResourceExists(ISZ(SIREUM_HOME, pluginDir, updateSiteDir, case_setup))
}

// clean codegen projects
val codeGenBuildCmd = SIREUM_HOME / "hamr" / "codegen" / "bin" / "build.cmd"
Os.proc(ISZ(codeGenBuildCmd.canon.value, "clean")).console.run()


val buildsbt = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "util" / "buildSbt.properties"

{ // check if arsit build properties file needs to be updated
  val f = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "bin" / "updateBuildSbtVersions.sc"
  f.chmod("700")
  val p = Os.proc(ISZ(sireum.value, "slang", "run", f.value)).console.run()
  if(!p.ok || p.exitCode != 0) {
    println(s"${buildsbt} wasn't ready")
    Os.exit(1)
  }
}

if(!fast) { // check if act's versions properties needs to be updated
  val f = SIREUM_HOME / "hamr" / "codegen" / "act" / "bin" / "updateActVersionsProperties.sc"
  f.chmod("700")
  val p = Os.proc(ISZ(sireum.value, "slang", "run", f.value)).console.run()
  if(!p.ok || p.exitCode != 0) {
    println(s"ACT versions properties wasn't ready")
    Os.exit(1)
  }
}

if(!fast){ // make sure arsit's ext lib macro are actually triggered
  val arsitJson = SIREUM_HOME / "out" / "sireum-proyek" / "modules" / "hamr-arsit.sha3.json"
  arsitJson.remove()
  println(s"removed ${arsitJson}")
}
/*
{ // remove cli.json to make sure build stamp gets updated correctly
  val cli = SIREUM_HOME / "out" / "sireum-proyek" / "modules" / "cli.sha3.json"
  if(cli.exists) {
    cli.remove()
    println(s"Removed ${cli}")
  }
}
*/


if(!fast) { // build sireum.jar
  val build_cmd = SIREUM_HOME / "bin" / "build.cmd"

  println("Running tipe")
  Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value, "tipe")).console.runCheck()

  println("Building sireum.jar")
  Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value)).console.runCheck()
}

if(!fast) { // run HAMR transpiler tests using the new sireum
  println("Running HAMR expensive regression tests")
  proc"$sireum proyek test -n sireum-proyek --par --sha3 --ignore-runtime --packages org.sireum.hamr.codegen.test.expensive ."
    .at(SIREUM_HOME).console.runCheck()
}

val props = buildsbt.properties

val sireumVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME)
val sireumVersionFull = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%H"), SIREUM_HOME)
val sireumTimestamp = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%y%m%d%H%M"), SIREUM_HOME)
val sireumBuildstamp = ops.StringOps(Os.proc(ISZ(sireum.value)).run().out).split(c => c =='\n')(2) // should be 3rd line

println(s"sireumVersion: ${sireumVersion}")
println(s"sireumBuildstamp: ${sireumBuildstamp}")
println(s"sireumTimestamp: ${sireumTimestamp}")

{ // vagrant
  val caseEnv = CASE_Env_home / "case-setup.sh"

  val contents = ops.StringOps(caseEnv.read)
  val pos = contents.stringIndexOf(": \"${SIREUM_V:=")
  val orig = contents.substring(pos, contents.indexOfFrom('\n', pos))
  val updated = contents.replaceAllLiterally(orig, s": \"$${SIREUM_V:=${sireumVersionFull}}\"")
  caseEnv.writeOver(updated)

  println(s"Updated ${caseEnv}")
}
