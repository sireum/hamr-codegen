::/*#! 2> /dev/null                                           #
@ 2>/dev/null # 2>nul & echo off & goto BOF                   #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then             #
  exec "$0.com" "$@"                                          #
fi                                                            #
rm -f "$0.com"                                                #
if [ -z "${SIREUM_HOME}" ]; then                              #
  echo "Please set SIREUM_HOME env var"                       #
  exit -1                                                     #
fi                                                            #
exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"       #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
"%SIREUM_HOME%\bin\sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / "sireum"
val sireumjar = SIREUM_HOME / "bin" / "sireum.jar"


// TODO get these from env or arguments
val pluginDir = Os.home / "devel" / "sireum" / "osate-plugin"
val updateSiteDir = Os.home / "devel" / "sireum" / "osate-update-site"

def assertResourceExists(p: ISZ[Os.Path]): Unit = { p.foreach((x: Os.Path) => assert(x.exists, s"${x} doesn't exist")) }
assertResourceExists(ISZ(SIREUM_HOME, pluginDir, updateSiteDir))

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

def replaceLines(replacements: ISZ[(String, String)], file: Os.Path): Unit = {
  val lines = file.readLines
  val found: MSZ[B] = MSZ.create(replacements.size, F)
  val modLines: ISZ[String] = lines.map(line => {
    var newLine = line
    for(i <- 0 until replacements.size if ops.StringOps(line).startsWith(replacements(i)._1)) {
      found(i) = T
      newLine = replacements(i)._2
    }
    newLine
  })
  for(i <- 0 until found.size) { assert(found(i), s"Didn't find ${replacements(i)._1} for ${file}") }
  file.writeOver(ops.ISZOps(modLines).foldRight((line: String, r: String) => s"${r}\n${line}", ""))
  println(s"Wrote: ${file}")
}

val sireumCommit = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME)
val sireumVersionFull = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%H"), SIREUM_HOME)
//val sireumTimestamp = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%y%m%d%H%M"), SIREUM_HOME)
val sireumYear = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%Y"), SIREUM_HOME)
val sireumMonthDay = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%m%d%H%M"), SIREUM_HOME)
val sireumBuildstamp = ops.StringOps(Os.proc(ISZ(sireum.value)).run().out).split(c => c =='\n')(2) // should be 3rd line

val sireumVersion = s"1.${sireumYear}.${sireumMonthDay}"
val sireumVersionQualified = s"1.${sireumYear}.${sireumMonthDay}.${sireumCommit}"

println(s"sireumVersion: ${sireumCommit}")
println(s"sireumBuildstamp: ${sireumBuildstamp}")
//println(s"sireumTimestamp: ${sireumTimestamp}")
println(s"sireumVersion: ${sireumVersion}")
println(s"sireumVersionQualified: ${sireumVersionQualified}")

// TODO figure out how plugin.properties file work, for now just modify the files directly

def updateManfiest(proj: OsateProject): Unit = {
  val a =     "Bundle-Version:"
  val aMod = s"Bundle-Version: ${sireumVersion}.qualifier"

  val manifest = proj.projectDir / proj.projectId / "META-INF" / "MANIFEST.MF"
  assert(manifest.exists, s"${manifest} does not exist")
  replaceLines(ISZ((a, aMod)), manifest)
}

@datatype class OsateProject(projectDir: Os.Path,
                             projectId: String,
                             addOsatePluginImport: B)

val ids: ISZ[OsateProject] = ISZ(
  OsateProject(pluginDir, "org.sireum.aadl.osate", F),
  OsateProject(pluginDir, "org.sireum.aadl.osate.awas", T),
  OsateProject(pluginDir, "org.sireum.aadl.osate.cli", T),
  OsateProject(pluginDir, "org.sireum.aadl.osate.hamr", T),
  //OsateProject(pluginDir / "aadl-security", "org.sireum.aadl.osate.securitymodel", F)
)

ids.foreach((proj: OsateProject) => updateManfiest(proj))


def updateFeatureXml(project: OsateProject): Unit ={

  val a =        "      version="
  val aMod = st"""      version="${sireumVersion}.qualifier"""".render

  val b =    st"""      <import plugin="org.sireum.aadl.osate""".render
  val bMod = st"""      <import plugin="org.sireum.aadl.osate" version="${sireumVersion}" match="greaterOrEqual"/>""".render

  val featureXML = project.projectDir / s"${project.projectId}.feature" / "feature.xml"
  assert(featureXML.exists, s"${featureXML} does not exist")
  val mods: ISZ[(String, String)] = ISZ[(String,String)]((a, aMod)) ++ (if(project.addOsatePluginImport) ISZ((b, bMod)) else ISZ[(String, String)]())
  replaceLines(mods, featureXML)
}

ids.foreach((proj: OsateProject) => updateFeatureXml(proj))


val releaseDir = updateSiteDir / sireumVersionQualified
releaseDir.mkdir()

val dirs = ids.map((proj: OsateProject) => {
  val d = releaseDir / proj.projectId
  d.mkdir()
  assert(d.exists, s"${d} does not exist")
  println(s"Created ${d}")

  st"${proj.projectId}.feature ~> ${d}"
})


val st = st"""
             |
             |Right click on each feature project in OSATE and choose
             |
             |  Export... >> Plug-in Development >> Deployable Features
             |
             |Under 'Destination' change the 'Directory' as follows:
             |  ${(dirs, "\n")}
             |
             |Under 'Options', change 'Categorize repository:' to 'Sireum' and
             |change 'Qualifier replacement' to '${sireumCommit}'
             |"""

println(st.render)
