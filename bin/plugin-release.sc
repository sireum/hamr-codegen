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

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin/sireum"
val currYear = java.time.Year.now.getValue

// TODO get these from env or arguments
val pluginDir = Os.home / "devel/sireum/osate-plugin"
val updateSiteDir = Os.home / "devel/sireum/osate-plugin-update-site"
val updateSiteHAMRDir = Os.home / "devel/sireum/hamr-plugin-update-site"
val caseDir = Os.home / "devel/case/CASE-loonwerks"

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
  for(i <- 0 until found.size) assert(found(i), s"Didn't find ${replacements(i)._1}")
  file.writeOver(ops.ISZOps(modLines).foldRight((line: String, r: String) => s"${r}\n${line}", ""))
  println(s"Wrote: ${file}")
}


// clean codegen projects
val codeGenBuildCmd = SIREUM_HOME / "hamr" / "codegen" / "bin" / "build.cmd"
Os.proc(ISZ(codeGenBuildCmd.canon.value, "clean")).console.run()


val buildsbt = SIREUM_HOME / "hamr/codegen/arsit/resources/util/buildSbt.properties"

{
  val sireum = SIREUM_HOME / "bin/sireum"
  val f = SIREUM_HOME / "hamr/codegen/arsit/bin/updateBuildSbtVersions.sc"
  f.chmod("700")
  val p = Os.proc(ISZ(sireum.value, "slang", "run", f.value)).console.run()
  if(!p.ok || p.exitCode != 0) {
    println(s"${buildsbt} wasn't ready")
    Os.exit(1)
  }
}



{ // build sireum.jar
  val build_cmd = SIREUM_HOME / "bin" / "build.cmd"

  println("Running tipe")
  Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value, "tipe")).console.runCheck()

  println("Building sireum.jar")
  Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value)).console.runCheck()
}


val props = buildsbt.properties

val sireumVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME)
val sireumVersionFull = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%H"), SIREUM_HOME)
val sireumTimestamp = runGit(ISZ("git", "show", "-s", "--format=%cd", "--date=format:%y%m%d%H%M"), SIREUM_HOME)
val sireumBuildstamp = ops.StringOps(Os.proc(ISZ(sireum.value)).run.out).split(c => c =='\n')(2) // should be 3rd line

println(s"sireumVersion: ${sireumVersion}")
println(s"sireumBuildstamp: ${sireumBuildstamp}")
println(s"sireumTimestamp: ${sireumTimestamp}")



{ // COPY sireum.jar over to osate lib directory

  val sireumJar = SIREUM_HOME / "bin/sireum.jar"
  val osateLibJar = pluginDir / "org.sireum.aadl.osate/lib/sireum.jar"

  println(s"Copying ${sireumJar} to ${osateLibJar}")
  sireumJar.copyOverTo(osateLibJar)
}

// TODO figure out how plugin.properties file work, for now just modify the files directly

{ // BASE MANIFEST

  val a =     "Bundle-Version:"
  val aMod = s"Bundle-Version: 1.0.${sireumTimestamp}.qualifier"

  val baseManifest = pluginDir / "org.sireum.aadl.osate/META-INF/MANIFEST.MF"
  replaceLines(ISZ((a, aMod)), baseManifest)
}

{ // HAMR MANIFEST

  val a =     "Bundle-Version:"
  val aMod = s"Bundle-Version: 1.0.${sireumTimestamp}.qualifier"

  val b =        " org.sireum.aadl.osate;bundle-version="
  val bMod = st""" org.sireum.aadl.osate;bundle-version="1.0.${sireumTimestamp}",""".render

  val hamrManifest = pluginDir / "org.sireum.aadl.osate.hamr/META-INF/MANIFEST.MF"
  replaceLines(ISZ((a, aMod), (b, bMod)), hamrManifest)
}

{ // BASE FEATURE

  val a =        "      version="
  val aMod = st"""      version="1.0.${sireumTimestamp}.qualifier"""".render

  val b =     "      Copyright (c)"
  val bMod = s"      Copyright (c) ${currYear}, Kansas State University"

  val feature = pluginDir / "org.sireum.aadl.osate.feature/feature.xml"
  replaceLines(ISZ((a, aMod), (b, bMod)), feature)
}

{ // HAMR FEATURE

  val a =        "      version="
  val aMod = st"""      version="1.0.${sireumTimestamp}.qualifier"""".render

  val b =    st"""      <import plugin="org.sireum.aadl.osate""".render
  val bMod = st"""      <import plugin="org.sireum.aadl.osate" version="1.0.${sireumTimestamp}" match="greaterOrEqual"/>""".render

  val c =     "      Copyright (c)"
  val cMod = s"      Copyright (c) ${currYear}, Kansas State University"

  val hamrFeature = pluginDir / "org.sireum.aadl.osate.hamr.feature/feature.xml"
  replaceLines(ISZ((a, aMod), (b, bMod), (c, cMod)), hamrFeature)
}

{ // Base Update Site version update

  val a =    st"""   <feature url="features/org.sireum.aadl.osate""".render
  val aMod = st"""   <feature url="features/org.sireum.aadl.osate.feature_1.0.${sireumTimestamp}.qualifier.jar" id="org.sireum.aadl.osate.feature" version="1.0.${sireumTimestamp}.qualifier">""".render

  val update = updateSiteDir / "org.sireum.aadl.osate.update.site/site.xml"
  replaceLines(ISZ((a, aMod)), update)
}

{ // HAMR Update Site version update

  val a =    st"""   <feature url="features/org.sireum.aadl.osate""".render
  val aMod = st"""   <feature url="features/org.sireum.aadl.osate.hamr.feature_1.0.${sireumTimestamp}.qualifier.jar" id="org.sireum.aadl.osate.hamr.feature" version="1.0.${sireumTimestamp}.qualifier">""".render

  val hamrUpdate = updateSiteHAMRDir / "org.sireum.aadl.osate.hamr.update.site/site.xml"
  replaceLines(ISZ((a, aMod)), hamrUpdate)
}

{ // UPDATE SITE README's

  val url = s"https://github.com/sireum/kekinian/tree/${sireumVersion}#installing"

  val a = "Built against"
  val aMod = s"Built against Sireum Kekinian ${sireumBuildstamp} - To install Kekinian see [$url]($url)"

  {
    val readme = updateSiteDir / "readme.md"
    replaceLines(ISZ((a, aMod)), readme)
  }

  {
    val hamrReadme = updateSiteHAMRDir / "readme.md"
    replaceLines(ISZ((a, aMod)), hamrReadme)
  }
}

{ // vagrant

  val a =    st""": "$${SIREUM_V:=""".render
  val aMod = st""": "$${SIREUM_V:=${sireumVersionFull}}"""".render

  val caseEnv = caseDir / "TA5/case-env/case-setup.sh"
  replaceLines(ISZ((a, aMod)), caseEnv)
}
