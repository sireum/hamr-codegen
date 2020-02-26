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
exec ${SIREUM_HOME}/bin/sireum slang run -s -n "$0" "$@"      #
:BOF
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run -s -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)

// TODO get these from env or arguments
val pluginDir = Os.home / "devel/sireum/osate-plugin"
val updateSiteDir = Os.home / "devel/sireum/osate-plugin-update-site"
val updateSiteHAMRDir = Os.home / "devel/sireum/hamr-plugin-update-site"
val caseDir = Os.home / "devel/sel4/home/CASE-loonwerks"

def replaceLine(startsWith: String, replacement: String, lines: ISZ[String]): ISZ[String] = {
  var found = F
  val ret = lines.map(str => if(ops.StringOps(str).startsWith(startsWith)) { found = T; replacement } else str)
  assert(found, s"Didn't find ${startsWith}")
  return ret
}

def collapse(lines: ISZ[String]):  String = {
  return ops.ISZOps(lines).foldRight((line: String, r: String) => s"${r}\n${line}", "")
}



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


val props = buildsbt.properties

val sireumVersion = props.get("org.sireum.version").get
val sireumBuildstamp = props.get("org.sireum.buildstamp").get
val sireumTimestamp = props.get("org.sireum.timestamp").get

println(s"sireumVersion: ${sireumVersion}")
println(s"sireumBuildstamp: ${sireumBuildstamp}")
println(s"sireumTimestamp: ${sireumTimestamp}")

val buildcmd = SIREUM_HOME / "bin" / "build.cmd"

{ // run tipe
  Os.proc(ISZ(buildcmd.value, "tipe")).console.runCheck()
}

{ // build sireum.jar
  println("Building sireum.jar")
  Os.proc(ISZ(buildcmd.value)).at(SIREUM_HOME).console.runCheck()
}

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
  val _baseManifest = collapse(replaceLine(a, aMod, baseManifest.readLines))
  baseManifest.writeOver(_baseManifest)
  println(s"Wrote: ${baseManifest}")
}

{ // HAMR MANIFEST
  
  val a =     "Bundle-Version:"
  val aMod = s"Bundle-Version: 1.0.${sireumTimestamp}.qualifier"
  
  val b =        " org.sireum.aadl.osate;bundle-version="
  val bMod = st""" org.sireum.aadl.osate;bundle-version="1.0.${sireumTimestamp}",""".render

  val hamrManifest = pluginDir / "org.sireum.aadl.osate.hamr/META-INF/MANIFEST.MF"
  val _hamrManifest = collapse(replaceLine(a, aMod, replaceLine(b, bMod, hamrManifest.readLines)))
  hamrManifest.writeOver(_hamrManifest)
  println(s"Wrote: ${hamrManifest}")
}

{ // BASE FEATURE
  
  val a =        "      version="
  val aMod = st"""      version="1.0.${sireumTimestamp}.qualifier"""".render
  
  val feature = pluginDir / "org.sireum.aadl.osate.feature/feature.xml"
  val _feature = collapse(replaceLine(a, aMod, feature.readLines))
  feature.writeOver(_feature)
  println(s"Wrote: ${feature}")
}

{ // HAMR FEATURE
  
  val a =        "      version="
  val aMod = st"""      version="1.0.${sireumTimestamp}.qualifier"""".render
  
  val b =    st"""      <import plugin="org.sireum.aadl.osate""".render
  val bMod = st"""      <import plugin="org.sireum.aadl.osate" version="1.0.${sireumTimestamp}" match="greaterOrEqual"/>""".render
  
  val hamrFeature = pluginDir / "org.sireum.aadl.osate.hamr.feature/feature.xml"
  val _hamrFeature = collapse(replaceLine(a, aMod, replaceLine(b, bMod, hamrFeature.readLines)))
  hamrFeature.writeOver(_hamrFeature)
  println(s"Wrote: ${hamrFeature}")
}

{ // Base Update Site version update
  
  val a =    st"""   <feature url="features/org.sireum.aadl.osate""".render
  val aMod = st"""   <feature url="features/org.sireum.aadl.osate.feature_1.0.${sireumTimestamp}.qualifier.jar" id="org.sireum.aadl.osate.feature" version="1.0.${sireumTimestamp}.qualifier">""".render

  val update = updateSiteDir / "org.sireum.aadl.osate.update.site/site.xml"
  val _update = collapse(replaceLine(a, aMod, update.readLines))
  update.writeOver(_update)
  println(s"Wrote: ${update}")
}

{ // HAMR Update Site version update

  val a =    st"""   <feature url="features/org.sireum.aadl.osate""".render
  val aMod = st"""   <feature url="features/org.sireum.aadl.osate.hamr.feature_1.0.${sireumTimestamp}.qualifier.jar" id="org.sireum.aadl.osate.hamr.feature" version="1.0.${sireumTimestamp}.qualifier">""".render
  
  val hamrUpdate = updateSiteHAMRDir / "org.sireum.aadl.osate.hamr.update.site/site.xml"
  val _update = collapse(replaceLine(a, aMod, hamrUpdate.readLines))
  hamrUpdate.writeOver(_update)
  println(s"Wrote: ${hamrUpdate}")
}

{ // UPDATE SITE README's
  
  val url = s"https://github.com/sireum/kekinian/tree/${sireumVersion}#installing"
  
  val a = "Built against"
  val aMod = s"Built against Sireum Kekinian ${sireumBuildstamp} - To install Kekinian see [$url]($url)"

  {
    val readme = updateSiteDir / "readme.md"
    val r = collapse(replaceLine(a, aMod, readme.readLines))
    readme.writeOver(r)
    println(s"Wrote: ${readme}")
  }

  {
    val hamrReadme = updateSiteHAMRDir / "readme.md"
    val r = collapse(replaceLine(a, aMod, hamrReadme.readLines))
    hamrReadme.writeOver(r)
    println(s"Wrote: ${hamrReadme}")
  }
}

{ // vagrant
  
  val a =    st""": "$${SIREUM_V:=""".render
  val aMod = st""": "$${SIREUM_V:=${sireumVersion}}"""".render
  
  val caseEnv = caseDir / "TA5/case-env/case-setup.sh"
  caseEnv.writeOver(collapse(replaceLine(a, aMod, caseEnv.readLines)))
  println(s"Wrote: ${caseEnv}")
}
