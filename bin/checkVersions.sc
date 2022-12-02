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
val versions = (SIREUM_HOME / "versions.properties").properties

val codegenVersionsP = SIREUM_HOME / "hamr" / "codegen" / "jvm" / "src" / "main" / "resources" / "codegen.versions"
val codegenVersions = codegenVersionsP.properties

val arsitBuildSbtProps = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "util" / "buildSbt.properties"

val noUpdate: B = ops.ISZOps(Os.cliArgs).contains("no-update")

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

var currentVers: Map[String, String] = Map.empty +
  ("org.sireum.kekinian.version" ~> runGit(ISZ("git", "describe", "--abbrev=0", "--tags"), SIREUM_HOME)) +
  ("org.sireum.version.scala" ~> versions.get("org.scala-lang%scala-library%").get) +
  ("org.sireum.version.scalac-plugin" ~> versions.get("org.sireum%%scalac-plugin%").get) +
  ("org.sireum.version.scalatest" ~> versions.get("org.scalatest%%scalatest%%").get) +
  ("art.version" ~> runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr" / "codegen" / "art"))

{
  val cli = (SIREUM_HOME / "hamr" / "phantom" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "phantom" / "cli.scala").readLines
  var osateVersion: String = ""
  for(i <- 0 until cli.size if osateVersion == "" && ops.StringOps(cli(i)).contains("version")) {
    val o = ops.StringOps(cli(i+1))
    osateVersion = o.substring(o.indexOf('"') + 1, o.lastIndexOf('"'))
  }
  currentVers = currentVers + ("org.osate.version" ~> osateVersion)
}

{
  def parse(url: String): String = {
    val temp = Os.slashDir / "temp"
    temp.downloadFrom(url)
    val lines = temp.readLines
    var v: String = ""
    for(i <- lines.size - 1 to 0 by -1 if v == "") {
      val op = ops.StringOps(lines(i))
      if(op.contains("child location")) {
        v = op.substring(op.indexOf('\'') + 1, op.lastIndexOf('\''))
      }
    }
    temp.remove()
    return v
  }
  currentVers = currentVers +
    ("org.sireum.aadl.osate.plugins.version" ~>
      parse("https://raw.githubusercontent.com/sireum/osate-update-site/master/compositeContent.xml")) +
    ("org.sireum.aadl.gumbo.plugins.version" ~>
      parse("https://raw.githubusercontent.com/sireum/aadl-gumbo-update-site/master/compositeContent.xml"))
}

for(k <- currentVers.keys if !codegenVersions.contains(k)) {
  halt(s"${codegenVersionsP} doesn't contain $k")
}

val artEmbeddedVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "art")
if(currentVers.get("art.version").get != artEmbeddedVersion) {
  for(i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  println(s"WARNING: ART versions do not match: ${currentVers.get("at.version").get} vs ${artEmbeddedVersion}")
  for(i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
}

var mod = ISZ[String]()
var changesDetected = F

for(l <- codegenVersionsP.readLines) {
  val s = ops.StringOps(l).split((c: C) => c == '=')
  if(s.size == 2 && currentVers.contains(s(0)) && currentVers.get(s(0)).get != s(1)) {
    changesDetected = T
    println(s"${s(0)} changed: ${s(1)} -> ${currentVers.get(s(0)).get}")
    mod = mod :+ s"${s(0)}=${currentVers.get(s(0)).get}"
  } else {
    mod = mod :+ l
  }
}

if(changesDetected) {
  if (!noUpdate) {
    codegenVersionsP.writeOver(st"${(mod, "\n")}\n".render)
    codegenVersionsP.copyOverTo(arsitBuildSbtProps)

    println(s"Updated:")
    println(s"  ${codegenVersionsP.toUri}")
    println(s"  ${arsitBuildSbtProps.toUri}")
  }
  Os.exit(1)
} else {
  Os.exit(0)
}

/*

def touchePath(path: Os.Path): Unit = {
  val content = path.read
  val lineSep: String = if (Os.isWin) "\r\n" else "\n"
  val sops = ops.StringOps(content)
  val newContent: String =
    if (sops.endsWith(lineSep)) ops.StringOps(content).trim
    else s"$content$lineSep"
  path.writeOver(newContent)
  println(s"Touched ${path}")
}

if(updated) {
  val artFiles = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "arsit" / "util" / "ArsitLibrary_Ext.scala"

  val pst = st"""${(props.entries.map(m => st"${m._1}=${m._2}"), "\n")}""".render
  buildSbtProps.writeOver(pst)
  println(s"$buildSbtProps updated")
  
  //println("\nRunning bin/build.cmd")
  touchePath(artFiles)
  //val build_cmd = SIREUM_HOME / "bin" / "build.cmd"
  //Os.proc(ISZ(sireum.value, "slang", "run", build_cmd.value)).console.runCheck()
  touchePath(artFiles)

  //println(s"\n$buildSbtProps updated and Sireum touched/rebuilt -- expect an error to follow")
  println(s"\n$buildSbtProps updated -- expect an error to follow")

  Os.exit(1) // return 1 to indicate versions have changed
} else {
  println(s"No Arsit updates needed")
  Os.exit(0)
}
*/
