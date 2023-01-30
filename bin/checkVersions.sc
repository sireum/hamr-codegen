// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val versions = (SIREUM_HOME / "versions.properties").properties

val noUpdate: B = ops.ISZOps(Os.cliArgs).contains("no-update")

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

val url = runGit(ISZ("git", "config", "--get", "remote.origin.url"), SIREUM_HOME)
if (url != "https://github.com/sireum/kekinian.git") {
  if (Os.env("GITLAB_CI").isEmpty && Os.env("GITHUB_ACTIONS").isEmpty) {
    println(s"Script requires SIREUM_HOME to point to a kekinian checkout rather than ${url}: ${(Os.slashDir / "checkVersions.sc").toUri}")
  }
  Os.exit(0)
}

val codegenVersionsP = SIREUM_HOME / "hamr" / "codegen" / "jvm" / "src" / "main" / "resources" / "codegen_versions.properties"
val phantomVersionsP = SIREUM_HOME / "hamr" / "codegen" / "jvm" / "src" / "main" / "resources" / "phantom_versions.properties"

var codegenCurrentVers: Map[String, String] = Map.empty
var phantomCurrentVers: Map[String, String] = Map.empty

{ // build maps containing the current versions
  codegenCurrentVers = codegenCurrentVers +
    ("org.sireum.kekinian.version" ~> runGit(ISZ("git", "describe", "--abbrev=0", "--tags"), SIREUM_HOME)) +
    //("org.sireum.kekinian.version" ~> runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME)) +
    ("org.sireum.version.scala" ~> versions.get("org.scala-lang%scala-library%").get) +
    ("org.sireum.version.scalac-plugin" ~> versions.get("org.sireum%%scalac-plugin%").get) +
    ("org.sireum.version.scalatest" ~> versions.get("org.scalatest%%scalatest%%").get) +
    ("art.version" ~> runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr" / "codegen" / "art"))

  println(codegenCurrentVers)

  {
    val cli = (SIREUM_HOME / "hamr" / "phantom" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "phantom" / "cli.scala").readLines
    var osateVersion: String = ""
    for (i <- 0 until cli.size if osateVersion == "" && ops.StringOps(cli(i)).contains("version")) {
      val o = ops.StringOps(cli(i + 1))
      osateVersion = o.substring(o.indexOf('"') + 1, o.lastIndexOf('"'))
    }
    phantomCurrentVers = phantomCurrentVers + ("org.osate.version" ~> osateVersion)
  }

  {
    def parse(key: String, url: String): Unit = {
      val temp = Os.slashDir / "temp"
      temp.downloadFrom(url)
      val lines = temp.readLines
      var v: String = ""
      var v_alt: String = ""
      for (i <- lines.size - 1 to 0 by -1 if v == "") {
        val op = ops.StringOps(lines(i))
        if (op.contains("child location")) {
          v = op.substring(op.indexOf('\'') + 1, op.lastIndexOf('\''))
          v_alt = v
          val vops = ops.StringOps(v).split((c: C) => c == '.')
          val tops = ops.StringOps(vops(2))
          if (tops.startsWith("0")) {
            v_alt = s"${vops(0)}.${vops(1)}.${tops.substring(1, tops.size)}.${vops(3)}"
          }
        }
      }
      temp.remove()
      phantomCurrentVers = phantomCurrentVers + (key ~> v)
      phantomCurrentVers = phantomCurrentVers + (s"${key}_alt" ~> v_alt)
    }

    parse("org.sireum.aadl.osate.plugins.version", "https://raw.githubusercontent.com/sireum/osate-update-site/master/compositeContent.xml")
    parse("org.sireum.aadl.gumbo.plugins.version", "https://raw.githubusercontent.com/sireum/aadl-gumbo-update-site/master/compositeContent.xml")
  }
}

{ // sanity checks
  for (k <- codegenCurrentVers.keys if !codegenVersionsP.properties.contains(k)) {
    halt(s"${codegenVersionsP} doesn't contain $k")
  }
  for (k <- phantomCurrentVers.keys if !phantomVersionsP.properties.contains(k)) {
    halt(s"${phantomVersionsP} doesn't contain $k")
  }

  val artEmbeddedVersion = runGit(ISZ("git", "log", "-n", "1", "--pretty=format:%h"), SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "art")
  if (codegenCurrentVers.get("art.version").get != artEmbeddedVersion) {
    for (i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    println(s"WARNING: ART versions do not match: ${codegenCurrentVers.get("art.version").get} vs ${artEmbeddedVersion}")
    for (i <- 0 to 10) println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  }
}

var changesDetected = F

val arsitUtilDir = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "resources" / "util"

def compare(p: Os.Path, currentVersions: Map[String, String]): Unit = {
  var mod = ISZ[String]()
  var hasChanges = F
  for (l <- p.readLines) {
    val s = ops.StringOps(l).split((c: C) => c == '=')
    if (s.size == 2 && currentVersions.contains(s(0)) && currentVersions.get(s(0)).get != s(1)) {
      hasChanges = T
      println(s"${s(0)} changed: ${s(1)} -> ${currentVersions.get(s(0)).get}")
      mod = mod :+ s"${s(0)}=${currentVersions.get(s(0)).get}"
    } else {
      mod = mod :+ l
    }
  }
  if (hasChanges && !noUpdate) {
    p.writeOver(st"${(mod, "\n")}\n".render)
    p.copyOverTo(arsitUtilDir / p.name)
    println(s"Updated:")
    println(s"  ${p.toUri}")
    println(s"  ${(arsitUtilDir / p.name).toUri}")
  }
  changesDetected = changesDetected || hasChanges
}

compare(codegenVersionsP, codegenCurrentVers)
compare(phantomVersionsP, phantomCurrentVers)

if(changesDetected && !noUpdate) {
  // force macro expansion
  val artFiles = SIREUM_HOME / "hamr" / "codegen" / "arsit" / "jvm" / "src" / "main" / "scala" / "org" / "sireum" / "hamr" / "arsit" / "util" / "ArsitLibrary_Ext.scala"
  val content = ops.StringOps(artFiles.read)
  artFiles.writeOver(if (content.endsWith(" ")) content.substring(0, content.size - 1) else s"${artFiles.read} ")

  Os.exit(1) // return 1 to indicate versions have changed
} else {
  Os.exit(0)
}
