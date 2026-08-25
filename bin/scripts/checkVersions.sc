// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

val versions = (SIREUM_HOME / "versions.properties").properties

val noUpdate: B = ops.ISZOps(Os.cliArgs).contains("no-update")

val useBleedingEdgeSireum: B = T

def runGit(args: ISZ[String], path: Os.Path): String = {
  val p = org.sireum.Os.proc(args).at(path).runCheck()
  return ops.StringOps(p.out).trim
}

val url = runGit(ISZ("git", "config", "--get", "remote.origin.url"), SIREUM_HOME)
if (url != "https://github.com/sireum/kekinian.git" && url != "https://github.com/sireum/kekinian" &&  url != "git@github.com:sireum/kekinian.git") {
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
    (if (useBleedingEdgeSireum)
      ("org.sireum.kekinian.version" ~> runGit(ISZ("git", "rev-parse", "--short=10", "HEAD"), SIREUM_HOME))
    else
      ("org.sireum.kekinian.version" ~> runGit(ISZ("git", "describe", "--abbrev=0", "--tags"), SIREUM_HOME))) +
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
    def parse(key: String, urlx: String): Unit = {
      val temp = Os.slashDir / "temp"
      temp.downloadFrom(urlx)
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

def exclamations(): Unit = { for (i <- 0 to 5) { println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!") } }

{ // sanity checks
  for (k <- codegenCurrentVers.keys if !codegenVersionsP.properties.contains(k)) {
    halt(s"${codegenVersionsP} doesn't contain $k")
  }
  for (k <- phantomCurrentVers.keys if !phantomVersionsP.properties.contains(k)) {
    halt(s"${phantomVersionsP} doesn't contain $k")
  }
}

var changesDetected = F
var jitpackFetches: ISZ[String] = ISZ()

def compare(p: Os.Path, currentVersions: Map[String, String]): Unit = {
  var mod = ISZ[String]()
  var hasChanges = F
  for (l <- p.readLines) {
    val s = ops.StringOps(l).split((c: C) => c == '=')
    if (s.size == 2 && currentVersions.contains(s(0)) && currentVersions.get(s(0)).get != s(1)) {
      hasChanges = T
      val newVersion = currentVersions.get(s(0)).get
      println(s"${s(0)} changed: ${s(1)} -> $newVersion")
      mod = mod :+ s"${s(0)}=${currentVersions.get(s(0)).get}"

      if (s(0) == "org.sireum.kekinian.version") {
        jitpackFetches = jitpackFetches :+
          s"${ops.StringOps(org.sireum.project.DependencyManager.librarySharedKey).replaceAllChars('%', ':')}$newVersion"
      }
      if (s(0) == "art.version") {
        jitpackFetches = jitpackFetches :+ s"org.sireum.slang-embedded-art::slang-embedded-art:$newVersion"
      }
    } else {
      mod = mod :+ l
    }
  }
  if (hasChanges && !noUpdate) {
    p.writeOver(st"${(mod, "\n")}\n".render)
    println(s"Updated:")
    println(s"  ${p.toUri}")
  }
  changesDetected = changesDetected || hasChanges
}

compare(codegenVersionsP, codegenCurrentVers)
compare(phantomVersionsP, phantomCurrentVers)

{
  val fmidecli = SIREUM_HOME / "bin" / "install" / "fmide-cli.sc"
  val cli = proc"$sireum slang run $fmidecli".run().out
  val tool = org.sireum.cli.JSON.toCliOpt(cli).left.asInstanceOf[org.sireum.cli.CliOpt.Tool]

  def fcompare(name: String, expected: String): Unit = {
    val actual = ops.ISZOps(tool.opts).filter(p => p.name == name)(0).tpe.asInstanceOf[org.sireum.cli.CliOpt.Type.Str].default.get
    val parts = ops.StringOps(expected).split(c => c == '.')
    // NOTE: eclipse will drop the leading zero for months 01-09 so add it back if needed
    val mdyhm: String = if (parts(2).size == 8) parts(2) else s"0${parts(2)}"
    val _expected = s"${parts(0)}.${parts(1)}.${mdyhm}.${parts(3)}"
    if (actual != _expected) {
      exclamations()
      println(s"WARNING: FMIDE version for ${name} does not match: ${actual} vs ${_expected}: ${fmidecli.toUri}")
      exclamations()
    }
  }

  fcompare("awas", phantomCurrentVers.get("org.sireum.aadl.osate.plugins.version_alt").get)
  fcompare("gumbo", phantomCurrentVers.get("org.sireum.aadl.gumbo.plugins.version_alt").get)
  fcompare("hamr", phantomCurrentVers.get("org.sireum.aadl.osate.plugins.version_alt").get)
}

var proversMisaligned = F

{ // provers-env pins the toolchain that generated Microkit systems are built
  // against, so what it installs has to be what codegen generates for: a system
  // emitted for one Verus, Microkit SDK or sDDF and built against another is a
  // failure at the user's `make`, not here.  The two files are in different
  // repositories, so a mismatch is reported rather than fixed -- whichever side
  // is behind has to be bumped deliberately.
  val proversVersionsUrl = "https://raw.githubusercontent.com/loonwerks/INSPECTA-models/main/provers-env/bin/versions.sh"
  val microkitVersionsP = SIREUM_HOME / "hamr" / "codegen" / "jvm" / "src" / "main" / "resources" / "microkit_versions.properties"

  // codegen's key, and the variable versions.sh pins the same tool as
  val pins: ISZ[(String, String)] = ISZ(
    ("microkit-sdk", "MICROKIT_SDK_VER"),
    ("rust-channel", "RUST_TOOLCHAIN_VER"),
    ("sdfgen", "SDFGEN_VER"),
    ("verus-release", "VERUS_VER"),
    ("lionsos", "LIONSOS_VER"))

  val temp = Os.slashDir / "temp-provers-versions.sh"
  temp.removeOnExit()
  if (!temp.downloadFrom(proversVersionsUrl)) {
    // Not a mismatch, so not a failure: an unreachable GitHub says nothing about
    // whether the pins agree.
    exclamations()
    println(s"WARNING: could not fetch $proversVersionsUrl, so the provers-env pins were not checked")
    exclamations()
  } else {
    // versions.sh pins each tool as an overridable default: : "${NAME:=value}"
    var proversVers = Map.empty[String, String]
    for (l <- temp.readLines) {
      val o = ops.StringOps(ops.StringOps(l).trim)
      if (o.startsWith(": \"${") && o.endsWith("}\"")) {
        val body = ops.StringOps(o.substring(5, o.size - 2))
        val i = body.stringIndexOf(":=")
        if (i > 0) {
          proversVers = proversVers + body.substring(0, i) ~> body.substring(i + 2, body.size)
        }
      }
    }

    val microkitVers = microkitVersionsP.properties
    var mismatches = ISZ[ST]()
    for (pin <- pins) {
      val key = pin._1
      val envVar = pin._2
      if (!microkitVers.contains(key)) {
        halt(s"$microkitVersionsP doesn't contain $key")
      } else if (!proversVers.contains(envVar)) {
        // renamed or dropped upstream, so this check has gone blind to that tool
        exclamations()
        println(s"WARNING: $proversVersionsUrl no longer pins $envVar, which $key was checked against")
        exclamations()
        proversMisaligned = T
      } else {
        val ours = microkitVers.get(key).get
        val theirs = proversVers.get(envVar).get
        if (ours != theirs) {
          mismatches = mismatches :+ st"$key = $ours, but versions.sh pins $envVar = $theirs"
        }
      }
    }

    if (mismatches.nonEmpty) {
      exclamations()
      println(
        st"""WARNING: provers-env installs a different toolchain than codegen generates for:
            |
            |  ${(mismatches, "\n  ")}
            |
            |  ${microkitVersionsP.toUri}
            |  $proversVersionsUrl""".render)
      exclamations()
      proversMisaligned = T
    }
  }
}

if (!noUpdate && jitpackFetches.nonEmpty) {
  val scalaKey = ops.StringOps(org.sireum.project.DependencyManager.scalaKey).replaceAllChars(':', '%')
  val scalaVer = versions.get(scalaKey).get

  ops.ISZOps(jitpackFetches).parMap((m: String) => {
    val sc = Os.tempFix(ops.StringOps(m).replaceAllChars(':', '_'), ".sc")
    sc.writeOver(
      st"""import org.sireum._
          |for (cif <- Coursier.fetch("$scalaVer", ISZ("$m"), Coursier.Proxy.empty)) {
          |  println(cif.path)
          |}""".render
    )
    sc.removeOnExit()
    println(s"Please wait while fetching/building $m via jitpack. You can stop this script if jitpack has to build the resource.")
    println(s"Refer to the 'Build' links for the following to see the build status:")
    println(s"  https://jitpack.io/#org.sireum/kekinian")
    println(s"  https://jitpack.io/#org.sireum/slang-embedded-art")
    Sireum.procCheck(proc"$sireum slang run $sc".console, message.Reporter.create)
  })
}

if (changesDetected && !noUpdate) {
  val hamrCodegenModule = SIREUM_HOME / "hamr" / "codegen" / "jvm"
  println(s"\nVersion changes detected: rebuild the hamr-codegen module to force macro expansion: ${hamrCodegenModule.toUri}")
}

// changesDetected on its own, not 'changesDetected && !noUpdate'.  Under no-update
// the properties files are left as they are, but the drift that was found is just
// as real -- and no-update is how VersionCheck runs this, so gating the exit on it
// meant that test could never fail on the versions it exists to watch.
if (changesDetected || proversMisaligned) {
  Os.exit(1) // versions have changed, or drifted from provers-env
} else {
  Os.exit(0)
}
