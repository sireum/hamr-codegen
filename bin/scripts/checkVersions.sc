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

def exclamations(): Unit = { for (i <- 0 to 1) { println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!") } }

// GitHub's releases atom feed rather than its API: the feed is not rate limited,
// so CI runners sharing an IP address cannot exhaust it, and its first entry is
// the most recent release.  None() means the feed could not be fetched, which
// says nothing about the pin; Some("") means it was fetched but held no release,
// i.e. its shape changed and this check has gone blind.
def latestGitHubRelease(releasesUrl: String): Option[String] = {
  val temp = Os.tempFix("releases-", ".atom")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"$releasesUrl.atom")) {
    return None()
  }
  // Each release is an <entry> whose <title> is its tag, e.g. <title>2.3.0</title>.
  // The feed's own <title> precedes the first entry, hence the guard.
  var latest: String = ""
  var inEntry = F
  for (l <- temp.readLines if latest == "") {
    val o = ops.StringOps(ops.StringOps(l).trim)
    if (o.contains("<entry>")) {
      inEntry = T
    } else if (inEntry && o.startsWith("<title>") && o.endsWith("</title>")) {
      latest = o.substring(7, o.size - 8)
    }
  }
  temp.remove()
  return Some(latest)
}

// crates.io lays its sparse index out by name length: 1/a, 2/ab, 3/a/abc, and
// ab/cd/abcd for everything longer.
@pure def cratesIndexPath(name: String): String = {
  val n = ops.StringOps(name).toLower
  val o = ops.StringOps(n)
  if (n.size == 1) {
    return s"1/$n"
  } else if (n.size == 2) {
    return s"2/$n"
  } else if (n.size == 3) {
    return s"3/${o.substring(0, 1)}/$n"
  } else {
    return s"${o.substring(0, 2)}/${o.substring(2, 4)}/$n"
  }
}

// value of "key" in one line of index JSON, "" when the line has no such key
@pure def jsonField(line: String, key: String): String = {
  val o = ops.StringOps(line)
  val i = o.stringIndexOf(s"\"$key\":")
  if (i < 0) {
    return ""
  }
  val rest = ops.StringOps(ops.StringOps(o.substring(i + key.size + 3, o.size)).trim)
  if (rest.startsWith("\"")) {
    val r = ops.StringOps(rest.substring(1, rest.size))
    return r.substring(0, r.indexOf('"'))
  } else {
    val j = rest.indexOf(',')
    return rest.substring(0, if (j < 0) rest.size else j)
  }
}

// dotted numeric comparison, falling back to string ordering for any component
// that is not a number
@pure def isNewerVersion(a: String, b: String): B = {
  val as = ops.StringOps(a).split((c: C) => c == '.')
  val bs = ops.StringOps(b).split((c: C) => c == '.')
  var i = 0
  while (i < as.size && i < bs.size) {
    (Z(as(i)), Z(bs(i))) match {
      case (Some(x), Some(y)) =>
        if (x != y) {
          return x > y
        }
      case _ =>
        if (as(i) != bs(i)) {
          return as(i) > bs(i)
        }
    }
    i = i + 1
  }
  return as.size > bs.size
}

// The sparse index rather than crates.io's API: the API refuses requests without
// a user agent, which downloadFrom does not set.  Index lines are ordered by
// publication, not by version, hence the comparison rather than taking the last.
// None()/Some("") carry the same meanings as latestGitHubRelease's.
def latestCrateVersion(name: String): Option[String] = {
  val temp = Os.tempFix(s"crates-index-", s"-$name.json")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"https://index.crates.io/${cratesIndexPath(name)}")) {
    return None()
  }
  var latest: String = ""
  for (l <- temp.readLines) {
    val vers = jsonField(l, "vers")
    // yanked releases are not upgrade targets, and neither are pre-releases
    if (vers != "" && jsonField(l, "yanked") != "true" && !ops.StringOps(vers).contains("-")) {
      if (latest == "" || isNewerVersion(vers, latest)) {
        latest = vers
      }
    }
  }
  temp.remove()
  return Some(latest)
}

// the commit each of the given submodule paths points at in one GitHub tree, e.g.
// submoduleTips("au-ts/lionsos", "main:dep", ISZ("sddf")).  None() means the tree
// could not be fetched -- GitHub's API allows 60 unauthenticated requests an hour
// per IP address, which CI runners share, so that is a real outcome rather than a
// hypothetical one.
def submoduleTips(repo: String, treeIsh: String, paths: ISZ[String]): Option[Map[String, String]] = {
  // GitHub can take seconds to resolve a tree it has not served recently, and
  // answers 504 when that outruns its gateway; the retry then hits the warm cache.
  for (attempt <- 0 to 1) {
    val r = readSubmoduleTips(repo, treeIsh, paths)
    if (r.nonEmpty) {
      return r
    }
  }
  return None()
}

def readSubmoduleTips(repo: String, treeIsh: String, paths: ISZ[String]): Option[Map[String, String]] = {
  val temp = Os.tempFix("tree-", ".json")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"https://api.github.com/repos/$repo/git/trees/$treeIsh")) {
    return None()
  }
  // Each entry lists its path before its sha, so the most recent path seen is the
  // one a sha belongs to.
  var r = Map.empty[String, String]
  var listing = F
  var path: String = ""
  for (l <- temp.readLines) {
    val o = ops.StringOps(ops.StringOps(l).trim)
    if (o.startsWith("\"path\":")) {
      listing = T
      path = jsonField(l, "path")
    } else if (o.startsWith("\"sha\":") && ops.ISZOps(paths).contains(path)) {
      r = r + path ~> jsonField(l, "sha")
    }
  }
  temp.remove()
  // A refusal -- a 504 or the rate limit -- still downloads as a body, and one
  // holding no entries at all is that rather than a tree without submodules, so it
  // is reported as unread instead of as an empty listing.
  return if (listing) Some(r) else None()
}

// the commit a branch points at, read from its commits atom feed rather than the
// API so it costs nothing against the rate limit
def latestCommit(repo: String, branch: String): Option[String] = {
  val temp = Os.tempFix("commits-", ".atom")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"https://github.com/$repo/commits/$branch.atom")) {
    return None()
  }
  // <id>tag:github.com,2008:Grit::Commit/&lt;sha&gt;</id>, newest first
  val marker: String = "Grit::Commit/"
  var sha: String = ""
  for (l <- temp.readLines if sha == "") {
    val o = ops.StringOps(l)
    val i = o.stringIndexOf(marker)
    if (i >= 0) {
      val rest = ops.StringOps(o.substring(i + marker.size, o.size))
      sha = rest.substring(0, rest.indexOf('<'))
    }
  }
  temp.remove()
  return if (sha == "") None() else Some(sha)
}

// the lines of one file in a GitHub repository at the given ref.  raw.github\
// usercontent.com rather than the API: it costs nothing against the rate limit.
def rawLines(repo: String, ref: String, path: String): Option[ISZ[String]] = {
  val temp = Os.tempFix("raw-", ".txt")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"https://raw.githubusercontent.com/$repo/$ref/$path")) {
    return None()
  }
  val r = temp.readLines
  temp.remove()
  return Some(r)
}

// the first double-quoted value on a line, "" when it has none
@pure def quoted(line: String): String = {
  val o = ops.StringOps(line)
  val i = o.indexOf('"')
  if (i < 0) {
    return ""
  }
  val rest = ops.StringOps(o.substring(i + 1, line.size))
  val j = rest.indexOf('"')
  return if (j < 0) "" else rest.substring(0, j)
}

// the name and version a Cargo.toml's [package] declares, "" for either it does not
@pure def cargoPackage(lines: ISZ[String]): (String, String) = {
  var name: String = ""
  var version: String = ""
  var inPackage = F
  for (l <- lines) {
    val o = ops.StringOps(ops.StringOps(l).trim)
    if (o.startsWith("[")) {
      inPackage = o.startsWith("[package]")
    } else if (inPackage && name == "" && o.startsWith("name")) {
      name = quoted(l)
    } else if (inPackage && version == "" && o.startsWith("version")) {
      version = quoted(l)
    }
  }
  return (name, version)
}

// the sdfgen release lionsos builds its examples against at the given commit-ish.
// It declares that in two places -- an env var in its examples workflow and a tag
// in its flake -- so the workflow is read first and the flake stands in should
// that env var ever be renamed or moved.  None()/Some("") carry the same meanings
// as latestGitHubRelease's.
def lionsosSdfgenVersion(ref: String): Option[String] = {
  val lionsosRepo = "au-ts/lionsos"
  var fetched = F

  // env:
  //   SDFGEN_VERSION: 0.33.0
  rawLines(lionsosRepo, ref, ".github/workflows/examples.yaml") match {
    case Some(lines) =>
      fetched = T
      var version: String = ""
      for (l <- lines if version == "") {
        val o = ops.StringOps(ops.StringOps(l).trim)
        if (o.startsWith("SDFGEN_VERSION:")) {
          version = ops.StringOps(o.substring(15, o.size)).trim
        }
      }
      if (version != "") {
        return Some(version)
      }
    case _ =>
  }

  //   sdfgen.url = "github:au-ts/microkit_sdf_gen/0.33.0";
  rawLines(lionsosRepo, ref, "flake.nix") match {
    case Some(lines) =>
      fetched = T
      var version: String = ""
      for (l <- lines if version == "") {
        val o = ops.StringOps(ops.StringOps(l).trim)
        if (o.startsWith("sdfgen.url")) {
          val rest = ops.StringOps(o.substring(o.lastIndexOf('/') + 1, o.size))
          version = rest.substring(0, rest.indexOf('"'))
        }
      }
      if (version != "") {
        return Some(version)
      }
    case _ =>
  }

  return if (fetched) Some("") else None()
}

// tags carry a leading v in microkit_versions.properties, release titles do not
@strictpure def dropV(v: String): String =
  if (ops.StringOps(v).startsWith("v")) ops.StringOps(v).substring(1, v.size) else v

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

val microkitVersionsP = SIREUM_HOME / "hamr" / "codegen" / "jvm" / "src" / "main" / "resources" / "microkit_versions.properties"

var proversMisaligned = F

{ // provers-env pins the toolchain that generated Microkit systems are built
  // against, so what it installs has to be what codegen generates for: a system
  // emitted for one Verus, Microkit SDK or sDDF and built against another is a
  // failure at the user's `make`, not here.  The two files are in different
  // repositories, so a mismatch is reported rather than fixed -- whichever side
  // is behind has to be bumped deliberately.
  val proversVersionsUrl = "https://raw.githubusercontent.com/loonwerks/INSPECTA-models/main/provers-env/bin/versions.sh"

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

var microkitOutdated = F

{ // codegen generates Microkit systems against the SDK that microkit-sdk pins, so
  // a newer Microkit release is drift the same way the provers-env pins are: it is
  // reported rather than fixed, since bumping the SDK also means regenerating the
  // expected results and moving provers-env's MICROKIT_SDK_VER in step -- rewriting
  // the pin here on its own would just trade this warning for the mismatch above.
  val microkitReleasesUrl = "https://github.com/seL4/microkit/releases"
  val microkitVers = microkitVersionsP.properties

  if (!microkitVers.contains("microkit-sdk")) {
    halt(s"$microkitVersionsP doesn't contain microkit-sdk")
  }
  val ours = microkitVers.get("microkit-sdk").get

  latestGitHubRelease(microkitReleasesUrl) match {
    case Some(latest) =>
      if (latest == "") {
        exclamations()
        println(s"WARNING: could not find a release tag in $microkitReleasesUrl.atom, so microkit-sdk was not checked")
        exclamations()
        microkitOutdated = T
      } else if (dropV(ours) != dropV(latest)) {
        exclamations()
        println(
          st"""WARNING: microkit-sdk is not the latest Microkit release:
              |
              |  microkit-sdk = $ours, but the latest release is $latest
              |
              |  ${microkitVersionsP.toUri}
              |  $microkitReleasesUrl""".render)
        exclamations()
        microkitOutdated = T
      }
    case _ =>
      // Not drift, so not a failure: an unreachable GitHub says nothing about
      // whether the pin is current.
      exclamations()
      println(s"WARNING: could not fetch $microkitReleasesUrl.atom, so microkit-sdk was not checked")
      exclamations()
  }
}

var cratesOutdated = F

{ // the crate versions generated Rust components are built against.  Newer releases
  // are reported rather than picked up, for the same reason as microkit-sdk: moving
  // a pin changes the emitted Cargo.toml, so it also means regenerating the expected
  // results.
  //
  // vstd, verus_builtin and verus_builtin_macros are deliberately left out: they are
  // published by the Verus release that verus-release pins, so they move with it
  // rather than on their own.
  val cratesIoKeys = ISZ[String](
    "linux-raw-sys", "log",                                                 // dependencies
    "lazy_static", "once_cell", "serial_test", "proptest", "env_logger")    // dev-dependencies

  // sel4 and sel4-logging are git dependencies on rust-sel4 pinned by tag rather
  // than crates.io releases -- RustUtil.sel4CargoDependencies emits them as
  // git = "https://github.com/seL4/rust-sel4", tag = "<pin>" -- so the tags are
  // what they are checked against.
  val sel4Keys = ISZ[String]("sel4", "sel4-logging")
  val sel4ReleasesUrl = "https://github.com/seL4/rust-sel4/releases"

  val microkitVers = microkitVersionsP.properties
  var stale = ISZ[ST]()
  var unreachable = ISZ[String]()
  var blind = ISZ[String]()

  // releaseUrl maps the release that came back to the page describing it, so a
  // warning points at what changed rather than just naming a number
  def check(key: String, ours: String, latestOpt: Option[String], where: String,
            releaseUrl: String => String @pure): Unit = {
    latestOpt match {
      case Some(latest) =>
        if (latest == "") {
          blind = blind :+ key
        } else if (dropV(ours) != dropV(latest)) {
          stale = stale :+
            st"""$key = $ours, but the latest $where release is $latest
                |  ${releaseUrl(latest)}"""
        }
      case _ => unreachable = unreachable :+ key
    }
  }

  for (key <- cratesIoKeys ++ sel4Keys if !microkitVers.contains(key)) {
    halt(s"$microkitVersionsP doesn't contain $key")
  }

  // one fetch per crate, so they are made at once rather than one after another
  val cratesLatest = ops.ISZOps(cratesIoKeys).parMap((key: String) => latestCrateVersion(key))
  for (i <- 0 until cratesIoKeys.size) {
    val key = cratesIoKeys(i)
    check(key, microkitVers.get(key).get, cratesLatest(i), "crates.io",
      (latest: String) => s"https://crates.io/crates/$key/$latest")
  }

  // both crates are tagged together in the one repository, so it is fetched once
  val sel4LatestOpt = latestGitHubRelease(sel4ReleasesUrl)
  for (key <- sel4Keys) {
    // the release page rather than a tag URL: the tags carry a leading v that the
    // release titles do not, so a tag URL built from a title would not resolve
    check(key, microkitVers.get(key).get, sel4LatestOpt, "rust-sel4",
      (latest: String) => sel4ReleasesUrl)
  }

  if (stale.nonEmpty) {
    exclamations()
    println(
      st"""WARNING: generated Rust components are pinned against crates that have newer releases:
          |
          |  ${(stale, "\n")}
          |
          |  ${microkitVersionsP.toUri}""".render)
    exclamations()
    cratesOutdated = T
  }

  if (blind.nonEmpty) {
    // no version came back, so these have gone unchecked in a way a retry will not fix
    exclamations()
    println(st"WARNING: no release could be found for ${(blind, ", ")}, so they were not checked".render)
    exclamations()
    cratesOutdated = T
  }

  if (unreachable.nonEmpty) {
    // Not drift, so not a failure, as above.
    exclamations()
    println(st"WARNING: could not fetch release information for ${(unreachable, ", ")}, so they were not checked".render)
    exclamations()
  }
}

var verusCratesDrifted = F

{ // vstd, verus_builtin and verus_builtin_macros are not chosen here either: they
  // are whatever the Verus release that verus-release pins declares.  Their versions
  // are bumped on their own cadence rather than once per release -- release
  // 0.2026.08.15 ships the same 0.0.0-2026-08-09-0044 crates as 0.2026.08.09 -- so
  // the date they carry says nothing, and the Cargo.toml files at the release tag
  // are what actually ties the two together.
  val verusRepo = "verus-lang/verus"
  // the directory under source/ each recorded crate is built from
  val crates: ISZ[(String, String)] = ISZ(
    ("vstd", "vstd"), ("builtin", "verus_builtin"), ("builtin_macros", "verus_builtin_macros"))

  val microkitVers = microkitVersionsP.properties
  for (key <- ISZ[String]("verus-release") ++ (for (crate <- crates) yield crate._2) if !microkitVers.contains(key)) {
    halt(s"$microkitVersionsP doesn't contain $key")
  }
  val release = microkitVers.get("verus-release").get
  val ref = s"release/$release"

  var mismatches = ISZ[ST]()
  var unchecked = ISZ[String]()

  for (crate <- crates) {
    val dir = crate._1
    val key = crate._2
    val recorded = microkitVers.get(key).get
    rawLines(verusRepo, ref, s"source/$dir/Cargo.toml") match {
      case Some(lines) =>
        val (name, version) = cargoPackage(lines)
        if (version == "") {
          mismatches = mismatches :+ st"source/$dir/Cargo.toml declares no version at $ref"
        } else if (name != key) {
          // renamed or moved upstream, so this pin is no longer that crate's
          mismatches = mismatches :+ st"source/$dir/Cargo.toml declares $name rather than $key at $ref"
        } else if (version != recorded) {
          mismatches = mismatches :+ st"$key = $recorded, but Verus $release declares $version"
        }
      case _ => unchecked = unchecked :+ key
    }
  }

  if (mismatches.nonEmpty) {
    exclamations()
    println(
      st"""WARNING: the recorded Verus crates are not the ones the pinned Verus release declares:
          |
          |  ${(mismatches, "\n")}
          |
          |  ${microkitVersionsP.toUri}
          |  https://github.com/$verusRepo/tree/$ref/source""".render)
    exclamations()
    verusCratesDrifted = T
  }

  if (unchecked.nonEmpty) {
    // Not drift, so not a failure: an unreachable GitHub says nothing about whether
    // the crates agree.
    exclamations()
    println(st"WARNING: could not read the Cargo.toml of ${(unchecked, ", ")} at $ref, so they were not checked".render)
    exclamations()
  }
}

var lionsosDrifted = F

{ // lionsos carries sDDF and libvmm as submodules and declares the sdfgen release
  // it builds its examples against, so the lionsos pin decides which of those APIs
  // generated systems are built against.  The two tips are not recorded here --
  // they are whatever the pinned commit carries -- so what is worth knowing is
  // whether the latest lionsos carries different ones, i.e. whether moving the pin
  // would move those APIs with it.  Reported rather than acted on: the models build
  // against what provers-env installs, so bumping lionsos is deliberate and done in
  // step with LIONSOS_VER.
  val lionsosRepo = "au-ts/lionsos"
  val paths = ISZ[String]("sddf", "libvmm")

  val microkitVers = microkitVersionsP.properties
  for (key <- ISZ[String]("lionsos", "sdfgen") if !microkitVers.contains(key)) {
    halt(s"$microkitVersionsP doesn't contain $key")
  }
  val pin = microkitVers.get("lionsos").get

  var mismatches = ISZ[ST]()
  var unchecked = ISZ[String]()

  // the branch tip is resolved to a commit rather than passed to GitHub as "main",
  // so the warning can name what it compared against
  val latestRef: String = latestCommit(lionsosRepo, "main") match {
    case Some(sha) => sha
    case _ => ""
  }
  val latest: String = if (latestRef == "") "the latest commit" else s"lionsos ${ops.StringOps(latestRef).substring(0, 7)}"

  if (latestRef == "") {
    unchecked = unchecked :+ "the commit lionsos main points at"
  }

  (submoduleTips(lionsosRepo, s"$pin:dep", paths),
   if (latestRef == "") None[Map[String, String]]() else submoduleTips(lionsosRepo, s"$latestRef:dep", paths)) match {
    case (Some(pinTips), Some(latestTips)) =>
      for (path <- paths) {
        (pinTips.get(path), latestTips.get(path)) match {
          case (Some(pinTip), Some(latestTip)) =>
            if (pinTip != latestTip) {
              mismatches = mismatches :+ st"dep/$path: $pinTip at $pin, but $latestTip at $latest"
            }
          case _ =>
            // gone from one side or the other, so the comparison no longer means anything
            mismatches = mismatches :+ st"dep/$path is not a submodule of both lionsos $pin and $latest"
        }
      }
    case _ =>
      // Not drift, so not a failure: an unreachable GitHub says nothing about
      // whether the tips agree.
      unchecked = unchecked :+ "the sDDF and libvmm tips"
  }

  (lionsosSdfgenVersion(pin), if (latestRef == "") None[String]() else lionsosSdfgenVersion(latestRef)) match {
    case (Some(pinSdfgen), Some(latestSdfgen)) =>
      if (pinSdfgen == "" || latestSdfgen == "") {
        mismatches = mismatches :+ st"lionsos no longer declares the sdfgen release it builds against"
      } else {
        // sdfgen, unlike the tips, is recorded here, so it can also fall out of step
        // with the commit that lionsos is pinned to
        val recorded = microkitVers.get("sdfgen").get
        if (pinSdfgen != recorded) {
          mismatches = mismatches :+ st"sdfgen = $recorded, but lionsos $pin expects $pinSdfgen"
        }
        if (pinSdfgen != latestSdfgen) {
          mismatches = mismatches :+ st"sdfgen: $pinSdfgen at $pin, but $latestSdfgen at $latest"
        }
      }
    case _ => unchecked = unchecked :+ "the sdfgen version"
  }

  if (mismatches.nonEmpty) {
    exclamations()
    println(
      st"""WARNING: the LionsOS dependencies generated systems are built against have moved:
          |
          |  ${(mismatches, "\n")}
          |
          |  ${microkitVersionsP.toUri}
          |  https://github.com/$lionsosRepo/compare/$pin...${if (latestRef == "") "main" else latestRef}""".render)
    exclamations()
    lionsosDrifted = T
  }

  if (unchecked.nonEmpty) {
    exclamations()
    println(st"WARNING: could not read ${(unchecked, " or ")} from $lionsosRepo, so they were not checked".render)
    exclamations()
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
if (changesDetected || proversMisaligned || microkitOutdated || cratesOutdated || verusCratesDrifted || lionsosDrifted) {
  Os.exit(1) // versions have changed, or drifted from provers-env or from the latest upstream releases
} else {
  Os.exit(0)
}
