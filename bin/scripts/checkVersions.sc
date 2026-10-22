// #Sireum

import org.sireum._

val SIREUM_HOME = Os.path(Os.env("SIREUM_HOME").get)
val sireum = SIREUM_HOME / "bin" / (if (Os.isWin) "sireum.bat" else "sireum")

val versions = (SIREUM_HOME / "versions.properties").properties

// ci: what the Version alignment workflow runs.  It implies no-update, fails only on
// what this project has to fix itself (see the end of the script), and with
// report=<file> writes what upstream has moved on to that file instead of failing.
val ciMode: B = ops.ISZOps(Os.cliArgs).contains("ci")
def reportArg(): Option[Os.Path] = {
  for (a <- Os.cliArgs if ops.StringOps(a).startsWith("report=")) {
    return Some(Os.path(ops.StringOps(a).substring(7, a.size)))
  }
  return None()
}
val reportOpt: Option[Os.Path] = reportArg()

val noUpdate: B = ciMode || ops.ISZOps(Os.cliArgs).contains("no-update")

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
    // the sbt arsit's generated sbt projects use (project/build.properties)
    ("org.sireum.version.sbt" ~> versions.get("org.sireum.version.sbt").get) +
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

// an exact Cargo requirement (=1.2.3) without its operator
@strictpure def dropEq(req: String): String =
  if (ops.StringOps(req).startsWith("=")) ops.StringOps(req).substring(1, req.size) else req

// The dependency requirements (name -> req) that the given vest_lib release declares
// in the crates.io sparse index; Some(empty) when the release is not in the index,
// None() when the index could not be fetched.
def vestLibRequirements(version: String): Option[Map[String, String]] = {
  val name = "vest_lib"
  val temp = Os.tempFix(s"crates-index-", s"-$name.json")
  temp.removeOnExit()
  if (!temp.downloadFrom(s"https://index.crates.io/${cratesIndexPath(name)}")) {
    return None()
  }
  var reqs = Map.empty[String, String]
  for (l <- temp.readLines if jsonField(l, "vers") == version) {
    // each dependency is an object that begins {"name":"<crate>","req":"<req>",...
    val entries = ops.StringOps(l).split((c: C) => c == '{')
    for (e <- entries) {
      val dep = jsonField(e, "name")
      val req = jsonField(e, "req")
      if (dep != "" && req != "") {
        reqs = reqs + dep ~> req
      }
    }
  }
  temp.remove()
  return Some(reqs)
}

// A GitHub API token, if one is at hand: GITHUB_TOKEN (set in GitHub Actions), else
// the gh CLI's when it is installed and logged in.
def githubToken(): Option[String] = {
  Os.env("GITHUB_TOKEN") match {
    case Some(t) if ops.StringOps(t).trim != "" => return Some(ops.StringOps(t).trim)
    case _ =>
  }
  val r = Os.proc(ISZ[String]("gh", "auth", "token")).run()
  val t = ops.StringOps(r.out).trim
  return if (r.ok && t != "") Some(t) else None()
}

// Downloads a GitHub API url into temp.  GitHub allows 60 unauthenticated API
// requests an hour per IP address -- shared by a whole campus or CI runner pool --
// and 5000 authenticated ones, so a token is used when one is at hand.  It reaches
// curl on stdin (-H @-), not on the command line, where other users could see it.
// Without a token, or if curl fails, the plain download is used, as before.
def githubApiDownload(temp: Os.Path, apiUrl: String): B = {
  githubToken() match {
    case Some(token) =>
      val r = Os.proc(ISZ[String]("curl", "-sfL", "-H", "@-", "-o", temp.string, apiUrl))
        .input(s"Authorization: Bearer $token\n").run()
      if (r.ok) {
        return T
      }
    case _ =>
  }
  return temp.downloadFrom(apiUrl)
}

// the commit each of the given submodule paths points at in one GitHub tree, e.g.
// submoduleTips("au-ts/lionsos", "main:dep", ISZ("sddf")).  None() means the tree
// could not be fetched -- GitHub's API allows 60 unauthenticated requests an hour
// per IP address, which CI runners share, so that is a real outcome rather than a
// hypothetical one; githubApiDownload authenticates when it can, which raises that
// to 5000.
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
  if (!githubApiDownload(temp, s"https://api.github.com/repos/$repo/git/trees/$treeIsh")) {
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

// the channel a rust-toolchain.toml's [toolchain] names, "" when it names none
@pure def toolchainChannel(lines: ISZ[String]): String = {
  var channel: String = ""
  var inToolchain = F
  for (l <- lines) {
    val o = ops.StringOps(ops.StringOps(l).trim)
    if (o.startsWith("[")) {
      inToolchain = o.startsWith("[toolchain]")
    } else if (inToolchain && channel == "" && o.startsWith("channel")) {
      channel = quoted(l)
    }
  }
  return channel
}

// The Verus release a Vest ref pins in its verus.json, with that release's Rust
// toolchain and Z3: (version, rust, z3), "" for any it does not declare.  None()
// when the file could not be fetched.
def vestVerusPin(ref: String): Option[(String, String, String)] = {
  rawLines("secure-foundations/vest", ref, "verus.json") match {
    case Some(lines) =>
      // verus.json puts one key per line and opens each object on its key's line,
      // so the enclosing keys are tracked to tell Verus' "version" from Z3's
      var path = ISZ[String]()
      var version: String = ""
      var rust: String = ""
      var z3: String = ""
      for (l <- lines) {
        val o = ops.StringOps(ops.StringOps(l).trim)
        if (o.endsWith("{")) {
          path = path :+ quoted(l) // "" for the outermost object
        } else if (o.startsWith("}")) {
          path = ops.ISZOps(path).dropRight(1)
        } else if (path == ISZ[String]("")) {
          if (version == "") {
            version = jsonField(l, "version")
          }
          if (rust == "") {
            rust = jsonField(l, "rust")
          }
        } else if (path == ISZ[String]("", "z3") && z3 == "") {
          z3 = jsonField(l, "version")
        }
      }
      return Some((version, rust, z3))
    case _ => return None()
  }
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

// The release an artifact's Maven Central metadata names as its newest.  None()/Some("")
// carry the same meanings as latestGitHubRelease's.
def latestMavenRelease(group: String, artifact: String): Option[String] = {
  val temp = Os.tempFix("maven-metadata-", ".xml")
  temp.removeOnExit()
  val groupPath = ops.StringOps(group).replaceAllChars('.', '/')
  if (!temp.downloadFrom(s"https://repo1.maven.org/maven2/$groupPath/$artifact/maven-metadata.xml")) {
    return None()
  }
  // <versioning><latest>..</latest><release>2.5.0</release>..., where release excludes
  // snapshots and latest does not
  var release: String = ""
  for (l <- temp.readLines if release == "") {
    val o = ops.StringOps(ops.StringOps(l).trim)
    if (o.startsWith("<release>") && o.endsWith("</release>")) {
      release = o.substring(9, o.size - 10)
    }
  }
  temp.remove()
  return Some(release)
}

var sbtAssemblyUnchecked = F

{ // sbt-assembly is the one plugin arsit's generated sbt projects add
  // (ProjectTemplate.sbtPluginsSbtContents).  It is versioned on its own rather than
  // with kekinian, so sbtassembly.version follows its latest release, and is updated
  // like the versions above.  The sbt 1.x plugin is published under the Scala 2.12 /
  // sbt 1.0 cross-version suffix.
  latestMavenRelease("com.eed3si9n", "sbt-assembly_2.12_1.0") match {
    case Some(latest) =>
      if (latest == "") {
        // fetched but with no release in it, so this check has gone blind
        exclamations()
        println("WARNING: the Maven Central metadata for sbt-assembly names no release, so sbtassembly.version was not checked")
        exclamations()
        sbtAssemblyUnchecked = T
      } else {
        codegenCurrentVers = codegenCurrentVers + ("sbtassembly.version" ~> latest)
      }
    case _ =>
      // Not drift, so not a failure: an unreachable Maven Central says nothing about
      // whether the pin is current.
      exclamations()
      println("WARNING: could not fetch the Maven Central metadata for sbt-assembly, so sbtassembly.version was not checked")
      exclamations()
  }
}

var artBehind = F

{ // art.version is the commit the hamr/codegen/art submodule is at, so it is the
  // latest ART only if the submodule is.  The tip of slang-embedded-art's master is
  // what it is compared with.  Reported rather than fixed: moving the submodule is a
  // commit of its own, after which this script picks the new commit up.
  val artRepo = "sireum/slang-embedded-art"
  val artVer = codegenCurrentVers.get("art.version").get
  latestCommit(artRepo, "master") match {
    case Some(tip) =>
      if (!ops.StringOps(tip).startsWith(artVer)) {
        exclamations()
        println(
          st"""WARNING: art.version is not the latest ART:
              |
              |  the hamr/codegen/art submodule is at $artVer, but $artRepo master is at ${ops.StringOps(tip).substring(0, 7)}
              |
              |  ${(SIREUM_HOME / "hamr" / "codegen" / "art").toUri}
              |  https://github.com/$artRepo/compare/$artVer...$tip""".render)
        exclamations()
        artBehind = T
      }
    case _ =>
      exclamations()
      println(s"WARNING: could not fetch the latest commit of $artRepo master, so art.version was not checked")
      exclamations()
  }
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

// Every microkit_versions.properties key some check below looks at.  Each check adds
// its own keys, so the coverage check at the end cannot drift from the checks.
var microkitKeysChecked = ISZ[String]()

var proversMisaligned = F

{ // provers-env pins the toolchain that generated Microkit systems are built
  // against, so what it installs has to be what codegen generates for: a system
  // emitted for one Verus, Microkit SDK or sDDF and built against another is a
  // failure at the user's `make`, not here.  The two files are in different
  // repositories, so a mismatch is reported rather than fixed -- whichever side
  // is behind has to be bumped deliberately.
  val proversVersionsUrl = "https://raw.githubusercontent.com/loonwerks/INSPECTA-models/main/provers-env/bin/versions.sh"

  // codegen's key, and the variable versions.sh pins the same tool as
  // and the prefix versions.sh writes before the version (Z3_VER is the z3 repository
  // tag, z3-<version>)
  val pins: ISZ[(String, String, String)] = ISZ(
    ("microkit-sdk", "MICROKIT_SDK_VER", ""),
    ("rust-channel", "RUST_TOOLCHAIN_VER", ""),
    ("sdfgen", "SDFGEN_VER", ""),
    ("verus-release", "VERUS_VER", ""),
    ("z3", "Z3_VER", "z3-"),
    ("lionsos", "LIONSOS_VER", ""))
  microkitKeysChecked = microkitKeysChecked ++ (for (pin <- pins) yield pin._1)

  // in the system temp dir, like the script's other downloads -- not next to the
  // script, where a run that ends before the exit hook leaves it in the repo
  val temp = Os.tempFix("provers-versions-", ".sh")
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
    temp.remove()

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
        val ours = s"${pin._3}${microkitVers.get(key).get}"
        val theirs = proversVers.get(envVar).get
        if (ours != theirs) {
          mismatches = mismatches :+ st"$key = ${microkitVers.get(key).get}, but versions.sh pins $envVar = $theirs"
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

// The text of an HTML page with its tags dropped.  The documentation sites
// syntax-highlight their code blocks, which splits e.g. sdfgen==0.35.0 across three
// spans, so what a reader sees is only found once the tags are gone.
@pure def stripTags(html: String): String = {
  val cis = conversions.String.toCis(html)
  val buf = MSZ.create(cis.size, ' ')
  var n: Z = 0
  var inTag = F
  for (c <- cis) {
    if (c == '<') {
      inTag = T
    } else if (c == '>' && inTag) {
      inTag = F
    } else if (!inTag) {
      buf(n) = c
      n = n + 1
    }
  }
  return ops.StringOps(conversions.String.fromCms(buf)).substring(0, n)
}

// The distinct values text shows right after prefix, in order: the longest run of
// digits and dots, and of hex letters too when hex is set (a commit, or a version
// ending in one).  A run that ends a sentence would otherwise keep its full stop.
@pure def valuesAfter(text: String, prefix: String, hex: B): ISZ[String] = {
  val textOps = ops.StringOps(text)
  val cis = conversions.String.toCis(text)
  var r = ISZ[String]()
  var i = textOps.stringIndexOf(prefix)
  while (i >= 0) {
    val start = i + prefix.size
    var j = start
    while (j < cis.size && (('0' <= cis(j) && cis(j) <= '9') || cis(j) == '.' ||
      (hex && 'a' <= cis(j) && cis(j) <= 'f'))) {
      j = j + 1
    }
    while (j > start && cis(j - 1) == '.') {
      j = j - 1
    }
    if (j > start) {
      val v = textOps.substring(start, j)
      if (!ops.ISZOps(r).contains(v)) {
        r = r :+ v
      }
    }
    i = textOps.stringIndexOfFrom(prefix, start)
  }
  return r
}

// The distinct Sireum release tags text shows -- 4.<yyyymmdd>.<commit>, e.g.
// 4.20261021.b1efad5d -- so that neither dev nor master, nor a version such as
// Cosmopolitan's 4.0.2, is mistaken for one.
@pure def releaseTags(text: String): ISZ[String] = {
  @strictpure def isTag(v: String): B = {
    val parts = ops.StringOps(v).split((c: C) => c == '.')
    parts.size == 2 && parts(0).size == 8 && parts(1).size >= 7 &&
      ops.ISZOps(conversions.String.toCis(parts(0))).forall((c: C) => '0' <= c && c <= '9')
  }
  return for (v <- valuesAfter(text, "4.", T) if isTag(v)) yield s"4.$v"
}

// The release GitHub marks as kekinian's latest, which both documentation sites
// show as the one to install.  The API rather than latestGitHubRelease's atom feed:
// the feed lists the moving dev pre-release too, and titles each entry with the
// release's name, not its tag.  None() when it could not be fetched.
def fetchLatestSireumRelease(): Option[String] = {
  val temp = Os.tempFix("kekinian-latest-", ".json")
  temp.removeOnExit()
  var tag: String = ""
  if (githubApiDownload(temp, "https://api.github.com/repos/sireum/kekinian/releases/latest")) {
    for (l <- temp.readLines if tag == "") {
      tag = jsonField(l, "tag_name")
    }
  }
  temp.remove()
  if (tag == "") {
    exclamations()
    println("WARNING: could not look up kekinian's latest release, so the release the documentation shows was not checked")
    exclamations()
    return None()
  }
  return Some(tag)
}
val latestSireumRelease: Option[String] = fetchLatestSireumRelease()

// a line saying what is wrong with the release tags a page shows, if anything
def releaseTagProblem(text: String, placeholder: String): Option[ST] = {
  latestSireumRelease match {
    case Some(latest) =>
      if (ops.StringOps(text).contains(placeholder)) {
        return Some(st"Sireum release: the page shows the placeholder $placeholder; publish.sh could not resolve it")
      }
      val tags = releaseTags(text)
      if (tags.isEmpty) {
        // reworded on the page, so this check has gone blind to it
        return Some(st"Sireum release: the page no longer shows a release tag; update this check")
      } else if (tags != ISZ(latest)) {
        return Some(st"the latest Sireum release is $latest, but the page shows ${(tags, ", ")}")
      }
      return None()
    case _ => return None()
  }
}

var installationPageStale = F

{ // The HAMR installation page tells readers which Verus, Rust toolchain, Microkit
  // SDK, sdfgen and LionsOS to install, and gives the latest Sireum release as its
  // example of a numbered one.  None of it is hand-written: website-hamr's
  // publish.sh fills the tools in from this file (Rust from the Verus release's
  // rust-toolchain.toml, which the Verus check below holds rust-channel to) and the
  // release from GitHub, so the page cannot disagree with what it was published
  // from.  But it is only as current as its last publish, and nothing republishes it
  // when a pin moves or a release is cut -- so what the live page shows is compared
  // with the pins here and with the latest release.
  val installationUrl = "https://hamr.sireum.org/hamr-doc/installation/"
  // codegen's key; the text the page puts right before the value; whether the value
  // may contain hex letters (a commit, or Verus's trailing commit) rather than only
  // digits and dots; and the placeholder publish.sh replaces with it
  val shown: ISZ[(String, String, B, String)] = ISZ(
    ("verus-release", "Version: ", T, "VERUS-RELEASE-VERSION"),
    ("rust-channel", "--toolchain ", F, "RUST-TOOLCHAIN-VERSION"),
    ("microkit-sdk", "microkit-sdk-", F, "MICROKIT-SDK-VERSION"),
    ("sdfgen", "sdfgen==", F, "SDFGEN-VERSION"),
    ("lionsos", "git checkout ", T, "LIONSOS-COMMIT"))
  val temp = Os.tempFix("hamr-installation-", ".html")
  temp.removeOnExit()
  if (!temp.downloadFrom(installationUrl)) {
    // Not stale, so not a failure: an unreachable site says nothing about whether
    // it is current.
    exclamations()
    println(s"WARNING: could not fetch $installationUrl, so the versions it documents were not checked")
    exclamations()
  } else {
    val text = stripTags(temp.read)
    temp.remove()
    val microkitVers = microkitVersionsP.properties
    var stale = ISZ[ST]()
    for (s <- shown) {
      val key = s._1
      if (!microkitVers.contains(key)) {
        halt(s"$microkitVersionsP doesn't contain $key")
      }
      val ours = microkitVers.get(key).get
      if (ops.StringOps(text).contains(s._4)) {
        stale = stale :+ st"$key: the page shows the placeholder ${s._4}; publish.sh could not resolve it"
      } else {
        val values = valuesAfter(text, s._2, s._3)
        if (values.isEmpty) {
          // reworded on the page, so this check has gone blind to that tool
          stale = stale :+ st"""$key: the page no longer shows a version after "${s._2}"; update this check"""
        } else if (values != ISZ(ours)) {
          stale = stale :+ st"$key = $ours, but the page shows ${(values, ", ")}"
        }
      }
    }
    releaseTagProblem(text, "SIREUM-RELEASE-VERSION") match {
      case Some(p) => stale = stale :+ p
      case _ =>
    }

    if (stale.nonEmpty) {
      exclamations()
      println(
        st"""WARNING: the HAMR installation page documents different versions than codegen pins
            |or than the latest Sireum release:
            |
            |  ${(stale, "\n")}
            |
            |  Once the pins are on kekinian master (publish.sh reads them from there),
            |  republish the site with website-hamr's publish.sh.
            |  $installationUrl
            |  ${microkitVersionsP.toUri}""".render)
      exclamations()
      installationPageStale = T
    }
  }
}

var sireumOrgStale = F

{ // sireum.org's getting-started page gives the latest release's install commands.
  // Its tag is not hand-written either -- website-sireum's publish.sh replaces the
  // XX.YY.ZZ placeholder with the release GitHub marks as latest -- but the page
  // keeps the tag it was published with until it is published again, so a release
  // cut since then leaves it pointing at the previous one.
  //
  // Only the two "Latest Release" sections are compared: the binary distributions
  // (#latest-release-bin) and the installer scripts (#latest-release), which take
  // the same placeholder.  The rest of the page pins releases deliberately -- a
  // course's section names the one its students use.
  val gettingStartedUrl = "https://sireum.org/getting-started/"
  val sections: ISZ[String] = ISZ("latest-release-bin", "latest-release")
  if (latestSireumRelease.nonEmpty) {
    val temp = Os.tempFix("sireum-getting-started-", ".html")
    temp.removeOnExit()
    if (!temp.downloadFrom(gettingStartedUrl)) {
      exclamations()
      println(s"WARNING: could not fetch $gettingStartedUrl, so the release it documents was not checked")
      exclamations()
    } else {
      val html = temp.read
      temp.remove()
      val htmlOps = ops.StringOps(html)
      var stale = ISZ[ST]()
      for (id <- sections) {
        // A section runs from its heading to the next heading of the same or a
        // higher level; both sections are h5, and their subsections h6.
        val i = htmlOps.stringIndexOf(s"""id="$id"""")
        if (i < 0) {
          stale = stale :+ st"#$id: the page no longer has this section; update this check"
        } else {
          val ends: ISZ[Z] = for (h <- ISZ[String]("<h5", "<h4", "<h3", "<h2")) yield htmlOps.stringIndexOfFrom(h, i)
          var end = html.size
          for (e <- ends if e >= 0 && e < end) {
            end = e
          }
          releaseTagProblem(stripTags(htmlOps.substring(i, end)), "XX.YY.ZZ") match {
            case Some(p) => stale = stale :+ st"#$id: $p"
            case _ =>
          }
        }
      }

      if (stale.nonEmpty) {
        exclamations()
        println(
          st"""WARNING: sireum.org's getting-started page does not show the latest Sireum release:
              |
              |  ${(stale, "\n")}
              |
              |  Republish sireum.org with website-sireum's publish.sh.
              |  $gettingStartedUrl""".render)
        exclamations()
        sireumOrgStale = T
      }
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

  microkitKeysChecked = microkitKeysChecked :+ "microkit-sdk"
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
  //
  // vest_lib and vest are included: a newer Vest release usually means a newer
  // Verus, which verus-release follows (see the Vest check below).
  val cratesIoKeys = ISZ[String](
    "linux-raw-sys", "log", "linked_list_allocator",                        // dependencies
    "lazy_static", "once_cell", "serial_test", "proptest", "env_logger",    // dev-dependencies
    "vest_lib", "vest")                                                     // Vest runtime library and generator

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

  microkitKeysChecked = microkitKeysChecked ++ cratesIoKeys ++ sel4Keys
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
  microkitKeysChecked = microkitKeysChecked ++ ISZ[String]("verus-release") ++ (for (crate <- crates) yield crate._2)
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

  // Verus refuses to run against any Z3 but the one it was released with, and not
  // every release asset bundles it (0.2026.09.06's did not), so the z3 pin -- which
  // provers-env installs when the asset has none -- must be that version.
  // tools/get-z3.sh is where Verus records it.
  if (!microkitVers.contains("z3")) {
    halt(s"$microkitVersionsP doesn't contain z3")
  }
  rawLines(verusRepo, ref, "source/tools/get-z3.sh") match {
    case Some(lines) =>
      var z3Ver: String = ""
      for (l <- lines) {
        val o = ops.StringOps(ops.StringOps(l).trim)
        if (o.startsWith("z3_version=")) {
          z3Ver = ops.StringOps(o.substring(11, o.size)).replaceAllLiterally("\"", "")
        }
      }
      val recorded = microkitVers.get("z3").get
      if (z3Ver == "") {
        mismatches = mismatches :+ st"source/tools/get-z3.sh declares no z3_version at $ref"
      } else if (z3Ver != recorded) {
        mismatches = mismatches :+ st"z3 = $recorded, but Verus $release requires $z3Ver"
      }
    case _ => unchecked = unchecked :+ "z3"
  }

  // Likewise the Rust toolchain: Verus is built against one, named by the
  // rust-toolchain.toml at the release tag, and rust-channel -- what the generated
  // crates select through their own rust-toolchain.toml -- must be that one.
  if (!microkitVers.contains("rust-channel")) {
    halt(s"$microkitVersionsP doesn't contain rust-channel")
  }
  microkitKeysChecked = microkitKeysChecked ++ ISZ[String]("z3", "rust-channel")
  rawLines(verusRepo, ref, "rust-toolchain.toml") match {
    case Some(lines) =>
      val channel = toolchainChannel(lines)
      val recorded = microkitVers.get("rust-channel").get
      if (channel == "") {
        mismatches = mismatches :+ st"rust-toolchain.toml declares no channel at $ref"
      } else if (channel != recorded) {
        mismatches = mismatches :+ st"rust-channel = $recorded, but Verus $release requires Rust $channel"
      }
    case _ => unchecked = unchecked :+ "rust-channel"
  }

  if (mismatches.nonEmpty) {
    exclamations()
    println(
      st"""WARNING: the recorded Verus crates, Z3 and Rust toolchain are not the ones the pinned Verus release declares:
          |
          |  ${(mismatches, "\n")}
          |
          |  ${microkitVersionsP.toUri}
          |  https://github.com/$verusRepo/tree/$ref""".render)
    exclamations()
    verusCratesDrifted = T
  }

  if (unchecked.nonEmpty) {
    // Not drift, so not a failure: an unreachable GitHub says nothing about whether
    // the crates agree.
    exclamations()
    println(st"WARNING: could not read the Verus release files for ${(unchecked, ", ")} at $ref, so they were not checked".render)
    exclamations()
  }
}

var vestMisaligned = F
var vestVerusMoved = F

{ // verus-release follows Vest rather than the latest Verus: vest_lib pins vstd (and
  // the builtin crates) exactly, so a developer crate generated from a .vest format
  // can only be a dependency of a generated component when both sides use the same
  // Verus crates.  The pinned vest_lib release's own requirements are read from the
  // crates.io index and compared with the Verus crates recorded above.
  val verusCrates = ISZ[String]("vstd", "verus_builtin", "verus_builtin_macros")
  val microkitVers = microkitVersionsP.properties
  for (key <- ISZ[String]("vest_lib", "vest") if !microkitVers.contains(key)) {
    halt(s"$microkitVersionsP doesn't contain $key")
  }
  val vestLib = microkitVers.get("vest_lib").get

  vestLibRequirements(vestLib) match {
    case Some(reqs) =>
      var mismatches = ISZ[ST]()
      for (crate <- verusCrates) {
        reqs.get(crate) match {
          case Some(req) =>
            val recorded = microkitVers.get(crate).get
            if (dropEq(req) != recorded) {
              mismatches = mismatches :+ st"$crate = $recorded, but vest_lib $vestLib requires $req"
            }
          case _ => // vest_lib does not depend on this crate directly, so it cannot conflict
        }
      }
      if (reqs.isEmpty) {
        mismatches = mismatches :+ st"vest_lib $vestLib was not found in the crates.io index"
      }
      if (mismatches.nonEmpty) {
        exclamations()
        println(
          st"""WARNING: the recorded Verus crates are not the ones Vest requires, so Vest-generated
              |crates cannot be used by generated components:
              |
              |  ${(mismatches, "\n")}
              |
              |  ${microkitVersionsP.toUri}
              |  https://crates.io/crates/vest_lib/$vestLib/dependencies""".render)
        exclamations()
        vestMisaligned = T
      }
    case _ =>
      // Not drift, so not a failure: an unreachable index says nothing about alignment.
      exclamations()
      println(s"WARNING: could not fetch the crates.io index entry for vest_lib, so its Verus crates were not checked")
      exclamations()
  }

  // Vest also records the Verus release itself, with that release's Rust toolchain
  // and Z3, in verus.json.  At the pinned vest_lib release's tag it is what
  // verus-release, rust-channel and z3 must match; main is where Vest moves to a
  // newer Verus first, before releasing it, so a difference there is the warning
  // that verus-release is about to have to follow.
  val vestRepo = "secure-foundations/vest"
  val vestTag = s"vest_lib-v$vestLib"
  for (key <- ISZ[String]("verus-release", "rust-channel", "z3") if !microkitVers.contains(key)) {
    halt(s"$microkitVersionsP doesn't contain $key")
  }
  val verusRelease = microkitVers.get("verus-release").get
  val rustChannel = microkitVers.get("rust-channel").get
  val z3Pin = microkitVers.get("z3").get

  vestVerusPin(vestTag) match {
    case Some((version, rust, z3)) =>
      var mismatches = ISZ[ST]()
      if (version == "") {
        mismatches = mismatches :+ st"verus.json declares no Verus version at $vestTag"
      } else if (version != verusRelease) {
        mismatches = mismatches :+ st"verus-release = $verusRelease, but vest_lib $vestLib pins Verus $version"
      }
      if (rust != "" && rust != rustChannel) {
        mismatches = mismatches :+ st"rust-channel = $rustChannel, but vest_lib $vestLib pins Rust $rust"
      }
      if (z3 != "" && z3 != z3Pin) {
        mismatches = mismatches :+ st"z3 = $z3Pin, but vest_lib $vestLib pins Z3 $z3"
      }
      if (mismatches.nonEmpty) {
        exclamations()
        println(
          st"""WARNING: the pinned Verus is not the one Vest is built against:
              |
              |  ${(mismatches, "\n")}
              |
              |  ${microkitVersionsP.toUri}
              |  https://github.com/$vestRepo/blob/$vestTag/verus.json""".render)
        exclamations()
        vestMisaligned = T
      }
    case _ =>
      exclamations()
      println(s"WARNING: could not fetch verus.json at $vestRepo $vestTag, so verus-release was not checked against Vest")
      exclamations()
  }

  vestVerusPin("main") match {
    case Some((version, rust, _)) =>
      if (version == "") {
        // fetched but unreadable, so this check has gone blind in a way a retry will not fix
        exclamations()
        println(s"WARNING: verus.json on $vestRepo main declares no Verus version, so Vest's next Verus was not checked")
        exclamations()
        vestVerusMoved = T
      } else if (version != verusRelease) {
        exclamations()
        println(
          st"""WARNING: Vest has moved to a different Verus than verus-release pins:
              |
              |  verus-release = $verusRelease, but Vest main pins Verus $version${if (rust == "") "" else s" (Rust $rust)"}
              |
              |  Bumping verus-release goes with the vest_lib release that pins it, and with
              |  the Verus crates, z3 and rust-channel recorded above, and provers-env's
              |  VERUS_VER, Z3_VER and RUST_TOOLCHAIN_VER.
              |
              |  ${microkitVersionsP.toUri}
              |  https://github.com/$vestRepo/blob/main/verus.json""".render)
        exclamations()
        vestVerusMoved = T
      }
    case _ =>
      exclamations()
      println(s"WARNING: could not fetch verus.json on $vestRepo main, so Vest's next Verus was not checked")
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
  microkitKeysChecked = microkitKeysChecked ++ ISZ[String]("lionsos", "sdfgen")
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

var microkitKeysUnchecked = F

{ // A pin added to microkit_versions.properties without a check -- as
  // linked_list_allocator first was -- would otherwise drift silently.  Every key
  // must either be looked at by one of the checks above or be listed here with the
  // reason it is not.
  val notChecked: Map[String, String] = Map.empty[String, String] +
    "r2u2" ~> "published on crates.io as r2u2_core and r2u2_cli rather than under this key, so the crates.io check cannot look it up by name" +
    "r2u2-c" ~> "a tag in github.com/R2U2/r2u2 rather than a crates.io release, and nothing checks that repository's tags yet"

  val checked = ops.ISZOps(microkitKeysChecked)
  val missing: ISZ[String] = for (key <- microkitVersionsP.properties.keys
                                  if !checked.contains(key) && !notChecked.contains(key)) yield key
  if (missing.nonEmpty) {
    exclamations()
    println(
      st"""WARNING: microkit_versions.properties has entries that no check covers:
          |
          |  ${(missing, "\n  ")}
          |
          |  Add each to the check that fits it (e.g. cratesIoKeys for a crates.io crate),
          |  or to notChecked above with the reason it is not checked.
          |  ${microkitVersionsP.toUri}""".render)
    exclamations()
    microkitKeysUnchecked = T
  }
  for (key <- notChecked.keys if !microkitVersionsP.properties.contains(key)) {
    // an exemption for a pin that no longer exists is dead weight
    println(s"NOTE: notChecked lists $key, which microkit_versions.properties no longer has")
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

// In CI only what this project has to fix itself fails: the documentation sites or
// provers-env disagreeing with what codegen pins.  Everything else is upstream
// moving on -- a newer crate, Microkit, ART or LionsOS, Vest taking a newer Verus --
// which is worth hearing about but not worth a red build, so it goes to the report
// for the workflow to file as an issue.  changesDetected is left out altogether:
// the properties it rewrites follow the local kekinian checkout, which in CI is
// always newer than what was last committed.
if (ciMode) {
  val upstream: ISZ[(B, String)] = ISZ(
    (microkitOutdated, "microkit-sdk is not the latest Microkit release"),
    (cratesOutdated, "pinned crates have newer crates.io releases"),
    (verusCratesDrifted, "the recorded Verus crates, Z3 or Rust toolchain are not the ones verus-release declares"),
    (vestMisaligned, "the pinned Verus or Verus crates are not the ones Vest is built against"),
    (vestVerusMoved, "Vest has moved to a different Verus than verus-release"),
    (lionsosDrifted, "the LionsOS dependencies have moved"),
    (artBehind, "art.version is not the latest ART"),
    (sbtAssemblyUnchecked, "sbt-assembly could not be checked"),
    (microkitKeysUnchecked, "microkit_versions.properties has entries no check covers"))
  val moved: ISZ[String] = for (u <- upstream if u._1) yield u._2
  reportOpt match {
    case Some(report) =>
      if (moved.isEmpty) {
        report.writeOver("")
      } else {
        report.writeOver(st"""${(for (m <- moved) yield st"- $m", "\n")}
                             |""".render)
      }
    case _ =>
  }
  if (moved.nonEmpty) {
    println(st"""
                |Upstream has moved on (reported, not failed, in CI):
                |  ${(moved, "\n")}""".render)
  }
  if (proversMisaligned || installationPageStale || sireumOrgStale) {
    Os.exit(1) // provers-env or a documentation site disagrees with codegen's pins
  } else {
    Os.exit(0)
  }
}

// changesDetected on its own, not 'changesDetected && !noUpdate'.  Under no-update
// the properties files are left as they are, but the drift that was found is just
// as real -- and no-update is how VersionCheck runs this, so gating the exit on it
// meant that test could never fail on the versions it exists to watch.
if (changesDetected || proversMisaligned || microkitOutdated || cratesOutdated || verusCratesDrifted || vestMisaligned ||
    vestVerusMoved || lionsosDrifted || microkitKeysUnchecked || sbtAssemblyUnchecked || artBehind ||
    installationPageStale || sireumOrgStale) {
  Os.exit(1) // versions have changed, or drifted from provers-env, the documentation sites or the latest upstream releases
} else {
  Os.exit(0)
}
