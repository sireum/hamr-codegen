// #Sireum

import org.sireum._

import java.time.ZonedDateTime
import java.time.Instant
import java.time.ZoneId
import Containers._

val sireumHome = Sireum.homeOpt.get
val codegenHome = sireumHome / "hamr" / "codegen"

Os.proc(ISZ("bash", "-c", "git tag -l | xargs git tag -d")).at(sireumHome).runCheck()
proc"git fetch --tags".at(sireumHome).runCheck()

val codegenCommits = ops.StringOps(proc"git log --pretty=format:%H|%ct|%s".at(codegenHome).runCheck().out).split(c => c == '\n')
var commits: ISZ[commit] = ISZ()
for (e <- codegenCommits) {
  val content = ops.StringOps(e).split(c => c == '|')
  commits = commits :+ commit(content(0), Z(content(1)).get, content(2))
}

val currentCodegenCommitTS = Z(ops.StringOps(Os.proc(ISZ("bash", "-c", s"git ls-tree HEAD hamr/codegen  | awk '{print $$3}' | xargs -I{} git -C hamr/codegen show -s --format=%ct {}")).at(sireumHome).runCheck().out).trim).get

val tags = ops.StringOps(proc"git for-each-ref --sort=committerdate --format=%(refname:short)|%(committerdate:unix) refs/tags".at(sireumHome).runCheck().out).split(c => c == '\n')
var releases: ISZ[release] = ISZ()
for (t <- tags) {
  val content = ops.StringOps(t).split(c => c == '|')
  val hamrCommitTS = ops.StringOps(Os.proc(ISZ("bash", "-c", s"git ls-tree -r ${content(0)} hamr/codegen  | awk '{print $$3}' | xargs -I{} git -C hamr/codegen show -s --format=%ct {}")).at(sireumHome).runCheck().out).trim
  releases = releases :+ release(tag = content(0), tagTimeStamp = Z(content(1)).get, codegenTimeStamp = Z(hamrCommitTS).get)
}

var released = HashSMap.empty[release, ISZ[commit]]
var preRelease: ISZ[commit] = ISZ()

for (commit <- commits) {
  var placed = F
  for(release <- releases if !placed) {
    if (commit.date.compareTo(release.codegenDate) <= 0) {
      if (!released.contains(release)) {
        released = released + release ~> ISZ()
      }
      released = released + release ~> (released.get(release).get :+ commit)
      placed = T
    }
  }
  if (!placed && commit.timeStamp <= currentCodegenCommitTS) {
    preRelease = preRelease :+ commit
  }
}

val changelog = codegenHome / "changelog.md"
val existing = changelog.read

// Every part of a release section except its hand-written notes is regenerated from git on each
// run, so the notes are all that has to be carried over from the previous changelog.
var proseMap = HashMap.empty[String, ST]
for (e <- released.entries) {
  proseOf(existing, e._1.tag) match {
    case Some(p) => proseMap = proseMap + e._1.tag ~> p
    case _ =>
  }
}

// install.cmd is a release asset that is published for recent releases and pruned from older ones,
// so presence is probed on every run rather than remembered: the 'How to install' section of a
// release that has lost its asset since the last run goes away with it.
val installable = installCmdTags(for (e <- released.entries) yield e._1.tag)

var releasedContent: ISZ[ST] = ISZ()
var dev = st""

for (e <- released.entries) {
  val tag = e._1.tag
  val isDev = tag == string"dev"
  val dest = s"https://github.com/sireum/kekinian/releases/tag/$tag"

  // dev is a moving tag, so record which kekinian commit it currently points at
  val suffix: ST =
    if (isDev) {
      val full = ops.StringOps(proc"git rev-list -n 1 $tag".at(sireumHome).runCheck().out).trim
      val h = ops.StringOps(full).substring(0, 8)
      st" <font size=3>as of ${e._1.tagDate.format(java.time.format.DateTimeFormatter.ofPattern(string"yyyy-MM-dd".native))} (kekinian commit tip [$h](https://github.com/sireum/kekinian/tree/$full))</font>"
    } else st""

  var sections: ISZ[ST] = ISZ()

  proseMap.get(tag) match {
    case Some(p) => sections = sections :+ p
    case _ =>
  }

  if (installable.contains(tag)) {
    // the dev binaries are rebuilt from whatever dev points at when they are published, which is
    // not necessarily the tip recorded above, so say so rather than let the two be read as one
    val install: ST =
      if (isDev)
        st"""<details><summary>How to install</summary>
            |
            |Follow the [latest development version](https://sireum.org/getting-started/#latest-dev-bin) instructions.
            |
            |Note that this installs the dev binaries as they were last published, built from whichever commit the
            |``dev`` tag pointed at when they were built -- not necessarily the commit tip recorded above.
            |
            |</details>"""
      else
        st"""<details><summary>How to install</summary>
            |
            |Follow the [latest release version](https://sireum.org/getting-started/#latest-release-bin) instructions,
            |with ``SIREUM_V`` set to this release's tag:
            |
            |```
            |export SIREUM_V=$tag
            |```
            |
            |</details>"""
    sections = sections :+ install
  }

  sections = sections :+
    st"""<details><summary>How to build</summary>
        |
        |```
        |git clone --rec --depth 1 --branch $tag https://github.com/sireum/kekinian.git
        |cd kekinian
        |./bin/build.cmd
        |```
        |
        |</details>"""

  sections = sections :+
    st"""<details><summary>Commits</summary>
        |
        |${(for (c <- e._2) yield st"* ${c.pretty}", "\n\n")}
        |</details>"""

  val content =
    st"""<!-- begin $tag -->
        |# [$tag]($dest) $suffix
        |
        |${(sections, "\n\n")}
        |<br>
        |<!-- end $tag -->
        |"""

  if (isDev) {
    dev = content
  } else {
    releasedContent = releasedContent :+ content
  }
}

val preReleaseContent =
  st"""<!-- begin pre-release -->
      |# Pre-Release
      |
      |<details><summary>How to build</summary>
      |
      |```
      |git clone --rec --depth 1 https://github.com/sireum/kekinian.git
      |cd kekinian
      |./bin/build.cmd
      |```
      |
      |</details>
      |
      |<details><summary>Commits</summary>
      |
      |${(for (c <- preRelease) yield st"* ${c.pretty}", "\n\n")}
      |</details>
      |<br>
      |<!-- end pre-release -->
      |"""

changelog.writeOver(
  st"""*Last Updated ${java.time.LocalDate.now()}*
      |
      |$preReleaseContent
      |$dev
      |<!-- released -->
      |
      |${(releasedContent, "\n")}""".render)

println(s"Wrote: $changelog")

object Containers {
  @datatype class commit(val hashy: String,
                         val timeStamp: Z,
                         val message: String) {
    @pure def pretty: ST = {
      val s = ops.StringOps(hashy).substring(0, 7)
      return st"[$s](https://github.com/sireum/hamr-codegen/commit/$s) $message"
    }

    @pure def date: ZonedDateTime = {
      return Instant.ofEpochSecond(timeStamp.toLong).atZone(ZoneId.systemDefault());
    }
  }

  @datatype class release(val tag: String,
                          val tagTimeStamp: Z,
                          val codegenTimeStamp: Z) {
    @pure def tagDate: ZonedDateTime = {
      return Instant.ofEpochSecond(tagTimeStamp.toLong).atZone(ZoneId.systemDefault());
    }

    @pure def codegenDate: ZonedDateTime = {
      return Instant.ofEpochSecond(codegenTimeStamp.toLong).atZone(ZoneId.systemDefault());
    }
  }

  // A release section's hand-written notes are the text between its '# [<tag>]' heading and the
  // first <details> block.  Sections have historically been emitted more than once, and the extra
  // copies are ones this script wrote without the notes, so the richest copy is the one to keep.
  @pure def proseOf(text: String, tag: String): Option[ST] = {
    val o = ops.StringOps(text)
    val begin = s"<!-- begin $tag -->"
    val end = s"<!-- end $tag -->"
    var best: Option[String] = None()
    var bestSize: Z = 0
    var from = o.stringIndexOf(begin)
    while (from >= 0) {
      val stop = o.stringIndexOfFrom(end, from)
      if (stop < 0) {
        from = -1
      } else {
        val block = ops.StringOps(o.substring(from, stop))
        val heading = block.stringIndexOf("# [")
        val details = block.stringIndexOf("<details>")
        if (heading >= 0 && details > heading) {
          val eol = block.indexOfFrom('\n', heading)
          if (eol >= 0 && eol < details) {
            val p = ops.StringOps(block.substring(eol + 1, details)).trim
            if (p.size > bestSize) {
              best = Some(p)
              bestSize = p.size
            }
          }
        }
        from = o.stringIndexOfFrom(begin, stop)
      }
    }
    best match {
      case Some(p) => return Some(st"$p")
      case _ => return None()
    }
  }

  // Probed in one pass rather than one process per release: a HEAD of each release's install.cmd
  // download URL, run through xargs so the ~90 of them do not go out one at a time.
  def installCmdTags(tgs: ISZ[String]): HashSet[String] = {
    var r = HashSet.empty[String]
    if (tgs.isEmpty) {
      return r
    }
    val p = Os.proc(ISZ[String]("xargs", "-P", "16", "-I@", "curl", "-sIL", "-o", "/dev/null",
      "-w", "@ %{http_code}\n",
      "https://github.com/sireum/kekinian/releases/download/@/install.cmd")).
      input(st"${(tgs, "\n")}".render).run()
    var codes = HashMap.empty[String, String]
    for (line <- ops.StringOps(p.out).split(c => c == '\n')) {
      val parts = ops.StringOps(ops.StringOps(line).trim).split(c => c == ' ')
      if (parts.size == 2) {
        codes = codes + parts(0) ~> parts(1)
      }
    }
    // A probe that neither found the asset (200) nor found it gone (404) means this run could not
    // reach GitHub.  Taking that for 'no install.cmd' would quietly strip the 'How to install'
    // section from every release, so make it stop the run instead.
    var unreachable: ISZ[String] = ISZ()
    for (t <- tgs) {
      codes.get(t) match {
        case Some(code) =>
          if (code == string"200") {
            r = r + t
          } else if (code != string"404") {
            unreachable = unreachable :+ s"$t ($code)"
          }
        case _ => unreachable = unreachable :+ s"$t (no response)"
      }
    }
    if (unreachable.nonEmpty) {
      halt(st"Could not determine install.cmd availability for: ${(unreachable, ", ")}".render)
    }
    return r
  }
}
