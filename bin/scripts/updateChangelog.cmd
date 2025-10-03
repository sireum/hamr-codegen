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
val existing = ops.StringOps(changelog.read)

val tag = existing.stringIndexOf("<!-- released -->")
assert (tag > 0 && tag < existing.s.size)

val releasedSection = ops.StringOps(existing.substring(tag + string"<!-- released -->".size + 1, existing.s.size))

var newContent: ISZ[ST] = ISZ()
var dev = st""

for(e <- released.entries) {
  val dest = s"https://github.com/sireum/kekinian/releases/tag/${e._1.tag}"
  val content =
    st"""<!-- begin ${e._1.tag} -->
        |# [${e._1.tag}]($dest) ${if (e._1.tag == "dev") s" <font size=3>as of ${e._1.tagDate.format(java.time.format.DateTimeFormatter.ofPattern(string"yyyy-MM-dd".native))}</font>" else "" }
        |
        |<details><summary>How to build</summary>
        |
        |```
        |git clone --rec --depth 1 --branch release/${e._1.tag} https://github.com/sireum/kekinian.git
        |cd kekinian
        |./bin/build.cmd
        |```
        |
        |</details>
        |
        |<details><summary>Commits</summary>
        |
        |${(for (c <- e._2) yield st"* ${c.pretty}", "\n\n")}
        |</details>
        |<br>
        |<!-- end ${e._1.tag} -->
        |"""

  if (e._1.tag == "dev") {
    dev = content
  } else if (!releasedSection.contains(s"<!-- begin ${e._1.tag} -->")) {
    newContent = newContent :+ content
  }
}

val newContentOpt: Option[ST] =
  if (newContent.isEmpty) None()
  else Some(
    st"""${(newContent, "\n\n")}
        |""")

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
      |$newContentOpt
      |${releasedSection.s}""".render)

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
}

