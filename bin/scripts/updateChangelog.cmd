// #Sireum

import org.sireum._

import java.time.ZonedDateTime
import java.time.Instant
import java.time.ZoneId
import Containers._

val sireumHome = Sireum.homeOpt.get
val codegenHome = sireumHome / "hamr" / "codegen"

// git tag -l | xargs git tag -d   # delete all local tags
// git fetch --tags                # fetch fresh tags from remote

val codegenCommits = ops.StringOps(proc"git log --pretty=format:%H|%ct|%s".at(codegenHome).runCheck().out).split(c => c == '\n')
var commits: ISZ[commit] = ISZ()
for (e <- codegenCommits) {
  val content = ops.StringOps(e).split(c => c == '|')
  commits = commits :+ commit(content(0), Z(content(1)).get, content(2))
}

val tags = ops.StringOps(proc"git for-each-ref --sort=creatordate --format=%(refname:short)|%(creatordate:unix) refs/tags".at(sireumHome).runCheck().out).split(c => c == '\n')
var releases: ISZ[release] = ISZ()
for (t <- tags) {
  val content = ops.StringOps(t).split(c => c == '|')
  releases = releases :+ release(content(0), Z(content(1)).get)
}

var m = HashSMap.empty[String, ISZ[commit]]
var postRelease: ISZ[commit] = ISZ()

for (c <- commits) {
  //val co = ops.StringOps(c.message)
  var placed = F //co.contains("update submodule")
  for(r <- releases if !placed && r.tag != "dev") {
    if (c.date.compareTo(r.date) <= 0) {
      if (!m.contains(r.tag)) {
        m = m + r.tag ~> ISZ()
      }
      m = m + r.tag ~> (m.get(r.tag).get :+ c)
      placed = T
    }
  }
  if (!placed) {
    postRelease = postRelease :+ c
  }
}

val changelog = codegenHome / "changelog.md"
val existing = ops.StringOps(changelog.read)
var newContent: ISZ[ST] = ISZ()
//for(e <- m.entries if !existing.contains(e._1)) {
for(e <- m.entries) {
  val dest = s"https://github.com/sireum/kekinian/releases/tag/${e._1}"
  newContent = newContent :+
    st"""# [${e._1}]($dest)
        |
        |<details><summary>Commits</summary>
        |
        |${(for(c <- e._2) yield st"* ${c.pretty}", "\n\n")}
        |</details>
        |<br>
        |"""
}

changelog.writeOver(
  st"""${(newContent, "\n\n")}
      |
      |${existing.s}""".render)


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
                          val timeStamp: Z) {
    @pure def date: ZonedDateTime = {
      return Instant.ofEpochSecond(timeStamp.toLong).atZone(ZoneId.systemDefault());
    }
  }
}

