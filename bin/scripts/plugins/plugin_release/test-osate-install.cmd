::/*#! 2> /dev/null                                   #
@ 2>/dev/null # 2>nul & echo off & goto BOF           #
if [ -z "${SIREUM_HOME}" ]; then                      #
  echo "Please set SIREUM_HOME env var"               #
  exit -1                                             #
fi                                                    #
exec "${SIREUM_HOME}/bin/sireum" slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
"%SIREUM_HOME%\bin\sireum.bat" slang run "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._


@datatype class Feature(name: String,
                        id: String,
                        updateSite: String,
                        localUpdateSite: Option[String])


val localUpdateSites: B = T || ops.ISZOps(Os.cliArgs).contains("local")

val sireumHome = Os.path(Os.env("SIREUM_HOME").get)

val osateLoc = Os.path("/Applications") / "osate.app" / "Contents" / "MacOS" / "osate"
val fmideLoc = sireumHome / "bin" / "mac" / "fmide.app" / "Contents" / "MacOS" / "osate"

val remoteUpdateSite = "https://raw.githubusercontent.com/sireum/osate-update-site/master"
val localUpdateSite = (Os.home / "devel" / "sireum" / "osate-update-site").toUri

val remoteGumboUpdateSite = "https://raw.githubusercontent.com/sireum/aadl-gumbo-update-site/master"
val localGumboUpdateSite = (Os.home / "devel" / "sireum" / "aadl-gumbo-update-site").toUri

val osateExe: Os.Path = if(ops.ISZOps(Os.cliArgs).contains("fmide")) fmideLoc else osateLoc

val awas = Feature(
  name = "Awas",
  id ="org.sireum.aadl.osate.awas.feature.feature.group",
  updateSite = remoteUpdateSite,
  localUpdateSite = Some(localUpdateSite))

val base = Feature(
  name="Base",
  updateSite = remoteUpdateSite,
  localUpdateSite = Some(localUpdateSite),
  id = "org.sireum.aadl.osate.feature.feature.group")

val hamr = Feature(
  name="HAMR",
  updateSite = remoteUpdateSite,
  localUpdateSite = Some(localUpdateSite),
  id = "org.sireum.aadl.osate.hamr.feature.feature.group")

val cli = Feature(
  name = "cli",
  updateSite = remoteUpdateSite,
  localUpdateSite = Some(localUpdateSite),
  id = "org.sireum.aadl.osate.cli.feature.feature.group")

val gumbo = Feature(
  name = "gumbo",
  updateSite = remoteGumboUpdateSite,
  localUpdateSite = Some(localGumboUpdateSite),
  id = "org.sireum.aadl.gumbo.feature.feature.group")

val gumbo2air = Feature(
  name = "gumbo2air",
  updateSite = remoteGumboUpdateSite,
  localUpdateSite = Some(localGumboUpdateSite),
  id = "org.sireum.aadl.osate.gumbo2air.feature.feature.group")


val order = ISZ(base, hamr, awas, cli, gumbo, gumbo2air)
//val order = ISZ(gumbo, gumbo2air)

for(o <- ops.ISZOps(order).reverse) {
  if (isInstalled(o.id, osateExe)) {
    uninstallPlugin(o, osateExe)
  }
}

gc(osateExe)

for(o <- order) {
  installPlugin(o, osateExe)
}

println(s"\n\nExecute the following to launch OSATE :\n")
println(s"${osateExe}&")



def gc(_osateExe: Os.Path): Unit = {
  println("Garbage collecting ...")
  proc"${_osateExe.string} -nosplash -console -consoleLog -application org.eclipse.equinox.p2.garbagecollector.application -profile DefaultProfile".at(_osateExe.up).runCheck()
}

def getVersion(featureId: String, _osateExe: Os.Path): Option[String] = {
  val installedPlugins = Os.proc(ISZ(_osateExe.string, "-nosplash", "-console", "-consoleLog", "-application", "org.eclipse.equinox.p2.director",
    "-listInstalledRoots")).at(_osateExe.up).runCheck()
  val l = ops.StringOps(installedPlugins.out).split(c => c == '\n')
  l.filter(f => ops.StringOps(f).startsWith(featureId)) match {
    case ISZ(i) =>
      val x = ops.StringOps(i)
      return Some(x.substring(x.indexOf('/') + 1, x.size))
    case _ => return None()
  }
}

def isInstalled(featureId: String, _osateExe: Os.Path): B = {
  return getVersion(featureId, _osateExe).nonEmpty
}

def uninstallPlugin(feature: Feature, _osateExe : Os.Path): Unit = {
  addInfo(s"Uninstalling ${feature.name} OSATE plugin")
  Os.proc(ISZ(_osateExe.string, "-nosplash", "-console", "-consoleLog", "-application", "org.eclipse.equinox.p2.director",
    "-uninstallIU", feature.id
  )).at(_osateExe.up).runCheck()
}

def installPlugin(feature: Feature, _osateExe : Os.Path): Unit = {
  val updateSite: String = if(localUpdateSites) feature.localUpdateSite.get else feature.updateSite
  addInfo(s"Installing ${feature.name} OSATE plugin from ${updateSite}")

  Os.proc(ISZ(_osateExe.string, "-nosplash", "-console", "-consoleLog", "-application", "org.eclipse.equinox.p2.director",
    "-repository", updateSite, "-installIU", feature.id
  )).at(_osateExe.up).runCheck()

  addInfo(s"Installed ${feature.name} ${getVersion(featureId = feature.id, _osateExe = _osateExe).get}")
}

def addInfo(s: String): Unit = { cprintln(F, s ) }
def addError(s: String): Unit = { cprintln(T, s"Error: $s.") }
def addWarning(s: String): Unit = { cprintln(F, s"Warning: $s") }


