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
import Templates._

@enum object Matches {
  "GreaterOrEqual"
}

@datatype class Require(pluginId: String,
                        version: Option[String],
                        matches: Option[Matches.Type]
                       )

@datatype class Feature (productId: String,
                         featureId: String,
                         version: String,

                         productName: String,
                         productDescription: String,
                         providerName: String,

                         requires: ISZ[Require],

                         updateSiteName: String,
                         directory: Os.Path
                         )

val osate_plugin_dir = Os.home / "devel" / "sireum" / "osate-plugin"
val dest = Os.home / "devel" / "sireum" / "osate-update-site"


val releases = getSortedDirs(dest)

for(releaseDir <- releases) {
  println(s"Processing: ${releaseDir.name}")
  val features = getSortedDirs(releaseDir)

  var previousVersion: String = ""
  for(featureUpdateDir <- features) {
    println(s"Processing ${featureUpdateDir}")

    val feature: Feature = parse(osate_plugin_dir / s"${featureUpdateDir.name}.feature")

    val featuresDir = (featureUpdateDir / "features")
    assert(featuresDir.list.size == 1, s"${featuresDir.value} -- ${featuresDir.list.size}")

    val featureJarFile = featuresDir.list(0)
    val jarName = ops.StringOps(featureJarFile.name)
    val pluginVersion: String = jarName.substring(jarName.lastIndexOf('_') + 1, jarName.lastIndexOf('.'))
    val site_xml_contents = site_xml(feature.updateSiteName, featureJarFile.name, feature.featureId, pluginVersion)

    (featureUpdateDir / "site.xml").writeOver(site_xml_contents.render)
    println(s"Wrote: ${(featureUpdateDir / "site.xml").value}")

    if(previousVersion == "") { previousVersion = pluginVersion}
    else { assert (previousVersion == pluginVersion, s"${previousVersion} vs ${pluginVersion} for ${featuresDir}")}
  }

  val (a,b): (ST, ST) = composite(features.map(m => m.name))
  (releaseDir / "compositeArtifacts.xml").writeOver(a.render)
  (releaseDir / "compositeContent.xml").writeOver(b.render)

  val versions = ops.StringOps(releaseDir.name).split(c => c == '.')
  val pluginReadme = releaseUpdateSiteReadme(releaseDir.name, versions(versions.size - 1))

  (releaseDir / "readme.md").writeOver(pluginReadme.render)
}

val (a,b): (ST, ST) = composite(releases.map(m => m.name))
(dest / "compositeArtifacts.xml").writeOver(a.render)
(dest / "compositeContent.xml").writeOver(b.render)
(dest / "readme.md").writeOver(rootUpdateSiteReadme(releases).render)

object Templates {

  def getSortedDirs(dir: Os.Path): ISZ[Os.Path] = {
    val results = Os.proc(ISZ("bash", "-c", "ls -d -1 *")).at(dir).runCheck()
    val files = ops.StringOps(results.out).split(c => c == '\n').map((s: String) => dir / s)
    return files.filter(f => f.isDir)
  }

  def getQuote(str_ : String): String = {
    val str = ops.StringOps(ops.StringOps(str_).trim)
    return str.substring(str.indexOf('"') + 1, str.lastIndexOf('"'))
  }

  def parseImport(str : ops.StringOps): Require = {
    val pluginPos = str.stringIndexOf("plugin=")
    val plugin = str.substring(pluginPos + 8, str.indexOfFrom('"', pluginPos + 9))
    var version: Option[String] = None()
    var matches: Option[Matches.Type] = None()

    val versionPos = str.stringIndexOf("version=")
    if(versionPos > 0) {
      version = Some(str.substring(versionPos + 9, str.indexOfFrom('"', versionPos + 10)))
    }
    val matchPos = str.stringIndexOf("match=")
    if(matchPos > 0) {
      val m = str.substring(matchPos + 7, str.indexOfFrom('"', matchPos + 8))
      m match {
        case "greaterOrEqual" => matches = Some(Matches.GreaterOrEqual)
        case x => halt(x)
      }
    }
    return Require(plugin, version, matches)
  }

  def parse(featureDir: Os.Path): Feature = {
    val featureXml = featureDir / "feature.xml"
    assert(featureXml.exists, featureXml.canon.value)

    val lines = featureXml.readLines

    var productId: String = ""
    var featureId: String = ""
    var productName: String = ""
    var productDescription: String = ""
    var version: String = ""
    var providerName: String = ""
    var updateSiteName: String = ""
    var requires: ISZ[Require]= ISZ()
    var i = 0
    while (i < lines.size) {
      var line = ops.StringOps(lines(i))
      if(line.startsWith("<feature")) {
        featureId = getQuote(lines(i + 1))
        productName = getQuote(lines(i + 2))
        version = getQuote(lines(i + 3))
        providerName = getQuote(lines(i + 4))

        updateSiteName = s"${productName} Update Site"
      }

      if(line.contains("<description")) {
        i = i + 1
        while (!ops.StringOps(lines(i)).contains("</description>")) {
          productDescription = st"""${productDescription}
                                   |${lines(i)}""".render
          i = i + 1
        }
      }

      if(line.contains("<requires>")) {
        i = i  + 1
        while(ops.StringOps(lines(i)).contains("<import")) {
          requires = requires :+ parseImport(ops.StringOps(lines(i)))
          i = i  + 1
        }
      }

      if(line.contains("<plugin")) {
        productId = getQuote(lines(i + 1))
      }

      i = i + 1
    }

    return Feature(productId, featureId, version, productName, ops.StringOps(productDescription).trim,
      providerName, requires, updateSiteName, featureDir)
  }

  def site_xml(updateSiteName: String,
               jarFile: String,
               featureId: String,
               featureVersion: String): ST = {
    val ret = st"""<?xml version="1.0" encoding="UTF-8"?>
                  |<site>
                  |   <description name="${updateSiteName}">
                  |      ${updateSiteName}
                  |   </description>
                  |   <feature url="features/${jarFile}" id="${featureId}" version="${featureVersion}">
                  |      <category name="org.sireum.aadl.osate.category"/>
                  |   </feature>
                  |   <category-def name="org.sireum.aadl.osate.category" label="Sireum">
                  |      <description>
                  |         Sireum features
                  |      </description>
                  |   </category-def>
                  |</site>
                  |"""
    return ret
  }

  def build_xml(destDir: Os.Path,
        featureId: String,
        qualifier: String): ST = {

    val ret: ST =
      st"""<?xml version="1.0" encoding="UTF-8"?>
          |<project default="feature_export" name="build">
          |	<target name="feature_export">
          |		<pde.exportFeatures destination="${destDir.canon.value}"
          |                       features="${featureId}"
          |                       qualifier="${qualifier}"
          |                       exportSource="false" exportType="directory" useJARFormat="true"/>
          |	</target>
          |</project>
          |"""
    return ret
  }

  def composite(locations: ISZ[String]): (ST, ST) = {
    val children = locations.map((m: String) => s"<child location='${m}'/>")
    val size = locations.size
    val a = st"""<?xml version='1.0' encoding='UTF-8'?>
                |<?compositeArtifactRepository version='1.0.0'?>
                |<repository name='Sireum OSATE Update Site'
                |    type='org.eclipse.equinox.internal.p2.artifact.repository.CompositeArtifactRepository' version='1.0.0'>
                |  <properties size='1'>
                |    <property name='p2.timestamp' value='1243822502440'/>
                |  </properties>
                |  <children size='${size}'>
                |    ${(children, "\n")}
                |  </children>
                |</repository>
                |"""

    val b = st"""<?xml version='1.0' encoding='UTF-8'?>
                |<?compositeMetadataRepository version='1.0.0'?>
                |<repository name='Sireum OSATE Update Site'
                |    type='org.eclipse.equinox.internal.p2.metadata.repository.CompositeMetadataRepository' version='1.0.0'>
                |  <properties size='1'>
                |    <property name='p2.timestamp' value='1243822502499'/>
                |  </properties>
                |  <children size='${size}'>
                |    ${(children, "\n")}
                |  </children>
                |</repository>
                |"""
    return (a, b)
  }

  def rootUpdateSiteReadme(dirs: ISZ[Os.Path]) : ST = {
    val _dirs = dirs.map((m: Os.Path) => s"- [${m.name}](${m.name})")
    val ret =
      st"""# Sireum OSATE Plugins Releases
          |
          |This update site contains releases of Sireum's OSATE plugins and is only
          |intended to be used with [Sireum's Phantom tool](https://github.com/sireum/phantom)
          |or the FMIDE install script (see the
          |[CASE](https://github.com/sireum/case-env#setting-up-fmide-and-hamr-only)
          |setup instructions for more information). No other support is offered.
          |
          |## How to Install the Latest Sireum OSATE Plugins Using Phantom
          |
          |Install [Sireum](https://github.com/sireum/kekinian#installing) and then run the following:
          |
          |```batch
          |$$SIREUM_HOME/bin/sireum hamr phantom -u
          |```
          |
          |## Installing a Specific Version of the Plugins
          |
          |Refer to the readme of a particular release for specific installation instructions.
          |
          |**Releases**
          |
          |${(_dirs, "\n")}
          |"""
    return ret
  }

  def releaseUpdateSiteReadme(version: String, sireumVersion: String) : ST = {
    val _features = ISZ[String]("cli", "hamr").map((m: String) => {
      s"org.sireum.aadl.osate.${m}.feature.feature.group=https://raw.githubusercontent.com/sireum/osate-update-site/master/${version}"
    })
    val features = st"${(_features, ";")}"
    val bt: String = "\\"
    val ret =
      st"""# Sireum OSATE Plugins ${version} Release
          |
          |This update site contains the ${version} release of Sireum's OSATE plugins and is only
          |intended to be used with [Sireum's Phantom tool](https://github.com/sireum/phantom)
          |or the FMIDE install script (see the
          |[CASE](https://github.com/sireum/case-env#setting-up-fmide-and-hamr-only)
          |setup instructions for more information). No other support is offered.
          |
          |## How to Install the ${version} Sireum OSATE Plugins Using Phantom
          |
          |Install [Sireum](https://github.com/sireum/kekinian#installing) and then run the following:
          |
          |```batch
          |$$SIREUM_HOME/bin/sireum hamr phantom -u --features "${features}"
          |```
          |
          |## Resolving Potential Version Issues
          |
          |The Sireum OSATE plugins use the version of Sireum installed at ``$$SIREUM_HOME/bin/sireum.jar``
          |which may no longer be compatible with this version of the plugins. If that is the case and
          |you still want to use this version of the plugins then you will need to build the
          |${sireumVersion} version of Sireum and then run Phantom as follows:
          |
          |* Windows:
          |
          |  ```batch
          |  git clone https://github.com/sireum/kekinian Sireum
          |  cd Sireum
          |  git checkout ${sireumVersion}
          |  git submodule update --init --recursive
          |  bin${bt}build.cmd
          |  bin${bt}sireum hamr phantom -u --features "${features}"
          |  ```
          |
          |* Linux:
          |
          |  ```bash
          |  git clone https://github.com/sireum/kekinian Sireum
          |  cd Sireum
          |  git checkout ${sireumVersion}
          |  git submodule update --init --recursive
          |  bin/build.cmd
          |  bin/sireum hamr phantom -u --features "${features}"
          |  ```
          |
          |* macOS:
          |
          |  ```bash
          |  git clone https://github.com/sireum/kekinian Sireum
          |  cd Sireum
          |  git checkout ${sireumVersion}
          |  git submodule update --init --recursive
          |  bin/build.cmd
          |  bin/sireum hamr phantom -u --features "${features}"
          |  ```
          |
          |"""
    return ret
  }
}