// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.util.ArsitLibrary

object ProjectTemplate {

  def arsitSlangInstructionsMessage(path: String, includeSbtMill: B): ST = {
    val sbtMillOpt: Option[ST] = if (includeSbtMill) Some(
      st"""Alternatively, refer to the comments in build.sbt or build.sc for SBT or Mill
          |based instructions.""")
    else None()

    val ret: ST =
      st"""Slang Instructions:
          |-------------------
          |  Slang-Embedded Project Directory: ${path}
          |
          |    Refer to the comments in bin/project.cmd for instructions on how to open the
          |    project in Sireum IVE and how to run the system/unit-tests from IVE or the command line.
          |    ${sbtMillOpt}"""
    return ret
  }

  def arsitCInstructionsMessage(cmakeProjDirectory: String,
                                devDir: String,
                                transpileScript: String,
                                compileScript: String,
                                runScript: String,
                                stopScript: String): ST = {
    var lng = st""
    lng = st"${lng}The cmake directory will be created by the transpiler. "
    lng = st"${lng}The transpiler only needs to be rerun when changes are made to Slang code. "

    val ret: ST =
      st"""C Instructions:
          |---------------
          |  CMake Project Directory:  ${cmakeProjDirectory}
          |  Developer Code Directory: ${devDir}
          |
          |  $lng
          |
          |  Execute the following scripts to build/run the system. Pass '-h' to see a script's options.
          |
          |    ${transpileScript}
          |    ${compileScript}
          |    ${runScript}
          |    ${stopScript}"""

    return ret
  }

  def arsitCAmkESInstructionsMessage(devDir: String,
                                     transpileScript: String): ST = {
    val ret: ST =
      st"""C Instructions for CAmkES:
          |--------------------------
          |  Developer Code Directory: ${devDir}
          |
          |  Execute the following script to transpile the Slang code for CAmkES (transpiler only needs
          |  to be rerun when changes are made to Slang code):
          |
          |    ${transpileScript}"""

    return ret
  }

  def proyekBuild(projectName: String,
                  basePackageName: String,
                  embedArt: B,
                  demoScalaPath: String,
                  bridgeTestPath: String): ST = {
    val (artDir, artIvy): (Option[ST], Option[ST]) =
      if (embedArt)
        (Some(st""""art", """), None())
      else
        (None(), Some(st""""org.sireum.slang-embedded-art::slang-embedded-art:", """))

    // removed from ivy deps the following from
    //, "com.intellij:forms_rt:"

    val ret =
      st"""::/*#! 2> /dev/null                                   #
          |@ 2>/dev/null # 2>nul & echo off & goto BOF           #
          |if [ -z "$${SIREUM_HOME}" ]; then                      #
          |  echo "Please set SIREUM_HOME env var"               #
          |  exit -1                                             #
          |fi                                                    #
          |exec "$${SIREUM_HOME}/bin/sireum" slang run "$$0" "$$@"    #
          |:BOF
          |setlocal
          |if not defined SIREUM_HOME (
          |  echo Please set SIREUM_HOME env var
          |  exit /B -1
          |)
          |"%SIREUM_HOME%\bin\sireum.bat" slang run %0 %*
          |exit /B %errorlevel%
          |::!#*/
          |// #Sireum
          |
          |// Example Sireum Proyek build definitions -- the contents of this file will not be overwritten
          |//
          |// To install Sireum (Proyek and IVE) see https://sireum.org/getting-started/
          |//
          |// The following commands should be executed in the parent of the 'bin' directory.
          |//
          |// Command Line:
          |//   To run the demo from the command line using the default scheduler:
          |//     sireum proyek run . ${basePackageName}.Demo
          |//
          |//   To see the available CLI options:
          |//     sireum proyek run . ${basePackageName}.Demo -h
          |//
          |//   To run the example unit tests from the command line:
          |//     sireum proyek test .
          |//
          |//   To build an executable jar:
          |//     sireum proyek assemble --uber --main ${basePackageName}.Demo .
          |//
          |// Sireum IVE:
          |//
          |//   Create the IVE project if Codegen was not run locally or if its no-proyek-ive
          |//   option was used:
          |//     sireum proyek ive .
          |//
          |//   Then in IVE select 'File > Open ...' and navigate to the parent of the
          |//   'bin' directory and click 'OK'.
          |//
          |//   To run the demo from within Sireum IVE:
          |//     Right click ${demoScalaPath} and choose "Run 'Demo'"
          |//
          |//   To run the unit test cases from within Sireum IVE:
          |//     Right click the ${bridgeTestPath} and choose "Run ScalaTests in bridge"
          |
          |import org.sireum._
          |import org.sireum.project.{Module, Project, Target}
          |
          |val home: Os.Path = Os.slashDir.up.canon
          |
          |val slangModule: Module = Module(
          |  id = "${projectName}",
          |  basePath = (home / "src").string,
          |  subPathOpt = None(),
          |  deps = ISZ(),
          |  targets = ISZ(Target.Jvm),
          |  ivyDeps = ISZ(${artIvy}"org.sireum.kekinian::library:",
          |                "org.sireum.kekinian::hamr-vision:"),
          |  sources = for(m <- ISZ(${artDir}"architecture", "bridge", "component", "data", "nix", "seL4Nix", "util")) yield (Os.path("main") / m).string,
          |  resources = ISZ(),
          |  testSources = for (m <- ISZ("bridge", "system", "util")) yield (Os.path("test") / m).string,
          |  testResources = ISZ(),
          |  publishInfoOpt = None()
          |)
          |
          |val inspectorModule: Module = slangModule(
          |  sources = slangModule.sources :+ (Os.path("main") / "inspector").string,
          |  ivyDeps = slangModule.ivyDeps ++ ISZ("org.sireum:inspector-capabilities:", "org.sireum:inspector-gui:", "org.sireum:inspector-services-jvm:")
          |)
          |
          |val slangProject: Project = Project.empty + slangModule
          |val inspectorProject: Project = Project.empty + inspectorModule
          |
          |val prj: Project = slangProject
          |//val prj: Project = inspectorProject()
          |
          |println(project.JSON.fromProject(prj, T))
          |"""
    return ret
  }

  def proyekVersionProperties(): ST = {
    val kekinianVersion = ArsitLibrary.getKekinianVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()
    val formsRtVersion = ArsitLibrary.getFormsRtVersion()
    val inspectorVersion = ArsitLibrary.getInspectorVersion()
    val artVersion = ArsitLibrary.getArtVersion()

    // remove the following from version.properties
    // |com.intellij%forms_rt%=${formsRtVersion}
    val ret: ST =
    st"""org.sireum.slang-embedded-art%%slang-embedded-art%=${artVersion}
        |
        |org.sireum%inspector-capabilities%=${inspectorVersion}
        |org.sireum%inspector-gui%=${inspectorVersion}
        |org.sireum%inspector-services-jvm%=${inspectorVersion}
        |
        |org.sireum.kekinian%%hamr-vision%=${kekinianVersion}
        |
        |# remove the following entries if you want to use the versions
        |# that ship with sireum (i.e. $$SIREUM_HOME/bin/sireum --version)
        |
        |# Scala compiler plugin for Slang
        |org.sireum%%scalac-plugin%=${sireumScalacVersion}
        |
        |org.sireum.kekinian%%library%=${kekinianVersion}
        |
        |org.scala-lang%scala-library%=${scalaVersion}
        |org.scalatest%%scalatest%%=${scalaTestVersion}
        |"""
    return ret
  }

  def sbtBuild(projectName: String,
               basePackageName: String,
               embedArt: B,
               demoScalaPath: String,
               bridgeTestPath: String): ST = {
    val artVersion = ArsitLibrary.getArtVersion()

    val (artVer, artJitpack, artSrcDir): (Option[ST], Option[ST], Option[ST]) = if (!embedArt) {
      (Some(
        st"""// https://github.com/sireum/slang-embedded-art/tree/${artVersion}
            |val artVersion = "${artVersion}""""),
        Some(st""""org.sireum.slang-embedded-art" %% "slang-embedded-art" % artVersion withSources(),"""),
        None())
    } else {
      (None(), None(), Some(st"""Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/art","""))
    }

    val ret: ST =
      st"""// Example sbt build definitions -- the contents of this file will not be overwritten
          |//
          |// sbt can be obtained from https://www.scala-sbt.org/download.html
          |//
          |// To run the demo from the command line using the default scheduler:
          |//   sbt run
          |//
          |// To see the available CLI options:
          |//   sbt "run -h"
          |//
          |// To run the example unit tests from the command line:
          |//   sbt test
          |//
          |// To build a runnable/executable jar:
          |//   sbt assembly
          |//
          |// To skip running the unit tests while building the executable jar:
          |//   sbt 'set test in assembly := {}' assembly
          |// on Linux/Mac, or
          |//   sbt "set test in assembly := {}" assembly
          |// on Windows
          |//
          |// Sireum IVE: Installation instructions available at https://sireum.org/getting-started/
          |//
          |//   In IVE select 'File > Open ...' and navigate to the directory containing
          |//   this file then click 'OK'.
          |//
          |//   To run the demo from within Sireum IVE:
          |//     Right click ${demoScalaPath} and choose "Run 'Demo'"
          |//
          |//   To run the unit test cases from within Sireum IVE:
          |//     Right click the ${bridgeTestPath} directory and choose "Run ScalaTests in bridge"
          |//
          |//   NOTE: A ClassNotFoundException may be raised the first time you try to
          |//         run the demo or unit tests.  If this occurs simply delete the directory
          |//         named 'target' and retry
          |
          |
          |lazy val ${projectName} = slangEmbeddedProject("${projectName}", ".")
          |
          |${libDependencies()}
          |
          |${artVer}
          |
          |val commonSettings = Seq(
          |  organization := "org.sireum",
          |  incOptions := incOptions.value.withLogRecompileOnMacro(false),
          |  scalaVersion := scalaVer,
          |  scalacOptions := Seq("-release:8", "-deprecation",
          |    "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings"),
          |  Test / parallelExecution := true,
          |  resolvers ++= Resolver.sonatypeOssRepos("public") ++ Seq("jitpack" at "https://jitpack.io"),
          |  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion),
          |  ThisBuild / evictionErrorLevel := Level.Warn,
          |  libraryDependencies ++= Seq(
          |    ${artJitpack}
          |    "org.sireum.kekinian" %% "library" % kekinianVersion withSources(),
          |    "org.sireum.kekinian" %% "hamr-vision" % kekinianVersion withSources()
          |  )
          |)
          |
          |import sbtassembly.AssemblyPlugin.defaultUniversalScript
          |val slangEmbeddedSettings = Seq(
          |  ${artSrcDir}
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/bridge",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/component",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/data",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/nix",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/seL4Nix",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/util",
          |
          |  Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
          |  Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/system",
          |  Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/util",
          |
          |  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
          |
          |  // Jetbrains UI Designer
          |  libraryDependencies += "com.intellij" % "forms_rt" % formsRtVersion,
          |
          |  mainClass in (Compile, run) := Some("${basePackageName}.Demo"),
          |
          |  mainClass in assembly := Some("${basePackageName}.Demo"),
          |  assemblyJarName in assembly := "${projectName}.jar",
          |  assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false))),
          |
          |  assemblyMergeStrategy in assembly := {
          |    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
          |    case x => MergeStrategy.first
          |  }
          |)
          |
          |val slangEmbeddedInspectorSettings = Seq(
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/inspector",
          |
          |  libraryDependencies += "org.sireum" % "inspector-capabilities" % inspectorVersion withSources(),
          |  libraryDependencies += "org.sireum" % "inspector-gui" % inspectorVersion withSources(),
          |  libraryDependencies += "org.sireum" % "inspector-services-jvm" % inspectorVersion withSources(),
          |
          |  mainClass in (Compile, run) := Some("${basePackageName}.InspectorDemo"),
          |)
          |
          |def slangEmbeddedProject(projId: String, projectDirectory: String) =
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings)
          |
          |def slangEmbeddedInspectorProject(projId: String, projectDirectory: String) = {
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings ++ slangEmbeddedInspectorSettings)
          |}
          |"""
    return ret
  }

  def sbtBuildPropertiesContents(): ST = {
    val sbtVersion: String = ArsitLibrary.getSBTVersion()
    val ret: ST =
      st"""sbt.version=${sbtVersion}
          |"""
    return ret
  }

  def sbtPluginsSbtContents(): ST = {
    val sbtAssemblyVersion = ArsitLibrary.getSbtAssemblyVersion()
    val ret: ST =
      st"""addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "${sbtAssemblyVersion}")
          |"""
    return ret
  }

  def millBuild(basePackageName: String,
                outputDirSimpleName: String,
                embedArt: B): ST = {
    val inspectorVersion = ArsitLibrary.getInspectorVersion()

    val artVersion = ArsitLibrary.getArtVersion()

    val (artVer, artJitpack, artSrcDir): (Option[ST], Option[ST], Option[ST]) = if (!embedArt) {
      (Some(
        st"""// https://github.com/sireum/slang-embedded-art/tree/${artVersion}
            |val artVersion = "${artVersion}""""),
        Some(st"""ivy"org.sireum.slang-embedded-art::slang-embedded-art::$${artVersion}","""),
        None())
    } else {
      (None(), None(), Some(st"""millSourcePath / os.up / "src" / "main" / "art","""))
    }

    val ret: ST =
      st"""import mill._
          |import scalalib._
          |
          |// Example mill build -- the contents of this file will not be overwritten.
          |//
          |// A custom mill build for Sireum can be obtained from https://github.com/sireum/rolling/releases/tag/mill
          |// On Windows, rename 'mill' to 'mill.bat'
          |//
          |// To run the demo from the command line:
          |//   mill ${basePackageName}.run
          |//
          |// To run the example unit tests:
          |//   mill ${basePackageName}.tests
          |//
          |// Sireum IVE: Installation instructions available at https://sireum.org/getting-started/
          |//
          |//   First cd to the directory containing this file and execute the following:
          |//
          |//     $$SIREUM_HOME/bin/sireum tools ivegen -f -m mill -n ${outputDirSimpleName} ../
          |//
          |//   Then in IVE select 'File > Open ...' and navigate to the directory
          |//   containing this file then click 'OK'.  To have the codebase and its
          |//   test suites recompiled upon changes, run:
          |//
          |//     $$SIREUM_HOME/bin/mill -w ${basePackageName}.tests.compile
          |//
          |// Visual Studio Code:
          |//   Follow Sireum Kekinian's instructions for setting up a development
          |//   environment using Scala Metals: https://github.com/sireum/kekinian#scala-metals
          |//   Then open the folder containing this file in VS Code and import the
          |//   mill build when asked.
          |
          |
          |object `${basePackageName}` extends slangEmbeddedProject
          |
          |trait SlangEmbeddedModule extends ScalaModule {
          |
          |  ${libDependencies()}
          |  ${artVer}
          |
          |  def scalaVersion = scalaVer
          |
          |  override def javacOptions = T { Seq("-source", "1.8", "-target", "1.8", "-encoding", "utf8") }
          |
          |  override def scalacOptions = T { Seq(
          |    "-release:8",
          |    "-deprecation",
          |    "-Yrangepos",
          |    "-Ydelambdafy:method",
          |    "-feature",
          |    "-unchecked",
          |    "-Xfatal-warnings",
          |    "-language:postfixOps"
          |  ) }
          |
          |  override def ivyDeps = Agg(
          |    ${artJitpack}
          |    ivy"org.sireum.kekinian::library::$${kekinianVersion}",
          |    ivy"org.sireum.kekinian::hamr-vision::$${kekinianVersion}",
          |
          |    // Jetbrains UI Designer
          |    ivy"com.intellij:forms_rt:$${formsRtVersion}"
          |  )
          |
          |  override def scalacPluginIvyDeps = Agg(ivy"org.sireum::scalac-plugin::$${sireumScalacVersion}")
          |
          |  override def repositories = super.repositories :+ coursier.Repositories.jitpack
          |
          |  override def mainClass = T { Some("${basePackageName}.Demo") }
          |
          |  implicit def osPath2PathRef(p: os.Path): PathRef = PathRef(p)
          |}
          |
          |trait slangEmbeddedProject extends SlangEmbeddedModule {
          |
          |  def contributedSources: Seq[PathRef] = Seq(
          |    millSourcePath / os.up / "src" / "main" / "architecture",
          |    ${artSrcDir}
          |    millSourcePath / os.up / "src" / "main" / "bridge",
          |    millSourcePath / os.up / "src" / "main" / "component",
          |    millSourcePath / os.up / "src" / "main" / "data",
          |    millSourcePath / os.up / "src" / "main" / "nix",
          |    millSourcePath / os.up / "src" / "main" / "seL4Nix",
          |    millSourcePath / os.up / "src" / "main" / "util"
          |  )
          |
          |  override def sources = T.sources(contributedSources)
          |
          |  object tests extends Tests {
          |
          |    final override def millSourcePath = super.millSourcePath / os.up / os.up / "src" / "test"
          |
          |    override def sources = T.sources( millSourcePath / "bridge",
          |                                      millSourcePath / "system",
          |                                      millSourcePath / "util" )
          |
          |    override def ivyDeps = Agg(ivy"org.scalatest::scalatest::$${scalaTestVersion}")
          |
          |    override def testFrameworks = T { Seq("org.scalatest.tools.Framework") }
          |  }
          |}
          |
          |trait slangEmbeddedInspectorProject extends slangEmbeddedProject {
          |
          |  override def mainClass = T { Some("${basePackageName}.InspectorDemo") }
          |
          |  override def contributedSources =
          |    super.contributedSources :+ millSourcePath / os.up / "src" / "main" / "inspector"
          |
          |  // FIXME: 2021.01.04 - the following doesn't work due to javafx/mill resolution issue
          |  //        -- refer to https://github.com/lihaoyi/mill/issues/767
          |  // override def ivyDeps = Agg(
          |  //   ivy"org.sireum::inspector-capabilities::$${inspectorVersion}",
          |  //   ivy"org.sireum::inspector-gui::$${inspectorVersion}",
          |  //   ivy"org.sireum::inspector-services-jvm::$${inspectorVersion}"
          |
          |  // workaround to #767 -- refer to https://github.com/lihaoyi/mill/issues/767#issuecomment-652799588
          |  override def unmanagedClasspath = T {
          |    import coursier._
          |
          |    val files = Fetch().addDependencies(
          |      dep"org.sireum:inspector-capabilities:${inspectorVersion}",
          |      dep"org.sireum:inspector-gui:${inspectorVersion}",
          |      dep"org.sireum:inspector-services-jvm:${inspectorVersion}"
          |    ).addRepositories(
          |      Repositories.sonatype("releases"),
          |      Repositories.jitpack
          |    ).run()
          |    val pathRefs = files.map(f => PathRef(os.Path(f)))
          |    Agg(pathRefs : _*)
          |  }
          |}
          |"""

    return ret
  }

  def libDependencies(): ST = {
    val kekinianVersion = ArsitLibrary.getKekinianVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()
    val formsRtVersion = ArsitLibrary.getFormsRtVersion()
    val inspectorVersion = ArsitLibrary.getInspectorVersion()

    val ret: ST =
      st"""// refer to https://github.com/sireum/kekinian/blob/master/versions.properties
          |// to get the most recent versions of the following dependencies
          |
          |// versions.properties key: org.scala-lang%scala-library%
          |val scalaVer = "${scalaVersion}"
          |
          |// versions.properties key: org.scalatest%%scalatest%%
          |val scalaTestVersion = "${scalaTestVersion}"
          |
          |// versions.properties key: org.sireum%%scalac-plugin%
          |// https://github.com/sireum/scalac-plugin/tree/${sireumScalacVersion}
          |val sireumScalacVersion = "${sireumScalacVersion}"
          |
          |
          |// refer to https://github.com/sireum/kekinian/releases to get the latest
          |// Sireum Kekinian release: https://github.com/sireum/kekinian/tree/${kekinianVersion}
          |val kekinianVersion = "${kekinianVersion}"
          |
          |
          |val inspectorVersion = "${inspectorVersion}"
          |
          |val formsRtVersion = "${formsRtVersion}"
          |"""
    return ret
  }
}
