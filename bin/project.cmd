::/*#! 2> /dev/null                                 #
@ 2>/dev/null # 2>nul & echo off & goto BOF         #
if [ -z ${SIREUM_HOME} ]; then                      #
  echo "Please set SIREUM_HOME env var"             #
  exit -1                                           #
fi                                                  #
exec ${SIREUM_HOME}/bin/sireum slang run "$0" "$@"  #
:BOF
setlocal
if not defined SIREUM_HOME (
  echo Please set SIREUM_HOME env var
  exit /B -1
)
%SIREUM_HOME%\bin\sireum.bat slang run "%0" %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.project.Module
import org.sireum.project.ProjectUtil._
import org.sireum.project.{Project, ProjectUtil}

val act = "hamr-act"

val arsit = "hamr-arsit"

val codegen = "hamr-codegen"

val homeDir = Os.slashDir.up.canon

var (codegenShared, codegenJvm): (Module, Module) = moduleSharedJvmPub(
  baseId = codegen,
  baseDir = homeDir,
  jvmDeps = ISZ(act, arsit),
  jvmIvyDeps = ISZ(),
  sharedDeps = ISZ("library-shared", "hamr-common-shared"),
  sharedIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "HAMR AADL Code Generator",
    url = "github.com/sireum/hamr-codegen",
    licenses = bsd2,
    devs = ISZ(jasonBelt)
  )
)

val externalTestDir: Os.Path = homeDir / "jvm" / "src" / "test-ext"
if(externalTestDir.exists) {
  for (testDir <- externalTestDir.list.filter((p: Os.Path) => p.isDir && (p / "scala").exists && (p / "scala").isDir)) {
    codegenJvm = codegenJvm(
      testSources = codegenJvm.testSources ++ ProjectUtil.dirs(homeDir, ISZ(ISZ("src", "test-ext", testDir.name, "scala"))))

    val resourcesDir = testDir / "resources"
    if (resourcesDir.exists && resourcesDir.isDir) {
      codegenJvm = codegenJvm(
        testResources = codegenJvm.testResources ++ ProjectUtil.dirs(homeDir, ISZ(ISZ("src", "test-ext", testDir.name, "resources"))))
    }
  }
}

val project = Project.empty + codegenShared + codegenJvm

projectCli(Os.cliArgs, project)
