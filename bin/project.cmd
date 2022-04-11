::#! 2> /dev/null                                   #
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
::!#
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.{Project, ProjectUtil}

val act = "hamr-act"

val arsit = "hamr-arsit"

val codegen = "hamr-codegen"

val homeDir = Os.slashDir.up.canon

var codegenJvm = moduleJvmPub(
  id = codegen,
  baseDir = homeDir,
  jvmDeps = ISZ(act, arsit),
  jvmIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "HAMR AADL Code Generator",
    url = "github.com/sireum/hamr-codegen",
    licenses = bsd2,
    devs = ISZ(jasonBelt)
  )
)

for(testDir <- (homeDir / "jvm" / "src").list.filter((p: Os.Path) => p.name != "test" && p.name != "main")) {
  assert ((testDir / "scala").exists, s"'scala' subdirectory missing in ${testDir}")
  assert ((testDir / "resources").exists, s"'resources' subdirectory missing in ${testDir}")

  codegenJvm = codegenJvm(
    testSources = codegenJvm.testSources ++ ProjectUtil.dirs(homeDir, ISZ(ISZ("src", testDir.name, "scala"))),
    testResources = codegenJvm.testResources ++ ProjectUtil.dirs(homeDir, ISZ(ISZ("src", testDir.name, "resources")))
  )
}

val project = Project.empty + codegenJvm

projectCli(Os.cliArgs, project)