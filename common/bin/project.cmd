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
import org.sireum.project.ProjectUtil._
import org.sireum.project.Project

val air = "hamr-air"

val common = "hamr-common"

val slangFrontEnd = "slang-frontend"

val homeDir = Os.slashDir.up.canon

val (commonShared, commonJvm) = moduleSharedJvmPub(
  baseId = common,
  baseDir = homeDir,
  sharedDeps = sharedId(air) :+ slangFrontEnd,
  sharedIvyDeps = ISZ(),
  jvmDeps = ISZ(air),
  jvmIvyDeps = ISZ(),
  pubOpt = pub(
    desc = "HAMR AADL Code Generator Common Library",
    url = "github.com/sireum/hamr-codegen",
    licenses = bsd2,
    devs = ISZ(jasonBelt)
  )
)

val project = Project.empty + commonShared + commonJvm

projectCli(Os.cliArgs, project)