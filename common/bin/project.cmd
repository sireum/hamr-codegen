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
%SIREUM_HOME%\bin\sireum.bat slang run -n "%0" %*
exit /B %errorlevel%
::!#
// #Sireum

import org.sireum._
import org.sireum.project.ProjectUtil._
import org.sireum.project.{JSON, Project}

val air = "hamr-air"

val common = "hamr-common"

val homeDir = Os.slashDir.up.canon

val (commonShared, commonJvm) = moduleSharedJvm(
  baseId = common,
  baseDir = homeDir,
  sharedDeps = sharedId(air),
  sharedIvyDeps = ISZ(),
  jvmDeps = ISZ(air),
  jvmIvyDeps = ISZ()
)

val project = Project.empty + commonShared + commonJvm

projectCli(Os.cliArgs, project)