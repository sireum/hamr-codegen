::/*#! 2> /dev/null                                                                                         #
@ 2>/dev/null # 2>nul & echo off & goto BOF                                                                 #
export SIREUM_HOME=$(cd -P $(dirname "$0")/.. && pwd -P)                                                    #
if [ ! -z ${SIREUM_PROVIDED_SCALA++} ]; then                                                                #
  SIREUM_PROVIDED_JAVA=true                                                                                 #
fi                                                                                                          #
"${SIREUM_HOME}/bin/init.sh" || exit $?                                                                     #
if [ -n "$COMSPEC" -a -x "$COMSPEC" ]; then                                                                 #
  export SIREUM_HOME=$(cygpath -C OEM -w -a ${SIREUM_HOME})                                                 #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/win/java":"${SIREUM_HOME}/bin/win/z3":"$PATH"                           #
    export PATH="$(cygpath -C OEM -w -a ${JAVA_HOME}/bin)":"$(cygpath -C OEM -w -a ${Z3_HOME}/bin)":"$PATH" #
  fi                                                                                                        #
elif [ "$(uname)" = "Darwin" ]; then                                                                        #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    export PATH="${SIREUM_HOME}/bin/mac/java/bin":"${SIREUM_HOME}/bin/mac/z3/bin":"$PATH"                   #
  fi                                                                                                        #
elif [ "$(expr substr $(uname -s) 1 5)" = "Linux" ]; then                                                   #
  if [ -z ${SIREUM_PROVIDED_JAVA++} ]; then                                                                 #
    if [ "$(uname -m)" = "aarch64" ]; then                                                                  #
      export PATH="${SIREUM_HOME}/bin/linux/arm/java/bin":"$PATH"                                           #
    else                                                                                                    #
      export PATH="${SIREUM_HOME}/bin/linux/java/bin":"${SIREUM_HOME}/bin/linux/z3/bin":"$PATH"             #
    fi                                                                                                      #
  fi                                                                                                        #
fi                                                                                                          #
if [ -f "$0.com" ] && [ "$0.com" -nt "$0" ]; then                                                           #
  exec "$0.com" "$@"                                                                                        #
else                                                                                                        #
  rm -fR "$0.com"                                                                                           #
  exec "${SIREUM_HOME}/bin/sireum" slang run -n "$0" "$@"                                                   #
fi                                                                                                          #
:BOF
setlocal
set SIREUM_HOME=%~dp0../
call "%~dp0init.bat" || exit /B %errorlevel%
if defined SIREUM_PROVIDED_SCALA set SIREUM_PROVIDED_JAVA=true
if not defined SIREUM_PROVIDED_JAVA set PATH=%~dp0win\java\bin;%~dp0win\z3\bin;%PATH%
set NEWER=False
if exist %~dpnx0.com for /f %%i in ('powershell -noprofile -executionpolicy bypass -command "(Get-Item %~dpnx0.com).LastWriteTime -gt (Get-Item %~dpnx0).LastWriteTime"') do @set NEWER=%%i
if "%NEWER%" == "True" goto native
del "%~dpnx0.com" > nul 2>&1
"%~dp0sireum.bat" slang run -n "%0" %*
exit /B %errorlevel%
:native
%~dpnx0.com %*
exit /B %errorlevel%
::!#*/
// #Sireum

import org.sireum._
import org.sireum.cli.CliOpt
import org.sireum.cli.CliOpt.Type

val homeBin: Os.Path = Os.slashDir
val home: Os.Path = homeBin.up
val sireum: Os.Path = homeBin / (if (Os.isWin) "sireum.bat" else "sireum")
val sireumJar: Os.Path = homeBin / "sireum.jar"
val appDir: Os.Path = homeBin / (if (Os.isMac) "mac" else if (Os.isWin) "win" else "linux")

val ignoreKeys: Set[String] = Set(ISZ(
  "devicesAsThreads", "line", "verbose", "parseableMessages", "genSbtMill", "noProyekIve", "noEmbedArt",
"runTranspiler", "system", "experimentalOptions", "sourcepath", "workspaceRootDir"))

def process(tool: CliOpt.Tool, optionName: String): Unit = {
  val tq = "\"\"\""
  var storeEntries: ISZ[ST] = ISZ()
  var buildEntriesCasing: ISZ[ST]= ISZ()
  var buildEntries: ISZ[ST] = ISZ()
  var initUpdateMethods: ISZ[ST] = ISZ()
  var changeListeners: ISZ[ST]= ISZ()
  var updateUi: ISZ[ST]= ISZ()
  var validityChecks: ISZ[ST] = ISZ()
  var hasChanges: ISZ[ST] = ISZ()
  var buildCommandLineArgs: ISZ[ST] = ISZ()
  def addOptions(opts: ISZ[org.sireum.cli.CliOpt.Opt], optGroup: String): Unit = {
    for (o <- opts if !ignoreKeys.contains(o.name)) {
      val oUpper = ops.StringOps(o.name).firstToUpper
      o.tpe match {
        case f: Type.Flag =>
          storeEntries = storeEntries :+
            st"""var ${o.name}: Boolean = ${f.default},
                |val ${o.name}Default: Boolean = ${f.default}"""

          buildEntries = buildEntries :+ st"store.${o.name} = o.${o.name}"

          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  currentStore.${o.name} = ${o.name}.isSelected
                |  update${oUpper}Cust()
                |}"""

          changeListeners = changeListeners :+
            st"${o.name}.addChangeListener(_ => update${oUpper}())"

          updateUi = updateUi :+
            st"${o.name}.setSelected(currentStore.${o.name})"

        case n: Type.Num =>
          assert (n.sep.isEmpty, "todo")

          storeEntries = storeEntries :+
            st"""var ${o.name}: String = "${n.default}",
                |val ${o.name}Default: String = "${n.default}",
                |var ${o.name}Valid: Boolean = true"""

          buildEntries = buildEntries :+ st"store.${o.name} = o.${o.name}.string.native"

          val minOpt: Option[ST] = if (n.min.nonEmpty)
            Some(st"""if (i < ${n.min.get}) {
                     |  currentStore.${o.name}Valid = false
                     |  ${o.name}.setToolTipText("value must be >= ${n.min.get}")
                     |  ${o.name}.setForeground(Color.red)
                     |}""")
          else None()
          val maxOpt: Option[ST] = if (n.max.nonEmpty)
            Some(st"""if (i < ${n.max.get}) {
                     |  currentStore.${o.name}Valid = false
                     |  ${o.name}.setToolTipText("value must be <= ${n.max.get}")
                     |  ${o.name}.setForeground(Color.red)
                     |}""")
          else None()
          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  val value = ${o.name}.getText
                |  currentStore.${o.name} = value
                |  parseInt(value) match {
                |    case Some(i) =>
                |      $minOpt
                |      $maxOpt
                |      currentStore.${o.name}Valid = true
                |      currentStore.${o.name} = value
                |      ${o.name}.setForeground(fgColor)
                |      update${oUpper}Cust()
                |    case _ =>
                |       currentStore.${o.name}Valid = false
                |       ${o.name}.setToolTipText(s"'$$value' is not a valid integer")
                |       ${o.name}.setForeground(Color.red)
                |  }
                |}"""

          changeListeners = changeListeners :+
            st"addChangeListener(${o.name}.getDocument, update${oUpper} _)"

          updateUi = updateUi :+
            st"${o.name}.setText(currentStore.${o.name})"

          validityChecks = validityChecks :+ st"currentStore.${o.name}Valid"

        case nf: Type.NumFlag =>
          halt(s"todo: $nf")

        case nc: Type.NumChoice =>
          val default = nc.choices(0)
          val choices: ISZ[ST] = for(choice <- nc.choices) yield st"i == $choice"
          storeEntries = storeEntries :+
            st"""var ${o.name}: String = "$default",
                |val ${o.name}Default: String = "$default",
                |var ${o.name}Valid: Boolean = true"""

          buildEntries = buildEntries :+ st"store.${o.name} = o.${o.name}.string.native"

          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  val value = ${o.name}.getSelectedItem.toString
                |  currentStore.${o.name} = value
                |  parseInt(value) match {
                |    case Some(i) =>
                |      if (!(${(choices, " || ")})) {
                |        ${o.name}.setToolTipText("value must be one of '${(nc.choices, ", ")}'")
                |        currentStore.${o.name}Valid = false
                |        ${o.name}.setForeground(Color.red)
                |      } else {
                |        currentStore.${o.name}Valid = true
                |        ${o.name}.setForeground(fgColor)
                |        update${oUpper}Cust()
                |      }
                |    case _ =>
                |      currentStore.${o.name}Valid = false
                |      ${o.name}.setToolTipText(s"'$$value' is not a valid integer")
                |      ${o.name}.setForeground(Color.red)
                |  }
                |}"""

          changeListeners = changeListeners :+
            st"""${o.name}.addActionListener((e: ActionEvent) => update${oUpper}())"""

          updateUi = updateUi :+
            st"${o.name}.setSelectedItem(currentStore.${o.name})"

          validityChecks = validityChecks :+ st"currentStore.${o.name}Valid"

        case s: Type.Str =>
          val default: String = if (s.default.nonEmpty) st""""${s.default.get}"""".render else st"""""""".render
          storeEntries = storeEntries :+
            st"""var ${o.name}: String = $default,
                |val ${o.name}Default: String = $default,
                |var ${o.name}Valid: Boolean = true"""
          if (s.sep.nonEmpty) {
            buildEntries = buildEntries :+ st"""store.${o.name} = if (o.${o.name}.nonEmpty) st$tq$${(o.${o.name}, "${s.sep}")}$tq.render.native else """""
          } else {
            buildEntries = buildEntries :+ st"""store.${o.name} = if (o.${o.name}.nonEmpty) o.${o.name}.get.native else """""
          }

          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  val value = ${o.name}.getText
                |  currentStore.${o.name} = value
                |  ${o.name}.setForeground(fgColor)
                |  update${oUpper}Cust()
                |}"""

          changeListeners = changeListeners :+
            st"addChangeListener(${o.name}.getDocument, update${oUpper} _)"

          updateUi = updateUi :+
            st"${o.name}.setText(currentStore.${o.name})"

          validityChecks = validityChecks :+ st"currentStore.${o.name}Valid"

        case c: Type.Choice =>
          val default = c.elements(0)
          val choices: ISZ[ST] = for(choice <- c.elements) yield st"""value == "${choice}""""
          storeEntries = storeEntries :+
            st"""var ${o.name}: String = "$default",
                |val ${o.name}Default: String = "$default",
                |var ${o.name}Valid: Boolean = true"""

          val enumName = s"${optionName}${ops.StringOps(c.name).firstToUpper}"

          val casingChoices: ISZ[ST]= for(choice <- c.elements) yield st"""case Cli.${enumName}.${ops.StringOps(choice).firstToUpper} => "${choice}""""
          buildEntriesCasing = buildEntriesCasing :+
            st"""def decase${oUpper}(e: Cli.${enumName}.Type): Predef.String = {
                |  e match {
                |    ${(casingChoices, "\n")}
                |  }
                |}"""
          buildEntries = buildEntries :+ st"store.${o.name} = decase${oUpper}(o.${o.name})"

          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  val value = ${o.name}.getSelectedItem.toString
                |  currentStore.${o.name} = value
                |  if (!(${(choices, " || ")})){
                |    currentStore.${o.name}Valid = false
                |    ${o.name}.setToolTipText("value must be one of '${(c.elements, ", ")}'")
                |    ${o.name}.setForeground(Color.red)
                |  } else {
                |    currentStore.${o.name}Valid = true
                |    ${o.name}.setForeground(fgColor)
                |    update${oUpper}Cust()
                |  }
                |}"""

        changeListeners = changeListeners :+
          st"""${o.name}.addActionListener((e: ActionEvent) => update${oUpper}())"""

          updateUi = updateUi :+
            st"${o.name}.setSelectedItem(currentStore.${o.name})"

          validityChecks = validityChecks :+ st"currentStore.${o.name}Valid"

        case p: Type.Path =>
          val default: String = if(p.default.nonEmpty) p.default.get else st"""""""".render
          storeEntries = storeEntries :+
            st"""var ${o.name}: String = $default,
                |val ${o.name}Default: String = $default,
                |var ${o.name}Valid: Boolean = true"""
          if (p.multiple) {
            buildEntries = buildEntries :+ st"""store.${o.name} = st$tq$${(o.${o.name}, Os.pathSep)}$tq.render.native"""
          } else {
            buildEntries = buildEntries :+ st"""store.${o.name} = if (o.${o.name}.nonEmpty) o.${o.name}.get.native else """""
          }

          initUpdateMethods = initUpdateMethods :+
            st"""def update${oUpper}(): Unit = {
                |  val value = ${o.name}.getText
                |  currentStore.${o.name} = value
                |  ${o.name}.setForeground(fgColor)
                |  update${oUpper}Cust()
                |}"""

          changeListeners = changeListeners :+
            st"addChangeListener(${o.name}.getDocument, update${oUpper} _)"

          updateUi = updateUi :+
            st"${o.name}.setText(currentStore.${o.name})"

          validityChecks = validityChecks :+ st"currentStore.${o.name}Valid"
      }
      hasChanges = hasChanges :+ st"initialStore(currentStore.platform).${o.name} != currentStore.${o.name}"
      buildCommandLineArgs = buildCommandLineArgs :+
        st"""if (currentStore.${o.name} != currentStore.${o.name}Default) {
            |   args = args :+ "--${o.longKey}"${ if (!o.tpe.isInstanceOf[Type.Flag]) s" :+ currentStore.${o.name}" else s""}
            |}"""
    }
  }
  addOptions(tool.opts, "")
  for (g <- tool.groups) {
    addOptions(g.opts, s"${g.name}_")
  }

  val path = Os.path("/Users/belt/devel/sireum/forms/jvm/src/main/scala/org/sireum/forms/HAMRCodeGenFormEx.scala")
  val contents =
    st"""  def parseInt(value: String): Option[Int] = {
        |    try {
        |      Some(value.toInt)
        |    } catch {
        |      case _ : Throwable => None
        |    }
        |  }
        |
        |  case class CodegenOptionStore(${(storeEntries, ",\n\n")})
        |  """

  val hamrUtil = Os.path("/Users/belt/devel/sireum/forms/app/jvm/src/main/scala/org/sireum/forms/HAMRUtil.scala")
  val buildContents =
    st"""  ${(buildEntriesCasing, "\n")}
        |  def buildHamrOptionStore(o: Cli.${optionName}Option): HAMRCodeGenFormEx.CodegenOptionStore = {
        |    val store = HAMRCodeGenFormEx.CodegenOptionStore()
        |    ${(buildEntries, "\n")}
        |    return store
        |  }"""

  val buildCommandLineArgsContent =
    st"""  def buildCommandLineArgs(): Option[String] = {
        |    if (!isValid()) {
        |      JOptionPane.showMessageDialog(new JFrame(), "Please correct invalid entries", "",
        |        JOptionPane.ERROR_MESSAGE);
        |      return None
        |    }
        |    if (!hasChanges()) {
        |      JOptionPane.showMessageDialog(new JFrame(), "No changes detected", "",
        |        JOptionPane.INFORMATION_MESSAGE);
        |      return None
        |    }
        |
        |    var args: Seq[String] = Seq()
        |
        |    if (currentStore.platform == currentStore.platformDefault) {
        |      args = args :+ "--platform" :+ currentStore.platform
        |    }
        |
        |    ${(buildCommandLineArgs, "\n")}
        |
        |    updateInitialStore()
        |
        |    if (args.size == 2) {
        |      JOptionPane.showMessageDialog(new JFrame(), s"Nothing inserted as those are the default options for $${currentStore.platform}", "",
        |        JOptionPane.INFORMATION_MESSAGE);
        |      return None
        |    } else {
        |      return Some(args.mkString(" "))
        |    }
        |  }"""

  writeOver(contents, "CodegenOptionStore", path)
  writeOver(buildContents, "build option store", hamrUtil)
  writeOver(st"    ${(initUpdateMethods, "\n\n")}", "init additions", path)
  writeOver(st"    ${(changeListeners, "\n")}", "change listeners", path)
  writeOver(st"    ${(updateUi, "\n")}", "update ui after platform change", path)
  writeOver(st"""  def isValid(): Boolean = {
                |    return isValidCust &&
                |      ${(validityChecks, " &&\n")}
                |  }""", "isValid", path)
  writeOver(st"""  def hasChanges(): Boolean = {
                |    return (
                |      ${(hasChanges, " ||\n")})
                |  }""", "hasChanges", path)
  writeOver(st"""  def updateInitialStore(): Unit = {
                |    initialStore = initialStore + (currentStore.platform -> currentStore.copy())
                |  }""", "update initial store", path)
  writeOver(buildCommandLineArgsContent, "build command line args", path)

  def writeOver(content: ST, marker: String, dest: Os.Path): Unit = {
    val hamrCliContent = ops.StringOps(dest.read)
    val start = hamrCliContent.stringIndexOf(s"// BEGIN $marker")
    val endText: String = s"// END $marker"
    val end = hamrCliContent.stringIndexOf(endText)
    if (start < 0 || end < 0) {
      halt(s"Couldn't find BEGIN/END markers for $marker")
    }
    dest.writeOver(
      st"""${hamrCliContent.substring(0, start - 1)}
          |// BEGIN $marker
          |$content
          |// END $marker
          |${hamrCliContent.substring(end + endText.size + 1, hamrCliContent.size)}""".render)
    println(s"Modified: $dest")
  }
}

// Ignore the Tipe error "'hamr' is not a member of package 'org.sireum'"
process(
  tool = org.sireum.hamr.sysml.cli.sysmlCodegen,
  optionName =  "SireumHamrSysmlCodegen"
)