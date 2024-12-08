// #Sireum

package org.sireum.hamr.codegen.act.util

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlThread, SymbolTable}

object PathUtil {
  val DIR_BIN: String = "bin"

  val RUN_CAMKES_SCRIPT_PATH: String = s"${DIR_BIN}/run-camkes.sh"

  def getComponentSourcePath(aadlThread: AadlThread, symbolTable: SymbolTable): String = {
    val componentDirName = Util.getCamkesComponentName(aadlThread, symbolTable)
    return s"${Util.DIR_COMPONENTS}/${componentDirName}/src"
  }

  def getComponentHeaderPath(aadlThread: AadlThread, symbolTable: SymbolTable): String = {
    val componentDirName = Util.getCamkesComponentName(aadlThread, symbolTable)
    return s"${Util.DIR_COMPONENTS}/${componentDirName}/includes"
  }

  def getComponentGlueCodeFilename(aadlThread: AadlThread): String = {
    return Util.brand(Util.getClassifier(aadlThread.component.classifier.get))
  }

  def getComponentGlueCodeHeaderFilename(aadlThread: AadlThread): String = {
    return Util.genCHeaderFilename(getComponentGlueCodeFilename(aadlThread))
  }

  def getComponentGlueCodeImplementationFilename(aadlThread: AadlThread): String = {
    return Util.genCImplFilename(getComponentGlueCodeFilename(aadlThread))
  }
}
