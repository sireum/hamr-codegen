// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlPort, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.ir

object WriteFrameBuilder {

  @datatype class ComponentWriteSet(val componentPath: IdPath,
                                    val outPorts: ISZ[AadlPort],
                                    val stateVars: ISZ[ir.GclStateVar])

  @pure def getGclInfoOpt(threadPath: IdPath, symbolTable: SymbolTable): Option[GclAnnexClauseInfo] = {
    symbolTable.annexClauseInfos.get(threadPath) match {
      case Some(clauses) =>
        return Some(clauses(0).asInstanceOf[GclAnnexClauseInfo])
      case _ => return None()
    }
  }

  @pure def computeWriteSet(component: AadlComponent, symbolTable: SymbolTable): ComponentWriteSet = {
    val outPorts: ISZ[AadlPort] = component.getPorts().filter(p => p.direction == ir.Direction.Out || p.direction == ir.Direction.InOut)
    val stateVars: ISZ[ir.GclStateVar] = getGclInfoOpt(component.path, symbolTable) match {
      case Some(info) => info.annex.state
      case _ => ISZ()
    }
    return ComponentWriteSet(
      componentPath = component.path,
      outPorts = outPorts,
      stateVars = stateVars)
  }

  @pure def buildAll(threads: ISZ[AadlComponent], symbolTable: SymbolTable): Map[IdPath, ComponentWriteSet] = {
    var result: Map[IdPath, ComponentWriteSet] = Map.empty
    for (thread <- threads) {
      result = result + thread.path ~> computeWriteSet(thread, symbolTable)
    }
    return result
  }
}
