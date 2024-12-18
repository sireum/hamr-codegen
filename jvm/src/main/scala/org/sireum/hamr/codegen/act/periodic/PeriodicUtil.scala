// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act.proof.ProofContainer.SchedulingType
import org.sireum.hamr.codegen.act.util.{ActPlatform, Util}
import org.sireum.hamr.codegen.common.properties.CaseSchedulingProperties
import org.sireum.hamr.codegen.common.properties.CaseSchedulingProperties.PacingMethod
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.HamrCli.CodegenHamrPlatform

object PeriodicUtil {

  def getDispatchingType(symbolTable: SymbolTable, usingPacer: B): PeriodicDispatchingType.Type = {
    val ret: PeriodicDispatchingType.Type =
      if (usingPacer) {
        if (symbolTable.hasVM()) {
          PeriodicDispatchingType.Pacer
        } else {
          symbolTable.getThreads()(0).getParent(symbolTable).getBoundProcessor(symbolTable).get.getPacingMethod() match {
            case Some(CaseSchedulingProperties.PacingMethod.Pacer) => PeriodicDispatchingType.Pacer
            case Some(CaseSchedulingProperties.PacingMethod.SelfPacing) => PeriodicDispatchingType.SelfPacer
            case _ => PeriodicDispatchingType.SelfPacer
          }
        }
      } else {
        PeriodicDispatchingType.PeriodicDispatcher
      }
    return ret
  }

  def requiresTimeServer(symbolTable: SymbolTable, usingPacer: B): B = {
    return symbolTable.hasPeriodicThreads() && !usingPacer
  }

  def requiresPacerArtifacts(aadlComponent: AadlDispatchableComponent, symbolTable: SymbolTable, usingPacer: B): B = {
    return aadlComponent.isPeriodic() && usingPacer
  }

  def getBoundProcessor(symbolTable: SymbolTable): AadlProcessor = {
    // TODO: assumes useDomainScheduling returned true so all processes
    // should be bound to the same processor.  Perhaps instead have
    // an 'ACT' symbol resolution phase that has sel4/case specific info

    val aadlThread = symbolTable.getThreads()(0)
    val aadlProcess = aadlThread.getParent(symbolTable)
    val ret: AadlProcessor = symbolTable.getBoundProcessors(aadlProcess) match {
      case ISZ(p: AadlProcessor) => p
      case ISZ(avp: AadlVirtualProcessor) =>
        symbolTable.getActualBoundProcessors(avp) match {
          case ISZ(ap) => ap
          case x =>
            halt(s"Infeasbile: Virtual processor ${avp.identifier} is bound to ${x.size} processors")
        }
      case x => halt("Unexpected: $x")
    }
    return ret
  }

  def getSchedulingType(symbolTable: SymbolTable, platform: ActPlatform.Type): SchedulingType.Type = {
    val ret: SchedulingType.Type =
      if (useDomainScheduling(symbolTable, platform)) {
        var cand: Set[PacingMethod.Type] = Set.empty
        for (b <- symbolTable.getAllBoundProcessors() if b.getPacingMethod().nonEmpty) {
          cand = cand + b.getPacingMethod().get
        }
        if (cand.nonEmpty) {
          assert(cand.size == 1, "Found multiple pacing strategies")
          cand.elements(0) match {
            case PacingMethod.SelfPacing => SchedulingType.SelfPacing
            case PacingMethod.Pacer => SchedulingType.Pacing
          }
        }
        else if (!symbolTable.hasVM()) {
          SchedulingType.SelfPacing
        }
        else {
          SchedulingType.Pacing
        }
      }
      else {
        SchedulingType.PeriodicDispatching
      }

    return ret
  }

  def useDomainScheduling(symbolTable: SymbolTable, platform: ActPlatform.Type): B = {
    val _platform: CodegenHamrPlatform.Type = platform match {
      case ActPlatform.SeL4_TB => CodegenHamrPlatform.SeL4_TB
      case ActPlatform.SeL4 => CodegenHamrPlatform.SeL4
      case ActPlatform.SeL4_Only => CodegenHamrPlatform.SeL4_Only
    }
    return PacerUtil.canUseDomainScheduling(symbolTable, _platform, Util.reporter)
  }
}
