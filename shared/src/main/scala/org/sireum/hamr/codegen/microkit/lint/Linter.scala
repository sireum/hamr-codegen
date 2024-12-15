// #Sireum
package org.sireum.hamr.codegen.microkit.lint

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlData, AadlFeatureData, AadlProcess, AadlProcessor, AadlVirtualProcessor, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlTypes, BaseType}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object Linter {

  def lint(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): B = {
    val allProcesses = symbolTable.getProcesses()

    for (p <- allProcesses) {
      p.getBoundProcessor(symbolTable) match {
        case Some(x) =>
          val actualProcessor: AadlProcessor =
            x match {
              case avp: AadlVirtualProcessor =>
                symbolTable.getActualBoundProcess(avp) match {
                  case Some(ap) => ap
                  case _ =>
                    reporter.error(avp.component.identifier.pos, MicrokitCodegen.toolName, "Virtual processors must be bound to an actual processor")
                    return F
                }
              case ap: AadlProcessor => ap
            }
          if (actualProcessor.getClockPeriod().isEmpty) {
            reporter.error(actualProcessor.component.identifier.pos, MicrokitCodegen.toolName, "Bound processors must be assigned a clock period.")
          }
          if (actualProcessor.getFramePeriod().isEmpty) {
            reporter.error(actualProcessor.component.identifier.pos, MicrokitCodegen.toolName, "Bound processors must be assigned a frame period.")
          }
        case _ =>
          reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, "Processes must be bound to an actual processor")
      }
    }

    var assignedDomains: Map[Z, AadlProcess] = Map.empty
    for (p <- allProcesses){
      if (p.getThreads().size > 1) {
        reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, "Processes can only contain a single thread.")
      }
      p.getDomain(symbolTable) match {
        case Some(dom) =>
          assignedDomains.get(dom) match {
            case Some(other) =>
              reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, s"Domain $dom has already been assigned to ${other.identifier}.")
            case _ =>
              assignedDomains = assignedDomains + dom ~> p
          }
        case _ =>
          reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, s"Processes must be assigned to a scheduling domain.")
      }
    }

    for (t <- symbolTable.getThreads()) {
      t.dispatchProtocol match {
        case Dispatch_Protocol.Periodic =>
          if (t.period.isEmpty) {
            reporter.error(t.component.identifier.pos, MicrokitCodegen.toolName, s"Periodic threads must be assigned a period property.")
          }
        case Dispatch_Protocol.Sporadic =>
        case s =>
          reporter.error(t.component.identifier.pos, MicrokitCodegen.toolName, s"Dispatch protocol ${s.name} is not currently supported.")
      }

      t.period match {
        case Some(_) =>
        case _ =>
      }

      for(p <- t.getPorts()) {
         p match {
          case i: AadlFeatureData =>
            i.aadlType match {
              case b: BaseType =>
                b.classifier match {
                  case ISZ("Base_Types::Integer") =>
                    reporter.error(p.feature.identifier.pos, MicrokitCodegen.toolName, "Unbounded Integer is not supported for Microkit")
                  case ISZ("Base_Types::Float") =>
                    reporter.error(p.feature.identifier.pos, MicrokitCodegen.toolName, "Unbounded Float is not supported for Microkit")
                  case _ =>
                }
              case _ =>
            }
          case _ =>
        }
      }
    }

    return !reporter.hasError
  }
}
