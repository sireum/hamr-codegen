// #Sireum
package org.sireum.hamr.codegen.microkit.lint

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlProcess, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object Linter {

  def lint(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): B = {

    for (p <- symbolTable.getAllBoundProcessors()) {
      if (p.getClockPeriod().isEmpty) {
        reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, "Bound processors must be assigned a clock period.")
      }
      if (p.getFramePeriod().isEmpty) {
        reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, "Bound processors must be assigned a frame period.")
      }
    }

    var assignedDomains: Map[Z, AadlProcess] = Map.empty
    for (p <- symbolTable.getProcesses()){
      if (p.getBoundProcessor(symbolTable).isEmpty) {
        reporter.error(p.component.identifier.pos, MicrokitCodegen.toolName, "Processes must be bound to a processor.")
      }
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
        //case Dispatch_Protocol.Sporadic =>
        case s =>
          reporter.error(t.component.identifier.pos, MicrokitCodegen.toolName, s"Dispatch protocol ${s.name} is not currently supported.")
      }

      t.period match {
        case Some(_) =>
        case _ =>
      }
    }

    return !reporter.hasError
  }


}
