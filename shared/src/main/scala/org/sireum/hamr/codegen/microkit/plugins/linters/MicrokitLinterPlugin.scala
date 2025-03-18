// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.linters

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store, StoreValue}
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlProcess, AadlProcessor, AadlThread, AadlVirtualProcessor, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.MicrokitLintPlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.ir.{Aadl, ConnectionInstance, Direction}
import org.sireum.message.Reporter

object MicrokitLinterPlugin {
  val KEY_MicrokitLinter: String = "KEY_MicrokitLinter"

  @strictpure def getTouchedTypesOpt(store: Store): Option[TouchedTypes] = store.get(KEY_MicrokitLinter).asInstanceOf[Option[TouchedTypes]]

  @strictpure def getTouchedTypes(store: Store): TouchedTypes = getTouchedTypesOpt(store).get
}

@datatype class TouchedTypes(val orderedDependencies: ISZ[String],
                             val substitutionTypeMap: Map[String, AadlType]) extends StoreValue

@sig trait MicrokitLinterPlugin extends MicrokitLintPlugin {
  @pure override def validModel(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes,
                                symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, B) = {
    var localStore = store

    val allProcesses = symbolTable.getProcesses()

    // getAllTouchedTypes also lints the touched types
    val touchedTypes = MicrokitTypeUtil.getAllTouchedTypes(types, symbolTable, reporter)
    localStore = localStore + MicrokitLinterPlugin.KEY_MicrokitLinter ~>
      TouchedTypes(touchedTypes._1, touchedTypes._2)

    if (types.rawConnections) {
      reporter.error(None(), MicrokitCodegen.toolName, "Raw connections are not currently supported")
    }

    for (process <- allProcesses) {
      process.getBoundProcessor(symbolTable) match {
        case Some(x) =>
          val actualProcessor: AadlProcessor =
            x match {
              case avp: AadlVirtualProcessor =>
                symbolTable.getActualBoundProcessors(avp) match {
                  case ISZ(ap) => ap
                  case x if x.size > 1 =>
                    reporter.error(avp.component.identifier.pos, MicrokitCodegen.toolName, s"Only a single bound processor is supported but found ${x.size}")
                    return (localStore, F)
                  case _ =>
                    reporter.error(avp.component.identifier.pos, MicrokitCodegen.toolName, "Virtual processors must be bound to an actual processor")
                    return (localStore, F)
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
          reporter.error(process.component.identifier.pos, MicrokitCodegen.toolName, "Processes must be bound to an actual processor")
      }
    }

    var assignedDomains: Map[Z, AadlProcess] = Map.empty
    for (process <- allProcesses){
      if (process.getThreads().size > 1) {
        reporter.error(process.component.identifier.pos, MicrokitCodegen.toolName, "Processes can only contain a single thread.")
      }
      process.getDomain(symbolTable) match {
        case Some(dom) =>
          assignedDomains.get(dom) match {
            case Some(other) =>
              reporter.error(process.component.identifier.pos, MicrokitCodegen.toolName, s"Domain $dom has already been assigned to ${other.identifier}.")
            case _ =>
              assignedDomains = assignedDomains + dom ~> process
          }
        case _ =>
          reporter.error(process.component.identifier.pos, MicrokitCodegen.toolName, s"Processes must be assigned to a scheduling domain.")
      }
    }

    for (thread <- symbolTable.getThreads()) {
      thread.dispatchProtocol match {
        case Dispatch_Protocol.Periodic =>
          if (thread.period.isEmpty) {
            reporter.error(thread.component.identifier.pos, MicrokitCodegen.toolName, s"Periodic threads must be assigned a period property.")
          }
        case Dispatch_Protocol.Sporadic =>
        case s =>
          reporter.error(thread.component.identifier.pos, MicrokitCodegen.toolName, s"Dispatch protocol ${s.name} is not currently supported.")
      }

      for(p <- thread.getPorts()) {
        p.direction match {
          case Direction.In =>
            for (inConn <- symbolTable.getInConnections(p.path)) {
              checkConnection(inConn, symbolTable, reporter)

              if (symbolTable.getInConnections(p.path).size > 1) {
                reporter.error(p.feature.identifier.pos, MicrokitCodegen.toolName, "Fan in connections (including event ports) are disallowed for Microkit")
              }
            }
          case Direction.Out =>
            for (outConn <- symbolTable.getOutConnections(p.path)) {
              checkConnection(outConn, symbolTable, reporter)
            }
          case Direction.InOut =>
            reporter.error(p.feature.identifier.pos, MicrokitCodegen.toolName, "Bidirectional ports are currently not supported")
          case Direction.None =>
            reporter.error(p.feature.identifier.pos, MicrokitCodegen.toolName, "Ports must either by in or out")
        }
      }
    }
    return (localStore, !reporter.hasError)
  }

  @pure def checkConnection(connection: ConnectionInstance, symbolTable: SymbolTable, reporter: Reporter): Unit = {
    (symbolTable.componentMap.get(connection.src.component.name).get, symbolTable.componentMap.get(connection.dst.component.name).get) match {
      case (a: AadlThread, b: AadlThread) =>
      case (src, dst) =>
        reporter.error(connection.name.pos, MicrokitCodegen.toolName,
          s"Currently only supporting thread to thread connections but ${src.identifier} is connected to ${dst.identifier}")
    }

    (symbolTable.featureMap.get(connection.src.feature.get.name).get, symbolTable.featureMap.get(connection.dst.feature.get.name).get) match {
      case (srcPort: AadlPort, dstPort: AadlPort) =>
        if (!(
          (srcPort.isInstanceOf[AadlDataPort] && dstPort.isInstanceOf[AadlDataPort]) ||
            (srcPort.isInstanceOf[AadlEventPort] && dstPort.isInstanceOf[AadlEventPort]) ||
            (srcPort.isInstanceOf[AadlEventDataPort] && dstPort.isInstanceOf[AadlEventDataPort]))) {
          reporter.error(connection.name.pos, MicrokitCodegen.toolName, s"Connected ports have the same type")
        }
        if (MicrokitTypeUtil.getPortType(srcPort) != MicrokitTypeUtil.getPortType(dstPort)) {
          reporter.error(connection.name.pos, MicrokitCodegen.toolName, s"Connected ports must have the same payload type")
        }
      case (src, dst) =>
        reporter.error(connection.name.pos, MicrokitCodegen.toolName, s"Currently only supporting port to port connections but ${src.identifier} is connected to ${dst.identifier}")
    }
  }
}

@datatype class DefaultMicrokitLinterPlugin extends MicrokitLinterPlugin {
  @strictpure override def name: String = "DefaultMicrokitLinter"
}
