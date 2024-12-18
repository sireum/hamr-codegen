// #Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.HamrProperties.HAMR__BIT_CODEC_MAX_SIZE
import org.sireum.message.{Position, Reporter}
import org.sireum.hamr.codegen.common.properties.{CasePropertiesProperties, CaseSchedulingProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.util.HamrCli.{CodegenHamrPlatform, CodegenOption}
import org.sireum.hamr.ir
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BaseType, TypeUtil}
import org.sireum.hamr.codegen.common.util.ExperimentalOptions

object Linter {
  @pure def lint(options: CodegenOption, symbolTable: SymbolTable, reporter: Reporter): Unit = {

    val shouldUseRawConnections: B = PropertyUtil.getUseRawConnection(symbolTable.rootSystem.properties)

    @pure def lint_Threads(): Unit = {
      for (thread <- symbolTable.getThreads()) {
        if (thread.toVirtualMachine(symbolTable)) {
          val parent = thread.getParent(symbolTable)
          parent.getBoundProcessor(symbolTable) match {
            case Some(avp: AadlVirtualProcessor) =>
              if (avp.dispatchProtocol != Dispatch_Protocol.Periodic) {
                val mesg = s"Virtual processor ${avp.identifier} has ${avp.dispatchProtocol.name} dispatching. Only periodic dispatching is supported."
                reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
              }
            case x =>
              val mesg = s"Thread ${thread.identifier} is going to a VM so its process ${parent.identifier} must be bound to a virtual processor. Instead it's bound to ${x}"
              reporter.error(thread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        } else {
          if (thread.dispatchProtocol == Dispatch_Protocol.Periodic && thread.period.isEmpty) {
            val mesg = s"Must specify ${OsateProperties.TIMING_PROPERTIES__PERIOD} for periodic thread ${thread.identifier}"
            reporter.error(thread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }
      }
    }

    // restrict when wire protocol and CakeML components are allowed
    @pure def lint_WireProtocol_CakeML(): Unit = {
      if (symbolTable.hasCakeMLComponents() || shouldUseRawConnections) {
        if (options.platform == CodegenHamrPlatform.SeL4_Only || options.platform == CodegenHamrPlatform.SeL4_TB) {
          var reasons: ISZ[String] = ISZ()
          if (symbolTable.hasCakeMLComponents()) {
            reasons = reasons :+ "CakeML components"
          }
          if (shouldUseRawConnections) {
            reasons = reasons :+ "wire protocol"
          }
          val mesg = st"${options.platform} platform does not support ${(reasons, ", ")}".render
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    @pure def lint_ProcessorBindings(): B = {
      var validProcessorBindings: B = T
      if (symbolTable.hasVM()) {

        var validProcessors: ISZ[AadlProcessor] = ISZ()
        var seenVirtualProcessor: Set[AadlVirtualProcessor] = Set.empty

        for (process <- symbolTable.getProcesses().filter(p => p.toVirtualMachine(symbolTable))) {

          symbolTable.getBoundProcessors(process) match {
            case ISZ(processor) =>
              processor match {
                case avp: AadlVirtualProcessor =>
                  if (seenVirtualProcessor.contains(avp)) {
                    val msg = s"Multiple processes are bound to the virtual processor ${avp.identifier}. That might be acceptable, but is currently unexpected. Please report"
                    reporter.error(avp.component.identifier.pos, CommonUtil.toolName, msg)
                    validProcessorBindings = F
                  }
                  seenVirtualProcessor = seenVirtualProcessor + avp

                  symbolTable.getBoundProcessors(avp) match {
                    case ISZ(avpsBoundProcessor) =>
                      avpsBoundProcessor match {
                        case avp2: AadlVirtualProcessor =>
                          val mesg = s"Chained virtual processors is not supported.  Bind virtual processor ${avp.identifier} to an actual processor"
                          reporter.error(avp2.component.identifier.pos, CommonUtil.toolName, mesg)
                          validProcessorBindings = F
                        case ap: AadlProcessor =>
                          PropertyUtil.getDiscreetPropertyValue(avp.component.properties, CasePropertiesProperties.PROP__CASE_PROPERTIES__OS) match {
                            case Some(ir.ValueProp(os)) =>
                              if (os != "Linux") {
                                val mesg = s"Invalid OS ${os} for virtual processor ${avp.identifier}.  HAMR only supports Linux based virtual machines"
                                reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                                validProcessorBindings = F
                              }
                            case _ =>
                              val mesg = s"${CasePropertiesProperties.PROP__CASE_PROPERTIES__OS} not provided for virtual processor ${avp.identifier}.  Assuming Linux"
                              reporter.warn(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                          }

                          // ok, virtual processor is bound to an actual processor
                          validProcessors = validProcessors :+ ap
                      }
                    case x if x.nonEmpty =>
                      val mesg = s"Virtual processor ${avp.identifier} can only be bound to a single processor but ${x.size} found"
                      reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                      validProcessorBindings = F

                    case _ =>
                      val mesg = s"Virtual processor ${avp.identifier} must be bound to an actual processor since process ${process.identifier} is bound to it"
                      reporter.error(avp.component.identifier.pos, CommonUtil.toolName, mesg)
                      validProcessorBindings = F
                  }
                case x: AadlProcessor =>
                  validProcessors = validProcessors :+ x // ok, process is bound directly to an actual processor
                case x =>
                  val mesg = s"Unexpected: process ${process.identifier} is bound to ${x} rather than a processor"
                  reporter.error(process.component.identifier.pos, CommonUtil.toolName, mesg)
                  validProcessorBindings = F
              }
            case x if x.nonEmpty =>
              reporter.error(process.component.identifier.pos, CommonUtil.toolName, s"Only a single bound processor is currently allows but found ${x.size}")
            case _ =>
              halt(s"Infeasible: ${process.identifier} is going to a vm but it isn't bound to a processor?")
          }
        }

        for (p <- validProcessors) {
          p.getPacingMethod() match {
            case Some(CaseSchedulingProperties.PacingMethod.SelfPacing) =>
              val mesg = s"Model has virtual machines so it must use the pacer component style of pacing"
              reporter.error(p.component.identifier.pos, CommonUtil.toolName, mesg)
            case _ =>
          }
        }
      }
      return validProcessorBindings
    }

    @pure def lint_CakeMLComponents(willUseDomainScheduling: B): Unit = {
      if (symbolTable.hasCakeMLComponents()) {

        for (cakemlThread <- symbolTable.getThreads().filter((a: AadlThread) => a.isCakeMLComponent())) {
          if (cakemlThread.dispatchProtocol != Dispatch_Protocol.Periodic) {
            val mesg = s"CakeML components must be periodic: ${cakemlThread.identifier}"
            reporter.error(cakemlThread.component.identifier.pos, CommonUtil.toolName, mesg)
          }
        }
        if (!symbolTable.rootSystem.getUseRawConnection()) {
          val mesg = "Raw connections (i.e. byte-arrays) must be used when integrating CakeML components."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }

        if (!willUseDomainScheduling) {
          val mesg = "Model contains CakeML components so it must use domain scheduling."
          reporter.error(None(), CommonUtil.toolName, mesg)
        }
      }
    }

    // if raw then all data components used in connectionInstances b/w threads
    // must have bit size specified and it must be greater than 0
    @pure def lint_RawConnections(): Unit = {
      if (shouldUseRawConnections) {
        for (conn <- symbolTable.aadlConnections) {
          conn match {
            case apc: AadlPortConnection =>
              if (!TypeUtil.isEmptyType(apc.connectionDataType)) {
                val connName = apc.name
                val typeName = apc.connectionDataType.name
                val pos: Option[Position] = apc.connectionDataType.container match {
                  case Some(c) => c.identifier.pos
                  case _ => None()
                }
                apc.connectionDataType.bitSize match {
                  case Some(z) =>
                    if (z <= 0) {
                      val mesg = s"${HAMR__BIT_CODEC_MAX_SIZE} must be greater than 0 for data type ${typeName} used by connection ${connName}"
                      reporter.error(pos, CommonUtil.toolName, mesg)
                    }
                  case _ =>
                    apc.connectionDataType match {
                      case b: BaseType =>
                        val mesg = s"Unbounded type ${typeName} is not currently supported when using the wire protocol -- used by connection ${conn} "
                        reporter.error(pos, CommonUtil.toolName, mesg)
                      case _ =>
                        val mesg = s"${HAMR__BIT_CODEC_MAX_SIZE} must be specified for data type ${typeName} used by connection ${connName}"
                        reporter.error(pos, CommonUtil.toolName, mesg)
                    }
                }
              }
            case _ =>
          }
        }
      }
    }

    // TODO: move to ACT linting
    @pure def lint_caseConnectors(): Unit = {
      if (!ExperimentalOptions.useCaseConnectors(options.experimentalOptions)) { // makes sure there are no fan outs from native components to vm components
        for (conns <- symbolTable.outConnections.entries) {
          var vmConns = F
          var nativeConns = F
          for (conn <- conns._2) {
            if (symbolTable.getThreadById(conn.dst.component.name).toVirtualMachine(symbolTable)) {
              vmConns = T
            } else {
              nativeConns = T
            }
          }
          assert(!(vmConns && nativeConns),
            s"Fan out from ${conns._1} involves both native and VM components which is not currently supported")
        }
      }
    }

    lint_Threads()

    lint_WireProtocol_CakeML()

    val validProcessorBindings = lint_ProcessorBindings()

    val willUseDomainScheduling: B =
      validProcessorBindings && PacerUtil.canUseDomainScheduling(symbolTable, options.platform, reporter)

    if (symbolTable.hasVM() && !willUseDomainScheduling) {
      val msg = "Model contains VM components so it must use domain scheduling"
      reporter.error(None(), CommonUtil.toolName, msg)
    }

    lint_CakeMLComponents(willUseDomainScheduling)

    lint_RawConnections()

    lint_caseConnectors()
  }
}
