// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.util.CodeGenPlatform
import org.sireum.message.Reporter

object PacerUtil {

  def canUseDomainScheduling(symbolTable: SymbolTable, platform: CodeGenPlatform.Type, reporter: Reporter): B = {
    // - platform is seL4 or seL4_Only (or JVM and any nix)
    // - all threads must be in separate processes
    // - each process with a thread must have domain info
    // - every process with a thread must be bound to the same processor
    // - the bound processor must have the following annotations
    //      - Frame_Period
    //		  - Clock_Period

    val threads = symbolTable.getThreads()

    var canUseDomainScheduling: B = T

    var mesg: ISZ[ST] = ISZ()

    if (platform == CodeGenPlatform.SeL4_TB) {
      canUseDomainScheduling = F
      mesg = mesg :+ st"Domain scheduling not supported for legacy Trusted Build platform"
    }

    // - all threads must be in separate processes
    var processesWithThreads: ISZ[AadlProcess] = ISZ()
    for (t <- threads) {
      val p = t.getParent(symbolTable)

      if (ops.ISZOps(processesWithThreads).contains(p)) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"More than one thread is in process ${p.identifier}.  Move ${t.identifier} to a different process."
      }
      processesWithThreads = processesWithThreads :+ p
    }

    // - each process with a thread must have domain info
    if (canUseDomainScheduling) {
      var withoutDomain: ISZ[AadlProcess] = ISZ()
      for (p <- processesWithThreads) {
        if (p.getDomain().isEmpty) {
          withoutDomain = withoutDomain :+ p
        }
      }

      if (withoutDomain.size > 0) {
        canUseDomainScheduling = F
        val withoutDomains = st"${(withoutDomain.map((m: AadlProcess) => m.path), ", ")}".render
        mesg = mesg :+ st"The following processes do not have domain information: ${withoutDomains}"
      }
    }

    var boundProcessor: Option[AadlProcessor] = None()

    // - each process must be bound to the same processor
    if (canUseDomainScheduling) {
      var boundProcessors: Set[AadlProcessor] = Set.empty
      var unboundedProcesses: ISZ[AadlProcess] = ISZ()
      for (p <- processesWithThreads) {
        symbolTable.getBoundProcessor(p) match {
          case Some(proc) => boundProcessors = boundProcessors + proc
          case _ => unboundedProcesses = unboundedProcesses :+ p
        }
      }

      if (unboundedProcesses.nonEmpty) {
        canUseDomainScheduling = F
        val x = st"${(unboundedProcesses.map((m: AadlProcess) => m.path), ",")}"
        mesg = mesg :+ st"The following processes are not bound to a processor: ${x}"
      }

      if (boundProcessors.size > 1) {
        canUseDomainScheduling = F
        val x = st"${(boundProcessors.elements.map((m: AadlProcessor) => m.path), ",")}"
        mesg = mesg :+
          st"""All processes containing threads must be bound to the same processor.
              |Bind all processes to exactly one of the following: ${x}"""
      } else if (canUseDomainScheduling) {
        boundProcessor = Some(boundProcessors.elements(0))
      }
    }

    if (canUseDomainScheduling) {
      assert(boundProcessor.nonEmpty, "Expecting a single bound processor") // sanity check
      val b = boundProcessor.get
      // - the bound processor must have the following annotations
      //      - Frame_Period
      //		  - Clock_Period

      if (b.getClockPeriod().isEmpty) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"Bound processor missing Clock_Period annotation: ${b.path}"
      }

      if (b.getFramePeriod().isEmpty) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"Bound processor missing Frame_Period annotation: ${b.path}"
      }
    }

    if (mesg.nonEmpty) {
      assert(!canUseDomainScheduling, "Only expecting messages related to why domain scheduling won't be used") // sanity check
      val m =
        st"""Domain scheduling will not be used due to the following reasons:
            |  ${(mesg, "\n\n")}"""
      reporter.info(None(), CommonUtil.toolName, m.render)
    }

    return canUseDomainScheduling
  }
}
