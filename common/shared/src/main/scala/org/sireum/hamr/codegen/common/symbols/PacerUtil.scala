// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.CaseSchedulingProperties
import org.sireum.hamr.codegen.common.util.CodeGenPlatform
import org.sireum.message.Reporter

object PacerUtil {

  def canUseDomainScheduling(symbolTable: SymbolTable, platform: CodeGenPlatform.Type, reporter: Reporter): B = {
    // - platform is seL4 or seL4_Only (or JVM and any nix)
    // - all threads must be in separate processes
    // - each process with a thread must have domain info
    // - every process with a thread must be bound to the same processor
    // - only processes bound to the same VM can be in the same domain
    // - must use pacer component if model has VMs
    // - domain 1 reserved if pacer component is used
    // - the bound processor must have the following annotations
    //      - Frame_Period
    //		  - Clock_Period

    val threads = symbolTable.getThreads()

    var canUseDomainScheduling: B = T

    var mesg: ISZ[ST] = ISZ()

    canUseDomainScheduling = platform match {
      case CodeGenPlatform.SeL4_TB =>
        mesg = mesg :+ st"Domain scheduling not supported for legacy Trusted Build platform"
        F
      case _ => T
    }

    var processesWithThreads: Set[AadlProcess] = Set.empty
    for (t <- threads) {
      val p = t.getParent(symbolTable)

      if (!t.toVirtualMachine(symbolTable) && processesWithThreads.contains(p)) {
        canUseDomainScheduling = F
        mesg = mesg :+ st"More than one thread is in non-VM process ${p.identifier}.  Move ${t.identifier} to a different process."
      }

      processesWithThreads = processesWithThreads + p
    }

    if (processesWithThreads.isEmpty) {
      canUseDomainScheduling = F
      mesg = mesg :+ st"No processes found."
    }

    // - each process with a thread must have domain info
    if (canUseDomainScheduling) {
      var withoutDomain: ISZ[AadlProcess] = ISZ()
      for (p <- processesWithThreads.elements) {
        if (p.getDomain(symbolTable).isEmpty) {
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
    // - only processes bound to the same VM can be in the same domain
    // - domain 1 reserved if pacer component is used
    if (canUseDomainScheduling) {
      var domain1InUse: Option[AadlProcess] = None()
      var seenNonVMDomains: Set[Z] = Set.empty
      var vmDomainMapping: Map[Z, AadlVirtualProcessor] = Map.empty

      var boundProcessors: Set[AadlProcessor] = Set.empty
      var unboundedProcesses: ISZ[AadlProcess] = ISZ()

      for (p <- processesWithThreads.elements) {
        val domain = p.getDomain(symbolTable).get
        if (domain == 1) {
          domain1InUse = Some(p)
        }
        symbolTable.getBoundProcessor(p) match {
          case Some(proc: AadlProcessor) =>
            boundProcessors = boundProcessors + proc
            if (seenNonVMDomains.contains(domain)) {
              mesg = mesg :+ st"More than one process in domain ${domain}. Only processes bound to the same VM can be in the same domain."
            }
            seenNonVMDomains = seenNonVMDomains + domain
          case Some(proc: AadlVirtualProcessor) =>
            boundProcessors = boundProcessors + symbolTable.getActualBoundProcess(proc).get
            vmDomainMapping.get(domain) match {
              case Some(proc2) if proc != proc2 =>
                mesg = mesg :+ st"Multiple VM bound processors are in domain ${domain}"
              case _ =>
                vmDomainMapping = vmDomainMapping + (domain ~> proc)
            }
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
        val processorWantsPacer: B = boundProcessor.get.getPacingMethod() match {
          case Some(CaseSchedulingProperties.PacingMethod.Pacer) => T
          case Some(CaseSchedulingProperties.PacingMethod.SelfPacing) =>
            if (symbolTable.hasVM()) {
              mesg = mesg :+ st"Processor ${boundProcessor.get.path} is annotated as self pacing, but the model contains VMs so it must use the Pacer component"
            }
            F
          case _ => F
        }
        if (domain1InUse.nonEmpty && (symbolTable.hasVM() || processorWantsPacer)) {
          mesg = mesg :+ st"Process ${domain1InUse.get.path} is in domain 1 but that is reserved for the pacer component"
        }
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

    val sel4Target: B = platform == CodeGenPlatform.SeL4 || platform == CodeGenPlatform.SeL4_Only || platform == CodeGenPlatform.SeL4_TB
    if (mesg.nonEmpty && sel4Target) {
      assert(!canUseDomainScheduling, "Only expecting messages related to why domain scheduling won't be used") // sanity check
      val m =
        st"""Domain scheduling will not be used due to the following reasons:
            |  ${(mesg, "\n\n")}"""
      reporter.info(None(), CommonUtil.toolName, m.render)
    }

    return canUseDomainScheduling
  }
}
