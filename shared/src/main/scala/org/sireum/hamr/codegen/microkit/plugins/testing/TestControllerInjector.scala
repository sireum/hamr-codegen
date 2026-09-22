// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.testing

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object TestControllerInjector {
  val toolName: String = "Test Controller Injector"

  // The controller runs a test script that drives the scheduler and then inspects and
  // mutates component state.  Its frames are deeper than a component's and Microkit's
  // default PD stack is 4 KiB, so it gets the same allowance the monitors take.
  val defaultStackSizeKiBytes: Z = z"64"

  // The controller holds no timeslice (TestScheduler-design.md D4), so its period is
  // never used for scheduling.  A value is still required for the model to resolve.
  val defaultPeriodMs: Z = z"50"
}

/** Injects the test controller protection domain: a process containing a single Rust
  * thread with **no ports at all**.
  *
  * This is deliberately not MonitorInjector or UserLandMonitorInjector.  Both of those
  * fan observation ports out across every connection in the model, and the user-land one
  * additionally injects sched_state/sched_schedule ports backed by HAMR queue types.  The
  * controller needs none of that: it talks to the scheduler through the test_cmd and
  * test_status regions, which are wired at the system description level rather than as
  * AADL ports, and its later inspection of component state (stage 5) uses the existing
  * port and sv_ regions directly.
  *
  * The only reason the controller exists as an AADL component at all is that the Rust
  * code generation pipeline is driven by the thread list; nothing else about it is
  * model-level.
  */
@datatype class TestControllerInjector {

  def inject(model: ir.Aadl,
             controllerProcessPath: IdPath,
             controllerThreadPath: IdPath,
             symbolTable: SymbolTable,
             reporter: Reporter): Option[ir.Aadl] = {

    val system = model.components(0)

    if (symbolTable.getProcesses().isEmpty) {
      reporter.error(None(), TestControllerInjector.toolName,
        "Cannot inject the test controller: the model has no processes to inherit a processor binding from")
      return None()
    }

    // Inherit the processor binding from the first process so the linter's "Processes must
    // be bound to an actual processor" check passes.
    val processorBindingProp: ir.Property = {
      val firstProcess = symbolTable.getProcesses()(0)
      firstProcess.getBoundProcessor(symbolTable) match {
        case Some(p) =>
          ir.Property(
            name = ir.Name(name = ISZ("Deployment_Properties::Actual_Processor_Binding"), pos = None()),
            propertyValues = ISZ(ir.ReferenceProp(ir.Name(name = p.path, pos = None()))),
            appliesTo = ISZ())
        case _ =>
          reporter.error(None(), TestControllerInjector.toolName,
            "Cannot inject the test controller: no process is bound to an actual processor")
          return None()
      }
    }

    // Assign the next available domain so the linter's "Processes must be assigned to a
    // scheduling domain" check passes.  The controller takes no timeslice in the MCS
    // schedule, so this domain id is never dispatched -- it exists only to satisfy the
    // linter and to give CComponentPlugin_MCS a channel id to allocate.
    val domainProp = ir.Property(
      name = ir.Name(name = ISZ("CASE_Scheduling::Domain"), pos = None()),
      propertyValues = ISZ(ir.UnitProp(value = symbolTable.computeMaxDomain().string, unit = None())),
      appliesTo = ISZ())

    val dispatchProtocolProp = ir.Property(
      name = ir.Name(name = ISZ("Thread_Properties::Dispatch_Protocol"), pos = None()),
      propertyValues = ISZ(ir.ValueProp("Periodic")),
      appliesTo = ISZ())

    val periodProp = ir.Property(
      name = ir.Name(name = ISZ("Timing_Properties::Period"), pos = None()),
      propertyValues = ISZ(ir.UnitProp(value = TestControllerInjector.defaultPeriodMs.string, unit = Some("ms"))),
      appliesTo = ISZ())

    val stackSizeProp = ir.Property(
      name = ir.Name(name = ISZ("Memory_Properties::Stack_Size"), pos = None()),
      propertyValues = ISZ(ir.UnitProp(value = TestControllerInjector.defaultStackSizeKiBytes.string, unit = Some("KiByte"))),
      appliesTo = ISZ())

    // Setting HAMR::Microkit_Language = "Rust" makes MicrokitUtil.isRusty return T for the
    // synthetic thread, which is what causes the Rust code generation plugins to emit a
    // crate for it (TestScheduler-design.md D3).
    val rustLangProp = ir.Property(
      name = ir.Name(name = ISZ("HAMR::Microkit_Language"), pos = None()),
      propertyValues = ISZ(ir.ValueProp("Rust")),
      appliesTo = ISZ())

    val processId = controllerProcessPath(controllerProcessPath.lastIndex)
    val threadId = controllerThreadPath(controllerThreadPath.lastIndex)

    val controllerThread = ir.Component(
      identifier = ir.Name(name = controllerThreadPath, pos = None()),
      category = ir.ComponentCategory.Thread,
      classifier = Some(ir.Classifier(s"${processId}::${threadId}")),
      features = ISZ(),
      subComponents = ISZ(),
      connections = ISZ(),
      connectionInstances = ISZ(),
      properties = ISZ(dispatchProtocolProp, periodProp, rustLangProp, stackSizeProp),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = "")

    val controllerProcess = ir.Component(
      identifier = ir.Name(name = controllerProcessPath, pos = None()),
      category = ir.ComponentCategory.Process,
      classifier = Some(ir.Classifier(s"${processId}_impl")),
      features = ISZ(),
      subComponents = ISZ(controllerThread),
      connections = ISZ(),
      connectionInstances = ISZ(),
      properties = ISZ(processorBindingProp, domainProp),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = "")

    return Some(model(components = ISZ(system(subComponents = system.subComponents :+ controllerProcess))))
  }
}
