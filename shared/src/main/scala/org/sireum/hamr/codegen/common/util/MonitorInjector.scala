// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlPortConnection, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object MonitorInjector {
  val monitorProcessId: String = "monitor_process"
  val monitorThreadId: String = "monitor_thread"

  val toolName: String = "Monitor Injector"

  /** Injects a synthetic monitor process/thread into the AIR model.  For each
    * unique source port in the model the monitor receives an incoming port of the
    * same category and classifier.  Fan-out connections sharing the same source
    * port produce only one monitor port.  The caller is responsible for
    * re-running ModelUtil.resolve on the returned model to obtain an updated
    * SymbolTable.
    */
  def inject(model: ir.Aadl, symbolTable: SymbolTable, reporter: Reporter): ir.Aadl = {
    // VM components cannot participate in runtime monitoring because the monitor's
    // fan-out connections would mix native and VM endpoints, which is unsupported.
    for (t <- symbolTable.getThreads()) {
      if (t.toVirtualMachine(symbolTable)) {
        reporter.error(t.component.identifier.pos, toolName,
          st"Runtime monitoring is not supported for models containing VM components (found VM thread '${t.identifier}'). Disable --runtime-monitoring or remove VM components.".render)
      }
    }
    if (reporter.hasError) {
      return model
    }

    val system = model.components(0)
    val systemPath: ISZ[String] = system.identifier.name

    val monitorProcessPath: ISZ[String] = systemPath :+ monitorProcessId
    val monitorThreadPath: ISZ[String] = monitorProcessPath :+ monitorThreadId

    var monitorThreadFeatures: ISZ[ir.Feature] = ISZ()
    var monitorProcessFeatures: ISZ[ir.Feature] = ISZ()
    var monitorProcessConnections: ISZ[ir.Connection] = ISZ()
    var systemFanOutConnections: ISZ[ir.Connection] = ISZ()
    var systemConnectionInstances: ISZ[ir.ConnectionInstance] = ISZ()

    // Deduplicate by source feature path: fan-out connections share the same
    // source port and should produce only one monitor port + one fan-out connection.
    var seenSrcFeaturePaths: Set[ISZ[String]] = Set.empty

    for (conn <- symbolTable.aadlConnections) {
      conn match {
        case pc: AadlPortConnection =>
          val srcFeaturePath = pc.srcFeature.path
          if (!seenSrcFeaturePaths.contains(srcFeaturePath)) {
            seenSrcFeaturePaths = seenSrcFeaturePaths + srcFeaturePath

            val srcThread = pc.srcComponent.asInstanceOf[AadlThread]
            val srcProcess = srcThread.getParent(symbolTable)
            val portName: String = CommonUtil.getLastName(pc.srcFeature.feature.identifier)
            val monitorPortName: String = s"${srcProcess.identifier}_${srcThread.identifier}_${portName}"

            val srcFeatureEnd = pc.srcFeature.feature.asInstanceOf[ir.FeatureEnd]

            // Monitor thread incoming port (mirrors the source thread's outgoing port type)
            val monitorThreadPortPath: ISZ[String] = monitorThreadPath :+ monitorPortName
            val monitorThreadPort = ir.FeatureEnd(
              identifier = ir.Name(name = monitorThreadPortPath, pos = None()),
              direction = ir.Direction.In,
              category = srcFeatureEnd.category,
              classifier = srcFeatureEnd.classifier,
              properties = ISZ(),
              uriFrag = "")
            monitorThreadFeatures = monitorThreadFeatures :+ monitorThreadPort

            // Monitor process boundary incoming port
            val monitorProcessPortPath: ISZ[String] = monitorProcessPath :+ monitorPortName
            val monitorProcessPort = ir.FeatureEnd(
              identifier = ir.Name(name = monitorProcessPortPath, pos = None()),
              direction = ir.Direction.In,
              category = srcFeatureEnd.category,
              classifier = srcFeatureEnd.classifier,
              properties = ISZ(),
              uriFrag = "")
            monitorProcessFeatures = monitorProcessFeatures :+ monitorProcessPort

            // Process-level delegation: monitorProcess.port → monitorThread.port (In-to-In going down)
            monitorProcessConnections = monitorProcessConnections :+
              ir.Connection(
                name = ir.Name(name = monitorProcessPath :+ s"deleg_${monitorPortName}", pos = None()),
                src = ISZ(ir.EndPoint(
                  component = ir.Name(name = monitorProcessPath, pos = None()),
                  feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                  direction = Some(ir.Direction.In))),
                dst = ISZ(ir.EndPoint(
                  component = ir.Name(name = monitorThreadPath, pos = None()),
                  feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                  direction = Some(ir.Direction.In))),
                kind = ir.ConnectionKind.Port,
                isBiDirectional = F,
                connectionInstances = ISZ(),
                properties = ISZ(),
                uriFrag = "")

            // System-level fan-out: srcProcess.outPort → monitorProcess.port
            val srcProcessPortPath: ISZ[String] = srcProcess.path :+ portName
            val systemFanOutConnName: ISZ[String] = systemPath :+ s"mon_${monitorPortName}"
            systemFanOutConnections = systemFanOutConnections :+
              ir.Connection(
                name = ir.Name(name = systemFanOutConnName, pos = None()),
                src = ISZ(ir.EndPoint(
                  component = ir.Name(name = srcProcess.path, pos = None()),
                  feature = Some(ir.Name(name = srcProcessPortPath, pos = None())),
                  direction = Some(ir.Direction.Out))),
                dst = ISZ(ir.EndPoint(
                  component = ir.Name(name = monitorProcessPath, pos = None()),
                  feature = Some(ir.Name(name = monitorProcessPortPath, pos = None())),
                  direction = Some(ir.Direction.In))),
                kind = ir.ConnectionKind.Port,
                isBiDirectional = F,
                connectionInstances = ISZ(),
                properties = ISZ(),
                uriFrag = "")

            // ConnectionInstance directly linking the source thread's port to the monitor thread's
            // port.  The src endpoint is taken from the existing connection instance (already at thread
            // level).  The two connectionRefs trace the path through the synthetic connections above.
            val monitorDelegConnName: ISZ[String] = monitorProcessPath :+ s"deleg_${monitorPortName}"
            val connInstNameStr: String =
              st"${(pc.connectionInstance.src.feature.get.name, ".")} -> ${(monitorThreadPortPath, ".")}".render
            systemConnectionInstances = systemConnectionInstances :+
              ir.ConnectionInstance(
                name = ir.Name(name = ISZ(connInstNameStr), pos = None()),
                src = pc.connectionInstance.src,
                dst = ir.EndPoint(
                  component = ir.Name(name = monitorThreadPath, pos = None()),
                  feature = Some(ir.Name(name = monitorThreadPortPath, pos = None())),
                  direction = Some(ir.Direction.In)),
                kind = ir.ConnectionKind.Port,
                connectionRefs = ISZ(
                  ir.ConnectionReference(
                    name = ir.Name(name = systemFanOutConnName, pos = None()),
                    context = ir.Name(name = systemPath, pos = None()),
                    isParent = T),
                  ir.ConnectionReference(
                    name = ir.Name(name = monitorDelegConnName, pos = None()),
                    context = ir.Name(name = monitorProcessPath, pos = None()),
                    isParent = F)),
                properties = ISZ())
          }

        case _ =>
      }
    }

    if (monitorThreadFeatures.isEmpty) {
      reporter.warn(None(), toolName, "No port connections found; monitor will have no incoming ports")
    }

    // Inherit the processor binding from the first process so the linter's
    // "Processes must be bound to an actual processor" check passes.
    val processorBindingProp: ir.Property = {
      val firstProcess = symbolTable.getProcesses()(0)
      val processorPath = firstProcess.getBoundProcessor(symbolTable).get.path
      ir.Property(
        name = ir.Name(name = ISZ("Deployment_Properties::Actual_Processor_Binding"), pos = None()),
        propertyValues = ISZ(ir.ReferenceProp(ir.Name(name = processorPath, pos = None()))),
        appliesTo = ISZ())
    }

    // Assign the next available domain so the linter's "Processes must be
    // assigned to a scheduling domain" check passes.
    val domainProp = ir.Property(
      name = ir.Name(name = ISZ("CASE_Scheduling::Domain"), pos = None()),
      propertyValues = ISZ(ir.UnitProp(value = symbolTable.computeMaxDomain().string, unit = None())),
      appliesTo = ISZ())

    // Default period for the monitor thread. Runtime monitoring can be expensive
    // so 50ms is a conservative starting point.  A future config option should
    // allow the user to override this value.
    val defaultMonitorPeriodMs: Z = z"50"

    val dispatchProtocolProp = ir.Property(
      name = ir.Name(name = ISZ("Thread_Properties::Dispatch_Protocol"), pos = None()),
      propertyValues = ISZ(ir.ValueProp("Periodic")),
      appliesTo = ISZ())

    val periodProp = ir.Property(
      name = ir.Name(name = ISZ("Timing_Properties::Period"), pos = None()),
      propertyValues = ISZ(ir.UnitProp(value = defaultMonitorPeriodMs.string, unit = Some("ms"))),
      appliesTo = ISZ())

    // The monitor thread is always implemented in Rust so that verification tooling
    // (Verus) can be applied to it.  Setting HAMR::Microkit_Language = "Rust" makes
    // MicrokitUtil.isRusty return T for the synthetic thread, which causes all Rust
    // codegen plugins (CRustComponentPlugin, etc.) to pick it up automatically.
    val rustLangProp = ir.Property(
      name = ir.Name(name = ISZ("HAMR::Microkit_Language"), pos = None()),
      propertyValues = ISZ(ir.ValueProp("Rust")),
      appliesTo = ISZ())

    val monitorThread = ir.Component(
      identifier = ir.Name(name = monitorThreadPath, pos = None()),
      category = ir.ComponentCategory.Thread,
      classifier = Some(ir.Classifier(s"${monitorProcessId}::${monitorThreadId}")),
      features = monitorThreadFeatures,
      subComponents = ISZ(),
      connections = ISZ(),
      connectionInstances = ISZ(),
      properties = ISZ(dispatchProtocolProp, periodProp, rustLangProp),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = "")

    val monitorProcess = ir.Component(
      identifier = ir.Name(name = monitorProcessPath, pos = None()),
      category = ir.ComponentCategory.Process,
      classifier = Some(ir.Classifier(s"${monitorProcessId}::${monitorProcessId}_impl")),
      features = monitorProcessFeatures,
      subComponents = ISZ(monitorThread),
      connections = monitorProcessConnections,
      connectionInstances = ISZ(),
      properties = ISZ(processorBindingProp, domainProp),
      flows = ISZ(),
      modes = ISZ(),
      annexes = ISZ(),
      uriFrag = "")

    val updatedSystem = system(
      subComponents = system.subComponents :+ monitorProcess,
      connections = system.connections ++ systemFanOutConnections,
      connectionInstances = system.connectionInstances ++ systemConnectionInstances)

    return model(components = ISZ(updatedSystem))
  }
}
