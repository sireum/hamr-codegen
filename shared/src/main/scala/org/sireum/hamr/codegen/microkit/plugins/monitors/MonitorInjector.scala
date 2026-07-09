// #Sireum

package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, MapValue, Store}
import org.sireum.hamr.codegen.common.symbols.{AadlFeatureEvent, AadlPort, AadlPortConnection, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.util.{MemoryMap, MicrokitDomain, MicrokitUtil, ProtectionDomain}
import org.sireum.hamr.ir
import org.sireum.message.Reporter
import MonitorInjector._

object MonitorInjector {
  val toolName: String = "Monitor Injector"

  // Registry for the monitor's observation of UNCONNECTED input ports. An unconnected
  // input still gets its own shared memory region (created by CConnectionProviderPlugin's
  // unconnected-port handling and mapped read-only into the consumer); the monitor
  // observes it by mapping that SAME region. The monitor's observation port is itself
  // unconnected, so the generic unconnected-port machinery keys its MemoryMap to a region
  // named after the monitor's port -- a region that is never created (region creation is
  // suppressed for synthetic threads). This registry maps that bogus region name to the
  // consumer's existing region name so the monitor plugins can re-key the MemoryMap when
  // they assemble their system-description variants (see rekeyObservedUnconnectedInputMaps).
  val KEY_ObservedUnconnectedInputRegions: String = "KEY_MonitorObservedUnconnectedInputRegions"

  @strictpure def getObservedUnconnectedInputRegions(store: Store): Map[String, String] =
    store.getOrElse(KEY_ObservedUnconnectedInputRegions, MapValue[String, String](Map.empty)).asInstanceOf[MapValue[String, String]].map

  @strictpure def addObservedUnconnectedInputRegion(monitorPortRegionName: String, observedRegionName: String, store: Store): Store =
    store + KEY_ObservedUnconnectedInputRegions ~> MapValue(
      getObservedUnconnectedInputRegions(store) + monitorPortRegionName ~> observedRegionName)

  // Mirrors PortSharedMemoryRegion.name (util/SystemDescription.scala): the shared memory
  // region created for a port is named <port path joined by '_'>_<queueSize>_Memory_Region.
  @strictpure def portRegionName(portPath: IdPath, queueSize: Z): String =
    st"${(portPath, "_")}_${queueSize}_Memory_Region".render

  @strictpure def queueSizeOf(port: AadlPort): Z =
    port match {
      case e: AadlFeatureEvent => e.queueSize
      case _ => 1
    }

  @pure def rekeyObservedUnconnectedInputMap(m: MemoryMap, registry: Map[String, String]): MemoryMap = {
    registry.get(m.memoryRegion) match {
      case Some(observedRegion) => return m(memoryRegion = observedRegion)
      case _ => return m
    }
  }

  @pure def rekeyObservedUnconnectedInputDomain(d: MicrokitDomain, registry: Map[String, String]): MicrokitDomain = {
    d match {
      case pd: ProtectionDomain =>
        return pd(
          memMaps = for (m <- pd.memMaps) yield rekeyObservedUnconnectedInputMap(m, registry),
          children = for (c <- pd.children) yield rekeyObservedUnconnectedInputDomain(c, registry))
      case x => return x
    }
  }

  // Re-keys monitor MemoryMaps for observed unconnected inputs: replaces references to
  // the (never created) monitor-port-keyed region with the observed input's existing
  // region. No-op when no unconnected inputs were observed. Recurses into child PDs
  // (the monitor user PD is a child of its _MON companion).
  @pure def rekeyObservedUnconnectedInputMaps(pds: ISZ[ProtectionDomain], store: Store): ISZ[ProtectionDomain] = {
    val registry = getObservedUnconnectedInputRegions(store)
    if (registry.isEmpty) {
      return pds
    }
    return for (pd <- pds) yield rekeyObservedUnconnectedInputDomain(pd, registry).asInstanceOf[ProtectionDomain]
  }
}

@datatype class DefaultMonitorInjector extends MonitorInjector

@sig trait MonitorInjector {

  @strictpure def processName(root: ir.Component, prefix: String): ISZ[String] =
    root.identifier.name :+ s"${prefix}_process"

  @strictpure def threadName(root: ir.Component, prefix: String): ISZ[String] =
    processName(root, prefix) :+ s"${prefix}_thread"

  /** Injects a synthetic monitor process/thread into the AIR model.  For each
    * unique source port in the model the monitor receives an incoming port of the
    * same category and classifier.  Fan-out connections sharing the same source
    * port produce only one monitor port.  The caller is responsible for
    * re-running ModelUtil.resolve on the returned model to obtain an updated
    * SymbolTable.
    *
    * @param isMCS when true, also injects the sched_state and sched_schedule data
    *              ports along with the hamr::SchedState and hamr::Schedule record
    *              types (plus their array sub-types) so that the MCS user-land
    *              scheduler's shared-memory broadcast regions are reflected in the
    *              AADL type system.
    */
  def inject(model: ir.Aadl,
             monitorProcessPath: IdPath,
             monitorThreadPath: IdPath,
             symbolTable: SymbolTable, store: Store, reporter: Reporter): (Option[ir.Aadl], Store) = {
    var localStore = store

    // VM components cannot participate in runtime monitoring because the monitor's
    // fan-out connections would mix native and VM endpoints, which is unsupported.
    for (t <- symbolTable.getThreads()) {
      if (t.toVirtualMachine(symbolTable)) {
        reporter.error(t.component.identifier.pos, toolName,
          st"Runtime monitoring is not supported for models containing VM components (found VM thread '${t.identifier}'). Disable --runtime-monitoring or remove VM components.".render)
      }
    }
    if (reporter.hasError) {
      return (None(), localStore)
    }

    val system = model.components(0)
    val systemPath: ISZ[String] = system.identifier.name

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
            val monitorPortName: String = s"${MicrokitUtil.getComponentIdPath(srcThread)}_${portName}"

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

    // Observe UNCONNECTED input ports: an input with no incoming connection still has its
    // own shared memory region (created by CConnectionProviderPlugin's unconnected-port
    // handling and mapped read-only into the consuming thread), so the monitor can retrieve
    // exactly what the consumer sees by mapping that same region. Mirror the fan-out
    // observation's naming (<threadId>_<portId>) but leave the monitor port unconnected --
    // there is no producer to fan out from; the unconnected-port machinery generates its
    // C/Rust getters, and the monitor plugins later re-key the resulting MemoryMap to the
    // consumer's existing region via the registry recorded here (see
    // rekeyObservedUnconnectedInputMaps). The observed port's Queue_Size property is copied
    // so the monitor's queue layout matches the region's actual layout.
    for (t <- symbolTable.getThreads() if !StoreUtil.isSynthetic(t.path, localStore)) {
      for (p <- t.getPorts()
           if p.direction == ir.Direction.In &&
             !symbolTable.inConnections.contains(p.path) &&
             !StoreUtil.isSynthetic(p.path, localStore)) {

        val monitorPortName: String = s"${MicrokitUtil.getComponentIdPath(t)}_${p.identifier}"
        val monitorThreadPortPath: ISZ[String] = monitorThreadPath :+ monitorPortName
        val observedFeatureEnd: ir.FeatureEnd = p.feature

        val queueSizeProps: ISZ[ir.Property] = observedFeatureEnd.properties.filter((pr: ir.Property) =>
          pr.name.name.nonEmpty && ops.StringOps(pr.name.name(pr.name.name.lastIndex)).endsWith("Queue_Size"))

        monitorThreadFeatures = monitorThreadFeatures :+ ir.FeatureEnd(
          identifier = ir.Name(name = monitorThreadPortPath, pos = None()),
          direction = ir.Direction.In,
          category = observedFeatureEnd.category,
          classifier = observedFeatureEnd.classifier,
          properties = queueSizeProps,
          uriFrag = "")

        val queueSize = queueSizeOf(p)
        localStore = addObservedUnconnectedInputRegion(
          monitorPortRegionName = portRegionName(monitorThreadPortPath, queueSize),
          observedRegionName = portRegionName(p.path, queueSize),
          store = localStore)
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

    val monitorProcessId = monitorProcessPath(monitorProcessPath.lastIndex)
    val monitorThreadId = monitorThreadPath(monitorThreadPath.lastIndex)

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
      classifier = Some(ir.Classifier(s"${monitorProcessId}_impl")),
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

    return (Some(model(components = ISZ(updatedSystem))), localStore)
  }
}
