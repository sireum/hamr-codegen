// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlPortConnection, AadlThread, SymbolTable}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object MonitorInjector {
  val monitorProcessId: String = "monitor_process"
  val monitorThreadId: String = "monitor_thread"

  val toolName: String = "Monitor Injector"

  // Classifier for the synthetic AADL record type that mirrors sched_state_t in scheduler_config.h.
  val schedStateClassifier: String = "hamr::SchedState"

  // Name of the incoming data port added to the monitor thread for schedule-state observation.
  val schedStatePortName: String = "sched_state"

  // Classifier for the synthetic AADL record type that mirrors user_schedule_t.
  val schedScheduleClassifier: String = "hamr::Schedule"

  // Classifiers for the array sub-types of hamr::Schedule.
  val schedTimeslicesClassifier: String = "hamr::ScheduleTimeslices"
  val schedChannelsClassifier: String = "hamr::ScheduleChannels"
  val schedUserPartitionsClassifier: String = "hamr::ScheduleUserPartitions"

  // Maximum number of timeslice slots in a schedule.  A thread may appear in
  // multiple slots per frame period, so this can be larger than the number of
  // partitions.  The struct is copied by value in the sb_queue API so it must
  // also fit on a microkit PD stack (~4 KB).  At 128 slots the struct is
  // ~1.7 KB (13*128 + 4).
  val maxScheduleSlots: Z = 128

  // Name of the incoming data port added to the monitor thread for schedule observation.
  val schedSchedulePortName: String = "sched_schedule"

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
  def inject(model: ir.Aadl, symbolTable: SymbolTable, isMCS: B, reporter: Reporter): ir.Aadl = {
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

    // Inject the synthetic sched_state incoming data port only for MCS user-land scheduling.
    // The domain scheduler does not use shared-memory broadcast, so the port and its type
    // are irrelevant there and would produce spurious memory regions in the system XML.
    // The port is intentionally left unconnected here; wiring to the shared memory region
    // is a separate step.
    if (isMCS) {
      val monitorSchedStateThreadPortPath: ISZ[String] = monitorThreadPath :+ schedStatePortName
      val monitorSchedStateProcessPortPath: ISZ[String] = monitorProcessPath :+ schedStatePortName

      monitorThreadFeatures = monitorThreadFeatures :+
        ir.FeatureEnd(
          identifier = ir.Name(name = monitorSchedStateThreadPortPath, pos = None()),
          direction = ir.Direction.In,
          category = ir.FeatureCategory.DataPort,
          classifier = Some(ir.Classifier(schedStateClassifier)),
          properties = ISZ(),
          uriFrag = "")

      monitorProcessFeatures = monitorProcessFeatures :+
        ir.FeatureEnd(
          identifier = ir.Name(name = monitorSchedStateProcessPortPath, pos = None()),
          direction = ir.Direction.In,
          category = ir.FeatureCategory.DataPort,
          classifier = Some(ir.Classifier(schedStateClassifier)),
          properties = ISZ(),
          uriFrag = "")

      monitorProcessConnections = monitorProcessConnections :+
        ir.Connection(
          name = ir.Name(name = monitorProcessPath :+ s"deleg_${schedStatePortName}", pos = None()),
          src = ISZ(ir.EndPoint(
            component = ir.Name(name = monitorProcessPath, pos = None()),
            feature = Some(ir.Name(name = monitorSchedStateProcessPortPath, pos = None())),
            direction = Some(ir.Direction.In))),
          dst = ISZ(ir.EndPoint(
            component = ir.Name(name = monitorThreadPath, pos = None()),
            feature = Some(ir.Name(name = monitorSchedStateThreadPortPath, pos = None())),
            direction = Some(ir.Direction.In))),
          kind = ir.ConnectionKind.Port,
          isBiDirectional = F,
          connectionInstances = ISZ(),
          properties = ISZ(),
          uriFrag = "")

      // Inject the sched_schedule incoming data port so the monitor can observe the
      // full schedule (all timeslice channels, durations, and user-partition flags).
      val monitorSchedScheduleThreadPortPath: ISZ[String] = monitorThreadPath :+ schedSchedulePortName
      val monitorSchedScheduleProcessPortPath: ISZ[String] = monitorProcessPath :+ schedSchedulePortName

      monitorThreadFeatures = monitorThreadFeatures :+
        ir.FeatureEnd(
          identifier = ir.Name(name = monitorSchedScheduleThreadPortPath, pos = None()),
          direction = ir.Direction.In,
          category = ir.FeatureCategory.DataPort,
          classifier = Some(ir.Classifier(schedScheduleClassifier)),
          properties = ISZ(),
          uriFrag = "")

      monitorProcessFeatures = monitorProcessFeatures :+
        ir.FeatureEnd(
          identifier = ir.Name(name = monitorSchedScheduleProcessPortPath, pos = None()),
          direction = ir.Direction.In,
          category = ir.FeatureCategory.DataPort,
          classifier = Some(ir.Classifier(schedScheduleClassifier)),
          properties = ISZ(),
          uriFrag = "")

      monitorProcessConnections = monitorProcessConnections :+
        ir.Connection(
          name = ir.Name(name = monitorProcessPath :+ s"deleg_${schedSchedulePortName}", pos = None()),
          src = ISZ(ir.EndPoint(
            component = ir.Name(name = monitorProcessPath, pos = None()),
            feature = Some(ir.Name(name = monitorSchedScheduleProcessPortPath, pos = None())),
            direction = Some(ir.Direction.In))),
          dst = ISZ(ir.EndPoint(
            component = ir.Name(name = monitorThreadPath, pos = None()),
            feature = Some(ir.Name(name = monitorSchedScheduleThreadPortPath, pos = None())),
            direction = Some(ir.Direction.In))),
          kind = ir.ConnectionKind.Port,
          isBiDirectional = F,
          connectionInstances = ISZ(),
          properties = ISZ(),
          uriFrag = "")
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

    // For MCS, build the synthetic hamr::SchedState record type.  Its two Unsigned_32 fields mirror
    // the sched_state_t struct in scheduler_config.h.  The type is added to model.dataComponents
    // so that TypeResolver builds it into the AadlTypes map and the Rust/C type generators
    // produce corresponding struct definitions and getters for the monitor thread's sched_state port.
    var newDataComponents: ISZ[ir.Component] = model.dataComponents
    if (isMCS) {
      val u32Classifier: String = "Base_Types::Unsigned_32"

      def makeFieldComp(fieldName: String): ir.Component = {
        return ir.Component(
          identifier = ir.Name(name = ISZ(fieldName), pos = None()),
          category = ir.ComponentCategory.Data,
          classifier = Some(ir.Classifier(u32Classifier)),
          features = ISZ(), subComponents = ISZ(), connections = ISZ(),
          connectionInstances = ISZ(), properties = ISZ(),
          flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")
      }

      val schedStateDataComp = ir.Component(
        identifier = ir.Name(name = ISZ(), pos = None()),
        category = ir.ComponentCategory.Data,
        classifier = Some(ir.Classifier(schedStateClassifier)),
        features = ISZ(),
        subComponents = ISZ(makeFieldComp("last_yielded_ch"), makeFieldComp("next_dispatch_ch"), makeFieldComp("current_timeslice")),
        connections = ISZ(), connectionInstances = ISZ(), properties = ISZ(),
        flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")

      // Ensure base types used by SchedState and Schedule fields are registered in
      // model.dataComponents so that SymbolResolver can look them up.
      def ensureBaseType(classifier: String): Unit = {
        val alreadyPresent: B = ops.ISZOps(newDataComponents).exists(
          (c: ir.Component) => c.classifier match {
            case Some(cl) => cl.name == classifier
            case _ => F
          }) || ops.ISZOps(model.dataComponents).exists(
          (c: ir.Component) => c.classifier match {
            case Some(cl) => cl.name == classifier
            case _ => F
          })
        if (!alreadyPresent) {
          newDataComponents = newDataComponents :+ ir.Component(
            identifier = ir.Name(name = ISZ(), pos = None()),
            category = ir.ComponentCategory.Data,
            classifier = Some(ir.Classifier(classifier)),
            features = ISZ(), subComponents = ISZ(), connections = ISZ(),
            connectionInstances = ISZ(), properties = ISZ(),
            flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")
        }
      }

      ensureBaseType(u32Classifier)
      newDataComponents = newDataComponents :+ schedStateDataComp

      // Build the synthetic hamr::Schedule record type and its array sub-types.
      // The arrays model the fields of user_schedule_t (scheduler_config.h) with
      // dimension = maxScheduleSlots (256), allowing threads to appear in multiple
      // timeslice slots per frame period.

      val u64Classifier: String = "Base_Types::Unsigned_64"
      val boolClassifier: String = "Base_Types::Boolean"

      ensureBaseType(u64Classifier)
      ensureBaseType(boolClassifier)

      def makeArrayTypeComp(arrayClassifier: String, baseTypeClassifier: String,
                            dim: Z, elementBits: Z): ir.Component = {
        val bitSize: Z = dim * elementBits
        return ir.Component(
          identifier = ir.Name(name = ISZ(), pos = None()),
          category = ir.ComponentCategory.Data,
          classifier = Some(ir.Classifier(arrayClassifier)),
          features = ISZ(), subComponents = ISZ(), connections = ISZ(),
          connectionInstances = ISZ(),
          properties = ISZ(
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Data_Representation"), pos = None()),
              propertyValues = ISZ(ir.ValueProp("Array")),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Base_Type"), pos = None()),
              propertyValues = ISZ(ir.ClassifierProp(baseTypeClassifier)),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Dimension"), pos = None()),
              propertyValues = ISZ(ir.UnitProp(value = dim.string, unit = None())),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Memory_Properties::Data_Size"), pos = None()),
              propertyValues = ISZ(ir.UnitProp(value = bitSize.string, unit = None())),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("HAMR::Array_Size_Kind"), pos = None()),
              propertyValues = ISZ(ir.ValueProp("Fixed")),
              appliesTo = ISZ())),
          flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")
      }

      // Array types must appear before the record type that references them.
      newDataComponents = newDataComponents :+ makeArrayTypeComp(
        arrayClassifier = schedTimeslicesClassifier,
        baseTypeClassifier = u64Classifier,
        dim = maxScheduleSlots,
        elementBits = 64)

      newDataComponents = newDataComponents :+ makeArrayTypeComp(
        arrayClassifier = schedChannelsClassifier,
        baseTypeClassifier = u32Classifier,
        dim = maxScheduleSlots,
        elementBits = 32)

      newDataComponents = newDataComponents :+ makeArrayTypeComp(
        arrayClassifier = schedUserPartitionsClassifier,
        baseTypeClassifier = boolClassifier,
        dim = maxScheduleSlots,
        elementBits = 8)

      // Field sub-components of a record that reference array types must carry the
      // full array properties (Data_Representation, Base_Type, Dimension, Data_Size,
      // Array_Size_Kind) inline, because the TypeResolver processes each field
      // sub-component via processType and checks for isArrayType on its properties.
      def makeArrayFieldComp(fieldName: String, arrayClassifier: String,
                             baseTypeClassifier: String, dim: Z, elementBits: Z): ir.Component = {
        val bitSize: Z = dim * elementBits
        return ir.Component(
          identifier = ir.Name(name = ISZ(fieldName), pos = None()),
          category = ir.ComponentCategory.Data,
          classifier = Some(ir.Classifier(arrayClassifier)),
          features = ISZ(), subComponents = ISZ(), connections = ISZ(),
          connectionInstances = ISZ(),
          properties = ISZ(
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Data_Representation"), pos = None()),
              propertyValues = ISZ(ir.ValueProp("Array")),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Base_Type"), pos = None()),
              propertyValues = ISZ(ir.ClassifierProp(baseTypeClassifier)),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Data_Model::Dimension"), pos = None()),
              propertyValues = ISZ(ir.UnitProp(value = dim.string, unit = None())),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("Memory_Properties::Data_Size"), pos = None()),
              propertyValues = ISZ(ir.UnitProp(value = bitSize.string, unit = Some("bits"))),
              appliesTo = ISZ()),
            ir.Property(
              name = ir.Name(name = ISZ("HAMR::Array_Size_Kind"), pos = None()),
              propertyValues = ISZ(ir.ValueProp("Fixed")),
              appliesTo = ISZ())),
          flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")
      }

      val schedScheduleDataComp = ir.Component(
        identifier = ir.Name(name = ISZ(), pos = None()),
        category = ir.ComponentCategory.Data,
        classifier = Some(ir.Classifier(schedScheduleClassifier)),
        features = ISZ(),
        subComponents = ISZ(
          makeArrayFieldComp("timeslices", schedTimeslicesClassifier, u64Classifier, maxScheduleSlots, 64),
          makeArrayFieldComp("timeslice_ch", schedChannelsClassifier, u32Classifier, maxScheduleSlots, 32),
          makeArrayFieldComp("is_user_partition", schedUserPartitionsClassifier, boolClassifier, maxScheduleSlots, 8),
          makeFieldComp("num_timeslices")),
        connections = ISZ(), connectionInstances = ISZ(), properties = ISZ(),
        flows = ISZ(), modes = ISZ(), annexes = ISZ(), uriFrag = "")

      newDataComponents = newDataComponents :+ schedScheduleDataComp
    }

    return model(components = ISZ(updatedSystem), dataComponents = newDataComponents)
  }
}
