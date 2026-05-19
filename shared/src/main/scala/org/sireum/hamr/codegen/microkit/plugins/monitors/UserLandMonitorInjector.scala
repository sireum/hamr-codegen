// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.monitors

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, Store}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import UserLandMonitorInjector._
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin

object UserLandMonitorInjector {

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

  // Name of the incoming data port added to the monitor thread for schedule observation.
  val schedSchedulePortName: String = "sched_schedule"

  // Maximum number of timeslice slots in a schedule.  A thread may appear in
  // multiple slots per frame period, so this can be larger than the number of
  // partitions.  The struct is copied by value in the sb_queue API so it must
  // also fit on a microkit PD stack (~4 KB).  At 128 slots the struct is
  // ~1.7 KB (13*128 + 4).
  val maxScheduleSlots: Z = 128
}

@datatype class UserLandMonitorInjector extends MonitorInjector {

  override def inject(model: Aadl,
                      monitorProcessPath: IdPath,
                      monitorThreadPath: IdPath,
                      symbolTable: SymbolTable, store: Store, reporter: Reporter): (Option[Aadl], Store) = {
    super.inject(model, monitorProcessPath, monitorThreadPath, symbolTable, store, reporter) match {
      case (Some(updatedModel), updatedStore) => {

        // get the injected monitor process
        val system = updatedModel.components(0)
        val monitorProcess = system.subComponents(system.subComponents.lastIndex)
        val monitorThread = monitorProcess.subComponents(0)

        var monitorThreadFeatures: ISZ[ir.Feature] = ISZ()
        var monitorProcessFeatures: ISZ[ir.Feature] = ISZ()
        var monitorProcessConnections: ISZ[ir.Connection] = ISZ()

        // Inject the synthetic sched_state incoming data port only for MCS user-land scheduling.
        // The domain scheduler does not use shared-memory broadcast, so the port and its type
        // are irrelevant there and would produce spurious memory regions in the system XML.
        // The port is intentionally left unconnected here; wiring to the shared memory region
        // is a separate step.
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

        val updatedMonitorThread = monitorThread(
          features = monitorThread.features ++ monitorThreadFeatures
        )

        val updatedMonitorProcess = monitorProcess(
          features = monitorProcess.features ++ monitorProcessFeatures,
          subComponents = ISZ(updatedMonitorThread),
          connections = monitorProcess.connections ++ monitorProcessConnections
        )

        val updatedSystem = system(subComponents = ops.ISZOps(system.subComponents).dropRight(1) :+ updatedMonitorProcess)

        val (newDataComponents, finalsStore) = injectMCSTypes(model.dataComponents, updatedStore)

        return (Some(updatedModel(components = ISZ(updatedSystem), dataComponents = newDataComponents)), finalsStore)
      }
      case _ => return (None(), store)
    }
  }

  /** Injects the synthetic hamr::SchedState and hamr::Schedule record types
    * (plus their array sub-types and required base types) into the given
    * dataComponents list.
    */
  def injectMCSTypes(dataComponents: ISZ[ir.Component], store: Store): (ISZ[ir.Component], Store) = {
    val u32Classifier: String = "Base_Types::Unsigned_32"

    // Build the synthetic hamr::SchedState record type.  Its Unsigned_32 fields mirror
    // the sched_state_t struct in scheduler_config.h.  The type is added to
    // model.dataComponents so that TypeResolver builds it into the AadlTypes map and the
    // Rust/C type generators produce corresponding struct definitions.
    var newDataComponents: ISZ[ir.Component] = dataComponents

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
        }) || ops.ISZOps(dataComponents).exists(
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
    // dimension = maxScheduleSlots, allowing threads to appear in multiple
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

    // Register the top-level scheduler type classifiers as force-touched so
    // that downstream codegen stages generate the necessary C artifacts even
    // when no thread port references these types.  Normally, type artifacts
    // are driven by port connections: getAllTouchedTypes walks thread ports to
    // discover which types need struct definitions (sb_aadl_types.h), and
    // CConnectionProviderPlugin creates queue wrapper files (sb_queue_*.h/.c)
    // from the TypeApiContributions of each connection.  When runtime
    // monitoring is disabled there is no monitor thread whose ports would
    // "touch" the scheduler types, yet the MCS scheduler.c still references
    // their queue wrappers.  By adding the classifiers here, getAllTouchedTypes
    // includes them in the ordered type dependency list (producing the struct
    // definitions), and CConnectionProviderPlugin generates the queue files.
    // Transitive dependencies (array sub-types and base types) are resolved
    // automatically by getAllTouchedTypes' recursive add().
    val updatedStore = CTypePlugin.addToForceTouchedTypes(
      ISZ(schedStateClassifier, schedScheduleClassifier), store)
    return (newDataComponents, updatedStore)
  }
}
