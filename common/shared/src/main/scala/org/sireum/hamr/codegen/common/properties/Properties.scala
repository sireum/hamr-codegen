// #Sireum

package org.sireum.hamr.codegen.common.properties

import org.sireum._

object OsateProperties {
  val DATA_MODEL__DATA_REPRESENTATION: String = "Data_Model::Data_Representation"
  val DATA_MODEL__DIMENSION: String = "Data_Model::Dimension"
  val DATA_MODEL__BASE_TYPE: String = "Data_Model::Base_Type"
  val DATA_MODEL__ENUMERATORS: String = "Data_Model::Enumerators"

  val THREAD_PROPERTIES__DISPATCH_PROTOCOL: String = "Thread_Properties::Dispatch_Protocol"
  val THREAD_PROPERTIES__DISPATCH_TRIGGER: String = "Thread_Properties::Dispatch_Trigger"
  val THREAD_PROPERTIES__PRIORITY: String =  "Thread_Properties::Priority"
  val THREAD_PROPERTIES__URGENCY: String =  "Thread_Properties::Urgency"

  val DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val COMMUNICATION_PROPERTIES__QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val MEMORY_PROPERTIES__STACK_SIZE: String = "Memory_Properties::Stack_Size"

  val PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Initialize_Entrypoint_Source_Text"
  val PROGRAMMING_PROPERTIES__SOURCE_TEXT: String = "Programming_Properties::Source_Text"
  val PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Compute_Entrypoint_Source_Text"

  val TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME: String = "Timing_Properties::Compute_Execution_Time"
  val TIMING_PROPERTIES__CLOCK_PERIOD: String = "Timing_Properties::Clock_Period"
  val TIMING_PROPERTIES__FRAME_PERIOD: String = "Timing_Properties::Frame_Period"
  val TIMING_PROPERTIES__PERIOD: String = "Timing_Properties::Period"
}

object HamrProperties {

  val HAMR__PLATFORM: String = "HAMR::Platform"
  val HAMR__HW: String = "HAMR::HW"

  val HAMR__DEFAULT_BIT_WIDTH: String = "HAMR::Default_Bit_Width"
  val HAMR__DEFAULT_MAX_SEQUENCE_SIZE: String = "HAMR::Default_Sequence_Size"
  val HAMR__MAX_STRING_SIZE: String = "HAMR::Max_String_Size"

  val HAMR__BIT_CODEC_SPEC: String = "HAMR::Bit_Codec_Spec"
  val HAMR__BIT_CODEC_ENCODED: String = "HAMR::Bit_Codec_Encoded"
  val HAMR__BIT_CODEC_MAX_SIZE: String = "HAMR::Bit_Codec_Max_Size"
  val HAMR__BIT_CODEC_RAW_CONNECTIONS: String = "HAMR::Bit_Codec_Raw_Connections"

  val HAMR__COMPONENT_TYPE: String = "HAMR::Component_Type"

}

object CaseSchedulingProperties {

  val DOMAIN: String = "CASE_Scheduling::Domain"
  val SCHEDULE_SOURCE_TEXT: String = "CASE_Scheduling::Schedule_Source_Text"

}

object CasePropertiesProperties {
  val PROP__CASE_PROPERTIES__COMPONENT_TYPE: String = "CASE_Properties::Component_Type"
}
