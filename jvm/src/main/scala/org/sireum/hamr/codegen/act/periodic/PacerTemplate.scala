// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act.ast
import org.sireum.hamr.codegen.act.templates.CAmkESTemplate.DOMAIN_FIELD
import org.sireum.hamr.codegen.act.templates.EventDataQueueTemplate
import org.sireum.hamr.codegen.act.util._
import org.sireum.hamr.codegen.common.properties.{CaseSchedulingProperties, OsateProperties}
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol

object PacerTemplate {

  val PACER_COMPONENT_TYPE: String = "Pacer"
  val PACER_IDENTIFIER: String = "pacer"

  val PACER_PERIOD_TYPE: String = "Period"
  val PACER_PERIOD_EMIT_IDENTIFIER: String = "period"

  val PACER_PERIOD_VM_TYPE: String = "Period_VM"
  val PACER_PERIOD_VM_EMIT_IDENTIFIER: String = "period_to"

  val PACER_DOMAIN: Z = z"1" // pacer has to be in domain 1

  val PACER_TICK_TOCK_TYPE: String = "TickTock"
  val PACER_TICK_IDENTIFIER: String = "tick"
  val PACER_TOCK_IDENTIFIER: String = "tock"
  val PACER_TICK_COUNT_IDENTIFIER: String = "tickCount"

  def pacerComponentTickIdentifier(): String = {
    return PACER_TICK_IDENTIFIER
  }

  def pacerComponentTockIdentifier(): String = {
    return PACER_TOCK_IDENTIFIER
  }

  def pacerPeriodEmitIdentifier(): String = {
    return PACER_PERIOD_EMIT_IDENTIFIER
  }

  def pacerComponentDir(): String = {
    return s"${Util.DIR_COMPONENTS}/${PACER_COMPONENT_TYPE}"
  }

  def pacerImport(): String = {
    return st""""${pacerComponentDir()}/${PACER_COMPONENT_TYPE}.camkes"""".render
  }

  def pacerGlueCodeFilename(): String = {
    return Util.genCImplFilename(PACER_COMPONENT_TYPE)
  }

  def pacerGlueCodePath(): String = {
    return s"${pacerComponentDir()}/${Util.DIR_SRC}/${pacerGlueCodeFilename()}"
  }

  def periodicEntrypointMethodName(classifier: String): String = {
    return Util.brand(s"entrypoint_period_${classifier}")
  }

  def callPeriodicComputEntrypoint(classifier: String, handler: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val dummyVarName = Util.brand("dummy")
    return (
      st"""{
          |  int64_t ${dummyVarName} = 0;
          |  ${methodName}(&${dummyVarName});
          |}""")
  }

  def wrapPeriodicComputeEntrypoint(classifier: String, userEntrypoint: String): ST = {
    val methodName = periodicEntrypointMethodName(classifier)
    val IN_ARG_VAR: String = "in_arg"

    return (
      st"""void ${methodName}(int64_t *${IN_ARG_VAR}) {
          |  ${userEntrypoint}((int64_t *) ${IN_ARG_VAR});
          |}""")
  }

  def pacerGlueCode(includes: ISZ[String],
                    methods: ISZ[ST],
                    loopEntries: ISZ[ST]): ST = {
    val _includes: ISZ[ST] = includes.map(m => st"#include ${m}")
    val _entries: ISZ[ST] = loopEntries.map(m => st"${m};")
    val ret: ST =
      st"""// Copyright 2019 Adventium Labs
          |
          |#include <camkes.h>
          |#include <stdio.h>
          |#include <sel4/sel4.h>
          |${(_includes, "\n")}
          |
          |${(methods, "\n\n")}
          |
          |int run(void) {
          |
          |  ${pacerDataportQueueElemType()} ${PACER_TICK_COUNT_IDENTIFIER} = 0;
          |
          |  while (1) {
          |    //printf("%s: Period tick %d\n", get_instance_name(), ${PACER_TICK_COUNT_IDENTIFIER});
          |
          |    ${PACER_TICK_COUNT_IDENTIFIER}++;
          |
          |    ${PACER_TICK_IDENTIFIER}_emit();
          |
          |    ${(_entries, "\n")}
          |
          |    ${PACER_TOCK_IDENTIFIER}_wait();
          |  }
          |
          |  return 0;
          |}"""
    return ret
  }


  def domainConfiguration(identifier: String, domain: Z): ast.Configuration = {
    return ast.GenericConfiguration(s"${identifier}.${DOMAIN_FIELD} = ${domain};", ISZ())
  }

  def pacerScheduleEntry(domain: Z,
                         length: Z,
                         comment: Option[ST]): ST = {
    return st"{ .domain = ${domain}, .length = ${length} }, ${comment}"
  }

  def pacerExampleSchedule(clock_period: Z,
                           frame_period: Z,
                           threadProperties: ISZ[ST],
                           entries: ISZ[ST],
                           usesPacerComponent: B): ST = {
    val pacerOpt: Option[ST] = if (usesPacerComponent) Some(st"Pacer runs at highest rate and should always be in domain 1") else None()
    val ret: ST =
      st"""#include <config.h>
          |#include <object/structures.h>
          |#include <model/statedata.h>
          |
          |// this file will not be overwritten and is safe to edit
          |
          |/************************************************************
          |
          |   This is a kernel data structure containing an example schedule.
          |   The length is in seL4 ticks (${clock_period} ms).
          |   This schedule should be generated from the AADL model
          |   using execution time and data flow latency specifications.
          |
          |   ${pacerOpt}
          |
          |   Properties from AADL Model
          |   --------------------------
          |
          |     Timing_Properties::Clock_Period : ${clock_period} ms
          |     Timing_Properties::Frame_Period : ${frame_period} ms
          |
          |     ${(threadProperties, "\n\n")}
          |
          | *********************************************************/
          |
          |const dschedule_t ksDomSchedule[] = {
          |  ${(entries, "\n")}
          |};
          |
          |const word_t ksDomScheduleLength = sizeof(ksDomSchedule) / sizeof(dschedule_t);
          |"""
    return ret
  }

  def pacerScheduleThreadPropertyComment(componentId: String,
                                         componentType: String,
                                         domain: Z,
                                         dispatchProtocol: Dispatch_Protocol.Type,
                                         computeExecutionTime: String,
                                         period: Option[Z]): ST = {
    val title = s"$componentId : $componentType"
    var dashes: String = s""
    for (x <- 0 until title.size) {
      dashes = s"${dashes}-"
    }

    val _period: Option[ST] =
      period match {
        case Some(p) => Some(st"${OsateProperties.TIMING_PROPERTIES__PERIOD} : ${p} ms")
        case _ => None()
      }

    val ret: ST =
      st"""${title}
          |${dashes}
          |
          |  ${CaseSchedulingProperties.DOMAIN} : ${domain}
          |  ${OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL} : ${dispatchProtocol}
          |  ${OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME} : ${computeExecutionTime}
          |  ${_period}"""
    return ret
  }

  def pacerWait(): ST = {
    return st"${pacerClientNotificationIdentifier()}_wait();"
  }

  def pacerClientNotificationIdentifier(): String = {
    return Util.brand("pacer_notification")
  }

  def pacerDataportFilename(): String = {
    return Util.getEventData_SB_QueueHeaderFileName(
      PacerTemplate.pacerDataportQueueElemType(),
      PacerTemplate.pacerDataportQueueSize()
    )
  }

  def pacerDataportFilenameForIncludes(): String = {
    return s"<${pacerDataportFilename()}>"
  }

  def pacerDataportQueueElemType(): String = {
    return "int8_t"
  }

  def pacerDataportQueueSize(): Z = {
    return z"1"
  }

  def pacerDataportQueueType(): String = {
    return Util.getEventDataSBQueueTypeName(pacerDataportQueueElemType(), pacerDataportQueueSize())
  }

  def pacerVM_PacerPeriodPrefix(vmID: String): String = {
    return s"${PACER_PERIOD_VM_EMIT_IDENTIFIER}_${vmID}"
  }

  def pacerVM_CaseConnectorDataportIdentifier(): String = {
    return s"${PACER_PERIOD_VM_EMIT_IDENTIFIER}_vm_queue"
  }

  def pacerVM_PacerPeriodDataportIdentifier(vmID: String): String = {
    return s"${pacerVM_PacerPeriodPrefix(vmID)}_queue"
  }

  def pacerVM_PacerPeriodEmitsIdentifier(vmID: String): String = {
    return s"${pacerVM_PacerPeriodPrefix(vmID)}_notification"
  }

  def pacerVM_PacerEmitPeriodToVMMethodName(vmID: String): String = {
    return s"${pacerVM_PacerPeriodEmitsIdentifier(vmID)}_emit"
  }

  def pacerVM_PacerSendPeriodToVmMethodName(vmID: String): String = {
    val portId = pacerVM_PacerPeriodPrefix(vmID)
    return s"send_${portId}"
  }

  def pacerVM_ClientPeriodNotificationIdentifier(): String = {
    return Util.brand("pacer_period_notification")
  }

  def pacerVM_ClientPeriodDataportIdentifier(): String = {
    return Util.brand("pacer_period_queue")
  }

  def pacerVM_PacerGcInitMethodEntry_Case_Connector(dataportName: String,
                                                    queueElementTypeName: String,
                                                    queueSize: Z): ST = {
    val queueInitMethodName = EventDataQueueTemplate.getQueueInitMethodName(queueElementTypeName, queueSize)
    return st"${queueInitMethodName}(${dataportName});"
  }

  def pacerVM_PacerGcInitMethodEntry(vmProcessId: String,
                                     queueElementTypeName: String,
                                     queueSize: Z): ST = {
    val queueInitMethodName = EventDataQueueTemplate.getQueueInitMethodName(queueElementTypeName, queueSize)
    return st"${queueInitMethodName}(${pacerVM_PacerPeriodDataportIdentifier(vmProcessId)});"
  }

  def pacerVM_PacerGcInitMethod(entries: ISZ[ST]): ST = {
    val ret: ST =
      st"""void pre_init(void) {
          |  ${(entries, "\n")}
          |}"""
    return ret
  }

  def pacerVM_PacerGCSendPeriod_Case_Connector(emitMethodName: String,
                                               dataportName: String,
                                               payloadName: String,
                                               queueElementTypeName: String,
                                               queueSize: Z): ST = {
    val enqueueMethodName = EventDataQueueTemplate.getQueueEnqueueMethodName(queueElementTypeName, queueSize)

    val ret: ST =
      st"""${enqueueMethodName}(${dataportName}, &${payloadName});
          |${emitMethodName}();"""
    return ret
  }

  def pacerVM_PacerGcSendPeriodMethod_Case_Connector(methodName: String,
                                                     dataportName: String,
                                                     queueElementTypeName: String,
                                                     queueSize: Z): ST = {
    val enqueueMethodName = EventDataQueueTemplate.getQueueEnqueueMethodName(queueElementTypeName, queueSize)

    val ret: ST =
      st"""void ${methodName}(${pacerDataportQueueElemType()} *data) {
          |  ${enqueueMethodName}(${dataportName}, data);
          |                      |  ${dataportName}_emit_underlying();
          |}"""
    return ret
  }

  def pacerVM_PacerGcSendPeriodMethod(vmID: String,
                                      queueElementTypeName: String,
                                      queueSize: Z,
                                      notificationName: String): ST = {
    val enqueueMethodName = EventDataQueueTemplate.getQueueEnqueueMethodName(queueElementTypeName, queueSize)

    val ret: ST =
      st"""void ${pacerVM_PacerSendPeriodToVmMethodName(vmID)}(${pacerDataportQueueElemType()} *data) {
          |  ${enqueueMethodName}(${pacerVM_PacerPeriodDataportIdentifier(vmID)}, data);
          |  ${notificationName}();
          |}"""
    return ret
  }


  def settings_cmake_entries(numDomains: Z): ST = {
    val ret: ST =
      st"""set(KernelDomainSchedule "$${CMAKE_CURRENT_LIST_DIR}/kernel/domain_schedule.c" CACHE INTERNAL "")
          |set(KernelNumDomains ${numDomains} CACHE STRING "" FORCE)
          |"""
    return ret
  }
}