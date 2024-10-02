// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.templates.{CommentTemplate, StackFrameTemplate}

object SchedulerTemplate {
  def schedulers(packageName: String,
                 bridges: ISZ[String],
                 processorTimingProperties: ISZ[ST],
                 threadTimingProperties: ISZ[ST],
                 framePeriod: Z): ST = {
    var slots: ISZ[ST] = ISZ()
    var domainToBridgeMap: ISZ[ST] = ISZ()
    var threadNickNames: ISZ[ST] = ISZ()
    for(i <- 0 until bridges.size) {
      slots = slots :+ st"Schedule.Slot($i, maxExecutionTime)"
      domainToBridgeMap = domainToBridgeMap :+ st"/* domain $i */ Arch.${bridges(i)}.id"
      threadNickNames = threadNickNames :+ st"Arch.${bridges(i)}.name ~> Arch.${bridges(i)}.id"
    }

    val ret =
      st"""// #Sireum
          |package ${packageName}
          |
          |import org.sireum._
          |import art.Art
          |import art.scheduling.legacy.Legacy
          |import art.scheduling.roundrobin.RoundRobin
          |import art.scheduling.static.Schedule.{DSchedule, DScheduleSpec}
          |import art.scheduling.static._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@datatype class ProcessorTimingProperties(val clockPeriod: Option[Z],
          |                                          val framePeriod: Option[Z],
          |                                          val maxDomain: Option[Z],
          |                                          val slotTime: Option[Z])
          |
          |@datatype class ThreadTimingProperties(val domain: Option[Z],
          |                                       val computeExecutionTime: Option[(Z, Z)])
          |
          |object Schedulers {
          |
          |  val threadNickNames: Map[String, Art.BridgeId] = Map(
          |    ISZ(
          |      ${(threadNickNames, ",\n")})
          |  )
          |
          |  val revThreadNickNames: Map[Art.BridgeId, String] = Map.empty[Art.BridgeId, String] ++ (for (e <- threadNickNames.entries) yield e._2 ~> e._1)
          |
          |  ${(processorTimingProperties, "\n\n")}
          |
          |  ${(threadTimingProperties, "\n\n")}
          |
          |
          |  /**********************************************************************
          |   * Round Robin Scheduler
          |   *********************************************************************/
          |
          |  // roundRobinSchedule represents the component dispatch order
          |  val roundRobinSchedule: ISZ[Art.BridgeId] = {
          |    // convert IS[Art.BridgeId, art.Bridge] to an IS[Z, Art.BridgeId] to allow bridges to be dispatched
          |    // multiple times during a hyper-period
          |    var ret: ISZ[Art.BridgeId] = ISZ()
          |    for (e <- Arch.ad.components) {
          |      ret = ret :+ e.id
          |    }
          |    ret
          |  }
          |
          |  def getRoundRobinScheduler(schedule: Option[ISZ[Art.BridgeId]]): RoundRobin = {
          |    if (roundRobinSchedule.isEmpty) {} // line needed for transpiler; do not remove
          |    schedule match {
          |      case Some(s) => return RoundRobin(s)
          |      case _ => return RoundRobin(ScheduleProviderI.getRoundRobinOrder())
          |    }
          |  }
          |
          |  /**********************************************************************
          |   * Static Scheduler
          |   *********************************************************************/
          |
          |  val framePeriod: Z = ${framePeriod}
          |  val numComponents: Z = Arch.ad.components.size
          |  val maxExecutionTime: Z = numComponents / framePeriod
          |
          |  // defaultStaticSchedule represents the component dispatch order
          |  val defaultStaticSchedule: DScheduleSpec = DScheduleSpec(0, 0, DSchedule(ISZ(
          |    ${(slots, ",\n")}
          |  )))
          |
          |  val defaultDomainToBridgeIdMap: ISZ[Art.BridgeId] = ISZ(
          |    ${(domainToBridgeMap, ",\n")}
          |  )
          |
          |  def getStaticSchedulerH(userProvided: MOption[(DScheduleSpec, ISZ[Art.BridgeId], Map[String, Art.BridgeId], CommandProvider)]): StaticScheduler = {
          |    if (defaultStaticSchedule.schedule.slots.isEmpty && defaultDomainToBridgeIdMap.isEmpty && threadNickNames.isEmpty) {} // line needed for transpiler; do not remove
          |    userProvided match {
          |      case MSome((schedule_, domainToBridgeIdMap_, threadNickNames_, commandProvider)) =>
          |        return getStaticScheduler(schedule_, domainToBridgeIdMap_, threadNickNames_, commandProvider)
          |      case _ =>
          |        return getStaticScheduler(
          |          ScheduleProviderI.getStaticSchedule(),
          |          // TODO: get the following from extension so they can be customized via C
          |          defaultDomainToBridgeIdMap,
          |          threadNickNames,
          |          DefaultCommandProvider())
          |    }
          |  }
          |
          |  def getStaticScheduler(schedule: DScheduleSpec,
          |                         domainToBridgeIdMap: ISZ[Art.BridgeId],
          |                         threadNickNames: Map[String, Art.BridgeId],
          |                         commandProvider: CommandProvider): StaticScheduler = {
          |    return StaticScheduler(schedule, Arch.ad.components, domainToBridgeIdMap, threadNickNames,
          |      if (commandProvider.isInstanceOf[InfoCommandProvider])
          |        commandProvider.asInstanceOf[InfoCommandProvider].init(
          |          threadNickNames,
          |          schedule.schedule.slots.size,
          |          domainToBridgeIdMap
          |        )
          |      else commandProvider)
          |  }
          |
          |
          |  /**********************************************************************
          |   * Legacy Scheduler
          |   *********************************************************************/
          |
          |  def getLegacyScheduler(): Legacy = {
          |    return Legacy(Arch.ad.components)
          |  }
          |}
          |
          |// the purpose of this extension is to allow users to provide custom schedules
          |// at the C level after transpiling
          |@ext(name = "ScheduleProvider") object ScheduleProviderI {
          |  def getRoundRobinOrder(): ISZ[Art.BridgeId] = $$
          |
          |  def getStaticSchedule(): DScheduleSpec = $$
          |}"""
    return ret
  }

  def scheduleProvider(packageName: String): ST = {
    val ret: ST =
      st"""package ${packageName}
          |
          |import org.sireum._
          |import art.Art
          |import art.scheduling.static.Schedule.DScheduleSpec
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ScheduleProvider {
          |
          |  def getRoundRobinOrder(): ISZ[Art.BridgeId] = {
          |    return Schedulers.roundRobinSchedule
          |  }
          |
          |  def getStaticSchedule(): DScheduleSpec = {
          |    return Schedulers.defaultStaticSchedule
          |  }
          |}
          |"""
    return ret
  }

  def c_legacy(): ST = {
    val ret: ST =
      st"""#include <all.h>
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |Unit art_scheduling_legacy_LegacyInterface_computePhase(STACK_FRAME IS_058E6F bridges) {
          |  printf("Infeasible.  You should not get here in C");
          |  exit(1);
          |}"""
    return ret
  }

  def c_roundRobin(packageName: String,
                   bridges: ISZ[String],
                   filepath: String): ST = {
    val slangPath = s"architecture/${packageName}/Schedulers.scala"
    val slangMethodName = s"${packageName}.ScheduleProviderI.getRoundRobinOrder"
    val cMethodName = s"${packageName}_ScheduleProviderI_getRoundRobinOrder"
    val slangSymbol = s"${packageName}.Schedulers.roundRobinSchedule"
    val symbol = s"${packageName}_Schedulers_roundRobinSchedule"
    val iszUpdates = bridges.map((m: String) => s"IS_FDDCB6_up(result, i++, (art_Art_BridgeId) ${m}(SF_LAST)->id);")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, filepath, "", cMethodName, 0)

    val ret: ST =
      st"""#include <all.h>
          |#include <signal.h>
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |// Transpiled signature of the Slang variable ${slangSymbol}
          |// in ${slangPath}.  This weak function declaration allows
          |// ${cMethodName} to detect whether the Slang variable was deleted
          |__attribute__((weak)) IS_FDDCB6 ${symbol}(STACK_FRAME_ONLY);
          |
          |volatile sig_atomic_t shouldStop = 0;
          |
          |/*!
          | * Example C implementation of the Slang extension method ${slangMethodName}()
          | * defined in ${slangPath}
          | *
          | * @param result an empty schedule.  Add components in the order you want them to be dispatched.
          | *               IS_FDDCB6=ISZ[art.Art.BridgeId], i.e. an immutable sequence of art.Bridge
          | */
          |void ${cMethodName}(STACK_FRAME IS_FDDCB6 result) {
          |  ${declNewStackFrame};
          |
          |  if(${symbol}) {
          |    printf("Using the round robin order provided in ${slangPath}. Edit method \n");
          |    printf("  ${cMethodName} located in ${filepath}\n");
          |    printf("to supply your own\n");
          |
          |    IS_FDDCB6 order = ${symbol}(SF_LAST);
          |    memcpy(result->value, order->value, sizeof(art_Art_BridgeId) * order->size);
          |    result->size = order->size;
          |
          |  } else {
          |    printf("Transpiled Slang variable ${slangSymbol} not found.  Using an example schedule from method");
          |    printf("  ${cMethodName} located in ${filepath}\n");
          |
          |    // example schedule
          |    int i = 0;
          |    ${(iszUpdates, "\n")}
          |
          |    result->size = i;
          |  }
          |}
          |
          |/*!
          | * signal handler that sets shouldStop to true when invoked
          | */
          |void sigHandler(int signo) {
          |  shouldStop = 1;
          |}
          |
          |/*!
          | * Example C implementation of Slang extension method art.scheduling.roundrobin.RoundRobinExtensions.init()
          | * defined in art/scheduling/roundrobin/RoundRobin.scala.  The scheduler calls this
          | * during the initialization phase
          | *
          | * It registers a signal handler that is used to shut down the demo when it receives
          | * SIGINT (CTRL+C), SIGTERM
          | */
          |Unit art_scheduling_roundrobin_RoundRobinExtensions_init(STACK_FRAME_ONLY){
          |  int sigs[] = {SIGINT, SIGTERM};
          |  for(int i = 0; i < sizeof(sigs) / sizeof(int); i++){
          |    if(signal(sigs[i], sigHandler) == SIG_ERR) {
          |      printf("Error occurred while setting signal handler for %i\n", sigs[i]);
          |      exit(-1);
          |    }
          |  }
          |}
          |
          |/*!
          | * Example C implementation of Slang extension method art.scheduling.roundrobin.RoundRobinExtensions.shouldStop()
          | * defined in art/scheduling/roundrobin/RoundRobin.scala.  The scheduler calls this
          | * during the compute phase to determine when it should transition to the finalize phase
          | */
          |B art_scheduling_roundrobin_RoundRobinExtensions_shouldStop(STACK_FRAME_ONLY){
          |    return shouldStop == 1;
          |}
          |"""
    return ret
  }

  def c_static_schedule(packageName: String,
                        bridges: ISZ[String],
                        filepath: String): ST = {
    val slangPath = s"architecture/${packageName}/Schedulers.scala"
    val slangMethodName = s"${packageName}.ScheduleProviderI.getStaticSchedule"
    val cMethodName = s"${packageName}_ScheduleProviderI_getStaticSchedule"
    val slangSymbol = s"${packageName}.Schedulers.staticSchedule"
    val symbol = s"${packageName}_Schedulers_defaultStaticSchedule"
    val slotSequences = bridges.map((m: String) => s"fillInSlot(&slotSequence, i++, ${m}(SF_LAST)->id, length);")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, filepath, "", cMethodName, 0)

    val ret: ST =
      st"""#include <all.h>
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |// Transpiled signature of the Slang variable ${slangSymbol}
          |// in ${slangPath}.  This weak function declaration allows
          |// ${cMethodName} to detect whether the Slang variable was deleted
          |__attribute__((weak)) art_scheduling_static_Schedule_DScheduleSpec ${symbol}(STACK_FRAME_ONLY);
          |
          |// helper method
          |void fillInSlot(IS_5AA467 slotSequence, int index, Z bridgeId, int length);
          |
          |/*!
          | * Example C implementation of the Slang extension method ${slangMethodName}()
          | * defined in ${slangPath}
          | *
          | * @param result an empty schedule. Add slots in the order you want components to be dispatched.
          | */
          |void ${cMethodName}(STACK_FRAME art_scheduling_static_Schedule_DScheduleSpec result){
          |  ${declNewStackFrame};
          |
          |  if(${symbol}) {
          |    printf("Using the static schedule provided in ${slangPath}. Edit method \n");
          |    printf("  ${cMethodName} located in ${filepath}\n");
          |    printf("to supply your own\n");
          |
          |    art_scheduling_static_Schedule_DScheduleSpec schedule = ${symbol}(SF_LAST);
          |    result->hyperPeriod = schedule->hyperPeriod;
          |    result->maxDomain = schedule->maxDomain;
          |    memcpy(&result->schedule, &schedule->schedule, sizeof(struct art_scheduling_static_Schedule_DSchedule));
          |
          |  } else {
          |    printf("Transpiled Slang variable ${slangSymbol} not found.  Using an example schedule from method");
          |    printf("  ${cMethodName} located in ${filepath}\n");
          |
          |    // IS_5AA467=IS[Z, art.scheduling.static.Schedule.Slot], i.e. an immutable sequence of art.scheduling.static.Schedule.Slot
          |    DeclNewIS_5AA467(slotSequence);
          |
          |    Z length = 1000 / ${bridges.size};
          |
          |    int i = 0;
          |    ${(slotSequences, "\n")}
          |    slotSequence.size = i;
          |
          |    DeclNewart_scheduling_static_Schedule_DSchedule(dschedule);
          |    art_scheduling_static_Schedule_DSchedule_apply(SF &dschedule, &slotSequence);
          |
          |    Z maxDomain = 100;
          |    Z hyperPeriod = 1000;
          |
          |    art_scheduling_static_Schedule_DScheduleSpec_apply(SF result, maxDomain, hyperPeriod, &dschedule);
          |  }
          |}
          |
          |void fillInSlot(IS_5AA467 slotSequence, int index, Z bridgeId, int length) {
          |  // TODO: need to refactor to adjust to 2023.10 Slang changes
          |  exit(1);
          |  //slotSequence->value[index].bridgeId = bridgeId;
          |  //slotSequence->value[index].length = length;
          |}
          |
          |Unit art_scheduling_static_StaticSchedulerIO_message(STACK_FRAME String m) {
          |  printf("%s\n", m->value);
          |}
          |"""
    return ret
  }

  def c_process(): ST = {
    val ret: ST =
      st"""#include <all.h>
          |
          |#include <sys/time.h>
          |#include <time.h>
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |/** Returns current system time in milliseconds
          |  * NOTE: this requires returning 64bit ints
          |  */
          |S64 art_Process_time(STACK_FRAME_ONLY) {
          |  struct timeval tv; //Get a time structure
          |  gettimeofday(&tv, NULL); //Get the current time
          |  int64_t t = tv.tv_sec;
          |  t *= 1000;
          |  t += tv.tv_usec/1000;
          |  return  t;
          |}
          |
          |Unit Os_Ext_exit(STACK_FRAME Z code) {
          |  exit(code);
          |}
          |"""
    return ret
  }
}