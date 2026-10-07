// #Sireum
package org.sireum.hamr.codegen.arsit.util

import org.sireum._
import org.sireum.hamr.codegen.arsit.Util
import org.sireum.hamr.codegen.common.properties.OsateProperties
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.TimeUtil

object SchedulerUtil {

  def getThreadTimingPropertiesName(thread: AadlThreadOrDevice): String = {
    return s"${thread.pathAsString("_")}_timingProperties"
  }

  def getProcessorTimingPropertiesName(processor: AadlProcessor): String = {
    return s"${processor.pathAsString("_")}_timingProperties"
  }

  def getSchedulerTouches(symbolTable: SymbolTable, devicesAsThreads: B): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    ret = ret ++ getThreadReachableProcessors(symbolTable).map((p: AadlProcessor) =>
      st"println(Schedulers.${getProcessorTimingPropertiesName(p)})")

    val components: ISZ[AadlThreadOrDevice] =
      if (devicesAsThreads) symbolTable.getThreadOrDevices()
      else symbolTable.getThreads().map(m => m.asInstanceOf[AadlThreadOrDevice])

    ret = ret ++ components.map((t: AadlThreadOrDevice) =>
      st"println(Schedulers.${getThreadTimingPropertiesName(t)})")

    return ret
  }

  def getThreadTimingProperties(symbolTable: SymbolTable, devicesAsThreads: B): ISZ[ST] = {
    val components: ISZ[AadlThreadOrDevice] =
      if (devicesAsThreads) symbolTable.getThreadOrDevices()
      else symbolTable.getThreads().map(m => m.asInstanceOf[AadlThreadOrDevice])

    return components.map((t: AadlThreadOrDevice) => {
      val what = s"Compute_Execution_Time of ${t.pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME})"
      val computeExecutionTime: ST = t.computeExecutionTimePs match {
        case Some((low, high)) =>
          val pos = t.component.identifier.pos
          st"Some((${Util.artTimeLiteral(Util.toArtNs(low, s"$what (low)", pos))}, ${Util.artTimeLiteral(Util.toArtNs(high, s"$what (high)", pos))}))"
        case _ => st"None()"
      }
      val domain: String = t.getDomain(symbolTable) match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val name = getThreadTimingPropertiesName(t)
      st"""val ${name}: ThreadTimingProperties = ThreadTimingProperties(
          |  computeExecutionTime = ${computeExecutionTime},
          |  domain = ${domain})"""
    })
  }

  def getThreadReachableProcessors(symbolTable: SymbolTable): ISZ[AadlProcessor] = {
    var processors: Set[AadlProcessor] = Set.empty
    for (process <- symbolTable.getThreads().map((t: AadlThread) => t.getParent(symbolTable))) {
      process.getBoundProcessor(symbolTable) match {
        case Some(processor: AadlProcessor) => processors = processors + processor
        case Some(processor: AadlVirtualProcessor) =>
          processors = processors ++ symbolTable.getActualBoundProcessors(processor)
        case _ =>
      }
    }
    return processors.elements
  }

  def getProcessorTimingProperties(symbolTable: SymbolTable): ISZ[ST] = {
    return getThreadReachableProcessors(symbolTable).map((p: AadlProcessor) => {
      val clockPeriod: ST = timeOpt(p.clockPeriodPs,
        s"Clock_Period of ${p.pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__CLOCK_PERIOD})", p)
      val framePeriod: ST = timeOpt(p.framePeriodPs,
        s"Frame_Period of ${p.pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__FRAME_PERIOD})", p)
      val maxDomain: String = p.getMaxDomain() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val slotTime: ST = timeOpt(p.slotTimePs,
        s"Slot_Time of ${p.pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__SLOT_TIME})", p)
      val name = getProcessorTimingPropertiesName(p)
      st"""val ${name}: ProcessorTimingProperties = ProcessorTimingProperties(
          |  clockPeriod = ${clockPeriod},
          |  framePeriod = ${framePeriod},
          |  maxDomain = ${maxDomain},
          |  slotTime = ${slotTime})"""
    })
  }

  // an optional time value as an Option[Art.Time] in ns
  def timeOpt(ps: Option[Z], what: String, p: AadlProcessor): ST = {
    ps match {
      case Some(v) => return st"Some(${Util.artTimeLiteral(Util.toArtNs(v, what, p.component.identifier.pos))})"
      case _ => return st"None()"
    }
  }

  val defaultFramePeriodPs: Z = 1000 * TimeUtil.psPerMs // 1000 ms

  /** The Frame_Period, in ns, of the single processor threads are bound to, else the default */
  def getFramePeriodNs(symbolTable: SymbolTable): Z = {
    val processors: ISZ[AadlProcessor] = getThreadReachableProcessors(symbolTable)
    if (processors.size == 1) {
      processors(0).framePeriodPs match {
        case Some(ps) =>
          return Util.toArtNs(ps,
            s"Frame_Period of ${processors(0).pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__FRAME_PERIOD})",
            processors(0).component.identifier.pos)
        case _ =>
      }
    }
    return Util.toArtNs(defaultFramePeriodPs, "the default Frame_Period", None())
  }
}
