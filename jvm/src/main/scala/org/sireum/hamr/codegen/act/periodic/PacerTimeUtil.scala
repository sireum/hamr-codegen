// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act.util.Util
import org.sireum.hamr.codegen.common.properties.OsateProperties
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.TimeUtil
import org.sireum.message.{Position, Reporter}

/** Time conversions for the CAmkES pacers' domain schedules, which are in seL4 ticks of
  * Clock_Period (doc/ExactTime-design.md, D7) */
object PacerTimeUtil {

  val target: String = "the CAmkES pacer's domain schedule"

  // ticks are stored in dschedule_t.length, a word_t, which is 32 bits on 32-bit targets
  val maxTicks: Z = 2147483647

  // the pacers' own fixed slots
  val otherLenPs: Z = 200 * TimeUtil.psPerMs
  val pacerLenPs: Z = 10 * TimeUtil.psPerMs
  val domainZeroLenPs: Z = 10 * TimeUtil.psPerMs

  // used for a thread (or a process, all of whose threads) without Compute_Execution_Time,
  // the same as Microkit's default
  val defaultComputeExecutionTimePs: Z = 50 * TimeUtil.psPerMs

  @pure def processorWhat(p: AadlProcessor, prop: String, propName: String): String = {
    return s"$prop of ${p.pathAsString(".")} ($propName)"
  }

  /** The bound processor's Clock_Period in ps.  A non-MCS seL4 tick is the integer
    * TIMER_TICK_MS, so a Clock_Period that is not a whole number of ms is an error. */
  def clockPeriodPs(p: AadlProcessor, reporter: Reporter): Z = {
    p.clockPeriodPs match {
      case Some(ps) =>
        if (ps % TimeUtil.psPerMs != 0) {
          reporter.error(p.component.identifier.pos, Util.toolName,
            s"${processorWhat(p, "Clock_Period", OsateProperties.TIMING_PROPERTIES__CLOCK_PERIOD)} is ${TimeUtil.format(ps)}, but the CAmkES pacer needs a whole number of milliseconds, as seL4 ticks are TIMER_TICK_MS")
        }
        return ps
      case _ => halt("Unexpected: Clock_Period not specified")
    }
  }

  def framePeriodPs(p: AadlProcessor): Z = {
    p.framePeriodPs match {
      case Some(ps) => return ps
      case _ => halt("Unexpected: Frame_Period not specified")
    }
  }

  /** Converts a model value to ticks; 0 ticks is an error */
  def toTicks(ps: Z, clockPs: Z, what: String, pos: Option[Position], reporter: Reporter): Z = {
    return TimeUtil.fromPicoseconds(ps, clockPs, maxTicks, what, target, pos, reporter)
  }

  /** Converts one of the pacer's own fixed slots to ticks, clamped to at least one tick */
  def fixedTicks(ps: Z, clockPs: Z, what: String, reporter: Reporter): Z = {
    return TimeUtil.fromPicosecondsAtLeastOne(ps, clockPs, maxTicks,
      s"The pacer's own slot for $what", target, None(), reporter)
  }

  @pure def cetWhat(c: AadlComponent): String = {
    return s"Compute_Execution_Time of ${c.pathAsString(".")} (${OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME})"
  }

  def warnDefaultComputeExecutionTime(c: AadlComponent, reporter: Reporter): Unit = {
    TimeUtil.warnOnce(c.component.identifier.pos, Util.toolName,
      s"${c.pathAsString(".")} has no ${OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME}; ${target} gives it ${TimeUtil.format(defaultComputeExecutionTimePs)}",
      reporter)
  }

  /** The high end of t's Compute_Execution_Time in ps, or the default (with a warning) */
  def threadComputeExecutionTimePs(t: AadlThreadOrDevice, reporter: Reporter): Z = {
    t.computeExecutionTimePs match {
      case Some((_, high)) => return high
      case _ =>
        warnDefaultComputeExecutionTime(t, reporter)
        return defaultComputeExecutionTimePs
    }
  }

  /** The largest Compute_Execution_Time (high end) of p's threads in ps, with the thread it
    * comes from, or the default (with a warning) if none of them has one */
  def processComputeExecutionTimePs(p: AadlProcess, reporter: Reporter): (Z, Option[AadlThread]) = {
    var max: Z = 0
    var from: Option[AadlThread] = None()
    for (t <- p.getThreads() if t.getMaxComputeExecutionTimePs() > max) {
      max = t.getMaxComputeExecutionTimePs()
      from = Some(t)
    }
    if (from.isEmpty) {
      warnDefaultComputeExecutionTime(p, reporter)
      return (defaultComputeExecutionTimePs, None())
    }
    return (max, from)
  }

  /** The pad that fills the frame, in ticks; an error if the entries do not fit (returns 0) */
  def padTicks(frameTicks: Z, usedTicks: Z, clockPs: Z, p: AadlProcessor, reporter: Reporter): Z = {
    val pad = frameTicks - usedTicks
    if (pad < 0) {
      reporter.error(p.component.identifier.pos, Util.toolName,
        s"${target} needs $usedTicks ticks (${TimeUtil.format(usedTicks * clockPs)}), which does not fit in ${processorWhat(p, "Frame_Period", OsateProperties.TIMING_PROPERTIES__FRAME_PERIOD)} ($frameTicks ticks, ${TimeUtil.format(frameTicks * clockPs)})")
      return 0
    }
    return pad
  }
}
