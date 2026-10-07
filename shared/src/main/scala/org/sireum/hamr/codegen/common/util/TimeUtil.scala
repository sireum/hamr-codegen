// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.message.{Message, Position, Reporter}

// the time units codegen converts to; named so as not to be confused with
// java.util.concurrent.TimeUnit, which ART uses
@enum object HamrTimeUnit {
  "ps"
  "ns"
  "us"
  "ms"
}

/** Exact time values (see doc/ExactTime-design.md): codegen holds every time value as a Z count
  * of picoseconds and converts it once, where it leaves codegen, to the resolution of the target */
object TimeUtil {

  // message kind of the warnings reported when a time value has to be rounded
  val timeRoundingKind: String = "time-rounding"

  val psPerNs: Z = 1000
  val psPerUs: Z = 1000 * psPerNs
  val psPerMs: Z = 1000 * psPerUs
  val psPerS: Z = 1000 * psPerMs

  // the period used when a thread or device has none (was 1 ms)
  val defaultPeriodPs: Z = psPerMs

  @strictpure def unitPs(unit: HamrTimeUnit.Type): Z =
    unit match {
      case HamrTimeUnit.ps => 1
      case HamrTimeUnit.ns => psPerNs
      case HamrTimeUnit.us => psPerUs
      case HamrTimeUnit.ms => psPerMs
    }

  /** Renders ps in the largest unit (s, ms, us, ns, ps) that shows it exactly, e.g. 1.5 ms */
  @pure def format(ps: Z): String = {
    val units: ISZ[(Z, String)] = ISZ((psPerS, "s"), (psPerMs, "ms"), (psPerUs, "us"), (psPerNs, "ns"))
    for (u <- units) {
      val (factor, name) = u
      // largest unit in which ps has at most 3 decimal places
      if (ps % (factor / 1000) == 0 && (ps >= factor || ps <= -factor)) {
        val whole = ps / factor
        val frac = ps % factor
        if (frac == 0) {
          return s"$whole $name"
        }
        val absFrac: Z = if (frac < 0) -frac else frac
        var digits = s"${absFrac / (factor / 1000) + 1000}"
        digits = ops.StringOps(digits).substring(1, 4)
        while (ops.StringOps(digits).endsWith("0")) {
          digits = ops.StringOps(digits).substring(0, digits.size - 1)
        }
        return s"$whole.$digits $name"
      }
    }
    return s"$ps ps"
  }

  /** Reports a warning unless the reporter already holds a warning with the same text at the same
    * position. Every time warning goes through here, so a value that is resolved or converted more
    * than once (Microkit re-resolution, a backend converting the same value at several sites, the
    * SysML front end followed by codegen) is reported once. */
  def warnOnce(pos: Option[Position], kind: String, msg: String, reporter: Reporter): Unit = {
    if (ops.ISZOps(reporter.messages).exists((m: Message) => m.isWarning && m.text == msg && m.posOpt == pos)) {
      return
    }
    reporter.warn(pos, kind, msg)
  }

  /** Converts ps to the given resolution, rounding to nearest (ties away from zero).
    *
    * @param what names the value for messages, e.g. "Period of top.proc.worker (Timing_Properties::Period)"
    * @param target names what the value is converted for, e.g. "the Microkit domain schedule"
    * @return the value in units of resolutionPs; reports a time-rounding warning if that is not
    *         exact, and an error (returning 0) if it is 0 or less or greater than maxValue */
  def fromPicoseconds(ps: Z, resolutionPs: Z, maxValue: Z, what: String, target: String,
                      pos: Option[Position], reporter: Reporter): Z = {
    return convert(ps, resolutionPs, maxValue, what, target, F, pos, reporter)
  }

  /** As fromPicoseconds, but a result of 0 is clamped to 1 with a time-rounding warning saying so,
    * instead of an error. Only for codegen's own constants (e.g. the CAmkES pacer's fixed slots). */
  def fromPicosecondsAtLeastOne(ps: Z, resolutionPs: Z, maxValue: Z, what: String, target: String,
                                pos: Option[Position], reporter: Reporter): Z = {
    return convert(ps, resolutionPs, maxValue, what, target, T, pos, reporter)
  }

  def convert(ps: Z, resolutionPs: Z, maxValue: Z, what: String, target: String, atLeastOne: B,
              pos: Option[Position], reporter: Reporter): Z = {
    if (resolutionPs <= 0) {
      halt(s"Infeasible: resolution $resolutionPs for $what")
    }
    if (ps <= 0) {
      reporter.error(pos, timeRoundingKind, s"$what is ${format(ps)}, but must be greater than 0")
      return 0
    }
    val q = ps / resolutionPs
    val r = ps % resolutionPs
    val rounded: Z = if (r * 2 >= resolutionPs) q + 1 else q
    if (rounded == 0) {
      if (atLeastOne) {
        warnOnce(pos, timeRoundingKind,
          s"$what is ${format(ps)}, which is less than half the ${format(resolutionPs)} resolution of $target; $target uses ${format(resolutionPs)}",
          reporter)
        return 1
      }
      reporter.error(pos, timeRoundingKind,
        s"$what is ${format(ps)}, which is 0 at the ${format(resolutionPs)} resolution of $target")
      return 0
    }
    if (rounded > maxValue) {
      reporter.error(pos, timeRoundingKind,
        s"$what is ${format(ps)}, which is too large for $target (at most ${format(maxValue * resolutionPs)})")
      return 0
    }
    if (r != 0) {
      warnOnce(pos, timeRoundingKind,
        s"$what is ${format(ps)}; $target uses ${format(rounded * resolutionPs)} (resolution ${format(resolutionPs)})",
        reporter)
    }
    return rounded
  }
}
