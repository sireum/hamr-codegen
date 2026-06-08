// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.ir.{GclSchedule, GclScheduleComponentRef, GclScheduleElement,
  GclScheduleSequence, GclScheduleSplitJoin}

object MHIPComputer {

  @pure def compute(schedule: GclSchedule): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    return collectFromElements(schedule.elements)
  }

  @pure def collectFromElements(elements: ISZ[GclScheduleElement]): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    var pairs: ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = ISZ()
    for (e <- elements) {
      e match {
        case sj: GclScheduleSplitJoin =>
          pairs = pairs ++ collectFromSplitJoin(sj)
        case _ =>
      }
    }
    return pairs
  }

  @pure def collectFromSplitJoin(sj: GclScheduleSplitJoin): ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = {
    var pairs: ISZ[(GclScheduleComponentRef, GclScheduleComponentRef)] = ISZ()

    val branchComponents: ISZ[ISZ[GclScheduleComponentRef]] =
      for (seq <- sj.sequences) yield collectComponents(seq)

    for (i <- z"0" until branchComponents.size) {
      for (j <- i + 1 until branchComponents.size) {
        for (c1 <- branchComponents(i)) {
          for (c2 <- branchComponents(j)) {
            pairs = pairs :+ ((c1, c2))
          }
        }
      }
    }

    for (seq <- sj.sequences) {
      pairs = pairs ++ collectFromElements(seq.elements)
    }

    return pairs
  }

  @pure def collectComponents(seq: GclScheduleSequence): ISZ[GclScheduleComponentRef] = {
    var result: ISZ[GclScheduleComponentRef] = ISZ()
    for (e <- seq.elements) {
      e match {
        case c: GclScheduleComponentRef =>
          result = result :+ c
        case sj: GclScheduleSplitJoin =>
          for (inner <- sj.sequences) {
            result = result ++ collectComponents(inner)
          }
        case _ =>
      }
    }
    return result
  }
}
