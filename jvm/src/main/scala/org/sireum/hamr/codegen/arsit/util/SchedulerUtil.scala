// #Sireum
package org.sireum.hamr.arsit.util

import org.sireum._
import org.sireum.hamr.codegen.common.symbols._

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
      val computeExecutionTime: String = t.getComputeExecutionTime() match {
        case Some((low, high)) => s"Some((${low}, ${high}))"
        case _ => "None()"
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
        case Some(processor: AadlVirtualProcessor) => processors = processors + symbolTable.getActualBoundProcess(processor).get
        case _ =>
      }
    }
    return processors.elements
  }

  def getProcessorTimingProperties(symbolTable: SymbolTable): ISZ[ST] = {
    return getThreadReachableProcessors(symbolTable).map((p: AadlProcessor) => {
      val clockPeriod: String = p.getClockPeriod() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val framePeriod: String = p.getFramePeriod() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val maxDomain: String = p.getMaxDomain() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val slotTime: String = p.getSlotTime() match {
        case Some(z) => s"Some(${z})"
        case _ => "None()"
      }
      val name = getProcessorTimingPropertiesName(p)
      st"""val ${name}: ProcessorTimingProperties = ProcessorTimingProperties(
          |  clockPeriod = ${clockPeriod},
          |  framePeriod = ${framePeriod},
          |  maxDomain = ${maxDomain},
          |  slotTime = ${slotTime})"""
    })
  }

  val defaultFramePeriod: Z = 1000 // 1000 ms


  def getFramePeriod(symbolTable: SymbolTable): Z = {
    val processors: ISZ[AadlProcessor] = getThreadReachableProcessors(symbolTable)
    if (processors.size == 1) {
      processors(0).getFramePeriod() match {
        case Some(z) => return z
        case _ =>
      }
    }
    return defaultFramePeriod
  }
}
