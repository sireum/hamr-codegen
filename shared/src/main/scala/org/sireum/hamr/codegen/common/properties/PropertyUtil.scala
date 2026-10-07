// #Sireum

package org.sireum.hamr.codegen.common.properties

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Property
import org.sireum.hamr.codegen.common.util.TimeUtil
import org.sireum.message.{Position, Reporter}

object PropertyUtil {
  @pure def hasProperty(properties: ISZ[ir.Property], propertyName: String): B = {
    return properties.filter(p => CommonUtil.getLastName(p.name) == propertyName).nonEmpty
  }

  @pure def getProperty(properties: ISZ[ir.Property], propertyName: String): Option[ir.Property] = {
    val op = properties.filter(container => CommonUtil.getLastName(container.name) == propertyName)
    val ret: Option[ir.Property] = if (op.nonEmpty) {
      assert(op.size == 1) // sanity check, OSATE doesn't allow properties to be assigned to more than once
      Some(op(0))
    } else {
      None()
    }
    return ret
  }

  @pure def getPropertyValues(properties: ISZ[ir.Property], propertyName: String): ISZ[ir.PropertyValue] = {
    return properties.filter(container => CommonUtil.getLastName(container.name) == propertyName).flatMap(p => p.propertyValues)
  }

  @pure def getDiscreetPropertyValue(properties: ISZ[ir.Property], propertyName: String): Option[ir.PropertyValue] = {
    val ret: Option[ir.PropertyValue] = getPropertyValues(properties, propertyName) match {
      case ISZ(a) => Some(a)
      case _ => None[ir.PropertyValue]()
    }
    return ret
  }

  def getUnitPropZ(props: ISZ[ir.Property], propName: String): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(props, propName) match {
      case Some(v: ir.UnitProp) =>
        R(v.value) match {
          case Some(vv) => Some(conversions.R.toZ(vv))
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }


  def getPriority(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, OsateProperties.THREAD_PROPERTIES__PRIORITY) match {
      case Some(ir.UnitProp(z, _)) =>
        R(z) match {
          case Some(v) => Some(conversions.R.toZ(v))
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }

  @memoize def getDomainMappings(properties: ISZ[ir.Property]): Map[IdPath, Z] = {
    def getEntry(e1: Property, e2: Property): (IdPath, Z) = {
      val idPath: IdPath = e1.propertyValues match {
        case ISZ(ir.ReferenceProp(name)) => name.name
        case _ => halt("Invalid entry")
      }

      val domain: Z = e2.propertyValues match {
        case ISZ(ir.UnitProp(value, _)) =>
          R(value) match {
            case Some(vv) => Some(conversions.R.toZ(vv)).get
            case _ => halt(s"Invalid Z: ${value}")
          }
        case _ => halt("Invalid entry")
      }
      return (idPath, domain)
    }

    val pvs = getPropertyValues(properties, CaseSchedulingProperties.DOMAIN_MAPPING)

    val entries: ISZ[(IdPath, Z)] = pvs.map((p: ir.PropertyValue) => {
      p match {
        case ir.RecordProp(ISZ(e1, e2)) =>
          if (CommonUtil.getLastName(e1.name) == CaseSchedulingProperties.DOMAIN_ENTRY__COMPONENT) {
            assert(CommonUtil.getLastName(e2.name) == CaseSchedulingProperties.DOMAIN_ENTRY__DOMAIN)

            getEntry(e1, e2)
          } else {
            assert(CommonUtil.getLastName(e2.name) == CaseSchedulingProperties.DOMAIN_ENTRY__COMPONENT)
            assert(CommonUtil.getLastName(e1.name) == CaseSchedulingProperties.DOMAIN_ENTRY__DOMAIN)

            getEntry(e2, e1)
          }
        case _ => halt(s"Invalid domain mapping property: ${p}")
      }
    })

    return Map.empty[IdPath, Z] ++ entries
  }


  /* unit conversions consistent with AADL/ISO */
  def getStackSizeInBytes(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, OsateProperties.MEMORY_PROPERTIES__STACK_SIZE) match {
      case Some(ir.UnitProp(z, u)) =>
        R(z) match {
          case Some(v) =>
            val _v = conversions.R.toZ(v)
            val _ret: Option[Z] = u match {
              case Some("bits") => Some(_v / z"8")
              case Some("Bytes") => Some(_v)
              case Some("KByte") => Some(_v * z"1000")
              case Some("KiByte") => Some(_v * z"1024")
              case Some("MByte") => Some(_v * z"1000" * z"1000")
              case Some("MiByte") => Some(_v * z"1024" * z"1024")
              case Some("GByte") => Some(_v * z"1000" * z"1000" * z"1000")
              case Some("GiByte") => Some(_v * z"1024" * z"1024" * z"1024")
              case Some("TByte") => Some(_v * z"1000" * z"1000" * z"1000" * z"1000")
              case Some("TiByte") => Some(_v * z"1024" * z"1024" * z"1024" * z"1024")
              case _ => None[Z]()
            }
            _ret
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
    return ret
  }

  def getQueueSize(f: ir.Feature, defaultQueueSize: Z): Z = {
    val ret: Z = getUnitPropZ(f.properties, OsateProperties.COMMUNICATION_PROPERTIES__QUEUE_SIZE) match {
      case Some(z) => z
      case _ => defaultQueueSize
    }
    return ret
  }

  def getDispatchProtocol(c: ir.Component): Option[Dispatch_Protocol.Type] = {
    val ret: Option[Dispatch_Protocol.Type] = getDiscreetPropertyValue(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL) match {
      case Some(ir.ValueProp("Periodic")) => Some(Dispatch_Protocol.Periodic)
      case Some(ir.ValueProp("Sporadic")) => Some(Dispatch_Protocol.Sporadic)
      case _ => None[Dispatch_Protocol.Type]()
    }
    return ret
  }

  def getPeriod(c: ir.Component): Option[Z] = {
    val ret: Option[Z] = getDiscreetPropertyValue(c.properties, OsateProperties.TIMING_PROPERTIES__PERIOD) match {
      case Some(ir.UnitProp(z, u)) =>
        assert(u.nonEmpty, s"period's unit not provided for ${CommonUtil.getName(c.identifier)}")
        Some(convertToMS(z, u.get))
      case _ => None[Z]()
    }
    return ret
  }

  def getActualProcessorBinding(c: ir.Component): ISZ[IdPath] = {
    val ps = getPropertyValues(c.properties, OsateProperties.DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING)
    var ret: ISZ[IdPath] = ISZ()
    for(p <- ps) {
      p match {
        case v: ir.ReferenceProp => ret = ret :+ v.value.name
        case x =>
          halt(s"Unexpected: was expecting a reference prop for ${c.identifier.name} but found $x")
      }
    }
    return ret
  }

  def getComputeEntrypointSourceText(properties: ISZ[ir.Property]): Option[String] = {
    val ret: Option[String] = getDiscreetPropertyValue(properties, OsateProperties.PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    return ret
  }

  def getInitializeEntryPoint(properties: ISZ[ir.Property]): Option[String] = {
    val ret: Option[String] = getDiscreetPropertyValue(properties, OsateProperties.PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT) match {
      case Some(ir.ValueProp(v)) => Some(v)
      case _ => None[String]()
    }
    return ret
  }

  def getSourceText(properties: ISZ[ir.Property]): ISZ[String] = {
    return getPropertyValues(properties, OsateProperties.PROGRAMMING_PROPERTIES__SOURCE_TEXT).map(p => p.asInstanceOf[ir.ValueProp].value)
  }

  def getUseRawConnection(properties: ISZ[ir.Property]): B = {
    val ret: B = PropertyUtil.getDiscreetPropertyValue(properties, HamrProperties.HAMR__BIT_CODEC_RAW_CONNECTIONS) match {
      case Some(ir.ValueProp("true")) => T
      case Some(ir.ValueProp("false")) => F
      case _ => F
    }
    return ret
  }

  /** Parses a time property value to picoseconds (doc/ExactTime-design.md, D2).
    *
    * The value is scaled exactly (Slang R) and rounded to the nearest picosecond. That is silent
    * when the value is within double precision of a whole picosecond (OSATE writes times as doubles,
    * e.g. 33.3 ms as "3.3299999999999996E10" ps); a larger fraction, which only a decimal SysML
    * value can have, is rounded with a time-rounding warning.
    *
    * @param what names the value for messages, e.g. "Period of top.proc.worker (Timing_Properties::Period)"
    * @return None, after reporting an error, if the unit is missing or unknown or the value does
    *         not parse */
  def toPicoseconds(value: String, unitOpt: Option[String], what: String, pos: Option[Position],
                    reporter: Reporter): Option[Z] = {
    val unit: String = unitOpt match {
      case Some(u) => u
      case _ =>
        reporter.error(pos, CommonUtil.toolName, s"$what has no time unit")
        return None()
    }
    val factor: Z = unit match {
      case "ps" => 1
      case "ns" => TimeUtil.psPerNs
      case "us" => TimeUtil.psPerUs
      case "ms" => TimeUtil.psPerMs
      case "sec" => TimeUtil.psPerS
      case "min" => 60 * TimeUtil.psPerS
      case "hr" => 3600 * TimeUtil.psPerS
      case _ =>
        reporter.error(pos, CommonUtil.toolName, s"$what has an unknown time unit '$unit'")
        return None()
    }
    val r: R = R(value) match {
      case Some(v) => v
      case _ =>
        reporter.error(pos, CommonUtil.toolName, s"$what has a value, '$value', that is not a number")
        return None()
    }
    val exact: R = r * conversions.Z.toR(factor)
    val half = R("0.5").get
    val ps: Z = if (exact < R("0").get) conversions.R.toZ(exact - half) else conversions.R.toZ(exact + half)
    val diff: R = exact - conversions.Z.toR(ps)
    val absDiff: R = if (diff < R("0").get) -diff else diff
    val absExact: R = if (exact < R("0").get) -exact else exact
    // 2^-50: a few times the 2^-53 rounding error of the one double multiplication OSATE does
    val noise: R = absExact / R("1125899906842624").get
    if (absDiff > noise) {
      TimeUtil.warnOnce(pos, TimeUtil.timeRoundingKind,
        s"$what is $value $unit, which is not a whole number of picoseconds; it is rounded to ${TimeUtil.format(ps)}",
        reporter)
    }
    return Some(ps)
  }

  /** Parses the time property propName of c to picoseconds (D2); None if c does not set it */
  def getTimePs(c: ir.Component, propName: String, what: String, reporter: Reporter): Option[Z] = {
    getDiscreetPropertyValue(c.properties, propName) match {
      case Some(ir.UnitProp(value, unitOpt)) =>
        return toPicoseconds(value, unitOpt, what, c.identifier.pos, reporter)
      case Some(x) =>
        reporter.error(c.identifier.pos, CommonUtil.toolName, s"$what must be a time value, found $x")
        return None()
      case _ => return None()
    }
  }

  /** Parses c's Compute_Execution_Time range to picoseconds (D2); None if c does not set it */
  def getComputeExecutionTimePs(c: ir.Component, path: String, reporter: Reporter): Option[(Z, Z)] = {
    val what = s"Compute_Execution_Time of $path (${OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME})"
    getDiscreetPropertyValue(c.properties, OsateProperties.TIMING_PROPERTIES__COMPUTE_EXECUTION_TIME) match {
      case Some(ir.RangeProp(low, high)) =>
        val lowPs = toPicoseconds(low.value, low.unit, s"$what (low)", c.identifier.pos, reporter)
        val highPs = toPicoseconds(high.value, high.unit, s"$what (high)", c.identifier.pos, reporter)
        if (lowPs.nonEmpty && highPs.nonEmpty) {
          return Some((lowPs.get, highPs.get))
        }
        return None()
      case Some(x) =>
        reporter.error(c.identifier.pos, CommonUtil.toolName, s"$what must be a time range, found $x")
        return None()
      case _ => return None()
    }
  }

  /** Parses c's Slot_Time to picoseconds (D2, D5). A Slot_Time without a unit keeps its old meaning:
    * its number was passed through as is, and AIR gives times in picoseconds, so it is read as
    * picoseconds, with a warning that the unit is missing. */
  def getSlotTimePs(c: ir.Component, path: String, reporter: Reporter): Option[Z] = {
    val what = s"Slot_Time of $path (${OsateProperties.TIMING_PROPERTIES__SLOT_TIME})"
    getDiscreetPropertyValue(c.properties, OsateProperties.TIMING_PROPERTIES__SLOT_TIME) match {
      case Some(ir.UnitProp(value, None())) =>
        TimeUtil.warnOnce(c.identifier.pos, CommonUtil.toolName,
          s"$what has no time unit; it is read as $value ps", reporter)
        return toPicoseconds(value, Some("ps"), what, c.identifier.pos, reporter)
      case _ => return getTimePs(c, OsateProperties.TIMING_PROPERTIES__SLOT_TIME, what, reporter)
    }
  }

  def convertToMS(value: String, unit: String): Z = {
    val ret: Z = R(value) match {
      case Some(v) =>
        val _v = conversions.R.toZ(v)
        val ret: Z = unit match {
          case "ps" => _v / (z"1000" * z"1000" * z"1000")
          case "ns" => _v / (z"1000" * z"1000")
          case "us" => _v / z"1000"
          case "ms" => _v
          case "sec" => _v * z"1000"
          case "min" => _v * z"1000" * z"60"
          case "hr" => _v * z"1000" * z"60" * z"60"
          case _ =>
            halt(s"Unexpected time unit ${unit}")
        }
        ret
      case _ =>
        halt(s"Could not convert the string '${value}' to Z")
    }
    return ret
  }
}
