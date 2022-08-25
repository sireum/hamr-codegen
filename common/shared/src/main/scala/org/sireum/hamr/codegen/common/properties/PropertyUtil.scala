// #Sireum

package org.sireum.hamr.codegen.common.properties

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.ir
import org.sireum.hamr.ir.Property

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
              case Some("MByte") => Some(_v * z"1000" * z"1000")
              case Some("GByte") => Some(_v * z"1000" * z"1000" * z"1000")
              case Some("TByte") => Some(_v * z"1000" * z"1000" * z"1000" * z"1000")
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

  def getActualProcessorBinding(c: ir.Component): Option[IdPath] = {

    val ret: Option[IdPath] =
      getDiscreetPropertyValue(c.properties, OsateProperties.DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING) match {
        case Some(v: ir.ReferenceProp) => Some(v.value.name)
        case _ => return None()
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
