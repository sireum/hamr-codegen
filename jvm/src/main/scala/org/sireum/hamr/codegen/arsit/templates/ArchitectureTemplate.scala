// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.ir.{Direction, FeatureCategory}

object ArchitectureTemplate {

  @pure def dispatchProtocol(dp: Dispatch_Protocol.Type, period: Z): ST = {
    val ret: ST = dp match {
      case Dispatch_Protocol.Sporadic => st"Sporadic(min = $period)"
      case Dispatch_Protocol.Periodic => st"Periodic(period = $period)"
    }
    return ret
  }


  @pure def connection(from: String, to: String): ST = {
    return st"""Connection(from = $from, to = $to)"""
  }

  @pure def demo(packageName: String,
                 architectureName: String,
                 architectureDescriptionName: String): ST = {
    val ad = s"${architectureName}.${architectureDescriptionName}"

    val ret: ST =
      st"""// #Sireum
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import art.scheduling.Scheduler
          |
          |${CommentTemplate.safeToEditComment_scala}
          |
          |object Demo extends App {
          |
          |  /** @return the scheduler to use for JVM based simulation as well as the 'default' scheduler
          |    *         that will be used when taking this program down to C/Linux.  Refer to
          |    *         'bin/run.sh -h' if you want to use a specific scheduler for C.  If the scheduler
          |    *         accepts a schedule and you want to provide that in C then just pass None()
          |    *
          |    *         If you want to use the legacy scheduler for C then you must use
          |    *           bin/transpile.cmd --legacy
          |    *           bin/compile.cmd
          |    *           bin/run.sh --legacy
          |    */
          |  def defaultScheduler(): Scheduler = {
          |    return Schedulers.getRoundRobinScheduler(None())
          |  }
          |
          |  def main(args: ISZ[String]): Z = {
          |    Cli(' ').parseRun(args, 0) match {
          |      case Some(o: Cli.RunOption) =>
          |        val scheduler: Scheduler = o.scheduler match {
          |          case Cli.RunChoice.Default => defaultScheduler()
          |          case Cli.RunChoice.RoundRobin => Schedulers.getRoundRobinScheduler(None())
          |          case Cli.RunChoice.Static => Schedulers.getStaticSchedulerH(MNone())
          |          case Cli.RunChoice.Legacy => Schedulers.getLegacyScheduler()
          |        }
          |
          |        Platform.setup()
          |
          |        art.Art.run(Arch.ad, scheduler)
          |
          |        Platform.tearDown()
          |      case Some(o: Cli.HelpOption) =>
          |      case _ => return 1
          |    }
          |    return 0
          |  }
          |}
          |
          |object Cli {
          |
          |  @datatype trait RunTopOption
          |
          |  @datatype class HelpOption extends RunTopOption
          |
          |  @enum object RunChoice {
          |    'Default
          |    'RoundRobin
          |    'Static
          |    'Legacy
          |  }
          |
          |  @datatype class RunOption(
          |                             val help: String,
          |                             val args: ISZ[String],
          |                             val scheduler: RunChoice.Type
          |                           ) extends RunTopOption
          |}
          |
          |import Cli._
          |
          |@record class Cli(val pathSep: C) {
          |
          |  def parseRunChoiceH(arg: String): Option[RunChoice.Type] = {
          |    arg match {
          |      case "default" => return Some(RunChoice.Default)
          |      case "roundRobin" => return Some(RunChoice.RoundRobin)
          |      case "static" => return Some(RunChoice.Static)
          |      case "legacy" => return Some(RunChoice.Legacy)
          |      case s =>
          |        eprintln(s"Expecting one of the following: { default, roundRobin, static, legacy }, but found '$$s'.")
          |        return None()
          |    }
          |  }
          |
          |  def parseRunChoice(args: ISZ[String], i: Z): Option[RunChoice.Type] = {
          |    if (i >= args.size) {
          |      eprintln("Expecting one of the following: { default, roundRobin, static, legacy }, but none found.")
          |      return None()
          |    }
          |    val r = parseRunChoiceH(args(i))
          |    return r
          |  }
          |
          |  def parseRun(args: ISZ[String], i: Z): Option[RunTopOption] = {
          |
          |    def help(): Unit = {
          |      println("Run Slang Embedded Program")
          |      println()
          |      println("Usage: <option>*")
          |      println()
          |      println("Available Options:")
          |      println("-s, --scheduler          The scheduler to use.  See Demo.scala for information")
          |      println("                           on 'default' (expects one of { default, roundRobin,")
          |      println("                           static, legacy }; default: default)")
          |      println("-h, --help               Display this information")
          |    }
          |
          |    var scheduler: RunChoice.Type = RunChoice.Default
          |    var j = i
          |    var isOption = T
          |    while (j < args.size && isOption) {
          |      var arg = args(j)
          |      if (arg == "-h" || arg == "--help") {
          |        help()
          |        return Some(HelpOption())
          |      } else if (arg == "-s" || arg == "--scheduler") {
          |        val o: Option[RunChoice.Type] = parseRunChoice(args, j + 1)
          |        o match {
          |          case Some(v) => scheduler = v
          |          case _ => return None()
          |        }
          |      } else {
          |        eprintln(s"Unrecognized option '$$arg'.")
          |        return None()
          |      }
          |      j = j + 2
          |    }
          |
          |    return Some(RunOption("", args, scheduler))
          |  }
          |}
          |"""
    return ret
  }

  @pure def portType(name: String,
                     typ: String,
                     id: Z,
                     identifier: String,
                     mode: String,
                     urgency: Option[Z]): ST = {
    val artPortType: String = if (urgency.nonEmpty) "UrgentPort" else "Port"
    val _urgency: String = if (urgency.nonEmpty) s", urgency = ${urgency.get}" else ""
    return st"""val $name = ${artPortType}[$typ] (id = portId"$id", name = "$identifier", mode = $mode${_urgency})"""
  }

  def genPort(port: Port): ST = {
    val id = port.portId
    val prefix: String = port.feature.category match {
      case FeatureCategory.EventPort => "Event"
      case FeatureCategory.EventDataPort => "Event"
      case FeatureCategory.DataPort => "Data"
      case _ => halt(s"Not handling ${port.feature.category}")
    }

    val dir: String = port.feature.direction match {
      case Direction.In => "In"
      case Direction.Out => "Out"
      case _ => "???"
    }

    val mode: String = s"${prefix}${dir}"

    return portType(
      port.name,
      port.getPortTypeNames.qualifiedReferencedTypeName,
      id,
      port.path,
      mode,
      port.urgency)
  }

  @pure def bridge(bridgeIdentifier: String,
                   instanceName: String,
                   typeName: String,
                   id: Z,
                   dispatchProtocol: ST,
                   dispatchTriggers: Option[ISZ[String]],
                   ports: ISZ[ST],
                   portArguments: ISZ[ST]
                  ): ST = {
    val _dispatchTriggers: ST =
      if (dispatchTriggers.isEmpty) st"None()"
      else st"Some(ISZ[Art.PortId](${(dispatchTriggers.get.map((f: String) => s"${f}.id"), ", ")}))"

    val ret: ST =
      st"""val ${bridgeIdentifier} : ${typeName} = {
          |  ${(ports, "\n")}
          |
          |  ${typeName}(
          |    id = bridgeId"$id",
          |    name = "$instanceName",
          |    dispatchProtocol = $dispatchProtocol,
          |    dispatchTriggers = ${_dispatchTriggers},
          |
          |    ${(portArguments, ",\n")}
          |  )
          |}"""
    return ret
  }

  @pure def architectureDescription(packageName: String,
                                    imports: ISZ[String],
                                    architectureName: String,
                                    architectureDescriptionName: String,
                                    bridges: ISZ[ST],
                                    components: ISZ[String],
                                    connections: ISZ[ST],
                                    touchMethod: Option[ST]): ST = {
    val _imports = imports.map((m: String) => st"import ${m}")

    val touches: (Option[String], Option[ST]) =
      if (touchMethod.nonEmpty)
        (Some("TranspilerUtil.touch()"), Some(
          st"""
              |object TranspilerUtil {
              |  ${touchMethod.get}
              |}
              |"""
        ))
      else (None(), None())

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import art.PortMode._
          |import art.DispatchPropertyProtocol._
          |import art.Art.BridgeId._
          |import art.Art.PortId._
          |${(_imports, "\n")}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object $architectureName {
          |  ${(bridges, "\n")}
          |
          |  val $architectureDescriptionName : ArchitectureDescription = {
          |    ${touches._1}
          |
          |    ArchitectureDescription(
          |      components = IS[Art.BridgeId, Bridge] (${(components, ", ")}),
          |
          |      connections = IS[Art.ConnectionId, UConnection] (${(connections, ",\n")})
          |    )
          |  }
          |}
          |${touches._2}
          |"""
    return ret
  }
}
