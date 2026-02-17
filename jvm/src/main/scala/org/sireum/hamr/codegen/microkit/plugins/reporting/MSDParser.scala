package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.message.{FlatPos, Position, Reporter}
import org.sireum.{ISZ, Os, U32, None => SNone, Option => SOption, Some => SSome, String => SString}
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.{Attributes, InputSource, Locator}
import org.sireum.hamr.codegen.microkit.plugins.reporting.MSDContainers._

import java.io.{FileReader, Reader}
import javax.xml.parsers.SAXParserFactory

object MSDParser {

  case class NodeSpan(
                       label: String,
                       beginLine: Int,
                       beginCol: Int,
                       beginOffset: Long,
                       endLine: Int,
                       endCol: Int,
                       endOffset: Long,
                       attrs: Map[String, String],
                       children: collection.mutable.ListBuffer[NodeSpan] = collection.mutable.ListBuffer()
                     )

  def illFormed(cond: Boolean, msg: String, posOpt: org.sireum.Option[Position], reporter: Reporter): Unit = {
    if (!cond) {
      reporter.error(posOpt = posOpt, kind = "MSDParser", message = msg)
      throw new Error("")
    }
  }

  def parse(xmlFile: Os.Path, rootDir: Os.Path, reporter: Reporter): SOption[system] = {
    class CountingReader(underlying: Reader) extends Reader {
      var line: Int = 1
      var col: Int = 1
      var offset: Long = 0

      var m = Map.empty[Int, (Int, Int, Long)]

      var lastTagLine: Int = -1
      var lastTagCol: Int = -1
      var lastTagOffset: Long = -1

      override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
        val n = underlying.read(cbuf, off, len)
        if (n > 0) {
          for (i <- off until off + n) {
            offset += 1
            if (cbuf(i) == '<') {
              lastTagLine = line
              lastTagCol = col
              lastTagOffset = lastTagOffset
            }
            if (cbuf(i) == '>') {
              m = m + (line -> (lastTagLine, lastTagCol, lastTagOffset))
            }

            if (cbuf(i) == '\n') {
              line += 1
              col = 1
            } else {
              col += 1
            }
          }
        }
        n
      }

      override def close(): Unit = underlying.close()
    }

    val factory = SAXParserFactory.newInstance()
    factory.setNamespaceAware(true)
    val parser = factory.newSAXParser()

    val handler = new DefaultHandler {
      var locator: Locator = _
      val stack = collection.mutable.Stack[NodeSpan]()
      var root: Option[NodeSpan] = None
      var counter: CountingReader = _

      def setCounter(c: CountingReader): Unit = counter = c

      override def setDocumentLocator(l: Locator): Unit = locator = l

      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
        val attrMap = (0 until attributes.getLength)
          .map(i => attributes.getQName(i) -> attributes.getValue(i))
          .toMap

        val last = counter.m(locator.getLineNumber)
        val node = NodeSpan(
          qName,
          last._1,
          last._2,
          last._3,
          -1, -1, -1,
          attrMap
        )

        if (stack.nonEmpty) {
          stack.top.children += node
        } else {
          root = Some(node)
        }
        stack.push(node)
      }

      override def endElement(uri: String, localName: String, qName: String): Unit = {
        val node = stack.pop()
        val completedNode = node.copy(
          endLine = locator.getLineNumber,
          endCol = locator.getColumnNumber,
          endOffset = counter.offset
        )

        if (stack.nonEmpty) {
          val siblings = stack.top.children
          siblings.update(siblings.indexWhere(_.eq(node)), completedNode)
        } else {
          root = Some(completedNode)
        }
      }
    }

    import scala.language.reflectiveCalls
    val countingReader = new CountingReader(new FileReader(xmlFile.value.native))
    handler.setCounter(countingReader)
    parser.parse(new InputSource(countingReader), handler)

    import scala.language.reflectiveCalls
    handler.root match {
      case Some(root) =>
        illFormed(root.label == "system", "Expected root label to be 'system'", org.sireum.Some(buildPosition(root, xmlFile, rootDir)), reporter)

        var domainSchedule: SOption[domain_schedule] = SNone()
        var protectionDomains: ISZ[protection_domain] = ISZ()
        var memoryRegions: ISZ[memory_region] = ISZ()
        var channels: ISZ[channel] = ISZ()
        for (c <- root.children) {
          c.label match {
            //case "domain_schedule" => domainSchedule = SSome(parseDomainSchedule(c, xmlFile, rootDir))
            case "protection_domain" => protectionDomains = protectionDomains :+ parseProtectionDomain(c, xmlFile, rootDir, reporter)
            case "memory_region" => memoryRegions = memoryRegions :+ parseMemoryRegion(c, xmlFile, rootDir)
            case "channel" => channels = channels :+ parseChannel(c, xmlFile, rootDir, reporter)
            case "xi:include" =>
              c.attrs.get("href") match {
                case Some(p) =>
                  val schedulePath = rootDir / p
                  if (!schedulePath.exists) {
                    reporter.error(org.sireum.None(), MicrokitCodegen.toolName, s"Didn't find file containing the static schedule at $schedulePath")
                  } else {
                    domainSchedule = SSome(domain_schedule(p, ISZ(), buildPosition(c, xmlFile, rootDir)))
                  }
                case _ =>
              }
            case x => throw new RuntimeException(s"Unexpected: $x")
          }
        }

        return SSome(system(
          domain_schedule = domainSchedule.get,
          protectionDomains = protectionDomains,
          memoryRegions = memoryRegions,
          channels = channels,
          pos = buildPosition(root, xmlFile, rootDir)))
      case _ =>
    }

    return SNone()
  }

  private def parseChannel(c: NodeSpan, xmlFile: Os.Path, rootDir: Os.Path, reporter: Reporter): channel = {
    illFormed(c.children.size == 2, s"Expected 2 children but found ${c.children.size}",
      org.sireum.Some(buildPosition(c, xmlFile, rootDir)), reporter)

    val end1 = {
      val e = c.children.head
      end(pd = e.attrs("pd"), id = e.attrs("id"), pos = buildPosition(e, xmlFile, rootDir))
    }
    val end2 = {
      val e = c.children.last
      end(pd = e.attrs("pd"), id = e.attrs("id"), pos = buildPosition(e, xmlFile, rootDir))
    }

    return channel(end1, end2, buildPosition(c, xmlFile, rootDir))
  }

  private def parseMemoryRegion(c: NodeSpan, xmlFile: Os.Path, rootDir: Os.Path): memory_region = {
    return memory_region(name = c.attrs("name"), size = c.attrs("size"), pos = buildPosition(c, xmlFile, rootDir))
  }

  private def parseProtectionDomain(pd: NodeSpan, xmlFile: Os.Path, rootDir: Os.Path, reporter: Reporter): protection_domain = {
    val name = pd.attrs("name")
    val kind: DomainKind.Type = {
      if (name == "pacer") DomainKind.Pacer
      else if (name.contains("_MON")) DomainKind.Monitor
      else DomainKind.Thread
    }

    val optId: SOption[SString] = pd.attrs.get("id") match {
      case Some(x) => SSome(x)
      case _ => SNone()
    }

    var program_image: SOption[SString] = SNone()
    var maps: ISZ[map] = ISZ()
    var irqs: ISZ[irq] = ISZ()
    var setvars: ISZ[setvar] = ISZ()
    var protection_domains: ISZ[protection_domain] = ISZ()
    var virtual_machines: ISZ[virtual_machine] = ISZ()

    for (c <- pd.children) {
      c.label match {
        case "program_image" => program_image = SSome(c.attrs("path"))
        case "map" => maps = maps :+ parseMap(c, xmlFile, rootDir)
        case "irq" => irqs = irqs :+ parseIrq(c, xmlFile, rootDir)
        case "setvar" => setvars = setvars :+ parseSetvar(c, xmlFile, rootDir)
        case "protection_domain" => protection_domains = protection_domains :+ parseProtectionDomain(c, xmlFile, rootDir, reporter)
        case "virtual_machine" => virtual_machines = virtual_machines :+ parseVirtualMachine(c, xmlFile, rootDir)
        case x =>
          illFormed(false, s"Unexpected: $x", org.sireum.Some(buildPosition(c, xmlFile, rootDir)), reporter)
      }
    }

    return protection_domain(
      name = name,
      kind = kind,
      schedulingDomain = pd.attrs("domain"),
      optId = optId,
      program_image = program_image.get,
      maps = maps,
      protection_domains = protection_domains,
      irqs = irqs,
      virtual_machines = virtual_machines,
      pos = buildPosition(pd, xmlFile, rootDir))
  }

  private def parseSetvar(c: NodeSpan, f: Os.Path, rootDir: Os.Path): setvar = {
    val symbol = c.attrs("symbol")
    val region_paddr = c.attrs("region_paddr")

    return setvar(symbol = symbol, region_paddr = region_paddr, pos = buildPosition(c, f, rootDir))
  }


  private def parseVirtualMachine(vm: NodeSpan, f: Os.Path, rootDir: Os.Path): virtual_machine = {
    var vcpuX: Option[vcpu] = None
    var maps: ISZ[map] = ISZ()

    for (child <- vm.children) {
      child.label match {
        case "vcpu" =>
          vcpuX = Some(
            vcpu(
              id = child.attrs("id"),
              pos = buildPosition(child, f, rootDir)))
        case "map" =>  maps = maps :+ parseMap(child, f, rootDir)
        case x => throw new RuntimeException(s"Unexpected: $x")
      }
    }

    return virtual_machine(
      name = vm.attrs("name"),
      vcpu = vcpuX.get,
      maps = maps,
      pos = buildPosition(vm, f, rootDir))
  }

  private def parseIrq(c: NodeSpan, f: Os.Path, rootDir: Os.Path): irq = {
    return irq(
      irq = c.attrs("irq"),
      id = c.attrs("id"),
      pos = buildPosition(c, f, rootDir))
  }

  private def parseMap(c: NodeSpan, f: Os.Path, rootDir: Os.Path): map = {
    val setvar_vaddr: SOption[SString] =
      c.attrs.get("setvar_vaddr") match {
        case Some(s) => SSome(s)
        case _ => SNone()
      }

    return map(
      mr = c.attrs("mr"),
      vaddr = c.attrs("vaddr"),
      perms = c.attrs("perms"),
      setvar_vaddr = setvar_vaddr,
      pos = buildPosition(c, f, rootDir))
  }

  private def buildPosition(n: NodeSpan, f: Os.Path, root: Os.Path): Position = {

    return FlatPos(
      uriOpt = SSome(root.relativize(f).value),
      beginLine32 = U32(n.beginLine),
      beginColumn32 = U32(n.beginCol),
      endLine32 = U32(n.endLine),
      endColumn32 = U32(n.endCol),
      offset32 = U32(0),
      length32 = U32(0))
  }
}
