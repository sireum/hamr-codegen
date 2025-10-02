// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.message.{FlatPos, Position}
import org.sireum.U32._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols.{AadlThread, GclAnnexClauseInfo}
import org.sireum.hamr.codegen.microkit.plugins.reporting.MSDContainers.system
import org.sireum.hamr.codegen.microkit.reporting._
import org.sireum.hamr.ir.{Direction, GclNamedElement}

object ReportUtil {

  @pure def deWin(url: String): String = {
    if (Os.isWin) {
      return ops.StringOps(url).replaceAllChars('\\', '/')
    } else {
      return url
    }
  }

  @pure def buildPos(beginLine: Z, endLine: Z, file: Os.Path,
                     workspaceDir: Os.Path, reportDir: Os.Path): Position = {
    return buildPosH(
      beginLine = beginLine + 1, endLine = endLine + 1,
      beginCol = 0, endCol = 0,
      offset = 0, length = 0,
      file = file, workspaceDir = workspaceDir, reportDir = reportDir)
  }

  @pure def buildPosA(portPos: Position, workspaceRoot: Os.Path, sel4OutputDir: Os.Path): Position = {
    return buildPosH(beginLine = portPos.beginLine, endLine = portPos.endLine,
      beginCol = portPos.beginColumn, endCol = portPos.endColumn,
      offset = portPos.offset, length = portPos.length,
      file = Os.path(portPos.uriOpt.get), workspaceDir = workspaceRoot, reportDir = sel4OutputDir)
  }

  @pure def buildPosH(beginLine: Z, endLine: Z, beginCol: Z, endCol: Z, offset: Z, length: Z,
                      file: Os.Path, workspaceDir: Os.Path, reportDir: Os.Path): Position = {
    assert (beginLine != -1)
    assert (endLine != -1)
    assert (beginLine <= endLine)

    val f: Os.Path =
      if (file.exists) {
        file
      } else {

        val nameo = ops.StringOps(deWin(file.value))
        if (nameo.startsWith("file")) {
          // probably from sysml
          Os.Path.fromUri(nameo.s)
        } else {
          // probably from osate
          // e.g. /isolette-artifacts-sel4/aadl/packages/Regulate.aadl
          assert(nameo.startsWith("/"), nameo.s)
          val sub = nameo.substring(nameo.indexOfFrom('/', 1) + 1, nameo.size)
          workspaceDir / sub
        }
      }

    assert(f.exists, f.value)
    val rel = reportDir.relativize(f)

    return FlatPos(
      uriOpt = Some(deWin(rel.value)),
      beginLine32 = conversions.Z.toU32(beginLine),
      beginColumn32 = conversions.Z.toU32(beginCol),
      endLine32 = conversions.Z.toU32(endLine),
      endColumn32 = conversions.Z.toU32(endCol),
      offset32 = conversions.Z.toU32(offset),
      length32 = conversions.Z.toU32(length))
  }

  @pure def createFullLink(name: String, title: String, position: Position): ST = {
    return st"<a title='$title' href='${createLink(position)}'>$name</a>"
  }

  @pure def createLink(position: Position): ST = {

    val fp = position.asInstanceOf[FlatPos]

    return st"${fp.uriOpt.get}#L${fp.beginLine32.toZ}" //-L${fp.endLine32.toZ}"
  }

  @pure def locateText(str: String, singleLine: B, region: ISZ[String]): (Z, Z) = {
    for (i <- 0 until region.size) {
      if (ops.StringOps(region(i)).contains(str)) {
        if (singleLine) {
          return (i, i + 1)
        } else {
          for (j <- i + 1 until region.size) {
            if (ops.StringOps(ops.StringOps(region(i)).trim).startsWith("//")) {
              return (i, j)
            } else {
              return (-1, -1)
            }
          }
        }
      }
    }
    return (-1, -1)
  }

  @pure def relativize(dir: Os.Path, root: Os.Path): Os.Path = {
    return root.relativize(dir)
  }

  @datatype class ModelPackageReport(val components: Map[String, ModelComponentReport],
                                     val libraryAnnexes: Map[String, Position])

  @datatype class ModelComponentReport(val pos: Position,
                                       val annexSubclauses: Map[String, Position])

  @pure def parseAadl(workspaceRoot: Os.Path, reportDir: Os.Path): Map[String, ModelPackageReport] = {
    var packages = Map.empty[String, ModelPackageReport]
    var libraryAnnexes = Map.empty[String, Position]

    val aadlFiles = Os.Path.walk(workspaceRoot, T, F, p => p.ext == "aadl")

    for(f <- aadlFiles) {
      var components = Map.empty[String, ModelComponentReport]
      var packageName: Option[String] = None()

      val content = StringUtil.split_PreserveEmptySegments(f.read, c => c == '\n')

      var currentComponent: Option[(String, Z)] = None()
      var gumbo: Option[Position] = None()

      for (i <- 0 until content.size) {
        val line = ops.StringOps(ops.StringOps(content(i)).trim)

        if (!line.startsWith("--")) {
          if (line.contains("package ")) {
            assert(packageName.isEmpty)
            packageName = Some(RustParser.getNameH("package", line, F, T).get)
          }

          if (currentComponent.nonEmpty) {
            //println(s"searching for '${currentComponent.get._1}'")
            if (line.contains(s"end ${currentComponent.get._1}")) {
              components = components + currentComponent.get._1 ~> ModelComponentReport(
                buildPos(currentComponent.get._2 - 1, i, f, workspaceRoot, reportDir),
                if (gumbo.nonEmpty) Map.empty[String, Position] + "GUMBO" ~> gumbo.get
                else Map.empty)
              currentComponent = None()
              gumbo = None()
            }
            else if (line.contains(s"annex ")) {
              val annexName = RustParser.getNameH("annex", line, F, T).get
              if (annexName == "GUMBO") {
                assert(gumbo.isEmpty, s"There should only be only one gumbo subclause per component: $f")
                gumbo = Some(buildPos(i, i, f, workspaceRoot, reportDir))
              }
            }
            else if (line.contains("**};") && gumbo.nonEmpty) {
              gumbo = Some(buildPos(gumbo.get.beginLine - 1, i, f, workspaceRoot, reportDir))
            }
          }
          else if (line.startsWith("**}") && gumbo.nonEmpty) {
            libraryAnnexes = libraryAnnexes + "GUMBO" ~> buildPos(gumbo.get.beginLine - 1, i, f, workspaceRoot, reportDir)
            gumbo = None()
          }
          else {
            if (line.startsWith("system implementation ")) {
              currentComponent = Some((RustParser.getNameH("system implementation", line, F, T).get, i))
            }
            else if (line.startsWith("system ")) {
              currentComponent = Some((RustParser.getNameH("system", line, F, T).get, i))
            }
            else if (line.startsWith("thread implementation ")) {
              currentComponent = Some((RustParser.getNameH("thread implementation", line, F, T).get, i))
            }
            else if (line.startsWith("thread ")) {
              currentComponent = Some((RustParser.getNameH("thread", line, F, T).get, i))
            }
            else if (line.startsWith("data implementation")) {
              currentComponent = Some((RustParser.getNameH("data implementation", line, F, T).get, i))
            }
            else if (line.startsWith("data ")) {
              currentComponent = Some((RustParser.getNameH("data", line, F, T).get, i))
            }
            else if (line.startsWith("annex GUMBO")) {
              // must be a library annex
              gumbo = Some(buildPos(i, i, f, workspaceRoot, reportDir))
            }
          }
        }
      } // line iter

      if (packageName.nonEmpty) {
        packages = packages + packageName.get ~> ModelPackageReport(components, libraryAnnexes)
        packageName = None()
      }

    } // file iter
    return packages
  }

  @pure def getArchDiagram(workspaceRoot: Os.Path): Option[Os.Path] = {
    val diagramDir = workspaceRoot / "diagrams"
    if (diagramDir.exists) {
      if ((diagramDir / "arch.svg").exists) {
        return Some(diagramDir / "arch.svg")
      } else if ((diagramDir / "arch.png").exists) {
        return Some(diagramDir / "arch.png")
      }
    }
    return None()
  }

  @pure def generateGumboReport(gumboReport: GumboReport,
                                gumboXReport: GumboXReport,
                                component: AadlThread,
                                gumboOpt: Option[GclAnnexClauseInfo],
                                workspaceDir: Os.Path,
                                rootDir: Os.Path): ST = {
    val stateReport: Option[ST] =
      if (gumboReport.stateReport.nonEmpty) {
        var s = st"""<table>
                    |<tr><th colspan=3>State Variables</th></tr>"""
        for (e <- gumboReport.stateReport.entries) {
          val modelCand = gumboOpt.get.annex.state.filter(p => p.name == e._1)
          assert (modelCand.size == 1, e.string)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)
          s =
            st"""$s
                |<tr><td>${e._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(e._2)}>Verus</a></td></tr>"""
        }
        Some(st"$s</table>")
      } else {
        None()
      }

    val methodsReport: Option[ST] =
      if(gumboReport.methodsReport.nonEmpty) {
        var s = st"""<table>
                    |<tr><th colspan=4>GUMBO Methods</th></tr>"""
        for (m <- gumboReport.methodsReport.entries) {
          val modelCand = gumboOpt.get.annex.methods.filter(p => p.method.sig.id.value == m._1)
          assert (modelCand.size == 1, m.string)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(m._1).get
          s =
            st"""$s
                |<tr><td>${m._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(m._2)}>Verus</a></td>
                |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                |</tr>"""
        }
        Some(st"$s</table>")
      }
      else {
        None()
      }

    val invariantsReport: Option[ST] = {
      None()
    }

    val integrationReport: Option[ST] =
      if (gumboReport.integrationReport.nonEmpty){
        var s = st"""<table>
                    |<tr><th colspan=4>Integration</th></tr>"""

        val threadsPortSpecs = gumboOpt.get.gclSymbolTable.integrationMap.entries.filter(p => ops.ISZOps(p._1.path).dropRight(1) == component.path)

        for(port <- threadsPortSpecs if port._1.direction == Direction.Out) {
          val specName = port._2.id
          val codegenLoc = gumboReport.integrationReport.get.guaranteesReport.get(specName).get
          val modelCand = threadsPortSpecs.filter(p => p._2.id == specName)
          assert (modelCand.size == 1, specName.string)
          val modelPos = buildPosA(modelCand(0)._2.posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"I_Guar_${port._1.identifier}").get

          s = st"""$s
                  |<tr><td>guarantee ${specName}</td>
                  |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                  |<td><a href=${createLink(codegenLoc)}>Verus</a></td>
                  |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                  |</tr>"""
        }

        for(port <- threadsPortSpecs if port._1.direction == Direction.In) {
          val specName = port._2.id
          val codegenLoc = gumboReport.integrationReport.get.assumesReport.get(specName).get

          val modelCand = threadsPortSpecs.filter(p => p._2.id == specName)
          assert (modelCand.size == 1, specName.string)
          val modelPos = buildPosA(modelCand(0)._2.posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"I_Assm_${port._1.identifier}").get

          s = st"""$s
                  |<tr><td>assume ${specName}</td>
                  |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                  |<td><a href=${createLink(codegenLoc)}>Verus</a></td>
                  |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                  |</tr>"""
        }
        Some(st"$s</table>")
      }
      else {
        None()
      }

    val intializeReport: Option[ST] =
      if (gumboReport.initializeReport.nonEmpty) {
        var s =
          st"""<table>
              |<tr><th colspan=4>Initialize</th></tr>"""
        for (i <- gumboReport.initializeReport.entries) {
          val modelCand = gumboOpt.get.annex.initializes.get.guarantees.filter(p => p.id == i._1)
          assert (modelCand.size == 1, i._1)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"initialize_${i._1}").get
          s =
            st"""$s
                |<tr><td>guarantee ${i._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(i._2)}>Verus</a></td>
                |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                |</tr>"""
        }
        Some(st"$s</table>")
      } else {
        None()
      }

    val computeReport: Option[ST] =
    if (gumboReport.computeReport.nonEmpty) {
      var s = st"""<table>
          |<tr><th colspan=4>Compute</th></tr>"""
      if (gumboReport.computeReport.get.assumesReport.nonEmpty) {
        for (a <- gumboReport.computeReport.get.assumesReport.entries) {
          val modelCand = gumboOpt.get.annex.compute.get.assumes.filter(p => p.id == a._1)
          assert(modelCand.size == 1)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"compute_spec_${a._1}_assume").get
          s =
            st"""$s
                |<tr><td>assume ${a._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(a._2)}>Verus</a></td>
                |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                |</tr>"""
        }
      }
      if (gumboReport.computeReport.get.guaranteesReport.nonEmpty) {
        for (a <- gumboReport.computeReport.get.guaranteesReport.entries) {
          val modelCand = gumboOpt.get.annex.compute.get.guarantees.filter(p => p.id == a._1)
          assert(modelCand.size == 1)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"compute_spec_${a._1}_guarantee").get
          s =
            st"""$s
                |<tr><td>guarantee ${a._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(a._2)}>Verus</a></td>
                |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                |</tr>"""
        }
      }
      if (gumboReport.computeReport.get.casesReport.nonEmpty) {
        for (a <- gumboReport.computeReport.get.casesReport.entries) {
          val modelCand = gumboOpt.get.annex.compute.get.cases.filter(p => p.id == a._1)
          assert(modelCand.size == 1)
          val modelPos = buildPosA(modelCand(0).posOpt.get, workspaceDir, rootDir)

          val gumbox = gumboXReport.gumboxMethods.get(s"compute_case_${a._1}").get
          s =
            st"""$s
                |<tr><td>case ${a._1}</td>
                |<td><a href=${createLink(modelPos)}>GUMBO</a></td>
                |<td><a href=${createLink(a._2)}>Verus</a></td>
                |<td><a href=${createLink(gumbox)}>GUMBOX</a></td>
                |</tr>"""
        }
      }
      Some(st"$s</table>")
    } else {
      None()
    }

    return(
    st"""- **GUMBO**
        |
        |    $stateReport
        |    $integrationReport
        |    $intializeReport
        |    $computeReport
        |    $methodsReport
        |    $invariantsReport""")
  }

  @pure def generateApiReport(apiReport: RustApiReport, thread: AadlThread, workspaceRoot: Os.Path, sel4OutputDir: Os.Path): ST = {
    var s = st"""<table>
                |<tr><th colspan=3>Ports</th></tr>"""

    for (a <- apiReport.developerApiReport.entries) {
      val port = thread.getPortByPath(thread.path :+ a._1).get
      val portPos = port.feature.identifier.pos.get

      val portPosRel = buildPosA(portPos, workspaceRoot, sel4OutputDir)

      s = st"""$s
              |<tr><td>${a._1}</td>
              |<td><a href=${createLink(portPosRel)}>Model</a></td>
              |<td><a href=${createLink(a._2)}>Rust API</a></td>"""
    }
    return st"$s</table>"
  }

  @pure def writeOutResource(content: ST, path: Os.Path, makeCrlf: B, verbose: B): Unit = {
    val lineSep: String = if (Os.isWin) "\r\n" else "\n" // ST render uses System.lineSep
    val replace: String = if (makeCrlf) "\r\n" else "\n"
    val c: String =
      if (lineSep == replace) content.render
      else ops.StringOps(content.render).replaceAllLiterally(lineSep, replace)
    writeOutResourceH(c, path, makeCrlf, verbose)
  }

  @pure def writeOutResourceH(content: String, path: Os.Path, makeCrlf: B,verbose: B): Unit = {
    path.writeOver(content)

    if (makeCrlf) {
      path.chmod("770")
    }

    if (verbose) {
      println(s"Wrote: $path")
    }
  }
}

@ext("MSDParser_Ext") object MSDParser {
  @pure def parse(xml: Os.Path, rootDir: Os.Path): Option[system] = $
}