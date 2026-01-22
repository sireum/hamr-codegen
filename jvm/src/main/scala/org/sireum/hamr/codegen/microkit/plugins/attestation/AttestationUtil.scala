// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.attestation

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.TypeIdPath
import org.sireum.hamr.codegen.common.symbols.{AnnexLibInfo, GclAnnexLibInfo}
import org.sireum.hamr.ir.{GclMethod, GclStateVar, MTransformer}
import org.sireum.lang.ast.Exp
import org.sireum.lang.{ast => AST}
import org.sireum.message.{Position => SlangPos}
import org.sireum.message.FlatPos
import org.sireum.hamr.codegen.microkit.plugins.attestation.AttestationReportContainers.{Position => AttestationPos}
import org.sireum.hamr.codegen.microkit.plugins.reporting.ReportUtil

object AttestationUtil {

  @pure def isLibraryMethod(m: AST.ResolvedInfo.Method): B = {
    return ops.ISZOps(m.owner).contains("GUMBO__Library")
  }

  @record class CollectSymbols extends MTransformer {
    var symbols: ISZ[AST.ResolvedInfo] = ISZ()

    override def pre_langastExpSelect(o: Exp.Select): MTransformer.PreResult[Exp] = {
      o.resOpt match {
        case Some(v: AST.ResolvedInfo.Var) => symbols = symbols :+ v
        case _ =>
      }
      return MTransformer.PreResult(F, MNone())
    }

    override def pre_langastExpIdent(o: AST.Exp.Ident): MTransformer.PreResult[AST.Exp] = {
      o.resOpt match {
        case Some(m: AST.ResolvedInfo.Method) => symbols = symbols :+ m
        case Some(v: AST.ResolvedInfo.Var) => symbols = symbols :+ v
        case _ =>
      }
      return MTransformer.PreResult(F, MNone())
    }
  }

  def getGumboSubclauseMethod(id: String, methods: ISZ[GclMethod]): Option[GclMethod] = {
    for (m <- methods if m.id == id) {
      return Some(m)
    }
    return None()
  }

  def getGumboLibAnnex(owner: ISZ[String], annexLibInfos: ISZ[AnnexLibInfo]): GclAnnexLibInfo = {
    for (a <- annexLibInfos) {
      a match {
        case gali: GclAnnexLibInfo if (gali.name == owner) =>
          return gali
        case _ =>
      }
    }
    halt(s"Didn't find lib annex ${owner}")
  }

  def getGumboLibAnnexMethod(id: String, methods: ISZ[GclMethod]): GclMethod = {
    for (m <- methods if (m.id == id)) {
      return m
    }
    halt(s"Didn't find lib annex method $id")
  }

  def isIndexingTypeFingerprintMethod(id: String, indexingTypes: Map[String, TypeIdPath]): B = {
    println(indexingTypes.keys)
    println(id)

    return indexingTypes.contains(id)
  }

  def toPos(p: SlangPos, workspaceDir: Os.Path, reportDir: Os.Path): AttestationPos = {
    assert (p.beginLine != -1)
    assert (p.endLine != -1)
    assert (p.beginLine <= p.endLine)

    val file = Os.path(p.uriOpt.get)

    val f: Os.Path =
      if (file.exists) {
        file
      } else {

        val nameo = ops.StringOps(ReportUtil.deWin(file.value))
        val isAadl = nameo.endsWith(".aadl")

        if (nameo.startsWith("file")) {
          // probably from sysml
          Os.Path.fromUri(nameo.s)
        } else if (!isAadl) {
          // must be a microkit artifact
          workspaceDir / nameo.s
        }
        else {
          // probably from osate so drop the aadl project prefix
          // e.g. /isolette-artifacts-sel4/aadl/packages/Regulate.aadl
          assert(nameo.startsWith("/"), nameo.s)
          val sub = nameo.substring(nameo.indexOfFrom('/', 1) + 1, nameo.size)
          workspaceDir / sub
        }
      }

    assert(f.exists, f.value)
    val rel = ops.StringOps(reportDir.relativize(f).value).replaceAllLiterally("/", "\\/")

    p match {
      case f: FlatPos =>
        return AttestationReportContainers.Position(
          uri = rel,
          beginLine = f.beginLine,
          beginCol = f.beginColumn,
          endLine = f.endLine,
          endCol = f.endColumn,
          offset = f.offset,
          length = f.length)
      case _ =>
        halt(s"Not expecting position $p")
    }
  }

  def getGumboStateVariable(id: String, stateVars: ISZ[GclStateVar]): GclStateVar = {
    for (s <- stateVars if s.id == id) {
      return s
    }
    halt(s"Didn't find gumbo state variable ${id}")
  }
}

