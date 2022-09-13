// #Sireum

package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.{BTSVariable => BTSVariableSym}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir._
import org.sireum.message.Reporter

@record class BTSResolver extends AnnexVisitor {

  def processBTSAnnex(a: BLESSAnnex,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      reporter: Reporter): Option[BTSSymbolTable] = {
    a match {
      case o: BTSBLESSAnnexClause =>
        val _states = Map[String, BTSState](o.states.map(m => {
          val name = CommonUtil.getName(m.id)
          (name, BTSState(name, m))
        }))

        var _variables: Map[IdPath, BTSVariableSym] = Map.empty
        for (o <- o.variables) {
          val path = o.name.name

          o.varType match {
            case BTSClassifier(classifier) =>
              aadlTypes.typeMap.get(classifier.name) match {
                case Some(t) =>
                  _variables = _variables + (path ~> BTSVariableSym(path, t, o))
                case _ =>
                  reporter.error(o.name.pos, CommonUtil.toolName,
                    s"Couldn't resolve AADL type '${classifier.name}' for variable ${path}")
              }

            case x =>
              reporter.error(o.name.pos, CommonUtil.toolName,
                s"Currently only handling BTSClassifiers but ${path} has type ${x}")
          }
        }

        if (reporter.hasError) {
          return None()
        } else {
          val bst = BTSExpResolver(symbolTable, aadlTypes, _states, _variables).resolve(o, reporter)
          return Some(BTSSymbolTable(bst._1, bst._2, bst._3, _states, _variables))
        }

      case x =>
        reporter.error(None(), CommonUtil.toolName, s"Unexpected BTS annex type ${x}")
        return None()
    }
  }

  var seenAnnexes: Set[Annex] = Set.empty

  def reset: B = {
    seenAnnexes = Set.empty
    return T
  }

  def offer(context: AadlComponent, annex: Annex, annexLibs: ISZ[AnnexLibInfo], symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): Option[AnnexClauseInfo] = {
    if (!seenAnnexes.contains(annex)) {
      seenAnnexes = seenAnnexes + annex
      annex.clause match {
        case b: BTSBLESSAnnexClause =>
          val btsSymTable = processBTSAnnex(b, symbolTable, aadlTypes, reporter).get
          return Some(BTSAnnexInfo(b, btsSymTable))
        case _ =>
      }
    }
    return None()
  }

  def offerLibraries(annexLibs: ISZ[AnnexLib], symbolTable: SymbolTable, aadlTypes: AadlTypes, reporter: Reporter): ISZ[AnnexLibInfo] = {
    return ISZ()
  }
}
