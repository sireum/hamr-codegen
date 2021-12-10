// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlData, AadlPort, AadlSymbol, GclKey, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, RecordType}
import org.sireum.hamr.ir.{GclAccessExp, GclAnnex, GclBinaryExp, GclExp, GclLiteralExp, GclLiteralType, GclNameExp, GclSubclause, GclUnaryExp, Name}
import org.sireum.message.Reporter

@record class GclResolver() {
  var symbols: Map[GclKey, AadlSymbol] = Map.empty
  var expTypes: HashMap[GclExp, AadlType] = HashMap.empty

  def fetchSubcomponent(name: Name, context: AadlComponent): Option[AadlComponent] = {
    val n = CommonUtil.getName(name)
    for(sc <-context.subComponents if sc.identifier == n) {
      return Some(sc)
    }
    return None()
  }

  def isSubcomponent(name: Name, context: AadlComponent): B = {
    return !fetchSubcomponent(name, context).isEmpty
  }

  def fetchPort(name: Name, context: AadlComponent): Option[AadlPort] = {
    val n = CommonUtil.getName(name)
    for(p <- context.getPorts() if p.identifier == n) {
      return Some(p)
    }
    return None()
  }

  def isPort(name: Name, context: AadlComponent): B = {
    return !fetchPort(name, context).isEmpty
  }

  def processGclAnnex(context: AadlComponent,
                      annex: GclAnnex,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      reporter: Reporter): Option[GclSymbolTable] = {

    val baseTypeBoolean = aadlTypes.typeMap.get("Base_Types::Boolean").get
    val baseTypeInteger = aadlTypes.typeMap.get("Base_Types::Integer").get
    val baseTypeString = aadlTypes.typeMap.get("Base_Types::String").get
    val baseTypeFloat = aadlTypes.typeMap.get("Base_Types::Float").get
    val baseTypeFloat32 = aadlTypes.typeMap.get("Base_Types::Float_32").get
    val baseTypeFloat64 = aadlTypes.typeMap.get("Base_Types::Float_64").get

    def isBool(a: AadlType): B = {
      return a == baseTypeBoolean
    }

    def isString(aadlType: AadlType): B = {
      return aadlType == baseTypeString
    }

    def isNumeric(aadlType: AadlType): B = {
      return isInteger(aadlType) || isReal(aadlType)
    }

    def isInteger(a: AadlType): B = {
      return a.name == baseTypeInteger.name
    }

    def isReal(a: AadlType): B = {
      return a.name == baseTypeFloat.name || a.name == baseTypeFloat32.name || a.name == baseTypeFloat64.name
    }

    def unifyInteger(a: AadlType, b: AadlType): Option[AadlType] = {
      return Some(a)
    }

    def unifyReal(a: AadlType, b: AadlType): Option[AadlType] = {
      return Some(a)
    }

    def unify(a: AadlType, b: AadlType): Option[AadlType] = {
      if (isBool(a) && isBool(b)) {
        return Some(a)
      } else if (isString(a) && isString(b)) {
        return Some(a)
      } else if (isNumeric(a) && isNumeric(b)) {
        return if (isInteger(a) && isInteger(b)) unifyInteger(a, b)
        else if (isReal(a) && isReal(b)) unifyReal(a, b)
        else Some(baseTypeFloat)
      } else {
        return None()
      }
    }

    def visitExp(exp: GclExp): AadlType = {
      exp match {
        case lit: GclLiteralExp =>
          val t: AadlType = lit.typ match {
            case GclLiteralType.Boolean => baseTypeBoolean
            case GclLiteralType.String => baseTypeString
            case GclLiteralType.Integer => baseTypeInteger
            case GclLiteralType.Real => baseTypeFloat
          }
          expTypes = expTypes + (exp ~> t)
          return t
        case be: GclBinaryExp =>
          val lhs = visitExp(be.lhs)
          val rhs = visitExp(be.rhs)
          unify(lhs, rhs) match {
            case Some(unified) =>
              expTypes = expTypes + (be ~> unified)
              return unified
            case _ =>
              println(lhs)
              println(rhs)
              println(exp)
              reporter.error(exp.pos, "", s"Could not unify type for binary expression ${exp}")
              return rhs
          }
        case ue: GclUnaryExp =>
          val et = visitExp(ue.exp)
          expTypes = expTypes + (ue ~> et)
          return et
        case ne: GclNameExp =>
          val name = ne.name
          if(name.name.size == 1) {
            if(isPort(name, context)) {
              halt("todo")
            } else if(isSubcomponent(name, context)) {
              val sc = fetchSubcomponent(name, context).get
               sc match {
                case ad: AadlData =>
                  val typ = ad.typ
                  expTypes = expTypes + (ne ~> typ)
                  return typ
                case x =>
                  halt(s"hi ${x}")
              }
            } else {
              halt(s"todo ${name}")
            }
          } else {
            halt(s"todo ${ne}")
          }
        case ae: GclAccessExp =>
          val e = visitExp(ae.exp)

          e match {
            case r: RecordType =>
              val typ = r.fields.get(ae.attributeName).get
              expTypes = expTypes + (ae ~> typ)
              return typ

            case x => halt(s"need to handle handle ${x}")
          }

        case x => halt(s"Need to handle exprs of type $x")
      }
    }

    annex match {
      case g: GclSubclause =>
        assert(g.state.isEmpty)

        for (i <- g.invariants) {
          visitExp(i.exp)
        }
        assert(g.initializes.isEmpty)
        assert(g.integration.isEmpty)
        assert(g.compute.isEmpty)

        return Some(GclSymbolTable(symbols, expTypes))
      case x =>
        halt(s"Not handling ${x}")
    }
  }
}
