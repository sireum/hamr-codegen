// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlData, AadlPort, AadlSymbol, GclKey, GclSymbolTable, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, RecordType}
import org.sireum.hamr.ir.{GclAccessExp, GclAnnex, GclBinaryExp, GclBinaryOp, GclExp, GclInvariant, GclLiteralExp, GclLiteralType, GclNameExp, GclSubclause, GclUnaryExp, GclUnaryOp, Name}
import org.sireum.message.Reporter

@record class GclResolver() {
  var symbols: Map[GclKey, AadlSymbol] = Map.empty
  var expTypes: HashMap[GclExp, AadlType] = HashMap.empty

  def fetchSubcomponent(name: Name, context: AadlComponent): Option[AadlComponent] = {
    val n = CommonUtil.getName(name)
    for (sc <- context.subComponents if sc.identifier == n) {
      return Some(sc)
    }
    return None()
  }

  def isSubcomponent(name: Name, context: AadlComponent): B = {
    return !fetchSubcomponent(name, context).isEmpty
  }

  def fetchPort(name: Name, context: AadlComponent): Option[AadlPort] = {
    val n = CommonUtil.getName(name)
    for (p <- context.getPorts() if p.identifier == n) {
      return Some(p)
    }
    return None()
  }

  def isPort(name: Name, context: AadlComponent): B = {
    return !fetchPort(name, context).isEmpty
  }

  def isBoolUnaryOp(op: GclUnaryOp.Type): B = {
    val ret: B = op match {
      case GclUnaryOp.Abs => F
      case GclUnaryOp.Neg => F
      case GclUnaryOp.Not => T
    }
    return ret
  }

  def isArithUnaryOp(op: GclUnaryOp.Type): B = {
    return !isBoolUnaryOp(op)
  }

  def isLogicalBinaryOp(op: GclBinaryOp.Type): B = {
    val ret: B = op match {
      case GclBinaryOp.And => T
      case GclBinaryOp.AndThen => T
      case GclBinaryOp.Or => T
      case GclBinaryOp.OrElse => T
      case GclBinaryOp.Xor => T
      case GclBinaryOp.Implies => T
      case GclBinaryOp.Equiv => T

      case GclBinaryOp.Eq => T
      case GclBinaryOp.Neq => T

      case _ => F
    }
    return ret
  }

  def isArithBinaryOp(op: GclBinaryOp.Type): B = {
    val ret: B = op match {
      case GclBinaryOp.Plus => T
      case GclBinaryOp.Minus => T
      case GclBinaryOp.Div => T
      case GclBinaryOp.Mult => T
      case GclBinaryOp.Mod => T
      case GclBinaryOp.Rem => T
      case GclBinaryOp.Exp => T

      case _ => F
    }
    return ret
  }

  def isRelationalBinaryOp(op: GclBinaryOp.Type): B = {
    val ret: B = op match {
      case GclBinaryOp.Eq => T
      case GclBinaryOp.Neq => T
      case GclBinaryOp.Lt => T
      case GclBinaryOp.Lte => T
      case GclBinaryOp.Gt => T
      case GclBinaryOp.Gte => T

      case _ => F
    }
    return ret
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
      return a.name == baseTypeBoolean.name
    }

    def isString(aadlType: AadlType): B = {
      return aadlType.name == baseTypeString.name
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
      // TODO
      return Some(a)
    }

    def unifyReal(a: AadlType, b: AadlType): Option[AadlType] = {
      // TODO
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
        else Some(baseTypeFloat) // TODO, mixed real/ints
      } else {
        return None()
      }
    }

    def visitExp(exp: GclExp): AadlType = {
      exp match {
        case lit: GclLiteralExp =>
          return visitLiteralExp(lit)
        case be: GclBinaryExp =>
          val lhs = visitExp(be.lhs)
          val rhs = visitExp(be.rhs)

          unify(lhs, rhs) match {
            case Some(unified) =>
              val beType: AadlType = {

                if(isLogicalBinaryOp(be.op) && isBool(unified)) {
                  baseTypeBoolean
                } else if (isArithBinaryOp(be.op) && isNumeric(unified)) {
                  unified
                } else if (isRelationalBinaryOp(be.op) && isNumeric(unified)) {
                  baseTypeBoolean
                } else {
                  reporter.error(exp.pos, "", s"Binary operator ${be.op} cannot be applied to expressions of type ${lhs.name} and ${rhs.name}")
                  baseTypeBoolean // dummy type as type-checking failed
                }
              }

              expTypes = expTypes + (be ~> beType)

              return beType
            case _ =>
              println(lhs)
              println(rhs)
              println(exp)
              reporter.error(exp.pos, "", s"Could not unify type for binary expression ${exp}")
              return rhs
          }
        case ue: GclUnaryExp =>
          val et = visitExp(ue.exp)

          if(isBoolUnaryOp(ue.op) && !isBool(et)) {
            reporter.error(exp.pos, "", s"Unary operator ${ue.op} cannot be applied to ${et.name}")
          }

          if(isArithUnaryOp(ue.op) && !isNumeric(et)) {
            reporter.error(exp.pos, "", s"Unary operator ${ue.op} cannot be applied to ${et.name}")
          }

          expTypes = expTypes + (ue ~> et)
          return et
        case ne: GclNameExp =>
          return visitNameExp(ne)
        case ae: GclAccessExp =>
          return visitAccessExp(ae)

        case x => halt(s"TODO: need to handle exps of type $x")
      }
    }

    def visitNameExp(ne: GclNameExp): AadlType = {
      val name = ne.name
      if (name.name.size == 1) {
        if (isPort(name, context)) {
          halt("TODO: handle port refs")
        } else if (isSubcomponent(name, context)) {
          val sc = fetchSubcomponent(name, context).get
          sc match {
            case ad: AadlData =>
              val typ = ad.typ
              expTypes = expTypes + (ne ~> typ)
              return typ
            case x =>
              halt(s"TODO: Accessing subcomponent ${sc.identifier} but it's not a data component")
          }
        } else {
          halt(s"TODO: name exp isn't a port or subcomponent: ${ne}")
        }
      } else {
        halt(s"TODO: handle external refs: ${ne}")
      }
    }

    def visitAccessExp(ae: GclAccessExp): AadlType = {
      val e = visitExp(ae.exp)

      e match {
        case r: RecordType =>
          val fieldType = r.fields.get(ae.attributeName).get
          expTypes = expTypes + (ae ~> fieldType)
          return fieldType

        case x => halt(s"TODO: need to handle non record type access exps: ${x}")
      }
    }

    def visitLiteralExp(exp: GclLiteralExp): AadlType = {
      // TODO: refine real/int
      val t: AadlType = exp.typ match {
        case GclLiteralType.Boolean => baseTypeBoolean
        case GclLiteralType.String => baseTypeString
        case GclLiteralType.Integer => baseTypeInteger
        case GclLiteralType.Real => baseTypeFloat
      }
      expTypes = expTypes + (exp ~> t)
      return t
    }

    def visitGclSubclause(s: GclSubclause): Unit = {
      var seenInvariantNames: Set[String] = Set.empty

      for (i <- s.invariants) {
        if(seenInvariantNames.contains(i.name)) {
          reporter.error(i.exp.pos, "", s"Duplicate invariant name: ${i.name}")
        }
        seenInvariantNames = seenInvariantNames + i.name
        visitInvariant(i)
      }

      assert(s.state.isEmpty, "not yet")
      assert(s.initializes.isEmpty, "not yet")
      assert(s.integration.isEmpty, "not yet")
      assert(s.compute.isEmpty, "not yet")
    }

    def visitInvariant(i: GclInvariant): Unit = {
      val r = visitExp(i.exp)
      if(r.name != baseTypeBoolean.name) {
        reporter.error(i.exp.pos, "", "Invariant expressions must be boolean")
      }
    }

    annex match {
      case g: GclSubclause =>
        visitGclSubclause(g)
        return Some(GclSymbolTable(symbols, expTypes))
      case x =>
        halt(s"TODO: need to handle gcl annex type: ${x}")
    }
  }
}
