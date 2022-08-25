// #Sireum
package org.sireum.hamr.codegen.common.resolvers

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir.{BTSAccessExp, BTSAssignmentAction, BTSBLESSAnnexClause, BTSBinaryExp, BTSExp, BTSLiteralExp, BTSLiteralType, BTSNameExp, BTSPortOutAction, BTSSubprogramCallAction, BTSUnaryExp, MTransformer => MAirTransformer}
import org.sireum.message.Reporter

@record class BTSExpResolver(symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,
                             states: Map[String, BTSState],
                             variables: Map[IdPath, BTSVariable]) {

  def resolve(annex: BTSBLESSAnnexClause, reporter: Reporter):
  (BTSBLESSAnnexClause, Map[BTSKey, AadlSymbol], HashMap[BTSExp, AadlType]) = {

    var m: Map[BTSKey, AadlSymbol] = Map.empty

    val expTypes = ExpWalker(symbolTable, aadlTypes, states, variables)
    val results: BTSBLESSAnnexClause = expTypes.transformBTSBLESSAnnexClause(annex) match {
      case MSome(rewrittenAnnex) => rewrittenAnnex
      case _ => annex
    }

    reporter.setMessages(expTypes.reporter.messages)

    return (results, m, expTypes.expType)
  }
}

@record class ExpWalker(symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        states: Map[String, BTSState],
                        variables: Map[IdPath, BTSVariable]) extends MAirTransformer {

  val reporter: Reporter = Reporter.create

  var expType: HashMap[BTSExp, AadlType] = HashMap.empty

  override def postBTSPortOutAction(o: BTSPortOutAction): MOption[BTSPortOutAction] = {
    o.exp match {
      case Some(exp) =>
        val path = o.name.name
        symbolTable.featureMap.get(path) match {
          case Some(a: AadlFeatureData) =>
            val evalledType = expType.get(exp).get
            if (a.aadlType != evalledType) {
              val simplePortName = CommonUtil.getLastName(o.name)
              reporter.error(exp.pos, CommonUtil.toolName,
                s"Expecting type ${a.aadlType.name} for port output on ${simplePortName}, but found ${evalledType.name}"
              )
              println()
            }
          case x =>
            halt(s"what is this feature ${x}")
        }

      case _ =>
    }

    return MNone()
  }

  override def postBTSAssignmentAction(o: BTSAssignmentAction): MOption[BTSAssignmentAction] = {
    expType.get(o.lhs) match {
      case Some(lhsType) =>
        expType.get(o.rhs) match {
          case Some(rhsType) =>
            if (lhsType != rhsType) {
              reporter.error(o.rhs.pos, CommonUtil.toolName,
                s"Type mismatch for assignment action: ${lhsType.name} vs ${rhsType.name} for ${o}")
            }
          case _ =>
            halt(s"Couldn't determine type for rhs of ${o}")
        }
      case _ => halt(s"No type found for lhs of ${o}")
    }
    return MNone()
  }

  override def postBTSSubprogramCallAction(o: BTSSubprogramCallAction): MOption[BTSSubprogramCallAction] = {
    val path = o.name.name
    val s = symbolTable.componentMap.get(path).get.asInstanceOf[AadlSubprogram]

    assert(s.parameters.size == o.params.size, s"Sizes don't match ${s.parameters.size} vs ${o.params.size}")

    for (i <- 0 until s.parameters.size) {
      val expectedType = s.parameters(i).aadlType
      val paramPair = o.params(i)
      paramPair.exp match {
        case Some(e) =>
          expType.get(e) match {
            case Some(pType) =>
              if (expectedType != pType) {
                reporter.error(None(), CommonUtil.toolName,
                  s"Expecting type ${expectedType.name} but found ${pType.name} for ${paramPair}")
              }
            case _ =>
              halt(s"no type found for ${paramPair}")
          }
        case _ => halt(s"How can the exp be empty? ${paramPair}")
      }
    }
    return MNone()
  }

  override def postBTSAccessExp(o: BTSAccessExp): MOption[BTSAccessExp] = {
    expType.get(o.exp) match {
      case Some(a: RecordType) =>
        a.fields.get(o.attributeName) match {
          case Some(attributeType) => expType = expType + (o ~> attributeType)
          case _ =>
            reporter.error(o.pos, CommonUtil.toolName,
              s"Attribute name ${o.attributeName} not found in ${a.name}")
        }
      case Some(e: EnumType) =>
        if (!ops.ISZOps(e.values).contains(o.attributeName)) {
          reporter.error(o.pos, CommonUtil.toolName,
            s"Enum value ${o.attributeName} is not a member of ${e.name}")
        }
        expType = expType + (o ~> e)
      case _ => halt(s"Type not resolved for accessed exp ${o}")
    }

    return MNone()
  }

  override def postBTSBinaryExp(o: BTSBinaryExp): MOption[BTSBinaryExp] = {

    val lhsType = expType.get(o.lhs).get
    val rhsType = expType.get(o.rhs).get

    if (lhsType != rhsType) {
      reporter.error(o.pos, CommonUtil.toolName,
        s"Types do not match: ${lhsType.name} vs ${rhsType.name} for ${o}")
    }

    expType = expType + (o ~> lhsType)
    return MNone()
  }

  override def postBTSUnaryExp(o: BTSUnaryExp): MOption[BTSUnaryExp] = {
    val typ = expType.get(o.exp).get
    expType = expType + (o ~> typ)
    return MNone()
  }

  override def postBTSLiteralExp(o: BTSLiteralExp): MOption[BTSLiteralExp] = {
    val typ: AadlType = o.typ match {
      case BTSLiteralType.BOOLEAN => aadlTypes.typeMap.get("Base_Types::Boolean").get
      case BTSLiteralType.STRING => {
        val segments = ops.StringOps(o.exp).split(c => c == '$')
        if (segments.size == 2) {
          segments(0) match {
            case "u16" =>

              val rewriteExp = BTSLiteralExp(BTSLiteralType.INTEGER, segments(1), o.pos)
              expType = expType + (rewriteExp ~> aadlTypes.typeMap.get("Base_Types::Unsigned_16").get)

              return MSome(rewriteExp)
            case _ =>
          }
        }
        aadlTypes.typeMap.get("Base_Types::String").get
      }
      case BTSLiteralType.INTEGER => aadlTypes.typeMap.get("Base_Types::Integer").get
      case BTSLiteralType.FLOAT => aadlTypes.typeMap.get("Base_Types::Float").get
      case _ => halt(s"Unexpected bts literal type ${o.typ}")
    }
    expType = expType + (o ~> typ)
    return MNone()
  }

  override def postBTSNameExp(o: BTSNameExp): MOption[BTSNameExp] = {
    val path = o.name.name

    if (variables.contains(path)) {
      // bts variable
      expType = expType + (o ~> variables.get(path).get.typ)
    } else if (symbolTable.featureMap.contains(path)) {
      // port??
      symbolTable.featureMap.get(path).get match {
        case afd: AadlFeatureData => expType = expType + (o ~> afd.aadlType)
        case aep: AadlEventPort => expType = expType + (o ~> TypeUtil.EmptyType)
        case x => halt(s"Need to resolve type for feature $x")
      }
    } else {
      val id = CommonUtil.getName(o.name)
      if (aadlTypes.typeMap.contains(id)) {
        aadlTypes.typeMap.get(id) match {
          case Some(et: EnumType) => expType = expType + (o ~> et)
          case x =>
            halt(s"Hmm, this doesn't look like an enum ${x}")
        }
      } else {
        halt(s"Need to resolve type for ${o}")
      }
    }
    return MNone()
  }
}