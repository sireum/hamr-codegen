// #Sireum
package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.message.Position

object IntegrationConstraintReporting {
  @enum object Smt2QueryResult {
    "Unsat"
    "Sat"
    "Unknown"
    "Timeout"
    "Error"
  }

  @datatype class IntegrationConstraint(val srcPort: String,
                                        val srcPortPos: Position,
                                        val srcPortIntegrationConstraintsPosition: Option[Position],

                                        val dstPort: String,
                                        val dstPortPos: Position,
                                        val dstPortIntegrationConstraintsPosition: Position,

                                        val connectionMidPoint: Position,

                                        val claim: String,

                                        val smt2QueryResult: Smt2QueryResult.Type,

                                        val logikaMessage: String,

                                        val smt2Query: String)
}
