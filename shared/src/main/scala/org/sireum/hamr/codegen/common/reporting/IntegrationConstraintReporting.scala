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

  @datatype class GclIntegerationConstraint(val id: String,
                                            val descriptor: Option[String],
                                            val position: Position)

  @datatype class IntegrationConstraint(val srcPort: String,
                                        val srcPortPos: Position,
                                        val srcPortIntegrationConstraint: Option[GclIntegerationConstraint],

                                        val dstPort: String,
                                        val dstPortPos: Position,
                                        val dstPortIntegrationConstraint: GclIntegerationConstraint,

                                        val connectionMidPoint: Position,

                                        val claim: String,

                                        val smt2QueryResult: Smt2QueryResult.Type,

                                        val logikaMessage: String,

                                        val smt2Query: String)
}
