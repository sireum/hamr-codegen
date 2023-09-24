// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.N._

object ExperimentalOptions {

  val USE_CASE_CONNECTORS: String = "USE_CASE_CONNECTORS"
  val GENERATE_DOT_GRAPHS: String = "GENERATE_DOT_GRAPHS"
  val GENERATE_REFINEMENT_PROOF: String = "GENERATE_REFINEMENT_PROOF"
  val PROCESS_BTS_NODES: String = "PROCESS_BTS_NODES"
  val DISABLE_SERGEN: String = "DISABLE_SERGEN"
  val DISABLE_SLANG_CHECK: String = "DISABLE_SLANG_CHECK"

  val ADD_CONNECTION_IDS: String = "ADD_CONNECTION_IDS"
  val ADD_COMPONENT_IDS: String = "ADD_COMPONENT_IDS"
  val ADD_PORT_IDS: String = "ADD_PORT_IDS"

  def addConnectionIds(experimentalOptions: ISZ[String]): Z = {
    for (e <- experimentalOptions) {
      val o = ops.StringOps(e)
      if (o.startsWith(s"${ADD_CONNECTION_IDS}=")) {
        Z(o.split(c => c == '=')(1)) match {
          case Some(c) => return c
          case _ => return z"0"
        }
      }
    }
    return z"0"
  }

  def addPortIds(experimentalOptions: ISZ[String]): Z = {
    for (e <- experimentalOptions) {
      val o = ops.StringOps(e)
      if (o.startsWith(s"${ADD_PORT_IDS}=")) {
        Z(o.split(c => c == '=')(1)) match {
          case Some(c) => return c
          case _ => return z"0"
        }
      }
    }
    return z"0"
  }

  def addComponentIds(experimentalOptions: ISZ[String]): Z = {
    for (e <- experimentalOptions) {
      val o = ops.StringOps(e)
      if (o.startsWith(s"${ADD_COMPONENT_IDS}=")) {
        Z(o.split(c => c == '=')(1)) match {
          case Some(c) => return c
          case _ => return z"0"
        }
      }
    }
    return z"0"
  }

  def disableSergen(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == DISABLE_SERGEN)
  }

  def disableSlangCheck(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == DISABLE_SLANG_CHECK)
  }

  def processBtsNodes(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == PROCESS_BTS_NODES)
  }

  def useCaseConnectors(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == USE_CASE_CONNECTORS)
  }

  def generateDotGraphs(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == GENERATE_DOT_GRAPHS)
  }

  def generateRefinementProof(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == GENERATE_REFINEMENT_PROOF)
  }
}
