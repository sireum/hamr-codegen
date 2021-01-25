// #Sireum

package org.sireum.hamr.codegen.common.util

import org.sireum._

object ExperimentalOptions {

  val USE_CASE_CONNECTORS: String = "USE_CASE_CONNECTORS"
  val GENERATE_DOT_GRAPHS: String = "GENERATE_DOT_GRAPHS"
  val PROCESS_BTS_NODES: String = "PROCESS_BTS_NODES"

  def processBtsNodes(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == PROCESS_BTS_NODES)
  }

  def useCaseConnectors(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == USE_CASE_CONNECTORS)
  }

  def generateDotGraphs(experimentalOptions: ISZ[String]): B = {
    return ops.ISZOps(experimentalOptions).exists(e => e == GENERATE_DOT_GRAPHS)
  }
}
