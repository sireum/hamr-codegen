// #Sireum
package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlEventDataPort, AadlEventPort, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object RosLinter {
  def lint(model: Aadl, options: HamrCli.CodegenOption,
           aadlTypes: AadlTypes, symbolTable: SymbolTable,
           plugins: ISZ[Plugin], store: Store, reporter: Reporter): B = {

    for (t <- aadlTypes.typeMap.values) {
      t match {
        case a: ArrayType =>
          if (a.dimensions.size > 1) {
            for (dim <- a.dimensions if dim == 0) {
              reporter.error(None(), Ros2Codegen.toolName, s"Invalid array definition '${a.name}'. Nested unbounded arrays are not supported in Ros2")
            }
          }
        case _ =>
      }
    }

    checkQueueSize(symbolTable, reporter)

    return !reporter.hasError
  }

  // Queue_Size reaches AIR intact and is then realized by neither backend: the rclcpp path
  // hardcodes a QoS depth of 1 and strict's enqueue is likewise depth 1, and the micro-ROS path
  // records a single pending arrival per port.  Only the AADL default of 1 is realized.
  //
  // This is an ERROR rather than a warning, and it lives in the linter rather than beside the
  // generators, for the same reason: the linter runs only when ROS 2 is the selected target.
  // Queue_Size is a legitimate AADL property that other backends honour, so rejecting a larger
  // depth outright would be wrong as a general judgement about the model -- but it is the right
  // judgement about generating *this* target from it, where the alternative is emitting a system
  // that silently drops what the model says it should buffer.
  //
  // Note that reporting this as an error anywhere further downstream would not actually stop
  // generation: Ros2Codegen's only hasError gate precedes the per-thread checks.  Here the gate
  // is this function's own return value.
  def checkQueueSize(symbolTable: SymbolTable, reporter: Reporter): Unit = {
    for (thread <- symbolTable.getThreads() if !RosUtil.isPlatformProvidedComponent(thread);
         p <- thread.getPorts() if p.direction == Direction.In && !RosUtil.isInfrastructureRealized(p)) {
      val declared: Z = p match {
        case e: AadlEventDataPort => e.queueSize
        case e: AadlEventPort => e.queueSize
        case _ => 1
      }
      if (declared > 1) {
        reporter.error(p.posOpt, Ros2Codegen.toolName,
          st"""${thread.identifier}.${p.identifier} declares Queue_Size ${declared}, but the ROS 2 backend
              |realizes only the AADL default of 1, on rclcpp and micro-ROS nodes alike.  Generating
              |from this model would produce a port that holds the most recent arrival and drops the
              |rest -- a system quietly weaker than the model it came from.  Drop the declaration, or
              |give the receiving thread a Sporadic dispatch protocol so each arrival is handled as
              |it lands.""".render)
      }
    }
  }
}
