// #Sireum
package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.{AadlTypes, ArrayType}
import org.sireum.hamr.codegen.common.util.HamrCli
import org.sireum.hamr.ir.Aadl
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

    return !reporter.hasError
  }
}
