// #Sireum
package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.Resource

object CodeGenResults {
  @strictpure def empty: CodeGenResults = CodeGenResults(ISZ())
}

@datatype class CodeGenResults(resources: ISZ[Resource])
