// #Sireum
package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{FileResource, Resource}

object CodeGenResults {
  @strictpure def empty: CodeGenResults = CodeGenResults(ISZ(), ISZ())
}

@datatype class CodeGenResults(resources: ISZ[FileResource],
                               auxResources: ISZ[Resource])
