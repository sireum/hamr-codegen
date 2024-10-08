// #Sireum
package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.containers.{FileResource, Resource}

@datatype class CodeGenResults(resources: ISZ[FileResource],
                               auxResources: ISZ[Resource])
