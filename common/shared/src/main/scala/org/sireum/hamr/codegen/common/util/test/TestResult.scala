// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

@datatype class TestResource(content: String,
                             overwrite: B,
                             makeExecutable: B)

@datatype class TestResult(map: Map[String, TestResource])
