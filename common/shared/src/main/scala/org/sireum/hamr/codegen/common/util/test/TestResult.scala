// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

@sig trait TestResource {
  def content: String
}

@datatype class ITestResource(val content: String,
                              overwrite: B,
                              makeExecutable: B,
                              makeCRLF: B) extends TestResource

@datatype class ETestResource(val content: String,
                              symlink: B) extends TestResource

@datatype class TestResult(map: Map[String, TestResource])
