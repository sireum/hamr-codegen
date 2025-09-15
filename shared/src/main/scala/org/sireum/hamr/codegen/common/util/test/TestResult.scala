// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

@sig trait TestResource {
  def content: String
}

// JSON serializer cannot serialize STs so create 'testing' versions
// which store strings instead
@datatype class TestMarker(val beginMarker: String,
                           val endMarker: String)

@datatype class ITestResource(val content: String,
                              val markers: ISZ[TestMarker],
                              val invertMarkers: B,
                              val overwrite: B,
                              val makeExecutable: B,
                              val makeCRLF: B,
                              val isDatatype: B) extends TestResource

@datatype class ETestResource(val content: String,
                              val symlink: B) extends TestResource

@datatype class TestResult(val map: Map[String, TestResource])
