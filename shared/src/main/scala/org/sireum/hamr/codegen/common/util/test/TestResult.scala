// #Sireum

package org.sireum.hamr.codegen.common.util.test

import org.sireum._

@sig trait TestResource {
  def content: String
}

// JSON serializer cannot serialize STs so create 'testing' versions
// which store strings instead

@sig trait TestMarker {
  @strictpure def id: String
}

@datatype class TestPlaceholderMarker(val id: String) extends TestMarker

@datatype class TestBlockMarker(val id: String,

                                val beginPrefix: String,
                                val optBeginSuffix: Option[String],

                                val endPrefix: String,
                                val optEndSuffix: Option[String]) extends TestMarker

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
