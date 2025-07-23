// #Sireum
package org.sireum.hamr.codegen.common.reporting

import org.sireum._
import org.sireum.message.Position
import org.sireum.hamr.codegen.common.CommonUtil.IdPath

@datatype class ModelReport(val system: ComponentReport,
                            val threads: ISZ[ComponentReport]) extends CodegenReport

@datatype class ComponentReport(val path: IdPath,

                                val componentType: ISZ[String],
                                val componentTypePosition: Position,

                                val componentImplementation: ISZ[String],
                                val componentImplementationPos: Position,

                                val properties: ISZ[(String, String)])