// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.ir.Name

@enum object VCKind {
  "InitState"
  "PreAssert"
  "NextAssertTask"
  "NextAssertSkip"
  "PostPre"
  "NonBlocking"
  "Preservation"
  "Commutativity"
}

@datatype class VCSource(val transitionIdx: Option[Z],
                         val componentOpt: Option[Name],
                         val mhipPairOpt: Option[(Name, Name)])

@datatype class VC(val kind: VCKind.Type,
                   val premises: ISZ[org.sireum.lang.ast.Exp],
                   val conclusion: ISZ[org.sireum.lang.ast.Exp],
                   val writeSetOpt: Option[WriteFrameBuilder.ComponentWriteSet],
                   val source: VCSource)
