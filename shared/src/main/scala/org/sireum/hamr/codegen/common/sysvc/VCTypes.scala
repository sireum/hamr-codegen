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

// `componentOpt` names the firing component when a VC is attributed to one (the
// component whose write frame is in `VC.writeSetOpt`). `mhipPairOpt` identifies an
// independence VC's MHIP pair by transition indices into `NextRelResult.transitions`
// (indices rather than component names, since one member may be a control-point
// transition that has no component).
@datatype class VCSource(val transitionIdx: Option[Z],
                         val componentOpt: Option[Name],
                         val mhipPairOpt: Option[(Z, Z)])

@datatype class VC(val kind: VCKind.Type,
                   val premises: ISZ[org.sireum.lang.ast.Exp],
                   val conclusion: ISZ[org.sireum.lang.ast.Exp],
                   val writeSetOpt: Option[WriteFrameBuilder.ComponentWriteSet],
                   val source: VCSource)
