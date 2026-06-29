// #Sireum
package org.sireum.hamr.codegen.common.sysvc

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols.AadlPort
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

// One integration-constraint VC per connected port pair whose destination
// in-port carries an integration assume. It states that the sender's
// integration guarantee on the source out-port (or `true`, when the sender
// declares none) implies the receiver's integration assume on the destination
// in-port, both evaluated over the single value the connection carries. This is
// a static, schedule- and property-independent obligation, generated once per
// composition crate (like the Commutativity VCs). `srcPort`/`dstPort` are the
// resolved endpoint ports (used to recover the connection's data type and to key
// the integration map); `srcGuaranteeId` is None when the sender has no
// integration guarantee on the source port (premise `true`).
@datatype class IntegrationVC(val connName: String,
                              val srcCompPath: IdPath,
                              val srcPort: AadlPort,
                              val srcGuaranteeId: Option[String],
                              val dstCompPath: IdPath,
                              val dstPort: AadlPort,
                              val dstAssumeId: String)
