//# Sireum
package org.sireum.hamr.codegen.common.symbols

import org.sireum._

@sig trait BTSSymbol extends AadlSymbol

@datatype class BTSState(x: Z) extends BTSSymbol

@datatype class BTSVariable(y: B) extends BTSSymbol
