// #Sireum
package org.sireum.hamr.codegen.arsit.plugin

import org.sireum._

@datatype class DefaultArsitConfigurationPlugin extends ArsitConfigurationPlugin {
  @pure override def name: String = {
    return "DefaultArsitConfigurationPlugin"
  }
}
