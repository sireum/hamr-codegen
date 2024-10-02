// #Sireum
package org.sireum.hamr.arsit.bts

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorProviderPlugin
import org.sireum.hamr.codegen.common.containers.{EResource, IResource, FileResource}
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir._
import org.sireum.message.Reporter

object BlessBehaviorProviderPlugin {
  def getBTSSubclauseBehaviorProvider(component: AadlThreadOrDevice): ISZ[BTSSubclauseBehaviorProvider] = {
    return component.annexes().filter((p: Annex) =>
      p.clause.isInstanceOf[BTSSubclauseBehaviorProvider]).map((m: Annex) =>
      m.clause.asInstanceOf[BTSSubclauseBehaviorProvider])
  }
}

@record class BlessBehaviorProviderPlugin extends BehaviorProviderPlugin {

  @pure def name: String = {
    return "BLESS Behavior Provider Provider"
  }

  @pure def canHandleBehaviorProvider(component: AadlThreadOrDevice,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = {
    return BlessBehaviorProviderPlugin.getBTSSubclauseBehaviorProvider(component).nonEmpty
  }

  override def handleBehaviorProvider(component: AadlThreadOrDevice,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                      filename: String,
                                      componentDirectory: ISZ[String],
                                      symbolTable: SymbolTable,
                                      aadlTypes: AadlTypes,
                                      reporter: Reporter): ISZ[FileResource] = {

    val behaviorProviders = BlessBehaviorProviderPlugin.getBTSSubclauseBehaviorProvider(component)

    val ret: ISZ[FileResource] = behaviorProviders.flatMap((m: BTSSubclauseBehaviorProvider) =>
      m.values.map((r: BTSResource) =>
        r match {
          case t: BTSText =>
            IResource(
              dstPath = filename,
              content = st"${t.source}",
              markers = ISZ(),
              overwrite = t.overwrite,
              makeExecutable = F,
              makeCRLF = F,
              isDatatype = F)

          case p: BTSPath =>
            EResource(
              srcPath = p.path,
              dstPath = filename,
              symlink = T)
        }
      ))

    return ret
  }
}
