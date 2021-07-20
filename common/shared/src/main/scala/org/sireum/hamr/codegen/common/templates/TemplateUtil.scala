// #Sireum

package org.sireum.hamr.codegen.common.templates

import org.sireum._

object TemplateUtil {

  def uniqueSTs(sts: ISZ[ST]): ISZ[ST] = {
    return (Set.empty[String] ++ (sts.map((x: ST) => x.render))).elements.map((s: String) => st"$s")
  }
}
