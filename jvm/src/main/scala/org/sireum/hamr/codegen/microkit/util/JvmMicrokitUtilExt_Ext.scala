package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.$internal.RC
import org.sireum.$internal.CollectionCompat.Converters._
import java.io.StringReader

object JvmMicrokitUtilExt_Ext {

  def getMicrokitVersions: Map[String, String] = {
    val map = RC.text(Vector(
      "../../../../../../../resources")) { (p, f) => p.size == 1 && p.head == "microkit_versions.properties" }
    val str = map.toSeq.head._2
    val p = new java.util.Properties()
    val reader = new StringReader(str)
    try p.load(reader)
    finally reader.close()
    var r = Map.empty[String, String]
    for (k <- p.stringPropertyNames.asScala) {
      r = r + k ~> p.getProperty(k)
    }
    r
  }
}
