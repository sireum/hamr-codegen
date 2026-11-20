// #Sireum
package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.hamr.codegen.common.util.HamrCli.{CodegenAirOutputFormat, CodegenOption}
import org.sireum.hamr.ir.{Aadl, JSON => irJSON, MsgPack => irMsgPack}
import org.sireum.message.Reporter

object AirOutput {

  /** Writes model, exactly as codegen received it, to options.airOutput (when set) in
    * options.airOutputFormat: JSON (not compact) or MsgPack with pooling, the latter as Base64
    * text since that is what `hamr codegen --msgpack` reads.  Creates the parent directories and
    * overwrites an existing file; prints `Wrote: <path>` only once the write succeeded.
    *
    * @return F after reporting an error when the file could not be written, T otherwise
    *         (including when no air-output was requested)
    */
  def write(model: Aadl, options: CodegenOption, toolName: String, reporter: Reporter): B = {
    if (options.airOutput.isEmpty) {
      return T
    }
    val path = Os.path(options.airOutput.get).canon
    val content: String = options.airOutputFormat match {
      case CodegenAirOutputFormat.Json => irJSON.fromAadl(model, F)
      case CodegenAirOutputFormat.Msgpack => conversions.String.toBase64(irMsgPack.fromAadl(model, T))
    }
    AirOutputExt.writeOver(path.value, content) match {
      case Some(err) =>
        reporter.error(None(), toolName, s"Could not write the AIR model to $path: $err")
        return F
      case _ =>
        println(s"Wrote: $path")
        return T
    }
  }
}

@ext object AirOutputExt {
  /** Creates path's parent directories, then writes content to path, replacing any existing file.
    *
    * @return the reason the write failed, or None() on success
    */
  def writeOver(path: String, content: String): Option[String] = $
}
