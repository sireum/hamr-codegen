// #Sireum

package org.sireum.hamr.codegen.common.templates

import org.sireum._

object StackFrameTemplate {
  /*
  #define DeclNewStackFrame(_caller, _uri, _owner, _name, _line) struct StackFrame sf[1] = { (struct StackFrame) { .caller = (_caller), .uri = (_uri), .owner = (_owner), .name = (_name), .line = (_line) } }
  #define sfUpdateLoc(l)    sf->line = l
  #define STACK_FRAME       StackFrame caller,
  #define STACK_FRAME_ONLY  StackFrame caller
  #define STACK_FRAME_SF    StackFrame sf,
  #define SF                sf,
  #define SF_LAST           sf
  #define CALLER            caller,
  #define CALLER_LAST       caller
  */

  // StackFrame caller,
  val STACK_FRAME: String = "STACK_FRAME"
  val STACK_FRAME_ST: ST = st"${STACK_FRAME}"

  // StackFrame caller
  val STACK_FRAME_ONLY: String = "STACK_FRAME_ONLY"
  val STACK_FRAME_ONLY_ST: ST = st"${STACK_FRAME_ONLY}"

  // StackFrame sf,
  val STACK_FRAME_SF: String = "STACK_FRAME_SF"
  val STACK_FRAME_SF_ST: ST = st"${STACK_FRAME_SF}"

  // sf,
  val SF: String = "SF"
  val SF_ST: ST = st"${SF}"

  // sf
  val SF_LAST: String = "SF_LAST"
  val SF_LAST_ST: ST = st"${SF_LAST}"

  // caller,
  val CALLER: String = "CALLER"
  val CALLER_ST: ST = st"${CALLER}"

  // caller
  val CALLER_LAST: String = "CALLER_LAST"
  val CALLER_LAST_ST: ST = st"${CALLER_LAST}"

  def DeclNewStackFrame(caller: B, uri: String, owner: String, name: String, line: Z): ST = {
    val _caller: String = if(caller) "caller" else "NULL"
    val ret: ST = st"""DeclNewStackFrame(${_caller}, "${uri}", "${owner}", "${name}", ${line})"""
    return ret
  }
}
