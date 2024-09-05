// #Sireum
/*
 Copyright (c) 2017-2024, Jason Belt, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.hamr.codegen

import org.sireum._
import org.sireum.cli.CliOpt._
import org.sireum.hamr.codegen.common.util.CommonCli

object HamrCodegenCli {
  val codeGenTool: Tool = Tool(
    name = "HamrCodegenCli",
    command = "codegen",
    description = "", // description not currently used by cligen
    header = "Generate code from AADL IR (AIR)",
    usage = "<option>* air-file",
    usageDescOpt = None(),
    opts = ISZ(
      Opt(name = "msgpack", longKey = "msgpack", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Input serialized using Msgpack (otherwise JSON assumed)"))
      ++ CommonCli.commonOptions,
    groups = CommonCli.commonGroups
  )
}
