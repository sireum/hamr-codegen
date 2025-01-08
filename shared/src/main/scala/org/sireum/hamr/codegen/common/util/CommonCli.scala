// #Sireum
/*
 Copyright (c) 2017-2025, Jason Belt, Kansas State University
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

package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.cli.CliOpt._

object CommonCli {

  val commonOptions: ISZ[Opt] = ISZ(
    Opt(name = "verbose", longKey = "verbose", shortKey = Some('v'),
      tpe = Type.Flag(F),
      description = "Enable verbose mode"),
    Opt(name = "runtimeMonitoring", longKey = "runtime-monitoring", shortKey = Some('m'),
      tpe = Type.Flag(F),
      description = "Enable runtime monitoring"),
    Opt(name = "platform", longKey = "platform", shortKey = Some('p'),
      tpe = Type.Choice(name = "HamrPlatform", sep = None(),
        elements = ISZ("JVM", "Linux", "Cygwin", "MacOS", "seL4", "seL4_Only", "seL4_TB", "Microkit", "ros2")),
      description = "Target platform"),
    Opt(name = "outputDir", longKey = "output-dir", shortKey = Some('o'),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Default output directory"),
    Opt(name = "parseableMessages", longKey = "parseable-messages", shortKey = None(),
      tpe = Type.Flag(F),
      description = "Print parseable file messages")
  )

  val commonGroups: ISZ[OptGroup] = ISZ(
    OptGroup(name = "Slang", opts = ISZ(
      Opt(name = "slangOutputDir", longKey = "slang-output-dir", shortKey = None(),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Output directory for the generated Slang project files"),
      Opt(name = "packageName", longKey = "package-name", shortKey = Some('n'),
        tpe = Type.Str(sep = None(), default = Some("base")),
        description = "Base package name for Slang project"),
      Opt(name = "noProyekIve", longKey = "no-proyek-ive", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Do not run Proyek IVE"),
      Opt(name = "noEmbedArt", longKey = "no-embed-art", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Do not embed ART project files"),
      Opt(name = "devicesAsThreads", longKey = "devices-as-thread", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Treat AADL devices as threads"),
      Opt(name = "genSbtMill", longKey = "sbt-mill", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Generate SBT and Mill projects in addition to Proyek")
    )),
    OptGroup(name = "Transpiler", opts = ISZ(
      Opt(name = "slangAuxCodeDirs", longKey = "aux-code-dirs", shortKey = None(),
        tpe = Type.Path(multiple = T, default = None()),
        description = "Auxiliary C source code directories"),
      Opt(name = "slangOutputCDir", longKey = "output-c-dir", shortKey = None(),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Output directory for C artifacts"),
      Opt(name = "excludeComponentImpl", longKey = "exclude-component-impl", shortKey = Some('e'),
        tpe = Type.Flag(F),
        description = "Exclude Slang component implementations, behavior code written in C"),
      Opt(name = "bitWidth", longKey = "bit-width", shortKey = Some('b'),
        tpe = Type.NumChoice(None(), ISZ(64, 32, 16, 8)),
        description = "Default bit-width for unbounded integer types (e.g., Z)"),
      Opt(name = "maxStringSize", longKey = "max-string-size", shortKey = Some('s'),
        tpe = Type.Num(None(), 100, None(), None()),
        description = "Size for statically allocated strings"),
      Opt(name = "maxArraySize", longKey = "max-array-size", shortKey = Some('a'),
        tpe = Type.Num(None(), 100, None(), None()),
        description = "Default sequence size (e.g., for ISZ, MSZ"),
      Opt(name = "runTranspiler", longKey = "run-transpiler", shortKey = Some('t'),
        tpe = Type.Flag(F),
        description = "Run Transpiler during HAMR Codegen")
    )),
    OptGroup(name = "CAmkES/Microkit", opts = ISZ(
      Opt(name = "sel4OutputDir", longKey = "sel4-output-dir", shortKey = None(),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Output directory for the generated CAmkES/Microkit project files"),
      Opt(name = "sel4AuxCodeDirs", longKey = "sel4-aux-code-dirs", shortKey = None(),
        tpe = Type.Path(multiple = T, default = None()),
        description = "Directories containing C files to be included in CAmkES/Microkit build"),
      Opt(name = "workspaceRootDir", longKey = "workspace-root-dir", shortKey = Some('r'),
        tpe = Type.Path(multiple = F, default = None()),
        description = "Root directory containing the architectural model project")
    )),
    OptGroup(name = "ROS2", opts = ISZ(
      Opt(name = "strictAadlMode", longKey = "strict-aadl-mode", shortKey = None(),
        tpe = Type.Flag(F),
        description = "Whether to generate strictly AADL-compliant code or not (will probably become obsolete soon)"),
      Opt(name = "ros2OutputWorkspaceDir", longKey = "ros2-output-workspace-dir", shortKey = None(),
        tpe = Type.Path(multiple = F, default = None()),
        description = "The path to the ROS2 workspace to generate the packages into"),
      Opt(name = "ros2Dir", longKey = "ros2-dir", shortKey = Some('r'),
        tpe = Type.Path(multiple = F, default = None()),
        description = "The path to your ROS2 installation, including the version (../ros/humble)"),
      Opt(name = "ros2NodesLanguage", longKey = "ros2-nodes-language", shortKey = Some('p'),
        tpe = Type.Choice(name = "NodesCodeLanguage", sep = None(), elements = ISZ("Python", "Cpp")),
        description = "The programming language for the generated node files"),
      Opt(name = "ros2LaunchLanguage", longKey = "ros2-launch-language", shortKey = Some('p'),
        tpe = Type.Choice(name = "LaunchCodeLanguage", sep = None(), elements = ISZ("Python", "Xml")),
        description = "The programming language for the launch file")
    )),
    OptGroup(name = "Experimental", opts = ISZ(
      Opt(name = "experimentalOptions", longKey = "experimental-options", shortKey = Some('x'),
        tpe = Type.Str(sep = Some(';'), default = None()),
        description = ""
      ))
    )
  )
}
