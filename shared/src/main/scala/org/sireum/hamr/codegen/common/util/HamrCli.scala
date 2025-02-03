// #Sireum
// @formatter:off

// This file is auto-generated from cliJson.sc

package org.sireum.hamr.codegen.common.util

import org.sireum._

object HamrCli {

  @datatype trait HamrCodegenCliTopOption

  @datatype class HelpOption extends HamrCodegenCliTopOption

  @enum object CodegenHamrPlatform {
    "JVM"
    "Linux"
    "Cygwin"
    "MacOS"
    "SeL4"
    "SeL4_Only"
    "SeL4_TB"
    "Microkit"
    "Ros2"
  }

  @enum object CodegenNodesCodeLanguage {
    "Python"
    "Cpp"
  }

  @enum object CodegenLaunchCodeLanguage {
    "Python"
    "Xml"
  }

  @datatype class CodegenOption(
    val help: String,
    val args: ISZ[String],
    val msgpack: B,
    val verbose: B,
    val runtimeMonitoring: B,
    val platform: CodegenHamrPlatform.Type,
    val outputDir: Option[String],
    val parseableMessages: B,
    val slangOutputDir: Option[String],
    val packageName: Option[String],
    val noProyekIve: B,
    val noEmbedArt: B,
    val devicesAsThreads: B,
    val genSbtMill: B,
    val slangAuxCodeDirs: ISZ[String],
    val slangOutputCDir: Option[String],
    val excludeComponentImpl: B,
    val bitWidth: Z,
    val maxStringSize: Z,
    val maxArraySize: Z,
    val runTranspiler: B,
    val sel4OutputDir: Option[String],
    val sel4AuxCodeDirs: ISZ[String],
    val workspaceRootDir: Option[String],
    val strictAadlMode: B,
    val ros2OutputWorkspaceDir: Option[String],
    val ros2Dir: Option[String],
    val ros2NodesLanguage: CodegenNodesCodeLanguage.Type,
    val ros2LaunchLanguage: CodegenLaunchCodeLanguage.Type,
    val invertTopicBinding: B,
    val experimentalOptions: ISZ[String]
  ) extends HamrCodegenCliTopOption
}

import HamrCli._

@record class HamrCli(val pathSep: C) {

  def parseCodegenHamrPlatformH(arg: String): Option[CodegenHamrPlatform.Type] = {
    arg.native match {
      case "JVM" => return Some(CodegenHamrPlatform.JVM)
      case "Linux" => return Some(CodegenHamrPlatform.Linux)
      case "Cygwin" => return Some(CodegenHamrPlatform.Cygwin)
      case "MacOS" => return Some(CodegenHamrPlatform.MacOS)
      case "seL4" => return Some(CodegenHamrPlatform.SeL4)
      case "seL4_Only" => return Some(CodegenHamrPlatform.SeL4_Only)
      case "seL4_TB" => return Some(CodegenHamrPlatform.SeL4_TB)
      case "Microkit" => return Some(CodegenHamrPlatform.Microkit)
      case "ros2" => return Some(CodegenHamrPlatform.Ros2)
      case s =>
        eprintln(s"Expecting one of the following: { JVM, Linux, Cygwin, MacOS, seL4, seL4_Only, seL4_TB, Microkit, ros2 }, but found '$s'.")
        return None()
    }
  }

  def parseCodegenHamrPlatform(args: ISZ[String], i: Z): Option[CodegenHamrPlatform.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { JVM, Linux, Cygwin, MacOS, seL4, seL4_Only, seL4_TB, Microkit, ros2 }, but none found.")
      return None()
    }
    val r = parseCodegenHamrPlatformH(args(i))
    return r
  }

  def parseCodegenNodesCodeLanguageH(arg: String): Option[CodegenNodesCodeLanguage.Type] = {
    arg.native match {
      case "Python" => return Some(CodegenNodesCodeLanguage.Python)
      case "Cpp" => return Some(CodegenNodesCodeLanguage.Cpp)
      case s =>
        eprintln(s"Expecting one of the following: { Python, Cpp }, but found '$s'.")
        return None()
    }
  }

  def parseCodegenNodesCodeLanguage(args: ISZ[String], i: Z): Option[CodegenNodesCodeLanguage.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { Python, Cpp }, but none found.")
      return None()
    }
    val r = parseCodegenNodesCodeLanguageH(args(i))
    return r
  }

  def parseCodegenLaunchCodeLanguageH(arg: String): Option[CodegenLaunchCodeLanguage.Type] = {
    arg.native match {
      case "Python" => return Some(CodegenLaunchCodeLanguage.Python)
      case "Xml" => return Some(CodegenLaunchCodeLanguage.Xml)
      case s =>
        eprintln(s"Expecting one of the following: { Python, Xml }, but found '$s'.")
        return None()
    }
  }

  def parseCodegenLaunchCodeLanguage(args: ISZ[String], i: Z): Option[CodegenLaunchCodeLanguage.Type] = {
    if (i >= args.size) {
      eprintln("Expecting one of the following: { Python, Xml }, but none found.")
      return None()
    }
    val r = parseCodegenLaunchCodeLanguageH(args(i))
    return r
  }

  def parseCodegen(args: ISZ[String], i: Z): Option[HamrCodegenCliTopOption] = {
    val help =
      st"""Generate code from AADL IR (AIR)
          |
          |Usage: <option>* air-file
          |
          |Available Options:
          |    --msgpack            Input serialized using Msgpack (otherwise JSON
          |                           assumed)
          |-v, --verbose            Enable verbose mode
          |-m, --runtime-monitoring    
          |                          Enable runtime monitoring
          |-p, --platform           Target platform (expects one of { JVM, Linux, Cygwin,
          |                           MacOS, seL4, seL4_Only, seL4_TB, Microkit, ros2 };
          |                           default: JVM)
          |-o, --output-dir         Default output directory (expects a path)
          |    --parseable-messages Print parseable file messages
          |-h, --help               Display this information
          |
          |Slang Options:
          |    --slang-output-dir   Output directory for the generated Slang project files
          |                           (expects a path)
          |-n, --package-name       Base package name for Slang project (expects a string;
          |                           default is "base")
          |    --no-proyek-ive      Do not run Proyek IVE
          |    --no-embed-art       Do not embed ART project files
          |    --devices-as-thread  Treat AADL devices as threads
          |    --sbt-mill           Generate SBT and Mill projects in addition to Proyek
          |
          |Transpiler Options:
          |    --aux-code-dirs      Auxiliary C source code directories (expects path
          |                           strings)
          |    --output-c-dir       Output directory for C artifacts (expects a path)
          |-e, --exclude-component-impl    
          |                          Exclude Slang component implementations, behavior
          |                           code written in C
          |-b, --bit-width          Default bit-width for unbounded integer types (e.g.,
          |                           Z) (expects one of { 64, 32, 16, 8 })
          |-s, --max-string-size    
          |                          Size for statically allocated strings (expects an
          |                           integer; default is 100)
          |-a, --max-array-size     Default sequence size (e.g., for ISZ, MSZ (expects an
          |                           integer; default is 100)
          |-t, --run-transpiler     Run Transpiler during HAMR Codegen
          |
          |CAmkES/Microkit Options:
          |    --sel4-output-dir    Output directory for the generated CAmkES/Microkit
          |                           project files (expects a path)
          |    --sel4-aux-code-dirs Directories containing C files to be included in
          |                           CAmkES/Microkit build (expects path strings)
          |-r, --workspace-root-dir    
          |                          Root directory containing the architectural model
          |                           project (expects a path)
          |
          |ROS2 Options:
          |    --strict-aadl-mode   Whether to generate strictly AADL-compliant code or
          |                           not (will probably become obsolete soon)
          |    --ros2-output-workspace-dir
          |                          The path to the ROS2 workspace to generate the
          |                           packages into (expects a path)
          |-r, --ros2-dir           The path to your ROS2 installation, including the
          |                           version (../ros/humble) (expects a path)
          |-p, --ros2-nodes-language    
          |                          The programming language for the generated node files
          |                           (expects one of { Python, Cpp }; default: Python)
          |-p, --ros2-launch-language    
          |                          The programming language for the launch file (expects
          |                           one of { Python, Xml }; default: Python)
          |    --invert-topic-binding
<<<<<<< HEAD
          |                          By default, topic names are based on in ports, and
          |                           fan out ports would have multiple publishers.  This
          |                           option inverts that behavior.
=======
          |                          By default, topic names are based on in ports, and fan
          |                           out ports would have multiple publishers.  This option
          |                           inverts that behavior."
>>>>>>> b76ef58 (Added ROS2 codegen option (invertTopicBinding) and added topic inversion option into generator)
          |
          |Experimental Options:
          |-x, --experimental-options    
          |                           (expects a string separated by ";")""".render

    var msgpack: B = false
    var verbose: B = false
    var runtimeMonitoring: B = false
    var platform: CodegenHamrPlatform.Type = CodegenHamrPlatform.JVM
    var outputDir: Option[String] = None[String]()
    var parseableMessages: B = false
    var slangOutputDir: Option[String] = None[String]()
    var packageName: Option[String] = Some("base")
    var noProyekIve: B = false
    var noEmbedArt: B = false
    var devicesAsThreads: B = false
    var genSbtMill: B = false
    var slangAuxCodeDirs: ISZ[String] = ISZ[String]()
    var slangOutputCDir: Option[String] = None[String]()
    var excludeComponentImpl: B = false
    var bitWidth: Z = 64
    var maxStringSize: Z = 100
    var maxArraySize: Z = 100
    var runTranspiler: B = false
    var sel4OutputDir: Option[String] = None[String]()
    var sel4AuxCodeDirs: ISZ[String] = ISZ[String]()
    var workspaceRootDir: Option[String] = None[String]()
    var strictAadlMode: B = false
    var ros2OutputWorkspaceDir: Option[String] = None[String]()
    var ros2Dir: Option[String] = None[String]()
    var ros2NodesLanguage: CodegenNodesCodeLanguage.Type = CodegenNodesCodeLanguage.Python
    var ros2LaunchLanguage: CodegenLaunchCodeLanguage.Type = CodegenLaunchCodeLanguage.Python
    var invertTopicBinding: B = false
    var experimentalOptions: ISZ[String] = ISZ[String]()
    var j = i
    var isOption = T
    while (j < args.size && isOption) {
      val arg = args(j)
      if (ops.StringOps(arg).first == '-') {
        if (args(j) == "-h" || args(j) == "--help") {
          println(help)
          return Some(HelpOption())
        } else if (arg == "--msgpack") {
           val o: Option[B] = { j = j - 1; Some(!msgpack) }
           o match {
             case Some(v) => msgpack = v
             case _ => return None()
           }
         } else if (arg == "-v" || arg == "--verbose") {
           val o: Option[B] = { j = j - 1; Some(!verbose) }
           o match {
             case Some(v) => verbose = v
             case _ => return None()
           }
         } else if (arg == "-m" || arg == "--runtime-monitoring") {
           val o: Option[B] = { j = j - 1; Some(!runtimeMonitoring) }
           o match {
             case Some(v) => runtimeMonitoring = v
             case _ => return None()
           }
         } else if (arg == "-p" || arg == "--platform") {
           val o: Option[CodegenHamrPlatform.Type] = parseCodegenHamrPlatform(args, j + 1)
           o match {
             case Some(v) => platform = v
             case _ => return None()
           }
         } else if (arg == "-o" || arg == "--output-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => outputDir = v
             case _ => return None()
           }
         } else if (arg == "--parseable-messages") {
           val o: Option[B] = { j = j - 1; Some(!parseableMessages) }
           o match {
             case Some(v) => parseableMessages = v
             case _ => return None()
           }
         } else if (arg == "--slang-output-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => slangOutputDir = v
             case _ => return None()
           }
         } else if (arg == "-n" || arg == "--package-name") {
           val o: Option[Option[String]] = parseString(args, j + 1)
           o match {
             case Some(v) => packageName = v
             case _ => return None()
           }
         } else if (arg == "--no-proyek-ive") {
           val o: Option[B] = { j = j - 1; Some(!noProyekIve) }
           o match {
             case Some(v) => noProyekIve = v
             case _ => return None()
           }
         } else if (arg == "--no-embed-art") {
           val o: Option[B] = { j = j - 1; Some(!noEmbedArt) }
           o match {
             case Some(v) => noEmbedArt = v
             case _ => return None()
           }
         } else if (arg == "--devices-as-thread") {
           val o: Option[B] = { j = j - 1; Some(!devicesAsThreads) }
           o match {
             case Some(v) => devicesAsThreads = v
             case _ => return None()
           }
         } else if (arg == "--sbt-mill") {
           val o: Option[B] = { j = j - 1; Some(!genSbtMill) }
           o match {
             case Some(v) => genSbtMill = v
             case _ => return None()
           }
         } else if (arg == "--aux-code-dirs") {
           val o: Option[ISZ[String]] = parsePaths(args, j + 1)
           o match {
             case Some(v) => slangAuxCodeDirs = v
             case _ => return None()
           }
         } else if (arg == "--output-c-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => slangOutputCDir = v
             case _ => return None()
           }
         } else if (arg == "-e" || arg == "--exclude-component-impl") {
           val o: Option[B] = { j = j - 1; Some(!excludeComponentImpl) }
           o match {
             case Some(v) => excludeComponentImpl = v
             case _ => return None()
           }
         } else if (arg == "-b" || arg == "--bit-width") {
           val o: Option[Z] = parseNumChoice(args, j + 1, ISZ(z"64", z"32", z"16", z"8"))
           o match {
             case Some(v) => bitWidth = v
             case _ => return None()
           }
         } else if (arg == "-s" || arg == "--max-string-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxStringSize = v
             case _ => return None()
           }
         } else if (arg == "-a" || arg == "--max-array-size") {
           val o: Option[Z] = parseNum(args, j + 1, None(), None())
           o match {
             case Some(v) => maxArraySize = v
             case _ => return None()
           }
         } else if (arg == "-t" || arg == "--run-transpiler") {
           val o: Option[B] = { j = j - 1; Some(!runTranspiler) }
           o match {
             case Some(v) => runTranspiler = v
             case _ => return None()
           }
         } else if (arg == "--sel4-output-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => sel4OutputDir = v
             case _ => return None()
           }
         } else if (arg == "--sel4-aux-code-dirs") {
           val o: Option[ISZ[String]] = parsePaths(args, j + 1)
           o match {
             case Some(v) => sel4AuxCodeDirs = v
             case _ => return None()
           }
         } else if (arg == "-r" || arg == "--workspace-root-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => workspaceRootDir = v
             case _ => return None()
           }
         } else if (arg == "--strict-aadl-mode") {
           val o: Option[B] = { j = j - 1; Some(!strictAadlMode) }
           o match {
             case Some(v) => strictAadlMode = v
             case _ => return None()
           }
         } else if (arg == "--ros2-output-workspace-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => ros2OutputWorkspaceDir = v
             case _ => return None()
           }
         } else if (arg == "-r" || arg == "--ros2-dir") {
           val o: Option[Option[String]] = parsePath(args, j + 1)
           o match {
             case Some(v) => ros2Dir = v
             case _ => return None()
           }
         } else if (arg == "-p" || arg == "--ros2-nodes-language") {
           val o: Option[CodegenNodesCodeLanguage.Type] = parseCodegenNodesCodeLanguage(args, j + 1)
           o match {
             case Some(v) => ros2NodesLanguage = v
             case _ => return None()
           }
         } else if (arg == "-p" || arg == "--ros2-launch-language") {
           val o: Option[CodegenLaunchCodeLanguage.Type] = parseCodegenLaunchCodeLanguage(args, j + 1)
           o match {
             case Some(v) => ros2LaunchLanguage = v
             case _ => return None()
           }
         } else if (arg == "--invert-topic-binding") {
           val o: Option[B] = { j = j - 1; Some(!invertTopicBinding) }
           o match {
             case Some(v) => invertTopicBinding = v
             case _ => return None()
           }
         } else if (arg == "-x" || arg == "--experimental-options") {
           val o: Option[ISZ[String]] = parseStrings(args, j + 1, ';')
           o match {
             case Some(v) => experimentalOptions = v
             case _ => return None()
           }
         } else {
          eprintln(s"Unrecognized option '$arg'.")
          return None()
        }
        j = j + 2
      } else {
        isOption = F
      }
    }
    return Some(CodegenOption(help, parseArguments(args, j), msgpack, verbose, runtimeMonitoring, platform, outputDir, parseableMessages, slangOutputDir, packageName, noProyekIve, noEmbedArt, devicesAsThreads, genSbtMill, slangAuxCodeDirs, slangOutputCDir, excludeComponentImpl, bitWidth, maxStringSize, maxArraySize, runTranspiler, sel4OutputDir, sel4AuxCodeDirs, workspaceRootDir, strictAadlMode, ros2OutputWorkspaceDir, ros2Dir, ros2NodesLanguage, ros2LaunchLanguage, invertTopicBinding, experimentalOptions))
  }

  def parseArguments(args: ISZ[String], i: Z): ISZ[String] = {
    var r = ISZ[String]()
    var j = i
    while (j < args.size) {
      r = r :+ args(j)
      j = j + 1
    }
    return r
  }

  def parsePaths(args: ISZ[String], i: Z): Option[ISZ[String]] = {
    return tokenize(args, i, "path", pathSep, F)
  }

  def parsePath(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a path, but none found.")
    }
    return Some(Some(args(i)))
  }

  def parseStrings(args: ISZ[String], i: Z, sep: C): Option[ISZ[String]] = {
    tokenize(args, i, "string", sep, F) match {
      case r@Some(_) => return r
      case _ => return None()
    }
  }

  def parseString(args: ISZ[String], i: Z): Option[Option[String]] = {
    if (i >= args.size) {
      eprintln("Expecting a string, but none found.")
      return None()
    }
    return Some(Some(args(i)))
  }

  def parseNums(args: ISZ[String], i: Z, sep: C, minOpt: Option[Z], maxOpt: Option[Z]): Option[ISZ[Z]] = {
    tokenize(args, i, "integer", sep, T) match {
      case Some(sargs) =>
        var r = ISZ[Z]()
        for (arg <- sargs) {
          parseNumH(F, arg, minOpt, maxOpt)._2 match {
            case Some(n) => r = r :+ n
            case _ => return None()
          }
        }
        return Some(r)
      case _ => return None()
    }
  }

  def tokenize(args: ISZ[String], i: Z, tpe: String, sep: C, removeWhitespace: B): Option[ISZ[String]] = {
    if (i >= args.size) {
      eprintln(s"Expecting a sequence of $tpe separated by '$sep', but none found.")
      return None()
    }
    val arg = args(i)
    return Some(tokenizeH(arg, sep, removeWhitespace))
  }

  def tokenizeH(arg: String, sep: C, removeWhitespace: B): ISZ[String] = {
    val argCis = conversions.String.toCis(arg)
    var r = ISZ[String]()
    var cis = ISZ[C]()
    var j = 0
    while (j < argCis.size) {
      val c = argCis(j)
      if (c == sep) {
        r = r :+ conversions.String.fromCis(cis)
        cis = ISZ[C]()
      } else {
        val allowed: B = c match {
          case c"\n" => !removeWhitespace
          case c" " => !removeWhitespace
          case c"\r" => !removeWhitespace
          case c"\t" => !removeWhitespace
          case _ => T
        }
        if (allowed) {
          cis = cis :+ c
        }
      }
      j = j + 1
    }
    if (cis.size > 0) {
      r = r :+ conversions.String.fromCis(cis)
    }
    return r
  }

  def parseNumChoice(args: ISZ[String], i: Z, choices: ISZ[Z]): Option[Z] = {
    val set = HashSet.empty[Z] ++ choices
    parseNum(args, i, None(), None()) match {
      case r@Some(n) =>
        if (set.contains(n)) {
          return r
        } else {
          eprintln(s"Expecting one of the following: $set, but found $n.")
          return None()
        }
      case r => return r
    }
  }

  def parseNum(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Z] = {
    if (i >= args.size) {
      eprintln(s"Expecting an integer, but none found.")
      return None()
    }
    return parseNumH(F, args(i), minOpt, maxOpt)._2
  }

  def parseNumFlag(args: ISZ[String], i: Z, minOpt: Option[Z], maxOpt: Option[Z]): Option[Option[Z]] = {
    if (i >= args.size) {
      return Some(None())
    }
    parseNumH(T, args(i), minOpt, maxOpt) match {
      case (T, vOpt) => return Some(vOpt)
      case _ => return None()
    }
  }

  def parseNumH(optArg: B, arg: String, minOpt: Option[Z], maxOpt: Option[Z]): (B, Option[Z]) = {
    Z(arg) match {
      case Some(n) =>
        minOpt match {
          case Some(min) =>
            if (n < min) {
              eprintln(s"Expecting an integer at least $min, but found $n.")
              return (F, None())
            }
          case _ =>
        }
        maxOpt match {
          case Some(max) =>
            if (n > max) {
              eprintln(s"Expecting an integer at most $max, but found $n.")
              return (F, None())
            }
          case _ =>
        }
        return (T, Some(n))
      case _ =>
        if (!optArg) {
          eprintln(s"Expecting an integer, but found '$arg'.")
          return (F, None())
        } else {
          return (T, None())
       }
    }
  }

  def select(mode: String, args: ISZ[String], i: Z, choices: ISZ[String]): Option[String] = {
    val arg = args(i)
    var cs = ISZ[String]()
    for (c <- choices) {
      if (ops.StringOps(c).startsWith(arg)) {
        cs = cs :+ c
      }
    }
    cs.size match {
      case z"0" =>
        eprintln(s"$arg is not a mode of $mode.")
        return None()
      case z"1" => return Some(cs(0))
      case _ =>
        eprintln(
          st"""Which one of the following modes did you mean by '$arg'?
              |${(cs, "\n")}""".render)
        return None()
    }
  }
}
// @formatter:on

// BEGIN USER CODE

// END USER CODE
