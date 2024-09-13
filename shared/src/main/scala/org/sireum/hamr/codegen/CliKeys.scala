// #Sireum

package org.sireum.hamr.codegen

import org.sireum._

// the following can be used when constructing HAMR codegen command line arguments
// from an external tool (e.g. OSATE).

object LongKeys {
  val msgpack: String = "--msgpack"
  val verbose: String = "--verbose"
  val runtimeMonitoring: String = "--runtime-monitoring"
  val platform: String = "--platform"
  val parseableMessages: String = "--parseable-messages"
  val Slang_slangOutputDir: String = "--slang-output-dir"
  val Slang_packageName: String = "--package-name"
  val Slang_noProyekIve: String = "--no-proyek-ive"
  val Slang_noEmbedArt: String = "--no-embed-art"
  val Slang_devicesAsThreads: String = "--devices-as-thread"
  val Slang_genSbtMill: String = "--sbt-mill"
  val Transpiler_slangAuxCodeDirs: String = "--aux-code-dirs"
  val Transpiler_slangOutputCDir: String = "--output-c-dir"
  val Transpiler_excludeComponentImpl: String = "--exclude-component-impl"
  val Transpiler_bitWidth: String = "--bit-width"
  val Transpiler_maxStringSize: String = "--max-string-size"
  val Transpiler_maxArraySize: String = "--max-array-size"
  val Transpiler_runTranspiler: String = "--run-transpiler"
  val CAmkES_camkesOutputDir: String = "--camkes-output-dir"
  val CAmkES_camkesAuxCodeDirs: String = "--camkes-aux-code-dirs"
  val CAmkES_workspaceRootDir: String = "--workspace-root-dir"
  val ROS2_strictAadlMode: String = "--strict-aadl-mode"
  val ROS2_ros2OutputWorkspaceDir: String = "--ros2-output-workspace-dir"
  val ROS2_ros2Dir: String = "--ros2-dir"
  val ROS2_ros2NodesLanguage: String = "--ros2-nodes-language"
  val ROS2_ros2LaunchLanguage: String = "--ros2-launch-language"
  val Experimental_experimentalOptions: String = "--experimental-options"
}

object ShortKeys {
  val verbose: String = "-v"
  val runtimeMonitoring: String = "-m"
  val platform: String = "-p"
  val Slang_slangOutputDir: String = "-o"
  val Slang_packageName: String = "-n"
  val Transpiler_excludeComponentImpl: String = "-e"
  val Transpiler_bitWidth: String = "-b"
  val Transpiler_maxStringSize: String = "-s"
  val Transpiler_maxArraySize: String = "-a"
  val Transpiler_runTranspiler: String = "-t"
  val CAmkES_workspaceRootDir: String = "-r"
  val ROS2_ros2Dir: String = "-r"
  val ROS2_ros2NodesLanguage: String = "-p"
  val ROS2_ros2LaunchLanguage: String = "-p"
  val Experimental_experimentalOptions: String = "-x"
}

object KeyUtil {
  val allLongKeys: ISZ[String] = ISZ(
    "msgpack",
    "verbose",
    "runtime-monitoring",
    "platform",
    "parseable-messages",
    "slang-output-dir",
    "package-name",
    "no-proyek-ive",
    "no-embed-art",
    "devices-as-thread",
    "sbt-mill",
    "aux-code-dirs",
    "output-c-dir",
    "exclude-component-impl",
    "bit-width",
    "max-string-size",
    "max-array-size",
    "run-transpiler",
    "camkes-output-dir",
    "camkes-aux-code-dirs",
    "workspace-root-dir",
    "strict-aadl-mode",
    "ros2-output-workspace-dir",
    "ros2-dir",
    "ros2-nodes-language",
    "ros2-launch-language",
    "experimental-options")

  // Paste the following into a java program if you want to ensure the known keys match.
  // To regenerate this, run '$SIREUM_HOME/hamr/codegen/build.cmd regen-cli'.
  // If this does fail then the CLI arguments being constructed for codegen will need
  // to be updated (that could be delayed if only new options were added).

  // scala.collection.Seq<org.sireum.String> seq = scala.jdk.javaapi.CollectionConverters.asScala(new java.util.ArrayList<org.sireum.String>());
  // scala.collection.immutable.Seq<org.sireum.String> iseq = ((scala.collection.IterableOnceOps<org.sireum.String, ?, ?>) seq).toSeq();
  // org.sireum.IS<org.sireum.Z, org.sireum.String> knownKeys = org.sireum.IS$.MODULE$.apply(iseq, org.sireum.Z$.MODULE$).$colon$plus(new org.sireum.String("msgpack")).$colon$plus(new org.sireum.String("verbose")).$colon$plus(new org.sireum.String("runtime-monitoring")).$colon$plus(new org.sireum.String("platform")).$colon$plus(new org.sireum.String("parseable-messages")).$colon$plus(new org.sireum.String("slang-output-dir")).$colon$plus(new org.sireum.String("package-name")).$colon$plus(new org.sireum.String("no-proyek-ive")).$colon$plus(new org.sireum.String("no-embed-art")).$colon$plus(new org.sireum.String("devices-as-thread")).$colon$plus(new org.sireum.String("sbt-mill")).$colon$plus(new org.sireum.String("aux-code-dirs")).$colon$plus(new org.sireum.String("output-c-dir")).$colon$plus(new org.sireum.String("exclude-component-impl")).$colon$plus(new org.sireum.String("bit-width")).$colon$plus(new org.sireum.String("max-string-size")).$colon$plus(new org.sireum.String("max-array-size")).$colon$plus(new org.sireum.String("run-transpiler")).$colon$plus(new org.sireum.String("camkes-output-dir")).$colon$plus(new org.sireum.String("camkes-aux-code-dirs")).$colon$plus(new org.sireum.String("workspace-root-dir")).$colon$plus(new org.sireum.String("strict-aadl-mode")).$colon$plus(new org.sireum.String("ros2-output-workspace-dir")).$colon$plus(new org.sireum.String("ros2-dir")).$colon$plus(new org.sireum.String("ros2-nodes-language")).$colon$plus(new org.sireum.String("ros2-launch-language")).$colon$plus(new org.sireum.String("experimental-options"));
  // boolean sameKeys = org.sireum.hamr.codegen.KeyUtil.allLongKeys().equals(knownKeys);
}
