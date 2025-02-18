// #Sireum

package org.sireum.hamr.codegen

import org.sireum._

// the following can be used when constructing command line arguments for HamrCodegenCli
// from an external tool (e.g. OSATE).

object LongKeys {
  val msgpack: String = "--msgpack"
  val verbose: String = "--verbose"
  val runtimeMonitoring: String = "--runtime-monitoring"
  val platform: String = "--platform"
  val outputDir: String = "--output-dir"
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
  val CAmkES_Microkit_sel4OutputDir: String = "--sel4-output-dir"
  val CAmkES_Microkit_sel4AuxCodeDirs: String = "--sel4-aux-code-dirs"
  val CAmkES_Microkit_workspaceRootDir: String = "--workspace-root-dir"
  val ROS2_strictAadlMode: String = "--strict-aadl-mode"
  val ROS2_ros2OutputWorkspaceDir: String = "--ros2-output-workspace-dir"
  val ROS2_ros2Dir: String = "--ros2-dir"
  val ROS2_ros2NodesLanguage: String = "--ros2-nodes-language"
  val ROS2_ros2LaunchLanguage: String = "--ros2-launch-language"
  val ROS2_invertTopicBinding: String = "--invert-topic-binding"
  val Experimental_experimentalOptions: String = "--experimental-options"

  val allKeys: Set[String] = Set.empty[String] ++ ISZ(msgpack, verbose, runtimeMonitoring, platform, outputDir, parseableMessages, Slang_slangOutputDir, Slang_packageName, Slang_noProyekIve, Slang_noEmbedArt, Slang_devicesAsThreads, Slang_genSbtMill, Transpiler_slangAuxCodeDirs, Transpiler_slangOutputCDir, Transpiler_excludeComponentImpl, Transpiler_bitWidth, Transpiler_maxStringSize, Transpiler_maxArraySize, Transpiler_runTranspiler, CAmkES_Microkit_sel4OutputDir, CAmkES_Microkit_sel4AuxCodeDirs, CAmkES_Microkit_workspaceRootDir, ROS2_strictAadlMode, ROS2_ros2OutputWorkspaceDir, ROS2_ros2Dir, ROS2_ros2NodesLanguage, ROS2_ros2LaunchLanguage, ROS2_invertTopicBinding, Experimental_experimentalOptions)

  @strictpure def sameKeys(keys: ISZ[String]): B = allKeys.elements == keys

  // Paste the following into a java program if you want to ensure the known keys match.
  // To regenerate this, run '$SIREUM_HOME/hamr/codegen/build.cmd regen-cli'.
  // If this does fail then the CLI arguments being constructed for codegen will need
  // to be updated (that could be delayed if only new options were added).

  // scala.collection.Seq<org.sireum.String> seq = scala.jdk.javaapi.CollectionConverters.asScala(new java.util.ArrayList<org.sireum.String>());
  // scala.collection.immutable.Seq<org.sireum.String> iseq = ((scala.collection.IterableOnceOps<org.sireum.String, ?, ?>) seq).toSeq();
  // org.sireum.IS<org.sireum.Z, org.sireum.String> knownKeys = org.sireum.IS$.MODULE$.apply(iseq, org.sireum.Z$.MODULE$).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.msgpack())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.verbose())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.runtimeMonitoring())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.platform())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.outputDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.parseableMessages())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_slangOutputDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_packageName())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_noProyekIve())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_noEmbedArt())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_devicesAsThreads())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Slang_genSbtMill())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_slangAuxCodeDirs())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_slangOutputCDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_excludeComponentImpl())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_bitWidth())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_maxStringSize())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_maxArraySize())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Transpiler_runTranspiler())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.CAmkES_Microkit_sel4OutputDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.CAmkES_Microkit_sel4AuxCodeDirs())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.CAmkES_Microkit_workspaceRootDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_strictAadlMode())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_ros2OutputWorkspaceDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_ros2Dir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_ros2NodesLanguage())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_ros2LaunchLanguage())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.ROS2_invertTopicBinding())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.LongKeys.Experimental_experimentalOptions()));
  // boolean sameKeys = org.sireum.hamr.codegen.LongKeys.sameKeys(knownKeys);

}

object ShortKeys {
  val verbose: String = "-v"
  val runtimeMonitoring: String = "-m"
  val platform: String = "-p"
  val outputDir: String = "-o"
  val Slang_packageName: String = "-n"
  val Transpiler_excludeComponentImpl: String = "-e"
  val Transpiler_bitWidth: String = "-b"
  val Transpiler_maxStringSize: String = "-s"
  val Transpiler_maxArraySize: String = "-a"
  val Transpiler_runTranspiler: String = "-t"
  val CAmkES_Microkit_workspaceRootDir: String = "-r"
  val ROS2_ros2Dir: String = "-r"
  val ROS2_ros2NodesLanguage: String = "-p"
  val ROS2_ros2LaunchLanguage: String = "-p"
  val Experimental_experimentalOptions: String = "-x"

  val allKeys: Set[String] = Set.empty[String] ++ ISZ(verbose, runtimeMonitoring, platform, outputDir, Slang_packageName, Transpiler_excludeComponentImpl, Transpiler_bitWidth, Transpiler_maxStringSize, Transpiler_maxArraySize, Transpiler_runTranspiler, CAmkES_Microkit_workspaceRootDir, ROS2_ros2Dir, ROS2_ros2NodesLanguage, ROS2_ros2LaunchLanguage, Experimental_experimentalOptions)

  @strictpure def sameKeys(keys: ISZ[String]): B = allKeys.elements == keys

  // Paste the following into a java program if you want to ensure the known keys match.
  // To regenerate this, run '$SIREUM_HOME/hamr/codegen/build.cmd regen-cli'.
  // If this does fail then the CLI arguments being constructed for codegen will need
  // to be updated (that could be delayed if only new options were added).

  // scala.collection.Seq<org.sireum.String> seq = scala.jdk.javaapi.CollectionConverters.asScala(new java.util.ArrayList<org.sireum.String>());
  // scala.collection.immutable.Seq<org.sireum.String> iseq = ((scala.collection.IterableOnceOps<org.sireum.String, ?, ?>) seq).toSeq();
  // org.sireum.IS<org.sireum.Z, org.sireum.String> knownKeys = org.sireum.IS$.MODULE$.apply(iseq, org.sireum.Z$.MODULE$).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.verbose())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.runtimeMonitoring())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.platform())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.outputDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Slang_packageName())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Transpiler_excludeComponentImpl())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Transpiler_bitWidth())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Transpiler_maxStringSize())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Transpiler_maxArraySize())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Transpiler_runTranspiler())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.CAmkES_Microkit_workspaceRootDir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.ROS2_ros2Dir())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.ROS2_ros2NodesLanguage())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.ROS2_ros2LaunchLanguage())).$colon$plus(new org.sireum.String(org.sireum.hamr.codegen.ShortKeys.Experimental_experimentalOptions()));
  // boolean sameKeys = org.sireum.hamr.codegen.ShortKeys.sameKeys(knownKeys);
}
