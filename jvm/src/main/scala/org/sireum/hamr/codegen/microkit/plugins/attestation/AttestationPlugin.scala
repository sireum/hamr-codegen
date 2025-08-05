// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.attestation

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{ExternalResource, InternalResource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{CodeGenResults, HamrCli}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter
import org.sireum.{B, strictpure}

@datatype class AttestationPlugin extends Plugin {

  val name: String = "AttestationPlugin"

  val workspace_dir_ENV: String = "%%workspace_dir%%"
  val codegen_dir_ENV: String = "%%codegen_dir%%"
  val attestation_root_ENV: String = "%%attestation_dir%%"

  val KEY_ATTESTATION_PLUGIN: String = "KEY_ATTESTATION_PLUGIN"

  val env_AM_REPOS_ROOT: String = "AM_REPOS_ROOT"

  @strictpure def hasFinalized(store: Store): B = store.contains(KEY_ATTESTATION_PLUGIN)

  @pure override def canFinalize(model: Aadl, aadlTypes: Option[AadlTypes], symbolTable: Option[SymbolTable], codegenResults: CodeGenResults, store: Store, options: HamrCli.CodegenOption, reporter: Reporter): B = {
    return (
      !reporter.hasError &&
        symbolTable.nonEmpty &&
        options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
        !hasFinalized(store))
  }

  @pure override def finalizePlugin(model: Aadl, aadlTypes: Option[AadlTypes], symbolTableo: Option[SymbolTable], codegenResults: CodeGenResults, store: Store, options: HamrCli.CodegenOption, reporter: Reporter): Store = {
    val symbolTable = symbolTableo.get

    val isAadl = ops.StringOps(symbolTable.rootSystem.component.identifier.pos.get.uriOpt.get).endsWith(".aadl")

    var localStore = store

    val workspaceRoot: Os.Path =
      options.workspaceRootDir match {
        case Some(wdir) =>
          val d = Os.path(wdir)
          assert(d.exists, d.value)
          d
        case _ =>
          println("Model workspace option was not provided. Cannot generate attestation artifacts")
          return localStore
      }

    val sel4OutputDir: Os.Path =
      options.sel4OutputDir match {
        case Some(d) => Os.path(d)
        case _ =>
          options.outputDir match {
            case Some(f) => Os.path(f) / "microkit"
            case _ => halt("Infeasible: no output directory was specified")
          }
      }
    assert(sel4OutputDir.exists, sel4OutputDir.value)

    val attestationDir = sel4OutputDir / "attestation"

    val lang: String = if (isAadl) "aadl" else "sysml"

    val modelFiles: ISZ[Os.Path] = Os.Path.walk(workspaceRoot, T, F, p => p.isFile && p.ext == lang)

    var codegenFiles: ISZ[Os.Path] = ISZ()

    for (r <- codegenResults.resources){
      r match {
        case i: InternalResource =>
          val path = Os.path(i.dstPath)
          assert(path.exists, path.value)

          if (i.overwrite) {
            codegenFiles = codegenFiles :+ path
          }
        case e: ExternalResource => halt(s"Not expecting $e")
        case e => halt(s"Not expecting $e")
      }
    }

    val appraisePath = attestationDir / s"${lang}_appraise.json"
    val provisionPath = attestationDir / s"${lang}_provision.json"
    val scriptPath = attestationDir / s"${lang}_attestation.cmd"
    val modelGolden = attestationDir / s"${lang}_model_golden.txt"
    val codegenGolden = attestationDir / s"${lang}_codegen_golden.txt"

    val provisionModel: ST = provisionST("provision_aadl_targ", attestationDir.relativize(modelGolden).value)
    val attestModel: ST = attestST("aadl_dir_targ", workspace_dir_ENV, attestationDir.relativize(modelGolden).value, workspaceRoot, modelFiles)

    val provisionCodegen: ST = provisionST("provision_microkit_targ", attestationDir.relativize(codegenGolden).value)
    val attestCodegen = attestST("microkit_dir_targ", codegen_dir_ENV, attestationDir.relativize(codegenGolden).value, sel4OutputDir, codegenFiles)

    provisionPath.writeOver(
      st"""{
          |  "RodeoClientRequest_attest_id": "micro_provision_protocol",
          |  "RodeoClientRequest_attest_args": {
          |    "hashdir": {
          |      $attestModel,
          |      $attestCodegen
          |    },
          |    "provision": {
          |      $provisionModel,
          |      $provisionCodegen
          |    }
          |  }
          |}""".render)
    println(s"Wrote: $provisionPath")

    appraisePath.writeOver(
      st"""{
          |  "RodeoClientRequest_attest_id": "micro_protocol",
          |  "RodeoClientRequest_attest_args": {
          |    "hashdir": {
          |      $attestModel,
          |      $attestCodegen
          |    }
          |  }
          |}""".render)
    println(s"Wrote: $appraisePath")

    val workspace_dir = attestationDir.relativize(workspaceRoot)

    scriptPath.writeOver(script(workspace_dir.value, lang).render)
    scriptPath.chmod("770")
    println(s"Wrote: $scriptPath")

    Os.env(env_AM_REPOS_ROOT) match {
      case Some(e) =>
        val results = proc"$scriptPath provision".run()
        if (!results.ok) {
          reporter.error(None(), name, "Provisioning of files failed")
        }
      case _ =>
        println(s"Please set ${env_AM_REPOS_ROOT} environment variable to provision the project")
    }
    return localStore + KEY_ATTESTATION_PLUGIN ~> BoolValue(T)
  }

  def provisionST(name: String, path: String): ST = {
    return (
      st""""$name": {
          |  "env_var": "",
          |  "paths": [],
          |  "omit_file_suffixes": [],
          |  "recursive": false,
          |  "env_var_golden": "",
          |  "filepath_golden": "$attestation_root_ENV/$path"
        |}"""
      )
  }

  @pure def attestST(name: String, envVar: String, goldenPath: String, root: Os.Path, provisionFiles: ISZ[Os.Path]): ST = {
    val files : ISZ[String] = for(i <- for(p <- provisionFiles) yield root.relativize(p)) yield s"\"$envVar/$i\""

    val files_ : ISZ[String] =
      if (files.size > 159) {
        println(st"""Dropping:
                    |  ${(ops.ISZOps(files).slice(159, files.size), "\n")}""".render)
        ops.ISZOps(files).slice(0, 159)
      } else {
        files
      }

    return (
      st""""$name": {
          |  "env_var": "",
          |  "paths": [
          |    ${(files_, ",\n")}
          |  ],
          |  "omit_file_suffixes": [],
          |  "recursive": false,
          |  "env_var_golden": "",
          |  "filepath_golden": "$attestation_root_ENV/$goldenPath"
          |}""")
  }

  @pure def script(workspace_dir: String, lang: String): ST = {
    return (st"""::/*#! 2> /dev/null                                   #
                |@ 2>/dev/null # 2>nul & echo off & goto BOF           #
                |if [ -z "$${SIREUM_HOME}" ]; then                      #
                |  echo "Please set SIREUM_HOME env var"               #
                |  exit -1                                             #
                |fi                                                    #
                |exec "$${SIREUM_HOME}/bin/sireum" slang run "$$0" "$$@"  #
                |:BOF
                |setlocal
                |if not defined SIREUM_HOME (
                |  echo Please set SIREUM_HOME env var
                |  exit /B -1
                |)
                |"%SIREUM_HOME%\bin\sireum.bat" slang run %0 %*
                |exit /B %errorlevel%
                |::!#*/
                |// #Sireum
                |
                |import org.sireum._
                |
                |@pure def exists(p: Os.Path): Unit = {
                |  if (!p.exists) {
                |    println(s"$$p doesn't exists")
                |    Os.exit(1)
                |    halt("")
                |  }
                |}
                |
                |val attestation_dir: Os.Path = Os.slashDir.canon
                |exists(attestation_dir)
                |
                |val workspace_dir: Os.Path = (attestation_dir / "$workspace_dir").canon
                |exists(workspace_dir)
                |
                |val codegen_dir: Os.Path = attestation_dir.up.canon
                |exists(codegen_dir)
                |
                |val env_AM_REPOS_ROOT: String = "AM_REPOS_ROOT"
                |
                |val provision: B = ops.ISZOps(Os.cliArgs).contains("provision")
                |val appraise: B = ops.ISZOps(Os.cliArgs).contains("appraise")
                |val verbose: B = ops.ISZOps(Os.cliArgs).contains("verbose")
                |
                |if (!(provision |^ appraise)) {
                |  println("Usage: (provision | appraise) <verbose>")
                |  Os.exit(0)
                |  halt("")
                |}
                |
                |val AM_REPOS_ROOT: Os.Path = Os.env(env_AM_REPOS_ROOT) match {
                |  case Some(r) =>
                |    val d = Os.path(r)
                |    exists(d)
                |    d
                |  case _ =>
                |    println(s"$$env_AM_REPOS_ROOT environment variable not set")
                |    Os.exit(1)
                |    halt("")
                |}
                |
                |val rust_am_clients: Os.Path = AM_REPOS_ROOT / "rust-am-clients"
                |
                |val cvm: Os.Path = AM_REPOS_ROOT / "cvm"/ "_build" / "install"/ "default" / "bin" / "cvm"
                |val RODEO_ENVS_DIR: Os.Path = AM_REPOS_ROOT / "rust-am-clients" / "rodeo_configs" / "rodeo_envs"
                |
                |val env_roedeo_micro: Os.Path = RODEO_ENVS_DIR / "env_rodeo_micro.json"
                |val env_rodeo_micro_provision: Os.Path = RODEO_ENVS_DIR / "env_rodeo_micro_provision.json"
                |
                |exists(rust_am_clients)
                |exists(cvm)
                |exists(env_roedeo_micro)
                |exists(env_rodeo_micro_provision)
                |
                |val provisionFile: Os.Path = attestation_dir / "${lang}_provision.json"
                |val appraiseFile: Os.Path = attestation_dir / "${lang}_appraise.json"
                |
                |exists (provisionFile)
                |exists (appraiseFile)
                |
                |val cargo: ST = st"cargo run --release --bin rust-rodeo-client -- -c $$cvm"
                |
                |@pure def replace(r: String): String = {
                |  var op = ops.StringOps(r)
                |  op = ops.StringOps(op.replaceAllLiterally("%%workspace_dir%%", workspace_dir.value))
                |  op = ops.StringOps(op.replaceAllLiterally("%%attestation_dir%%", attestation_dir.value))
                |  return op.replaceAllLiterally("%%codegen_dir%%", codegen_dir.value)
                |}
                |
                |if (provision) {
                |  val ptemp = Os.temp()
                |  ptemp.writeOver(replace(provisionFile.read))
                |
                |  val a = st"$$cargo -r $$ptemp -e $$env_rodeo_micro_provision"
                |  var p = proc"$${a.render}".at(rust_am_clients)
                |  if (verbose) {
                |    p = p.console.echo
                |  }
                |  val results = p.run()
                |  exists(attestation_dir / "${lang}_model_golden.txt")
                |  exists(attestation_dir / "${lang}_codegen_golden.txt")
                |
                |  println("Provisioning successful!")
                |
                |} else {
                |  val atemp = Os.temp()
                |  atemp.writeOver(replace(appraiseFile.read))
                |
                |  exists(attestation_dir / "${lang}_model_golden.txt")
                |  exists(attestation_dir / "${lang}_codegen_golden.txt")
                |
                |  val a = st"$$cargo -r $$atemp -e $$env_roedeo_micro"
                |  var p = proc"$${a.render}".at(rust_am_clients)
                |
                |  if (verbose) {
                |    p = p.console.echo
                |  }
                |  val results = p.run()
                |
                |  if (results.ok) {
                |    val o = ops.StringOps(results.out)
                |    if (o.contains("\"RodeoClientResponse_success\":true")) {
                |      println("Appraisal successful!")
                |    } else {
                |      println("Appraisal failed")
                |    }
                |  } else {
                |    cprintln(T, results.err)
                |    Os.exit(results.exitCode)
                |  }
                |}""")
  }
}
