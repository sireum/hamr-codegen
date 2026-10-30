# HAMR Codegen To Do

Planned work too small to need its own design document. Each entry says what is wanted, why, and
enough of the design to start from. When an entry grows open questions that need several rounds,
move it to its own `*-design.md` in this folder and leave a one-line pointer here. Delete an entry
once it is done; the changelog records it. Add each new entry to the contents below.

## Contents

- [Option to write out the AIR model (JSON or MsgPack)](#option-to-write-out-the-air-model-json-or-msgpack)
  (proposed 2026-10-21)

## Option to write out the AIR model (JSON or MsgPack)

**Status:** proposed (2026-10-21).

### What

A path option on codegen. When it is set, codegen serializes the AIR model it was given to that
file, overwriting whatever is there. AIR export is explicit: without the option, codegen writes
no AIR file. This replaces the CLI's automatic post-success SysML dump.

### Why

- **SysML models have no AIR file.** The SysML front end builds AIR in memory, so there is
  nothing to hand to `hamr codegen`, attach to a bug report, diff between two versions of a model,
  or feed to other AIR consumers.
- **Reproducing codegen failures.** The exported AIR can be passed directly to `hamr codegen`
  without SysML parsing or instantiation and without OSATE. Reproducing the run still requires
  the same effective codegen options and external inputs, such as auxiliary code files; those
  are not captured in AIR. For SysML, this includes the options merged from `//@ HAMR:` directives.
- **One explicit mechanism.** SysML codegen already writes AIR JSON as a side effect, but only to
  `<workspace-root-dir>/.slang/<root system>.json`, and only when the workspace root exists and
  codegen succeeded (`cli/jvm/src/main/scala/org/sireum/cli/HAMR.scala`, in `codeGenS`). Remove
  that dump in favor of this explicit option. AADL codegen has no equivalent today.

### Design

- **Option.** Add it to `CommonCli.commonOptions`
  (`shared/src/main/scala/org/sireum/hamr/codegen/common/util/CommonCli.scala`), so both
  `hamr codegen` (AIR input) and `hamr sysml codegen` get it. It is a single path, for example
  `--air-output <file>` (`airOutput: Option[String]`).
- **Format.** Add a second option, `--air-output-format json|msgpack`, defaulting to `json`. The
  format is not inferred from the file extension, and it is independent of `hamr codegen`'s
  existing input flag `--msgpack`.
- **MsgPack is written as Base64 text.** `hamr codegen --msgpack` reads its input as Base64 before
  decoding (`codeGen` in `HAMR.scala`). Writing `conversions.String.toBase64(MsgPack.fromAadl(model, T))`
  means the file round-trips through `hamr codegen --msgpack`. JSON is
  `JSON.fromAadl(model, F)` (not compact), the same call the existing `.slang` dump uses.
- **MsgPack pooling.** Export with pooling enabled (`T`). The current MessagePack reader
  detects pooling from the encoded header, so its setting need not match the writer's;
  `MsgPack.toAadl` accepts both pooled and unpooled input.
- **Which model.** The `Aadl` value handed to codegen: after JSON/MsgPack decoding for AADL
  input, and after SysML instantiation and system selection for SysML input. It is taken before
  any codegen-internal transformation, so the file is a valid input for re-running codegen.
- **When it is written.** At entry to `CodeGen.codeGen`
  (`jvm/src/main/scala/org/sireum/hamr/codegen/CodeGen.scala`), before structural checks,
  `ModelUtil.resolve`, plugin transformations, or platform dispatch. The file must exist even
  when subsequent validation fails, including invalid GUMBO/Slang expressions. Every frontend
  passes the export options through to this shared entry point rather than implementing its
  own serialization. If deserialization or SysML instantiation fails before producing AIR,
  there is no model to export and no file is created or overwritten.
- **Writing.** Create the parent directories (`mkdirAll`), then `writeOver`, then print
  `Wrote: <path>` only after the write succeeds. An I/O failure is reported as a codegen error
  and stops generation; the command returns a nonzero exit status.
- **Remove the legacy SysML dump.** Delete the automatic post-success AIR write in CLI
  `HAMR.codeGenS`. Do not use its `.slang` path as an implicit default for the new option.
  Callers that need an AIR file must request it with `--air-output`.
- **SysML file options.** Add `airOutput` to `resolveFileOptionPaths` in `HAMR.scala`, so a
  relative path in a model file's `//@ HAMR:` options resolves against that file, like the other
  path options.
- **Plumbing.**
  - Regenerate the CLI with cligen: `HamrCli.scala`, and in kekinian `cli/.../Cli.scala` and
    `sireum-cli-spec.json`.
  - Thread the option through `convertSysmlOptions`, `toCodeGenOptions`, both `optionsToStringH`
    and `mergeOptionsM` in `HAMR.scala`.
  - The `sireum_hamr_codegen` and `sireum_hamr_sysml_codegen` MCP tools pick it up from the spec.

### Tests

- For a SysML model and an AADL `.slang/*.json` model, emit both formats and read them back. JSON
  goes through `JSON.toAadl`; MsgPack goes through Base64 decoding and then `MsgPack.toAadl`. The
  result must equal the model codegen was given.
- Run `hamr codegen` (with `--msgpack` for the MsgPack file) on each emitted file and check that
  the output matches the run that produced it, reusing the same effective codegen options and
  external inputs, including merged SysML file options.
- Verify that both pooled and unpooled MsgPack inputs decode to the original model, and that
  MsgPack exports use pooling.
- For a decodable AIR model, verify that the export is written before each of these failures:
  a structural check, model resolution, and invalid GUMBO/Slang expressions. Read the export
  back and check that it equals the original input model, before any codegen transformation.
- A deserialization or SysML-instantiation failure that produces no AIR creates no export;
  an existing destination file is left unchanged.
- An unwritable export destination reports an error, returns nonzero, and does not proceed
  with generation or print a successful `Wrote:` message.
- A relative `airOutput` in a SysML file's `//@ HAMR:` options resolves against that file.
- An existing file at the path is overwritten.
- A successful SysML run with a workspace root and no `--air-output` creates no automatic
  `.slang/<root system>.json` dump. With an explicit destination, only that export is written.
