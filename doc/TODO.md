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
file, overwriting whatever is there. When it is not set, nothing changes.

### Why

- **SysML models have no AIR file.** The SysML front end builds AIR in memory, so there is
  nothing to hand to `hamr codegen`, attach to a bug report, diff between two versions of a model,
  or feed to other AIR consumers.
- **Reproducing codegen failures.** One file reproduces a codegen run with no SysML parsing or
  instantiation and no OSATE.
- **One explicit mechanism.** SysML codegen already writes AIR JSON as a side effect, but only to
  `<workspace-root-dir>/.slang/<root system>.json`, and only when the workspace root exists and
  codegen succeeded (`cli/jvm/src/main/scala/org/sireum/cli/HAMR.scala`, in `codeGenS`). AADL
  codegen has no equivalent.

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
- **Which model.** The `Aadl` value handed to codegen: after JSON/MsgPack decoding for AADL
  input, and after SysML instantiation and system selection for SysML input. It is taken before
  any codegen-internal transformation, so the file is a valid input for re-running codegen.
- **When it is written.** Before codegen runs, so the file exists even when codegen then fails;
  failures are the main case for reproducing a run. If deserialization or SysML instantiation
  fails, there is no model and nothing is written.
- **Writing.** Create the parent directories (`mkdirAll`), then `writeOver`, then print
  `Wrote: <path>` like the existing dump. An I/O failure is reported as a codegen error.
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
  the output matches the run that produced it.
- A relative `airOutput` in a SysML file's `//@ HAMR:` options resolves against that file.
- An existing file at the path is overwritten.

### Open questions

1. **The existing `.slang` dump in SysML codegen.** Keep it as is, make it the default value of
   the new option when a workspace root is set, or remove it now that the option exists.
   Something may read those files (for example IVE or the AADL-side tooling), so check before
   removing it.
2. **MsgPack pooling.** `MsgPack.fromAadl` takes a `pooling` flag. `T` gives smaller files, but
   it must match whatever OSATE writes and what other MsgPack consumers expect.
