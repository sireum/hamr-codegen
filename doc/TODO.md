# HAMR Codegen To Do

Planned work too small to need its own design document. Each entry says what is wanted, why, and
enough of the design to start from. When an entry grows open questions that need several rounds,
move it to its own `*-design.md` in this folder and leave a one-line pointer here. Delete an entry
once it is done; the changelog records it. Add each new entry to the contents below.

## Contents

- [Option to write out the AIR model (JSON or MsgPack)](#option-to-write-out-the-air-model-json-or-msgpack)
  (proposed 2026-10-21)
- [Should `--system-testing` gate the JVM/Slang system tests too?](#should---system-testing-gate-the-jvmslang-system-tests-too)
  (proposed 2026-10-30)
- [System tests: warn about incomplete setup, and offer isolated runs](#system-tests-warn-about-incomplete-setup-and-offer-isolated-runs)
  (proposed 2026-10-30)
- [Tag generated Verus contract clauses with their GUMBO ids (`proof_note`)](#tag-generated-verus-contract-clauses-with-their-gumbo-ids-proof_note)
  (proposed 2026-11-10)
- [Investigate restoring `Structural` on generated Rust enums](#investigate-restoring-structural-on-generated-rust-enums)
  (proposed 2026-11-11)
- Toolchain findings from the RASE experiments (proposed 2026-11-18):
  - [Notices on successful runs that look like problems](#notices-on-successful-runs-that-look-like-problems)
  - [System tests: injecting a data port for one consumer of a fan-out](#system-tests-injecting-a-data-port-for-one-consumer-of-a-fan-out)

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

## Should `--system-testing` gate the JVM/Slang system tests too?

### What

Decide whether the JVM/Slang (arsit) path should take its system-test scaffolding from the
opt-in `--system-testing` option, as the Microkit path now does, rather than from
`--runtime-monitoring`.

### Why

`--system-testing` (in the CAmkES/Microkit option group, off by default) replaced the
`ENABLE_TEST_SCHEDULER` experimental option and currently affects only Microkit; codegen warns
and ignores it on other platforms. On the JVM, the system-test scaffolding -- the
`SystemTestSuite`, each component's `*_SystemTestAPI` and the user-editable `SystemTests.scala`
(`GumboXRuntimeMonitoring.genSystemTestSuite` / `genSystemTest`, gated in `GumboXPlugin`) -- is
generated only under `--runtime-monitoring`, so a user who wants system tests gets the runtime
monitor as well, and vice versa. Microkit separated the two (TestScheduler-design.md, D22).

### Design

To investigate first:

- What `--runtime-monitoring` adds to a JVM deployment beyond the system-test scaffolding (the
  monitor GUI, the per-dispatch GUMBOX checks), and whether any of it is needed by the system
  tests themselves.
- Whether the scaffolding is cheap and harmless enough on the JVM to generate unconditionally,
  which would make an option unnecessary there.
- If an option is wanted: move `--system-testing` out of the CAmkES/Microkit group into the
  common options, gate `genSystemTestSuite` / the `*_SystemTestAPI` on it, and keep
  `--runtime-monitoring` for the monitor alone -- noting that existing JVM projects that rely on
  `--runtime-monitoring` for their system tests would need the new option added.

## System tests: warn about incomplete setup, and offer isolated runs

### What

Make it harder for a system test to pass or fail because of state an earlier test left behind:

1. **Warn about incomplete setup.** Tell the user when a test steps threads whose pre-state it
   did not set.
2. **Offer isolated runs.** Give `bin/run-tests.cmd` a way to start each test from the state
   initialization leaves, rather than from whatever the previous test left.

### Why

System tests share one boot: threads run `_initialize` once, and the runner only normalizes
the schedule position between tests (TestScheduler-design.md, D13). Each test therefore has to
establish the pre-state of every port and state variable it depends on, which is a source of
errors:

- **Forgetting to reset.** A test that does not set some thread's inputs or state variables
  silently starts from whatever the previous test left there. It may pass or fail depending on
  which tests ran before it and in what order -- including under a `TESTS=` filter (D17), which
  changes the order and the predecessors. The whole-component setters (D14) make a complete
  setup possible, but only for the threads a test thinks to set up.
- **The setters cannot reach everything.** They, and any reset codegen could generate, cover
  only what codegen knows about: ports and GUMBO state variables. Any other field of a thread's
  application struct, and any `static` or other state in the user's behavior code, keeps its
  value from the previous test, and the generated setters alone cannot restore it.
- **Property tests have the same problem inside one test.** `run_property`'s generated cases,
  and its shrink attempts, share the running system ("Property-based tests" in
  TestScheduler-design.md), so a case can depend on the state an earlier case left. Isolating
  each registered test does not isolate the cases within it.

D13 rejected one-test-per-boot to keep a run fast; the second and third points are cases it
did not weigh.

### Design

**The ways to run, as separate choices.** Document all four, and let `run-tests.cmd` select
among those that are implemented:

| Approach | What it restores | Main limitation |
|---|---|---|
| Shared execution (today) | Schedule position only | Every test must set everything it depends on, and cannot reach hidden state |
| Generated reset plus user reset hooks | Ports, state variables, and whatever hidden state the developer's hook resets | Only as complete as the reset; works on real hardware too |
| QEMU snapshot/restore | Guest RAM, CPU and supported device state, including application statics and seL4's own state | Host orchestration; unverified for the generated Microkit configuration |
| Fresh boot per test | Everything initialization sets up | Repeats boot and initialization for every test |

Keep **fresh boot per test** as the straightforward isolation reference and the fallback, and
prototype and measure **snapshot/restore** before choosing a default for CI.

- **Reset hooks.** Codegen cannot discover every `static` or external resource, but user code
  can: a per-thread hook that rebuilds the application object and clears its private state.
  Calling `_initialize` again on its own is not a reset -- port queues and receive cursors, the
  scheduler's and controller's bookkeeping, the observers, the heap and devices may also hold
  state, and each needs its own reset.
- **Snapshot/restore.** Boot once, park after initialization at an explicit "ready for test
  selection" point, save a whole-machine snapshot, and restore it before each test.
  - QEMU's `savevm`/`loadvm` snapshots include CPU, RAM, device and writable-disk state, but
    need writable qcow2 storage to hold them (the images boot with `-kernel` and no disk today,
    so one would have to be added) and support only some devices. `-snapshot`, which only keeps
    disk writes temporary, is a different thing. Whether this works for the generated
    configuration -- the board, the timer and the serial device -- is to be verified first.
  - **Test selection has to move after the restore.** Today `TESTS=` is patched into the
    controller's ELF before boot (D17), so every restore of one snapshot would run the same
    tests. The host has to tell the restored guest which test to run: a new input channel to
    the controller, or writing the selection into guest memory over the QEMU monitor before
    resuming.
  - Verdicts and logs have to live on the host, outside the restored guest, and the host's
    serial buffering has to be split per test.
- **Fresh boot per test** (`run-tests.cmd --isolate`, say):
  - Get the selected tests from a `--list` run, then build and boot once per test. The image
    is relinked, not recompiled, per filter (only `TESTS` changes; see "Build plumbing" in
    TestScheduler-design.md), so the cost is mostly QEMU boot and initialization per test.
  - The filter matches by substring (D17), so `suite::test` would also select
    `suite::test_more`. Per-test runs need an exact match: a filter syntax for it, or matching
    the whole qualified name in this mode. (Snapshot/restore needs the same.)
  - Combine the per-test verdicts into the usual summary and exit code, and keep each test's
    serial log so a failure can be read on its own. `QEMU_TIMEOUT` would apply per boot.
- **Property tests.** None of the per-test approaches isolates `run_property`'s cases. Decide
  whether a case should get a reset (only the reset-hook and snapshot approaches could do that
  without rebooting per case), or whether property tests are documented as needing to set
  everything in every case.

**Warnings about incomplete setup.** The controller sees which setters a test calls and which
threads it steps; it cannot in general tell which values arbitrary C or Rust behavior code
actually depends on. So the warnings report incomplete *setup*, not detected dependencies:

- In the generated `tests.rs` and the documentation, at least: say plainly that state is not
  reset between tests, that the setters reset only ports and state variables, and that other
  application state is never reset.
- In a run, per test: the inputs and state variables of the threads it steps that it did not
  set. Values produced by the test's own earlier steps count as set up, so a test that builds
  on what its first steps produced is not flagged. A warning, not a failure.
- Running the selected tests in a different order (reversed, or shuffled with a printed seed,
  like `PROPTEST_SEED`) and comparing verdicts catches some contamination, but cannot establish
  isolation: a contaminated test can give the same verdict in both orders.

Update D13 (and the "Test isolation" section) with whatever is decided, and the website's
System Testing chapter, whose §6 and §12 describe the shared-boot behavior.

## Tag generated Verus contract clauses with their GUMBO ids (`proof_note`)

**Status:** proposed (2026-11-10).

### What

Put `#![verifier::proof_note("...")]` in front of every `requires`/`ensures` clause that codegen
emits from a GUMBO `assume`, `guarantee` or compute `case`. The note text identifies the
component and the clause, for example `"<thread classifier>::guarantee::<id>"`. A failed
obligation is then reported under the GUMBO id that produced it, both in Verus's diagnostics and
in its `--output-json` output.

### Why

- **Codegen already keeps the id, but only in a comment.** `GumboRustUtil.processGumboSpec` and
  `processGumboCase` emit `// guarantee <id>` / `// case <id>` (plus the descriptor) above each
  clause. Verus ignores comments, so the only way to map a failure back to its clause is by
  source position.
- **Verus has a mechanism for this.** `proof_note` attaches a string to a single
  `requires`/`ensures` clause (or an `assert`/`assume`). When that obligation fails, the
  diagnostic carries a `note: <text>` line. With `--output-json`, every function's entry under
  `func-details` lists `obligation_proof_notes` (all labeled obligations it had to prove) and
  `failed_proof_notes` (the labeled obligations that failed). That gives GUMBO-level
  requirement traceability without scraping error positions. The attribute was added in Verus in
  March 2026 (verus-lang PRs #2226 and #2233), so the pinned `verus-release=0.2026.10.26.d17fbd0`
  in `microkit_versions.properties` supports it. Documentation: Verus guide,
  `reference-attributes.md`, section "`#[verifier::proof_note("text")]`".

### Design

- **Syntax.** The inner form (`#!`) goes immediately before the clause expression, after the
  existing comment lines:

  ```rust
  ensures
      // guarantee lowerBound
      //   the output is at least the lower bound
      #![verifier::proof_note("Example_Thread_i::guarantee::lowerBound")]
      api.out >= LOWER,
  ```

  `assert`/`assume` take the outer form, `#[verifier::proof_note("...")] assert(...)`.
- **Where.** Add the attribute in `GumboRustUtil.processGumboSpecH` and `processGumboCase`, so
  every caller in `GumboRustPlugin` (initialize guarantees; compute assumes, guarantees and cases;
  the integration constraints on port getters and setters) picks it up. Also add it to the
  clauses that `VerusVCSerializer` emits from GUMBO ids (for example `srcGuaranteeId` and
  `dstAssumeId`), and to any GUMBOX/`proof fn` output that calls a `compute_spec_<id>_*` function.
- **One note per clause.** Verus attaches the note to the next clause only and rejects a second
  `proof_note` on the same clause. `getAadlRequires` (`AADL_Requirement`) and `getAlertEnsures`
  (`Monitor_Requirement`) currently emit several comma-separated clauses in one `RAST.ExprST`.
  Either split each into one clause per port, each with its own note, or join them with `&&`
  into a single clause.
- **Note text.** GclResolver reports duplicate spec names only within a subclause, so the same
  id can appear in different components. Qualify the note with the component and the clause
  kind. Verus stores notes as sets, so two clauses with the same text would be merged.
  - Decide whether the qualifier is the classifier or the instance path. A classifier shared by
    several instances generates one set of code, so the classifier is probably the right choice.
  - Decide whether to include the descriptor text. Probably not: it can be long, and it is
    already in the comment.
- **Not `custom_err`.** `#![verifier::custom_err("...")]` replaces the primary error message but
  is left out of the JSON `func-details` sets. It also cannot be combined with `proof_note`. Use
  `proof_note`.
- **Markers.** The attribute lives inside the existing contract markers
  (`GumboMarkers.initializationEnsures` and the others), so regeneration replaces it together
  with the clauses.
- **Requires vs. ensures in the JSON.** A failed `ensures` is recorded against the function
  that declares it. A failed `requires` is recorded against the *caller* that failed to establish
  it. Document this for anyone building a traceability report from `func-details`.
- **Follow-up (separate entry if wanted).** Run `verus --output-json` from the generated build
  scripts, and turn `func-details` into a per-requirement report that marks each GUMBO clause as
  verified, failed, or not exercised.

### Tests

- Expected output for the Rust/Verus Microkit test models changes. Regenerate it only after the
  diffs have been reviewed.
- A generated crate with one deliberately violated guarantee, one violated compute case and one
  violated integration constraint should fail Verus with `note: <qualified id>` for each. With
  `--output-json`, the failing function's `failed_proof_notes` should contain exactly those ids,
  and its `obligation_proof_notes` should contain every GUMBO clause in that function's contract.
- Two components that use the same GUMBO id produce distinct notes.
- The rewritten `AADL_Requirement` / `Monitor_Requirement` clauses still verify, and each port
  clause carries its own note.

## Investigate restoring `Structural` on generated Rust enums

**Status:** proposed investigation (2026-11-11).

### What

Determine whether generated Microkit Rust enums can safely derive Verus's `Structural` again,
allowing executable `==` comparisons to be verified against logical equality in contracts.
The generation site is `plugins/rust/types/CRustTypePlugin.scala`, where `Structural` remains
commented out after `derive(Copy, Clone, Debug, PartialEq, Eq)`.

### Why and historical context

- HAMR originally generated `Structural`. Commit
  [`604eb1eb`](https://github.com/sireum/hamr-codegen/commit/604eb1ebe499126a59848a0e8fdcf9336eb51b97),
  which switched to `cargo-verus`, removed it from enum derives. The remembered reason was
  compatibility with ordinary Rust compilation or the older Verus/Cargo integration; the
  commit does not record the original diagnostic.
- Verus later fixed `Structural` macro output under ordinary Cargo in
  [`2788156e9` / PR #2007](https://github.com/verus-lang/verus/commit/2788156e9ed0b0311e74de257bb93fd5bab3d4e8)
  (2025-11-23). The fix suppresses verification-only expansion during ordinary compilation
  and adds an unverified Cargo regression test. It is included in the paper's pinned Verus
  `0.2026.10.26.d17fbd0`. This supports revisiting the workaround, but does not establish
  which failure originally prompted HAMR's change.
- In the RASE Task 3 Luna run, executable equality on generated enums produced failed
  postconditions; rewriting eight source lines as pattern matches made all three affected
  component proofs pass. A standalone example with the pinned verifier also verifies enum
  `==` when `Structural` is derived. That check alone does not establish compatibility with
  the generated project's complete build and test paths.

### Investigation and acceptance checks

- Try the derive on scratch-generated enum definitions, preserving their `repr(C)`, explicit
  discriminants, and existing derives. Check executable equality across the shared `data`
  crate boundary as well as within a crate, including its connection to equality in contracts.
- Check ordinary `cargo build` and `cargo test`, component/GUMBOX tests, `cargo-verus verify`
  and build paths, system proof crates, and the supported Microkit deployment targets.
  A direct `verus` invocation alone is insufficient to retire the historical workaround.
- Confirm the minimum supported Verus and macro-crate versions, including ordinary Cargo's
  ghost-erasure path. Decide whether compatibility requires conditional generation or a
  version requirement before enabling the derive by default.
- If supported, update generation and the agent guidance recommending pattern matching,
  with regression coverage for both ordinary compilation and verification. Document any
  remaining limitations. Update golden/expected outputs only with explicit user approval.

## Toolchain findings from the RASE experiments

**Status:** proposed (2026-11-18).

These entries come from the RASE experiments (November 2026), in which coding agents developed
and changed HAMR Microkit systems under the HAMR agent workflows. A code such as
`t3-sonnet:HC-04` is a finding from one run's own assessment: Task 2 (`t2-`) built a new
three-component pipeline, and Task 3 (`t3-`) added a return-to-launch mode to an existing
system. The number of runs that report a problem is what measures how often it occurs.
Thirteen findings from those runs are already fixed: a bare state variable in a case assume
read as post-state (codegen `de2ec819`), `sysml tipe` exiting 0 when it reported errors
(kekinian `9941d156`), `make verus` succeeding from cargo's cache without a results line
(codegen `5ca39381`), `NoSend`/`MustSend` rejected on composition port aliases (codegen
`b0e03bb9`), `sys_assert!` being unusable in helpers that return a value (now
`sys_check!`/`sys_check_eq!`), repeated identical expected violations never all being met,
trailing whitespace in generated Makefile rules (codegen `dad81bbb`), GUMBO library
functions that verified code could not call (each executable library function now gets an
`assume_specification` tying it to its `<name>_spec` twin, unless its body uses
arithmetic, where the machine-integer and mathematical versions can disagree; codegen
`0e29c457`), regeneration rewriting `generators.rs` in crates a change does not touch
(each crate now gets strategies only for its own port and state-variable types and what
they contain; codegen `abd5de95`), `sysml tipe` accepting a property that binds a
schedule place twice (`GclResolver` now runs `ScheduleNextRel.decorate`; codegen
`74bd9408`), and, on 2026-11-18, GUMBO diagnostics that pointed away from the fix:
`In(x)` in an assume (no longer shown as `In(In(x))`), `modifies (a, b);`, `MustSend(p, v)`
on an event port, and a reserved word used as a name. The parser errors that follow a
reserved word still cascade; that would take error recovery in the grammar. Also fixed on
2026-11-18: a panic in component code aborted the whole host-test process
(t3-sonnet:HC-06, where a `test_compute` written before a new input data port did not set
it). The entry points are now `extern "C-unwind"`, so the panic fails just that test and
proptest can shrink its input, and the unset-port panic names the port and its
`test_apis::put_<port>` setter.
Fixed on 2026-11-19: host tests that call an entry point several times had to clear the
previous dispatch's event outputs themselves (t3-astra:DG-02). Each component's
`test_apis.rs` now has `dispatch_boundary()`, which clears the outgoing event and
event-data mocks and keeps component state, data ports and incoming ports; it is a
no-op for a thread without such outputs (codegen `a5a264a2`). Once `MicrokitPortSemantics-design.md` D2/D5 is
implemented, it should call the runtime's own boundary code.

### Notices on successful runs that look like problems

**What.** Successful runs print messages that agents treated as possible failures and had
to investigate. Five runs reported this (t2-sonnet:HCT-03, t2-opus:HT-03,
t2-fable:H2-TOOLCHAIN-NOISE, t2-luna:HAMR-003, t2-sol:AHE-002).

**Done (2026-11-18).** The `AM_REPOS_ROOT` provisioning notice (`AttestationPlugin.scala`)
printed on every Microkit codegen run. It is now printed only with `--verbose`, and worded
as optional.

**Remaining: sDDF drivers and the GIC.** A successful build prints "info: failed to open
sDDF driver directory 'drivers/gpio'", and a QEMU boot prints a GIC fallback warning.
Neither comes from codegen. The sDDF message is sdfgen's: `Sddf(path)` in the generated
`meta.py` scans every driver class sdfgen 0.35.0 knows, including GPIO, and the sDDF that
LionsOS `7554a0f` vendors has no `drivers/gpio`. The fix is upstream: an sdfgen release
that matches the vendored sDDF, or sdfgen not reporting a missing optional class.
Meanwhile, list both messages as expected in the build-and-test documentation and the agent
guidance.

### System tests: injecting a data port for one consumer of a fan-out

**What.** A data-port injection writes the shared channel, so every consumer of a fan-out
sees the injected value, not only the one under test. t3-opus:TOOL-04 found this when
injecting the Mode Manager's output: FirewallTx's input changed as well. The system-test
documentation says injected values are per output, which is accurate, but this
consequence is not spelled out.

**Investigated (2026-11-19): documented, receiver-scoped injection deferred.** The
consumers of a connection share the producer's region; each one's cursor and retained copy
of a data value (`last_<port>_payload`) are in its own protection domain, which the
controller cannot write. D39 (TestScheduler-design.md) holds that different values for the
consumers of one connection describe no reachable state, so for most tests this is no
limitation. It is one for fault-injection tests, which feed one consumer a value its producer
did not send. Done: the generated PreState container names a field's other consumers, and
TestScheduler-design.md ("A data input is per connection outright") explains the behavior
and that such a test must expect the other consumers' reports. Injecting the consumer's own
state does not substitute when, as here, the consumer has no state variable for that input.

**Remaining.** A receiver-scoped data-input override, for fault injection, built with
`MicrokitPortSemantics-design.md` D1/D2: the override would write the consumer's private
input slot after test injections are applied and before it is frozen. Building it before
then needs a per-reader injection region, a test-variant change to every consumer's C
getter, and the checks' and latch cursors following it -- plumbing D1 replaces.
