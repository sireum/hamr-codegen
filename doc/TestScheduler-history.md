# Test Scheduler: Implementation History

Companion to [TestScheduler-design.md](TestScheduler-design.md), which describes the design as it
stands.  This document records how it was built: the stages and what verified each, what
building and running it established and corrected, stage 7's steps as built, and what the review
rounds changed.  A quoted section name that is not in this document is one in the design
document; decision numbers (D1-D38) are its "Decisions".

## Stages

| Stage | Work | Verified by |
|-------|------|-------------|
| 1 &#10003; | This document | review |
| 2 &#10003; | `TestSchedulerPlugin` + the `test_scheduler.*` bundle, and a `scheduler.c` that is a **rewrite of the default, not a copy with a flag** — see below | golden `expected/` diff on temp-control **and** on a contract-free model (the vms `data_receiver` model); the three existing monitor bundles unchanged (at stage 2; since stage 7 the stripped injected entries shift map addresses and channel ids); QEMU boot under `CONFIG=test_scheduler.mk` behaving as the default variant does |
| 3 &#10003; | Controller PD injected at lowest priority with the busy-wait blocking loop (volatile `ack_seq` load, run-once guard) and the all-ready kick (D4), channel + region maps, generated Rust command API, `system_tests!` macro (with suites) + in-crate runner + `sys_assert_*` (D11-D12), `TESTS=` filter via ELF section (D17), serial verdict + `run-tests.cmd` host driver forcing `MICROKIT_CONFIG=debug` (D18), `TESTS` added to the rebuild hash and routed to `meta.py` (D17), user-editable `tests.rs` | QEMU boot of temp-control under `CONFIG=test_scheduler.mk`, with a passing test, a deliberately failing test, the two run in both orders (D13), and `TESTS=` selecting a suite, a single test, and a filter matching nothing (D17) |
| 4 &#10003; | Completion-driven dispatch: bridge notify-back (C2) emitted unconditionally, per-slot timeout dropped, pads skipped, timer demoted to a watchdog at 10x the slot's configured budget (floor 1 s) raising `TEST_FLAG_OVERRUN` | `hstep(20)` inside a 12 s window that also covered build-check, boot and QEMU startup -- timer-gated needs 20 s of guest time alone |
| 5a &#10003; | Inspection + port injection (D15): observable-region inventory (`ObservableRegion`: every port region, its type, queue size and readers), C accessors in the controller bridge over the generated `sb_queue` API, `system_tests/inspect.rs`, `channels::` constants | 3 tests on target: injected values reach shared memory, state vars observable after a dispatch, the controller's reads do not disturb the real consumer's |
| 5b &#10003; | State var injection (D16/D16a): plugin-declared `inj_<thread>_sv_<var>` regions mapped `r` into the owning thread with `setvar_vaddr` and `rw` into the controller, thread-side `get_inj_sv_*` + `is_injection_enabled()` NULL gate, Rust ingest in `libComputePre`, `put_` suppressed on the `sv_` regions | 2 tests on target: an injected state var is adopted by the next dispatch and observable on its `sv_` region; a following dispatch with nothing set keeps it. Both checked against negative controls |
| 5c &#10003; | Whole-component setter (D14): one `<thread>_PreState` container per thread with no `Default`, and `set_<thread>` delegating to the existing per-field setters | a test that establishes all 7 of `tcp_tct`'s fields in one call and reads them back; deleting one field is a compile error (`E0063: missing field`) |
| 6 | *Deferred -- possible future work, not planned.* Serial CLI PD + host driver over QEMU stdio. Blocked on UART contention and on whether it earns its cost at all; see "Interactive CLI (deferred)" | n/a |
| 7 &#10003; | Contract observation in the controller (D19-D26): (1) `ContractObserverPlugin` generating the observer layers into `crates/observers`, with the completion/dispatch split; (2) monitor PDs rewritten as thin wrappers; (3) `sv_` port creation and `handleCBackend` separated from the monitor wiring and driven by system testing as well; (4) the component-layer gate widened to "has GUMBO thread contracts", controller `SystemView` on its own `test_obs_get_*` cursors with last-value caching, pending-injection values, `TestSink` with `VIOLATION` lines and one `FAIL` per test, the recording window, `observe::` API; (5) the scheduler's observation park, `completed_seq`, and the completion check at command completion; (6) the two switches at build, suite and test level; (7) overrun handling | (1-2) golden diffs showing code moving without behavior change, `make verus` still passing on a model with monitors, and the monitor variants' on-target logs unchanged (they keep checking CEP_Post after a failed CEP_Pre); (3) golden diffs for a model built with system testing and without `--runtime-monitoring`, which now carries the `sv_` plumbing and no monitor bundles; (4-7) on target: a test that breaks a CEP_Post fails with one `FAIL` line and its `VIOLATION` lines, and the host driver's counts reconcile; a test with several violations still prints one `FAIL`; a violation in the last slot of a test's last command is charged to that test; a command that dispatches nothing (`run_to_thread` while already parked before its target, `info_state`) leaves the marking and saved pre-states untouched; the same test under `observe::expect` passes, and fails if the expected violation does not occur; checking does not change what `inspect::get_*` returns to a test, and isolette's initialization tests still see the post-initialization outputs; an injected state variable does not produce a false CEP_Post; an out-of-range injected input is reported as an assumption not met and skips that dispatch's CEP_Post, not a failure; an IEP_Post violation fails the run through `DONE init=failed`; a negative test does not make the next test fail; a run with system testing but without `--runtime-monitoring` still checks state-variable contracts; each switch disables its layer at build, suite and test level without affecting the next test; both switches off at build level gives a run with no parks; a test that re-enables a layer turned off at build level fails with a message naming the switch; a watchdog trip produces one `INFO` and no false violations; and a model with event ports (`gumbo-verus/pure_event_port`) checks the same events the threads saw, including an event port with two consumers. `SystemTestingTests` now regression-tests these, among them `nothing_dispatched_nothing_checked`, `last_slot_violation_charged` and `injected_state_is_the_owners_only` |
| 7r &#10003; | Review rounds (D27-D32, and the D10 amendment): see "As built: review fixes" and "As built: later review rounds". Built along the way without a stage of their own: proptest on target (`run_property`), `system_test_files!`, the test table and list-only mode, `command_outcome` (D10), C threads' contract checks, `GclResolver`'s `MustSend` value fix, the host scheduler harness, and `SystemTestingTests` with `models/SystemTesting` | `SystemTestingTests`: codegen checks in CI; opt-in QEMU runs of its system tests (three builds, and a listing) and of four monitor images; the host scheduler harness |

### Stage 2 scope

Stage 2 was originally sketched as "the default scheduler plus a command budget, so the
variant is behaviorally identical to the default". Two review passes have made that framing
wrong: the test variant's `scheduler.c` shares the default's *structure* but replaces its
control flow, and carries infrastructure the default does not have.

What stage 2 delivers:

| | |
|---|---|
| Plugin | `TestSchedulerPlugin`, gated on `ENABLE_TEST_SCHEDULER` (D5). Planned as a subtype of `UserLandMonitorPlugin`; built standalone (see "Plugin structure") |
| Bundle | `test_scheduler.meta.py`, `.scheduler.c`, `.scheduler_config.h`, `.user_config.h`, `.mk` (C3) |
| Control flow | `try_advance` / `on_slot_complete` state machine replacing the default's `notify` / `next_partition` (C1) |
| Command path | `test_cmd` / `test_status` regions, sequence-number handshake, decode and validation |
| Schedule publish | the variant's own schedule region (built as `test_schedule`) and init-time publish — absent from the default template, and needed for slot-index-to-channel mapping |
| Robustness | bounded `RunTo*`, filtering of stale timer expiries, `Stop` |
| Timing | timer-gated with pads honoured, so wall-clock behavior matches the default |

What stage 2 did **not** deliver: any controller. With none, nothing issued commands, so the
scheduler started in an implicit run-forever mode at the all-ready point. That made the stage 2
image *observationally* equivalent to the default variant — which is precisely what made it
verifiable on its own: the golden diffs confirmed the bundle was well-formed and disturbed
nothing else, and a QEMU boot confirmed the rewritten control flow still scheduled the system
correctly before any of it was driven by commands.

**Superseded by stage 3.** `init()` now parks at `TEST_CMD_NONE` and the all-ready point hands
over to the controller, so the current scheduler dispatches nothing until asked. `RUN_FOREVER`
survives as a *command* — what an interactive CLI (stage 6, deferred) would use for "run freely
until I interrupt" — but it is no longer the startup state. This subsection is kept because the
staging is the reason stage 2 could be verified at all, not because it describes current
behavior.

## Stages 1-5 on target

**Stages 1-5 and 7 are built and running on seL4.** Stage 6 is deferred indefinitely (see
"Interactive CLI (deferred)").

| Stage | State |
|-------|-------|
| 1 document | done |
| 2 bundle + scheduler rewrite | done |
| 3 controller, command API, macro, runner, selection, verdict | done |
| 4 completion-driven dispatch | done |
| 5a inspection + port injection | done |
| 5b state var injection (D16/D16a) | done |
| 5c whole-component setter (D14) | done |
| 6 serial CLI | deferred -- possible future work |
| 7 contract observation | done (shared `crates/observers`, thin monitor wrappers, `sv_` plumbing via `StateVarPortsPlugin`, the controller hosting the checks, the observation park, the switches, overrun handling), and revised through twelve review rounds (D27-D32, and the D10 amendment) |

New files: `microkit/plugins/testing/TestSchedulerPlugin.scala` and `TestControllerInjector.scala`;
`ExperimentalOptions.ENABLE_TEST_SCHEDULER`; registration in `MicrokitPlugins`; stage 7's
`ContractObserverPlugin.scala` and `StateVarPortsPlugin.scala`; golden tests in
`MicrokitTests.scala` (temp-control, with and without `--runtime-monitoring`, and the
contract-free vms model); and `SystemTestingTests.scala` with `models/SystemTesting`.

Observed end to end on temp-control under QEMU, at stage 5 -- a run of the two seeded `smoke`
tests and 5b's two `state_vars` tests; 5a's three tests are not in it (later runs add the `LIST`
table and `DONE`'s `init=`):

```
TEST SCHEDULER | All partitions ready, handing over to the test controller
TEST | BEGIN smoke::schedule_advances_by_hyperperiod
TEST | PASS  smoke::schedule_advances_by_hyperperiod
TEST | BEGIN smoke::stepping_one_slot_at_a_time_wraps
TEST | PASS  smoke::stepping_one_slot_at_a_time_wraps
TEST | BEGIN state_vars::injected_state_var_is_adopted_by_the_thread
TEST | PASS  state_vars::injected_state_var_is_adopted_by_the_thread
TEST | BEGIN state_vars::state_is_kept_when_nothing_is_injected
TEST | PASS  state_vars::state_is_kept_when_nothing_is_injected
TEST | DONE  matched=4 passed=4 failed=0
OK: 4 of 4 tests passed
```

The two `state_vars` tests are D16's on-target check. They were run against negative controls
first — one asserting a value that was never injected, one injecting without stepping — and
both failed, so the passes are not vacuous: the value only appears on the `sv_` region after a
real dispatch has adopted it. They lived outside the codegen repo then, because `tests.rs` is
seeded once per model and the results tree is regenerated by `MicrokitTests`;
`SystemTestingTests` now installs its own `tests.rs` into a generated tree, which is where
such tests go.

**Isolette.** The JVM system tests of `hamr-system-testing-case-studies` are ported to the
`microkit_mcs` variant of isolette in INSPECTA-models, one file per JVM test class under
`crates/test_controller/src/system_tests/tests/` (`smoke_`, `illustrations_`, `cat_`,
`zhaoxiang_tests.rs`), joined by `system_test_files!`. `.ci/ci.cmd` runs them under QEMU via
`bin/run-tests.cmd` whenever `qemu-system-aarch64` is present. Isolette is also the model on
which the host `make test` exposed the raw-extern ingest (see the table below).

### What running it established

Every one of these was previously supported only by argument:

- **D4's busy-wait holds.** The lowest-priority controller spins on `ack_seq` and the threads
  still run; the interleaved `BEGIN` / thread dispatches / `PASS` in the log is the evidence.
- **The boot handshake works despite ordering.** The controller's protection domains
  initialize *after* the scheduler sends its kick. The notification really does stay pending.
- **The run-once guard is needed and sufficient.** One `DONE`, and the re-entry it absorbs is
  visible in the log as a second `compute entrypoint invoked`.
- **The controller is genuinely absent from `part_ready_check`** — the handshake names only
  the three real threads.
- **Completion-driven dispatch is a large win.** A `hstep(20)` test finished inside a
  12-second window that also covered build-check, boot and QEMU startup; timer-gated it would
  have needed 20 s of guest time alone.
- **The `TESTS=` filter selects correctly and forces a rebuild** on every change: at stage 3,
  with two smoke tests, unset -> 2 matched, `stepping` -> 1, `smoke::` -> 2, `nosuchtest` -> 0.
- **D14's completeness guarantee is real, not documentary.** Deleting one field from a
  `set_tcp_tct` call fails the build with ``E0063: missing field `sv_fanError` ``. That is the
  whole value of the container: adding a state variable to the model breaks stale tests at
  compile time instead of silently defaulting the field and producing a green run against a
  pre-state nobody meant to set up.
- **D16 round-trips through the thread.** A value set on `tcp_tct.latestTemp` is adopted at
  the start of the next dispatch and comes back on the `sv_` region; a following dispatch with
  nothing set keeps it rather than reverting. Draining the injection queue is what makes
  "nothing set" mean "keep your own state" — no dirty flag was needed.
- **D18's mandatory-`DONE` and `matched=` rules fire** (the `init=` field came later, with
  stage 7). Success gives `OK` / exit 0; a zero-match filter and a
  deliberately hung test (no `DONE`) both fail with a named diagnostic. The count-reconciliation
  rule is unexercised, since it needs serial output to be lost mid-run.

Stage 7's own verification is recorded in its "As built" sections, and is repeated
automatically by `SystemTestingTests`.

### What building and running it corrected

Findings that no amount of review produced, recorded so they are not rediscovered:

| Finding | Consequence |
|---------|-------------|
| `MicrokitUtil.KiBytesToHex` rounds **up** to the 4 KiB alignment | Regions spaced 1 KiB apart collapsed onto one address; `test_status` and `test_schedule` aliased. Region vaddrs must be 4-KiB-aligned. |
| The stage-2 sentinel `#define TEST_CONTROLLER_CH 63` was never replaced | `#if TEST_CONTROLLER_CH < MICROKIT_MAX_CHANNELS` silently compiled out both the kick and the command arm. The real id is threaded from the MSD (an `_MON` PD's scheduling domain id *is* its channel id) and the `#if` was **deleted** — a guard that silently removes code is a worse failure mode than a link error. |
| The all-ready branch still armed the default's settling timer | The schedule free-ran and the controller was never dispatched. It now sets `scheduler_running` and notifies the controller; `init()` parks at `TEST_CMD_NONE` rather than `RUN_FOREVER`. |
| The controller signals its own `_MON` from `init()` | That reaches the scheduler on the controller channel and is **not** a command; it arrives after the hand-over, and `try_advance` finds no new command (`test_cmd->seq == accepted_seq`) and does nothing. |
| **QEMU's serial console emits CRLF** | Every line carries a trailing `\r`, so `Z("0\r")` is `None` and the driver's parse threw a Java trace *after* printing a green-looking log. Output is normalized before parsing, and the numeric parses use `getOrElse(-1)` so a format change reads as a clean `FAILED`. |
| **QEMU never exits on its own** | Waiting for the timeout made every successful run cost the full bound. Piping into `sed '/DONE/q'` does not help: once the suite ends the guest is idle, so no further write raises SIGPIPE. The driver polls the log and kills the process group; successful runs went from ~300 s to ~13 s. |
| The D16 ingest declared `get_inj_sv_*` / `is_injection_enabled` in a raw `extern "C"` block in `lib.rs` | The seL4 image linked, but the host `make test` (per-crate `cargo test`) has no C bridge, so every thread with state vars failed with `undefined symbol: is_injection_enabled` -- caught by the isolette CI job, not by the golden tests or the on-target runs. Now routed through `extern_c_api.rs` with `#[cfg(test)]` stubs, as `is_monitoring_enabled` always was. Any new C symbol a component crate calls must take that route. |
| Synthetic **model ports** reach the component test harness and the verification model | The first D16 implementation added `inj_sv_X` input ports; they surfaced as `api_inj_sv_*` fields in `PreStateContainer_wGSV` and as `// channel` fields in `sys_<id>_proof/system_state.rs`. Reverted: injection regions are plugin-declared, not model ports (D16a). |
| `MakefileTemplate` has **two** `system.mk` templates | `RUST_PROFILE_DIR` was added only to the MCS one while the link rules come from the shared `MakefileContainer`, leaving domain-scheduled models referencing an undefined variable. Any change touching `system.mk` must be applied to `systemMakefileDomainScheduler` *and* `systemMakefileMCS`. |

## Stage 7: contract observation, as built

### As built: steps 1-2

`ContractObserverPlugin` generates `crates/observers`; `GumboMonitorPlugin` and
`GumboSysAssertMonitorPlugin` now emit only the schedule-locating code, `thread_of`, a
`MonitorView` over the monitor's API, and a `LogSink`. Where this departs from the design's
sections on splitting the checks and on the thin wrappers:

- **Threads, not channels.** `on_complete` / `on_dispatch` take a generated `Thread` enum, one
  variant per model thread. Channel ids belong to an MSD variant and the crate is shared by all
  of them, so each consumer maps its own channels onto `Thread` (`thread_of` in the monitors;
  the controller's `channels::` in step 4). `COMPONENT_TRANSITIONS`, the table of each thread's
  transitions (in- and out-places) that `validate_schedule` walks, is keyed by `Thread` too.
- **`SystemView` getters return the monitors' API types**, not `Option<T>`: `T` for data ports
  and state variables, `Option<T>` for event-data ports, `bool` for event ports -- exactly what
  the monitors' getters return, so the monitors cannot behave differently.  (Later review rounds
  changed the last: a pure event port's getter is `Option<u8>` -- `Option` of its empty
  *payload*, the `u8` GUMBOX stands in for the data a pure event does not carry -- and the
  monitors poll their `bool` API by port kind; see "As built: later review rounds".) The
  controller's "never written" case (D25) is for step 4 to represent.
- **The sink takes an `Event` enum** (`IepPostViolation`, `CepPreViolation`, `CepPostViolation`,
  `CepPostSkipped`, `CepPostExcused`, `SysAssertViolation`, and the schedule-conformance events;
  step 4 added `CheckSkipped`), carrying pre/post state as `&dyn Debug`. `LogSink` lives in each
  monitor's own app module, so the log records keep their target and the monitors' output is
  unchanged.
- **The crate is generated only when a monitor consumes it**: when the gumbo or the sys-assert
  monitor was actually injected (its `KEY_<monitor>_Model_Transformed` store key is set), which
  takes `--runtime-monitoring` and the MCS user-land scheduler, and for the gumbo monitor GUMBO
  thread contracts (state variables, before D33), for the sys-assert monitor a composition
  (D34). The first
  version re-derived only the first two conditions, so domain-scheduled models built with
  `--runtime-monitoring` -- which get the `domain_monitor`, not these monitors -- carried an
  `observers` crate nothing used; see "Where stage 7 stands". System testing alone requests the
  crate from step 4, when the controller becomes a consumer (D22); the test scheduler is
  MCS-only as well, so domain-scheduled models never get it.
- **Fully generated crates overwrite their `Cargo.toml`.** A component whose profile is
  `userEditable = F` -- the gumbo and sys-assert monitors, `domain_monitor`, any synthetic
  component without an explicit profile -- already had its sources overwritten on every run,
  but its manifest was written once, so a dependency codegen added later (here `observers`)
  never reached a tree regenerated in place. The manifest now follows the sources: overwritten,
  headed "do not edit". User-editable crates are unchanged.

Verified on isolette under QEMU, old code against new, in both the gumbo and the
sys-assert monitor variants: identical monitor output on a clean run, and identical violation
messages -- pre/post values included -- with one IEP_Post, one CEP_Pre, one CEP_Post and one
system property forced to fail in both trees. `make verus` passes on both monitor crates.

### As built: step 3

A new `StateVarPortsPlugin` (`plugins/gumbo/StateVarPortsPlugin.scala`, registered in
`GumboPlugins` after `ContractObserverPlugin` and before `GumboMonitorPlugin`) owns the `sv_`
plumbing:

- **Model transform.** `isRequested` is Microkit, `--runtime-monitoring` **or**
  `ENABLE_TEST_SCHEDULER`, the MCS scheduler, and threads with state variables. For each such
  thread it adds the missing `sv_<var>` output port on the thread, the matching port on its
  process, and the thread-to-process delegation, then re-resolves; only the thread port is
  registered as synthetic, which is why `StoreUtil.modelSymbolTable` finds the process port as
  its same-named twin. It
  stores `KEY_ModelTransformed`. With no monitor attached, the process ports are unconnected
  outputs, and their regions come from `CConnectionProviderPlugin`'s unconnected-port loop as
  planned.
- **C backend.** Its `handle` is the old `GumboMonitorPlugin.handleCBackend`, moved verbatim:
  `is_monitoring_enabled()`, the `put_sv_*` guards, the Rust externs, wrappers and mocks,
  `KEY_RUST_MONITORING`, and the `lib.rs` hooks. It runs once (`KEY_CBackend`), so the
  sys-assert monitor's "skip if the gumbo monitor already ran" check is gone along with its
  own `handleCBackend` override.
- **Monitor wiring.** `wireMonitorStateVars` still wires each thread to the monitor, but it
  takes the source ports from `StateVarPortsPlugin.sourcePortElements` and creates them only if
  the transform above has not already done so. `updateThreadInModel` moved to the new plugin's
  object. `GumboMonitorPlugin` is down to two phases, and the monitor PDs and bundles still
  require `--runtime-monitoring`.
- **D5 dropped.** `TestSchedulerPlugin` no longer errors when a model with state variables is
  built with the test scheduler and without `--runtime-monitoring` (D22).

Two parts of the planned step 3 were deferred to step 4, where they have a consumer (both
done there):

- The `observers` crate gate stays "a monitor was injected". Widening it to the test scheduler
  now would generate a crate nothing uses.
- The component layer's gate stays `hasThreadsWithStateVars` until then as well. The `sv_`
  plumbing itself stays keyed on state variables: a thread with contracts but no state
  variables has no `sv_` ports to create.

Verified: the refactor alone left every `MicrokitTests` expectation unchanged (all 38 at the
time), and `R2U2MonitorTests` and `SharedMemorySafetyTests` still pass. A new case,
`test_sched_no_rm_sysml_…temp-control__9F67`, builds temp-control with the test scheduler and
without `--runtime-monitoring`. It generates no monitor crates, PDs or bundles and no
`observers` crate. `tcp_tct` still carries `is_monitoring_enabled()` and the guarded `put_sv_*`,
and the `sv_` regions appear in both `microkit.system` variants. On target, isolette regenerated
with `ENABLE_TEST_SCHEDULER` and without `--runtime-monitoring` passes all 31 system tests under
QEMU. Those tests inspect and inject state variables, so they exercise the plumbing end to end.

### As built: step 4

The controller hosts the layers. (Step 5 revised two of the points below: data ports never
written, and event reads within one check; see "As built: step 5".  The review rounds made the
system layer's frame per composition: a system assertion is now preceded by
`s.focus_system(c)` as well, and `frame_ended` takes the composition; see "Frames and event
values".)

- **The observers crate is generated for system testing too.** `ContractObserverPlugin.isRequested`
  also accepts `TestSchedulerPlugin.hasTransformed`. The test scheduler is MCS-only, so
  domain-scheduled models still get no crate.
- **The component layer's gate widens** from `hasThreadsWithStateVars` to "has checkable GUMBO
  thread contracts": the layer is generated when any thread -- Rust or, since the review fixes,
  C -- has a GUMBO subclause GUMBOX compiles, state variables or not.
- **Two hooks on `SystemView`, both defaulted.** Generated checks call `s.focus(Some(thread))`
  before a thread's check and `s.focus(None)` before each system assertion, and skip the check
  when `s.missing()` says a read found a region never written: `IEP_Post`, `CEP_Post` and the
  pre-state capture report `Event::CheckSkipped` instead (a skipped capture leaves no pre-state, so
  the completion is skipped too), and a system assertion is not reported. The monitors' views take
  the defaults -- no focus, never missing -- and their `LogSink` ignores `CheckSkipped`, so their
  behaviour is unchanged. This is how D25's "`None` means never written" is represented while the
  getters keep returning the monitors' API types (but for a pure event port's, `Option<u8>` since
  the later review rounds).
- **`validate_schedule` takes slices.** It took a `hamr::Schedule`, a type the data crate defines
  only under `--runtime-monitoring`; the observers crate did not compile for a model built without
  it. It now takes the slot count, channels and user-partition bits. The sys-assert monitor passes
  its schedule's fields, the controller the published `test_schedule`.
- **The observation cursors are per region, not per getter.** `CComponentPlugin_MCS` writes the C
  bridges in the first handle pass, before GUMBOX and hence the observers crate exist, so the
  controller's `test_obs_get_*` functions cannot be derived from the getters the checks use.
  `TestSchedulerPlugin` emits them in its first stage whenever the model has a GUMBO subclause on a
  thread or a composition (`checksRequested`), for every observable region: one cursor for a data
  port or state variable, and for an event or event-data port one per reader -- its producer, each
  consumer, and `sys` for the system assertions. `ObservableRegion` gained `isEvent`,
  `isPureEvent` and `readers` for this. A second handle stage, run once the observers crate is in
  the store, adds the crate dependency.
- **The controller crate is fully generated.** Its manifest was written once and kept user edits,
  so the `observers` dependency could not reach a tree regenerated in place. The test script is
  `system_tests/tests.rs`, which the plugin writes once and never overwrites, and the
  `*_tests.rs` files it names, which the user creates; so nothing the user edits was in the app
  module or manifest: the controller's profile is now
  `userEditable = F`, and both are overwritten. Existing trees change only in those files' header
  line.
- **`system_tests/observe.rs`** (generated when the crate has a layer):
  - `ControllerView`: data ports and state variables through a last-value cache; an injected
    state variable from its pending value, recorded by `inspect::put_<thread>_sv_<var>` and
    cleared when `on_complete` sees the thread (shown to the owning thread's checks only, since
    later review rounds; the system layer reads the state the thread actually has); event and
    event-data ports through the reader's cursor, chosen by `focus`, so an event is seen once
    per reader. A getter with no region warns at codegen time and reads as missing.
  - `TestSink`: `TEST | VIOLATION IEP_Post|CEP_Post <thread> <init | hp=.. slot=..>`,
    `SysAssert <composition>/<property> at <point> ...` and `Schedule <composition>: ...`, with the
    pre/post values as `INFO` lines; a failed CEP_Pre, an excused CEP_Post and a skipped check are
    `INFO` only (D24); `excuse_post_on_failed_pre` is set.
  - The recording window: violations during `on_init` set `init=failed`; inside a test they are
    recorded; between tests they are dropped.
  - The API: `observe::expect(Expect::CepPost(t) | SysAssert(c, p))` (an `IepPost` variant was
    removed later: initialization is checked before any test, so it could never be met), with
    `observe::Thread` and constants `observe::sys::<composition>::{COMPOSITION, <property>}`;
    `observe::take()`, which returns a `Taken` and clears the test's violations so they no longer
    fail it. Expectations match at property level, not by place.
  - `on_init`, `on_complete(ch, hp, slot)` and `on_dispatch(ch, hp, slot)`; the last two were
    wired to the scheduler's parks in step 5 (`at_park`, `at_command_end`).
- **Harness.** `fail_with` prints a test's first failure as its `TEST | FAIL` line and later ones
  as `INFO`, so one test prints one `FAIL` however it failed. `run_all` calls `observe::on_init()`
  once, before the first test; each test body runs between `begin_test()` and `end_test()`, and
  `end_test` fails the test for unexpected, untaken violations (`contract violation`) and for each
  expectation never met. `DONE` always carries `init=ok|failed`, and `run-tests.cmd` fails the run
  on `init=failed`.

Verified: isolette with the test scheduler, with and without `--runtime-monitoring`, passes its 31
system tests with `DONE ... init=ok`, and its `gumbo_monitor.mk` and `sys_nominal_monitor.mk`
images still build; temp-control without `--runtime-monitoring` (event-data ports, and the R2U2
aux code) passes with `init=ok`. With isolette's `thermostat_rt_mhs_mhs` changed to put
`heat_control = On` at initialization, the run prints `TEST | VIOLATION IEP_Post
thermostat_rt_mhs_mhs init` with its post-state, `DONE ... init=failed`, and fails; the three
tests that assert that same initial output still see it through `inspect::`, so the checks' reads
did not consume it. In the golden tests only cases with a monitor or the test scheduler change.

### As built: step 5

The park is as designed ("The scheduler's observation park"), with these specifics:

- **Wire format.** `test_command_t` gains `observe` and `obs_ack`; `test_status_t` gains
  `next_ch`, `completed_seq` and `obs_seq`, appended so the existing fields keep their offsets.
  The controller set `observe` on every command when it hosted the checks (`api::OBSERVE`), and
  never otherwise, so a model without contracts runs exactly as before.  (Step 6 replaced this
  with `api::observe_flag`: only while a layer is live.)
- **Scheduler.** In `try_advance`, before arming the watchdog for a user slot, an observing
  command publishes the position (`publish_position`, now shared with `publish_status`) and then
  `obs_seq`, notifies the controller and returns; the slot is dispatched when a notification
  finds `obs_ack == obs_seq`. `on_slot_complete` increments `completed_seq`. A notification on
  the controller's channel while a slot is in flight is now ignored -- nothing the controller
  sends then can change that slot.
- **Controller.** `api::issue` services parks while it waits for `ack_seq`:
  `observe::at_park(status)` runs the completion check if `completed_seq` moved and then the
  dispatch check for `next_ch`, and only then writes `obs_ack`. After the ack,
  `observe::at_command_end(status)` runs the completion check for the command's last dispatch.
  Both go through `account_completion`, which completes the dispatch in flight only when
  `completed_seq` moved by exactly one and names it; a larger jump prints `INFO` and drops every
  saved pre-state (`ComponentContracts::forget`, new). An `OVERRUN` does the same for the dispatch
  that never completed. Suspending the system layer until it can resume (D30) is left to
  step 7.
- **System assertions are checked where they are entered.** `cascade_acc` (the cascade, returning
  the final marking and the places it entered) used to report every
  place still marked, so a place waiting at a join -- isolette's "after ma" waits for the
  regulator branch -- had its assertions re-evaluated at every later completion of the frame.
  With a test injecting mid-frame, that compared `ma`'s alarm with a temperature `ma` never saw:
  42 violations across 7 of isolette's tests. Now a completion checks the places it entered (its
  transition's out-places and the cascade's), and the places the cascade from START enters are
  checked when it happens -- `on_init`, and the restart at END -- so the system layer's
  `on_init` takes a view and a sink, and the monitors changed with it. An "after X" assertion is
  about the moment X completes.
- **A data port never written reads as its default.** The thread's own C getter returns a
  zero-initialized last value until something arrives, so the thread computes with the default;
  skipping the check (step 4) made the checks see something the thread did not, and isolette's
  `mmm` and `mrm` skipped every CEP_Pre. `missing` now applies to state variables only.
- **Event reads are idempotent within a check.** A system assertion reads a port more than once
  (`x.is_some() && x.unwrap()...`), which the monitors' API allows; a second dequeue returned
  `None` and the controller panicked on temp-control. The view caches the first read until the
  next `focus`.

Verified on target, with the test scheduler: isolette with and without `--runtime-monitoring`
passes its 31 tests with no violation, and temp-control (event-data ports) its 2; isolette's
`gumbo_monitor.mk` and `sys_nominal_monitor.mk` images build and run 60 s under QEMU with no
violation. Negative tests, in a scratch copy of isolette:

- Without a fault: `observe::expect(CepPost(mhs))` fails with "expected violation did not occur";
  a `take()` asserting that violation fails; a test driving `mhs`'s CEP_Pre false (lower above
  upper) passes, with `assumption not met` and the excused CEP_Post as `INFO`.
- With `mhs`'s seeded bug (heat Off below the lower desired temperature): every test that
  reaches the branch fails with `VIOLATION CEP_Post thermostat_rt_mhs_mhs`, its pre/post values,
  and `VIOLATION SysAssert nominal/Normal_Mode_Heat at after mhs`, and exactly one `FAIL` --
  `violate_mhs_gumbo_contract`, 64 violations, one `FAIL`; 12 `FAIL` lines for `failed=12`. The
  test that `take()`s them passes; the one that `expect`s only the CEP_Post still fails, on the
  system assertion it did not declare; the test after the negative ones passes; and the CEP_Pre
  test, whose CEP_Post is excused, still fails on `Normal_Mode_Heat`, since a thread's assumption
  does not excuse a system assertion (D24).

Not yet verified at step 5: a model whose event port has two consumers, and the watchdog path.
Both were verified at step 7.

### As built: step 6

The switches are as designed ("Enabling and disabling checks", D23):

- **Build level.** `make CONFIG=test_scheduler.mk GUMBO_CHECKS=off SYSVERIF_CHECKS=off` (also
  honoured from the environment, so `GUMBO_CHECKS=off bin/run-tests.cmd` works). Both are in the
  MCS `system.mk` rebuild hash beside `TESTS`, and reach `test_scheduler.meta.py` through the
  environment of the MSD step rather than as arguments: most of `meta.py` is written once, so a
  tree regenerated in place keeps its old argument parser, which rejected unknown arguments and
  broke the build. An old `meta.py` ignores the variables instead. The test-selection block turns
  them into bits of `TestSelection.flags` (bit 0 GUMBO off, bit 1 SYSVERIF off), rejects any
  value but empty, `on` and `off`, and warns about a switch for a layer the model does not have.
- **Controller.** `observe` loads the flags at `on_init`. A layer is *live* when generated and
  not off at build level: only live layers are called at all, and commands set `observe` only
  while one is (`api::observe_flag`), so with both off no park is taken. A layer is *on* when its
  violations are reported; every check tags the layer it belongs to, and `TestSink` drops what an
  off layer finds. `begin_test` resets both to the build's setting.
- **Test level.** `observe::set_gumbo(bool)` and `observe::set_sysverif(bool)`, generated only for
  the layers the model has, so asking for another is a compile error. Turning on a layer that is
  off at build level fails the test, naming the make variable.
- **Suite level.** `suite name(gumbo = off, sysverif = on) { .. }` in `system_tests!`. The macro
  first normalizes every suite to `suite name [settings] { .. }` -- a macro cannot repeat a
  suite's settings inside the per-test repetition otherwise -- and each test starts with
  `suite_settings!`, which calls `observe::suite_setting::<layer>(on|off)`. Keys and values are
  paths, so a typo or a layer the model does not have is a compile error.

Verified on target, isolette with `mhs`'s seeded bug and scratch tests: with defaults, a
`(gumbo = off)` suite hides the CEP_Post but still reports `Normal_Mode_Heat`, the next suite
checks again, and a test that turns GUMBO off and back on sees only the second dispatch's
CEP_Post. With `GUMBO_CHECKS=off` no CEP_Post is reported, system assertions still are, and
`set_gumbo(true)` fails with "the GUMBO checks are off for this build (GUMBO_CHECKS=off); a test
cannot turn them back on" -- one `FAIL`, later failures as `INFO`. With both off nothing is
reported and `obs_seq` does not move: no parks. `GUMBO_CHECKS=maybe` fails the build. Isolette's
31 tests still pass with and without `--runtime-monitoring`.

### As built: step 7

Overrun handling is as designed ("After a watchdog trip"), for an unobserved dispatch as well:

- `observe::lose_track` drops every saved pre-state and, when a system layer is live, suspends
  the system layers until the next hyperperiod (or, D30, the hyperperiod's first user slot),
  with one `TEST | INFO` line naming the cause: a command ending in `OVERRUN` ("channel N
  overran its slot"), or `completed_seq` moving by more than one ("N dispatch(es) completed
  unobserved").
- While suspended, the system layers' completions are not run. The first park in the next
  hyperperiod -- every user dispatch parks, so it is the hyperperiod's first dispatch -- resumes
  them with `on_init`, which restarts the marking at START and checks the places the cascade
  enters, and prints "system assertions resumed at hp=N".  (A later review let a loss detected
  at the hyperperiod's first park resume there; a further one restricted that to losses that
  are not overruns -- see "After a watchdog trip"; and later ones start the resumed frames at the
  hyperperiod's start, `HP_FRAME_AT`, not at the resume -- see "Frames and event values".)
- The GUMBO layer needs no suspension: with the pre-states dropped, each thread's next
  completion is skipped and its next dispatch saves a fresh one.

Verified on target:

- **Watchdog.** A scratch fault makes isolette's `mhs` spin for several seconds, once, past its
  1 s watchdog. The test sees `FLAG_OVERRUN`, the controller prints the overrun and, at the next
  frame, the resume, and three further hyperperiods run with no violation; the next test is
  unaffected. Isolette's 31 tests still pass.
- **An event port with two consumers.** A scratch copy of `gumbo-verus/pure_event_port` with a
  second receiver on `snd`'s event and data: both receivers latch the announced value with no
  violation -- with one shared event cursor, the second receiver's check would have seen no
  event and reported `holdOtherwise` when its state changed. With the second receiver made not to
  latch, only it reports `CEP_Post`, its pre-state showing the event present, while the first
  receiver is clean.

### As built: review fixes

A review of this document against the code (2026-09-29) found these gaps, now closed:

- **A command that did not do what it was asked fails the test (D10).** The scheduler set
  `OVERRUN`, `UNREACHABLE` and `BAD_COMMAND`, but nothing read them: most tests discard a
  command's status, so an overrun passed silently. `api::issue` now hands every command's flags
  to `harness::command_outcome`, which fails the running test naming the command and the reason
  (`sstep failed: a thread overran its slot's watchdog, or is still running after one (flags
  0x8)`); between tests, in the
  runner's normalization, there is no test to fail and it prints `INFO` instead (later
  rounds retry the normalization after an overrun, and fail each test that cannot start at slot
  0, unrun).
- **The test table and list-only mode (D17)** were required but never built; see "Selecting
  which tests run".
- **C threads' contracts are checked.** GUMBOX processed Rust threads only, so a C thread's
  GUMBO contracts -- temp-control's sensor has an initialize guarantee and an integration range
  -- generated no checks, in the controller or the monitors. `GumboRustPlugin` now also records
  C threads with a GUMBO subclause (`getCThreadsWithContracts`), and `GumboXRustPlugin` runs the
  language-independent part of its analysis for them into `cComponentContributions`: the
  integration, initialize and compute predicates and the subclause's functions, without the
  test harness, `cb_apis` or bridge module a Rust crate would get. `crates/observers` and the
  monitors take both maps. A C thread's state variables are checked only if its code publishes
  them; otherwise the check reads them as never written and is skipped. If the model trips
  GUMBOX's float restriction (floating-point types it cannot yet generate predicates over), C
  threads are left unchecked rather than failing a model whose Rust side is fine.
- **The monitors read each value once per run.** A monitor reads each port through one cursor,
  and every read dequeues. When the completion of `prev` and the dispatch of `next` both read an
  event port in one monitor slot -- a producer's output guarantee and its consumer's input --
  the second read got nothing, and the consumer's guarantee looked broken. Checking
  temp-control's C sensor exposed it. Each monitor's `MonitorView` now reads through a per-run
  cache, cleared by `begin_monitor_run()` (later `begin_monitor_run(api)`) at the start of
  `timeTriggered`. The controller never had the problem: it has a cursor per reader.  (Later
  rounds replaced the cache for event ports with per-reader event counts and per-frame values;
  see "Event ports" and "Frames and event values".)
- **`MustSend(port, value)` with a port as the value.** `GclResolver` spliced the value into the
  rewritten expression without rewriting it, so a port stayed a bare name that the Verus and
  GUMBOX code could not resolve. Existing models passed only state variables there; C threads'
  contracts, now compiled, exposed it. The value is now rewritten like any other expression.
- **Event queues deeper than one** draw a codegen warning (see "Event ports").
- **`SystemTestingTests`** regression-tests all of the above and the stage 7 behaviour on its own
  model, `models/SystemTesting`: a Rust producer, a Rust and a C consumer of one event-data
  port, and a composition assertion, with behaviour code that breaks a contract when a test
  injects a trigger value and overruns its watchdog once. The codegen tests run in CI; the QEMU
  test (opt-in, `HAMR_ST_QEMU`) runs the system tests in three builds -- default,
  `GUMBO_CHECKS=off`, both off -- and lists them in a fourth, and checks each test's verdict
  and lines (and the listing's table).

Verified: the new suite passes, QEMU test included; isolette with and without
`--runtime-monitoring` and temp-control pass their system tests from clean copies; both models'
monitor images build with Verus (0 errors) and run clean under QEMU; with temp-control's sensor
initializing to 71, the controller reports `init=failed` and the gumbo monitor the sensor's
IEP_Post violation.

### As built: later review rounds

Eleven more reviews (2026-09-29 to 2026-10-01) compared this document with the code.  What they
changed, in the order of this document (each is described where it belongs).  The review cycle
ended with the twelfth round: the code had converged -- the eleventh and twelfth found only
small items -- and it was followed by a full clean on-target run of the case-study models.

- **Scheduler.**  The watchdog's deadline check replaced the slot generation, which could blame
  the next slot for a stale expiry; a command that ends `UNREACHABLE` is acknowledged (it hung
  the controller); a command is acknowledged only when it completes, so the scheduler and the
  controller no longer signal each other forever after `Stop`; run-to budgets and `hstep` counts
  saturate; `completed_seq` counts user slots only; `run_to_thread(0)` is rejected; a command
  that completes while the position is on the slot whose dispatch overran reports `OVERRUN`
  again -- the runner's `run_to_slot(0)` (or a test's `info_state`) at slot 0 returned clean
  flags while the slot-0 thread overran (pad-last builds), so the runner's retry could not see
  it.
- **Build.**  The controller and the `sv_` regions are stripped from the production image
  whenever they survive in `normal`, whichever plugin wrote it last; the production build no
  longer compiles the controller (`EXTRA_IMAGES`, set only by `test_scheduler.mk`) or runs Verus
  on it; injected threads leave the frame budget alone ("Relationship to the runtime monitor");
  the rebuild stamp names each switch; `TESTS` reaches `meta.py` through the environment and is
  quote-safe in the stamp (a filter holding `'`, `"` or a backtick no longer breaks either; `%`
  is escaped before `'`, so no two filters hash alike), `run-tests.cmd` rejects a filter holding
  `$` (which make expands) or `\` (which the stamp's `echo` reads), and the filter is length-
  and encoding-checked; `run-tests.cmd` classifies a line by how it starts, so a `FAIL` line
  quoting `TEST | PASS` is not a pass, gives QEMU a second after `DONE` appears before stopping
  it, and fails a run whose `DONE` line lost its `init=ok` or whose counts do not add up to
  `matched` (a listing, `list=1`, runs nothing and is exempt) -- a line cut short is not a pass,
  and one cut before its counts is reported as lost output, not as a filter that matched
  nothing; `meta.py`'s plugin contributions sit in marker regions ("Build plumbing").
- **Runner.**  The between-test `run_to_slot(0)` is repeated while it reports `OVERRUN` (the
  scheduler takes the late completion first; up to four tries in all) -- since the scheduler
  re-reports an overrun at the slot a command stops on, also for an overrun at slot 0.  A test
  that still cannot start at slot 0 with no overrun outstanding -- the position is elsewhere, or
  it is slot 0 but `OVERRUN` is still reported, since an overran slot keeps the position -- is
  failed without running its body ("not run: the test could not start at the frame's start")
  rather than counting its steps, and leaving its injections, from wherever the overran thread
  left the position; so is every test after `api::stop()` ("not run: the session was stopped
  (api::stop) by an earlier test"), even one that issues no scheduler command and would
  otherwise pass against a frozen system.
- **Proofs and linting.**  The system-VC generator works on the model as written
  (`StoreUtil.modelSymbolTable`: synthetic components, synthetic ports and their connections
  removed, through the whole component tree, a synthetic thread port's same-named process twin
  included -- derived only from ports whose parent is a thread, since only the thread-side `sv_`
  port is registered as synthetic), so enabling system testing or runtime monitoring no longer
  puts the test controller, the monitors or the `sv_` mirror ports into `sys_*_proof`; the
  component `test_apis` get no accessors for synthetic ports either.  The Microkit linter
  rejects a thread connected directly to itself (keep such state in a GUMBO state variable).
- **Controller.**  An injection into a producer's output is hidden from the producer's own
  check; a pending state variable is shown only to its owner's checks; after an overrun the
  overran thread's cursors are drained and the system layer resumes only in the next
  hyperperiod, and the resumed frame starts at the hyperperiod's start -- the last completion of
  the hyperperiod before (`HP_FRAME_AT`, recorded from `LAST_DONE_AT`), not the resume or the
  first stop in the new one -- so an injection made at its first stop, or at a stop on the
  trailing pad before it, belongs to it; an injection a consumer that already ran this
  hyperperiod will receive only in the next is reported with a `TEST | INFO` line (only for a
  consumer with no slot left in the hyperperiod, while a dispatch remains in it, and where the
  model has compositions and the system assertions are live and switched on); initialization
  outputs are skipped on the producer's own cursor; injecting a state variable is Rust-only; an
  expectation for a switched-off layer fails at once; `STOPPED` fails the command that meets it,
  so `api::stop()` in a test fails its next command, while `stop` itself never fails
  (`command_outcome` returns at once for it -- before, only `STOPPED` was exempted for it, and
  an overrun still outstanding made it print a failure); a controller getter with no region to
  read from (the "has no region ... the contract checks that read it are skipped" warning) marks
  the value missing for every type, so those checks really are skipped -- a `bool` used to get
  `false` and an `Option` `None`, made-up values that could pass or fail a check; suites are
  modules; state variables are recognised by the synthetic marker, not the `sv_` prefix.
- **Frames and compositions.**  Event values per frame and per composition (a logical clock),
  set by each dispatch, `frame_ended`, several compositions,
  aliases read as the reader received them, and partial compositions, whose warnings count only
  the aliases a concrete property's check reads ("Frames and event values", "Compositions").
- **Monitors.**  They read unconnected output ports (a contract may name a port nothing
  consumes; the monitor maps the producer's own region, as it does for unconnected inputs);
  their event tracking is per reader and per frame; `begin_monitor_run(api)` drains every event
  port and keeps the latest element, as the controller does (one element per run left the rest
  to arrive, as new events, in later runs).  A pure `event port` is polled by port kind, through
  the monitor API's `bool` getter: its `SystemView` getter is `Option<u8>` (`Option` of the
  empty payload), as GUMBOX reads it, in contract and composition getters alike (the composition
  path used `bool`, so a port read by both had two types), and polling it as event data did not
  compile -- the GUMBO monitor of `gumbo-verus/pure_event_port`.  Every event getter is now
  `Option<T>`, so the `bool` branches of the `SystemView` getters in the controller and the
  monitor adapter are gone (the monitors still poll a pure event port through the API's `bool`
  getter), and the generated `observers/src/lib.rs` says which getter differs from the monitors'
  API, and what a `get_recv_` alias returns.  The deep-queue warning fires only for an input
  port.

Two review findings were shown not to occur and were left as they are: a second overrun
overwriting the first, and a re-reported overrun charged to a thread that completed normally.
Both need a dispatch between an overrun and its late completion, and the scheduler dispatches
nothing then.  The unobserved-dispatch path (`forget_ran`) cannot be reached from a test and is
covered by code review only.

`SystemTestingTests` covers the rest: every system test in `models/SystemTesting/qemu/tests.rs`
under QEMU, run in three builds and listed in a fourth (on a model with three compositions, one
ending before the others and one leaving a thread out), the four monitor images booted clean, a
host harness driving the generated scheduler C through the cases QEMU cannot time
(`models/SystemTesting/host/scheduler_harness.c`: a stale expiry, a late completion, an overrun
at the slot a command stops on, the park, `UNREACHABLE`, saturated counts, `run_to_thread(0)`),
and codegen checks, among them that a self-connected thread is rejected, that the production
build and the system proofs leave the controller out, that `run-tests.cmd` rejects a filter make
would rewrite, that a monitor drains its event ports, and that a monitor polls a pure event port
a contract reads through its `bool` API (with `HAMR_ST_QEMU`, the `gumbo_monitor.mk` image of
`gumbo-verus/pure_event_port` is also built).  The model's `src` also signals a pure event port,
`ping`, whenever it sends on `val` (its guarantee `pinged`: `(mode != 99 implies MustSend(ping))
& (mode == 99 implies NoSend(ping))`), received by `dst1`, which does nothing with it; `nominal`
aliases `s_ping = src.ping` and `d1_ping = dst1.ping` in a property `Pinged`, `after dst1:
HasEvent(d1_ping) implies HasEvent(s_ping)` -- so a pure event port is read by a contract and a
composition alike, and the controller, the `sys_nominal` monitor and the QEMU builds compile
that path; the runtime-monitoring codegen check asserts the monitor polls `get_srcp_src_ping`
through the `bool` API and that the getters are `Option`.

### Where stage 7 stands

**Stage 7 is complete** (2026-09-29, revised through 2026-10-01): a violation at any dispatch
fails the test that caused it, each layer can be switched off at build, suite and test level,
and an overrun fails its test with one `INFO` line and no false violations (an unobserved
dispatch costs the `INFO` line alone). The steps were built in
order, 2026-09-25 to 2026-09-29, each recorded in its "As built" section above; the review
fixes followed.

Along the way: `ContractObserverPlugin.isRequested` had generated the `observers` crate for
domain-scheduled models, whose only monitor is the `domain_monitor`, which does not use it; it
now asks whether a consumer was actually injected. And isolette's and temp-control's `ci.cmd`
boot the `gumbo_monitor.mk` and `sys_nominal_monitor.mk` images under QEMU for 60 s after each
build (INSPECTA `4fe6d903`), failing on any violation, a panic, or a monitor that never logged.

## Related Defects Found Along The Way

None was caused by this design; all were found while building it or testing it.

### Fixed: the Rust link path ignored `RUST_MAKE_TARGET`

`system.mk` hardcoded `target/aarch64-unknown-none/release` in every Rust component's link
rule, while `RUST_MAKE_TARGET=build` (and `build-verus`) produce `debug/`. Any such build
failed with `unable to find library -l<crate>`. `MakefileTemplate` now derives

```make
RUST_PROFILE_DIR := $(if $(RUST_MAKE_TARGET),$(if $(filter %-release,$(RUST_MAKE_TARGET)),release,debug),release)
```

and `MakefileContainer` uses it. Note this had to go in **both** `system.mk` templates: the
link rules are shared, so adding it only to the MCS one left domain-scheduled models
referencing an undefined variable, which make expands to empty rather than erroring.

### Fixed: `1 << ch` was undefined behavior for `ch >= 31`

Both the default and monitor schedulers built their readiness bitmask with

```c
part_ready       |= (1 << ch);                                 // part_ready is uint64_t
part_ready_check |= (1 << user_schedule.timeslice_ch[i]);
```

`1` is an `int`, so `1 << ch` is undefined behavior once `ch >= 31`. `MICROKIT_MAX_CHANNELS`
is 62, so channel ids reach 61. The bug was latent in the models at the time — temp-control
uses three channels — but the test variant adds channels, and any system past roughly thirty
would have hit it. The default (`SystemDescriptionProvider_MCS`) and monitor
(`UserLandMonitorPlugin`) templates now use `1ULL << ch`, as the test scheduler's copy always
did.

### Fixed: a monitor checked a thread that had not finished its dispatch

Seen once (2026-10-19): in a full `SystemTestingTests` run under QEMU, the `models/SystemTesting`
`sys_head_monitor` image reported one `d1p_dst CEP_Post` violation and one `Acks` assertion
violation in its 30 s boot; four later runs were clean.  The boot log showed the monitor's
warning interleaved character by character with `d1p_dst`'s first log line, "compute entrypoint
invoked": on one core, the monitor ran in the middle of `d1p_dst`'s dispatch, which had received
`val` (`api_val: Some(0)`) and not yet acknowledged it (`api_ack: None`) -- so `acks` and `Acks`
failed.  The monitor variants' slots are timer-driven (50 ms each here) and do not wait for
their threads; QEMU's clock follows the host's, so a host stall cost `d1p_dst` most of its slot;
and the monitor runs at the threads' priority (140), so it took turns with the unfinished
thread.  Making `d1p_dst` spin past its slot once, in a scratch copy, reproduced the same two
violations with the same pre- and post-states.  What stalled the host in the original run was
not established.

The fix is D35 ("The monitor PDs are thin wrappers"): the scheduler publishes whether the
previous user slot's thread completed, and a monitor checks nothing about one that has not.
`SystemTestingTests`' "a monitor reports a thread that overran its slot instead of checking it
mid-dispatch" boots the GUMBO and `sys_head` monitor images with `d1p_dst` spinning past its
slot once, and requires the `OVERRUN` line, no violation, and the system assertions resuming.

## Open items resolved by building

Resolved by building: the watchdog bound (10x the slot's configured budget, floor 1 s), stage 3's
wall-clock cost (stage 4 removed it), and the synthetic-element stripping interaction (clean --
the monitor bundles came out the same, but for map addresses and channel ids that the stripped
entries shift).
