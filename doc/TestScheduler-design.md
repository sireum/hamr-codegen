# Test Scheduler Design: Command-Driven Scheduling for Microkit System Testing

## Problem

On the JVM, HAMR provides a static scheduler with a command API (`art.scheduling.static`)
that lets users step through the schedule slot-by-slot or hyperperiod-by-hyperperiod,
inspect system state (ports, component state), inject mutations, and observe results.
This has proven valuable for system-level testing. Example workflow:

1. Run the schedule for 2 hyperperiods
2. Inject mutations to ports or state vars
3. Run for another hyperperiod
4. Observe and check results

There is no equivalent for Microkit projects. The MCS user-land scheduler free-runs on
timer interrupts with no external control.

## Platform Constraints

Three properties of the generated Microkit system shape the design. Each of these
corrects an assumption made in the first draft of this document.

### C1. A passive PD cannot run while the scheduler is inside `notified()`

Microkit manual, "Protection Domains":

> A passive PD will have its scheduling context revoked after initialisation and then
> bound instead to the PD's notification object. This means the PD will be scheduled on
> receiving a notification, whereby it will run on the notification's scheduling context.

Every component PD is `passive=True`, and the scheduler PD sits above them in priority
(scheduler 200, `<thread>_MON` 150, `<thread>` 140). So `microkit_notify(ch)` does **not**
transfer control: the scheduler keeps running until it returns from `notified()` and
blocks on `Recv`, and only then does the notified PD run.

Consequence: a blocking `dispatch_and_wait()` helper is impossible. **The test scheduler
must be a state machine driven by `notified()` events**, holding "what the active command
still needs" in static state across invocations.

### C2. Threads do not signal completion

The generated bridge (`<thread>.c`) calls `microkit_notify(PORT_FROM_MON)` only from
`init()`, as part of the readiness handshake. On dispatch it runs `_timeTriggered()` and
returns. The default scheduler documents the matching assumption:

> Runtime signals from partitions are ignored: the schedule is static and each partition
> runs for its full allotted time regardless of early completion.

So there is currently no "slot finished" event. Adding one is cheap and safe:

- One `microkit_notify(PORT_FROM_MON)` at the end of the `PORT_FROM_MON` case in the
  generated bridge.
- The `_MON` wrapper already forwards it — `case USER_PD: microkit_notify(SCHEDULER_CH)`.
- The default scheduler already ignores it: the signal lands in the
  `(part_ready_check & (1 << ch)) != 0` branch with the ready bit already set, whose
  `else` arm is a no-op.

Because it is inert in the default variant, the notify-back can be emitted unconditionally
rather than gated per variant — which matters, since component C sources are shared across
variants (only the MSD, `scheduler.c` and the config headers are variant-specific).

### C3. The variant-bundle mechanism already exists

`UserLandMonitorPlugin.handleForMonitor` already emits, per variant:

```
<name>.meta.py                            MSD variant (SystemDescriptionProviderPlugin.putMSD)
scheduler/src/<name>.scheduler.c
scheduler/include/<name>.scheduler_config.h
scheduler/include/<name>.user_config.h
<name>.mk                                 selected by: make CONFIG=<name>.mk
```

`Makefile` has `ifdef CONFIG / include $(CONFIG)`, and `MSD`, `SCHEDULER_C` and
`SCHEDULER_CONFIG_HEADERS` are all `?=` overridable in `system.mk`.
`SystemDescription.templateContributions: ISZ[ST]` injects per-variant Python into the
generated `meta.py`.

So the test scheduler is a **new bundle in the existing shape**, not a new mechanism. The
first draft's proposed `meta.test.py` is superseded.

## Architecture

```
┌──────────────────┐   test_cmd (shared mem)  ┌──────────────────┐  notify   ┌──────────┐
│ Test Controller  │─────────────────────────▶│  Test Scheduler  │──────────▶│ <t>_MON  │
│ PD (Rust)        │◀─────────────────────────│  PD              │◀──────────│ ──▶ <t>  │
└──────────────────┘   test_status + notify   └──────────────────┘ complete  └──────────┘
        │                                                                     ┌──────────┐
        │  reads/writes port + sv_ memory regions                             │ <t>_MON  │
        └────────────────────────────────────────────────────────────────────▶│ ──▶ <t>  │
                                                                              └──────────┘
```

The test controller is a **passive PD, child of the scheduler, with no timeslice of its
own**. It is driven entirely by the scheduler's completion notification, and runs while
the schedule is paused (the scheduler is blocked on `Recv` and no thread is runnable).
This supersedes the first draft's "adjust the schedule to include test controller
timeslices".

### Controller blocking and the boot handshake

Two properties that are easy to miss and that the rest of the design depends on.

#### The controller must be the lowest-priority PD, and blocks by busy-waiting

A command API call (`hstep(n)`, `run_to_thread(..)`) has to wait for the scheduler to finish
before returning. None of the obvious mechanisms work:

- **Busy-waiting at a priority above the threads deadlocks.** Component PDs sit at 140/150.
  A controller above them that spins on `ack_seq` starves exactly the threads whose execution
  would change `ack_seq`.
- **`microkit_ppcall` to the scheduler is legal but useless.** Manual 304: "a protected call
  is only possible if the callee has strictly higher priority than the caller", and the
  scheduler is 200. But the scheduler cannot complete a multi-slot command inside
  `protected()` — dispatching requires returning to the event loop (C1) — so it would have to
  reply before the work was done.
- **Restructuring test scripts as state machines** works and throws away the straight-line
  script model that D11, D13 and D14 are built on.

The mechanism that does work: **the controller is the lowest-priority PD in the system**
(below the component PDs' 140) **and busy-waits on `ack_seq`**. Every other PD then preempts
it (manual 338: "if the notified PD has a higher priority than the current PD, then the
current PD will be preempted"), the schedule advances, and the spin is simply "idle while
everyone else works". Wasting cycles is irrelevant in a test image.

The controller busy-waits inside `notified()` rather than returning to `Recv`. That is safe:
it is polling shared memory, not waiting on its notification, and a signal arriving mean-
while stays pending on the notification object.

**The spin must read `ack_seq` through a `volatile` (or atomic acquire) load.** The command
protocol below specifies release ordering for the *writer*; the reader needs the matching
constraint. A plain load in a spin loop is hoistable — the compiler may read it once and
loop on a register — producing an infinite loop with no diagnostic.

**The controller needs a run-once guard.** Because it never returns to `Recv` while a
command is outstanding, every completion notification the scheduler sends accumulates as a
pending bit on the controller's notification object. When the suite finishes and `notified()`
finally returns, that pending signal immediately re-invokes `notified()` and the suite runs
again — forever. A `static bool suite_done`, checked on entry, is sufficient; without it the
symptom is a run that never terminates rather than a visible fault.

**The spin costs nothing in scheduling-context terms.** Manual 988-989: `budget` defaults to
1,000us and `period` "defaults to the budget", and 184: a budget equal to its period is a
"full" budget, never throttled — it is replenished every period and merely rotates
round-robin among PDs at the same priority. `meta.py` passes neither attribute, and sdfgen
omits them from the XML when they are `None`, so Microkit's defaults apply; a passive PD's SC
is its own, rebound to the notification object (189), and keeps that full budget. There is
therefore no replenishment latency and no mid-step budget exhaustion.

This holds *because the controller is alone at the lowest priority* — which D4 already
requires so the spin does not starve the threads. A second PD placed at that same priority
would round-robin against the spinning controller.

#### Boot handshake

The default scheduler starts the schedule on a one-second timeout once
`part_ready == part_ready_check`. The test scheduler must not: it idles waiting for a
command. But the controller is passive and runs only when notified, so without an explicit
kick the system boots and sits forever.

**The test scheduler notifies the controller at the all-ready point**, in place of the
default's start-timer, and sets `scheduler_running` there. Note that the controller is
deliberately *not* in `part_ready_check` — that mask is built from
`user_schedule.timeslice_ch[]` and the controller holds no slot (D4) — so the scheduler never
waits on it.

*Confirmed on target.* The controller's protection domains initialize **after** the scheduler
sends the kick, and it is still delivered — the notification really is a pending binary
semaphore. The boot log shows the ordering plainly:

```
TEST SCHEDULER | All partitions ready, handing over to the test controller
MON|INFO: PD 'tcp_tct' is now passive!
test_controller_process_test_controller_thread_MON | INIT!
test_controller_process_test_controller_thread | INIT!
INFO [test_controller::...] compute entrypoint invoked
```

*One consequence found by running it:* the controller signals this same channel from its own
`init()` (via its `_MON`), which is not a command. The `TEST_CONTROLLER_CH` arm therefore
ignores signals until `scheduler_running`, and `init()` parks at `TEST_CMD_NONE` rather than
`RUN_FOREVER` so nothing dispatches until the controller actually asks.

### Scheduler state machine

The scheduler is always positioned *at a slot that has not yet run*. This matches the JVM
Explorer's stop-before semantics — `InfoInputs` is documented as "values of input ports of
component to be executed in the next slot" — so after a command completes, the controller
can inspect the inputs of the component that is about to run.

As built (abridged; the generated source is `scheduler/src/test_scheduler.scheduler.c`):

```c
// Move to the next slot and charge the active command for the one just finished.
static void advance_position(void) {
    if (slots_remaining > 0) slots_remaining--;
    if (runto_budget > 0)    runto_budget--;
    current_timeslice++;
    if (current_timeslice >= user_schedule.num_timeslices) {
        current_timeslice = 0;
        hyperperiod_num++;
    }
    if (is_runto(active_cmd) && runto_budget == 0 && !at_stop_point()) {
        status_flags |= TEST_FLAG_UNREACHABLE;   // predicate can never be satisfied
        active_cmd = TEST_CMD_NONE;
    }
}

static void try_advance(void) {
    poll_command();

    // Padding is skipped in this loop rather than dispatched -- iteratively, not by
    // recursing through on_slot_complete: a schedule can be mostly padding and this
    // runs on a 4 KB protection domain stack.
    while (true) {
        if (active_cmd == TEST_CMD_NONE || at_stop_point()) {
            active_cmd = TEST_CMD_NONE;
            publish_status();                    // writes test_status, ack_seq last
            microkit_notify(TEST_CONTROLLER_CH);
            return;                              // parked: nothing dispatched, no watchdog
        }

        microkit_channel ch = user_schedule.timeslice_ch[current_timeslice];
        if (ch == 0) {            // channel 0 pads out a schedule
            advance_position();   // counted, so slot indices stay aligned
            continue;
        }

        slot_generation++;
        last_dispatched_ch = ch;
        armed_generation   = slot_generation;

        uint64_t bound = user_schedule.timeslices[current_timeslice] * TEST_WATCHDOG_FACTOR;
        if (bound < TEST_WATCHDOG_MIN_NS) bound = TEST_WATCHDOG_MIN_NS;
        sddf_timer_set_timeout(config.driver_id, bound);   // watchdog only

        microkit_notify(ch);      // returns immediately (see C1)
        return;
    }
}

static void on_slot_complete(void) {
    armed_generation = 0;
    advance_position();
    try_advance();
}

void notified(microkit_channel ch) {
    if (ch == config.driver_id) {
        if (armed_generation != 0 && armed_generation == slot_generation) {
            status_flags |= TEST_FLAG_OVERRUN;   // the slot in flight never reported back
            active_cmd = TEST_CMD_NONE;
            armed_generation = 0;
            publish_status();
            microkit_notify(TEST_CONTROLLER_CH);
        }
        // else a stale expiry: the timeout cannot be cancelled, so a slot that completed
        // normally leaves its bound armed. The generation check tells the two apart.
    } else if (ch == TEST_CONTROLLER_CH) {
        // Ignored before the schedule is live: the controller signals this same channel
        // from its own init(), by way of its _MON, and that is not a command.
        if (scheduler_running) try_advance();
    } else if ((part_ready_check & (1ULL << ch)) != 0) {
        if ((part_ready & (1ULL << ch)) == 0) {
            /* readiness handshake; on the last one, scheduler_running = true and
               microkit_notify(TEST_CONTROLLER_CH) hands over to the controller */
        } else if (scheduler_running && armed_generation != 0 && ch == last_dispatched_ch) {
            on_slot_complete();   // this is what paces the schedule; the clock does not
        }
    }
}
```

Padding slots (`timeslice_ch == 0`) are never dispatched, but are still *counted*, so slot
indices stay aligned with the published `sched_schedule`. **Whether a pad slot consumes its
wall-clock time differs by stage**, and the two must not be conflated:

- **Stage A honours the pad**, arming its timeout like any other slot. Stage A's premise is
  that it changes nothing about timing, and temp-control's pad is 850ms of a 1000ms frame —
  skipping it would make a "hyperperiod" take 150ms and diverge from the default variant for
  anything timing-sensitive.
- **Stage B skips the pad**, completing it immediately. There is no timer driving slots in
  stage B, so padding has no meaning; honouring it would mean reintroducing a timeout purely
  to wait.

Stage B is what is built, so pads are skipped, and a test that steps hyperperiods runs far
faster than the frame period suggests. That is expected and is the point of stage B.

**Every `RunTo*` command must be bounded.** `RunToThread(ch)` for a channel absent from the
schedule, `RunToHP(n)` with `n <= hyperperiod_num`, and `RunToState` targeting a state
already passed all have stop predicates that are never satisfied, so the scheduler would
dispatch forever. Commands are validated at decode where possible (target channel present in
the schedule; target hyperperiod in the future), and every `RunTo*` also carries a slot
budget, set at decode and charged in `advance_position`: one hyperperiod plus one slot for
`RunToThread` / `RunToSlot`, and enough hyperperiods to reach the target (plus one slot) for
`RunToHP` / `RunToState`. When the budget runs out without a match, the command completes with
`TEST_FLAG_UNREACHABLE` in `test_status`.

**Stale watchdog expiries must be filtered.** `sddf_timer_set_timeout` is one-shot and cannot
be cancelled, so a slot that completes normally leaves its watchdog armed; it fires during a
later slot and is indistinguishable from a real trip. The watchdog therefore carries a
deadline or slot generation that is checked on every timer notification, and expiries that do
not match the slot currently in flight are discarded.

### Dispatch models

**Stage A — timer-gated (superseded; stage 2 only).** Kept `sddf_timer_set_timeout` exactly
as the default scheduler uses it, pad slots included, with the timer tick as the slot-complete
event. Required no change to any component, which is what made stage 2 verifiable on its own.
Cost: stepping ran in wall-clock time — temp-control's frame is
`[pad 850ms, tsp_tst 50ms, tcp_tct 50ms, fp_ft 50ms]`, so `Hstep(2)` took two real seconds.

**Stage B — completion-driven (current).** Uses the C2 notify-back as the slot-complete event
and drops the per-slot timeout. Stepping then runs as fast as the threads do, and slot boundaries are
deterministic rather than timing-dependent. The timer is **retained as a watchdog**: a slot
that does not report completion within a generous bound sets an overrun flag in
`test_status` and aborts the command, so a hung thread fails the run instead of wedging it.

### Command protocol

Two 4 KB memory regions, continuing the vaddr block already used by the monitor variants
(`sched_state` at `0x4_000_000`, `sched_schedule` at `0x4_001_000`):

| Region        | vaddr         | scheduler | controller |
|---------------|---------------|-----------|------------|
| `test_cmd`    | `0x4_002_000` | `r`       | `rw`       |
| `test_status` | `0x4_003_000` | `rw`      | `r`        |

```c
typedef struct test_command {
    uint32_t seq;          // incremented by the controller on every command
    uint32_t type;         // cmd_type_t
    uint32_t count;        // Sstep / Hstep
    uint32_t target_ch;    // RunToThread
    uint32_t target_hp;    // RunToHP / RunToState
    uint32_t target_slot;  // RunToSlot / RunToState
} test_command_t;

typedef struct test_status {
    uint32_t ack_seq;             // echoes test_command.seq once the command completes
    uint32_t current_timeslice;
    uint32_t hyperperiod_num;
    uint32_t last_dispatched_ch;
    uint32_t flags;               // overrun / unknown-command / stopped
} test_status_t;
```

The handshake is **sequence-number based, not flag based**: the controller writes the
command body, then `seq = n+1`, then notifies; the scheduler processes it and publishes
`ack_seq = n+1` last. Both sides are single-writer per field, so ordered 32-bit writes with
a release barrier before the `seq`/`ack_seq` store are sufficient — no atomics needed.

`test_cmd`/`test_status` are plain structs in a dedicated region rather than HAMR queues,
unlike `sched_state`/`sched_schedule`, because there is no producer/consumer queueing
semantics here — just a request/response cell.

### Command vocabulary

Mirrors the JVM `art.scheduling.static.Command`:

| Command             | Description                                        | JVM equivalent          |
|---------------------|----------------------------------------------------|-------------------------|
| `Sstep(n)`          | Step `n` schedule slots                            | `Sstep(n)`              |
| `Hstep(n)`          | Step `n` hyperperiods                              | `Hstep(n)`              |
| `RunToSlot(n)`      | Run until slot index `n` is next                   | `RunToSlot(n)`          |
| `RunToHP(n)`        | Run until hyperperiod `n`                          | `RunToHP(n)`            |
| `RunToState(hp, s)` | Run until `(hyperperiod, slot)`                    | `RunToState(hp, slot)`  |
| `RunToThread(ch)`   | Run until the given thread's slot is next          | `RunToThread(name)`     |
| `InfoState`         | Publish current `(hyperperiod, slot)`              | `Infostate`             |
| `InfoSchedule`      | Publish the full schedule                          | `Infoschedule`          |
| `Stop`              | End the test session (see below)                   | `Stop`                  |

`RunToThread` takes a channel id rather than a thread name; the controller maps names to
channels using the generated per-thread channel constants (the same constants
`UserLandMonitorPlugin` already emits as `moduleLevelEntries` for the monitor crate).

**`InfoSchedule` does need a new contribution.** `sched_state` and `sched_schedule` were moved
out of the default MCS template into the monitor plugin's `templateContributions`; the
default `scheduler_c` contains no publishing code for them. The test variant must therefore
contribute the `sched_schedule` region and the init-time publish itself. This is not optional
polish — the controller needs the schedule to correlate slot indices with channels, which
both `RunToSlot` and the runner's between-test position normalization (D13) depend on. It is
a stage 2 deliverable.

`InfoState` and `InfoSchedule` are otherwise **no-ops**: `try_advance` calls
`publish_status()` on every command completion, so the controller already holds current
status after any command, and `sched_schedule` is published once at init. Both are retained
for parity with the JVM vocabulary and for the stage 6 CLI, where a human does want to ask.

The JVM's `InfoInputs` / `InfoOutputs` / `InfoComponentState` are **not** scheduler
commands here — they are reads the controller performs directly against the port and
state var memory regions (see below), which is strictly more capable.

**`Stop` semantics.** The runner issues `Stop` at the end of every run, so this is the normal
path, not a corner case. On `Stop` the scheduler sets a `stopped` bit in `test_status.flags`,
acknowledges as usual, and then idles permanently: it dispatches nothing further and ignores
subsequent commands other than by re-acknowledging with `stopped` still set. It is
deliberately **not** resumable — a resumable `Stop` would mean the schedule can restart after
the controller has published its verdict, which makes the verdict meaningless. Stage 6's
interactive CLI, where a human may well want to continue, should use a distinct `Pause` if
that turns out to be wanted; it is not needed for stages 3-5.

### Test controller PD (Rust)

The controller is injected the same way monitor PDs are, so it inherits the whole
`CRustComponentPlugin` pipeline: crate generation, generated port APIs, `extern_c_api.rs`,
the test harness, and — eventually — reuse of generated GUMBO contract code for assertions.

Generated (non-editable) half: a typed command API over `test_cmd`/`test_status`.

```rust
sstep(n); hstep(n);
run_to_thread(TCP_TCT_MON); run_to_slot(n); run_to_hp(n); run_to_state(hp, slot);
info_state() -> TestStatus;
stop();
```

Each call writes the command, notifies the scheduler, and returns when the matching
`ack_seq` is observed. User-editable half: the test script itself — the Microkit analogue
of JVM test code that calls `Explorer.stepSystemNHPIMP(2)` and then inspects bridges.

### State inspection and mutation

Inspection and injection are **not** symmetric. Inspection reuses existing infrastructure
almost entirely; injection needs new machinery for state vars, and none exists today.

#### Inspection

Map each thread's port regions and `sv_` state var regions into the controller read-only.
Because the schedule is paused at a slot boundary when the controller runs (D4), the values
are quiescent — no tearing, no barriers beyond the command handshake. This overlaps with the
item still open in `GumboMonitorPlugin-design.md` (monitor-side consumption of state var
data); the two should share whatever decoding is generated.

**Inspection is non-destructive**, which is not obvious and worth stating. The queue is
broadcast ("Every receiver receives the sent data"), and each receiver owns a private
`sb_queue_*_Recv_t` holding its own `numRecv`; the shared region carries only `numSent` and
`elt[]`. The controller becomes an additional receiver with its own cursor, so its reads
cannot consume data the real consumer has not seen yet.

#### Port injection: act as the producer

On target a thread's input port is an `extern "C"` dequeue from the **producer's** outgoing
region:

```rust
extern "C" { fn get_currentTemp(value: *mut Temperature) -> bool; }
```

(The `put_currentTemp` in a component's `test_apis.rs` is a *host-only mock* writing
`extern_api::IN_currentTemp.lock()`. It is behind `#[cfg(test)]` and does not exist on
target, so it cannot be reused here.)

Injecting therefore means **enqueuing into the producer's region exactly as the producer
would**. The consumer cannot distinguish the two, and the queue's reader-side state stays
consistent. Required:

- map each producer region `rw` into the controller (MSD change, test variant only);
- emit the producer-side `put_<port>` into the controller's bridge, bound to the same region
  at the controller's own vaddr.

That second item is code `CConnectionProviderPlugin` already generates; the controller
simply becomes an additional writer of each region. **No thread-side change is needed.**

*This depends on D4.* `sb_queue_*.h` opens with "Single sender multiple receiver Queue
implementation". Injection makes the controller a **second sender**: two writers incrementing
`numSent` and writing `elt[numSent % SIZE]`. It is safe only because D4 guarantees the real
producer is not running while the controller is. **If D4 is ever relaxed, injection corrupts
queues** — the dependency is not optional.

#### Unconnected input ports: the clean case

An input port with no producer still gets a region. `CConnectionProviderPlugin`'s
unconnected-port loop covers both directions, and `ConnectionUtil.processInPort` called with
`srcPort = None()` names the region after the input port's own path and maps it `READ` into
the consuming thread:

```scala
val memoryRegionName: ISZ[String] = if (srcPort.nonEmpty) srcPort.get.path else dstPort.path
```

So **D15 covers every port** — there is no category of input that cannot be injected into.

Unconnected inputs are in fact the cleanest target in the system. Today such a region has a
reader and no writer, so the thread always dequeues empty. Once the controller maps it `rw`
the controller is the *sole* sender, which means neither of the caveats above applies: there
is no single-sender contract violation and no producer to race, so the `run_to_thread` /
inject / `sstep(1)` ordering is not forced for these ports. Both constraints are specific to
ports that have a real producer.

D16's injection regions are the same shape one step removed: `sv_X` is an unconnected
*output* (thread writes, observer reads) and `inj_<thread>_sv_<var>` is its mirror — thread
reads, controller writes. It is not a port, though (D16a), so it does not ride this path; the
plugin declares the region itself and maps it in both directions.

*Ordering is required, not merely advised.* The queue is declared `SIZE 2`, and the header
notes that one cell is always dirty, so it holds exactly **one** element. Where a port has a
real producer in the schedule, that producer enqueues during its own slot and displaces an
injected value. The sequence `run_to_thread(consumer)`, inject, `sstep(1)` is therefore
mandatory, not a stylistic preference — and it is what the scheduler's stop-before semantics
exist to enable.

#### State var injection: mirrored regions, NOT model ports

**Nothing exists for this today.** `lib.rs` emits only `put_sv_*`, after `_initialize` and
`_timeTriggered`, and the synthetic `sv_X` ports are *outputs* — the flow is thread→observer
only. There is no `get_sv_*` anywhere. (An earlier draft of this document claimed the
thread-side get-before/put-after pattern was already in place. It is not; only the put half
was implemented.)

The controller→thread direction mirrors the `sv_X` outputs:

```
thread ──put_sv_X──▶  sv_X region      ──▶ controller   (inspect, built)
thread ◀──get_sv_X──  inj_sv_X region  ◀── controller   (inject, D16)
```

Making the `sv_X` regions *bidirectional* instead was rejected, and that still holds:

1. **The queue's emptiness is the dirty flag.** Nothing injected means nothing dequeued, so
   the thread keeps its own state. A shared bidirectional region would need a hand-rolled
   generation counter.
2. It preserves one-writer-per-region.
3. The thread-side ingest is an ordinary dequeue, so `lib.rs` grows a `get_sv` block before
   `_app.timeTriggered(..)` mirroring the `put_sv` block after it.

##### The regions must not be AADL ports

The first implementation added synthetic `inj_sv_X` **input ports** to each thread and let
`CConnectionProviderPlugin` create the regions, exactly as an earlier revision of this
section prescribed. Mechanically it worked on the first try — regions created, `get_inj_sv_*`
generated into the bridge, `extern_c_api.rs` declaring them, no new machinery.

**It was reverted.** Putting them in the model means every downstream consumer of the model
sees them:

```rust
// crates/<component>/src/test/util/test_apis.rs   -- the COMPONENT TEST HARNESS
pub struct PreStateContainer_wGSV {
  pub api_inj_sv_currentSetPoint: TempControl_SysVerif::SetPoint,   // test plumbing
  ...

// crates/sys_<id>_proof/src/system_state.rs        -- the VERIFICATION MODEL
pub inj_sv_currentFanState: TempControl_SysVerif::FanCmd, // channel
```

Component proptests would have to supply values for test-infrastructure ports, those ports
would enter CEP-Pre/CEP-Post, and the system proof would reason about ports that exist only
for testing. The default variant's `meta.py` also gained regions it never uses.

"No new machinery" was true, but it bought that by putting the ports in the model — and the
model is also the input to the component test harness and to system verification. That
tradeoff was not weighed when D16 was written.

So the `inj_sv_X` regions are created the way `test_cmd` / `test_status` / `test_schedule`
already are: **template-managed regions declared by the plugin**, mapped into the owning
thread and into the controller, with generated C accessors on both sides. The cost is the
region creation and the thread-side getter, which the plugin already does for the controller
and can do again for the thread. The gain is that nothing outside this plugin can see them.

##### Thread side

`lib.rs` gains an ingest block in the `libComputePre` slot (added to
`CRustComponentPlugin.ComponentContributions` for this; the position already existed but was
hardcoded for R2U2):

As built, for `tcp_tct`:

```rust
pub extern "C" fn tcp_tct_timeTriggered() {
  unsafe {
    if let Some(_app) = app.as_mut() {
      // Injected GUMBO state variables, if the test controller set any.
      if crate::bridge::extern_c_api::unsafe_is_injection_enabled() {         // libComputePre
        if let Some(v) = crate::bridge::extern_c_api::unsafe_get_inj_sv_latestTemp() {
          _app.latestTemp = v;                                                // absent => keep own state
        }
        // ... one per state var
      }
      _app.timeTriggered(&mut compute_api);
      if monitoring_enabled { /* existing put_sv_* block */ }
    }
  }
}
```

The C functions are declared the way `is_monitoring_enabled` is: as `CRustApiPlugin`
contributions to the thread's `bridge/extern_c_api.rs`, not by an `extern "C"` block in
`lib.rs`. That file puts the real declarations behind `#[cfg(not(test))]` and supplies
`#[cfg(test)]` stand-ins (`INJECTION_ENABLED`, `INJ_SV_<var>` mocks, both defaulting to
"nothing injected"), plus the `unsafe_` wrappers `lib.rs` calls; `unsafe_get_inj_sv_<var>`
returns an `Option`. The C bridge exists only in the seL4 image, so a raw extern in `lib.rs`
builds on target and fails to link under the host `make test` -- see "What building and
running it corrected".

*Gating:* the `inj_sv_` regions exist only in the test variant, so the ingest is gated by a
sibling of `is_monitoring_enabled()` — `is_injection_enabled()`, a NULL check on the
`inj_sv_` queue pointers — so the default variant pays nothing and never dequeues from an
unmapped region. The pointers are the `setvar_vaddr` targets microkit patches at build time,
which is exactly why the check works: in the default variant nothing maps them and they stay
NULL. The generated C getters carry the same guard individually, so a partially-mapped variant
degrades to "nothing injected" rather than to a fault.

The test variant sets **both**: injection is gated at each call, and `monitoring_enabled` is
latched once during `_initialize` for the `put_sv_*` block that inspection reads — a property
of the variant's build, not something a test can change.

##### Controller side

The generated API presents one accessor pair per state var, hiding the two regions:

| call | region |
|------|--------|
| `get_<thread>_sv_<var>` | reads the `sv_` region the thread publishes to |
| `put_<thread>_sv_<var>` | writes the `inj_sv_` region the thread ingests from |

A state var therefore has a `get_` but **no** `put_` on its `sv_` region: that region is the
thread's output, so a writer there would enqueue successfully and be read by nobody — a no-op
that looks like it works. The `put_` a test calls is the one above, on the `inj_sv_` region.
Suppressing the `sv_` writer is also what keeps the two from colliding: both would be named
`test_put_<thread>_sv_<var>`.

This sequencing is what D13 and D14 rest on: `run_to_thread(T)`, then enqueue the injected
ports and state vars, then `sstep(1)`, and the thread ingests before it computes. The order
matters for ports with a real producer -- enqueueing before `run_to_thread` lets that
producer's own slot overwrite the value on the way -- and is harmless for `inj_sv_` regions and
unconnected ports, where the controller is the only sender. The
whole-component setter of D14 is then N port enqueues plus M state var enqueues behind one
call.

### Reporting the verdict (D18)

#### Only serial gets a verdict off the target

A summary written into `test_status` lives in guest memory; nothing on the host can read a
Microkit memory region without a debugger or a core dump. It is an *on-target* path — useful
for stage 6's CLI or for another PD — and gives CI nothing. Serial is the only channel that
carries a verdict off the target, and `sddf_dprintf` already works (the scheduler uses it for
`SCHEDULER | ...`).

What exists today is only `make qemu`, which is `$(QEMU) -nographic $(QEMU_ARCH_ARGS)`: it
blocks forever, never exits, and nothing scrapes it. The host driver is new work.

#### Absence of evidence must be failure

Every interesting failure mode of a bare-metal QEMU run is *silence*: the image hangs, a PD
panics, the controller never receives its all-ready kick, a `TESTS=` filter matches nothing.
A host rule of "fail if I see FAIL" passes all of them. The rule is therefore inverted: **a
run without a positive completion marker is a failure.**

#### Line format

```
TEST | BEGIN nominal::fan_turns_on_when_too_hot
TEST | PASS  nominal::fan_turns_on_when_too_hot
TEST | FAIL  nominal::setpoint_is_latched tests.rs:42
TEST | DONE  matched=2 passed=1 failed=1
```

- **`DONE` is mandatory.** Its absence fails the run whatever preceded it. This is what catches
  the hang, the panic, and the controller that never started.
- **`matched=`** catches D17's zero-match trap: the host asserts `matched > 0`, so a mistyped
  filter fails loudly instead of passing with nothing run.
- **`passed`/`failed`** must reconcile with the `PASS`/`FAIL` lines actually seen; a mismatch
  means output was lost, which is also a failure.
- **The `TEST | ` prefix** makes scraping a line-anchored regex rather than prose matching, and
  keeps it distinct from the scheduler's existing `SCHEDULER | ...` output.

This composes with the panic handler already in `lib.rs`, which logs and then spins forever:
a panic emits no `DONE`, so the timeout fails the run with the panic text in the captured log.
D12's recording assertions keep *test* failures from panicking, but the `app is None` paths can
still panic.

#### The verdict exists only in a debug build

`sddf_dprintf` is compiled out unless `CONFIG_DEBUG_BUILD` is set:

```c
#ifdef CONFIG_DEBUG_BUILD
#define sddf_dprintf(fmt, ...) sddf_printf(fmt, ##__VA_ARGS__);
#else
#define sddf_dprintf(...)
#endif
```

`MICROKIT_CONFIG ?= debug` is the default, so this works — but `make MICROKIT_CONFIG=release`
removes every `TEST |` line. There is then no `DONE`, and the rule above fails the run. That
is the safe direction, but the diagnosis is baffling: a release build reports failing tests
that never ran. **The host driver forces `MICROKIT_CONFIG=debug`** and reports plainly if it
has been overridden, rather than letting it surface as a test failure.

The same mechanism is why D18 is cheap at stage 3. temp-control's `meta.py` instantiates **no
serial driver PD** — `serial=` is only a field of the `Board` dataclass — yet the scheduler's
`SCHEDULER | ...` output appears. Debug output goes straight out through the seL4 debug
syscall, so the verdict path needs no new protection domain.

**This is also what put stage 6 out of reach.** A serial CLI needs a real sDDF serial driver
PD owning the same PL011 the debug syscall writes to directly — two writers, one UART, and
D18's scraping is precisely what interleaved output would break. Deferred; see "Interactive
CLI (deferred)".

#### Stopping QEMU

Host-side: the driver reads stdio until `DONE` and then kills QEMU; a hard timeout is the other
exit. Guest-side termination (ARM semihosting `SYS_EXIT`) would yield real exit codes but needs
plumbing the image does not have — a later refinement, not a stage 3 prerequisite.

The timeout cannot be a fixed constant. Stage 3 runs about a second per hyperperiod while
stage 4 is far faster, so any single value is either too tight for the former or meaningless
for the latter. It should be derived from the frame period and the dispatch model, or simply
set generously — `DONE` is the real terminator, and the timeout only exists to bound the
silent-failure case.

#### Where the driver lives

`sysml/bin/run-tests.cmd`, a Slash script mirroring `run-hamr.cmd`: build under
`CONFIG=test_scheduler.mk`, run `make qemu`, scrape, exit nonzero. `.ci/ci.cmd` already gates
Microkit steps on `MICROKIT_SDK`, so it slots in beside them.

`test_status` still carries the same counters — not for CI, but so stage 6's CLI can ask how a
run went without re-reading the log.

### Relationship to the runtime monitor

**The runtime monitor does not run during system testing, and does not need to.**

`TestSchedulerPlugin.finalizeMicrokit` builds the variant from the pre-monitor MSD snapshot
(`UserLandMonitorPlugin.MONITOR_ORIG_MSD_KEY`, or `normal` when no monitor ran), so the
monitors' interleaved slots were never in it. It then drops every other injected protection
domain (`userland_monitor`, `gumbo_monitor`, `sys_nominal_monitor` and their `_MON`s) with
their channels, keeping only the controller — one injected PD per variant, as the monitor
variants have. The test frame is therefore not extended by monitor time, and the schedule
under test remains the production schedule. The controller is to do the checking instead,
running the same generated contract checks at every dispatch (D3; planned as stage 7, see
"Contract observation in the controller").

**`monitoring_enabled` is nonetheless required, and is not about the monitor's existence.**
`GumboMonitorPlugin` generates:

```c
bool is_monitoring_enabled(void) {
  return sv_currentSetPoint_queue_1 != NULL && sv_currentFanState_queue_1 != NULL && ...;
}
```

— a NULL check on the `sv_` queue pointers, i.e. "were the state var regions mapped into
me?". An unmapped region leaves its `setvar_vaddr` pointer NULL. Threads therefore publish
state vars whenever the variant maps the `sv_` regions, with or without a monitor PD. The
name is a misnomer in this context; the mechanism is what the test variant needs.

#### Models without GUMBO contracts

`GumboMonitorPlugin.canHandleModelTransform` requires `hasThreadsWithStateVars(symbolTable)`.
A model with no GUMBO state vars therefore gets no `sv_` ports, no `sv_` regions, and no
`is_monitoring_enabled()` at all — its threads receive the plain `CRustComponentPlugin`
`lib.rs`. (vest is the existing example: `monitoring_enabled` appears nowhere in its crates.)

The test scheduler must still work there, and mostly does:

| Capability | Without GUMBO contracts |
|------------|-------------------------|
| Scheduler state machine, commands, controller, runner, `sys_assert_*`, suites, `TESTS=` | unaffected |
| Port inspection and injection (D15) | unaffected — port regions exist for real AADL ports regardless of contracts |
| State var inspection and injection (D16) | vacuous — there are no state vars. Degrades to nothing rather than breaking |
| Reusing CEP predicates to assert postconditions (the D3 payoff) | unavailable — tests hand-write their expectations |

Three obligations follow:

1. **`--runtime-monitoring` is required only when the model has state vars** -- until stage 7,
   which drops the requirement altogether (D22). D5's rationale
   is that the flag creates the `sv_` plumbing; for a contract-free model it creates none, and
   demanding it would buy nothing but a userland monitor PD that the test variant then strips.
   The check and its diagnostic are conditional on `hasThreadsWithStateVars`.
2. **The D16 ingest is emitted per thread, not per system.** `is_injection_enabled()` and the
   `get_inj_sv_*` accessors are generated only for threads that have state vars. In a mixed
   model, emitting the ingest block for a thread without them would reference symbols that do
   not exist for it.
3. **`TestSchedulerPlugin` does not inherit from either monitor plugin.** Inheriting from
   `GumboMonitorPlugin` would drag in its `hasThreadsWithStateVars` gate and silently disable
   the test scheduler for precisely the contract-free models this section is about. See
   "Plugin structure".

#### The `sv_` regions must survive into the variant

The monitor variants go through `UserLandMonitorPlugin.handleForMonitor`, which strips every
non-model memory region not named by `getRetainedNonModelPorts` (default `ISZ()`;
`GumboMonitorPlugin` overrides it to keep the `sv_` ports). Were the test variant built that
way without the same override, the `sv_` regions would be stripped, the pointers left NULL,
`is_monitoring_enabled()` false, and the threads silently not publishing — inspection would
return stale or zero state with **no crash and no diagnostic**.

The test variant avoids this by construction rather than by override: it is built from the
pre-monitor snapshot and removes only injected protection domains and their channels, never
memory regions, so every `sv_` region is still present. Anyone moving the variant onto
`handleForMonitor` must add the retention. The `state_vars` tests guard it either way: they
read back a state var they just injected, which fails loudly if either region is unmapped.

The `inj_sv_` regions of D16 are not ports (D16a) and never subject to synthetic-element
stripping; they are declared in the MSD template alongside `test_cmd` and reach the thread
through `setvar_vaddr`.

### How the developer writes a system test

#### What the platform provides: nothing reusable

- **Microkit** has no test infrastructure. Its `tests/` directory holds three hand-written
  `.system` smoke tests (`capfault`, `simplemrs`, `overlapping_pages`), each a C file plus a
  README describing what to eyeball. No assertion library, no runner, no harness.
- **seL4**'s `sel4test` targets the kernel and its libraries as its own rootserver image. It
  is not reachable from a Microkit PD — Microkit is a separate, minimal ABI.
- **sDDF / LionsOS** CI boots QEMU and greps serial output. That is the ecosystem idiom, and
  it is what stage 6 is aiming at.

#### Mirror the existing component-test shape

HAMR already has a well-developed *component*-level story, host-run via `make test` →
per-crate `cargo test`:

| Artifact | Generated? |
|----------|-----------|
| `src/test/util/test_apis.rs` — `PreStateContainer{,_wGSV}`, `put_concrete_inputs*` | yes, overwritten |
| `src/test/util/generators.rs` — per-datatype proptest strategies | yes, overwritten |
| `src/test/util/cb_apis.rs` — GUMBO CEP-Pre / CEP-Post predicates | yes, overwritten |
| `src/test/tests.rs` — the tests themselves | **no, preserved across regen** |
| `testInitializeCB_macro!` / `testComputeCB_macro!` / `testComputeCBwGSV_macro!` | yes |

The controller crate presents the **same shape one level up**: generated command and
inspect/mutate APIs under `src/system_tests/` (`api.rs`, `inspect.rs`, overwritten), the
system test script in a preserved `src/system_tests/tests.rs`. A developer who has written a
component test already knows the idiom. As built, for temp-control:

```rust
// crates/test_controller/src/system_tests/tests.rs — preserved across regen
use crate::system_tests::{api, inspect};
use crate::{system_tests, sys_assert_eq};
use data::TempControl_SysVerif::*;

system_tests! {
  suite nominal {
    fn fan_turns_on_when_too_hot() {
      let _ = api::hstep(1);                                          // settle

      // Park immediately before the consumer: currentTemp has a real producer
      // (tsp_tst) that runs earlier in the frame and would overwrite the injection.
      let _ = api::run_to_thread(inspect::channels::tcp_tct_MON);
      inspect::set_tcp_tct(inspect::tcp_tct_PreState {               // every field (D14)
        currentTemp: temp(95),                                        // port, via its producer's region
        setPoint: set_point(70, 80),                                  // unconnected port
        fanAck: FanAck::Ok,
        sv_currentSetPoint: set_point(70, 80),                        // state vars, via inj_sv_ regions
        sv_currentFanState: FanCmd::Off,
        sv_latestTemp: temp(72),
        sv_fanError: false,
      });
      let _ = api::sstep(1);                                          // tcp_tct runs next and ingests

      sys_assert_eq!(inspect::get_tcp_tct_fanCmd(), Some(FanCmd::On));
      sys_assert_eq!(inspect::get_tcp_tct_sv_currentFanState(), Some(FanCmd::On));  // sv_ region
    }
  }
}
```

(`temp` and `set_point` stand for small test-local constructors, as isolette's `tws` is.)
The shape differs from a component test in three deliberate ways, each covered below:
tests live in a `system_tests!` block (D11), assertions are the recording `sys_assert*`
macros rather than `assert!` (D12), and a port with a real producer is only written while
the schedule is parked immediately before its consumer (D15). `inspect::set_<thread>` and
`inspect::put_<producer>_<port>` are the generated setters; `get_*` return `Option`, `None`
when nothing is queued.

Because the controller is Rust (D3), the generated GUMBO CEP-Pre/CEP-Post predicates in the
component crates are ordinary Rust functions available to it. A system test can therefore
assert *"every thread's postcondition held at every dispatch"* by reusing them, rather than
hand-writing expectations per test. This is the main reason D3 went the way it did.

#### The controller needs its own runner

`cargo test` does **not** work on target. Component crates open with
`#![cfg_attr(not(test), no_std)]`, and `proptest`, `serial_test` and `lazy_static` are
`[dev-dependencies]` — so the whole existing test stack is host-only and std-only. On target
the crate is a `no_std` staticlib: no `#[test]` collection and no dev-deps. proptest does
run on target, but as a regular dependency built without std — see "Property-based tests"
below.

**Test declaration (D11).** Tests are declared inside a single `system_tests!` block in the
preserved `tests.rs`. The macro emits the bodies verbatim plus the registration table the
runner walks:

```rust
system_tests! {
    fn fan_turns_on_when_too_hot() { /* ... */ }
    fn setpoint_is_latched()       { /* ... */ }
}
```

expands to the two functions plus

```rust
pub static SYSTEM_TESTS: &[(&str, fn())] = &[
    ("fan_turns_on_when_too_hot", fan_turns_on_when_too_hot),
    ("setpoint_is_latched",       setpoint_is_latched),
];
```

The macro also nests into `suite` blocks, which qualify the registered names and give test
selection its granularity — see D17 below.

**Tests split across files.** A larger suite can mirror the JVM layout of one test class per
file. `tests.rs` then holds a `system_test_files!` block in place of `system_tests!`:

```rust
system_test_files! {
    mod nominal_tests;          // system_tests/tests/nominal_tests.rs
    mod fault_injection_tests;  // system_tests/tests/fault_injection_tests.rs
}
```

Each file carries its own `system_tests!` block, and so its own `SYSTEM_TESTS` table. The
macro declares the modules and concatenates their tables, in declaration order, into the one
`SYSTEM_TESTS` the runner walks. The concatenation happens at compile time: a `const fn`
copies the tables into an array whose length is the sum of their `len()`s. Nothing is listed
twice, so the no-drift property of D11 survives. The runner is unchanged. Files must end in
`_tests.rs`, because the models' `clean.cmd` preserves any path containing `tests.rs` and
would otherwise delete them. The isolette port of the JVM system tests is laid out this way.

Rejected alternatives: `inventory`/`linkme` register through custom link sections, which is
not a good bet under Microkit's own `microkit.ld`; a per-test macro plus a hand-maintained
`register![..]` list drifts as soon as someone adds a test and forgets the list.

**Assertions (D12).** `panic = abort` under `no_std` means `catch_unwind` does not exist, so
a failing `assert!` would kill the PD and end the run. Tests use recording assertions
(`sys_assert!`, `sys_assert_eq!`) that note the failure with `file!()`/`line!()` and return
from the test body, letting the runner proceed to the next test. This is a visible
difference from component tests, where cargo's harness catches the panic.

There is no `format!` without `alloc`, so a failure message cannot be built into a `String`.
Messages go out through `sddf_dprintf` / `log::error!` — the path the existing `#[panic_handler]`
already uses — which means reporting actual-vs-expected values requires those types to be
printable through that path. Where they are not, the assertion records position and the
test name only.

The runner itself is generated into `util/` (overwritten): walk `SYSTEM_TESTS`, normalize
schedule position, run, publish each verdict, then issue `Stop`.

**Property-based tests.** The JVM system tests draw random inputs with SlangCheck; on target
that role goes to proptest, which runs without std given `alloc`. Every controller crate gets
it: the plugin adds `proptest` (`default-features = false`, features `alloc` and `no_std`) and
`linked_list_allocator` to the controller's `[dependencies]`, and the generated `harness.rs`
carries a 256 KiB heap as the global allocator plus
`run_property(cases, make_strategy, property)`. That sets up the heap on first use, builds the
strategy, runs a fixed-seed `TestRunner`, and returns whether every case passed. A failure is
shrunk on target to a minimal input, and its message and input are printed as `TEST | INFO`
lines, so a test writes `sys_assert!(run_property(..))`. Each case drives the real system —
inject, step, observe — exactly as a fixed-value test does.

Why generated rather than left to the developer: the models' `clean.cmd` deletes the
controller's `Cargo.toml` before regeneration, so dependencies added by hand are lost on every
behavior-test run.

Three constraints shape `run_property`:

- It takes a strategy *builder*: building a strategy can allocate (`prop_flat_map` wraps its
  closure in an `Arc`), and the heap does not exist until the first property test starts.
- Failure text goes out line by line: proptest's messages span several lines, and the host
  driver keeps only lines with the `TEST | ` prefix.
- The seed is fixed (`PROPTEST_SEED`), since there is no OS entropy on target. Every run
  explores the same cases; varying the seed per build, patched in like `TESTS=`, is open.

#### Selecting which tests run (D17)

A controller carries several scripts, and a developer usually wants one of them. Selection is
a **filter string** consulted by the runner, populated two ways.

The filter lives in an ELF section of the controller PD, following the precedent `meta.py`
already sets for the schedule itself — the schedule is patched in via `objcopy` rather than
compiled in:

```python
data_path = output_dir + "/schedule_config.data"
with open(data_path, "wb+") as f: f.write(user_schedule.serialise())
update_elf_section(obj_copy, scheduler.program_image, user_schedule.section_name, data_path)
```

The controller has a C bridge like any other component, so it can carry the same kind of
section and expose it to Rust through the existing extern pattern:

```c
typedef struct test_selection {
    char     filter[256];   // empty => run everything
    uint32_t flags;         // list-only, stop-on-first-failure, ...
} test_selection_t;
```

The runner runs `(name, f)` when `filter.is_empty() || name.contains(filter)`.

Integration detail: `sdfgen_helper.py` generates the Python serializer class by scanning the
headers named in `SCHEDULER_CONFIG_HEADERS`, so `test_selection_t` must be declared in a
header the test variant's `.mk` puts on that list — the same route
`test_scheduler.user_config.h` already takes.

Second integration detail: the helper maps `char[256]` to `c_char * 256` and serializes it as
`self.filter + [c_char()] * (256 - len(self.filter))`, i.e. it expects a **list**, not a
Python `str` — passing a string raises `TypeError`. `meta.py` must hand it
`[bytes([b]) for b in filter.encode()]`.

**Why a string rather than a bitmask or index list.** The build side never needs to know what
tests exist — the Makefile serializes `TESTS=` verbatim, so there is no manifest file to keep
in sync and no proc-macro emitting build artifacts. And a string survives reordering, where
indices silently shift onto the wrong test. It also matches the idiom developers already use
for the component tests in this same repo: `cargo test fan_` alongside
`make CONFIG=test_scheduler.mk TESTS=fan_`.

**Suites come free.** `system_tests!` nests, and qualified names make the one filter cover
both granularities:

```rust
system_tests! {
    suite nominal {
        fn fan_turns_on_when_too_hot() { /* ... */ }
        fn setpoint_is_latched()       { /* ... */ }
    }
    suite fault_injection {
        fn fan_failure_sets_error()    { /* ... */ }
    }
}
```

Table entries become `"nominal::fan_turns_on_when_too_hot"`, so `TESTS=nominal::` runs a
suite and `TESTS=fan_turns` runs one test — no second selection concept, and the same
behavior as `cargo test module::`.

#### Build plumbing (two things the mechanism does not get for free)

**`TESTS` must be in the rebuild hash.** The generated rule is

```make
$(SYSTEM_FILE): $(IMAGES) $(DTB) ${CHECK_FLAGS_BOARD_MD5}
	$(PYTHON) $(SDFGEN_HELPER) ...
	$(PYTHON) $(MSD) --sddf ... --objcopy $(OBJCOPY)
```

and the stamp hashes `CFLAGS`, `BOARD`, `MICROKIT_CONFIG`, `MICROKIT_SDK`, `MSD`,
`SCHEDULER_C` and `SCHEDULER_CONFIG_HEADERS` — **not `TESTS`**. Without a change,
`make TESTS=nominal::` followed by `make TESTS=fault_` leaves the stamp filename unchanged,
so `$(SYSTEM_FILE)` is up to date, `meta.py` never re-runs, `.test_selection` is never
re-patched, and the image silently runs the *previous* filter to a green result. `${TESTS}`
must be added to that hash; the stamp exists for exactly this purpose, and changing `MSD` or
`SCHEDULER_C` already forces a rebuild through it.

**`TESTS` must be routed into `meta.py`.** The rule invokes `$(MSD)` with a fixed argument
list — `--sddf --board --dtb --output --sdf --objcopy` — with no way to pass a filter. The
rule in `MakefileTemplate.scala` gains `--tests $(TESTS)`. An argument is preferred over
`os.environ` because it matches how every other value reaches the metaprogram and because it
appears in the build log, where a wrong filter is otherwise invisible.

**Two binding times, one filter:**

| When | How | Cost |
|------|-----|------|
| Image build (stage 3) | `TESTS=<filter>` -> meta.py serializes -> objcopy -> repack | seconds; no Rust recompile, no cargo |
| True runtime (stage 6, serial) | `list`, `run <filter>`, `run all` overwrite the same in-memory struct | none |

Stage 6 therefore adds no new selection design; it writes the struct the runner already
consults.

**Required behaviors:**

- A filter matching **zero** tests is a distinct nonzero verdict, never a pass. Reporting
  "0 tests matched" as success is how a typo'd filter silently turns a CI job green.
- The controller `sddf_dprintf`s the test table at init, and honors a list-only flag.
  Without it the host cannot discover what is runnable and stage 6's `list` has nothing
  to print.

**Dependency on D13.** Running an arbitrary subset is sound *only* because tests are
order-independent with explicit setup. If D13 is ever weakened, subset selection breaks with
it; the two decisions must move together.

#### Test isolation: order-independent with explicit setup (D13)

Threads run `_initialize` once, from their `init()`, so there is no way to return the system
to a fresh-boot state between tests without extending the scheduler protocol. Rather than
add a `Reset` command or accept one-test-per-boot, **tests are required to be
order-independent and to establish their own preconditions**. Consequences for how they are
written:

- A test must not assume anything it did not set. Whatever the previous test left in a port
  or state var is what this test starts with.
- A test must set *every* input it depends on, not just the one it varies — otherwise it
  silently inherits the prior test's value.
- The runner still normalizes **schedule position** between tests by advancing to a
  hyperperiod boundary, so `hstep(1)` means the same thing in every test. That is cheap and
  removes a whole class of order dependence without any re-initialization.
- *With contract checking (stage 7):* a previous test's injected unconnected inputs and state
  variables survive the runner's normalization. A broken assumption on them is only `INFO`,
  but a thread whose assumptions hold on them and that then breaks a guarantee fails whichever
  test is running. Setting those inputs before the first step keeps such a fault attributed to
  the test that caused it -- isolette's `reinitialize()` already does this for state variables.

To keep "set every input" tractable rather than error-prone, the controller generates a
whole-component setter per thread, the direct analogue of the component-level
`PreStateContainer_wGSV` / `put_concrete_inputs_container_wGSV` that `test_apis.rs` already
emits: one call establishes every **input** port and every GUMBO state var of a thread. A
test's setup is then a single container literal per thread it cares about.

Outputs are deliberately not in it. A pre-state is what the component reads; writing its
outputs would establish nothing about the component and would instead feed whatever
downstream thread consumes them — which is that thread's pre-state, and has its own
container.

As built, for `tcp_tct`:

```rust
pub struct tcp_tct_PreState {
  pub currentTemp: TempControl_SysVerif::Temperature,   // input, produced by tsp_tst
  pub setPoint: TempControl_SysVerif::SetPoint,         // input, unconnected
  pub fanAck: TempControl_SysVerif::FanAck,             // input, produced by fp_ft
  pub sv_currentSetPoint: TempControl_SysVerif::SetPoint,
  pub sv_currentFanState: TempControl_SysVerif::FanCmd,
  pub sv_latestTemp: TempControl_SysVerif::Temperature,
  pub sv_fanError: bool,
}
pub fn set_tcp_tct(s: tcp_tct_PreState) { /* delegates to the per-field setters */ }
```

Fields are named as the *component* sees them (`currentTemp`), not by the producer the region
is named after (`put_tsp_tst_currentTemp`), because the container describes one component's
inputs. State vars keep their `sv_` prefix, which also keeps them clear of a port of the same
name. A thread with no inputs and no state vars gets no container -- `tsp_tst` in this model.

*The setter inherits D15's ordering.* `set_tcp_tct` writes `currentTemp` into `tsp_tst`'s
region, and `tsp_tst` runs earlier in the frame, so the call is only reliable while the
schedule is parked immediately before `tcp_tct`: `run_to_thread(channels::tcp_tct_MON)`,
`set_tcp_tct(..)`, then step. The generated doc comment on every `set_<thread>` says so.

*Direction cannot be read off the region.* For an **unconnected** input the region is named
after the reader, so its `outgoingPortPath` equals the thread's own port path and is
indistinguishable from an output's. The AADL feature's direction is asked instead.

**The container requires every field (D14).** The setter takes the container struct by
value, and the container derives **no** `Default`. A plain Rust struct literal already
requires every field, so rustc — not developer vigilance — enforces that a test's setup is
complete, and a state var added to the model later breaks every test that has not accounted
for it. That failure is the point: silently defaulting a new field is how a test starts
inheriting the previous test's value again, which is exactly what D13 exists to prevent.

This is why the container must not gain `#[derive(Default)]` as a convenience: it would
re-enable `..Default::default()` and reopen the hazard. The existing component-level
`PreStateContainer{,_wGSV}` derive nothing today, and the controller's analogue should stay
that way.

### Contract observation in the controller (stage 7, planned)

**Status: designed, not built.**

Today a contract violation cannot fail a system test. The monitors that check contracts are
stripped from the test variant (see "Relationship to the runtime monitor"), and even where they
run they report only `log::warn!("*** CONTRACT VIOLATION ...")` lines, which the D18 host
driver ignores because they lack the `TEST | ` prefix. Stage 7 makes the controller check the
same contracts at every dispatch and turns a violation into a failure of the test that caused
it. This is the D3 payoff "every thread's postcondition held at every dispatch", obtained
without adding a monitor PD to the schedule under test.

#### What requesting system testing generates

Requesting system testing (`ENABLE_TEST_SCHEDULER`) is enough; `--runtime-monitoring` is not
needed. The test variant gets everything stages 1-5 provide, plus whichever checks the model
has contracts for:

| Generated into the controller | When the model has |
|-------------------------------|--------------------|
| GUMBO checks (the component layer) | any GUMBO thread contracts: `initialize` / `compute` clauses or integration constraints; data invariants once they are implemented |
| System-verification checks (the system layer) | a composition (`hasCompositions`); one system layer per composition |
| neither | no contracts at all (vms) |

Integration constraints need no separate treatment: GUMBOX already folds them into the
checks, `I_Assm_<port>` into CEP_Pre and `I_Guar_<port>` into the IEP/CEP post-conditions
(isolette's `thermostat_mt_mmi_mmi` and `operator_interface_oip_oit`). Data invariants will
arrive the same way, through the GUMBOX predicates, when they are implemented.

**Consequence for D5.** The component layer reads GUMBO state variables, and today the `sv_`
ports, their regions and `is_monitoring_enabled()` exist only when `--runtime-monitoring` is
given, which is why D5 demands that flag. Under stage 7 system testing brings that plumbing
itself, and the gate for the component layer widens from today's `hasThreadsWithStateVars` to
"has GUMBO thread contracts", since a model with contracts but no state variables still has
CEP_Pre/CEP_Post to check. D5's diagnostic goes away with it. Two pieces of today's monitor
plugins have to be separated from the monitor PDs for this:

- **The `sv_` ports.** They are not created on their own today: `wireMonitorStateVars` adds
  each thread's `sv_` output ports while wiring that thread to a monitor, in the same loop that
  calls `injectMonitorPDNamed`. The port creation moves into a step of its own that runs for
  `--runtime-monitoring` **or** `ENABLE_TEST_SCHEDULER`; the monitor wiring stays behind
  `--runtime-monitoring`. With no monitor attached the `sv_` ports are unconnected outputs,
  and their regions come from `CConnectionProviderPlugin`'s unconnected-port loop, the route
  D15 already relies on. `handleCBackend` (`is_monitoring_enabled()` and the `put_sv_*`
  guards) moves with it, including the check `GumboSysAssertMonitorPlugin` uses today to skip
  it when the gumbo monitor already ran.
- **The check code.** `GumboMonitorPlugin` and `GumboSysAssertMonitorPlugin` are gated on
  `options.runtimeMonitoring`, so they cannot be what generates the layers for a run that asks
  only for system testing. The layers are generated by a new `ContractObserverPlugin` that runs
  for `--runtime-monitoring` **or** `ENABLE_TEST_SCHEDULER` and puts `crates/observers` in the
  store; the monitor plugins and `TestSchedulerPlugin` consume it.

The monitor PDs and their variant bundles still require `--runtime-monitoring`.

#### The two monitors are layers, and the design keeps them as layers

`sys_<id>_monitor` is `gumbo_monitor` plus one layer. Its `timeTriggered` calls
`self.gumbo_monitor(api)` and then `self.sys_assert_monitor(api)`, and its `gumbo_monitor`
body is identical to the gumbo monitor's apart from the API type. The two cannot run together
(one injected PD per variant, and each brings its own slots), which costs nothing because the
system monitor already includes the component checks.

Stage 7 generates the checks once, as two layers, and lets each consumer hold the layers its
role needs:

| Role | Component layer (IEP_Post, CEP_Pre, CEP_Post) | System layer (Petri-net marking, sys asserts) |
|------|-----------------------------------------------|-----------------------------------------------|
| `gumbo_monitor` PD | yes | no |
| `sys_<id>_monitor` PD | yes | yes, composition `<id>` |
| test controller | yes, if generated; switchable | yes, one per composition, if generated; switchable |

The monitor PDs are fixed combinations: the system monitor is the only way to get both, and
the two cannot run together. The controller has neither restriction. Its two layers are
independent -- the system layer's marking and assertions use nothing from the component
layer -- so GUMBO checking and system-verification checking are enabled and disabled
separately, and every combination is available, including system checks alone. Every
composition's system layer runs, and the component layer runs once however many there are.

#### Step 1: split "where am I" from "what to check"

Each monitor body currently does two jobs:

- **Locate.** Read `sched_state.current_timeslice`, look up `prev_user_ch[idx]` and
  `next_user_ch[idx]` (tables built from `sched_schedule.is_user_partition`), and detect a new
  frame from the index wrapping. This belongs to the monitor variants alone: `idx` is the index
  of a *monitor* slot, and the test variant's schedule has none.
- **Check.** On the first run, IEP_Post for every thread. On each boundary, two things:
  **completion** of `prev` -- CEP_Post against its saved pre-state, and for the system layer,
  advancing the marking from `prev` and checking the assertions at every place the cascade
  visits -- then **dispatch** of `next` -- save its pre-state and check its CEP_Pre.

A monitor sees both halves at once, in its slot between `prev` and `next`. The controller does
not always: the last slot of a command completes when the command does, and the next dispatch
may be a test step later (see Step 4). The check half is therefore split along that line, and
needs only a channel and a way to read ports and state variables.
It is generated once, generic over two traits, into a new `crates/observers` crate beside
`data` and `GUMBO_Library`:

```rust
pub trait SystemView {                  // one getter per port / state var the checks read
    fn get_thermostat_rt_mri_mri_displayed_temp(&self) -> Option<Temp_i>;
    fn get_thermostat_rt_mhs_mhs_sv_lastCmd(&self) -> Option<On_Off>;
    // ...
}
pub trait ViolationSink { fn report(&mut self, v: Violation); }

pub struct ComponentContracts { /* Option<PreState_<thread>> per thread */ }
impl ComponentContracts {
    pub fn on_init(&mut self, s: &impl SystemView, out: &mut impl ViolationSink);
    pub fn on_complete(&mut self, prev: u32, s: &impl SystemView, out: &mut impl ViolationSink);
    pub fn on_dispatch(&mut self, next: u32, s: &impl SystemView, out: &mut impl ViolationSink);
}

pub struct SysAssert_nominal { ready: u64 }          // one struct per composition
impl SysAssert_nominal {
    pub fn validate_schedule(sched: &Schedule) -> bool;   // today's validate_schedule_conformance
    pub fn on_init(&mut self);                            // start place + initial cascade
    pub fn on_complete(&mut self, prev: u32, s: &impl SystemView, out: &mut impl ViolationSink);
}
```

`ContractObserverPlugin` emits `ComponentContracts` and one `SysAssert_<id>` per composition.
`Violation` carries its kind (`IepPost`, `CepPre`, `CepPost`, `SysAssert`), the thread or
composition and place, the position, and the rendered pre/post values, and its message text is
produced in the shared code. The `gumbox/*.rs` modules today
copied into every monitor crate move into `observers` as well.

#### Step 2: the monitor PDs become thin wrappers

```rust
// gumbo_monitor
let (prev, next, first) = self.locate(api.get_sched_state(), api.get_sched_schedule());
if first { self.components.on_init(&view, &mut LogSink) }
else {
    self.components.on_complete(prev, &view, &mut LogSink);
    self.components.on_dispatch(next, &view, &mut LogSink);
}

// sys_<id>_monitor: the same, then its own layer, in today's timeTriggered order
self.sys_nominal.on_complete(prev, &view, &mut LogSink);
```

`view` implements `SystemView` by delegating to the monitor's existing `api.get_*`; `LogSink`
reports with `log::warn!` and the existing message text. The monitors behave exactly as
before, so step 2 is verified by golden diffs showing code moving without changing and by the
monitor variants' logs on target being unchanged. It is worth landing on its own, before any
controller work.

#### Step 3: the controller hosts the layers

```rust
struct Observer {
    components: ComponentContracts,   // generated when the model has GUMBO thread contracts
    sys_nominal: SysAssert_nominal,   // generated per composition
    // ...
    gumbo_live: bool,                 // build level: is the layer tracked at all this run
    sysverif_live: bool,
    gumbo_enabled: bool,              // suite / test level: is a live layer checked right now
    sysverif_enabled: bool,
}
```

The switches work at two depths (see "Enabling and disabling checks"):

- **Build level decides whether a layer is live.** A layer disabled at build level is not
  tracked at all: no pre-states saved, no marking advanced, and it takes no part in the
  scheduler's park.
- **Suite and test level decide whether a live layer checks.** A layer disabled there still
  saves each thread's pre-state and still advances its marking, so re-enabling it later in the
  same test, mid-frame included, is immediately correct, with no restart and no pre-state
  missing.

**`SystemView` for the controller** reads the port and `sv_` regions the controller already
maps, but **not through `inspect::get_*`**. Those reads are destructive: `inspect::get_*`
dequeues through the controller's receive cursor, so a second read of the same region with no
dispatch in between returns `None` (isolette's `tests.rs` documents this for test writers).
Sharing that cursor would let the checks consume the values a test is about to read, let a
test's read make a check see `None` and skip silently, break a region read twice in one park
(the output of `prev` read by `on_complete` that is also an input of `next` read by
`on_dispatch`), and let `on_init` consume the post-initialization outputs that isolette's
`regulator_post_init()` snapshots before its first test.

The queues are broadcast with a private cursor per receiver, which is exactly how each monitor
PD reads them without disturbing anyone. So the controller bridge gains a second accessor
family, `obs_get_*`, with its own `Recv_t` per region, used only by the view. The view keeps
the **last value** it read per region and returns it until a newer one arrives -- data-port
semantics, what a thread sees -- so a check reads a region as often as it likes, and `None`
means only "never written". `inspect::` is unchanged and remains the tests' own cursor.

One more difference, for state variables: a value the controller has injected but the thread
has not yet adopted is still in the `inj_sv_` queue, while the `sv_` region holds the old
value. Recording that old value
as `In_<var>` would make CEP_Post compare against a pre-state the thread never started from.
The generated `put_<thread>_sv_<var>` therefore also records a pending value, the view returns
it in preference to the `sv_` region, and it is cleared once `<thread>` has dispatched. Port
injections need no such care, because they are written into the producer's region, which is
what the view reads. A getter returning `None` (a region never written) makes the check skip,
matching the monitors' existing "post check skipped: no saved pre-state".

**`TestSink`** cannot report the way `sys_assert!` does. A `sys_assert!` fails in the test
body and returns from it; a violation is found inside `api::hstep` and friends, in the
controller's wait loop, where there is no test body to return from. And D18's host driver
counts `TEST | FAIL` lines and fails the run as "output was lost" unless the count equals
`DONE`'s `failed=`, so one failed test must produce exactly one `FAIL` line however many
violations it saw. So:

- A violation is printed as `TEST | VIOLATION <kind> <thread or composition/place> hp=<n>
  slot=<n>`, with the pre/post values as `TEST | INFO` lines, and marks the test failed. The
  test keeps running to the end of its body; there is nothing to return from.
- The **first** failure of a test prints its `TEST | FAIL` line: `sys_assert!` with its file
  and line as today, or the runner, at the end of the test, with `contract violation` when the
  test failed only through violations. Later failures of the same test print no second `FAIL`.
  (Today the runner prints no `FAIL` line itself; `fail()` does.)
- The host driver already passes `VIOLATION` lines through, as it does every `TEST | ` line.
  Its only change is the `init=` field of `DONE` (Step 4).

Negative tests that trip a guarantee or a system assertion on purpose declare it with
`observe::expect(..)`:

- It matches on kind and location: `expect(CepPost, tcp_tct)`, `expect(SysAssert,
  nominal, "after mri")`, with the thread, composition and place names generated as constants
  so a typo is a compile error.
- It covers the rest of the test. Matching violations are printed as `TEST | INFO` instead of
  failing it; any other violation still fails it.
- **An expectation never met fails the test**, like `#[should_panic]`, with a `TEST | FAIL`
  naming it. Otherwise a negative test keeps passing after the fault it targets stops
  happening.

`observe::take()` returns the violations recorded so far in the test and clears them, for a
test that wants to assert on them itself. Tests that only feed invalid inputs need neither -- a
broken assumption is not a failure (see "Assumptions versus guarantees").

#### Step 4: an observation park in the scheduler

The controller is the lowest-priority PD (D4), so during `hstep(n)` it never runs between
slots and cannot see intermediate dispatches on its own. The scheduler provides the boundary:

- `test_command_t` gains `observe`. When it is set, the scheduler parks before dispatching
  each user slot (pads and non-user slots excluded), publishing `prev_ch`, `next_ch`,
  hyperperiod, slot and `obs_seq` in `test_status`, and notifies the controller.
- `test_status` also gains `completed_seq`, incremented by the scheduler on every user-slot
  completion, observed or not. It identifies a *dispatch*, which a channel cannot: the same
  channel completes once per frame. The controller keeps the last `completed_seq` it ran
  `on_complete` for, and runs it again only when `completed_seq` has moved past that value.
  This matters because `on_complete` is not idempotent -- the system layer's advances the
  marking -- so completing one dispatch twice would desynchronize the marking and falsify every
  system assertion after it.
- While any layer is live the scheduler parks before every user dispatch, so `completed_seq`
  never moves by more than one between completion checks. A larger jump means a dispatch went
  unobserved (a command issued without `observe`, say), leaving a saved pre-state or the marking
  stale. The controller treats it like a watchdog trip: a `TEST | INFO` line, saved pre-states
  dropped, and the system layer resynchronized at the next frame boundary.
- The controller's busy-wait loop, which already runs whenever the scheduler is parked,
  watches `obs_seq` as well as `ack_seq`. On a new `obs_seq` it calls `on_complete(prev)` if
  `completed_seq` has moved, then `on_dispatch(next)`, then writes `obs_ack` and notifies the
  scheduler, which dispatches. This is the D9 sequence-number
  handshake a second time. The scheduler's `TEST_CONTROLLER_CH` arm tells the two apart by
  which sequence number moved.
- **Command completion is also a completion point.** When a command completes, the last slot it
  dispatched has finished, but no park follows until the next command starts -- which may be
  after the test has ended. So when the controller sees `ack_seq`, it calls
  `on_complete(last_dispatched_ch)` before returning from the API call -- **if
  `completed_seq` has moved**. A command can complete without dispatching anything
  (`run_to_thread(T)` while already parked before `T`, `info_state()`, a `RunTo*` rejected at
  decode), and then `last_dispatched_ch` names a slot already checked; the `completed_seq`
  comparison is what keeps it from being completed again. Beyond publishing
  `completed_seq`, this needs no scheduler change: the controller runs at command completion
  anyway. Without it, a violation in
  a test's last dispatch would fall outside its recording window and be lost.
- The park comes **before** the slot's watchdog is armed, so time the controller spends
  checking is never charged to the thread as an overrun.
- The park is taken **when the dispatch is about to start**, not when the previous command
  completed. A test injects between those two moments, so the pre-state captured for `next`
  already includes the injection. Parking at command completion would recreate the
  stale-pre-state problem this design avoids.
- `observe` is set when at least one layer is **live**: generated, and not disabled at build
  level. Suite- and test-level switches do not clear it, because a live layer keeps tracking
  while its checking is off. The cost is two handshakes per dispatch, small beside a dispatch
  under QEMU.
- With no live layer the controller never sets `observe`, the scheduler never parks, and the
  run is exactly stages 2-5. That covers a model with no GUMBO contracts and no composition,
  which generates no layer, and a model whose layers are all disabled at build level.

`on_init` runs once, from the runner, before any dispatch: the controller is kicked at the
all-ready point (D4), when every thread has initialized and none has computed, which is the
point the monitors' first run observes. For the system layers, `SysAssert_<id>::on_init` puts
the marking at its start place and fires the initial cascade, as the system monitor's first run
does, and `validate_schedule` runs against `test_schedule`; its `is_user_partition` bits are the
production schedule's.

Initialization is not part of any test, so its violations fall outside every recording window
(see below) and need their own verdict. They cannot become a synthetic always-run test, which
would count toward `matched=` and defeat D17's zero-match check. Instead IEP_Post violations
are printed as `TEST | VIOLATION` lines as usual, and the `DONE` line gains `init=ok|failed`;
the host driver fails the run on `init=failed`. This is the one host-driver change stage 7
needs.

Layer state persists across tests. Saved pre-states are per dispatch anyway, and the marking
must see every dispatch, so tracking stays on through the runner's normalization; only
recording pauses (see "Recording window").

**After a watchdog trip.** `TEST_FLAG_OVERRUN` aborts a command with a thread that never
reported completion. Its saved pre-state no longer describes anything, and the marking has lost
its place. The controller drops every saved pre-state (the next completion check of each thread
is skipped, as for a missing pre-state), suspends the system layer until the next frame
boundary, and prints a `TEST | INFO` line saying so, instead of producing a run of false
violations from one hang.

#### Recording window

Violations are **recorded only while a test runs**: from its `BEGIN` to the end of its body,
including the completion check of its last command (see Step 4). Everything the runner does
between tests -- its `run_to_slot(0)` normalization -- is observed but not recorded. The
layers keep **tracking** throughout, saving pre-states and advancing markings, so the next test
starts with correct state; only reporting pauses.

This makes "between tests" disappear as a category. A finished test's injected values may still
drive the rest of the frame out of contract during normalization, and none of that is anyone's
verdict. What can still carry over is an injected value that outlives the normalization -- an
unconnected input, which only the controller writes, or an injected state variable the thread
keeps. If it breaks a thread's assumption, that is only `INFO` (see "Assumptions versus
guarantees"). If the thread's assumptions hold on it and the thread still breaks a guarantee,
that is a real fault, charged to the test that was running -- one test late, which D13's
"set every input you depend on" avoids (see D13).

#### Assumptions versus guarantees

GUMBO contracts are assume-guarantee: when a dispatch's CEP_Pre (including `I_Assm_*`) does not
hold, the component owes nothing. The controller follows that directly:

- **CEP_Pre fails** -> reported as `TEST | INFO assumption not met`, and that dispatch's
  CEP_Post is skipped. Not a failure by itself.
- **IEP_Post, CEP_Post and `I_Guar_*`** fail the test whenever they are checked.
- **System assertions** fail the test whenever they are checked.

The skip is a **setting of the shared checks** (`ComponentContracts::excuse_post_on_failed_pre`),
switched on by the controller only. The monitor PDs leave it off and keep checking CEP_Post
regardless, as they do today, so Step 2's "monitors unchanged" holds; whether they should adopt
the rule is a separate decision.

This needs no record of where a value came from. A robustness test can feed out-of-range values
without `observe::expect`: the consumer's broken assumption is information, and it is excused
from its guarantee for that dispatch. Integration faults are still caught, by the guarantees: a
producer that hands its consumer a value outside the consumer's assumptions has either broken
its own guarantee (CEP_Post or `I_Guar`), which fails the test, or has a guarantee too weak for
that assumption. The second is a mismatch between the contracts, not a runtime event, and for
integration constraints the build already checks it statically (`I_Guar => I_Assm`, the
"Checking integration constraints" step of `.ci/ci.cmd`).

What this gives up: a `compute` assumption, which can relate several ports and state variables,
is not covered by that static check, so a legitimate producer output that violates one appears
only as `INFO`. If that proves to matter, a strict switch can turn CEP_Pre failures into
failures; it is not planned.

A test that deliberately drives the system out of contract and so breaks a system assertion,
or a downstream guarantee, declares that with `observe::expect`, or turns the relevant checks
off for its suite.

#### Enabling and disabling checks

Each generated layer is **enabled by default**. The user disables either one at three levels:

| Level | How | Scope |
|-------|-----|-------|
| Build | `make CONFIG=test_scheduler.mk GUMBO_CHECKS=off SYSVERIF_CHECKS=off`, new fields of `test_selection_t` patched through the same ELF section as `TESTS=` (D17), and part of the rebuild hash with it | the whole run; the layer is **not live**: not tracked, and not parked for |
| Suite | `suite fault_injection(gumbo = off) { ... }` in `system_tests!` | every test in the suite; checking only |
| Test | `observe::set_gumbo(false)` / `observe::set_sysverif(false)` in the test body | the rest of that test; checking only |

Build level is the only way to get the unchecked run at stages 2-5 speed: with both switches
off, nothing is live, and the scheduler never parks. Suite and test level can turn a live
layer's checking off and back on, but they **cannot bring a layer back** that build level
turned off, because it has not been tracking. A suite or test that asks for one --
`(gumbo = on)`, or `observe::set_gumbo(true)` -- fails at that point with a message naming the
build switch, rather than running and silently checking nothing. It cannot be a compile error:
build-level settings are patched into the ELF after compilation, like `TESTS=`.

The runner restores the build-level setting before every test, so a test or suite that turns a
check off cannot leave it off for the next one -- the same isolation D13 requires of inputs.
Asking for a layer the model did not generate is a compile error on the per-test API (the
function is not generated) and a warning on the build variable.

Disabling is the blunt tool. A negative test that trips one particular contract on purpose
should keep checking on and declare the expectation with `observe::expect(..)`, so every other
contract is still checked while it runs.

#### Event ports

The monitors read every port with their own `get_x(*mut T) -> bool` on a private receiver
cursor, so for an event or event-data port "was an event present at this dispatch" is answered
by their own dequeue. The controller's `obs_get_*` has its own cursor too, but keeps the last
value per region, which is right for data ports and wrong for event ports: an event must be
reported once, at the dispatch that consumes it, not held. So `obs_get_*` needs event-port
semantics for event and event-data ports -- present at a dispatch only if it arrived since the
previous check of that consumer -- or CEP_Pre/CEP_Post will see a different event than the
thread did.

That also means **one cursor per (region, consumer)** for event ports, not one per region.
An event port that fans out to several consumers is dequeued by each of them independently; a
single observer cursor would consume the event at consumer A's dispatch, and consumer B's
CEP_Pre would see none. Data ports keep one cursor per region, since the last-value cache
serves every reader. A queue deeper than one raises the further question of which element a
dispatch consumed, which the view must answer the way the thread's own `get_` does. Isolette and
temp-control exercise only data ports, so this is unverified: stage 7's verification includes a
model with event ports (`gumbo-verus/pure_event_port`).

#### What stage 7 does not cover

It tests the *unmonitored* configuration against the contracts; it does not test a monitor PD.
Whether the monitored configuration, with the monitor's slots in the frame, still meets its
contracts, and whether the monitor itself is correct, is the separate open item "a variant with
both the controller and a monitor PD". The steps above are prerequisites for that variant too:
it needs the same fix for injected state variables (the monitor would read the `inj_sv_` queues
through its own receiver cursor, without consuming them), and a violation counter the
controller can read in place of log lines. Its parking points would also have to move to
before the monitor slot that precedes the target thread, which is where the monitor records
that thread's pre-state.

### Interactive CLI (deferred)

**Status: not planned work. Kept here as a possible future item, decided 2026-09-22.**

The idea: a serial CLI PD parses text commands (`h 2`, `s 5`, `run thread tcp_tct`) from the
UART and emits the same `test_command_t`, with a host-side expect/pytest driver over QEMU
stdio. sDDF's serial driver and components are already in `SDDF_MAKEFILES`.

It is deferred because it cannot be built without changing how output works for every PD, and
the schedule it would contaminate is the thing under test.

#### Why the UART is the obstacle

Output today needs no device ownership at all. `sddf_dprintf` links sDDF's `putchar_debug.c`:

```c
void _sddf_dbg_puts(const char *s) { while (*s) { seL4_DebugPutChar(*s); s++; } }
```

That is a kernel syscall -- the *kernel* owns the PL011 and writes it from kernel mode. No PD
claims the device, no arbitration is needed, and none exists. All seven PDs in temp-control
print freely and D18 scrapes clean lines. No model in the test corpus instantiates an sDDF
serial driver; `serial="pl011@9000000"` is only a `Board` field, never claimed.

An *interactive* CLI needs UART **input**, and the debug syscall surface is output-only:
`seL4_DebugPutChar` and `seL4_DebugPutString` exist; there is no `seL4_DebugGetChar`. The only
way to read the UART is for a userspace PD to own the PL011 -- map its MMIO, take its IRQ,
initialize it. That is the sDDF serial driver, and once it exists the kernel's debug putchar
is still writing the same registers behind its back, after the driver has reconfigured FIFOs,
IRQ masks and baud out from under a path that assumed its boot-time setup. D18 treats a
missing terminator as a failure, so garbled output reads as a real failure rather than a
flake -- correct, but fragile exactly where the verdict has to be trustworthy.

#### The options, and why this is deferred rather than solved

| Option | Assessment |
|--------|------------|
| Route **all** output through sDDF serial (`putchar_serial.c` everywhere, tx queue + channel per PD) | The only honest path. But `putchar_serial` buffers until `\n` then notifies the tx virtualiser, and under a **stepped** scheduler the virtualiser runs only when dispatched -- verdict lines could sit unflushed while the controller busy-waits. That has to be solved before anything else is built. Also a channel and queue regions per PD against a 62-channel budget |
| Keep debug output, add no driver | Then there is no input path, and it is not an interactive CLI |
| Semihosting / gdb stub / host-written command region | All bypass the UART, all are QEMU-only; the design is meant to survive on hardware |
| **Don't build it** | Stage 3's scripted controller plus the `TESTS=` filter already gives CI everything: select at build time, run, scrape a verdict, exit nonzero. The CLI is developer convenience for exploration, not verification, and its price is adding serial PDs to the schedule under test -- the system being tested stops being the system that ships |

The last row is the reason for deferral. If interactivity is wanted later, the first row is the
path and the flush-under-stepped-scheduling problem is the first thing to solve.

## Decisions

| # | Decision | Rationale |
|---|----------|-----------|
| D1 | Scheduler is an event-driven state machine, not a blocking loop | Forced by C1 |
| D2 | Emit a `test_scheduler.*` variant bundle in the existing 5-file shape | C3; no new mechanism |
| D3 | Controller is Rust | Inherits crate/API/test-harness generation and GUMBO contract reuse |
| D4 | Controller is a passive child PD with no timeslice, at the **lowest priority in the system**, blocking by busy-wait; the scheduler kicks it at the all-ready point | It must run while the schedule is paused; lowest priority is what lets a busy-wait coexist with thread execution, and the kick is what avoids a boot deadlock |
| D5 | Gate on experimental option `ENABLE_TEST_SCHEDULER` (`-x`, `;`-separated). Require `--runtime-monitoring` alongside it **only when the model has threads with state vars**, erroring with a diagnostic if absent | No cligen regen, no public flag while the design is in flux. Where it is required, `--runtime-monitoring` is needed not because the monitor runs — it is stripped from the test variant — but because it is what makes `GumboMonitorPlugin` create the `sv_` ports, regions and `is_monitoring_enabled` plumbing. The diagnostic must say so, or the flag reads as spurious and gets dropped. For a contract-free model that plumbing does not exist, so the flag buys nothing and is not demanded. *Stage 7 (D22) removes the `--runtime-monitoring` requirement: system testing will bring the `sv_` plumbing itself* |
| D6 | Stage A (timer-gated) before Stage B (completion-driven) | Stage A needs no component changes at all |
| D7 | Scripted controller before serial CLI | Fewest moving parts to a working step/inspect loop. In the event the serial CLI was deferred indefinitely and the scripted controller turned out to cover CI on its own |
| D8 | Pilot on temp-control | Already carries 3 variant bundles and 3 threads, and has GUMBO state vars for inspect/mutate |
| D9 | Sequence-number handshake, not a completion flag | Race-free with plain ordered writes |
| D10 | Keep the timer as a watchdog in Stage B | A hung thread should fail the run, not wedge it |
| D11 | Tests declared in `system_tests!` blocks that emit the registration table -- one block, or one per file joined by `system_test_files!` | No dev-deps on target; no linker-section tricks; the list cannot drift |
| D12 | Recording assertions (`sys_assert_eq!`), not `assert!` | `no_std` + `panic = abort`: no `catch_unwind`, so a panic would end the run |
| D13 | Tests are order-independent with explicit setup | Avoids extending the scheduler protocol with `Reset` and avoids one-test-per-boot |
| D14 | The whole-component setter requires every field; no `Default` on the container | rustc enforces complete setup; a new state var breaks stale tests instead of silently defaulting. Verified: deleting a field gives `E0063: missing field` |
| D15 | Port injection enqueues into the producer's region (controller is an extra writer) | Semantically identical to the real producer; reuses generated queue code; no thread-side change |
| D16 | State var injection via mirrored `inj_sv_X` regions, not bidirectional `sv_X` regions | Queue emptiness is the dirty flag; keeps one-writer-per-region |
| D16a | Those regions are **plugin-declared, not AADL ports** | Model ports leak into the component test harness and the system verification model; see "The regions must not be AADL ports" |
| D17 | Test selection is a filter string in an ELF section, matched by substring against qualified `suite::test` names | Follows the `user_schedule` objcopy precedent; no manifest to sync; survives reordering; suites and single tests use one mechanism; mirrors `cargo test <filter>` |
| D18 | Verdict leaves the target as prefixed serial lines with a **mandatory** `DONE matched=/passed=/failed=` terminator, scraped by a host driver with a hard timeout | Serial is the only channel off-target — `test_status` is guest memory the host cannot read. Every real failure mode is silence, so absence of the terminator must fail rather than pass |
| D19 | Contract checks are generated once, by a `ContractObserverPlugin` that runs for `--runtime-monitoring` or system testing, as a component layer and a per-composition system layer, generic over `SystemView` and `ViolationSink`, and shared by the monitor PDs and the controller | The system monitor is already the gumbo monitor plus a layer; making that structural gives one copy of the checks and lets a role be a choice of layers. The monitor plugins are gated on `--runtime-monitoring`, so they cannot own code that system testing needs without it |
| D20 | The checks split into a completion half (`on_complete`) and a dispatch half (`on_dispatch`). The controller runs them at a scheduler park taken just before each user dispatch, gated by `observe` in `test_cmd`, and runs the completion half again at command completion; a completion is run only when the scheduler's `completed_seq` has moved | The lowest-priority controller cannot otherwise run between slots. Parking at dispatch makes the captured pre-state include the test's injections; checking completion at command completion keeps a test's last dispatch inside its own verdict. `completed_seq` identifies a dispatch, which a channel cannot, so a command that dispatched nothing cannot complete a slot twice and desynchronize the marking |
| D21 | A violation is printed as `TEST \| VIOLATION` and fails the running test, which prints exactly one `TEST \| FAIL`; violations are recorded only from a test's `BEGIN` to the end of its body, while tracking continues between tests; `observe::expect` covers guarantees a test breaks on purpose | A violation should be a verdict, not a log line. D18 counts `FAIL` lines against `failed=`, and a violation is found inside an API call, where a test body cannot be returned from. What the runner's normalization dispatches is nobody's verdict |
| D22 | Requesting system testing generates the GUMBO checks when the model has GUMBO thread contracts (integration constraints included, data invariants once implemented) and the system-verification checks when it has a composition, without `--runtime-monitoring`; the `sv_` plumbing follows `--runtime-monitoring` **or** system testing | The user asks for system testing, not for monitoring; the checks belong to it. Supersedes D5's requirement once stage 7 lands |
| D23 | GUMBO and system-verification checks are separate switches, each on by default when generated. The build-level switch decides whether a layer is live (tracked and parked for); suite- and test-level switches decide only whether a live layer checks, and cannot revive a layer turned off at build level | The layers are independent, so the monitor PDs' fixed combinations need not apply to the controller. Build-level off is a request for the unchecked run, so it restores stage 2-5 behavior with no parks; within a run, tracking regardless of checking keeps re-enabling correct at any point |
| D24 | In the controller, a failed CEP_Pre is reported as information and excuses that dispatch's CEP_Post (a setting of the shared checks the monitor PDs leave off); IEP_Post, CEP_Post, `I_Guar` and system assertions fail the test whenever checked | GUMBO is assume-guarantee: a component owes nothing when its assumptions fail. Integration faults still surface as broken guarantees, and `I_Guar => I_Assm` is checked statically, so no record of where a value came from is needed |
| D25 | The controller's checks read through their own `obs_get_*` receive cursors with a last-value cache for data ports, and one cursor per (region, consumer) with event semantics for event ports, never through the tests' `inspect::get_*` | `inspect::get_*` reads are destructive; sharing the cursor would let checks and tests consume each other's values, and a check reading `None` would skip silently |

## Implementation Plan

| Stage | Work | Verified by |
|-------|------|-------------|
| 1 &#10003; | This document | review |
| 2 &#10003; | `TestSchedulerPlugin` + the `test_scheduler.*` bundle, and a `scheduler.c` that is a **rewrite of the default, not a copy with a flag** — see below | golden `expected/` diff on temp-control **and** on a contract-free model (the vms `data_receiver` model); the three existing monitor bundles byte-identical; QEMU boot under `CONFIG=test_scheduler.mk` behaving as the default variant does |
| 3 &#10003; | Controller PD injected at lowest priority with the busy-wait blocking loop (volatile `ack_seq` load, run-once guard) and the all-ready kick (D4), channel + region maps, generated Rust command API, `system_tests!` macro (with suites) + in-crate runner + `sys_assert_*` (D11-D12), `TESTS=` filter via ELF section (D17), serial verdict + `run-tests.cmd` host driver forcing `MICROKIT_CONFIG=debug` (D18), `TESTS` added to the rebuild hash and routed to `meta.py` (D17), user-editable `tests.rs` | QEMU boot of temp-control under `CONFIG=test_scheduler.mk`, with a passing test, a deliberately failing test, the two run in both orders (D13), and `TESTS=` selecting a suite, a single test, and a filter matching nothing (D17) |
| 4 &#10003; | Completion-driven dispatch: bridge notify-back (C2) emitted unconditionally, per-slot timeout dropped, pads skipped, timer demoted to a watchdog at 10x the slot's configured budget (floor 1 s) raising `TEST_FLAG_OVERRUN` | `hstep(20)` inside a 12 s window that also covered build-check, boot and QEMU startup -- timer-gated needs 20 s of guest time alone |
| 5a &#10003; | Inspection + port injection (D15): observable-region inventory, C accessors in the controller bridge over the generated `sb_queue` API, `system_tests/inspect.rs`, `channels::` constants | 3 tests on target: injected values reach shared memory, state vars observable after a dispatch, reads are cursor-independent |
| 5c &#10003; | Whole-component setter (D14): one `<thread>_PreState` container per thread with no `Default`, and `set_<thread>` delegating to the existing per-field setters | a test that establishes all 7 of `tcp_tct`'s fields in one call and reads them back; deleting one field is a compile error (`E0063: missing field`) |
| 5b &#10003; | State var injection (D16/D16a): plugin-declared `inj_<thread>_sv_<var>` regions mapped `r` into the owning thread with `setvar_vaddr` and `rw` into the controller, thread-side `get_inj_sv_*` + `is_injection_enabled()` NULL gate, Rust ingest in `libComputePre`, `put_` suppressed on the `sv_` regions | 2 tests on target: an injected state var is adopted by the next dispatch and observable on its `sv_` region; a following dispatch with nothing set keeps it. Both checked against negative controls |
| 6 | *Deferred -- possible future work, not planned.* Serial CLI PD + host driver over QEMU stdio. Blocked on UART contention and on whether it earns its cost at all; see "Interactive CLI (deferred)" | n/a |
| 7 | Contract observation in the controller (D19-D24): (1) `ContractObserverPlugin` generating the observer layers into `crates/observers`, with the completion/dispatch split; (2) monitor PDs rewritten as thin wrappers; (3) `sv_` port creation and `handleCBackend` separated from the monitor wiring and driven by system testing as well, component-layer gate widened to "has GUMBO thread contracts"; (4) controller `SystemView` on its own `obs_get_*` cursors with last-value caching, pending-injection values, `TestSink` with `VIOLATION` lines and one `FAIL` per test, the recording window, `observe::` API; (5) the scheduler's observation park, `completed_seq`, and the completion check at command completion; (6) the two switches at build, suite and test level; (7) overrun handling | (1-2) golden diffs showing code moving without behavior change, `make verus` still passing on a model with monitors, and the monitor variants' on-target logs unchanged (they keep checking CEP_Post after a failed CEP_Pre); (3) golden diffs for a model built with system testing and without `--runtime-monitoring`, which now carries the `sv_` plumbing and no monitor bundles; (4-7) on target: a test that breaks a CEP_Post fails with one `FAIL` line and its `VIOLATION` lines, and the host driver's counts reconcile; a test with several violations still prints one `FAIL`; a violation in the last slot of a test's last command is charged to that test; a command that dispatches nothing (`run_to_thread` while already parked before its target, `info_state`) leaves the marking and saved pre-states untouched; the same test under `observe::expect` passes, and fails if the expected violation does not occur; checking does not change what `inspect::get_*` returns to a test, and isolette's initialization tests still see the post-initialization outputs; an injected state variable does not produce a false CEP_Post; an out-of-range injected input is reported as an assumption not met and skips that dispatch's CEP_Post, not a failure; an IEP_Post violation fails the run through `DONE init=failed`; a negative test does not make the next test fail; a run with system testing but without `--runtime-monitoring` still checks state-variable contracts; each switch disables its layer at build, suite and test level without affecting the next test; both switches off at build level gives a run with no parks; a test that re-enables a layer turned off at build level fails with a message naming the switch; a watchdog trip produces one `INFO` and no false violations; and a model with event ports (`gumbo-verus/pure_event_port`) checks the same events the threads saw, including an event port with two consumers |

### Stage 2 scope

Stage 2 was originally sketched as "the default scheduler plus a command budget, so the
variant is behaviorally identical to the default". Two review passes have made that framing
wrong: the test variant's `scheduler.c` shares the default's *structure* but replaces its
control flow, and carries infrastructure the default does not have.

What stage 2 delivers:

| | |
|---|---|
| Plugin | `TestSchedulerPlugin` subtyping `UserLandMonitorPlugin`, with the three supertype adjustments below, gated on `ENABLE_TEST_SCHEDULER` (D5) |
| Bundle | `test_scheduler.meta.py`, `.scheduler.c`, `.scheduler_config.h`, `.user_config.h`, `.mk` (C3) |
| Control flow | `try_advance` / `on_slot_complete` state machine replacing the default's `notify` / `next_partition` (C1) |
| Command path | `test_cmd` / `test_status` regions, sequence-number handshake, decode and validation |
| Schedule publish | the variant's own `sched_schedule` region and init-time publish — absent from the default template, and needed for slot-index-to-channel mapping |
| Robustness | bounded `RunTo*`, stale-watchdog filtering, `Stop` |
| Timing | timer-gated with pads honoured, so wall-clock behavior matches the default |

What stage 2 did **not** deliver: any controller. With none, nothing issued commands, so the
scheduler started in an implicit run-forever mode at the all-ready point. That made the stage 2
image *observationally* equivalent to the default variant — which is precisely what made it
verifiable on its own: the golden diffs confirmed the bundle was well-formed and disturbed
nothing else, and a QEMU boot confirmed the rewritten control flow still scheduled the system
correctly before any of it was driven by commands.

**Superseded by stage 3.** `init()` now parks at `TEST_CMD_NONE` and the all-ready point hands
over to the controller, so the current scheduler dispatches nothing until asked. `RUN_FOREVER`
survives as a *command* — it is what stage 6's CLI will use for "run freely until I interrupt"
— but it is no longer the startup state. This subsection is kept because the staging is the
reason stage 2 could be verified at all, not because it describes current behavior.

### Plugin structure

The first plan was to subtype `UserLandMonitorPlugin` and adjust it in three places (an
overridable gate in place of the hard-coded `options.runtimeMonitoring`, a way to skip the
slot interleaving the controller does not want, and a `getRetainedNonModelPorts` override).
As built, `TestSchedulerPlugin` is instead a standalone
`@datatype class … extends ModelTransformerPlugin with MicrokitPlugin with MicrokitFinalizePlugin`,
so none of those adjustments were needed:

- **Model transform** -- `TestControllerInjector` adds the controller process and thread, with
  no ports (deliberately not `MonitorInjector`).
- **`handle`** -- C bridge additions through `CConnectionProviderPlugin` (controller wrappers,
  observable-region accessors, the thread-side `get_inj_sv_*` / `is_injection_enabled`),
  Rust extern declarations and test stubs through `CRustApiPlugin`, and `lib.rs` weaving
  through `CRustComponentPlugin` (`libComputePre` ingest, the controller's `system_tests`
  module). `canHandle` waits for both Rust plugins' contributions to exist.
- **`finalizeMicrokit`** -- the MSD variant, built from the pre-monitor snapshot (see
  "Relationship to the runtime monitor"), the five-file bundle, the `system_tests/` modules and
  the host driver.

The only thing it takes from the monitor plugins is `MONITOR_ORIG_MSD_KEY`. Not inheriting
from `GumboMonitorPlugin` also keeps its `hasThreadsWithStateVars` gate away from
contract-free models.

Registration goes in `MicrokitPlugins.defaultMicrokitPlugins`.

### Invoking codegen

`-x` / `--experimental-options` is semicolon-separated (`HamrCli.scala`,
`parseStrings(args, j + 1, ';')`):

```
sireum hamr sysml codegen ... --runtime-monitoring --experimental-options ENABLE_TEST_SCHEDULER
```

For the pilot, `sysml/bin/run-hamr.cmd` appends `Os.cliArgs` to its fixed arguments, so the
option passes straight through, and `.ci/ci.cmd` already assembles the codegen option string
(it carries `--platform Microkit --runtime-monitoring --scheduling UserLand ...`). Enabling
the test scheduler for temp-control is appending the option to that line — which is also how
stage 2's golden outputs are regenerated.

The constant belongs in `ExperimentalOptions.scala` beside `USE_CASE_CONNECTORS` and
`DISABLE_SERGEN`, with a predicate following the `disableSergen` shape.

## Key Files

### This design's own code

| File | Role |
|------|------|
| `microkit/plugins/testing/TestSchedulerPlugin.scala` | The whole plugin: model transform, handle (C bridge wrappers + lib.rs weaving), finalize (MSD variant, bundle, `system_tests/` modules, host driver), and every template |
| `microkit/plugins/testing/TestControllerInjector.scala` | Injects the controller process/thread -- **no ports**, deliberately not `MonitorInjector` |
| `common/util/ExperimentalOptions.scala` | `ENABLE_TEST_SCHEDULER` (D5) |
| `microkit/plugins/MicrokitPlugins.scala` (jvm) | Registration, before the system description providers |
| `jvm/.../test/microkit/MicrokitTests.scala` | The two golden tests: temp-control and the contract-free vms model |

### Generated artifacts (per model, under the Microkit output directory)

| Path | Role |
|------|------|
| `test_scheduler.meta.py`, `.mk`, `scheduler/{src,include}/test_scheduler.*` | The variant bundle, selected by `make CONFIG=test_scheduler.mk` |
| `crates/test_controller/src/system_tests/{mod,api,harness,selection}.rs` | Generated: command API, runner, macros, `TESTS=` filter |
| `crates/test_controller/src/system_tests/inspect.rs` | Generated: `get_`/`put_` per port and state var, plus `channels::` for `run_to_thread` |
| `crates/test_controller/src/system_tests/tests.rs` | **User-editable**, preserved across regeneration |
| `crates/test_controller/src/system_tests/tests/*_tests.rs` | **User-created**, optional: one file per test class, pulled in by `system_test_files!` in `tests.rs` |
| `components/<thread>/src/<thread>.c` | Gains `get_inj_sv_*` + `is_injection_enabled()` in the test variant (D16); the `inj_sv_*_queue` pointers are `setvar_vaddr` targets, NULL elsewhere |
| `crates/<thread>/src/bridge/extern_c_api.rs`, `crates/<thread>/src/lib.rs` | Rust side of D16 for threads with state vars: extern declarations, `unsafe_` wrappers and `#[cfg(test)]` stubs in `extern_c_api.rs`; the `libComputePre` ingest in `lib.rs` |
| `bin/run-tests.cmd` | Host driver (D18) |

### Files this design depends on

| File | Why it matters |
|------|----------------|
| `microkit/plugins/msd/SystemDescriptionProvider_MCS.scala` | Default scheduler templates; `meta.py` rendering; `templateContributions` and `templateTailContributions` |
| `microkit/plugins/monitors/UserLandMonitorPlugin.scala` | The variant-bundle pattern; `MONITOR_ORIG_MSD_KEY`, the pre-monitor snapshot this variant reads |
| `microkit/plugins/gumbo/GumboMonitorPlugin.scala` | `sv_` state var regions and `is_monitoring_enabled`; **stage 5 builds directly on this** |
| `microkit/plugins/c/connections/CConnectionProviderPlugin.scala` | Unconnected-port region creation; `putCConnectionStore`, the route for C bridge additions. **Central to stage 5** |
| `microkit/connections/ConnectionUtil.scala` | `processInPort` / `processOutPort` -- where an unconnected input's region comes from (D15) |
| `microkit/plugins/rust/component/CRustComponentPlugin.scala` | Owns `lib.rs`; `libModDecls` / `libComputePre` / `libComputePost` are how generated code is woven in without re-emitting it. `libComputePre` was added by this work and is shared with the R2U2 hook |
| `microkit/plugins/rust/testing/CRustTestingPlugin.scala` | The component-level harness that D11/D14 mirror one level up |
| `microkit/util/MakefileTemplate.scala` | **Two** `system.mk` templates (domain and MCS) -- a change to one usually needs the other; `CHECK_FLAGS_BOARD_MD5`, the `$(MSD)` rule, `RUST_PROFILE_DIR` |
| `microkit/util/MakefileContainer.scala` | The per-crate link rule, shared by both templates |
| `microkit/util/SystemDescription.scala` | PD / MemoryRegion / Channel model; `templateContributions`, `templateTailContributions` |
| `microkit/plugins/c/components/CComponentPlugin_MCS.scala` | Per-thread PD/channel/region construction; the bridge `.c` carrying the C2 notify-back |
| `types/include/sb_queue_*.h` | The queue contract D15 rests on: single-sender, broadcast, private per-receiver `Recv_t`, effective depth 1 |
| `art/scheduling/static/{Command,Explorer}.scala` | The JVM vocabulary and stop-before semantics this mirrors |
| `doc/GumboMonitorPlugin-design.md` | State var region design; its "Bidirectional Use Cases" section was corrected by this work |

## Implementation Status

**Stages 1-5 are built and running on seL4.** Stage 7 (contract observation in the
controller) is designed and not yet built. Stage 6 is deferred indefinitely (see
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
| 7 contract observation | designed, not built |

New files: `microkit/plugins/testing/TestSchedulerPlugin.scala` and `TestControllerInjector.scala`;
`ExperimentalOptions.ENABLE_TEST_SCHEDULER`; registration in `MicrokitPlugins`; two golden tests in
`MicrokitTests.scala` (temp-control and the contract-free vms model).

Observed end to end on temp-control under QEMU:

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
real dispatch has adopted it. They live outside the codegen repo because `tests.rs` is seeded
once per model and the results tree is regenerated by `MicrokitTests`.

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
- **The `TESTS=` filter selects correctly and forces a rebuild** on every change: unset -> 2
  matched, `stepping` -> 1, `smoke::` -> 2, `nosuchtest` -> 0.
- **D14's completeness guarantee is real, not documentary.** Deleting one field from a
  `set_tcp_tct` call fails the build with `E0063: missing field \`sv_fanError\``. That is the
  whole value of the container: adding a state variable to the model breaks stale tests at
  compile time instead of silently defaulting the field and producing a green run against a
  pre-state nobody meant to set up.
- **D16 round-trips through the thread.** A value set on `tcp_tct.latestTemp` is adopted at
  the start of the next dispatch and comes back on the `sv_` region; a following dispatch with
  nothing set keeps it rather than reverting. Draining the injection queue is what makes
  "nothing set" mean "keep your own state" — no dirty flag was needed.
- **D18's three rules all fire.** Success gives `OK` / exit 0; a zero-match filter and a
  deliberately hung test both fail with a named diagnostic. Only the count-reconciliation rule
  is still unexercised, since it needs serial output to be lost mid-run.

### What building and running it corrected

Findings that no amount of review produced, recorded so they are not rediscovered:

| Finding | Consequence |
|---------|-------------|
| `MicrokitUtil.KiBytesToHex` rounds **up** to the 4 KiB alignment | Regions spaced 1 KiB apart collapsed onto one address; `test_status` and `test_schedule` aliased. Region vaddrs must be 4-KiB-aligned. |
| The stage-2 sentinel `#define TEST_CONTROLLER_CH 63` was never replaced | `#if TEST_CONTROLLER_CH < MICROKIT_MAX_CHANNELS` silently compiled out both the kick and the command arm. The real id is threaded from the MSD (an `_MON` PD's scheduling domain id *is* its channel id) and the `#if` was **deleted** — a guard that silently removes code is a worse failure mode than a link error. |
| The all-ready branch still armed the default's settling timer | The schedule free-ran and the controller was never dispatched. It now sets `scheduler_running` and notifies the controller; `init()` parks at `TEST_CMD_NONE` rather than `RUN_FOREVER`. |
| The controller signals its own `_MON` from `init()` | That reaches the scheduler on the controller channel and is **not** a command; the arm ignores signals before the schedule is live. |
| **QEMU's serial console emits CRLF** | Every line carries a trailing `\r`, so `Z("0\r")` is `None` and the driver's parse threw a Java trace *after* printing a green-looking log. Output is normalized before parsing, and the numeric parses use `getOrElse(-1)` so a format change reads as a clean `FAILED`. |
| **QEMU never exits on its own** | Waiting for the timeout made every successful run cost the full bound. Piping into `sed '/DONE/q'` does not help: once the suite ends the guest is idle, so no further write raises SIGPIPE. The driver polls the log and kills the process group; successful runs went from ~300 s to ~13 s. |
| The D16 ingest declared `get_inj_sv_*` / `is_injection_enabled` in a raw `extern "C"` block in `lib.rs` | The seL4 image linked, but the host `make test` (per-crate `cargo test`) has no C bridge, so every thread with state vars failed with `undefined symbol: is_injection_enabled` -- caught by the isolette CI job, not by the golden tests or the on-target runs. Now routed through `extern_c_api.rs` with `#[cfg(test)]` stubs, as `is_monitoring_enabled` always was. Any new C symbol a component crate calls must take that route. |
| Synthetic **model ports** reach the component test harness and the verification model | The first D16 implementation added `inj_sv_X` input ports; they surfaced as `api_inj_sv_*` fields in `PreStateContainer_wGSV` and as `// channel` fields in `sys_<id>_proof/system_state.rs`. Reverted: injection regions are plugin-declared, not model ports (D16a). |
| `MakefileTemplate` has **two** `system.mk` templates | `RUST_PROFILE_DIR` was added only to the MCS one while the link rules come from the shared `MakefileContainer`, leaving domain-scheduled models referencing an undefined variable. Any change touching `system.mk` must be applied to `systemMakefileDomainScheduler` *and* `systemMakefileMCS`. |

## Related Defects Found Along The Way

Neither was caused by this design; both were hit while building it.

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

### Open: `1 << ch` is undefined behavior for `ch >= 31`

Both the default and monitor schedulers build their readiness bitmask with

```c
part_ready       |= (1 << ch);                                 // part_ready is uint64_t
part_ready_check |= (1 << user_schedule.timeslice_ch[i]);
```

`1` is an `int`, so `1 << ch` is undefined behavior once `ch >= 31`. `MICROKIT_MAX_CHANNELS`
is 62, so channel ids reach 61. These should be `1ULL << ch`. The bug is latent in today's
models — temp-control uses three channels — but the test variant adds channels, and any
system past roughly thirty would hit it. The test scheduler's own copy already uses
`1ULL << ch`; the default and monitor templates have not been touched.

## Open Items

Resolved by building: the watchdog bound (10x the slot's configured budget, floor 1 s), stage 3's
wall-clock cost (stage 4 removed it), and the synthetic-element stripping interaction (clean --
the monitor bundles came out byte-identical).

### Left open by stage 5

- Whether injection goes through generated setters only, or also exposes raw region writes for
  negative testing.
- `tests.rs` is seeded once and then owned by the developer, but `MicrokitTests` regenerates
  the whole results tree, so the golden baseline can only ever hold the seed. On-target
  coverage beyond the seed lives with the models instead -- isolette's ported suite in
  INSPECTA-models, preserved by its `clean.cmd` and run by its CI.

### Only if stage 6 is ever revived

- **UART contention**, and ahead of it the question of whether an interactive CLI is worth
  adding serial PDs to the schedule under test. Both are written up under "Interactive CLI
  (deferred)". Nothing in stages 1-5 depends on either.

### Not blocking anything

- Slash scripts exit **252** rather than 1 on failure -- the host driver, and equally the
  models' `.ci/ci.cmd` (isolette's CI reports `FAILED (exit 252)` for a `make test` failure).
  `Os.exit(1)` is remapped somewhere in the Slash launcher, the same class of thing as the `23`
  already noted in `run-hamr.cmd`. Nonzero is correct for `if ! ./run-tests.cmd`, but an exact
  code needs chasing.
- D18's count-reconciliation rule is still unexercised: it needs serial output to be lost
  mid-run, which cannot be staged cheaply. The other two rules are verified on target.
- Whether a predefined command script loaded at init from a memory region is worth having
  alongside the compiled-in Rust script -- it would let one image run several test sequences.
- How `is_user_partition` should be reported for the controller, which holds no slot.
- Whether the test scheduler should optionally retain real timer budgets for mixed
  real-time/stepped scenarios, or whether that is better served by the default variant.
- Whether a variant that keeps *both* the controller and a monitor PD is worth having. Stage 7
  checks contracts against the unmonitored configuration; testing the monitored one -- does
  the system still meet its contracts with the monitor's slots in the frame, and is the monitor
  itself right? -- is a different and legitimate question that the one-injected-PD-per-variant
  rule currently forecloses. Stage 7's steps are prerequisites; the remaining work is listed at
  the end of "Contract observation in the controller".
