# Test Scheduler Design: Command-Driven Scheduling for Microkit System Testing

**Status:** stages 1-5 and 7 are built and run on seL4; stage 6, an interactive CLI, is deferred
("Interactive CLI (deferred)").  This document is the design as it stands.  How it was built --
the stages and what verified each, what building and running it corrected, the as-built records
of stage 7's steps and what its twelve review rounds changed -- is in
[TestScheduler-history.md](TestScheduler-history.md).

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

### Terms

- **MSD** -- the Microkit system description, rendered as `meta.py`; a *variant* is a further
  system description (e.g. `test_scheduler.meta.py`) selected with `make CONFIG=<variant>.mk`.
  `normal` is the default variant: the image that ships, built by plain `make`.
- **`_MON`** -- the wrapper protection domain HAMR generates for each thread; the scheduler
  dispatches a thread by notifying its `_MON`, and a `_MON`'s scheduling domain id is its
  channel id.
- **MCS** -- seL4's mixed-criticality scheduling configuration, which the Microkit builds here
  use; the test scheduler exists only for MCS models.
- **sDDF** -- seL4's device driver framework; its serial and timer drivers are part of every
  generated image.
- **GUMBO** -- HAMR's contract language for AADL/SysML components (assume-guarantee contracts,
  state variables, compositions).
- **R2U2** -- a runtime-verification engine; HAMR's R2U2 monitors are a separate kind of
  runtime monitor, mentioned where they share the build.
- **IEP_Post, CEP_Pre, CEP_Post** -- a thread's GUMBO contracts as checkable predicates: the
  initialize entry point's guarantee, and the compute entry point's assumption and guarantee.
  `I_Assm` / `I_Guar` are the integration constraints on a port, folded into them.
- **GUMBOX** -- the executable (non-Verus) form of those predicates that HAMR generates.
- **Frame** -- one pass through the schedule, i.e. one hyperperiod.  For the system layer each
  composition's frame ends when its marking reaches END (see "Frames and event values"), which
  need not be at a hyperperiod boundary; where the difference matters (resume points, D30) this
  document says "hyperperiod".
- **Frame period** -- the length of one hyperperiod in the schedule; slots shorter in total
  are padded out to it.
- **User slot** -- a schedule slot whose `is_user_partition` bit is set: one dispatching a
  model thread.  A *non-user slot* dispatches nothing the tests see -- a pad, or in the monitor
  variants a monitor's slot.
- **Composition, place, marking, cascade, START / END** -- a GUMBO `composition` describes the
  schedule as a workflow net: *places* between the threads, where system assertions are stated
  ("after dst1"); the *marking* is the set of places the frame has reached; firing a thread's
  or a control point's transition moves it, and the *cascade* fires control points until none
  is enabled; START and END bound a frame.  A *control point* is a schema element that is not
  a thread -- a `split`, a join, a `label` -- whose transition fires on its own.  A marking is a
  64-bit mask, so a composition has at most 64 places.
- **Stage A / Stage B** -- the two dispatch models (see "Dispatch models"): timer-gated, used
  in stages 2-3, and completion-driven, which replaced it in stage 4.
- **temp-control, isolette, vms** -- the INSPECTA-models (the INSPECTA project's example models)
  examples used here: the stage 2 pilot,
  the model whose JVM system tests were ported, and the contract-free model.

## Platform Constraints

Three properties of the generated Microkit system shape the design. Each of these
corrects an assumption made in the first draft of this document.

### C1. A passive PD cannot run while the scheduler is inside `notified()`

Microkit 2.3.1 manual, "Protection Domains" > "Scheduling":

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
  `else` arm is a no-op.  (The monitor variants' scheduler counts it, but does not advance on
  it either: see "The monitor PDs are thin wrappers".)

Because it is inert in the default variant, the notify-back can be emitted unconditionally
rather than gated per variant — which matters, since component C sources are shared across
variants: only the MSD, `scheduler.c`, the config headers and the `.mk` are variant-specific.
(What a component's sources contain still depends on codegen options -- with the test scheduler
enabled, a Rust thread with state variables gains the D16 ingest in every variant, gated at
run time on its regions being mapped.)

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
        │  reads port + sv_ regions; writes producers' port, inj_ regions     │ <t>_MON  │
        └────────────────────────────────────────────────────────────────────▶│ ──▶ <t>  │
                                                                              └──────────┘
```

The test controller is a **passive PD with no timeslice of its own**: a thread like the
model's, so it has a `_MON` wrapper (priority 101) that is the scheduler's child, and the
controller itself (priority 100) is the `_MON`'s child -- the two lowest priorities in the
system. It is driven entirely by the scheduler's completion notification, and runs while
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
- **`microkit_ppcall` to the scheduler is legal but useless.** The manual ("Channels" >
  "Protected procedures"): "a protected call is only possible if the callee has strictly
  higher priority than the caller", and the
  scheduler is 200. But the scheduler cannot complete a multi-slot command inside
  `protected()` — dispatching requires returning to the event loop (C1) — so it would have to
  reply before the work was done.
- **Restructuring test scripts as state machines** works and throws away the straight-line
  script model that D11, D13 and D14 are built on.

The mechanism that does work: **the controller is the lowest-priority PD in the system** (below
the component PDs' 140) **and busy-waits on `ack_seq`**. Every other PD then preempts it
(manual, "Channels" > "Notifications": "if the notified PD has a higher priority than the
current PD, then the current PD will be preempted"), the schedule advances, and the spin is
simply "idle while everyone else works". Wasting cycles is irrelevant in a test image.

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

**The spin costs nothing in scheduling-context terms.** The manual ("System Description
File" > `protection_domain`): `budget` defaults to 1,000us and `period` "defaults to the
budget"; and ("Protection Domains" > "Scheduling") a budget equal to its period is a "full"
budget, never throttled — it is replenished every period and merely rotates round-robin among
PDs at the same priority. `meta.py` passes neither attribute, and sdfgen omits them from the
XML when they are `None`, so Microkit's defaults apply; a passive PD's SC is its own, rebound
to the notification object (same section, "Passive"), and keeps that full budget. There is
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
`init()` (via its `_MON`), which is not a command. As the log shows, that signal arrives after
the hand-over, when `scheduler_running` is already set, so the `TEST_CONTROLLER_CH` arm's
`scheduler_running` gate is not what discards it: `try_advance` finds no new command
(`test_cmd->seq == accepted_seq`) and does nothing.  The gate covers a signal before the
hand-over.  `init()` parks at `TEST_CMD_NONE` rather than `RUN_FOREVER` so nothing dispatches
until the controller actually asks.

### Scheduler state machine

The scheduler is always positioned *at a slot that has not yet run*. This matches the JVM
Explorer's stop-before semantics — `InfoInputs` is documented as "values of input ports of
component to be executed in the next slot" — so after a command completes, the controller
can inspect the inputs of the component that is about to run.

As built (abridged; the generated source is `scheduler/src/test_scheduler.scheduler.c`, and the
host harness `models/SystemTesting/host/scheduler_harness.c` drives it through the cases QEMU
cannot time):

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
        ended_unreachable = true;                // still owes its acknowledgement
    }
}

static void try_advance(void) {
    bool accepted = poll_command();
    while (true) {                               // pads are skipped iteratively (4 KB stack)
        if (active_cmd == TEST_CMD_NONE || at_stop_point()) {
            // Acknowledge only a command completing now -- one just accepted, one that was
            // running, or one that just ended UNREACHABLE.  Answering a signal that carried
            // no command would make the scheduler and the controller signal each other forever.
            bool completing = accepted || active_cmd != TEST_CMD_NONE || ended_unreachable;
            active_cmd = TEST_CMD_NONE;
            ended_unreachable = false;
            if (completing) {
                // stopping on the slot whose dispatch overran: its thread may still be running
                if (overrun_pending && user_schedule.timeslice_ch[current_timeslice] == overrun_ch)
                    status_flags |= TEST_FLAG_OVERRUN;
                publish_status(); microkit_notify(TEST_CONTROLLER_CH);
            }
            return;                              // parked: nothing dispatched, no watchdog
        }
        microkit_channel ch = user_schedule.timeslice_ch[current_timeslice];
        if (ch == 0) { advance_position(); continue; }   // padding: counted, not dispatched

        if (overrun_pending && ch == overrun_ch) {
            // still running the dispatch that overran: report the overrun again and stop
            status_flags |= TEST_FLAG_OVERRUN;
            active_cmd = TEST_CMD_NONE;
            publish_status(); microkit_notify(TEST_CONTROLLER_CH);
            return;
        }
        // Observation park (stage 7): before the dispatch and before the watchdog is armed.
        if (cmd_observe && user_schedule.is_user_partition[current_timeslice]) {
            if (!obs_pending) {
                obs_pending = true; obs_seq++;
                publish_position();              // position, last_dispatched_ch, next_ch, completed_seq
                test_status->obs_seq = obs_seq;  // after a release fence, stored last
                microkit_notify(TEST_CONTROLLER_CH);
                return;                          // parked until the controller acknowledges
            }
            if (test_cmd->obs_ack != obs_seq) return;   // still parked
            obs_pending = false;                 // after an acquire fence
            if (test_cmd->obs_skip == obs_seq) { // rejected (D36): the slot passes undispatched
                advance_position();
                continue;
            }
        }
        last_dispatched_ch = ch;
        armed = true;
        uint64_t bound = max(timeslices[current_timeslice] * TEST_WATCHDOG_FACTOR, TEST_WATCHDOG_MIN_NS);
        armed_deadline = sddf_timer_time_now(config.driver_id) + bound;
        sddf_timer_set_timeout(config.driver_id, bound);   // watchdog only
        microkit_notify(ch);                     // returns immediately (see C1)
        return;
    }
}

static void on_slot_complete(void) {
    armed = false;
    if (user_schedule.is_user_partition[current_timeslice]) completed_seq++;  // slots that park
    advance_position();
    try_advance();
}

void notified(microkit_channel ch) {
    if (ch == config.driver_id) {
        if (armed && sddf_timer_time_now(config.driver_id) >= armed_deadline) {
            armed = false;                       // the slot in flight never reported back
            overrun_pending = true; overrun_ch = last_dispatched_ch;
            status_flags |= TEST_FLAG_OVERRUN; active_cmd = TEST_CMD_NONE;
            publish_status(); microkit_notify(TEST_CONTROLLER_CH);
        }                                        // else a stale expiry (see below)
    } else if (ch == TEST_CONTROLLER_CH) {
        // A command, or a park's acknowledgement.  Ignored before the schedule is live and
        // while a slot is in flight; a signal carrying no new command (the controller's own
        // init() signals this channel) is dropped by try_advance: seq == accepted_seq.
        if (scheduler_running && !armed) try_advance();
    } else if ((part_ready_check & (1ULL << ch)) != 0) {
        if ((part_ready & (1ULL << ch)) == 0) {
            /* readiness handshake; on the last one, scheduler_running = true and
               microkit_notify(TEST_CONTROLLER_CH) hands over to the controller */
        } else if (overrun_pending && ch == overrun_ch) {
            overrun_pending = false;             // the late completion: absorbed, not counted
        } else if (scheduler_running && armed && ch == last_dispatched_ch) {
            on_slot_complete();                  // this is what paces the schedule
        }
    }
}
```

Padding slots (`timeslice_ch == 0`) are never dispatched, but are still *counted*, so slot
indices stay aligned with the published `test_schedule`. **Whether a pad slot consumes its
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
dispatch forever. Those cases are rejected at decode with `TEST_FLAG_BAD_COMMAND` -- as are
`RunToThread(0)` (padding is never dispatched) and a slot index past the schedule.  Every
`RunTo*` also carries a slot budget, set at decode and charged in `advance_position`: one
hyperperiod plus one slot for `RunToThread` / `RunToSlot`, and enough hyperperiods to reach the
target (plus one slot) for `RunToHP` / `RunToState`, computed in 64 bits and saturated.  When
the budget runs out without a match, the command completes with `TEST_FLAG_UNREACHABLE`.  With
decode validation in place the budget is defence in depth: no valid command reaches it.
`RunToHP(n)` stops at slot 0 of hyperperiod `n`, before its first dispatch; hyperperiods count
from 0.  **Already at the target:** `RunToSlot`, `RunToThread` and `RunToState` whose target is
the current position (the slot about to be dispatched) complete at once without dispatching --
the runner's `run_to_slot(0)` at slot 0 is this case -- whereas `RunToHP(n)` with `n` the current
hyperperiod is rejected even when parked at its slot 0: it names a hyperperiod to reach, and
this one has been reached.

**Stale watchdog expiries must be filtered.** `sddf_timer_set_timeout` is one-shot and cannot
be cancelled.  Arming the next slot replaces the timeout (the sDDF driver keeps one per
client), but an expiry already signalled for the previous slot is still delivered -- possibly
after the next slot was armed, when a slot completes right at its bound.  The scheduler records
the slot's deadline when it arms the watchdog, and trips only when a slot is armed and the
clock has reached that deadline; any other expiry is stale and dropped.  (A first version used a
slot generation counter, which could not tell a stale expiry from the next slot's.)

**A controller notification while a slot is armed is dropped**, not deferred (the
`TEST_CONTROLLER_CH` arm's `!armed` test), and that loses nothing: the slot's completion runs
`try_advance`, which polls `test_cmd` before anything else, so a command written while the slot
was in flight is picked up there.  Under D4 the case does not even arise: the lowest-priority
controller cannot run while a dispatched thread does, and parks happen only between slots.

### Dispatch models

**Stage A — timer-gated (superseded; stages 2-3).** Kept `sddf_timer_set_timeout` exactly
as the default scheduler uses it, pad slots included, with the timer tick as the slot-complete
event. Required no change to any component, which is what made stage 2 verifiable on its own.
Cost: stepping ran in wall-clock time — temp-control's frame is
`[pad 850ms, tsp_tst 50ms, tcp_tct 50ms, fp_ft 50ms]`, so `Hstep(2)` took two real seconds.

**Stage B — completion-driven (current).** Uses the C2 notify-back as the slot-complete event
and drops the per-slot timeout. Stepping then runs as fast as the threads do, and slot
boundaries are deterministic rather than timing-dependent. The timer is **retained as a
watchdog**: a slot that does not report completion within its bound (ten times its budget, at
least 1 s) sets `TEST_FLAG_OVERRUN` in `test_status` and aborts the command. The controller
fails the running test on it (`harness::command_outcome`), so a thread that overruns fails its
test instead of wedging the run. The slot is not advanced.  Until the late completion arrives,
every command that reaches that slot -- or stops on it, as the runner's `run_to_slot(0)` (or a
test's `info_state`) does when slot 0 overran -- reports `OVERRUN` again without dispatching
anything -- so nothing else runs, or overruns, in between -- and the late completion, when it
comes, is absorbed: it completes nothing and is not counted.  The next command then dispatches
that slot again.  A thread that never returns at all still wedges the run: it starves the
lowest-priority controller, and only the host driver's timeout ends it, as a run without `DONE`.
(A faulted thread stays suspended: the runner cannot bring the schedule back to slot 0 with no
overrun outstanding, so every later test fails, unrun, with "the test could not start at the
frame's start".)

### Command protocol

Three 4 KB memory regions, continuing the vaddr block already used by the monitor variants
(`sched_state` at `0x4_000_000`, `sched_schedule` at `0x4_001_000`):

| Region          | vaddr         | scheduler | controller |
|-----------------|---------------|-----------|------------|
| `test_cmd`      | `0x4_002_000` | `r`       | `rw`       |
| `test_status`   | `0x4_003_000` | `rw`      | `r`        |
| `test_schedule` | `0x4_004_000` | `rw`      | `r`        |

```c
typedef struct test_command {
    uint32_t seq;          // incremented by the controller on every command
    uint32_t type;         // TEST_CMD_*
    uint32_t count;        // Sstep / Hstep
    uint32_t target_ch;    // RunToThread
    uint32_t target_hp;    // RunToHP / RunToState
    uint32_t target_slot;  // RunToSlot / RunToState
    uint32_t observe;      // stage 7: park before every user dispatch
    uint32_t obs_ack;      // stage 7: echoes obs_seq once a park has been checked
    uint32_t obs_skip;     // stage 7: equal to obs_ack when the parked dispatch is rejected (D36);
                           //   stored before obs_ack
} test_command_t;

typedef struct test_status {
    uint32_t ack_seq;             // echoes test_command.seq once the command completes
    uint32_t current_timeslice;
    uint32_t hyperperiod_num;
    uint32_t last_dispatched_ch;
    uint32_t flags;               // STOPPED / BAD_COMMAND / UNREACHABLE / OVERRUN
    uint32_t next_ch;             // stage 7: the channel of the slot at current_timeslice
    uint32_t completed_seq;       // stage 7: user-slot completions so far
    uint32_t obs_seq;             // stage 7: incremented at each observation park
} test_status_t;

typedef struct test_schedule {   // published once at init
    uint32_t num_timeslices;
    uint32_t timeslice_ch[MAX_SCHEDULE_SLOTS];
    uint64_t timeslices[MAX_SCHEDULE_SLOTS];
    bool     is_user_partition[MAX_SCHEDULE_SLOTS];
} test_schedule_t;
```

`BAD_COMMAND` covers an unknown command type and a command rejected at decode (a `RunToSlot`
past the end of the schedule, a `RunToThread` channel not in it or 0, a `RunToHP` or `RunToState`
target already passed). `STOPPED` is sticky; the others describe the last command only. The
stage 7 fields are described under "The scheduler's observation park".

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
| `Hstep(n)`          | Finish the hyperperiod in progress, then `n - 1` more (at slot 0 the one in progress is a whole one) | `Hstep(n)` |
| `RunToSlot(n)`      | Run until slot index `n` is next                   | `RunToSlot(n)`          |
| `RunToHP(n)`        | Run until hyperperiod `n`                          | `RunToHP(n)`            |
| `RunToState(hp, s)` | Run until `(hyperperiod, slot)`                    | `RunToState(hp, slot)`  |
| `RunToThread(ch)`   | Run until the given thread's slot is next          | `RunToThread(name)`     |
| `InfoState`         | Publish current `(hyperperiod, slot)`              | `Infostate`             |
| `InfoSchedule`      | Publish the full schedule                          | `Infoschedule`          |
| `Stop`              | End the test session (see below)                   | `Stop`                  |
| `RunForever`        | Dispatch until the next command                    | --                      |

`RunForever` has no JVM counterpart and no wrapper in the generated `api.rs`; it was stage 2's
start-up mode, and is kept for the interactive CLI (stage 6, deferred).  `InfoSchedule` has no
wrapper either: `api::schedule()` reads the published schedule directly (see below).

`RunToThread` takes a channel id rather than a thread name; tests name it with the generated
per-thread constants `inspect::channels::<thread>_MON` in the controller's `inspect.rs`.

**The schedule needs its own region.** `sched_state` and `sched_schedule` were moved out of
the default MCS template into the monitor plugin's `templateContributions`; the default
`scheduler_c` contains no publishing code for them, and their HAMR queue types exist only
under `--runtime-monitoring`. The test variant therefore contributes a plain `test_schedule`
region and publishes the schedule into it at init. The controller needs it to correlate slot
indices with channels, which both `RunToSlot` and the runner's between-test position
normalization (D13) depend on, and `api::schedule()` reads it directly.

`InfoState` and `InfoSchedule` are otherwise **no-ops**: `try_advance` calls
`publish_status()` on every command completion, so the controller already holds current
status after any command, and `test_schedule` is published once at init. Both are retained
for parity with the JVM vocabulary and for the interactive CLI (stage 6, deferred), where a
human does want to ask.

The JVM's `InfoInputs` / `InfoOutputs` / `InfoComponentState` are **not** scheduler
commands here — they are reads the controller performs directly against the port and
state var memory regions (see below), which is strictly more capable.

**`Stop` semantics.** The runner issues `Stop` at the end of every run, so this is the normal
path, not a corner case. On `Stop` the scheduler sets a `stopped` bit in `test_status.flags`,
acknowledges as usual, and then idles permanently: it dispatches nothing further and ignores
subsequent commands other than by re-acknowledging with `stopped` still set. It is
deliberately **not** resumable — a resumable `Stop` would mean the schedule can restart after
the controller has published its verdict, which makes the verdict meaningless. An
interactive CLI (stage 6, deferred), where a human may well want to continue, would need a
distinct `Pause`; nothing built so far does.

### Test controller PD (Rust)

The controller is injected the same way monitor PDs are, so it inherits the whole
`CRustComponentPlugin` pipeline: crate generation, generated port APIs, `extern_c_api.rs`
and the test harness.  Since stage 7 it also runs the model's generated contract checks
(through the `observers` crate), at every dispatch while a layer is live ("Enabling and
disabling checks").

Generated (non-editable) half: a typed command API over `test_cmd`/`test_status`, in
`system_tests/api.rs`:

```rust
pub fn sstep(n: u32) -> TestStatus;       pub fn hstep(n: u32) -> TestStatus;
pub fn run_to_thread(ch: u32) -> TestStatus;   // ch: inspect::channels::tcp_tct_MON
pub fn run_to_slot(slot: u32) -> TestStatus;   pub fn run_to_hp(hp: u32) -> TestStatus;
pub fn run_to_state(hp: u32, slot: u32) -> TestStatus;
pub fn info_state() -> TestStatus;        pub fn stop() -> TestStatus;
pub fn schedule() -> &'static TestSchedule;
```

Each call writes the command, notifies the scheduler, and returns when the matching `ack_seq` is
observed -- servicing the stage 7 observation parks on the way (see "The scheduler's observation
park"). A command that ends with a flag (`OVERRUN`, `UNREACHABLE`, `BAD_COMMAND`, or `STOPPED`)
fails the running test (`harness::command_outcome`), so tests write `let _ = api::hstep(1);` and
need not check the status.  `stop` itself never fails (`command_outcome` returns at once for
it): it dispatches nothing, so nothing about it can fail -- not even with an overrun
outstanding, which was already charged to the command that met it.  But `STOPPED` is sticky, so
a test that calls `api::stop()` fails at its next command, and every later test fails without
running its body (the runner checks `STOPPED` first, so even a test that issues no command
cannot pass against a frozen system) -- the session cannot run anything after `Stop`.  `sstep`
and `run_to_slot` count pad slots, so a script that counts slots is not portable across
`--runtime-monitoring`; `run_to_thread` and `hstep` are (see "Known limits").  User-editable
half: the test script itself — the Microkit analogue of JVM test code that calls
`Explorer.stepSystemNHPIMP(2)` and then inspects bridges.

### State inspection and mutation

Inspection and injection are **not** symmetric. Inspection reuses existing infrastructure
almost entirely; injection needed new machinery for state vars (stage 5b, D16).

#### Inspection

Map each thread's port regions and `sv_` state var regions into the controller -- `rw`, since
the same maps serve injection, which regions a test treats as inputs not being knowable at
codegen.  Because the schedule is paused at a slot boundary when the controller runs (D4), the
values are quiescent — no tearing, no barriers beyond the command handshake.

*Where the regions sit.*  Each owning protection domain maps its port regions from
`0x10_000_000`, as it does without the test scheduler.  The controller's own views are in
separate blocks, so the two never share an address space layout:

| Block | Controller vaddr | Thread vaddr |
|-------|------------------|--------------|
| `test_cmd`, `test_status`, `test_schedule` | `0x4_002_000`-`0x4_004_000`, 4 KiB each | -- (the scheduler maps them at the same addresses) |
| observable regions (ports, `sv_`) | from `0x20_000_000` | the owner's own map, from `0x10_000_000` |
| `inj_<thread>_sv_<var>` (D16) | from `0x20_400_000` | from `0x20_800_000` |

Within a block regions are packed by their real size, each followed by a guard page
(SharedMemorySafety-design.md), and codegen errors if the observable or the injection block
outgrows the 4 MiB before the next one starts.

**Inspection does not disturb the system**, which is not obvious and worth stating. The queue
is broadcast ("Every receiver receives the sent data"), and each receiver owns a private
`sb_queue_*_Recv_t` holding its own `numRecv`; the shared region carries only `numSent` and
`elt[]`. The controller becomes an additional receiver with its own cursor, so its reads
cannot consume data the real consumer has not seen yet.

**But a read consumes from the controller's own cursor.** `inspect::get_<port>` dequeues:
reading an event port twice, with no dispatch in between, returns `None` the second time
(`None` means "nothing queued for this reader").  So does a data port or a state variable: the
accessor dequeues whatever the region holds.  Only the contract checks keep a last-value cache.
They read through cursors of their own (D25), so a test's reads and the checks never take
values from each other.

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

Unconnected inputs are in fact the cleanest target in the system. Outside the test variant such
a region has a reader and no writer, so the thread always dequeues empty. Once the controller
maps it `rw` the controller is the *sole* sender, which means neither the single-sender caveat
above nor the ordering caveat below applies: there is no producer to break the contract with or
to race, so the `run_to_thread` / inject / `sstep(1)` ordering is not forced for these ports.
Both constraints are specific to ports that have a real producer.

D16's injection regions are the same shape one step removed: `sv_X` is an unconnected *output*
(thread writes, observer reads) and `inj_<thread>_sv_<var>` is its mirror — thread reads,
controller writes.  (The region is named `inj_<thread>_sv_<var>`; below it is written `inj_sv_X`
for short, and the thread-side getter is `get_inj_sv_<var>`.) It is not a port, though (D16a),
so it does not ride this path; the plugin declares the region itself and maps it in both
directions.

*Ordering is required, not merely advised.* The queue is declared `SIZE 2`, and the header
notes that one cell is always dirty, so it holds exactly **one** element (a depth-1 queue: the
default, and the limit for any thread with GUMBO contracts, Rust or C, and for any event port a
composition reads; only other ports may declare deeper ones -- see "Event ports"). Where a port has a
real producer in the schedule, that producer enqueues during its own slot and displaces an
injected value. The sequence `run_to_thread(consumer)` -- the producer's earliest consumer (see
"Frames and event values") -- inject, `sstep(1)` is therefore
mandatory, not a stylistic preference — and it is what the scheduler's stop-before semantics
exist to enable.

#### State var injection: mirrored regions, NOT model ports

**Before stage 5b nothing existed for this.** `lib.rs` emitted only `put_sv_*`, after
`_initialize` and `_timeTriggered`, and the synthetic `sv_X` ports are *outputs* — the flow
was thread→observer only, with no `get_sv_*` anywhere. (An earlier draft of this document
claimed the thread-side get-before/put-after pattern was already in place. It was not; only
the put half was implemented.)

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
would enter CEP_Pre/CEP_Post, and the system proof would reason about ports that exist only
for testing. The default variant's `meta.py` also gained regions it never uses.

"No new machinery" was true, but it bought that by putting the ports in the model — and the
model is also the input to the component test harness and to system verification. That
tradeoff was not weighed when D16 was written.

So the `inj_sv_X` regions are created the way `test_cmd` / `test_status` / `test_schedule`
already are: **template-managed regions declared by the plugin**, mapped into the owning
thread and into the controller, with generated C accessors on both sides. The cost is the
region creation and the thread-side getter, which the plugin already does for the controller
and can do again for the thread. The gain is that nothing outside this plugin can see them.

(The proof and harness half of that leak is now also closed for synthetic ports in general: the
system-VC generator works on `StoreUtil.modelSymbolTable`, the model without synthetic
components and ports, and the component `test_apis` skip synthetic ports -- which is how the
`sv_` mirror ports, which are AADL ports, stay out of both.  D16a still stands: the injection
regions are not needed as ports at all, and as ports they would still add regions to the
default `meta.py`.)

##### Thread side

`lib.rs` gains an ingest block in the `libComputePre` slot.  The position already existed but
was hardcoded for R2U2; this work made it a `CRustComponentPlugin.ComponentContributions`
field any plugin can fill.  Only Rust threads ingest: a C thread with state variables gets a
codegen warning that a test cannot set them, and no injection plumbing.

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
running it corrected" in TestScheduler-history.md.

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

This sequencing is what D13 and D14 rest on: `run_to_thread(T)` (for a port with a producer, T
its earliest consumer; see "Frames and event values"), then enqueue the injected
ports and state vars, then `sstep(1)`, and the thread ingests before it computes. The order
matters for ports with a real producer -- enqueueing before `run_to_thread` lets that
producer's own slot overwrite the value on the way -- and is harmless for `inj_sv_` regions and
unconnected ports, where the controller is the only sender. The
whole-component setter of D14 is then N port enqueues plus M state var enqueues behind one
call.

### Reporting the verdict (D18)

#### Only serial gets a verdict off the target

A summary written into `test_status` lives in guest memory; nothing on the host can read a
Microkit memory region without a debugger or a core dump. It is an *on-target* path — useful for
an interactive CLI (stage 6, deferred) or for another PD — and gives CI nothing. Serial is the
only channel that carries a verdict off the target, and `sddf_dprintf` already works (the
scheduler uses it for `SCHEDULER | ...`).

Before stage 3 there was only `make qemu`, which is `$(QEMU) -nographic $(QEMU_ARCH_ARGS)`: it
blocks forever, never exits, and nothing scrapes it. The host driver, `bin/run-tests.cmd`, is
new with stage 3.

#### Absence of evidence must be failure

Every interesting failure mode of a bare-metal QEMU run is *silence*: the image hangs, a PD
panics, the controller never receives its all-ready kick, a `TESTS=` filter matches nothing.
A host rule of "fail if I see FAIL" passes all of them. The rule is therefore inverted: **a
run without a positive completion marker is a failure.**

#### Line format

```
TEST | LIST  nominal::fan_turns_on_when_too_hot
TEST | LIST  nominal::setpoint_is_latched
TEST | BEGIN nominal::fan_turns_on_when_too_hot
TEST | PASS  nominal::fan_turns_on_when_too_hot
TEST | BEGIN nominal::setpoint_is_latched
TEST | FAIL  src/system_tests/tests.rs:42 fan_cmd == Some(FanCmd::Off)
TEST | INFO  also failed: src/system_tests/tests.rs:43 set_point == Some(set_point(70, 80))
TEST | DONE  matched=2 passed=1 failed=1 init=ok
```

A test prints at most one `FAIL` line -- its first failure -- and any later failure of the
same test prints as `INFO  also failed: ...`; the test is the one the preceding `BEGIN` named.
The `FAIL` line says what failed:

| Failure | `FAIL` text |
|---------|-------------|
| a `sys_assert*` | `<file>:<line> <condition>` |
| a command that ended with a flag (`harness::command_outcome`) | `<command> failed: <why> (flags 0x..)` |
| an unexpected contract violation (stage 7) | `contract violation (<n> in this test)` |
| an expectation never met | `expected violation did not occur: <expectation>` |
| an expectation for a switched-off layer | `expected violation <expectation> cannot be reported: the <layer> checks are off` |
| a test the runner could not start at the frame's start (not run) | `not run: the test could not start at the frame's start: the schedule is at slot <n>[, whose thread overran and has not completed] (flags 0x..)` |
| a test after `api::stop()` (not run) | `not run: the session was stopped (api::stop) by an earlier test, so nothing runs (flags 0x..)` |
| a GUMBO switch on for a build without it | `the GUMBO checks are off for this build (GUMBO_CHECKS=off); ...` |

The `LIST` lines are the test table (D17), one per registered test, with
`(not selected)` after those the filter leaves out. Stage 7 adds `VIOLATION` and `INFO` lines
and `DONE`'s `init=` field (see "Contract observation in the controller"), and a listing run
adds `list=1`.

- **`DONE` is mandatory.** Its absence fails the run whatever preceded it. This is what catches
  the hang, the panic, and the controller that never started.
- **`matched=`** catches D17's zero-match trap: the host asserts `matched > 0`, so a mistyped
  filter fails loudly instead of passing with nothing run.
- **`passed`/`failed`** must reconcile with the `PASS`/`FAIL` lines actually seen, and add up to
  `matched` (except in a listing, `list=1`, which runs nothing); a mismatch means output was
  lost, which is also a failure.  So does a `DONE` line missing `matched=`, `passed=` or
  `failed=`, and one without `init=ok` (see "Contract observation in the controller (stage 7)").
- **The `TEST | ` prefix** makes scraping a line-anchored regex rather than prose matching, and
  keeps it distinct from the scheduler's existing `SCHEDULER | ...` output.  Both uses are
  anchored: the verdict reads only lines that *start* with `TEST | `, and the condition that
  stops QEMU looks for `DONE` only at the start of a line (`^TEST | DONE`).  Unanchored, a
  test's own `INFO` or `FAIL` text that mentions the marker would stop the run before the real
  `DONE`.  That could not pass a run -- the verdict would then find no `DONE` line -- but it
  would fail a good one.
- **`list=` must match what was asked for.** A `list=1` verdict when `--list` was not given
  fails, and so does a run without `list=1` when it was: otherwise a listing image reused for a
  run would pass having run nothing.  `LIST_TESTS` is part of the build's rebuild hash, so that
  should not happen, but the host checks rather than assumes it.  A listing must also be well
  formed: `passed=0 failed=0` with no `PASS` or `FAIL` lines, as many selected `LIST` lines as
  `matched`, and `init=ok`.

`RUN_TESTS_TARGET_CMD` runs a shell command in place of building the image and booting it
under QEMU; its output stands in for the serial console.  It exists so that
`SystemTestingTests` can drive the stop condition and the verdict with crafted output (a
message mentioning `DONE`, an unrequested listing, a malformed one) without an SDK or QEMU.

This composes with the panic handler in `lib.rs`, which logs `PANIC` and then traps (`udf`), so
the thread's `_MON` reports a fault.  A component thread that panics or faults once the schedule
runs never completes its dispatch: the watchdog fails that test, the later tests fail unrun, and
the run still ends with `DONE`.  A panic in the test controller, or a fault or panic in any
protection domain before the schedule starts (it never reports ready, so the test scheduler never
hands over), leaves no way to `DONE`; `run-tests.cmd` stops QEMU as soon as it sees one, rather
than at the timeout, and fails the run quoting the `FAULT in user PD` and `PANIC` lines.
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
that never ran. **The host driver forces `MICROKIT_CONFIG=debug`** on the `make` command line,
where nothing can override it. (It also forces `RUST_MAKE_TARGET=build-release`: the system
tests build with plain cargo, not Verus.)

The same mechanism is why D18 is cheap at stage 3. temp-control's `meta.py` instantiates **no
serial driver PD** — `serial=` is only a field of the `Board` dataclass — yet the scheduler's
`SCHEDULER | ...` output appears. Debug output goes straight out through the seL4 debug
syscall, so the verdict path needs no new protection domain.

**This is also what put stage 6 out of reach.** A serial CLI needs a real sDDF serial driver
PD owning the same PL011 the debug syscall writes to directly — two writers, one UART, and
D18's scraping is precisely what interleaved output would break. Deferred; see "Interactive
CLI (deferred)".

#### Stopping QEMU

Host-side: the driver reads stdio until `DONE`, gives QEMU one more second for the rest of that
line, and then kills it; a hard timeout is the other
exit. Guest-side termination (ARM semihosting `SYS_EXIT`) would yield real exit codes but needs
plumbing the image does not have — a later refinement, not a stage 3 prerequisite.

The timeout is set generously -- 300 s by default, overridden by `QEMU_TIMEOUT=<seconds>` --
because `DONE` is the real terminator and the timeout only bounds the silent-failure case.
Reaching it fails the run with a message naming the bound, so a suite that simply needs
longer is not mistaken for a hang. (Under stage 3's timer-gated dispatch a
hyperperiod took about a frame period, which a fixed value would not have suited; completion-
driven dispatch removed that constraint.)

#### Where the driver lives

`<microkit output>/bin/run-tests.cmd`, a Slash script generated beside the Microkit project
rather than in the model's `sysml/bin`, so it is self-contained and works for any model: build
under `CONFIG=test_scheduler.mk`, run `make qemu`, scrape, exit nonzero. `run-tests.cmd [--list]
[<filter>]` takes the `TESTS=` filter as its argument; `--list` builds with `LIST_TESTS=1` and
reports the listing. The build-level check switches (`GUMBO_CHECKS`, `SYSVERIF_CHECKS`) are read
from its environment, and so is `PROPTEST_SEED`, which it otherwise picks afresh for each run
(see "Property-based tests"). It needs `sh`, `pkill` and `kill` to stop QEMU, so it does not run
on Windows. A model's `.ci/ci.cmd` calls it among its Microkit steps, which it gates on
`MICROKIT_SDK`.

### Relationship to the runtime monitor

**The runtime monitor does not run during system testing, and does not need to.**

`TestSchedulerPlugin.finalizeMicrokit` builds the variant from the pre-monitor MSD snapshot
(`UserLandMonitorPlugin.MONITOR_ORIG_MSD_KEY`, or `normal` when no monitor ran), taken before it
strips anything from `normal` (below), so the variant keeps the controller and the `sv_` regions
whatever happens to `normal`. The snapshot still has every injected protection domain -- the
monitors' and the controller's, each with a slot -- but not the monitors' interleaved slots,
which only the monitor variants add.  The variant drops every injected PD other than the
controller (`userland_monitor`, `gumbo_monitor`, `sys_<id>_monitor` and their `_MON`s) with its
channels and slot, drops the controller's own slot (D4), and pads the frame back to the frame
period, with the pad where the shipped schedule has it -- read from the final `normal`: first
when a monitor rebuilt it, last otherwise -- so a slot index means the same slot in both.  The
test frame is therefore not extended by monitor time, and the schedule under test remains the
production schedule. The controller does the checking instead, running the same generated
contract checks at every dispatch (D19-D21; stage 7, see "Contract observation in the
controller").

**The production image must not be changed by system testing (D32).**  Every monitor plugin
rebuilds `normal` without injected protection domains, but which synthetic regions it drops
depends on the plugin: from `normal`, `handleForMonitor` strips only the regions its plugin
retains for its own variant, and `DefaultUserLandMonitorPlugin` retains none, so a `normal` it
wrote last would keep the `sv_` regions.  With no monitor, nothing was rebuilt: `normal` kept
the controller, its channels and a slot -- the default image then dispatched a controller whose
regions it does not map, which faulted -- and the `sv_` regions, mapped into their threads,
which then published their state in production (`is_monitoring_enabled()` is a NULL check on
those maps).  The test scheduler therefore strips the controller and the `sv_` regions from
`normal` itself whenever either survives there, whichever plugin wrote `normal` last -- not only
when no monitor ran. The build is kept apart too: `system.mk`'s `IMAGES` lists no controller ELF
(it ends in `$(EXTRA_IMAGES)`, which only `test_scheduler.mk` sets, to the two controller ELFs),
and `make verus` skips the controller crate, so a `tests.rs` that does not compile cannot break
the production build.  "Unchanged" means the same protection domains, schedule and regions; map
addresses and channel ids in the generated `meta.py` may still differ from a build without
system testing, since the stripped entries leave gaps.  For the same reason injected threads do
not count toward the frame budget (`CComponentPlugin_MCS`): a model that fits its frame must not
fail codegen because the test scheduler is enabled.

**`monitoring_enabled` is nonetheless required, and is not about the monitor's existence.**
`StateVarPortsPlugin` (originally part of `GumboMonitorPlugin`; see "What requesting system
testing generates") generates:

```c
bool is_monitoring_enabled(void) {
  return sv_currentSetPoint_queue_1 != NULL && sv_currentFanState_queue_1 != NULL && ...;
}
```

— a NULL check on the `sv_` queue pointers, i.e. "were the state var regions mapped into
me?". An unmapped region leaves its `setvar_vaddr` pointer NULL. Threads therefore publish
state vars whenever the variant maps the `sv_` regions, with or without a monitor PD. The
name is a misnomer in this context; the mechanism is what the test variant needs.

#### Models without GUMBO state variables

`StateVarPortsPlugin` requires `hasThreadsWithStateVars(symbolTable)`. A model with no GUMBO state
vars -- in particular one with no GUMBO contracts at all -- therefore gets no `sv_` ports, no
`sv_` regions, and no `is_monitoring_enabled()` at all; its threads receive the plain
`CRustComponentPlugin` `lib.rs`. (vms, the `data_receiver` model, is the example:
`monitoring_enabled` appears nowhere in its crates.)  A model with contracts but no state
variables still gets the contract checks of stage 7, and with `--runtime-monitoring` the GUMBO
and system-assertion monitors (D33); only the state variable rows below are vacuous for it.

The test scheduler must still work there, and mostly does:

| Capability | Without GUMBO state variables (or contracts) |
|------------|-------------------------|
| Scheduler state machine, commands, controller, runner, `sys_assert_*`, suites, `TESTS=` | unaffected |
| Port inspection and injection (D15) | unaffected — port regions exist for real AADL ports regardless of contracts |
| State var inspection and injection (D16) | vacuous — there are no state vars. Degrades to nothing rather than breaking |
| Contract checks at every dispatch (stage 7) | only for the contracts there are; none without contracts, where tests hand-write their expectations |

Three obligations follow:

1. **`--runtime-monitoring` is never required.** D5 once required it for a model with state
   vars, because only it created the `sv_` plumbing; system testing now creates that plumbing
   itself (D22).
2. **The D16 ingest is emitted per thread, not per system.** `is_injection_enabled()` and the
   `get_inj_sv_*` accessors are generated only for threads that have state vars. In a mixed
   model, emitting the ingest block for a thread without them would reference symbols that do
   not exist for it.
3. **`TestSchedulerPlugin` does not inherit from either monitor plugin.** Inheriting from
   `GumboMonitorPlugin` would drag in its gate -- `--runtime-monitoring` and thread contracts
   (state variables, before D33) -- and silently disable the test scheduler for precisely the
   contract-free models this section is about. See
   "Plugin structure".

#### The `sv_` regions must survive into the variant

The monitor variants go through `UserLandMonitorPlugin.handleForMonitor`, which, for its own
variant, strips every non-model memory region not named by `getRetainedNonModelPorts` (default
`ISZ()`; `GumboMonitorPlugin` overrides it to keep the `sv_` ports). Were the test variant built
that way without the same override, the `sv_` regions would be stripped, the pointers left NULL,
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
  it is what stage 3's host driver does (D18).

#### Mirror the existing component-test shape

HAMR already has a well-developed *component*-level story, host-run via `make test` →
per-crate `cargo test`:

| Artifact | Generated? |
|----------|-----------|
| `src/test/util/test_apis.rs` — `PreStateContainer{,_wGSV}`, `put_concrete_inputs*` | yes, overwritten |
| `src/test/util/generators.rs` — per-datatype proptest strategies | yes, overwritten |
| `src/test/util/cb_apis.rs` — GUMBO CEP_Pre / CEP_Post predicates | yes, overwritten |
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

Because the controller is Rust (D3), the model's generated contract predicates can run in it.
The component crates' `cb_apis.rs` copies are host-only (`#[cfg(test)]`), so stage 7 generates
them once more into a shared `crates/observers` crate, which the controller calls at every
dispatch while a layer is live: a system test gets *"every thread's contracts held at every
dispatch"* without hand-writing it, and a violation fails the test that caused it.  This is the
main reason D3 went the way it did.

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
selection its granularity — see D17 below.  Each suite is emitted as a module of its own
(`use super::*` inside), so two suites may each have a test of the same name; a suite must not
share its name with anything the file declares or imports.

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

Reporting does not depend on the heap. The controller has one (the property-test allocator,
below), but it is set up only by the first property test; a `TEST | ` line is formatted with
`format_args!` into a fixed 256-byte `LineBuf` and handed to a C shim that calls
`sddf_dprintf`, so a verdict can be printed whether or not the heap exists. A longer line is
truncated, not lost. (`run_property` alone allocates, to split proptest's multi-line failure
text into lines.) `sys_assert_eq!` reports the position and the stringified condition, not
the two values.

The runner itself is generated into `system_tests/harness.rs` (overwritten): walk
`SYSTEM_TESTS`, normalize schedule position, run, publish each verdict, then issue `Stop`.

#### Property-based tests

The JVM system tests draw random inputs with SlangCheck; on target
that role goes to proptest, which runs without std given `alloc`. Every controller crate gets
it: the plugin adds `proptest` (`default-features = false`, features `alloc` and `no_std`) and
`linked_list_allocator` to the controller's `[dependencies]`, and the generated `harness.rs`
carries a 256 KiB heap as the global allocator plus
`run_property(cases, make_strategy, property)`. That sets up the heap on first use, builds the
strategy, runs a seeded `TestRunner`, and returns whether every case passed. A failure is
shrunk on target to a minimal input, and its message and input are printed as `TEST | INFO`
lines, so a test writes `sys_assert!(run_property(..))`. Each case drives the real system —
inject, step, observe — exactly as a fixed-value test does.

Why generated rather than left to the developer: the controller crate is fully generated
(its `Cargo.toml` is overwritten on every regeneration), so dependencies
added by hand would be lost the next time codegen runs.

Three constraints shape `run_property`:

- It takes a strategy *builder*: building a strategy can allocate (`prop_flat_map` wraps its
  closure in an `Arc`), and the heap does not exist until the first property test starts.
- Failure text goes out line by line: proptest's messages span several lines, and the host
  driver keeps only lines with the `TEST | ` prefix.
- The seed comes from the image, since there is no OS entropy on target: `PROPTEST_SEED`
  (64 hex digits) is patched into the `.test_selection` section like `TESTS=` (D17), and an
  image built without it uses the built-in `DEFAULT_PROPTEST_SEED`, so a plain `make` explores
  the same cases every time.  `run-tests.cmd` picks a fresh seed for each run unless
  `PROPTEST_SEED` is set, and prints it; so do the property tests' own `TEST | INFO` lines (`16
  generated cases passed (PROPTEST_SEED=...)`, and after a failure `reproduce with
  PROPTEST_SEED=...`).  Passing the printed seed back reproduces the run's cases, and a new
  seed relinks the image (it is in the rebuild hash), as a new filter does.

With contract checking (stage 7), a violation during a property case fails the *test* -- the
contract checks record it against the running test like any other -- but not the case:
`run_property` decides pass or fail from the property's own result, so a violation neither
drives shrinking nor stops the run, and shrinking's re-runs may print further `VIOLATION`
lines.  A failed CEP_Pre is the exception in effect, not in mechanism: its dispatch is rejected
(D36), which changes what the system does, so the property's own result -- and with it
shrinking -- can depend on it.  Cases share the running system, as the tests of a suite do (D13): each case must set
what it depends on.

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

As built, the section is declared on the Rust side of the controller, with no C header or
`sdfgen_helper` serializer involved:

```rust
#[repr(C)]
pub struct TestSelection {
  pub filter: [u8; 256],   // NUL-terminated; empty => run everything
  pub flags: u32,          // bit 0 GUMBO_CHECKS=off, bit 1 SYSVERIF_CHECKS=off, bit 2 LIST_TESTS=1
  pub seed: [u8; 32],      // PROPTEST_SEED; all zero => DEFAULT_PROPTEST_SEED
}

#[no_mangle]
#[link_section = ".test_selection"]
pub static mut TEST_SELECTION: TestSelection = ...;
```

`meta.py` writes the 292 bytes itself: the filter, the flags word (little-endian), then the
property tests' seed.  A
filter longer than 255 bytes (UTF-8), or one that is not valid text, stops the build with an
error: truncating would change which tests run, or split a character, silently.  The runner
runs `(name, f)` when `filter.is_empty() || name.contains(filter)`; a filter that is somehow not
UTF-8 selects nothing rather than everything. (An earlier draft declared a C
`test_selection_t` serialized by `sdfgen_helper.py` from `SCHEDULER_CONFIG_HEADERS`; a Rust
static with a raw byte layout needs neither.)

**Why a string rather than a bitmask or index list.** The build side never needs to know what
tests exist — the Makefile passes `TESTS=` to `meta.py` verbatim, so there is no manifest file
to keep in sync and no proc-macro emitting build artifacts. And a string survives reordering,
where indices silently shift onto the wrong test. It also matches the idiom developers already
use for the component tests in this same repo: `cargo test fan_` alongside `make
CONFIG=test_scheduler.mk TESTS=fan_`.

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

#### Build plumbing (what the mechanism does not get for free)

**`TESTS` is in the rebuild hash.** The generated rule is

```make
$(SYSTEM_FILE): $(IMAGES) $(DTB) ${CHECK_FLAGS_BOARD_MD5}
	$(PYTHON) $(SDFGEN_HELPER) ...
	LIST_TESTS=... GUMBO_CHECKS=... SYSVERIF_CHECKS=... $(PYTHON) $(MSD) --sddf ... --tests="$$TESTS"
```

and the stamp originally hashed `CFLAGS`, `BOARD`, `MICROKIT_CONFIG`, `MICROKIT_SDK`, `MSD`,
`SCHEDULER_C` and `SCHEDULER_CONFIG_HEADERS` — **not `TESTS`**.  Then `make TESTS=nominal::`
followed by `make TESTS=fault_` leaves the stamp filename unchanged, so `$(SYSTEM_FILE)` is up
to date, `meta.py` never re-runs, `.test_selection` is never re-patched, and the image silently
runs the *previous* filter to a green result.  The stamp exists for exactly this purpose, so
`TESTS` and the switches below are added to it -- **each with its name** (`'TESTS=$(subst
',%27,$(subst %,%25,$(TESTS)))' LIST_TESTS=${LIST_TESTS} ...`): unlabelled, `GUMBO_CHECKS=off`
and `SYSVERIF_CHECKS=off` hashed alike, since an empty variable leaves no trace, and the image
kept the previous setting.

**`TESTS` is handed to `meta.py` as an argument, its value through the environment.** The rule
used to invoke `$(MSD)` with a fixed argument list with no way to pass a filter;
`MakefileTemplate.scala` adds `--tests="$$TESTS"` (the `=` form, so a filter starting with `-`
is not taken for an option). `system.mk` does `export TESTS`, so the recipe's shell expands
`"$TESTS"` once, inside double quotes, and no character of the filter is re-parsed: pasted in by
make (`--tests="$(TESTS)"`, as first built), a `"`, `$` or backtick ended the quoting or was
expanded by the shell.  In the stamp a `'` is replaced (`$(subst ',%27,...)`) so it cannot end
the single quotes that hold the value, after a `%` is (`%25`), so no two filters hash alike.
Two characters remain unsafe, one read by make and one by `echo`: a `$` in the value is expanded
by make (in the hash and in what it exports), and the stamp is `echo`ed by the shell, which
reads `\` escapes.  `run-tests.cmd` therefore rejects a filter holding either (`FAILED: a test
filter cannot contain '$' or '\'`); no test name can contain them.

The values added later -- `LIST_TESTS`, `GUMBO_CHECKS`, `SYSVERIF_CHECKS`, `PROPTEST_SEED` --
are in the hash too, but reach `meta.py` only through the MSD step's **environment**, as named
variables (`LIST_TESTS="$(LIST_TESTS)" ... $(PYTHON) $(MSD) ...`). The argument parser sits in
the part of `meta.py` that is written once and kept, so a tree regenerated in place still has
the parser it was first generated with; a new argument made that parser reject the command and
broke the build, where an unknown environment variable is simply ignored. `--tests` predates
every tree in use, so it stays an argument. Both changes are to the MCS template only: the test
scheduler exists only for MCS models.

**Plugin contributions to `meta.py` are regenerated.** The test variant contributes Python
before the main `META MARKER` region (its command, status and schedule regions, and its
state-variable injection regions) and after it (the
test-selection block); the monitor variants contribute before it too (their `sched_state` and
`sched_schedule` regions).  They depend on the model, so each sits in a marker region of its own
(`META TEMPLATE MARKER`, `META TAIL MARKER`) and is rewritten on regeneration like the main
one; before, they were written once, and adding a GUMBO state variable left the injection
region undeclared.  A variant without contributions (the default `meta.py`) gets no extra
marker and is unchanged.  An existing test or monitor `meta.py` regenerated without cleaning --
a tree built with only `--runtime-monitoring` included -- fails once with codegen's "did not
contain the following markers" error and a `_fixme` copy to merge from.

**No recompile, but not free: every PD relinks.** A change to one of these variables changes the
stamp, and every ELF depends on the stamp, so every protection domain relinks; cargo runs too,
finding nothing to do. It is still seconds, not a rebuild.

**Two binding times, one filter:**

| When | How | Cost |
|------|-----|------|
| Image build (stage 3) | `TESTS=<filter>` -> meta.py serializes -> objcopy -> repack | seconds: a relink, no recompile |
| True runtime (stage 6, serial; deferred) | `list`, `run <filter>`, `run all` overwrite the same in-memory struct | none |

A stage 6 CLI would therefore need no new selection design; it would write the struct the
runner already consults.

**Required behaviors:**

- A filter matching **zero** tests is a distinct nonzero verdict, never a pass. Reporting
  "0 tests matched" as success is how a typo'd filter silently turns a CI job green.
- The controller prints the test table at init (`TEST | LIST` lines), and honors a list-only
  flag (`LIST_TESTS=1`, or `run-tests.cmd --list`): the table and a
  `DONE matched=<n> passed=0 failed=0 ... list=1` line, with `matched` the number the filter
  selects, and nothing run. The host still requires `matched > 0`, and that the listing was
  asked for and is well formed (D18), then reports the listing and succeeds. Without it the
  host cannot discover what is runnable.

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
- The runner still normalizes **schedule position** between tests by advancing to a hyperperiod
  boundary, so `hstep(1)` means the same thing in every test. That is cheap and removes a whole
  class of order dependence without any re-initialization.  If a thread overruns during it, the
  runner repeats `run_to_slot(0)` (up to four tries in all); a test that still cannot start at
  slot 0 with no overrun outstanding fails without running its body ("not run: the test could
  not start at the frame's start"); after `api::stop()` every later test likewise fails unrun.
- *With contract checking (stage 7):* a previous test's injected unconnected inputs and state
  variables survive the runner's normalization. A broken assumption on them fails whichever test
  is running (D36), and so does a guarantee a thread then breaks on them. Setting those inputs
  before the first step keeps such a fault attributed to the test that caused it -- isolette's
  `reinitialize()` already does this for state variables.

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

**An event input can be set to "none" (D39).** Until D39, an event or event data port's field
was a plain `T`, and its setter enqueued it, so the container could add an event but never say
that none was pending. An event the consumer has not taken survives the runner's normalization
-- for example one injected, or sent, after the consumer's slot in the frame -- so the next
test could inherit it, and its "complete" setup add a second on top. D39 closes this: event
fields are `Option<T>`, and `None` clears what is pending for that consumer (on a broadcast
connection, `Some` sets the connection rather than one consumer; see D39).

#### Clearing a pending event (D39)

*Status: built (2026-10-26), in four stages.  Raised in an external review of this design, and
revised after four more.  Stages 1 and 2: the `Option` fields, the
clear-request regions and the consumers applying them, and the setup batches -- the setters
`setup_some_<thread>_<port>` / `setup_none_<thread>_<port>` behind each container's event
fields, per-connection batch state in the controller, `put_` split into the public send that ends
the batch and the internal `inject_` the setters use, and `end_setup_batches()` at every
`api::issue` and before each test's `BEGIN`.  Stage 3 is built too: `test_clear_<thread>_<port>`
moves the cleared reader's checks cursor with the consumer's, and the `sys` cursor for an
unconnected input.  Stage 4, the on-target tests, is built: the `clearing` suite in
`models/SystemTesting/qemu/tests.rs`, run under QEMU by `SystemTestingTests`.  Of the tests listed
below, the two consecutive tests with no dispatch between them cannot be written -- the runner
always issues commands between tests -- so the `end_setup_batches()` call before each `BEGIN` is
checked in the generated code instead.*

**What it is for.** D13 asks a test to set every input it depends on, and D14 lets rustc check
that it did. For an event input "set" has to include "nothing pending", which the container
cannot express today (see the limitation above).

**The container.** An event or event data input's field becomes `Option<T>` (`Option<()>`, in
effect a flag, for a pure event port):

- `None`: nothing is pending for this consumer;
- `Some(v)`: exactly `v` is pending for it.

Data ports and GUMBO state variables are unchanged: a data port always holds a value, so
"absent" means nothing for it. The container still derives no `Default` (D14), so every test
must decide, for every event input, between an event and none.

**Why clearing cannot be done from the controller alone.** The queues are broadcast with a
private cursor per receiver ("Inspection does not disturb the system"): the shared region holds
`numSent` and `elt[]`, but a consumer's `numRecv` is in its own `Recv_t`, in its own protection
domain, which the controller cannot write. Advancing only the controller's cursors would hide
the event from the checks while the consumer still dequeues it.

**So the consumer moves its own cursor, on request, at its next dispatch.** The request travels
as an injected state variable does (D16): through a region the controller writes and the thread
maps read-only. It cannot be a queue of counters, as first proposed -- HAMR generates queue types
only for model data types -- so it is a pair of counters per port, `{seq, cutoff}`, and the
consumer remembers privately which `seq` it last applied. Nothing in the region is ever cleared:

1. While paused (D4), the controller writes the event region's current `numSent` as the
   port's *cutoff*, then, after a release fence, increments its `seq`, in the consumer's region
   `inj_<thread>_clr` -- one region per thread with event inputs, one pair per event input port
   (`test_clear_<thread>_<port>`, behind the Rust `inspect::clear_<thread>_<port>()`).
2. At the start of its next dispatch, before reading any port, the consumer's generated bridge
   (C, so the same code serves Rust and C threads) compares each port's `seq` with the one it
   last applied. A new one is applied: after an acquire fence it reads the cutoff and moves its
   `Recv_t.numRecv` up to it if it is behind. "Behind" is a wrap-safe comparison -- the
   difference `cutoff - numRecv` taken as signed -- although with a 64-bit `numSent` a wrap is
   not a practical concern. Only the newest request counts, each is applied once, and the
   consumer never writes the region. The bridge code is the first of the thread's compute
   contributions (the plugin puts its connection-store entry first), so it precedes every read,
   a sporadic thread's emptiness check included.
3. At the same time as step 1 the controller moves the contract checks' cursor for this reader
   (`<accessor>__<reader>`, D25) as the consumer will move, so the checks do not record, for its
   next dispatch, an event it will never see -- nor miss one it will.  The consumer applies only
   the newest cutoff, to where it stood at its last dispatch.  So the cursor keeps a *clear
   window*: the first request after the cursor's last read records where it stood (its base),
   and every request in the window sets it to the later of the base and the newest cutoff.  That
   can move it back over a position an earlier request in the same window discarded -- `Some(v)`,
   `None`, then `Some(v)` again for one consumer in one batch ends with `v` received, and seen.
   A read closes the window, since it is the dispatch the requests were for, and a rejected
   dispatch's rewind (D36) restores the window with the cursor, since the dispatch did not run.

Only that reader's cursors move, so the other consumers of a broadcast connection keep what is
pending for them. An injection after the cutoff raises `numSent` past it, so the consumer still
receives it: "clear, then inject" keeps its order.

**What the test and the system assertions see.**

- *A test's own reads* (`inspect::get_<port>`) go through one cursor per region, not per reader:
  they are a view of what was sent on the connection, not of what one consumer has yet to
  receive. Clearing does not move that cursor, and a test cannot ask "what is pending for this
  consumer" through it. If tests need that, it is a separate addition: reader-specific getters
  beside the existing per-region ones, which stay as they are -- with cursors of their own, not
  the checks': a test reading through the checks' cursor would take events from them, the
  interference D25 exists to prevent.
- *The system assertions* keep the distinction D27 draws between what was sent and what was
  received, and clearing touches only the second:
  - A **producer's output** is what the frame carried, whoever then received it. Clearing does
    not change it: an event cleared for every consumer was still sent, and the producer's frame
    value still shows it.
  - A **connected input's alias** (`d2_val = dst2.val`) is what the reader received, latched at
    its dispatch through the reader's own view -- the checks' per-reader cursor that step 3
    moves. A cleared reader's latch therefore shows nothing received, with no further change.
  - An **unconnected input** is what its reader received, read through the region's `sys`
    cursor at the reader's completion. Its only reader is the cleared consumer, so the
    controller moves `sys` to the cutoff as well; the value is taken at completion, so a value
    already recorded for a dispatch that completed earlier in the frame is history and stays.

**Broadcast connections: `Some(v)` sets the connection, not one consumer.** An injection goes
into the producer's queue, which every consumer of the connection reads; on a depth-1 queue it
also replaces what was pending for them. That is not a defect to work around: a real producer
cannot send different values to different consumers of one connection either, so a pre-state
that has them pending different values does not describe any state the system can be in. The
only per-consumer difference a broadcast connection really has is that one consumer has taken an
event the other has not -- which is `None` for the first and `Some` for the second. So:

- `None` is per consumer: nothing is pending for that consumer.
- `Some(v)` is per connection: `v` is pending for that consumer, and every consumer of the
  connection set to `Some` in the same batch receives the same `v`, once.

The setters for the consumers of one connection are separate calls, one per thread's
container, so the result must not depend on their order. Applied naively it would: with
`numSent = 10`, `None` for A sends cutoff 10, then `Some(v)` for B injects event 11, which A
then receives. So the setups of one connection between two scheduler commands are treated as
a batch (defined below), applied as each setter runs:

- The first `Some` on the connection in this batch records `s0`, the region's `numSent` before
  it, and injects `v` once. A consumer set to `Some` gets cutoff `s0`: everything before `v` is
  discarded, `v` is kept.
- A consumer set to `None` gets the region's current `numSent` as its cutoff, which discards `v`
  too. If a later setter in the batch then injects on the connection, the controller re-sends
  every `None` consumer's cutoff, at the new `numSent` (the consumer applies only the newest).
- A later `Some(v)` with the same `v` injects nothing; it only sends that consumer cutoff `s0`.
  A later `Some(w)` with a different `w` fails the test, naming the connection and both values,
  rather than letting the last setter win silently.

So `None` for A and `Some(v)` for B leave A with nothing and B with `v` in either order.

**What a batch is.** A connection's batch is the setter calls made on it between two scheduler
commands, and nothing in between but other setters. It ends at the first of:

- *any scheduler command* the test issues -- every one, including those that dispatch nothing
  (`info_state()`, `sstep(0)`, a `run_to_thread` already at its target), so the rule does not
  depend on what a command happened to do;
- *the end of the test*, and the runner's normalization before the next one -- so an earlier
  test's batch can never reject a later test's `Some(w)` as a conflict, even when no dispatch
  separates them;
- *an explicit `put_` on the connection.* That is a deliberate send, which every consumer then
  receives like any later send. It also invalidates the batch's saved position: after
  `set_A(Some(1)); put_<port>(2); set_B(Some(1))`, reusing cutoff `s0` and injecting nothing
  would leave B, on a depth-1 queue, with 2. So the `put_` closes the batch, and `set_B` starts
  a new one at the queue's current position -- recording a new `s0` and injecting 1 again, which
  A then receives as well: what the test sent, in the order it sent it.

Ending a batch resets only its bookkeeping -- `s0`, the value injected, the `None` consumers to
re-send to. (For the implementation: a setter injects through the same queue write as `put_`,
but must not close its own batch, so the controller has to tell the two apart -- an internal
enqueue for setters, and the public `put_` that closes the batch.) Cutoff requests already queued for a consumer stay queued: they are the consumer's,
applied at its next dispatch whatever the batch does.

Order independence holds within one batch. A test that splits one connection's setup across a
command -- `set_A(None); info_state(); set_B(Some(v))` -- has made two batches, and A, cleared
before `v` was injected, receives it. Setting up every consumer of a connection between the same
two commands, as a test's setup naturally does, is what the guarantee asks.

**Interaction with the rest of the design.**

- *Overruns and lost tracking:* `forget_ran` already drains event cursors for dispatches that ran
  unobserved, and drops pending injections. A request the unobserved dispatch saw has been
  applied, and one it did not is still waiting for the next, so the request region needs nothing
  extra; the controller only has to move the checks' cursor the same way the consumer did.
- *A rejected dispatch (D36):* a dispatch the controller skips leaves its events pending, which
  is exactly when a test needs to clear them -- the request stays and applies at the next dispatch
  that does run.
- *The contract checks:* a cleared event never reaches the consumer, so the saved pre-state for
  its next dispatch must not include it; moving the checks' cursor in step 3 does that.
- *Code size:* one region per thread with event inputs -- 16 bytes per input, in whole pages,
  so a page per 256 inputs -- packed after the state
  variable injection regions in both the controller's and the threads' injection blocks, and a
  compare per event input at dispatch start. The bridge code is in every variant, but the
  pointer is NULL wherever the region is not mapped -- everywhere but the test variant -- so it
  does nothing there.

**Migration.** Every existing test's container literal that sets an event input stops compiling
until it wraps the value in `Some(...)`. That is the point of D14 -- rustc finds every test that
has to decide -- and the change is mechanical.

**Tests.**

- Clearing after a rejected dispatch: inject an event that breaks an assumption, let the dispatch
  be rejected, set the field to `None`, step, and check the consumer saw no event.
- Clearing one consumer of a broadcast connection (`models/SystemTesting`'s `val`, read by `dst1`
  and `dst2`): `None` for one, step both, and check only the other received the event -- and that
  the system assertions still saw it sent.
- Clearing every consumer of a connection: neither receives the event, both received-input
  aliases show nothing, and the producer's output still shows it was sent.
- Mixed setup in both orders: `None` for `dst1` then `Some(v)` for `dst2`, and the reverse, give
  the same result -- `dst1` receives nothing and `dst2` receives `v`.
- An explicit `put_` inside a batch: `set_dst1(Some(1)); put_<port>(2); set_dst2(Some(1))`
  delivers 1 to `dst2` (the `put_` closed the batch, so `set_dst2` injected again).
- Batch lifetime: a command that dispatches nothing (`info_state()`, `sstep(0)`) between two
  setters ends the batch, so a different value after it starts a new batch rather than failing as
  a conflict; and two consecutive tests with no dispatch between them, the first setting `Some(1)`
  and the second `Some(2)` on the same connection, both pass.
- `None` on an unconnected input: the event is gone from the consumer, the checks and the system
  assertions alike.
- `Some(v)` with an event already pending: the consumer receives exactly `v`, once.
- Successive setup of two consumers of one connection: the same `Some(v)` for both delivers `v`
  once to each, on a depth-1 queue and on a deeper one; different values fail the test, naming
  the connection.
- Clear, then a later `put_` in the same pause: the consumer receives the later value.

### Contract observation in the controller (stage 7)

How each piece was built and verified, and what the review rounds changed, is in
TestScheduler-history.md ("Stage 7").  The implementation steps map onto the sections here:
(1) "Splitting "where am I" from "what to check"", (2) "The monitor PDs are thin wrappers",
(3) "What requesting system testing generates", (4) "The controller hosts the layers", (5) "The
scheduler's observation park", (6) "Enabling and disabling checks", (7) "After a watchdog trip".

Before stage 7 a contract violation could not fail a system test. The monitors that check
contracts are stripped from the test variant (see "Relationship to the runtime monitor"), and
even where they run they report only `log::warn!("*** CONTRACT VIOLATION ...")` lines, which the
D18 host driver ignores because they lack the `TEST | ` prefix. Stage 7 makes the controller
check the same contracts at every dispatch (while a layer is live) and turns a violation into a
failure of the test that caused it. This is the D3 payoff "every thread's contracts held at
every dispatch", obtained without adding a monitor PD to the schedule under test.

#### What requesting system testing generates

Requesting system testing (`--system-testing`) is enough; `--runtime-monitoring` is not
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

**The plumbing is separate from the monitors (D22, superseding D5).** The component layer reads
GUMBO state variables, and before stage 7 the `sv_` ports, their regions and
`is_monitoring_enabled()` existed only when `--runtime-monitoring` was given, which is why D5
demanded that flag. Under stage 7 system testing brings that plumbing itself.  The component
layer is generated for every thread with checkable GUMBO contracts -- Rust or C, with or without
state variables -- since a model with contracts but no state variables still has
CEP_Pre/CEP_Post to check.  D5's diagnostic went away with it. Two pieces of the monitor plugins
were separated from the monitor PDs for this:

- **The `sv_` ports.** They were not created on their own: `wireMonitorStateVars` added
  each thread's `sv_` output ports while wiring that thread to a monitor, in the same loop that
  calls `injectMonitorPDNamed`. The port creation moved into a step of its own
  (`StateVarPortsPlugin`) that runs for
  `--runtime-monitoring` **or** `--system-testing`; the monitor wiring stays behind
  `--runtime-monitoring`. With no monitor attached the `sv_` ports are unconnected outputs,
  and their regions come from `CConnectionProviderPlugin`'s unconnected-port loop, the route
  D15 already relies on. `handleCBackend` (`is_monitoring_enabled()` and the `put_sv_*`
  guards) moved with it, so `GumboSysAssertMonitorPlugin` no longer needs its check for whether
  the gumbo monitor had already run it.
- **The check code.** `GumboMonitorPlugin` and `GumboSysAssertMonitorPlugin` are gated on
  `options.runtimeMonitoring`, so they cannot be what generates the layers for a run that asks
  only for system testing. The layers are generated by a new `ContractObserverPlugin`, which
  runs when a consumer was injected -- a GUMBO or system-assertion monitor, or the test
  controller (D19) -- and puts `crates/observers` in the store; the monitor plugins and
  `TestSchedulerPlugin` consume it.

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

#### Splitting "where am I" from "what to check"

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
may be a test step later (see "The scheduler's observation park"). The check half is therefore
split along that line, and needs only a thread and a way to read ports and state variables. It
is generated once, generic over two traits, into a `crates/observers` crate beside `data` and
`GUMBO_Library`.  As built (abridged):

```rust
pub enum Thread { tsp_tst, tcp_tct, fp_ft }           // the model's threads; each host maps its channels
pub trait SystemView {                                // one getter per port / state var the checks read,
    fn get_tcp_tct_sv_latestTemp(&mut self) -> Temperature;   // returning what the monitors' API returns
    fn get_tsp_tst_currentTemp(&mut self) -> Option<Temperature>;
    fn get_recv_tcp_tct_fanAck(&mut self) -> Option<FanAck>;   // an alias of a connected input
    // ...
    fn focus(&mut self, _t: Option<Thread>) {}        // whose check is reading (None: the system layer)
    fn missing(&self) -> bool { false }               // a read since focus found nothing: skip the check
    fn focus_system(&mut self, _composition: usize) {} // after focus(None): whose assertion is reading
    fn frame_ended(&mut self, _composition: usize) {}  // that composition's frame is over
}
pub trait ViolationSink { fn report(&mut self, e: Event); }   // Event: IepPostViolation, CepPreViolation,
                                                              // CepPostViolation, CepPostSkipped,
                                                              // CheckSkipped, SysAssertViolation, Schedule*
pub struct ComponentContracts { /* Option<PreState_<thread>> per thread */ }
impl ComponentContracts {
    pub fn on_init(&mut self, s: &mut impl SystemView, out: &mut impl ViolationSink);
    pub fn on_complete(&mut self, prev: Thread, s: &mut impl SystemView, out: &mut impl ViolationSink);
    pub fn on_dispatch(&mut self, next: Thread, s: &mut impl SystemView, out: &mut impl ViolationSink);
    pub fn forget(&mut self);                         // drop every saved pre-state (D35)
    pub fn pre_failed(&self, t: Thread) -> bool;      // t's latest CEP_Pre failed: a host may reject (D36)
}
pub struct SysAssert_nominal { ready: u64 }           // one struct per composition
impl SysAssert_nominal {
    pub fn validate_schedule(num_timeslices: usize, timeslice_ch: &[u32], is_user_partition: &[bool],
                             thread_of: fn(u32) -> Option<Thread>, out: &mut impl ViolationSink);
    pub fn on_init(&mut self, s: &mut impl SystemView, out: &mut impl ViolationSink);   // START + cascade
    pub fn on_complete(&mut self, prev: Thread, s: .., out: ..);  // complete; at END, frame_ended + restart_frame
    pub fn complete(&mut self, prev: Thread, s: .., out: ..) -> bool;   // fire, cascade, check: reached END?
    pub fn restart_frame(&mut self, s: .., out: ..);                    // START + cascade, check
}
```

`ContractObserverPlugin` emits `ComponentContracts` and one `SysAssert_<id>` per composition.
An `Event` carries the kind, the thread or composition, property and place, and the pre/post
values; each host turns it into its own output (the monitors' log lines, the controller's
`VIOLATION` lines).  The `gumbox/*.rs` modules, copied into every monitor crate before, live in
`observers`.

#### The monitor PDs are thin wrappers

```rust
// gumbo_monitor
let (prev, next, first) = self.locate(api.get_sched_state(), api.get_sched_schedule());
if first { self.components.on_init(&mut view, &mut LogSink) }
else {
    self.components.on_complete(prev, &mut view, &mut LogSink);
    self.components.on_dispatch(next, &mut view, &mut LogSink);
}

// sys_<id>_monitor: the GUMBO layer as above when there are thread contracts (D34), then its own
if first { self.sys_nominal.on_init(&mut view, &mut LogSink) }        // START + cascade
else { self.sys_nominal.on_complete(prev, &mut view, &mut LogSink) }
```

`view` implements `SystemView` by delegating to the monitor's existing `api.get_*`; `LogSink`
reports with `log::warn!` and the existing message text.

**A thread that overran its slot (D35).**  The monitor variants keep the default timer-driven
scheduling: each slot lasts its configured time whether or not its thread has finished, and a
monitor runs at the threads' priority.  A thread still running when the monitor's slot starts
would be read mid-dispatch -- under QEMU a host stall is enough -- and its unfinished outputs
reported as a broken contract.  So the monitor variants' scheduler counts, per channel, the
dispatches a thread has not yet reported completing (the C2 notify-back), and publishes in
`sched_state.last_yielded_ch` the previous user slot's channel once its thread has completed
every dispatch, 0 while it has not, and in `sched_state.outstanding` the dispatches of every
thread not yet completed.  A monitor treats the slot as overrun when its `prev` has not
completed, or when any earlier thread is still running past later slots (`outstanding != 0`)
-- that thread may be writing what `next` reads, though `prev` finished.  Then it checks
nothing that the running thread could affect:

- the GUMBO layer logs `*** OVERRUN: <thread> did not complete within its slot ... ***` (or,
  when `prev` finished, `an earlier thread is still running past its slot`), drops
  every saved pre-state (`ComponentContracts::forget`, so each thread's next completion is
  skipped), and does not capture the next thread's pre-state in that run, since the overran
  thread may still be writing what that thread reads;
- the system layer is suspended, as the controller's is after an overrun (D30), and resumes
  with a new frame (`frame_ended`, `restart_frame`) at the first monitor run of a later
  hyperperiod at which no dispatch is outstanding.

The count per channel is capped at 2.  A thread still running when its slot comes round again
gets the dispatch as a pending notification, and the kernel holds one: further dispatches merge
into it, and the thread completes once for all of them.  Counting each would leave the count
above zero for good, and with it every monitor run taken for an overrun.  (Completions do not
merge: a thread signalling its `_MON`, which outranks it, is preempted at once, and the `_MON`
forwards to the scheduler, which outranks both.)

A thread that never finishes is the timing fault itself, which the monitor reports rather than
masks; what it no longer does is turn it into a contract violation.  A thread that *faults* also
never notifies back -- its `_MON` leaves it suspended -- so in the runtime-monitoring variants
the `_MON` answers for it (`HAMR_MON_FAULTED_COMPLETES`, defined by those variants' `.mk` through
`MON_CFLAGS`): once for each dispatch in flight when it faulted, and at once for every later
dispatch, which it completes doing nothing; a thread that faulted while initializing is also
reported ready, so the schedule still starts.  The counts stay balanced and the monitors keep
checking the other threads.  The faulted thread's own guarantees are then reported as broken,
and so can be a composition's assertions that read it, after the `FAULT in user PD` line that
says why.  A Rust thread that panics takes the same path: the generated panic handler logs
`PANIC` and then traps (`udf`), rather than spinning in its slots forever.  The test-scheduler
variant (`test_scheduler.mk` sets `MON_CFLAGS` empty) and the image that ships keep a faulted
thread unanswered: under the test scheduler its dispatch trips the watchdog and fails the test,
and a thread that faults while initializing keeps the schedule from starting (see "Reporting the
verdict").  In the domain-scheduled builds the `_MON` reports a fault the same way, and leaves the
thread suspended.

All variants build in one `build/` directory, so an object compiled with one variant's
`MON_CFLAGS` must not be linked into the next.  sDDF ties the objects to its own stamp
(`board/common.mk`, `.board_cflags-*`), but hashes `CFLAGS` before `system.mk` extends it, so it
does not see `MON_CFLAGS`.  `system.mk` therefore hashes the final `CFLAGS` (`CHECK_CFLAGS_MD5`)
and, when it changes, removes every object and archive in the build directory before anything is
built -- only when make runs in `$(TOP_BUILD_DIR)`, as the top Makefile has it.  HAMR's own image
stamp is named `.hamr_flags-*`: it used to share sDDF's name, and as each stamp's rule deletes
its name's files, each deleted the other and every `make` recompiled and relinked everything.

#### The controller hosts the layers

As built (abridged), the layers and their switches are module statics in
`system_tests/observe.rs`:

```rust
static mut COMPONENTS: ComponentContracts = ..;   // generated when the model has GUMBO thread contracts
static mut SYS_nominal: SysAssert_nominal = ..;   // generated per composition
// ...
static mut GUMBO_LIVE: bool = ..;                 // build level: is the layer tracked at all this run
static mut SYSVERIF_LIVE: bool = ..;
static mut GUMBO_ON: bool = ..;                   // suite / test level: is a live layer checked right now
static mut SYSVERIF_ON: bool = ..;
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
maps, but **not through `inspect::get_*`**. Those reads consume: `inspect::get_*`
dequeues through the controller's receive cursor, so a second read of the same region with no
dispatch in between returns `None` (isolette's `tests.rs` documents this for test writers).
Sharing that cursor would let the checks consume the values a test is about to read, let a
test's read make a check see `None` and skip silently, break a region read twice in one park
(the output of `prev` read by `on_complete` that is also an input of `next` read by
`on_dispatch`), and let `on_init` consume the post-initialization outputs that isolette's
`regulator_post_init()` snapshots before its first test.

The queues are broadcast with a private cursor per receiver, which is exactly how each monitor
PD reads them without disturbing anyone. So the controller bridge gains a second accessor
family, `test_obs_get_*`, with cursors of its own, used only by the view (one per data region;
one per reader for an event region, see "Event ports"). For a data port or state variable the
view keeps the **last value** it read and returns it until a newer one arrives -- what a thread
sees -- so a check reads it as often as it likes.  A data port never written reads as its
default, as it does to the thread; a state variable never written sets `missing`, and the check
is skipped and reported as `CheckSkipped`.  (The monitors never raise `CheckSkipped`: their reads
are never missing.  Their "post check skipped" line is `CepPostSkipped`, no saved pre-state.)  `inspect::` is unchanged and remains
the tests' own cursor.

One more difference, for state variables: a value the controller has injected but the thread
has not yet adopted is still in the `inj_sv_` queue, while the `sv_` region holds the old
value. Recording that old value
as `In_<var>` would make CEP_Post compare against a pre-state the thread never started from.
The generated `put_<thread>_sv_<var>` therefore also records a pending value, which the view
returns to `<thread>`'s own checks only -- it is the pre-state that thread will start from;
everyone else, the system layer included, reads the thread's actual state -- and which is
cleared when `<thread>`'s completion is accounted for (or, after an overrun or unobserved
dispatches, when the controller learns the thread has run).

Port injections need care too.  An injection into a producer's *output* region is what its
consumers receive, but it is not the producer's own output: `put_<producer>_<port>` also
consumes the element from the producer's own cursor, so its next completion check does not take
the injection for something it sent, and makes it the frame's value for the system layer (see
"Frames and event values").

**`TestSink`** cannot report the way `sys_assert!` does. A `sys_assert!` fails in the test
body and returns from it; a violation is found inside `api::hstep` and friends, in the
controller's wait loop, where there is no test body to return from. And D18's host driver
counts `TEST | FAIL` lines and fails the run as "output was lost" unless the count equals
`DONE`'s `failed=`, so one failed test must produce exactly one `FAIL` line however many
violations it saw. So:

- A violation is printed as `TEST | VIOLATION CEP_Pre|CEP_Post <thread> hp=<n> slot=<n>`,
  `TEST | VIOLATION SysAssert <composition>/<property> at <point> hp=<n> slot=<n>`, or
  `TEST | VIOLATION IEP_Post <thread> init` (an initialization guarantee; `init` also replaces
  the position for anything checked at initialization), and `Schedule <composition>: ...` for a
  schedule that does not conform, with the pre/post values as `TEST | INFO` lines, and marks
  the test failed. The test keeps running to the end of its body; there is nothing to return
  from.
- The **first** failure of a test prints its `TEST | FAIL` line: `sys_assert!` with its file
  and line as before, or the runner, at the end of the test, with `contract violation (<n> in
  this test)` when the test failed only through violations. Later failures of the same test
  print as `INFO  also failed`. (`harness::fail_with` prints the `FAIL` line, as before stage 7;
  what is new is that the runner calls it too, from `end_test`, for violations and unmet
  expectations.)
- The host driver passes `VIOLATION` lines through, as it does every `TEST | ` line.  It also
  fails a run on `DONE`'s `init=failed` (see "The scheduler's observation park"), honours
  `--list`/`list=1` (D17), classifies a line by its prefix, and requires a complete `DONE` line
  (see "Line format").

Negative tests that trip a guarantee or a system assertion on purpose declare it with
`observe::expect(..)`:

- It matches on kind and location: `expect(Expect::CepPost(Thread::srcp_src))`,
  `expect(Expect::SysAssert(sys::nominal::COMPOSITION, sys::nominal::Echoed))` (both from
  `models/SystemTesting`).  (An
  initialization guarantee is checked before any test runs and fails the run through `init=`,
  so it cannot be expected.)  The threads, compositions and properties are generated as
  constants, so a typo is a compile error.  A system assertion is matched by property, not by
  place: a property's assertions at every place are covered.
- It covers the rest of the test. Matching violations are printed as `TEST | INFO` instead of
  failing it; any other violation still fails it.
- **An expectation never met fails the test**, like `#[should_panic]`, with a `TEST | FAIL`
  naming it. Otherwise a negative test keeps passing after the fault it targets stops
  happening.  An expectation for a layer that is switched off fails at once: its violations
  cannot be reported, so it could never be met.

`observe::take()` returns the violations recorded so far in the test and clears them, for a
test that wants to assert on them itself. A test that feeds a thread inputs outside its
assumptions on purpose declares that with `Expect::CepPre(thread)` (see "Assumptions versus
guarantees").

#### The scheduler's observation park

The controller is the lowest-priority PD (D4), so during `hstep(n)` it never runs between
slots and cannot see intermediate dispatches on its own. The scheduler provides the boundary:

- `test_command_t` gains `observe`. When it is set, the scheduler parks before dispatching
  each user slot (pads and non-user slots excluded), publishing the position,
  `last_dispatched_ch`, `next_ch`, `completed_seq` and then `obs_seq` in `test_status`, and
  notifies the controller.
- `test_status` also gains `completed_seq`, incremented by the scheduler on every user-slot
  completion, observed or not -- user slots only, the ones that park. It identifies a
  *dispatch*, which a channel cannot: the same channel completes once per frame. The controller
  keeps the last `completed_seq` it accounted for, and runs `on_complete` only when
  `completed_seq` has moved by exactly one.
  This matters because `on_complete` is not idempotent -- the system layer's advances the
  marking -- so completing one dispatch twice would desynchronize the marking and falsify every
  system assertion after it.
- While any layer is live the scheduler parks before every user dispatch, so `completed_seq`
  never moves by more than one between completion checks. A larger jump means a dispatch went
  unobserved -- hypothetically, a command issued without `observe` -- leaving a saved pre-state
  or the marking stale. The controller treats it like a watchdog trip: a `TEST | INFO` line,
  saved pre-states dropped, and the system layer suspended until it can resume (D30: at the
  hyperperiod's first user slot, or in the next hyperperiod; see "After a watchdog trip").  It
  also walks the schedule back over the dispatches that ran unobserved, clearing those threads'
  pending injections and draining their event cursors (`forget_ran`).  No test command can
  produce this -- every command parks while a layer is live, and the test variant has no
  non-user slots other than pads -- so this path is covered by code review only.
- The controller's busy-wait loop, which already runs whenever the scheduler is parked,
  watches `obs_seq` as well as `ack_seq`. On a new `obs_seq` it calls `on_complete(prev)` if
  `completed_seq` has moved, starts a new frame or resumes the system layers if the park is in a
  new hyperperiod, then `on_dispatch(next)`; if that rejects the dispatch (D36) it stores
  `obs_skip = obs_seq`, and then, after a release fence, writes `obs_ack` and notifies the
  scheduler, which dispatches -- or, on `obs_skip`, passes the slot without dispatching. A
  rejected slot is charged like a dispatched one (`advance_position`: it counts toward
  `sstep`/`hstep` and the `RunTo*` budgets), and leaves `completed_seq` and
  `last_dispatched_ch` where they were, since nothing ran. This is the D9 sequence-number
  handshake a second time. The scheduler's `TEST_CONTROLLER_CH` arm tells the two apart by
  which sequence number moved.
- **Command completion is also a completion point.** When a command completes, the last slot it
  dispatched has finished, but no park follows until the next command starts -- which may be
  after the test has ended. So when the controller sees `ack_seq`, it calls
  `on_complete(last_dispatched_ch)` before returning from the API call -- **if
  `completed_seq` has moved by one**. A command can complete without dispatching anything
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
  run is exactly stages 4-5. That covers a model with no GUMBO contracts and no composition,
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
the host driver fails the run on `init=failed`.  A schedule that does not conform to a
composition is reported the same way (`VIOLATION Schedule <composition>: ...`) and also makes
`init=failed`; threads a composition leaves out are passed over (see "Compositions").

Layer state persists across tests. Saved pre-states are per dispatch anyway, and the marking
must see every dispatch, so tracking stays on through the runner's normalization; only
recording pauses (see "Recording window").

#### After a watchdog trip

`TEST_FLAG_OVERRUN` aborts a command with a thread that never reported completion. Its saved
pre-state no longer describes anything, and the marking has lost its place. The controller drops
every saved pre-state (the next completion check of each thread is skipped, as for a missing
pre-state), drains the overran thread's event cursors (what its unobserved dispatch consumed and
sent is not its next dispatch's), suspends the system layer, and prints a `TEST | INFO` line
saying so, instead of producing a run of false violations from one hang.  The system layer
resumes at the first park of the **next hyperperiod**: the aborted dispatch's output still
reaches its consumers in the frame it ran in, which then mixes two dispatches of the thread, so
no frame-level assertion about that frame means anything.  (A loss that is not an overrun --
dispatches completed unobserved -- has no aborted output, and may resume at the hyperperiod's
first user slot.)  The resumed frames start when the hyperperiod did (`HP_FRAME_AT`; see
"Frames and event values"), not at the resume, so what a test injected
at the stop before that first park belongs to them, including a stop on the trailing pad of the
hyperperiod before; `SystemTestingTests`' `watchdog::injection_at_frame_start_after_overrun` and
`watchdog::injection_on_the_trailing_pad_after_overrun` check both.

#### Recording window

Violations are **recorded only while a test runs**: from its `BEGIN` to the end of its body,
including the completion check of its last command (see "The scheduler's observation park").
Everything the runner does between tests -- its `run_to_slot(0)` normalization -- is observed
but not recorded. The layers keep **tracking** throughout, saving pre-states and advancing
markings, so the next test starts with correct state; only reporting pauses.

This makes "between tests" disappear as a category. A finished test's injected values may still
drive the rest of the frame out of contract during normalization, and none of that is anyone's
verdict. What can still carry over is an injected value that outlives the normalization -- an
unconnected input, which only the controller writes, an injected state variable the thread
keeps, or an event injected after a consumer ran: the consumer dequeues it at its next dispatch
(unless the producer's own send displaces it first), and if the test ends there, the runner's
`run_to_slot(0)` stops before that dispatch, so the event lands in the next test's first frame,
where the consumer's check and any received-versus-sent assertion see it (see "An injection is
in the frame it is made in" under "Frames and event values"). If it breaks a thread's
assumption, or the thread's assumptions hold on it and the thread still breaks a guarantee,
that is charged to the test that was running -- one test late, which D13's "set every input you
depend on" avoids (see D13).

#### Assumptions versus guarantees

GUMBO contracts are assume-guarantee.  A dispatch's CEP_Pre (its `compute` assumptions and the
`I_Assm_*` of its input ports) is the receiver's contract with whoever sends to it: when it does
not hold, the sender has not met it, and the values it sent are not allowed in (D36).  The
controller treats it as Slang system testing does (`SystemTestSuiteSlang` asserts every
CEP_Pre, and the failed assert stops the dispatch):

- **CEP_Pre fails** -> `TEST | VIOLATION CEP_Pre <thread>`, which fails the test, and the
  dispatch is **rejected**: the controller tells the scheduler at the park (`obs_skip`), the
  slot passes without dispatching the thread, and the cursors the check read on its behalf are
  put back, so the thread's next check sees what the thread will.  A rejected dispatch owes no
  guarantee, and the workflow net's transition for it never fires: the system layers are
  suspended until the next hyperperiod, as after an overrun.  Its input stays where it was --
  a data port's value until the sender replaces it, an event until the thread next consumes
  it or, the queue holding one element, the sender's next send displaces it -- so a thread
  whose sender keeps it out of contract is rejected at every dispatch.  What the dispatch
  check latched for the system layers' aliases of the thread's inputs is put back too: the
  thread received nothing.  The controller prints `TEST | INFO  channel <ch> not dispatched at
  hp=<n> slot=<s>: its inputs did not meet its CEP_Pre` (with `; system assertions suspended
  until the next frame` when it suspends them).  The runner's normalization between tests
  rejects in the same way, with no test to fail: outside a test's recording window only the
  `INFO` line shows it.
- **IEP_Post, CEP_Post and `I_Guar_*`** fail the test whenever they are checked.
- **System assertions** fail the test whenever they are checked.

It makes no difference whether the value came from a producer or from the test, as in Slang.
A robustness test that injects an input outside a thread's assumptions on purpose declares it,
like any violation it means to cause: `observe::expect(Expect::CepPre(thread))`.  The dispatch is
still rejected -- the expectation changes the verdict, not what the system does.  This is the
system-test counterpart of GUMBOX's unit tests, where a generated vector that fails CEP_Pre is
discarded before the entry point runs, and only counts as a failure if the user asks
(`failOnUnsatPreconditions`).

The monitors report a failed CEP_Pre as a violation too, but do not reject the dispatch: they
observe, as Slang's runtime monitoring does, which records a failed CEP_Pre and lets the
dispatch run.  As in Slang, that dispatch's CEP_Post is still checked against the pre-state it
ran on (D37), so a monitor log shows both what the sender broke and what the thread then did.
The controller never meets this case: a rejected dispatch does not run, so it never completes.

This catches the integration faults the static check cannot: `I_Guar => I_Assm` (the "Checking
integration constraints" step of `.ci/ci.cmd`) covers single-port integration constraints, but
not a `compute` assumption relating several ports and state variables, which a producer's
legitimate-looking output can still break.

A test that deliberately drives the system out of contract and so breaks a system assertion,
or a downstream guarantee, declares that with `observe::expect`, or turns the relevant checks
off for its suite.  With the GUMBO checks off nothing is checked, so nothing is rejected.

#### Enabling and disabling checks

Each generated layer is **enabled by default**. The user disables either one at three levels:

| Level | How | Scope |
|-------|-----|-------|
| Build | `make CONFIG=test_scheduler.mk GUMBO_CHECKS=off SYSVERIF_CHECKS=off` (or the same variables in `run-tests.cmd`'s environment): bits of `TestSelection.flags`, patched through the same ELF section as `TESTS=` (D17), and part of the rebuild hash with it | the whole run; the layer is **not live**: not tracked, and not parked for |
| Suite | `suite fault_injection(gumbo = off) { ... }` in `system_tests!` | every test in the suite; checking only |
| Test | `observe::set_gumbo(false)` / `observe::set_sysverif(false)` in the test body | the rest of that test; checking only |

Build level is the only way to get the unchecked run at stages 4-5 speed: with both switches
off, nothing is live, and the scheduler never parks. Suite and test level can turn a live
layer's checking off and back on, but they **cannot bring a layer back** that build level
turned off, because it has not been tracking. A suite or test that asks for one --
`(gumbo = on)`, or `observe::set_gumbo(true)`; a suite's setting goes through the same switch
at the start of each of its tests -- fails at that point with a message naming the build
switch, rather than running and silently checking nothing. It cannot be a compile error:
build-level settings are patched into the ELF after compilation, like `TESTS=`.

The runner restores the build-level setting when every test ends (`end_test`), so a test or suite
that turns a check off cannot leave it off for the next one -- not even for the normalization to
slot 0 before it, which checks, and rejects, as the build says -- the same isolation D13 requires of inputs.
Asking for a layer the model did not generate is a compile error on the per-test API (the
function is not generated) and a warning on the build variable.

Disabling is the blunt tool. A negative test that trips one particular contract on purpose
should keep checking on and declare the expectation with `observe::expect(..)`, so every other
contract is still checked while it runs.

#### Event ports

The last-value cache is right for data ports and wrong for event ports: an event must be seen
once, at the dispatch that consumes it, not held.  A **thread's** check reads an event port the
way the thread does: present at a dispatch only if it arrived since that thread's previous
check.  So an event region has **one cursor per reader** -- the producer (whose own guarantees
read its output), each consumer, and the system layer (`sys`) -- not one per region: an event
that fans out is dequeued by each consumer independently, and a single cursor would consume it
at consumer A's dispatch and leave consumer B's CEP_Pre with none.  Data ports keep one cursor
per region, since the last-value cache serves every reader.  The monitors do the same with
their own reads: every event port is drained at the start of each run into an event count,
keeping the latest element, and each reader keeps the count it last saw (`EventTrack`).

The **system layer** reads event ports differently -- as what the frame carried, see "Frames
and event values".  A producer's output is taken from the `sys` cursor as the producer
completes.  An unconnected input has no producer: its value is taken from its `sys` cursor as
its reader completes, drained to the latest element -- what the reader received when it took
one element per dispatch.  A composition's alias of a connected input is latched from the
reader's own cursor at its dispatch.  The latch is the host's,
not a layer's: the controller calls `latch_received` in its dispatch hook whichever layers are
live, and the sys-assert monitor calls it itself.

Only single-element queues are supported.  A deeper one raises the question of which element a
dispatch consumed, which the checks do not settle: a consumer's checks see one element per
dispatch -- in the controller the next one through the reader's own cursor, which matches a
thread that consumes one per dispatch; in a monitor the latest it drained, which can differ from
what the thread consumed when elements queue up; the producer's own check, the frame value and
hiding an injection from its producer (see "The controller hosts the layers") take the latest
element, which is what the producer's `put`s leave, in the controller and the monitors alike. A
thread with GUMBO contracts, Rust or C, may not have a deeper event queue, and neither may an
event port a composition declares an alias for, on any thread (whether or not an assertion
reads the alias): when system testing or `--runtime-monitoring`
is requested, codegen stops with an error naming the thread (or the composition) and the port
(`MicrokitGumboLinter`).  Without them it is a warning: nothing then reads the queue one element
per dispatch at run time, and a Verus proof of the thread does not read it that way (D38).  An output feeding consumers with
different queue sizes has one region per size, which the controller's accessors (named after the
port) cannot tell apart: that is a codegen error.

Isolette exercises only data ports; temp-control's ports are event-data ports, so it is the
model that exercises the event semantics.  `SystemTestingTests` covers them: an event with two
consumers, a C consumer, an unconnected input read by two assertions, and a feedback edge.

#### Frames and event values

A system assertion is about one moment of one frame.  For a data port or state variable that
moment is simply "now".  For an event port the question is **"did the frame carry an event
here"**, which neither a cursor nor the moment of reading can answer: whichever assertion read
first would consume the event, and a later assertion -- at the same place or another -- would see
none.  So the system layer reads event ports as **per-frame values**:

- **A producer's output** is what the producer's latest dispatch in the current frame sent --
  taken when its completion is accounted for; *nothing*, if that dispatch sent nothing, so a
  thread that fires twice in a frame is read as its second firing left it, as the proofs'
  write frames have it -- or what a test injected into it that no send has replaced
  (`put_<producer>_<port>` sets it: the consumers receive the injection).
- **An unconnected input** is what its reader received, taken at the reader's completion (from
  the port's `sys` cursor, drained to the latest element).
- **A composition's alias of a connected input** (`d2_val = dst2.val`) is what the *reader*
  received, taken when the reader is dispatched (`get_recv_<reader>_<port>`).  On a feedback
  edge -- the producer runs after the consumer, as temp-control's `fanAck` does -- this is the
  previous frame's value, which is what the consumer really got; reading the producer's output
  instead would see "nothing yet" at every place before the producer.  The reader's own checks
  read the same latched value, so nothing is dequeued twice.  For a data input it is the value
  the reader read at dispatch.  A thread connected directly to itself would be the producer and
  the reader of one connection in the same dispatch, which neither rule describes; the Microkit
  linter rejects such a model (its state belongs in a GUMBO state variable).

**An injection is in the frame it is made in.**  Its consumers that have yet to run in this
hyperperiod receive it in this frame, and the producer's frame value shows it.  A consumer that
has already run receives it at its next dispatch, in the next hyperperiod (unless the producer
sends again before that dispatch, displacing it -- see "Port injection: act as the producer") --
where the producer's frame value is what the producer then sends, not the injection.  An
assertion relating the two (`after dst1: HasEvent(d1_val) implies HasEvent(s_val)`) then sees
something received that was not sent, and reports it: that is what happened in that frame, not a
checking error, and carrying the injection into the next frame would claim the producer sent it
there. To have every consumer see an injection in one frame, inject after the producer has run
and before its earliest consumer runs, e.g. after `run_to_thread(<earliest consumer>)`.
Injecting after `run_to_thread(<producer>)` stops *before* the producer, whose own send then
displaces the injection (see "Port injection: act as the producer") -- unless the producer sends
nothing, as in the QEMU tests that set src's mode to 99
(`injected_output_reaches_the_system_assertions`,
`injection_at_frame_start_belongs_to_that_frame`).  On a feedback edge, where a consumer runs
before its producer, no such point exists.  The controller says so where it happens: an
injection into a producer's output prints `TEST | INFO  <port> injected after <consumer> ran in
hp=N: <consumer> receives it in the next hyperperiod, where <producer> did not send it` for a
consumer that was dispatched in the current hyperperiod (`DISPATCHED_HP`,
recorded in `on_dispatch`) and has no user slot left in it (`slot_left_for`: a multi-rate
consumer with a later slot, or one that just overran and is about to be re-dispatched, receives
it now).  It does so only while a user dispatch is still to come in the hyperperiod
(`dispatch_left_in_hp`, from the position `roll_frame` last saw): at a stop after the last one
-- on a trailing pad -- the injection is already the next frame's (`LAST_DONE_AT`, see
`roll_frame` below), and every consumer receives it there.  And only where the mismatch can be
seen: the model has compositions and the system assertions are live and switched on
(`SYSVERIF_LIVE && SYSVERIF_ON`), so a test or suite that switches them off gets no such line.

**When a frame ends.**  A composition's frame ends when its marking reaches END: the observers
call `SystemView::frame_ended(c)` and then put the marking at START.  So START's assertions see
only events that arrive from then on.  **Each composition has its own frame**: compositions
need not end at the same completion (one may leave out the frame's last thread, D29), and one
ending its frame must not touch another's.  The controller stamps every value it takes with a
logical time (`Stamped`, `tick()`), each composition records the time its frame started
(`COMP_START`), and before each system assertion the observers name the composition
(`focus_system(c)`): a value is in that composition's frame if it was taken after its start.
The monitors check one composition per protection domain: they advance a frame counter at END,
and drain every event port at the start of each run, so an event is stamped with the frame it
arrived in (polling only when read stamped it with the frame of the first read, which could be
the next); a producer's dispatch that sent nothing is recorded when its completion is checked
(`note_dispatch` records that a thread was dispatched, `producer_completed` that its completion
was accounted for, in each event port's `EventTrack`).

The controller's `roll_frame` runs at every park and command end.  It always records the
position (`POS_SLOT`), and at the first park or command end of each new hyperperiod it records
when that hyperperiod's frame began; only when no system layer is running -- switched off at
build level, or suspended -- does it also start a frame for every composition.  A running
system layer ends its frames itself, at END.  Two logical times are involved:
`LAST_DONE_AT` is the time the latest completion was accounted for, updated at every
accounting; `HP_FRAME_AT` is the `LAST_DONE_AT` that `roll_frame` recorded when the hyperperiod
changed -- the end of the hyperperiod before, not where the new one is first observed.  So when
no system layer is running, or one resumes, a hyperperiod's frame begins at `HP_FRAME_AT`: a
test's injection made after the previous hyperperiod's last dispatch -- e.g. at a stop on a
trailing pad -- and before the next frame's first park belongs to the frame that follows.  A
system layer resuming after a suspension starts its frames there too, not at the resume: the
resume happens at the hyperperiod's first park, after the stop at which a test may already have
injected into it.  (Starting them at the resume, and then at the first observation of the new
hyperperiod, hid such an injection.)
The comparison with a frame's start is wrap-safe (`Stamped::visible`).

**Initialization.**  What producers sent while initializing is the first frame's value, for
the controller and the monitors alike, until they send again; START at initialization sees it.
It is not the producer's first dispatch's output: after the initialization checks, the
controller skips it on the producer's own cursor, and the monitors mark it seen for the
producer (`init_checked`).

**Several compositions** share the controller's view, and each completes on its own
(`SysAssert_<id>::on_complete`); the per-composition frames keep them apart.  (A first version
cleared the shared values when any composition reached END, and a composition ending earlier
wiped what another was still checking.)

#### Compositions

**Assertions are checked where they are entered.**  A completion fires its thread's transition,
the cascade fires control points, and the assertions at every place entered on the way are
checked -- the places this completion reached, not every place still marked.  An assertion
"after X" is about the moment X completes; re-checking it at later completions while its place
waits at a join would compare X's outputs with inputs that have moved on.  The monitors check
them the same way.

**A composition may leave threads out.**  Its schema, and its proof, are about the threads it
names: the system VCs reason over the schema's transitions, not over a deployed schedule, so a
thread the schema does not mention is not part of what the composition claims.  At run time
there is a real schedule, and the schedule check passes over the slots of such threads -- a
thread in the composition that runs out of order is still reported.  The run-time assertions
check the real system, so a left-out thread that disturbs what the composition reads still
shows up as a violation.  Codegen warns about exactly that case -- a left-out thread writing a
port of a thread whose values the composition reads: *"Thread `d1p_dst` is not in composition
`partial` but writes `ack`, which `srcp_src` in it reads: the composition's proof does not
account for it, and its run-time checks may fail where `d1p_dst` runs."*  It also warns about a
thread the composition names (`components`) and reads but whose schema never fires it: its
values are read but not tracked.  "Reads" means read by a check: both warnings count only the
aliases a concrete property's assertion uses, not every alias the composition declares -- an
alias no check reads cannot be disturbed.  (`models/SystemTesting`'s `partial` and `head` carry
a property that always holds, `Noted` and `Untracked`, so that each warning has an alias to
fire on.)  (Isolette illustrates both sides: a composition of only the
regulator subsystem leaves out threads that write nothing it reads; one that leaves out `drf`
would draw the warning, since `drf` writes `internal_failure`, which `mrm` reads.)

The monitors need `--runtime-monitoring` and the MCS scheduler.  The GUMBO monitor is injected
when a thread has GUMBO contracts, and a system-assertion monitor for each composition, whether
or not any thread has contracts (D34): the system layer reads ports and state variables, not
the threads' contracts, as in the controller.  Neither needs state variables (D33): the monitors
check a thread's contracts over its ports alone when it has none.  Both are built on the same
scheduling scaffolding -- the user-channel tables and the adapter over the monitor's API --
which the GUMBO monitor plugin generates whether or not the GUMBO layer is there; with no
thread contracts the `observers` crate has only the system layer.

#### What stage 7 does not cover

It tests the *unmonitored* configuration against the contracts; it does not test a monitor PD.
Whether the monitored configuration, with the monitor's slots in the frame, still meets its
contracts, and whether the monitor itself is correct, is a separate open item ("Whether a
variant that keeps *both* the controller and a monitor PD is worth having", under "Open
Items"). The design above is a prerequisite for that variant too:
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
| D5 | Gate on the experimental option `ENABLE_TEST_SCHEDULER` (`-x`, `;`-separated). *Its requirement of `--runtime-monitoring` for models with state variables is superseded by D22; the experimental option itself by the opt-in `--system-testing` (see "Invoking codegen")* | No cligen regen, no public flag while the design is in flux |
| D6 | Stage A (timer-gated) before Stage B (completion-driven) | Stage A needs no component changes at all |
| D7 | Scripted controller before serial CLI | Fewest moving parts to a working step/inspect loop. As it turned out, the scripted controller covers CI on its own, and the serial CLI is deferred indefinitely (see "Interactive CLI (deferred)") |
| D8 | Pilot on temp-control | Already carries 3 variant bundles and 3 threads, and has GUMBO state vars for inspect/mutate |
| D9 | Sequence-number handshake, not a completion flag | Race-free with plain ordered writes |
| D10 | Keep the timer as a watchdog in Stage B; an overrun, like an unreachable target or a rejected command, fails the running test | A thread that overruns should fail its test, not wedge the run. (A thread that never returns still wedges it: it starves the controller, and the host driver's timeout ends the run) |
| D11 | Tests declared in `system_tests!` blocks that emit the registration table -- one block, or one per file joined by `system_test_files!` | No dev-deps on target; no linker-section tricks; the list cannot drift |
| D12 | Recording assertions (`sys_assert_eq!`), not `assert!` | `no_std` + `panic = abort`: no `catch_unwind`, so a panic would end the run |
| D13 | Tests are order-independent with explicit setup | Avoids extending the scheduler protocol with `Reset` and avoids one-test-per-boot |
| D14 | The whole-component setter requires every field; no `Default` on the container | rustc enforces complete setup; a new state var breaks stale tests instead of silently defaulting. Verified: deleting a field gives `E0063: missing field` |
| D15 | Port injection enqueues into the producer's region (controller is an extra writer) | Semantically identical to the real producer; reuses generated queue code; no thread-side change |
| D16 | State var injection via mirrored `inj_sv_X` regions, not bidirectional `sv_X` regions | Queue emptiness is the dirty flag; keeps one-writer-per-region |
| D16a | Those regions are **plugin-declared, not AADL ports** | Model ports leak into the component test harness and the system verification model (for synthetic ports now also filtered by `StoreUtil.modelSymbolTable`), and add regions to the default image; see "The regions must not be AADL ports" |
| D17 | Test selection is a filter string in an ELF section, matched by substring against qualified `suite::test` names; the runner prints the test table, and a list-only flag (`LIST_TESTS=1`) runs nothing | Follows the `user_schedule` objcopy precedent; no manifest to sync; survives reordering; suites and single tests use one mechanism; mirrors `cargo test <filter>`; the host can discover what is runnable |
| D18 | Verdict leaves the target as prefixed serial lines with a **mandatory** `DONE matched=/passed=/failed=/init=` terminator, scraped by a host driver with a hard timeout | Serial is the only channel off-target — `test_status` is guest memory the host cannot read. Every real failure mode is silence, so absence of the terminator must fail rather than pass |
| D19 | Contract checks are generated once, by a `ContractObserverPlugin` that runs when a consumer was injected -- a GUMBO or system-assertion monitor (`--runtime-monitoring`, MCS) or the test controller -- as a component layer and a per-composition system layer, generic over `SystemView` and `ViolationSink`, and shared by the monitor PDs and the controller | The system monitor is already the gumbo monitor plus a layer; making that structural gives one copy of the checks and lets a role be a choice of layers. The monitor plugins are gated on `--runtime-monitoring`, so they cannot own code that system testing needs without it |
| D20 | The checks split into a completion half (`on_complete`) and a dispatch half (`on_dispatch`). The controller runs them at a scheduler park taken just before each user dispatch, gated by `observe` in `test_cmd`, and runs the completion half again at command completion; a completion is run only when the scheduler's `completed_seq` has moved by exactly one (more means dispatches went unobserved, and tracking is dropped) | The lowest-priority controller cannot otherwise run between slots. Parking at dispatch makes the captured pre-state include the test's injections; checking completion at command completion keeps a test's last dispatch inside its own verdict. `completed_seq` identifies a dispatch, which a channel cannot, so a command that dispatched nothing cannot complete a slot twice and desynchronize the marking |
| D21 | A violation is printed as `TEST \| VIOLATION` and fails the running test, which prints exactly one `TEST \| FAIL`; violations are recorded only from a test's `BEGIN` to the end of its body, while tracking continues between tests; `observe::expect` covers guarantees a test breaks on purpose | A violation should be a verdict, not a log line. D18 counts `FAIL` lines against `failed=`, and a violation is found inside an API call, where a test body cannot be returned from. What the runner's normalization dispatches is nobody's verdict |
| D22 | Requesting system testing generates the GUMBO checks when the model has GUMBO thread contracts (integration constraints included, data invariants once implemented) and the system-verification checks when it has a composition, without `--runtime-monitoring`; the `sv_` plumbing follows `--runtime-monitoring` **or** system testing | The user asks for system testing, not for monitoring; the checks belong to it. Superseded D5's requirement at stage 7 step 3. C threads' contracts are included (review fixes) |
| D23 | GUMBO and system-verification checks are separate switches, each on by default when generated. The build-level switch decides whether a layer is live (tracked and parked for); suite- and test-level switches decide only whether a live layer checks, and cannot revive a layer turned off at build level | The layers are independent, so the monitor PDs' fixed combinations need not apply to the controller. Build-level off is a request for the unchecked run, so it restores stage 4-5 behavior with no parks; within a run, tracking regardless of checking keeps re-enabling correct at any point |
| D24 | *Superseded by D36.* A failed CEP_Pre was information in the controller, excusing the dispatch's CEP_Post | -- |
| D25 | The controller's checks read through their own `test_obs_get_*` receive cursors -- with a last-value cache for data ports and state variables, and for an event port one cursor per reader (the producer, each consumer, and the system layer) with event semantics -- never through the tests' `inspect::get_*`. A data port never written reads as its default, as it does to the thread; a state variable never written makes the check skip (step 5) | `inspect::get_*` reads consume; sharing the cursor would let checks and tests consume each other's values. An event is consumed by each reader separately. The thread's own getter returns a zero-initialized last value until something arrives, so the checks must see the same |
| D26 | A completion checks the assertions at the places it entered, not every place still marked | An "after X" assertion is about the moment X completes; re-checking a place waiting at a join compares X's outputs with inputs that have moved on (step 5) |
| D27 | The system layer reads event ports as per-frame values: a producer's output is what its latest dispatch in the frame sent (nothing, if it sent nothing), or what a test injected that no send has replaced; each composition has its own frame, ending when its marking reaches END (`frame_ended(c)`), so START sees only later events, except at initialization, whose outputs belong to the first frame; when no system layer is running, or one resumes, a hyperperiod's frame begins at the previous hyperperiod's last completion (`HP_FRAME_AT`, recorded from `LAST_DONE_AT`); an injection is in the frame it is made in, so a consumer that already ran in this hyperperiod receives it in the next, where the producer's value no longer shows it (noted by an `INFO` line) | What an assertion sees must not depend on which assertion read the event first, or on where it is checked; the controller and the monitors must agree (on the single-element queues that are supported). See "Frames and event values" |
| D28 | A composition's alias of a connected input reads what the reader received, latched at its dispatch | On a feedback edge the producer's output in the frame is not what the consumer got; the input is what the consumer read |
| D29 | A composition may leave threads out: the run-time schedule check passes over their slots, and codegen warns when a left-out thread writes what the composition's checks read. The proofs are unchanged | The VCs are about the schema, not a deployed schedule, so a partial composition is legitimate; only run time holds it against a real schedule, and its assertions still check the real system |
| D30 | After an overrun the system layer resumes only in the next hyperperiod; a loss that is not an overrun may resume at the hyperperiod's first user slot | The aborted dispatch's output reaches consumers in the frame it ran in, which then mixes two dispatches of the thread |
| D31 | The runtime monitors observe unconnected output ports, by mapping the producer's own region, as they do unconnected inputs | A contract may name a port nothing consumes (`MustSend(fwd, ..)`); without it the monitor image did not compile. A thread's output seen before any consumer exists is also what a system test sees |
| D32 | The production image carries no test controller and no system-testing regions (the test scheduler strips the controller and the `sv_` regions from `normal` whenever they survive there), the production build does not compile the controller, and the test variant keeps the pad where the shipped schedule has it | Enabling system testing must not change the image that ships (its protection domains, schedule and regions; map addresses and channel ids may shift), nor break its build; a slot index must mean the same slot in both |
| D33 | The GUMBO and system-assertion monitors are gated on GUMBO thread contracts, not on state variables; the monitors wire `sv_` ports only for the threads that have state variables | The gate came from the plugin's first job, exposing state variables to the userland monitor; once it became the contract monitor it silently left a model whose contracts read only ports with no GUMBO or composition monitor at all. Verified on `models/SystemTesting/stateless` (three monitor images build and report the stub behaviour's CEP_Post violations under QEMU, and nothing else) |
| D34 | A composition's system-assertion monitor needs no thread contracts: the monitors' scheduling scaffolding is generated with or without the GUMBO layer, and the `observers` crate is generated once GUMBOX has run or is known not to (`GumboXRustPlugin.contractsOrNone`) -- without GUMBOX's contracts, only when there is a composition to check | The system layer is independent of the component layer, and contract projection makes a composition over threads without contracts legitimate for the proofs. The sys-assert monitor used to borrow the GUMBO monitor's method, tables and adapter, so a model with no thread contracts got no monitor at all; the fourth review's codegen error for the remaining case ("needs at least one thread with GUMBO contracts") went with the dependency. Verified on `models/SystemTesting/compositions-only` |
| D35 | In the monitor variants a monitor checks a thread's dispatch only once the scheduler reports it, and every earlier dispatch, completed (`sched_state.last_yielded_ch`, `sched_state.outstanding`); after an overrun the GUMBO layer drops its saved pre-states and the system layer is suspended until a later hyperperiod with nothing outstanding.  A channel's count is capped at 2 (a running thread's further dispatches merge into one pending notification), and in these variants a faulted thread's `_MON` answers its dispatches, so neither leaves a count stuck above zero | A timer-driven slot does not wait for its thread, and the monitor shares the threads' priority, so an overran thread was read mid-dispatch and reported as breaking its contract -- seen under QEMU when a host stall cost `d1p_dst` most of its 50 ms slot, and reproduced by making it spin past the slot |
| D36 | A failed CEP_Pre is a contract violation: the sender did not meet the receiver's contract.  The test controller fails the test, as Slang system testing does, and rejects the dispatch -- the scheduler passes the slot without dispatching the thread, and the controller rewinds the cursors its check read; the system layers resume with the next hyperperiod.  A test that does it on purpose declares `Expect::CepPre(thread)`.  The monitors report it but do not reject it, as Slang's runtime monitoring does not (D37) | An input outside a thread's assumptions is the sender's fault, not the thread's; letting it through would test the thread outside its contract.  Treating it as information missed multi-port `compute` assumptions, which `I_Guar => I_Assm` does not cover.  Injected and produced values are treated alike, as in Slang; a robustness test says so explicitly |
| D37 | The monitors check a dispatch's CEP_Post even when its CEP_Pre failed, as Slang's runtime monitoring does.  There is no excuse flag; `CepPostExcused` is gone | Matching Slang keeps the two platforms' monitor output comparable.  The controller is unaffected: it rejects the dispatch, which then never completes |
| D38 | An event port a composition declares an alias for must have a single-element queue on any thread, contracts or not, as a contracted thread's ports must (`MicrokitGumboLinter`).  Both are errors when system testing or `--runtime-monitoring` is requested, and warnings otherwise | The run-time checks read one element per dispatch, which matches the thread only when its queue holds one; the compositions-only models (D34) read ports of threads without contracts, which the thread rule did not reach.  A build with neither has no such reader, and a Verus proof of a thread does not read its queue that way, so it is not refused |

## Plugin structure

The first plan was to subtype `UserLandMonitorPlugin` and adjust it in three places (an
overridable gate in place of the hard-coded `options.runtimeMonitoring`, a way to skip the
slot interleaving the controller does not want, and a `getRetainedNonModelPorts` override).
As built, `TestSchedulerPlugin` is instead a standalone
`@datatype class … extends ModelTransformerPlugin with MicrokitPlugin with MicrokitFinalizePlugin`,
so none of those adjustments were needed:

- **Model transform** -- `TestControllerInjector` adds the controller process and thread, with
  no ports (deliberately not `MonitorInjector`).
- **`handle`** -- C bridge additions through `CConnectionProviderPlugin` (controller wrappers,
  observable-region accessors, stage 7's `test_obs_get_*` cursors, the thread-side `get_inj_sv_*` /
  `is_injection_enabled`), Rust extern declarations and test stubs through `CRustApiPlugin`,
  and `lib.rs` weaving through `CRustComponentPlugin` (`libComputePre` ingest, the controller's
  `system_tests` module). `canHandle` waits for both Rust plugins' contributions to exist. A
  second handle stage adds the `observers` crate dependency once `ContractObserverPlugin` has
  run (stage 7).
- **`finalizeMicrokit`** -- the MSD variant, built from the pre-monitor snapshot (see
  "Relationship to the runtime monitor"), the controller and the `sv_` regions stripped from
  `normal` whenever they survive there, the five-file bundle, the `system_tests/` modules
  (`observe.rs` when the model has contracts) and the host driver.

The only thing it takes from the monitor plugins is `MONITOR_ORIG_MSD_KEY`. Not inheriting
from `GumboMonitorPlugin` also keeps its gate (`--runtime-monitoring` and thread contracts) away
from contract-free models.

It is registered in `MicrokitPlugins.defaultMicrokitPlugins`.

## Invoking codegen

System testing is requested with `--system-testing`, a flag in the CAmkES/Microkit option group
(`CommonCli.scala`), off by default -- opt-in like `--runtime-monitoring`:

```
sireum hamr sysml codegen ... --scheduling UserLand --system-testing
```

It only affects Microkit models with UserLand (MCS) scheduling: on another platform codegen warns
that it is ignored, and on a domain-scheduled system the Microkit linter warns that no
system-testing variant is generated.  These, and the deprecation warning below, are reported
under `CommonUtil.optionWarningKind`, which codegen prints even without `--verbose`.

For the pilot, `sysml/bin/run-hamr.cmd` appends `Os.cliArgs` to its fixed arguments, so the
option passes straight through, and `.ci/ci.cmd` already assembles the codegen option string
(it carries `--platform Microkit --runtime-monitoring --scheduling UserLand ...`).
`--runtime-monitoring` is not needed for system testing (D22), but does no harm: it adds the
monitor variants beside the test variant. Enabling the test scheduler for a model is appending
the option to that line; the golden tests (`MicrokitTests`, the `test_sched_*` cases) set
`systemTesting = T` in their options.

The plugins read only `options.systemTesting`.  The experimental option it replaced,
`-x ENABLE_TEST_SCHEDULER` (`ExperimentalOptions.ENABLE_TEST_SCHEDULER`), is still accepted so
existing scripts keep working: `CodeGen.codeGen` warns that it is deprecated and turns it into
`systemTesting` before any plugin runs, model transforms included.

## Key Files

### This design's own code

| File | Role |
|------|------|
| `microkit/plugins/testing/TestSchedulerPlugin.scala` | The whole plugin: model transform, handle (C bridge wrappers + lib.rs weaving), finalize (MSD variant, bundle, `system_tests/` modules, host driver), and every template |
| `microkit/plugins/testing/TestControllerInjector.scala` | Injects the controller process/thread -- **no ports**, deliberately not `MonitorInjector` |
| `common/util/CommonCli.scala` | `--system-testing` (the option; `HamrCli.scala` and the `CliKeys` are generated from it). `ExperimentalOptions.ENABLE_TEST_SCHEDULER` (D5) remains as a deprecated alias |
| `microkit/plugins/MicrokitPlugins.scala` (jvm) | Registration, before the system description providers |
| `microkit/plugins/gumbo/ContractObserverPlugin.scala` | Stage 7: generates `crates/observers`, the contract checks shared by the monitor PDs and the controller |
| `microkit/plugins/gumbo/StateVarPortsPlugin.scala` | Stage 7 step 3: the `sv_` ports and `is_monitoring_enabled`, for `--runtime-monitoring` or system testing |
| `jvm/.../test/microkit/MicrokitTests.scala` | The golden tests: temp-control (with and without `--runtime-monitoring`) and the contract-free vms model |
| `jvm/.../test/microkit/SystemTestingTests.scala` | Regression tests on `models/SystemTesting` (sysml, the `qemu/` fixtures -- behaviour code and `tests.rs` -- `host/`, and `stateless/`, the model without state variables for D33, and `compositions-only/`, the one without thread contracts for D34): codegen checks, the host scheduler harness, and the QEMU runs (`HAMR_ST_QEMU`; opt-in locally, and run on every push by `CI_linux.yml`'s `microkit-qemu` job in the `jasonbelt/microkit_provers` image) |
| `jvm/.../resources/models/SystemTesting/host/scheduler_harness.c` | Compiles the generated scheduler C on the host with stub Microkit and sDDF-timer headers and drives `notified()` with a controlled clock |

### Generated artifacts (per model, under the Microkit output directory)

| Path | Role |
|------|------|
| `test_scheduler.meta.py`, `.mk`, `scheduler/{src,include}/test_scheduler.*` | The variant bundle, selected by `make CONFIG=test_scheduler.mk` |
| `crates/test_controller/src/system_tests/{mod,api,harness,selection}.rs` | Generated: command API, runner, macros, `TESTS=` filter |
| `crates/test_controller/src/system_tests/inspect.rs` | Generated: `get_`/`put_` per port and state var, plus `channels::` for `run_to_thread` |
| `crates/test_controller/src/system_tests/observe.rs` | Generated when the model has contracts (stage 7): the controller's view and sink, and the `observe::` API |
| `crates/observers/` | Generated when a monitor or the controller hosts the checks (stage 7): the contract checks |
| `crates/test_controller/src/system_tests/tests.rs` | **User-editable**, written once and never overwritten -- the rest of the controller crate, its `Cargo.toml` and app module included, is fully generated (stage 7 step 4) |
| `crates/test_controller/src/system_tests/tests/*_tests.rs` | **User-created**, optional: one file per test class, pulled in by `system_test_files!` in `tests.rs` |
| `components/<thread>/src/<thread>.c` | For a Rust thread with state vars, gains `get_inj_sv_*` + `is_injection_enabled()` whenever `--system-testing` is given (D16); the `inj_sv_*_queue` pointers are `setvar_vaddr` targets, mapped only in the test variant and NULL elsewhere |
| `crates/<thread>/src/bridge/extern_c_api.rs`, `crates/<thread>/src/lib.rs` | Rust side of D16 for threads with state vars: extern declarations, `unsafe_` wrappers and `#[cfg(test)]` stubs in `extern_c_api.rs`; the `libComputePre` ingest in `lib.rs` |
| `bin/run-tests.cmd` | Host driver (D18) |

### Files this design depends on

| File | Why it matters |
|------|----------------|
| `microkit/plugins/msd/SystemDescriptionProvider_MCS.scala` | Default scheduler templates; `meta.py` rendering; `templateContributions` and `templateTailContributions`, each in its own marker region |
| `microkit/plugins/monitors/UserLandMonitorPlugin.scala` | The variant-bundle pattern; `MONITOR_ORIG_MSD_KEY`, the pre-monitor snapshot this variant reads; the monitor variants' scheduler, which reports whether the previous user slot's thread completed (D35) |
| `microkit/plugins/gumbo/GumboMonitorPlugin.scala` | Creator of the `sv_` state var ports and `is_monitoring_enabled` that stage 5 builds on, until stage 7 step 3 moved them to `StateVarPortsPlugin`; now wires the `sv_` ports to the gumbo monitor PD |
| `microkit/plugins/gumbo/GumboXRustPlugin.scala`, `GumboRustPlugin.scala` | The GUMBOX predicates the checks evaluate, for Rust threads and (review fixes) C threads |
| `common/resolvers/GclResolver.scala` | Rewrites GUMBO expressions over ports (`port` -> `api.port`), including `MustSend`'s value |
| `microkit/plugins/c/connections/CConnectionProviderPlugin.scala` | Unconnected-port region creation; `putCConnectionStore`, the route for C bridge additions. **Central to stage 5** |
| `microkit/connections/ConnectionUtil.scala` | `processInPort` / `processOutPort` -- where an unconnected input's region comes from (D15) |
| `microkit/plugins/rust/component/CRustComponentPlugin.scala` | Owns `lib.rs`; `libModDecls` / `libComputePre` / `libComputePost` are how generated code is woven in without re-emitting it. `libComputePre` existed for the R2U2 hook and was made a field any plugin can fill by this work |
| `microkit/plugins/rust/testing/CRustTestingPlugin.scala` | The component-level harness that D11/D14 mirror one level up |
| `microkit/util/MakefileTemplate.scala` | **Two** `system.mk` templates (domain and MCS) -- a change to one usually needs the other, except for the test scheduler's (`TESTS`, `LIST_TESTS`, the check switches, `EXTRA_IMAGES`), which is MCS-only; `CHECK_FLAGS_BOARD_MD5`, the `$(MSD)` rule, `RUST_PROFILE_DIR` |
| `microkit/util/MakefileContainer.scala` | The per-crate link rule, shared by both templates |
| `microkit/util/SystemDescription.scala` | PD / MemoryRegion / Channel model; `templateContributions`, `templateTailContributions` |
| `microkit/plugins/c/components/CComponentPlugin_MCS.scala` | Per-thread PD/channel/region construction; the bridge `.c` carrying the C2 notify-back; the frame budget, which leaves injected threads out |
| `microkit/plugins/monitors/MonitorInjector.scala` | The monitor's observation ports, unconnected inputs and outputs included (D31) |
| `types/include/sb_queue_*.h` | The queue contract D15 rests on: single-sender, broadcast, private per-receiver `Recv_t`, effective depth 1 by default (deeper for a larger `Queue_Size`, allowed only on a contract-free thread's port no composition reads) |
| `art/scheduling/static/{Command,Explorer}.scala` | The JVM vocabulary and stop-before semantics this mirrors |
| `doc/GumboMonitorPlugin-design.md` | State var region design; its "Bidirectional Use Cases" section was corrected by this work |

## Open Items

### Left open


- Whether injection goes through generated setters only, or also exposes raw region writes for
  negative testing.
- `tests.rs` is seeded once and then owned by the developer, but `MicrokitTests` regenerates
  the whole results tree, so the golden baseline can only ever hold the seed. On-target
  coverage beyond the seed lives with the models instead -- isolette's ported suite in
  INSPECTA-models, preserved by its `clean.cmd` and run by its CI -- and, for stage 7,
  `SystemTestingTests`, which installs its own `tests.rs` into a generated tree.  Since
  2026-10-19 hamr-codegen's CI runs `SystemTestingTests` under QEMU on every push (the
  `microkit-qemu` job in `CI_linux.yml`), so that suite is compiled and run on target, not only
  generated.
- Data invariants are not checked: GUMBOX will fold them into the predicates when they are
  implemented (see "What requesting system testing generates").
- The committed INSPECTA trees lag the code whenever it changes: isolette and temp-control were
  last regenerated on 2026-10-20 (INSPECTA-models `aa1d91ee`, through D38).  Regenerating a tree
  whose `meta.py` predates the contribution marker regions needs `clean.cmd`, or a hand merge
  from the `_fixme` copy.

### Decisions still open

None.  Recently settled: the monitors observe a failed CEP_Pre and do not reject the dispatch,
as Slang's runtime monitoring does (D36), and still check its CEP_Post, as Slang does (D37); and event queues deeper than one are out of scope,
since only single-element queues are supported (codegen rejects a deeper one on a thread with
contracts, or on a port a composition aliases, when the run-time checks are generated; D38).

### Known limits

- **A composition has at most 64 places** (the marking is a `u64`).  `MAX_SCHEDULE_SLOTS` is
  128, and `_Static_assert`s check that each command region's struct fits its 4 KB region.
- **After tracking is lost at a park**, the dispatch at that park saves its pre-state from event
  cursors `forget_ran` has just drained, so its completion check can be wrong.  Unreachable from
  tests (only unobserved dispatches lose tracking that way); covered by code review.
- **An overrun on a multicore or budgeted system, or a thread that blocks mid-dispatch**: the
  overran thread's cursors are drained when the controller handles the overrun, so anything the
  late dispatch sends after that is taken for its re-dispatch's output.  On today's
  single-core, unbudgeted images, with components at priority 140 and the controller at 100,
  a CPU-bound overrun has finished and sent before the controller runs; only a dispatch that
  blocked could send later.  (A fix would drain the overran channel's cursors again at the first
  park that re-dispatches it.)
- **A marking that never exactly reaches END** (a non-conformant schedule, e.g. a thread in two
  slots of a frame): its frame never ends -- in the monitors and in the controller alike, since
  `roll_frame` leaves a running system layer's frames to it.  `validate_schedule` reports such a
  schedule at initialization.
- **START after a resume** is checked at the park where the layers resume, after any injection
  made at the stop before it; a normal START is checked at the previous frame's last
  completion.
- **Saturated counts**: an `hstep` or run-to that would cover more than 2^32 slots ends early
  (after about 4x10^9 dispatches).
- **`init_checked` finds a port's producer by its getter's name prefix**, which for an unconnected
  input names the reader; nothing sends on an unconnected input in a monitor build, so it has no
  effect today.
- **`account_completion` assumes the test variant has no non-user, non-padding slot**: its user
  slots and `completed_seq` agree only then.  A non-user slot dispatched between two user slots
  would change `last_dispatched_ch` after the first one's completion, so the next park would
  report "completed unobserved" and suspend the system layers.  No generated schedule has such
  a slot: the test variant strips every injected protection domain, and pads are never
  dispatched.
- **An injection a consumer receives in the next hyperperiod** is not in the producer's frame
  value there, so an assertion relating the consumer's input to the producer's output reports
  it.  Intended, and noted by a `TEST | INFO` line (see "Frames and event values").
- **Slot counts are not portable across `--runtime-monitoring`.**  `sstep` and `run_to_slot`
  count pad slots, and the shipped schedule -- which the test variant follows (D32) -- has its pad
  first when a monitor plugin rebuilt `normal` (with `--runtime-monitoring`, for a model that
  gets a monitor) and last otherwise.  After the runner's `run_to_slot(0)`,
  `sstep(1)` consumes the pad in one build and dispatches the first thread in the other, and
  every slot index shifts by one.  Tests meant to run in both should use `run_to_thread` and
  `hstep`.
- **The vms test-enabled `system.mk`** repeats one `clean` line and has empty `test::` and
  `verus:` targets, from merging the controller's Rust targets into a model with no Rust
  component.  Cosmetic: the production `system.mk` and image are unaffected.

### Only if stage 6 is ever revived

- **UART contention**, and ahead of it the question of whether an interactive CLI is worth
  adding serial PDs to the schedule under test. Both are written up under "Interactive CLI
  (deferred)". Nothing built depends on either.

### Not blocking anything

- Slash scripts exit **252** rather than 1 on failure -- the host driver, and equally the
  models' `.ci/ci.cmd` (isolette's CI reports `FAILED (exit 252)` for a `make test` failure).
  `Os.exit(1)` is remapped somewhere in the Slash launcher, the same class of thing as the `23`
  already noted in `run-hamr.cmd`. Nonzero is correct for `if ! ./run-tests.cmd`, but an exact
  code needs chasing.
- D18's count-reconciliation rule is still unexercised: it needs serial output to be lost
  mid-run, which cannot be staged cheaply. The mandatory-`DONE` and `matched=` rules are verified
  on target, and `init=` by stage 7's `init=failed` run.
- Whether a predefined command script loaded at init from a memory region is worth having
  alongside the compiled-in Rust script -- it would let one image run several test sequences.
- `timeout-minutes` for INSPECTA's `hamr-codegen-linux` workflow: a hang in the Sireum build
  ran to GitHub's 6 h limit on 2026-09-23.
- Stale `src/gumbox/` directories are left in monitor crates of trees regenerated without
  `clean.cmd`.
- Whether the test scheduler should optionally retain real timer budgets for mixed
  real-time/stepped scenarios, or whether that is better served by the default variant.
- Whether a VM guest's vCPU priority can starve the priority-100 test controller in a model
  with VMs (e.g. `vms/data_receiver`'s test variant): not checked.
- Whether a variant that keeps *both* the controller and a monitor PD is worth having. Stage 7
  checks contracts against the unmonitored configuration; testing the monitored one -- does
  the system still meet its contracts with the monitor's slots in the frame, and is the monitor
  itself right? -- is a different and legitimate question that the one-injected-PD-per-variant
  rule currently forecloses. Stage 7's steps are prerequisites; the remaining work is listed in
  "What stage 7 does not cover".
