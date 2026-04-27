# Test Scheduler Design: Command-Driven Scheduling for Microkit System Testing

## Problem

On the JVM, HAMR provides a static scheduler with a command API (`art.scheduling.static`) that lets users step through the schedule slot-by-slot or hyperperiod-by-hyperperiod, inspect system state (ports, component state), inject mutations, and observe results. This has proven valuable for system-level testing. Example workflow:

1. Run the schedule for 2 hyperperiods
2. Inject mutations to ports or state vars
3. Run for another hyperperiod
4. Observe and check results

There is no equivalent for Microkit projects. The current MCS user-land scheduler (`scheduler.c`) free-runs on timer interrupts — it advances automatically through timeslices with no external control.

## Proposed Solution: Command-Driven Test Scheduler

Replace the timer-driven scheduler with a command-driven variant for testing. The test scheduler receives commands from a test controller PD (or interactive CLI) via shared memory and channels, executes them by stepping through the schedule, and reports back when done.

### Why This Works

The architecture is favorable for this approach:

- **Threads are passive PDs**: They run only when notified by the scheduler and notify back when done. They don't need to change at all.
- **The scheduler is user-land code**: It's just a C program in a PD — nothing prevents a different implementation with different behavior.
- **Timer is optional for testing**: The current scheduler uses `sddf_timer_set_timeout` to enforce real-time timeslice budgets. For testing, dispatch order matters but real-time budgets don't. The test scheduler can drop the timer entirely and use a synchronous notify/wait-for-reply pattern, which is simpler and deterministic.
- **Same MSD mechanism**: Users select the test scheduler by pointing to a test variant of the MSD (e.g., `meta.test.py`), following the same pattern as `meta.monitor.py` for runtime monitoring.

## Architecture

### Components

```
┌──────────────────┐    commands     ┌──────────────────┐   notify     ┌─────────────┐
│ Test Controller  │───(shared mem)──│  Test Scheduler  │───(channel)──│  Thread PD  │
│ (or CLI/Serial)  │◄──(shared mem)──│                  │◄──(channel)──│  (passive)  │
└──────────────────┘    status       └──────────────────┘              └─────────────┘
                                           │                          ┌─────────────┐
                                           ├──────────────────────────│  Thread PD  │
                                           │                          └─────────────┘
                                           │                          ┌─────────────┐
                                           └──────────────────────────│  Thread PD  │
                                                                      └─────────────┘
```

### Test Scheduler PD

A variant of `scheduler.c` that:

1. **Incoming command port** (shared memory, from test controller): receives commands
2. **Outgoing status port** (shared memory, to test controller): publishes schedule state and command completion
3. **Thread dispatch**: same `microkit_notify(ch)` mechanism as the current scheduler
4. **No timer-driven advance**: instead of `sddf_timer_set_timeout`, waits for each thread's completion notification, then checks the command's stop condition

Core loop (pseudocode):

```c
void execute_command(test_command_t *cmd) {
    switch (cmd->type) {
        case CMD_HSTEP:
            // step cmd->count hyperperiods
            for (int hp = 0; hp < cmd->count; hp++) {
                int remaining = num_timeslices - current_timeslice;
                for (int i = 0; i < remaining; i++) {
                    dispatch_and_wait(current_timeslice);
                    advance_timeslice();
                }
            }
            break;

        case CMD_SSTEP:
            // step cmd->count slots
            for (int i = 0; i < cmd->count; i++) {
                dispatch_and_wait(current_timeslice);
                advance_timeslice();
            }
            break;

        case CMD_RUN_TO_THREAD:
            // advance until the named thread's slot is next
            while (timeslice_ch[current_timeslice] != cmd->target_ch) {
                dispatch_and_wait(current_timeslice);
                advance_timeslice();
            }
            break;

        case CMD_RUN_TO_HP:
            // advance until hyperperiod number matches
            while (hyperperiod_num < cmd->target_hp) {
                dispatch_and_wait(current_timeslice);
                advance_timeslice();
            }
            break;
    }
    // signal completion to test controller
    publish_status();
    microkit_notify(test_controller_ch);
}

void dispatch_and_wait(uint32_t timeslice) {
    microkit_channel ch = timeslice_ch[timeslice];
    if (ch != 0) {
        microkit_notify(ch);
        // thread is passive — it runs, completes, and notifies back
        // the scheduler receives the reply in its next notified() call
    }
}
```

### Command Vocabulary

Modeled after the JVM `Command.scala` (`art.scheduling.static`):

| Command | Description | JVM Equivalent |
|---------|-------------|----------------|
| `Sstep(n)` | Step `n` schedule slots | `Sstep(n)` |
| `Hstep(n)` | Step `n` hyperperiods | `Hstep(n)` |
| `RunToThread(ch)` | Run until the given thread's slot is reached | `RunToThread(name)` |
| `RunToSlot(n)` | Run until slot number `n` | `RunToSlot(n)` |
| `RunToHP(n)` | Run until hyperperiod `n` | `RunToHP(n)` |
| `RunToState(hp, slot)` | Run until specific (hyperperiod, slot) | `RunToState(hp, slot)` |
| `InfoState` | Query current schedule state | `Infostate` |
| `InfoSchedule` | Query full schedule | `Infoschedule` |
| `Stop` | End test session | `Stop` |

Commands are encoded as a simple struct in the shared memory region:

```c
typedef struct {
    uint32_t type;       // command enum
    uint32_t count;      // for Sstep/Hstep
    uint32_t target_ch;  // for RunToThread
    uint32_t target_hp;  // for RunToHP/RunToState
    uint32_t target_slot; // for RunToSlot/RunToState
} test_command_t;

typedef struct {
    uint32_t current_timeslice;
    uint32_t hyperperiod_num;
    uint32_t command_complete; // flag: 1 when command finishes
    uint32_t last_dispatched_ch;
} test_status_t;
```

### Test Controller PD (Programmatic)

A PD that runs a scripted test sequence. Workflow:

```
1. Write command to shared memory (e.g., Hstep(2))
2. Notify scheduler
3. Wait for scheduler's "command complete" notification
4. Read status from shared memory
5. Read port values / state vars via shared memory (using GUMBO monitor infrastructure)
6. Optionally mutate state vars or port values
7. Repeat from step 1
```

The test controller is where assertions live — it checks GUMBO contracts, validates port values, etc. This is the Microkit equivalent of JVM test code that calls `Explorer.stepSystemNHPIMP(2)` and then inspects bridges.

### Interactive CLI Mode

For interactive exploration, a serial/UART PD can stand in for the test controller:

```
┌──────────┐  chars   ┌────────────┐  commands   ┌──────────────────┐
│ UART/    │──────────│ Serial CLI │─────────────│  Test Scheduler  │
│ Console  │◄─────────│ PD         │◄────────────│                  │
└──────────┘  output  └────────────┘  status     └──────────────────┘
```

- **Serial CLI PD**: Reads characters from the UART driver, parses text commands (e.g., `h 2`, `s 5`, `run thread ma_ma`), encodes them as `test_command_t`, writes to shared memory, notifies the scheduler. On completion, reads status and prints results.
- **Parsing**: The JVM `CliCommandProvider` is simple string matching — straightforward to implement in C or Rust.
- **Microkit serial support**: The sDDF serial driver framework already exists. The current scheduler already uses `sddf_dprintf` for output.
- **Latency**: Each command involves cross-PD notifications, but for interactive debugging this is imperceptible.

While the scheduler is paused waiting for the next command, no threads run — which is exactly the behavior wanted for step-by-step exploration.

## Integration with GUMBO Monitor Infrastructure

The test scheduler works naturally with the shared-memory state var infrastructure from the GumboMonitorPlugin design:

1. **Test controller sends** `Hstep(2)` to scheduler
2. Scheduler dispatches threads through 2 hyperperiods. After each thread dispatch, the thread writes its state vars to the shared memory region (per the GUMBO monitor design).
3. Scheduler signals completion to test controller
4. **Test controller reads** port values (from port memory regions) and state vars (from state var memory regions)
5. **Test controller writes** mutated state vars to the shared memory region (bidirectional, per GUMBO monitor design)
6. **Test controller sends** next command

The `monitoring_enabled` flag in each thread's `lib.rs` would be `true` when the test MSD is used (since the state var memory regions are mapped), enabling the get-before/put-after pattern automatically.

## System Description

The test MSD (`meta.test.py`) would:

1. Replace the timer-driven scheduler PD with the test scheduler PD
2. Add the test controller PD (or serial CLI PD)
3. Add shared memory regions for command and status between test controller and scheduler
4. Add channels between test controller and scheduler
5. Include the state var memory regions (same as monitor MSD) for port/state inspection
6. Adjust the schedule to include test controller timeslices (or make it passive and command-driven)

## Differences from JVM Static Scheduler

| Aspect | JVM | Microkit |
|--------|-----|----------|
| Execution model | In-process: `Explorer` directly calls `bridge.compute()` | Cross-PD: scheduler notifies passive thread PDs |
| State inspection | Direct: read bridge port variables in same JVM | Shared memory: read mapped memory regions |
| State mutation | Direct: write to bridge port variables | Shared memory: write to state var / port regions |
| CLI | In-process Scala REPL / `CliCommandProvider` | Separate serial CLI PD over UART |
| Timing | No real-time constraints | Timer optional; synchronous dispatch for determinism |
| Thread code changes | None | None — threads are passive PDs regardless of scheduler |

## Key Files

| File | Role |
|------|------|
| `scheduler/src/scheduler.c` | Current MCS scheduler — template for test scheduler |
| `art/scheduling/static/Explorer.scala` | JVM Explorer — command execution logic to replicate |
| `art/scheduling/static/Command.scala` | JVM command vocabulary — defines the command set |
| `art/scheduling/static/CommandInterpreter.scala` | JVM command interpreter — dispatch logic |
| `art/scheduling/static/Schedule.scala` | JVM schedule structure and invariants |
| `art/scheduling/static/StateObserver.scala` | JVM state inspection — port/component state display |
| `meta.monitor.py` / `meta.py` | Existing MSD variants — pattern for `meta.test.py` |
| `RustModelTransformerPlugin.scala` | Plugin that creates MSD variants — pattern for test scheduler plugin |
| `GumboMonitorPlugin-design.md` | State var shared memory design — reused by test scheduler |

## Open Items

- Define the exact shared memory layout for commands and status (endianness, alignment, atomicity)
- Determine whether the test controller and monitor should be the same PD or separate PDs that share state var regions
- Decide whether the serial CLI PD should be a separate component or built into the test controller
- Consider whether predefined test scripts (loaded at init from a memory region) would be useful in addition to interactive/programmatic modes
- Determine how `InfoInputs` / `InfoOutputs` / `InfoComponentState` queries work — the test controller needs to know which memory regions correspond to which ports, which could be published by the scheduler at init (similar to the current `sched_schedule`)
- Consider whether the test scheduler should optionally retain timer support for mixed real-time/stepped testing scenarios
