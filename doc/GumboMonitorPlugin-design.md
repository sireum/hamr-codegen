# GumboMonitorPlugin Design: State Variable Access for Runtime Monitoring

## Problem

The existing runtime monitor (created by `RustScheduleAwareMonitorPlugin` via `MonitorInjector`) is connected to every outgoing thread port and can observe port values. To check GUMBO pre/post conditions, the monitor also needs access to each thread's **state variables** (e.g., `lastCmd: On_Off` in the isolette's `thermostat_mt_ma_ma`).

All threads and the monitor run in separate seL4/Microkit protection domains. seL4 provides **no debug-mode introspection** of another PD's private memory — isolation is absolute. The only cross-PD data sharing mechanism is explicitly declared shared memory regions.

## Proposed Solution: Port-Based State Variable Exposure

### Overview

A new `GumboMonitorPlugin` post-processes the model returned by `MonitorInjector.inject` to add port-based state variable exposure. The approach reuses the existing port/shared-memory infrastructure rather than introducing a separate mechanism:

1. For each thread with GUMBO state vars, add **output data ports** to the source thread (one per state var)
2. Add corresponding **input data ports** to the monitor thread
3. The ports are left **unconnected** in the AADL connection graph
4. Existing codegen handles the rest: unconnected output ports on normal threads automatically get shared memory regions; the GumboMonitorPlugin wires those regions into the monitor PD at MSD generation time

### Plugin Architecture

`GumboMonitorPlugin` is a separate plugin that runs **after** `RustScheduleAwareMonitorPlugin`:

```
RustScheduleAwareMonitorPlugin.handleModelTransform
  └── MonitorInjector.inject(model, symbolTable, ...)
      └── returns (ir.Aadl, Store) with monitor thread wired to all outgoing ports
  └── ModelUtil.resolve → updated SymbolTable

GumboMonitorPlugin.handleModelTransform  (runs next)
  └── takes the already-resolved model + SymbolTable
  └── iterates threads, finds GCL state vars via symbolTable.annexClauseInfos
  └── adds output data ports to source threads/processes
  └── adds input data ports to monitor thread/process
  └── adds delegation connections inside both processes
  └── ModelUtil.resolve → updated SymbolTable
  └── returns (Store, Aadl, AadlTypes, SymbolTable)
```

The plugin gates on `RustScheduleAwareMonitorPlugin` having completed its model transform (via its store key). If GUMBO monitoring is not needed, the plugin simply doesn't run and `MonitorInjector`'s output passes through unchanged.

### How State Vars Are Found

Each thread's GUMBO state variables are accessible via:

```scala
symbolTable.annexClauseInfos.get(threadPath) match {
  case Some(clauses) =>
    val gclInfo = clauses(0).asInstanceOf[GclAnnexClauseInfo]
    val stateVars: ISZ[GclStateVar] = gclInfo.annex.state
    // each GclStateVar has .name (String) and .classifier (String)
}
```

This is the same path used by `GumboRustPlugin` (via `GumboRustUtil.getGumboSubclauseOpt`).

### Port Injection (Model Transform Phase)

For each thread with GUMBO state vars, the plugin adds ports at both thread and process levels, plus delegation connections. All ports are **unconnected** — no system-level connections or connection instances are created.

**Source thread side** (e.g., `thermostat_mt_ma_ma` with state var `lastCmd: On_Off`):

- Output data port on thread: `sv_lastCmd` with classifier `On_Off`
- Output data port on process: `sv_lastCmd` (same name, process-level boundary)
- Delegation connection inside process: thread.`sv_lastCmd` → process.`sv_lastCmd`

**Monitor side**:

- Input data port on monitor thread: `thermostat_mt_ma_ma_sv_lastCmd`
- Input data port on monitor process: `thermostat_mt_ma_ma_sv_lastCmd`
- Delegation connection inside monitor process: process port → thread port

### How Shared Memory Regions Are Created

The key insight is how `CConnectionProviderPlugin` handles unconnected ports (lines 103–136):

1. **Normal threads**: Unconnected output ports get full shared memory regions + C queue wrappers + put APIs automatically. The source thread's `sv_lastCmd` output port triggers all of this.

2. **Plugin-generated threads** (the monitor): Unconnected input ports get C type/API scaffolding (get functions, type definitions) but **no shared memory regions** — those are suppressed via the `isPluginThread` check. The comment says: "those ports are wired at the meta.py template level, not via HAMR queues."

This is exactly the split we need: the source side gets memory regions automatically, and the GumboMonitorPlugin wires those regions into the monitor PD at MSD generation time.

### MSD Wiring (Handle Phase)

In its `handle` phase (after model transform), the GumboMonitorPlugin:

1. Finds the shared memory regions created for each source thread's state var output ports
2. Maps those same regions into the monitor PD with read permissions
3. Connects them to the monitor's corresponding input ports via `setvar_vaddr`

The naming convention is the glue: `sv_` prefix on source threads maps to `{process}_{thread}_sv_{name}` on the monitor.

### Naming Conventions

| Artifact | Pattern | Example |
|----------|---------|---------|
| Source thread output port | `sv_{stateVarName}` | `sv_lastCmd` |
| Monitor input port | `{srcProcess}_{srcThread}_sv_{stateVarName}` | `thermostat_mt_ma_ma_sv_lastCmd` |
| Shared memory region | (auto-generated from output port path) | — |

## Bidirectional Use Cases

Because the shared memory regions can be mapped `rw` for both PDs:

1. **Post-condition checking**: Monitor reads state vars + output ports after a thread runs, evaluates GUMBO post-conditions
2. **Pre-condition checking**: Monitor reads the receiving thread's state vars + incoming port values, evaluates GUMBO pre-conditions
3. **Unit testing**: A test monitor mutates state vars before dispatching the thread, then checks outputs against expected post-conditions
4. **Fault injection**: A monitor could inject invalid state to test error handling

Different monitor implementations share the same infrastructure — they differ only in what they do with the get/put APIs during their timeslice.

## Why Not seL4 Debug Facilities?

Confirmed (April 2026) that seL4/Microkit provides no applicable runtime monitoring:

- **Fault handler**: Parent PDs can read child registers via `seL4_TCB_ReadRegisters`, but only on fault — gives CPU registers, not application state vars in heap/stack
- **Debug syscalls** (`seL4_DebugDumpScheduler`, `seL4_DebugSnapshot`): Kernel-level console dumps, not programmatic access
- **Benchmark mode**: Exports hardware PMU for performance counters only
- **No cross-PD memory read**: No syscall exists to read another PD's private memory. This is fundamental to seL4's isolation guarantee with no debug escape hatch.

## Timing Safety

The static ARINC schedule guarantees temporal separation — the monitor's timeslice runs after the thread's timeslice completes. No locks or atomics are needed. This is the same safety argument used for the existing port shared memory regions.

## Key Files

| File | Role |
|------|------|
| `codegen/.../plugins/monitors/RustScheduleAwareMonitorPlugin.scala` | Existing plugin — calls `MonitorInjector.inject`, handles model transform + MSD generation |
| `codegen/.../plugins/monitors/MonitorInjector.scala` | Injects monitor thread into AIR model, creates ports + connections for outgoing thread ports |
| `codegen/.../plugins/c/connections/CConnectionProviderPlugin.scala` | Processes connected and unconnected ports — creates shared memory regions (suppressed for plugin-generated threads) |
| `codegen/.../plugins/rust/gumbo/GumboRustPlugin.scala` | Existing GUMBO plugin — shows how to access GCL state vars per thread |
| `codegen/.../plugins/rust/gumbo/GumboRustUtil.scala` | `getGumboSubclauseOpt` — retrieves GCL annex clause info from SymbolTable |
| `air/.../ir/GumboAST.scala` | `GclStateVar` definition (`.name`, `.classifier`); `GclSubclause.state: ISZ[GclStateVar]` |
| `codegen/.../common/symbols/AadlSymbols.scala` | `GclAnnexClauseInfo` — wraps `GclSubclause` + `GclSymbolTable` |

## Implementation Status (updated 2026-05-03)

### Done

1. **Model Transform** — `GumboMonitorPlugin.handleModelTransform` adds synthetic `sv_` output ports to threads with GUMBO state vars, corresponding input ports to the monitor thread, and delegation connections inside both processes. Gates on `RustScheduleAwareMonitorPlugin`'s store key.
2. **MSD Wiring** — `GumboMonitorPlugin.handle` wires the source thread's shared memory regions into the monitor PD with read permissions.
3. **Rust extern_c_api.rs** — `GumboMonitorPlugin.handle` generates `is_monitoring_enabled` (extern C declaration, unsafe wrapper, test mock with `MONITORING_ENABLED` lazy_static). No separate get/put_state_var functions — uses the existing `put_sv_X` wrappers that CRustComponentPlugin already generates for the synthetic ports.
4. **Rust lib.rs replacement** — `GumboMonitorPlugin.finalizeMicrokit` emits a replacement `lib.rs` (via resource override) that tracks a `monitoring_enabled` flag and calls `unsafe_put_sv_X` after both `_initialize` and `_timeTriggered`, conditional on `monitoring_enabled`. Non-monitoring threads are unaffected.
5. **Default codegen is monitoring-clean** — `CRustComponentPlugin` is not modified. The default `lib.rs` template, meta.py template, and system description contain no monitoring-conditional code. Monitor plugins add/remove their own infrastructure.
6. **sched_state/sched_schedule regions moved to monitor plugin** — Removed from the default MCS template (`SystemDescriptionProvider_MCS`) and `CComponentPlugin_MCS`. Moved to `RustScheduleAwareMonitorPlugin` via a new `SystemDescription.templateContributions: ISZ[ST]` field that allows plugins to inject Python code into meta.py per MSD variant.
7. **All 31 Microkit tests pass** (4 ignored).

### Open Items

- Monitor-side consumption of state var data (the monitor PD can read the `sv_` shared memory regions but no codegen yet for how it processes them)
- GUMBO contract evaluation in the monitor (pre/post condition checking using state var + port data)
