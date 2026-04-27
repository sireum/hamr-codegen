# GumboMonitorPlugin Design: State Variable Access for Runtime Monitoring

## Problem

The existing runtime monitor (created by `RustModelTransformerPlugin`) is connected to every outgoing thread port and can observe port values. To check GUMBO pre/post conditions, the monitor also needs access to each thread's **state variables** (e.g., `lastCmd: On_Off` in the isolette's `thermostat_mt_ma_ma`).

All threads and the monitor run in separate seL4/Microkit protection domains. seL4 provides **no debug-mode introspection** of another PD's private memory — isolation is absolute. The only cross-PD data sharing mechanism is explicitly declared shared memory regions.

## Proposed Solution: Shared Memory Regions for State Vars

### Overview

A new `GumboMonitorPlugin` (modeled after `RustModelTransformerPlugin`) would:

1. For each thread with GUMBO state vars, add a `memory_region` to the **monitor MSD** (e.g., `thermostat_mt_ma_ma_state_vars_mr`)
2. Map the region `rw` into both the thread PD and the monitor PD
3. Generate C and Rust infrastructure for reading/writing state vars through the region
4. The **normal MSD** has no such region — this is the enable/disable toggle

Users enable runtime monitoring by pointing to the monitor version of the MSD (e.g., `meta.monitor.py` instead of `meta.py`), which is the existing pattern.

### Zero Overhead When Monitoring Is Disabled

The same generated code works for both configurations with no runtime cost when monitoring is off:

- **C side**: State var pointers are initialized to `NULL`. The Microkit loader patches them via `setvar_vaddr` only when the monitor MSD provides the memory region mapping.
- **Rust side**: On initialization, `lib.rs` calls a single FFI function to check if the pointer is non-null, and stores the result in a `static mut monitoring_enabled: bool`. All subsequent state var get/put calls are guarded by this boolean — no FFI crossing when monitoring is off.

### Rust Integration (`lib.rs`)

State var reads/writes are placed in the generated `lib.rs` (not in user code):

```rust
static mut monitoring_enabled: bool = false;

pub extern "C" fn thermostat_mt_ma_ma_initialize() {
    unsafe {
        monitoring_enabled = extern_c_api::is_monitoring_enabled();

        let mut _app = thermostat_mt_ma_ma::new();
        _app.initialize(&mut init_api);

        if monitoring_enabled {
            extern_c_api::put_state_var_lastCmd(_app.lastCmd);
        }

        app = Some(_app);
    }
}

pub extern "C" fn thermostat_mt_ma_ma_timeTriggered() {
    unsafe {
        if let Some(_app) = app.as_mut() {
            // Monitor may have mutated state vars (e.g., for unit testing)
            if monitoring_enabled {
                _app.lastCmd = extern_c_api::get_state_var_lastCmd();
            }

            _app.timeTriggered(&mut compute_api);

            // Write state vars for monitor to observe
            if monitoring_enabled {
                extern_c_api::put_state_var_lastCmd(_app.lastCmd);
            }
        }
    }
}
```

### C Backend

Per-state-var functions avoid struct construction overhead:

```c
// Patched by setvar_vaddr when monitor MSD is used; stays NULL otherwise
volatile state_vars_t *state_vars_region = NULL;

bool is_monitoring_enabled(void) {
    return state_vars_region != NULL;
}

void put_state_var_lastCmd(On_Off value) {
    if (state_vars_region != NULL) {
        state_vars_region->lastCmd = value;
    }
}

On_Off get_state_var_lastCmd(void) {
    return state_vars_region->lastCmd;
}
```

### System Description (Monitor MSD)

The monitor MSD adds the state var memory region and maps it into both PDs:

```python
# State var memory region for thermostat_mt_ma_ma
ma_ma_state_vars_mr = MemoryRegion("thermostat_mt_ma_ma_state_vars_mr", size=0x1000)

# Thread PD: rw access, setvar_vaddr patches the C pointer
thermostat_mt_ma_ma.add_map(Map(ma_ma_state_vars_mr, 0x10_00A_000,
    perms="rw", setvar_vaddr="state_vars_region"))

# Monitor PD: rw access (bidirectional for unit testing)
monitor_thread.add_map(Map(ma_ma_state_vars_mr, 0x10_00X_000,
    perms="rw", setvar_vaddr="thermostat_mt_ma_ma_state_vars_region"))
```

The normal MSD has none of this, so the pointer stays `NULL` and `monitoring_enabled` is `false`.

## Bidirectional Use Cases

Because the region is `rw` for both PDs:

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
| `codegen/.../plugins/monitors/RustModelTransformerPlugin.scala` | Existing plugin — template for GumboMonitorPlugin |
| `isolette/.../thermostat_mt_ma_ma/src/component/thermostat_mt_ma_ma_app.rs` | Example thread with GUMBO state var `lastCmd` |
| `isolette/.../thermostat_mt_ma_ma/src/lib.rs` | Where state var get/put calls would be inserted |
| `base_type/.../monitor_process_monitor_thread/src/component/monitor_process_monitor_thread_app.rs` | Example monitor implementation |
| `base_type/.../monitor_process_monitor_thread.c` | C backend showing shared memory pointer + extern pattern |
| `meta.monitor.py` / `meta.py` | Monitor vs normal MSD configs |

## Open Items

- Determine how GUMBO state vars are represented in the symbol table / AIR for the plugin to read
- Define the `state_vars_t` struct layout convention (field ordering, alignment, padding)
- Decide naming conventions for the generated memory regions, C functions, and Rust extern declarations
- Consider whether the plugin should be a separate `@sig trait` or extend the existing `RustModelTransformerPlugin`
