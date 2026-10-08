# Exact Time Design: Carrying the Model's Time Values to Every Backend Without Silent Loss

## Problem

GitHub issue [sireum/hamr-codegen#12](https://github.com/sireum/hamr-codegen/issues/12)
reports that a periodic thread with `Period => 100 us` generates
`dispatchProtocol = Periodic(period = 0)` on the Linux backend, with no diagnostic. The cause is
not specific to that backend or to `Period`: **codegen converts every time property to whole
milliseconds by integer division, and every backend then assumes milliseconds.** Anything below
1 ms becomes 0 and everything else is floored, for `Period`, `Frame_Period`, `Clock_Period` and
`Compute_Execution_Time` alike.

A zero time is never what the user meant, and every backend misbehaves on it: ART dispatches in a
busy loop, CAmkES takes a modulo by zero in generated C, Microkit domain scheduling rejects the
system description, and Microkit MCS programs a zero-length timeslice. A floored time is quieter
but equally wrong: the user's schedule is silently changed.

**Goal: never change the user's time values silently.** Codegen carries each value exactly, in
the finest unit there is, and converts it once, at the boundary of each backend, to the finest
unit that backend can represent. An exact conversion is silent. An inexact one rounds to nearest
and warns, naming the value written and the value used. A conversion that would produce 0 is an
error.

This document records the findings (F1-F15), the design decisions (D1-D11), the per-backend
changes, the test plan and the open questions. **Status: design agreed (2026-10-05) and revised
after six review rounds and six full reviews; ready to implement.**

## Findings

### F1. AIR already carries picoseconds

The AADL property set `Time_Units` has `ps` as its base unit (`ps, ns, us, ms, sec, min, hr`).
Both AIR producers already normalise every time value to it:

- **OSATE** (`osate-plugin/org.sireum.aadl.osate/src/main/java/org/sireum/aadl/osate/architecture/Visitor.java:881-889`):
  `NumberValueOperations.getScaledValue(nv)` with `UnitLiteralOperations.getAbsoluteUnit(...)`,
  serialised as `Double.toString(v)`. So `100 us` reaches AIR as
  `UnitProp(value = "1.0E8", unit = Some("ps"))`. The checked-in test models' `.slang/*.json`
  files use only `"ps"` for times, with values such as `"1.0E12"`, `"2.0E9"` and `"1.0000E+12"`.
- **SysML** (`hamr/sysml/frontend/jvm/src/main/scala/org/sireum/hamr/sysml/instantiation/Instantiate.scala:654-683`):
  also always emits `unit = Some("ps")`, scaling SI and `HAMR_Time_Units` values to picoseconds
  with Slang `R` arithmetic. `R` wraps a Scala `BigDecimal` (`runtime/.../R.scala:83`), whose
  default math context is `DECIMAL128` (34 significant digits), so scaling a model value by powers of ten is
  exact for any realistic input. `InstantiateUtil.scala:240-272` requires a unit on `Period`,
  `Frame_Period`, `Clock_Period`, `Timing_Period` and the `Compute_Execution_Time` range.

The only non-picosecond times codegen sees are ones it injects itself (F6). The OSATE values are
doubles, which is not exact (F8).

### F2. Codegen throws the precision away in one place

`PropertyUtil.convertToMS` (`shared/.../common/properties/PropertyUtil.scala:203-223`) parses the
value with `R(value)`, truncates it with `conversions.R.toZ`, then divides:
`case "ps" => _v / (z"1000" * z"1000" * z"1000")`. With picosecond input, every time below 1 ms
becomes 0 and every other time is floored to whole milliseconds. Its callers are the only way
codegen reads times:

| Accessor | Location | Unit returned |
|---|---|---|
| `PropertyUtil.getPeriod(c)` | `PropertyUtil.scala:151-159` | ms; fills `period` on `AadlThreadOrDevice` and `AadlVirtualProcessor` (`SymbolResolver.scala:368,472`); also called directly by CAmkES (`act/periodic/PeriodicDispatcher.scala:80`) |
| `Processor.getFramePeriod()` | `AadlSymbols.scala:103-111` | ms |
| `Processor.getClockPeriod()` | `AadlSymbols.scala:113-121` | ms |
| `AadlThreadOrDevice.getComputeExecutionTime()` | `AadlSymbols.scala:239-252` | ms, low and high |
| `getMaxComputeExecutionTime()` | `AadlSymbols.scala:254-260` | ms; 0 if absent |
| `Processor.getSlotTime()` | `AadlSymbols.scala:127-129` | **unit ignored** (`getUnitPropZ`), so a raw picosecond count |
| `CommonUtil.getPeriod(m)` | `CommonUtil.scala:149-155` | ms; **1 if absent** |

`period` itself is declared on the trait `AadlDispatchableComponent` (`AadlSymbols.scala:154`), on
`AadlThreadOrDevice` (`:237`) and on the concrete `AadlVirtualProcessor`, `AadlDevice` and
`AadlThread` (`:182,309,321`). Its readers include the CAmkES pacers (`Pacer.scala:683-684`,
including a virtual processor's `dc.period`; `SelfPacer.scala:224`;
`PacerTemplate.pacerScheduleThreadPropertyComment`). None of these accessors is used outside
`hamr/codegen` in kekinian.

No linter checks time values: `Linter.scala:34-35`, `PacerUtil.scala:148-156` and
`MicrokitLinterPlugin.scala:72-77,116` check only that they are present. The one value check in
the codebase is Microkit's `length <= 0` error on domain schedule entries
(`SystemDescriptionProvider_DomainSchduler.scala:57-59`).

### F3. Each backend's native resolution

| Backend | Uses today | Finest it can use |
|---|---|---|
| ART on the JVM | ms: `System.currentTimeMillis`, `Thread.sleep(ms)` | ns: `System.nanoTime` |
| ART in Linux nix (transpiled C) | ms: `gettimeofday`→ms, `usleep(n*1000)` | ns: `clock_gettime`, `nanosleep` |
| ART in seL4 nix (CAmkES + Slang) | no ART clock (F4); only the `Periodic` literal | ns (the literal) |
| Microkit domain scheduling | ms, rendered as `"${length * 1000} us"` | **us** in the system description: Microkit 2.3 `schedule_entry` accepts only integer, non-zero `us` or `ticks` (Microkit 2.3.1, `tool/microkit/src/sdf/domains.rs:286-305`). What the kernel does with it is F12 |
| Microkit MCS / user-land | ms × 1,000,000 → ns timeslices | **ns**: the runtime already programs `sddf_timer_set_timeout` in ns |
| CAmkES dispatcher | 1 ms calendar ticks (`PeriodicDispatcherTemplate.scala:35-41,61,78-92`) | ms |
| CAmkES pacer domain schedule | `Clock_Period` ticks (F11) | `Clock_Period` ticks |
| ROS 2 C++ and micro-ROS | `std::chrono::milliseconds`, `RCL_MS_TO_NS` | ns |
| ROS 2 Python | `create_wall_timer(<ms integer>)` | ns, as decimal seconds |

### F4. ART is in milliseconds throughout

- **The clock:** `Art.Time = S64` (`art/shared/src/main/scala/art/Art.scala:16`) has no declared
  unit, but every implementation is ms since the Unix epoch:
  - JVM: `ArtNative_Ext.time()` = `System.currentTimeMillis` (`ArtNative_Ext.scala:237`);
  - Linux nix, single-process `Demo`: the transpiler forwards `art.ArtNative=art.ArtNativeSlang`
    (`ArtNixGen.scala:404`), so `Art.time()` is `ArtNativeSlang.time()`, which calls
    `art.Process.time()`, an `@ext(name = "art.ArtNative_Ext")` (`ArtNativeSlang.scala:325-332`;
    on the JVM that is `ArtNative_Ext.time()`). Its C implementation is `art_Process_time` from
    `SchedulerTemplate.c_process` (`SchedulerTemplate.scala:360-378`; generated as
    `ext-schedule/process.c` by `ArtNixGen.scala:433`), using `gettimeofday` and
    `tv_sec*1000 + tv_usec/1000`. The resource `jvm/src/main/resources/ext-schedule/Process.c`
    has the same code but is not referenced anywhere, and returns `Z` rather than `S64`.
  - Linux nix, legacy build (one process per thread, `transpile.cmd --legacy`): forwards
    `art.ArtNative=<pkg>.ArtNix` (`ArtNixGen.scala:358`). `ArtNix.time()` calls `<pkg>.Process.time()`
    (`ArtNixTemplate.scala:399`), which has no C implementation (only the JVM stub at
    `ArtNixTemplate.scala:654`); the legacy apps never read the clock, they only sleep (below).
  - seL4 nix: the app never calls `Art.run` and provides no `art_Process_time`
    (`SeL4NixTemplate.scala:262-305`); dispatch is driven by CAmkES.
- **The clock is also read outside `Art.run`.** Generated JVM unit tests and GumboX tests drive
  bridges through `ArtNative_Ext.initTest`, `testInitialise` and `testCompute`
  (`ArtNative_Ext.scala:333-372`), and `putValue` and `sendOutput` stamp `Art.time()` there
  (`ArtNative_Ext.scala:149,181,192,453`).
- **Dispatch protocols:** `DispatchPropertyProtocol.Periodic(period: Z)` and `Sporadic(min: Z)`
  (`ArchitectureDescription.scala:131,135`) are ms in every scheduler:
  - roundrobin compares `Art.time() - lastDispatch > period` (`RoundRobin.scala:27-34`), see F10;
  - legacy does `Thread.sleep(rate * slowdown)` (`LegacyInterface_Ext.scala:17,27`).
  - `Art.register` logs `"(periodic: $period)"` with no unit (`Art.scala:47-49`).
- **The static scheduler** works in abstract slots ("ticks") and never reads the clock (F14).
- **`ArtTimer.schedule` / `scheduleTrait(id, replaceExisting, delay: Art.Time, callback)`**
  (`ArtTimer.scala:13-16`) is user-facing API on an `@ext object` (`ArtTimer.scala:11`),
  implemented with `TimeUnit.MILLISECONDS`, a `delay * slowdown` adjustment and the log text
  `"Callback scheduled for $id: $delay ms"` (`ArtTimer_Ext.scala:71-74`).
- **Timestamps:** message timestamps (`ArtSlangMessage`, `ArtMessage`) and the `ArtDebug`
  callbacks carry `Art.Time` values. `ArtSlangMessage.UNSET_TIME` and `ArtMessage.UNSET_TIME` are
  the sentinel `-1`.
- **The legacy build runs one process per thread.** Each component app calls `Art.run` itself
  (`ArtNixTemplate.scala:197`).
- **arsit emits ms into generated code:**
  - `Periodic(period = ...)` (`ArchitectureGenerator.scala:296`, `ArchitectureTemplate.scala:13-16`,
    `SeL4NixGen.scala:84,97`);
  - legacy component apps: `Process.sleep(period)` after each periodic dispatch, and a 10 ms sleep
    otherwise (`ArtNixTemplate.scala:148-158`), declared `def sleep(n: Z)` (`ArtNixTemplate.scala:634`)
    and implemented in C as `<pkg>_Process_sleep` = `usleep(n * 1000)` at the end of
    `util/ipc_shared_memory.c`. The Scala `Process_Ext.sleep(millis: Z)` is a `halt("stub")`
    (`ArtNixTemplate.scala:652`). `util/ipc_message_queue.c` has the same code but is unreachable:
    `getIpc` halts for anything but shared memory (`arsit/Util.scala:137-142`);
  - timing properties in `Schedulers.scala` (`SchedulerUtil.scala:38-104`), see F14.
- **Event-port dispatch order uses arrival timestamps.** When several event ports of the same
  urgency have data, sporadic dispatch orders them by `dstArrivalTimestamp`
  (`ArtNative_Ext.scala:268-276`, `ArtNativeSlang.scala:66-73`), see F15.

### F5. Arithmetic that assumes milliseconds

Every backend does its own scaling, sums and comparisons on the ms values:

- **Microkit:**
  - `* 1000` (`SystemDescription.scala:162`);
  - `* 1_000_000` (`CComponentPlugin_MCS.scala:121,582`, `UserLandMonitorPlugin.scala:270`,
    `TestSchedulerPlugin.scala:1143`) and `/ 1_000_000` (`UserLandMonitorPlugin.scala:361`);
  - budget sums and frame-period comparisons (`CComponentPlugin_DomainScheduler.scala:554-589`,
    `CComponentPlugin_MCS.scala:104-121,568-583`, `DomainMonitorPlugin.scala:148-241`).
- **CAmkES:** `/ clockPeriod` in the pacers (F11), and `counter % (period / aadl_tick_interval)` in
  generated C (`PeriodicDispatcherTemplate.scala:35-41`), a modulo by zero for sub-ms periods.

### F6. Hard-coded millisecond constants

- **common:** default period 1 (`CommonUtil.getPeriod`); CAmkES has its own `DEFAULT_PERIOD = 1`
  (`act/util/Util.scala:57`).
- **Microkit:**
  - pacer slot 30 and default CET 50 (`CComponentPlugin_DomainScheduler.scala:32,34`,
    `CComponentPlugin_MCS.scala:29`);
  - injected monitor and test-controller periods 50, as `UnitProp("50", Some("ms"))`
    (`MonitorInjector.scala:333,342`, `TestControllerInjector.scala:20,87`). Their comments say the
    period is required by the linter but never used for scheduling.
- **CAmkES:** 200, 10, 10 (`Pacer.scala`).
- **arsit:** `defaultFramePeriod = 1000` (`SchedulerUtil.scala`) and the 10 ms idle sleep
  (`ArtNixTemplate.scala`). The static-schedule C constants are ticks, not ms (F14).

### F7. Related defects found during the survey

1. **SysML microseconds are wrong by 10^6.** `Instantiate.scala:677` multiplies `us` by
   `R("1.06")` instead of `R("1.0E6")`, so `1 us` becomes 1.06 ps.
2. **`getSlotTime` ignores the unit** (F2), so `Slot_Time` reaches `Schedulers.scala` as a raw
   picosecond count.
3. **ROS 2 Python timer:** `GeneratorPy.scala:513-526` emits
   `self.create_wall_timer(<ms integer>, ...)`. `create_wall_timer` is the rclcpp (C++) name. The
   rclpy equivalent, as far as we know, is `create_timer(timer_period_sec, ...)`, which takes
   seconds. To be verified against rclpy before changing.
4. **arsit static schedule:** `SchedulerTemplate.scala:85-87` computes
   `maxExecutionTime = numComponents / framePeriod`, which is normally 0 and looks inverted. The
   C `ScheduleProvider` ignores the model (`SchedulerTemplate.scala:328-348`). To be confirmed
   with the original intent before changing.

### F8. OSATE's doubles carry sub-picosecond noise

OSATE scales the model value to picoseconds in floating point. Usually the result is a whole
number, but not always. For example, the JVM gives:

| Model value | Scaling | Double result |
|---|---|---|
| `1.1 us` | `1.1 × 1e6` | `1100000.0` |
| `0.3 ms` | `0.3 × 1e9` | `3.0E8` |
| `33.3 ms` | `33.3 × 1e9` | `3.3299999999999996E10` |

The noise is far below 1 ps; it cannot be something the user wrote, since `Time_Units` has no
unit below `ps`. A rule that rejects a fractional picosecond count would therefore reject
ordinary models.

A double represents every integer up to 2^53 exactly, about 2.5 hours in picoseconds, so values
up to that size lose at most this sub-picosecond noise.

### F9. `Z` in transpiled C is as wide as `--bit-width`

`Periodic(period: Z)` and `Sporadic(min: Z)` are `Z`, and the C transpiler sizes `Z` from codegen's
`--bit-width` option (`HamrCli.scala:61,204,384`, default 64; passed as `--bits` by
`TranspilerTemplate.scala:108`). Users do choose 32: the expected test outputs contain 61
`--bits 32` transpiler invocations across 16 files (one `CodeGenTest_Base` model and 15
`CodegenTest_CASE` models). A signed 32-bit value holds at most about 2.1 s in nanoseconds, so a
3 s period in nanoseconds would wrap. (The ms representation has the same limit at 16 bits, about
32 s.)

The same applies to the timing types arsit generates into `Schedulers.scala`:
`ProcessorTimingProperties` and `ThreadTimingProperties` (`SchedulerTemplate.scala:36,41`) have
`Option[Z]` fields for the clock period, frame period, slot time and compute execution time (and
also for `maxDomain` and `domain`, which are domain numbers, not times).

### F10. ART's dispatch logic depends on the clock's origin

`RoundRobin` initialises `lastDispatch` and `lastSporadic` to `0` (`RoundRobin.scala:11-12`).
Because `Art.time()` is milliseconds since 1970, `Art.time() - 0 > period` holds immediately:
every periodic thread is dispatched on the first scheduler pass, and every sporadic thread may be
dispatched as soon as an event arrives.

A clock that starts near 0 would change this silently: periodic threads would wait one period
before their first dispatch, and sporadic threads would ignore events for their first `min`. And
`System.nanoTime()` is allowed to be negative, which could collide with the `-1` `UNSET_TIME`
sentinel (F4). The clock is also read in unit tests that never call `Art.run` (F4).

### F11. The CAmkES pacer's resolution is `Clock_Period`

The CAmkES pacer builds the seL4 domain schedule in `Clock_Period` ticks with integer division:
`otherLen / clockPeriod`, `pacerLen / clockPeriod`, `domainZeroLen / clockPeriod`,
`computeExecutionTime / clockPeriod` and `(framePeriod - (...)) / clockPeriod`
(`act/periodic/Pacer.scala:654-704`; `SelfPacer.scala:194-236` likewise). Converting values to
milliseconds and stopping there would still truncate silently to whole ticks.

### F12. How seL4 turns domain schedule durations into kernel time

Checked against the Microkit 2.3.1 release, the seL4 commit its manifest (`seL4/microkit-manifest`)
pins (`6e7c3b733d29`, release 16.0.0), and the `rust-sel4` revision its `Cargo.lock` pins
(`dbe6445d5605`):

- **Microkit always builds an MCS kernel** (`build_sdk.py`: `"KernelIsMCS": True`). The system
  description's integer, non-zero `us` (`tool/microkit/src/sdf/domains.rs:286-305`) is converted by
  the capDL initializer's `us_to_ticks` (`rust-sel4`, `crates/sel4-capdl-initializer/src/initialize.rs`):
  on ARM and RISC-V, round-to-nearest at the timer frequency (`s * f + (us * f + 500000) / 1000000`),
  which is finer than 1 us on current boards; on x86, `us * TSC MHz`, which is exact. So the only
  lossy step codegen controls is ps → us, which D4 reports; the kernel's own rounding is below a
  microsecond and is not reported.
- On a non-MCS kernel, which Microkit does not build, the same initializer **panics at boot** unless
  each duration is a multiple of `TIMER_TICK_MS`. If codegen ever targets a non-MCS Microkit SDK,
  domain schedule durations must be converted to that resolution instead.
- **Zero-length domain entries are never valid:**
  - In seL4 16.0.0, a duration of 0 with a non-zero domain is rejected
    (`src/object/domain.c:103-108`), and domain 0 with duration 0 is the schedule's end marker, so a
    zero-length padding entry would end the schedule early (`domain.c:82`, `src/kernel/thread.c:327-338`).
  - In seL4 14.0.0, the static-schedule, non-MCS shape CAmkES builds on, the tick handler does
    `ksDomainTime--` before testing for 0 (`src/kernel/thread.c:697-698` at that release), so a
    zero-length entry that becomes current wraps around and that domain runs effectively forever.
  - Microkit's own tool already rejects zero durations, and HAMR's Microkit domain scheduler already
    omits a zero pad (`CComponentPlugin_DomainScheduler.scala:586`, `if (padding > 0)`).

### F13. Generated text that says "ms"

Besides the values themselves, codegen writes units into generated comments and messages:

- CAmkES: `PacerTemplate.scala:149,158,159,188` ("The length is in seL4 ticks (${clock_period} ms)",
  "Clock_Period : … ms", "Period : … ms", CET "… ms"), `Pacer.scala:655,688`,
  `SelfPacer.scala:206,220`, and `PeriodicDispatcher.scala:83` ("Period not provided …, using
  ${Util.DEFAULT_PERIOD}", no unit);
- Microkit: `SystemDescriptionProvider_DomainSchduler.scala:59` ("has a duration of
  ${sd.length}ms"), `CComponentPlugin_DomainScheduler.scala:577` ("Frame period ${framePeriod}ms
  …"), the `DomainMonitorPlugin` and `UserLandMonitorPlugin` overrun warnings, and
  `MicrokitReporterPlugin.scala:592-593`;
- ART: `ArtTimer_Ext.scala:74` ("… $delay ms") and `Art.scala:47-49` ("(periodic: $period)",
  no unit).

### F14. arsit's static scheduler is in abstract ticks

The static schedule's slot lengths and hyper-period are abstract ticks (`static/Schedule.scala`).
The generated `Schedulers.scala` also emits `val framePeriod: Z = ${framePeriod}` and
`val maxExecutionTime: Z = numComponents / framePeriod` (`SchedulerTemplate.scala:85-87`). That
`framePeriod` is not a tick count: `SchedulerUtil.getFramePeriod` (`SchedulerUtil.scala:96-103`)
reads the model's `Frame_Period` in ms through `getFramePeriod()` (default 1000), so a sub-ms
`Frame_Period` already produces `numComponents / 0` in generated Slang. The C side hard-codes
`hyperPeriod = 1000` and `length = 1000 / n` (`SchedulerTemplate.scala:328-340`). The C scheduler
files `legacy.c`, `round_robin.c` and `static_scheduler.c` are generated with `overwrite = F`
(`ArtNixGen.scala:430-432`), so existing projects never receive changes to those templates; only
`process.c` is overwritten.

### F15. Event-port dispatch order depends on the clock's resolution

When a sporadic thread has data on several event ports of the same urgency, ART dispatches them
in order of `dstArrivalTimestamp` (`ArtNative_Ext.scala:268-276`, `ArtNativeSlang.scala:66-73`),
comparing with `<`. With millisecond timestamps, arrivals within the same millisecond tie, and the
stable sort falls back to the ports' declaration order. Generated unit tests, GumboX tests and
system tests insert values one after another (`insertInInfrastructurePort`, `ArtNative_Ext.scala:453`), so
today they almost always tie and see declaration order. With nanosecond timestamps they rarely tie,
so the order becomes arrival order, and on a platform with a coarse `nanoTime` it can still tie
sometimes. The dispatch order would then depend on clock resolution and test timing.

## Design

### D1. One internal unit: picoseconds

Codegen reads every time value as a `Z` count of **picoseconds**, the AADL base unit. This
matches what AIR already carries (F1), so the AIR producers are unchanged except for the SysML fix
(D9). AIR keeps meaning "a value and its unit", and codegen still accepts every `Time_Units`
literal, because codegen injects `ms` values itself (F6) and hand-written or older AIR may use
other units.

### D2. Parsing to picoseconds

`PropertyUtil.toPicoseconds` replaces `convertToMS`:

```scala
/** Parses a time property value to picoseconds (D2).  Reports a missing or unknown unit, or a
  * value that does not parse, as an error and returns None; reports a time-rounding warning,
  * through TimeUtil's de-duplicating helper (D4), when the value is more than double noise away
  * from a whole ps. */
def toPicoseconds(value: String, unitOpt: Option[String], what: String, pos: Option[Position],
                  reporter: Reporter): Option[Z]
```

`what` names the component and property for messages, and `pos` is the component's position (D3).

- It parses the value with `R`, accepting OSATE's double forms (`"1.0E8"`, `"1.0000E+12"`,
  `"2.0E+9"`) and plain integers or decimals, and scales it by the unit's factor to picoseconds.
  `R`'s 34-digit precision is ample for this (F1).
- It then rounds to the nearest picosecond. Whether that is silent depends on how far the value is
  from a whole picosecond:
  - **Within double precision of the value** (a relative difference of at most 2^-50, a few times
    the 2^-53 rounding error of one double operation): **silent**. This is OSATE's floating-point
    noise (F8), e.g. `3.3299999999999996E10` → `33300000000`, not a value the user wrote.
  - **Anything larger**, e.g. `1.4 ps` written in SysML, whose values are scaled in decimal (F1):
    rounded with the usual `time-rounding` warning (D4, D11), because the user really wrote a
    sub-picosecond fraction.
  - **Zero** (e.g. `"0.0"`) skips the relative check, which would divide by the value; the value
    linter reports it as an error (D5).
- A missing unit, an unknown unit, or a value that does not parse is a codegen error naming the
  component and property. Today these are `assert`/`halt`.

### D3. Picosecond accessors

Each accessor in F2 is replaced by a `...Ps` variant returning picoseconds:
`getPeriodPs`, `getFramePeriodPs`, `getClockPeriodPs`, `getComputeExecutionTimePs`,
`getMaxComputeExecutionTimePs` and `getSlotTimePs`. `getSlotTimePs` now honours the unit (F7.2).
They are symbol members, not `PropertyUtil` functions: `getPeriodPs` on `AadlDispatchableComponent`
(which `AadlThreadOrDevice` and `AadlVirtualProcessor` extend; the CAmkES pacer reads a virtual
processor's period, `Pacer.scala:683`), `getComputeExecutionTimePs` and
`getMaxComputeExecutionTimePs` on `AadlThreadOrDevice`, and the others on `Processor`. `CommonUtil.getPeriod(m)` (1 ms if absent)
is replaced by `CommonUtil.getPeriodPs(m)`, which returns `periodPs` or the common default period
in picoseconds.
`period` becomes `periodPs` everywhere it is declared: the `AadlDispatchableComponent` trait,
`AadlThreadOrDevice`, and the concrete `AadlVirtualProcessor`, `AadlDevice` and `AadlThread` (F2).

**Parse and report once.** The accessors are `@pure` methods on symbol datatypes, with no
reporter, and are called many times, so they cannot report D2's errors and warnings without
repeating them. Instead `SymbolResolver` parses every time property once, when it builds the
symbol, reports any error or `time-rounding` warning, and stores the picosecond values in the
symbol: `periodPs`, plus new fields for the frame period, clock period, slot time and compute
execution time. The `...Ps` accessors return those stored values and stay pure. The new fields are `Option`s of picoseconds (a pair
for the compute execution time), not symbols, so the symbol traversers (`MTransformer`/
`Transformer`) need no change: they rebuild symbols with `o2(features = ..., subComponents = ...)`
and carry other fields through.

- **Where diagnostics point:** at the component's position (`c.identifier.pos`), with the property
  named in the message by the instance's full path (D4), e.g. "Period of top.proc.worker
  (Timing_Properties::Period) is 1.5 ms …"; `toPicoseconds`'s `what` (D2) uses the path too. Not at the
  property's own position: OSATE builds that from the property *definition*
  (`VisitorUtil.buildPosition(pa.getProperty())`, `osate-plugin/.../Visitor.java:863-877`), which
  would point into the predeclared `Timing_Properties` set, and properties codegen injects
  (`MonitorInjector.scala:342`, `TestControllerInjector.scala:87`) have no position at all.
- **Once per codegen run, not per resolution.** `ModelUtil.resolve`, which runs `SymbolResolver`, is
  called once by `CodeGen.scala:145` and again by five Microkit plugins after they inject components
  (`DomainMonitorPlugin.scala:84`, `UserLandMonitorPlugin.scala:138`, `GumboMonitorPlugin.scala:347`,
  `StateVarPortsPlugin.scala:202`, `TestSchedulerPlugin.scala:511`), and for SysML models the front
  end resolves each system before codegen does (`hamr/sysml/.../FrontEnd.scala:71-86`). All of these
  use the same reporter (`cli/.../HAMR.scala:168,233` for SysML), and `Reporter.report` does not
  de-duplicate (`runtime/.../message/Reporter.scala:111-115`). So every time warning `SymbolResolver`
  reports, whether `time-rounding` or the missing-unit warning for `Slot_Time` (D5), goes through
  TimeUtil's de-duplicating helper (D4), which skips a warning whose text and position the reporter
  already holds. Each system's warnings then appear once however many times it is resolved, with no
  store state and no call site changes. Errors are reported as usual: the first pass stops codegen
  on any time error, and injected components use codegen's own exact values.

**Where the values live.** The frame period, clock period and slot time accessors are on the shared
`@sig trait Processor` (`AadlSymbols.scala:103-129`), the period on `AadlDispatchableComponent`,
and the compute execution time on `AadlThreadOrDevice`. The `...Ps` members are declared abstract on those traits, and the stored
fields go on the concrete `AadlProcessor`, `AadlVirtualProcessor`, `AadlThread` and `AadlDevice`,
which only `SymbolResolver` constructs (`SymbolResolver.scala:371,389,446,474`).

**Removing the old names.** The old millisecond names are removed in the end, so that every caller is
visited and none keeps assuming milliseconds. To keep each implementation step compiling, step 2
keeps them as thin wrappers over the stored picosecond values, with today's flooring, and the last
code step removes them (Implementation order). The callers to visit include the CAmkES readers
(`PeriodicDispatcher.scala:80`, `Pacer.scala:683-684`, `SelfPacer.scala:224`, `PacerTemplate`).
`PeriodicDispatcher.scala:80` calls `PropertyUtil.getPeriod(aadlThread.component)` on the IR; it
switches to the stored `aadlThread.periodPs`, and `PropertyUtil`'s parsing becomes
`SymbolResolver`-only, so every time value is parsed and reported in one place.
The same applies to the `period` field, which eleven places read directly (`CommonUtil.scala:150`,
`Linter.scala:34`, `MicrokitLinterPlugin.scala:116`, `Pacer.scala:683,684`, `SelfPacer.scala:224`,
`ros2/Generator.scala:2690,2699,5472`, `GeneratorPy.scala:514,522`): in step 2 the field becomes
`periodPs`, and a concrete `@pure def period: Option[Z]` (ms, floored) on
`AadlDispatchableComponent` stands in for it, replacing the abstract re-declaration at
`AadlSymbols.scala:237`, until step 8 removes it. The two separate 1 ms defaults
(`CommonUtil.getPeriod` and CAmkES's `DEFAULT_PERIOD`) become one picosecond constant in the common
layer. Nothing outside `hamr/codegen` uses the old names (F2).

### D4. Backend conversion: `TimeUtil`

A new `common/util/TimeUtil.scala` provides:

```scala
@enum object HamrTimeUnit {
  "ps"
  "ns"
  "us"
  "ms"
}

/** Converts ps to the given resolution (a unit, or a Clock_Period tick given in ps),
  * rounding to nearest (ties away from zero).  Reports a warning if the result is not
  * exact and an error if it is 0 (or less) or greater than maxValue (the largest value
  * the target's integer type holds, e.g. S64.Max for ART); returns the converted value. */
def fromPicoseconds(ps: Z, resolutionPs: Z, maxValue: Z, what: String, target: String,
                    pos: Option[Position], reporter: Reporter): Z

/** As fromPicoseconds, but a result of 0 is clamped to 1 with a time-rounding warning saying so,
  * instead of an error.  Only for codegen's own constants (the CAmkES pacer's fixed slots, D7). */
def fromPicosecondsAtLeastOne(ps: Z, resolutionPs: Z, maxValue: Z, what: String, target: String,
                              pos: Option[Position], reporter: Reporter): Z
```

`target` names what the value is converted for (e.g. "the Microkit domain schedule"), for the
messages below. The enum is named `HamrTimeUnit` to avoid confusion with `java.util.concurrent.TimeUnit`, which ART
uses. The resolution is given in picoseconds, so the same function converts to a fixed unit
(`1000` for ns) or to a model-defined tick (the CAmkES `Clock_Period`, D7).

- **Exact** → returned silently.
- **Inexact** → rounded to nearest, with a **warning**, e.g. `"Compute_Execution_Time of
  top.proc.worker (Timing_Properties::Compute_Execution_Time) is 1.5 us; the Microkit domain
  schedule uses 2 us (resolution 1 us)"`. Values are shown in the most readable exact unit.
- **Rounds to 0, or is ≤ 0** → **error**, e.g. `"Period of top.proc.worker
  (Timing_Properties::Period) is 100 us, which is 0 at the CAmkES resolution of 1 ms"`.
- **`what` names the instance by its full path** (e.g. `top.proc.worker`), not its simple
  identifier. OSATE builds a component's position from its subcomponent *declaration*
  (`osate-plugin/.../Visitor.java:603`), so two instances of one declaration (`p1.worker`,
  `p2.worker`) share a position, and only the path keeps their warnings distinct for the
  de-duplicating helper below.
- **Too large for the target** → **error**. `maxValue` per backend: `S64.Max` for ART (D6), the
  Microkit MCS timeslices and micro-ROS/C++ nanoseconds; `U64.Max` for Microkit domain `us`
  (Microkit parses a `u64`); `S32.Max` for the CAmkES calendar period and pacer ticks, a safe
  common bound for the generated C, which compares the period against a `uint32_t` counter
  (`PeriodicDispatcherTemplate.scala:36-38,60-61`) and stores ticks in `dschedule_t.length`, a
  `word_t`, 32 bits on 32-bit targets. The ROS 2 timers share one ns conversion, bounded by
  `S64.Max`; the (unreachable) Python timer's seconds expression is exact up to 10^15 ns (open
  question 1).

Each backend calls it once per value, at the point where the value leaves codegen's internal
representation. Sums, comparisons and padding are done on the converted values, so the generated
artifact is self-consistent. A frame period whose padding would be negative is an error: Microkit's existing
"too small for the used budget" error, and a new equivalent for the CAmkES pacer (D7).

All generated text that names a time unit (F13) is updated to the unit actually used, or shows the
value with its unit through a shared formatter in `TimeUtil`.

**Each warning once.** Backends can convert the same value more than once: the Microkit monitor
and test-scheduler plugins re-read and re-convert the frame period (`DomainMonitorPlugin.scala:151`,
`UserLandMonitorPlugin.scala:266`, `TestSchedulerPlugin.scala:1142`) after `CComponentPlugin_*` has.
So `TimeUtil` reports every time warning through one helper that skips a warning whose text and
position the reporter already holds. It covers backend conversions, Microkit re-resolution and the
SysML front end followed by codegen (D3). Two rules make it work:

- **Identical text for the same value:** each value a backend converts in several places goes
  through one shared function, so every site produces the same message. Microkit gets
  `frameTimeUs`/`frameTimeNs` helpers for the frame period, called by `CComponentPlugin_*`,
  `DomainMonitorPlugin.scala:151`, `UserLandMonitorPlugin.scala:266` and
  `TestSchedulerPlugin.scala:1142`; arsit gets one `periodNs` helper, called by
  `ArchitectureGenerator.scala:296`, `ArtNixGen.scala:164` and `SeL4NixGen.scala:84`; and ROS 2
  gets one `periodNs` helper of its own, called by `ros2/Generator.scala:2690,2699,5472` and
  `GeneratorPy.scala:514,522`.
- **One reporter per warning:** arsit (`ReporterUtil.reporter`) and CAmkES (`act`'s `Util.reporter`)
  have their own reporters, reset per run and merged into the main one without de-duplication. The
  helper de-duplicates within a reporter, not across them, so no shared code reports a time
  warning into more than one of them: common code (`SymbolResolver`, `TimeUtil` called from
  common) reports into the main reporter, and each backend reports only its own conversions.

### D5. Value linter

`Linter.scala` adds a backend-independent check of every present time property:

- `Period`, `Frame_Period`, `Clock_Period` and `Slot_Time` must be > 0;
- `Compute_Execution_Time` must have **low ≥ 0, high > 0 and low ≤ high**. A low end of 0 is
  valid and common (`0 ms .. 5 ms` means "up to 5 ms"), and no backend uses the low end; the high
  end becomes a schedule slot, so it must not be 0 (F12). Low ≤ high is today only an `assert` in
  the two Microkit component plugins (`CComponentPlugin_MCS.scala:106`,
  `CComponentPlugin_DomainScheduler.scala:176`); as a linter error it now also applies to JVM,
  Linux and CAmkES models (D8). The Microkit `assert`s stay as a backstop.

A property that fails to parse is not also reported as missing ("Must specify Period",
`Linter.scala:34-35`): `buildSymbolTable` returns `None` on any error
(`SymbolResolver.scala:761-763`), so the linter never runs after a D2 error. No code is needed for
this; the test keeps it so.

**A `Slot_Time` without a unit** keeps today's meaning: `getSlotTime` passes the number through as
is, and the AIR producers would give it in picoseconds, so a unitless `Slot_Time` is read as
picoseconds, with a warning that the unit is missing. This special case lives in `SymbolResolver`
(D2 otherwise makes a missing unit an error), and the warning goes through the de-duplicating
helper (D4). Only AADL models can set `Slot_Time`: the SysML front end does not handle it at all
(it is absent from `InstantiateUtil.scala:250-260`).

### D6. ART moves to nanoseconds

**The clock.** `Art.Time` stays `S64` and is documented as **nanoseconds since the process's ART
clock started**. It is never negative, and S64 nanoseconds cover about 292 years.

- **JVM:** `ArtNative_Ext` reads `System.nanoTime()` once, when the object is initialised, and
  `time()` returns `System.nanoTime() - startNanos`. `nanoTime` is monotonic but has an arbitrary,
  possibly negative, origin (F10). Starting the clock at object initialisation rather than in
  `Art.run` covers the unit-test entry points that never call `Art.run` (F4).
- **Linux nix, `Demo` (C):** the clock is `art_Process_time` (F4), which `SchedulerTemplate.c_process`
  implements with `clock_gettime(CLOCK_MONOTONIC)` and a static start value read on the first call,
  returning nanoseconds since that call. `process.c` is already generated with `overwrite = T`
  (F14), so existing projects pick up the new version when they regenerate.
- **Linux nix, legacy apps:** never read the clock (F4), so only their sleeps change (below). The
  unused `ArtNix.time()` is left as it is.
- **seL4 nix:** has no ART clock (F4); only the `Periodic` literal changes.
- **One origin per process.** Each process's clock starts in that process. The `Demo` runs every
  component in one process, and the legacy apps do not read the clock, so no behaviour depends on
  comparing times across processes; it is still noted in Compatibility for user code that logs or
  exchanges timestamps.
- **C portability:** `clock_gettime(CLOCK_MONOTONIC)` and `nanosleep` exist on Linux, on macOS 10.12
  and later, and on Cygwin. The transpiler's CMake sets `CMAKE_C_STANDARD 99` and leaves compiler
  extensions on (`StaticTemplate.scala:346`), so builds use `-std=gnu99`, where glibc declares both
  from `<time.h>` by default. No feature-test macro is defined: `#define _POSIX_C_SOURCE 199309L`
  would turn off glibc's `_DEFAULT_SOURCE` (and lower `__DARWIN_C_LEVEL` on macOS), hiding
  declarations the generated C relies on, such as `usleep`/`useconds_t`, which `-Werror` turns into
  build failures.
- **32-bit targets:**
  - 64-bit time is not new there: `Art.Time` is already `S64`, and `RoundRobin` already widens each
    period with `conversions.Z.toS64(period)` before comparing. Storing periods as `S64` changes
    neither the per-dispatch arithmetic nor its cost.
  - C99 guarantees `int64_t`. On a 32-bit CPU, adds, subtracts and compares take a few instructions,
    and multiplies and divides call compiler helpers (e.g. `__aeabi_ldivmod` on ARM) from libgcc,
    which glibc (Linux) and musl (seL4) toolchains provide. The cost is one 64-bit divide per sleep
    (splitting nanoseconds into a `timespec`) and one multiply per clock read; each period grows
    from 4 to 8 bytes per bridge. A bare-metal target without libgcc could not link the divides,
    but transpiled ART targets only Linux and seL4.
  - 64-bit values are not read or written atomically on a 32-bit CPU. No ART time or sequence
    number ever crosses a process boundary: shared memory carries only the port's payload,
    `Option[DataContent]` (`ipc_shared_memory.c` copies `art_DataContent` into the segment), not ART
    messages or timestamps. Within a process, the Demo is single-threaded and the JVM uses
    synchronisation (below), so no torn value can be observed.
  - Conversions widen before multiplying: on 32-bit Linux `tv_sec` can be a 32-bit `time_t`, so the
    clock computes `(int64_t) ts.tv_sec * 1000000000 + ts.tv_nsec`, never
    `ts.tv_sec * 1000000000`. Today's `art_Process_time` already widens first
    (`int64_t t = tv.tv_sec; t *= 1000;`).
  - `CLOCK_MONOTONIC` counts from boot, so it is unaffected by the January 2038 overflow of a
    32-bit `time_t`, which today's `gettimeofday`-based clock hits on 32-bit glibc unless built with
    64-bit time. The change removes that existing problem.

**Dispatch.**

- `Periodic(period)` and `Sporadic(min)` change from `Z` to **`Art.Time`** (S64), in nanoseconds.
  Their range then no longer depends on `--bit-width` (F9).
- **Same first-dispatch behaviour (F10):** `RoundRobin.initialize()` sets each periodic bridge's
  `lastDispatch` to `Art.time() - period - s64"1"` and each sporadic bridge's `lastSporadic` to
  `Art.time() - min`, so every thread can be dispatched on the first pass, as it is today. All
  operands are `S64` (`RoundRobin.scala` already imports `org.sireum.S64._`). The
  `conversions.Z.toS64(period)` and `conversions.Z.toS64(minRate)` calls in `shouldDispatch`
  (`RoundRobin.scala:28,34`) are removed, as they no longer type-check once the fields are `S64`.
  Likewise `ArchitectureTemplate.dispatchProtocol(dp, period: Z)` takes the period in ns as `Z` and
  emits it as an `s64` literal. `RoundRobin.initialize()`
  runs from `Art.run`'s system setup after `assemble` has registered the bridges, so every bridge's
  dispatch protocol is available there. The legacy scheduler's sleep loop and the static
  scheduler's slots do not depend on the origin.
- **Legacy scheduler sleep (JVM):** `LegacyInterface_Ext` gets a helper,
  `def sleepArgs(rate: Art.Time, slowdown: Z): (scala.Long, scala.Int)`, which computes the sleep in
  nanoseconds as a Slang `Z` (`val ns = rate.toMP * slowdown.toMP`) and returns
  `((ns / 1000000).toLong, (ns % 1000000).toInt)`; the scheduler loop calls
  `Thread.sleep(ms, nanos)` with it. The helper is unit-tested on its own (Tests).
- `Art.register` logs the period with its unit (F13).
- **Event-port order no longer depends on the clock (F15):** ART keeps a per-process arrival counter
  and stamps each message with it when it arrives at its destination port (a new field,
  `dstArrivalSeq`, in `ArtMessage` and `ArtSlangMessage`, `UNSET` = -1 until delivery).
  - `ArtSlangMessage` is a `@datatype`: `putValue`'s constructor (`ArtNativeSlang.scala:148`)
    passes `dstArrivalSeq = UNSET`, and `sendOutput` bumps the counter once per destination inside
    the loop over `Art.connections` in `sendOutputPort` (`ArtNativeSlang.scala:189-193`), alongside
    `dstArrivalTimestamp = Art.time()`.
  - `ArtMessage` (JVM) is a `case class` with `var` fields: the number is assigned in the
    `msg.copy(...)` that builds each delivered message (`ArtNative_Ext.scala:181`), before it is put
    into `inInfrastructurePorts`. Today `sendOutput` inserts first and sets `dstArrivalTimestamp`
    afterwards (`:186-192`); with the legacy scheduler's threads a concurrent sort could then see an
    unset number. `insertInInfrastructurePort` (`:453`) assigns it the same way, and so do the
    messages `ArtDebug_Ext` injects (`ArtDebug_Ext.scala:55,57`, `ArtMessage(data)`), so every
    message in an in-port has a sequence number and no port sorts first with `UNSET`.
  - It is `S64`, not `Z`: under `--bits 32` a `Z` counter would wrap after about 2.1e9 deliveries
    (about 60 hours at one event per 100 µs) and silently reorder ports.
  - On the JVM the legacy scheduler runs each bridge on its own thread (`LegacyInterface_Ext`), so
    `ArtNative_Ext` uses a `java.util.concurrent.atomic.AtomicLong`, as it already uses concurrent
    maps there; a plain `var` could hand out the same number twice. `ArtNativeSlang` uses a plain
    `var`: the transpiled `Demo` is single-threaded.
  - The Linux legacy apps are unaffected: `ArtNix.dispatchStatus` (`ArtNixTemplate.scala:330-340`)
    does not order ports by time, so neither F15 nor the counter applies to them.
  Both arrival comparisons switch from `dstArrivalTimestamp` to `dstArrivalSeq`: the one between
  urgent ports of the same urgency, and the one between non-urgent ports
  (`ArtNative_Ext.scala:272,276`, `ArtNativeSlang.scala:69,73`). The order is then true arrival order on every platform, whatever the clock's resolution. It differs from
  today's order only where today's millisecond timestamps tied and the sort fell back to
  declaration order (D8).

**Generated code.**

- **Generated timing types:** the *time* fields of `ProcessorTimingProperties` and
  `ThreadTimingProperties` change type: clock period, frame period and slot time from `Option[Z]` to
  `Option[Art.Time]`, and compute execution time from `Option[(Z, Z)]` to
  `Option[(Art.Time, Art.Time)]` (`SchedulerTemplate.scala:36-42`). `maxDomain` and `domain` stay
  `Option[Z]`; they are domain numbers (F9).
- **Static scheduler:** its slots and hyper-period stay in abstract ticks, and the C schedule
  constants are unchanged (F14, D10). `Schedulers.framePeriod` is not a tick count (F14); its only
  consumer is the next line, `maxExecutionTime`, used as each default static slot's length
  (`SchedulerTemplate.scala:18,85-87`). It becomes nanoseconds, like the timing types' frame period:

  ```scala
  val framePeriod: Art.Time = s64"<ns>"          // default s64"1000000000" (1000 ms)
  val numComponents: Z = Arch.ad.components.size
  val maxExecutionTime: Z =
    conversions.S64.toZ(conversions.Z.toS64(numComponents) * s64"1000000" / framePeriod)
  ```

  This is today's `numComponents / framePeriodMs` computed in `S64` nanoseconds: for any
  whole-millisecond frame it gives exactly today's value (e.g. 1 for a 2 ms frame with 3
  components), so the default static schedule does not change. Converting `framePeriod` itself to
  `Z` would exceed a 32-bit `Z` under `--bits 32` for any frame of 2.15 s or more (F9); only the
  small result is converted. A sub-ms `Frame_Period` is just a nanosecond value (only 0 is an
  error, D5), which removes today's `numComponents / 0`. Whether the formula itself is right is
  open question 2.
- **Legacy apps' sleep:** `Process.sleep(n: Art.Time)` takes nanoseconds. `ipc_shared_memory.c` gets
  one sleep helper and uses it for every sleep, replacing all three `usleep` calls:

  ```c
  // sleeps ns nanoseconds, resuming after signals (the C scheduler installs handlers)
  static void sleep_ns(int64_t ns) {
      struct timespec req = { .tv_sec = (time_t) (ns / 1000000000), .tv_nsec = (long) (ns % 1000000000) };
      struct timespec rem;
      while (nanosleep(&req, &rem) == -1 && errno == EINTR) req = rem;
  }
  ```

  `<pkg>_Process_sleep`'s C parameter becomes `S64 n` (was `Z n`), matching the transpiled
  declaration of `Process.sleep(n: Art.Time)`, in `ipc_shared_memory.c` and the unreachable
  `ipc_message_queue.c`; it calls `sleep_ns(n)`, and the 10 ms wait loops in `SharedMemory_receive`
  and `SharedMemory_send` call `sleep_ns(10000000)`. `ipc.c` then uses neither `usleep` nor
  `useconds_t`, so it does not depend on feature-test macros, and every sleep resumes after a
  signal; today's `usleep` returns early instead. `ipc.c` includes `<time.h>` (it already includes
  `<errno.h>`). The Scala stub's parameter is renamed to match. The unreachable
  `ipc_message_queue.c` is updated the same way so it does not rot further.
- **Literals:** codegen emits these values as `s64"<ns>"`, a compile-time constant. Every generated
  Slang file that contains one gains `import org.sireum.S64._`, which the `s64` interpolator requires
  and which those templates do not emit today: `Arch.scala`, `Schedulers.scala`, the Linux legacy
  component apps, and the seL4 nix apps (`seL4Nix/<pkg>/<Comp>/<comp>_seL4App.scala`, which contain
  `Periodic(period = ...)` via `SeL4NixGen.scala:97`).
- **Dead resource:** `jvm/src/main/resources/ext-schedule/Process.c` (F4) is deleted.

**ArtTimer: breaking change, made explicit.**

- `ArtTimer.schedule` and `scheduleTrait` take a delay in nanoseconds; the parameter is renamed
  `delayNs`. `ArtTimer_Ext` uses `TimeUnit.NANOSECONDS`, applies `slowdown` to the nanosecond value,
  and logs the delay with its unit (F13).
- Conversion helpers live in a plain Slang object, not on the `@ext object ArtTimer` (whose members
  would each need a C implementation): a new `art.ArtTime` object with
  `@strictpure def millis(n: Z): Art.Time = conversions.Z.toS64(n) * s64"1000000"` and
  `@strictpure def micros(n: Z): Art.Time = conversions.Z.toS64(n) * s64"1000"` (importing
  `org.sireum.S64._`). The multiplication is done in `S64`, not `Z`: with `--bit-width 32`, `Z` is
  32 bits in transpiled C, and `n * 1000000` in `Z` would already wrap at `millis(2148)`. Call sites
  read `ArtTimer.schedule(id, T, ArtTime.millis(500), cb)`.
- Code that passed a bare millisecond count fires 10^6 times sooner, so the change is called out in
  `changelog.md` and the ART release notes.
- A search of the Sireum, HAMR and OSATE plugin repositories found no callers outside ART itself.

Message timestamps and the `ArtDebug` callbacks become nanoseconds with no API change. So does the
`"time"` field of ART's JSON log (`ArtNative_Ext.scala:307`), which loses its wall-clock meaning
(Compatibility).

### D7. Per-backend units

| Backend | Resolution | Changes |
|---|---|---|
| ART: JVM and Linux nix | ns | `Arch.scala` emits `Periodic(period = s64"<ns>")` (`ArchitectureGenerator`, `ArchitectureTemplate`); the legacy component apps sleep `Process.sleep(s64"<ns>")` and their 10 ms idle sleep becomes `s64"10000000"` (`ArtNixGen`, `ArtNixTemplate`); the time fields of `Schedulers.scala`'s timing types in ns, and `Schedulers.framePeriod` from `getFramePeriodPs` in ns (`SchedulerUtil`, `SchedulerTemplate`); the `S64._` import added where needed (D6); the static scheduler's ticks unchanged (D6, D10) |
| ART: seL4 nix | ns | `Periodic(period = s64"<ns>")` and the `S64._` import in the seL4 nix apps (`SeL4NixGen`) |
| Microkit domain scheduling | us | `SchedulingDomain` duration in us (see below), rendered as `"<n> us"` without `* 1000`; pacer slot (30 ms) and default CET (50 ms) in us; budget, padding and monitor-variant arithmetic in us (`CComponentPlugin_DomainScheduler`, `DomainMonitorPlugin`); the `length <= 0` check stays as a backstop; a zero pad is omitted, as today. Kernel conversion: F12 |
| Microkit MCS / user-land | ns | Budgets, frame period and padding in ns end to end; remove every `* 1_000_000` and `/ 1_000_000` (`CComponentPlugin_MCS`, `UserLandMonitorPlugin`, `TestSchedulerPlugin`); messages and `MicrokitReporterPlugin` show values with their unit |
| CAmkES dispatcher | ms | `Period` goes through `fromPicoseconds(..., 1 ms, ...)` for the 1 ms dispatcher calendar (`PeriodicDispatcher`), so a sub-ms period is a clear error instead of a modulo by zero in generated C |
| CAmkES pacer | `Clock_Period` | See below |
| ROS 2 C++ | ns | `std::chrono::nanoseconds(<ns>)` |
| micro-ROS | ns | `<ns>` passed directly instead of `RCL_MS_TO_NS(<ms>)` |
| ROS 2 Python | ns → seconds | `create_timer((<ns> + 0.5) / 1e9, ...)` (open question 1); Python node generation is not reachable today |

**`SchedulingDomain` gets an explicit unit.** Today its single `length` field means ms for
domain scheduling and ns for MCS, which only its doc comment records. It is replaced by an
explicit duration: `length: Z` plus `unit: HamrTimeUnit.Type` (us for domain scheduling, ns for
MCS), so each slot carries its unit. The domain renderer (`prettyST`) halts unless its unit is us;
Slang allows `halt` only as a statement, so `prettyST` becomes a `@pure` method instead of
`@strictpure`. The MCS renderer checks for ns. The class's doc comment (`SystemDescription.scala:147-157`), which still
says "milliseconds for domain scheduling", is updated.

**CAmkES pacer.** The pacer's domain schedule is in `Clock_Period` ticks (F11):

- **`Clock_Period` must be a positive whole number of milliseconds.** The entries are seL4 ticks
  (`PacerTemplate.scala:149`), and a non-MCS seL4 tick is the integer `TIMER_TICK_MS` (F12), which
  codegen does not set. The CAmkES pacer reports an error otherwise, where it first reads
  `Clock_Period` (`Pacer.scala:643`, `SelfPacer.scala:194`), through `act`'s `Util.reporter`, at
  the bound processor's position (`component.identifier.pos`). Not in
  `PacerUtil.canUseDomainScheduling` (`PacerUtil.scala:148-166`): that is a predicate the common
  linter calls for every platform (`Linter.scala:229`), which only emits an info message, and an
  error there would also reject JVM, Linux and Microkit models. Today a sub-ms `Clock_Period`
  divides by zero; 1.5 ms would otherwise silently produce ticks no kernel provides.
- Each user entry (a thread's compute execution time) converts to ticks with
  `fromPicoseconds(..., clockPeriodPs, ...)` instead of `/ clockPeriod`, in `Pacer` and `SelfPacer`.
  A value that rounds to 0 ticks is an error, as everywhere (D4).
- The pacer's own fixed slots (200/10/10 ms) convert with `fromPicosecondsAtLeastOne` (D4), which
  **clamps them to at least one tick**, with a warning: they are codegen's constants, and with a `Clock_Period` over 20 ms the
  10 ms slots would otherwise round to 0, which the user could only avoid by changing
  `Clock_Period`.
- `Frame_Period` converts to ticks once, and the pad is `frameTicks - Σ entryTicks`, computed from
  the converted entries, so the entries always add up to the frame (D4). The pad is never rounded
  on its own.
- **No zero-length entries.** seL4 cannot run a zero-length domain entry safely (F12), so the
  pacer never emits one.
- **A thread without `Compute_Execution_Time`:** CAmkES does not require it (`PacerUtil.scala:148-156`
  checks only `Clock_Period` and `Frame_Period`), and `getMaxComputeExecutionTime()` returns 0
  (`AadlSymbols.scala:254-260`), which today becomes a 0-tick entry (`Pacer.scala:694`). It gets a
  default instead, the same 50 ms as Microkit's `defaultComputeExecutionTime`
  (`CComponentPlugin_DomainScheduler.scala:34`), converted to ticks like any other value, with a
  **warning** naming the thread and the default used. The same applies to a process or VM whose
  entry is the maximum over its threads, all without `Compute_Execution_Time`
  (`Pacer.scala:674-682`).
- **A pad below 0** means the entries do not fit in the frame, and today it is emitted unchecked
  (`Pacer.scala:704`, `SelfPacer.scala:236`). It becomes an **error**, like Microkit's
  "too small for the used budget". **A pad of exactly 0 is omitted**, as Microkit does
  (`CComponentPlugin_DomainScheduler.scala:586`).
- Generated comments show the values used with their units (F13).

The injected monitor and test-controller periods (F6) keep their 50 ms value; they are not used
for scheduling.

### D8. Defaults keep today's behaviour

Every hard-coded constant (F6) is re-expressed in picoseconds from its current millisecond value,
then converted per backend like any model value. Existing models that use whole milliseconds
therefore generate behaviourally identical systems, with only the units of the numbers in the
generated code changing, **except for the deliberate changes**:

- SysML `us` values are now correct (F7.1, D9);
- the (unreachable) ROS 2 Python timer uses rclpy's `create_timer` with seconds (F7.3);
- `ArtTimer` delays are nanoseconds (D6);
- same-urgency event ports are dispatched in true arrival order, by sequence number, where today's
  millisecond timestamps tied and fell back to declaration order (F15, D6). Generated tests that
  insert values on several such ports in one step may see a different, but now deterministic,
  order;
- the CAmkES pacer rounds to the nearest `Clock_Period` tick, with a warning, instead of flooring
  (F11). A model whose `Compute_Execution_Time` is not a whole number of ticks gets a different
  domain schedule: for example 5 ms with a 2 ms clock was 2 ticks and becomes 3. This applies to
  the pacer's own fixed slots (200, 10, 10 ms) too, so a `Clock_Period` that does not divide them
  now produces `time-rounding` warnings about codegen's constants rather than the user's values;
  the warning says so. The pad is computed from the rounded entries, so the schedule still fills
  the frame exactly;
- a CAmkES pacer schedule whose entries exceed the frame (a pad below 0) is now a codegen error;
  today it is emitted with a negative or zero pad. A zero pad is now omitted. A CAmkES thread (or
  process/VM) without `Compute_Execution_Time` now gets the 50 ms default, with a warning, instead
  of a 0-tick entry, which seL4 cannot run safely (F12);
- a `Slot_Time` without a unit is read as picoseconds, with a warning (D5); today its number is
  passed through unconverted;
- a `Compute_Execution_Time` whose low end exceeds its high end is now a codegen error on every
  platform; today it is only a Microkit `assert` (D5);
- a zero `Period`, `Frame_Period`, `Clock_Period` or `Slot_Time`, or a `Compute_Execution_Time` whose
  high end is 0 (e.g. `0 ms .. 0 ms`), is now a codegen error on every platform (D5). Today these
  are harmless on JVM, Linux and ROS 2, which never build a schedule from them;
- in CAmkES, a `Clock_Period` that is not a whole number of milliseconds is now an error, and a
  `Compute_Execution_Time` below half a `Clock_Period` (e.g. 1 ms with a 3 ms clock), which today
  floors to a 0-tick entry, now rounds to 0 and is an error; the pacer's fixed slots are clamped to
  at least one tick instead (D7).

ART's first-dispatch behaviour is deliberately kept (D6, F10).

### D9. SysML microsecond fix

`Instantiate.scala:677`: `R("1.06")` → `R("1.0E6")` (F7.1). This is independent of the rest and
lands first.

### D10. Out of scope

- arsit's static scheduler (F14) stays in abstract ticks, and its arithmetic (F7.4) is not changed
  by this design. It is raised separately once its intent is confirmed. Because its C files are
  generated with `overwrite = F`, any later change would not reach existing projects anyway.
- No new time properties (`Deadline`, `Dispatch_Offset`, ...) are read. Codegen reads none today:
  a search of every property constant and property read in codegen and the SysML frontend found
  no time-valued property beyond the five in F2 (and the injected periods, F6).
- The CAmkES pacer and Microkit domain scheduling run each thread once per frame and do not use its
  `Period` ("TODO handle components with different periods", `Pacer.scala:55`). That is an existing
  silent difference from the user's model, which this design does not change.
- Not affected, as they carry no time unit:
  - `CASE_Scheduling::Schedule_Source_Text` points to a user-written CAmkES schedule file, which
    codegen includes verbatim (`Pacer.scala:623`, `SelfPacer.scala:174`) without interpreting it;
  - GUMBO/R2U2 temporal operators (`F`, `G`, `O`, `H` and the binary operators) take intervals in
    R2U2 time steps, passed through unchanged (`SlangExpUtil.scala:635-642`);
  - `Timing_Properties::Timing_Period` and `AADL_Project::Time`, which the SysML frontend accepts
    with a unit (`InstantiateUtil.scala:260`, `TypeHierarchy.scala:356`) but codegen never reads.

### D11. How rounding is reported

Rounding warnings are ordinary codegen warnings, so they appear in the CLI output, the IDE and the
codegen report. They are given their own message kind (`"time-rounding"`) so that the test harness
can assert that a rounding warning is, or is not, reported for a model.

There is no option to turn them into errors; a user who wants exact schedules reads the warnings.
Such an option (e.g. `--strict-time`) is open question 3.

Errors (a value that becomes 0, does not fit, or does not parse) stop codegen as usual.

## Implementation order

Each step type-checks and compiles before the next.

1. **SysML fix** (D9).
2. **Common:** `toPicoseconds`, `TimeUtil`, the stored picosecond fields and `...Ps` accessors, the
   value linter and the de-duplicating warning helper (D2-D5). The old millisecond accessors stay,
   as wrappers over the stored values with today's flooring, so every backend still compiles.
3. **ART → ns** (D6): the changes inside the `art` submodule (`art/shared/...`), committed locally.
   The codegen-side parts of D6 (`SchedulerTemplate.c_process`, `ipc_shared_memory.c` and
   `ipc_message_queue.c`, the `ArtNixTemplate` `Process.sleep` signature, the `s64` literals and
   imports, and deleting `ext-schedule/Process.c`) belong to step 4. Codegen embeds ART's sources in
   generated projects by default and the tests use that embedded copy, so ART does not need to be
   on JitPack for the following steps. The embedding is an `RC.text` macro in
   `ArsitLibrary_Ext.scala`, which incremental compilation does not rebuild when only `art/`
   changes: touch `ArsitLibrary_Ext.scala` after every ART edit, or the tests silently generate
   the old ART. The one exception is `CodeGenTest_Base`'s
   `JVM-Do-not-embed-art` case (`CodeGenTest_Base.scala:83-86`, `noEmbedArt = T`), which resolves
   ART by `art.version`; it is expected to fail in modes that compile the generated project until
   step 10. Between this step and step 4, generated projects still emit `Z` period literals against
   the `S64` ART, so tests that compile generated JVM/Linux projects are also expected to fail until
   step 4 is done.
4. **arsit** (D7, ART rows).
5. **Microkit** domain and MCS (D7).
6. **CAmkES** dispatcher and pacer (D7).
7. **ROS 2** (D7), after verifying F7.3.
8. **Remove the millisecond accessors** (D3). Every remaining caller is then a compile error, which
   confirms no backend still assumes milliseconds.
9. **Tests, expectations and release notes:** the tests and expectations below, plus the
   `changelog.md` entries and ART release notes for the behaviour and API changes (D6, D8,
   Compatibility). The codegen test harness first gains a way to expect warnings: `CodegenTest.test`
   takes only `expectedErrorReasons` (`CodegenTest.scala:67`), so it gets an expected warning-kind
   parameter (e.g. `time-rounding` and how many), which the D11 assertions use.
10. **Release ART once:** push the `art` submodule, get its JitPack build to pass (it can take
   several retries; JitPack hosts are flaky), then bump `art.version` with
   `bin/scripts/checkVersions.sc`. One release at the end instead of one per ART change.

## Tests

- **#12 regression:** the reporter's model (`period.txt`: one periodic thread, `Period => 100 us`)
  as a `HamrTranspileTests` Linux case, expecting `Periodic(period = s64"100000")` and passing the
  bound-checked C Demo smoke run.
- **Unit tests for `toPicoseconds`:**
  - OSATE double formats, including noisy ones such as `"3.3299999999999996E10"` → 33300000000
    with no warning;
  - a genuine sub-picosecond fraction (`"1.4"` ps, as SysML can produce) → 1 with a
    `time-rounding` warning;
  - every `Time_Units` literal;
  - missing or unknown units and unparsable values as errors.
- **Unit tests for `fromPicoseconds`:**
  - exact conversions;
  - rounding to nearest with a `time-rounding` warning, including ties;
  - a model-defined resolution (a `Clock_Period` tick);
  - 0 and too-large values as errors.
- **Common:**
  - the value linter (D5): a zero `Period`, `Frame_Period`, `Clock_Period` and `Slot_Time` are
    each an error; `Compute_Execution_Time => 0 ms .. 5 ms`
    is accepted, while `0 ms .. 0 ms` and `5 ms .. 2 ms` are errors, the latter on a non-Microkit
    platform;
  - `Slot_Time` is read with its unit (F7.2), and a unitless `Slot_Time` is read as picoseconds
    with a warning (D5);
  - the de-duplicating helper (D4): the same text and position is reported once; the same text at
    a different position, and different text at the same position, are both reported;
  - two instances of one subcomponent declaration (`p1.worker`, `p2.worker` of one `P.i`), with
    `Period => 1.5 ms`, under the CAmkES periodic dispatcher (1 ms resolution, so each rounds with
    a warning), give two warnings, each naming its instance's path (D4);
  - a `Period` that does not parse is reported once, not also as missing (D5).
- **SysML:** a SysML model using `us` produces the right picosecond value (D9).
- **ART:** runtime-level tests (the sleep helper, `ArtTime`, the clock origin and event-port order
  on the JVM) go in the `art` submodule's existing test-source folder, `art/shared/src/test/scala`,
  which the `moduleSharedPub` helper in `art/bin/project.cmd` already declares
  (`ProjectUtil.scala:107-108`). They need no new module or dependency: `slang-embedded-art`
  depends on `library-shared`, which already depends on the runtime's `test` module, the one that
  provides `TestSuite` (`runtime/bin/project.cmd:66`). `ArsitLibrary_Ext` embeds only
  `art/shared/src/main/scala`, so the tests are not copied into generated projects. Tests that need
  generated code go in codegen's suites.
  - a 32-bit `--bit-width` Linux case with a period, and a `Frame_Period`, over 2.1 s (F9, D6);
  - first dispatch: a periodic thread is dispatched on the first scheduler pass, and a sporadic
    thread on its first event (F10);
  - the clock is non-negative in the JVM unit-test path that never calls `Art.run` (F4, D6);
  - the legacy scheduler's sleep: `LegacyInterface_Ext.sleepArgs` (D6), unit-tested directly, including sub-millisecond and multi-second
    values (`LegacyInterface_Ext.computePhase` itself blocks on `Console.in.readLine()`, so it is not
    run in tests);
  - `ArtTimer` delays are nanoseconds and `ArtTime.millis`/`micros` convert correctly (ART module);
    and `ArtTime.millis(3000)` in a 32-bit `--bit-width` build: the #12 regression model's
    behaviour code is given an `ArtTime.millis(3000)` value that it logs, run through the Linux
    Demo smoke run;
  - event-port order on the JVM: a sporadic thread with values inserted on several same-urgency
    event ports in one step, in the reverse of the ports' declaration order, sees them in insertion
    order (today's declaration-order fallback would give the opposite), independent of the clock
    (F15);
  - event-port order in the transpiled Linux `Demo` (`ArtNativeSlang`): one producer that
    `sendOutput`s on two event ports connected crosswise to the same consumer's same-urgency ports,
    so arrival order is the reverse of the consumer's declaration order, and the consumer logs the
    order it handles them in (F15, D6);
  - the default period: a periodic device without `Period` (devices default to periodic,
    `SymbolResolver.scala:358`, and are not checked by the linter; a periodic thread without
    `Period` is already an error, `Linter.scala:34-35`), and a sporadic thread whose `min`
    comes from that default (`ArchitectureTemplate.scala:13-16`), get 1 ms in ns, including the
    legacy apps' post-dispatch sleep (`ArtNixTemplate.scala:148-158`, whose period `ArtNixGen.scala:164` reads).
- **Microkit:**
  - a model with sub-millisecond `Compute_Execution_Time` on both domain scheduling (us entries) and
    MCS (ns timeslices);
  - a value that is not a whole microsecond under domain scheduling (warning);
  - a value that rounds to 0 (error);
  - a model whose `Frame_Period` is not a whole microsecond (e.g. `1000.5 us`), under domain
    scheduling with a monitor plugin enabled, which re-resolves the model and re-converts the frame
    period, reports exactly one `time-rounding` warning (D3, D4);
  - a zero pad is omitted from the domain schedule.
- **SysML, several systems:** a SysML file with two systems, where only the second has an inexact
  time, run through SysML codegen for the second system, reports its `time-rounding` warning once
  (D3). The value is a sub-picosecond fraction that still rounds to a usable time, e.g.
  `Period = 1000000.4 [ps]`, which rounds to 1000000 ps = 1 µs, exact in nanoseconds. (For SysML
  input the only warning `SymbolResolver` can raise is a sub-picosecond one, D2, and a value such
  as `1.4 ps` would round to 1 ps, which is 0 ns and an error on the ART backends.)
- **CAmkES:**
  - in periodic-dispatcher mode (not the pacer, where `Period` only appears in comments), a sub-ms
    `Period` gives an error, not a generated modulo by zero;
  - a `Compute_Execution_Time` that is not a whole number of `Clock_Period` ticks gives a warning;
  - a `Clock_Period` that does not divide the pacer's fixed slots warns about them (D8);
  - a `Clock_Period` that is not a whole number of milliseconds (e.g. `1.5 ms`) gives an error,
    reported once, at the bound processor, and only for CAmkES (the same model on JVM/Linux does
    not error);
  - a `Compute_Execution_Time` below half a `Clock_Period` gives an error;
  - with a `Clock_Period` over 20 ms, the 10 ms fixed slots are clamped to one tick with a warning;
  - a thread without `Compute_Execution_Time`, and a process/VM whose threads all lack it, get the
    50 ms default with a warning, and no zero-length entry is generated;
  - entries that exceed the frame (a pad below 0) give an error, from a small dedicated model whose
    entries do not fit its `Frame_Period`; a pad of exactly 0 is omitted. (`VPM_ben`, which
    generated a pad of -55 ticks, had its `Frame_Period` raised from 500 ms to 700 ms so that it
    still tests a valid schedule);
  - the generated entries, pad included, add up to the frame in ticks.
- **ROS 2:** expected timers in ns (C++, micro-ROS) and decimal seconds (Python).
- **Expectations:** most expected files change (ns periods, ART sources, Microkit schedules). The
  suites are run and the diffs reported for review; expectations are regenerated only once the diffs
  are accepted.

**Where the tests live (step 9).**

- `ExactTimeTests`: one small AADL model per scenario under `resources/models/ExactTimeTests`
  (AIR generated with phantom), run through codegen with expected errors and expected warnings. The
  harness's `test` gained an `expectedWarnings` parameter (`CodegenTest.ExpectedWarnings`: message
  kind, a text fragment and an exact count), checked whether or not errors are expected.
- `ExactTimeResolveTests`: what AADL cannot express, on those models' AIR patched in memory and
  resolved directly (so phantom mode, which regenerates AIR from the AADL, does not bypass the
  patch): an inverted `Compute_Execution_Time` (OSATE rejects `5 ms .. 2 ms` itself), a `Period`
  that does not parse, and `Slot_Time` with and without a unit.
- `HamrTranspileTests`: the #12 model (`period-100us`), a 32-bit `--bit-width` model with a 3 s
  period and `Frame_Period` (`long-period-32bit`), and the default period on a periodic device and
  a sporadic thread (`default-period`), each built and run through the Linux Demo and legacy apps.
- `TimeUtilTests` and the ART unit tests (`art/shared/src/test/scala`), as above.
- SysML: `TestFrontEnd_TimeUnits` (every unit, and a decimal value) and `TestFrontEnd_TwoSystems`.
  Writing the latter found that the front end stopped on any decimal time value (`1.5[ms]`): it
  parsed the literal's printed form, which `R` cannot read. It now reads the literal's value.
- Two items need behaviour code in a generated project, which the harness does not provide, and are
  covered at another level instead: `ArtTime.millis(3000)` in a 32-bit build (`ArtTime` is unit
  tested, and `long-period-32bit` carries 3 s values through a 32-bit build), and event-port order
  in the transpiled Demo (`ArtNativeSlang`'s ordering is unit tested on the JVM through
  `sendOutput` with crosswise connections).

## Compatibility

- **Generated code:** times in generated code change units: ART ns, Microkit domain us, MCS ns,
  ROS ns. For models with whole-millisecond values the behaviour is unchanged apart from the
  deliberate changes (D8).
- **User code:**
  - code that reads `Art.time()`, message timestamps or the `"time"` field of ART's JSON log
    sees nanoseconds since its process's ART clock started, not milliseconds since the epoch, and
    timestamps from different processes have different origins (D6);
  - code that calls `ArtTimer` must pass nanoseconds or use `ArtTime.millis`/`micros` (D6);
  - code that matches on `Periodic(period)` or `Sporadic(min)` sees `Art.Time` instead of `Z`;
  - code that reads the generated `Schedulers.scala` timing types sees `Option[Art.Time]` for the
    time fields (`Option[(Art.Time, Art.Time)]` for the compute execution time), and
    `Schedulers.framePeriod` is an `Art.Time` in nanoseconds;
  - code that calls the Linux nix `Process.sleep` passes nanoseconds as `Art.Time`;
  - generated tests that depend on the dispatch order of same-urgency event ports may see a
    different order (D8).
  All of these are noted in `changelog.md`.
- **Existing projects:** regenerating updates `Arch.scala`, `Schedulers.scala`, the bridges,
  `process.c` and `ipc.c` (both `overwrite = T`; `ipc.c` gains `nanosleep`, `ArtNixGen.scala:275`),
  but not `legacy.c`, `round_robin.c` or `static_scheduler.c` (`overwrite = F`, F14).
  None of those three changes in this design.
- **SysML:** SysML models that used `us` were generating values 10^6 times too small (F7.1). They
  are now correct, which is a behavioural change for those models.

## Open questions

1. **rclpy timer API (F7.3): resolved in step 7.** rclpy (Humble, Jazzy and Rolling) has no
   `create_wall_timer`, so the generated call would fail; its `create_timer(timer_period_sec, ...)`
   takes seconds and computes `int(float(timer_period_sec) * S_TO_NS)`, which truncates: a plain
   `ns / 1e9` loses a nanosecond for about 2% of values. The generator emits
   `create_timer((<ns> + 0.5) / 1e9, ...)`, which truncates back to exactly `<ns>` (checked for
   3.5 million values up to 10^15 ns, about 11.5 days; beyond that the half nanosecond is lost to
   the double's precision).
   Python node generation is itself unreachable (`Ros2Codegen` reports it as not supported), so
   this keeps the dormant generator correct for whoever wires it up.
2. **arsit static schedule (F7.4, F14):** confirm the intent of `maxExecutionTime` and the
   hard-coded C `ScheduleProvider` before touching them.
3. **Strict mode (D11):** whether codegen should offer an option that turns `time-rounding`
   warnings into errors.
