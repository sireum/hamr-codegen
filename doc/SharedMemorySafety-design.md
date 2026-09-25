# Shared Memory Safety Design: Sizes, Layout and Validation of Inter-PD Messages on Microkit

## Problem

An external review of INSPECTA (Red Balloon Security, September 2026) reported that when an
inter-protection-domain message has an array type, HAMR does not check the developer-provided
message size against the size of the C type it generates. A mismatch can overflow a buffer, or
copy only part of one, in the generated system. The defect is in generated C, below the Rust
components, so neither Verus nor the Rust compiler can see it. The review also raised memory
alignment: different compilers may lay the same type out differently, which could be used to
inject malicious data.

Checking the code confirmed the report and turned up two more defects of the same family. All
of them come from one design flaw: **the generated system trusts sizes and layouts it never
checks.** The size of a message comes from a model property instead of the type; the size of a
shared memory region comes from one element instead of the queue it holds; and the two ends of
a region -- possibly built by different compilers -- are assumed to agree on its layout and on
which bit patterns are valid.

**Why the model declares sizes at all.** `Memory_Properties::Data_Size` was required on array
types because a character had a different size in C, Rust and Scala, so HAMR could not derive
one byte size for all of them. That is no longer true on Microkit: `Base_Types::Character` is
`char` in C and `u8` in Rust (`MicrokitTypeUtil.scala:176-178`, deliberately not Rust's 4-byte
`char`), and strings are already a HAMR-sized fixed array of `Character`. HAMR can therefore
compute every size itself, and the declared size has become a second, unchecked source of truth.

This document records the threat model, the findings (F1-F4), what is already correct (F5), and
the fixes (D1-D8). **Status: implemented and verified -- see "As built" at the end; still
pending: the INSPECTA models under QEMU in GitHub CI, and review of the golden diffs.** Scope is
the Microkit platform: generated C types, queues, bridges, the Rust `data` crate and the system
description. The JVM/Slang platform shares memory through Slang
values and is not affected.

## Threat model

Each shared memory region connects one sending protection domain to one or more receivers. The
sender maps it read-write; each receiver maps it read-only. seL4 guarantees nothing else can
touch it.
(The one exception is the test-scheduler variant, whose test controller maps every observable
region read-write in order to inject values. It is trusted test infrastructure and does not ship,
so it is outside this threat model.)

- A **faulty or compromised sender** already controls the full contents of its outgoing regions:
  it can send any bytes it likes. That is not something this design can or should prevent.
- What it **must not** be able to do is (a) cause undefined behaviour or memory corruption in a
  **receiver**, or (b) affect a **different connection**.
- Separately, a **correct system built from a wrong model** must not corrupt itself: the
  generated code must not depend on numbers the model can get wrong.

Against that:

| Finding | Who can trigger it | Severity |
|---------|--------------------|----------|
| F1 declared-size copies | a wrong model; the copy size is fixed at codegen, so a sender cannot choose it | High: silent overflow or truncation in every system whose model is wrong, on both the send and receive side |
| F3 region smaller than its queue | any system with large elements, even with a correct model | High for correctness. Sender side: corrupts the sender's own next region -- another connection. Receiver side: reads past the region into the receiver's next region |
| F4 layout disagreement | a build that compiles one side differently | High: the two sides read different fields from the same bytes |
| F4 invalid bit patterns | a faulty or compromised sender | High: undefined behaviour in the receiving Rust component, voiding its Verus guarantees. **This is the real attack surface** |

## Findings

### F1. Array copies use the declared size, not the type's size (the reported defect)

For an array type the linter requires `Memory_Properties::Data_Size` and checks only that it is
positive (`MicrokitTypeUtil.scala:341-348`). The C type plugin then emits two independent sizes
for the same type (`CTypePlugin.scala:165-177`):

```c
#define T_BYTE_SIZE 80          // Memory_Properties::Data_Size / 8   -- declared
#define T_DIM_0 10              // Data_Model::Dimension             -- real
typedef base T [T_DIM_0];       // the real layout: T_DIM_0 * sizeof(base)
```

Every generated copy of an array value uses the declared one:

| Where | Code |
|-------|------|
| queue enqueue (`QueueTemplate.scala:674`) | `memcpy(&queue->elt[index], data, T_BYTE_SIZE)` |
| queue dequeue (`QueueTemplate.scala:681`) | `memcpy(data, &queue->elt[index], T_BYTE_SIZE)` |
| data-port getters (`QueueTemplate.scala:261-262`) | `memcpy(&last, &fresh_data, T_BYTE_SIZE)`, `memcpy(data, &last, T_BYTE_SIZE)` |
| event-data getters (`QueueTemplate.scala:362-364`) | `memcpy(data, &freshData, T_BYTE_SIZE)`, `memcpy(data, &lastPayload, T_BYTE_SIZE)` |

Records and base types are copied by assignment (`queue->elt[index] = *data`), which uses the
real `sizeof`; only arrays are affected.

- **Declared size too large:** each copy overflows. On enqueue it writes past `elt[index]` into
  the next element and past the end of the queue struct, into whatever the sending PD has mapped
  next (F3), and it also *reads* past the sender's source buffer -- Rust memory, for a Rust
  sender. On receive it writes past the caller's buffer.
- **Declared size too small:** part of every message is silently never copied.

The Rust side does not protect against this. The `data` crate declares arrays correctly as
`[T; DIM]` (`CRustTypePlugin.scala:362-372`) but also exports the declared size as
`T_BYTE_SIZE`, and the bridge passes a Rust buffer into the C getter:

```rust
let value: *mut T = &mut [Base::default(); T_DIM_0];
if (get_<port>(value)) { return Some(*value); } else { return None; }
```

The getter then copies `T_BYTE_SIZE` bytes into that Rust-owned buffer. That is an `unsafe` FFI
boundary: neither Verus nor rustc reasons about what C does behind it. (That buffer is also a
stack temporary sized like the element, so a large array type is a stack-size risk on every read
independently of F1; D6's static staging buffer does not remove it, because the value still has
to be returned to Rust. PD stack sizes must be checked against the largest element a component
reads.)

Example: INSPECTA's `aadl_port_types/event_data/array` declares `ArrayOfStruct` with
`Dimension => (10)` over an 8-byte record and `Data_Size => 80 Bytes`, which happens to be
right. Declaring `160 Bytes` instead generates 160-byte copies into 80-byte objects, and nothing
in the toolchain reports it.

### F2. The declared size depends on the ABI, so developers cannot reliably get it right

For an array of records, the correct `Data_Size` includes padding, which depends on the target
ABI. Under AArch64 (AAPCS64), `struct { Integer_8 a; Integer_32 b; }` is 8 bytes, not the 5 a
developer gets by adding up the fields. The property asks the developer for a number the compiler
decides, and F1 turns a wrong answer into memory corruption.

### F3. A shared memory region can be smaller than the queue it holds

Each port's region is sized from **one element's declared size**, with a 4 KiB minimum
(`ConnectionUtil.scala:44-50`, `173-181`, `219-227`):

```
region = max(4 KiB, ceil(Data_Size in bytes / 1024) KiB), rounded up to the 4 KiB page
region = 4 KiB                                          when the type declares no Data_Size
```

Arrays must declare `Data_Size`, but records usually do not, so **a record's region is 4 KiB
whatever the record's real size**. A record containing a 5 KiB array field overflows its region
with nothing declared wrong.

What the region actually holds is the queue struct (`sb_queue_<T>_<q>.h`):

```c
typedef struct {
  _Atomic sb_event_counter_t numSent;   // uintmax_t: 8 bytes
  T elt[q + 1];                         // one extra slot is always dirty
} sb_queue_T_q_t;
```

So the struct needs `8 + (q + 1) * sizeof(T)` bytes (plus any alignment padding before `elt`),
which the region formula ignores. With queue size 1 (every data port, and the default for event
data ports) the struct outgrows a 4 KiB region once an element exceeds about 2044 bytes; a
4096-byte element needs 8200 bytes in a 4096-byte region. **This overflows even when every
declaration is correct.**

Within a protection domain, regions are placed back to back in its virtual address space
(`CComponentPlugin_MCS.scala:247-251`: `next += size`). An overflow therefore does not fault: on
the sending side it lands in the next of the sender's regions -- possibly a queue to a different
receiver; on the receiving side the read continues into the receiver's next region.

### F4. Both ends of a region must agree on its layout and on valid values, and nothing checks either

- **Layout.** C components are compiled by clang; Rust components mirror the same types with
  `#[repr(C)]`. The two agree only while both follow the same target ABI. A compiler flag such as
  `-fshort-enums` or `-fpack-struct`, a different C compiler for one component, or a
  hand-written component with its own declaration of the type changes one side's layout
  silently. The two sides then read different fields from the same bytes. Nothing checks that
  their views match.
- **Validity.** Rust `#[repr(C)] enum` and `bool` have validity invariants: an enum must hold one
  of its declared discriminants, a `bool` must be 0 or 1. A faulty or compromised sender can
  write any bit pattern into its region. When the receiver's C getter copies such a value into a
  Rust `enum` or `bool`, the Rust program has undefined behaviour, and every property Verus
  proved about the component assumes it cannot happen. Today the dequeue copies straight into
  the Rust caller's buffer (F1's `&mut [Base::default(); T_DIM_0]`), so an invalid value lands
  in typed Rust memory before anything could check it -- even before the dequeue's own check
  that the sender did not overwrite the slot mid-read.

### F5. What is already correct

- **seL4/Microkit alignment.** Microkit requires memory regions to be page-aligned and a whole
  number of pages. HAMR rounds both the virtual address and the size of every region up to the
  4 KiB page (`MicrokitUtil.KiBytesToHexH`, used for `vaddr` and for `size` in
  `SystemDescription.scala:383`). A page-aligned base also satisfies the 8-byte alignment of the
  atomic `numSent` counter and of any element type HAMR generates. The Microkit-specific defect
  is region *sizing* (F3), not alignment.
- **Queue control fields.** `numSent` is written by the sender, so a compromised sender controls
  it. The receiver computes the slot as `(numRecv - 1) % SIZE` from its own private `numRecv`
  (`sb_queue_<T>_<q>.c`), so a bad `numSent` can cause dropped or repeated messages but never an
  out-of-bounds index.
- **Strings** are already sized by HAMR: every `Base_Types::String` is replaced by one fixed
  array of `Character` whose length is `1 +` the largest `Data_Model::Dimension` declared on any
  string in the model (`MicrokitTypeUtil.scala:440-465`). When no string declares one, the length
  is a hard-coded 100 rather than `--max-string-size` (D2).
- **Unbounded arrays** are already rejected on Microkit, as are arrays that are not
  `HAMR::Array_Size_Kind => Fixed` (`MicrokitTypeUtil.scala:324-337`).

## Fixes

### D1. HAMR computes every size; `Data_Size` is an error on Microkit

- **HAMR computes the layout** -- size, alignment and field offsets -- of every data type it
  generates, from the AArch64 ABI rules: fixed sizes and natural alignment for the base types
  (`Character` is 1 byte), records laid out in field order with padding, enums as 32-bit
  integers, arrays as `dim * size(elem)`. HAMR's types use only fixed-width integers, `bool`,
  floats and 32-bit enums, so the same layout holds on every 64-bit target: the AArch64 seL4
  image and the x86_64 / arm64 host builds of `make test`. A 32-bit target would need its own
  rules. This computation supplies D1's sizes, D4's region sizing, D5's layout assertions and
  D6's per-type validators.
- **A `Memory_Properties::Data_Size` that differs from HAMR's computed size is a lint error
  when the target is Microkit**, with a message saying HAMR computes the size. (Originally any
  declared `Data_Size` on a model data type was to be an error, with the base-type libraries
  exempt; see the next bullet for why only a mismatch is.) Base types are checked like any
  other; the values the AADL `Base_Types` package and the SysML AADL libraries declare match.
  The existing "`Data_Size` must be specified" check for arrays is removed.
- **The rule fires only where the property is declared on the type itself, not inherited.** A
  model type that extends a base type (`data Temp extends Base_Types::Integer_32`, common AADL
  style) inherits its `Data_Size`. `TypeResolver` reads the property from `c.properties`, which
  does not say where a value came from; if AIR flattens inherited properties onto the extending
  type, a package-based exemption would fire on a model that declared nothing. Before writing
  the lint, check how an inherited `Data_Size` appears in AIR (no INSPECTA model extends a base
  type today, so add one to the golden tests). If the declaring type cannot be recovered, a
  possible fallback is to error only when a declared value differs from the computed size.
  **Decided: the declaring type cannot be recovered, so the fallback was adopted** -- see "As
  built".
- C: `#define T_BYTE_SIZE (sizeof(T))`, emitted after the `typedef`. Every `memcpy` in F1 then
  copies exactly the type's size.
- Rust: the `T_BYTE_SIZE` constant is dropped: no generated code read it, and its value was
  the declared size.
- Region sizing never reads `bitSize` (the type's declared size): it uses the computed layout
  (`MicrokitLayout`). `bitSize` is now only compared against that layout by the lint.

### D2. Strings default to `--max-string-size`

Keep the current rule -- the string array's length is `1 +` the largest `Data_Model::Dimension`
declared on any string in the model -- but when no string declares one, use `--max-string-size`
(`HamrCli.maxStringSize`) instead of the hard-coded 100. The option's default is also 100, so
nothing changes unless it is set.

### D3. Array dimensions must be declared

Unbounded arrays stay unsupported on Microkit. The existing rejection becomes an explicit lint
rule: an array data type with no `Data_Model::Dimension` is an error whose message tells the
developer to declare one. (A future bounded representation -- `struct { uint32_t len; base
elems[--max-array-size]; }` with `len` validated on receive -- was considered and deferred.)

### D4. Regions are sized for the whole queue, and the build proves it

- Size each region from the queue struct: `8 + (q + 1) * size(T)`, plus alignment padding,
  rounded up to the page, with `size(T)` from D1's computed layout -- for records as well as
  arrays.
- Emit `#define <QUEUE>_REGION_BYTES <bytes>` and
  `_Static_assert(sizeof(<queue>_t) <= <QUEUE>_REGION_BYTES, "...")` in each queue's header,
  which every component that maps the queue includes. Any future sizing error then fails the
  build instead of overflowing into a neighbour. The regions HAMR itself places at fixed
  addresses with a fixed 4 KiB size -- the scheduler's `sched_state` and `sched_schedule`
  queues, and the test scheduler's `test_cmd`, `test_status` and `test_schedule` structs --
  get the same assertion in C (and, for the test controller's Rust mirror of those structs,
  in Rust).
- **Every allocator of virtual addresses must place regions by their real size.**
  `CComponentPlugin_MCS` already accumulates `sizeInKiBytes`, but `TestSchedulerPlugin` maps the
  observable and injection regions into the test controller at a fixed 4 KiB stride
  (`observableVaddrKiB(i) = base + i * 4`, `injectVaddrKiB`, `injectThreadVaddrKiB`; lines 146,
  285, 292). That is only correct while every region fits in 4 KiB; once D4 makes one larger,
  the controller's mapping of it overlaps the next. Those allocators must accumulate region
  sizes too, and the compile-time constants the controller's accessors use must come from the
  same computation.

### D5. Both sides assert the layout HAMR computed

From D1's layout, generate for every type that crosses a region:

- C: `_Static_assert(sizeof(T) == ...)`, `_Static_assert(_Alignof(T) == ...)`, and
  `_Static_assert(offsetof(T, f) == ...)` for every field.
- Rust: the same with `size_of`, `align_of` and `core::mem::offset_of!`.

A compiler, flag or hand-written declaration that lays a type out differently then fails that
component's build. The two sides can no longer silently disagree about shared memory.

### D6. Received data is validated in a C staging buffer before any receiver sees it

- For an element type that can hold an invalid bit pattern, the dequeue copies into a **C
  staging buffer**, not the caller's buffer. Its existing check that the sender did not
  overwrite the slot during the read runs first; then every enum, `bool` and string reachable
  in the value (including inside records and arrays) is checked on the staging copy -- enum
  within its declared discriminants, `bool` 0 or 1, a string terminated by a NUL within its
  bounds (a C receiver that treats an unterminated one as a C string would read past it). Only
  a valid value is copied to the caller. Every other element type copies straight to the
  caller, as before (see "As built").
- The staging buffer is **static**, not a stack temporary: one per queue type in each PD (as
  built; shared by that PD's ports of the same type and queue size). Elements can be several KiB
  (the reason for D4) while protection-domain stacks are small; a static buffer is safe because
  a PD is single-threaded, `notified()` does not re-enter, and the buffer is used up within one
  dequeue.
- Validating the staging copy, not shared memory, matters: the sender can change shared memory
  between a check and a copy.
- An invalid message is dropped: event ports return "no new value"; data ports keep returning
  the last valid value (which the staging buffer makes possible). Each drop is counted in a
  per-port counter in the receiver's own memory, readable through a generated getter (for
  logging or a GUMBO-level check): `get_<port>_num_invalid()` in C and in the Rust component
  API. Making the counters visible to other protection domains --
  a monitor, the test controller -- would need a region and is a follow-up. Nothing invalid
  reaches Rust.
- The monitor PDs and the test controller receive through the same generated C getters, so they
  are covered with no extra work.
- Cost: one extra copy per read, paid only by queues whose element type can hold an invalid bit
  pattern (see "As built": the staging and check are skipped for every other type).
- Alternative, if dropping in C is undesirable: pass enums and `bool`s across the FFI boundary
  as integers and convert with a checked `TryFrom` in the Rust bridge. Either way, the invariant
  is that no unchecked bit pattern from another protection domain becomes a Rust `enum` or
  `bool`.

### D7. Guard pages between regions (defense in depth)

Leave one unmapped page between consecutive regions in each protection domain's address space --
in `CComponentPlugin_MCS` (`nextMemAddressInKiBytes`) and in the test-scheduler allocators D4
converts to size-based placement, which also place regions back to back in the test controller.
A residual overflow then faults at the first byte past a region instead of silently reaching a
neighbour. The cost is virtual address space only. The fixed virtual-address blocks of the
monitor and test-scheduler regions (`sched_state`, `test_cmd`, observable and injection regions)
must not move.

### D8. Migrate the models that declare `Data_Size`

As first designed, D1 turned every existing `Data_Size` on a model data type into a Microkit
lint error. (As built only a mismatch is an error, so no declaration had to go -- see "As
built".) Today 28 model files in INSPECTA-models declare it, across about 16 models (the
`aadl_port_types` and `port_queues` examples, `aadl_datatypes`, `gumbo-verus/structs_arrays`,
`vest`, `r2u2_monitor`, `firewall-simple-byte-array`, `isolate-ethernet-simple`,
`open-platform`, and others). Each is checked against the compiler's `sizeof` of the generated
type (stage 0), so any model whose declaration was wrong (and has been running on a truncated or
overflowing copy) is identified. The original plan also removed them before D1 landed; with
mismatch-only D1, the correct ones stay, and a model that still targets a platform needing
`Data_Size` keeps working.

## Decisions

| # | Decision | Rationale |
|---|----------|-----------|
| D1 | HAMR computes every type's layout from the AArch64 ABI rules; C `T_BYTE_SIZE` is `sizeof` (the unused Rust constant is removed); a `Data_Size` on a data type is a Microkit lint error when it differs from the computed size | The declared size existed because character sizes differed across languages, which no longer holds on Microkit (1-byte `Character` in C and Rust). A declared size is only a way to be wrong (F1, F2). Only a mismatch is an error because the frontend flattens inherited properties, so a declared value cannot be told from an inherited one |
| D2 | Strings keep the largest declared dimension, defaulting to `--max-string-size` when none is declared | Same behaviour by default, but the length is configurable instead of hard-coded |
| D3 | Unbounded arrays stay unsupported; an array without `Data_Model::Dimension` is a lint error | Every shared type has a fixed, computable size; a bounded representation is deferred |
| D4 | Regions are sized for `8 + (q + 1) * size(T)` rounded to the page, each component asserts its queue fits, and every vaddr allocator places regions by size | Fixes F3, which overflows even with a correct model |
| D5 | Both C and Rust assert HAMR's computed layout (size, alignment, field offsets) | Two sides of a region cannot silently disagree about its layout (F4) |
| D6 | A queue whose element type holds a `bool`, an enum or a string stages each received value in a static C buffer (one per queue type per PD) and validates it there before it reaches the receiver, C or Rust; invalid messages are dropped and counted per port, readable as `get_<port>_num_invalid()` in C and Rust. Other queues copy straight to the caller, as before | An unchecked enum or `bool` bit pattern from another PD is undefined behaviour in Rust and voids Verus' guarantees (F4), and an unterminated string is an over-read in any C receiver that treats it as a C string; validating shared memory in place is a time-of-check/time-of-use race. Types with no invalid bit patterns have nothing to check, so they pay nothing |
| D7 | One unmapped guard page between regions | Any overflow that survives D1-D5 faults instead of reaching a neighbouring queue |
| D8 | Existing `Data_Size` declarations are audited against the compiler's `sizeof`; since D1 rejects only a mismatch, correct ones stay | A mismatch found in the audit is a model that has been running on bad copies (none was found) |

## Implementation Plan

| Stage | Work | Verified by |
|-------|------|-------------|
| 0 | D8: for each model, compile one small C file that includes the model's generated headers and prints `sizeof` of every generated type; compare with each declared `Data_Size` and list mismatches; remove the declarations from INSPECTA-models and other known models. Keep the program: it is the compiler's own ground truth for checking D1's layout computation in stage 1 | the mismatch list reviewed; the models still build |
| 1 | D1-D3: layout computation; `T_BYTE_SIZE` from `sizeof`/`size_of`; the `Data_Size` and missing-dimension lint errors; `--max-string-size` default | golden diffs; HAMR's computed size of every type in the stage-0 models equals the compiler's `sizeof`; a model with `Data_Size` on a data type fails codegen naming the type; a model with an undimensioned array fails codegen; the INSPECTA array models build and run |
| 2 | D4: queue-sized regions, per-region size defines and `_Static_assert`; size-based vaddr allocation in `TestSchedulerPlugin` | golden diffs; a model with a 4096-byte array, and one with a record holding a 5 KiB field, each with queue size 1, build, run under QEMU and exchange messages intact (today both overflow); shrinking a region by hand fails the build; the test controller's observable and injection mappings of an oversized region do not overlap, and the isolette system tests still pass |
| 3 | D5: C and Rust layout assertions | golden diffs; building one C component with `-fshort-enums` or `-fpack-struct` fails with an assertion naming the type and field |
| 4 | D6: per-port static staging buffers, validation of enums and `bool`s, drop counters | after an out-of-range enum and a `bool` of 2 are written into a region, the message is dropped, the drop counter's getter reports it, and the Rust component is unaffected. Needs a raw region write: either the test scheduler's open item "raw region writes for negative testing" or a small C test component |
| 5 | D7: guard pages | a deliberately oversized write faults at the region boundary under QEMU instead of changing the neighbouring queue; the fixed monitor and test-scheduler vaddr blocks are unchanged |

Stages 1 and 2 close the two concrete defects (F1, F3). Stages 3 and 4 answer the layout and
injection concerns (F2, F4). Every stage changes generated code across the Microkit golden
tests, which must be reviewed and regenerated.

This table is the plan as written before implementation. Three rows were superseded -- stage 0
kept the correct `Data_Size` declarations, stage 1's lint rejects only a mismatched
`Data_Size`, and stage 4's staging buffers are per queue type, only for types that need
checking -- see "As built".

## Open Items

- `HAMR::Bit_Codec_Max_Size` is rejected on Microkit today; if it is ever supported, its sizes
  need the same treatment.
- D6 covers enums, `bool`s and strings. Floating-point NaN payloads and signed integers have no
  validity invariant in Rust or C, so they need no check for memory safety; whether GUMBO
  contracts should reject them is a separate, semantic question.
- A bounded representation for unbounded arrays using `--max-array-size` (D3, deferred).
- The review's own reproduction was not shared; once available, it should become a regression
  test for stage 1.
- **Stack size for large messages (F1's stack-temporary risk).** The Rust bridge reads a value
  into a stack temporary and returns it by value, so a component that reads a large message
  needs several times the element size in stack. Microkit's default PD stack is 8 KiB: the QEMU
  test's Rust consumer, which reads a 4400-byte and a 5004-byte message, faulted on its first
  dispatch with a stack overflow until its thread was given `Memory_Properties::Stack_Size`
  (64 KiB). Models with large messages already set it (e.g. `firewall-simple-byte-array`, 1 MiB),
  but nothing tells the developer. HAMR could warn, or raise the PD's stack to a floor computed
  from the largest element it reads.
- **The mapped region size is not asserted.** `<QUEUE>_REGION_BYTES` and the region size in the
  system description come from the same computation, but the build checks the queue only against
  the former. Hand-editing the system description to shrink a region still builds; the guard page
  then turns the overflow into a fault (verified under QEMU).
- The invalid-message counters are visible only inside the receiving PD; exposing them to a
  monitor or the test controller needs a region (D6).
- **Pre-existing: a Microkit model that uses `Base_Types::String` but not `Base_Types::Character`
  crashes codegen.** The string's substituted `Character` array needs `Character` among the
  touched types for the type ordering, and the GUMBO and Rust type plugins need it among the
  model's own types; neither holds unless the model uses `Character` itself. (Found while adding
  a string port to `models/SharedMemorySafety`, which now also has a `Character` port to avoid
  it; independent of this design.)

## As built

All of D1-D7 are implemented; D8 needed no model changes. Where the implementation departs from
the text above:

- **D1 -- a declared `Data_Size` is an error only when it disagrees.** The AADL frontend flattens
  inherited properties and records no origin (the OSATE plugin collects
  `getAllPropertyAssociations()` and positions each property at its *definition*), so a type
  extending a base type carries the base type's `Data_Size` and HAMR cannot tell it from one the
  developer wrote. Decision: `Data_Size` on a model data type is a lint error only when it differs
  from HAMR's computed size; a matching one is accepted. Every value that could do harm is still
  rejected.
- **D1 -- the Rust `T_BYTE_SIZE` constant is removed**, not redefined: nothing read it.
- **D3 -- two paths reject an undimensioned array**: a `Fixed` or `Bounded` array without a
  dimension is already rejected by the common type resolver; an array with neither a size kind
  nor a dimension resolves as unbounded and reaches the Microkit lint, whose message now says to
  attach `Data_Model::Dimension`.
- **D6 -- validation lives inside the queue's dequeue**, not in each bridge getter. The dequeue is
  where shared bytes are copied out: it copies the slot into a `static` staging buffer with
  `memcpy` (a typed load of an invalid `bool` is itself undefined in C), runs its existing
  overwrite check, then calls the element type's generated `<T>_is_valid`. Rejections are counted
  in the receiver's own `Recv_t` (`numInvalid`, read with `<queue>_numInvalid`); each data and
  event data port a component receives on exposes its count as `get_<port>_num_invalid()`, a C
  function and a method of the Rust component API (`u64`, no contract: it is not a port value).
  Synthetic ports -- a state variable's injection port -- have no such getter. The data-port
  getters already keep the last value when a dequeue returns false, so data ports keep their last
  valid value with no change, and the `peek` paths are covered because they call dequeue.
- **D6 -- staging and checking only where a value can be invalid.** Only a `bool` or an enum has
  bit patterns that are not values; every bit pattern of the integer, float and character types
  is one. A queue stages and checks its element only when the element type holds a `bool` or an
  enum somewhere inside it (`MicrokitLayout.hasInvalidBitPatterns`); any other queue copies the
  slot straight into the caller's buffer, exactly as before the fix, and its `numInvalid` stays
  0. This matters for large payloads: on the host, the extra staging copy made the dequeue of a
  4400-byte `int32` array about 1.4-1.8x slower (56-74 ns to about 102 ns) and put a 4400-byte
  static buffer in the image; now both are gone for such types. For a 16-byte record with a
  `bool` and an enum the check costs a few ns. The claim in D6 that data ports pay nothing
  (because staging replaces the `fresh_data` -> `last` copy) was not implemented: a data port
  whose type needs checking pays the extra copy like an event data port.
- **D6 -- strings.** A string's validator (`Base_Types_String_is_valid`) accepts it only if a
  NUL lies within its bounds, so a string queue stages and checks like an enum or `bool` one.
  (Found in review after D6 was built: an unterminated string is not undefined behaviour in
  Rust, where it is a byte array, but it is an over-read in any C receiver.)
- **D6 -- each protection domain links only the queues it uses.** The queue objects are linked
  through an archive, `$(TYPES_LIB)` (`libhamr_types.a`, built from `$(TYPE_OBJS)`), instead of
  every object being linked into every ELF, so a PD no longer carries the code -- and, for the
  checked types, the static staging buffer -- of every queue in the system.
- **D4 -- the test scheduler's injection regions** were also fixed-size (4 KiB, `0x1000`) whatever
  the state variable's type; they are now sized by the same layout computation, and the observable
  and injection blocks are checked not to run into each other.
- **D4 -- HAMR's own fixed 4 KiB regions** (`sched_state`, `sched_schedule`, `test_cmd`,
  `test_status`, `test_schedule`) had no fits-in-region check either. All fit today -- the
  largest, the `sched_schedule` queue, is 3352 bytes -- but raising `MAX_SCHEDULE_SLOTS` past
  about 155 would have overflowed it silently; each now has a `_Static_assert` (and the test
  controller's Rust structs a `const` assertion). These are compiled only by models built with
  runtime monitoring or the test scheduler, so their first build is pending GitHub CI
  (isolette, temp-control).
- **D8 -- no migration was needed**: stage 0 found every declared array size in INSPECTA-models
  correct, and no existing queue larger than its region (the largest are 3208-3352 bytes in 4 KiB
  regions). Both defects were latent in today's models.

**Verification.**

- `SharedMemorySafetyTests` (`jvm/src/test`), over `models/SharedMemorySafety`, whose types are
  chosen to hit every defect -- a 4400-byte array, a record holding a 5000-byte array, and a
  padded record with a `bool` and a 3-value enum: a wrong `Data_Size` is a codegen error and a
  matching one is accepted; an undimensioned array is rejected on both paths; the generated C
  compiles with every layout and region assertion holding; `#pragma pack(1)` fails the build with
  the layout message on every compiler, and so does `-fshort-enums` wherever the compiler honors
  it (clang targeting the MSVC ABI ignores it, so HAMR's layout holds there and the header
  compiles, as it should); a host-run C harness (`harness/harness.c`) round-trips
  the large queues with canaries around each region and destination, and writes an out-of-range
  enum, a `bool` of 2 and an enum of -1 into a region, each rejected and counted with the
  receiver's buffer untouched, and an unterminated string likewise; only the queues of the
  record with a `bool` and an enum and of the string stage and check, and every ELF links the
  queue archive rather than all queue objects; every port region in `meta.py` is followed by a
  guard page; and the Rust data crate's layout assertions hold under `cargo check`.
- **Negative controls:** the same harness run against code generated by the pre-fix HAMR fails
  14 checks -- both large queues overflow their regions (8808 bytes in 8192, 10016 in 4096) and
  all three invalid values are delivered -- and, with `BigArray` declared at twice its size, every
  copy overflows the receiver's buffer and the queue writes past its region. The pre-fix HAMR
  accepted that wrong size without complaint.
- All 227 generated queue files across the 46 Microkit golden output trees compile for the host
  and for AArch64 with every assertion holding: HAMR's layout matches clang's everywhere.
- Isolette's `microkit_mcs` variant end to end: codegen, `make` (Verus), `make test`, both monitor
  variants under QEMU with no violations, and the ported system tests.
- **On seL4 under QEMU** (`SharedMemorySafetyTests`, opt-in with `HAMR_SMS_QEMU`, driven by
  `models/SharedMemorySafety/qemu/`): a C producer sends valid messages, then writes an enum of 7
  and a `bool` of 2 into its padded region, then a valid message, then one byte past the end of
  its big region; the Rust consumer checks everything it receives. Stage 2: the 4400- and
  5004-byte messages arrive intact. Stage 4: both invalid messages are dropped before Rust sees
  them, `get_padded_num_invalid()` counts 1 then 2, and the next valid message arrives. Stage 5:
  the write faults at exactly the byte past the region (`0x10003000`, the guard page), and the
  neighbouring `rec` queue is still intact afterwards.
- **INSPECTA-models:** all 11 models with array types generate and build locally with this
  implementation (codegen, `make`, and `make test` where the model's CI runs it). Every model
  CI now also boots each Microkit image it builds under QEMU
  (`.github/workflows/hamr/simulate.cmd`), failing on a fault, panic or monitor violation;
  those runs -- stage 1's "build and run", and isolette end to end with the counter API, the
  string check and the fixed-region assertions -- are pending GitHub CI. (The isolette run
  above predates those three changes.)
- **Shrinking a region by hand:** lowering `<QUEUE>_REGION_BYTES` fails the build with the D4
  message; lowering the region's size in the system description still builds (see Open Items),
  and the first enqueue that crosses the shrunken end faults at the guard page instead of
  corrupting the next region.

## Key Files

| File | Role |
|------|------|
| `microkit/types/MicrokitLayout.scala` | HAMR's own layout computation, queue/region sizing, and which types can hold invalid bit patterns (D1, D4, D5, D6) |
| `microkit/types/MicrokitTypeUtil.scala` | Microkit type lint (`Data_Size` mismatch, dimensions, fixed arrays), the String substitution, and the queue archive make rules (D1-D3, D6) |
| `microkit/plugins/linters/MicrokitLinterPlugin.scala` | Passes `--max-string-size` to the type lint (D2) |
| `microkit/plugins/c/types/CTypePlugin.scala` | C type definitions; `T_BYTE_SIZE` as `sizeof`, layout `_Static_assert`s, `<T>_is_valid` validators, including a string's NUL check (D1, D5, D6) |
| `microkit/plugins/rust/types/CRustTypePlugin.scala` | Rust `data` crate types and their layout `const` assertions (D1, D5) |
| `microkit/types/QueueTemplate.scala` | Queue structs and region assertion, the staged, validating dequeue, the port getters and `get_<port>_num_invalid` (D4, D6) |
| `microkit/connections/ConnectionUtil.scala` | Region sizes from the whole queue; each receiving port's C getters, including the counter (D4, D6) |
| `microkit/plugins/rust/apis/CRustApiUtil.scala`, `CRustApiPlugin.scala` | The Rust component API, including `get_<port>_num_invalid` (D6) |
| `microkit/util/MicrokitUtil.scala` | `guardPageKiBytes` and `packedVaddrKiB`, region placement with guard pages (D7) |
| `microkit/plugins/c/components/CComponentPlugin_MCS.scala`, `CComponentPlugin_DomainScheduler.scala` | Placement of a PD's regions by size, with guard pages (D4, D7) |
| `microkit/plugins/monitors/UserLandMonitorPlugin.scala`, `DomainMonitorPlugin.scala` | Monitor region placement, with guard pages (D7); the scheduler's fixed-region assertions (D4) |
| `microkit/plugins/testing/TestSchedulerPlugin.scala` | Size-based placement of the controller's observable and injection mappings; injection regions sized by layout; C and Rust assertions that `test_cmd`, `test_status` and `test_schedule` fit their regions (D4, D7) |
| `microkit/util/MakefileTemplate.scala`, `MakefileContainer.scala` | Each PD links the queue archive `libhamr_types.a`, not every queue object (D6) |
| `microkit/util/SystemDescription.scala` | Region `size`/`vaddr` and PD `stack_size` rendering into the system description (F5) |
| `common/types/TypeResolver.scala` | Reads `Data_Size` into `bitSize` from `c.properties` (D1 inheritance question) |
| `jvm/src/test/.../microkit/SharedMemorySafetyTests.scala`, `resources/models/SharedMemorySafety` | Regression tests, their model, the host C harness (`harness/`) and the QEMU test (`qemu/`) |
