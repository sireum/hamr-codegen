# Microkit Port Semantics: Stable Inputs and Buffered Outputs

**Status:** proposed, 2026-11-10. This document records the reviewed soundness finding,
the intended runtime design, and the work required to connect that runtime to component
and system proofs. No implementation or full AADL conformance is claimed.

## Problem

Red Balloon Security reported that HAMR-generated Verus receive contracts do not describe
the behavior of the generated Microkit runtime. The finding is confirmed: the contracts
make a port's input stable, but each runtime getter consumes another queue entry.

For an event-data input `p`, the generated application API promises:

```rust
ensures
  old(self).p == final(self).p,
  res == final(self).p,
```

Starting with one pending message and no producer activity during the call sequence:

```text
                       first get_p()       second get_p()
runtime                Some(v)            None
contract, after first  Some(v)            Some(v)
```

For larger queues the second call can return a different message instead. Pure event
ports have the same problem, with a Boolean result derived from the ghost option's
presence. Two sequential calls suffice; static scheduling and one thread per protection
domain do not prevent the defect. This is a mismatch at a trusted external-function
boundary, not a demonstrated defect in Verus's proof engine.

There is also a send-side mismatch. Each runtime put publishes immediately, while the
output ghost field records only the last put. A final compute guarantee therefore need
not describe earlier messages published by that dispatch. Where a port has an integration
guarantee, each verified application put must establish that guarantee, but this does not
give earlier messages the final compute guarantee's relationship to the dispatch inputs.

## Evidence and review scope

The review used codegen revision `34bd345f` and INSPECTA-models revision `9e9c3baa`, plus
the working-tree source inspected on 2026-11-10. Line numbers below identify that reviewed
source and may move. Paths beginning `shared/`, `jvm/`, or `art/` are relative to codegen.

| Evidence | Source and reviewed lines |
| --- | --- |
| Stable receive contracts and last-put ghost updates | [CRustApiUtil.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/rust/apis/CRustApiUtil.scala), `getApiContract`, 326–348; application putter, 351–390 |
| Rust getters call C without caching | Same file, `getUnsafeReadWrapper`, 273–308 |
| C getters consume, data getters retain the last successful payload | [QueueTemplate.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/types/QueueTemplate.scala), `getClientGetter_C_MethodPoll`, `getClientGetter_C_Method`, `getClientDataGetter_C_Method` |
| Queue reads advance the receiver cursor; empty reads fail | Same file, generated dequeue, 848–895 |
| Integration guarantees are added to application puts where declared | [GumboRustPlugin.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/gumbo/GumboRustPlugin.scala), 383–394, 644–704 |
| Rust periodic entry point directly invokes application code; other dispatch protocols are not implemented there | [CRustComponentPlugin.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/rust/component/CRustComponentPlugin.scala), 422–437 |
| Initialization has a separate API object and invocation | Same file, 465–484 |
| Monitors use input gets and input/output peeks | GumboRustPlugin.scala, 944–955 |
| Test-scheduler clear requests move receive cursors before dispatch reads | [TestSchedulerPlugin.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/testing/TestSchedulerPlugin.scala), 1011–1060 |
| System VCs alias connected endpoints to one channel field | [VerusVCSerializer.scala](../shared/src/main/scala/org/sireum/hamr/codegen/microkit/plugins/gumbo/VerusVCSerializer.scala), 35–60 |
| System contract calls use pre-state inputs and post-state outputs | Same file, 1122–1149; task VCs use contracts as premises at 1333–1346 |

Concrete generated examples are under `/Users/belt/devel/git/inspecta-models`:

- `micro-examples/microkit/gumbo-verus/structs_arrays/hamr/microkit/crates/consumer_consumer/src/bridge/consumer_consumer_api.rs`,
  78–85: receive contract. Its external body at 15–23 trusts the C result to equal a ghost input.
- The adjacent `extern_c_api.rs`, 22–31: uncached production getter; 103–113:
  the test getter reads a `Mutex<Option<T>>` without consuming it.
- `micro-examples/microkit/aadl_port_types/event_data/base_type/hamr/microkit/components/consumer_p_p_consumer/src/consumer_p_p_consumer.c`,
  20–27: get calls dequeue; 50–65: initialization and dispatch do not freeze inputs.
- The corresponding `types/src/sb_queue_int8_t_1.c`, 81–107: empty test, cursor
  advancement and payload copy. `types/include/sb_queue_int8_t_1.h`, 62–65:
  physical ring size two has usable capacity one because one cell is dirty.
- The corresponding producer's `src/producer_p_p1_producer.c`, 13–16:
  put immediately enqueues.
- The corresponding sporadic consumer's `src/consumer_p_s_consumer.c`, 59–68:
  queue-emptiness tests select handlers before they run.
- `micro-examples/microkit/aadl_port_types/data/base_type/hamr/microkit/components/consumer_p_p_consumer/src/consumer_p_p_consumer.c`,
  14–24: every get attempts a dequeue, updating the retained payload on success.

## Semantic baseline

The baseline standard reviewed was the purchased AS5506C (2017), section 8.3. Page
numbers here are printed page numbers, which matched the reviewed PDF extraction.

| Rule | Reference | Consequence |
| --- | --- | --- |
| Input becomes available at the configured input time; subsequent arrivals are excluded from that execution's input | §8.3.2(16)–(21), pp.140–141 | Freeze all eligible input values and event absence, including data ports |
| Default input time is dispatch | §8.3.2(17), p.140 | First-get sampling requires an equivalence argument or a different declared timing profile |
| Dispatch conditions select triggering ports; other dispatch-triggering ports are not frozen | §8.3.2(22)–(23), p.141 | Do not consume every sporadic input before deciding the dispatch |
| Default queue protocol is OneItem | §8.3.3(37), p.143 | One eligible frozen item is consumed independent of application getter use |
| Get_Value repeats the current value until Receive_Input or Next_Value changes it | §8.3.5(50), p.145 | Repeated application reads do not dequeue |
| Put_Value supplies a port variable for later Send_Output or runtime transmission | §8.3.5(45), p.145 | Separate application output state from transport publication |
| Output timing includes explicit sends and runtime-configured output times | §8.3.2(25)–(29), p.141; §8.3.4(41), p.143 | Sending once at completion is a selected execution profile, not all AADL behavior |
| Empty event-data queues retain access to the most recent data value | §8.3.1(13), p.140 | Distinguish retained payload from the presence of a fresh event |
| Storage may be optimized while preserving semantics | §8.3.5(59), p.147 | A private payload copy is an implementation choice, not a universal requirement |

AS5506D superseded C in April 2022. The full D text was not available for this review;
do not assume paragraph numbering or all details are unchanged.

The [AADL RTS formalization](https://arxiv.org/html/2507.06881v1), Table 1 and §§3, 6–7,
separates infrastructure port state from application port state. It selects a
receive-execute-send discipline and singleton output storage. It also explicitly assumes
`Dequeue_Protocol => AllItems`, rather than the standard default of `OneItem`. The
proposed design uses its state separation, not an assertion that its entire queue
semantics already matches this backend.

The JVM runtime provides a useful reference: `art/shared/src/main/scala/art/ArtNative_Ext.scala`,
137–217, separates infrastructure inputs, application inputs and pending outputs.
The periodic bridge in `jvm/src/main/scala/org/sireum/hamr/codegen/arsit/plugin/SingletonEntryPointProviderPlugin.scala`,
177–185, receives before computation and sends afterward. This confirms the relevant
stable-read/buffered-write behavior; it does not establish complete AADL conformance
of ART, including arbitrary queue sizes and dispatch policies.

## Intended supported profile

Initially implement and validate periodic execution with one input snapshot per dispatch,
`OneItem` consumption for event/event-data inputs, and at most one publication per output
port after successful computation. Existing low-level queues remain the transport.
Queue capacity greater than one does not change the per-dispatch OneItem rule.

Before enabling this behavior, determine how the frontend represents explicit input/output
timing and dequeue properties. Diagnose unsupported combinations rather than silently
treating AllItems, MultipleItems, multiple input/output times, deadline output or NoIO as
this profile. If a frontend cannot represent a property, document that restriction.

Sporadic C execution needs explicit dispatch-boundary integration as described below.
Rust sporadic entry points remain unsupported until implemented; this design does not
implicitly add them. Multicore support requires the snapshot and publication arguments
below; it must not be inferred from passing single-core tests.

## Design

### D1. Separate transport state from application port state

Keep the shared ring and receiver cursor as infrastructure state. Generate private,
per-component application slots:

| Port kind | Input slot | Output slot |
| --- | --- | --- |
| Event | Presence Boolean | Pending Boolean |
| Event data | Presence Boolean and valid payload | Pending Boolean and payload |
| Data | Retained valid value, plus freshness if the C API exposes it | Retained value plus dirty/publication bookkeeping |

Use private storage with a lifetime spanning the entry point and its monitor hooks.
An event-data payload is exposed to Rust only when present and validated. Pure event
semantics need no meaningful payload, even if transport encodes a dummy byte.
Keep typed sizes, layout checks, invalid-value rejection and queue coherence protections
from [SharedMemorySafety-design.md](SharedMemorySafety-design.md).

Application `get_p` and input `peek_p` read the same frozen slot. Neither advances the
transport cursor or re-samples event presence. Application `put_p` overwrites its output
slot and marks it pending. It does not enqueue. Output `peek_p` observes pending
application output with the same presence meaning as the ghost API.

Do not globally replace transport dequeue or infrastructure peek: dispatch selection,
injection, transport observers and diagnostics may need those operations. Separate and
name these uses so an application read cannot accidentally bypass the snapshot.

### D2. Establish one lifecycle boundary per accepted dispatch

The intended order is:

```text
apply test input injections / clear requests, when enabled
determine whether a dispatch is accepted and which inputs are eligible
establish the input snapshot; consume eligible OneItem events once
clear pending event/event-data outputs; prepare data output bookkeeping
run pre-compute observers against the frozen inputs
invoke application computation
run post-compute observers against frozen inputs and pending outputs
publish each pending application output once
complete the dispatch
```

Use one owner for these operations; do not run the same begin/end hooks in both C and
Rust. The C scheduling path and Rust exports must agree on what constitutes a dispatch,
including direct invocation from generated test harnesses. Helpers may be shared, but
concrete hook names and placement are implementation details still to be selected.

Eager input preparation consumes eligible events even when application code never reads
them. There is no second dequeue at completion. An empty snapshot stays empty for the
whole entry point. Newly arriving items are reserved for a subsequent dispatch.

Data ports retain their previous valid value when no new input is available. Their
initial value must be defined by model/runtime initialization rather than assumed from
an unproved default. Data output bookkeeping must distinguish retaining a value from
having published a new value, and agree with the declared data output timing policy.

No pending event output is replayed in the next dispatch. A dispatch that faults before
normal completion does not publish its pending outputs through the normal completion
path; recovery behavior must explicitly reset or handle them.

### D3. Preserve dispatch selection and initialization

For sporadic execution, do not dequeue inputs and then use the now-empty infrastructure
queues to decide whether to invoke a handler. Determine dispatch status first, preserve
the selected trigger set, and freeze only inputs eligible for the accepted dispatch.
Unselected dispatch-triggering inputs remain pending. Respect the model's supported
urgency/selection policy; explicitly reject unsupported policies.

The current C path can invoke multiple handlers during one notification. Define whether
these are separate accepted dispatches or part of one dispatch before inserting hooks;
the number of snapshots and output commits follows that decision. Do not silently give
each handler a new snapshot while claiming one stable input state for the whole dispatch.

Initialization also permits outputs. Give it its own output preparation/publication
boundary after transport initialization. Flush successful initialization outputs once,
make them visible according to startup ordering, and keep them from being replayed by
the compute API. Tests must exercise initialization followed by computation and repeated
test initialization. Input reads remain unavailable through the initialization API.

### D4. Preserve component contracts with an explicit runtime relation

Keep stable input ghost fields and last-put output ghost fields as the intended component
abstraction. At each verified entry-point boundary, establish or justify the relation:

- Every input ghost value represents that dispatch's frozen application slot.
- Initial event output ghosts represent no pending output.
- Initial data output ghosts represent the chosen retained data state.
- Each verified put updates the ghost value and its corresponding executable slot.
- Completion publishes the executable output represented by the final ghost state.

Ghost values are erased: initializing executable slots does not itself establish these
proof facts. Audit trusted external bodies and entry-point wrappers rather than merely
adding executable caching. Persistent `compute_api` objects and separate initialization
and compute APIs need a boundary argument for refreshing/resetting their logical state.
If existing wrappers do not establish the relation, update them while preserving the
intended application-facing meaning. Do not claim all contracts can remain untouched
before this audit is complete.

An integration guarantee is a per-call requirement only where declared, and only along
the verified application API path. It is not a runtime validator for C/extern/unverified
callers. Producer guarantees imply receiver assumptions only with the corresponding
connection compatibility proof and transport assumptions. Pure event ports do not have
payload integration clauses.

### D5. Integrate observers and tests

Pre-compute monitors must observe the same frozen inputs that application code reads;
post-compute monitors must see pending application outputs before publication. Define
separately any observer intended to examine infrastructure queues or committed output.
Do not let a monitor get consume application input or let an output peek return an old
transport value after a new put.

Apply test-scheduler input injections and cursor-clear requests before freezing. Preserve
existing per-receiver cursor ownership and diagnostic counters. Define application poll
drop/freshness results from dispatch preparation so repeated reads do not recount the
same drop or consume another item; transport poll remains a distinct operation.

The current Rust unit-test stubs already return stable input options, so GUMBOX can pass
while deployed C violates the contract. Keep the GUMBO oracle meaning, but align test
setup, dispatch reset and output observation with the new lifecycle. Tests that call
application methods or exported entry points directly must deliberately establish the
same boundary, or explicitly identify themselves as tests of an already-prepared state.

### D6. Keep system-proof applicability separate

The system VC generator uses one channel field for both ends of a connection and frames
fields outside the firing component's outputs/state as unchanged
(`VerusVCSerializer.scala`, 940–1006). Component guarantees enter system VCs as premises;
they do not prove that runtime queue consumption implements this channel abstraction.

Buffering once per dispatch is insufficient for general queued communication. Consider
two producer dispatches emitting `v1` then `v2` before a consumer dispatch. A queue of
capacity greater than one can make `v1` the consumer's next frozen input while the latest
producer channel value is `v2`. Even a singleton transport requires accounting for a
second consumer dispatch with no intervening send: event presence has been consumed.
Broadcast adds independent receive histories for each consumer.

Before claiming end-to-end system soundness, either:

1. State and enforce restrictions on schedules, queues and event consumption, and prove
   that they establish the channel abstraction used by the existing VCs; or
2. Extend system state and transitions to model the required infrastructure/application
   distinction, pending events, per-receiver consumption and queue history.

This is an open proof obligation, not a completed choice in this document. Preserve
existing abstract VC results as results about that abstraction; do not present the runtime
bridge repair as automatically discharging their runtime correspondence. Multicore and
non-lockstep schedules require particular attention.

### D7. Snapshot timing and cost

Use eager freezing as the initial implementation. The relevant scheduling assumption is
whether a producer can execute between the required freeze point and a later read, not
merely whether the machine has one core. Preemption can violate that assumption on one
core. Freezing just before execution also requires justification when logical dispatch
occurred earlier; supported scheduling must define the sampling point precisely.

On multicore, private slots prevent later application reads from changing, but populating
multiple slots sequentially is not automatically a simultaneous dispatch-time snapshot.
Specify a linearization/cutoff scheme or scheduling restriction for cross-port snapshots.
Retain coherent-copy checks and handle concurrent overwrite/rejection during preparation.

A private payload slot costs approximately one payload per frozen data/event-data input
and one per buffered payload output, plus presence/bookkeeping and alignment. There can
also be existing queue-validation staging and Rust return temporaries. Existing dequeue
already copies; exact extra copy cost depends on generated code and optimization. Check
PD stack/static-memory budgets for large records and arrays rather than promising one
universal copy count. Pure events need only presence state at the application layer.

Lazy first-get caching is deferred. It must preserve the intended snapshot, including
empty ports and unread events. An end-of-dispatch dequeue alone can consume an item that
arrived after the intended freeze time, or lose the item that should have been selected
through overflow. A future optimization needs a reservation, cutoff/storage guarantee,
or an execution-equivalence argument. Ownership transfer or safe buffer reuse may avoid
copies; semantic stability is the requirement.

## Implementation scope and sequence

1. Record supported timing/dequeue/dispatch profiles and diagnostics; resolve sporadic
   handler boundaries and data-port initial/publication semantics.
2. Add application slots and private transport-facing preparation/publication helpers in
   QueueTemplate and connection/component generation, without changing raw queue dequeue.
3. Integrate C scheduling, Rust entry points and initialization with exactly one lifecycle
   owner. Audit ghost/runtime correspondence and all get/put/peek/poll call paths.
4. Adapt monitors, test-scheduler injection/clear ordering, Rust test stubs and GUMBOX
   harnesses; retain validation, freshness and drop accounting.
5. Run the behavioral and verification checks below. Resolve system-VC applicability
   separately before making runtime system-proof claims.
6. Regenerate affected INSPECTA models only in the implementation change. Inspect and
   report generated diffs; do not accept/update expected or golden results without the
   developer's explicit authorization.

This affects more than the C/Rust FFI wrapper: queue API templates, dispatch generation,
initialization, observers and test infrastructure participate. Additional proof/model
changes depend on the audits above. No implementation is authorized by this design alone.

## Validation and acceptance criteria

Use generated deployed C/Rust paths in addition to host unit tests. A proof-only test or
the current stable-value mock cannot detect the reported runtime failure.

| Case | Required observation |
| --- | --- |
| One event-data item, two reads | Both reads return the same Some(v); next eligible dispatch is None without a new send |
| Empty event-data input, producer writes during execution | All reads in this dispatch remain None; arrival is eligible later |
| Pure event input | Repeated gets/peeks preserve presence; one event consumed per eligible dispatch |
| Capacity greater than one | OneItem selects one item per dispatch; repeated reads never advance to the next item |
| Unread event input | Exactly one eligible item is consumed; later arrivals are not accidentally consumed at completion |
| Data input | Reads and peeks agree within a dispatch; no arrival retains the prior valid value; startup value is defined |
| Two puts | No intermediate publication; one final payload/event is published after successful completion |
| No put in a later dispatch | No stale event/event-data replay; data behavior matches the declared retained-value policy |
| Initialization put | Published once at initialization completion; not lost or duplicated on first compute |
| Fault before completion | Normal completion publication does not run; recovery/reset policy is respected |
| Sporadic selection | Selected handlers still run after preparation; unselected triggering ports are not consumed |
| Monitor and test-controller paths | Input/output snapshots match application observations; clear/injection precedes freezing |
| GUMBOX and direct harness calls | Boundary/reset behavior agrees with deployed runtime across successive invocations |
| Invalid values and concurrent overwrite | Existing validation/coherence rejection and diagnostic counts are preserved |
| Broadcast | Each receiver consumes independently without modifying another receiver's state |

Include scalar, large array and record payloads and both supported scheduling backends.
For claimed multicore support, test interleavings during input preparation, repeated reads,
puts and publication. Single-core success is not a multicore acceptance criterion.

Component verification should establish representative repeated-read and last-put
properties under the preserved contracts; runtime tests must confirm the same behavior.
System validation must include backlog, repeated-consumer/no-new-producer and independent
broadcast histories, then document the restrictions or model changes needed for the VCs.
Passing existing system VCs alone does not settle those cases.

## Review conclusions to preserve in external communication

- The reported receive mismatch is confirmed without requiring concurrency.
- The proposed stable-input/buffered-output direction is appropriate for a declared
  execution profile. It does not establish complete AADL conformance.
- Data reads are stable only under a suitable sampling/execution argument; one core is
  insufficient by itself. Event presence and retained payload are distinct concepts.
- Integration requirements constrain each verified put where declared, not every possible
  runtime sender or every aspect of message behavior.
- ART demonstrates the relevant application/infrastructure state separation, not universal
  conformance of every backend and dispatch policy.
- The runtime repair does not automatically keep monitors, GUMBOX and system-level VCs
  sound unchanged. Their boundary behavior and runtime abstraction must be checked.
- Confirm the reporter's concrete reproduction, implement runtime regression cases, and
  regenerate examples after review of the implementation and expected-result differences.
