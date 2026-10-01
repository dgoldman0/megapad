# Rich Desk and committed task runtime integration

The first local checkpoint merges rich-object integration `fb94ade` with the parallel
runtime branch through committed `c6e7852`. The source merge is `7f834e5`; it was
conflict-free and includes no uncommitted changes from the parallel worktree.
The new task adapter still being edited there is outside this checkpoint.

Both native extensions were rebuilt with GCC/G++ in this checkout. The Make
supervisor then ran 1,135 tests: **all passed in 48.25 seconds**, with no skips.
The scope covers native task roots/children, scalar service runtime/session,
manifest and ABI, service benchmark parity, nested sessions, foreign dispatch
and guards, private host-abort identity, restoration/cancellation, and the
simulator/input boundaries for FIELD, STATUS_FIELD, GRID, PANE and TASKBAR.
The native build reports one existing signedness warning in the peer's task
child-count comparison; it does not prevent compilation or these tests.

This qualifies the merged runtime and semantic-object boundaries. It does not
claim a complete combined rich-shell Desk journey. The in-flight FIELD and
SERIES qualifications keep their frozen `f2ec566` and `fb94ade` runtimes; their
source/image hashes and timing records belong to those runs. The final Desk
qualification will identify its own actual paired source checkpoint.

All changes remain on `integration/flowing-task-runtime`. Neither repository's
main branch nor any remote branch is changed by this integration.

## Committed ownership and accounting alignment

Source merge `b2f1fdc7cbd1891927a99d9df93192003c09c048` advances the peer
checkpoint to `58260e9ae72efd5ac5a34d520a00c1df779f901b`, again conflict-free.
This includes atomic foreign-word publication, preserved return frontiers,
the original-owner task adapter, parked native validation and engine-issued
semantic receipt settlement. The peer's uncommitted suspension/cursor work
was not imported. The cursor commit at the merged tip records its planned
contract; public task/composite capability gates remain unchanged.

Both native extensions built with GCC/G++. The focused Make gate passed
**599 tests in 92.25 seconds**, with no skips. It covers native task roots,
children, adapter and parked validation; task/service/prepared/nested sessions;
registration, dispatch/guards, semantic receipts, return frontiers and host
abort; and simulator/viewer input boundaries for all five rich control families.

The final combined Desk run may use this rebuilt checkpoint. The in-flight
standalone waveform run remains on `fb94ade`; this synchronization does not
rewrite its provenance or timing evidence.

## Committed callback idle and deadline alignment

Source merge `737867a6710cddf9104bd7bbfb3ad5390c3fbc9a` advances the peer
checkpoint to `e723e50accbfb811a4bb26deb6ddff735f398660`, without conflicts.
It includes `d5ab2d9` task-only receipt accounting: cumulative native and
semantic work remains attached to the original installed owner, independently
of mixed-profile diagnostics. Repeated receipt observation, interrupted
counter publication and later private roots cannot duplicate or refund that
task work.

Canonical semantic `Idle` and `IdleUntil` callbacks now retain the original
task root, execution limits, stack ownership, native-chain state and exact
engine-issued suspension witness through detach and resume. Deadline waits
also preserve the original RTC route and clock identity. Wake-up does not
replenish execution limits; invalid suspension authority is rejected, and
cancellation releases native frames before return-stack cleanup.

Both native build targets, `accel` and `simulator-accel`, completed
successfully with GCC/G++. The Make-supervised regression gate passed **410
tests in 45.50 seconds**, with no skips. This adds callback idle/deadline
suspension, clock and witness authority, deadline host abort, task-only
receipts and KDOS exception checks to the ordinary suspension,
registration/runtime/stack, native adapter and prepared-session coverage. It
also rechecks FIELD, STATUS_FIELD, GRID, PANE and TASKBAR
simulator/viewer-input boundaries.

The public task/composite capability gates remain unchanged. Prepared-session
tests still require `callback_suspension`, `shared_task_exceptions` and
`composite_suspension` to be false. The peer's separate machine-cursor
scheduling implementation and capability activation are outside this merge.
This checkpoint supplies richer runtime regression coverage for the combined
Desk qualification; it does not replace a complete Desk journey or change the
frozen standalone runs' source and timing records.

## Prepared-task machine quanta and qualified composite sessions

The next qualification uses the isolated branch
`integration/flowing-machine-runtime`.
Conflict-free source merge `162086eb663dd069fff6e4457ff444e0186dc453`
starts from the previous integration
`5bf70614ce7f06105463f6448abe5f342fcc1b70` and imports the committed peer
through frozen `4ef08d88817af1fbe9c6a5ef17e834295bc1f17e`. This includes
`abd77639be99284d42c55ed6c09288bae2d7c60b` machine scheduling plus the
successor's prepared-task exception and capability activation. No peer working
files or later commits are part of this checkpoint.

A selected machine quantum now bounds actual prepared-task instructions during
one host turn, shared across the root and its children. Runnable machine yields
retain an exact one-shot cursor, owner, operation token and accepted task
receipt; resume preserves cumulative instruction, callback, entry and semantic
budgets. Machine progress does not require an outer semantic step. Genuine idle
and deadline callbacks retain their separate wake conditions.

This checkpoint supersedes the previous section's capability limitation.
Qualified prepared-task sessions advertise shared exceptions, callback
suspension and composite suspension only when their original installed routes
satisfy the corresponding capability checks. Finite machine quanta require
composite support before session entry. `None` preserves synchronous execution,
including the supported older revision2 transport path. Generic task manifests,
automatic publication ordering and task launcher support remain deferred.

Both native extensions were rebuilt using `make build`, `CC=gcc` and `CXX=g++`.
The toolchain was GCC/G++ 13.3.0 and Python 3.12.14. The existing task
child-count signedness warning remains the only compiler warning. The built
extension SHA-256 values are:

| Extension | SHA-256 |
| --- | --- |
| `_mp64_accel.cpython-312-x86_64-linux-gnu.so` | `83c4e55b9c7e5c9977245d98db843e838c814371b55c4692d438dd84360dc119` |
| `_megaforth_native.cpython-312-x86_64-linux-gnu.so` | `c70a2c53201ae1196d4237f7c662e8c8911b4c048c7d5c636708cc336625f3f0` |

The focused regression gate ran through `make test-sequential` in supervisor
namespace `flowing-machine`: **644 passed in 182.94 seconds**, with no skips.
It covers the new foreign machine quantum, session quantum, composite session
and capability suites; existing foreign suspension authority, task
deadlines/abort, receipt accounting, registration, stack and runtime behavior;
idle/deadline and KDOS exceptions; native task adapters/parked state; prepared,
private, nested and service sessions; and FIELD, STATUS_FIELD, GRID, PANE and
TASKBAR simulator/viewer-input boundaries.

Test-only commit `b52374cb130e26ad47db820a3c98081044bc1589` adds the missing
intersection of enhanced input with a retained prepared-task cursor. The
existing rich hybrid-session scenario now runs synchronous private and genuine
prepared-task quantum1 profiles on both Python and native executors. At the
one-instruction yield it queues an acknowledged TASK activation and proves
admission leaves the exact cursor, receipt, stacks and semantic count intact.
Subsequent turns return the original invocation and apply the event exactly
once; a later revision-aware FIELD adjustment also applies once without
changing the completed task receipt. This separate Make-supervised gate passed
**4 tests in 4.70 seconds**, with no skips
(namespace `flowing-machine-rich`).
There are **648 passing tests across the two disjoint gates**. Their wall-clock
times include brief concurrent execution and are not a Desk performance
benchmark.

This qualifies the combined runtime and rich input boundary for the next full
Desk journey; it does not claim that journey has completed or alter the source
and timing records of frozen standalone qualifications. The older task-runtime
checkout remains at `5bf70614ce7f06105463f6448abe5f342fcc1b70`. Neither main
nor a remote branch was changed, and nothing was pushed.
