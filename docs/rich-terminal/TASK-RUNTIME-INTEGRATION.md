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
