# Native semantic execution boundary

The extension and its Python owner are built from the same checkout and use
one current internal interface. Rebuild the extension after changing this
boundary with `python setup_simulator_accel.py build_ext --inplace --force`.
The extension exports `SEMANTIC_API_VERSION = 10`; its Python owner checks this
version before admission. An explicit native request rejects older builds;
automatic selection keeps the reference executor and the Python dictionary
when the version mismatches.

`_megaforth_native.NativeProgram(regions, page_size, continuation_type,
dense_regions=())` retains sparse ordinary regions as `(base, size, pages)`
triples. `pages` is the existing sparse
region's dictionary of page index to fixed-size bytearray. The executor holds
the GIL and uses those same bytearrays; there is no copied guest address space.
Scalar preflight resolves each page fragment once. A scalar wholly inside a
page retains one contiguous byte pointer; cross-page scalars retain per-byte
pointers for their qualified fragments, including sub-cell page sizes. Loads
and stores remain explicit little-endian byte operations and require no host
alignment. On little-endian hosts, contiguous full cells use unaligned-safe
`memcpy` loads/stores. Missing read fragments supply zero; missing write pages return to
Python before any effects. Page pointers are cached only within one native run. Exact qualified page
spans are reused separately for data-stack, return-stack, and ordinary memory
access. Partial final region pages remain clipped to their region. All caches
expire before Python can replace or materialize a page, and ordinary access
to return-stack backing still falls through before the cache lookup.

The optional `dense_regions` iterable contains distinct `(base, size, buffer)`
descriptors. Each buffer must export writable, one-dimensional, C-contiguous
bytes, with capacity exactly equal to the nonzero declared size. The program
retains a buffer lease through its own independent view until destruction,
keeping its exporter alive and preventing resizing. Releasing the caller's
view does not affect this lease. All sparse and dense guest spans must be nonwrapping and
mutually disjoint; dense host byte spans must not overlap each other either.
Malformed construction does not publish a program or retain partial leases.
`clear()` invalidates plans, retaining all region leases and shared bytes.

Dense scalar and bulk preflight resolves directly to the pinned pointer plus
region offset; it performs no Python page-dictionary lookup or per-page view
allocation. A hot span may cover the whole dense region, but wrapping,
cross-region and ordinary return-stack exclusion checks still precede access.
Dense output never requires sparse page materialization. Both storage forms
retain the same stack, continuation, fault and little-endian contracts.

Python selects dense storage only through
`SparseAddressSpace(..., dense_backing=owner)` or `dense_backing=True`, or the corresponding platform
factory argument. `shared.memory_backing.DenseMemoryBacking` allocates a
separate zero-filled exporter for each `(base, size)` region and holds private
pins; `buffer_at(base)` gives each client an independent writable view.
The supplied owner's geometry must exactly match all configured ordinary
regions. `True` allocates an owner from the constructor's already-validated
region geometry, avoiding a second topology definition. The read-only
`memory.dense_backing` property exposes the selected owner (or `None`).
The canonical `SparseAddressSpace` outer type and all MMIO routing remain
unchanged. Default construction stays sparse. `resident_page_count` counts
written sparse pages, or all configured dense pages rounded up separately
per region. Clients serialize writes and execution; no GIL release, backing
replacement, transition copying, or hybrid execution is introduced here.

`install(xt, operations, overlaid=False)` installs a plan of `(opcode, a, b)`
triples, one per original IR operation, so branch targets and returned IPs
remain original IR indices. An overlaid plan belongs to a word with a Python
execution overlay: Python may enter it after the overlay declines, and a
return may resume inside it, but a native `EXECUTE` never enters it, and
calls to such a word compile to stops, so every entry meets the overlay
first. `clear()` drops all plans. The Python owner invalidates plans when
the dictionary removes a definition or reclaims a body, and for mutable
execution bindings; publishing a new definition leaves them valid. Opcode
constants are exported with the `OP_` prefix.

Installation also recognizes literal/constant arithmetic, address pushes
followed by scalar fetches, and `DUP` conditional branch superinstructions
without removing either original index. A whole pair
must fit the allowance and pass preflight; otherwise its original first
instruction runs. Intermediate popped bytes are still materialized. Stack
operations may resolve an entire operand span through a qualified page;
fragmented, absent and cross-page spans retain scalar handling.

Address/fetch pairs cover literal and plain created/constant addresses followed
by `@`, `C@`, `W@` or `L@`. Both operations pass preflight before either commits.
The pushed address is written before the read, preserving partial aliases
between the source bytes and that stack slot. Direct entry into the original
fetch still executes the ordinary instruction.

`COMPARE` and `FILL` preflight complete ordinary byte spans, retaining one
inline chunk for the common single-page case and a caller-sized page list for
longer spans. Comparisons preserve unsigned byte order and validate both full
common prefixes before using an early difference. Missing read pages supply
zero. Zero fills skip absent pages without allocation; nonzero fills needing
new pages return to Python before any effects. MMIO, region crossings, wrapped
spans and return-stack overlap keep their reference fault and side effects.
Both primitives retain their two semantic ticks, including zero-length work.

`CMOVE`, `CMOVE>` and `MOVE` use the same byte-span preflight and preserve
their distinct forward, backward and snapshot overlap semantics. Ordinary
copies snapshot their source before writing, including when host-replaced
page objects alias. Missing destination pages, MMIO, wrapping and crossing
spans return to Python before mutation so the reference byte loops retain
their partial writes and fault order. Zero-length copies do not inspect the
addresses; `MOVE` also skips nonempty self-copies at any address. No new
guest capacity or semantic step charge is introduced.

Bulk handlers stay outside the scalar dispatch loop in a non-inlined helper.
This keeps byte-span allocation and exception cleanup out of the hot loop;
the ordinary preflight, stack commit and original instruction boundary are
unchanged.

`run(xt, ip, data_state, return_state, continuations, remaining_steps, fpcsr=0)` accepts:

- `data_state = (floor, empty_pointer, pointer)`;
- `return_state = (floor, empty_pointer, pointer, continuation_cookie)`;
- the return stack's ordinary slot-to-`(Continuation, raw_cookie)` dictionary;
- a nonnegative allowance in the existing semantic-step units;
- the scalar service's current FPCSR cell (zero by default for raw callers).

It returns `(xt, ip, completed_steps, data_pointer, return_pointer,
continuation_cookie, continuation_updates, pointer_captures, fpcsr)`. The FPCSR
cell is returned on every exit, including zero progress and unsupported stack
states. Each continuation update is
`(slot_address, caller_xt, return_ip, raw_cookie)`. The Python owner installs
these as ordinary non-root, non-fault continuations, preserving updates to
already-popped slots, and updates the stack pointers and cookie counter. A zero
`caller_xt` is a type deletion from a user push, including one that wrote the
same raw value as the previous cookie. Zero is never a valid colon XT.

The Python owner interns immutable ordinary continuation values by original
`(caller_xt, return_ip)` until native-plan invalidation. This does not intern
raw cookies: each call still writes a fresh cookie, and each retained slot
keeps that cookie beside its frame. Root and fault frames are not interned.

`snapshot_stack((floor, empty_pointer, pointer), continuations=None)` reads
every active cell into an immutable bottom-to-top tuple. Passing the return
stack's exact metadata dictionary restores matching continuation objects,
including root and fault frames. Stale active types are removed only after
the complete snapshot has passed preflight; inactive types remain available
to `RP!`. Sparse holes read as zero without allocating backing. Page caches
are fresh for this call and support the same fragmented cells as execution.
Unsupported bounds, backing or metadata return `None` for reference fallback.
The owner admits only canonical stacks backed by its exact sparse memory;
custom stack/memory behavior remains in Python. Snapshotting advances no
guest steps or clocks. Resumption still compares every data and return entry,
and data-mutation rejection precedes return snapshot cleanup.

`SP@` and `RP@` push the current pre-push data/return frontier. Each successful
native `RP@` increments the returned capture delta, which Python adds to the
unbounded return-stack capture generation before a callback, fault or host
suspension can observe the state. An overflowing `RP@` push returns to Python,
where registration still occurs before the push fault. Existing nested
dispatch, cancellation and escape guards consume the same capture evidence.

Native colon calls write exact opaque continuation cookies into the shared
return-stack bytes. Native returns can also consume preexisting ordinary
continuations, read lazily from the same dictionary and verified against shared
bytes and the exact `continuation_type`. Root/fault returns and stale metadata
return to Python before mutation. `>R`, `R>`, `R@` and the pair words `2>R`,
`2R>` and `2R@` use the single ordered return stack; exposed continuations are
never mistaken for user cells or loop indices. A pair word moves or copies both
cells in one operation, and stops before any effect unless both top return
cells are user cells, so Python raises the reference error. Counted loops use fixed-position limit/index pairs and preserve
modular equality termination, retained bytes, and type deletion on both
new frame slots. Identity-bound `I` and `J` never search past a continuation.
Identity-bound `EXECUTE` can enter an already installed colon plan, consuming
its XT and creating the same continuation as reference dispatch. Missing or
non-colon target plans fall through before effects; Python owns invalid-token,
stack-fault, service, and source-accelerator behavior. Dynamic-call target
availability is invalidated with the other plans.

Static call instructions retain their resolved plan object in the unused call
operand. `EXECUTE` reuses a cached plan only when the current XT matches.
These objects remain stable through map rehash and body replacement; an empty
replacement still falls through, and `clear()` destroys every cached target
with its containing plan. The dispatch loop carries the current plan directly
across calls and returns. Instruction records remain 24 bytes on the current
64-bit build. The extra FPCSR run argument/result does not change plan records.

Within a native interval, continuation changes use growable arrays indexed
relative to the entry return pointer, separately above and below it. They
allocate only the span reached by actual stack movement, not the unused return
stack capacity. Untouched slots remain distinct from explicit type deletions;
all changed inactive slots are exported for subsequent `RP!`. Allocation for
paired loop slots finishes before either type changes.

Native still excludes stack-pointer restoration, other dynamic execution, services other than the
identity-bound scalar FP operations described below, and any
ordinary memory access intersecting the return-stack backing interval.

`OP_LITERAL`, `OP_BRANCH`, `OP_BRANCH_ZERO`, `OP_CALL`, `OP_RETURN`,
`OP_STORE_VALUE`, `OP_STRING_LITERAL`, `OP_R_PUSH`, `OP_R_POP`, `OP_R_PEEK`,
`OP_DO`, `OP_QUESTION_DO`, `OP_LOOP`, `OP_PLUS_LOOP`, `OP_UNLOOP`,
`OP_R_PUSH_PAIR`, `OP_R_POP_PAIR`, `OP_R_PEEK_PAIR` and `OP_ABORT_IF` cost
one semantic step. `OP_ABORT_IF` is the test of a compiled
`ABORT"`: it consumes a zero flag and continues, and stops before any effect
on a true one, so Python performs the abort. Compiled calls to the original
`EVAL-STATUS`, `EVAL-LINE`, `EVAL-COLUMN`, `EVAL-DEPTH` and `EVAL-THROW`
plan as `OP_PUSH_CELL` of the fixed cell each pushes. `OP_PUSH_CELL`
(constant or plain created-body address), `OP_FETCH_VALUE`, and admitted
primitive-call operations cost two. `OP_STOP` is uncharged. `a` holds the
literal, branch IP, called XT, or data address where applicable;
`OP_STRING_LITERAL` uses `(a, b)` for the already-resolved address and length.
Arithmetic, stack, and scalar-memory primitive operations ignore `a` and `b`.
`CELL+` adds eight with the same unsigned cell wrapping as the reference
primitive. The original hosted `COREID` and `TASK-ID` callbacks both push zero
and share the native false opcode. These remain two-tick primitive calls;
admission binds to the original installed word objects, so later colon or
host-callback definitions with those names retain their ordinary behavior.

`OP_SCALAR_FP` uses `(a, b)` for an FC operation byte and its operand count:
one for unary operations, two for binary operations, three for fused
multiply-add/subtract. Reading inputs top-first as `v`, the exact shared value
kernel receives `(Rd, Rs, Rt)` as `(v[0], v[0], 0)`, `(v[1], v[0], 0)`, or
`(v[0], v[2], v[1])`, respectively. FCMP has no cell result and is not admitted.
Invalid operation/arity descriptors, reserved rounding modes, missing operands
and unavailable output storage return unchanged before kernel execution.
Successful execution writes one full result cell and ORs its sticky flags
into the native FPCSR state. `OP_FPCSR_FETCH` pushes that state;
`OP_FPCSR_STORE` pops one cell and applies the existing `0x1f7` write mask,
which permits reserved rounding modes for later operation validation. Both
ignore `a` and `b`. These three opcodes each cost two semantic ticks.

The Python owner derives scalar plans from `shared.scalar_fp.BIOS_WORDS` and
binds the original installed Word identities. It captures the scalar service
owned by those BIOS closures; replacing `runtime.scalar_float` does not retarget
an existing word. Native FPCSR is settled into that captured service before
clock accounting, profiling, Python fallback, or a host boundary. A declined
operation leaves reference dispatch responsible for sequential operand pops,
fault callbacks and tick order. Direct primitive dispatch and `EXECUTE` of a
scalar primitive retain the Python service path; only compiled direct calls
use these native opcodes.

Every operation is preflighted before its ticks or effects. Unsupported
operations, missing plans, insufficient allowance, missing destination pages,
stack shape/capacity failures, and unsafe scalar memory spans return at that
uncharged original operation. Python then executes that exact operation,
including its normal partial fault effects. In particular, an allowance of
one does not admit a two-tick primitive call. Successful stack operations
write their ordinary shared bytes immediately, including retained bytes below
a popped frontier. Reads from missing ordinary pages return zero.

The Python owner stores compact entry costs with each prepared plan and
declines known stops or insufficient allowances before marshalling stacks.
These costs share plan-generation invalidation. The reference dispatcher
retains its partial-budget effects and original outer-IR quantum boundary;
the guard does not extend the native allowance or move a host boundary.

The Python owner settles the completed step count into the cumulative meter,
diagnostics, and exact timer state before executing a fallback operation or
publishing a host boundary. Native execution does not admit events, invent
IDL wakes, read wall time, or reset a watchdog. Empty/unsupported native runs
must fall through to the Python operation instead of being retried unchanged.

## Native dictionary

`_megaforth_native.NativeDictionary(start_address, active_limit, memory,
regions, page_size, dense_regions, has_external, external_base,
external_limit)` is the native backend's dictionary and side index. `memory`
is the owning `SparseAddressSpace` or `None`; the region descriptors are the
same ones `NativeProgram` receives and are validated by the shared
`guest_memory.h`. `simulator/native_dictionary.py` is its only intended
client and presents it through the Python `Dictionary` and
`HostedDictionaryIndex` interfaces; the Python classes stay the reference.

Records are kept in definition order and bound case-insensitively, newest
first. `define(name, initial_body, immediate, kind, constant, directive,
has_action, action_xt, action_ip, implementation)` publishes one header and
returns `(index, header_address, xt, uid)`. The kind is one of the exported
`KIND_*` values; Python implementation objects are kept for Python, and a
created word's action is kept natively. `find`, `resolve`, `live` and `view`
return a record's cached Python view, or its index when Python has not asked
for it yet; the facade then builds the view from `record_info(index)` and
stores it with `set_view`. `publish_rollback`, `rollback_to` and `set_latest`
return the removed XTs so the facade can drop cached views.

Guest bytes are written directly into the region pages, and a missing sparse
page is created as a zeroed page-sized `bytearray`, as the Python write path
does. A span outside one ordinary region goes through the `SparseAddressSpace`
methods instead, so its checks and exceptions are unchanged. The side index
keeps the BIOS slot layout, FNV-1a hash, probe order, publication order and
flag transitions of `HostedDictionaryIndex`.

Natively published colon definitions keep their IR in the record as
`IR_*` operations, `(kind, a, b)` with `WRITE_OUTPUT` and `ABORT_IF` payloads
held separately. `record_info` returns a `NativeColonBody` handle for such a
record; its `operations()` gives `(kind, a, b, payload)` tuples, from which
the facade builds the Python IR only when Python first reads it.

## Native line walker

`_megaforth_native.NativeLineWalker(dictionary, directive_values, primitives,
protocol, cells, evaluator_cells, evaluate_checked_xt, status_throw_xt,
bootstrap_base, bank0_base, bank0_limit)` runs one loader overlay's loop:
`WALK_SOURCE` for `SOURCE-EVALUATE-CHECKED` or `WALK_LOADER` for `_LD-WALK`.
`simulator/native_walker.py` is its only intended client. `directive_values`
maps every `DirectiveKind` name to its value; `primitives` binds the admitted
interpret-time and defining primitives by XT, and the walker captures each
record's identity at construction.

`walk(data_state, return_state, frames_depth, compiler, blocked, allowance,
dictionary_base, dictionary_limit, base_address)` returns `(outcome, current,
evaluated, data_pointer, return_pointer, cookie, steps, compiler,
cleared_token)`. Outcome 0 ends the overlay's loop, 1 hands back one measured
line still to be evaluated, and 2 asks Python to run the next whole
iteration. The caller settles the stack pointers, cookie counter and steps,
and takes back the open definition, exactly as after a native execution
interval; after an exception, `take_error_state()` returns the same tuple
for the lines already committed. `compiler` crosses the boundary as
`(name, location, compile_mode, temporary, operations, controls, pool)` and
comes back with the name token's column after the location. `stats()` gives
the lines evaluated natively and handed back.

## Tile words

`run` takes an optional `tile_state = (mode, source0, source1, destination)`
with the TMODE register, TSRC0, TSRC1 and TDST. When it is given, the run
admits `OP_TILE_MODE_SET` (`FP16-MODE` and `BF16-MODE`, with the mode in
`a`), `OP_TILE_MODE_STORE` (`TMODE!`, keeping seven bits),
`OP_TILE_SOURCE0_STORE`, `OP_TILE_SOURCE1_STORE`,
`OP_TILE_DESTINATION_STORE` and `OP_TILE_BINARY` (`TADD`, `TSUB` or `TMUL`,
as `TILE_ADD`, `TILE_SUBTRACT` or `TILE_MULTIPLY` in `a`), each costing the
two ticks of an admitted primitive call. A binary operation declines before
any effect unless the mode names a defined format and both 64-byte sources
and the backed destination lie in ordinary memory outside the return
stack; it reads both sources before writing, and computes with the same
`tile_values` kernel the Python service uses. The result gains a last
element, `(mode, source0, source1, destination, operations)` or `None`,
which the owner publishes to the tile service and the Field ALU registers
and to the tile-operation accountant before anything else observes them.
The owner passes tile state only while the runtime keeps the tile service it
started with, bound to this extension's own `tile_execute_values`. It also
binds the service's identity guard with `bind_tile_guard(callable)`; the
program calls it once, at the first tile word of a run, and leaves every tile
word of that run to Python when it returns false.
