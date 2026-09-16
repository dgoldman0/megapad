# Native semantic execution boundary

The extension and its Python owner are built from the same checkout and use
one current internal interface. Rebuild the extension after changing this
boundary with `python setup_simulator_accel.py build_ext --inplace --force`.

`_megaforth_native.NativeProgram(regions, page_size, continuation_type)` retains the ordinary
regions as `(base, size, pages)` triples. `pages` is the existing sparse
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

`install(xt, operations)` installs a plan of `(opcode, a, b)` triples, one per
original IR operation, so branch targets and returned IPs remain original IR
indices. `clear()` drops all plans. The Python owner invalidates plans for
dictionary publication/rollback and mutable execution bindings. Opcode
constants are exported with the `OP_` prefix.

Installation also recognizes literal/constant arithmetic and `DUP` conditional
branch superinstructions without removing either original index. A whole pair
must fit the allowance and pass preflight; otherwise its original first
instruction runs. Intermediate popped bytes are still materialized. Stack
operations may resolve an entire operand span through a qualified page;
fragmented, absent and cross-page spans retain scalar handling.

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

`run(xt, ip, data_state, return_state, continuations, remaining_steps)` accepts:

- `data_state = (floor, empty_pointer, pointer)`;
- `return_state = (floor, empty_pointer, pointer, continuation_cookie)`;
- the return stack's ordinary slot-to-`(Continuation, raw_cookie)` dictionary;
- a nonnegative allowance in the existing semantic-step units.

It returns `(xt, ip, completed_steps, data_pointer, return_pointer,
continuation_cookie, continuation_updates, pointer_captures)`. Each continuation update is
`(slot_address, caller_xt, return_ip, raw_cookie)`. The Python owner installs
these as ordinary non-root, non-fault continuations, preserving updates to
already-popped slots, and updates the stack pointers and cookie counter. A zero
`caller_xt` is a type deletion from a user push, including one that wrote the
same raw value as the previous cookie. Zero is never a valid colon XT.

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
return to Python before mutation. `>R`, `R>`, and `R@` use the single ordered
return stack; exposed continuations are never mistaken for user cells or loop
indices. Counted loops use fixed-position limit/index pairs and preserve
modular equality termination, retained bytes, and type deletion on both
new frame slots. Identity-bound `I` and `J` never search past a continuation.
Identity-bound `EXECUTE` can enter an already installed colon plan, consuming
its XT and creating the same continuation as reference dispatch. Missing or
non-colon target plans fall through before effects; Python owns invalid-token,
stack-fault, service, and source-accelerator behavior. Dynamic-call target
availability is invalidated with the other plans.
Native still excludes pair return-stack operations,
stack-pointer restoration, other dynamic execution, services, and any
ordinary memory access intersecting the return-stack backing interval.

`OP_LITERAL`, `OP_BRANCH`, `OP_BRANCH_ZERO`, `OP_CALL`, `OP_RETURN`,
`OP_STORE_VALUE`, `OP_STRING_LITERAL`, `OP_R_PUSH`, `OP_R_POP`, `OP_R_PEEK`,
`OP_DO`, `OP_QUESTION_DO`, `OP_LOOP`, `OP_PLUS_LOOP`, and `OP_UNLOOP`
cost one semantic step. `OP_PUSH_CELL`
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

Every operation is preflighted before its ticks or effects. Unsupported
operations, missing plans, insufficient allowance, missing destination pages,
stack shape/capacity failures, and unsafe scalar memory spans return at that
uncharged original operation. Python then executes that exact operation,
including its normal partial fault effects. In particular, an allowance of
one does not admit a two-tick primitive call. Successful stack operations
write their ordinary shared bytes immediately, including retained bytes below
a popped frontier. Reads from missing ordinary pages return zero.

The Python owner settles the completed step count into the cumulative meter,
diagnostics, and exact timer state before executing a fallback operation or
publishing a host boundary. Native execution does not admit events, invent
IDL wakes, read wall time, or reset a watchdog. Empty/unsupported native runs
must fall through to the Python operation instead of being retried unchanged.
