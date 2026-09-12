# Native semantic execution boundary

`_megaforth_native.NativeProgram(regions, page_size, continuation_type)` retains the ordinary
regions as `(base, size, pages)` triples. `pages` is the existing sparse
region's dictionary of page index to fixed-size bytearray. The executor holds
the GIL and uses those same bytearrays; there is no copied guest address space.

`install(xt, operations)` installs a plan of `(opcode, a, b)` triples, one per
original IR operation, so branch targets and returned IPs remain original IR
indices. `clear()` drops all plans. The Python owner invalidates plans for
dictionary publication/rollback and mutable execution bindings. Opcode
constants are exported with the `OP_` prefix.

`run(xt, ip, data_state, return_state, continuations, remaining_steps)` accepts:

- `data_state = (floor, empty_pointer, pointer)`;
- `return_state = (floor, empty_pointer, pointer, continuation_cookie)`;
- the return stack's ordinary slot-to-`(Continuation, raw_cookie)` dictionary;
- a nonnegative allowance in the existing semantic-step units.

It returns `(xt, ip, completed_steps, data_pointer, return_pointer,
continuation_cookie, continuation_updates)`. Each continuation update is
`(slot_address, caller_xt, return_ip, raw_cookie)`. The Python owner installs
these as ordinary non-root, non-fault continuations, preserving updates to
already-popped slots, and updates the stack pointers and cookie counter. A zero
`caller_xt` is a type deletion from a user push, including one that wrote the
same raw value as the previous cookie. Zero is never a valid colon XT.

Native colon calls write exact opaque continuation cookies into the shared
return-stack bytes. Native returns can also consume preexisting ordinary
continuations, read lazily from the same dictionary and verified against shared
bytes and the exact `continuation_type`. Root/fault returns and stale metadata
return to Python before mutation. `>R`, `R>`, and `R@` use the single ordered
return stack; exposed continuations are never mistaken for user cells or loop
indices. Native still excludes pair return-stack operations, DO/LOOP,
stack-pointer introspection/restoration, dynamic execution, services, and any
ordinary memory access intersecting the return-stack backing interval.

`OP_LITERAL`, `OP_BRANCH`, `OP_BRANCH_ZERO`, `OP_CALL`, `OP_RETURN`,
`OP_STORE_VALUE`, `OP_STRING_LITERAL`, `OP_R_PUSH`, `OP_R_POP`, and `OP_R_PEEK`
cost one semantic step. `OP_PUSH_CELL`
(constant or plain created-body address), `OP_FETCH_VALUE`, and admitted
primitive-call operations cost two. `OP_STOP` is uncharged. `a` holds the
literal, branch IP, called XT, or data address where applicable;
`OP_STRING_LITERAL` uses `(a, b)` for the already-resolved address and length.
Arithmetic, stack, and scalar-memory primitive operations ignore `a` and `b`.

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
