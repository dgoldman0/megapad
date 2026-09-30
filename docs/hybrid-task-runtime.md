# Host-prepared task routines

The task adapter lets declared machine routines call captured Forth definitions
on the caller's task stacks. It uses the existing hybrid CPU, memory, terminal
backend and session owner. Task callback dispatch runs through the Python
reference interpreter, including when the outer semantic executor is native.

Prepare the runtime before attaching a session. A registration batch can define
machine words, define their callback policies, capture exports, and attach
callback sites atomically. A failed batch removes its new dictionary bodies and
native publications. Captured words retain their original identities through
name shadowing; forgetting a captured body invalidates its authority.

The default semantic stack allocations cover Bank0. Machine code bodies and
ordinary borrowed buffers must be outside those allocations. Configure an
external dictionary **after** constructing the core runtime; do not move core
installation into external memory.

```python
from asm import assemble
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession
from hybrid.task_adapter import NativeTaskAdapter
from shared.foreign_abi import ForeignSignatureV1
from simulator.memory import EXTERNAL_BASE

hybrid = HybridRuntime.create(
    executor="native", geometry={"bank0_size": 65536, "external_size": 65536})
session = None
try:
    runtime = hybrid.semantic
    base = EXTERNAL_BASE + 0x1000
    runtime.configure_dictionary_bounds(base, EXTERNAL_BASE + 0x10000,
                                        runtime.main_context)
    runtime.allot_dictionary(base - runtime.dictionary.here,
                             runtime.main_context)
    adapter = NativeTaskAdapter(hybrid)
    with adapter.registration_batch() as batch:
        word = batch.define_operation(
            "MACHINE-INC", bytes(assemble("inc r4\nret.l")),
            ForeignSignatureV1(input_cells=1, output_cells=1),
            max_instructions=16, max_callbacks=0)
    runtime.evaluate(b": ENTRY 41 MACHINE-INC ;")
    session = HybridSession(hybrid, "ENTRY", semantic_step_budget=1000,
                            semantic_quantum_steps=64,
                            machine_quantum_instructions=256)
    session.boot()
    # This particular entry cannot wait for input or an IDL wake.
    while not session.halted:
        session.run_boundary()
    assert runtime.main_context.data.snapshot() == (42,)
finally:
    if session is None:
        hybrid.close()
    else:
        session.close()
```

For a callback, use `batch.capture_export(target, signature, task_grants=...,
dynamic_targets=..., fault_target=...)`, then
`batch.set_callbacks(word, ((call_offset, stub_offset, export), ...))`.
Offsets identify actual `CALL.L` sites and sealed `RET.L` stubs in the machine
image. Use position-independent code because the batch allocates each body.
Machine borrows and semantic task grants are separate explicit permissions;
grant the callback the stack and ordinary memory ranges its captured words
need. Dynamic targets must be declared when capturing the export. Child target
tables are derived from those exact captured dependencies.

`machine_quantum_instructions` is an optional exact integer from 1 through
10,000,000. `None` preserves synchronous execution. A selected quantum limits
actual task machine instructions across one host turn; it does not replenish
the root's instruction, callback, entry or semantic budgets. Resume retains the
original selected quantum. Runnable yields use the existing session
continuation and require no interrupt. Genuine IDL and IDLE-UNTIL retain their
ordinary input/deadline wake behavior. The session backend owns wake, resume,
terminal backpressure and cancellation.

Shared-session status keeps aggregate machine counters and private ABI metadata
in `machine_execution`. Its separate `task_execution` entry reports the task
profile, reference callback executor, configured machine quantum, diagnostic
registration names, active depth and task-only counters. Registration names do
not promise that a dictionary lease remains live. Callback semantic counts
come from engine-issued receipts, independently of machine cycles and the
outer semantic step count.

Closing a session cancels its retained continuation before releasing semantic
ownership and native pins. Native cleanup is still attempted if semantic close
raises, and the first exception is preserved. An original semantic root cannot
mix private and task machine entries; separate roots may use either profile.

Generic task manifests, automatic source/publication ordering and launcher
support remain deferred. Transport availability alone does not enable the
`shared_task_exceptions` or `composite_suspension` application capabilities;
those remain gated on the corresponding prepared-session qualification.
