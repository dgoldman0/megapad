# Hybrid native dictionary and compiler integration

Date: 2026-09-30

Status: reviewed staged design, committed as the compiler-integration design
deliverable for the unified-runtime work. Implementation remains separate.
This document enables no compiler, loader, JIT, or introspection capability. It
builds on the current [hybrid routines](hybrid-runtime.md), whose callbacks
already run on the caller's stacks and whose machine frames already live on
the Forth return stack.

If compiler integration proceeds, start with read-only execution mapping and
lifetime inspection, followed by bounded machine-module publication. Transparent
compilation of existing semantic words requires additional stack, allocation,
and accounting contracts before `JIT-ON` can acquire a new meaning.

## Source facts and incompatible representations

| Current source | Constraint on this work |
| --- | --- |
| [`simulator/dictionary.py`](../simulator/dictionary.py), `Dictionary.define`, `Word.body_address` | Header is an eight-byte link, one flags/length byte, unpadded name, and an eight-byte semantic code slot. XT is the slot address; body is `XT + 8`. The slot is not MP64 instructions. |
| [`bios.asm`](../bios.asm), `entry_to_code`, `w_create`, `w_to_body`, `does_runtime` | Architectural XTs point to executable bytes. Native CREATE emits a 30-byte trampoline; its data field is `XT + 30`. DOES> patches that trampoline. Equal header prefixes do not make the two body layouts interchangeable. |
| [`simulator/runtime.py`](../simulator/runtime.py), `_evaluate_token`, `_apply_directive` | Non-immediate calls compile to semantic `Call(xt)`; immediate words execute during compilation. Semicolon publishes a complete `ColonDefinition` and its literal pool. This compiler does not emit BIOS call sequences. |
| `CreatedDefinition`, `DoesBodyRef`, `InstallDoes` in the same file | CREATE execution pushes the child's existing body address. Its action identifies a defining word's XT and an absolute IR index. Installing DOES> explicitly invalidates native semantic plans. |
| [`simulator/native_execution.py`](../simulator/native_execution.py), `NativeExecutor` | Native execution currently means C++ execution of semantic IR. Plans preserve original operation indices and stop at unsupported boundaries. Dictionary execution-generation changes clear plans; this is not an MP64 code cache. |
| `BodyAllocationLease` in `simulator/dictionary.py` | A lease covers one exact, nonempty `initial_body` allocation. Later comma/ALLOT growth does not extend it. An empty CREATE followed by `,` cannot acquire this lease for the appended bytes. |
| [`hybrid/runtime.py`](../hybrid/runtime.py), `register` and `call` | A routine word has a `RoutineDefinition` whose body holds its code. Each call checks only that the body allocation is still live. Publication is an idle host operation, not an action inside guest compilation. |
| [`simulator/stacks.py`](../simulator/stacks.py), `ReturnStack`, and native executor settlement | Raw return cookies, continuation metadata, popped cells, and inactive slots remain observable through later pointer restoration. Native settlement includes popped-slot updates, not merely active stack contents. |
| [`simulator/core_words.py`](../simulator/core_words.py), `JIT-ON`, `JIT-OFF` | Both are currently semantic no-ops. The architectural BIOS words instead select native emission/peepholes; `JIT-STATS` and `JIT-RESET` operate on those BIOS counters. |
| [`docs/dictionary-acceleration.md`](dictionary-acceleration.md) | Name caches and indexes accelerate authoritative dictionary lookup. They neither confer executable authority nor replace code-publication and allocation lifetimes. |

Source-visible `>BODY` in the simulator additionally requires a live
CREATE-family XT. A host `Word.body_address` property on another word does not
grant source `>BODY` access or execution authority. The current machine bridge
does not convert an arbitrary body address into a semantic XT.

The BIOS compiler's `compile_call` emits a ten-byte `SEP R16` plus inline XT;
its return uses `SEP R17`. Its handlers manipulate the architectural return
stack, and inlined primitives use the native Forth data stack. The bounded
integer runner fixes selectors and excludes program-counter selection. Loading
those BIOS bytes into today's runner is therefore not a native dictionary
integration strategy.

## Capability stages

These are separate capabilities, not automatic consequences of selecting
`--mode hybrid` or `--executor native`. New wire schemas and native ABI versions
must be assigned in the relevant implementation contract; this document does
not consume a callback-version number.

| Stage | First admitted behavior | Required gate before enabling it |
| --- | --- | --- |
| D1: execution mapping inspection | Host queries distinguish semantic XT/IR, native semantic plan, and a declared MP64 entry, including stale entries. No guest code generation. | Identity, shadowing, rollback, closed-session, and read-only inspection tests. |
| D2: bounded leaf machine module | A validated package publishes independent integer routine exports using the current explicit function ABI. No BIOS dictionary image or implicit linking. | Atomic package publication, lease/seal/cache tests, architectural differentials, and application loading before source compilation. |
| D3: compiler execution overlay | A host-enabled compiler may accelerate a qualified region of an existing semantic definition while leaving its Word, body, and IR authoritative. Initially straight-line integer operations only. | A new compiler stack/accounting boundary, owned executable arena, every IR fault/budget boundary, and inactive-slot equivalence. |
| D4: defining-word and guest-JIT integration | A separately enabled source profile may request native compilation; CREATE/DOES>, immediate behavior, and publication timing remain defined. Unsupported source continues through the semantic compiler under the declared policy. | Defining-word, compile-time effects, allocation/rollback, raw-emission rejection, and unchanged-source gates. |
| D5: native image authority | A different construction profile owns a native dictionary/image and semantically accelerates qualified regions. | A separate native-authority ABI and image/state-mapping design. Not part of D1–D4. |

D1 and D2 do not require general callbacks. Adding an optional callback import
to a later module profile requires its exact already-qualified callback ABI;
callback availability alone never admits compiler words, arbitrary `EXECUTE`,
source evaluation, or dictionary mutation from a machine frame.

## Execution mapping and introspection

Keep three identities distinct:

1. A semantic binding: owning runtime, exact live Word object, numeric XT, and
   implementation identity. Name lookup selects the latest binding; previously
   compiled calls retain their existing XT semantics.
2. A semantic plan: exact IR definition, original instruction indices, captured
   dependencies, and a plan generation. It belongs to the C++ semantic executor
   and has no MP64 entry address.
3. A machine publication: exact registration/compiler-overlay identity, code
   allocation lease and serial, published padded span, entry offset, profile,
   and native publication identity where applicable.

D1 should extend the existing value-only registration inspection surface with
a host query for an exact Word or a resolved live XT. Proposed result fields
are `execution_kind`, `semantic_xt`, `header_address`, `body_address`,
`ir_entry`, `machine_entry`, `code_span`, `profile`, `publication_id`, and a
fresh liveness/rejection status. Absent concepts are null, not fabricated
addresses. Private lease objects, native pointers, and continuation cookies
are not returned. An old inspection result never grants
entry or renews a lease.

Inspecting a word must not compile it, run it, allocate code, flush I-cache,
or repair a stale registration. A machine-byte/disassembly request names an
existing sealed publication and its bounded span; semantic slots are labeled
as semantic metadata rather than disassembled as code. Report backing bytes
and publication identity, without claiming that arbitrary raw writes are
already instruction-visible. Guest-visible introspection requires its own
versioned vocabulary and rejection contract at D4; do not silently change
`'`, `[']`, `EXECUTE`, `>BODY`, `HERE`, or dictionary search.

An overlay for an existing colon must not replace `ColonDefinition`, insert a
hidden public word, append bytes to the literal pool, or move source-visible
`HERE` as a side effect of first execution. Its machine entry lives in a
separately reserved executable arena. Existing D2 wrapper words may retain
the current body-allocation representation because their publication and
dictionary growth are explicit.

## Minimal machine-module profile

D2 begins with a package of the existing bounded integer-routine declarations,
not a general object-file linker. Each export has one independent sealed image,
one declared entry, zero through eight input/output cells, existing buffer
rules, and the shared Forth return stack like every routine. Code uses the admitted integer
runner, local relative control flow, and data addresses supplied as arguments.
No constructors execute during loading.

The initial package has no relocations, undefined symbols, inter-export machine
calls, callback imports, writable static image sections, native dictionary
headers, services, or MMIO. Two exports can be called sequentially by semantic
source and share caller-owned data through their explicit buffer rules. This
is useful composition without requiring general native Forth linkage.

Reuse the existing finite session ceilings: 64 issued routines, 1 MiB padded
code per routine, 16 MiB total code, 4 MiB private control storage, 64 KiB stack
per routine, and existing per-call/outer-dispatch instruction limits. Revoked
entries do not reset issuance accounting to allow an unbounded load/unload
loop. An aggregate package and all referenced images must be validated before
publication, with bounded file and declaration counts. Loader paths and hashes
remain ordinary host provenance, not guest execution authority.

All package exports publish as one session-owner transaction. Preflight names,
geometry, budgets, code/control reservations, and host table capacity; stage
complete bytes; publish native code; then expose all semantic bindings. Failure
revokes any provisional native publications and restores the package's
dictionary checkpoint before guest source can observe a partial module.
Failure after forwarding a native publication must be detected by its exact
publication identity, as in current registration cleanup. A cleanup failure
poisons further entry rather than making a partial package callable.

This is a new aggregate guarantee: current per-routine registration cleanup is
the foundation, not proof that an arbitrary multi-export loader is atomic.
Keep KDOS's `PROVIDED`/`REQUIRE` source-module registry separate; a native
package does not become a provided source module by name coincidence.

## Allocation, publication, and revocation

D3 needs an executable-arena allocator with issued, non-copyable leases and
allocation serials. Reserve its fixed ordinary-memory span at session
construction, disjoint from dictionary allocation zones, data allocators,
all semantic stacks, and private control storage. The reservation must be
visible to the relevant allocator policy before source starts. It is shared
guest backing, not a second authoritative image. Exhaustion declines the
optional overlay without changing the source definition.

Do not reuse `Dictionary.execution_generation` as an allocation serial. It
does not change for every frontier move; raw guest stores also have different
meaning from allocator reuse. Extend allocation ownership explicitly before
emitting overlays. Existing `BodyAllocationLease` remains an exact initial-body
lease; it must not be widened retrospectively to cover later CREATE data.

At one quiescent owner boundary, an overlay publication must:

1. Revalidate the exact source Word/IR, dependency Words, signature and admitted
   operations. No immediate word or source parser runs during lowering.
2. Reserve code/control storage with finite limits, generate into private host
   staging, validate code and metadata completely, then write the owned span.
3. Seal every executable byte including padding, acquire the final allocation
   lease, and publish through the runner's explicit code-publication operation.
4. Invalidate only overlapping architectural I-cache lines and clear the fetch
   window through that publication boundary. Publish the execution mapping
   only after all prior steps succeed.

Raw backing writes continue to leave resident architectural I-cache lines
unchanged, so Forth writes over published code take effect as the instruction
cache allows, as on the chip.
Revocation removes entry authority and semantic plans which depend on that
publication. It does not globally flush architectural caches. A replacement
allocation, even at the same address with identical bytes, receives a new
identity and an explicit publication that makes those bytes fetch-visible.

Rollback, `LATEST!`, backward ALLOT, zone reopening, overlapping comma/C, or
transient writes must retain their existing source semantics while revoking
affected allocation/word dependencies before reuse. Unrelated publication and
name shadowing do not revoke an older live Word merely because it is no longer
the newest binding. Initially broad plan invalidation is acceptable; precise
dependency invalidation is a later measured optimization. Unknown/reused XTs
must resolve through the semantic dictionary, never through an address-only
machine cache.

The current `InstallDoes` invalidation must cover future machine dependencies
too: an overlay captures the child action and the exact defining Word/IR
entry it lowered. A changed action, removed source Word, or reused XT declines
that overlay; it does not silently bind generated code to a same-named word.
Semantic fallback continues to use the existing dispatcher behavior.

## Compiler and retained-stack boundary

The current routine ABI is not a transparent colon optimizer. It peeks inputs,
preflights output space, and replaces inputs only on successful machine return.
Ordinary semantic execution instead exposes completed IR effects on underflow,
overflow, budget exhaustion, and faults. For example, `DROP DROP` with one
input consumes that input before the second DROP fails. Whole-word input
preflight would change this result. Even a successful `DUP DROP` can leave an
observable inactive data-stack cell; final outputs alone do not describe it.

D3 therefore requires a separate compiler execution contract, reviewed before
code. Its first subset is straight-line literal/canonical integer-primitive
IR, with no memory/service operations, dynamic XT, colon call, defining word,
loop, exception, suspension, or stack-pointer operation. Entry and return stay
at original IR indices; the ordinary dispatcher owns the source word's root
continuation and final Return. This deliberately avoids native BIOS stack and
selector conventions.

The new adapter must preserve the complete ordered data-stack write prefix,
including values in popped/inactive slots, and settle the exact pointer before
any host observer or semantic fallback. Access to semantic stack storage is a
new compiler-owned role with canonical geometry and emitted-operation proof;
ordinary declared machine routines retain their blanket stack exclusion.
Do not grant a general buffer permission merely to let compiled code use R14.
Whether this role writes shared cells directly or settles a bounded native
write record must be fixed in that ABI and differentially checked.

Each emitted interval needs an original-IR boundary/cost map. Budget and host
quantum exhaustion resume the correct IR operation with its completed prefix;
they must not restart a partly executed word. Optional admission may decline
before effects when the remaining allowance or stack geometry cannot be
proved safe, allowing the normal semantic dispatcher to continue. Once a
machine interval has effects, failure requires exact settlement; it cannot
replay from entry or turn into a successful fallback.

Source semantic ticks still advance semantic diagnostics and Timer exactly
once. MP64 instructions/cycles are separate counters and consume the same
outer machine allowance across intervals. RTC remains the configured RTC
policy. A fused sequence does not collapse source ticks, and publication or
recompilation cannot replenish an execution budget.

Later inclusion of semantic colon calls, `>R`, loops, `RP@`/`RP!`, or suspension
requires all continuation cookies, original `(XT, IR index)` locations,
pointer-capture generations, and retained inactive return slots. A saved
semantic continuation must remain semantic; replacing it with a machine PC
would make rollback and later RP! depend on reclaimed generated code. Use
revocable execution overlays around that canonical continuation, not native
PCs hidden in semantic return cells. Clear/abort paths must preserve or restore
the same retained metadata as the reference guard.

## CREATE, DOES>, immediates, and guest JIT prerequisites

Keep parsing and compile-time execution under the existing semantic owner.
Immediate words run at the same point and with the same source cursor, stack,
output, and dictionary effects. Lower only a completed IR body; never execute
an immediate twice while discovering compilation eligibility. Persistent
compile state, bracket mode, temporary interpreted IF definitions, errors,
and their existing rollback behavior remain reference-dispatcher work.

Immediate metadata and installed directives are not the complete BIOS
metacompiler vocabulary. The current core installer does not publish guest
`IMMEDIATE`, `POSTPONE`, `COMPILE,`, or `LITERAL` words. D4 must either admit and
qualify each required source word explicitly or report the narrower supported
compiler profile. An emission backend does not fill these source-language
gaps automatically. In particular, marking the most recent definition
immediate must preserve its XT/body and invalidate any affected compile-time
lookup metadata; the implementation must not accidentally replace an existing
Word with a new identity merely to change its frozen metadata.

The first defining-word stage leaves CREATE and `InstallDoes` in that
dispatcher. A child continues to push its original `XT + 8` body address and
then enters the recorded semantic DOES> suffix. A later qualified suffix
overlay maps the exact defining Word plus absolute IR entry index; it does
not add a 30-byte native trampoline to the child's body or move existing data.
Multiple children, old compiled bindings, deferred action cells, and literal
pool addresses retain their current source behavior.

Before D4 can change guest `JIT-ON`/`JIT-OFF`, all of the following are required:

- D3's code ownership, prefix settlement, accounting, and revocation gates
  pass. Installation must have a compiler-owned publication boundary: today's
  host registration rejects active source/dispatch and cannot simply be
  called from semicolon.
- Construction explicitly selects the new compiler profile and reports its
  capability. Pure simulator and earlier hybrid profiles retain no-op words.
  `JIT-ON` requests lowering of subsequently completed eligible definitions;
  `JIT-OFF` stops new lowering. Already published overlays remain valid until
  normal revocation or an explicit owner operation removes them. Neither word
  changes the selected C++/Python semantic executor.
- Eligibility failure has no guest compile-time effects beyond the existing
  semantic compilation. Diagnostics distinguish declined, compiled, invalidated,
  and executed overlays. They must not claim BIOS inlining/byte-saving counters
  or native-image compatibility without corresponding implementations.
- Raw opcode emission and native-address assumptions have explicit boundaries.
  Current hosted C, has a narrow compile-time IDL special case; D4 must not
  reinterpret arbitrary comma bytes as code. BIOS compile_call/SEP sequences,
  trampoline patching, relocations, and native code-field inspection remain
  rejected or semantic-only unless separately admitted.

## Qualification and next implementation order

Existing regression anchors, to be run through the serialized Make targets
when an implementation touches their contracts:

| Contract | Existing paths |
| --- | --- |
| Dictionary layout, identity, rewind and allocation | `tests/simulator/test_dictionary.py`, `tests/simulator/test_dictionary_numeric_rollback.py`, `tests/simulator/test_dictionary_leases.py` |
| Compiler, defining words, immediate/source state | `tests/simulator/test_runtime.py`, `tests/simulator/test_compiler_extensions.py`, `tests/simulator/test_bios_evaluator.py`, `tests/simulator/test_bios_source_primitives.py` |
| Semantic execution, original budgets and retained slots | `tests/simulator/test_native_execution.py`, `tests/simulator/test_native_stack_pointers.py`, `tests/simulator/test_stacks.py`, `tests/simulator/test_stack_snapshots.py` |
| Machine publication, admission and composition | `tests/test_native_hybrid_routine.py`, `tests/test_hybrid_runtime.py`, `tests/test_hybrid_registration_failure.py`, `tests/test_hybrid_manifest.py` |
| Application ownership and source bootstrap | `tests/test_hybrid_session.py`, `tests/test_unified_launcher.py` |

New D1/D2 tests must cover same-name shadowing, exact XT reuse, stale inspection
records, identical-byte reallocation, publication failure after forwarding,
partial module cleanup, and read-only inspection of a closed owner. A raw
runner cache differential must separately demonstrate stale cached code before
publication and new code after publication; composition must reject a broken
seal before either can execute.

D3/D4 gates compare Python semantics, C++ semantics, and compiler-enabled
hybrid at every original IR budget/quantum boundary. Include stack underflow,
transient overflow, inactive SP!/RP! restoration, literal addresses, immediate
side effects, CREATE children sharing a DOES> suffix, suffix branch targets,
shadowed definitions, action replacement, rollback and XT reuse. Architectural
differentials check admitted generated MP64 execution, not semantic-source
layout equivalence with the BIOS's different dictionary representation.

Only after those focused gates should a bounded MegaPad-owned source workload
measure cold lowering/publication cost, warm execution, invalidation, code
bytes, peak memory, and fallbacks. Existing
`bench_guest_jit_source_load.py` targets the architectural BIOS path; its name
does not qualify semantic or hybrid guest JIT. A separate equivalent workload
and unchanged-source acceptance report are required before a speed claim or
default-policy change.

Implement D1 first, then D2's explicit package transaction. Commit the compiler
stack/arena ABI before D3 code, and qualify D3 before enabling D4's guest controls.
General native images, arbitrary self-modifying code, complete snapshots,
multicore execution, and live conversion between semantic/native authority
remain deferred. No callback milestone or host introspection API implicitly
enables them.
