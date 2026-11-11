# Hybrid mode and compiled code: review of the plan and its gaps

Date: 2026-11-11

State reviewed: MegaPad `93d9cc07` and Akashic `f4cb2298`. The review read
documents, code and git history and ran nothing, so its account of what the
simulator does with an application image rests on the code alone.

A compiled build of the system's modules is to take a token form that every
backend loads, and hybrid mode is to load that form and machine-code
dictionary images as well
([`hybrid-native-dictionary-plan.md`](hybrid-native-dictionary-plan.md),
"Compiled modules: hybrid mode loads both forms"). This review asked what
stands between today's hybrid mode and each form.

The token form asks nothing of hybrid mode that it does not ask of the
simulator. A machine-code image asks for work that no stage of the plan
describes. The plan's one answer, stage D5, makes a native image the
authority for a whole session and has no design, and nothing covers a single
image inside a session whose dictionary is semantic. Three routes could close
that gap. The review recommends the smallest one that runs real machine code,
an image island beside semantic words. None of it starts before the format of
Akashic's applets is decided, because those applets are the only machine-code
dictionary images the system has.

## What was planned, and when

Hybrid mode entered MegaPad with the unified runtime plan on 2026-09-30
(`e95bcc92`). Running a machine-code dictionary was in that plan from its
first commit, as a later configuration under a different authority from the
mode then being built. Decision 15 of
[`unified-runtime-plan.md`](unified-runtime-plan.md) still reads as it did
that day:

> General binary compatibility needs native authority. A later hybrid
> configuration supporting arbitrary native dictionaries and binaries should
> use a native machine image as authority and accelerate qualified regions
> semantically. The initial semantic profile cannot silently promote itself
> into that capability or hot-switch at an arbitrary point.

Phase 5 of the same commit called general native images "later deliverables,
not implied results of Phase 4", and decision 14 kept machine-code support
apart from a guest JIT.

The mode that was built has the opposite arrangement. The semantic dictionary
is the authority, and declared routines run beside it as machine code
([`hybrid-runtime.md`](hybrid-runtime.md)). Its "Not covered" section sends
"running compiled Forth or BIOS code as machine code" to the staged design in
`hybrid-native-dictionary-plan.md`, written the same day (`c9f61a52`). That
design has five stages:

| Stage | What it admits | What it offers a machine-code image |
| --- | --- | --- |
| D1 | Host queries that tell a semantic word, a native semantic plan and a declared machine entry apart | Nothing; it runs no code |
| D2 | A package of bounded integer routines, published as one transaction | Nothing: "No BIOS dictionary image or implicit linking", and a package has no relocations, undefined symbols or native dictionary headers |
| D3, D4 | Machine code compiled from existing semantic definitions, first by the host and then at a guest's request | Nothing; they produce machine code from semantic words and load none |
| D5 | "A different construction profile owns a native dictionary/image and semantically accelerates qualified regions" | A whole session under native authority. Its gate is "A separate native-authority ABI and image/state-mapping design. Not part of D1–D4", and one table row is all it has |

The design's "Source facts" already rule out the short cut. The BIOS
compiler's call sequence changes the program-counter selector, so "Loading
those BIOS bytes into today's runner is therefore not a native dictionary
integration strategy."

The other documents draw the same boundary:

- The deferred list of `unified-runtime-plan.md` names "native image
  authority and arbitrary executable dictionaries" and "General
  self-modifying/native images". Its decision 7 says "Unknown, stale, or
  invalid XTs remain errors. Semantic dispatch never guesses that an
  arbitrary address is executable code."
- [`simulator-contract.md`](simulator-contract.md), section 2, claims no
  "execution of native MF64 dictionary images". Section 9 lets the simulator
  read, copy, hash and persist MF64 files, "but it does not execute them".
  Section 9 also reserves "A future semantic warm artifact" with its own magic
  and a cache key that includes the simulator's version: a compiled form
  private to the simulator. A token form that every backend loads is a
  different thing, and section 9 will need to say so when that form exists.
- [`integration-cleanup-plan.md`](integration-cleanup-plan.md) calls hybrid
  and unified execution "core goals". Its step 7 found that "Desk has no
  machine code to call: Akashic and KDOS are entirely Forth, and on the chip
  the only machine code is the BIOS and compiler output, which is the
  separate native dictionary design."
- The twelve hybrid documents that `bb2174eb` replaced with
  `hybrid-runtime.md` were routine ABIs and callback plans, and each closed by
  excluding native dictionaries. The first of them, `hybrid-runtime-abi.md`,
  said its profile "does not boot a native BIOS, execute an arbitrary native
  dictionary, or translate semantic XTs into machine addresses."

The plan has therefore always pointed at machine-code dictionaries, always
placed them after the routine mode, and never said how one image would enter
a session whose dictionary is semantic.

## What exists

Hybrid mode today is the routine mode. None of D1 to D5 has code.

- `HybridRuntime` in `hybrid/runtime.py` owns a semantic runtime and one MP64
  core over the same memory. `register` defines one routine word whose body
  holds its code, under a dictionary checkpoint, and refuses while the guest
  is running. `register_manifest` calls it once for each routine, so a
  manifest is not one transaction. `call`, `resume`, `_callback` and `_settle`
  carry a routine through its entry, its callbacks on the caller's stacks,
  its yields and its faults.
- `RoutineRunner` in `emulator/accel/mp64_accel.cpp` runs the machine side.
  `make_hybrid_image` accepts an image only if it decodes from its first byte
  to its last into instructions that `admitted` allows
  (`emulator/accel/cpu/mp64/routine_runner.h`), and `admitted` refuses an
  invalid instruction, a change of the program-counter selector and a write
  to r15. `RoutineRunner::ordinary` reaches memory only inside the entry's
  declared spans, `RoutineRunner::stack_slot` only inside the entry's own
  cells of the return stack, and `RoutineRunner::execute` interprets one
  instruction at a time.
- `RoutineDefinition` and `MegaForthRuntime.define_routine` in
  `simulator/runtime.py` make a routine an ordinary word, and
  `_machine_call`, `_machine_return_flow` and `_machine_flow` pass control to
  the owner and back. `shared/accel/routine_call.h` lets the native semantic
  executor call and resume a routine without Python.
- `HybridSharedMachine.status` in `hybrid/session.py` advertises machine
  code, declared machine routines and semantic callbacks. It denies arbitrary
  machine code, machine MMIO, native BIOS boot, multicore execution and
  native snapshots. `SimulatorSharedMachine.status` in `simulator/session.py`
  reports no machine code at all.
- `JIT-ON` and `JIT-OFF` are still no-ops in the simulator (`install_core` in
  `simulator/core_words.py`).
- The tests are `tests/test_hybrid_routines.py`, `tests/test_hybrid_session.py`,
  `tests/test_hybrid_manifest.py` and
  `tests/test_rich_terminal_hybrid_session.py`. `bench_hybrid_crossing.py`
  measures a crossing, and Akashic's
  `local_testing/physical_desktop_acceptance.py --backend hybrid` runs the
  physical Desktop journey in hybrid mode.

## The token form

Hybrid mode needs nothing of its own for it. `HybridSession` is
`SimulatorMachineSession` over the same `MegaForthRuntime`, and
`prepare_server` in `hybrid/server.py` boots through the same
`prepare_image_bootstrap` as the simulator's server. Whatever loads a token
file in the simulator loads it in hybrid mode.

One constraint follows for the token design. `prepare_server` registers a
manifest's routine words before the image boots, so a hybrid session's
dictionary holds words that the simulator's lacks. A token file must
therefore bind the words that exist before it by name and never by their
position in the dictionary. One check follows for its acceptance: the Desktop
journey once with `--backend hybrid`.

## Machine-code images

The image format in use is MF64, defined by Akashic's
`akashic/utils/binimg.f`. An image holds a segment, which is the dictionary
bytes compiled between `IMG-MARK` and the build, with tables of relocations,
exports and imports. `_IMG-LOAD-VERIFIED` copies the segment to `HERE` or
into XMEM, relocates it, has `_IMG-PATCH-IMPORTS` store the execution tokens
it found by name, and has `_IMG-SPLICE-VALID` link the image's headers into
the dictionary with `LATEST!`. The left column below names what such a
segment contains, by the BIOS routine that emits it; the right column names
where today's hybrid mode stops.

| | What the BIOS compiler puts in a dictionary (`bios.asm`) | Where hybrid mode stops |
| --- | --- | --- |
| Call and return | `compile_call` emits `sep r16` and an inline 8-byte XT, `compile_ret` emits `sep r17`, and `forth_next` and `forth_exit` dispatch them | `admitted` refuses a change of selector. Inline XTs, strings and data fields are not instructions, so `make_hybrid_image` refuses the image before anything runs |
| Data stack | `compile_literal` and the control-flow words, `w_if` among them, emit inline instructions that push and pop through r14 | A routine takes and gives cells in r4 to r11 and reaches memory only through declared buffers. Both semantic stacks do live in guest memory (`DataStack` and `ReturnStack` in `simulator/stacks.py`), so they could be shared |
| Return stack | `forth_next` pushes return addresses on r15, and `w_do` emits inline code that keeps the loop frame there | `admitted` refuses a write to r15 outside CALL.L and RET.L. Semantic return cells also carry continuation records (plan, "Compiler and retained-stack boundary") |
| `CREATE` and `DOES>` | `w_create` emits a 30-byte trampoline, `w_to_body` answers XT+30, and `does_runtime` patches the trampoline | A semantic body is XT+8, and `>BODY` requires a semantic `CREATE` word |
| Headers and lookup | The loader links the image's headers into the chain with `LATEST!` | `Dictionary.set_latest` in `simulator/dictionary.py` accepts zero or a live semantic ancestor and refuses any other chain |
| Calls out of the image | Every word that `jit_compile_word` does not inline becomes a `compile_call`; with the JIT off that is every word. The loader patches each import with the XT found by name, which in a semantic session is a semantic XT | A callback exists only at a site the manifest declares, with one fixed target word. Nothing takes an XT from the instruction stream |
| `EXECUTE` and `THROW` | `w_execute` calls the XT on the stack | `EXECUTE` of an image word needs a semantic `Word` for it. Unwinding past machine entries already works (`hybrid-runtime.md`, "Callbacks") |
| BIOS addresses | `_IMG-FIXED-ABI?` in `binimg.f` keeps references to low addresses that have no dictionary entry, BIOS runtime helpers among them, verbatim | A semantic session has no BIOS code at those addresses. Such an image is also tied to the BIOS build it was compiled under |
| Inlined code | KDOS turns the JIT on at its start and off only after `autoexec.f` returns (`JIT-ON` and `JIT-OFF`, lines 39 and 10182 of `kdos.f`), and the Desktop's autoexec runs Desk without returning. KDOS, every module the boot loads and an applet built in Pad therefore hold the primitives of `jit_inline_table` and `jit_bigram_table` inline, `@` and `!` among them. Code compiled at the prompt after a boot is compiled with the JIT off | An inlined load or store reaches whatever address is on the stack. The machine would need the session's ordinary memory, and MMIO wherever code stores to a device, which the mode denies |
| Publication | An image loads from guest code at run time, into the dictionary or into XMEM | `HybridRuntime.register` is a host operation at an idle boundary, and a body lease covers one routine |
| Rollback | `binimg` has no unload, and Akashic's applet loader rolls back with `DICT-ROLLBACK` | Revocation follows the body lease of one routine |
| Devices, snapshots, cores | Image code reaches devices through BIOS words, and through any store it inlines to a device address | Machine MMIO, native snapshots and multicore execution are all denied |

The row on calls out of the image has a cost that the table cannot show. In a
semantic session every call to a BIOS word would cross into a semantic word,
at what `hybrid-runtime.md` measures for a callback, between 1.1 and 1.2
microseconds with the native executor. The eighteen primitives of
`jit_inline_table` never cross in code compiled with the JIT on, and every
other BIOS word still does. The figure is an inference from the document's
table and has no measurement of image code behind it. It means an island
serves code whose inner loops stay within the inlined primitives, or small
images, unless it learns to run more BIOS words without a crossing.

## Routes

| | A. Native authority (stage D5) | B. Image island | C. Lifting |
| --- | --- | --- | --- |
| What it is | The whole session is native, BIOS and KDOS included, and the semantic engine accelerates regions of it | The routine runner learns the chip's Forth convention, and an image runs as machine code inside a semantic session | An image's words are translated into semantic definitions when it is spliced, by recognizing the BIOS compiler's templates; anything else is refused |
| What it needs | The two designs D5 lacks. Nothing from D1 to D4. The one existing piece of this kind is the emulator's accel hooks (`_ACCEL_HOOKS` in `emulator/session.py`), which replace four BIOS graphics routines | A new stage beside D2: `sep r16` and `sep r17`, r14 and r15 on the shared stacks, the session's ordinary memory open to inlined loads and stores, decoding at run time, the image's headers adopted as words at `LATEST!`, calls to semantic XTs as callbacks, and a listed set of fixed BIOS and KDOS addresses that it provides itself | A template table generated from `bios.asm` or checked against the emulator, and semantic words that keep the native body layout |
| What it covers | Every image and the BIOS itself | Images whose outside references are named imports or addresses on that list, with or without inlined primitives. Akashic's applet images are of that kind | Images made only of recognized templates |
| What it proves | The machine code, as the emulator does | The image's machine code, with the BIOS words it calls still semantic | The file and its loader; no instruction executes |
| Size | The largest; in effect an accelerated emulator | Comparable to steps 3 to 5 of the integration cleanup | Comparable to B, with less risk at run time and a standing coupling to the compiler's output |
| Journey and contract | A journey under it is a new, slower gate. The simulator contract is untouched | Simulator mode still refuses images. A hybrid journey could cover applets | Works in simulator mode too, so sections 2 and 9 of the contract change and the ordinary journey could load images |

D1 to D4 lead to none of these. D2 excludes dictionary images, and D3 and D4
compile semantic words into machine code, which is the opposite direction.

The review recommends B if machine-code images must run in hybrid mode. It
continues what the cleanup built, a shared return stack and callbacks on the
caller's stacks, and it is the smallest route on which the image's own
instructions execute. Its first two steps would be a written ABI and a
measurement on one real applet image, because the cost of crossings decides
whether it serves anything larger than an applet. A stays the plan for
whole-system native execution. C fits only if the aim is that the ordinary
journey loads image files.

Reading could not establish four things: the effort of each route beyond
these comparisons, the speed of image code on the runner against semantic
execution, how many templates the BIOS compiler emits, and whether the two
backends place the stacks at the same addresses, which matters for A.

## Application images on the simulator

Akashic's Desk requires `app-loader.f` and `app-builder.f`, and Pad requires
`app-builder.f`, so `binimg.f` loads from source on every backend. What
happens next was read from the code.

Nothing refuses a build. `IMG-MARK` sets the cells `_RELOC-ACTIVE`,
`_RELOC-COUNT` and `_RELOC-BUF`, which the simulator provides as plain cells
that its compiler never fills (`install_core`), and `_IMG-VERIFY-MEM` checks
structure only. Pad's Build and Install therefore appears to install an image
made of headers and 8-byte semantic code slots, with no imports and no code.

A launch fails at the splice. `Dictionary.set_latest` refuses the image's
header chain, `MegaForthRuntime.set_dictionary_latest` raises a dictionary
fault, and KDOS's `_KDOS-DICT-FAULT` throws `U-DICT-E-FULL`, the
dictionary-full code, into the CATCH under which `ALOAD-PATH` and its
siblings run. The loader rolls the dictionary back. An image built on the
emulator fails at the same point.

No test covers either. MegaPad asserts the refusal of `LATEST!` itself
(`test_latest_store_rejects_unknown_machine_code_chains_without_consuming`
in `tests/simulator/test_bios_source_primitives.py`). Akashic's image tests
construct the emulator directly, and its journey that builds, installs and
launches an applet belongs to the profile `desktop-local-applet`, which is
not the physical Desktop journey.

## Decided on 2026-11-11

- For now, handling a machine-code image means loading and running one that
  an emulator or a device built. Building an image inside a semantic session
  and booting a whole system from machine code are not asked of hybrid mode.
- Whether Akashic's applets take the token form is decided when the token
  form has measurements. With token-form applets no machine-code image is
  left for hybrid mode to load, which is the cleanup's step 7 finding again.
- No machine-code work in hybrid mode starts before that decision.
- If images must then run in hybrid mode, the island is the recommended
  route.
- A system whose dictionary holds no machine code refuses to build or load
  an image at once, with a status that says why.

## Open

- The format of Akashic's applets, which waits for the token form's
  measurements.
- The island's ABI, which would be written before any of its code.
- The two designs D5 lacks: the native-authority ABI, and the mapping between
  the native image's state and the semantic engine's.
- Where machine-code work would fall relative to the multicore simulator.
  Hybrid mode reports one core, and either route has to say what a machine
  entry means with several.
- The plan still speaks of sealed images in D1 to D3, while the integration
  cleanup removed code seals from the routine path. Whether a package is
  sealed again belongs to D2's design.
