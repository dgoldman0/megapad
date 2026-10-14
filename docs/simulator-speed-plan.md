# Simulator speed plan: boot compile, typing host work, guest work

Status: agreed 2026-10-13 (see Decisions). Step 1 is done (see Step 1
results): the boot compile fell from about 40 s to 2.5 s. Step 2 is done
(see Step 2): a keystroke fell from 0.40 s to 0.37 s. Step 3 is under way:
five of its design's seven parts are done (see Step 3).

The hosted simulator with the native executor already runs guest Forth in
C++, at about 80 million semantic steps per second. These measurements ask
what still makes the Desktop slow on the host, and what a "fully native
simulator" would and would not buy. The answer shapes three steps:

1. make the boot compile native;
2. remove the host work on the typing path;
3. find why a keystroke costs about 29 million guest steps.

Only step 3 also helps the device. Steps 1 and 2 are host-only gains for
development and tests, and the emulator smokes are unaffected, since they
do not use the simulator.

## What was measured

The setup was MegaPad `79ac6745` with Akashic `2c315526`, the `desktop-apt1`
image built for the simulator backend, and the native executor. The image
was driven by `bench_unified_desktop.py` at 280x84 with the Desktop's own
terminal policies, through the bundled eight-step keyboard journey plus 12
characters typed into Pad.

| Phase | Wall time | Native execution | Python |
| --- | --- | --- | --- |
| Boot compile (session preparation) | about 40 s | about 1 s | about 39 s, all on the main thread |
| Desk start to ready | about 15 s | 3 s | about 6 s on the simulator thread, about 1 s on the main thread; about 5 s waiting, not yet explained |
| One keystroke | 0.57 s | 0.37 s, for 29 million steps | about 0.11 s on the simulator thread, about 0.08 s on the main thread |

**Boot compile.** About 210,000 source lines are evaluated: KDOS, then
`networking.f` and `rich-terminal.f`, then 50 linked Akashic chunks. That is
about 850,000 tokens and 49,000 definitions. The time divides into four parts:

- **Per-line work, about 70 µs per line, about 15 s in total.** Most of it is
  bookkeeping:
  - about 18 checked memory reads and 11 writes per line, mostly the
    loader's walker variables and the `EVAL-*` cells;
  - about 2.3 full return-stack snapshots per line;
  - a duplicate tokenize-and-lookup prescan, which proves that skipping
    `CATCH` is safe.
- **Per-token work, about 5.8 µs per token, about 5 s.** Parsing, lookup,
  dispatch and building IR.
- **Dictionary index rebuilds, about 4 to 5 s.** Every interpret-mode
  `IF … THEN` at top level rolls back a temporary compiler.
  `_discard_temporary_compiler` (`simulator/runtime.py:3466`) then rebuilds the
  whole index, even when the rollback removed no word. About 15 rebuilds
  happen. Eleven of them come from Akashic load-time checks, and those cost
  about 0.4 s each at 40,000 or more words.
- **Tile-engine words at load time.** About 150,000 native fallbacks at
  `FP16-MODE`, `TSRC0!`, `TSRC1!`, `TDST!`, `TMUL`, `TADD` and `TSUB`, from
  half-precision math that runs while modules load. Each costs about 6 to
  21 µs of Python.

**Typing.** Each keystroke falls back from native code to Python about
2,500 times. The largest causes are:

| Cause | Fallbacks per keystroke |
| --- | --- |
| `2>R` / `2R>` pairs, which have no native opcode | about 630 |
| Quantum boundaries | about 270 |
| `KEY?` and UART read attempts | about 320 |
| `AbortIf` | about 65 |

The frame CRC is not a typing cost: about 17 KiB per keystroke, about 2 ms.
The simulator thread's Python time goes to rich-terminal host processing,
guest fallbacks and native entry overhead.

**Method.** `cProfile` distorts the live session too much. Boot ran four
times slower, and the rich terminal's negotiation failed on its real-time
deadlines. So the live figures come from two sources: the executor's own
counters (`MEGAFORTH_NATIVE_PROFILE=1`: `native_run_ns`, `settlement_ns` and
the exit counts), and per-thread CPU clocks. `cProfile` was used only for
the boot compile. For typing, a stack sampler that records each thread
separately says where the Python time goes. Unprivileged `perf` is disabled
on the development host.

## Step 1: boot compile

**1a. Remove measured redundancy in Python.** Each change must state why the
work it removes is redundant, and keep guest-visible behavior identical.

- **No index rebuild when a temporary-compiler rollback removed no word.**
  The bindings are then unchanged, so the incrementally maintained index is
  still exact. The BIOS `THEN` likewise only clears the temporary bytes and
  restores HERE. Such a rollback also need not advance the execution
  generation, which today discards every native plan. Hash each name once
  per insert.
- **Walker, `EVAL-*` and `BASE` cells through the qualified-view accessors.**
  Every value is still read each time, so the guest sees the same thing; the
  checked general path costs three times as much.
- **No `EVAL-STATUS` re-read after a token that cannot run guest code.** Only
  guest code or the evaluator changes it, and the evaluator knows when it did.
- **The existing native stack snapshot in the guard.**

The expected saving is 9 to 10 of the 40 seconds. 1a is measured on its own,
before 1b starts.

**1b. A native evaluator that owns the dictionary.** The design is in
[simulator-native-evaluator.md](simulator-native-evaluator.md). The
dictionary's host-side metadata moves into `_megaforth_native`: the bindings,
the words, their compiled IR and the side index. Python reads and changes
them through a facade with the existing `Dictionary` interface. This is a
step toward native construction of the dictionary generally, not only a
faster loader.

The two loader overlays pass whole runs of lines to a native line walker. It
evaluates each line the checked-line rule admits, and hands any other line
back to Python whole, because that rule chooses the per-line protocol for the
whole line. The work landed in three slices: the dictionary, the walker,
and plans that survive publication.

The rules:

- **Exactness.** Guest memory, both stack pointers, the continuation cookie
  counter, semantic steps, `HERE`, `LATEST`, every definition and its IR, the
  side index, the evaluator cells and UART output must match the Python
  evaluator's. Only host-side counters may differ.
- **Reference.** The Python backend stays as the reference for a differential
  gate. The gate compares complete boots of KDOS, the system modules and all
  50 Akashic chunks.
- **Contracts.** The evaluator keeps every rule in `simulator-contract.md` §4:
  - at most 255 bytes per `EVALUATE`;
  - sticky first failure;
  - the temporary `IF` lifetime;
  - transaction rollback;
  - identity-bound overlays that fall back to the original definition.

  It is not a compiled-Forth cache: source is evaluated every boot.

The walker should remove most of the per-line and per-token bookkeeping,
about 18 of the 27 s left after 1a. The wider rule moves most of the
remaining Python lines to the walker too. Both have to be measured; neither
is a promise.

**1c. Native tile-engine words.** Admit `FP16-MODE`, `TSRC0!`, `TSRC1!`,
`TDST!`, `TMUL`, `TADD` and `TSUB` in the native executor.

- Use the existing native tile kernel (`shared/accel/tile_values.cpp`).
- Carry the tile and Field ALU registers in and out the way FPCSR is
  carried.
- Decline before any tick or effect wherever Python would fault.

This removes about 150,000 boot fallbacks, about 1 to 2 s.

**1c result.** Done, together with `BF16-MODE` and `TMODE!`. The tile words
run natively only while the runtime keeps the canonical tile service bound
to this extension's own value kernel, so their results are the ones the
Python service computes; any other binding leaves them in Python. Native
executor entries during the `desktop-apt1` boot fall from 192,000 to
39,000, the boot fingerprint is unchanged, and four alternating boot
compiles gave medians of 4.7 s against 6.1 s before.

**Overlaid words' bodies.** Found while profiling after 1c: a word with a
Python execution overlay got no native plan, so whenever its overlay
declined, every operation of its body made a zero-progress native call and
then ran in Python. That is KDOS `EVALUATE-CHECKED`'s `CATCH` body on each of
the 9,343 boot lines the checked-line rule does not admit. Such words now
get plans that Python enters after the overlay declines, while calls and
native `EXECUTE` still stop so the overlay keeps every entry decision. The
boot fingerprint is unchanged, and four alternating boot compiles gave
medians of 3.1 s against 3.9 s before.

## Step 2: typing host work

Two changes, measured while typing in Pad:

- **Native pair operations** (`2060a7e9`). `2>R`, `2R>` and `2R@` run in the
  native executor. They were the largest cause of native stops, about 1,260
  per keystroke.
- **The control state table built once** (`740fa27d`). The rich terminal's
  control validator rebuilt a constant table on every call, about 220 times
  per keystroke. A call now takes 3.9 µs instead of 17.5 µs, which saves
  about 3 ms per keystroke.

| Per keystroke | After step 1 | With native pair operations |
| --- | --- | --- |
| Wall time | 0.403 s | 0.374 s |
| Simulator-thread CPU | 0.345 s | 0.322 s |
| Native execution | 0.268 s | 0.255 s |
| Native stops | 4,591 | 2,090 |

**What remains on the host.** About 0.07 s of simulator-thread Python per
keystroke. About half is rich-terminal host processing: decoding and
validating each retained operation the guest sends, then validating the
control graph again when each frame is projected. That validation is the
terminal's input checking, and the second pass is not shown to be
redundant, so it stays. The benchmark's main thread adds about 0.05 s: its
own polling of the screen text, which is harness work, and the viewer's
drawing.

The rest of a keystroke is guest work: 0.255 s of native execution for
about 29 million steps. That is step 3.

## Step 3: guest work per keystroke

A keystroke costs about 29 million semantic steps. This is the only step
that also makes the device faster, and the work is in Akashic.

**Method.** The guest phase profiler (`akashic_tui.py accept
--phase-profile`) runs only on the emulator: the simulator rejects its
instruction diagnostics. The simulator was sampled instead. At every host
quantum yield, which comes every 65,536 guest steps whether the quantum ended
in native or Python code, the sampler recorded the current word and every
colon continuation on the return stack. Twelve characters typed into Pad gave
5,357 samples, 446 per keystroke.

**Where a keystroke goes.** 89% of its guest steps are the rich-terminal
producer's prepare, which runs when the screen flush begins
(`RTHP-PREPARE`). Desk's own painting is 3.4%, and the terminal service 4%.
Inside the prepare:

| Share of the keystroke | Work |
| --- | --- |
| 34% | the shell family producer: build the family batch for the whole Desk screen (22%, of which its preflight validation is 14%), then freeze it into a bank (8%) |
| 32% | rebuild the candidate: the UIDL snapshot (10%) and the observed candidate (18.5%, of which its hybrid preflight is 9%) |
| 16% | compare the candidate with the active one |
| 7% | emit the delta |

Each keystroke presents one delta, yet the prepare rebuilds, validates and
compares the whole screen. Across all of it, the checks that caller spans do
not alias module storage take 19% of the keystroke, about 37,000 span
overlap tests. They nest: one module's check calls those of the modules it
depends on, so one top-level check can test the same region up to four
times.

**Range primitives** (Akashic `f0645eea`). `URANGE-VALID?`,
`URANGE-OVERLAP?` and `MSPAN-OVERLAP?` held 14% of the keystroke on their
own. They now compute the same results with fewer stack operations: a
disjoint `MSPAN-OVERLAP?` takes 46 steps instead of 92. A keystroke fell
from 29.27 to 27.51 million guest steps (6%). Simulator wall time did not
change measurably, since stack-only steps are the native executor's cheapest.

**Design for the rest.** Akashic `docs/rich-terminal/KEYSTROKE-FRAME-WORK.md`
(Akashic `28d118d2`) proposes seven parts, each labeled as a behavior change, a
removal of provably repeated work, or a design change kept exact by checking it
against the full path. It also records a measurement this plan did not have:
each key resends six controls whose content did not change, about 15.8 KB of
16.2 KB on the wire.

Parts 1 to 4 and 7 were approved on 2026-10-14 and are done, on Akashic
`feature/keystroke-speed`. Measured with Akashic's typing tool
(`physical_typing_cadence.py --screen-profile`), a burst frame fell from 25.0
to 22.8 million guest steps, and from 17.6 KB to 0.7 KB on the wire. Part 7
also stops zero-filling a glyph slot for every cell of the screen on each key,
which the simulator does not count as steps. Parts 5 and 6 need their own
approval; the design estimates about 11 million steps a frame after both.

## Decisions

Reviewed 2026-10-13:

1. **Order.** 1a lands and is measured before 1b starts.
2. **Dictionary authority.** The native evaluator owns publication, and the
   dictionary's host-side state moves into native code, as described in 1b.
3. **Step 2.** Rich-terminal host work and native pair operations, as
   described above.

## Step 1a results

Step 1a is four changes. Each has its redundancy argument in its
commit message, a boot fingerprint check and a pass of the simulator suite.

| Change | Commit | Saving per paired run |
| --- | --- | --- |
| No index rebuild after an interpret `IF` that removed no word | `f38e1db7` | about 1.5 s |
| `EVAL-STATUS` read only after tokens that may run guest code | `e0b1cbdd` | about 0.6–1.4 s |
| The guard's return-stack snapshot through the native executor | `b90e5456` | about 0.8–2.3 s |
| The loader's fixed cells without the checked path, and a shorter line scan | `3bcdf27e` | about 1.8–2.4 s |

**Overall.** Four alternating boot compiles of main and of the end of 1a
gave medians of 37.9 s for main and 27.3 s for 1a: about 10 s faster, or
28%. Another project's tests were running on the host, so single runs
varied by several seconds. The paired differences ranged from 6 to 13 s.

**Desktop journey.** The physical Desktop journey passed on the end of 1a,
with Akashic main: 55 acknowledged frames and 67 inputs, in 249 s
including the image build, with a peak aggregate RSS of 529 MiB.

**Equivalence with main.** After the boot compile, these are identical:

- guest steps;
- every definition, with its addresses and IR;
- memory outside the dictionary index;
- the index's set of entries;
- the stack pointers.

Only the index table's bytes differ: colliding names sit in other slots,
because the table is no longer rebuilt newest-first after an interpret `IF`.

**Found while measuring.** The boot compile is not deterministic across
Python hash seeds. Two runs with random seeds differed in guest steps (16.2
million against 10.2 million), in memory and in stack pointers. With a fixed
seed, runs repeat exactly, and every comparison above pins it. The cause is
not yet known. It is an existing defect, to be fixed on its own.

**What remains.** About 27 s, still almost all Python:

- the per-line protocol, meaning the nested guarded executions and the
  prescan;
- per-token parsing and lookup;
- publication.

Step 1b addresses these.

## Step 1 results

Boot compile medians of the `desktop-apt1` image, each from alternating runs
against the change before it, with `PYTHONHASHSEED` pinned:

| Change | Commit | Boot compile |
| --- | --- | --- |
| Before step 1 | `79ac6745` | about 38 s |
| 1a: measured Python redundancy | `3bcdf27e` | 27.3 s |
| 1b slice 1: native dictionary and side index | `cf2c0155` | 26.5 s |
| 1b slice 2: native line walker | `13ea0f10` | 5.8 s |
| 1b slice 3: plans that survive publication | `06b5731a` | 6.0 s, on a busier host |
| 1c: native tile words | `c37f7310` | 4.7 s |
| Plans for overlaid words | `102ff674` | 3.1 s |
| The tile guard once per run | `998dc617` | 2.7 s |
| Evaluator cell words and the `ABORT"` test | `092f2d97` | 2.5 s |

The host was shared with another project's tests, so a row compares only
with the one above it, measured in the same session. Every change kept the
boot fingerprint identical to step 1a's: guest steps, every definition and
its IR, all memory, the side index and the stack pointers.

The physical Desktop journey passed at slice 2 (225 s) and with the
overlaid plans (204 s, against 249 s at 1a).

**What remains of the boot compile.** About half of the 2.5 s is the 9,343
lines the checked-line rule does not admit, mostly the guest words they run,
such as `CMP-CELL:`, `PROVIDED`, `DEFER` and `REQUIRE`. The native walker
takes about 0.4 s for its 195,000 lines. Most native-run stops are now
structural: root returns and `EXECUTE` of a primitive. The most frequent
remaining primitive stops are the dictionary emitters `C,` and `,`.
