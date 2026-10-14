# Simulator speed plan: boot compile, typing host work, guest work

Status: agreed 2026-10-13 (see Decisions). Step 1a is done (see Step 1a
results). Step 1b is designed and in progress.

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
A profile of the simulator thread during typing alone says where its Python
time goes:

| Share | Work |
| --- | --- |
| About a third | building full-screen text snapshots, about 30 per keystroke, each joining 23,520 cells |
| About a quarter | rich-terminal host processing and enum flag operations |
| About a sixth | guest fallbacks |
| The rest | native entry overhead |

**Method.** `cProfile` distorts the live session too much. Boot ran four
times slower, and the rich terminal's negotiation failed on its real-time
deadlines. So the live figures come from two sources: the executor's own
counters (`MEGAFORTH_NATIVE_PROFILE=1`: `native_run_ns`, `settlement_ns` and
the exit counts), and per-thread CPU clocks. `cProfile` was used only for
the boot compile and for the simulator thread during typing. Unprivileged
`perf` is disabled on the development host.

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
whole line. The work lands in four slices: the dictionary, the walker, plans
that survive publication, and a wider checked-line rule.

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

## Step 2: typing host work

- **Screen text snapshots.** Stop building full-screen text snapshots on the
  simulator thread where nothing reads them, or build them only for the rows
  that changed.
- **Rich-terminal host processing.** Trim it, starting with the enum flag
  operations on its hot path.
- **Native pair operations.** Give the native executor `2>R`, `2R>` and `2R@`.
  Its existing return-stack operations already provide the building blocks.

Together these address most of the 0.11 s of simulator-thread Python per
keystroke. Typing would go from about 0.57 s to perhaps 0.45 s per
keystroke; the rest is guest work.

This replaces the earlier idea of making the tile words and the CRC native
for typing. The per-phase counts show that the tile words run at boot, and
that the CRC costs about 2 ms per keystroke.

## Step 3: guest work per keystroke

A keystroke costs about 29 million semantic steps. The guest phase profiler
(`akashic_tui.py accept --phase-profile`) can show where they go, and the
work then is in Akashic. This is the only step that also makes the device
faster.

## Decisions

Reviewed 2026-10-13:

1. **Order.** 1a lands and is measured before 1b starts.
2. **Dictionary authority.** The native evaluator owns publication, and the
   dictionary's host-side state moves into native code, as described in 1b.
3. **Step 2.** Host snapshot and rich-terminal work plus native pair
   operations, as described above.

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
