# The loader's cost per line — 2026-11-11

Loading source cost about 15,000 instructions for every line, whether the
line held code or a comment. A boot of Akashic's Desktop evaluates more than
six megabytes of source a line at a time, so nearly all of the boot was that
cost. This note records where the instructions went, the three changes that
removed most of them (6370590a, 0f8e2302 and 00074ef7), and the boot measured
before and after.

## Where a line's cost went

The guest's program counter was sampled every 4,099 instructions while the
emulator booted `desktop-apt1`, and each sample was put to the BIOS label or
the Forth word whose code held it. The run before the changes sampled the
first 600 million instructions of the module phase at c723d121; the runs
after them sampled the whole phase at 00074ef7.

| Share of the module phase | Linked, before | Linked, after | System volume, after |
|---|---|---|---|
| saving, restoring and copying input | 32.1% | 5.1% | 4.8% |
| finding the line's end | in the three rows below | 5.2% | 6.2% |
| KDOS's per-line words | 12.3% | 7.6% | 7.4% |
| word calls (NEXT, EXIT) | 16.3% | 13.0% | 18.7% |
| other primitives run by Forth words | 19.9% | 12.2% | 14.5% |
| parsing, lookup, numbers, compiling | 15.4% | 47.2% | 37.4% |
| checksums of the source | 3.0% | 8.5% | 2.7% |
| ext4, VFS, disk | 0 | 0.1% | 5.6% |

Two things took about two thirds of the phase and compiled nothing. Every
`EVALUATE` copied all 256 bytes of the input buffer out and back, so that a
nested evaluation could not disturb its caller's line. And each of KDOS's
three line walkers found the end of its line in a loop of Forth words that
ran once for every byte.

## The changes

- **6370590a** saves and restores only the caller's live input around a
  nested `EVALUATE`: its active bytes and the one after them, a cell at a
  time. Every reader of the input buffer stops at the active length. One did
  not: `S"` inside a definition scanned on to a quote or a NUL, and with no
  closing quote it compiled bytes of an earlier line. It now stops at the
  line's end. The line itself is copied into the buffer by cells as well.
- **0f8e2302** returns at once from `EVALUATE` for a line that is empty, all
  spaces, or whose first token starts with a backslash. The return comes
  after the status clear, the nested-failure gate, the depth check and the
  length check, and only when no `[IF]` skip is pending, so such a line's
  statuses and errors are what they were.
- **00074ef7** adds `SCAN ( c-addr u char -- c-addr' u' )` to the BIOS and
  states what a line is once in KDOS, in `_LINE-LEN` and `_LINE-TEXT`, which
  `SOURCE-EVALUATE-CHECKED`, the loader and the module prescan now share.
  The hosted simulator has `SCAN` as a core word, and its checked-source and
  loader-walk overlays bind the two line words.

## The boot before and after

Emulator, `desktop-apt1`, from reset to the boot line that says the guest is
entering Desk, in millions of instructions read between boot lines at a
resolution of two million. Before is c723d121; after is 00074ef7 with Akashic
9d1ae0b3. The system-volume column is the Desktop built with Akashic's
`--system-volume`, its modules loaded as plain files from an ext4 volume.

| Linked chunks | Before | After |
|---|---|---|
| BIOS and KDOS | 56 | 26 |
| networking, rich terminal, cold-source loader | 294 | 86 |
| the Desktop's modules, 52 chunks | 2,642 | 906 |
| the rest | under 2 | 2 |
| total | 2,992 | 1,020 |
| system cycles | 3,716 | 1,324 |

| System volume, 4 KiB blocks | Before | After |
|---|---|---|
| BIOS and KDOS | 56 | 26 |
| networking, rich terminal, cold-source loader | 292 | 86 |
| the volume's driver | 404 | 136 |
| the mount | 12 | 12 |
| the Desktop's modules, 309 files | 3,558 | 1,170 |
| the rest | 2 | 2 |
| total | 4,324 | 1,432 |
| system cycles | 5,388 | 1,852 |

At the SoC's 100 MHz the linked boot goes from about 37 seconds to about 13,
and the boot from the system volume from about 54 to about 19. The three
changes as first made gave 1,078 M and 1,508 M; copying the line into the
buffer by cells took the rest.

In the native simulator the boots take the host time they took, 2.6 to 2.8 s
linked and 10.1 to 10.8 s from the system volume on either tree: its overlays
already stood in for these loops.

## What is left

Parsing, lookup and compiling are now the largest share, about 430 million
instructions of the module phase in either boot. Around each line there
remain KDOS's own per-line words with their calls, roughly a quarter of the
phase; `SCAN`, which still steps a byte at a time, at 5%; and the checksums
of the source chunks, at 8.5% of the linked boot.

## What it passed

One process at a time, on the final tree: eight emulator test files (289
tests), the 99 classes of `tests/test_system.py` (2,260), and the simulator
suite on each executor (3,433 each), 9,415 tests with no failure and no
process above 3.35 GiB. Thirty-eight faults planted one at a time each failed
a test. At 00074ef7 and Akashic 9d1ae0b3 the physical Desktop journey passed
linked and with `--system-volume`.

The dictionary index test's boot table now holds 767 names, one short of the
768 at which it doubles. The next name added to the BIOS or to KDOS's prefix
makes that table grow during the test's setup, and the test will need a new
starting point.
