# Module loading, storage units and the evaluator — 2026-11-11

On 2026-11-10 and 2026-11-11 MegaPad gained what loading modules from an ext4
system volume asks of it: a module source under KDOS's loader, and storage
units for the system and user volumes. The work exposed faults in the
exception words, the evaluator, the shared disk port and the hosted
simulator's memory, and each was repaired. This note records every change,
the commit that made it, what it passed, and what was left as it is.

Akashic's side of the same work, the VFS module source, the system volume's
mount and the Desktop's boot from that volume with its measured cost, is in
Akashic's `local_testing/evidence/system-volume-boot-20261110.md`.

Every test and journey named here ran one process at a time. A physical
Desktop journey is Akashic's `local_testing/physical_desktop_acceptance.py`
on the native simulator, under its 900-second watchdog and 3.5 GiB guard. A
test unit is a test file run by `python3 -m fantasy_engine.testing`.

## The loader's module source (d8a92092)

`LOAD` and `REQUIRE` find and read a file through the current module source,
a record of four operations: resolve a path, read the file, and take and give
back the source's current directory. KDOS's MP64FS is the source selected at
boot, `MODULE-SOURCE!` selects another, and
[`module-source-contract.md`](module-source-contract.md) is the interface.
The module registry keeps the identity of each file whose module is loaded,
so a later `REQUIRE` of that file ends once its path has resolved, on MP64FS
as well. Before this, `REQUIRE` read only KDOS's own MP64FS, and it read a
whole file before the file's `PROVIDED` line could show that the module was
already loaded.

Akashic's `utils/fs/vfs-module-source.f` (Akashic fc8b0fb2) makes a mounted
VFS such a source. Its tests run in the native simulator, which loads the
whole ext4 driver in 0.6 s and mounts the volume in 0.3 s; the emulator takes
about 1.2 billion steps to load the driver. The simulator has since gained
`:NONAME` itself (488b6b23), with anonymous bodies run by its reference
interpreter.

At d8a92092 and Akashic fc8b0fb2 the physical Desktop journey passed, and so
did the 214 test units the changes reach.

## An uncaught THROW at the prompt (1f4030a7)

KDOS used to go silent after an uncaught `THROW` at the prompt, which left
the machine idle while a test's run loop went on stepping it toward its
budget. It now prints `uncaught THROW` with the code and returns to the
prompt.

## Three system tests that had failed since October (e42b0d1c)

Three of the system tests had failed on main since October, and none showed a
loader defect. Two caught a failed load's `THROW` across a plain `EVALUATE`
with a `CATCH` of their own. The BIOS keeps that `EVALUATE`'s input frame
outside the return stack, so the evaluator stayed one level deep with a
failure recorded, and at a nonzero depth with a failure recorded it runs no
`EVALUATE`: the repeated load evaluated nothing and threw the old code again.
Their catching words were made to unwind the evaluator, as `EVALUATE-CHECKED`
does. The third expected a finished nested dependency to survive its parent's
failure; it now expects the loader's rule, that the dependency is rolled back
with the parent, and `README.md` states that rule.

The whole system test then passed class by class, 2,236 tests in 97 classes,
each class in a process of its own. One class of network tests reaches
3.9 GiB and ran in two parts; the whole file in one process peaks near
6.7 GiB.

## Two exception hazards (2e5d8c99 and 7cd045ed)

Those tests exposed two hazards, and both are closed in KDOS (decided
2026-11-10). An `ABORT` taken inside a `CATCH` used to leave that frame's
address in `HANDLER`, since the BIOS reset the stacks without telling KDOS,
and a later `THROW` unwound into the dead frame. BIOS `QUIT`, where `ABORT`
and `ABORT"` end, now runs a callback once it has reset the return stack
(`QUIT-XT!`), and KDOS installs one that clears its exception chain.

A `THROW` that crossed plain `EVALUATE` used to leave the evaluator one level
deep, which disabled every later `EVALUATE` until an `ABORT`. KDOS now
shadows `EVALUATE` with a word that runs the BIOS word under `CATCH`, gives
the evaluator its depth back when an exception passes, and rethrows;
`EVALUATE-CHECKED` runs through the same word. `CATCH` and `THROW` are
unchanged: each owner gives back its own state as an exception passes, as the
loader and `WITH-GUARD` do.

The system test passed class by class, 2,239 tests, and the simulator suite
on both executors, at 7cd045ed. On the merged state (8706dc99, Akashic
fc8b0fb2) the physical Desktop journey passed, and so did 280 test units and
Akashic's ext4 sample of 65 tests.

## The status a failed load left set (d63c5f87)

One case of the second hazard was left, and it is closed as well (decided
2026-11-11). When a line of a file fails, the evaluator records the failure
in its status cell, and while that cell is set no enclosing line continues
and no nested `EVALUATE` runs. The loader turned the status into a `THROW`
and left the cell set. A word that caught a failed load and carried on
therefore worked at the prompt, where the next line starts with a clear
status, but not while a file that called it was still loading: the rest of
its line was skipped and its own file failed with the code it had caught.
Desk started from a boot script runs in that position for its whole life.

The loader's failure path now clears the status as it rethrows, as it already
gave back the evaluator's depth and compiler state. After a failed load a
program reads the failure from the code it caught. `EVALUATE-CHECKED` and
`SOURCE-EVALUATE-CHECKED`, which hand the status to their caller as a value,
keep their rule. The branch passed the emulator test files the change
reaches, every class of the system test and the simulator suite on both
executors, 9,363 tests.

## Storage units for the system and user volumes (f69a52f8, b949a08c, 54b49bf6)

Each volume of a booted system has a storage unit of its own (decided
2026-11-10). The boot volume stays on unit 0, where the BIOS reads it, and
the settings volume on unit 1; two more units hold the system and user
volumes.

The machine now has four storage controllers of one design: unit 0 the boot
disk at MMIO +0x0200, unit 1 the settings disk at +0x0280, unit 2 the system
disk at +0x0D00 and unit 3 the user disk at +0x0D80. The settings and system
disks' media are write-protected. One table in `shared/storage.py` defines
the units for the emulator, the simulator and the host's options
(`--system-storage`, `--user-storage`); the BIOS's unit words and KDOS's
`BOOT-UNIT`, `SETTINGS-UNIT`, `SYSTEM-UNIT` and `USER-UNIT` name the same
four; and the RTL generates one controller per unit behind an arbiter that
takes any number of masters. "System" now names unit 2 only: unit 0 is the
boot disk in code and documents, and KDOS's raw singletons for it are
`BOOT-BD` and `BOOT-RAW-VOLUME`.

Building the units exposed a fault the settings disk had carried since
e5b4ae34. With two controllers active at once under strict cycles a run
stopped, because each controller numbers its own beats and the shared port
passed those numbers on; the port now numbers the beats it offers.

Before the merge the units and the rename passed the emulator test files the
change reaches, every class of the system test and the simulator suite on
both executors, 8,884 tests, and the RTL benches under iverilog. On the
merged state (c723d121, Akashic 14f39dec) the physical Desktop journey
passed, and so did 275 test units and Akashic's ext4 sample of 65 tests.

Two questions are left for the hardware. The RTL gives each unit an SD slot
of its own, as the settings disk has, where a device may instead map units
onto partitions of one medium inside the storage controller. And the emulator
grants the shared port in unit order where the RTL rotates the grant.

## A machine of the native engine is freed (c723d121)

A machine of the simulator's native engine was never freed. The extension's
classes own Python references that lead back to the machine, and Python's
collector could not see through them, so every machine a process made stayed
with its memory. `tests/simulator/test_native_direct_path.py` makes 2,880
machines and passed 4 GiB in a process of its own, and the whole simulator
suite in one process reached 7 GiB.

`NativeProgram`, `NativeDictionary` and `NativeLineWalker` now take part in
cycle collection. That test file peaks at 89 MiB, and the largest process of
the simulator suite, run in eight groups, at 126 MiB where it was 978 MiB.
The extension has to be rebuilt wherever this commit is pulled: a library
built before it still loads and still keeps every machine.

## Left as they are

The BIOS does not end a background task that aborts: the prompt returns on
the foreground's stack and `TASK-ID` still names the task. And `APP-EVAL`
leaves its memory-protection window set when a `THROW` passes it, which
matters only once that window is enforced.
