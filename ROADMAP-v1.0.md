# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, comprehensive documentation — that feels
complete and cohesive as a v1.0 release.

**Current state (Feb 2025):** BIOS (242 dict entries, 9,379 lines ASM,
~22 KB binary), KDOS v1.1 (247 `:` definitions + 138 variables/constants,
3,158 lines), Emulator (2,516 lines + 474-line quad-core SoC), FPGA RTL
(13 Verilog modules + 8 testbenches), 1,207 tests passing.

Core subsystems — BIOS Forth, KDOS kernel, filesystem, tile engine,
scheduler, pipelines, networking, disk I/O, BIOS FSLOAD auto-boot — are
**functionally complete**.  What remains is **documentation, UX polish,
and release hardening**.

---

## Completed Work

### BIOS v1.0 — ✅ DONE

242 dictionary entries, 9,379 lines ASM, ~22 KB binary.

- ✅ Full subroutine-threaded Forth: arithmetic, logic, stack, memory,
  control flow (IF/ELSE/THEN, BEGIN/UNTIL/WHILE/REPEAT, DO/LOOP/+LOOP,
  LEAVE), string ops, number parsing, dictionary, compilation
- ✅ Disk I/O primitives (DISK@, DISK-SEC!, DISK-DMA!, DISK-N!,
  DISK-READ, DISK-WRITE)
- ✅ **FSLOAD** — reads MP64FS directory, loads a named file from disk,
  EVALUATEs it line by line (solves the boot chicken-and-egg problem)
- ✅ **Auto-boot** — on startup, if disk is present, scans the directory
  for the first Forth-type file and loads it via FSLOAD
- ✅ `."` works in both interpret and compile modes
- ✅ Timer, NIC, tile-engine CSR access words
- ✅ EVALUATE, COMPARE, VALUE/TO, POSTPONE, DOES>, RECURSE, 2>R/2R>/2R@
- ✅ Bus-fault handler, ABORT/ABORT"
- ✅ **Multicore**: COREID, NCORES, IPI-SEND, IPI-STATUS, IPI-ACK, MBOX!,
  MBOX@, SPIN@, SPIN!, WAKE-CORE, CORE-STATUS (11 words)
- ✅ **Extended tile**: TSUMSQ, TMINIDX, TMAXIDX, TWMUL, TMAC, TFMA,
  TDOTACC (7 words)
- ✅ **Performance counters**: PERF-CYCLES, PERF-STALLS, PERF-TILEOPS,
  PERF-EXTMEM, PERF-RESET (5 words)
- ✅ **CRC engine**: CRC-POLY!, CRC-INIT!, CRC-FEED, CRC@, CRC-RESET,
  CRC-FINAL (6 words)
- ✅ **Memory BIST**: BIST-FULL, BIST-QUICK, BIST-STATUS, BIST-FAIL-ADDR,
  BIST-FAIL-DATA (5 words)
- ✅ **Tile self-test**: TILE-TEST, TILE-TEST@, TILE-DETAIL@ (3 words)
- ✅ **Stride/2D**: TSTRIDE-R!, TSTRIDE-R@, TTILE-H!, TTILE-W!, TLOAD2D,
  TSTORE2D (6 words)
- ✅ **FP16/BF16**: FP16-MODE, BF16-MODE (2 words)

### KDOS v1.1 — ✅ DONE (core + multicore)

247 word definitions + 138 variables/constants/creates, 3,158 lines.

16 sections:
- §1 Utility words, §2 Buffer subsystem, §3 Tile-aware buffer ops
- §4 Kernel registry, §5 Sample kernels (12 kernels including kadd,
  knorm, khistogram, kpeak, kconvolve, etc.)
- §6 Pipeline engine, §7 Storage & persistence
- §7.5–7.8 Filesystem (MP64FS), documentation browser, dictionary search
- §8 Scheduler & tasks, §9 Interactive screens (9-tab TUI)
- §10 Data ports (NIC ingestion), §11 Benchmarking
- §12 Dashboard, §13 Help system, §14 Startup
- §15 Pipeline bundles (versioned, declarative config format)
- §8.1 Multicore dispatch (CORE-RUN, CORE-WAIT, BARRIER, LOCK/UNLOCK, P.RUN-PAR)

### Filesystem — ✅ DONE

- ✅ MP64FS: superblock, bitmap, 64-entry directory, data sectors
- ✅ diskutil.py: build_image, build_sample_image, inject/read/delete/list
- ✅ sample.img: KDOS + 10 docs + 5 tutorials + demo-data + demo-bundle (18 files)
- ✅ KDOS words: DIR, CATALOG, CAT, RENAME, FS-FREE, SAVE-BUFFER, LOAD,
  MKFILE, RMFILE, FORMAT, FIND-BY-NAME, FS-LOAD
- ✅ BIOS FSLOAD for disk-only boot

### Emulator & Tools — ✅ DONE

- ✅ megapad64.py: Full CPU emulation (2,516 lines, incl. extended tile, FP16/BF16)
- ✅ system.py: Quad-core SoC — UART, timer, storage, NIC, mailbox IPI, spinlocks (474 lines)
- ✅ asm.py: Two-pass assembler (788 lines), SKIP instruction
- ✅ cli.py: Interactive monitor/debugger (995 lines)
- ✅ diskutil.py: Filesystem tooling (1,038 lines)
- ✅ devices.py: MMIO peripherals including CRC engine (964 lines)

### Test Suite — ✅ 1,207 tests

- TestBIOS: 128, TestBIOSHardening: 12, TestMulticore: 17
- TestKDOS: 229, TestKDOSHardening: 12, TestKDOSFilesystem: 15
- TestKDOSMulticore: 19, TestPipelineBundles: 13
- TestDiskUtil: 19, TestAssemblerBranchRange: 11
- TestNIC: 11, TestSystemMMIO: 3, TestUART: 3, TestStorage: 2,
  TestTimer: 1, TestDeviceBus: 2
- TestExtendedTile: ~670+ (saturating, rounding, FP16/BF16, strided/2D,
  SHUFFLE, PACK, UNPACK, RROT, CRC, BIST, tile self-test, perf counters)
- test_megapad64.py: 23 CPU + tile tests

### FPGA RTL — ✅ DONE (full ISA + extended tile + multicore)

13 Verilog modules in `fpga/rtl/`, 8 testbenches, 72 hardware tests passing.

- ✅ mp64_cpu.v — Full ISA implementation (all 16 instruction families)
- ✅ mp64_soc.v — Quad-core SoC top-level (bus arbiter, MMIO, IPI wiring)
- ✅ mp64_bus.v — Round-robin bus arbiter with per-core QoS
- ✅ mp64_mailbox.v — Inter-core mailbox + spinlocks (CSR + MMIO dual-path)
- ✅ mp64_tile.v — Full tile engine (TALU, TMUL, TRED, TSYS + extended ops,
  saturating, rounding, SHUFFLE, PACK, UNPACK, RROT, VSHR, VSHL, VCLZ,
  LOAD2D, STORE2D)
- ✅ mp64_fp16_alu.v — FP16/BF16 half-precision tile operations
- ✅ mp64_memory.v, mp64_timer.v, mp64_uart.v, mp64_disk.v, mp64_nic.v, mp64_extmem.v
- ✅ Nexys A7-200T target (no post-synthesis resource numbers yet)

### Extended TPU — ✅ IMPLEMENTED

Fully implemented in both emulator and RTL with comprehensive test coverage.
5 feature families:

- ✅ Enhanced tile engine: TMUL/MAC/FMA/DOTACC, tile views (SHUFFLE/PACK/
  UNPACK/RROT), richer reductions (SUMSQ/MINIDX/MAXIDX), extended TALU
  (VSHR/VSHL/VCLZ), saturating, rounding, strided/2D (LOAD2D/STORE2D),
  FP16/bfloat16 with FP32 accumulation
- ✅ Crypto accelerators: AES-256-GCM, SHA-3/SHAKE, CRC32/CRC32C/CRC64
- ✅ Data movement: HW tile DMA, prefetch/write-combine, per-core QoS
- ✅ Reliability: memory BIST (March C−, checkerboard, addr-as-data),
  tile self-test, 5 performance counters
- ☐ Optional scalar FP32 unit (not yet implemented)

---

## Remaining for v1.0

### Phase 1: Documentation (← NEXT)

The biggest gap.  The existing .md files (README, EMULATOR, KDOS) are all
badly outdated — they reference v0.9d, 62 words, 103 tests.  We need a
proper documentation library that serves as both user reference and
development aid.

**1.1  Forth Dialect References (highest priority)**

These prevent having to re-read the source every time:

- [x] **`docs/bios-forth.md`** — Complete BIOS Forth word reference.
  All 242 entries grouped by category (stack, arithmetic, logic, memory,
  control flow, string, I/O, compilation, disk, timer, tile engine,
  NIC, system, extended tile, CRC, BIST, perf counters, FP16/BF16).
- [x] **`docs/kdos-reference.md`** — Complete KDOS word/definition
  reference.  All 247 `: ` definitions + key variables/constants.
  Grouped by section (§1–§15).  Stack effects, usage examples,
  cross-references to BIOS primitives they use.

**1.2  Architecture & System Docs**

- [x] **`docs/isa-reference.md`** — Megapad-64 instruction set.  All 16
  families (~110 mnemonics), encoding format, condition codes, CSRs,
  tile engine registers.  The authoritative ISA document.
- [x] **`docs/architecture.md`** — System overview: CPU, memory map,
  MMIO register layout (UART, timer, storage, NIC, tile engine),
  boot sequence, interrupt model.
- [x] **`docs/filesystem.md`** — MP64FS specification: sector layout,
  directory entry format, file types, bitmap allocation, diskutil.py API.
- [x] **`docs/tile-engine.md`** — Tile engine programming: CSR registers,
  MEX instruction, element widths, reduction operations, buffer/kernel
  integration with KDOS.

**1.3  User-Facing Docs**

- [x] **`docs/getting-started.md`** — Quick-start guide: how to boot,
  run KDOS, use SCREENS, create buffers, run a pipeline.
- [x] **`docs/tools.md`** — CLI monitor, assembler usage, diskutil
  commands, test suite.
- [x] **`README.md`** — Full rewrite with current stats, architecture
  summary, quick-start, links to docs/.

**1.4  In-Disk Documentation**

- [x] Review and update the 10 doc topics and 5 tutorials already in
  sample.img — ensure they reflect current KDOS word names, stack effects,
  and BIOS FSLOAD boot flow.

---

### Phase 2: UX & Interactive Polish

The SCREENS TUI works but is read-only.  The REPL works but has no
creature comforts.  For v1.0 these should feel usable, not just
demonstrable.

**2.1  SCREENS Improvements**

- [x] **Interactive actions** in screens — e.g. on Buffers screen: select
  a buffer to inspect, see its data preview; on Tasks screen: resume/kill
  a task; on Docs screen: select a topic to read inline
- [x] **Auto-refresh** — optional timer-driven redraw (currently
  manual 'r')
- [x] **Screen 8: Storage** — dedicated file browser: DIR listing,
  select file → CAT/LOAD/info
- [x] **Better formatting** — column alignment, proper truncation of long
  names, color consistency

**2.2  REPL UX**

- [ ] **Command history** — store last N lines, recall with up-arrow
  (requires escape sequence parsing in read_line)
- [ ] **Tab completion** — WORDS-LIKE prefix match on partial input
- [x] **Error messages** — show the word that failed + context, not
  just "?"
- [ ] **`.S` stack display** — consider always showing depth in prompt

**2.3  Help & Discoverability**

- [x] **HELP \<word\>** — look up a specific word's stack effect and
  one-line description (requires a built-in word database or doc lookup)
- [ ] **Contextual hints** — when a word errors, suggest related words
- [x] Ensure TOPICS, LESSONS, DESCRIBE all work correctly with
  disk-booted KDOS

---

### Phase 3: Hardening & Edge Cases

**3.1  Robustness**

- [x] FSLOAD error recovery — if a line in a loaded file has an undefined
  word, print error with file/line context instead of silently continuing
- [x] Stack underflow protection in BIOS (currently silently corrupts)
- [x] EVALUATE nested depth limit (prevent unbounded RSP growth)
- [x] Graceful handling of full dictionary (HERE approaching stack)

**3.2  Missing Tests**

- [x] BIOS FSLOAD with multi-sector files (> 512 bytes)
- [x] BIOS FSLOAD with files containing `: ` definitions, `."` strings,
  nested `EVALUATE`
- [x] End-to-end disk-only boot test (no `--forth`, just `--storage`)
  that verifies KDOS words work after loading
- [x] SCREENS TUI test (render each screen, verify output contains
  expected sections)
- [x] Edge cases: empty file FSLOAD, file with only comments, 255-char
  line

**3.3  Emulator Polish (nice-to-have)**

- [x] Configurable RAM size (`--ram` flag, CSR reports actual size)
- [x] Assembler listing output (`-l` flag)
- [x] Assembler size report after assembly

---

### Phase 4: Release

- [ ] Final `README.md` with correct stats, architecture diagram,
  quick-start, links to all docs
- [ ] All docs written and proofread
- [ ] Full test run — target 1,207+ tests, all green
- [ ] `sample.img` rebuilt with updated in-disk docs
- [ ] Git tag `v1.0`

---

## Implementation Order

```
Phase 1.1  Forth dialect docs (bios-forth.md, kdos-reference.md) ← NEXT
Phase 1.2  Architecture docs (isa, architecture, filesystem, tile-engine)
Phase 1.3  User docs (getting-started, tools, README rewrite)
Phase 1.4  In-disk doc review
Phase 2.1  SCREENS interactive actions
Phase 2.2  REPL UX (history, completion, better errors)
Phase 2.3  Help & discoverability
Phase 3.1  Robustness fixes
Phase 3.2  Missing tests
Phase 4    Release tag
```

Phases 2 and 3 can be interleaved.  Phase 1 is the gating factor —
without docs, every subsequent change requires re-reading source.

---

## File Summary

| File | Lines | Status |
|------|-------|--------|
| `bios.asm` | 9,379 | ✅ Done (242 words, ~22 KB) |
| `kdos.f` | 3,158 | ✅ Done (247 defs + 138 vars, multicore) |
| `megapad64.py` | 2,516 | ✅ Done (incl. extended tile, FP16/BF16) |
| `system.py` | 474 | ✅ Done (quad-core SoC) |
| `cli.py` | 995 | ✅ Done |
| `asm.py` | 788 | ✅ Done (listing support) |
| `devices.py` | 964 | ✅ Done (+ Mailbox, Spinlock, CRC) |
| `diskutil.py` | 1,038 | ✅ Done |
| `test_megapad64.py` | 2,193 | 23 tests ✅ |
| `test_system.py` | 6,234 | 1,184 tests ✅ |
| `sample.img` | — | Built by diskutil.py ✅ |
| `fpga/rtl/` | 7,242 | ✅ 13 Verilog modules |
| `fpga/sim/` | 3,930 | ✅ 8 testbenches (72 HW tests) |
| `docs/` | 9 files | ✅ Written |
| `README.md` | 340 | ✅ Rewritten |
