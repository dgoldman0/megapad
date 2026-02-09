# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, comprehensive documentation — that feels
complete and cohesive as a v1.0 release.

**Current state (Feb 2025):** BIOS (197 dict entries, 8,187 lines ASM,
19.7 KB binary), KDOS (217 `: ` definitions + 86 variables/constants,
2,519 lines), Emulator (1,358 lines), 619 tests passing.

Core subsystems — BIOS Forth, KDOS kernel, filesystem, tile engine,
scheduler, pipelines, networking, disk I/O, BIOS FSLOAD auto-boot — are
**functionally complete**.  What remains is **documentation, UX polish,
and release hardening**.

---

## Completed Work

### BIOS v1.0 — ✅ DONE

197 dictionary entries, 8,187 lines ASM, 19.7 KB binary.

- ✅ Full subroutine-threaded Forth: arithmetic, logic, stack, memory,
  control flow (IF/ELSE/THEN, BEGIN/UNTIL/WHILE/REPEAT, DO/LOOP/+LOOP,
  LEAVE), string ops, number parsing, dictionary, compilation
- ✅ Disk I/O primitives (DISK@, DISK-SEC!, DISK-DMA!, DISK-N!,
  DISK-READ, DISK-WRITE)
- ✅ **FSLOAD** — reads MP64FS directory, loads a named file from disk,
  EVALUATEs it line by line (solves the boot chicken-and-egg problem)
- ✅ **Auto-boot** — on startup, if disk is present, runs
  `FSLOAD autoexec.f` which bootstraps KDOS from disk
- ✅ `."` works in both interpret and compile modes
- ✅ Timer, NIC, tile-engine CSR access words
- ✅ EVALUATE, COMPARE, VALUE/TO, POSTPONE, DOES>, RECURSE, 2>R/2R>/2R@
- ✅ Bus-fault handler, ABORT/ABORT"

### KDOS v1.0 — ✅ DONE (core)

217 word definitions + 86 variables/constants/creates, 2,519 lines.

14 sections:
- §1 Utility words, §2 Buffer subsystem, §3 Tile-aware buffer ops
- §4 Kernel registry, §5 Sample kernels (12 kernels including kadd,
  knorm, khistogram, kpeak, kconvolve, etc.)
- §6 Pipeline engine, §7 Storage & persistence
- §7.5–7.8 Filesystem (MP64FS), documentation browser, dictionary search
- §8 Scheduler & tasks, §9 Interactive screens (7-tab TUI)
- §10 Data ports (NIC ingestion), §11 Benchmarking
- §12 Dashboard, §13 Help system, §14 Startup

### Filesystem — ✅ DONE

- ✅ MP64FS: superblock, bitmap, 64-entry directory, data sectors
- ✅ diskutil.py: build_image, build_sample_image, inject/read/delete/list
- ✅ sample.img: KDOS + autoexec.f + 10 docs + 5 tutorials + demo-data
  (18 files)
- ✅ KDOS words: DIR, CATALOG, CAT, RENAME, FS-FREE, SAVE-BUFFER, LOAD,
  MKFILE, RMFILE, FORMAT, FIND-BY-NAME, FS-LOAD
- ✅ BIOS FSLOAD for disk-only boot

### Emulator & Tools — ✅ DONE

- ✅ megapad64.py: Full CPU emulation (1,358 lines)
- ✅ system.py: System integration — UART, timer, storage, NIC, tile engine
- ✅ asm.py: Two-pass assembler (677 lines), SKIP instruction
- ✅ cli.py: Interactive monitor/debugger (990 lines)
- ✅ diskutil.py: Filesystem tooling (941 lines)

### Test Suite — ✅ 619 tests

- TestBIOS: 128, TestKDOS: 212, TestKDOSFilesystem: 15
- TestDiskUtil: 19, TestAssemblerBranchRange: 11
- TestNIC: 11, TestSystemMMIO: 3, TestUART: 3, TestStorage: 2,
  TestTimer: 1, TestDeviceBus: 2

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
  All 197 entries grouped by category (stack, arithmetic, logic, memory,
  control flow, string, I/O, compilation, disk, timer, tile engine,
  NIC, system).  Stack effects, brief description, any quirks.
- [x] **`docs/kdos-reference.md`** — Complete KDOS word/definition
  reference.  All 217 `: ` definitions + key variables/constants.
  Grouped by section (§1–§14).  Stack effects, usage examples,
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
- [ ] **`README.md`** — Full rewrite with current stats, architecture
  summary, quick-start, links to docs/.

**1.4  In-Disk Documentation**

- [ ] Review and update the 10 doc topics and 5 tutorials already in
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

- [ ] FSLOAD error recovery — if a line in a loaded file has an undefined
  word, print error with file/line context instead of silently continuing
- [ ] Stack underflow protection in BIOS (currently silently corrupts)
- [ ] EVALUATE nested depth limit (prevent unbounded RSP growth)
- [ ] Graceful handling of full dictionary (HERE approaching stack)

**3.2  Missing Tests**

- [ ] BIOS FSLOAD with multi-sector files (> 512 bytes)
- [ ] BIOS FSLOAD with files containing `: ` definitions, `."` strings,
  nested `EVALUATE`
- [ ] End-to-end disk-only boot test (no `--forth`, just `--storage`)
  that verifies KDOS words work after loading
- [ ] SCREENS TUI test (render each screen, verify output contains
  expected sections)
- [ ] Edge cases: empty file FSLOAD, file with only comments, 255-char
  line

**3.3  Emulator Polish (nice-to-have)**

- [ ] Configurable RAM size (`--ram` flag, CSR reports actual size)
- [ ] Assembler listing output (`-l` flag)
- [ ] Assembler size report after assembly

---

### Phase 4: Release

- [ ] Final `README.md` with correct stats, architecture diagram,
  quick-start, links to all docs
- [ ] All docs written and proofread
- [ ] Full test run — target ~650+ tests, all green
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
| `bios.asm` | 8,187 | ✅ Done (197 words, 19.7 KB) |
| `kdos.f` | 2,519 | ✅ Done (217 defs + 86 vars) |
| `megapad64.py` | 1,358 | ✅ Done |
| `system.py` | 300 | ✅ Done |
| `cli.py` | 990 | ✅ Done |
| `asm.py` | 677 | ✅ Done |
| `diskutil.py` | 941 | ✅ Done |
| `test_system.py` | 4,693 | 619 tests ✅ |
| `sample.img` | — | Built by diskutil.py ✅ |
| `docs/` | — | **Needs full write-up** |
| `README.md` | 389 | **Needs rewrite** (outdated) |
| `EMULATOR.md` | 465 | **Needs rewrite** (says v0.9, 62 words) |
| `KDOS.md` | 1,191 | **Needs rewrite** (says v0.9d) |
