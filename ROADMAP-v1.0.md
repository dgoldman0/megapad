# ROADMAP to v1.0

**Goal:** A fully functioning emulator, v1 BIOS, v1 OS (KDOS), a complete
working compiled BIOS binary, and a sample virtual disk with the OS installed
and files loaded.

**Current state:** BIOS v1.0 (196 words, 7,774 lines ASM, 18.9 KB binary),
KDOS v1.0 (2,407 lines, 213 definitions), Emulator (1,358 lines),
385 tests passing. ✅ BIOS and KDOS at v1.0.

---

## 1  BIOS v1.0 — ✅ COMPLETE

**196 words**, 7,774 lines ASM, 18.9 KB binary.

All Priority-A and Priority-B words implemented:
- ✅ ABORT, ABORT", LEAVE, 2OVER, EVALUATE, FIND, SOURCE, >IN, >NUMBER, QUIT
- ✅ 2SWAP, 2ROT, 2>R, 2R>, 2R@, MOVE, RECURSE, DOES>, COUNT, COMPARE
- ✅ U<, U>, 2/, VALUE, TO, WITHIN, CHAR, [CHAR], POSTPONE
- ✅ W@, W!, L@, L!, UCHAR, .ZSTR, >=, <=, OFF, TALIGN (promoted from KDOS)
- ✅ SKIP instruction in assembler + BIOS
- ✅ LEAVE/LOOP/+LOOP: var_leave_fixups[8] array (replaced broken data-stack approach)
- ✅ ABORT": LBR over inline string data (fixed code-as-data execution bug)
- ✅ STATE reset on undefined word

---

## 2  KDOS v1.0 — ✅ COMPLETE

**213 definitions**, 2,407 lines.

- ✅ Removed 11 BIOS-duplicate definitions (OFF, >=, <=, TALIGN, etc.)
- ✅ Rewrote SAMESTR? using COMPARE
- ✅ Rewrote 8 LEAVE workaround patterns → native LEAVE
- ✅ NEEDS/ASSERT use ABORT/ABORT" for real error recovery
- ✅ LOAD (read file from MP64FS, EVALUATE line-by-line)
- ✅ AUTOEXEC (boot-time auto-load of autoexec.f)
- ✅ FIND-BY-NAME helper
- ✅ Version strings v0.9d → v1.0
- ✅ Forward reference fixes (ANSI, ports)
- ✅ MKFILE/RMFILE comment bugs fixed

---

## 3  Emulator — Mostly Complete

### 3.1  Done

- ✅ SKIP instruction implemented (with `_next_instruction_size` helper)
- ✅ All device models: disk, timer, network, tile engine
- ✅ 1,358 lines, fully functional

### 3.2  Remaining (nice-to-have, not blocking v1.0)

- [ ] **Configurable memory size**: Accept `--ram` in MiB (8/16/32/64).
  Update `CSR_MEGAPAD_SZ` to report actual size. Currently fixed at 1 MiB.
- [ ] Pipeline-aware cycle counting
- [ ] Bank-level memory access tracking

---

## 4  Assembler (asm.py) — Mostly Complete

677 lines. Assembles bios.asm → 18.9 KB binary.

- ✅ SKIP opcode support
- ✅ `\xNN` hex escape support in string literals
- [ ] **Listing output**: Optional `-l` flag for address + hex + source
- [ ] **Size report**: Print final binary size after assembly
- [ ] **Better errors**: Show source line on assembly errors

---

## 5  Filesystem & Disk — NEXT UP

### 5.1  Current filesystem layout (MP64FS)

| Sectors | Content |
|---------|---------|
| 0 | Superblock (magic `MP64`, version 1) |
| 1 | Allocation bitmap (2048 bits = 256 bytes covers 1 MiB disk) |
| 2–5 | Directory (4 sectors × 16 entries = **64 files max**) |
| 6+ | Data area |

Directory entry: 32 bytes — 16-byte name, start sector (u16), sector count
(u16), used bytes (u32), type (u8), flags (u8), 4 bytes reserved.

File types: 0=free, 1=raw, 2=text, 3=forth, 4=doc, 5=data, 6=tutorial.

**Current disk content:** ~15 files (10 docs + 5 tutorials), well within
64-file limit. KDOS source loaded via serial, not from disk.

### 5.2  The 64 vs 256 file question

**64 files is fine for v1.0.** Here's why:

- Current usage: ~15 files. Even with KDOS on disk + autoexec + user files,
  reaching 30 would be ambitious.
- 64 files keeps the directory cache at 2 KB (fits easily in RAM).
- The disk is 1 MiB — storage is the bottleneck, not file count.
- Expanding later is straightforward (bump superblock version, widen dir).

**When 256+ would matter:**
- Package/library system where each module is a separate file
- Project workspaces with many source files
- A `/lib/` or `/pkg/` convention with dozens of utility packages

**Decision: Keep 64 files for v1.0.** Revisit in v1.1 if a package system
is added. The superblock version field makes future expansion backward-
compatible.

### 5.3  What to build now

**Phase A — Build infrastructure (diskutil.py → sample.img):**

1. [ ] Update `diskutil.py` build_image to inject KDOS source as a file
2. [ ] Create `autoexec.f` that does `S" kdos.f" LOAD`
3. [ ] Build `sample.img` with full content (KDOS + docs + tutorials + autoexec)
4. [ ] Verify end-to-end boot: `cli.py --bios bios.asm --storage sample.img`

**Phase B — Filesystem robustness (kdos.f):**

5. [ ] Add `FCAT` / `CAT` — print file contents to terminal
6. [ ] Add `SAVE-BUFFER` ( buf "name" -- ) — persist a buffer to a named file
7. [ ] Add `FWRITE-NEW` / `WRITEFILE` — create file + write data in one step
8. [ ] Improve `DIR` output — show used bytes, type name, percentage full
9. [ ] Add `FS-FREE` — report free sectors/bytes
10. [ ] Add `RENAME` ( "old" "new" -- ) — rename a file

**Phase C — File type support:**

11. [ ] Define file type conventions:
    - Type 3 (forth): Loadable source — `LOAD` already works
    - Type 2 (text): Human-readable, viewable with `CAT`
    - Type 5 (data): Binary blobs, used by kernels/pipelines
    - Type 4 (doc): Browsable with DOC system (already works)
    - Type 6 (tutorial): Browsable with TUTORIAL system (already works)
12. [ ] Add `FTYPE!` ( type "name" -- ) — change file type
13. [ ] `LOAD` should verify file is type 3 (forth) before evaluating

### 5.4  Sample disk content for v1.0

| File | Type | Description |
|------|------|-------------|
| `autoexec.f` | forth (3) | `S" kdos.f" LOAD` + optional `SCREENS` |
| `kdos.f` | forth (3) | Complete KDOS OS source (~2,407 lines) |
| 10 doc pages | doc (4) | getting-started, buffers, kernels, etc. |
| 5 tutorials | tutorial (6) | hello-world, first-kernel, etc. |
| `demo-data` | data (5) | Sample 256-byte dataset for tutorials |

Total: **18 files**, 28% of 64-file limit.

### 5.5  End-to-end boot

```
python3 cli.py --bios bios.asm --storage sample.img
```

Boot sequence: BIOS starts → finds disk → runs AUTOEXEC → loads
`autoexec.f` → EVALUATE runs `S" kdos.f" LOAD` → KDOS loads from disk
→ KDOS banner appears → REPL ready (or SCREENS TUI).

---

## 6  Testing — 385 and counting

### 6.1  Current: 385 tests ✅

All passing. Cover BIOS words (196), KDOS subsystems, filesystem,
scheduler, networking, tile engine, and pipelines.

### 6.2  New tests for remaining work

| Area | Tests needed |
|------|--------------|
| Sample disk build | Build sample.img, verify file list |
| End-to-end boot from disk | Boot with autoexec.f, verify KDOS loads |
| CAT / FCAT | Print file contents |
| SAVE-BUFFER | Round-trip write + read |
| FS-FREE | Free space reporting |
| RENAME | File rename |
| DIR formatting | Enhanced directory listing |

Target: **~400 tests** after filesystem phase.

---

## 7  Documentation

- [ ] **README.md**: Update BIOS word count (196), binary size (18.9 KB),
  KDOS version (v1.0), test count (385).
- [ ] Document end-to-end boot with sample.img
- [ ] Update in-disk doc pages (storage, getting-started, reference)

---

## 8  Implementation Order (remaining)

### Phase 4: Filesystem & Disk (← NEXT)

1. Build sample.img with KDOS + autoexec + docs + tutorials
2. Test end-to-end boot from disk
3. Add CAT, SAVE-BUFFER, FS-FREE, RENAME to KDOS
4. Tests for all new filesystem words

### Phase 5: Polish

5. Update README.md with final stats
6. Update in-disk documentation
7. Final test run (target ~400 tests)
8. Git tag v1.0

---

## 9  File Summary

| File | Current | Remaining work |
|------|---------|----------------|
| `bios.asm` | 7,774 lines, 196 words, 18.9 KB | ✅ Done |
| `asm.py` | 677 lines | ✅ Done (listing/size report nice-to-have) |
| `megapad64.py` | 1,358 lines | ✅ Done (configurable RAM nice-to-have) |
| `system.py` | 300 lines | — |
| `cli.py` | 990 lines | — |
| `kdos.f` | 2,407 lines, 213 words | CAT, SAVE-BUFFER, FS-FREE, RENAME |
| `diskutil.py` | 914 lines | Build script for sample.img |
| `test_system.py` | 4,427 lines, 385 tests | ~15 more tests |
| `sample.img` | (not yet built) | Build with full content |
| `README.md` | Needs update | Final stats |
