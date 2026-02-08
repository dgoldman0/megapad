# ROADMAP to v1.0

**Goal:** A fully functioning emulator, v1 BIOS, v1 OS (KDOS), a complete
working compiled BIOS binary, and a sample virtual disk with the OS installed
and files loaded.

Current state: BIOS v0.5 (157 words, 13 KB binary), KDOS v0.9d (2,393 lines,
215 definitions), Emulator (1,305 lines), 327 tests passing.

---

## 1  BIOS v0.5 → v1.0

### 1.1  Current BIOS word inventory (157 words)

| Category          | Words |
|-------------------|-------|
| **Stack**         | DUP DROP SWAP OVER ROT NIP TUCK 2DUP 2DROP DEPTH PICK ?DUP |
| **Return stack**  | >R R> R@ J UNLOOP |
| **Arithmetic**    | + - * / MOD /MOD NEGATE ABS MIN MAX 1+ 1- 2* |
| **Logic/Bitwise** | AND OR XOR INVERT LSHIFT RSHIFT |
| **Comparison**    | = <> < > 0= 0< 0> 0<> |
| **Memory**        | @ ! C@ C! +! HERE ALLOT , C, CELLS CELL+ FILL CMOVE |
| **I/O**           | EMIT KEY KEY? CR . U. .S TYPE SPACE SPACES ." S" ACCEPT |
| **Control flow**  | IF ELSE THEN BEGIN UNTIL WHILE REPEAT DO LOOP +LOOP I J AGAIN EXIT |
| **Compilation**   | : ; VARIABLE CONSTANT CREATE STATE [ ] LITERAL IMMEDIATE ' EXECUTE |
| **Parsing**       | WORD BL WORDS LATEST |
| **Number base**   | BASE HEX DECIMAL |
| **Misc**          | TRUE FALSE BYE DUMP |
| **Tile engine**   | TI TVIEW TFILL TSRC0! TSRC1! TDST! TMODE! TCTRL! TADD TSUB TAND TOR TXOR TMUL TDOT TSUM TMIN TMAX TTRANS TZERO TABS TPOPCNT TL1 TEMIN TEMAX TMODE@ TCTRL@ |
| **Accumulators**  | ACC@ ACC1@ ACC2@ ACC3@ CYCLES |
| **Devices**       | DISK@ DISK-SEC! DISK-DMA! DISK-N! DISK-READ DISK-WRITE TIMER! TIMER-CTRL! TIMER-ACK EI! DI! ISR! NET-STATUS NET-SEND NET-RECV NET-MAC |

### 1.2  Missing ANS Forth core words (32 words needed for v1)

**Priority A — Required by KDOS / eliminates Forth workarounds:**

| Word | Impact |
|------|--------|
| `ABORT` | KDOS NEEDS/ASSERT currently print warning instead of aborting. Essential for error recovery. |
| `LEAVE` | KDOS ICONTAINS? uses IC-OK flag variable as workaround. Performance-critical in dictionary search. |
| `2OVER` | Defined in KDOS as `3 PICK 3 PICK`. Native ASM would be faster. |
| `EVALUATE` | Needed for LOAD-from-disk (interpret a string as Forth source). Core feature for file execution. |
| `FIND` | Dictionary lookup by name. Required for EVALUATE, introspection, and meta-programming. |
| `SOURCE` | Returns current input source buffer. Needed for EVALUATE/LOAD. |
| `>IN` | Input buffer offset variable. Needed for EVALUATE/LOAD. |
| `>NUMBER` | Number parsing from string. Needed for robust EVALUATE. |
| `QUIT` | Main REPL/interpreter loop as a callable word. |

**Priority B — ANS conformance & good practice:**

| Word | Notes |
|------|-------|
| `ABORT"` | Conditional abort with message. High-value for debugging. |
| `2SWAP` | Stack pair manipulation. |
| `2ROT` | Stack pair manipulation. |
| `2>R` `2R>` `2R@` | Double-cell return stack ops. |
| `MOVE` | Like CMOVE but handles overlapping regions. |
| `RECURSE` | Self-referential colon definition. |
| `DOES>` | CREATE…DOES> defining pattern. |
| `COUNT` | ( c-addr -- addr len ) for counted strings. |
| `COMPARE` | String comparison (KDOS defines SAMESTR? as substitute). |
| `U<` `U>` | Unsigned comparison (currently must use manual tricks). |
| `2/` | Arithmetic shift right. |
| `VALUE` `TO` | Named values (cleaner than VARIABLE for constants). |
| `WITHIN` | Range check. |
| `CHAR` `[CHAR]` | Character literal. |
| `POSTPONE` `[COMPILE]` | Compilation control. |

### 1.3  KDOS words to promote into BIOS assembly

These words are defined in KDOS Forth but would benefit from native assembly
for speed, correctness, or because they are fundamental infrastructure:

| Word | Currently | Why promote |
|------|-----------|-------------|
| `W@` `W!` | 16-bit LE fetch/store in Forth (byte-pair assembly) | Used heavily by filesystem. 1-instruction native vs ~10 Forth ops. |
| `L@` `L!` | 32-bit LE fetch/store in Forth | Used by filesystem directory parsing. Same speed rationale. |
| `UCHAR` | Forth: `DUP 97 >= OVER 123 < AND IF 32 - THEN` | Called per-character in dictionary search (ICONTAINS?). Hot path. |
| `SAMESTR?` | 25-line Forth using 3 variables | Core of all filename lookups. Native assembly = ~5× faster. |
| `.ZSTR` | Forth: `BEGIN DUP C@ ?DUP WHILE EMIT 1+ REPEAT DROP` | Called from DIR, CATALOG, DOC browser. Trivial in ASM. |
| `>=` `<=` | `< 0=` / `> 0=` (two ops) | Single-instruction with flags already computed. |
| `OFF` | `0 SWAP !` | Trivial but called everywhere. |
| `TALIGN` | `BEGIN HERE 63 AND WHILE 0 C, REPEAT` | Every buffer allocation. Could be a single HERE round-up in ASM. |

### 1.4  Other BIOS v1 improvements

- **SKIP instruction**: Add to assembler and BIOS use. The chip has SKIP
  (conditional skip next instruction) but the assembler doesn't emit it and
  the BIOS doesn't use it. Some IF/ELSE/THEN patterns can be replaced with
  SKIP for tighter code.
- **Optimised NEXT**: Review the inner interpreter loop. Consider inlining
  DOCOL/EXIT for common words.
- **Error handling**: Implement proper ABORT with stack reset + return to REPL.
  Wire into `ABORT"` for conditional error messages.
- **Compilation target**: Reassemble BIOS with asm.py after all additions;
  ship updated `bios.bin`.

### 1.5  Estimated BIOS v1 size

Current: 157 words, 5,483 lines ASM, 13 KB binary.
Projected: ~190 words, ~6,500 lines ASM, ~16 KB binary.

---

## 2  KDOS v0.9d → v1.0

### 2.1  Remove workarounds for missing BIOS words

Once BIOS v1 provides the missing words, remove these KDOS definitions:

| KDOS definition | Replacement |
|-----------------|-------------|
| `: 2OVER 3 PICK 3 PICK ;` | BIOS `2OVER` |
| `: UCHAR … ;` | BIOS `UCHAR` |
| `: >= < 0= ;` | BIOS `>=` |
| `: <= > 0= ;` | BIOS `<=` |
| `: OFF 0 SWAP ! ;` | BIOS `OFF` |
| `: .ZSTR … ;` | BIOS `.ZSTR` |
| `: SAMESTR? … ;` | BIOS `COMPARE` or `SAMESTR?` |
| `: W@ … ; : W! … ;` | BIOS `W@` `W!` |
| `: L@ … ; : L! … ;` | BIOS `L@` `L!` |
| `: TALIGN … ;` | BIOS `TALIGN` |
| `: NEEDS … ;` (warning only) | Use BIOS `ABORT` for real abort |
| `: ASSERT … ;` (warning only) | Use BIOS `ABORT"` |
| `: ICONTAINS? … ;` (flag-var) | Rewrite with BIOS `LEAVE` |

### 2.2  Screens on by default

**Current:** Startup prints a banner, loads the filesystem, and drops to the
REPL. The user must type `SCREENS` to enter the TUI.

**v1.0:** After loading the filesystem, auto-enter SCREENS (the 7-screen TUI).
Users press `q` to drop to the REPL. This makes the first-run experience
interactive rather than a blank prompt.

Implementation:
```
\ §14 Startup — change last line from REPL to:
DISK? IF FS-LOAD THEN
SCREENS
```

Add a `--no-screens` or `HEADLESS` flag in the BIOS/system config for
automated testing and scripted use (test harness needs REPL mode).

### 2.3  Filesystem expansion: beyond 64 files

**Current limit:** 4 directory sectors × 16 entries/sector = 64 files max.

**Proposed v1 layout (256 files):**

| Sectors | Content |
|---------|---------|
| 0 | Superblock |
| 1 | Allocation bitmap |
| 2–17 | Directory (16 sectors × 16 entries = 256 files) |
| 18+ | Data area |

Changes needed:
- **diskutil.py**: `DIR_SECTORS = 16`, `DATA_START = 18`, `MAX_FILES = 256`.
- **kdos.f**: `FS-DATA-START` → 18, `FS-MAX-FILES` → 256, directory cache size
  → 8,192 bytes (16 × 512).
- **Superblock version**: Bump to 2 to distinguish old/new layouts.
- **Backward compatibility**: `FS-LOAD` checks superblock version and adjusts
  `FS-MAX-FILES` and `FS-DATA-START` accordingly.

Alternatively, keep 64 files for v1.0 and expand in v1.1 — the current
disk image only uses 15 files. Expanding is a good-to-have, not a blocker.

### 2.4  New KDOS features for v1.0

- **LOAD** ( "filename" -- ): Read a Forth source file from MP64FS and
  EVALUATE it. Requires BIOS EVALUATE + SOURCE + >IN.
- **SAVE-BUFFER** ( buf "name" -- ): Save a buffer to a named MP64FS file.
- **EDIT** (stretch goal): Minimal in-memory line editor for Forth source.
- **AUTOEXEC**: On boot, if a file named `autoexec.f` exists on the disk,
  LOAD it automatically. Enables custom startup configurations.

### 2.5  Version bump

- KDOS banner: `v0.9d` → `v1.0`
- Screen header: `KDOS v0.9d` → `KDOS v1.0`
- DASHBOARD, STATUS, HELP: update version strings.

---

## 3  Emulator v0 → v1.0

### 3.1  Current emulator gaps vs chip spec

| Feature | Chip Spec | Emulator Status |
|---------|-----------|-----------------|
| Memory size | 8–64 MiB SRAM (tape-out config) | 1 MiB default (`mem_size = 1 << 20`) |
| CSR_MEGAPAD_SZ | Reports configured SRAM size | Returns 0 (stub) |
| SKIP instruction | Conditional skip next instruction | Not implemented (comment-only reference) |
| Pipeline model | 4-stage IF/ID/EX/WB | No pipeline (functional only) |
| Bank model | 16 logical banks, 4R+2W ports | Flat byte array |
| IDLE power | Fully static, retains state to DC | `idle` flag stops execution loop |
| Cycle accuracy | Real pipeline timing | Hardcoded cycle counts per instruction |

### 3.2  v1.0 emulator changes

**Must-have:**

- [ ] **Configurable memory size**: Accept `--ram` in MiB (8/16/32/64).
  Default to 8 MiB. Update `CSR_MEGAPAD_SZ` to report actual size.
- [ ] **SKIP instruction**: Implement the SKIP opcode (conditional skip of
  the next instruction). Already defined in condition code table.
- [ ] **CSR_MEGAPAD_SZ**: Return configured memory size instead of 0.

**Nice-to-have (v1.x):**

- Pipeline-aware cycle counting (bubble on taken branch, etc.)
- Bank-level memory access tracking for profiling
- SRAM bank arbitration warnings (detect >4R or >2W per cycle)

### 3.3  System-level changes

- **system.py**: Pass `--ram` value through to `Megapad64(mem_size=…)`.
  Update SP initialisation to top of new RAM size.
- **cli.py**: Accept `--ram` in MiB (currently KiB). Update help text.
- **test_system.py**: Tests should work with any RAM size ≥ 1 MiB.

---

## 4  Assembler (asm.py)

### 4.1  Current state

649 lines. Assembles bios.asm → bios.bin (13 KB). Single-pass with
backpatching for forward references.

### 4.2  v1.0 changes

- [ ] **SKIP opcode**: Add SKIP encoding to the assembler instruction table.
- [ ] **Listing output**: Optional `-l` flag to produce a listing file showing
  addresses + hex + source for debugging.
- [ ] **Size report**: Print final binary size after assembly.
- [ ] **Error messages**: Show line number and source line on assembly errors.

---

## 5  Sample Virtual Disk

### 5.1  What ships with v1.0

A pre-built `sample.img` containing:

| File | Type | Description |
|------|------|-------------|
| **KDOS source** | forth (3) | `kdos.f` — the complete OS loaded via AUTOEXEC |
| **10 doc pages** | doc (4) | getting-started, buffers, kernels, pipelines, storage, scheduler, screens, data-ports, tile-engine, reference |
| **5 tutorials** | tutorial (6) | hello-world, first-kernel, build-pipeline, data-ingest, custom-kernel |
| **autoexec.f** | forth (3) | Bootstrap script: loads KDOS, enters SCREENS |
| **demo-data** | data (5) | Sample 256-byte dataset for tutorial use |

Total: ~18 files, well within 64-file limit (or 256 with expanded FS).

### 5.2  Build process

```
python3 -c "
from diskutil import build_image, MP64FS, FTYPE_FORTH, FTYPE_DATA
from pathlib import Path

# Build base image with docs + tutorials
fs = build_image()

# Inject KDOS source
kdos_src = Path('kdos.f').read_bytes()
fs.inject_file('kdos.f', kdos_src, ftype=FTYPE_FORTH)

# Inject autoexec
autoexec = b'S\" kdos.f\" LOAD\nSCREENS\n'
fs.inject_file('autoexec.f', autoexec, ftype=FTYPE_FORTH)

# Inject sample data
demo = bytes(range(256))
fs.inject_file('demo-data', demo, ftype=FTYPE_DATA)

# Save
fs.save('sample.img')
print('Built sample.img:', fs.info())
"
```

### 5.3  Compiled BIOS binary

- Reassemble `bios.asm` → `bios.bin` after all v1 additions.
- Ship `bios.bin` in the repository (already present, needs update).
- Document: `python3 asm.py bios.asm -o bios.bin` to rebuild.

### 5.4  End-to-end boot command

```
python3 cli.py --bios bios.bin --storage sample.img --ram 8192
```

This boots the BIOS, which finds `autoexec.f` on disk, loads KDOS, and
enters the SCREENS TUI — a complete out-of-the-box experience.

---

## 6  Testing

### 6.1  Current: 327 tests (test_system.py)

All passing. Cover BIOS words, KDOS subsystems, filesystem, scheduler,
networking, tile engine, and pipelines.

### 6.2  New tests for v1.0

| Area | Tests needed |
|------|--------------|
| New BIOS words | ABORT, LEAVE, 2OVER, EVALUATE, FIND, SOURCE, >IN, >NUMBER, 2SWAP, 2ROT, W@, W!, L@, L!, UCHAR, SAMESTR?/COMPARE, >=, <=, OFF, TALIGN |
| KDOS LOAD | Load a Forth file from disk and verify execution |
| KDOS AUTOEXEC | Boot with autoexec.f and verify KDOS loads |
| Screens auto-start | Verify SCREENS runs on startup (may need headless flag) |
| Expanded FS | Create >64 files if FS expansion is done |
| SKIP instruction | Assembler + emulator tests for SKIP |
| Large RAM | Test with 8 MiB / 64 MiB RAM configurations |
| Sample disk | End-to-end boot from sample.img |

Target: **~380–400 tests**.

---

## 7  Documentation

### 7.1  Updates needed

- **README.md**: Update BIOS word count, binary size, KDOS version, test count.
- **EMULATOR.md**: Document SKIP instruction, configurable RAM.
- **KDOS.md**: Document LOAD, AUTOEXEC, expanded FS if applicable.
- **In-disk docs**: Update getting-started, storage, reference pages.

---

## 8  Implementation Order

### Phase 1: BIOS v1 (foundation)

1. Add missing ANS Priority-A words to `bios.asm` (ABORT, LEAVE, 2OVER,
   EVALUATE, FIND, SOURCE, >IN, >NUMBER, QUIT)
2. Promote KDOS performance-critical words to BIOS ASM (W@, W!, L@, L!,
   UCHAR, .ZSTR, SAMESTR?/COMPARE, >=, <=, OFF, TALIGN)
3. Add ANS Priority-B words (ABORT", 2SWAP, 2ROT, 2>R, 2R>, MOVE,
   RECURSE, DOES>, COUNT, U<, U>, 2/, VALUE, TO, WITHIN, CHAR, [CHAR])
4. Implement SKIP in assembler + BIOS usage
5. Reassemble → `bios.bin`
6. Write tests for all new BIOS words

### Phase 2: Emulator v1

7. Configurable RAM size (8–64 MiB), update CSR_MEGAPAD_SZ
8. Implement SKIP instruction in emulator
9. Update cli.py --ram to accept MiB
10. Tests for new emulator features

### Phase 3: KDOS v1

11. Remove KDOS workarounds (use new BIOS words)
12. Implement LOAD (requires EVALUATE)
13. Implement AUTOEXEC
14. Screens on by default (with headless flag for tests)
15. Rewrite ICONTAINS? with LEAVE
16. Rewrite NEEDS/ASSERT with ABORT
17. Version bump to v1.0
18. Update help text and screen headers

### Phase 4: Filesystem & Disk

19. (Optional) Expand FS to 256 files
20. Update diskutil.py: inject KDOS + autoexec into sample image
21. Build `sample.img` with full content
22. End-to-end boot test

### Phase 5: Polish

23. Update README.md, EMULATOR.md, KDOS.md
24. Update in-disk documentation
25. Final test run (target 380+ tests)
26. Git tag v1.0

---

## 9  File Summary

| File | Current | v1.0 changes |
|------|---------|--------------|
| `bios.asm` | 5,483 lines, 157 words | ~6,500 lines, ~190 words |
| `bios.bin` | 13 KB | ~16 KB |
| `asm.py` | 649 lines | SKIP opcode, listing output |
| `megapad64.py` | 1,305 lines | SKIP, configurable RAM, CSR fix |
| `system.py` | 300 lines | RAM config passthrough |
| `cli.py` | 990 lines | --ram in MiB |
| `kdos.f` | 2,393 lines, 215 words | ~2,200 lines (remove workarounds), add LOAD/AUTOEXEC |
| `diskutil.py` | 914 lines | (optional) 256-file FS, KDOS injection |
| `test_system.py` | 3,956 lines, 327 tests | ~4,500 lines, ~380 tests |
| `sample.img` | (not yet) | Pre-built disk with OS + docs + data |
| `README.md` | 222 lines | Update stats |
