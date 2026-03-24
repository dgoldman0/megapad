# Megapad-64 — Current Situation

**Date:** 2026-03-07  
**Branch:** `main`  
**Status:** 1,797 tests passing, 35 skipped.

---

## 1. Recent changes

### UART TX ring buffer (2026-03-07)

- **BIOS ring buffer** — 4096-byte RAM-resident TX ring buffer.  `EMIT`
  appends to the buffer (fast RAM write); the buffer auto-flushes on
  overflow or explicit `TX-FLUSH`.  `KEY` and `BYE` flush before
  blocking/halting.  R19 holds the ring descriptor pointer (set at boot,
  registered via TX_RING_BASE MMIO).
- **UART device registers** — TX_FLUSH (`+0x06`, W) and TX_RING_BASE
  (`+0x08`–`+0x0F`, W, 64-bit LE) added to the UART MMIO block.
- **Python batch callbacks** — `on_tx_batch(data)` in `devices.py`;
  `cli.py` uses `os.write` for console, `sendall` for headless TCP.
- **New Forth word** — `TX-FLUSH` (dictionary entry #87).

### SEP dispatch & 1802 heritage restoration (Phases 0–9)

- **Phase 0 — Audit & test harness** — baseline measurements, SEP smoke
  tests added.
- **Phase 1 — SEP leaf I/O** — R4 (`emit_char`), R5 (`key_char`),
  R6 (`print_hex_byte`) converted from `call.l`/`ret.l` to SEP dispatch.
  ~54 call sites updated.  Zero stack traffic for the most frequent BIOS
  calls.
- **Phase 4 — Q semaphore** — `SEQ`/`REQ` in `emit_char` as UART-busy
  signal, testable via `BR.BQ`/`BR.BNQ`.
- **Phase 5 — Secondary core SEP** — verified per-core register files
  make SEP I/O safe across all 4 full cores.
- **Phase 7 — STXI byte processing** — 16 routines converted to
  `sex` + `glo`/`ghi` + `stxi`/`stxd.d` chains: FILL, TFILL, CMOVE,
  MOVE (bwd + fwd), `write_mmio_addr8_le`, `write_mmio_u32_le`,
  `w_disk_sec_store`, `w_disk_dma_store`, `compile_call`,
  `compile_literal`, `w_create`, `w_var_name_done`, `w_val_name_done`,
  `does_runtime`, `w_lstore`.  4 bugs found and fixed (C++ missing STXI
  opcode, dead code in `w_move_fwd`, `lsri` imm4 overflow, VRAM bounds
  overflow in C++ tile memory access).
- **Phase 8 — Cooperative multitasking** — `PAUSE`, `YIELD`,
  `BACKGROUND`, `TASK-STOP`, `TASK-STATUS` words.  SEP-based two-task
  model on core 0 with independent stacks.  1 cycle / 0 memory context
  switch via `sep r13`.
- **Phase 9 — Fault diagnostics** — T-register inspection in bus-fault
  and privilege-fault handlers.  Reports which SEP context was active
  when a fault occurred.
- **Phase 3 — DEFERRED** — deep analysis showed ITC threading destroys
  JIT inline table (17 entries, 3–13B) and bigram fusion (6 entries).
  Net regression for code density.  Skipped.

### ISA additions

- **STXI** (0x89) — `M(R(X)) ← D[7:0]; R(X)++`.  Replaces `st.b + inc` pairs.
- **STXD.D** (0x8B) — `M(R(X)) ← D[7:0]; R(X)--`.  Replaces `st.b + dec` pairs.
- Both implemented in RTL, Python emulator, C++ accelerator, and assembler.

### C++ accelerator

- STXI/STXD.D opcodes implemented (replaced SDB.X/SMB.X which were unused).
- VRAM bounds overflow fixed in all tile memory access functions
  (`tile_read_64bytes`, `tile_write_64bytes`, `sys_read*`, `sys_write*`).
  Changed `addr + N <= base + size` to `(addr - base) + N <= size` to
  prevent 64-bit overflow near top of address space.

### WOTS+ Chain Accelerator & Bus Timeout (2026-03-07)

- **WOTS+ Chain Accelerator** (MMIO 0x8A0, 32 bytes) — Hardware chain
  sequencer wrapping SHA3/SHAKE.  DMA-read context from RAM, iterates
  SHAKE-256 internally, returns 16-byte result.  Emulator device class
  (`WotsChainAccelerator`) + 8 integration tests.
- **Bus arbiter MMIO/MEM timeout** — RTL: 6-bit MMIO (63 cycles) and
  8-bit MEM (255 cycles) watchdog counters in `mp64_bus.v`.  Timeout
  returns sentinel `0xDEAD_DEAD_DEAD_DEAD`, asserts `bus_err` pulse +
  sticky latch, fires `IRQX_BUS` interrupt.  W1C `CSR_BUS_ERR` (0x5A).
  Emulator: `BusError` exception on unmapped MMIO, caught by SoC layer
  and converted to `TrapError(IVEC_BUS_FAULT)`.  6 Python tests.
  RTL testbench: 38/38 pass (tests 8–9 cover MMIO/MEM timeout).

### Earlier (prior to Phase work)

- **Crypto**: SHA3-512, SHA-256 accelerator, AES-128 mode, CRC
  ISA migration (MMIO removed), AES-GCM partial block masking.
- **TLS 1.3**: Dual cipher suites (0xFF01, 0x1301), record framing, SNI.
- **Network tools**: HTTP/HTTPS, FTP/FTPS, Gopher, DNS-LOOKUP.
- **System**: Userland memory isolation, headless mode, ext mem,
  micro-cluster fix, NET-IDLE, autoexec.f boot chain.

## 2. What's known-broken

### TestPrivilege (3 tests)

The C++ accelerator raises `TrapError` directly instead of dispatching
through the IVT.  Pure-Python mode passes all 3.  Low priority — the
privilege enforcement itself works; only the trap *delivery path* differs.

### Skipped tests (3)

DNS/network tests requiring live internet are skipped in CI.

## 3. Architecture at a glance

| Layer | File(s) | Lines |
|-------|---------|-------|
| BIOS | `bios.asm` → `bios.rom` | 14,524 (367 words) |
| KDOS | `kdos.f` | 11,760 |
| Tools | `tools.f` | 990 |
| CPU emulator | `megapad64.py` | 3,002 |
| SoC | `system.py` | 1,018 |
| Devices | `devices.py` | 2,542 |
| CLI | `cli.py` | 1,557 |
| C++ accel | `accel/mp64_accel.cpp` | 3,295 |
| Assembler | `asm.py` | 909 |
| Tests | `tests/test_system.py` | 24,761 (1,634 tests, 77 classes) |
|       | `tests/test_networking.py` | 187 (13 tests) |
|       | `tests/test_megapad64.py` | 2,647 (25 tests) |
|       | `tests/test_fs_hardening.py` | (27 tests) |

**Total tests: 1,731** (3 skipped)

## 4. MMIO address map (crypto region)

| Offset | Peripheral |
|--------|-----------|
| 0x0700 | AES-256/128-GCM |
| 0x0780 | SHA-3/SHAKE (96 bytes) |
| 0x0800 | TRNG |
| 0x0840 | Field ALU |
| 0x08C0 | NTT Engine |
| 0x0900 | KEM (ML-KEM-512) |
| 0x0940 | SHA-256 |
| ~~0x0980~~ | ~~CRC32/CRC64~~ *(removed — now ISA-only)* |
| 0x08A0 | WOTS+ Chain Accelerator (32 bytes) |
| 0x0A00 | Framebuffer |

## 5. How to run things

```bash
make test             # all 1,731 tests (~23s with C++ accel)
make test-one K=X     # single class/test
make test-status      # check progress
make test-net         # networking tests (requires TAP)
make disk             # rebuild sample.img

# Interactive:
python cli.py --bios bios.asm --storage sample.img --nic-tap

# Headless:
python cli.py --bios bios.asm --storage sample.img --headless
python cli.py --connect localhost:6464
```
