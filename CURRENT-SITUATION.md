# Megapad-64 — Current Situation

> Historical snapshot: this document records the project as of 2026-03-07,
> including the since-retired WOTS prototype. Current crypto behavior and
> checkpoint status are defined by
> [`docs/crypto-interface-contract.md`](docs/crypto-interface-contract.md).
> Current TCP/TLS claims are defined by
> [`docs/tls-hardening.md`](docs/tls-hardening.md); the active implementation
> history and combined-integration evidence ledgers live in
> `MEGAPAD_SECURE_SERVER_TRANSPORT_HANDOFF.md` and
> `MEGAPAD_KDOS_MODULE_REGISTRY_HANDOFF.md` at the workspace root outside this
> repository.

> **M* closure correction — 2026-08-23:** The qualified MegaPad code checkpoint
> is branch `integration/secure-registry-burrow` at
> `ca02a40c04840791c731dbb7c77ecd7e85eb4909`. It combines the closed secure-server
> lower transport with the exact, available-memory-bound module registry. Stable
> registry nodes and buckets use the Bank-0 heap and survive `XMEM-RESET`.
> Userland now derives disjoint dictionary and general-XMEM spans from actual
> remaining capacity; the default divides that capacity equally, while a
> pre-initialization `U-XMEM-RESERVE!` selects the general-XMEM reserve and
> complementary dictionary span. BIOS active bounds preflight every HERE-growing
> path and throw caught KDOS fault `-8` before any rewind, wrap, or overrun write.
> It also exposes `PROVIDED-SPAN`, the caller-owned exact-ID registration entry
> needed by paired binary-image loaders, with the same duplicate neutrality,
> active-loader transaction ownership, and 1-through-246-byte identity envelope
> as parsed `PROVIDED`. The earlier `8f0e478` checkpoint's sparse-work `BALANCE`
> correction remains part of M* without changing the historical TLS LAST-ACK
> diagnosis.
>
> At exact M* code `ca02a40`, the in-sandbox sequential sweep passed 3,614
> tests, skipped 36, and deselected four host-loopback UDP-backend cases in
> 1,502.26 seconds. The focused dynamic-registry selector passed 11/11, and the
> module/`PROVIDED` guard selector passed 69/69. Historical adjoining evidence
> at `8f0e478` remains `TestKDOSMulticore` 87/87 in 854.40 seconds plus 11 other
> `tests/test_networking.py` cases in the sandbox. Final host-environment
> confirmation at the same executable code then passed all four AF_INET
> loopback/UDP-backend cases through the required sequential harness in 2.25
> seconds (4 passed, 3,650 deselected).
>
> The first documentation-only M* head,
> `c3210bf54f2116190770c0b95caaa0b9b396e937`, records the original ledger.
> Later documentation ledgers advance the exact pre-landing M* closure head to
> `a8cb7995363ebd5177e7e94375abd068e322329f`. Neither head replaces exact
> `ca02a40` as the qualified MegaPad executable code.
>
> **Paired A* completion — 2026-08-23:** Exact Akashic executable code
> `4b8680568a229b1bd114d3a05fa4e73f745157ab` was qualified against exact
> MegaPad executable code `ca02a40c04840791c731dbb7c77ecd7e85eb4909`.
> Its Checkpoint-5 product journey passed at 27.1 billion guest steps in
> 811.14 seconds with stable replay, the read-only Rabbit data plane, and final
> teardown; the current canonical Desktop passed at 13.288 billion steps in
> 332.51 seconds. Akashic documentation-only A* closure head
> `c69fbe57cb6169c80560033e94d3d9a640ad9def` records that result without
> replacing the tested executable revisions. Checkpoint 5 is paired
> application-integration evidence, not an additional MegaPad TCP/TLS claim or
> a MegaPad code change.
>
> **Local landing state — 2026-08-24:** Local MegaPad `main` was fast-forwarded
> to the exact pre-landing M* closure head `a8cb7995363ebd5177e7e94375abd068e322329f`,
> and local Akashic `main` was fast-forwarded to exact A* closure head
> `c69fbe57cb6169c80560033e94d3d9a640ad9def`. The cached `origin/main` refs,
> confirmed fresh during landing preflight, remain MegaPad
> `f4b8144786001e423291b9458f24e0efa7ab70ce` and Akashic
> `d2e9551ffc37e324bb83acf51108f506599edfd5`; neither repository has been
> pushed. The documentation-only landing record containing this update follows
> those pre-landing heads and adds no executable qualification. Exact
> `ca02a40`/`4b86805` remain the qualified executable pair.

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
- **Python batch callbacks** — `on_tx_batch(data)` in `emulator/devices.py`;
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
- **Network tools**: HTTP/HTTPS, FTP and a provisional non-interoperable FTPS
  helper, Gopher, DNS-LOOKUP.
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

The standard disk boot compiles `kdos.f` into Bank 0.  KDOS then runs
`autoexec.f`, which enters the XMEM userland dictionary, loads
`networking.f` with its batched `REQUIRE` path, configures the link, and loads
`tools.f`. The inventory below is part of this dated snapshot; do not use its
line or test counts as current qualification evidence.

| Layer | File(s) | Lines |
|-------|---------|-------|
| BIOS | `bios.asm` → `bios.rom` | 14,524 (367 words) |
| KDOS core (Bank 0) | `kdos.f` | ~8,100 |
| Networking (userland) | `networking.f` | ~7,500 |
| Tools | `tools.f` | 990 |
| CPU emulator | `emulator/megapad64.py` | 3,002 |
| SoC | `emulator/system.py` | 1,018 |
| Devices | `emulator/devices.py` | 2,542 |
| CLI | `cli.py` | 1,557 |
| C++ accel | `emulator/accel/mp64_accel.cpp` | 3,295 |
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
