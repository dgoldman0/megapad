# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, crypto stack, full network stack,
multicore OS, and comprehensive documentation — that feels complete and
cohesive as a v1.0 release.

**Last audited: 2026-03-07** (425 commits, 140K total lines, 165 tracked files)

**Current state:** Nearly everything is done.  Layers 0–3 and 5–6 are
fully shipped.  The remaining open items are a handful of application-level
features (editor, remote REPL, SCROLL network fetcher) and a few
speculative/optional items (USB controller, CSR_CORE_TYPE, bitfield ALU,
FPGA bitstream).  The project is functionally a complete system.

---

## Project at a Glance

| Component | Lines | Key metric |
|-----------|------:|------------|
| `bios.asm` | 15,203 | 396 dictionary entries |
| `kdos.f` | 11,815 | 962 colon defs, 523 vars/constants, §1–§20 |
| `megapad64.py` | 3,315 | Full CPU + extended tile + FP16/BF16 |
| `accel/mp64_accel.cpp` | 3,295 | C++ CPU core (pybind11, ~63× speedup) |
| `accel/*.h` | 2,436 | Crypto, FB, NIC, timer C++ device accel |
| `accel_wrapper.py` | 897 | Drop-in C++ ↔ Python bridge |
| `system.py` | 1,018 | Multi-core SoC (up to 16 cores + clusters) |
| `devices.py` | 2,542 | 18 device classes |
| `cli.py` | 1,557 | Interactive monitor/debugger |
| `display.py` | 1,872 | Pygame GUI: terminal + graphics + debug |
| `asm.py` | 909 | Two-pass assembler (full ISA + EXT) |
| `nic_backends.py` | 405 | Pluggable NIC: Loopback, UDP, TAP |
| `diskutil.py` | 1,628 | MP64FS tooling |
| **RTL modules** | **16,175** | **36 portable Verilog + 4 headers** |
| RTL testbenches | 13,227 | 31 testbenches |
| RTL target overrides | 782 | 12 files (Xilinx 7-series + ASIC stubs) |
| Tests (Python) | 28,823 | 1,742 tests (1,739 pass, 3 skip) |
| Documentation | 13,914 | 20 docs in `docs/` |
| Forth source | 13,460 | `kdos.f` + `autoexec.f` + `tools.f` + `graphics.f` |
| FPGA scripts | 1,736 | Synthesis, constraints, BIOS hex gen |
| **Total** | **~140K** | |

### Test suite

- **88 test classes** across 4 test files (78 in `test_system.py`, 2 in `test_networking.py`, 8 in `test_fs_hardening.py`)
- **1,707 test methods** defined (`test_system.py`: 1,642, `test_megapad64.py`: 25, `test_networking.py`: 13, `test_fs_hardening.py`: 27)
- **1,742 collected** by pytest (parametrized expansion), **1,739 passing**, **3 skipped** (TAP network tests requiring internet)
- RTL: 31 Verilog testbenches with ~450+ hardware assertions

### RTL inventory (48 modules + 4 headers)

| Category | Modules | Key files |
|----------|--------:|-----------|
| Core | 8 | cpu, cpu_micro, alu, fp16_alu, icache, cluster, string, dict |
| Crypto | 7 | sha3, sha256, aes, field_alu, ntt, kem, wots |
| Memory | 2 | memory (4-bank), extmem |
| Bus | 1 | bus (arbiter + QoS + timeout) |
| Peripherals | 7 | uart, timer, disk, nic, trng, rtc, mailbox (crc removed—now ISA) |
| GPU | 1 | tile (64-lane SIMD) |
| SoC | 2 | soc, top |
| Primitives | 7 | sram_sp, sram_dp, mul, pll, clkgate, rst_sync, rom |
| Headers | 4 | defs.vh, pkg.vh, cpu_common.vh, cpu_funcs.vh |
| Xilinx 7 | 6 | sram_sp/dp, mul, pll, clkgate, synth_top |
| ASIC stubs | 6 | sram_sp/dp, mul, pll, clkgate, rst_sync |

---

## Completed Work

### Layer 0: Foundation (Items 1–4) — ✅ DONE

1. ✅ **Memory allocator** — `ALLOCATE`, `FREE`, `RESIZE`, `HEAP-SETUP`, `.HEAP`
2. ✅ **CATCH/THROW** — exception handling, nested catch, re-throw
3. ✅ **CRC integration** — `CRC-BUF`, `CRC32-BUF`, `CRC32C-BUF`, `CRC64-BUF`
4. ✅ **Hardware diagnostics** — `.DIAG`, `BIST-REPORT`, `TILE-REPORT`, `.PERF`

### Layer 1: Crypto Stack (Items 5–8) — ✅ DONE

5. ✅ **AES-256-GCM** — 10 BIOS words, KDOS §1.5
6. ✅ **SHA-3 / SHAKE / TRNG** — Keccak-f[1600], 4 modes, XOF squeeze, CSPRNG
7. ✅ **KDOS crypto words** — `HASH`, `HMAC`, `ENCRYPT`, `DECRYPT`, `VERIFY`
8. ✅ **Filesystem encryption** — `FENCRYPT`, `FDECRYPT`, `FS-KEY!`

### Layer 2: Network Stack (Items 9–18) — ✅ DONE

Full L2–L7 network stack, bottom-up:

9. ✅ **Ethernet framing** — MAC address, EtherType, `ETH-BUILD`/`ETH-PARSE`/`ETH-SEND`
10. ✅ **ARP** — table, request/reply, auto-responder
11. ✅ **IPv4** — header build/parse, checksum, `IP-SEND`/`IP-RECV`
12. ✅ **ICMP** — echo request/reply, auto-responder, `PING`/`PING-IP`
13. ✅ **UDP** — header, checksum, `UDP-SEND`/`UDP-RECV`, port demux
14. ✅ **DHCP** — full state machine, auto-configure on boot
15. ✅ **DNS** — A-record client, `DNS-RESOLVE`
16. ✅ **TCP** — 11-state machine, 3-way handshake, sliding window,
    congestion control, retransmit, TIME_WAIT reaper (37 tests)
17. ✅ **TLS 1.3** — HKDF, record layer, handshake, app data,
    AES-256-GCM + SHA-3 HMAC (37 tests)
18. ✅ **Socket API** — `SOCKET`, `BIND`, `LISTEN`, `ACCEPT`, `CONNECT`,
    `SEND`, `RECV`, `CLOSE` (TCP + UDP)

### Layer 3: Multi-Core OS (Items 19–24) — ✅ DONE

19. ✅ **Per-core run queues**
20. ✅ **Work stealing**
21. ✅ **Core affinity**
22. ✅ **Per-core preemption** — timer IRQ on all cores
23. ✅ **IPI messaging** — mailbox for structured inter-core messages
24. ✅ **Shared resource locks** — dictionary, UART, filesystem

### Layer 5: Field ALU & Post-Quantum Crypto (Items 34–38) — ✅ DONE

34. ✅ **Field ALU** — GF(p) coprocessor, 4 primes (Curve25519, secp256k1,
    P-256, custom), modes 0–12 (15 + 39 tests)
35. ✅ **NTT Engine** — 256-point NTT/INTT, dual modulus (12 tests)
36. ✅ **SHA-3 SHAKE Streaming** — XOF auto-squeeze
37. ✅ **ML-KEM-512 (Kyber)** — keygen/encaps/decaps via NTT+SHA3+TRNG (11 tests)
38. ✅ **Hybrid PQ Key Exchange** — X25519 + ML-KEM + HKDF (7 tests)

### Layer 6: Architecture & Portability (Items 39–42) — ✅ DONE

39. ✅ **Micro-core CPU (MP64µ)** — shift-add mul, no cache/BIST, shared
    cluster tile engine (16 tests)
40. ✅ **Multi-prime Field ALU** — 4-prime programmable modulus, FCMOV,
    FCEQ, FMAC, MUL_ADD_RAW
41. ✅ **4-bank memory** — Bank 0 (1 MiB) + HBW Banks 1–3 (1 MiB each) +
    external memory, `HBW-ALLOT`/`HBW-BUFFER`/`HBW-RESET`
42. ✅ **Technology-agnostic RTL** — `rtl/prim/` abstractions,
    `rtl/target/xilinx7/` + `rtl/target/asic/` overrides

### SoC Hardening (Items 0, 5-bridge, 7, 9, 10) — ✅ DONE

Per `docs/SoC-hardening.md`:

- §0 ✅ **STXI/STXD.D instructions** + IO OUT bug fix
- §1 ✅ **SHA-256 ISA migration** — spec + emulator + BIOS + C++ accel + tests done; RTL datapath deferred to RTL phase
- §2 ✅ **EXT.STRING engine** — ISA extension (prefix F9), 5 sub-ops (CMOVE, CMOVE>, BFILL, BCOMP, BSRCH)
- §3 ✅ **EXT.DICT engine** — ISA extension (prefix FA), 4 sub-ops (DFIND, DINS, DDEL, DCLR)
- §5 ✅ **Port I/O bridge** — 1802 OUT/INP → MMIO remap, byte-serial DMA
- §7 ✅ **WOTS+ chain accelerator** — DMA-read FSM, wraps SHA3, 5.3× chain speedup
- §9 ✅ **Bus timeout** — MMIO/MEM ACK watchdog, sticky latch, W1C clear, `IVEC_BUS_FAULT`
- §10 ✅ **BIOS SHA3/WOTS lock guards** — `SHA3-LOCKED?`, `WOTS-STATUS@`, `BUS-ERR@`, `BUS-ERR-CLR`

### Additional Completed Items

- 25 ✅ **Outbound data** — `NET-SEND` (BIOS + NIC DMA), `PORT-SEND` / `PORT-SEND-SLICE` (KDOS §10.1 UDP data port transport)
- 26 ✅ **FP16 tile mode** — `FP16-MODE`, `BF16-MODE` BIOS words, KDOS §3.1 (`F.SUM`, `F.DOT`, `F.SUMSQ`, `F.ADD`, `F.MUL`, `BF.SUM`, `BF.DOT`)
- 27 ✅ **QoS** — per-port bus bandwidth weights in `mp64_bus.v`
- 29 ✅ **Scripting / AUTOEXEC** — BIOS auto-boots first Forth file from disk; `autoexec.f` (DHCP + static fallback + userland + `REQUIRE tools.f`)
- 31 ✅ **CLI boot performance** — C++ accelerator engaged, boot ~2–3 s
- 32 ✅ **Network hardening** — 24 unit tests + 12 TAP integration tests
- 33 ✅ **BIOS `.'` delimiter bug** — ANS Forth compliant
- 43 ✅ **Display screen 8 exits to RPL** — stack imbalance fix, dynamic core-type detection
- 44 ✅ **CLI `--clusters` flag** — uncapped `--cores`, `--clusters` arg
- 47 ✅ **Memory management hardening** — 5 phases: coalescing, MARKER/FORGET, heap/stack collision guard, HEAP-CHECK, in-place RESIZE, buffer registry scaling, `.MEM`
- 48 ✅ **Arena allocator** — 4 phases: heap-backed, multi-source + snapshots, scoped arena stack, arena-scoped buffers (33 tests)
- 49 ✅ **STC compiler hardening** — branch offset overflow checks, LEAVE overflow (12 tests)

### BIOS v1.0 — ✅ DONE

15,203 lines ASM, 396 dictionary entries.  Full Forth kernel with:
compile/interpret loop, dictionary operations, string operations,
math (integer + 64-bit), control flow, defining words (VARIABLE,
CONSTANT, VALUE, CREATE/DOES>, DEFER), memory ops, I/O, UART
driver, timer driver, disk driver (MP64FS read/write + FSLOAD +
FSSAVE), NIC driver (send/recv/DMA), crypto (AES, SHA3, SHA256,
TRNG, Field ALU, NTT, KEM, WOTS), CRC, framebuffer, port I/O
bridge, multicore (IPI, spinlocks), trap handlers (including bus
fault with ERR= diagnostic), auto-boot, STC compiler.

### KDOS v1.1 — ✅ DONE

11,815 lines Forth, 962 colon defs, 523 vars/constants.
Sections: §1 Utility (allocator, arenas, exceptions, CRC, diagnostics,
AES, SHA3, SHA256, HKDF, X25519, Field ALU, NTT, KEM, PQ exchange,
HBW/ext-mem allocators, userland isolation), §2 Buffers, §3 Tile ops
(+ FP16/BF16), §4 Kernel registry, §5 Sample kernels, §6 Pipeline
engine, §7 Storage (filesystem, encryption, subdirectories, doc
browser), §8 Scheduler (preemptive, multicore, work stealing), §9 TUI
(9 screens + subscreens, widget SDL), §10 Data port transport, §11
Benchmarking, §12 Dashboard, §13 Help system, §14 Startup, §15 Pipeline
bundles, §16 Network stack (Ethernet → ARP → IPv4 → ICMP → UDP → DHCP
→ DNS → TCP → TLS 1.3 → TLS connection API), §17 Socket API, §18 Ring
buffers, §19 Hash tables, §20 Module system.

### Emulator — ✅ DONE

- `megapad64.py`: 3,315 lines — full ISA including EXT.STRING, EXT.DICT,
  64-lane tile engine with FP16/BF16, BIST, I-cache model
- `accel_wrapper.py`: 897 lines — transparent C++ ↔ Python bridge
- `accel/mp64_accel.cpp`: 3,295 lines + 2,436 lines headers — C++ fast
  path (~63× speedup), crypto device integration, pybind11
- `system.py`: 1,018 lines — multi-core SoC wiring (up to 16 cores +
  micro-clusters), device bus, IRQ routing
- `devices.py`: 2,542 lines — 18 device classes (UART, Timer, Storage,
  SysInfo, NIC, Mailbox, Spinlock, KEM, CRC, NTT, Framebuffer, CppFBProxy,
  CppTimerProxy, RTC, PortBridgeCSR, WotsChainAccel, DeviceBus, plus
  AES/SHA3/SHA256/TRNG/FieldALU in C++ accel)
- `cli.py`: 1,557 lines — interactive monitor/debugger, `--run` auto-boot
- `display.py`: 1,872 lines — pygame GUI with terminal + graphics tabs,
  debug panel, VT100 emulation
- `asm.py`: 909 lines — two-pass assembler, full ISA + EXT prefixes
- `nic_backends.py`: 405 lines — Loopback, UDP, TAP backends
- `diskutil.py`: 1,628 lines — MP64FS tooling

### RTL — ✅ DONE

31,608 total lines across 84 Verilog files:
- 36 portable modules (16,175 lines): CPU (full + micro), ALU, FP16 ALU,
  I-cache, cluster, string engine, dictionary engine, SHA3, SHA256, AES,
  field ALU, NTT, KEM, WOTS, tile engine, 4-bank memory, ext memory,
  bus arbiter (QoS + timeout), UART, timer, disk, NIC, CRC, TRNG, RTC,
  mailbox, SoC, top, primitives (SRAM, MUL, PLL, clkgate, rst_sync, ROM)
- 4 headers (1,298 lines): defs, pkg, cpu_common, cpu_funcs
- 31 testbenches (13,227 lines)
- 12 target overrides (782 lines): Xilinx 7-series (6) + ASIC stubs (6)

### FPGA — 🔄 PARTIAL

- ✅ Synthesis scripts (Yosys + Vivado), pin constraints (Genesys 2, Nexys A7)
- ✅ BIOS hex generator, RTL fully synthesizable
- ❌ No complete synthesis run (Yosys can't handle the full SoC; Vivado needed)
- ❌ No bitstream

### Test Suite — ✅ 1,739 passing

| File | Classes | Methods | Lines |
|------|--------:|--------:|------:|
| `test_system.py` | 78 | 1,642 | 24,970 |
| `test_megapad64.py` | — | 25 | 2,647 |
| `test_networking.py` | 2 | 13 | 187 |
| `test_fs_hardening.py` | 8 | 27 | 283 |
| `conftest.py` | — | — | 242 |
| **Total** | **88** | **1,707** | **28,329** |

1,742 collected (parametrized expansion), 1,739 passing, 3 skipped (TAP/network).

### Documentation — ✅ 20 docs

`docs/`: architecture, arenas, BIOS-DICTIONARY, bios-forth, extended-tpu-spec,
filesystem, framebuffer-plan, fs-hardening-roadmap, getting-started,
IDEAS-scratchpad, isa-reference, kdos-reference, memory-management,
net-hardening, register-hardening, screens-hardening, sep-dispatch-roadmap,
SoC-hardening, tile-engine, tools.  Plus top-level README, KDOS, EMULATOR,
CURRENT-SITUATION.

---

## Remaining for v1.0

### Truly open

| # | Item | Effort | Priority |
|---|------|--------|----------|
| 28 | **On-device editor** — line/screen editor in Forth | ~1–2 days | Medium |
| 30 | **Remote REPL** — UART or TCP-based remote Forth session | ~1 day | Medium |
| 45 | **SCROLL** — network resource fetcher (HTTP/1.1, TFTP, Gopher); `SCROLL-GET`, `SCROLL-SAVE`, `SCROLL-LOAD` for over-LAN package loading | ~2–3 days | High |

### Nice-to-have (post-v1.0 candidates)

| # | Item | Effort | Notes |
|---|------|--------|-------|
| 39g | `CSR_CORE_TYPE` per-core CSR | ~1 hour | Workaround via SysInfo `NUM_FULL` exists |
| 46 | **USB controller** | ~2–6 weeks | 3-phase (emulator → RTL USB 2.0 → USB 3.0) |
| §4e | **Bitfield ALU ops** (POPCNT, CLZ, CTZ, BITREV, BEXT, BDEP, RORI, BSWAP) | ~1–2 days | ~270 LUTs, pure ALU sub-ops, high bang-for-buck |
| §4a–d,f | **Other SoC accelerators** (crypto pipeline, deflate, regex, DSP, trace unit) | ~1–4 weeks each | Speculative / wish-list |
| §2/§3 | **C++ accel EXT.STRING/EXT.DICT** native | ~2 days | Performance only; correctness is fine via Python fallback |
| 45b | **Emulator timing model** | ~1 day | Wall-clock `MS` word or IDL+timer hybrid for robust network polling |
| — | **FPGA bitstream** | ~1 week | Requires Vivado; RTL is ready |

### Shortest path to "v1.0 done"

SCROLL is the highest-value remaining feature — it turns the network
stack into a practical tool (fetch Forth source from a server, load
packages over LAN, browse docs).  After SCROLL, the system is
genuinely self-sufficient: it boots, auto-configures networking,
can fetch and evaluate remote code, has a TUI, a filesystem, crypto,
and a full Forth development environment.

The editor and remote REPL are polish items that round out the
interactive experience.  Everything else is post-v1.0 optimization
or hardware extension.

**Proposed order:**
1. SCROLL (§45) — network fetcher + SCROLL-LOAD
2. Editor (§28) — on-device line editor
3. Remote REPL (§30) — TCP session
4. Tag v1.0

---

## Implementation Order — Final Status

```
Layer 0  Items  1– 4  Foundation                              ✅ DONE
Layer 1  Items  5– 8  Crypto Stack                            ✅ DONE
Layer 2  Items  9–18  Network Stack (L2–L7, TLS 1.3, Sockets) ✅ DONE
Layer 3  Items 19–24  Multi-Core OS                           ✅ DONE
Layer 4  Items 25–30  Application-Level                       🔄 3 of 6 open
Layer 5  Items 34–38  Field ALU & Post-Quantum Crypto          ✅ DONE
Layer 6  Items 39–42  Architecture & Portability               ✅ DONE
SoC      §0–§10      SoC Hardening                            ✅ DONE
Bugs     Items 31–33, 43–44  Known Issues                     ✅ ALL FIXED
Memory   Items 47–48  Hardening + Arenas                      ✅ DONE
Compiler Item 49      STC Compiler Checks                     ✅ DONE
```

**6 of 7 layers complete.  Layer 4 needs 3 items (SCROLL, editor, remote REPL).**

---

## File Summary

| File | Lines | Status |
|------|------:|--------|
| `bios.asm` | 15,203 | ✅ 396 dictionary entries |
| `kdos.f` | 11,815 | ✅ 962 colon defs, 523 vars/constants, §1–§20 |
| `autoexec.f` | 47 | ✅ Boot script: DHCP + userland + REQUIRE |
| `tools.f` | 1,534 | ✅ Loadable tools module |
| `graphics.f` | 64 | ✅ Graphics module |
| `megapad64.py` | 3,315 | ✅ Full CPU + extended tile + FP16/BF16 |
| `accel/mp64_accel.cpp` | 3,295 | ✅ C++ CPU core (pybind11, ~63× speedup) |
| `accel/*.h` | 2,436 | ✅ Crypto, FB, NIC, timer C++ devices |
| `accel_wrapper.py` | 897 | ✅ Drop-in C++ ↔ Python wrapper |
| `system.py` | 1,018 | ✅ Multi-core SoC (16 cores + clusters) |
| `devices.py` | 2,542 | ✅ 18 device classes |
| `cli.py` | 1,557 | ✅ Interactive monitor/debugger |
| `display.py` | 1,872 | ✅ Pygame GUI: terminal + graphics + debug |
| `asm.py` | 909 | ✅ Two-pass assembler (full ISA + EXT) |
| `nic_backends.py` | 405 | ✅ Pluggable NIC (Loopback, UDP, TAP) |
| `diskutil.py` | 1,628 | ✅ MP64FS tooling |
| `setup_accel.py` | 36 | ✅ pybind11 build config |
| `Makefile` | 160 | ✅ Build, test, accel targets |
| `rtl/` | 31,608 | ✅ 36+4+31+12 Verilog modules |
| `fpga/` | 1,736 | 🔄 Scripts ready, no bitstream |
| `tests/` | 28,329 | ✅ 1,739 passing, 3 skipped |
| `docs/` | 13,914 | ✅ 20 documents |
| `README.md` | 351 | ✅ Current |
