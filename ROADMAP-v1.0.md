# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, crypto stack, full network stack,
multicore OS, and comprehensive documentation — that feels complete and
cohesive as a v1.0 release.

**Current state (Feb 2026):** BIOS (265 dict entries, 9,895 lines ASM),
KDOS v1.1 (3,850 lines), Emulator (2,541 lines + 546-line quad-core SoC +
1,929-line C++ accelerator), FPGA RTL (14 Verilog modules + 9 testbenches),
devices.py (1,418 lines), 3,112 test methods passing in 23 s (CPython + C++).
Branch: `features/cpp-accelerator`.

Core subsystems — BIOS Forth, KDOS kernel, filesystem, tile engine,
scheduler, pipelines, disk I/O, BIOS FSLOAD auto-boot — are
**functionally complete**.  Foundation layer (items 1–4) and crypto
accelerators (items 5–6) are done.  Remaining work: KDOS crypto words,
filesystem encryption, full network stack (Ethernet → TLS 1.3 → sockets),
multicore OS, and application-level features.

---

## Completed Work

### BIOS v1.0 — ✅ DONE

247 dictionary entries, 9,379 lines ASM, ~22 KB binary.

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

- ✅ megapad64.py: Full CPU emulation (2,541 lines, incl. extended tile, FP16/BF16)
- ✅ accel/mp64_accel.cpp: C++ CPU core via pybind11 (1,929 lines, 63× speedup)
- ✅ accel_wrapper.py: Drop-in wrapper for C++ CPU (829 lines)
- ✅ system.py: Quad-core SoC — UART, timer, storage, NIC, mailbox IPI, spinlocks, `run_batch()` (546 lines)
- ✅ asm.py: Two-pass assembler (788 lines), SKIP instruction
- ✅ cli.py: Interactive monitor/debugger (995 lines)
- ✅ diskutil.py: Filesystem tooling (1,039 lines)
- ✅ devices.py: MMIO peripherals including CRC engine, AES, SHA3 (1,418 lines)

### Test Suite — ✅ 3,112 tests

- TestBIOS: 128, TestBIOSHardening: 12, TestMulticore: 17
- TestKDOS: 229, TestKDOSAllocator: 242, TestKDOSExceptions: 237
- TestKDOSCRC: 237, TestKDOSDiagnostics: 231, TestKDOSAES: 238
- TestKDOSSHA3: 239, TestKDOSCrypto: 239, TestKDOSHardening: 241
- TestKDOSFilesystem: 244, TestKDOSFileCrypto: 237
- TestPipelineBundles: 242, TestKDOSMulticore: 19
- TestDiskUtil: 19, TestAssemblerBranchRange: 11
- TestNIC: 11, TestSystemMMIO: 3, TestUART: 3, TestStorage: 2,
  TestTimer: 1, TestDeviceBus: 2
- TestExtendedTile: ~670+ (saturating, rounding, FP16/BF16, strided/2D,
  SHUFFLE, PACK, UNPACK, RROT, CRC, BIST, tile self-test, perf counters)
- test_megapad64.py: 23 CPU + tile tests

### FPGA RTL — ✅ DONE (full ISA + extended tile + multicore)

14 Verilog modules in `fpga/rtl/`, 9 testbenches, 137 hardware tests passing.

- ✅ mp64_cpu.v — Full ISA + 2-stage pipeline (IF+DEX) with I-cache interface
- ✅ mp64_soc.v — Quad-core SoC top-level (bus arbiter, MMIO, IPI wiring)
- ✅ mp64_bus.v — Round-robin bus arbiter with per-core QoS
- ✅ mp64_mailbox.v — Inter-core mailbox + spinlocks (CSR + MMIO dual-path)
- ✅ mp64_tile.v — Full tile engine (TALU, TMUL, TRED, TSYS + extended ops,
  saturating, rounding, SHUFFLE, PACK, UNPACK, RROT, VSHR, VSHL, VCLZ,
  LOAD2D, STORE2D)
- ✅ mp64_fp16_alu.v — FP16/BF16 half-precision tile operations
- ✅ mp64_icache.v — Per-core 4 KiB direct-mapped instruction cache (256×16B lines)
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

### Foundation (Items 1–4) — ✅ DONE

1. ✅ **Memory allocator** — `ALLOCATE`, `FREE`, `RESIZE`, `HEAP-SETUP`,
   `.HEAP` (commit `4d69ab9`, 13 tests)
2. ✅ **CATCH/THROW** — exception handling, nested catch, re-throw
   (commit `c505f8d`, 8 tests)
3. ✅ **CRC integration** — `CRC-BUF`, `CRC32-BUF`, `CRC32C-BUF`,
   `CRC64-BUF` KDOS convenience words (commit `da56135`, 8 tests)
4. ✅ **Hardware diagnostics** — `.DIAG`, `BIST-REPORT`, `TILE-REPORT`,
   `.PERF`, live test monitor infrastructure (commit `a9b353c`)

---

### Layer 1: Crypto Stack (Items 5–8)

5. ✅ **Emulator support (AES)** — `AESDevice` in `devices.py`: full
   AES-256-GCM with S-box, key expansion, CTR-mode encryption,
   GHASH/GCM tag generation and verification.  10 BIOS words:
   `AES-KEY!`, `AES-IV!`, `AES-AAD-LEN!`, `AES-DATA-LEN!`, `AES-CMD!`,
   `AES-STATUS@`, `AES-DIN!`, `AES-DOUT@`, `AES-TAG@`, `AES-TAG!`.
   KDOS §1.5: `AES-ENCRYPT`, `AES-DECRYPT`, `.AES-STATUS`.
   (commit `c77c77f`, 9 tests)

6. ✅ **SHA-3 (Keccak-256)** — `SHA3Device` in `devices.py`: full
   Keccak-f[1600] permutation, byte-streaming absorb, SHA3-256 with
   rate=136.  4 BIOS words: `SHA3-INIT`, `SHA3-UPDATE`, `SHA3-FINAL`,
   `SHA3-STATUS@`.  KDOS §1.6: `SHA3`, `.SHA3-STATUS`, `.SHA3`.
   (commit `82548db`, 10 tests)

7. ✅ **KDOS crypto words** — `HASH`, `HMAC`, `ENCRYPT`, `DECRYPT`, `VERIFY`
   (commit `d77db63`, 10 tests)

8. ✅ **Filesystem encryption** — `FENCRYPT`, `FDECRYPT`, `FS-KEY!`, `ENCRYPTED?`
   (commit `463cac6`, 8 tests)

---

### Layer 2: Network Stack (Items 9–18)

Building bottom-up; the crypto accelerators make this genuinely useful.
Each protocol item is large enough to warrant **multiple commits** — the
sub-commit plan below ensures continuous progress and test coverage at
every step.

9.  ☐ **Ethernet framing** — MAC address, EtherType parsing/generation
    - 9a. KDOS constants + frame buffer layout (EtherType, MAC addrs)
    - 9b. `ETH-BUILD` / `ETH-PARSE` BIOS words + KDOS wrappers
    - 9c. NIC TX integration: `ETH-SEND` writes frame via NIC DMA
    - 9d. NIC RX integration: `ETH-RECV` reads frame from NIC ring buffer

10. ☐ **ARP** — address resolution (small table, ~8 entries)
    - 10a. ARP table data structure + `ARP-LOOKUP` / `ARP-INSERT`
    - 10b. ARP request/reply frame build+parse, `ARP-RESOLVE`
    - 10c. ARP responder: auto-reply to incoming ARP requests

11. ☐ **IPv4** — minimal: header build/parse, checksum, no fragmentation
    - 11a. IP header struct, `IP-BUILD` / `IP-PARSE`, HW-CRC checksum
    - 11b. `IP-SEND` — ARP-resolve → Ethernet-frame → NIC TX
    - 11c. `IP-RECV` — demux incoming Ethernet frames by EtherType

12. ☐ **ICMP** — ping reply (essential for diagnostics)
    - 12a. ICMP echo-request / echo-reply parse+build
    - 12b. Auto-responder: incoming ping → automatic pong

13. ☐ **UDP** — connectionless datagrams
    - 13a. UDP header build/parse, checksum (pseudo-header)
    - 13b. `UDP-SEND` / `UDP-RECV` words, port demux table

14. ☐ **DHCP client** — auto-configure IP/mask/gateway
    - 14a. DHCP DISCOVER/OFFER/REQUEST/ACK state machine
    - 14b. `DHCP-START` word, auto-configure on boot

15. ☐ **DNS client** — name resolution
    - 15a. DNS query builder (A record), response parser
    - 15b. `DNS-RESOLVE` ( c-addr len -- ip ) word

16. ☐ **TCP** — connection-oriented streams (largest item)
    - 16a. TCB (Transmission Control Block) data structure + state enum
    - 16b. TCP header build/parse, sequence number handling
    - 16c. 3-way handshake: SYN → SYN-ACK → ACK (active open)
    - 16d. Data TX: segmentation, `TCP-SEND`, retransmit timer
    - 16e. Data RX: reassembly, `TCP-RECV`, ACK generation
    - 16f. Connection teardown: FIN/FIN-ACK, TIME_WAIT
    - 16g. Sliding window + congestion control (basic)

17. ☐ **TLS 1.3** — AES-256-GCM + SHA-3 for HMAC/key derivation
    - 17a. HKDF-Extract / HKDF-Expand using SHA-3 HMAC
    - 17b. TLS record layer: content type, length, encryption
    - 17c. Handshake: ClientHello → ServerHello → key schedule
    - 17d. Application data encrypt/decrypt via AES-256-GCM
    - 17e. `TLS-CONNECT` / `TLS-SEND` / `TLS-RECV` words

18. ☐ **Socket API** — unified interface over TCP/UDP
    - 18a. Socket descriptor table, `SOCKET` / `CLOSE`
    - 18b. `BIND` / `LISTEN` / `ACCEPT` (TCP server)
    - 18c. `CONNECT` / `SEND` / `RECV` (TCP client + UDP)

---

### Layer 3: Multi-Core OS (Items 19–24)

19. ☐ **Per-core run queues** — each core has its own task list
20. ☐ **Work stealing** — idle cores pull from busy cores' queues
21. ☐ **Core-affinity** — pin tasks to specific cores
22. ☐ **Per-core preemption** — timer IRQ on all cores, not just core 0
23. ☐ **IPI messaging** — use mailbox for structured inter-core messages
    (not just wake-up)
24. ☐ **Shared resource locks** — dictionary lock, UART lock, filesystem
    lock (currently unprotected)

---

### Layer 4: Application-Level (Items 25–30)

25. ☐ **Outbound data** — `NET-SEND` integration, `PORT-SEND` to
    transmit buffer data
26. ☐ **FP16 tile mode** — expose `FP16-MODE` for ML/signal processing
    workloads
27. ☐ **QoS** — bus bandwidth allocation per core
28. ☐ **Editor** — simple line/screen editor for writing Forth on-device
29. ☐ **Scripting** — `AUTOEXEC` file loaded at boot, cron-like
    scheduled tasks
30. ☐ **Remote REPL** — UART or TCP-based remote Forth session

---

## Implementation Order

```
Layer 0  Items  1– 4  Foundation (allocator, exceptions, CRC, diag) ✅ DONE
Layer 1  Items  5– 8  Crypto Stack (AES ✅, SHA-3 ✅, crypto words, FS encrypt)
Layer 2  Items  9–18  Network Stack (Ethernet → ARP → IP → ICMP → UDP →
                      DHCP → DNS → TCP → TLS 1.3 → Socket API)
                      ~35 sub-commits across 10 protocol items
Layer 3  Items 19–24  Multi-Core OS (run queues, work stealing, affinity,
                      preemption, IPI, locks)
Layer 4  Items 25–30  Application-Level (net send, FP16, QoS, editor,
                      scripting, remote REPL)
```

Each item is committed individually with its own test class and run via
`make test-bg K=TestClassName` + `make test-status`.  Layer 2 is
strictly bottom-up — each protocol builds on the one below it.
**Layer 2 items are large enough that each is broken into multiple
sub-commits** (a–d typically), each independently tested.  This ensures
continuous progress, reviewable diffs, and a working system at every step.

---

## File Summary

| File | Lines | Status |
|------|-------|--------|
| `bios.asm` | 9,895 | ✅ 265 dictionary entries |
| `kdos.f` | 3,850 | ✅ KDOS definitions + §1.1–§1.7 + §7.6.1 crypto |
| `megapad64.py` | 2,541 | ✅ Full CPU + extended tile + FP16/BF16 |
| `accel/mp64_accel.cpp` | 1,929 | ✅ C++ CPU core (pybind11, 63× speedup) |
| `accel_wrapper.py` | 829 | ✅ Drop-in wrapper for C++ CPU core |
| `system.py` | 546 | ✅ Quad-core SoC + `run_batch()` C++ fast path |
| `cli.py` | 995 | ✅ Interactive monitor/debugger |
| `asm.py` | 788 | ✅ Two-pass assembler |
| `devices.py` | 1,418 | ✅ AES-256-GCM, SHA3, CRC, Mailbox, Spinlock |
| `diskutil.py` | 1,039 | ✅ MP64FS tooling |
| `test_megapad64.py` | 2,193 | 23 tests ✅ |
| `test_system.py` | 7,270 | 3,089 test methods (24 classes) ✅ |
| `setup_accel.py` | 35 | ✅ pybind11 build configuration |
| `bench_accel.py` | 139 | ✅ C++ vs Python speed comparison |
| `Makefile` | 159 | ✅ Build, test, & accel targets |
| `conftest.py` | 159 | ✅ Test fixtures, snapshot caching, live status |
| `sample.img` | — | Built by diskutil.py ✅ |
| `fpga/rtl/` | ~8,200 | ✅ 14 Verilog modules |
| `fpga/sim/` | ~4,500 | ✅ 9 testbenches (137 HW tests) |
| `docs/` | 9 files | ✅ Written |
| `README.md` | 350 | ✅ Current |
