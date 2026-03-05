# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, crypto stack, full network stack,
multicore OS, and comprehensive documentation — that feels complete and
cohesive as a v1.0 release.

**Current state (Feb 2026):** BIOS (355 dict entries, 14,353 lines ASM),
KDOS v1.1 (11,004 lines, 923 colon defs, 707 vars/constants), Emulator
(2,671 lines + 849-line 16-core heterogeneous SoC + 1,978-line C++ accelerator),
FPGA RTL (27 Verilog modules + 18 testbenches, ~180 HW tests),
devices.py (2,066 lines, 17 device classes), 1,539 test methods passing
(CPython + C++).  Branch: `main`.

Core subsystems — BIOS Forth, KDOS kernel, filesystem, tile engine,
scheduler, pipelines, disk I/O, BIOS FSLOAD auto-boot — are
**functionally complete**.  Foundation (items 1–4), crypto stack
(items 5–8), full network stack L2–L7 including TLS 1.3 and socket API
(items 9–18), multicore OS (items 19–24), and field ALU / post-quantum
crypto (items 34–38) are done.  Crypto enhanced with hardware TRNG,
SHAKE XOF support, Field ALU (general GF(p) coprocessor), NTT engine,
and ML-KEM-512 with hybrid PQ key exchange.  Real-network testing
infrastructure added (TAP device backends, 38 integration tests).
TCP fully implemented (16–256 dynamic TCB slots with TIME_WAIT reaper,
3-way handshake, sliding window, congestion control, retransmit, graceful
close, 37 tests).  TLS 1.3
fully implemented (HKDF, record layer, handshake, app data).  Socket
API done (8 words, TCP+UDP).  Post-quantum: ML-KEM-512 keygen/encaps/
decaps + hybrid X25519+ML-KEM exchange with HKDF key derivation.
Remaining work: application-level features (items 25–30).

---

## Completed Work

### BIOS v1.0 — ✅ DONE

355 dictionary entries, 14,353 lines ASM, ~28 KB binary.

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
- ✅ **SHA-3/SHAKE**: SHA3-INIT, SHA3-UPDATE, SHA3-FINAL, SHA3-STATUS@,
  SHA3-MODE!, SHA3-MODE@, SHA3-SQUEEZE (7 words, 4 modes: SHA3-256/512,
  SHAKE128/256)
- ✅ **Hardware TRNG**: RANDOM, RANDOM8, SEED-RNG (3 words, CSPRNG-backed
  in emulator, ring-oscillator + SHA-3 conditioner on FPGA)
- ✅ **Multicore**: COREID, NCORES, IPI-SEND, IPI-STATUS, IPI-ACK, MBOX!,
  MBOX@, SPIN@, SPIN!, WAKE-CORE, CORE-STATUS (11 words)
- ✅ **Micro-cluster / HBW**: CLUSTER-EN!, CLUSTER-EN@, BARRIER-ARRIVE,
  BARRIER-STATUS, SPAD, HBW-BASE, HBW-SIZE, N-FULL, MICRO? (9 words)
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
- ✅ **Field ALU**: FADD, FSUB, FMUL, FSQR, FINV, FPOW, FMUL-RAW,
  FIELD-A!, FIELD-B!, FIELD-CMD!, FIELD-STATUS@, FIELD-RESULT@,
  FIELD-RESULT-HI@ (13 words)
- ✅ **NTT Engine**: NTT-LOAD, NTT-STORE, NTT-FWD, NTT-INV, NTT-PMUL,
  NTT-PADD, NTT-SETQ, NTT-STATUS@, NTT-WAIT (9 words)
- ✅ **KEM Engine**: KEM-KEYGEN, KEM-ENCAPS, KEM-DECAPS, KEM-SETQ,
  KEM-STATUS@, KEM-PK@, KEM-CT@ (7 words)

### KDOS v1.1 — ✅ DONE (core + multicore + crypto + network + PQC)

923 colon definitions + 707 variables/constants/creates, 11,004 lines.

19 sections:
- §1 Utility words (§1.1–§1.13: buffer, AES, SHA3, TRNG, X25519, HKDF,
  Field ALU, NTT, ML-KEM-512, Hybrid PQ Exchange, HBW allocator)
- §2 Buffer subsystem, §3 Tile-aware buffer ops
- §4 Kernel registry, §5 Sample kernels (12 kernels including kadd,
  knorm, khistogram, kpeak, kconvolve, etc.)
- §6 Pipeline engine, §7 Storage & persistence
- §7.5–7.8 Filesystem (MP64FS), documentation browser, dictionary search
- §8 Scheduler & tasks, §8.8 Micro-cluster dispatch, §9 Interactive screens (9-tab TUI)
- §10 Data ports (NIC ingestion), §11 Benchmarking
- §12 Dashboard, §13 Help system, §14 Startup
- §15 Pipeline bundles (versioned, declarative config format)
- §8.1 Multicore dispatch (CORE-RUN, CORE-WAIT, BARRIER, LOCK/UNLOCK, P.RUN-PAR)
- §16 Network Stack (Ethernet, ARP, IPv4, ICMP, UDP, DHCP, DNS, TCP, TLS 1.3)
- §17 Socket API (SOCKET, BIND, LISTEN, ACCEPT, CONNECT, SEND, RECV, CLOSE)

### Filesystem — ✅ DONE

- ✅ MP64FS: superblock, bitmap, 128-entry directory, data sectors
- ✅ diskutil.py: build_image, build_sample_image, inject/read/delete/list
- ✅ sample.img: KDOS + 10 docs + 5 tutorials + demo-data + demo-bundle (18 files)
- ✅ KDOS words: DIR, CATALOG, CAT, RENAME, FS-FREE, SAVE-BUFFER, LOAD,
  MKFILE, RMFILE, FORMAT, FIND-BY-NAME, FS-LOAD
- ✅ BIOS FSLOAD for disk-only boot

### Emulator & Tools — ✅ DONE

- ✅ megapad64.py: Full CPU emulation (2,671 lines, incl. extended tile, FP16/BF16, micro-core variant)
- ✅ accel/mp64_accel.cpp: C++ CPU core via pybind11 (1,978 lines, 63× speedup)
- ✅ accel_wrapper.py: Drop-in wrapper for C++ CPU (840 lines)
- ✅ system.py: 16-core heterogeneous SoC — 4 full + 3×4 micro-clusters, HBW math RAM, UART, timer, storage, NIC, mailbox IPI, spinlocks, TRNG, `run_batch()` (849 lines)
- ✅ asm.py: Two-pass assembler (788 lines), SKIP instruction
- ✅ cli.py: Interactive monitor/debugger (995 lines)
- ✅ diskutil.py: Filesystem tooling (1,039 lines)
- ✅ devices.py: MMIO peripherals — CRC, AES-256-GCM, SHA3/SHAKE, TRNG, Field ALU, NTT, KEM (2,066 lines, 17 device classes)

### Test Suite — ✅ 1,539 tests

- TestBIOS: 128, TestBIOSHardening: 12, TestMulticore: 17
- TestKDOS: 229, TestKDOSAllocator: 13, TestKDOSExceptions: 8
- TestKDOSCRC: 8, TestKDOSDiagnostics: 7, TestKDOSAES: 9
- TestKDOSSHA3: 10, TestKDOSSHAKE: 6, TestKDOSTRNG: 8
- TestKDOSCrypto: 10, TestKDOSHardening: 12
- TestKDOSFilesystem: 15, TestKDOSFileCrypto: 8
- TestPipelineBundles: 13, TestKDOSMulticore: 79
- TestKDOSNetStack: 161
- TestSQuote: 5, TestKDOSHKDF: 7, TestKDOSTLSRecord: 7
- TestKDOSTLSHandshake: 8, TestKDOSTLSAppData: 7, TestKDOSSocket: 8
- TestFieldALU: 15, TestNTT: 12, TestMLKEM: 11, TestPQExchange: 7
- TestNetHardening: 24
- TestMicroCluster: 16, TestHBWMemory: 14
- TestDiskUtil: 19, TestAssemblerBranchRange: 11
- TestNIC: 11, TestSystemMMIO: 3, TestUART: 3, TestStorage: 2,
  TestTimer: 1, TestDeviceBus: 2
- test_megapad64.py: 23 CPU + tile tests
- test_networking.py: 38 real-network tests (NIC backends, TAP,
  ARP, ICMP, UDP, TCP) across 8 test classes

### RTL — ✅ DONE (full ISA + extended tile + multicore + PQC + SoC)

31 portable Verilog modules in `rtl/` + 12 target overrides (Xilinx-7 + ASIC stubs),
29 testbenches, ~419 hardware assertions passing.
~26,300 lines RTL + ~11,300 lines testbench.

- ✅ mp64_cpu.v — Full ISA + 2-stage pipeline (IF+DEX) with I-cache interface
- ✅ mp64_soc.v — Full SoC integration (903 lines): 4 CPU cores + I-caches,
  3 micro-clusters, bus arbiter, 4-bank memory, ext-mem controller,
  tile engine, MMIO decoder (12 peripherals), BIOS hex init, NIC PHY,
  SysInfo register.  All 19 module ports verified (0 mismatches).
  SoC smoke test: 5/5 PASS.
- ✅ mp64_top.v — Parameterized top-level instantiating mp64_soc with
  CLOCK_HZ, NUM_CORES, NUM_CLUSTERS, MEM_DEPTH passthrough
- ✅ mp64_bus.v — Round-robin bus arbiter with per-core QoS
- ✅ mp64_mailbox.v — Inter-core mailbox + spinlocks (CSR + MMIO dual-path)
- ✅ mp64_tile.v — Full tile engine (TALU, TMUL, TRED, TSYS + extended ops,
  saturating, rounding, SHUFFLE, PACK, UNPACK, RROT, VSHR, VSHL, VCLZ,
  LOAD2D, STORE2D)
- ✅ mp64_fp16_alu.v — FP16/BF16 half-precision tile operations
- ✅ mp64_icache.v — Per-core 4 KiB direct-mapped instruction cache (256×16B lines)
- ✅ mp64_trng.v — True Random Number Generator (ring-oscillator + LFSR
  conditioner, health monitoring, 9 unit tests)
- ✅ mp64_field_alu.v — General GF(2²⁵⁵−19) ALU: FADD/FSUB/FMUL/FSQR/FINV/FPOW
  + MUL_RAW 256×256→512-bit (11 HW tests)
- ✅ mp64_ntt.v — 256-point NTT/INTT, configurable modulus (q=3329/8380417),
  butterfly + Montgomery reduction (8 HW tests)
- ✅ mp64_kem.v — ML-KEM-512 key encapsulation: KeyGen/Encaps/Decaps
  via NTT engine + SHA-3 + TRNG (15 HW tests)
- ✅ mp64_memory.v, mp64_timer.v, mp64_uart.v, mp64_disk.v, mp64_nic.v, mp64_extmem.v
- ✅ Kintex-7 325T target (Genesys 2); est. ~145K–185K LUTs, ~420–620 DSPs

### FPGA Synthesis — 🔄 IN PROGRESS

SoC integration + Yosys synthesis pipeline established.

- ✅ SRAM decomposition: 512-bit dp/sp RAM → 8×64-bit BRAM slices
  for Yosys inference (`mp64_sram_dp_xilinx7.v`, `mp64_sram_sp_xilinx7.v`).
  Memory subsystem synthesizes to 1,024 RAMB36E1.
- ✅ BIOS hex generation: `fpga/gen_bios_hex.py` → `fpga/bios.hex`
  (3,795 × 64-bit words for Bank 0 SRAM init)
- ✅ ASIC dp stub port fix: `mp64_sram_dp_asic.v` asymmetric interface
- ✅ NIC async reset fix: `data_window` register block converted to
  sync reset to avoid Yosys PROC_ARST error on unpacked arrays
  (30/30 NIC tests pass, RX/TX FSMs unchanged)
- ✅ IVerilog clean compile of full SoC (28 source files, 0 warnings)
- ✅ Yosys SoC synth script (`fpga/synth_yosys_soc.tcl`, NIC blackboxed)
- ☐ Full Yosys synthesis completion (field_alu 256-bit reduction is
  bottleneck; PROC_MUX pass generates enormous mux trees for
  `field_reduce_p256`/`field_reduce_secp` combinational logic)
- ☐ Vivado/nextpnr place & route for timing closure

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

6. ✅ **SHA-3 / SHAKE / TRNG** — `SHA3Device` in `devices.py`: full
   Keccak-f[1600] permutation, 4 modes (SHA3-256, SHA3-512, SHAKE128,
   SHAKE256), XOF squeeze support.  `TRNGDevice`: hardware CSPRNG
   backed by `os.urandom()`, MMIO at 0x0800.  7 BIOS words:
   `SHA3-INIT`, `SHA3-UPDATE`, `SHA3-FINAL`, `SHA3-STATUS@`,
   `SHA3-MODE!`, `SHA3-MODE@`, `SHA3-SQUEEZE`.  3 TRNG words:
   `RANDOM`, `RANDOM8`, `SEED-RNG`.  KDOS §1.6: `SHA3`, `SHAKE128`,
   `SHAKE256`, `RANDOM32`, `RANDOM16`, `RAND-RANGE`, `.SHA3-STATUS`,
   `.SHA3`.  FPGA: `mp64_trng.v` with ring-oscillator entropy +
   conditioned pool + health monitoring (9 HW tests).
   (commits `82548db`..`pending`, 24 tests)

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

9.  ✅ **Ethernet framing** — MAC address, EtherType parsing/generation
    - 9a. KDOS constants + frame buffer layout (EtherType, MAC addrs)
    - 9b. `ETH-BUILD` / `ETH-PARSE` BIOS words + KDOS wrappers
    - 9c. NIC TX integration: `ETH-SEND` writes frame via NIC DMA
    - 9d. NIC RX integration: `ETH-RECV` reads frame from NIC ring buffer

10. ✅ **ARP** — address resolution (small table, ~8 entries)
    - 10a. ARP table data structure + `ARP-LOOKUP` / `ARP-INSERT`
    - 10b. ARP request/reply frame build+parse, `ARP-RESOLVE`
    - 10c. ARP responder: auto-reply to incoming ARP requests

11. ✅ **IPv4** — minimal: header build/parse, checksum, no fragmentation
    - 11a. IP header struct, `IP-BUILD` / `IP-PARSE`, HW-CRC checksum
    - 11b. `IP-SEND` — ARP-resolve → Ethernet-frame → NIC TX
    - 11c. `IP-RECV` — demux incoming Ethernet frames by EtherType

12. ✅ **ICMP** — ping reply (essential for diagnostics)
    - 12a. ICMP echo-request / echo-reply parse+build
    - 12b. Auto-responder: incoming ping → automatic pong

13. ✅ **UDP** — connectionless datagrams
    - 13a. UDP header build/parse, checksum (pseudo-header)
    - 13b. `UDP-SEND` / `UDP-RECV` words, port demux table

14. ✅ **DHCP client** — auto-configure IP/mask/gateway
    - 14a. DHCP DISCOVER/OFFER/REQUEST/ACK state machine
    - 14b. `DHCP-START` word, auto-configure on boot

15. ✅ **DNS client** — name resolution
    - 15a. DNS query builder (A record), response parser
    - 15b. `DNS-RESOLVE` ( c-addr len -- ip ) word

16. ✅ **TCP** — connection-oriented streams (32 tests)
    - 16a. ✅ TCB (Transmission Control Block) data structure + 11-state enum
    - 16b. ✅ TCP header build/parse, sequence number handling, checksums
    - 16c. ✅ 3-way handshake: `TCP-CONNECT` (active), `TCP-LISTEN` (passive)
    - 16d. ✅ Data TX: `TCP-SEND`, MSS segmentation, retransmit timer
    - 16e. ✅ Data RX: `TCP-RECV`, `TCP-RX-PUSH`/`TCP-RX-POP`, ACK generation
    - 16f. ✅ Connection teardown: `TCP-CLOSE`, FIN/FIN-ACK, TIME_WAIT
    - 16g. ✅ Sliding window (`CWND`/`SSTHRESH`) + congestion control

17. ✅ **TLS 1.3** — AES-256-GCM + SHA-3 for HMAC/key derivation (37 tests)
    - 17a. ✅ HKDF-Extract / HKDF-Expand using SHA-3 HMAC
    - 17b. ✅ TLS record layer: content type, length, encryption
    - 17c. ✅ Handshake: ClientHello → ServerHello → key schedule
    - 17d. ✅ Application data encrypt/decrypt via AES-256-GCM
    - 17e. ✅ `TLS-CONNECT` / `TLS-SEND` / `TLS-RECV` / `TLS-CLOSE` words

18. ✅ **Socket API** — unified interface over TCP/UDP (8 tests)
    - 18a. ✅ Socket descriptor table, `SOCKET` / `CLOSE`
    - 18b. ✅ `BIND` / `LISTEN` / `ACCEPT` (TCP server)
    - 18c. ✅ `CONNECT` / `SEND` / `RECV` (TCP client + UDP)

---

### Layer 5: Field ALU & Post-Quantum Crypto (Items 34–38)

Promotes the existing X25519 block into a general GF(p) coprocessor
and adds post-quantum cryptographic primitives.

34. ✅ **Field ALU** — General GF(2²⁵⁵−19) ALU with raw 256×256→512-bit
    multiply (15 tests, 11 HW tests)
    - 34a. ✅ RTL: `mp64_field_alu.v` — modes 0–7 (X25519, FADD, FSUB,
           FMUL, FSQR, FINV, FPOW, MUL_RAW). 488 lines.
    - 34b. ✅ RTL: RESULT_HI read register, MMIO at 0x0880.
    - 34c. ✅ Emulator: `FieldALUDevice` in devices.py, backward-compatible.
    - 34d. ✅ BIOS words: 13 new words (FADD–FIELD-WAIT).
    - 34e. ✅ KDOS §1.10: FADD, FSUB, FMUL, FSQR, FINV, FPOW, FMUL-RAW.
    - 34f. ✅ Tests: TestFieldALU (15 tests).

35. ✅ **NTT Engine** — 256-point NTT/INTT, configurable modulus
    (q=3329 / q=8380417) (12 tests, 8 HW tests)
    - 35a. ✅ RTL: `mp64_ntt.v` — Cooley-Tukey butterfly, Montgomery
           reduction. MMIO base 0x8C0. 443 lines.
    - 35b. ✅ RTL: Wired into mp64_soc.v, defines in mp64_defs.vh.
    - 35c. ✅ Emulator: `NTTDevice` in devices.py, Python NTT.
    - 35d. ✅ BIOS words: 9 new words (NTT-LOAD–NTT-WAIT).
    - 35e. ✅ KDOS §1.11: NTT-POLYMUL convenience word.
    - 35f. ✅ Tests: TestNTT (12 tests).

36. ✅ **SHA-3 SHAKE Streaming** — XOF auto-squeeze for SPHINCS+ speedup
    - 36a. ✅ RTL: SQUEEZE_NEXT command in mp64_sha3.v.
    - 36b. ✅ Emulator: squeeze_next support in SHA3Device.
    - 36c. ✅ BIOS word: SHA3-SQUEEZE-NEXT.
    - 36d. ✅ KDOS: SHAKE-STREAM helper.
    - 36e. ✅ Tests: covered in TestKDOSSHAKE.

37. ✅ **ML-KEM-512 (Kyber)** — Key encapsulation using NTT engine
    (11 tests, 15 HW tests)
    - 37a. ✅ RTL: `mp64_kem.v` — KeyGen/Encaps/Decaps via NTT+SHA3+TRNG.
           MMIO base 0x0940. 337 lines.
    - 37b. ✅ Emulator: `KEMDevice` in devices.py.
    - 37c. ✅ BIOS words: 7 words (KEM-KEYGEN–KEM-CT@).
    - 37d. ✅ KDOS §1.12: KYBER-KEYGEN, KYBER-ENCAPS, KYBER-DECAPS.
    - 37e. ✅ Tests: TestMLKEM (11 tests).

38. ✅ **Hybrid PQ Key Exchange** — X25519 + ML-KEM combined (7 tests)
    - 38a. ✅ KDOS §1.13: PQ-EXCHANGE (X25519 + ML-KEM + HKDF derivation).
    - 38b. ✅ Tests: TestPQExchange (7 tests).

---

### Layer 3: Multi-Core OS (Items 19–24)

19. ✅ **Per-core run queues** — each core has its own task list
20. ✅ **Work stealing** — idle cores pull from busy cores' queues
21. ✅ **Core-affinity** — pin tasks to specific cores
22. ✅ **Per-core preemption** — timer IRQ on all cores, not just core 0
23. ✅ **IPI messaging** — use mailbox for structured inter-core messages
    (not just wake-up)
24. ✅ **Shared resource locks** — dictionary lock, UART lock, filesystem
    lock (currently unprotected)

---

### Layer 4: Application-Level (Items 25–30)

25. ☐ **Outbound data** — `NET-SEND` integration, `PORT-SEND` to
    transmit buffer data
26. ☐ **FP16 tile mode** — expose `FP16-MODE` for ML/signal processing
    workloads
27. ✅ **QoS** — per-port bus bandwidth weights implemented in
    `mp64_bus.v` (weighted round-robin, `qos_weight[]`, `qos_bwlimit[]`
    CSRs); absorbed into Items 39/41.
28. ☐ **Editor** — simple line/screen editor for writing Forth on-device
29. ☐ **Scripting** — `AUTOEXEC` file loaded at boot, cron-like
    scheduled tasks
30. ☐ **Remote REPL** — UART or TCP-based remote Forth session

45. ☐ **SCROLL** — Socket Client for Remote Object Retrieval Over
    Links.  Multi-protocol resource fetcher: given a URL, resolve,
    connect, transfer, and deliver the payload to a KDOS buffer, the
    disk, or directly to the evaluator.  Protocols: HTTP/1.1 (TCP),
    TFTP (UDP), Gopher; HTTPS piggybacks the existing `TLS-CONNECT`.
    Effectively a `curl(1)` for KDOS — and crucially enables network
    package loading: `" http://server/pkg.f" SCROLL-LOAD` fetches
    Forth source and EVALUATEs it, enabling over-LAN firmware update,
    remote doc browsing, and package distribution.  **High priority.**
    - 45a. KDOS §18 SCROLL core: URL parser + protocol dispatch
           (`http://`, `gopher://`, `tftp://`)
    - 45b. HTTP/1.1 client: `GET /path HTTP/1.1` over TCP, response
           header parse, chunked / Content-Length body
    - 45c. TFTP client: RRQ, DATA/ACK loop, timeout + retry (UDP)
    - 45d. Gopher client: type-0 (text) + type-1 (menu) selector fetch
    - 45e. `SCROLL-GET ( url -- buf len )` — fetch to RAM buffer
    - 45f. `SCROLL-SAVE ( url file -- )` — fetch and write to MP64FS
    - 45g. `SCROLL-LOAD ( url -- )` — fetch Forth source, EVALUATE
           (chain-load: remote packages, firmware, config)
    - 45h. Emulator: mock HTTP + TFTP loopback fixture in conftest.py
    - 45i. Tests: `TestSCROLL` — HTTP GET, TFTP RRQ, Gopher,
           SCROLL-LOAD evaluate round-trip

---

### Known Issues / Investigation

31. ✅ **CLI boot performance** — FIXED.  C++ accelerator is properly
    engaged in the CLI path; boot to KDOS prompt is ~2–3 s with accel
    built.  Stale-KDOS issue resolved (image rebuild via `make disk`).

32. ✅ **Real-world networking hardening** — comprehensive edge-case,
    stress, and robustness testing for the full networking stack:
    - 32a. ✅ `PING` / `PING-IP` commands — ARP-resolve target, send
      ICMP echo request, poll for reply, print result.  `NEXT-HOP`
      subnet routing with GW-IP zero-check fallback.  `.IP` formatting.
    - 32b. ✅ Outbound connectivity validation — ARP → IP → ICMP/UDP
      full round-trip verified via TAP integration tests.
    - 32c. ✅ Stress / robustness — 24 unit tests covering truncated
      IP headers, IP version≠4, bad IHL, TTL=0, zero-length payloads,
      oversized/runt/empty frames, IP fragment flags, bad UDP/TCP
      checksums, ARP insert+lookup, rapid 20-frame burst, 30-frame
      broadcast storm, TCP SYN flood (10 SYNs), DNS wrong-ID, ICMP
      non-echo types, mixed protocol burst, all-zeros/all-0xFF frames,
      truncated TCP/UDP headers, NEXT-HOP unconfigured gateway.
    - 32d. ✅ TAP integration tests — 12 tests: PING-IP outbound,
      rapid ARP/PING poll (100×), broadcast storm drain, full UDP
      roundtrip, ARP→ICMP→UDP sequence, IP-RECV noise resilience,
      NET-STATUS after heavy traffic, TCP-INIT/TCP-POLL on live TAP.

33. ✅ **BIOS `.'` delimiter bug** — FIXED. Added `inc r13` before
    `dq_interp_loop` and `dq_scan` in `w_dotquote` to skip the
    delimiter space (ANS Forth compliant).  `S"` already had the fix.
    All 661 `."` strings in KDOS and 348 in tests were mechanically
    updated to add an explicit leading space, preserving exact output.

43. ✅ **Display: screen 8 exits to RPL** — DONE.  Root cause: stack
    imbalance in `.CORE-ROW` — IF branch (core == self) never consumed `i`,
    leaked value corrupted `TUI-LIST` loop causing `EXECUTE` to jump to
    address 0x0000 (BIOS boot entry), wiping stacks and re-entering RPL.
    Fix: `DROP` in IF branch.  Only triggered with `--cores > 1` because
    `.CORE-ROW` is only called when `NCORES > 1`.

    Also factored `SCREENS` into `SCREEN-LOOP` + `SCREEN ( n -- )`;
    added `[full]`/`[mu]` type labels to `.CORE-ROW`; made core-type
    detection fully dynamic via new SysInfo `NUM_FULL` register at offset
    `0x48` — BIOS `N-FULL` and `MICRO?` now read from SysInfo instead of
    hardcoding threshold to 4.  KDOS `MICRO-CORE?` / `FULL-CORE?` use
    `N-FULL` so they adapt to any core configuration.

44. ✅ **CLI: add `--clusters` flag, uncap `--cores`** — DONE.  Removed
    `choices=[1,2,3,4]` cap from `--cores`, added `--clusters CLUSTERS`
    argument (type=int, default=0, max 3), wired `num_clusters` through
    to `MegapadSystem`.  `test_screen_header_tabs` updated to verify
    `[8]Core` tab.

45. ☐ **Emulator timing robustness** — The emulated CPU has no
    wall-clock time model; network polling loops (ARP, PING, DHCP, DNS,
    TCP) currently use a busy-wait `NET-IDLE` word (200× `NET-RX?`
    polls) to yield enough real time for TAP replies to arrive.  This is
    fragile: too few iterations → timeouts on slow hosts; too many →
    tests hit `max_steps`.  A proper fix would be one of:
    - A `MS` word backed by a wall-clock timer (e.g. `CYCLES`-based
      with a known cycles-per-µs ratio, or a new MMIO real-time clock).
    - IDL-based sleep with NIC RX as a wake source (partially
      implemented in system.py but breaks test harness assumptions).
    - A hybrid: IDL with a timer-IRQ deadline so the CPU wakes on
      whichever comes first (NIC RX or timeout expiry).

46. ☐ **USB controller** — Add a USB host/device peripheral to the
    system.  MMIO slot at `0x0B00` (1,280 bytes free from `0xB00` to
    `0xFFF`).  Recommended phased approach:

    **Phase 1 — Emulator + BIOS + KDOS (USB 2.0 software model):**
    - `devices.py`: `USBDevice` class (~200-300 lines) with register
      file (control, status, endpoint buffers, DMA), backed by host
      `/dev/bus/usb` passthrough or virtual mass-storage device.
    - `bios.asm`: ~10-15 words (USB-INIT, USB-STATUS, USB-XFER,
      USB-EP-READ, USB-EP-WRITE, USB-SET-ADDR, USB-DESC@, etc.)
      following the existing `ldi64 r11, MMIO; ldn/str` pattern.
    - `kdos.f`: USB enumeration (SET_ADDRESS, GET_DESCRIPTOR,
      SET_CONFIGURATION), mass-storage class driver (SCSI over
      bulk-only transport), optional HID driver.
    - Estimated effort: ~1 week.

    **Phase 2 — RTL USB 2.0 (ULPI PHY):**
    - `rtl/periph/mp64_usb.v`: USB 2.0 controller with ULPI
      interface (~8K-12K LUTs).  No GTX transceivers needed —
      uses regular I/O pins via a $15 ULPI PHY Pmod (USB3300).
    - SoC wiring: `mmio_sel_usb = (bus_mmio_addr[11:8] == 4'hB)`,
      add to read-data mux in `mp64_soc.v`.
    - Genesys 2: route ULPI signals to Pmod JA/JB headers in XDC.
    - Testbench: `tb_usb.v` with enumeration + bulk transfer tests.
    - Estimated effort: ~2-3 weeks.

    **Phase 3 — RTL USB 3.0 SuperSpeed (optional, bigger FPGA):**
    - Requires GTX transceiver (5 Gbps SERDES, 8b/10b) + PIPE
      interface (~5K-10K additional LUTs) + FMC daughter card with
      USB 3.0 PHY (TUSB1310 or similar).
    - **BRAM budget is the blocker:** K7-325T has 890 RAMB36E1 but
      the 4-bank memory alone needs 1,024.  USB 3.0 endpoint FIFOs
      add ~4-8 more.  Requires either halving RAM to 2 banks (512 KiB)
      or targeting VU095 / Artix UltraScale+.
    - Software stack is identical to USB 2.0 — USB 3.0 is backward-
      compatible at the protocol level.
    - Estimated effort: ~4-6 weeks (including PHY bring-up).

    Resource budget (Kintex-7 325T):
    ```
    Resource   Available   Current est.     USB 2.0    USB 3.0
    LUTs       203,800     145K-185K        +8-12K     +20-35K
    DSPs       840         420-620          +0         +0
    BRAM36     890         1,024 (over!)    +2-4       +4-8
    GTX        16          0                +0         +1
    ```

47. ✅ **Memory management hardening** — Incremental improvements to
    the existing allocator + memory subsystem.  No architectural
    rewrites; each sub-item is a self-contained change with its own
    tests.  Goal: make long-running sessions stable, reduce
    fragmentation, add visibility, and prepare for eventual GC.

    **Phase 1 — Heap coalescing (high priority):**
    - 47a. ✅ Add adjacent-block coalescing to `FREE` in KDOS §1.1.
           After inserting the freed block into the address-sorted
           free list, check forward: if `block + /ALLOC-HDR +
           block.size == next`, merge by absorbing next's header +
           size into block.  Check backward: if `prev + /ALLOC-HDR
           + prev.size == block`, merge block into prev.  ~15 lines
           of Forth.  Add `TestKDOSAllocatorCoalesce` (alloc A B C,
           free B, free A, verify single merged block; alloc/free
           interleave stress test with `HEAP-FREE-BYTES` invariant).
    - 47b. ✅ Add `HEAP-FRAG` ( -- n ) word: walk free list, count
           number of free blocks.  Fragmentation = n − 1 when n > 0.
           Useful for diagnostics and tests.

    **Phase 2 — Dictionary reclamation (medium priority):**
    - 47c. ✅ Implement `MARKER` (ANS Forth TOOLS EXT).  `MARKER xxx`
           saves HERE + LATEST (dict head) at define time.  Executing
           `xxx` later restores both, effectively forgetting
           everything defined after it.  ~20 lines.  This covers the
           most common "load module → use → unload" pattern without
           needing GC.  Works in both system and userland dict zones.
    - 47d. ✅ Implement `FORGET` as a thin wrapper: parse next word,
           find its XT, walk dict to find corresponding HERE value,
           reset.  Simpler than `MARKER` but less safe (can corrupt
           if interleaved definitions exist).  Mark as "use MARKER
           instead" in help.

    **Phase 3 — Runtime safety (medium priority):**
    - 47e. ✅ Add heap/stack collision guard.  In `ALLOCATE`, after
           finding a block, verify its end address is below the
           current stack pointer minus a 4 KiB guard zone.  On
           violation, return OOM rather than silently corrupting.
           In the scheduler's `YIELD`, check `SP@ HEAP-BASE @ <`
           as a cheap stack-overflow canary; if tripped, print
           warning and halt the task.
    - 47f. ✅ Add `HEAP-CHECK` ( -- flag ) word: walk the free list
           validating that each block's next pointer is within
           `[HEAP-BASE, stack-guard)` and that sizes are positive.
           Returns true if the heap is consistent.  Useful for
           `.DIAG` and post-crash forensics.

    **Phase 4 — Allocator improvements (low priority):**
    - 47g. ✅ Scale the buffer registry: replace the 16-slot
           `BUF-TABLE` array with a linked-list threaded through
           each descriptor's header.  Add a `B.NEXT` field (or reuse
           the type cell's upper bits).  `BUFFERS` walks the list.
           No hard cap.
    - 47h. ✅ Add `RESIZE` in-place growth: before alloc+copy, check
           if the block is followed by a free block large enough to
           absorb the growth.  If so, merge and adjust size in place.
           Depends on 47a (coalescing) being done first.

    **Phase 5 — Unified visibility (low priority):**
    - 47i. ✅ Add `.MEM` word that prints a unified memory report
           across all regions:
           ```
           Bank 0:  1,048,576 total   dict 24,320   heap free 498,712   stack used ~2,048
           HBW:     3,145,728 total   used 131,072   free 3,014,656
           Ext mem: 16,777,216 total  uland 45,056   xmem free 15,683,584
           Buffers: 7 registered (3 Bank0, 2 HBW, 2 ext)
           Heap: 3 free blocks, largest 498,200 bytes  (frag=2)
           ```
           Combines `.HEAP`, `.HBW`, `.XMEM`, `.USERLAND`, `BUFFERS`
           into one snapshot.  Helpful for interactive debugging
           and the TUI dashboard (screen 0).

    Estimated effort: Phase 1 = ~1 hour, Phase 2 = ~2 hours,
    Phase 3 = ~2 hours, Phase 4 = ~2 hours, Phase 5 = ~1 hour.
    Total: ~1 day of focused work.  Each phase is independently
    shippable.

48. ✅ **Arena allocator** — Region-aware scoped allocation for scratch
    memory.  Eliminates manual per-object `FREE` for short-lived data
    (file parsing, tile undo, packet assembly, task-local scratch).
    Unique property: arenas can target any of Megapad-64's three memory
    regions (Bank 0 heap, XMEM, HBW) behind a single API.
    Full design: [`docs/arenas.md`](docs/arenas.md).

    **Phase 1 — Heap-backed MVP (~30 lines, ~8 tests):**
    - 48a. ✅ Arena descriptor (4 cells: base, size, ptr, source).
           `ARENA-NEW ( size source -- arena ior )` allocates backing
           region via `ALLOCATE`, builds descriptor in dictionary.
    - 48b. ✅ `ARENA-ALLOT ( arena u -- addr )` bump-allocate with
           8-byte alignment.  Aborts on overflow.  `ARENA-ALLOT?`
           variant returns ior instead.
    - 48c. ✅ `ARENA-RESET ( arena -- )` rewind ptr to base (O(1) bulk
           reclaim).  `ARENA-DESTROY ( arena -- )` frees backing block
           and zeroes descriptor.
    - 48d. ✅ `ARENA-FREE`, `ARENA-USED`, `.ARENA` diagnostics.
           (commit `20a0f4d`, 13 tests)

    **Phase 2 — Multi-source + snapshots (~20 lines, ~6 tests):**
    - 48e. ✅ XMEM and HBW backing for `ARENA-NEW` / `ARENA-DESTROY`.
           Same API, dispatches via `(AR-ALLOC-BACKING)` / `(AR-FREE-BACKING)`.
    - 48f. ✅ `ARENA-SNAP ( arena -- snap )` save bump pointer.
           `ARENA-ROLLBACK ( arena snap -- )` rewind to snapshot.
           `ARENA-SNAP-DROP ( snap -- )` no-op for API symmetry.
           (commit `e512e4f`, 10 new tests, 23 total)

    **Phase 3 — Scoped arena stack (~15 lines, ~4 tests):**
    - 48g. ✅ `ARENA-PUSH ( arena -- )` / `ARENA-POP ( -- )` manage
           a 4-deep "current arena" stack.  `AALLOT ( u -- addr )`
           allocates from the top arena.  Enables region-agnostic
           library code.
           (commit `dad4d3a`, 5 new tests, 28 total)

    **Phase 4 — Arena-scoped buffers (~20 lines, ~4 tests):**
    - 48h. ✅ `ARENA-BUFFER ( type width length arena "name" -- )`
           creates a buffer whose descriptor + data live in the arena.
           `ARENA-DESTROY` auto-unregisters arena-scoped buffers from
           the buffer linked list via `(AR-UNREG-BUFS)`.
           (commit `7d7faa6`, 5 new tests, 33 total)

    Estimated effort: Phase 1 = ~1 hour, Phase 2 = ~1.5 hours,
    Phase 3 = ~1 hour, Phase 4 = ~1.5 hours.  Total: ~5 hours.
    Each phase is independently shippable.

49. ✅ **STC compiler hardening** — Runtime checks for silent
    corruption bugs in the STC (subroutine-threaded) Forth compiler.
    - 49a. ✅ `check_branch16` subroutine: validates that branch
           offsets fit in 16-bit signed range (−32768..+32767) before
           emitting LBR-family instructions.  Aborts with "Branch
           offset overflow" instead of silently truncating.
           Uses R0 as scratch (saved/restored via return stack R15).
           Called from 10 control-flow words: `IF/THEN`, `ELSE`,
           `BEGIN/UNTIL`, `BEGIN/WHILE/REPEAT`, `BEGIN/AGAIN`,
           `DO/LOOP`, `DO/+LOOP`, `CASE/OF/ENDOF/ENDCASE`.
    - 49b. ✅ LEAVE overflow check: aborts with "Too many LEAVEs
           (max 8)" instead of silently skipping the LEAVE when the
           leave-fixup table is full.
    - 12 new tests in `TestKDOSCompilerChecks` covering all control
      flow words plus overflow abort paths.
      (commit `dd6e945`)

---

### Layer 6: Architecture & Portability (Items 39–42)

Hardware-level redesigns.  Detailed design notes, trade-offs, open
questions, and implementation sketches are in
[`docs/IDEAS-scratchpad.md`](docs/IDEAS-scratchpad.md) — a living
document that will be folded into proper docs as each item ships.

39. ✅ **Micro-core CPU variant (MP64µ)** — DONE. Stripped-down core:
    no 64-bit hardware multiplier (shift-add, 0 DSPs), no I-cache,
    FP16 ALU, or BIST.  Tile/MEX access via shared cluster engine
    (round-robin arbitrated, +3 cycle overhead).  Shared
    decoder/ALU/flags/branch factored into `mp64_cpu_common.vh`,
    `mp64_cpu.v` refactored to use it.  Emulator has `MicroCluster`
    class with scratchpad, barrier, MPU, cluster enable/disable
    gating.  Bus arbiter has per-port QoS weights.
    - 39a. ✅ `mp64_cpu_common.vh` — shared decoder, ALU, FSM states
    - 39b. ✅ `mp64_cpu_micro.v` — shift-add mul, no cache/BIST (tile via cluster MEX)
    - 39c. ✅ `mp64_cpu.v` refactored to use common core
    - 39d. ✅ Parameterize `mp64_top.v` for mixed major+micro configs
           (`mp64_soc.v` created with NUM_CORES/NUM_CLUSTERS params,
           `mp64_top.v` now instantiates mp64_soc with passthrough)
    - 39e. ✅ Bus arbiter: weighted round-robin with per-port QoS CSRs
    - 39f. ✅ Emulator: `MicroCluster` + `num_clusters` in `MegapadSystem`
    - 39g. ☐ `CSR_CORE_TYPE` (0=major, 1=micro) — not yet wired
    - 39h. ✅ Tests: `TestMicroCluster` — 16 tests passing

40. ✅ **Multi-prime Field ALU** — DONE (commits 86f9dd5–b450bd7).
    Modulus programmable across 4 primes: Curve25519 ($2^{255}-19$),
    secp256k1, P-256, custom 256-bit.  Dedicated fast reducers per
    prime (×38 fold, sparse subtract, FIPS 186-4 §D.2 word-based),
    plus Verilog `%` simulation path for custom primes (REDC FSM
    reserved for synthesis).  Modes 8–12: FCMOV, FCEQ, LOAD_PRIME,
    FMAC, MUL_ADD_RAW.  41 RTL tests, 39 emulator field ALU tests.
    - 40a. ✅ `PRIME_SEL` register + prime table (CMD bits [7:6])
    - 40b. ✅ Dedicated reduction: secp256k1 ×(2³²+977) fold
    - 40c. ✅ Dedicated reduction: P-256 NIST FIPS 186-4 §D.2
    - 40d. ✅ Custom prime via LOAD_PRIME (mode 10, `PRIME_SEL=3`)
    - 40e. ✅ Mode 8: `FCMOV` (constant-time 256-bit mux)
    - 40f. ✅ Mode 9: `FCEQ` (constant-time XOR-OR-reduce equality)
    - 40g. ✅ Mode 11: `FMAC` (field multiply-accumulate, 0 new DSPs)
    - 40h. ✅ Mode 12: `MUL_ADD_RAW` (raw 512-bit accumulate)
    - 40i. ✅ Emulator: multi-prime + custom in `FieldALUDevice`
    - 40j. ✅ RTL: `mp64_field_alu.v` (~710 lines, was 489)
    - 40k. ✅ BIOS/KDOS: `PRIME-SECP`, `PRIME-P256`, `PRIME-CUSTOM`,
           `LOAD-PRIME`, `FCMOV`, `FCEQ`, `FMAC`, `FMUL-ADD-RAW`
    - 40l. ✅ Tests: cross-prime switching, backward compat, near-miss

41. ✅ **Memory model redesign** — DONE.  Expanded from 1 MiB
    monolithic to 4-bank architecture with differentiated bandwidth.
    Bank 0 (1 MiB, system at 0x0) + Banks 1–3 (1 MiB each, HBW at
    0xFFD0_0000–0xFFFF_FFFF).  External memory fills the gap.
    RTL: `mp64_memory.v` has 4-bank address decode, per-bank tile
    port arbitration, ext-mem forwarding.  Emulator: `system.py` has
    `_hbw_mem`, `HBW_BASE`/`HBW_SIZE`.  KDOS: bump allocator
    (`HBW-ALLOT`, `HBW-BUFFER`, `HBW-RESET`).  SysInfo reports bank
    sizes.  RTL testbench covers HBW bank read/write + cross-bank
    concurrent access.
    - 41a. ✅ 4-bank memory in `rtl/mem/mp64_memory.v` (Bank 0 + HBW 1–3)
    - 41b. ✅ Bank address decode, tile port arbiter, ext-mem forwarding
    - 41c. ✅ `mp64_pkg.vh` — `INT_MEM_BYTES=4M`, bank parameters
    - 41d. ✅ Bus arbiter: per-port QoS weights + bandwidth limiting
    - 41e. ✅ Emulator: HBW memory model in `system.py`
    - 41f. ✅ BIOS: `HBW-BASE`, `HBW-SIZE`; KDOS: `HBW-ALLOT`,
           `HBW-BUFFER`, `HBW-RESET`, `HBW-FREE`, `HBW-TALIGN`
    - 41g. ✅ SysInfo: `BANK0_SIZE`, `HBW_BASE`, `HBW_SIZE`, total mem
    - 41h. ✅ Tests: `TestHBWMemory` (emulator) + tb_memory.v HBW tests

42. ✅ **Technology-agnostic RTL** — DONE (commits 50320a5, 6221469).
    Moved all FPGA/ASIC-specific primitives behind clean wrappers;
    core builds portably.  Wrapped: block RAM, PLL/MMCM, clock gating,
    IO buffers, DSP multiply, reset synchronizers, FIFOs.
    - 42a. ✅ `rtl/prim/` — abstract interfaces (RAM, MUL, PLL, clkgate, rstsync)
    - 42b. ✅ `mp64_prim_ram.v` — parameterized SRAM (depth, width, ports)
    - 42c. ✅ `mp64_prim_mul.v` — multiplier with optional pipelining
    - 42d. ✅ `mp64_prim_pll.v`, `mp64_prim_clkgate.v`, `mp64_prim_rstsync.v`
    - 42e. ✅ `rtl/target/xilinx7/` — Xilinx 7-series implementations
    - 42f. ✅ Refactored `mp64_memory.v` → uses `mp64_prim_ram`
    - 42g. ✅ Refactored `mp64_cpu.v` multiply → uses `mp64_prim_mul`
    - 42h. ✅ `rtl/target/asic/` — stub ASIC wrappers (placeholders)
    - 42i. ✅ All 28 testbenches pass under `SIMULATION` define (414 assertions)

---

## Implementation Order

```
Layer 0  Items  1– 4  Foundation (allocator, exceptions, CRC, diag) ✅ DONE
Layer 1  Items  5– 8  Crypto Stack (AES ✅, SHA-3 ✅, crypto words ✅, FS encrypt ✅) ✅ DONE
Layer 2  Items  9–18  Network Stack (Ethernet ✅ → ARP ✅ → IP ✅ → ICMP ✅ → UDP ✅ →
                      DHCP ✅ → DNS ✅ → TCP ✅ → TLS 1.3 ✅ → Socket API ✅) ✅ DONE
Layer 3  Items 19–24  Multi-Core OS ✅ DONE (run queues, work stealing, affinity,
                      preemption, IPI, locks)
Layer 4  Items 25–30, 45–49  Application-Level
                      (net send ☐, FP16 ☐, QoS ✅, editor ☐,
                      scripting ☐, remote REPL ☐, SCROLL ☐,
                      USB ☐, mem hardening ✅, arenas ✅,
                      STC compiler hardening ✅)
Layer 5  Items 34–38  Field ALU & Post-Quantum Crypto ✅ DONE
                      (Field ALU, NTT engine, SHA-3 SHAKE streaming,
                      ML-KEM-512, Hybrid PQ key exchange)
Layer 6  Items 39–42  Architecture & Portability ✅ DONE
                      (micro-core ✅, multi-prime Field ALU ✅,
                      banked memory ✅, tech-agnostic RTL ✅)
```

Layer 6 status: **4 of 4 items done.**  ✅ COMPLETE.

Each item is committed individually with its own test class and run via
`make test-one K=TestClassName` + `make test-status`.  Layer 2 is
strictly bottom-up — each protocol builds on the one below it.
**Layer 2 items are large enough that each is broken into multiple
sub-commits** (a–d typically), each independently tested.  This ensures
continuous progress, reviewable diffs, and a working system at every step.

---

## File Summary

| File | Lines | Status |
|------|-------|--------|
| `bios.asm` | 12,510 | ✅ 291 dictionary entries |
| `kdos.f` | 11,004 | ✅ 923 colon defs, 707 vars/constants, §1–§17 |
| `megapad64.py` | 2,541 | ✅ Full CPU + extended tile + FP16/BF16 |
| `accel/mp64_accel.cpp` | 1,930 | ✅ C++ CPU core (pybind11, 63× speedup) |
| `accel_wrapper.py` | 830 | ✅ Drop-in wrapper for C++ CPU core |
| `system.py` | 610 | ✅ Quad-core SoC + TRNG + `run_batch()` C++ fast path |
| `cli.py` | 1,012 | ✅ Interactive monitor/debugger |
| `asm.py` | 788 | ✅ Two-pass assembler |
| `devices.py` | 2,066 | ✅ 17 devices: AES, SHA3, TRNG, CRC, Field ALU, NTT, KEM, + 7 more |
| `nic_backends.py` | 399 | ✅ Pluggable NIC backends (Loopback, UDP, TAP) |
| `diskutil.py` | 1,039 | ✅ MP64FS tooling |
| `test_megapad64.py` | 2,193 | 23 tests ✅ |
| `test_system.py` | 20,320 | 1,019 test methods (41 classes) ✅ |
| `test_networking.py` | 860 | 38 real-network tests (8 classes) ✅ |
| `setup_accel.py` | 35 | ✅ pybind11 build configuration |
| `bench_accel.py` | 139 | ✅ C++ vs Python speed comparison |
| `Makefile` | 190 | ✅ Build, test, & accel targets |
| `conftest.py` | 197 | ✅ Test fixtures, snapshot caching, live status |
| `sample.img` | — | Built by diskutil.py ✅ |
| `rtl/` | ~26,300 | ✅ 31 portable Verilog modules + 12 target overrides |
| `rtl/sim/` | ~11,300 | ✅ 29 testbenches (~419 HW assertions) |
| `fpga/` | — | ✅ Synthesis scripts, BIOS hex, SoC synth pipeline |
| `docs/` | 10 files | ✅ Written |
| `README.md` | 350 | ✅ Current |
