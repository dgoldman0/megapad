# ROADMAP to v1.0

**Goal:** A polished, self-documenting computer system — emulator, BIOS,
OS, filesystem, interactive TUI, crypto stack, full network stack,
multicore OS, and comprehensive documentation — that feels complete and
cohesive as a v1.0 release.

**Current state (Feb 2026):** BIOS (291 dict entries, 11,158 lines ASM),
KDOS v1.1 (8,296 lines, 653 colon defs, 405 vars/constants), Emulator
(2,541 lines + 610-line quad-core SoC + 1,930-line C++ accelerator),
FPGA RTL (23 Verilog modules + 18 testbenches, ~180 HW tests),
devices.py (2,314 lines, 14 device classes), 1,068 test methods passing
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
TCP fully implemented (4 TCB slots, 3-way handshake, sliding window,
congestion control, retransmit, graceful close, 32 tests).  TLS 1.3
fully implemented (HKDF, record layer, handshake, app data).  Socket
API done (8 words, TCP+UDP).  Post-quantum: ML-KEM-512 keygen/encaps/
decaps + hybrid X25519+ML-KEM exchange with HKDF key derivation.
Remaining work: application-level features (items 25–30).

---

## Completed Work

### BIOS v1.0 — ✅ DONE

291 dictionary entries, 11,158 lines ASM, ~24 KB binary.

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

653 colon definitions + 405 variables/constants/creates, 8,296 lines.

19 sections:
- §1 Utility words (§1.1–§1.13: buffer, AES, SHA3, TRNG, X25519, HKDF,
  Field ALU, NTT, ML-KEM-512, Hybrid PQ Exchange)
- §2 Buffer subsystem, §3 Tile-aware buffer ops
- §4 Kernel registry, §5 Sample kernels (12 kernels including kadd,
  knorm, khistogram, kpeak, kconvolve, etc.)
- §6 Pipeline engine, §7 Storage & persistence
- §7.5–7.8 Filesystem (MP64FS), documentation browser, dictionary search
- §8 Scheduler & tasks, §9 Interactive screens (9-tab TUI)
- §10 Data ports (NIC ingestion), §11 Benchmarking
- §12 Dashboard, §13 Help system, §14 Startup
- §15 Pipeline bundles (versioned, declarative config format)
- §8.1 Multicore dispatch (CORE-RUN, CORE-WAIT, BARRIER, LOCK/UNLOCK, P.RUN-PAR)
- §16 Network Stack (Ethernet, ARP, IPv4, ICMP, UDP, DHCP, DNS, TCP, TLS 1.3)
- §17 Socket API (SOCKET, BIND, LISTEN, ACCEPT, CONNECT, SEND, RECV, CLOSE)

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
- ✅ system.py: Quad-core SoC — UART, timer, storage, NIC, mailbox IPI, spinlocks, TRNG, `run_batch()` (610 lines)
- ✅ asm.py: Two-pass assembler (788 lines), SKIP instruction
- ✅ cli.py: Interactive monitor/debugger (995 lines)
- ✅ diskutil.py: Filesystem tooling (1,039 lines)
- ✅ devices.py: MMIO peripherals — CRC, AES-256-GCM, SHA3/SHAKE, TRNG, Field ALU, NTT, KEM (2,314 lines, 14 device classes)

### Test Suite — ✅ 1,068 tests

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
- TestDiskUtil: 19, TestAssemblerBranchRange: 11
- TestNIC: 11, TestSystemMMIO: 3, TestUART: 3, TestStorage: 2,
  TestTimer: 1, TestDeviceBus: 2
- test_megapad64.py: 23 CPU + tile tests
- test_networking.py: 38 real-network tests (NIC backends, TAP,
  ARP, ICMP, UDP, TCP) across 8 test classes

### FPGA RTL — ✅ DONE (full ISA + extended tile + multicore + PQC)

23 Verilog modules in `fpga/rtl/`, 18 testbenches, ~180 hardware tests passing.
13,367 lines RTL + 8,677 lines testbench.

- ✅ mp64_cpu.v — Full ISA + 2-stage pipeline (IF+DEX) with I-cache interface
- ✅ mp64_soc.v — Quad-core SoC top-level (bus arbiter, MMIO, IPI wiring, TRNG,
  Field ALU, NTT, KEM)
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
    - 34e. ✅ KDOS §1.10: F+ F- F* wrappers, BIG-MUL-LIMB.
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
27. ☐ **QoS** — bus bandwidth allocation per core
28. ☐ **Editor** — simple line/screen editor for writing Forth on-device
29. ☐ **Scripting** — `AUTOEXEC` file loaded at boot, cron-like
    scheduled tasks
30. ☐ **Remote REPL** — UART or TCP-based remote Forth session

---

### Known Issues / Investigation

31. ☐ **CLI boot performance** — interactive `cli.py --bios bios.asm
    --storage sample.img` takes noticeably longer to reach the KDOS
    prompt than the equivalent test-suite boot (which processes the same
    KDOS source in seconds via C++ accel).  Investigate whether the CLI
    is actually engaging the C++ accelerator, whether the TAP backend
    poll loop is throttling throughput, or whether the interactive UART
    injection path is the bottleneck.  Also, even after building image,
    it appers that certain words are missing (out of date KDOS somehow)?
    Goal: CLI boot should feel
    instant (~2–3 s) with the C++ accel built.

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

---

### Layer 6: Architecture & Portability (Items 39–42)

Hardware-level redesigns.  Detailed design notes, trade-offs, open
questions, and implementation sketches are in
[`docs/IDEAS-scratchpad.md`](docs/IDEAS-scratchpad.md) — a living
document that will be folded into proper docs as each item ships.

39. ☐ **Micro-core CPU variant (MP64µ)** — a stripped-down core that
    is base-ISA-compatible but removes the 64-bit hardware multiplier
    (shift-add instead, 0 DSPs), I-cache, tile engine, FP16 ALU,
    BIST, and most perf counters.  Requires factoring the shared
    decoder/ALU/flags/branch/interrupt logic into `mp64_cpu_common.v`,
    building `mp64_cpu_micro.v` on top, and refactoring the existing
    `mp64_cpu.v` to also use the common core.  Bus arbiter generalized
    to `NUM_MAJOR_CORES + NUM_MICRO_CORES` with per-type default QoS
    weights.  Emulator gains a `micro=True` CPU flag.
    - 39a. Factor `mp64_cpu_common.v` (shared decoder, ALU, FSM)
    - 39b. `mp64_cpu_micro.v` (shift-add mul, no tile/cache/BIST)
    - 39c. Refactor `mp64_cpu.v` to use common core
    - 39d. Parameterize `mp64_soc.v` for mixed major+micro configs
    - 39e. Generalize bus arbiter for N cores with type-aware QoS
    - 39f. Emulator: micro-core mode + mixed-core `MegapadSystem`
    - 39g. Add `CSR_CORE_TYPE` (0=major, 1=micro) for KDOS scheduler
    - 39h. Tests: `TestMicroCore` — base ISA ops, MEX faults, slow mul

40. ☐ **Multi-prime Field ALU** — make the modulus programmable across
    a small set of primes ($2^{255}-19$, secp256k1, P-256) with
    dedicated fast-reduction circuits per prime, plus a generic
    Montgomery reduction path for arbitrary 256-bit primes.  Add
    constant-time primitives (FCMOV, FCEQ) required for real ECC.
    - 40a. `PRIME_SEL` register + prime table (4 entries)
    - 40b. Dedicated reduction: secp256k1 sparse subtract
    - 40c. Dedicated reduction: P-256 NIST special form
    - 40d. Generic Montgomery reduction path (`PRIME_SEL=3`)
    - 40e. Mode 8: `FCMOV` (constant-time conditional move)
    - 40f. Mode 9: `FCEQ` (constant-time equality comparison)
    - 40g. Emulator: multi-prime + Montgomery in `FieldALUDevice`
    - 40h. RTL: `mp64_field_reduce_*.v` modules + mux
    - 40i. BIOS/KDOS: `PRIME-SEL!`, `FCMOV`, `FCEQ` words
    - 40j. Tests: `TestFieldALUMultiPrime` — secp256k1, P-256,
           Montgomery, constant-time ops

41. ☐ **Memory model redesign** — expand from 1 MiB monolithic to a
    banked architecture with differentiated bandwidth tiers.  Default
    4 MiB: Bank 0 (1 MiB, regular BW at 0x0 — BIOS/OS/stacks) +
    Banks 1–3 (1 MiB each, high BW, dual-port for tile engine, pinned
    to the top of the internal address space).  High-BW banks keep
    fixed addresses so external regular-BW memory fills the gap.
    Last 256 MiB of address space guarded (unmapped, faults) to
    protect MMIO.  Hard QoS: per-bank bandwidth allocation, bank
    affinity CSRs per core.
    - 41a. `mp64_memory_bank.v` — parameterized single-bank module
    - 41b. `mp64_memory_subsys.v` — 4-bank instantiation, address
           decoder, tile port arbiter across banks, ext-mem forwarding
    - 41c. Update `mp64_defs.vh` — `INT_MEM_BYTES=4M`, `NUM_BANKS`,
           `HBW_BASE_ADDR`, `GUARD_BASE`
    - 41d. Bus arbiter: bank-aware routing, per-bank QoS
    - 41e. Emulator: banked memory model in `system.py`
    - 41f. BIOS/KDOS: `HBW-BASE`, `BANK@`, `HBW-ALLOC`, `BANK-COPY`
    - 41g. SysInfo: report bank count, sizes, HBW base
    - 41h. Tests: `TestBankedMemory` — bank routing, QoS, guard faults

42. ☐ **Technology-agnostic RTL** — move all FPGA/ASIC-specific
    primitives behind clean wrappers so the core builds portably.
    Wrap: block RAM, PLL/MMCM, clock gating, IO buffers, DSP multiply,
    reset synchronizers, FIFOs.
    - 42a. Create `fpga/rtl/prim/` directory with abstract interfaces
    - 42b. `mp64_prim_ram.v` — parameterized SRAM (depth, width, ports)
    - 42c. `mp64_prim_mul.v` — multiplier with optional pipelining
    - 42d. `mp64_prim_pll.v`, `mp64_prim_clkgate.v`, `mp64_prim_rstsync.v`
    - 42e. `fpga/rtl/target/xilinx7/` — Xilinx 7-series implementations
    - 42f. Refactor `mp64_memory.v` → use `mp64_prim_ram`
    - 42g. Refactor `mp64_cpu.v` multiply → use `mp64_prim_mul`
    - 42h. `fpga/rtl/target/asic/` — stub ASIC wrappers (placeholders)
    - 42i. Verify all 18 testbenches still pass under `SIMULATION` define

---

## Implementation Order

```
Layer 0  Items  1– 4  Foundation (allocator, exceptions, CRC, diag) ✅ DONE
Layer 1  Items  5– 8  Crypto Stack (AES ✅, SHA-3 ✅, crypto words ✅, FS encrypt ✅) ✅ DONE
Layer 2  Items  9–18  Network Stack (Ethernet ✅ → ARP ✅ → IP ✅ → ICMP ✅ → UDP ✅ →
                      DHCP ✅ → DNS ✅ → TCP ✅ → TLS 1.3 ✅ → Socket API ✅) ✅ DONE
Layer 3  Items 19–24  Multi-Core OS ✅ DONE (run queues, work stealing, affinity,
                      preemption, IPI, locks)
Layer 4  Items 25–30  Application-Level (net send, FP16, QoS, editor,
                      scripting, remote REPL)
Layer 5  Items 34–38  Field ALU & Post-Quantum Crypto ✅ DONE
                      (Field ALU, NTT engine, SHA-3 SHAKE streaming,
                      ML-KEM-512, Hybrid PQ key exchange)
Layer 6  Items 39–42  Architecture & Portability
                      (micro-core variant, multi-prime Field ALU,
                      banked memory model, tech-agnostic RTL)
```

Suggested Layer 6 ordering: **42 → 41 → 40 → 39** — abstraction
layer first (clean foundation), then memory redesign (on top of
clean primitives), then Field ALU (self-contained), and finally
micro-core (most complex, benefits from all prior work).  Alternatively
**40 → 42 → 41 → 39** for an early visible win (multi-prime ALU is
fully independent and immediately useful).

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
| `bios.asm` | 11,158 | ✅ 291 dictionary entries |
| `kdos.f` | 8,296 | ✅ 653 colon defs, 405 vars/constants, §1–§17 |
| `megapad64.py` | 2,541 | ✅ Full CPU + extended tile + FP16/BF16 |
| `accel/mp64_accel.cpp` | 1,930 | ✅ C++ CPU core (pybind11, 63× speedup) |
| `accel_wrapper.py` | 830 | ✅ Drop-in wrapper for C++ CPU core |
| `system.py` | 610 | ✅ Quad-core SoC + TRNG + `run_batch()` C++ fast path |
| `cli.py` | 1,012 | ✅ Interactive monitor/debugger |
| `asm.py` | 788 | ✅ Two-pass assembler |
| `devices.py` | 2,314 | ✅ 14 devices: AES, SHA3, TRNG, CRC, Field ALU, NTT, KEM, + 7 more |
| `nic_backends.py` | 399 | ✅ Pluggable NIC backends (Loopback, UDP, TAP) |
| `diskutil.py` | 1,039 | ✅ MP64FS tooling |
| `test_megapad64.py` | 2,193 | 23 tests ✅ |
| `test_system.py` | 14,751 | 1,007 test methods (40 classes) ✅ |
| `test_networking.py` | 860 | 38 real-network tests (8 classes) ✅ |
| `setup_accel.py` | 35 | ✅ pybind11 build configuration |
| `bench_accel.py` | 139 | ✅ C++ vs Python speed comparison |
| `Makefile` | 190 | ✅ Build, test, & accel targets |
| `conftest.py` | 197 | ✅ Test fixtures, snapshot caching, live status |
| `sample.img` | — | Built by diskutil.py ✅ |
| `fpga/rtl/` | 13,367 | ✅ 23 Verilog modules |
| `fpga/sim/` | 8,677 | ✅ 18 testbenches (~180 HW tests) |
| `docs/` | 10 files | ✅ Written |
| `README.md` | 350 | ✅ Current |
