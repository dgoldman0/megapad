# MegaPad-64: Cycle-Level Non-Determinism Audit

> Comprehensive analysis of all potential sources of non-determinism at cycle-level
> granularity across the full MegaPad-64 architecture (RTL, Python emulator, C++
> accelerator).

---

## Table of Contents

- [Category A — Bus Arbitration & Contention](#category-a--bus-arbitration--contention)
- [Category B — Memory Subsystem](#category-b--memory-subsystem)
- [Category C — I-Cache](#category-c--i-cache)
- [Category D — Interrupt & Exception Handling](#category-d--interrupt--exception-handling)
- [Category E — Multi-Core Synchronization](#category-e--multi-core-synchronization)
- [Category F — Peripheral / MMIO Timing](#category-f--peripheral--mmio-timing)
- [Category G — Crypto Accelerator Engines](#category-g--crypto-accelerator-engines)
- [Category H — Tile / GPU Engine](#category-h--tile--gpu-engine)
- [Category I — Clock, Reset & PLL](#category-i--clock-reset--pll)
- [Category J — Emulator / RTL Divergence](#category-j--emulator--rtl-divergence)
- [Category K — External Interface & Target Platform](#category-k--external-interface--target-platform)
- [Category L — Miscellaneous](#category-l--miscellaneous)
- [Summary Matrix](#summary-matrix)

---

## Category A — Bus Arbitration & Contention

### A-1. Weighted Round-Robin Grant Depends on Request Timing
- **File:** `rtl/bus/mp64_bus.v` lines 49–120
- **Mechanism:** The bus arbiter uses a weighted round-robin scheme across 7 master
  ports (4 major cores + 3 clusters). The grant sequence depends on which masters
  assert `req` in which cycle. Three parameters control priority:
  `req_weight[i]` (CSR-configurable), current `req_credit[i]`, and the rotating
  `rr_ptr`. When credits deplete, a master must wait until the credit window
  resets. Any change in the relative timing of bus requests (e.g., one core stalls
  in cache refill while another does not) produces a different grant sequence and
  different per-core cycle counts.
- **Cycle-accuracy impact:** Every bus transaction (memory load/store, I-cache
  refill, peripheral access, BIST, SHA W-load, DMA) flows through this arbiter.
  A single grant reordering can cascade through the entire execution timeline.
- **Severity:** **HIGH**

### A-2. Bus Timeout Mechanism
- **File:** `rtl/bus/mp64_bus.v` lines 130–145
- **Mechanism:** A 32-cycle timeout counter (`BUS_TIMEOUT` in `mp64_defs.vh`
  line 370) fires `bus_err` if a granted transaction receives no `ack`. The
  timeout latency is fixed, but *whether* it fires depends on the target's
  response time, which varies with contention.
- **Cycle-accuracy impact:** A timeout inserts a fixed penalty, but the decision
  to time-out vs. complete normally is non-deterministic from the perspective of
  an individual core.
- **Severity:** **MEDIUM**

### A-3. QoS Bandwidth Limiting
- **File:** `rtl/bus/mp64_bus.v` lines 95–115, CSR definitions in
  `rtl/pkg/mp64_defs.vh` lines 460–464 (`CSR_QOS_WEIGHT`, `CSR_QOS_BWLIMIT`)
- **Mechanism:** Per-core QoS weights and bandwidth limits are software-writable
  CSRs. Changing them at runtime alters the bus grant pattern mid-execution.
- **Cycle-accuracy impact:** Dynamic QoS changes reshape bus traffic patterns
  unpredictably.
- **Severity:** **MEDIUM**

---

## Category B — Memory Subsystem

### B-1. Dual-Port SRAM Read-During-Write Collision
- **File:** `rtl/mem/mp64_sram_dp.v` lines 34–60
- **Mechanism:** The dual-port SRAM primitive has a `CONFLICT` output that flags
  simultaneous read+write to the same address, but it is **informational only** —
  the RTL performs the write and returns the *new* data on the read port
  (`data_out_b = mem[addr_b]` after the write). In synthesis, actual BRAM
  behavior on same-address collision is vendor-specific (Xilinx: "write-first" /
  "read-first" / "no-change" depending on primitives). The behavioral model
  assumes write-first, which may not hold on all targets.
- **Cycle-accuracy impact:** If two ports collide, the read value is
  architecturally undefined on some FPGA families, creating data-dependent
  non-determinism.
- **Severity:** **HIGH** (for multi-core scenarios with shared HBW banks)

### B-2. SRAM Uninitialized Contents
- **File:** `rtl/mem/mp64_sram_sp.v` lines 14–24, `rtl/mem/mp64_sram_dp.v`
  lines 18–28
- **Mechanism:** Neither `mp64_sram_sp` nor `mp64_sram_dp` zero-initialize
  their `mem` arrays on reset. In simulation, Verilog `reg` arrays default to
  `x` (unknown). In synthesis, BRAM contents are indeterminate after
  power-on (some FPGA families support bitstream init, but the design does not
  use `initial` blocks on SRAM).
- **Cycle-accuracy impact:** Any read-before-write returns unpredictable data,
  affecting subsequent computation. Particularly dangerous for tile memory and
  stack areas if software does not initialize.
- **Severity:** **MEDIUM**

### B-3. External Memory Variable Latency
- **File:** `rtl/mem/mp64_extmem.v` lines 45–90
- **Mechanism:** The external memory controller uses a `BURST_LEN`-parameterized
  burst protocol. Each burst takes `BURST_LEN` cycles minimum, plus an
  additional 1-cycle handshake. The external PHY signals (`phy_rd_valid`,
  `phy_busy`) are not synchronized — the latency depends on the external
  memory device (DRAM refresh, bank conflicts, etc.).
- **Cycle-accuracy impact:** Variable, potentially multi-cycle latency on every
  external memory access. Core-visible.
- **Severity:** **HIGH**

### B-4. Memory Bank Decode is Combinational
- **File:** `rtl/soc/mp64_soc.v` lines 80–120
- **Mechanism:** Address decode for Bank 0 (system), Banks 1–3 (HBW), VRAM,
  and MMIO is purely combinational. Multiple simultaneous accesses to
  different banks can proceed in parallel (bank-level parallelism), but
  same-bank contention goes through the shared arbiter.
- **Cycle-accuracy impact:** Timing depends on bank address distribution of
  concurrent accesses.
- **Severity:** **LOW**

---

## Category C — I-Cache

### C-1. Cache Hit vs. Miss Timing
- **File:** `rtl/core/mp64_icache.v` lines 30–130
- **Mechanism:** 4 KiB direct-mapped I-cache, 32 B lines. A hit returns data in
  1 cycle; a miss initiates a multi-cycle refill via the bus (4-beat burst ×
  bus latency per beat). The refill latency is bus-contention-dependent (see
  A-1).
- **Cycle-accuracy impact:** Instruction fetch takes 1 cycle on hit, 5+ cycles
  on miss (4 burst beats + tag update). The total penalty depends on bus
  contention at the time of the miss.
- **Severity:** **HIGH**

### C-2. Invalidation on Write-Hit
- **File:** `rtl/core/mp64_icache.v` lines 95–105
- **Mechanism:** When a store hits a cached I-cache line (`snoop_hit`), the
  valid bit is cleared. The next fetch to that address will miss, incurring
  refill cost. Self-modifying code or cross-core code patching triggers
  unpredictable invalidation patterns.
- **Cycle-accuracy impact:** One invalidation → one forced miss → bus-dependent
  refill penalty.
- **Severity:** **MEDIUM**

### C-3. CPU_SKIP Reads Next Instruction via I-Cache
- **File:** `rtl/core/mp64_cpu.v` lines 1750–1800 (CPU_SKIP state)
- **Mechanism:** The SKIP instruction must determine the byte length of the next
  instruction to calculate the skip target. It does this by reading through
  the I-cache. If the next instruction spans a cache-line boundary or is a
  miss, the skip takes additional cycles.
- **Cycle-accuracy impact:** 1 extra cycle per I-cache miss during skip-target
  computation.
- **Severity:** **LOW**

---

## Category D — Interrupt & Exception Handling

### D-1. IRQ Priority Encoding and Timing
- **File:** `rtl/core/mp64_cpu.v` lines 1670–1720 (CPU_IRQ state)
- **Mechanism:** Five interrupt sources checked in fixed priority order:
  IPI > BUS_ERR > TIMER > UART > NIC. IRQs are checked only when the
  instruction buffer is empty (`ibuf_len == 0`) and interrupts are enabled
  (`ie == 1`). If multiple interrupts arrive simultaneously, only the
  highest-priority one is serviced; the others must wait for RTI.
- **Cycle-accuracy impact:** The *instruction boundary* at which an interrupt is
  recognized depends on instruction length and I-cache hit/miss status. Two
  otherwise-identical runs with different I-cache states will recognize the
  same timer interrupt at different cycle counts.
- **Severity:** **HIGH**

### D-2. IRQ Handler Entry Bus Transactions
- **File:** `rtl/core/mp64_cpu.v` lines 1680–1710 (CPU_IRQ_PUSH, CPU_IRQ_LOAD)
- **Mechanism:** Entering an IRQ handler requires: (1) push saved state to stack
  via bus (2 × 64-bit writes = 2 bus transactions), (2) read IVT entry via
  bus (1 read). Each transaction has bus-contention-dependent latency.
- **Cycle-accuracy impact:** IRQ entry latency = 3 bus transactions + priority
  stall cycles. Varies with bus load.
- **Severity:** **MEDIUM**

### D-3. MPU Fault Handling
- **File:** `rtl/core/mp64_cpu.v` lines 1600–1650 (CPU_MEM_READ/WRITE fault paths)
- **Mechanism:** MPU base/limit violations trigger a trap that pushes state to
  the stack (bus transactions) and redirects to the fault handler via the IVT.
  The total penalty depends on bus availability.
- **Cycle-accuracy impact:** Fault handling adds 3+ bus cycles, bus-dependent.
- **Severity:** **LOW** (faults are exceptional)

---

## Category E — Multi-Core Synchronization

### E-1. Cluster Round-Robin Shared-Unit Arbitration
- **File:** `rtl/core/mp64_cluster.v` lines 55–120
- **Mechanism:** Each micro-core cluster (4 cores) shares a MUL/DIV unit, CRC
  engine, SHA ISA engine, and MEX (tile) engine. Access is round-robin
  arbitrated — if two cores request the same unit simultaneously, one waits.
  Wait time depends on the operation length of the winning core (MUL = 4
  cycles, DIV = 64 cycles, SHA compress = 64–80 cycles, tile ops = variable).
- **Cycle-accuracy impact:** A core requesting MUL while another is mid-DIV
  waits up to 64 cycles. Completely timing-dependent.
- **Severity:** **HIGH**

### E-2. Hardware Barrier Auto-Clear Race
- **File:** `rtl/core/mp64_cluster.v` lines 140–165
- **Mechanism:** The cluster barrier tracks which cores have arrived via
  `barrier_arrive` bits. When all cores arrive, `barrier_done` is set for one
  cycle, then auto-clears. If a core reads `CSR_BARRIER_STATUS` on the exact
  cycle of auto-clear, it may see `done=0` despite having arrived.
- **Cycle-accuracy impact:** Barrier synchronization granularity is cycle-exact;
  a 1-cycle polling window difference can cause a core to spin an extra
  iteration.
- **Severity:** **MEDIUM**

### E-3. Tile Memory Priority Arbiter (Starvation)
- **File:** `rtl/gpu/mp64_tile.v` lines 50–85
- **Mechanism:** Tile memory has a strict priority arbiter:
  core0 > cluster0 > cluster1 > cluster2. Under sustained tile traffic from
  core0, all clusters are starved indefinitely. This violates fairness and
  creates unbounded cycle-count differences between cores.
- **Cycle-accuracy impact:** Worst case: a cluster core's tile op is delayed
  indefinitely.
- **Severity:** **HIGH**

### E-4. Mailbox Requester ID Hardcoded
- **File:** `rtl/periph/mp64_mailbox.v` lines 55–70
- **Mechanism:** `requester_id` is hardcoded to `0` rather than derived from the
  requesting core. All IPI messages appear to come from core 0, preventing
  target cores from distinguishing actual senders.
- **Cycle-accuracy impact:** Functional bug rather than timing non-determinism,
  but it affects IPI acknowledgment routing and can cause spurious or missed
  acknowledgments.
- **Severity:** **MEDIUM**

### E-5. Spinlock MMIO has No Backoff
- **File:** `rtl/periph/mp64_mailbox.v` (spinlock registers) lines 80–100
- **Mechanism:** The hardware spinlock (test-and-set at MMIO offset `0x600`)
  supports 4 lock slots. Contention resolution is arrival-order (first core
  to complete the bus transaction wins). There is no hardware backoff —
  software must implement backoff to avoid livelock under contention.
- **Cycle-accuracy impact:** Bus-transaction arrival order determines lock
  acquisition; this depends on all upstream arbitration.
- **Severity:** **LOW**

---

## Category F — Peripheral / MMIO Timing

### F-1. Timer Free-Running Counter
- **File:** `rtl/periph/mp64_timer.v` lines 20–55
- **Mechanism:** 32-bit auto-reload timer, incrementing every cycle. When
  `counter == compare`, IRQ fires (if enabled) and counter reloads. The
  compare match is checked combinationally — if the compare register is
  written on the exact cycle of a match, behavior depends on the bus
  write timing (write-before-compare or compare-before-write).
- **Cycle-accuracy impact:** ±1 cycle jitter on timer IRQ delivery relative to
  the compare write.
- **Severity:** **LOW**

### F-2. UART 2-FF Synchronizer on RX
- **File:** `rtl/periph/mp64_uart.v` line 132
- **Mechanism:** `uart_rx` input passes through a 2-stage flip-flop
  synchronizer before being sampled. This introduces 2 cycles of latency
  and a metastability window. The exact cycle at which a start bit is
  recognized depends on the phase alignment of the external RX signal
  relative to the system clock.
- **Cycle-accuracy impact:** ±1–2 cycle jitter on RX character availability.
- **Severity:** **LOW** (external input — inherently asynchronous)

### F-3. NIC PHY Signals NOT Synchronized
- **File:** `rtl/periph/mp64_nic.v` lines 70–120
- **Mechanism:** Unlike the UART, the NIC does **not** have synchronizer
  flip-flops on `phy_rx_data`, `phy_rx_valid`, `phy_tx_ready`. These are
  directly sampled by the FSM. If the PHY clock domain differs from the
  system clock, this creates metastability hazards.
- **Cycle-accuracy impact:** Metastability can corrupt received data or cause
  FSM state corruption — catastrophic non-determinism.
- **Severity:** **HIGH** (if PHY is in a different clock domain)

### F-4. RTC Leap-Year Calculation
- **File:** `rtl/periph/mp64_rtc.v` lines 60–80
- **Mechanism:** The RTC uses the Verilog `%` (modulo) operator for leap year
  calculation: `year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)`.
  This is non-synthesizable for efficient hardware (requires divider) and may
  produce different results vs. synthesis tools that approximate the modulo.
- **Cycle-accuracy impact:** Affects February day-count once per year — minimal
  direct cycle impact, but creates RTL/synthesis divergence risk.
- **Severity:** **LOW**

### F-5. Disk DMA Not Wired
- **File:** `rtl/periph/mp64_disk.v` lines 80–95
- **Mechanism:** The disk controller has DMA request/acknowledge ports
  (`dma_req`, `dma_ack`, `dma_addr`, `dma_data`) but they are
  **not connected** in `mp64_soc.v`. DMA transfers are effectively stub-only.
- **Cycle-accuracy impact:** Disk operations cannot complete via DMA;
  software must use PIO, adding variable cycle counts.
- **Severity:** **LOW** (stub; not functional)

---

## Category G — Crypto Accelerator Engines

### G-1. AES Variable Latency
- **File:** `rtl/crypto/mp64_aes.v` lines 60–200
- **Mechanism:** AES-128-GCM: 10 rounds + key expansion + GHASH = ~128 cycles
  total. AES-256-GCM: 14 rounds + key expansion + GHASH = ~160 cycles.
  Latency is fixed once the key size is selected, but key expansion is
  re-triggered on every `cmd_go` write, adding variable setup cost.
- **Cycle-accuracy impact:** Deterministic per-operation, but the **bus
  transactions** for reading/writing plaintext through MMIO are
  contention-dependent.
- **Severity:** **LOW** (fixed-latency core, bus-variable envelope)

### G-2. SHA-2 ISA W-Buffer Loading via Bus
- **File:** `rtl/core/mp64_cpu.v` lines 1830–1870 (CPU_SHA_LOAD)
- **Mechanism:** The SHA-2 ISA instruction loads 8 × 64-bit words into the
  W-schedule buffer via 8 sequential bus reads. Each read goes through the
  bus arbiter. Total latency = 8 × (1 + bus_wait) cycles.
- **Cycle-accuracy impact:** 8–40+ cycles depending on bus contention.
- **Severity:** **MEDIUM**

### G-3. SHA-3 / Keccak-f Fixed 24-Round Pipeline
- **File:** `rtl/crypto/mp64_sha3.v` lines 30–140
- **Mechanism:** Keccak-f[1600] runs 24 rounds, each taking 1 cycle
  (combinational round function). Total: 24 cycles per permutation. This is
  deterministic.
- **Cycle-accuracy impact:** Fixed 24 cycles. MMIO bus access adds variable
  envelope.
- **Severity:** **LOW**

### G-4. Field ALU Non-Synthesizable Modular Arithmetic
- **File:** `rtl/crypto/mp64_field_alu_isa.v` lines 80–130
- **Mechanism:** The `field_add_sel`, `field_sub_sel`, and `field_mul_sel`
  functions use the Verilog `%` operator for 256-bit modular reduction. This
  is non-synthesizable — behavioral simulation uses arbitrarily wide integer
  division, but synthesis will either fail or produce incorrect logic.
- **Cycle-accuracy impact:** Behavioral simulation is "instant" (combinational);
  real hardware would need multi-cycle reduction. This means cycle counts in
  simulation do not reflect realizable hardware.
- **Severity:** **HIGH** (synthesis-blocking)

### G-5. X25519 Scalar Multiply — Long Fixed Pipeline
- **File:** `rtl/crypto/mp64_field_alu_isa.v` lines 501–700 (S_X_LADDER, S_POWER)
- **Mechanism:** X25519: 255 ladder iterations × 14 phases each + ~768 cycles
  for Z2 inversion (binary square-and-multiply) + cleanup ≈ 4,338+ cycles.
  Deterministic but extremely long, during which the core is stalled.
- **Cycle-accuracy impact:** Fixed, deterministic. But bus requests from other
  cores during this window are not serviced by this core.
- **Severity:** **LOW** (deterministic)

### G-6. NTT Non-Synthesizable Twiddle Factor Initialization
- **File:** `rtl/crypto/mp64_ntt.v` lines 40–60
- **Mechanism:** NTT twiddle factors are computed using `%` in an `initial` block:
  `tw[i] = (ROOT**i) % PRIME`. This is simulation-only; synthesizable NTT
  would need a ROM or runtime computation.
- **Cycle-accuracy impact:** Simulation-only divergence. If synthesized with a
  ROM, the NTT itself is deterministic.
- **Severity:** **MEDIUM** (synthesis divergence)

### G-7. KEM Engine is a 256-Cycle Stub
- **File:** `rtl/crypto/mp64_kem.v` lines 30–60
- **Mechanism:** The KEM engine is implemented as a stub: it counts 256 cycles
  and writes a deterministic bit pattern. No actual encapsulation/
  decapsulation logic.
- **Cycle-accuracy impact:** Fixed 256 cycles, deterministic. But results are
  cryptographically incorrect.
- **Severity:** **LOW** (stub, known incomplete)

### G-8. WOTS+ Engine Returns Zero
- **File:** `rtl/crypto/mp64_wots.v` lines 30–50
- **Mechanism:** The WOTS+ engine is a stub that returns `hash_data_out = 0`
  after a brief FSM cycle. No actual hash chain computation.
- **Cycle-accuracy impact:** Nearly instant, deterministic, but functionally
  incorrect.
- **Severity:** **LOW** (stub, known incomplete)

### G-9. CRC Fully Combinational — Zero Extra Cycles
- **File:** `rtl/crypto/mp64_crc_isa.v` lines 20–70
- **Mechanism:** CRC32/32C/64 computation (byte or quad) is purely
  combinational. `done` fires one cycle after `go`. Fully deterministic.
- **Cycle-accuracy impact:** None.
- **Severity:** **NONE**

### G-10. OP_CMOV Constant-Time Conditional Move
- **File:** `rtl/crypto/mp64_field_alu_isa.v` lines 300–310
- **Mechanism:** `OP_CMOV` writes `rd ← rs if rd != 0`. Designed to be
  constant-time (no branching), but the `rd_val != 64'd0` comparison may
  exhibit data-dependent timing at the gate level (carry-chain propagation
  differences). In behavioral simulation this is invisible.
- **Cycle-accuracy impact:** Potentially ±1 gate-delay at post-synthesis;
  zero in behavioral simulation.
- **Severity:** **LOW** (security-relevant, not cycle-count-relevant)

---

## Category H — Tile / GPU Engine

### H-1. Tile Memory Strict Priority Arbiter
- **See E-3 above** — repeated here for completeness.
- **File:** `rtl/gpu/mp64_tile.v` lines 50–85
- **Severity:** **HIGH**

### H-2. FP WMUL Precision Mismatch (RTL vs. Emulator)
- **File:** `rtl/gpu/mp64_tile.v` lines 625–650 (RTL), `megapad64.py`
  lines 3180–3195 (emulator)
- **Mechanism:** RTL computes FP WMUL as: `fp16_mul(a, b)` then widen the
  FP16 product to FP32 (losing precision to 10-bit mantissa before widening).
  The emulator does: `float(a) * float(b)` using full Python double
  precision, then converts to FP32 bits. These produce different numerical
  results for any product that needs more than 10 bits of mantissa.
- **Cycle-accuracy impact:** Not a timing issue but a **data divergence** —
  tile WMUL results differ between RTL and emulator for non-trivial inputs.
- **Severity:** **HIGH** (correctness)

### H-3. FP32 Accumulator Adder Tree Ordering
- **File:** `rtl/gpu/mp64_tile.v` lines 1510–1570 (5-level binary tree),
  `megapad64.py` lines 3155–3170 (sequential `sum()`)
- **Mechanism:** RTL reduces 32 FP32 partial products via a 5-level binary
  tree (32→16→8→4→2→1). The emulator uses Python's `sum()` which is
  left-to-right sequential accumulation. Different addition ordering produces
  different FP32 rounding results.
- **Cycle-accuracy impact:** Not a timing issue but a **data divergence** —
  DOT/DOTACC/SUM reductions produce different FP32 results.
- **Severity:** **HIGH** (correctness)

### H-4. FP NaN Propagation in Reductions
- **File:** `rtl/gpu/mp64_tile.v` lines 750–820 (RTL MIN/MAX),
  `megapad64.py` lines 3370–3420 (emulator MIN/MAX)
- **Mechanism:** RTL skips NaN values in MIN/MAX reductions (first non-NaN
  wins, using a signed-magnitude comparison with unsigned operators on the
  exponent+mantissa bits, sign bit directing the sense). Emulator uses Python
  `math.isnan()` filter then `min()`/`max()` on non-NaN values. Both skip
  NaNs but the RTL's signed-magnitude comparison may disagree with IEEE 754
  total ordering for -0.0 / +0.0 / subnormals.  Note: TALU lane-wise
  MIN/MAX (via `mp64_fp16_alu`) is NaN-*propagating* (returns qNaN), while
  TRED reduction MIN/MAX is NaN-*skipping* — the docs must distinguish these.
- **Cycle-accuracy impact:** Data divergence for inputs containing NaN, -0.0, or
  subnormal values.
- **Severity:** **MEDIUM**

### H-5. LOAD2D / STORE2D Variable Cycle Count
- **File:** `rtl/gpu/mp64_tile.v` lines 1800–1900 (S_LOAD2D_REQ/WAIT,
  S_STORE2D_REQ/WAIT), `megapad64.py` lines 3530–3570
- **Mechanism:** 2D strided load/store performs `ttile_h` row reads/writes
  through external memory, each requiring a bus transaction. Cycle count =
  `ttile_h` × (1 + bus_wait). Emulator returns `h` cycles (no bus model).
- **Cycle-accuracy impact:** Variable, bus-dependent. Up to `8 × 32 = 256`
  cycles for a full 8-row tile with contention.
- **Severity:** **MEDIUM**

### H-6. Internal vs. External Tile Memory Path
- **File:** `rtl/gpu/mp64_tile.v` lines 1700–1790 (S_LOAD_A, S_LOAD_B, S_STORE)
- **Mechanism:** The tile engine FSM loads source tiles from either internal
  tile SRAM (1 cycle if no priority conflict) or external memory via
  `ext_tile_req` → `ext_tile_ack` (variable latency). The path taken depends
  on the tile address and whether it falls within the tile memory aperture.
- **Cycle-accuracy impact:** 1 cycle (internal) vs. multi-cycle (external).
  Software-dependent but not obvious from the instruction encoding.
- **Severity:** **MEDIUM**

### H-7. Tile Engine FSM Compute Latencies
- **File:** `rtl/gpu/mp64_tile.v` lines 1700–2100
- **Mechanism:** The S_COMPUTE state duration varies by operation: TALU = 1
  cycle, TMUL = 1–3 cycles (pipelined), TRED = 1–2 cycles, TSYS = 1–3
  cycles. These are deterministic per-operation but the emulator returns 0
  additional cycles for TALU and TRED, and 1–3 for others, creating a
  systematic cycle-count mismatch.
- **Cycle-accuracy impact:** Consistent per-operation, but emulator cycle
  counts do not match RTL for any tile operation.
- **Severity:** **MEDIUM** (systematic, correctable)

---

## Category I — Clock, Reset & PLL

### I-1. Behavioral PLL (Passthrough)
- **File:** `rtl/prim/mp64_pll.v` lines 10–30
- **Mechanism:** The PLL is a behavioral passthrough: `clk_core = clk_in`,
  `locked` goes high after 10 cycles. No actual frequency synthesis,
  jitter, or phase alignment. In real silicon, PLL lock time is
  temperature/voltage-dependent and introduces startup non-determinism.
- **Cycle-accuracy impact:** Simulation starts deterministically after 10
  cycles. Real hardware would have variable lock time (100s to 1000s of
  cycles).
- **Severity:** **LOW** (sim-only; real hardware would need characterization)

### I-2. 4-Stage Reset Synchronizer
- **File:** `rtl/prim/mp64_rst_sync.v` lines 10–35
- **Mechanism:** Reset deassertion passes through a 4-FF synchronizer chain.
  In simulation, deassertion is deterministic (4 cycles after `rst_n_in`
  goes high). In real hardware with an asynchronous reset source, the
  deassertion cycle has metastability risk.
- **Cycle-accuracy impact:** ±1 cycle in real hardware. Deterministic in
  simulation.
- **Severity:** **LOW**

### I-3. Latch-Based Integrated Clock Gate
- **File:** `rtl/prim/mp64_clkgate.v` lines 10–25
- **Mechanism:** ICG uses a latch to gate `clk_in` with `enable`: the latch
  captures `enable` on the low phase of `clk_in`, and the output is
  `clk_in & latch_out`. In simulation, this is glitch-free. In synthesis,
  hold-time violations on the latch can cause glitches.
- **Cycle-accuracy impact:** None in simulation. Potential clock glitches in
  poorly-constrained synthesis.
- **Severity:** **LOW**

---

## Category J — Emulator / RTL Divergence

### J-1. No Bus Contention Model
- **File:** `system.py` lines 500–520 (step/run methods)
- **Mechanism:** The Python emulator performs all memory reads/writes as instant
  Python dict/bytearray accesses. There is no bus arbiter, no wait states,
  no bank conflicts. Every load/store takes 0 additional cycles.
- **Cycle-accuracy impact:** **Every bus transaction** in the RTL has ≥1 cycle
  latency, potentially much more under contention. The emulator under-counts
  cycles for all memory-accessing instructions.
- **Severity:** **HIGH**

### J-2. Round-Robin Stepping (No True Concurrency)
- **File:** `system.py` lines 810–840 (step method), lines 870–920 (run_batch)
- **Mechanism:** `step()` iterates cores sequentially:
  `for core in self.cores: core.step()`. There is no interleaving within an
  instruction, no concurrent bus requests, no race conditions. `run_batch()`
  uses `CHUNK=1000` steps per core per round, creating artificial
  interleaving granularity.
- **Cycle-accuracy impact:** Completely different interleaving than RTL's
  cycle-by-cycle concurrent execution. Multi-core synchronization behavior
  is fundamentally different.
- **Severity:** **HIGH**

### J-3. MicroCluster Shared MUL/DIV — No Contention
- **File:** `system.py` lines 650–680 ("modelled as immediate, no contention")
- **Mechanism:** The system.py comments explicitly state that shared MUL/DIV
  is "modelled as immediate, no contention." The emulator adds a flat +3
  cycle overhead (`megapad64.py` line 4048) regardless of actual conflict.
- **Cycle-accuracy impact:** RTL contention can add 4–64+ cycles; emulator
  always adds exactly 3.
- **Severity:** **HIGH**

### J-4. BIST is Instant in Emulator
- **File:** `system.py` lines 660–690 (MicroCluster BIST), `rtl/core/mp64_cpu.v`
  lines 1780–1830 (CPU_BIST March C-)
- **Mechanism:** Emulator BIST runs as a synchronous Python loop (instant).
  RTL BIST performs March C- write/read patterns through the bus arbiter,
  taking thousands of cycles depending on memory size and bus contention.
- **Cycle-accuracy impact:** O(1) in emulator vs. O(N × bus_latency) in RTL.
- **Severity:** **MEDIUM**

### J-5. FP Precision — Python Double vs. RTL Bit-Exact
- **File:** `megapad64.py` lines 50–120 (`_fp_decode`/`_fp_encode`), 
  `rtl/core/mp64_fp16_alu.v` (full file)
- **Mechanism:** The emulator converts FP16/BF16 to Python `float` (64-bit
  double), performs arithmetic in double precision, then re-encodes to FP16
  via `_fp_encode()` with a manual RNE implementation. The RTL uses native
  FP16/FP32 bit-exact arithmetic. Double precision intermediates can
  represent values that FP16/FP32 cannot, producing different rounding
  results at each intermediate step.
- **Cycle-accuracy impact:** Not timing but **data divergence**. Accumulates
  over chains of FP operations.
- **Severity:** **HIGH** (correctness)

### J-6. SHA-256 Operator Precedence Bug
- **File:** `megapad64.py` lines 165–175 (`_sha256_compress`)
- **Mechanism:** The `ch` computation in the emulator's SHA-256 compression:
  ```python
  ch = (e & f) ^ ((~e) & g) & _M32
  ```
  Due to Python operator precedence, this evaluates as
  `(e & f) ^ (((~e) & g) & _M32)` rather than the intended
  `((e & f) ^ ((~e) & g)) & _M32`. The final `& _M32` mask only applies to
  the second operand, not the full XOR result. This can produce values with
  bits above bit 31 set.
- **Cycle-accuracy impact:** SHA-256 hash results may differ from RTL.
- **Severity:** **HIGH** (correctness bug)

### J-7. Timer Batch Tick
- **File:** `devices.py` lines 200–240 (Timer.step_batch)
- **Mechanism:** The emulator Timer advances by `n` ticks in O(1) using
  arithmetic (modular division to compute how many compare-match events
  occur in `n` ticks). The RTL increments per-cycle. Final timer value is
  identical, but any code polling the timer between ticks sees different
  intermediate values.
- **Cycle-accuracy impact:** Timer reads during a batch return end-of-batch
  value rather than mid-batch value.
- **Severity:** **MEDIUM**

### J-8. Timer IRQ Delivery Timing
- **File:** `system.py` lines 830–845
- **Mechanism:** In the emulator, timer IRQs are delivered after all cores have
  stepped in a round. In RTL, the timer IRQ fires on the exact cycle of the
  compare match and is visible to each core independently.
- **Cycle-accuracy impact:** Timer interrupt may be recognized at a different
  instruction boundary in the emulator vs. RTL.
- **Severity:** **MEDIUM**

### J-9. UART TX Always Ready
- **File:** `devices.py` lines 100–130
- **Mechanism:** The emulator UART has a 16-byte TX FIFO but `tx_ready` is
  always true (infinite drain rate). RTL UART TX uses a shift register with
  baud-rate timing — FIFO can fill and stall the CPU.
- **Severity:** **LOW**

### J-10. String/Dict Engine Cycle Counts
- **File:** `megapad64.py` lines 1600–1700 (string), 1720–1800 (dict),
  `rtl/core/mp64_string.v`, `rtl/core/mp64_dict.v`
- **Mechanism:** Emulator string ops return `len + 2` cycles (1 cycle per byte
  + setup). RTL string engine performs byte-by-byte bus transactions, each
  subject to bus contention. Dict engine similarly: emulator uses Python
  dict lookups; RTL does FNV-1a hash + SRAM lookups through the bus.
- **Cycle-accuracy impact:** Emulator assumes 1 cycle per memory access; RTL
  has variable bus latency per access.
- **Severity:** **MEDIUM**

### J-11. Division Fixed 64 Cycles vs. Variable
- **File:** `megapad64.py` lines 2831–2900 (`_exec_muldiv`),
  `rtl/core/mp64_cpu.v` lines 1500–1530 (CPU_MULDIV)
- **Mechanism:** RTL uses a 64-cycle iterative restoring divider. Emulator
  uses Python `//` and `%` operators (instant). The emulator does not model
  the 64-cycle stall.
- **Cycle-accuracy impact:** 64 cycles missing from emulator cycle count for
  every DIV instruction.
- **Severity:** **MEDIUM**

### J-12. I-Cache Not Modeled in Emulator
- **File:** `megapad64.py` lines 2270–2340 (step function — flat fetch)
- **Mechanism:** The emulator `fetch8()` reads directly from the memory array.
  There is no I-cache model — every fetch is a hit. CSRs for I-cache hits/
  misses are maintained but never incremented during normal execution.
- **Cycle-accuracy impact:** All instruction fetches take 1 cycle in emulator;
  RTL has hit/miss variability (see C-1).
- **Severity:** **HIGH**

### J-13. Micro-Core MEX Flat +3 Overhead
- **File:** `megapad64.py` lines 3995–4000 (`Megapad64Micro._exec_mex`)
- **Mechanism:** Micro-core MEX operations add a flat `+3` cycle overhead for
  "shared unit arbitration" regardless of actual contention or queue depth.
  RTL arbitration delay is O(queue_depth × op_latency).
- **Cycle-accuracy impact:** Emulator systematically under-estimates contention.
- **Severity:** **MEDIUM**

---

## Category K — External Interface & Target Platform

### K-1. NIC PHY Unsynchronized (CDC Hazard)
- **See F-3 above.**
- **Severity:** **HIGH**

### K-2. External Memory PHY Signals
- **File:** `rtl/mem/mp64_extmem.v` lines 20–40
- **Mechanism:** External memory PHY signals (`phy_rd_data`, `phy_rd_valid`,
  `phy_busy`) are directly sampled without synchronizers. If the external
  memory is in a different clock domain (e.g., DDR PHY at 2× or 4× system
  clock), this creates CDC hazards.
- **Cycle-accuracy impact:** Metastability → data corruption, FSM lockup.
- **Severity:** **HIGH** (if clock domains differ)

### K-3. ASIC Target Stubs are Behavioral
- **Files:** `rtl/target/asic/mp64_sram_sp_asic.v`,
  `rtl/target/asic/mp64_sram_dp_asic.v`, `rtl/target/asic/mp64_pll_asic.v`,
  `rtl/target/asic/mp64_clkgate_asic.v`, `rtl/target/asic/mp64_rst_sync_asic.v`
- **Mechanism:** All ASIC target primitives are behavioral stubs with a
  `$display("FIXME: ...")` warning. They instantiate the behavioral prim/
  versions instead of technology-specific macros.
- **Cycle-accuracy impact:** Behavioral models have zero setup/hold/propagation
  delay. Real ASIC primitives would have process-dependent timing.
- **Severity:** **LOW** (known stubs)

### K-4. Xilinx 7-Series MUL Stub
- **File:** `rtl/target/xilinx7/mp64_mul_xilinx7.v`
- **Mechanism:** The Xilinx 7-series multiplier wrapper is a stub that
  instantiates the behavioral `mp64_mul.v`. A real implementation would use
  DSP48E1 primitives with 3-stage pipeline latency.
- **Cycle-accuracy impact:** Behavioral MUL takes 4 cycles (as modeled in
  `mp64_mul.v`). DSP48E1 takes 3 cycles. ±1 cycle per MUL.
- **Severity:** **LOW**

---

## Category L — Miscellaneous

### L-1. TRNG is Deterministic LFSR in Simulation
- **File:** `rtl/periph/mp64_trng.v` lines 20–60
- **Mechanism:** The TRNG uses a 64-bit LFSR (Galois feedback, polynomial
  `0xD800000000000000`) seeded from a parameter. In simulation, the sequence
  is fully deterministic. On real hardware, a TRNG would sample environmental
  noise — inherently non-deterministic.
- **Cycle-accuracy impact:** Simulation is deterministic. Any software that
  relies on TRNG for control flow (nonces, random backoff, key generation)
  will diverge between simulation and silicon.
- **Severity:** **MEDIUM**

### L-2. Performance Counters Reflect All Timing Variations
- **Files:** `rtl/core/mp64_cpu.v` lines 150–170 (perf counter logic),
  `megapad64.py` lines 2360–2375 (`perf_cycles`, `perf_stalls`, `perf_tileops`)
- **Mechanism:** Hardware performance counters (`PERF_CYCLES`, `PERF_STALLS`,
  `PERF_TILEOPS`, `PERF_EXTMEM`) faithfully reflect actual cycle counts
  including all bus contention, cache misses, and interrupt latency. The
  emulator performance counters reflect the emulator's simplified model.
  Any comparison of performance counters between RTL and emulator will show
  divergence.
- **Cycle-accuracy impact:** Counters are *symptoms* of non-determinism, not
  sources. But software that makes decisions based on counters inherits all
  upstream non-determinism.
- **Severity:** **LOW**

### L-3. SysInfo Cluster Enable Mask is Runtime-Writable
- **File:** `rtl/soc/mp64_soc.v` lines 160–180, `rtl/pkg/mp64_defs.vh` line 320
  (`SYSINFO_BASE = 0x300`)
- **Mechanism:** The SysInfo register bank includes a cluster enable mask
  that is software-writable. Changing it at runtime can enable/disable
  clusters, altering the number of active bus masters and completely
  reshaping bus arbitration patterns.
- **Cycle-accuracy impact:** System-level reconfiguration at runtime is
  inherently non-deterministic from the perspective of other cores.
- **Severity:** **MEDIUM**

### L-4. DMA Ring Buffer Not Fully Implemented
- **File:** `rtl/core/mp64_cpu.v` lines 1000–1020 (DMA CSR writes),
  `megapad64.py` lines 720–740 (DMA state in CPU init)
- **Mechanism:** DMA ring buffer CSRs (`CSR_DMA_RING_BASE`, `CSR_DMA_RING_SZ`,
  `CSR_DMA_HEAD`, `CSR_DMA_TAIL`, `CSR_DMA_CTRL`, `CSR_DMA_STATUS`) are
  declared and writable, but DMA processing logic is not connected. Writes
  update state; no DMA transactions are generated.
- **Cycle-accuracy impact:** No DMA operations occur — any software expecting
  DMA completion will hang.
- **Severity:** **LOW** (stub)

### L-5. ROM Boot Determinism
- **File:** `rtl/prim/mp64_rom.v` lines 10–25
- **Mechanism:** Boot ROM is loaded from `bios.rom` via `$readmemh`. The ROM
  content is fixed at elaboration time. Boot is deterministic given the same
  ROM image.
- **Cycle-accuracy impact:** None (deterministic).
- **Severity:** **NONE**

### L-6. Bitfield ALU Tier 1 Sub-Ops Stub
- **File:** `rtl/core/mp64_bitfield.v` lines 50–90
- **Mechanism:** `BEXT`, `BDEP`, `RORI`, `BSWAP` (Tier 1 bitfield ops) have
  stubs returning 0 in certain configurations. When the `ENABLE_BF_TIER1`
  parameter is 0, these operations silently return zero rather than trapping.
- **Cycle-accuracy impact:** Incorrect results when disabled — data divergence
  but deterministic.
- **Severity:** **LOW**

---

## Summary Matrix

| ID   | Category              | Severity | Timing? | Data? | Synthesis? |
|------|-----------------------|----------|---------|-------|------------|
| A-1  | Bus RR arbitration    | HIGH     | ✓       |       |            |
| A-2  | Bus timeout           | MEDIUM   | ✓       |       |            |
| A-3  | QoS runtime change    | MEDIUM   | ✓       |       |            |
| B-1  | DP SRAM collision     | HIGH     | ✓       | ✓     | ✓          |
| B-2  | SRAM uninitialized    | MEDIUM   |         | ✓     |            |
| B-3  | Ext mem latency       | HIGH     | ✓       |       |            |
| B-4  | Bank decode combo     | LOW      | ✓       |       |            |
| C-1  | I-cache hit/miss      | HIGH     | ✓       |       |            |
| C-2  | I-cache invalidation  | MEDIUM   | ✓       |       |            |
| C-3  | SKIP via I-cache      | LOW      | ✓       |       |            |
| D-1  | IRQ timing            | HIGH     | ✓       |       |            |
| D-2  | IRQ handler bus txns  | MEDIUM   | ✓       |       |            |
| D-3  | MPU fault bus txns    | LOW      | ✓       |       |            |
| E-1  | Cluster shared units  | HIGH     | ✓       |       |            |
| E-2  | Barrier auto-clear    | MEDIUM   | ✓       |       |            |
| E-3  | Tile mem priority     | HIGH     | ✓       |       |            |
| E-4  | Mailbox requester ID  | MEDIUM   |         | ✓     |            |
| E-5  | Spinlock no backoff   | LOW      | ✓       |       |            |
| F-1  | Timer compare race    | LOW      | ✓       |       |            |
| F-2  | UART RX sync          | LOW      | ✓       |       |            |
| F-3  | NIC PHY no sync       | HIGH     | ✓       | ✓     | ✓          |
| F-4  | RTC % operator        | LOW      |         |       | ✓          |
| F-5  | Disk DMA unwired      | LOW      | ✓       |       |            |
| G-1  | AES fixed + bus var   | LOW      | ✓       |       |            |
| G-2  | SHA-2 W-load bus      | MEDIUM   | ✓       |       |            |
| G-3  | SHA-3 fixed           | LOW      |         |       |            |
| G-4  | Field ALU `%` op      | HIGH     |         |       | ✓          |
| G-5  | X25519 fixed          | LOW      |         |       |            |
| G-6  | NTT twiddle init      | MEDIUM   |         |       | ✓          |
| G-7  | KEM stub              | LOW      |         | ✓     |            |
| G-8  | WOTS stub             | LOW      |         | ✓     |            |
| G-9  | CRC combinational     | NONE     |         |       |            |
| G-10 | CMOV timing           | LOW      | ✓       |       |            |
| H-1  | Tile mem priority     | HIGH     | ✓       |       |            |
| H-2  | FP WMUL precision     | HIGH     |         | ✓     |            |
| H-3  | FP32 tree ordering    | HIGH     |         | ✓     |            |
| H-4  | FP NaN propagation    | MEDIUM   |         | ✓     |            |
| H-5  | LOAD2D bus variable   | MEDIUM   | ✓       |       |            |
| H-6  | Tile int/ext path     | MEDIUM   | ✓       |       |            |
| H-7  | Tile compute cycles   | MEDIUM   | ✓       |       |            |
| J-1  | No bus model          | HIGH     | ✓       |       |            |
| J-2  | RR stepping           | HIGH     | ✓       |       |            |
| J-3  | MUL/DIV no contention | HIGH     | ✓       |       |            |
| J-4  | BIST instant          | MEDIUM   | ✓       |       |            |
| J-5  | FP double precision   | HIGH     |         | ✓     |            |
| J-6  | SHA-256 precedence    | HIGH     |         | ✓     |            |
| J-7  | Timer batch tick      | MEDIUM   | ✓       |       |            |
| J-8  | Timer IRQ timing      | MEDIUM   | ✓       |       |            |
| J-9  | UART TX always ready  | LOW      | ✓       |       |            |
| J-10 | String/Dict cycles    | MEDIUM   | ✓       |       |            |
| J-11 | DIV 64 cycles missing | MEDIUM   | ✓       |       |            |
| J-12 | No I-cache model      | HIGH     | ✓       |       |            |
| J-13 | Micro MEX flat +3     | MEDIUM   | ✓       |       |            |
| K-1  | NIC PHY CDC           | HIGH     | ✓       | ✓     | ✓          |
| K-2  | ExtMem PHY CDC        | HIGH     | ✓       | ✓     | ✓          |
| K-3  | ASIC stubs            | LOW      | ✓       |       |            |
| K-4  | Xilinx7 MUL stub     | LOW      | ✓       |       |            |
| L-1  | TRNG LFSR             | MEDIUM   |         | ✓     |            |
| L-2  | Perf counters         | LOW      | ✓       |       |            |
| L-3  | SysInfo enable mask   | MEDIUM   | ✓       |       |            |
| L-4  | DMA ring stub         | LOW      | ✓       |       |            |
| L-5  | ROM boot              | NONE     |         |       |            |
| L-6  | Bitfield stubs        | LOW      |         | ✓     |            |

### Severity Distribution

| Severity | Count |
|----------|-------|
| HIGH     | 18    |
| MEDIUM   | 20    |
| LOW      | 18    |
| NONE     | 2     |
| **Total**| **58**|

### Top Priority Items (HIGH severity)

1. **A-1** — Bus arbitration is the root cause of most timing non-determinism
2. **J-1/J-2/J-3/J-12** — Emulator completely lacks bus, concurrency, and cache models
3. **H-2/H-3/J-5/J-6** — FP precision and SHA-256 data correctness bugs
4. **E-1/E-3/H-1** — Multi-core shared-resource contention
5. **B-1** — Dual-port SRAM collision behavior is target-dependent
6. **F-3/K-1/K-2** — Missing CDC synchronizers (synthesis-blocking)
7. **G-4** — Non-synthesizable field ALU modular arithmetic
8. **C-1/D-1** — I-cache and interrupt timing cascade from bus
