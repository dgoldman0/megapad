# Megapad-64 FPGA Design and Status

This document describes the current portable RTL, its FPGA-facing integration,
and the evidence still required before claiming a physical implementation.
The functional TACC RTL is present; routed resource, timing, and board
acceptance are not.

## 1. Current SoC topology

The production topology contains sixteen instruction-executing cores and seven
physical tile engines:

| Compute domain | Instances | Tile, ACC, and TACC state |
|---|---:|---|
| Full core | 4 | One private engine per core |
| Microcore cluster | 3 × 4 microcores | One engine shared by the four callers in each cluster |

Each full core has its own I-cache, scalar execution state, tile configuration,
legacy 256-bit ACC, and 2,048-bit TACC. Within a microcluster, each caller keeps
private tile cursor, mode, control, source, destination, and stride shadows.
The cluster's legacy ACC, TACC, and TACC ownership metadata belong to its one
shared physical engine.

The four private full-core engines and three cluster-shared engines form seven
requestors at the common tile-memory port. The production arbiter uses equal
round-robin service. TACC image operations use the same seven source lanes and
a chip-wide four-beat transfer stage; they do not add an eighth memory source.

The scalar memory bus separately admits the four full-core ports, three
cluster ports, NIC DMA, and disk DMA. I-cache refill and data traffic are
time-multiplexed at each full-core bus port.

### 1.1 Scalar CPI and modernization boundary

The checked-in full-core RTL is a correctness-first, in-order, buffered
fetch/decode state machine. Its current state flow clears the instruction
buffer after decode and returns through fetch for the next instruction. A
common warm simple instruction therefore has a roughly four-clock floor by
inspection, and execute or memory paths take longer. This is an implementation
description, not a post-route CPI measurement.

The intended modernization target is roughly 1--2 **average** CPI for common
hot scalar code. It is not a requirement that every operation finish in one
or two clocks. An in-order pipeline, forwarding, safe branch handling, and a
more capable cache/SRAM path are compatible with MegaPad's architecture.
Out-of-order retirement or speculative MMIO and shared side effects are not
required and would contradict the deterministic machine boundary.

The scalar core is only one term in realized performance. There is no data
cache in the current full-core path; internal RAM, I-cache refill, shared-bus
arbitration, XMEM, DMA, and peripherals impose their own latency and
contention. Any cache or pipeline redesign must preserve precise retirement,
prefix and PSEL behavior, instruction-boundary interrupts,
self-modifying-code visibility, deterministic ordering, DMA coherence, and
the architecturally visible distinctions among Bank 0, XMEM, and HBW.

This target belongs to RTL or silicon. The functional emulator should continue
to count retired instructions and advance its deterministic virtual-cycle
model without simulating pipeline bubbles globally. See
[`EMULATOR.md`](../../EMULATOR.md#steps-cycles-and-hardware-time) for the three
timing domains and hardware projection formula.

## 2. Internal and external memory

`mp64_soc` currently defaults `MEM_DEPTH` to 16,384 512-bit rows per bank.
`mp64_memory` instantiates four such asymmetric dual-port banks:

| Region | Address range | Current default |
|---|---|---:|
| Bank 0, system RAM | `0x0000_0000`–`0x000F_FFFF` | 1 MiB |
| External allocation window | `0x0010_0000` up to the VRAM aperture | Board-dependent |
| VRAM aperture | `0xFF00_0000`–`0xFF3F_FFFF` | 4 MiB external window |
| Bank 1, HBW RAM | `0xFFD0_0000`–`0xFFDFFFFF` | 1 MiB |
| Bank 2, HBW RAM | `0xFFE0_0000`–`0xFFEFFFFF` | 1 MiB |
| Bank 3, HBW RAM | `0xFFF0_0000`–`0xFFFFFFFF` | 1 MiB |

The four internal banks therefore expose 4 MiB of architectural payload.
Their tile port is 512 bits wide and their CPU port is 64 bits wide. Requests
outside an internal bank are forwarded to the external-memory controller.

The portable RTL defines the controller-side external-memory handshake,
including cancellation, bounded response handling, and tile serialization.
It does not by itself provide a production DDR3, HyperRAM, or Ethernet PHY.

## 3. Known K325T memory mismatch

The current comparison target is the Genesys 2
`xc7k325tffg900-2`. It contains 445 RAMB36 blocks, totaling 2,002.5 KiB.
The RTL's four 1 MiB banks exceed that raw capacity before accounting for any
cache, FIFO, or peripheral storage.

The asymmetric 512-bit geometry makes the mismatch larger in practice. The
physical-preflight lower bound is eight RAMB36 blocks across each 512-bit row,
32 blocks deep for 16,384 rows, across four banks: at least 1,024 RAMB36 blocks
for the default memory alone. The implementation runner must reject this
configuration before launching a heavyweight tool.

Routed acceptance therefore requires one explicit production decision:

- select a device large enough for the four-bank memory contract; or
- first derive the memory address widths and apertures from the selected
  depth, then reduce on-chip capacity and define what moves to external RAM.

The current memory module retains 14/17-bit addresses and fixed 1 MiB
apertures when only `MEM_DEPTH` changes.  The runner therefore rejects a
reduced-depth build as production evidence until that RTL contract is
corrected.  The selected target and `MEM_DEPTH` must then remain identical
across every comparison build. Until that decision is made, the K325T is a
measurement target, not an accepted production fit.

## 4. Board wrapper versus comparison harness

The Xilinx board wrapper,
`rtl/target/xilinx7/mp64_synth_top.v`, serves board-facing integration. It
contains the Genesys 2 clock path from the 200 MHz differential oscillator to
a 100 MHz system clock, synchronizes reset, exposes UART, SD, and debug LEDs,
and instantiates `mp64_soc`.

That wrapper is not a complete board design. Its external-memory and NIC inputs
are tied inactive, no production memory or network PHY is instantiated, and
its existing Genesys 2 Tcl is a synthesis-oriented helper rather than routed
acceptance evidence.

Landing 2.9 uses a separate common comparison harness. It measures `mp64_soc`
directly under one internal 100 MHz constraint and an explicit memory depth.
This avoids treating board-wrapper clock, pin, or historical source-list
differences as TACC resource deltas. The comparison harness is for
like-for-like implementation analysis; it is not a deployable bitstream top.

## 5. Functional RTL verification

The supported RTL gate graph is in `rtl/sim/Makefile`. Relevant focused gates
must be run sequentially, including:

```sh
make -C rtl/sim -j1 memory
make -C rtl/sim -j1 extmem
make -C rtl/sim -j1 tile_port_arbiter
make -C rtl/sim -j1 tacc_transfer
make -C rtl/sim -j1 tacc
make -C rtl/sim -j1 tacc_cycles
make -C rtl/sim -j1 cluster
make -C rtl/sim -j1 cpu_smoke
make -C rtl/sim -j1 cpu_micro
make -C rtl/sim -j1 tacc_soc
make -C rtl/sim -j1 soc_elaborate
```

`tacc_cycles` consumes the emulator-generated integer and floating-point TAMAC
fixtures; `tacc_vectors` intentionally aliases that authoritative gate.
`tacc_soc` is the supported seven-domain topology, image, isolation, and reset
integration bench. `soc_elaborate` is the supported complete-SoC elaboration
gate.

The old BIOS-heavy `tb_mp64_soc.v` is retired and intentionally absent from the
Make graph because its hierarchy predates private full-core tile engines. This
document identifies supported gates but does not claim that they were freshly
run as part of this documentation rewrite.

Generic frontend or hierarchy checks can support source-list and elaboration
confidence. They do not establish FPGA resource fit, arithmetic sharing,
routed timing, or unconstrained-path closure.

## 6. Attested physical workflow

Physical comparison is a three-build campaign:

1. materialize the locked pre-topology baseline;
2. materialize the immutable seven-engine topology checkpoint;
3. materialize the final full-TACC RTL;
4. bind all three to the same audited harness, constraints, part, memory
   depth, tool version, and implementation directives;
5. run implementation only after explicit approval; and
6. compare attested post-route reports.

The report gate must verify source commits and manifests, campaign and harness
identity, LUT/FF/BRAM/DSP deltas and remaining headroom, WNS/TNS and derived
Fmax, zero unconstrained paths, exactly seven tile engines and seven TACC
banks, and the locked multiplier and FP-feedback sharing limits.

No post-route utilization or timing result is accepted yet. Behavioral
simulation, manual estimates, and generic synthesis must not be promoted into
physical acceptance numbers. Exact preparation and comparison commands belong
in `fpga/README.md` and the chip-math handoff after the runner interface is
settled.

## 7. Documented nonblocking limitations

The following do not block the functional RTL landing, but remain explicit
physical or integration work:

- the production target and on-chip memory depth are undecided;
- no accepted routed implementation, bitstream, or board validation exists;
- the Genesys 2 wrapper has no production external-memory or NIC PHY;
- paired full-core and individual microcore reset seams exist and are covered
  by focused verification, but remain tied inactive until a production reset
  controller supplies them;
- the composed SoC bench does not repeat every leaf-level in-flight reset,
  cancellation, or stale-acknowledgement window;
- owner preservation through a CPU interrupt and the complete migration
  `STORE`/`RELEASE`/`LOAD` sequence remain composition tests;
- the topology bench does not repeat an actual CPU-fetched full-core TAMAC,
  although arithmetic and full-core dispatch are covered independently;
- the composed image route has additional registered no-progress cycles beyond
  the locked strict-system baseline; and
- microcluster scratchpad is not a legal TAMAC or TACC image route and faults
  before traffic even when scalar scratchpad policy is enabled.

These gaps must remain visible in documentation rather than being mistaken for
routed or board-ready closure.

## 8. References

- [`fpga/README.md`](../README.md) — current FPGA entry point and workflow
- [`docs/chip-math-update-handoff.md`](../../docs/chip-math-update-handoff.md) —
  locked TACC contracts, landings, and physical acceptance gates
- [`docs/isa-reference.md`](../../docs/isa-reference.md) — normative TACC ISA
- [`docs/architecture.md`](../../docs/architecture.md) — complete system map
