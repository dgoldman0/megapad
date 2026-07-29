# Megapad-64 FPGA Prototype

Synthesizable RTL for the Megapad-64 system-on-chip.

## Architecture

```text
4 full cores ───────────► 4 private tile / ACC / TACC engines ─┐
                                                              ├─► 7-way tile port
3 × 4-microcore clusters ► 3 shared tile / ACC / TACC engines ┘
                                                                    │
                                      ┌─────────────────────────────┼──────┐
                                      ▼                             ▼      ▼
                               4 internal banks               external RAM  MMIO
```

## Target FPGAs

| Measurement target | LUTs | BRAM | DSPs | Status |
|---|---:|---:|---:|---|
| **Xilinx Kintex-7 325T** | 203,800 | 445 BRAM36 (2,002.5 KiB) | 840 | Genesys 2; current 4 MiB memory does not fit |

The measurement harness is currently pinned to the **Kintex-7 325T**
(Digilent Genesys 2).  That is a measurement target, not yet an accepted
production target: the default internal-memory configuration exceeds its
block-RAM capacity.

## Directory Structure

```
fpga/
├── README.md                       ← this file
├── run_tacc_impl.py                 ← isolated prepare/implementation runner
├── check_tacc_reports.py            ← fail-closed three-build comparator
├── tacc_impl_harness.tcl            ← common direct-SoC routed flow
├── synth_genesys2.tcl               ← board-wrapper synthesis helper
├── synth_yosys*.tcl                 ← generic frontend helpers
├── constraints/
│   ├── tacc_measurement.xdc         ← internal comparison constraint
│   ├── genesys2.xdc                 ← Genesys 2 board constraints
│   └── nexys_a7.xdc                 ← legacy Nexys A7 constraints
├── docs/
│   └── fpga.md                     ← FPGA-specific documentation
└── build/                          ← local helper output (gitignored)

RTL source and testbenches are in the portable `rtl/` tree at the
project root.  See `rtl/README.md` or the project-level README for
the full module listing.
```

## Memory Architecture

The current RTL declares four internal 1 MiB banks.  Each bank is organized
as dual-port memory with a 512-bit tile port and a 64-bit CPU port.  One
system bank occupies the low address range and three high-bandwidth banks
occupy the high internal apertures.  Four private full-core tile engines and
three cluster-shared engines reach the common tile port through the
seven-requestor round-robin arbiter.

External memory (HyperRAM or SDRAM) connects through a cache/bridge that
translates 64-bit CPU accesses into burst transactions.  The tile engine
can also target external memory, but at reduced throughput (~8× slower
than internal BRAM).

## Physical Resource Status

There are no accepted post-route resource or timing numbers yet.  Manual LUT,
FF, DSP, or inferred-sharing estimates are not substitutes for a routed
implementation report.

The memory capacity mismatch is known without synthesis.  The default
`mp64_soc` configuration has four banks of 16,384 × 512 bits, or 4 MiB of
payload before caches, FIFOs, and peripheral storage.  The K325T contains
445 RAMB36 blocks, totaling 2,002.5 KiB.  The implementation runner's
conservative asymmetric-port geometry check requires at least 1,024 RAMB36
blocks for the default bank configuration, so it rejects that configuration
before launching Vivado.

Physical acceptance therefore needs one explicit decision applied identically
to all three comparison builds:

- choose a device large enough for the production internal-memory contract; or
- first land a depth-derived address-width/aperture contract, then reduce the
  on-chip bank depth and define which capacity moves to external memory.

The present `mp64_memory` keeps 14/17-bit addresses and fixed 1 MiB apertures,
so the runner refuses to label a reduced-depth build production-valid.  With
the current RTL and locked historical baselines, physical acceptance therefore
requires a larger target; a reduced-memory comparison needs its own RTL
contract landing first.

After that decision, the attested implementation flow compares the locked
pre-topology baseline, the seven-engine topology checkpoint, and the final
seven-TACC RTL with the same part, memory depth, constraints, directives, and
measurement harness.  It must demonstrate resource headroom, 100 MHz timing,
zero unconstrained paths, exactly seven tile engines and TACCs, and the locked
multiplier/FP-feedback sharing limits before physical completion is claimed.

## Preparing the Physical Comparison

Preparation is lightweight and does not launch Vivado.  Use one campaign ID
for all three snapshots:

```bash
python fpga/run_tacc_impl.py \
  --source-ref c8e8118e82a899ec3f101f63d277a1bf4ef5f84a \
  --label current-main --campaign-id <campaign> \
  --out /tmp/megapad-tacc-reports/current-main
python fpga/run_tacc_impl.py \
  --source-ref 364d44283ba5c2fad8187b63da6917af60344c26 \
  --label topology-only --campaign-id <campaign> \
  --out /tmp/megapad-tacc-reports/topology-only
python fpga/run_tacc_impl.py \
  --source-tree /path/to/megapad-full-tacc-rtl \
  --label full-tacc --campaign-id <campaign> \
  --out /tmp/megapad-tacc-reports/full-tacc
```

Each output records the full source identity and the common measurement
harness digest.  Omitting `--mem-depth` deliberately records the unresolved
production-memory decision.

On a future Vivado workstation, recreate all three outputs with the same
approved `--mem-depth <rows>` and add `--run-vivado`.  The runner is the only
supported entry point for acceptance: it keeps build products outside the
attested sources, captures the full Vivado build identity, and emits the
canonical routed reports.  It refuses the current 16,384-row K325T
configuration before starting Vivado.

After all three routed runs, compare them with the exact full-TACC identity
printed by its runner invocation:

```bash
python fpga/check_tacc_reports.py \
  --current-main /tmp/megapad-tacc-reports/current-main \
  --topology-only /tmp/megapad-tacc-reports/topology-only \
  --full-tacc /tmp/megapad-tacc-reports/full-tacc \
  --expected-full-commit <40-digit-commit> \
  --expected-full-manifest-sha256 <64-digit-manifest>
```

The comparison output must be written outside all three input packages.
Direct use of `synth_genesys2.tcl` remains useful for board-wrapper
experiments, but it is not TACC physical-acceptance evidence.
