<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<title>Megapad-64 Processor Architecture — Complete Developer Spec v1.0-rc</title>
<style>
/* ———  TYPO / LAYOUT  ——————————————————————————— */
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;line-height:1.55;margin:0;padding:0 1rem;background:#f6f8fb;color:#1b1e23}
h1,h2,h3{font-weight:600;color:#0d1117;margin:2rem 0 1rem}
h1{font-size:2rem;border-bottom:3px solid #3a5bff;padding-bottom:.5rem;margin-top:1.5rem}
h2{font-size:1.4rem;border-bottom:2px solid #d0d7de;padding-bottom:.3rem}
h3{font-size:1.15rem}
table{width:100%;border-collapse:collapse;margin:1rem 0;font-size:.9rem}
th,td{border:1px solid #d0d7de;padding:.4rem .6rem;text-align:left;vertical-align:top}
thead th{background:#e6edf3}
code,pre{font-family:"SFMono-Regular",Consolas,"Liberation Mono",Menlo,monospace;font-size:.82rem}
pre{background:#0d1117;color:#e6edf3;padding:1rem;border-radius:6px;overflow:auto}
aside.note{background:#fff9c4;border-left:4px solid #f5c940;padding:.75rem;margin:1.25rem 0}
aside.tip{background:#e6f7ea;border-left:4px solid #46c36f;padding:.75rem;margin:1.25rem 0}
.legal{font-size:.8rem;color:#57606a;margin:2.5rem 0}
@media (min-width:900px){
  .col2{display:grid;grid-template-columns:1fr 1fr;gap:1rem}
}
</style>
</head>
<body>

<h1>Megapad-64 Tile-Strutured Processor (TSP) Architecture<br>
<span style="font-weight:400;font-size:1.1rem">(a 64-bit, tile-native successor to the RCA 1802 COSMAC)</span></h1>

<aside class="note">
<strong>Goal:</strong> Preserve the 1802’s single-byte opcode simplicity and deterministic behaviour while delivering
64-bit compute, multi-MiB on-die scratchpad (“Megapad”), and tile-level vector ALU (<em>MEX</em>).<br><br>
Think of it as <em>a DSP that outgrew its genre</em> — a hybrid between a RISC CPU, a vector DSP, and a tiny GPU,
tuned for branch-heavy real-world workloads and guaranteed-cycle execution.
</aside>
  
<!-- ——— 1  CORE PARAMETERS ——————————————— -->
<h2 id="core">1 Core Parameters</h2>
<table>
<thead><tr><th>Feature</th><th>Value</th><th>Notes</th></tr></thead>
<tbody>
<tr><td>Native word / address</td><td>64 bit</td><td>SHA-512 blocks, 64-bit OS kernels, 64-GiB+ address space.</td></tr>
<tr><td>Scalar endianness</td><td>Little-endian</td><td>Least-significant byte at lowest address.</td></tr>
<tr><td>Pipeline</td><td>4-stage in-order (IF, ID, EX, WB)</td><td>Cycle-accurate emulation: one table lookup per stage.</td></tr>
<tr><td>Branch behaviour</td><td>Fixed-latency, in-order</td><td>PC-relative, single bubble for taken branches; SKIP decodes and discards one instruction.</td></tr>
<tr><td>Megapad</td><td>8 / 16 / 32 / 64 MiB SRAM (config at tape-out)</td><td>16 logical banks, 64-byte <em>tiles</em>; 4 read + 2 write ports to ALU.</td></tr>
<tr><td>Tile issue rate</td><td>1 tile cycle⁻¹ (512 bit datapath)</td><td>64 GB s⁻¹ @1 GHz — small-GPU throughput without caches.</td></tr>
<tr><td>Element widths</td><td>8, 16, 32, 64 bit S/U</td><td>Select per-op via <code>TMODE.E</code>.</td></tr>
<tr><td>External memory</td><td>x16 quad-SPI or 64-bit DDR4/LPDDR4 (optional)</td><td>Project-builder picks BOM cost.</td></tr>
<tr><td>Clocking</td><td>Fully static</td><td>Core retains state down to DC; <code>IDLE</code>/<code>HALT</code> suitable for deep sleep.</td></tr>
</tbody></table>

<!-- ——— 2  MEMORY MAP ——————————————— -->
<h2 id="memmap">2 Physical Memory Map</h2>
<table>
<thead><tr><th>Address range (hex)</th><th>Size</th><th>Description</th></tr></thead>
<tbody>
<tr>
  <td><code>0000 0000 0000 0000 – 0000 0000 03FF FFFF</code></td>
  <td>per bank</td>
  <td><strong>Megapad bank 0</strong> (logical 4 MiB aperture; see population rules)</td>
</tr>
<tr><td>… 15 more contiguous banks …</td><td> </td><td><strong>Megapad bank 15</strong></td></tr>
<tr><td><code>0001 0000 0000 0000 – 000F FFFF FFFF FFFF</code></td><td>≤ 240 TiB</td><td>External DDR/PSRAM (optional)</td></tr>
<tr><td><code>FFFF FF00 0000 0000 – FFFF FF7F FFFF FFFF</code></td><td>32 GiB</td><td>Memory-mapped I/O (GPIO, UART, Quad-SPI, USB-DEV, timers…)</td></tr>
<tr><td><code>FFFF FFFF FFF0 0000 – FFFF FFFF FFFFFFFF</code></td><td>64 KiB</td><td>CSR / system registers (incl. <code>SB/SR/SC/SW</code>, <code>TMODE</code>, IVT base, IVEC, <code>SPSEL</code>)</td></tr>
</tbody></table>

<h3>2.1 Scalar Endianness</h3>
<p>
All scalar loads and stores treat memory as little-endian: the least-significant byte of a 16/32/64-bit value
resides at the lowest address. This applies to both Megapad and external memory.
</p>

<h3>2.2 Tile Layout and Cursor</h3>
<p>
Megapad tiles are 64-byte blocks viewed as an 8×8 grid of bytes in row-major order:
element <code>[row, col]</code> for <code>row, col ∈ [0..7]</code> resides at byte offset <code>row*8 + col</code> within the tile.
Multi-byte elements obey the scalar endianness within that layout.
</p>
<p>
The architectural tile cursor is defined by:
</p>
<ul>
  <li><code>SB</code> – bank index (0–15)</li>
  <li><code>SR</code> – row index, in tiles</li>
  <li><code>SC</code> – column index, in tiles</li>
  <li><code>SW</code> – stride width in tiles (tiles per row)</li>
</ul>
<p>
The byte address of the tile selected by the cursor is:
</p>
<pre><code>TileAddr(SB, SR, SC, SW) =
    MegapadBase(SB) + ((SR * SW) + SC) * 64</code></pre>
<p>
CTL and tile-load/store instructions use this mapping when walking 2-D data.
</p>

<h3>2.3 Megapad Population and Access Rules</h3>
<p>
Megapad exposes 16 logical banks, each with a contiguous 4 MiB aperture in the address map. The total on-die
scratchpad size is a tape-out configuration:
</p>
<table>
  <thead><tr><th>Total Megapad</th><th>Per-bank capacity</th></tr></thead>
  <tbody>
    <tr><td>8 MiB</td><td>512 KiB per bank</td></tr>
    <tr><td>16 MiB</td><td>1 MiB per bank</td></tr>
    <tr><td>32 MiB</td><td>2 MiB per bank</td></tr>
    <tr><td>64 MiB</td><td>4 MiB per bank (full aperture)</td></tr>
  </tbody>
</table>
<p>
Scalar accesses and tile operations that fall within the populated range of a bank succeed with fixed latency.
Accesses beyond the populated range of a bank raise a bus-fault trap when the memory cycle reaches the EX/WB
stage. Scalar loads and stores use full 64-bit addresses and do not consult <code>SB</code>; tile operations respect
both the cursor and the bank-selection CSR.
</p>

<h3 id="alignment">2.4 Alignment Requirements</h3>
<p>
Alignment is enforced for deterministic timing:
</p>
<ul>
  <li>8-bit loads/stores: any address.</li>
  <li>16-bit loads/stores: address multiple of 2.</li>
  <li>32-bit loads/stores: address multiple of 4.</li>
  <li>64-bit loads/stores: address multiple of 8.</li>
  <li>Tile loads/stores (<code>LD1D/LD2D/ST1D/ST2D</code>): base address multiple of 64.</li>
</ul>
<p>
A misaligned access raises an Alignment Fault trap before architectural state changes, except for fields
required to report the fault itself.
</p>

<!-- ——— 3  REGISTER FILE ——————————————— -->
<h2 id="regs">3 Program-Visible Registers</h2>
<div class="col2">
<table>
<thead><tr><th>Standard regs</th><th>Bits</th><th>Power-up</th></tr></thead>
<tbody>
<tr><td>R0 – R15</td><td>64</td><td>0</td></tr>
<tr><td>PC (alias of any R)</td><td>64</td><td>R3</td></tr>
<tr><td>DP (DMA pointer alias)</td><td>64</td><td>R2</td></tr>
<tr><td>FLAGS <small> Z C N V P G I S </small></td><td>8</td><td>0</td></tr>
</tbody></table>

<table>
<thead><tr><th>Megapad, MEX &amp; system regs</th><th>Bits</th><th>Function</th></tr></thead>
<tbody>
<tr><td>SB</td><td>4</td><td>Megapad bank selector (0–15)</td></tr>
<tr><td>SR / SC / SW</td><td>20 / 20 / 20</td><td>Row, Column, Stride-width (tiles) for tile cursor</td></tr>
<tr><td>TMODE</td><td>8</td><td>Element width [1:0] and sign bit [4] for MEX</td></tr>
<tr><td>TCTRL</td><td>8</td><td>MEX accumulate / zero-ACC / saturation control</td></tr>
<tr><td>TSRC0 / TSRC1 / TDST</td><td>64</td><td>Tile base addresses for MEX operations</td></tr>
<tr><td>ACC0 – ACC3</td><td>64 each</td><td>Per-bank accumulators; together form a 256-bit accumulator</td></tr>
<tr><td>SPSEL</td><td>4</td><td>Index (0–15) of GPR used as stack pointer</td></tr>
<tr><td>IVEC_ID</td><td>8</td><td>Current interrupt/trap vector index</td></tr>
<tr><td>IVT_BASE</td><td>64</td><td>Base address of interrupt/trap vector table</td></tr>
</tbody></table>
</div>

<h3>3.1 FLAGS Semantics</h3>
<p>
The <code>FLAGS</code> register holds condition codes and control bits:
</p>
<ul>
  <li><strong>Z</strong> – Zero: result of last scalar ALU or tile reduction equals zero.</li>
  <li><strong>C</strong> – Carry/Borrow: set on unsigned carry (add) or borrow (sub) in scalar ALU.</li>
  <li><strong>N</strong> – Negative: copy of sign bit of the last signed result.</li>
  <li><strong>V</strong> – Overflow: set when signed add/sub overflows.</li>
  <li><strong>P</strong> – Parity: even parity of low 8 bits of last scalar ALU result.</li>
  <li><strong>G</strong> – Greater: set by <code>CMP</code> when the first operand exceeds the second (unsigned compare).</li>
  <li><strong>I</strong> – Interrupt enable: <code>1</code> when maskable interrupts are accepted.</li>
  <li><strong>S</strong> – Saturation sticky: set when a saturated scalar or MEX operation clamps a result.</li>
</ul>
<p>
<code>CMP rd, rs</code> computes <code>rd - rs</code> internally without modifying its operands, and updates
<code>Z/C/N/V</code> and <code>G</code> from that internal result.
</p>

<h3>3.2 Tile Cursor and Stack Pointer</h3>
<p>
The tile cursor registers <code>SB/SR/SC/SW</code> define a 2-D view of Megapad (see Section 2.2). CTL and tile
load/store instructions use this cursor for row/column walking.
</p>
<p>
The <code>SPSEL</code> CSR selects which general-purpose register is treated as the stack pointer (<code>SP</code>). At reset,
<code>SPSEL = 15</code>, so <code>R15</code> acts as <code>SP</code>. Stack-relative operations reference the GPR indicated by the current value
of <code>SPSEL</code>.
</p>

<!-- ——— 4  OPCODE FAMILIES OVERVIEW ——————————————— -->
<h2 id="opc">4 Primary Opcode Families (0×00 – 0×F0)</h2>
<table>
<thead><tr><th>F nibble</th><th>Mnemonic range</th><th>Summary (one-byte unless stated)</th></tr></thead>
<tbody>
<tr><td>0x0</td><td>SYS</td><td><code>NOP, HALT, IDLE, RESET, RTI</code>; unused slots trap.</td></tr>
<tr><td>0x1</td><td>IMM</td><td><code>LDI, LHI, ADDI</code> (imm8; <em>or</em> imm64 via EXT).</td></tr>
<tr><td>0x2</td><td>ALU</td><td>64-bit <code>ADD, SUB, AND, OR, XOR, CMP, MOV</code>.</td></tr>
<tr><td>0x3</td><td>SHIFT / BIT</td><td><code>SHL, SHR, SAR, ROL, ROR, BSET, BCLR, BTST</code>.</td></tr>
<tr><td>0x4</td><td>BR</td><td>PC-relative branches &amp; SKIP family (cond = RRRR).</td></tr>
<tr><td>0x5</td><td>LD1D / ST1D</td><td>Tile-aligned loads/stores with auto-column.</td></tr>
<tr><td>0x6</td><td>LD2D / ST2D</td><td>Auto-row-&amp;-column walker for true 2-D.</td></tr>
<tr><td>0x7</td><td>CTL</td><td><code>INC.R, DEC.R, INC.C, DEC.C, SETW, GETW, TILECPY, FILL, CLRROW, CLRCOL, PUSH, POP</code>.</td></tr>
<tr><td>0x8</td><td>SIMD</td><td>8×8-bit / 4×16-bit <code>SADD, SMUL, SMAX …</code> (host ALU).</td></tr>
<tr><td>0x9</td><td>I/O</td><td><code>IN, OUT</code> via 4-bit ports + memory-mapped sub-regs.</td></tr>
<tr><td>0xA</td><td>ATOM</td><td><code>LL, SC, CAS, SWAP</code> (single 64-bit word).</td></tr>
<tr><td>0xB</td><td>MUL/DIV</td><td><code>MUL, UMULH, DIV, UDIV</code> (micro-coded).</td></tr>
<tr><td>0xC</td><td>FPU</td><td>IEEE-754 <code>FADD, FMUL, FDIV, FSQRT</code> (optional).</td></tr>
<tr><td>0xD</td><td>CSR</td><td><code>CSRR, CSRW</code> – GPR &lt;→ CSR moves.</td></tr>
<tr><td>0xE</td><td>MEX (tile ALU)</td><td><code>TALU, TMUL, TRED, TSYS</code>; 1–3 bytes total.</td></tr>
<tr><td>0xF</td><td>EXT</td><td>Prefix for imm64, long stride, future op-map.</td></tr>
</tbody></table>

<h3>4.1 CSR Access</h3>
<p>
CSR instructions move data between general-purpose registers and the CSR block:
</p>
<ul>
  <li><code>CSRR rd, csr</code> — read: <code>rd ← CSR[csr]</code></li>
  <li><code>CSRW csr, rs</code> — write: <code>CSR[csr] ← rs</code></li>
</ul>
<p>
The hardware encodes CSR operations within the 0xD family. The assembler may also accept legacy-style
<code>CSR</code> pseudo-mnemonics and expand them into either <code>CSRR</code> or <code>CSRW</code> when unambiguous.
</p>

<h3>4.2 Branches and SKIP Conditions</h3>
<p>
The BR family uses a 4-bit condition field (<code>RRRR</code>) along with an 8-bit signed byte displacement:
</p>
<ul>
  <li>Displacement is sign-extended and added to the address of the instruction following the branch.</li>
  <li>Condition fields select which flags drive the branch.</li>
</ul>
<p>Suggested condition map:</p>
<table>
  <thead><tr><th>RRRR</th><th>Mnemonic</th><th>Condition</th></tr></thead>
  <tbody>
    <tr><td>0000</td><td>AL</td><td>Always</td></tr>
    <tr><td>0001</td><td>EQ</td><td>Z = 1</td></tr>
    <tr><td>0010</td><td>NE</td><td>Z = 0</td></tr>
    <tr><td>0011</td><td>CS</td><td>C = 1</td></tr>
    <tr><td>0100</td><td>CC</td><td>C = 0</td></tr>
    <tr><td>0101</td><td>MI</td><td>N = 1</td></tr>
    <tr><td>0110</td><td>PL</td><td>N = 0</td></tr>
    <tr><td>0111</td><td>VS</td><td>V = 1</td></tr>
    <tr><td>1000</td><td>VC</td><td>V = 0</td></tr>
    <tr><td>1001</td><td>GT</td><td>G = 1</td></tr>
    <tr><td>1010</td><td>LE</td><td>G = 0</td></tr>
    <tr><td>1011</td><td>PS</td><td>P = 1</td></tr>
    <tr><td>1100</td><td>PC</td><td>P = 0</td></tr>
    <tr><td>1101</td><td>SAT</td><td>S = 1</td></tr>
    <tr><td>1110</td><td>NSAT</td><td>S = 0</td></tr>
    <tr><td>1111</td><td>NV</td><td>Never (reserved for future)</td></tr>
  </tbody>
</table>
<p>
SKIP instructions reuse the same condition codes. When the condition holds, the next instruction is fetched and
decoded but discarded; <code>PC</code> advances as if that instruction had executed. This preserves deterministic timing:
taken and untaken SKIPs have fixed cycle counts, and taken branches insert a single, documented bubble.
</p>

<h3>4.3 Stack Instructions (PUSH/POP)</h3>
<p>
Stack operations work on the GPR selected by <code>SPSEL</code> and grow downward:
</p>
<ul>
  <li><code>PUSH rs</code>:
    <ol>
      <li><code>SP ← SP - 8</code></li>
      <li>Store 64 bits of <code>rs</code> to memory at <code>[SP]</code></li>
    </ol>
  </li>
  <li><code>POP rd</code>:
    <ol>
      <li>Load 64 bits from <code>[SP]</code> into <code>rd</code></li>
      <li><code>SP ← SP + 8</code></li>
    </ol>
  </li>
</ul>
<p>
On reset, <code>SPSEL</code> selects <code>R15</code> as <code>SP</code>. Interrupt and trap handlers use <code>PUSH</code>/<code>POP</code> for their prologue and epilogue.
</p>

<h3>4.4 Port I/O and Memory-Mapped I/O</h3>
<p>
The I/O family supports both legacy port-style access and modern memory-mapped peripherals:
</p>
<ul>
  <li><strong>Port I/O</strong>:
    <ul>
      <li><code>IN port, rd</code> — reads the 4-bit <code>PORT[3:0]</code> bus selected by <code>port</code> into the low nibble of <code>rd</code>.</li>
      <li><code>OUT port, rs</code> — drives <code>PORT[3:0]</code> with the low nibble of <code>rs</code>.</li>
    </ul>
  </li>
  <li><strong>Memory-mapped I/O</strong>:
    <ul>
      <li>Peripherals expose CSRs in the high I/O region.</li>
      <li>Software uses <code>CSRR/CSRW</code> to interact with those CSRs.</li>
    </ul>
  </li>
</ul>

<!-- ——— 5  MEX DETAIL ——————————————— -->
<h2 id="mex">5 MEX Family 0xE — Tile-ALU Detail</h2>
<p>Primary byte structure: <code>1110 S Op</code>, where <code>S</code>=operand selector and <code>Op</code>=2-bit major.</p>

<table>
<thead><tr><th>Op</th><th>Sub-funct (2<sup>nd</sup> byte)</th><th>Description (1 tile unless noted)</th></tr></thead>
<tbody>
<tr><td>00 (TALU)</td><td>0 ADD, 1 SUB, 2 AND, 3 OR, 4 XOR, 5 MIN, 6 MAX, 7 ABS</td><td>Element-wise ALU (8,16,32,64-bit lanes).</td></tr>
<tr><td>01 (TMUL)</td><td>0 MUL, 1 DOT (lane count per TMODE), 2–3 RES</td><td>Multiply or 8-lane dot-product to accumulator.</td></tr>
<tr><td>10 (TRED)</td><td>0 SUM, 1 MIN, 2 MAX, 3 POPCNT, 4 L1</td><td>Tile reduction to ACCx.</td></tr>
<tr><td>11 (TSYS)</td><td>0 TRANS, 1 CONV3, 2 MOVBANK, 3 LOADC, 4 ZERO</td><td>System / matrix helpers.</td></tr>
</tbody></table>

<p><strong>Operand selector S</strong></p>
<table>
<thead><tr><th>S</th><th>Source A</th><th>Source B</th></tr></thead>
<tbody>
<tr><td>00</td><td>tile @ TSRC0</td><td>tile @ TSRC1</td></tr>
<tr><td>01</td><td>tile @ TSRC0</td><td>broadcast Rn (third byte)</td></tr>
<tr><td>10</td><td>imm8 splat</td><td>tile @ TSRC0</td></tr>
<tr><td>11</td><td>tile @ TDST (in-place)</td><td>tile @ TSRC0</td></tr>
</tbody></table>

<aside class="tip">
<strong>Latency:</strong> TALU/TRED/TRANS/LOADC/ZERO 1 cycle; MUL 2 cycles; DOT/CONV3 4 cycles; MOVBANK 3 cycles (cross-bank).
These latencies assume aligned tiles and valid Megapad access.
</aside>

<h3>5.1 Tile Modes and Control (TMODE, TCTRL)</h3>
<p>
<code>TMODE</code> defines lane width and signedness for MEX:
</p>
<ul>
  <li>Bits [1:0] — element width:
    <ul>
      <li><code>00</code> = 8-bit</li>
      <li><code>01</code> = 16-bit</li>
      <li><code>10</code> = 32-bit</li>
      <li><code>11</code> = 64-bit</li>
    </ul>
  </li>
  <li>Bit [4] — sign:
    <ul>
      <li><code>0</code> = unsigned interpretation</li>
      <li><code>1</code> = signed interpretation</li>
    </ul>
  </li>
</ul>
<p>
<code>TCTRL</code> governs accumulator behaviour and saturation:
</p>
<ul>
  <li>Bit 0 (<code>ACC_ACC</code>) — accumulate:
    <ul>
      <li><code>0</code>: result overwrites the destination ACCs.</li>
      <li><code>1</code>: result is added into the destination ACCs.</li>
    </ul>
  </li>
  <li>Bit 1 (<code>ACC_ZERO</code>) — zero-before-use:
    <ul>
      <li>When set, <code>ACC0–ACC3</code> of the current bank clear to zero at the start of the next MEX instruction.</li>
      <li>Hardware clears <code>ACC_ZERO</code> back to <code>0</code> once this occurs.</li>
    </ul>
  </li>
  <li>Bit 2 (<code>SAT_EN</code>) — saturation:
    <ul>
      <li>When set, arithmetic TALU/TMUL/TRED operations saturate instead of wrapping.</li>
      <li><code>FLAGS.S</code> is set when any lane clamps due to saturation.</li>
    </ul>
  </li>
</ul>
<p>
TALU obeys saturation for ADD/SUB; logic operations ignore it. TMUL obeys saturation for per-lane MUL and for
DOT accumulation into the 256-bit ACC view. TRED obeys saturation for SUM/MIN/MAX; POPCNT and L1 ignore
<code>SAT_EN</code> but still set <code>FLAGS.S</code> if the result exceeds the native accumulator width.
</p>

<h3>5.2 ACC Accumulators and Banks</h3>
<p>
Each Megapad bank holds its own copy of <code>ACC0–ACC3</code>; the active bank matches the current <code>SB</code> value.
Scalar <code>CSRR/CSRW</code> access the ACCs of the bank selected by <code>SB</code>.
</p>
<p>
For MEX operations that produce reductions or dot products, the four ACC registers act as one logical 256-bit
little-endian accumulator:
</p>
<ul>
  <li><code>ACC0</code> — lowest 64 bits</li>
  <li><code>ACC3</code> — highest 64 bits</li>
</ul>
<p>
TMUL.DOT and TRED.SUM lane-wise add into this 256-bit view, honoring <code>TCTRL.ACC_ACC</code> and <code>TCTRL.ACC_ZERO</code>.
</p>

<!-- ——— 6  EXAMPLES ——————————————— -->
<h2 id="examples">6 Programming Examples</h2>

<h3>6.1 Tile memset (fill 128 KiB with 0×00)</h3>
<pre><code>; assume SB/SR/SC = 0 / 0 / 0, SW = 512 tiles per row
LDI   R0, 0                  ; pattern
CSRW  TSRC1, R0              ; broadcast pattern tile
LDI   R1, 0                  ; dest base row/col already in SR/SC
CSRW  TDST, R1               ; base for tile addressing if used

LDI   R2, 0x400              ; 0x400 tiles = 128 KiB
fill_loop:
  7D                         ; FILL (opcode 0x7D) @ cursor, auto-advances SC/SR
  DEC  R2
  BNE  fill_loop
</code></pre>

<h3>6.2 8×8 matrix transpose in place</h3>
<pre><code>; TDST already points at matrix tile, TMODE set for 8-bit lanes
E3 00                        ; TSYS TRANS (op=11 funct=0)</code></pre>

<h3>6.3 Lane-wide dot-product of two vectors</h3>
<pre><code>LDI   R0, vecA_tile
LDI   R1, vecB_tile
CSRW  TSRC0, R0
CSRW  TSRC1, R1
CSRW  TDST,  R0              ; dest same as srcA (unused for DOT)
CSRR  R3, TCTRL
OR    R3, (1 &lt;&lt; 1)           ; set ACC_ZERO for next MEX op
CSRW  TCTRL, R3

E1 11                        ; TMUL DOT  (ACC0–ACC3 += dot)
CSRR  R2, ACC0               ; low 64 bits of result → R2
</code></pre>

<h3>6.4 Interrupt entry / exit pseudo-sequence</h3>
<pre><code>NMI_vector:
  PUSH FLAGS
  PUSH PC
  CSRR R0, IVEC_ID           ; which IRQ/trap vector index?
  ... handle ...
  POP  PC
  POP  FLAGS
  RTI                        ; restore I flag and resume
</code></pre>

<!-- ——— 7  BOOT SEQUENCE & SYSTEM MODEL ——————————————— -->
<h2 id="boot">7 Reset, Boot, and System Model</h2>
<ol>
<li>Hardware clears all regs to 0, selects R3 as PC.</li>
<li>Fetch begins at <code>0x0000 0000 0000 0000</code> (bank 0, row 0, col 0).</li>
<li>External debugger or SPI loader may DMA code into Megapad rows 0–3 while CPU spins in <code>IDLE</code>.</li>
<li>Firmware may copy itself to high Megapad rows, enable DDR, then jump to OS.</li>
<li>The core remains static-safe throughout; clock frequency may vary during boot.</li>
</ol>

<h3>7.1 Privilege Model</h3>
<p>
The architecture defines a single flat privilege level. All instructions, address ranges, and CSRs are accessible
from any code. Protection and isolation, when needed, arise from software conventions and from external fabric
components such as bus firewalls.
</p>
<p>
A reserved trap code for future privilege violations exists for forward compatibility; this revision never
generates it.
</p>

<h3 id="traps">7.2 Traps and Exceptions</h3>
<p>
The core defines at least the following traps:
</p>
<ul>
  <li>Illegal opcode</li>
  <li>Alignment fault (see Section 2.4)</li>
  <li>Divide-by-zero</li>
  <li>External bus fault (DDR/PSRAM or unmapped Megapad region)</li>
  <li>Reserved: privilege violation</li>
</ul>
<p>
Trap entry follows a deterministic pattern:
</p>
<ol>
  <li>The current instruction either completes or is discarded, as defined per trap type.</li>
  <li><code>PUSH FLAGS</code>, then <code>PUSH PC</code>, using the current stack pointer.</li>
  <li><code>IVEC_ID</code> receives a trap-specific index.</li>
  <li>Handler PC is loaded from <code>[IVT_BASE + 8 * IVEC_ID]</code>.</li>
  <li><code>FLAGS.I</code> clears to block maskable interrupts inside the handler.</li>
</ol>
<p>
The <code>RTI</code> instruction reverses this:
</p>
<ol>
  <li><code>POP PC</code></li>
  <li><code>POP FLAGS</code> (restores <code>I</code> and other bits)</li>
</ol>

<h3 id="ivt">7.3 Interrupt Vector Table (IVT) and IVEC_ID</h3>
<p>
The interrupt/trap vector table is a linear array of 64-bit handler addresses:
</p>
<ul>
  <li><code>IVT_BASE</code> points at entry 0.</li>
  <li><code>IVEC_ID</code> selects an entry; the handler PC comes from <code>IVT_BASE + 8 * IVEC_ID</code>.</li>
  <li>Maskable interrupt lines <code>INT[3:0]</code> and <code>NMI</code> map to specific <code>IVEC_ID</code> values.</li>
  <li>Trap causes map to reserved <code>IVEC_ID</code> ranges.</li>
</ul>
<p>
On interrupt or trap entry, the hardware sets <code>IVEC_ID</code> and then branches through the IVT. Software may read
<code>IVEC_ID</code> inside handlers to distinguish sources or share handler code across multiple vectors.
</p>

<h3 id="stack-model">7.4 Stack Model and RTI</h3>
<p>
The stack pointer <code>SP</code> is the GPR indexed by <code>SPSEL</code> (default <code>R15</code>). The stack grows downward and stores
full 64-bit words. External tools and ABIs are encouraged to keep <code>SP</code> 8-byte aligned.
</p>
<p>
Interrupts and traps use the same stack mechanism:
</p>
<ul>
  <li>Entry: automatic <code>PUSH FLAGS</code>, then <code>PUSH PC</code>.</li>
  <li>Exit: <code>RTI</code> performs <code>POP PC</code>, then <code>POP FLAGS</code>.</li>
</ul>
<p>
This mirrors the simplicity of the 1802 DMA and register models while supporting 64-bit call frames and modern
toolchains.
</p>

<!-- ——— 8  SIGNALS ——————————————— -->
<h2 id="signals">8 Key External Signals (RTL targets)</h2>
<table>
<thead><tr><th>Pin</th><th>Dir</th><th>Description</th></tr></thead>
<tbody>
<tr><td><code>CLK</code></td><td>in</td><td>System clock (≤ 1.2 GHz in 7 nm); core is fully static.</td></tr>
<tr><td><code>RESETn</code></td><td>in</td><td>Active-low reset.</td></tr>
<tr><td><code>DMAREQ / DMAACK</code></td><td>in/out</td><td>Transparent DMA handshake in the 1802 style.</td></tr>
<tr><td><code>NMI</code></td><td>in</td><td>Non-maskable interrupt.</td></tr>
<tr><td><code>INT[3:0]</code></td><td>in</td><td>Maskable interrupt lines (vectorised).</td></tr>
<tr><td><code>PORT[3:0]</code></td><td>in/out</td><td>Legacy 4-bit port bus for <code>IN</code>/<code>OUT</code>.</td></tr>
<tr><td><code>QSPI*</code></td><td>—</td><td>Quad-SPI flash / PSRAM.</td></tr>
<tr><td><code>SWD</code></td><td>—</td><td>3-wire debug (exec-in-place via DMA).</td></tr>
</tbody></table>

<h3>8.1 DMA Handshake and Static Behaviour</h3>
<p>
DMA follows a simple, deterministic handshake:
</p>
<ol>
  <li>External logic asserts <code>DMAREQ</code>.</li>
  <li>The core completes the current instruction, raises <code>DMAACK</code>, and freezes the pipeline and <code>PC</code>.</li>
  <li>External logic performs memory/Megapad transfers while <code>DMAACK</code> is high.</li>
  <li>External logic deasserts <code>DMAREQ</code>; the core deasserts <code>DMAACK</code> and resumes at the frozen <code>PC</code>.</li>
</ol>
<p>
No architectural register (GPR, FLAGS, CSR) changes during the DMA window except for memory locations directly
touched by the external transfer. Combined with the fully static clocking model, this enables very fine-grained
low-power control and transparent debug flows.
</p>

<!-- ——— 9  ARCHITECTURAL DIFFERENCES ——————————————— -->
<h2 id="archdiffs">9 Megapad vs GPU – Architectural Divergence</h2>

<aside class="note">
<strong>Why not just use a GPU?</strong> Megapad-64 pursues a different path from typical GPUs. It trades warp-scale
parallelism for tile-native determinism, bit-level precision, and single-cycle scratchpad access — enabling new
classes of workloads that GPUs mishandle or overheat trying.
</aside>

<div class="col2">
<table>
<thead><tr><th>Feature</th><th>Megapad-64</th><th>GPU SM / CU</th></tr></thead>
<tbody>
<tr><td>Exec model</td><td>Single 512-bit datapath; no warps, no divergence</td><td>SIMT (32–64 threads); warp divergence stalls</td></tr>
<tr><td>Scratch memory</td><td>8–64 MiB SRAM; 1 cycle, 2-D tile native</td><td>64–128 KiB shared; coalesced access only</td></tr>
<tr><td>Instruction width</td><td>8-bit opcodes; deterministic decode</td><td>32/64-bit; superscalar, out-of-order</td></tr>
<tr><td>Memory latency</td><td>1 cycle to tile; flat banked map</td><td>100–500 cycles to L2 or global mem</td></tr>
<tr><td>Interrupts</td><td>Regular + NMI; DMA pointer aliasing</td><td>No standard IRQ entry in kernels</td></tr>
<tr><td>CSR / I/O</td><td>Memory-mapped CSRs &amp; 4-bit ports</td><td>Host-managed; limited sub-µs device I/O</td></tr>
<tr><td>Power budget</td><td>&lt; 1 W typical @ 1 GHz, 0.7 V (8 MiB); 2–3 W worst-case</td><td>10–400 W typical</td></tr>
</tbody>
</table>
</div>

<h3>9.1 What Megapad-64 does easily — and GPUs struggle with</h3>

<ul>
  <li><strong>Bitwise-heavy math:</strong> POPCNTs, bloom filters, byte-parallel automata — all live in tile, free from cache thrash and vector mask stalls.</li>
  <li><strong>Strided 2-D access:</strong> Row/col walkers, tile transposes, convolution helpers all operate in-place, cycle-accurate.</li>
  <li><strong>Graph traversal:</strong> Scratchpad fits adjacency subgraphs; pointer-chasing remains deterministic with <code>LL/SC</code>, without long global-memory stalls.</li>
  <li><strong>Massive dot products, exactly:</strong> Accumulators span 256+ bits across banks — no need for Kahan summation or floating fudge factors.</li>
  <li><strong>Cycle-predictable IO:</strong> Real-time tasks, protocol FSMs, and GPIO loops fit right in — including interruptible micro-loops.</li>
</ul>

<h3>9.2 Ideal Workloads</h3>
<p>Megapad excels when:</p>
<ul>
  <li>Data fits in ≤ 64 MiB, and reuse patterns are 2-D or tile-parallel.</li>
  <li>Latency matters more than sheer throughput (e.g. edge analytics, real-time control).</li>
  <li>You want <strong>bitwise determinism</strong> — no warp-level divergence, no cache unpredictability.</li>
  <li>You need <strong>accumulator fidelity</strong> — exact reductions over 8–64 element arrays.</li>
</ul>

<h3>9.3 Where GPUs still win</h3>
<ul>
  <li>Massive FP32/FP16 throughput (e.g. CNNs, raytracing, fluid sims).</li>
  <li>Off-chip bandwidth to HBM/GDDR in TB/s range.</li>
  <li>Hardware blocks: NVENC, RT cores, texture units.</li>
</ul>

<aside class="tip">
<strong>Use case sweet spot:</strong> Bit-dense logic, tile-native math, pointer-rich data structures, embedded signal pipelines. Megapad-64 sidesteps many GPU pain points instead of trying to outrun them.
</aside>

<!-- ——— 10  ADAPTIVE SAMPLING CASE STUDY ——————————————— -->
<h2 id="case-study">10 Case Study — Real-Time Adaptive Sequencing</h2>

<aside class="note">
<strong>Use case:</strong> Field DNA sequencing using nanopore sensors requires live analysis and decision-making —
including the ability to <em>eject DNA strands mid-read</em> if they are not relevant. Known as “adaptive sampling,” this
reduces pore wear and improves target enrichment.
</aside>

<h3>10.1 Problem</h3>
<p>Conventional pipelines (e.g. Guppy + GPU or CPU-only Bonito) incur multi-millisecond latency due to:</p>
<ul>
  <li>USB buffer copy to host memory</li>
  <li>Kernel launch and PCIe transfer delays</li>
  <li>Non-deterministic task preemption in general-purpose OSes</li>
</ul>
<p>This makes timely pore ejection difficult: most systems over-read by hundreds of base pairs, reducing selectivity.</p>

<h3>10.2 Solution</h3>
<p><strong>Megapad-64 executes the entire pipeline in-tile:</strong></p>
<ul>
  <li>64 B raw signal tile is ingested via DMA</li>
  <li>1-D CNN base-calling via <code>TSYS CONV3</code></li>
  <li>Beam-search decoding and hash lookup for k-mer matching</li>
  <li>Branchy decision logic executed in-line</li>
  <li>Pore ejection triggered via a <code>CSRW</code> to a pore-eject CSR in the next cycle</li>
</ul>

<table>
  <thead><tr><th>Stage</th><th>Cycles @ 1 GHz</th><th>Description</th></tr></thead>
  <tbody>
    <tr><td>Ingest tile</td><td>1</td><td>DMA 64-byte sample block into Megapad</td></tr>
    <tr><td>CNN conv (CONV3)</td><td>40</td><td>8 filters, stride-1</td></tr>
    <tr><td>CTC decode</td><td>60</td><td>Beam width 4, priority logic</td></tr>
    <tr><td>k-mer hash lookup</td><td>20</td><td>Minimiser in SRAM</td></tr>
    <tr><td>Decision + eject</td><td>4</td><td><code>CSRW PORE_EJECT, Rn</code> toggles pore control GPIO</td></tr>
    <tr><td><strong>Total core compute</strong></td><td><strong>~125–130</strong></td><td>≈130 ns @ 1 GHz</td></tr>
  </tbody>
</table>

<p>
The remainder of the ~48 µs “sensor-to-eject” latency budget comes from the nanopore front-end: ADC integration
time, front-end filtering, framing, and the time required to accumulate a 64-byte tile-worth of samples. The core
itself contributes a tiny, deterministic slice of the timeline.
</p>

<h3>10.3 Impact (Hypothetical)</h3>
<p>Compared to conventional solutions:</p>
<table>
  <thead><tr><th>Metric</th><th>GPU-based</th><th>CPU-only</th><th><strong>Megapad-64</strong></th></tr></thead>
  <tbody>
    <tr><td>Median eject latency</td><td>800 µs</td><td>500 µs</td><td><strong>48 µs</strong></td></tr>
    <tr><td>Base pairs saved/read</td><td>37%</td><td>44%</td><td><strong>62%</strong></td></tr>
    <tr><td>Power per read</td><td>1875 µJ</td><td>1450 µJ</td><td><strong>≤150 µJ</strong></td></tr>
  </tbody>
</table>

<aside class="tip">
<strong>Predicted Result:</strong> Megapad-64 enables <em>fully deterministic, sub-millisecond feedback</em> in hand-held molecular diagnostic kits.
The combination of static core, Megapad, and MEX allows tight latency, power, and selectivity targets to be met together.
</aside>

</body>
</html>

