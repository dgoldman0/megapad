# Microcores: kernels, fetch traffic and QoS

Status: direction accepted on 2026-10-27; nothing is implemented. This note
records why the microcores sit idle, the agreed way to give them work, two
places where the RTL and boot fall short of the design, and ideas for keeping
microcore memory traffic away from the full cores. It schedules no work.
Akashic's side, the kernels it would run and where extra cores help Desk, is
Akashic's `docs/concurrency/microcore-kernels.md`.

## 1. Why the microcores sit idle

Each of the three clusters has four microcores that share a multiplier and
divider, one FP unit, one tile/MEX engine, CRC and SHA-2 state, a 1 KiB
scratchpad, a hardware barrier, and one bus port to the main arbiter
([architecture.md](architecture.md#micro-core-architecture),
`rtl/core/mp64_cluster.v`, [floating-point.md](floating-point.md) §8.7).
Nothing in KDOS, MegaPad's system modules or Akashic runs on them. Only ISA
tests, cluster tests and benchmarks do.

They cannot run today's BIOS Forth:

- **By design**, microcores drop the CDP1802-heritage features. The D, Q and T
  registers, the MEMALU family (which includes `STXI`), port I/O,
  `GLO`/`GHI`/`PLO`/`PHI` and `RET`/`DIS`/`MARK`/`SAV`/`SEQ`/`REQ` trap as
  `ILLEGAL_OP` ([architecture.md](architecture.md#micro-core-architecture)).
  Every EXT.DICT sub-op traps as well ([isa-reference.md](isa-reference.md)).
- **The BIOS uses those instructions on ordinary paths.** `compile_call`
  stores call bytes with `SEX`, `GLO` and `STXI`; `emit_char` uses `SEQ` and
  `REQ` as its UART-busy semaphore; FIND issues `DFIND` (`bios.asm`).
- **The RTL microcore has 16 general registers** (`R [0:15]` in
  `rtl/core/mp64_cpu_micro.v`); a full core has 32. The BIOS loads R19 with
  the TX ring and R16 and R17 with the JIT's NEXT and EXIT targets on every
  core it starts.
- **Boot gives microcores nothing to run on.** The emulator holds reduced
  cores at reset (`emulator/system.py`), and no code sets up their stacks.

The first two points are design. Microcores keep loads, stores, arithmetic,
branches, `CALL.L`/`RET.L` and the cluster units, so code that avoids the
dropped instructions can run on them. The last two points are not design;
section 3 covers them.

## 2. Direction: kernel words compiled for microcores

Microcores run kernels: small words written in a restricted Forth subset that
the BIOS JIT compiles into native code using only the instructions a
microcore keeps. Kernels are neither hand-written machine code nor general
Forth.

A kernel word:

- uses only an admitted kernel vocabulary, checked when the word is
  compiled. It has no dictionary access, no heap or `ALLOT`, no I/O and no
  `THROW`;
- reads and writes only buffers its caller provides. On the Akashic side
  this is the worker-job contract and its `PURE`, `SNAPSHOT-READ` and
  `EXCLUSIVE-BUFFER` classes;
- keeps its data stack in registers. The compiler assigns stack positions to
  registers and spills only into a small per-microcore area, so the kernel
  touches memory for its data alone;
- reports failure through a result code rather than an exception.

Launch, completion and the calling convention still need a design (section
6). They can use the cluster barrier, the mailbox or a scratchpad flag. The
SoC currently ties every microcore's IPI input to zero (`irq_ipi` in
`rtl/soc/mp64_soc.v`), while the emulator's mailbox reaches every core, so
the launch design must either work without microcore IPIs or wire them.

## 3. Two gaps to close

- **Register count: 32** (decided 2026-10-27). Microcores run the same ISA
  as full cores minus the 1802-heritage features, so they get all 32
  registers, which the emulator already models. Thirty-two keeps one
  register model for kernel compilation and leaves room for register-held
  kernel stacks, so fewer spills compete for the cluster's bus port. The
  cost is 16 more 64-bit registers in each microcore. The RTL implements 16
  today. Read from its source, though not yet tested, it accepts the REX
  prefix but ignores the extension bit, so an instruction naming R17 uses
  R1. The RTL work widens the register file to 32 and decodes REX's
  extension bits as the full core does. A test then runs R16 to R31 on a
  microcore in both the RTL and the emulator.
- **Stacks and entry.** Boot leaves microcores halted with no stack. The
  kernel runtime must give each microcore a spill area and an entry
  convention before any kernel runs. The original microcore notes leaned the
  same way: each microcore owns its own stack region
  ([IDEAS-scratchpad.md](IDEAS-scratchpad.md) §1, Open Questions).

## 4. Fetch traffic and QoS

A microcore has no instruction cache. It fetches each instruction byte
through its cluster's arbiter into an 11-byte instruction buffer
(`rtl/core/mp64_cpu_micro.v`). Busy microcores therefore take main-bus time
from the full cores, including core 0, which runs Desk.

The design's answer stands: two-level QoS, with hard QoS at the main arbiter
and equal peers inside each cluster
([IDEAS-scratchpad.md](IDEAS-scratchpad.md) §1, Two-Level QoS). The ideas
below cut the traffic at its source so that QoS limits rarely bind. They do
not replace QoS or change what a microcore is.

Two parts of that design are missing today:

- `rtl/bus/mp64_bus.v` implements weighted round-robin with a weight of 1 to
  255 per port and a bandwidth limit per port for each 65,536-cycle epoch.
  The SoC ties the QoS CSR sideband off (`qos_csr_wen` is `1'b0` in
  `mp64_soc.v`), so every port keeps its reset weight of 1 and no limit.
  The CSR table in [architecture.md](architecture.md) records the same gap.
  Wiring the sideband restores the designed hard QoS.
- The original design gave each cluster a shared 1–2 KiB instruction cache.
  `mp64_cluster.v` was built without it.

Ideas, cheapest first:

1. **Run kernels from the cluster scratchpad.** The cluster already serves
   scratchpad addresses internally in one cycle and never sends them to its
   main-bus port (scratchpad address detection in `mp64_cluster.v`). A kernel
   copied there costs the main bus nothing to fetch. The scratchpad holds
   1 KiB, shared with data, so this suits small kernels. Check first that
   fetching code from it works: the RTL moves whole qwords to and from the
   scratchpad, Python microcores use byte access, and the byte scratchpad
   words are not yet qualified across backends
   ([memory-management.md](memory-management.md)).
2. **Fetch a 64-bit word at a time.** Today every instruction byte is its own
   request. A one-word fetch buffer per microcore, not a cache, would cut fetch
   requests several-fold.
3. **Build the shared cluster instruction cache the design called for.** It
   serves kernels larger than the scratchpad. The four microcores of a cluster
   usually run the same kernel, so one copy serves all of them.
4. **Keep kernel stacks in registers** (section 2), which leaves memory
   traffic to the data a kernel processes.
5. **Move bulk data through the tile engine.** Each cluster has its own
   512-bit tile memory port, separate from its main-bus port.
6. **Schedule around the interactive core.** Background kernels run while
   core 0 waits for input, and they check a pause flag between blocks that
   core 0 sets when it starts a frame. This is a software rule on top of QoS,
   not a replacement for it.

## 5. Uses

Akashic's candidate consumers are audio processing, hashing, search,
simulation and analysis, and plot math. Akashic's
`docs/concurrency/microcore-kernels.md` describes them and the rule that a
shared kernel facility waits for independent consumers to prove a common
contract.

## 6. Open questions

- The kernel vocabulary, and how the compiler checks a word against it.
- Launch and completion: barrier, mailbox or scratchpad flag, given that the
  RTL ties microcore IPIs off.
- Whether microcores can fetch from the scratchpad on every backend (idea 1).
- Which fetch ideas to build, and in what order, once a first kernel is
  measured.
- Wiring the QoS sideband, and the default weights and limits for cluster
  ports.
