# Full TACC ISA and implementation handoff

- Status: Phase 1 complete and integrated with the Phase 5 production
  scheduler; Phase 2 functional Landings 2.1 through 2.8 complete; Landing
  2.9 preparation is in progress, while routed physical acceptance remains
  blocked on the production target/memory decision and Vivado availability
- Date: 2026-07-29
- Phase-1 feature branch: `feature/megapad-full-tacc`
- Phase-1 feature tip: `967dfc0d5792f9feaec9820b0a73d7b2212304c8`
- Integration branch: `integration/phase5-full-tacc`
- Phase-5 merge: `5f1e4e51a48a3394504b95d92cefb111b92fd616`
- Combined TACC merge: `895fca8e2e26c7f91cc525e90e5edd3ab13cb0f4`
- Phase-0 base commit: `c8e8118e82a899ec3f101f63d277a1bf4ef5f84a`
- Phase-1 worktree:
  `/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc`
- Integration worktree:
  `/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-phase5-full-tacc-integration`
- Phase-2 branch: `feature/megapad-full-tacc-rtl`
- Phase-2 worktree:
  `/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc-rtl`

This document supersedes the condensed workspace-root
`CHIP_MATH_UPDATE_HANDOFF.md`. The original correctly identified persistent
lane accumulation as valuable, but its proposed 512-bit state was not wide
enough to preserve products, it did not define ownership on shared engines,
and it did not provide an executable implementation sequence.

## 1. Locked outcome

Megapad will gain one **2,048-bit tile accumulator (TACC) per physical tile
engine**. Source tiles remain 512 bits wide. Products are widened and added
lane-by-lane into TACC; the ordinary tile datapath is not widened globally.

The chip has exactly seven physical tile engines:

1. one private engine for full core 0;
2. one private engine for full core 1;
3. one private engine for full core 2;
4. one private engine for full core 3;
5. one engine shared by microcluster 0;
6. one engine shared by microcluster 1; and
7. one engine shared by microcluster 2.

This restores the documented topology: every full core has private tile
execution and each four-microcore cluster amortizes one engine behind a local
round-robin arbiter. The current portable RTL connects only full core 0 and
ties off MEX completion for full cores 1–3; that is an implementation gap, not
the architectural contract. Phase 2 restores those three private instances.
Every one of the seven engines receives its own TACC bank and lifecycle state.

TACC is explicit architectural state. Software claims it, initializes it by
clearing or loading, accumulates into it, stores it, and releases it. Hardware
does not infer lifetimes, evict owners, silently spill state, or redirect an
existing instruction according to a sticky mode bit.

The existing 256-bit reduction accumulator, `ACC0`–`ACC3`, remains a separate
architectural object. Existing `TMAC`, `TFMA`, `TDOT`, `TDOTACC`, `TRED`, and
`TCTRL` behavior is not redefined by TACC.

## 2. Phase map

| Phase | Outcome | Exit condition |
|---|---|---|
| **0 — contract** | Lock the ISA, state, ownership, memory image, numeric behavior, topology, failure behavior, resource limits, and landing sequence. | This document is reviewed and committed alone. |
| **1 — emulator** | Make the Python oracle, assembler, system topology, native C++ accelerator, guest words, and differential tests implement the contract. | Python and native execution agree bit-for-bit; seven-engine isolation and cluster contention tests pass; public ISA documentation is current. |
| **2 — RTL** | Implement the same contract in private full-core engines, cluster arbiters, CPU interfaces, memory routing, integer and floating datapaths, and RTL verification. | Emulator-generated vectors match RTL; focused and integration benches pass; target synthesis meets the resource and timing gates. |

Phase 1 is the executable oracle for Phase 2. RTL must conform to it; Phase 2
must not quietly redefine Phase-1 results to match a convenient circuit.

## 3. Architectural topology and state ownership

### 3.1 Physical versus caller-private state

Each physical tile engine owns:

- one legacy 256-bit `ACC`;
- one 2,048-bit TACC bank;
- TACC ownership, validity, dirty, format, and busy metadata; and
- the arithmetic and memory-transfer machinery used by tile instructions.

Each full core is the sole hardware caller of its private engine, so that
engine's complete CSR and accumulator state is naturally private. Within each
microcluster, every microcore owns private shadow copies of:

- `SB`, `SR`, `SC`, and `SW`;
- `TMODE` and `TCTRL`;
- `TSRC0`, `TSRC1`, and `TDST`; and
- `TSTRIDE_R`, `TSTRIDE_C`, `TTILE_H`, and `TTILE_W`.

The selected microcore's shadows are sampled with the granted MEX request.
Another microcore may change its own shadows while an operation is active
without changing the granted operation.

`ACC0`–`ACC3`, TACC, `TACC_STATUS`, and `TACC_CTL` follow engine ownership.
They are private to a full core and shared by the four microcores within one
microcluster. The chip therefore has seven legacy ACC domains and seven TACC
domains. Phase 2 must reconcile both existing duplicate implementations:

- each full CPU's local `acc_reg` and its private tile module's `acc`; and
- each microcluster's `cl_sha_acc` and its shared tile module's `acc`.

Each engine exposes exactly one architecturally visible legacy ACC to CSR,
SHA, and tile operations. A full core's single instruction stream already
serializes writers. Each cluster adds an acknowledged deterministic common
ACC admission point across `CSR_ACC0`–`CSR_ACC3`, SHA, and MEX so reads are
coherent and simultaneous writers cannot race. SHA samples the granted
microcore's private `TSRC0` shadow. `TCTRL` remains caller-private;
`ACC_ZERO` takes effect atomically with the granted caller's MEX operation
and auto-clears only that caller's shadow.

`TACC_CTL` uses a separate acknowledged cluster-control sideband rather than
waiting behind MEX admission. A privileged sibling can therefore latch
`FORCE_PENDING` while the owner's TACC operation is active. This feature does
not integrate the currently bench-only Field ALU RTL into the production SoC.

### 3.2 Arbitration

Full-core MEX instructions dispatch directly to that core's private engine.
Microcore MEX instructions, including TACC lifecycle instructions, enter the
microcluster engine's ordinary deterministic arbiter. Requests from all seven
engines enter the existing shared tile-memory port arbiter when they need
memory.

- Cluster admission and the seven-source tile-memory port are equal
  round-robin in this work.
- Production tile-memory requestor IDs are fixed: 0–3 are full cores 0–3
  and 4–6 are microclusters 0–2. Requestor and write-completion metadata use
  three bits.
- A future software-programmable weight mechanism may alter admission order,
  but it must not alter TACC semantics.
- TACC ownership reserves only persistent TACC state. It does not reserve the
  tile engine while the owner is idle.
- Nonowners may continue to execute stateless MEX operations and legacy
  ACC-producing operations.
- A failed `TACC.TRY` retires normally and never waits internally.
- TACC operations other than `TACC.TRY` require ownership and fail before
  mutation when issued by a nonowner.

This keeps waiting and backoff policy in software. A BIOS or OS word may loop
over `TACC.TRY` and `PAUSE`, but the ISA does not contain an unbounded blocking
claim.

In the production 4+3 topology, full-core IDs are 0–3 and microcluster core-ID
bases are 4, 8, and 12. `TACC_STATUS.OWNER` always reports the issuing
system's actual absolute `COREID`, never a cluster-local index. Configurable
emulator instances and parameter-reduced RTL test builds may retain their
existing compact core IDs and compact only the instantiated requestor slots;
those are verification configurations, not alternate production mappings.
Both the production mapping and reduced-configuration mapping are tested
explicitly.

Private full-core placement does not make the lifecycle implicit. A full core
still claims, initializes, stores, and releases its TACC through the same ISA;
its claim cannot lose to another core. If state remains claimed by that same
core ID, `TACC.TRY` is idempotent and the OS remains responsible for tracking
which task owns it. This keeps software context accounting and recovery
uniform across full cores and microclusters.

## 4. ISA encoding

### 4.1 Lane accumulation

`TMUL` function 6 is assigned to `TAMAC`.

| Form | Encoding | Meaning |
|---|---|---|
| `TAMAC` tile × tile | `E1 06` | `TACC[i] ← TACC[i] + widen([TSRC0][i] × [TSRC1][i])` |
| `TAMAC` tile × scalar | `E5 06 Rn` | Use the low active element of `Rn` as the scalar for every lane. |
| `TAMAC` in-place source form | `ED 06` | Use `[TDST]` as A and `[TSRC0]` as B; TACC remains the only destination. |

Assembly spelling is `t.amac`. The BIOS/Forth word is `TAMAC`.

Source selector 2, immediate splat, is illegal for `TAMAC` because that MEX
form uses the function byte as the immediate. It traps before reading memory
or changing TACC.

The `TAMAC` function byte is exactly `0x06`; nonzero upper five bits are
noncanonical and trap as illegal.

`TMUL` function 7 remains reserved. Existing `TMUL` functions 0–5 retain
their current meanings, especially `TMAC`, which continues to update `TDST`.

### 4.2 Lifecycle operations

The existing `F8` extended-tile namespace and unused extended-TSYS functions
2–6 carry the lifecycle instructions. Their canonical source selector is
zero; any other source selector is illegal.

| Function | Encoding | ISA name | Assembly spelling | Operation |
|---:|---|---|---|---|
| 2 | `F8 E3 02` | `TACC.TRY` | `t.acc.try` | Atomically claim a free TACC, or retain self-ownership. Never waits. |
| 3 | `F8 E3 03` | `TACC.CLEAR` | `t.acc.clear` | Require ownership, latch the current format, and initialize every active lane to zero. |
| 4 | `F8 E3 04` | `TACC.LOAD` | `t.acc.load` | Require ownership and load the canonical 256-byte image at `TSRC0`. |
| 5 | `F8 E3 05` | `TACC.STORE` | `t.acc.store` | Require valid owned state and store the canonical image at `TDST`. |
| 6 | `F8 E3 06` | `TACC.RELEASE` | `t.acc.release` | Require ownership, zeroize and invalidate the bank, then release it. |
| 7 | `F8 E3 07` | reserved | — | Trap as illegal. |

BIOS/Forth words are `TACC-TRY`, `TACC-CLEAR`, `TACC-LOAD`,
`TACC-STORE`, and `TACC-RELEASE`.

The upper five bits of the function byte must be zero for these instructions.
Noncanonical encodings trap as illegal instead of creating aliases.

### 4.3 TACC status CSR

`TACC_STATUS` is a read-only CSR at `0x1D`. It is caller-relative only for
`MINE`; all other fields describe the physical engine.

| Bits | Name | Meaning |
|---|---|---|
| `[0]` | `CLAIMED` | One caller owns this engine's TACC. |
| `[1]` | `MINE` | `CLAIMED` and the owner is the reading core. |
| `[2]` | `VALID` | `CLEAR` or `LOAD` established an accumulator format and value. |
| `[3]` | `DIRTY` | State changed since the last successful `LOAD` or `STORE`. |
| `[4]` | `BUSY` | A TACC arithmetic or lifecycle operation is in flight. |
| `[7:5]` | `FORMAT_EW` | Latched `TMODE.EW`; zero when invalid. |
| `[8]` | `FORMAT_SIGNED` | Latched integer signedness; zero for floating formats or invalid state. |
| `[9]` | `FORCE_PENDING` | A privileged force-release is queued behind the active instruction. |
| `[15:10]` | reserved | Read as zero. |
| `[20:16]` | `OWNER` | Absolute core ID 0–31; `31` means no owner. |
| `[63:21]` | reserved | Read as zero. |

`TACC.TRY` has no hidden flag result. Software reads `TACC_STATUS.MINE`.
Because a successful claim persists until release, this is an atomic and
race-free success test:

1. execute `TACC.TRY`;
2. read `TACC_STATUS`; and
3. proceed only when `MINE=1`.

When another core owns the bank, `TACC.TRY` changes no TACC data or metadata
and retires normally.

### 4.4 TACC control CSR

`TACC_CTL` is at `0x1E`.

- Reads return zero.
- Bit 0 is supervisor-only `FORCE_RELEASE`, a write-one pulse.
- Bits 63:1 are reserved and ignored.
- A user-mode write of bit 0 raises `IVEC_PRIV_FAULT`.

This is an intentional narrow privilege check even in execution paths where
the retained privilege field is otherwise inert. A full core authorizes the
write from its `priv_level`; a microcore uses its cluster's authoritative
`cl_priv_level`. Phase 1 and Phase 2 must enforce this identically. This
feature does not otherwise reintroduce a removed MPU or broaden user-mode
enforcement.

`FORCE_RELEASE` zeroizes the bank, clears valid/dirty/format metadata, and
removes ownership. If a TACC instruction is already active, the force request
sets `FORCE_PENDING`, blocks admission of another TACC instruction, and takes
effect at the active instruction's terminal boundary: normal retirement or
trap completion. Stateless MEX work may continue afterward. Whole-SoC or
paired-engine reset and cluster disable do not wait; they immediately wipe
TACC and ownership.

An accepted `FORCE_RELEASE` has priority over same-cycle TACC admission. If no
TACC operation was active at the start of the cycle, hardware wipes the bank
and leaves the competing MEX request pending for fresh validation on a later
cycle. If an operation was already active, hardware sets `FORCE_PENDING`;
completion or fault is published first and the wipe follows at that terminal
boundary. No new TACC request is admitted between those events.

This is the recovery path for a terminated or otherwise dead owner. Ordinary
software must use `TACC.RELEASE`.

## 5. Lifecycle state machine

The architectural state is:

```
FREE
  └─ TACC.TRY by caller A ─→ OWNED_INVALID(A)

OWNED_INVALID(A)
  ├─ TACC.CLEAR by A ─────→ OWNED_VALID_DIRTY(A, format)
  ├─ TACC.LOAD by A ──────→ OWNED_VALID_CLEAN(A, format)
  └─ TACC.RELEASE by A ───→ FREE

OWNED_VALID(A, format)
  ├─ TAMAC by A ──────────→ OWNED_VALID_DIRTY(A, format)
  ├─ TACC.STORE by A ─────→ OWNED_VALID_CLEAN(A, format)
  ├─ TACC.CLEAR by A ─────→ OWNED_VALID_DIRTY(A, new format)
  ├─ TACC.LOAD by A ──────→ OWNED_VALID_CLEAN(A, new format)
  └─ TACC.RELEASE by A ───→ FREE
```

The `CLEAR` transition is marked dirty because zero is an architecturally
meaningful value that differs from an unknown or previously loaded context.

These rules are locked:

- `TRY` establishes ownership but not validity.
- `CLEAR` and `LOAD` require `MINE` and establish `VALID`.
- `TAMAC` and `STORE` require both `MINE` and `VALID`.
- `RELEASE` requires `MINE`, then zeroizes and invalidates.
- `TRY` by the current owner is idempotent.
- A format change is legal only through `CLEAR` or `LOAD`.
- `TAMAC` compares the current `TMODE.EW` and integer signed bit with the
  latched format. A mismatch traps before source reads or mutation.
- Floating formats ignore `TMODE.SIGNED`; the latched signed field is zero.
- `TMODE` saturation and shift-rounding bits are not part of the TACC format
  and do not affect accumulation.
- `STORE` uses the latched format and does not require current `TMODE` to
  match.
- Store, interrupt, trap, task switch, and ordinary MEX execution never
  release ownership implicitly.

## 6. Numeric contract

### 6.1 Legal modes and physical layout

| `TMODE.EW` | Input lanes | Product interpretation | TACC lane | Active image bytes |
|---:|---:|---|---:|---:|
| 0 — 8-bit integer | 64 | exact signed or unsigned 8×8 product | 32-bit integer | 256 |
| 1 — 16-bit integer | 32 | exact signed or unsigned 16×16 product | 64-bit integer | 256 |
| 2 — 32-bit integer | 16 | exact signed or unsigned 32×32 product | 64-bit integer | 128 |
| 3 — 64-bit integer | — | unsupported | — | — |
| 4 — FP16 | 32 | exact FP16 product represented in binary32 | binary32 | 128 |
| 5 — BF16 | 32 | exact BF16 product added before binary32 rounding | binary32 | 128 |
| 6–7 | — | reserved | — | — |

EW 3, 6, or 7 on `CLEAR`, `LOAD`, or `TAMAC` raises
`IVEC_ILLEGAL_OP` before mutation. Supporting 64×64→128 accumulation is not
part of this feature.

Physical lane `i` begins at bit `i × accumulator_lane_width`. Active lanes
are contiguous from bit zero. Inactive high bits of the 2,048-bit bank are
always zero after `CLEAR`, `LOAD`, or `TAMAC`.

### 6.2 Integer accumulation

For every integer lane:

1. interpret both input elements according to the latched signedness;
2. form the exact product at twice the input width;
3. sign-extend or zero-extend it to the accumulator lane width; and
4. add modulo `2^accumulator_lane_width`.

Accumulation never saturates. It is intentionally widened enough for useful
multi-step accumulation, while wrap behavior remains exact and testable.
Narrowing, rounding, and saturation are separate explicit work and are not
silently applied by `TACC.STORE`.

The scalar broadcast form uses only the low input-width bits of the selected
GPR. The current RTL behavior that repeats the complete 64-bit register
pattern is nonconforming and will be corrected for all MEX broadcast
operations during Phase 2.

### 6.3 FP16 and BF16 accumulation

Each lane is evaluated in program order:

```
TACC.fp32[i] = round_binary32(
    TACC.fp32[i] + exact_product(srcA.half[i], srcB.half[i])
)
```

The product is not rounded back to FP16 or BF16 before it reaches binary32.
For FP16, every finite product is representable with sufficient binary32
significand precision before the addition. BF16 extreme exponents may still
overflow or underflow binary32 and follow the rules below.

The contract is:

- round-to-nearest, ties-to-even;
- binary32 subnormals are supported and are not flushed to zero;
- signed zero follows IEEE-754 round-to-nearest behavior;
- infinities behave as IEEE-754 values;
- any NaN input, `0 × infinity`, or invalid infinity addition produces the
  canonical quiet NaN `0x7FC00000`;
- once a lane contains NaN, later `TAMAC` operations leave it canonical NaN;
  and
- there is one binary32 rounding point per lane per `TAMAC`.

Python host `float` evaluation is not itself the oracle. Phase 1 must provide
a bit-exact helper, and Phase 2 must use vectors generated by that helper.
The current RTL path that rounds a product to half and then widens it is not
valid for TACC.

## 7. Canonical memory image

`TACC.LOAD` and `TACC.STORE` transfer exactly 256 bytes, aligned to a 64-byte
boundary, as four consecutive 64-byte tile beats.

- Lanes are in increasing lane order.
- Each integer or binary32 lane is little-endian.
- U8/S8 and U16/S16 modes occupy all 256 bytes.
- U32/S32, FP16, and BF16 modes occupy bytes 0–127.
- `STORE` writes zero to bytes 128–255 for the 128-byte-active modes.
- `LOAD` ignores incoming inactive bytes and commits zeros there.
- `TSRC0`, `TDST`, and all cursor CSRs are unchanged by the transfer.
- Software saving a context must save the format from `TACC_STATUS` or its
  own `TMODE` value alongside the 256-byte image.

`LOAD` latches the current `TMODE.EW` and signedness after validating that the
mode is legal. It stages all four beats and changes no TACC state if any beat
fails.

`STORE` validates alignment, privilege, and the complete address span before
issuing its first write. It clears `DIRTY` only after all four writes are
acknowledged. A transport failure after an acknowledged external-memory beat
may leave an acknowledged prefix visible, as with other multi-beat bus
operations; TACC remains valid and its preinstruction `DIRTY` value is
unchanged.

Internal memory, attached RAM, and external RAM must use the same
architectural image. MMIO is not a legal TACC image target.

The microcluster-local scratchpad aperture is not a legal source or image
route for `TAMAC`, `TACC.LOAD`, or `TACC.STORE`. The 64-byte source and
256-byte image preflights reject that aperture with `IVEC_BUS_FAULT` before
issuing traffic, even when the caller's scalar
`mex_allow_cluster_spad` policy is enabled. Supporting wide scratchpad tile
traffic later requires an explicit physical route and a new contract update;
the policy bit alone must never turn an unconnected aperture into a hanging
request.

Image preflight applies the issuing caller's ordinary scalar memory-access
policy to the complete 256-byte span, including its active MPU window or
equivalent routed-memory permissions. A one-bit privilege signal is not
sufficient. RTL tile-memory and external-memory paths must return an
acknowledged error and fault address; an invalid request must never wait
forever for an ACK.

Only one TACC image transfer may own the chip-wide staging image at a time.
Contending image transfers acquire it with equal round-robin service across
the seven physical engines, and ownership spans all four beats through
completion, fault, or cancellation. The memory arbiter may serve ordinary
tile traffic between those beats, but a second `TACC.LOAD` or `TACC.STORE`
waits for the staging owner to finish or cancel. Those waits use ordinary
stall accounting and are modeled in Phase 1 as well as RTL.

External memory provides an explicit PHY-error sideband. A PHY that cannot
report a device error ties it low, but the external-memory bridge still
converts 255 cycles without a response on the current 64-bit PHY word into an
acknowledged bus error. Its fault address is the current PHY word address;
tile-level address errors report the base of the faulting 64-byte tile beat.
This timeout is part of the cycle model and prevents a TACC transfer from
hanging indefinitely.

## 8. Fault, retirement, and reset contract

| Condition | Result |
|---|---|
| Failed `TACC.TRY` because another owner exists | Normal retirement; no TACC mutation; `MINE=0`. |
| Protected TACC operation by a nonowner | `IVEC_ILLEGAL_OP`; no source read, memory write, state, or metadata mutation. |
| `TAMAC`/`STORE` while invalid | `IVEC_ILLEGAL_OP`; no mutation. |
| Unsupported or mismatched format | `IVEC_ILLEGAL_OP`; no mutation. |
| Reserved/noncanonical encoding or illegal source selector | `IVEC_ILLEGAL_OP`; no mutation. |
| Misaligned TACC image address | `IVEC_ALIGN_FAULT`; no TACC mutation and no store beat issued. |
| Invalid/forbidden image span | Existing bus or privilege fault; `LOAD` leaves TACC unchanged and `STORE` issues no beat after deterministic preflight failure. |
| User-mode `TACC_CTL.FORCE_RELEASE` | `IVEC_PRIV_FAULT`; no mutation. |

Alignment faults set `TRAP_ADDR` to the misaligned base. Span-preflight faults
set it to the first forbidden byte. A source or transfer bus error sets it to
the faulting beat address. Illegal-operation and ownership faults leave
`TRAP_ADDR` unchanged. Every TACC trap saves the architectural PC after the
complete decoded instruction—the same end-of-instruction return-PC convention
used by existing MEX and crypto faults.

Arithmetic results and normally completing lifecycle results become visible
only at the terminal MEX boundary. RTL may update hidden lane slices
internally, but no other instruction may observe a partial accumulation.
`BUSY` becomes visible when an operation is admitted, `FORCE_PENDING` becomes
visible when its control write is accepted, and reset or cluster disable may
wipe state immediately. A fault publishes no partial TACC result before its
trap-completion boundary.

`PERF_TILE_OPS` increments for every normally retired `TAMAC` or TACC
lifecycle instruction, including an unsuccessful nonblocking `TACC.TRY`. A
faulting TACC instruction does not retire and does not increment it. Direct
`CSRR TACC_STATUS` and `CSRW TACC_CTL` operations remain CSR accounting and do
not increment `PERF_TILE_OPS`.

This counter is per issuing core, including microcores. The current
microcore implementation exposes only its cycle counter; Phase 1 and Phase 2
add `PERF_TILE_OPS` read/reset/increment behavior rather than narrowing the
TACC accounting contract.

Whole-SoC reset, paired full-engine reset, and cluster disable:

- zero the 2,048-bit bank;
- clear ownership, validity, dirty, busy, and force-pending state; and
- restore `OWNER=31`.

Ordinary traps and interrupts preserve ownership and TACC state. The OS must
store and release a dirty TACC before moving an owning context to another
core. Resuming on the same core may retain ownership deliberately.

### 8.1 Cycle accounting and reset cancellation

Cycle parity is part of the contract. The following are full-core
engine-local base totals. They include instruction decode, one service cycle
per source or transfer beat, and arithmetic beats, and assume each memory
request is granted and acknowledged on its first eligible service cycle:

| Operation | Full-core base cycles |
|---|---:|
| `CSRR TACC_STATUS` or `CSRW TACC_CTL` | 1 |
| `TACC.TRY`, `TACC.CLEAR`, or `TACC.RELEASE` | 2 |
| `TACC.LOAD` or `TACC.STORE` | 6 |
| Integer `TAMAC` tile×tile or in-place, U8/U16/U32 | 7 / 5 / 4 |
| Integer `TAMAC` broadcast, U8/U16/U32 | 6 / 4 / 3 |
| FP16/BF16 `TAMAC` tile×tile or in-place | 7 |
| FP16/BF16 `TAMAC` broadcast | 6 |

The lifecycle totals include the `F8` prefix. Each tile×tile or in-place
`TAMAC` consumes two serialized source beats; broadcast consumes one. Integer
arithmetic then consumes 4/2/1 beats and floating arithmetic consumes four.
`LOAD` and `STORE` consume four transfer beats.

The existing registered request path does not require a new combinational
fast path. Request capture, time to first grant, registered ACK return,
image-stage acquisition, and later contention add one stall cycle per elapsed
cycle beyond each base service beat, even when the physical port is otherwise
idle. An exact-cycle bench locks those additions against the Phase-1 timed
system model.

A microcore MEX instruction adds the existing fixed three-cycle
cluster-dispatch cost after winning the cluster grant. `TACC_STATUS` and
`TACC_CTL` are CSR operations, not MEX requests, and retain the ordinary
one-cycle CSR latency after any explicit CSR backpressure. Failed `TACC.TRY`
uses the same base latency as successful `TRY`. A validation fault consumes
decode plus any cluster-admission delay but no source or transfer beat:
`TACC_STATUS`, `TACC_CTL`, and non-transfer lifecycle validation faults take
one full-core base cycle, while `TAMAC`, `TACC.LOAD`, and `TACC.STORE`
validation faults take two. A transport fault additionally consumes every
issued beat and wait cycle through the faulting acknowledgement.
`PERF_CYCLES` and `PERF_STALLS` use these same rules in Python, native
execution, and RTL. Microcores gain the currently missing per-caller
`PERF_STALLS` path along with `PERF_TILE_OPS`.

Phase-1 measurement locked the registered-system and external-PHY additions:

| Image path | Functional/native step | Strict timed system |
|---|---:|---:|
| Internal, uncontended | 6 cycles | 9 cycles |
| External, one-cycle response per 64-bit word | 34 cycles / 28 stalls | 37 cycles / 31 stalls |
| External, two-cycle response per 64-bit word | 66 cycles / 60 stalls | 69 cycles / 63 stalls |

An external image is four 64-byte beats and 32 serialized 64-bit words, so
both execution modes increment `PERF_EXTMEM` by 32 on success. A response at
word-relative cycle 255 wins; no response or a later response faults at cycle
255 and reports that word's exact address. The measured model also confirms
atomic external LOAD, acknowledged-prefix STORE behavior, reset cancellation
without late callbacks or effects, and successful reuse after a timeout or
explicit error. Because the PHY response has no transaction tag, an accepted
word timeout or reset closes the current response epoch with
`PHY_CANCEL`; the controller may not reuse the interface until
`PHY_CANCEL_DONE` guarantees `PHY_ACK` is low and the canceled epoch can
produce no later response.

Reset scope is also explicit:

- whole-SoC reset wipes all seven engines;
- resetting one full-core execution domain wipes only its paired private
  engine;
- cluster disable or cluster-engine reset wipes that cluster's shared engine;
- resetting or terminating one microcore cancels that caller's pending or
  active request and discards its hidden partial result, but does not wipe
  shared cluster TACC state; and
- no reset of one domain changes any of the other six TACC banks.

RTL therefore carries an explicit reset/cancel sideband with every full-core
engine and one caller-cancel bit plus caller epoch per microcore through the
cluster arbiter. The current production top may tie individually unsupported
reset inputs inactive, but global reset, cluster disable, focused benches, and
any future reset controller use the same sidebands. A cluster-wide epoch is
not a substitute for per-caller cancellation.

Every in-flight engine operation carries an epoch or equivalent cancellation
token. An engine reset increments the engine epoch; an individual microcore
reset increments that caller's request epoch. Either cancels its unissued
beats, releases the shared transfer stage if that request owned it, and
prevents a late result or acknowledgement from committing. Engine reset also
wipes TACC immediately; individual microcore reset does not. Already
acknowledged external store beats remain visible, because reset does not
provide memory rollback. A deferred `FORCE_RELEASE`, unlike reset, lets the
active instruction reach its terminal retirement-or-trap boundary and then
wipes the bank before another TACC instruction is admitted.

## 9. RTL implementation constraints

These are design constraints, not suggestions:

- There is one TACC bank per physical engine, seven banks chip-wide.
- The persistent storage floor is therefore 14,336 bits. Banks are organized
  locally as FF or distributed-RAM slices capable of serving the locked 16
  feedback lanes; synthesis inference and banking are reported explicitly.
- Because all image traffic uses one physical tile-memory port, the chip uses
  one shared 2,048-bit load/store staging image, not seven per-engine shadows.
  Only one TACC image transfer may own that staging state at a time; ordinary
  tile requests may still be interleaved between its beats.
- Persistent banks plus the shared staging image cap dedicated TACC storage
  at 16,384 bits before small metadata. This plan does not allocate BRAM to
  TACC.
- Restoring the three missing private full-core tile instances necessarily
  restores their existing multiplier datapaths. Within each engine, TACC
  reuses that engine's multiplier lanes and must not instantiate a second
  integer or half-precision multiplier array.
- TACC uses a maximum of 16 feedback-add lanes per arithmetic beat.
- Integer work is scheduled as 4 beats for 8-bit mode, 2 for 16-bit mode,
  and 1 for 32-bit mode after operands are available.
- Floating work is scheduled as two 16-lane groups. Fixed pipeline staging is
  allowed, but the arithmetic portion must remain bounded to four cycles
  after operands are available.
- The FP32 feedback bank must be shared with or refactored from existing
  reduction arithmetic. A new dedicated 32-lane FP32 adder bank is rejected.
- Tile × tile `TAMAC` reads two source tiles; broadcast reads one source tile;
  neither writes `TDST`.
- `TACC.LOAD` and `TACC.STORE` use four serialized memory beats and retire
  only after the final required acknowledgement.
- Ownership does not hold an arithmetic lane or memory port between
  instructions.

The functional emulator records the fixed arithmetic beat count. End-to-end
RTL latency also includes source reads, bus acknowledgements, cluster
shared-engine admission where applicable, and seven-source tile-memory
arbitration.

## 10. Resource and timing gates

The repository's current FPGA figures are manual estimates, not synthesis
reports. Phase 2 must establish two like-for-like measurements with the same
tool version, target, constraints, and strategy:

1. the Phase-0 main baseline at
   `c8e8118e82a899ec3f101f63d277a1bf4ef5f84a`, which physically contains one
   full-core and three cluster tile engines; and
2. the topology-only Phase-2 landing with four private full-core and three
   cluster tile engines but no TACC.

The first comparison makes the cost of restoring the architectural topology
visible. The second is the baseline for measuring TACC itself and for every
growth percentage below. The two deltas must never be blended into one
unexplained utilization number.

The feature must meet all of these gates:

- no TACC-specific multiplier-array duplication;
- post-synthesis DSP growth no greater than 5% relative to the measured
  baseline;
- whole-design FF growth no greater than 7%;
- whole-design LUT growth no greater than 12%;
- no new BRAM requirement for TACC state;
- routed design retains at least 5% LUT, FF, and DSP headroom;
- post-route WNS and TNS are nonnegative at the 100 MHz target;
- target clock closes with no more than 10% Fmax regression relative to the
  seven-engine no-TACC baseline; and
- no unbounded combinational path is added through product, FP addition, and
  ownership muxing.

The topology-only checkpoint and final TACC design must fit and close timing
before Phase 2 closes. Approval-gated physical measurement does not block the
functional RTL landings. If the topology lacks the required final headroom,
optimize it rather than silently deleting private engines. If TACC misses a
gate, refactor or add bounded pipeline stages; do not add more arithmetic
copies.

Vivado synthesis and implementation may be heavyweight. Per repository
resource rules, obtain user approval before running a job that may spawn
workers, exceed 4 GiB, or exceed checked-in step limits. Never run a large
test suite and synthesis concurrently.

## 11. Phase 0 landing — contract

### Scope

- Add this tracked handoff under `docs/`.
- Do not modify the workspace-root handoff, main checkout, emulator, generated
  artifacts, or RTL.
- Record the feature branch and isolated worktree.

### Verification

Run:

```sh
git diff --check
git status --short
```

Then review every encoding, CSR address, state transition, legal mode, fault,
and phase landing in this document.

### Commit

Commit this document alone:

```text
Lock the full TACC ISA and implementation contract

Define the seven-engine topology, 2,048-bit canonical state, explicit
ownership lifecycle, numeric behavior, memory image, and failure rules.

Break emulator and RTL delivery into independently testable landings with
resource, timing, verification, and commit gates.
```

## 12. Phase 1 — emulator implementation

Phase 1 changes only the isolated feature worktree. Tests are run sequentially
with xdist disabled. Use an isolated runtime namespace such as
`MP64_RUNTIME_NAMESPACE=megapad-full-tacc`.

### Landing 1.1 — executable ISA and arithmetic oracle

Primary files:

- `asm.py`
- `cli.py`
- `megapad64.py`
- new `tests/test_tacc_isa.py`

Work:

- add the instruction spellings, encodings, CSR constants, and disassembly;
- add the 2,048-bit state model and metadata;
- implement the lifecycle state machine on a single Python engine;
- implement canonical image pack/unpack;
- implement bit-exact integer and floating lane helpers;
- implement full-core single-caller `TAMAC` and lifecycle execution;
- enforce all source-selector, mode, validity, ownership, alignment, and
  pre-mutation fault rules;
- set `TRAP_ADDR`, preserve the complete-instruction return PC, and apply the
  caller's ordinary routed-memory permissions on address faults; and
- reset and zeroize all state according to the contract.

Focused gate:

```sh
python -m pytest -q tests/test_tacc_isa.py
git diff --check
```

Commit:

```text
Define the executable full TACC ISA oracle

Add canonical encodings, lifecycle state, memory images, and per-lane
arithmetic to the assembler and Python emulator.

Cover format errors, ownership violations, and trap-before-mutation behavior
with focused contract tests.
```

### Landing 1.2 — seven-engine topology and cluster-private tile context

Primary files:

- `system.py`
- `megapad64.py`
- `accel/mp64_accel.cpp`
- `accel_wrapper.py` where system routing requires it
- new `tests/test_full_core_tile_engine.py`
- `tests/test_native_microcore.py`
- focused updates to `tests/test_system.py`
- focused updates to `tests/test_native_cycle_execution.py`

Work:

- preserve one private tile-engine domain for each of full cores 0–3;
- preserve three independent cluster-shared engine domains;
- route each full core directly to its engine and microcores through their
  cluster-local engine;
- make every listed configuration/cursor/stride CSR microcore-private;
- remove those configuration fields from authoritative native
  `ClusterState` shared snapshots and preserve private values across
  compatibility fallback;
- keep `ACC` and TACC private on full-core engines and cluster-shared on
  microcluster engines;
- make cluster ACC CSRs, SHA, and MEX use the same authoritative legacy ACC
  with acknowledged deterministic cross-producer admission, and make SHA
  sample the granted microcore's private `TSRC0`;
- keep `TCTRL` caller-private, apply `ACC_ZERO` only with the granted MEX
  request, and auto-clear only that caller's shadow;
- establish exactly four authoritative full-core engine states and three
  authoritative `ClusterState` engine states; inherited tile fields in the
  twelve microcore CPU objects are transient execution staging only;
- implement deterministic equal round-robin cluster admission;
- preserve the production core/requestor map while proving compact IDs remain
  coherent in configurable reduced-system tests;
- invert tests that currently assume cluster-wide tile configuration; and
- prove isolation between all seven physical domains.

Focused gates, run one command at a time:

```sh
python setup_accel.py build_ext --inplace
python -m pytest -q tests/test_full_core_tile_engine.py
python -m pytest -q tests/test_native_microcore.py -k "tile or mex or sha or acc or csr"
python -m pytest -q tests/test_system.py -k "tile or mex or sha or acc or csr"
python -m pytest -q tests/test_native_cycle_execution.py -k mex
git diff --check
```

Commit:

```text
Model the seven physical tile engines

Preserve one private tile engine for every full core and one round-robin
shared engine for every microcluster.

Keep microcore tile configuration in private shadows while ACC and TACC
follow their engine's private or cluster-shared ownership domain.
```

### Landing 1.3 — ownership, recovery, and context transport

Primary files:

- `system.py`
- `megapad64.py`
- `accel_wrapper.py`
- `accel/mp64_accel.cpp`
- `tests/test_full_core_tile_engine.py`
- `tests/test_native_system_state.py`
- `tests/test_native_mex_oracle.py`
- `tests/test_native_microcore.py`
- `tests/test_accel_wrapper_fallback.py`
- new `tests/test_tacc_cycle_api.py`

Work:

- add owner, valid, dirty, format, busy, and force-pending metadata to each
  physical engine;
- make `TACC_STATUS.MINE` caller-relative;
- store absolute owner IDs and reject a snapshot whose owner is not a caller
  of that engine; specifically distinguish local microcore 0 in clusters 0,
  1, and 2 as absolute owners 4, 8, and 12;
- implement nonblocking claim, explicit release, reset/disable wipe, and
  supervisor force-release;
- include the complete TACC domain and operation epoch in
  `CPUExecutionCheckpoint`, authoritative `ClusterState` rollback,
  pybind snapshot/update methods, `_sync_cs_to_py`, `_sync_py_to_cs`, and
  direct microcore compatibility stepping;
- make every snapshot a deep copy and atomically validate image length,
  owner domain, valid/format coherence, and busy/pending coherence before
  commit;
- model in-flight operations in the cycle API so sibling status reads observe
  `BUSY`, direct or reentrant supervisor control writes can set
  `FORCE_PENDING`, and reset or cluster disable cancels late commits by epoch;
  prove a deferred force release runs after either normal retirement or trap
  completion;
- include the chip-wide image-transfer staging owner, partial image, beat
  index, and epoch in system-level snapshot, rollback, and reset handling;
  establish its validated compatibility API here, while timed tenure,
  interleaved beats, and contention begin in Landing 1.5;
- add and transport per-microcore `PERF_STALLS` and `PERF_TILE_OPS` state;
- move TACC state watchers into the native MEX oracle before adding native
  arithmetic; and
- initially route new instructions through the Python oracle when native
  execution is not yet implemented.

Focused gates:

```sh
python setup_accel.py build_ext --inplace
python -m pytest -q tests/test_native_system_state.py -k tacc
python -m pytest -q tests/test_native_mex_oracle.py -k tacc
python -m pytest -q tests/test_native_microcore.py -k tacc
python -m pytest -q tests/test_accel_wrapper_fallback.py -k tacc
python -m pytest -q tests/test_full_core_tile_engine.py -k tacc
python -m pytest -q tests/test_tacc_cycle_api.py
git diff --check
```

Commit:

```text
Carry TACC ownership across every emulator boundary

Make each physical tile-engine domain authoritative for the complete TACC
image and lifecycle metadata.

Preserve that state through native snapshots, Python fallback, rollback,
reset, cluster disable, and privileged recovery.
```

### Landing 1.4 — native TACC execution

Primary files:

- `accel/mp64_accel.cpp`
- `accel_wrapper.py` if binding changes are required
- `tests/test_native_mex_oracle.py`
- `tests/test_native_batch_boundaries.py`
- `tests/test_accel_wrapper_fallback.py`

Work:

- implement lifecycle operations and `TACC_STATUS` natively;
- implement integer `TAMAC` using unsigned/wider intermediates without C++
  signed-overflow undefined behavior;
- deliberately retain every FP `TAMAC` on the exact integer oracle until the
  complete binary32 add-product path, including wide exponent alignment and
  one-point RNE, is ported bit-for-bit;
- keep callback-overridden and system-routed TACC memory operations on the
  transactional oracle in this landing; Landing 1.5 replaces that synchronous
  seam with the timed seven-engine transport, while ordinary resolved
  RAM/HBW/external/VRAM spans execute natively here;
- preflight the complete operation before TACC mutation;
- implement the locked per-form cycle table, extended instruction sizing,
  rewind, stall, and performance-counter behavior exactly;
- carry callback and routed-memory faults back with exact fault address,
  atomic-load behavior, preservation of both clean and dirty pre-store state
  on failure, and end-of-instruction trap PC;
- prove lifecycle and `TAMAC` never alter legacy `ACC`, caller configuration,
  or destination memory except for an explicit `TACC.STORE`;
- prove nonowner legacy MEX/ACC operations neither block on ownership nor
  alter TACC metadata; and
- expand native MEX watchers to include all TACC data and metadata.

Differential cases include every legal mode, signed extremes, wrap boundaries,
repeated accumulation, broadcast and in-place sources, NaN, infinity,
subnormal, signed zero, format mismatch, image faults, reset, and exceptional
fallback.

Focused gates:

```sh
python setup_accel.py build_ext --inplace
python -m pytest -q tests/test_native_mex_oracle.py -k tacc
python -m pytest -q tests/test_native_batch_boundaries.py -k tacc
python -m pytest -q tests/test_accel_wrapper_fallback.py -k tacc
git diff --check
```

Commit:

```text
Accelerate full TACC execution natively

Implement lifecycle and widened lane accumulation in the C++ execution core
without weakening transactional fallback.

Prove bit-for-bit state, memory, trap, cycle, and counter parity against the
Python oracle.
```

### Landing 1.5 — ownership and QoS contention closure

Primary files:

- `accel/mp64_accel.cpp`
- `system.py`
- `tests/test_full_core_tile_engine.py`
- `tests/test_phase3_reduced_core_execution.py`
- `tests/test_native_system_state.py`
- new `tests/test_tacc_contention.py`
- new `tests/test_tile_engine_memory_arbitration.py`

Work:

- prove simultaneous claims on the four private full-core engines succeed
  independently;
- send competing same-cluster claims through ordinary round-robin admission;
- make an accepted force write win over same-cycle TACC admission in the
  timed scheduler;
- retire losing cluster claims without internal wait or scheduler eligibility
  change;
- model all seven physical engines as distinct requestors of the timed
  tile-memory port and apply deterministic equal round-robin service;
- allow nonowners to continue stateless MEX work;
- reject protected nonowner operations before mutation;
- preserve ownership across interrupts and preemption;
- apply force-release only at the locked retirement-or-trap boundary;
- grant the shared image stage with equal round-robin service, retain it
  across the winner's four interleavable beats, make a second image transfer
  wait, and continue serving ordinary tile traffic between those beats;
- model explicit external-memory errors and the locked 255-cycle no-response
  timeout in Python and native timed-system paths, including exact fault
  address, cancellation recovery, and cycle/stall counts; and
- prove the locked reset scopes, transfer cancellation epoch, absolute owner
  IDs, memory-port order, cycle accounting, and final results under supported
  scheduler worker counts.

Focused gates, run sequentially:

```sh
python setup_accel.py build_ext --inplace
python -m pytest -q tests/test_full_core_tile_engine.py -k "ownership or reset"
python -m pytest -q tests/test_tile_engine_memory_arbitration.py
python -m pytest -q tests/test_timed_tile_scheduler.py
python -m pytest -q tests/test_tacc_external_phy.py
python -m pytest -q tests/test_native_tacc_external_phy.py
python -m pytest -q tests/test_timed_tacc_external_phy.py
git diff --check
```

The 1/2/4-worker comparison in `tests/test_tacc_contention.py` and the reduced
core execution suite spawn workers. Obtain explicit user approval before
running either, regardless of prior resource observations, and run them
sequentially:

```sh
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_tacc_contention.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_phase3_reduced_core_execution.py
```

Phase-1 worker-gate record, 2026-07-28: explicit approval was obtained and
both commands passed sequentially. The contention oracle passed its one
production-topology scenario, and the complete reduced-core suite passed 34
selections across one, two, and four scheduler workers. The complete reduced
suite is intentional: it has no meaningful TACC-only selector, and the
cluster retirement invariant is shared by TACC, CRC, SHA, and ordinary MEX
requests.

The first complete reduced-core run exposed two closure defects. A stale test
looked for the removed `tile_engine_locked` snapshot key instead of the
authoritative SHA transaction lock, and coordinator settlement accepted any
terminal zero-step cluster callback as if it were a TACC cancellation.
Commit `3f0176d` corrected the schema assertion, introduced an explicit
cancellation tag, restricted zero-retirement to genuine TACC requests, and
prevented cancelled requests from publishing an arbitration grant. The
34-selection rerun covers both tagged and untagged false-cancellation attempts;
the dedicated microcore reset test also passed on step, core-batch, and
system-batch surfaces while proving that grant counters and round-robin state
remain unchanged.

Commit:

```text
Close TACC ownership under deterministic arbitration

Retire failed claims without blocking a physical tile engine and preserve
stateless MEX service for nonowners.

Add independent full-core and microcluster contention oracles covering
scheduler widths, preemption, reset, and dead-owner recovery.
```

### Landing 1.6 — guest software, public documentation, and capstone

Primary files:

- `bios.asm`
- `docs/BIOS-DICTIONARY.md`
- `docs/bios-forth.md`
- `docs/isa-reference.md`
- `docs/tile-engine.md`
- `docs/architecture.md`
- `docs/extended-tpu-spec.md`
- `docs/tools.md`
- `docs/chip-math-update-handoff.md`
- `EMULATOR.md`
- `diskutil.py`
- focused BIOS/system tests

Work:

- add thin Forth words for every instruction and `TACC_STATUS`;
- add a convenience claim word that returns `MINE` without hiding a spin;
- document a software-controlled `PAUSE`/backoff example;
- document the seven ownership domains, cluster-private shadows, image, formats,
  faults, context-switch rules, and recovery;
- add at least one multi-step integer kernel and one FP kernel that avoid
  intermediate tile stores;
- update this handoff's Phase-1 checklist and measured cycle notes; and
- run the complete focused Phase-1 matrix sequentially.

Do not regenerate `fpga/bios.hex` merely because BIOS source changed.

Focused capstone gates:

```sh
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_tacc_isa.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_megapad64.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_system.py K=tacc
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_system.py K=TestDiskUtil
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_native_mex_oracle.py K=tacc
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_tacc_cycle_api.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_full_core_tile_engine.py K='ownership or reset'
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_tile_engine_memory_arbitration.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_timed_tile_scheduler.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_tacc_external_phy.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_native_tacc_external_phy.py
MP64_RUNTIME_NAMESPACE=megapad-full-tacc make test-sequential TEST_PATH=tests/test_timed_tacc_external_phy.py
git diff --check
```

After explicit worker-spawning approval, close the two resource gates
sequentially as described in Landing 1.5. They remain part of Phase-1
acceptance even though they are intentionally absent from the ordinary
capstone block.

Phase-1 ordinary capstone record, 2026-07-28: all commands above passed
sequentially after the final cancellation-invariant correction (281 selected
tests in total, including the focused BIOS and sample-image documentation
checks). The zero-match `tests/test_megapad64.py -k tacc` draft selector was
replaced by that file's complete 28-test suite. The approval-gated worker
suites add 35 green selections and are intentionally not included in the
ordinary pass count, for 316 Phase-1 acceptance selections overall.

Commit:

```text
Expose the explicit TACC lifecycle to guest software

Add Forth words and user documentation for claim, initialization,
accumulation, persistence, release, and software-controlled waiting.

Close Phase 1 with integer and floating kernels that demonstrate reduced data
movement on every physical engine topology.
```

## 13. Phase 2 — RTL implementation

Phase 2 begins only after the Phase-1 oracle and differential vectors are
stable. RTL tests and synthesis are always run sequentially.

The repository has no established formal harness. Directed benches,
emulator-generated differential vectors, and inline simulation assertions are
required; introducing formal verification is not a dependency of this
feature.

Any RTL module added by a landing must enter the focused simulation build and
every explicit FPGA synthesis source list in that same landing.

### Landing 2.1 — encodings, status, and MEX fault plumbing

Primary files:

- `rtl/pkg/mp64_defs.vh`
- `rtl/pkg/mp64_pkg.vh`
- `rtl/pkg/mp64_cpu_common.vh`
- `rtl/pkg/mp64_cpu_funcs.vh`
- `rtl/core/mp64_cpu.v`
- `rtl/core/mp64_cpu_micro.v`
- `rtl/core/mp64_cluster.v`
- `rtl/gpu/mp64_tile.v`
- `rtl/soc/mp64_soc.v`
- every affected CPU, cluster, and SoC testbench instantiation
- `rtl/sim/Makefile`

Work:

- add the locked constants and CSR addresses to both RTL packages;
- extend the MEX request with absolute caller core ID, privilege, ordinary
  memory-access bounds/context, engine epoch, and per-caller epoch;
- route paired full-core engine reset and per-microcore cancel sidebands
  through every CPU, cluster, and SoC instantiation; keep individually
  unsupported production reset sources tied inactive rather than omitting
  the transport;
- extend completion with a fault code capable of illegal, alignment, bus, and
  privilege faults plus a 64-bit fault address;
- route faults into the existing CPU trap machinery without retirement,
  setting `TRAP_ADDR` and saving the end-of-instruction return PC;
- add decode and transport plumbing for caller-relative `TACC_STATUS` and
  supervisor-only `TACC_CTL`; Landing 2.3 supplies the real state and
  per-caller shaping, and the cluster control write has an acknowledged path
  capable of latching force-pending while MEX is active;
- add the missing per-microcore `PERF_STALLS` and `PERF_TILE_OPS` counter
  paths;
- carry service, arbitration, and acknowledgement stalls into the locked
  cycle and performance-counter accounting;
- make the `cpu_micro` simulation target fail closed on a failed bench; and
- correct `TCTRL.ACC_ZERO` one-shot clearing so RTL matches the existing
  documented/emulator behavior.

Focused gates:

```sh
make -C rtl/sim cpu_smoke
make -C rtl/sim opcodes
make -C rtl/sim cpu_micro
make -C rtl/sim cluster
make -C rtl/sim soc_elaborate
```

Run those targets sequentially. Add focused encoding/fault assertions before
committing.

Commit:

```text
Add RTL plumbing for the locked TACC ISA

Carry caller identity, privilege, status, and precise MEX faults between the
CPU, arbiters, and tile engine.

Define the TACC encodings in both RTL packages and repair the existing
ACC_ZERO one-shot divergence.
```

#### Landing 2.1 execution record — 2026-07-28

Landing 2.1 was implemented on the isolated
`feature/megapad-full-tacc-rtl` worktree. The full and microcore CPU paths now
preserve the complete function byte, expose caller and protection context,
pretrap malformed TACC encodings, map precise MEX completion faults, and use
dedicated acknowledged `TACC_STATUS`/`TACC_CTL` paths. The cluster captures
request context and epochs at grant, de-duplicates level-held MEX and control
requests, masks same-edge caller resets, and shapes `MINE` independently for
each absolute caller. The tile leaf implements reset/cancellation generation
checks, fail-closed placeholders for the assigned TACC namespaces, and the
correct `ACC_ZERO` one-shot boundary. Cores 1–3 deliberately return
`ILLEGAL_OP` for MEX/TACC until Landing 2.2 installs their private engines;
they cannot falsely retire accelerator work in this bridge state.

The focused benches are fail-closed for this landing and cover complete
encoding lengths, all four MEX fault mappings, saved end-of-instruction PCs,
`TRAP_ADDR`, fault non-retirement, raw status, held control requests,
privilege, arbiter replay, same-edge and active cancellation, late
acknowledgements, caller epochs, status shaping, and `ACC_ZERO` reuse.

Sequential verification record:

- `cpu_smoke`: 101 passed;
- `cpu_micro`: 64 passed;
- `cluster`: 55 passed;
- `tile`: 78 tile assertions and 34 write-ack assertions passed;
- `soc_elaborate`: passed;
- full-source Yosys frontend and `mp64_soc` hierarchy check: passed;
- `string`: 39 passed;
- `dict`: 16 passed;
- `multicore_smoke`: 37 passed;
- reduced-parameter `soc_smoke`: 5 passed; and
- `soc_tile_icache`: 7 passed.

`opcodes` remains at its exact pre-landing baseline of 104 passes and one
MARK/SAV mismatch. That unrelated bench still exits successfully despite its
reported failure, so the new TACC assertions live in fail-closed benches
rather than treating `opcodes` as acceptance evidence.

Nonblocking findings intentionally deferred from the critical path:

- the long-standing MARK/SAV mismatch and the existing sized-hex compiler
  warnings are unrelated to TACC and remain visible in the verification log;
- the fixed leaf cancellation bundle assumes the production `N <= 4`
  microcluster size; a future parameter-general cluster must widen that
  bundle or reject larger `N` explicitly;
- a bare `tile_engine_reset` is a paired-reset transport, not a standalone
  microcore cancellation API. It is tied inactive in the Landing 2.1 SoC and
  must be wired only with the corresponding core/cluster reset in later
  topology work; individual microcore cancellation uses `micro_reset` and
  caller epochs;
- the pre-existing microcluster trap architecture does not expose a general
  local-trap notification for every illegal/alignment/bus exception, so
  cluster privilege transition outside the new TACC privilege paths remains
  a broader RTL hardening item; and
- heavyweight Genesys 2 implementation evidence remains approval-gated. The
  checked-in four-bank memory estimate alone requests 1,024 RAMB36 blocks
  against 445 on the target, so Landing 2.2 must establish a fit-capable
  like-for-like configuration or a larger target before route results can
  meaningfully measure the seven-engine delta.

### Landing 2.2 — restore the seven-engine topology

Primary files:

- `rtl/soc/mp64_soc.v`
- `rtl/core/mp64_cluster.v`
- `rtl/gpu/mp64_tile.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- `rtl/sim/tb_cluster.v`
- `rtl/sim/tb_tile_port_arbiter.v`
- `rtl/sim/tb_soc_tile_icache.v`
- a new focused full-core private-tile bench
- `rtl/sim/Makefile`

Work:

- replace the core-1–3 MEX tie-offs with one private `mp64_tile` instance per
  full core;
- connect each full core directly to its private CSR, MEX, ACC, and completion
  path with no full-core MEX arbiter;
- retain one shared tile instance in each microcluster and add the locked
  per-microcore configuration shadows there;
- reconcile full-core `acc_reg`/tile `acc` and cluster
  `cl_sha_acc`/tile `acc` so every engine has exactly one architectural legacy
  ACC domain;
- serialize cluster `CSR_ACC0`–`CSR_ACC3`, SHA, and MEX accesses that read or
  write legacy ACC through an acknowledged deterministic common admission
  point, and make SHA sample the granted microcore's private `TSRC0`;
- keep `TCTRL` private, apply `ACC_ZERO` atomically with the admitted MEX
  operation, and return its auto-clear to only the granted caller;
- widen the shared tile-memory port arbiter from four to seven physical
  sources with the production 0–3 full-core/4–6 cluster map, three-bit
  requestor metadata, captured requests, ACK routing, and equal round-robin
  service;
- wire `sysinfo_cluster_en` to the actual cluster enable inputs instead of
  tying every cluster high;
- invalidate only the paired full core's private I-cache after its own tile
  engine commits a write, preserving the existing explicitly noncoherent
  cross-core policy;
- correct scalar broadcast to splat the low active element instead of
  repeating a 64-bit pattern;
- prove non-power-of-two wrap from source 6 to 0, all-seven pending requests,
  delayed ACKs, correct write-requestor metadata, and coherent compact
  requestor/core IDs in reduced-parameter elaboration;
- prove all four full cores can execute MEX simultaneously until they contend
  for a shared memory port;
- add focused CSR/SHA/MEX legacy-ACC coexistence, simultaneous writer,
  caller-private SHA source, and per-caller `ACC_ZERO` tests;
- add every new RTL module to each explicit FPGA synthesis source list.

Critical-path refinement (2026-07-28): the checked implementation runner,
Vivado report mode, topology-only physical measurement, and fail-closed
report comparison are consolidated in Landing 2.9. They are approval-gated
physical acceptance infrastructure rather than prerequisites for building
the functional RTL landings. Commit `364d44283ba5c2fad8187b63da6917af60344c26`
is the immutable topology-only comparison point. This landing adds no new RTL
module, so the existing explicit source lists require no new entry.

Focused gates, run sequentially:

```sh
make -C rtl/sim tile_port_arbiter
make -C rtl/sim full_core_tile
make -C rtl/sim cluster
make -C rtl/sim soc_tile_icache
make -C rtl/sim soc_elaborate
```

Commit:

```text
Restore private tile engines for every full core

Replace the core-1–3 MEX tie-offs with private tile instances while retaining
one shared engine in every microcluster.

Route seven physical engines fairly to tile memory, isolate full-core state,
give cluster callers private configuration shadows, and establish the
immutable topology checkpoint used by final physical comparison.
```

#### Landing 2.2 topology sublanding — 2026-07-28

The buildable topology portion is implemented separately before the shared
state refactor. All four production full cores now instantiate a private tile
engine, while each microcluster retains its private shared engine. The memory
arbiter accepts seven production sources with three-bit owners and equal
round-robin service in the fixed 0–3 full-core/4–6 cluster map. Parameter-
reduced verification builds compact only their instantiated sources and core
IDs. `CLUSTER_EN` now drives the real cluster enable inputs, and each full-core
tile write invalidates only its paired private I-cache.

Sequential evidence for this sublanding:

- `tile_port_arbiter`: 50 assertions passed, including all seven sources,
  delayed acknowledgements, retained pulses, and explicit 6-to-0 wrap;
- `full_core_tile`: 10 assertions passed, including four isolated CSR domains,
  simultaneous execution, one completion per engine, production owner order,
  and live cluster-enable wiring;
- `soc_tile_icache`: 11 assertions passed for paired core-0/core-1
  invalidation and explicitly noncoherent cluster writes;
- reduced-parameter `soc_smoke`: 7 assertions passed, including compact
  requestor and absolute core-ID mapping; and
- `soc_elaborate`: passed with the seven production engine instances.

This is intentionally not the Landing 2.2 completion point. The existing
full-core CPU/tile and cluster-SHA/tile legacy ACC copies still diverge, and
cluster tile configuration is still globally shared. Those are architectural
correctness blockers and remain on the active path for the next sublanding.
One nonblocking cancellation detail is deferred with the already planned
transfer/lifecycle work: disabling a cluster resets its engine but does not
yet purge a request that the shared memory arbiter captured before disable.
No heavyweight implementation run has been performed without approval.

#### Landing 2.2 shared-state and admission sublanding — 2026-07-28

Landing 2.2 now closes the architectural blockers left by the topology
sublanding. Each of the seven tile modules is the sole owner of its physical
engine's legacy ACC. A full CPU reaches that state through its paired private
tile sideband, while cluster CSR, SHA, and MEX users reach the shared tile ACC
through one acknowledged common domain. The cluster keeps `TMODE`, `TCTRL`,
source, destination, shape, stride, and scalar configuration in per-caller
shadows and captures the granted caller's values at admission.

Cluster common admission is caller-first equal round-robin across ACC CSRs,
SHA, and MEX. SHA `INIT` acquires the legacy engine lock, `FINAL` retains it,
and `RELEASE` is the only ordinary unlock; the lock protects every
ACC-dependent reduction, DOT, and DOTACC form, including raw SS=2 requests,
while stateless MEX work remains available. Scalar MEX broadcasts now splat
the low active EW8, EW16, EW32, FP16, or BF16 element rather than repeating a
64-bit pattern. `ACC_ZERO` is captured and consumed only for the admitted
caller's operation.

Cancellation is aligned with architectural mutation. ACC CSR and SHA
metadata writes commit only at their cancellation-aware terminal boundary.
A canceled SHA compression resets the iterative child before fresh work can
be admitted. A canceled, already-issued SHA read retains the external cluster
port until its stale response is drained and discarded, so it cannot
acknowledge a newly admitted microcore request. MEX admission snapshots legacy
ACC and restores it if the caller is canceled after a leaf mutation but before
retirement. External memory writes remain nonrollback side effects as
specified by the fault/reset contract. The cluster's ordinary bus arbiter and
SHA loader now feed a single output mux, eliminating their former multiple
procedural drivers and preventing a SHA reservation from stealing an in-flight
ordinary transfer.

Sequential verification record for the completed landing:

- `cluster`: 126 assertions passed, including mixed-kind equal round-robin,
  authoritative ACC visibility, SHA ownership, raw-SS lock protection,
  caller-private configuration and `ACC_ZERO`, terminal cancellation, MEX
  rollback, stale external-response draining, and fresh compression after
  cancellation;
- `full_core_tile`: 14 assertions passed for four simultaneous private
  engines and paired ACC visibility;
- `tile`: 84 assertions and 34 write-ack assertions passed;
- `cpu_micro`: 84 assertions passed;
- `cpu_smoke`: 101 assertions passed;
- `tile_port_arbiter`: 50 assertions passed;
- reduced-parameter `soc_smoke`: 7 assertions passed;
- `soc_tile_icache`: 11 assertions passed;
- `soc_elaborate`: passed;
- `string`: 39 assertions passed;
- `dict`: 16 assertions passed; and
- `multicore_smoke`: 37 assertions passed.

`opcodes` remains at its pre-existing 104-pass/one MARK/SAV mismatch baseline.
An Icarus `-Wall` cluster elaboration reported no implicit-net or
multiple-driver warning. Yosys resolved both the full SoC and focused cluster
hierarchies, including all seven tile instances. The checked-in all-module
script cannot complete with the installed Yosys because that version rejects
its `synth_xilinx -json` option. A focused `proc; check` run was bounded and
stopped in `PROC_MUX` after hierarchy success because expanding the existing
highly unrolled tile arithmetic dominated the run for roughly two minutes;
the final `check` pass therefore did not run. These limitations are not
presented as synthesis or physical acceptance evidence.

Nonblocking findings intentionally remain documented rather than entering
the feature's critical path:

- cluster disable does not yet purge a tile-memory request already captured
  by the SoC arbiter; this becomes build-critical in Landing 2.4 when TACC
  image transfers begin using that path, so its cancel/drain sideband moves
  with that landing rather than widening lifecycle-only Landing 2.3;
- production microcore Field/GF inputs remain dangling because Field ALU SoC
  integration is an explicit non-goal;
- unused cursor and sideband signals, a default-topology-only parameter
  assertion, established sized-hex warnings, and the MARK/SAV bench mismatch
  are cleanup outside TACC correctness;
- the existing SHA-384, SHA-512, and padding gaps are unrelated to shared
  ACC ownership; and
- no approval-gated Vivado placement or routing was run. Resource, timing,
  unconstrained-path, and final seven-instance physical evidence remains
  Landing 2.9 work.

### Landing 2.3 — TACC state and lifecycle

Primary files:

- new `rtl/gpu/mp64_tacc.v` or an equivalently contained state module
- `rtl/gpu/mp64_tile.v`
- `rtl/core/mp64_cluster.v`
- `rtl/soc/mp64_soc.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- new `rtl/sim/tb_tacc.v`
- new `rtl/sim/tb_tacc_cycles.v`
- `rtl/sim/tb_cluster.v`
- `rtl/sim/tb_full_core_tile.v`
- `rtl/sim/tb_tile.v`
- `rtl/sim/Makefile`
- FPGA and Yosys explicit RTL source manifests

Work:

- instantiate one 2,048-bit bank and metadata block in each of the seven
  physical engines;
- implement claim, clear, release, force-pending, zeroization, and reset;
- make same-owner claim idempotent and competing cluster claims nonblocking;
- validate absolute owner IDs against each engine's fixed full-core caller or
  four-member microcluster caller set;
- shape `TACC_STATUS.MINE` separately for each cluster CSR reader while
  replicating the same physical-engine fields;
- enforce owner, valid, format, and supported-mode preconditions before
  mutation;
- preserve ordinary stateless MEX service while a TACC owner is idle;
- defer force-release during an active instruction until its terminal
  retirement-or-trap boundary;
- give an acknowledged force-release priority over same-cycle TACC admission
  while retaining the displaced MEX request for later validation;
- implement the locked system, paired-full-core, cluster, and individual
  microcore reset scopes with distinct engine and per-caller epochs and
  explicit reset/cancel sidebands;
- hold a lifecycle response and its staged mutation until the receiver's
  explicit retirement handshake, with cancellation taking priority at both
  the leaf-publication and registered microcore-delivery edges;
- carry cluster-disable cancellation of captured/issued tile-memory work into
  Landing 2.4, before lifecycle expands into image traffic; and
- add simulation assertions for legal metadata transitions, owner-only
  mutation, and fault-implies-completion.

The focused bench covers all seven independent domains, simultaneous
full-core claims, cluster contention, clear, release, reset, individual
microcore reset without cluster-state loss, mode latch, every lifecycle
protected-operation fault, privilege source, cancellation epochs, and
same-cycle force/admission priority. It also targets cancellation before
cluster capture and during final registered microcore delivery. Multi-cycle
force-after-fault and cluster-disable memory draining join the first tokened
image-transfer bench. The cycle bench makes request capture, cluster
admission, retirement, and lifecycle/CSR timing fail-closed rather than
relying on waveform inspection.

Focused gates, run sequentially:

```sh
make -C rtl/sim tacc
make -C rtl/sim tacc_cycles
make -C rtl/sim cluster
make -C rtl/sim tile_port_arbiter
make -C rtl/sim soc_elaborate
```

Commit:

```text
Implement explicit TACC lifecycle state

Add one 2,048-bit bank to each of the seven tile engines with explicit
ownership, validity, dirty tracking, format latching, and zeroization.

Enforce nonblocking cluster claims and safe privileged recovery without
reserving otherwise stateless tile service.
```

#### Landing 2.3 completion — 2026-07-28

Landing 2.3 is implemented in the isolated Phase-2 worktree. Each generated
tile engine now contains one authoritative TACC state leaf with a declared
2,048-bit bank, fixed caller domain, owner, validity, dirty and normalized
format metadata, BUSY, FORCE_PENDING, and raw physical status. The four
full-core instances accept only their fixed core IDs; each cluster instance
accepts only `CLUSTER_ID_BASE + caller_slot`. `MINE` remains absent from raw
status and is inserted at the existing full-core and per-microcore CSR
fanouts.

`TRY`, `CLEAR`, and `RELEASE` validate before admission and publish BUSY only
for accepted work. The leaf holds its response and staged mutation until an
explicit receiver-retire handshake. Full-core engines accept that response on
the CPU delivery edge; a cluster accepts it only when registered completion is
delivered to the granted microcore and the common arbitration turn advances.
Physical BUSY therefore remains visible through response delivery, and a
caller cancellation in either the leaf-publication or registered-delivery
window suppresses completion and mutation together without a second 2,048-bit
rollback bank. Free, self-owned, and foreign-owned `TRY` all retire normally
with the locked nonblocking behavior. Protected owner or format failures,
noncanonical encodings, `LOAD`, `STORE`, and `TAMAC` fail before BUSY and
cannot enter legacy memory or ACC paths. Engine reset wipes immediately.

The independent control transport now performs real privileged recovery.
Reserved-only writes remain no-ops, user FORCE faults, an idle supervisor
FORCE wipes immediately, and a FORCE accepted while a lifecycle response is
pending survives either retirement or caller cancellation and leaves the bank
wiped at that terminal edge. An authorized same-cycle FORCE deasserts
lifecycle readiness. Because full-core MEX is a one-cycle pulse, the tile
holds the complete displaced request and its original cancellation tokens,
accounts the wait as a stall, and revalidates it after recovery instead of
dropping it. Cancellation drops held valid on its terminal edge so an
immediately following request cannot deadlock behind the leaf's de-duplication
latch.

Sequential verification for this landing currently records:

- `tacc`: 166 lifecycle, status, ownership, format, privilege, force,
  cancellation, and reset checks passed;
- `tacc_cycles`: 44 direct leaf-cycle, terminal-cancellation, and
  force-displacement checks passed;
- `tile`: 85 datapath/lifecycle checks and 34 write-ack checks passed;
- `cluster`: 136 checks passed, including cancellation at both leaf-response
  publication and final registered microcore delivery;
- `full_core_tile`: 22 assertions passed; simultaneous claims proved all
  seven physical domains, fixed owners, caller-relative MINE shaping, losing
  cluster contention, and private full-core release alongside the existing
  private-engine checks;
- `tile_port_arbiter`: the existing 50 checks passed;
- reduced-parameter `soc_smoke`: 7 checks passed;
- `soc_tile_icache`: the existing 11 checks passed; and
- `soc_elaborate`: passed with only the repository's established sized-hex
  warnings; focused `mp64_tacc` Yosys `hierarchy`, `proc`, and `check` also
  passed.

The critical-path boundary is deliberate. The premature untagged
LOAD/STORE/TAMAC terminal seam was removed: Landing 2.4 must add tokened image
staging and stale-response drain rather than accepting a late terminal from a
canceled operation, and arithmetic gets its whole-bank terminal commit in its
own landing. Until one of those paths can write nonzero data, synthesis may
constant-fold the declared zero-only bank; no resource claim is made from
Landing 2.3 alone.

Nonblocking work retained in the plan rather than expanded here:

- cluster-disable cancellation and stale tile-port response draining move to
  Landing 2.4, before the first TACC image beat can issue;
- deferred FORCE visibility across a multi-cycle success or fault, including
  the Phase-1 rule that an already accepted FORCE survives individual caller
  cancellation, must be exercised when the first tokened multi-cycle
  operation lands; and
- the leaf does not yet materialize Phase 1's separate wipe-generation token
  because no independently returning memory or arithmetic result exists in
  this landing; lifecycle staging is closed by the retire handshake. Landing
  2.4 must add that token with its transfer stage. Existing RTL caller and
  engine cancellation tokens are eight bits while the oracle uses 64-bit
  epochs; reset/disable draining and wraparound hardening remain explicit
  transfer-controller requirements rather than inflating idle lifecycle state.

### Landing 2.4 — canonical TACC image transfer

Primary files:

- `rtl/gpu/mp64_tacc.v`
- `rtl/gpu/mp64_tile.v`
- a shared `rtl/soc/mp64_tacc_transfer.v` or equivalent staging block
- `rtl/core/mp64_cluster.v`
- `rtl/mem/mp64_memory.v`
- `rtl/mem/mp64_extmem.v`
- `rtl/soc/mp64_soc.v`
- `rtl/soc/mp64_top.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- `rtl/platform/sim/mp64_platform_sim.v`
- `rtl/target/xilinx7/mp64_synth_top.v`
- every affected memory, cluster, SoC, and target-wrapper instantiation
- `rtl/sim/tb_memory.v`
- `rtl/sim/tb_extmem.v`
- `rtl/sim/tb_platform_sim.v`
- `rtl/sim/tb_tacc.v`
- `rtl/sim/tb_tacc_cycles.v`
- `rtl/sim/tb_tile_write_ack.v`
- `rtl/sim/Makefile`

Work:

- implement `TACC.LOAD` and `TACC.STORE` as four serialized 512-bit beats at
  offsets 0, 64, 128, and 192;
- validate ownership, validity where required, mode, alignment, and complete
  address span against the caller's ordinary memory permissions before
  issuing the first request;
- add acknowledged error and fault-address routing through internal memory,
  external memory, the seven-source arbiter, SoC, tile engine, and MEX
  completion; invalid memory requests must complete with error rather than
  hang without ACK;
- add a PHY-error input through target wrappers and a fail-closed 255-cycle
  per-PHY-word timeout in `mp64_extmem`; wrappers without a hardware error
  source tie only that sideband low, not the timeout;
- arbitrate one chip-wide 2,048-bit transfer staging image with equal
  round-robin admission across engines, retain its owner through all four
  beats, and never create seven per-engine staging copies;
- stage a complete load and publish it atomically only after the fourth
  successful acknowledgement;
- preflight stores, preserve TACC on a bus fault, and clear dirty only after
  the fourth successful write acknowledgement; a failed store preserves the
  preinstruction dirty bit whether it began clean or dirty;
- ignore acknowledgements associated with a pre-reset operation epoch and
  never commit a staged load after reset;
- add per-source cancel to the shared tile-port arbiter so cluster disable
  drops captured unissued work, drains and suppresses stale issued
  acknowledgements, and cannot consume a re-enabled source's fresh pulse;
- emit zeroes for every inactive canonical-image byte and normalize ignored
  inactive load bytes to zero;
- keep ownership after both operations; and
- verify exact request-capture, stage-wait, grant, ACK-return, timeout, and
  retirement cycle accounting under seven-engine memory-port contention.

The bench injects delayed acknowledgements on each beat, every address and bus
fault, clean and dirty failed stores, reset during every FSM state, competing
requests from all seven engines, exact beat order, and canonical zero padding.

Focused gates, run sequentially:

```sh
make -C rtl/sim memory
make -C rtl/sim extmem
make -C rtl/sim platform_sim
make -C rtl/sim tile_port_arbiter
make -C rtl/sim tacc
make -C rtl/sim tacc_cycles
make -C rtl/sim tile
make -C rtl/sim soc_elaborate
```

Commit:

```text
Transfer canonical TACC images in four beats

Stage complete loads and serialize acknowledged stores through the shared
tile-memory port without premature retirement.

Preserve ownership and architectural TACC state across transfer faults while
normalizing every inactive byte of the 256-byte image.
```

#### Landing 2.4 completion — 2026-07-28

Landing 2.4 is implemented in the isolated Phase-2 worktree. One chip-wide
2,048-bit stage now admits image requests from the four private full-core and
three cluster-shared engines with equal round-robin selection. It snapshots a
STORE image or assembles a LOAD image, owns that state across four serialized
512-bit beats, returns a token-qualified held response, and does not expose a
partial LOAD. U32/S32, FP16, and BF16 transfers normalize bytes 128–255 to
zero. The stage enters the focused simulation target and every explicit FPGA
source manifest.

The tile leaf preflights the complete aligned 256-byte span before BUSY or
traffic. It rejects wrapping, MMIO, holes, cross-region spans, user HBW, and
MPU violations with the first forbidden address. LOAD atomically publishes
the complete image only at retirement. STORE uses the latched TACC format,
retains a stable bank snapshot for all four beats, preserves the bank and
preinstruction DIRTY value on a transport fault, and clears DIRTY only after
the fourth successful acknowledgement and retirement.

The internal and external 512-bit targets now explicitly acknowledge request
capture and return terminal error plus a 64-bit fault address. External tile
accesses retain their complete payload, serialize eight one-word PHY
transactions per tile beat, hold each request through `PHY_READY`, and bound
both launch and response waits to 255 cycles. An accepted-word timeout or
controller reset closes the external response epoch: `PHY_CANCEL` remains
asserted, new requests remain blocked, and late acknowledgements are ignored
until `PHY_CANCEL_DONE` guarantees that `PHY_ACK` is low and no response from
the canceled epoch can recur. Tied-off wrappers acknowledge immediately only
because they contain no PHY response state. The seven-source tile arbiter
holds target requests through acceptance, including coincident acceptance and
cancel, routes error metadata only to the captured owner, and cancels pending
work or drains accepted stale work without delivering its completion. Cluster
disable drives that cancellation directly, so a disabled engine cannot leave
a captured image beat behind for a re-enabled caller.

The production SoC routes each TACC beat through its owning engine's existing
source lane rather than creating an eighth memory source. The full-core
integration bench seeds four SRAM rows, loads them through the shared stage
into core 0's private TACC, stores the same canonical image into four new
rows, checks exact owner/address order, and proves that the other three
private banks remain unchanged. The prior transfer advances the ordinary
memory arbiter cursor, and the existing four-core test correctly continues
equal round-robin service in 1–2–3–0 order.

Sequential verification record:

- `memory`: 30 checks passed;
- `extmem`: 175 checks passed, including timeout/reset quarantine, late-response
  suppression, and post-flush reuse;
- `platform_sim`: 5 checks passed after making its source manifest elaborate
  the real SoC hierarchy;
- `tile_port_arbiter`: 114 checks passed;
- `tacc_transfer`: 395 checks passed, plus focused Yosys `hierarchy`, `proc`,
  and `check`;
- `tacc`: 179 lifecycle and transfer-terminal checks passed;
- `tacc_cycles`: 56 timing, preflight, and fault checks passed;
- `tile`: 85 datapath/lifecycle checks and 34 write-ack checks passed;
- `cluster`: 136 checks passed;
- `full_core_tile`: 31 private-engine and end-to-end image checks passed;
- `disk_bus_dma`: 15 checks passed;
- `nic_bus`: 11 checks passed;
- reduced-parameter `soc_smoke`: 7 checks passed;
- `soc_tile_icache`: 11 checks passed; and
- full `mp64_soc` elaboration passed with only the established sized-hex
  warnings.

Nonblocking findings intentionally remain visible for closure rather than
expanding this functional landing:

- `PERF_STALLS` does not yet distinguish the four TACC transfer service
  acknowledgements from stage-acquisition, arbiter, and target wait cycles.
  Exact emulator/RTL cycle and counter parity remains an explicit Landing 2.8
  closure gate alongside `PERF_TILE_OPS`. The locked successful external-image
  increment of 32 serialized PHY words is also not yet connected to
  `PERF_EXTMEM` and remains part of that counter closure;
- a microcore's `mex_allow_cluster_spad` policy bit is captured with the
  operation, but TACC image preflight and the four-beat transport do not yet
  recognize or route the cluster-local scratchpad aperture. Landing 2.8 must
  either implement that explicit private route or close the policy contract
  before differential sign-off;
- the focused stage, target, and arbiter benches cover faults, timeouts,
  cancellation, stale responses, canonical padding, and all-seven admission,
  but the exhaustive reset-at-every-state and seven-engine contended SoC
  matrix remains in the planned `tacc_soc` closure bench in Landing 2.8;
- the pre-existing CPU/DMA memory transports have no error qualifier. External
  PHY error or timeout therefore completes those non-TACC requests with
  deterministic zero data instead of a precise trap, while TACC receives the
  new error and fault-address path. Ordinary non-TACC tile operations also do
  not yet consume the target error qualifier;
- the standalone legacy `tb_mp64_soc` harness is not an active Make target and
  still names hierarchy that predates the private full-core tile engines. A
  direct elaboration therefore fails before simulation; Landing 2.8 should
  rebase that harness onto supported observability points or remove it, while
  the active full-SoC elaboration, platform, smoke, private-engine, and
  coherence gates all pass; and
- heavyweight implementation, physical resource deltas, timing, and exact
  seven-bank post-synthesis evidence remain approval-gated Landing 2.9 work.

### Landing 2.5 — integer accumulation slice

Primary files:

- `rtl/gpu/mp64_tile.v`
- optional factored integer TACC datapath module
- `rtl/sim/tb_tacc.v`
- emulator-generated integer vector fixture

Work:

- reuse existing 8×8, 16×16, and 32×32 products;
- implement the locked 16-lane feedback slice;
- schedule 4/2/1 accumulation beats;
- meet the locked 7/5/4 tile-source and 6/4/3 broadcast full-core cycle totals
  before contention;
- implement signed extension, unsigned extension, and modular accumulation;
- reject 64-bit and reserved modes before source reads;
- make the complete lane update visible only at retirement; and
- cover tile×tile, scalar broadcast, and in-place source forms.

Focused gates, run sequentially:

```sh
make -C rtl/sim tacc
make -C rtl/sim tacc_cycles
make -C rtl/sim tile
make -C rtl/sim cluster
```

Commit:

```text
Add widened integer accumulation to RTL TACC

Reuse the existing multiplier paths and a sixteen-lane feedback slice for
8-, 16-, and 32-bit integer modes.

Match emulator vectors across signedness, wrap boundaries, source forms, and
multi-instruction accumulation without adding a multiplier array.
```

#### Landing 2.5 completion — 2026-07-29

The integer TAMAC datapath is implemented for U8/S8, U16/S16, and U32/S32.
It reuses the tile engine's existing exact 8×8, 16×16, and 32×32 products
with one structurally explicit bank of sixteen 64-bit feedback additions;
width and beat muxes feed that same bank, and U8 consumes each sum's low
32 bits. Four, two, or one feedback beats produce 32-bit modular U8
accumulators and 64-bit modular U16/U32 accumulators. Existing tile scratch
registers hold the in-flight canonical 2,048-bit image, including the required
zero upper half for U32, so the landing adds neither a second TACC bank nor
another multiplier array.

Tile×tile uses `TSRC0 × TSRC1`, scalar broadcast uses `TSRC0` and only the
low source-width element of the selected GPR, and the in-place form uses
`TDST × TSRC0`. The engine launches source A on the admission edge and meets
the locked uncontended totals: 7/5/4 cycles for tile and in-place U8/U16/U32,
and 6/4/3 cycles for broadcast. Reserved widths, unsupported formats,
noncanonical encodings, ownership failures, validity failures, and format
mismatches are rejected before source traffic.
TAMAC and image-operation validation faults retain the locked two-cycle base
latency without publishing physical TACC BUSY.

Every required 64-byte source span is preflighted in architectural operand
order before the first read. A subsequently acknowledged internal or
external source error terminates the instruction with BUS and the target's
exact fault address. Cancellation propagates into the shared tile-port
arbiter's cancel/drain path so an accepted stale source cannot complete into
a new owner. The leaf keeps the persistent bank unchanged throughout
execution, exposes no partial beat, and commits the complete staged image
only at normal retirement. Cancellation, source faults, and preflight faults
therefore preserve the preinstruction image and DIRTY state.

The checked-in integer fixture is generated by executing the canonical TAMAC
instructions through the Phase-1 Python emulator. Six adversarial cases cover
both signedness choices at all three widths, tile, broadcast, and in-place
forms, poisoned broadcast upper bits, signed extrema, positive and negative
modular wrap, repeated accumulation, and exact complete 2,048-bit images.
Focused RTL tests additionally cover no early visibility, legacy ACC
isolation, exact source-read counts and cycles, source-span preflight,
dynamic second-source failure, external-source routing, cancellation while a
source is stalled, direct private-engine execution, and registered execution
through a cluster-shared engine.

Sequential verification record:

- the generator reproduced `tamac_integer_vectors.vec` byte-for-byte;
- `tacc`: 196 checks passed;
- `tacc_cycles`: 224 checks passed;
- `tile`: 85 datapath checks and 34 write-ack checks passed;
- `cluster`: 143 checks passed;
- reduced-parameter `soc_smoke`: 7 checks passed; and
- full `mp64_soc` elaboration passed with only the established sized-hex
  warnings.

Nonblocking findings intentionally remain visible for Landing 2.8 closure:

- the tile programming contract describes `TSRC0`, `TSRC1`, and `TDST`
  source tiles as 64-byte-aligned, while the Phase-1 executable source-span
  preflight currently admits unaligned spans and the physical 512-bit port is
  row-granular. The current architectural vectors are aligned. Differential
  sign-off must make one behavior explicit across the contract, emulator,
  and RTL: either reject an unaligned source before traffic or implement its
  exact row-assembly semantics;
- the captured `mex_allow_cluster_spad` policy is not yet connected to a
  cluster-local scratchpad route for TAMAC source spans, matching the
  outstanding image-transfer route finding from Landing 2.4;
- exact `PERF_STALLS`, `PERF_TILE_OPS`, and external-word counter parity
  remains part of the planned counter closure;
- the complete SoC elaborates all four private full-core engines with the
  integer datapath, but `tb_full_core_tile` does not yet execute a TAMAC
  through a CPU dispatch. That end-to-end private-core arithmetic case remains
  in the Landing 2.8 differential SoC matrix; and
- FP16/BF16 TAMAC remains deliberately illegal until Landings 2.6 and 2.7,
  while reset-at-every-state and exhaustive seven-engine contention remain
  in the Landing 2.8 differential SoC matrix.

### Landing 2.6 — shared exact FP32 tile arithmetic

Primary files:

- `rtl/core/mp64_fp16_alu.v`
- new narrowly scoped exact product and reusable FP32 feedback modules
- `rtl/gpu/mp64_tile.v`
- existing tile and FP-focused benches
- emulator-generated FP arithmetic fixtures

Work:

- expose or implement an exact, unrounded FP16/BF16 product descriptor plus
  the product-only binary32 RNE path needed by `WMUL`;
- provide a reusable bit-exact binary32 RNE add-product feedback
  adder/sequencer;
- support canonical NaN, infinities, signed zero, and subnormals;
- correct `WMUL` so its binary32 result is produced directly from the exact
  half-precision product, without first rounding the product back to FP16 or
  BF16;
- mux the reusable feedback lanes with an applicable existing FP reduction
  stage so the physical bank is shared rather than dedicated to TACC, without
  requiring a wholesale rewrite of the legacy reduction trees in this
  landing;
- cap the reusable bank at 16 feedback lanes per engine;
- preserve the existing legacy-operation completion interfaces; and
- pass existing regressions plus adversarial FP fixtures before TACC is
  connected.

#### Landing 2.6 scope clarification — 2026-07-29

A pre-implementation audit found that the Phase-1 executable behavior of the
legacy FP reductions is not a binary32 feedback-tree contract. `DOT` and
`DOTACC` accumulate lane-ordered products in host binary64 and round once
when publishing binary32 ACC state. `SUM` and `SUMSQ` currently use the
running CPython `sum()` implementation before that final binary32
conversion. Phase-1 native differential tests deliberately lock adversarial
cancellation and rounding results that an iterated or balanced binary32
adder tree does not reproduce.

Moving all four legacy reductions onto the new sixteen-lane binary32 bank
would therefore either change Phase-1 architectural results or require a
separate wider arithmetic and sequencing contract. The latter is not the
same resource described by this landing, and reproducing an implementation
detail of CPython is not an acceptable production ISA definition.

The critical path consequently proceeds with the exact product descriptor,
direct binary32 `WMUL` result, bit-exact add-product feedback operation, and
one physically shared/muxed sixteen-lane bank needed by floating TACC. It
does not claim that DOT, DOTACC, SUM, or SUMSQ have been made bit-for-bit
equivalent to their CPython-dependent Phase-1 behavior by that bank, and it
does not disturb their current RTL completion interface merely to force the
refactor.

Legacy FP reduction reconciliation is an explicit nonblocking production
item: choose and document a deterministic reduction order and intermediate
precision as an ISA contract, then either amend the Phase-1 oracle and its
tests or implement the corresponding wider RTL. This choice is not required
to build and verify the TACC datapath, but legacy FP differential parity must
remain open and must not be claimed at production sign-off until the choice
is implemented.

Focused gates, run sequentially:

```sh
make -C rtl/sim tile
make -C rtl/sim tacc
```

Commit:

```text
Share exact FP32 arithmetic across tile operations

Convert FP16 and BF16 products directly into binary32 feedback addition
without intermediate half rounding.

Correct widening multiply and time-multiplex a bounded feedback bank with an
applicable existing reduction stage while preserving legacy cycle interfaces.
```

#### Landing 2.6 completion — 2026-07-29

`mp64_fp_exact.v` now provides a bounded exact FP16/BF16 product descriptor,
a product-only binary32 RNE packer, canonical binary32 addition, and
binary32-plus-exact-product feedback with one final RNE point. The common
finite adder uses a 27-bit normalized guard/round/sticky path with bounded
right-shift-with-jam; it covers subnormals, signed zero, finite cancellation,
overflow, infinities, invalid `0 × infinity`, and canonical NaN without
`real`, `shortreal`, or exponent-sized arithmetic.

Each tile engine has one 32-lane exact half-product array. Its independently
rounded outputs directly feed WMUL and the applicable legacy reductions, so
WMUL no longer rounds through FP16/BF16. Product-only rounding uses a
dedicated packer with no add/subtract cone. One generated bank of sixteen
`mp64_fp32_feedback_rne` lanes is muxed between the first reduction level and
floating TACC. Later reduction levels remain ordinary FP32 adders rather than
additional exact-product feedback lanes. Yosys hierarchy inspection reports
exactly 32 exact-product cells and 16 feedback-bank cells per physical tile
engine.

Floating arithmetic has a real timing boundary ready for Landing 2.7: even
TAMAC beats register one selected group of sixteen exact descriptors, and
the following odd beat presents only that stable group to the feedback bank.
Floating TAMAC admission remains deliberately disabled until Landing 2.7
adds result capture, the beat-three terminal condition, and canonical image
publication atomically.

The checked-in fixture is reproduced byte-for-byte by
`gen_fp_exact_vectors.py`. It covers both formats, exact product bits,
tie-to-even in both directions, subnormal boundaries, overflow, signed zero,
cancellation, NaN/infinity cases, the fused BF16 half-subnormal-ULP case, and
deterministic randomized alignment cases. Sequential verification passed:

- `fp_exact`: 640 checks;
- `tile`: 89 datapath checks, including exact WMUL in both destination tiles;
- `tile_write_ack`: 34 checks;
- `tacc`: 196 lifecycle checks; and
- Icarus warning elaboration plus Yosys frontend/hierarchy validation, with
  only the established array-sensitivity and memory-to-register warnings.

The legacy reduction semantic conflict described above remains explicitly
nonblocking. This landing removes the duplicated DOTACC, SUM, and SUMSQ RTL
trees and shares their physical arithmetic, but it does not claim bit parity
with the Phase-1 host-binary64/CPython oracle for adversarial legacy
reductions.

### Landing 2.7 — FP16 and BF16 TACC accumulation

Primary files:

- `rtl/gpu/mp64_tacc.v`
- `rtl/gpu/mp64_tile.v`
- reusable exact FP modules from Landing 2.6
- `rtl/sim/tb_tacc.v`
- emulator-generated TACC FP vector fixture

Work:

- connect exact FP16/BF16 product widening and FP32 feedback addition to 32
  TACC lanes;
- process two 16-lane groups with bounded pipeline staging;
- meet the locked seven-cycle tile/in-place and six-cycle broadcast
  full-core totals before contention;
- enforce owner, valid, format, and supported-mode checks before source reads
  or mutation;
- support tile×tile, low-element scalar broadcast, and in-place sources;
- match persistence, repeated accumulation, cancellation, NaN, infinity,
  signed-zero, subnormal, and tie-to-even vectors; and
- verify canonical 256-byte images for both floating formats.

Focused gates, run sequentially:

```sh
make -C rtl/sim tacc
make -C rtl/sim tacc_cycles
make -C rtl/sim tile
make -C rtl/sim cluster
```

Commit:

```text
Accumulate FP16 and BF16 products into TACC

Connect the exact widened product and shared FP32 feedback datapath to every
physical tile engine.

Match the emulator across persistent mixed-precision accumulation, source
forms, canonical images, and IEEE edge cases.
```

#### Landing 2.7 completion — 2026-07-29

FP16 and BF16 are now legal TAMAC formats after the same owner, valid-state,
canonical-encoding, and latched-format checks as integer TAMAC. Floating
signedness is normalized away, so either incoming `TMODE.SIGNED` setting
matches the one canonical floating format rather than creating a hidden
second format.

Each engine executes floating TAMAC as two registered groups of sixteen
lanes. An even arithmetic beat captures exact product descriptors from the
Landing-2.6 multiplier array; the following odd beat performs the sole
binary32 round-to-nearest-even feedback addition through the shared bank.
The second odd beat captures lanes 16–31 and terminates. Tile and in-place
forms therefore retain the locked seven-cycle total, while broadcast retains
six cycles because it reads one source tile. Only the low 1,024 result bits
are published; the inactive image half is always zero.

`gen_tamac_fp_vectors.py` executes the Phase-1 emulator to produce six
checked-in full-image fixtures covering FP16 and BF16 tile, poisoned
broadcast, and in-place forms. The fixture crosses lanes 15/16, repeats
feedback, and covers exact fused rounding, subnormals, signed zero,
cancellation, overflow, infinity, invalid products, and canonical NaN. The
RTL gate regenerates and compares the fixture byte-for-byte before running
it. A separate timing case cancels after the first registered product group
and proves that no partial or late image is published.

Sequential verification passed:

- `tacc`: 270 lifecycle, admission, atomicity, mismatch, cancellation, and
  fault checks;
- `tacc_cycles`: 399 timing, source-traffic, emulator-vector, persistence,
  and cancellation checks;
- `fp_exact`: 640 exact product/feedback checks;
- `tile`: 89 datapath checks;
- `tile_write_ack`: 34 write-retirement checks; and
- `cluster`: 148 checks, including production-dispatch FP16 TAMAC on a
  cluster-shared engine.

Focused Yosys SystemVerilog frontend and `mp64_tile` hierarchy validation
also passed. Its only diagnostics were the established unpacked-array
memory-to-register lowering warnings.

### Landing 2.8 — arbitration, SoC, and differential closure

Primary files:

- `rtl/core/mp64_cluster.v`
- `rtl/soc/mp64_soc.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- `rtl/sim/tb_cluster.v`
- `rtl/sim/tb_tacc.v`
- `rtl/sim/tb_tacc_cycles.v`
- fail-closed `tacc_vectors` target backed by the authoritative exact-cycle
  vector consumer
- new focused `rtl/sim/tb_tacc_soc.v`
- `rtl/sim/Makefile`
- vector generator and fixtures

Work:

- exercise simultaneous independent full-core claims, competing cluster
  claims, and ordinary MEX requests;
- prove losing cluster claims retire and stateless work remains available;
- prove all seven engines remain isolated while the shared memory port is
  contended;
- prove owner state survives unrelated traps/interrupts;
- prove reset, cluster disable, and force-release wipe state;
- prove individual microcore reset preserves its cluster's TACC and every
  domain reset leaves the other six banks unchanged;
- prove `TACC_CTL` privilege behavior matches full-core and cluster privilege
  sources;
- check `PERF_STALLS` and `PERF_TILE_OPS` behavior;
- add fail-closed `tacc_vectors` and `tacc_soc` targets, make them
  dependencies of `all`, and avoid using the BIOS-dependent
  five-million-cycle `tb_mp64_soc.v` as the feature gate;
- run every emulator-generated integer and FP vector in RTL; and
- compare canonical stored images, status, memory side effects, and faults.

Closure gates, run sequentially with make explicitly limited to one job:

```sh
make -C rtl/sim -j1 tacc_vectors
make -C rtl/sim -j1 tacc_cycles
make -C rtl/sim -j1 tacc_soc
make -C rtl/sim -j1 all
make -C rtl/sim -j1 soc_smoke
make -C rtl/sim -j1 soc_tile_icache
make -C rtl/sim -j1 soc_elaborate
```

Commit:

```text
Close TACC behavior across RTL arbitration and SoC integration

Prove the same ownership, fault, retirement, reset, and canonical-image
contract on private full-core and cluster-shared tile engines.

Consume emulator-generated arithmetic vectors so RTL and Phase 1 agree
bit-for-bit at every architectural boundary.
```

#### Landing 2.8 completion — 2026-07-29

Landing 2.8 was split into three reviewable commits. The source-preflight
landing made operand ordering identical in Python, native execution, and RTL:
every required source is aligned and routed before the first read, and the
unsupported cluster-scratchpad aperture now fails BUS before traffic. The
counter landing defined a stall as a held request cycle with no cancellation,
completion, or acknowledged progress, routed cluster stalls only to the
granted microcore, counted every successfully completed external PHY word,
and attributed those words only to the owning full core. The final SoC
landing closes composition, reset scope, and fail-closed vector execution.

The former `tb_full_core_tile` harness is now `tb_tacc_soc`. It proves four
private full-core and three cluster-shared ownership domains, private legacy
ACC/CSR state, canonical full-core LOAD/STORE through the shared image stage
and seven-source port, cluster sibling claim behavior, and four simultaneous
private ordinary tile operations. Seven distinct valid dirty TACC images are
then used to prove that individual microcore reset preserves shared state,
paired full-core reset wipes only its private domain, cluster disable wipes
only that shared domain, supervisor FORCE wipes only the selected domain, all
three wiped domains can be reclaimed, and whole-SoC reset zeroizes all seven.
Named `core_domain_reset` and `cluster_micro_reset` seams make the intended
scope explicit, but remain tied inactive until a production reset controller
is specified.

The production cluster arbiter now has direct coverage for simultaneous
losing sibling `TACC.TRY` and stateless work, owner-microcore reset, rejected
user FORCE, successful supervisor FORCE, and reclaim/release. Full-core and
microcore CPU benches check exact `PERF_TILE_OPS` and controlled
`PERF_STALLS` deltas, fault non-retirement, and that TACC status/control CSR
traffic is not counted as a tile operation.

`tacc_vectors` intentionally does not introduce a second fixture parser. It
depends on `tb_tacc_cycles`, which already executes all six integer and all
six floating emulator-generated fixtures with exact images, cycles, source
counts, faults, cancellation, and retirement checks. Both generators are
run into isolated temporary files and must exit successfully before
byte-for-byte comparison. The fixture parser rejects every malformed
non-comment record and any extra field, then requires exactly six records per
file. This keeps one authoritative RTL consumer while remaining fail closed.

Sequential focused verification passed:

- `cpu_smoke`: 107 checks;
- `cpu_micro`: 88 checks;
- `cluster`: 167 checks;
- `tacc_vectors` / `tacc_cycles`: 404 checks across all twelve generated
  arithmetic fixtures;
- `tacc_soc`: 43 seven-domain topology, image, isolation, and reset checks;
  and
- full `mp64_soc` elaboration, with only the established sized-hex warnings.

The following are intentionally documented rather than added to the
build-critical path:

- the independent reset seams have no production controller inputs yet and
  are exercised by focused hierarchical force;
- the composed SoC bench does not repeat leaf/stage/port coverage for reset in
  every in-flight window or stale accepted ACK, nor launch seven simultaneous
  memory-producing engines;
- full-core owner preservation across an actual CPU interrupt/trap and the
  task-migration STORE/RELEASE/LOAD sequence remain composition tests; the
  underlying lifecycle, image, cancellation, FORCE, and CPU trap primitives
  are covered independently;
- an actual CPU-fetched full-core TAMAC is not repeated in the topology
  harness; exact arithmetic runs through the tile leaf and cluster production
  dispatch, while full-core CPU MEX dispatch is covered with controlled
  normal completion; and
- the portable full-SoC image route currently adds registered no-progress
  cycles beyond the locked strict-system internal-image total. Stall
  accounting is now truthful, but strict composed-cycle parity remains a
  nonblocking optimization/measurement item rather than a functional
  correctness claim.

The BIOS-heavy `tb_mp64_soc.v` remains explicitly retired and outside the
Make graph because its hierarchy predates private full-core engines.
`tb_tacc_soc`, reduced SoC smoke/coherence gates, and standalone
`soc_elaborate` are the supported integration checks.

### Landing 2.9 — synthesis, timing, and Phase-2 capstone

Primary files:

- `fpga/synth_genesys2.tcl`
- `fpga/synth_yosys_soc.tcl`
- `fpga/synth_yosys_all.tcl`
- `fpga/run_tacc_impl.py`
- new `fpga/check_tacc_reports.py`
- any additional explicit production source list discovered during
  implementation
- checked-in resource/timing summary
- this handoff and public architecture documents

#### Landing 2.9 execution constraint — 2026-07-29

Final routed acceptance is presently blocked on two external prerequisites:
the production FPGA target and memory configuration must be fixed, and a
working Vivado installation must be available for like-for-like
implementation runs. Until both are present, no routed LUT, FF, BRAM, DSP,
WNS, TNS, or Fmax result may be inferred from behavioral simulation or a
lightweight frontend check, and Phase 2 must not be marked physically
complete.

This does not block lightweight preparation. Source-manifest auditing,
fail-closed runner and report-checker implementation, configuration
validation, Yosys frontend/hierarchy checks, simulation elaboration, and
documentation remain in scope. Those artifacts should clearly report the
missing routed prerequisites rather than fabricate or silently skip
acceptance data.

Work:

1. verify that every new module is present in every explicit FPGA source
   list;
2. add the checked Vivado implementation/report mode, isolated source
   materialization runner, and fail-closed report comparator described by the
   physical contract;
3. obtain approval for the heavyweight tool runs;
4. measure the locked Phase-0 base, immutable topology-only commit
   `364d44283ba5c2fad8187b63da6917af60344c26`, and final TACC branch with
   identical tool and constraint settings;
5. report current-main → topology-only and topology-only → TACC hierarchical
   LUT, FF, BRAM, DSP, WNS, TNS, and Fmax deltas separately;
6. confirm exactly seven tile engines and seven TACC banks in the elaborated
   hierarchy;
7. inspect multiplier and FP-adder sharing rather than trusting totals alone;
8. confirm no unconstrained paths and nonnegative WNS/TNS at 100 MHz;
9. make the report checker fail on any resource percentage, remaining
   headroom, timing, unconstrained-path, or seven-instance-count violation;
10. refactor and repeat if a gate fails; and
11. record final measured latency, utilization, timing, and remaining
    headroom.

Do not commit a feature that only passes behavioral simulation but exceeds
the device or timing budget.

After approval, run all three implementations and the fail-closed comparison:

```sh
python fpga/run_tacc_impl.py \
  --source-ref c8e8118e82a899ec3f101f63d277a1bf4ef5f84a \
  --label current-main --out /tmp/megapad-tacc-reports/current-main
python fpga/run_tacc_impl.py \
  --source-ref 364d44283ba5c2fad8187b63da6917af60344c26 \
  --label topology-only --out /tmp/megapad-tacc-reports/topology-only
python fpga/run_tacc_impl.py \
  --source-tree /home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc-rtl \
  --label full-tacc --out /tmp/megapad-tacc-reports/full-tacc
python fpga/check_tacc_reports.py \
  --current-main /tmp/megapad-tacc-reports/current-main \
  --topology-only /tmp/megapad-tacc-reports/topology-only \
  --full-tacc /tmp/megapad-tacc-reports/full-tacc
```

Commit:

```text
Close full TACC resource and timing acceptance

Record like-for-like baseline and TACC synthesis results, hierarchical
sharing, routed timing, and remaining device headroom.

Confirm every locked functional and physical gate and mark the handoff
complete only after the routed design passes.
```

## 14. Verification matrix

Every row must be covered by the Python oracle, native differential tests,
and RTL vectors or benches before Phase 2 closes.

| Area | Required cases |
|---|---|
| Encoding | Canonical tile, broadcast, in-place, lifecycle; reserved function; nonzero reserved bits; illegal SS. |
| Ownership | Free claim, idempotent self-claim, losing claim, nonowner protected op, release, force-release, reset, cluster disable. |
| Formats | U8/S8, U16/S16, U32/S32, FP16, BF16; U64 and EW6/7 rejection; format mismatch. |
| Integer arithmetic | Zero, one, signed extrema, unsigned extrema, exact products, positive and negative wrap, repeated accumulation. |
| Floating arithmetic | Normal, tie-to-even, cancellation, signed zero, subnormal input/result, overflow, infinity, NaN, invalid product/add. |
| Sources | Tile×tile, low-element GPR broadcast at every width, in-place source form. |
| Images | Four-beat order, little-endian lanes, inactive zero padding, load normalization, clean/dirty transitions. |
| Faults | Ownership, validity, format, alignment, bus span, dynamic memory/PHY error, PHY timeout, privilege; exact `TRAP_ADDR`, end-of-instruction return PC, no pre-fault mutation, and no unacknowledged hang. |
| Topology and concurrency | Exactly four private full-core plus three cluster-shared engines and TACCs; concurrent full-core execution; same-cluster contention; seven-domain isolation; equal-RR image-stage acquisition; seven-source memory-port fairness; stateless service during ownership. |
| Legacy isolation | TACC operations preserve legacy ACC/config/destination state; full-core and cluster CSR/SHA/tile legacy ACC users see one acknowledged deterministic domain; caller-private `ACC_ZERO`; nonowner legacy MEX remains available. |
| Retirement and cycles | Locked per-form cycles, cluster/port/ACK stalls, busy/force-pending visibility, final-ACK retirement, retirement-or-trap deferred release, failed-TRY count, fault non-retirement, and per-core `PERF_STALLS`/`PERF_TILE_OPS`. |
| Reset | Whole SoC, paired full-core domain, cluster disable, individual microcore, every in-flight stage, stale ACK epoch, and isolation of the other six engines. |
| Context | Store/release/load round trip, interrupt preservation, task migration, dead-owner recovery. |

## 15. Worktree and commit discipline

Phase-2 RTL implementation work stays in:

```text
/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc-rtl
```

The primary `megapad/` checkout remains on `main` and must not receive
generated extensions, test caches, BIOS artifacts, or intermediate edits from
this feature.

Before each commit:

1. run only the landing's focused tests, sequentially;
2. run `git diff --check`;
3. inspect `git status --short`;
4. stage only files owned by the landing;
5. inspect `git diff --cached`; and
6. use the detailed multi-paragraph commit message specified above.

Do not combine emulator and RTL landings in one commit. Do not carry a
temporarily failing half-implementation across a landing boundary. Do not run
smoke, integration, persistence, worker, sanitizer, or synthesis jobs
concurrently.

## 16. Explicit non-goals

This feature does not include:

- tile engines beyond the locked four private full-core and three
  cluster-shared instances;
- per-microcore tile engines or TACC banks;
- TACC banks beyond one per locked physical tile engine;
- 64×64→128 TACC arithmetic;
- FP32 source tiles;
- implicit claim, spill, restore, eviction, or release;
- a hardware blocking claim;
- a new QoS weighting implementation;
- production-SoC integration of the currently bench-only Field ALU RTL;
- TACC-controlled redirection of `TMAC` or `TFMA`;
- saturating accumulation;
- direct narrowed TACC stores;
- changing legacy `ACC` arithmetic, encodings, or visibility except to
  reconcile the duplicate full-core and microcluster RTL copies into the
  locked seven engine domains; or
- treating current manual FPGA estimates as closure evidence.

Direct narrowed store may be proposed later using the reserved extended-TSYS
function 7, but only after full-width TACC is measured and in use.

## 17. Completion checklist

Phase 0:

- [x] Width, topology, formats, ISA, ownership, status, image, faults, and
  recovery are locked.
- [x] Emulator and RTL work are split into commit-sized landings.
- [x] Resource and timing gates are explicit.
- [x] Phase-0 document committed alone.

Phase 1:

- [x] Python executable oracle.
- [x] Seven-engine topology and cluster-private caller shadows.
- [x] Ownership and state transport.
- [x] Native execution parity.
- [x] Contention/QoS closure, including the explicitly approved one-, two-,
  and four-worker contention and complete reduced-core gates.
- [x] Guest words, public docs, and capstone kernels.

Phase 2:

- [x] RTL encodings and precise fault plumbing.
- [x] Seven-engine restoration, private/shadow state, and common admission.
- [x] Lifecycle state and privileged recovery.
- [x] Canonical four-beat image transfer.
- [x] Integer accumulation.
- [x] Shared exact FP32 arithmetic.
- [x] FP16/BF16 TACC accumulation.
- [x] Differential SoC closure, with nonblocking composition gaps recorded
  in the Landing 2.8 completion notes.
- [ ] Approved synthesis, routed timing, and resource acceptance.

## 18. Phase 1 integration closure

Phase 1 was integrated only after the previously unmerged Phase 5 production
concurrency rollout. Commit `5f1e4e5` merges Phase 5 onto the newer main
security and networking base. Commit `895fca8` then merges the full-TACC
feature tip and resolves the two overlapping scheduler files.

The combined contract is:

- every positive production batch remains owned by Phase 5's single native
  system coordinator, with `worker_count=None` resolving once to the supported
  one-, two-, or four-lane policy;
- cluster-capable public `step()` uses that same native coordinator when no
  core has a deliberate step override, so direct stepping and batching see
  the same common tile-engine admission;
- exact cycle-bounded execution remains limited to its proved full-core-only
  topology;
- every full core has a private ACC/TACC engine and each microcluster shares
  one ACC/TACC engine, for seven physical ownership domains; and
- dedicated multi-core TACC oracle fixtures explicitly use one worker.
  Cross-width
  behavior is tested only by the named Phase 5 and TACC contention gates, so
  reference semantics do not silently depend on host affinity.

### 18.1 Combined-tree evidence

All emulator tests below ran sequentially with
`MP64_RUNTIME_NAMESPACE=megapad-p5-tacc-integration`.

| Gate | Result |
|---|---:|
| Accelerator rebuild | passed |
| Native batch boundaries | 25 passed |
| Public-step versus cluster batch admission | 1 passed |
| Native exact-cycle execution | 56 passed |
| Native system state, including TACC and TRNG state | 87 passed |
| Phase-1 TACC capstone matrix | 281 passed |
| Phase 5 rollout policy | 17 passed |
| Phase 5 cross-entrypoint closure selection | 14 passed |
| TACC one-/two-/four-worker contention | 1 passed |
| Complete reduced-core execution | 34 passed |
| Full fallback, private-engine, and microcluster sweep | 91 passed |
| Checked BIOS SHA-2 | 18 passed |
| BIOS entropy and caller-span boundary | 7 passed |
| Akashic host packaging/import against this integration worktree | 14 passed |

The 281-case TACC capstone comprises 69 ISA cases, 28 portable-core cases,
8 system TACC cases, 21 disk-tool cases, 51 native MEX/TACC cases, 35
cycle-API cases, 2 focused private-engine ownership/reset cases, 17
tile-memory arbitration cases, 15 timed scheduler cases, and 11 portable,
9 native, and 15 timed external-PHY cases.

`git diff --check` passed, no unresolved paths or stale compatibility
scheduler symbols remain, and the merged `networking.f` is byte-identical to
the newer main version. The Akashic host-only packaging selector imported
`diskutil` and the network boot harness directly from the isolated integration
worktree without modifying the concurrent Akashic checkout.

### 18.2 Deliberately unclaimed heavyweight evidence

This bounded merge did not rerun the 400-million-step KDOS AES smoke, the
800-million-step Akashic AES contract, the 2-billion-step maximum-SNI guest
case, the approximately 1.98-billion-step staged hostname lifecycle, or the
0.8–1.6-billion-step Akashic SR2 gates. It also did not rerun unrestricted,
sanitizer, persistence, FPGA, or synthesis suites. Those jobs require fresh
resource approval and must run individually.

The additional eight-case native-TRNG concurrency-handoff selector was not
rerun because it constructs helper-worker systems outside the approved named
worker gates. Its native state-machine coverage is represented here by the
complete 87-case native-system-state file, not claimed as an equivalent
replacement for that selector.

The checked SHA, entropy, caller-span, and DNS/SNI changes were already in the
TACC base or newer main, merged without semantic conflict, and were retained
by source audit. The focused combined-tree checks above are integration
evidence; they do not relabel earlier downstream or heavyweight results as
fresh evidence.
