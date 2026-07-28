# Full TACC ISA and implementation handoff

- Status: Phase 0 complete; ISA and architectural contracts locked
- Date: 2026-07-28
- Feature branch: `feature/megapad-full-tacc`
- Phase-0 base commit: `c8e8118e82a899ec3f101f63d277a1bf4ef5f84a`
- Isolated worktree:
  `/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc`

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

The topology-only checkpoint must fit and close timing before TACC RTL begins.
If it lacks the required final headroom, optimize the baseline rather than
silently deleting private engines. If TACC misses a gate, refactor or add
bounded pipeline stages; do not add more arithmetic copies.

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
- implement FP `TAMAC` or deliberately retain transactional fallback for
  edge cases until native behavior is bit-exact;
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
git diff --check
```

The 1/2/4-worker comparison in `tests/test_tacc_contention.py` and the reduced
core execution suite spawn workers. Obtain explicit user approval before
running either, regardless of prior resource observations, and run them
sequentially:

```sh
python -m pytest -q tests/test_tacc_contention.py
python -m pytest -q tests/test_phase3_reduced_core_execution.py -k tacc
```

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
python -m pytest -q tests/test_tacc_isa.py
python -m pytest -q tests/test_megapad64.py -k tacc
python -m pytest -q tests/test_system.py -k tacc
python -m pytest -q tests/test_native_mex_oracle.py -k tacc
python -m pytest -q tests/test_tacc_cycle_api.py
python -m pytest -q tests/test_tile_engine_memory_arbitration.py
git diff --check
```

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
- `fpga/synth_genesys2.tcl`
- `fpga/synth_yosys_soc.tcl`
- `fpga/synth_yosys_all.tcl`
- new `fpga/run_tacc_impl.py`
- FPGA source lists and a checked-in topology-only utilization summary

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
- add a checked implementation mode to the Vivado script that executes
  `opt_design`, placement, physical optimization, and routing and emits
  post-route reports; and
- add a fail-closed implementation runner accepting exactly one of
  `--source-tree` or `--source-ref`, plus `--label` and `--out`; it
  materializes refs without modifying another checkout and records the source
  commit, tool version, constraints, strategy, hierarchy counts, utilization,
  timing, and unconstrained paths; and
- add every new RTL module to each explicit FPGA synthesis source list.

Before TACC RTL begins, obtain approval for the heavyweight run and synthesize
and route both the locked Phase-0 main baseline and this topology-only landing
with identical settings through the checked implementation mode. Record
hierarchical utilization, post-route timing, unconstrained-path status, and
the expected count of seven tile instances. If the corrected topology does
not fit or close timing, optimize it here; do not mask the problem by
returning to core-0-only or shared-full-core execution.

After approval, the like-for-like commands are:

```sh
python fpga/run_tacc_impl.py \
  --source-ref c8e8118e82a899ec3f101f63d277a1bf4ef5f84a \
  --label current-main --out /tmp/megapad-tacc-reports/current-main
python fpga/run_tacc_impl.py \
  --source-tree /home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc \
  --label topology-only --out /tmp/megapad-tacc-reports/topology-only
```

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
give cluster callers private configuration shadows, and record the corrected
topology synthesis baseline.
```

### Landing 2.3 — TACC state and lifecycle

Primary files:

- new `rtl/gpu/mp64_tacc.v` or an equivalently contained state module
- `rtl/gpu/mp64_tile.v`
- `rtl/core/mp64_cluster.v`
- `rtl/soc/mp64_soc.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- new `rtl/sim/tb_tacc.v`
- new `rtl/sim/tb_tacc_cycles.v`
- `rtl/sim/Makefile`

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
- on cluster disable, cancel its captured but unissued memory request, ignore
  any already-issued stale acknowledgement, and prevent subsequent beats; and
- add simulation assertions for legal metadata transitions, owner-only
  mutation, and fault-implies-completion.

The focused bench covers all seven independent domains, simultaneous
full-core claims, cluster contention, clear, release, reset, cluster disable,
individual microcore reset without cluster-state loss, mode latch, every
protected-operation fault, privilege source, cancellation epochs, and
force-release after both successful and faulting active operations, including
same-cycle force/admission priority. The cycle bench makes request capture,
cluster admission, and lifecycle/CSR timing fail-closed rather than relying
on waveform inspection.

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

### Landing 2.6 — shared exact FP32 tile arithmetic

Primary files:

- `rtl/core/mp64_fp16_alu.v`
- new narrowly scoped exact product and reusable FP32 feedback modules
- `rtl/gpu/mp64_tile.v`
- existing tile and FP-focused benches
- emulator-generated FP arithmetic fixtures

Work:

- expose or implement an unrounded FP16/BF16 product-to-binary32 path;
- provide a reusable bit-exact binary32 RNE feedback adder/sequencer;
- support canonical NaN, infinities, signed zero, and subnormals;
- refactor the existing applicable DOT, DOTACC, SUM, SUMSQ, and WMUL paths to
  share the bounded arithmetic without changing architectural results;
- cap the reusable bank at 16 feedback lanes per engine;
- preserve the locked completion-cycle interface while refactoring internal
  pipeline stages; and
- pass existing regressions plus adversarial FP fixtures before TACC is
  connected.

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

Replace duplicated reduction adders with a bounded reusable datapath while
preserving existing tile results and IEEE edge behavior.
```

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

### Landing 2.8 — arbitration, SoC, and differential closure

Primary files:

- `rtl/core/mp64_cluster.v`
- `rtl/soc/mp64_soc.v`
- `rtl/soc/mp64_tile_port_arbiter.v`
- `rtl/sim/tb_cluster.v`
- `rtl/sim/tb_tacc.v`
- `rtl/sim/tb_tacc_cycles.v`
- new focused `rtl/sim/tb_tacc_vectors.v`
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

Work:

1. obtain approval for the heavyweight tool run;
2. verify that every new module is present in every explicit FPGA source
   list;
3. reproduce the current-main and seven-engine topology-only measurements if
   tool or constraint settings changed after Landing 2.2;
4. synthesize and route the final seven-engine TACC branch through the checked
   implementation mode added in Landing 2.2;
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

After approval, run the final implementation and the fail-closed comparison:

```sh
python fpga/run_tacc_impl.py \
  --source-tree /home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc \
  --label full-tacc --out /tmp/megapad-tacc-reports/full-tacc
python fpga/check_tacc_reports.py \
  --current-main /tmp/megapad-tacc-reports/current-main \
  --topology-only /tmp/megapad-tacc-reports/topology-only \
  --full-tacc /tmp/megapad-tacc-reports/full-tacc
```

If tool, constraint, or strategy settings changed after Landing 2.2, rerun
the two baseline commands from that landing first against worktrees at the
recorded source commits.

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

All implementation work stays in:

```text
/home/kir/Documents/Projects/fantasy-computing/.worktrees/megapad-full-tacc
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

- [ ] Python executable oracle.
- [ ] Seven-engine topology and cluster-private caller shadows.
- [ ] Ownership and state transport.
- [ ] Native execution parity.
- [ ] Contention/QoS closure.
- [ ] Guest words, public docs, and capstone kernels.

Phase 2:

- [ ] RTL encodings and precise fault plumbing.
- [ ] Seven-engine restoration, private/shadow state, and topology baseline.
- [ ] Lifecycle state and privileged recovery.
- [ ] Canonical four-beat image transfer.
- [ ] Integer accumulation.
- [ ] Shared exact FP32 arithmetic.
- [ ] FP16/BF16 TACC accumulation.
- [ ] Differential SoC closure.
- [ ] Approved synthesis, routed timing, and resource acceptance.
