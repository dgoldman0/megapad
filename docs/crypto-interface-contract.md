# Crypto interface contract

Status: implemented and qualified through checkpoint 4. KDOS GPT uses checked
reflected hardware CRC, authoritative diagnostics are live, and the checked-in
backends advertise `CRYPTO_CAPS = 0xF`; source presence alone remains
insufficient support evidence. Fresh final artifacts reproduced exactly, the
ordered focused matrix and full serial RTL sweep passed, and the approved
Python regression completed with 3,425 passed and three conditional
live-network skips. Akashic refactoring is a separate task that requires a
user-selected worktree.

This document is the numeric source of truth for MegaPad's reflected CRC,
SHA3/SHAKE, raw Keccak-f[1600], and WOTS chain work. It defines both the
landed checkpoint-2 interface and the selected checkpoint-3 production WOTS
interface. The implementation ledger near the end records completed cutovers
and the remaining adoption/qualification boundary; it is not an alternate
contract.

The keywords **must**, **must not**, **shall**, **shall not**, and **may** are
normative. Numeric values are hexadecimal unless a table says otherwise.

## Scope and release boundary

The selected work has four independently advertised parts:

1. reflected CRC modes and atomic raw finalization;
2. reconciled SHA3/SHAKE streaming;
3. caller-owned raw Keccak-f[1600] state; and
4. a production WOTS chain sequencer using the same Keccak round service.

The ordinary SHA3/SHAKE, raw Keccak, and WOTS paths share one physical Keccak
round service. They do not imply multiple hardware contexts. Software owns
caller contexts and serializes complete transactions with the guard defined
below.

The portable SoC currently treats all MMIO-capable code as trusted and
cooperative. MMIO is exempt from MPU checks and the peripheral bus carries no
enforced privilege or protection-domain sideband. The owner fields and crypto
guard prevent accidental protocol overlap; they are not a security isolation
boundary. If untrusted code can issue MMIO, raw-state access and WOTS DMA must
not be enabled until requester privilege and DMA protection-domain checks are
implemented.

## Common numeric assignments

### Checked BIOS status values

Every status-bearing checked CRC, SHA3/SHAKE, raw-Keccak, and WOTS word in this
document uses the following status namespace. The adjacent SHA-256 HMAC/HKDF
wrappers retain the checked SHA-2 namespace documented in the KDOS reference
(`0=OK`, `1=STATE`, `2=RANGE`, `3=CONTEXT-ALIAS`, and
`4=LENGTH-OVERFLOW`). `CRC-FINAL@` is the sole result-only operation among the
checked transaction words. `CRYPTO-CAPS@` is a raw discovery query, not a
checked operation:

| Value | Name | Meaning |
|---:|---|---|
| 0 | `OK` | The complete operation succeeded. |
| 1 | `UNSUPPORTED` | The required capability bit is clear. |
| 2 | `STATE/OWNER` | The caller does not own the transaction, the resource is owned, or the operation is illegal in the current phase. |
| 3 | `RANGE` | A scalar, mode, length, address arithmetic result, or required memory domain is outside the contract. |
| 4 | `PROTECTED` | The complete caller span is mapped but is not permitted for the requested access. |
| 5 | `TIMEOUT` | Bounded hardware progress did not complete. |
| 6 | `HARDWARE/PROTOCOL` | The device reported an internal, bus, DMA, or protocol failure not represented above. |

For each checked BIOS transaction word, capability and complete argument-span
checks occur before engine state or that word's caller destination is mutated.
The explicit exception is an unsupported
`CRC-RAW-FINAL@` issued by the owner of a non-reflected transaction: it uses
ordinary finalization only to release that already-owned engine, as specified
below. A destination-returning BIOS word stages its whole result in BIOS-owned
scratch, quiesces or clears the hardware as required, then publishes the
caller destination while it still owns the crypto guard. On every nonzero
return from that word, its destination remains unchanged. KDOS composites that
request several such chunks have the explicitly streaming publication rule
defined under the guard allocation below.

### Capability discovery

System Info contains a read-only `CRYPTO_CAPS` qword at exact offset `+0x60`
and a read-only `NUM_BUS_PORTS` qword at exact offset `+0x68`. The System Info
device therefore occupies the half-open byte range `[+0x00,+0x70)`, exactly
112 bytes. Existing registers through `+0x5F` do not move.

| Bit | Name | Meaning when set |
|---:|---|---|
| 0 | `CRC_REFLECT_RAW` | CRC modes 4, 5, and 6 plus `CRC.FINRAW` are implemented. |
| 1 | `SHA3_STREAM` | SHA3/SHAKE commands, phase reporting, backpressure, abort, and 64-byte windows follow this contract. |
| 2 | `KECCAK_F1600` | The indexed 200-byte state and raw permutation command are implemented. |
| 3 | `WOTS_CHAIN` | The 64-bit context address, real Bank 0 DMA, checked state machine, and shared-core path are implemented. |
| 4..63 | reserved | Read as zero. |

`NUM_BUS_PORTS` reports the exact number of requester ports participating in
the main weighted bus arbiter, including full-core, microcluster, NIC, disk,
and WOTS ports that exist in the executing configuration. It is at least one.
The WOTS port is included exactly when it exists, whether or not software has
started a request. This field makes the synthesized topology available to the
single portable BIOS image; firmware does not embed a requester-count build
constant or infer the count from core topology.

The device extension through `+0x6F` must exist in every backend even when all
capability bits are zero. Capability bits are driven independently and must
remain clear in a backend until the complete associated contract and checked
BIOS path qualify there. Probe reads of optional MMIO devices are not a
fallback.

Byte reads within the System Info window return the corresponding
little-endian byte. Naturally aligned halfword, word, and qword reads are
permitted only when their complete span remains inside the exact device
window. A misaligned, crossing, or out-of-window access faults as one
architectural access; it must not alias an earlier register. Writes to
`CRYPTO_CAPS` or `NUM_BUS_PORTS` are acknowledged and ignored.

The exact System Info extension is implemented in the execution models and
integrated RTL. The checked-in checkpoint-3 configuration advertises bits 0
through 3 (`0xF`) for reflected/raw CRC, SHA3/SHAKE, raw Keccak, and the
production WOTS path. Bit 3 was held at zero until the real
DMA/shared-core/controller path and checked BIOS word passed the required
cross-backend qualification. Source presence alone never sets a capability.

## Portable crypto guard

### Allocation

The portable BIOS guard is hardware spinlock 8. KDOS reserves hardware
spinlocks 9 through 11 for HMAC/HKDF, TLS workspace ownership, and short TLS
credential-registry/cancellation transitions:

| Lock | Purpose | Acquire | Release |
|---:|---|---:|---:|
| 8 | Checked BIOS crypto/device ownership | `SPINLOCK_BASE + 0x20`, absolute offset `+0x620` | `SPINLOCK_BASE + 0x21`, absolute offset `+0x621` |
| 9 | KDOS HMAC/HKDF shared scratch | `SPINLOCK_BASE + 0x24`, absolute offset `+0x624` | `SPINLOCK_BASE + 0x25`, absolute offset `+0x625` |
| 10 | KDOS TLS shared workspace | `SPINLOCK_BASE + 0x28`, absolute offset `+0x628` | `SPINLOCK_BASE + 0x29`, absolute offset `+0x629` |
| 11 | KDOS TLS credential registry/cancellation | `SPINLOCK_BASE + 0x2C`, absolute offset `+0x62C` | `SPINLOCK_BASE + 0x2D`, absolute offset `+0x62D` |

The spinlock bank is 16 locks in the documented 64-byte aperture. Locks 0
through 7 are already assigned by KDOS; lock 8 is named `CRYPTO-LOCK` and is
reserved for the BIOS contract, while lock 9 is named `HMAC-HKDF-LOCK` and is
reserved by KDOS. Lock 10 is named `TLS-OWNER-LOCK` and serializes KDOS TLS
transcript, certificate, record, plaintext, exporter, credential signing, and
related handshake scratch. Lock 11 is `TLS-CREDENTIAL-LOCK`; it protects only
short credential record, reference, operation-generation, and cancellation
transitions. The checked acquisition paths perform an atomic attempt, except
for the signer's terminal lock-11 arbitration after it already owns lock 10.
Busy maps to the family's checked state status; callers may yield and retry
outside the checked word.

The guard is held for these complete lifetimes:

- SHA3: acquisition in `SHA3-BEGIN`, after capability and mode validation,
  through fixed digest publication and cleanup, or through the final
  `SHA3-CLEAR` of a SHAKE transaction;
- raw Keccak: acquisition after complete state-span preflight, then state load
  through permutation, staged state readback, hardware clear, caller
  publication, and scratch wipe; and
- WOTS: acquisition after complete scalar and span preflight, then programming
  through terminal wait, staged result read, device `CLEAR`, caller
  publication, and scratch wipe.

The checked API manages this lock internally. `SHA3-BEGIN` acquires and retains
it; SHA3 continuations verify the retained owner; fixed finalization or
`SHA3-CLEAR` releases it. The one-shot raw Keccak and WOTS words acquire and
normally release it within the call; the explicitly defined failed-quiescence
paths retain it fail-closed. Callers must not wrap these words in generic
`LOCK`/`UNLOCK` words.

KDOS holds lock 9 across each complete public SHA3-HMAC, SHA256-HMAC, or HKDF
call, including every internal hash transaction and the final wipe of pads,
intermediate digests, normalized keys, pointer/length metadata, and HKDF
counters. SHA3-family entry points test the SHA3 capability before attempting
lock 9, so `UNSUPPORTED` has priority over wrapper contention. Inside these
wrappers, lock ordering is strictly lock 9 followed by the BIOS-managed lock 8.
HKDF invokes private no-lock HMAC cores and therefore never recursively
acquires lock 9.

Lock 9 uses the same architectural requester identity as the rest of the
spinlock bank and serializes scratch access across every advertised full and
microcore. The current KDOS wrappers contain no scheduler or yielding point;
that property, together with the no-lock internal cores, also prevents a
same-core reacquire (which the hardware lock itself treats as successful).
Adding a yield or preemptible callback inside this lifetime requires full-width
core/task owner bookkeeping equivalent to the BIOS lock-8 guard. Callers must
not acquire lock 9 themselves or call a public HMAC/HKDF wrapper while already
holding it. They also must not enter those wrappers while retaining lock 8 via
an active `SHA3-BEGIN`/SHAKE transaction; such a call is an API-order error even
though both guards are nonblocking and therefore cannot deadlock.

There is an open limitation in the cleanup-failure wording. KDOS retains lock
9 when its selected lower hash clear fails and describes that state as
fail-closed. The depthless hardware bank still reports a same-physical-core
reacquire as success, however, so the retained lock excludes other cores but
does not exclude a later task or wrapper on the retaining core; one later
release can free it. This record does not choose between adding exact software
owner bookkeeping to KDOS and changing the hardware lock contract. Until one
of those changes is qualified, callers must treat the retaining core as
unusable for further HMAC/HKDF work after that cleanup failure.

Public KDOS TLS operations that touch shared workspace acquire lock 10 first.
Recursive entry is tracked in software by the exact
`(COREID,TASK-ID)` and a depth count because hardware same-core reacquisition
does not supply recursion depth. An ordinary credential operation may briefly
take lock 11 under lock 10 but releases it before allocator or cryptographic
work. TLS may then call KDOS HMAC/HKDF and checked BIOS crypto only in the
strict order 10, optional 11 and release, then 9, then 8. Code holding either
lower crypto lock must not call upward into TLS.

`TLS-CREDENTIAL-SIGN-CANCEL` is deliberately outside that nesting: it takes
only lock 11, whose nonrecursive software owner records exact
`(COREID,TASK-ID)`, resolves the two-cell `(slot+1,generation)` authority without
touching the signer's shared metadata, and copies the active operation
generation into its cancellation field. It neither acquires lock 10 nor calls
an allocator or crypto word. This permits the metadata request to be issued
from another physical core while signing holds lock 10. Same-core cancellation
while lock 10 is active returns credential busy; depthless hardware same-core
reacquisition can neither steal nor release the registry lock. A qualified
four-core emulator journey has exercised peer-core cancellation during one
real full-batch credential signature and verified atomic output plus complete
owner and operation-metadata cleanup; this is not interrupt-handler or
physical-board evidence.

### Lower-owned TLS signing capability

The public server-signing boundary accepts a raw 32-byte little-endian P-256
scalar only during core-0 credential provisioning. It copies the scalar into
a 184-byte lower record only after the complete P-256 public point matches the
leaf certificate, then returns a two-cell opaque generational handle rather
than a pointer. The pool size is caller-selected and certificate-chain count
has no private cap. Credential storage is a leaf-first concatenation of
self-delimiting DER Certificate values, not TLS framing. Every entry has an
exact shallow Certificate envelope and only the leaf is deeply parsed. The
synthesized wire sum `sum(DER length + 5)` must fit `0xFFFFFB`, the TLS uint24
Certificate-body remainder after its empty request context and list length.
This is KDOS memory ownership against accidental exposure, not an HSM or a
privileged-code protection boundary.

Opaque signing uses deterministic ECDSA-P256-SHA256, lower DER staging, the
actual encoded caller capacity, and both the signer's complete-batch cancel
sample and lock-11 late-publication arbitration. Delete preserves only stale
generation metadata while wiping the record and the complete allocated DER
chain before free. Provision, sign, cancel, delete, alias,
capacity, lower-crypto, and unexpected-throw paths are defined to publish no
partial authority or signature and to wipe their owned private staging before
release.

The KDOS multi-window SHAKE wrappers and multi-block HKDF expansion preflight
their complete caller output spans, then publish successful checked chunks in
order. A later hardware/hash failure returns the first nonzero status but does
not roll back a prefix already published by an earlier successful chunk. Each
individual BIOS `SHAKE-READ` (0 through 32 bytes), fixed digest, raw state, and
HMAC output remains all-or-nothing. This streaming rule avoids an unrelated
global output staging capacity; HKDF's only output limit remains the RFC 5869
limit of 255 hash blocks.

The named HMAC/HKDF pads, intermediate hashes, normalized-key buffers,
counters, and metadata in the KDOS dictionary are reserved implementation
storage. Caller key, message, info, PRK, and output spans must not alias that
storage. The present cooperative MMIO boundary does not enforce this ABI rule
as a protection domain.

HKDF expansion additionally rejects an output span that overlaps its caller
PRK or nonempty info span. Expansion rereads both inputs for every block, so
permitting such an alias would make earlier output publication mutate the input
to a later block. The SHA3 family reports `RANGE`; the SHA-256 family reports
its own `SHA256-RANGE` value.

### Requester identity

Lock correctness requires the architectural global core ID of the CPU that
issued the MMIO transaction. The RTL bus must capture requester-valid and
requester-ID metadata with the winning request and preserve it through the
response:

- a full-core port is valid and supplies that full core's global ID;
- a cluster port is valid and supplies `CLUSTER_ID_BASE +` the latched winning
  microcore index;
- cluster-internal SHA memory traffic and all DMA ports are requester-invalid;
  and
- invalid or out-of-range requesters receive an acknowledged, non-mutating
  spinlock response, with acquire reporting busy.

The spinlock owner-valid bits and owner-ID storage must cover exactly the
total global core count advertised by System Info `NUM_CORES`. This capacity
is independent of the mailbox/IPI full-core count: the lock implementation
must size its owner storage and requester comparison for all advertised full
and microcores. A requester is in range exactly when
`requester_id < NUM_CORES`.

A main-bus port number is insufficient because every microcore in a cluster
shares one port. The integrated RTL now carries the latched global requester
identity through the main bus and into the spinlock block; the former
core-zero tie-off is gone. TACC is not an alternative: its ownership domain
is per full core or per cluster rather than machine-wide, and MegaPad has no
qualified general CAS or load-linked/store-conditional primitive.

### Same-core task ownership

The existing spinlock block treats an acquire by its owning physical core as
successful and does not count recursion. BIOS therefore keeps two shared
full-cell owner fields in Bank 0:

```text
CRYPTO-OWNER-CORE = UINT64_MAX          unowned
CRYPTO-OWNER-CORE = full COREID         owned
CRYPTO-OWNER-TASK = full TASK-ID        meaningful while owned
```

No core or task identifier is truncated. After acquiring lock 8, a checked
entry point verifies that `CRYPTO-OWNER-CORE` is `UINT64_MAX`, writes the full
task ID, then publishes the full core ID. A published owner after a reentrant
hardware acquire returns `STATE/OWNER` and must not release the outer owner's
lock.

The acquire/check/publish sequence is one BIOS critical section: it saves the
calling core's interrupt-enable state, disables interrupts/preemption, makes
no scheduler or yielding call, and restores the saved interrupt state only
after the owner is published or the failed acquisition is resolved. This is
the required same-core exclusion mechanism, not an advisory “no scheduling
point” convention.

Every continuation compares both full owner fields before touching the
device. Cleanup quiesces and scrubs the device, then enters the same critical
section, unpublishes the owner by writing `UINT64_MAX` to
`CRYPTO-OWNER-CORE`, clears `CRYPTO-OWNER-TASK`, releases lock 8, and restores
the saved interrupt state. Reset frees the lock, sets the owner-core sentinel,
and zeroes the task field. An unexpected owner crash requires machine
recovery; the guard is a correctness mechanism for cooperative code, not a
lease.

## CRC ISA contract

### Encoding and modes

CRC operations use `EXT.CRYPTO` prefix `FB`. The selected sub-operations are:

| Sub-op | Encoding | Bytes | Operation |
|---:|---|---:|---|
| `0x00` | `FB 00` | 2 | `CRC.INIT`: load the selected mode's all-ones initial value. |
| `0x01` | `FB 01 DR` | 3 | `CRC.B Rd,Rs`: feed `Rs[7:0]`; publish the running accumulator to `Rd`. |
| `0x02` | `FB 02 DR` | 3 | `CRC.Q Rd,Rs`: feed the eight bytes of `Rs` least-significant byte first; publish the running accumulator to `Rd`. |
| `0x03` | `FB 03 DR` | 3 | `CRC.FIN Rd,Rs`: apply XOR-out, publish to `CRC_ACC` and `Rd`, and release shared ownership atomically. `Rs` is ignored. |
| `0x04` | `FB 04 imm8` | 3 | `CRC.MODE imm8`: select a complete mode value. |
| `0x05` | `FB 05 DR` | 3 | `CRC.SEED Rd,Rs`: store the mode-width value from `Rs` and publish it to `Rd`. |
| `0x06` | `FB 06 DR` | 3 | `CRC.FINRAW Rd,Rs`: publish the unmodified accumulator and release shared ownership atomically. `Rs` is ignored. |
| `0x07..0x0F` | `FB op` | 2 | Reserved; trap `ILLEGAL_OP`. |

REX extends register operands in the same way as the existing register-form
CRC operations. Instruction-length, skip, trap-PC, rewind, and native batch
classification must all treat sub-operation `0x06` as a register form, not a
bare reserved operation.

`CRC.MODE` accepts exactly the six complete values in the following table.
Every other complete immediate and every other full-core 64-bit `CRC_MODE`
CSR write canonicalizes to mode 0. Microcore writes to the CRC CSRs remain
ignored.

| Mode | Direction | Width | Polynomial used by recurrence | Init | XOR-out | `123456789` |
|---:|---|---:|---:|---:|---:|---:|
| 0 | MSB first | 32 | `0x04C11DB7` | `0xFFFFFFFF` | `0xFFFFFFFF` | `0xFC891918` |
| 1 | MSB first | 32 | `0x1EDC6F41` | `0xFFFFFFFF` | `0xFFFFFFFF` | `0x05440F15` |
| 2 | MSB first | 64 | `0x42F0E1EBA9EA3693` | `0xFFFFFFFFFFFFFFFF` | `0xFFFFFFFFFFFFFFFF` | `0x62EC59E3F1A4F00A` |
| 4 | LSB first | 32 | `0xEDB88320` | `0xFFFFFFFF` | `0xFFFFFFFF` | `0xCBF43926` |
| 5 | LSB first | 32 | `0x82F63B78` | `0xFFFFFFFF` | `0xFFFFFFFF` | `0xE3069283` |
| 6 | LSB first | 64 | `0xC96C5795D7870F42` | `0xFFFFFFFFFFFFFFFF` | `0xFFFFFFFFFFFFFFFF` | `0x995DC9BBDF1939FA` |

Bit 2 is the processing direction and bits 1:0 retain the existing tuple
selection. Values 3 and 7 are not modes. No arbitrary-polynomial register is
part of this contract.

For 32-bit modes, every result stored in `CRC_ACC` or a general register by
`INIT`, `B`, `Q`, `SEED`, `FIN`, or `FINRAW` is zero-extended to 64 bits. In
particular, high bits injected through the full-core `CRC_ACC` CSR cannot
survive a subsequent mode-width operation. Modes 2 and 6 retain all 64 bits.

`CRC.FIN` XORs the accumulator in place and is not idempotent; applying it
twice toggles the XOR-out twice. `CRC.FINRAW` applies neither XOR-out nor a bit
reversal and leaves the published raw value in `CRC_ACC`.

### CRC ownership

Full cores retain private CRC state. A microcluster has one shared CRC state
and transaction owner. `MODE`, `INIT`, and `SEED` acquire or retain that
owner; `FIN` and `FINRAW` release it in the same grant that publishes the
result. A nonowner stalls and retries while another microcore owns the engine.

The low-level ISA also permits `B`, `Q`, `FIN`, and `FINRAW` when the cluster
engine is unowned. Such a one-operation grant does not make `B` or `Q` a
retained transaction, and an unowned final has no owner to release. Checked
software must begin with `CRC-MODE!` before feeding or finalizing. Traps and
Forth `THROW` do not unwind the hardware owner; checked failure cleanup must
finalize through the owning context. An individual micro-core reset is an
explicit hardware cancellation: it suppresses completion and writeback for an
admitted operation, preserves state committed by earlier operations, and
releases the transaction lock if the reset caller owned it.

### Checked CRC words

The checked BIOS surface is:

| Word | Stack effect | Contract |
|---|---|---|
| `CRC-MODE!` | `( mode -- status )` | Validate and select one of the six complete modes, beginning a checked transaction without changing `CRC_ACC`. |
| `CRC-RESET` | `( -- status )` | Verify the checked owner and load the selected mode's all-ones initial value with `CRC.INIT`. |
| `CRC-INIT!` | `( seed -- status )` | Verify the checked owner and load the mode-width seed with `CRC.SEED`. |
| `CRC-FEED` | `( cell -- status )` | Verify the checked owner and feed eight bytes least-significant byte first with `CRC.Q`. |
| `CRC-FEED-BYTE` | `( byte -- status )` | Verify the checked owner and feed exactly the low byte with `CRC.B`. |
| `CRC@` | `( -- raw status )` | Return the zero-extended running accumulator only to the checked owner; status is on top. |
| `CRC-RAW-FINAL@` | `( -- raw status )` | Atomically publish the raw accumulator and end the checked transaction; status is on top. |
| `CRC-FINAL@` | `( -- finalized )` | Atomically XOR-finalize and end a transaction begun by a successful `CRC-MODE!`. |

`CRC-MODE!` performs no CRC instruction for an invalid argument, a missing
reflected/raw capability, or a conflicting BIOS transaction. Its exact
returns are:

| Condition | Status |
|---|---:|
| Mode 0, 1, or 2 selected | 0 |
| Mode 4, 5, or 6 selected and `CRC_REFLECT_RAW` is set | 0 |
| Mode 4, 5, or 6 with `CRC_REFLECT_RAW` clear | 1 |
| The current global core already has a checked CRC transaction owned by any task | 2 |
| Any other complete cell value | 3 |

This table is ordered: recognize the six modes, apply the reflected/raw
capability check to modes 4 through 6, reject every other cell with status 3,
then check for an existing owner before issuing `CRC.MODE`. Thus an unsupported
reflected mode returns 1 and an invalid cell returns 3 even if a transaction is
already active; a supported valid mode reaches the owner check and can return
2.

`CRC-MODE!` preserves the accumulator exactly, matching `CRC.MODE`; selecting
a tuple is not an initialization operation. A caller must successfully follow
it with `CRC-RESET` or `CRC-INIT!` before a feed when it needs a defined
starting value.

BIOS records the full owning `(COREID,TASK-ID)` in a per-global-core table
after `CRC.MODE` retires. Each record uses the same full-cell core sentinel and
full task field defined for the portable guard. The table is sized from
advertised `NUM_CORES` rather than imposing a caller limit. The owner check,
`CRC.MODE` instruction, and owner-record publication execute in one
saved-interrupt-state BIOS critical section with interrupts/preemption
disabled and no scheduler call.
The same rule covers each continuation's owner check and CRC instruction, and
the final instruction plus owner-record clear. This prevents same-core
interrupt or task re-entry between the software check and the architectural
operation.

Because a different microcore in the same cluster can already be stalled
inside `CRC.MODE`, status 2 is a checked-software observation for same-core
task misuse, not a promise that every hardware contention case is
nonblocking. `CRC-RESET`, `CRC-INIT!`, `CRC-FEED`, and `CRC-FEED-BYTE` return
0 after the exact owner instruction retires; a wrong or absent full owner
record returns 2 without issuing a CRC instruction. `CRC@` follows the same
owner rule, returning raw zero and status 2 on misuse and the zero-extended
mode-width accumulator followed by status 0 on success.

On `CRC-RAW-FINAL@`, a clear capability returns raw zero and status 1; if the
calling context owns an existing non-reflected transaction, BIOS executes
ordinary `CRC.FIN` solely to release it, clears the owner record, and does not
publish that finalized value. A wrong or absent owner returns raw zero and
status 2 without issuing a CRC instruction. Success returns the zero-extended
mode-width raw value followed by status 0.

`CRC-FINAL@` retains its result-only public shape. After a successful
`CRC-MODE!` it executes `CRC.FIN`, clears the owner record, and returns the
finalized value. Misuse without the matching owner returns zero and does not
touch hardware. The misleading `CRC-POLY!` public name and the racy no-result
`CRC-FINAL` word are removed. No aliases are retained.

## SHA3/SHAKE and raw Keccak MMIO contract

### Register map and access widths

The SHA3 block occupies offsets `+0x780..+0x7DF`, exactly 96 bytes. Register
offsets below are relative to `SHA3_BASE = +0x780`.

| Offset | Register | Access | Width | Meaning |
|---:|---|---|---|---|
| `+0x00` | `CMD` | W | byte | Command value. Reads return zero. |
| `+0x01` | `STATUS` | R | byte | Packed owner and phase. Writes are invalid accesses. |
| `+0x02` | `CTRL` | RW | byte | Hash mode 0 through 3. |
| `+0x03` | `ERROR` | R | byte | Stable error code. Writes are invalid accesses. |
| `+0x08` | `DIN` | W | byte | One streaming input byte. Reads return zero. |
| `+0x10..+0x4F` | `DOUT` | R | byte or aligned qword | Stable 64-byte output window. |
| `+0x50` | `STATE_INDEX` | RW | byte | Raw lane selector, 0 through 24. |
| `+0x58..+0x5F` | `STATE_DATA` | RW | byte or aligned qword at `+0x58` | Selected raw 64-bit lane, little endian. |

Offsets `+0x04..+0x07` and `+0x51..+0x57` are reserved. A reserved address,
wrong-direction write, forbidden width, misalignment, or access whose complete
span crosses a register or the device window is one architectural bus fault.
The front end must preflight the whole access before any byte callback or
device mutation. Halfword and word accesses are not part of the SHA3
interface. Qword reads from `DOUT` start at `+0x10`, `+0x18`, ..., `+0x48` and
return eight consecutive little-endian bytes.

`STATUS`, `ERROR`, and `CTRL` reads remain acknowledged while the round
service is busy or WOTS owns it. An invalid access is distinct from a
well-formed command rejected by the device; bus-faulting accesses do not
change `ERROR`, phase, owner, state, or output.

`SHA3_STREAM`, `KECCAK_F1600`, and `WOTS_CHAIN` are independently meaningful.
In any backend that sets at least one of those bits, `STATUS` and `ERROR`
follow this contract, command 7 follows this contract, and the complete
96-byte window is decoded. Status and error remain responsive during WOTS
work:

- `SHA3_STREAM` additionally guarantees `CTRL`, `DIN`, `DOUT`, and commands
  1, 3, and 4;
- `KECCAK_F1600` additionally guarantees `STATE_INDEX`, `STATE_DATA`, and
  command 6, including raw-owner status, error reporting, abort, and wipe; and
- `WOTS_CHAIN` independently guarantees status `0x0D` while its sequencer owns
  the shared service, followed by `0x00` after its ordered cleanup; and
- when either `SHA3_STREAM` or `KECCAK_F1600` is clear, reads from that
  feature's data/control registers return zero without mutation, while its
  otherwise well-formed commands or writes are rejected with error 6. A
  `BUSY`/WOTS preservation case has first priority, a cross-owner sponge/raw
  conflict has second priority and records error 2 when nonbusy, and feature
  unavailability has third priority and records error 6 only when no such
  owner conflict exists.

Thus a backend may expose raw Keccak without hash streaming, hash streaming
without raw state, or WOTS without either public MMIO data path. It may not set
a bit while borrowing incomplete status, clear, ownership, or zeroization
behavior from another feature. When all three bits are clear, the selected
SHA MMIO behavior is unadvertised and checked BIOS does not access it.

### Hash modes

| `CTRL` | Construction | Rate bytes | Fixed output |
|---:|---|---:|---:|
| 0 | SHA3-256 | 136 | 32 bytes |
| 1 | SHA3-512 | 72 | 64 bytes |
| 2 | SHAKE128 | 168 | extendable |
| 3 | SHAKE256 | 136 | extendable |

A `CTRL` write is legal only at owner none, phase `IDLE`. The complete byte
must be 0 through 3; no masking is permitted. An invalid value leaves `CTRL`
unchanged and enters owner-none `ERROR` with error code 3. A valid write does
not claim the round service.

### Commands

| Value | Name | Contract |
|---:|---|---|
| 0 | reserved | Reject as invalid command. |
| 1 | `INIT` | Zero the complete sponge state and transient/output storage, reset stream cursors, and claim owner `MMIO sponge`. |
| 2 | reserved | Reject as invalid command. Full-rate `DIN` absorption is automatic. |
| 3 | `FINAL` | Apply construction padding, permute, and publish the first output window. |
| 4 | `NEXT` | Publish the next sequential 64-byte SHAKE window, permuting once if the window crosses a rate boundary. |
| 5 | reserved | Reject as invalid command. There is no architectural 32-byte sliding-window command. |
| 6 | `KECCAK_F1600` | Execute exactly one raw 24-round Keccak-f[1600] permutation. |
| 7 | `CLEAR` | Abort or acknowledge the MMIO transaction, wipe transient and visible state, and release the MMIO owner. |
| `8..255` | reserved | Reject as invalid command. The complete byte is decoded; low-bit aliases are forbidden. |

Every command other than `CLEAR` is rejected while an MMIO operation is
`BUSY`. `CLEAR` is accepted in any owner-none, MMIO-sponge, or MMIO-raw phase,
including `BUSY` and `ERROR`. It is not accepted while WOTS owns the round
service.

### Phase, owner, and error encodings

`STATUS[1:0]` is the complete phase:

| Value | Phase |
|---:|---|
| 0 | `IDLE` |
| 1 | `BUSY` |
| 2 | `DONE` |
| 3 | `ERROR` |

`STATUS[3:2]` is the owner class:

| Value | Owner |
|---:|---|
| 0 | none |
| 1 | MMIO sponge |
| 2 | MMIO raw state |
| 3 | WOTS |

`STATUS[7:4]` reads as zero. The reachable packed values are:

| Status | Meaning |
|---:|---|
| `0x00` | none / `IDLE` |
| `0x03` | none / `ERROR` |
| `0x04` | MMIO sponge / `IDLE` |
| `0x05` | MMIO sponge / `BUSY` |
| `0x06` | MMIO sponge / `DONE` |
| `0x07` | MMIO sponge / `ERROR` |
| `0x08` | MMIO raw / `IDLE` |
| `0x09` | MMIO raw / `BUSY` |
| `0x0A` | MMIO raw / `DONE` |
| `0x0B` | MMIO raw / `ERROR` |
| `0x0D` | WOTS / `BUSY` |

WOTS never publishes a shared-core `DONE` or `ERROR` state: it scrubs and
releases the core before publishing its own terminal status. A zero-step WOTS
request never claims the core, so SHA3 status remains owner none.

The `ERROR` register uses:

| Value | Meaning |
|---:|---|
| 0 | none |
| 1 | invalid or reserved command |
| 2 | owner, busy, or phase conflict |
| 3 | invalid hash mode or `CTRL` write |
| 4 | a `STATE_INDEX` write greater than 24 |
| 5 | internal round-service failure or internal timeout |
| 6 | the independently advertised SHA-stream or raw-state feature is unavailable |
| `7..255` | reserved |

An accepted `INIT`, raw transaction start, or `CLEAR` clears an old error.
Protocol errors encountered while idle or at an MMIO-owned terminal phase
enter the corresponding `ERROR` phase and retain the MMIO owner until
`CLEAR`. An internal failure in an MMIO sponge or raw operation suppresses
output, wipes and releases the round service, then publishes owner-none
`ERROR` with code 5. A WOTS-owned failure instead wipes and releases the
shared service to `0x00` and publishes only the WOTS-local terminal error.

A well-formed command or state request presented while an operation is
already `BUSY`, or while WOTS owns the core, must not alter the active
operation's phase, owner, `ERROR`, state, cursor, or output. The rejecting
caller observes `BUSY` or WOTS ownership in unchanged `STATUS`. This rule
prevents one caller from making the legitimate owner observe a false terminal
failure. The sole backpressure exception is a `DIN` byte presented while an
automatic full-rate permutation is active: that access is held and accepted
after the permutation. `DIN` presented during `FINAL`, `NEXT`, raw work, or
WOTS work is rejected without mutation.

### Legal transitions

The following table is exhaustive for state-changing, well-formed accesses
after the feature-availability rule above. `Reject` means no datapath or
cursor mutation. Unless the preceding busy/WOTS preservation rule applies,
rejection records error 2 and enters the current MMIO owner's `ERROR` phase.

| Access | Legal starting state | Result |
|---|---|---|
| Valid `CTRL` write | none / `IDLE` | Mode changes; remains `0x00`. |
| Invalid `CTRL` write | none / `IDLE` | Mode unchanged; status `0x03`, error 3. |
| Recognized command or write for a clear capability bit | any nonbusy phase | Reject; data and cursor unchanged; error 6. |
| `INIT` | none / `IDLE` | Complete wipe; status `0x04`, error 0. |
| `DIN` not filling rate | sponge / `IDLE` | Byte accepted; remains `0x04`. |
| `DIN` filling rate | sponge / `IDLE` | Byte accepted atomically, then `0x05`; automatic permutation returns to `0x04`. |
| `FINAL` | sponge / `IDLE` | `0x05`, then `0x06` with stable output. |
| `NEXT` | sponge / `DONE`, SHAKE mode | `0x05`, then `0x06` with next 64 bytes. |
| `NEXT` | any fixed-output mode or non-`DONE` phase | Reject; error 3 for fixed mode, otherwise error 2. |
| `DOUT` read | sponge / `DONE` | Return the requested byte/qword; no state change. |
| Valid `STATE_INDEX` write | none / `IDLE`, raw / `IDLE`, or raw / `DONE` | Index changes; owner and phase otherwise unchanged. It does not claim raw ownership. |
| Valid `STATE_DATA` write | none / `IDLE` | Atomically claims raw ownership, writes selected lane byte/qword, status `0x08`. |
| Valid `STATE_DATA` write | raw / `IDLE` | Selected lane byte/qword changes; remains `0x08`. |
| `STATE_DATA` read | raw / `IDLE` or raw / `DONE` | Selected lane byte/qword returned; no state change. |
| Invalid state index | none / `IDLE`, raw / `IDLE`, or raw / `DONE` | Raw state unchanged; enter the corresponding `ERROR`, error 4. |
| `KECCAK_F1600` | none / `IDLE` | Atomically claim raw ownership of the zeroized state, `0x09`, then `0x0A`. |
| `KECCAK_F1600` | raw / `IDLE` | `0x09`, then `0x0A`. |
| `KECCAK_F1600` | raw / `DONE` | `0x09`, then `0x0A`; exactly one additional permutation. |
| `CLEAR` | none, sponge, or raw; any phase | Abort if needed, wipe, then status `0x00`, error 0. |
| Any raw-state access | sponge owner | Reject as owner conflict. |
| Any sponge mutation | raw owner | Reject as owner conflict. |
| Any command/state mutation | WOTS owner | Preserve WOTS operation; status remains `0x0D`. |

`STATE_INDEX` reads are legal wherever its writes are legal and return the
complete selected index. A `STATE_INDEX`, `DOUT`, or `STATE_DATA` read while
the service is `BUSY` or WOTS-owned returns zero and preserves the active
operation. In another illegal nonbusy phase, each returns zero and records
error 2. A `STATE_DATA` read while owner none is therefore an owner conflict
rather than an implicit claim. Writing raw state after a raw `DONE` result
first requires `CLEAR`; this keeps the readback phase stable. Repeated raw
permutations are permitted by issuing command 6 again from raw `DONE`.

### Streaming and output ordering

`DIN` XORs bytes into ascending Keccak state bytes. The byte that fills the
selected rate is accepted and starts an automatic permutation. A following
`DIN` access is held by MMIO backpressure and accepted at byte zero only after
that permutation completes; it is never acknowledged and discarded. The
maximum hold for one automatic permutation is 32 core clocks, below the main
bus's 6-bit terminal count 63 and 64-clock response deadline. An
implementation that cannot meet that bound must return for contract review
rather than reintroduce command 2.

After an exact-rate message, automatic permutation leaves the input cursor at
zero. `FINAL` therefore pads a new empty rate block. It never reabsorbs the
completed block or writes the delimiter outside the rate.

SHA3 uses delimiter `0x06`; SHAKE uses delimiter `0x1F`. The final rate byte
has bit `0x80` XORed into it. Lanes are little endian as defined below.

Fixed-output `FINAL` places the complete 32- or 64-byte digest at the start of
`DOUT`; bytes beyond a 32-byte SHA3-256 digest read as zero. SHAKE `FINAL`
publishes output bytes 0 through 63. Each successful `NEXT` publishes the next
64 sequential bytes. Since every selected rate is at least 72 bytes, one
64-byte window crosses at most one rate boundary. There are no gaps, repeated
bytes, or architectural 32-byte command steps.

`DOUT` changes as one completed window and remains stable throughout `DONE`.
No byte of a new window becomes visible during `BUSY`.

An accepted `CLEAR` during `BUSY` takes effect at the next Keccak round
boundary, suppresses late completion and writeback, and remains `BUSY` until
the wipe is complete. It wipes all 25 lanes, input/rate storage, `DOUT`,
digest/squeeze staging, cursors, state index, error, and mode-derived transient
state, then releases the MMIO owner and reaches `0x00`. Repeated `CLEAR` at
`0x00` is successful and has no additional effect. `CTRL` retains its selected
mode across `CLEAR`; `INIT` uses the currently selected mode.

One accepted permutation, padding/finalization, or output-window command must
leave `BUSY` within `SHA_COMMAND_CYCLES = 32` core clocks. An accepted
`CLEAR`, including cancellation at a round boundary and wipe, must reach
`0x00` within `SHA_CLEAR_CYCLES = 64` core clocks. BIOS bounds a normal
completion wait to `SHA_STATUS_POLLS = 64` successfully acknowledged status
reads and a clear wait to `SHA_CLEAR_POLLS = 128` such reads. These constants
are architectural qualification limits, not emulator shortcuts. BIOS
classifies the byte returned by each read before deciding whether that poll
budget is exhausted: a terminal status returned by the 64th normal read or
128th clear read wins, while `BUSY` on that final permitted read expires the
wait. A successfully acknowledged read is one that returns the addressed
`STATUS` byte without an architectural bus fault. A faulting read is not
retried; it is common status 6 and takes the same cleanup path. Expiry of a
normal command is common status 5 and triggers `CLEAR`; expiry of the clear
wait is fail-closed as defined below.

### Raw state mapping

The raw state is exactly 25 64-bit lanes. Lane index is `x + 5*y`. Within a
lane, byte zero contains bits 7:0. A caller-owned 200-byte memory image maps:

```text
memory[8 * (x + 5*y) + b] = state[x + 5*y][8*b +: 8]
```

No lane, word, or byte reversal is applied. `STATE_INDEX` does not
auto-increment. A byte access to `STATE_DATA + b` transfers only byte `b` of
the selected lane. An aligned qword access at `STATE_DATA` transfers the
complete lane little endian. The front end must not expose backend callback
decomposition or partial qword acceptance.

Command 6 performs exactly the 24 Keccak-f[1600] rounds on the loaded lanes.
It does not absorb, pad, apply a domain separator, squeeze, or interpret
`CTRL`. On completion all 25 lanes remain readable until `CLEAR`.

### Checked SHA3, SHAKE, and Keccak words

| Word | Stack effect | Contract |
|---|---|---|
| `CRYPTO-CAPS@` | `( -- caps )` | Read the System Info capability qword. |
| `SHA3-BEGIN` | `( mode -- status )` | Check `SHA3_STREAM`, validate mode, acquire the guard, set `CTRL`, and issue `INIT`. |
| `SHA3-UPDATE` | `( src len -- status )` | Verify the owner and complete source span, then stream all bytes with boundary backpressure. |
| `SHA3-FINAL` | `( dst -- status )` | In fixed-output modes, qualify 32/64 destination bytes, finalize, stage the digest, clear hardware, publish, wipe scratch, and release. |
| `SHAKE-FINAL` | `( -- status )` | In SHAKE modes, finalize and set the BIOS logical output cursor to zero while retaining ownership. |
| `SHAKE-READ` | `( dst len -- status )` | Publish the next 0 through 32 sequential bytes after staging the complete requested chunk. |
| `SHA3-CLEAR` | `( -- status )` | Idempotently abort/clear an owned transaction and release the guard. |
| `KECCAK-F1600` | `( state-200 -- status )` | Permute one caller-owned 200-byte state in place through the guarded indexed window. |

Capability absence has priority over argument validation and returns status 1
without touching the guard or device. For one-shot raw Keccak and WOTS calls,
scalar and complete-span validation then precede guard acquisition and
hardware mutation. `SHA3-BEGIN` similarly validates its mode before acquiring
the guard; later SHA continuations necessarily validate their arguments while
the successful `BEGIN` guard remains held. `SHA3-BEGIN` accepts only modes 0
through 3; other values return status 3. A guard/owner-field conflict returns
status 2.

`SHA3-CLEAR` is part of the checked surface whenever
`(SHA3_STREAM | KECCAK_F1600) != 0`; it selects the capability corresponding
to the retained owner and is therefore also the recovery operation for a raw
Keccak failed-quiescence return. `WOTS_CHAIN` alone guarantees hardware
command 7 and responsive WOTS-owner reporting, but does not advertise the
checked `SHA3-CLEAR` word because no MMIO sponge/raw transaction can be
started in that capability combination.

After a successful `SHA3-BEGIN`, every continuation requires the exact guard
owner fields. Wrong owner or phase returns status 2 without device access. A
source or destination arithmetic/mapping error returns 3; a mapped but
forbidden span returns 4. Device error 2 maps to status 2, a bounded wait
expiry maps to 5, and unexpected device errors 1, 3, 4, 5, or 6 map to 6
because caller arguments were already preflighted. Device error values 7
through 255, an `ERROR` phase carrying code 0, any packed status outside the
reachable table, or an owner/phase transition impossible for the issued
operation also maps to 6 after the same cleanup attempt.

The continuation check order is capability, exact owner, logical mode/phase,
scalar values, complete spans, then device access. An earlier failure does not
inspect a later pointer. Once an active exact-owner continuation reaches a
mode, phase, scalar, or span failure, it performs the cleanup rule below
before returning the first status selected by that order when cleanup succeeds.
If cleanup itself cannot prove quiescence, its status takes precedence and the
guard remains held fail-closed.

The checked mode/phase rejections are exact:

| Call condition | Status and effect |
|---|---|
| `SHA3-FINAL` while the owned mode is SHAKE128 or SHAKE256 | 2; clear the active transaction before returning. |
| `SHAKE-FINAL` while the owned mode is SHA3-256 or SHA3-512 | 2; clear the active transaction before returning. |
| `SHAKE-READ` before a successful `SHAKE-FINAL` | 2; clear the active transaction before returning. |
| `SHA3-UPDATE` after successful `SHAKE-FINAL` | 2; clear the active transaction before returning. |
| `SHA3-UPDATE` after successful fixed `SHA3-FINAL` | 2; the prior final already released, so do not access the device. |

Repeated finalization and any other continuation in the wrong logical phase
use status 2. A scalar or span failure retains statuses 3 or 4 when the
already-active transaction is successfully cleared as part of failure cleanup.

`SHA3-UPDATE` requires a nonnegative length. A zero-length update still
verifies the exact owner and absorb phase but ignores `src`, performs no
memory access or `DIN` write, and returns 0. A nonzero source must be a
complete nonwrapping caller-readable span under the common span policy;
arithmetic, mapping, and protection failures return 3 or 4 as defined above.

Any failure after an engine has been claimed issues and awaits `CLEAR` and
wipes BIOS scratch. The owner fields and lock are released only after status
`0x00` proves the device quiescent and scrubbed. If the clear wait reaches
`SHA_CLEAR_POLLS`, the word returns status 5; a checked MMIO/protocol failure
during clear returns status 6. Either cleanup failure leaves caller output
unchanged and retains the full owner fields and lock 8; releasing them while
late hardware work can still publish is forbidden. The exact owner may retry
`SHA3-CLEAR`; machine reset is the recovery if quiescence cannot be
established. `SHA3-CLEAR` returns 0 when repeated after successful cleanup
while the hardware and guard are both unowned. A different context cannot
clear an active owner's transaction and receives 2.

`SHAKE-READ` accepts `len` 0 through 32. It tracks a BIOS logical cursor into
the current 64-byte hardware window and issues `NEXT` only when required.
It stages the whole requested chunk before publishing and advances the
logical cursor only on success. For nonzero `len`, `dst` must name a complete
nonwrapping caller-writable span; mapping and protection failures return 3 or
4. A value outside 0 through 32 returns 3. A zero-length read performs
owner/phase and length checks, performs no destination access, issues no
device command, and returns 0.

`KECCAK-F1600` checks `KECCAK_F1600`, qualifies the complete nonwrapping
200-byte readable/writable in-place span, acquires the guard, loads all lanes,
issues one command 6, stages all lanes only after `DONE`, clears hardware,
then copies the staged state to the caller and releases. The caller pointer is
never retained by hardware. Every failure leaves all 200 caller bytes
unchanged.

Checkpoint-2 caller source migration is complete. Its focused source-slice
checks and complete KDOS/TLS networking source-load qualification are green.
The public transaction words
`SHA3-MODE!`, `SHA3-INIT`, `SHA3-SQUEEZE`, `SHA3-SQUEEZE-NEXT`, and
`SHA3-DOUT@` are removed and are not kept as aliases in the unreleased
interface.

## WOTS chain contract

### Context and register map

The WOTS accelerator occupies `WOTS_BASE = +0x8A0` through `+0x8BF`, exactly
32 bytes. It reads one contiguous, caller-owned 64-byte context:

```text
context + 0x00   PK.seed       16 bytes
context + 0x10   ADRS          32 bytes
context + 0x30   chain input   16 bytes
total                          64 bytes
```

This is an exact algorithm record, not a maximum-capacity buffer. Hardware
does not retain three independent pointers and never writes the context.

| Offset | Register | Access | Contract |
|---:|---|---|---|
| `+0x00..+0x07` | `CONTEXT_ADDR` | RW | 64-bit little-endian physical address, programmed one byte at a time. |
| `+0x08` | `STEPS` | RW | Complete byte; valid request values are 0 through 15. |
| `+0x09` | `START` | RW | Complete byte; valid request values are 0 through 15. |
| `+0x0A` | `CMD/STATUS` | W/R | Command writes and terminal status reads. |
| `+0x0B` | `ERROR` | R | Stable terminal error code. |
| `+0x0C..+0x0F` | `CYCLES` | R | Saturating 32-bit little-endian service-cycle count. |
| `+0x10..+0x1F` | `DOUT` | R | Stable 16-byte result. |

Every WOTS register is architecturally byte-access-only. Any halfword, word,
qword, misaligned, crossing, reserved-address, or wrong-direction access is
one architectural bus fault. The SoC or emulator preflights the complete
access before asserting the device request or invoking a byte callback. The
fault does not mutate a programming byte, start a request, acknowledge a
terminal state, or change the WOTS error register.

Programming registers change only in `IDLE`. Reads return the currently
latched programming bytes. Writes attempted in `BUSY`, `DONE`, or `ERROR` are
acknowledged as state rejections and leave the complete active or terminal
request unchanged; software must issue `CLEAR` before reprogramming.

### Command, status, and error values

Command writes to `+0x0A` use the complete byte:

| Value | Command | Meaning |
|---:|---|---|
| 0 | `NOP` | No state change in any phase. |
| 1 | `GO` | Validate, snapshot, and begin the programmed request. |
| 2 | `CLEAR` | Acknowledge a terminal state or abort active work. |
| `3..255` | invalid | Invalid command. |

Status reads from the same offset use:

| Value | Status |
|---:|---|
| 0 | `IDLE` |
| 1 | `BUSY` |
| 2 | `DONE` |
| 3 | `ERROR` |

`ERROR` uses:

| Value | Meaning |
|---:|---|
| 0 | none |
| 1 | invalid command |
| 2 | Keccak owner busy or otherwise unavailable |
| 3 | `STEPS` is greater than 15 |
| 4 | `START` is greater than 15, or nonzero `START + STEPS` is greater than 15 |
| 5 | context span wraps, targets the wrong memory domain, or crosses Bank 0 |
| 6 | DMA target bus fault |
| 7 | main-bus memory timeout at the 8-bit watchdog's terminal count 255 (completion deadline 256 clocks after grant) |
| 8 | WOTS-local request-accept timeout |
| 9 | internal Keccak or controller protocol failure |
| `10..255` | reserved |

An invalid command written in `IDLE` zeroes `DOUT` and publishes status
`ERROR`, error 1, without DMA or Keccak ownership. In `BUSY`, only `NOP` and
`CLEAR` are accepted; every other command preserves the active request and
its error/output state. In `DONE` or `ERROR`, only `NOP` and `CLEAR` are
accepted; `GO` and invalid commands preserve the terminal result until it is
explicitly acknowledged.

### `GO` validation and transitions

A `GO` written in `IDLE` clears old `DOUT` and error, resets `CYCLES` to zero,
and snapshots all programming bytes. It then applies this ordered validation:

1. `STEPS <= 15`;
2. `START <= 15` and, when `STEPS` is nonzero, `START + STEPS <= 15` using
   widened arithmetic;
3. the complete context span is a nonwrapping Bank 0 span; and
4. for nonzero work, the shared Keccak round service can be claimed.

The first failing check determines error 3, 4, 5, or 2 respectively. Argument
and owner failures publish terminal `ERROR` with `DOUT` zero and no DMA beat.
An accepted request enters `BUSY` before issuing its first DMA request.

The context span is valid only when a widened calculation proves:

```text
0 <= CONTEXT_ADDR
CONTEXT_ADDR + 64 does not wrap 64-bit arithmetic
[CONTEXT_ADDR, CONTEXT_ADDR + 64) is wholly within [0, BANK0_SIZE)
```

MMIO, cluster scratchpad, HBW, external RAM, and VRAM are not accepted for
this contract. This Bank 0 rule is a common-backend memory-domain choice, not
32-bit address truncation.

For `STEPS = 0`, hardware still reads all 64 ascending context bytes and
publishes context bytes 48 through 63 unchanged. It never claims Keccak.
For nonzero work, Keccak ownership is claimed before the first context byte is
consumed and held across the complete chain.

Normal transitions are:

| Start | Event | Result |
|---|---|---|
| `IDLE` | Valid `GO` | `BUSY`, `CYCLES=0`, `DOUT=0`. |
| `BUSY` | Successful zero-step DMA and staging | Scrub request state, then `DONE` with the 16-byte identity result. |
| `BUSY` | Successful nonzero chain | Copy private result, scrub and release Keccak, then atomically publish `DOUT` and `DONE`. |
| `BUSY` | DMA/internal failure | Drain/cancel, scrub and release, then publish persistent `ERROR` and its code. |
| `BUSY` | `CLEAR` | Stop new work, drain/cancel and scrub, then `IDLE` with error/output cleared. |
| `DONE` or `ERROR` | `CLEAR` | Clear output/error/programming state and enter `IDLE`. |
| `IDLE` | `CLEAR` | Zero programming bytes, output, error, and private transients; retain `CYCLES`; remain `IDLE`. |

`CYCLES` increments once for each cycle spent in `BUSY` or abort-drain state
and saturates at `0xFFFFFFFF`. It includes zero-step DMA service and any
abort-drain delay. It remains readable across `CLEAR` as the count for the
last request, resets on the next `GO` accepted from `IDLE`, and resets to zero
on machine reset. It is diagnostic and cannot affect completion.

### DMA, abort, cleanup, and publication

WOTS is a real read-only main-bus requester with a 64-bit address. It is
appended after the existing disk requester so existing NIC and disk port
indices do not move. It participates in the same weighted round-robin policy
at weight 1 and with no bandwidth throttle. Software may not reduce that
weight or impose a bandwidth cap while `WOTS_CHAIN` is advertised; competing
ports may use any legal weight through 255. There is no out-of-band or claimed
"lowest-priority" path. Its requester metadata is invalid for MMIO spinlock
ownership.

Every successful, non-aborted request issues exactly 64 ascending byte reads
and never wraps an address modulo available memory. A request cancelled or
failed before completion may issue fewer reads and never issues another read
after its abort point.

The WOTS requester uses an explicit one-beat handshake:

- it asserts request-valid with one stable 64-bit byte address;
- the bus pulses request-accept exactly once when it captures that beat;
- before request-accept, `CLEAR` may withdraw request-valid with no response
  owed; request-accept wins over `CLEAR` or the local deadline on the same
  clock;
- after request-accept, WOTS has exactly one irrevocably outstanding beat,
  issues no next request, and waits for exactly one terminal response; and
- the bus pulses response-valid with data and this two-bit response code:

| Response code | Name | WOTS result |
|---:|---|---|
| 0 | `OK` | Consume the byte and advance. |
| 1 | `TARGET_FAULT` | Error 6. |
| 2 | `MEM_TIMEOUT` | Error 7. |
| 3 | reserved/protocol | Error 9. |

An accepted beat receives its terminal response no later than the main bus's
8-bit terminal-count deadline. Because WOTS permits only one accepted beat at
a time and does not reuse the interface until that response is consumed, no
transaction tag is required and a response cannot be attributed to a later
request. Common reset clears both sides of the handshake. Sentinel data and
the shared sticky `BUS_ERR` latch are diagnostic only; neither is sufficient
to classify a WOTS beat.

The bounded constants are:

```text
MEM_TIMEOUT_COUNTER_MAX = 255
MEM_RESPONSE_DEADLINE   = 256
BUS_BEAT_SLOT_CYCLES = 258
WOTS_DMA_ACCEPT_CYCLES =
    (N_BUS_PORTS - 1) * 255 * BUS_BEAT_SLOT_CYCLES + 1
WOTS_DMA_BEAT_CYCLES =
    WOTS_DMA_ACCEPT_CYCLES + MEM_RESPONSE_DEADLINE
KECCAK_SERVICE_CYCLES = 32
WOTS_CONTROL_CYCLES   = 512
WOTS_REQUEST_CYCLES(steps) =
    64 * WOTS_DMA_BEAT_CYCLES
    + steps * KECCAK_SERVICE_CYCLES
    + WOTS_CONTROL_CYCLES
WOTS_CLEAR_CYCLES = WOTS_DMA_BEAT_CYCLES
                    + KECCAK_SERVICE_CYCLES + 64
```

`MEM_RESPONSE_DEADLINE` is the elapsed accept-to-terminal-response bound; a
target response on that final cycle wins over timeout. The 258-clock beat slot
also covers the checked-in arbiter's post-completion `served_last` bubble
before another weighted beat is captured. `N_BUS_PORTS` is the synthesized
main-bus requester count, including WOTS itself, and must equal the read-only
System Info `NUM_BUS_PORTS` value. The WOTS controller uses its elaborated
count in the local deadline; BIOS reads `NUM_BUS_PORTS` and evaluates the same
formula at run time. RTL elaboration assertions and Python/native construction
checks require the advertised count to equal the arbiter's actual port count.
The other factor 255 is the maximum programmable weight of each competing
port; the bound therefore does not change if software adjusts QoS while a
request is active. If request-valid is not accepted by
`WOTS_DMA_ACCEPT_CYCLES`, WOTS withdraws the unaccepted beat and publishes
error 8 after cleanup; acceptance on that final cycle wins.
Once accepted, the bus's own terminal response and code resolve the beat.
Python and native execution model the same request-accept edge, arbitration,
outstanding beat, response code, and timeout levels; they must not read host
memory synchronously out of band.

Elaboration and backend construction require `N_BUS_PORTS >= 1`, derive
watchdog widths from these formulas, and prove the request and clear deadlines
are each below `2^63`. BIOS treats an advertised zero, formula overflow, or a
result at least `2^63` as hardware/protocol failure and does not touch WOTS.
Any mismatch between the advertised count and the actual backend topology is
a construction or qualification failure. No fixed requester-count assumption
or inference from `NUM_CORES` substitutes for the advertised field and those
checks.

If `CLEAR` arrives before request-accept, request-valid is withdrawn and no
response is owed. If a beat is already accepted, the controller enters
abort-drain, consumes its one terminal response, and only then scrubs or
returns `IDLE`. It does not issue another beat while draining. An accepted
WOTS operation must reach terminal `DONE`/`ERROR` within
`WOTS_REQUEST_CYCLES(STEPS)`; an accepted `CLEAR` must reach `IDLE` within
`WOTS_CLEAR_CYCLES`.

Keccak cancellation takes effect at a safe round boundary and suppresses any
late done or result publication. Success, failure, abort, and reset wipe the
staged seed, address, input node, constructed Keccak state, DMA address and
counter, step state, and mode transients before another owner can acquire the
round service. Terminal errors perform this ordered cleanup before publishing
`ERROR`. `DONE` retains only private `DOUT` and `CYCLES`; it does not retain
Keccak ownership.

Each nonzero chain step constructs one SHAKE256 rate block in zeroized Keccak
state:

- bytes 0 through 15 are `PK.seed`;
- bytes 16 through 47 are ADRS bytes 0 through 31, except ADRS bytes 28
  through 31 contain `START + step` as a 32-bit big-endian integer;
- bytes 48 through 63 are the current 16-byte node;
- byte 64 is `0x1F`;
- byte 135 has `0x80`; and
- all other rate and capacity bytes are zero.

One raw 24-round permutation produces the next node in state bytes 0 through
15. The controller repeats exactly `STEPS` times, copies the final node into
its private result latch, wipes the shared service, releases it, and only then
publishes WOTS `DONE`.

### Checked WOTS word

The public BIOS word is:

```forth
WOTS-CHAIN  ( context-64 start steps dst-16 -- status )
```

It checks `WOTS_CHAIN` first, then validates the complete scalar and memory
arguments before touching the guard or device. `context-64` must be the exact
nonwrapping readable Bank 0 span described above. `dst-16` must be a complete
caller-writable span under the common caller-managed-span policy. Mapped but
forbidden access returns status 4; scalar, arithmetic, mapping, or Bank 0
domain failure returns 3.

After capability and argument preflight, BIOS reads System Info
`NUM_BUS_PORTS`, rejects zero or deadline-arithmetic overflow with status 6,
and computes the request and clear bounds from that value. Those failures do
not touch the guard or WOTS device.

After acquiring the crypto guard, BIOS programs every register with byte
stores, issues `GO`, and uses the calling CPU's 64-bit `CSR_PERF_CYCLES`
counter at CSR `0x68` (the source exposed by `PERF-CYCLES`) to enforce
`WOTS_REQUEST_CYCLES(steps) + 128` after the acknowledged `GO`; 128 is two
complete 64-clock MMIO response deadlines. It maps WOTS owner error 2 to status
2, timeout errors 7 or 8 (or expiry of the BIOS deadline) to status 5, and
device errors 1, 5, 6, or 9 to status 6 because arguments were already
preflighted. Errors 3 and 4 after BIOS preflight also map to 6 as a protocol
disagreement.

WOTS error values 10 through 255, `ERROR` with code 0, `DONE` with a nonzero
error, a status byte outside 0 through 3, or an unexpected `IDLE` after the
acknowledged `GO` are common status 6 and take the same post-programming
cleanup path. Each wait sample consists of one successfully acknowledged
byte read of `STATUS` followed immediately by the elapsed-cycle observation
for that completed read. A successful read returns the addressed byte without
an architectural bus fault; a fault is immediate status 6 and is not retried.
Protocol validation precedes the elapsed-time decision: an impossible
status/error combination, an out-of-range status, or unexpected `IDLE` is
status 6 regardless of sample time. For protocol-valid states, a terminal
status whose observed elapsed time is at most the deadline is classified
before timeout, so terminal status observed exactly at the deadline wins.
`BUSY` observed with elapsed time at least the deadline, or a protocol-valid
terminal status first observed after the deadline, is status 5. The clear
wait uses the same ordering, with `IDLE` as its only successful terminal
status; any other non-`BUSY` clear status is status 6.

BIOS saves `CSR_PERF_CTRL`, writes `saved | 1` to enable the physical core's
counter without setting reset bit 1, and restores the saved bit-0 enable state
before every return, including a fail-closed timeout. The checked WOTS word
makes no scheduler or yield call during that complete save/enable/wait/restore
interval, and same-core interrupt handlers must not write `CSR_PERF_CTRL`.
Elapsed time is the modulo-`2^64` unsigned difference from a start value read
after `GO` is acknowledged. BIOS takes a new start value after `CLEAR` is
acknowledged for the clear deadline. Elaboration must prove each compared
deadline is less than `2^63`. No checked deadline uses the WOTS `CYCLES`
register or the 32-bit timer word named `CYCLES`.

On `DONE`, BIOS stages all 16 result bytes, issues and awaits `CLEAR`, then
copies the complete staged result to `dst-16`, wipes scratch, clears its owner
fields, and releases the guard. Context/destination overlap is allowed because
the complete context and result are staged before caller publication. A
failure after programming begins issues `CLEAR` and leaves all destination
bytes unchanged; capability, argument, and guard failures do not touch the
device. BIOS bounds the clear wait to `WOTS_CLEAR_CYCLES + 128` using the same
cycle counter. It releases the owner fields and lock only after WOTS reaches
`IDLE`; if that clear deadline expires, it returns status 5 with destination
unchanged and scratch wiped, but retains the guard fail-closed. Machine reset
is then the required recovery. The old print-and-return `WOTS-CHAIN-HW`,
`SHA3-LOCKED?`, and probe-style feature detection are removed rather than
retained as aliases.

## Reset state

Cold reset and a reset of the corresponding hardware domain produce these
exact externally observable states before a new request can be accepted:

| Surface | Reset state |
|---|---|
| Portable guards | Spinlocks 8 through 11 are free; `CRYPTO-OWNER-CORE = UINT64_MAX`; `CRYPTO-OWNER-TASK = 0`; every per-core checked CRC owner record is unowned. KDOS initializes its software TLS owner and credential registry separately when their modules load. |
| CRC engine | Mode 0; `CRC_ACC = 0x00000000FFFFFFFF`; every microcluster CRC hardware owner is none; no CRC operation is pending. |
| SHA/Keccak front end | `CTRL = 0`; `STATUS = 0x00`; `ERROR = 0`; `STATE_INDEX = 0`; all 25 lanes, `DOUT`, rate/input storage, staging, and cursors are zero; no completion is pending and no round-service owner exists. |
| WOTS | `STATUS = IDLE`; `ERROR = 0`; `CYCLES = 0`; all programming bytes, `DOUT`, private context, constructed state, counters, and transients are zero; no DMA beat is outstanding and no Keccak claim exists. |

Capability values are configuration state, not mutable transaction state; a
reset does not manufacture support that the executing backend does not
implement.

Runtime `XMEM-RESET` is a KDOS memory-lifetime action, not the hardware reset
described by this table. The networking module defers it through lock 10 and,
when XMEM exists, refuses it with credential busy status while any credential
is active. After synchronous deletion wipes and frees every DER chain,
reset reclaims only above `XMEM-FLOOR` and therefore preserves the
caller-sized credential record pool and its stale generations. Without XMEM,
the bulk-reset action is a no-op, but the XMEM-dependent credential pool cannot
be initialized. Credential handles are volatile and make no claim of
surviving cold machine reset.

## Shared implementation invariants

- There is one Keccak round datapath. Ordinary SHA3/SHAKE, raw command 6, and
  WOTS arbitrate that service; no feature gate may instantiate a duplicate to
  avoid ownership work.
- Accepted operations have bounded progress. Busy, error, abort, and reset
  paths suppress late completion before publishing terminal state.
- Mode, lane, digest, context, CRC feed, DMA, and MMIO byte order are explicit
  and shared by RTL, Python, and native execution.
- A rejected request cannot partially overwrite an active transaction.
- Secret or potentially secret transient state is wiped before ownership is
  released. Each destination-returning checked BIOS word is all-or-nothing;
  KDOS multi-chunk composites use the documented streaming-prefix rule.
- Caller buffers are exact spans. The fixed sizes 16, 32, 64, and 200 in this
  document come from algorithms or interfaces, not arbitrary caller-count or
  storage caps.
- Native execution models visible `BUSY`, command ordering, abort, and
  outstanding DMA state. Synchronous host computation must not hide firmware
  timing errors.
- Capability bits describe complete landed behavior in the executing backend,
  not source presence, a standalone RTL module, or emulator convenience.

## Implementation status and downstream adoption ledger

This section records the completed MegaPad implementation and qualification
cutovers plus the separately authorized downstream adoption boundary. It is
status evidence, not an alternate contract.

### CRC implementation and consumers

The CRC path implements all six modes and register-form sub-operation `0x06`.
Mode 7 and operation 7 are the first invalid reflected-mode and CRC-operation
fixtures in:

- `tests/test_megapad64.py`;
- `tests/test_system.py`;
- `tests/test_phase3_reduced_core_execution.py`;
- `rtl/sim/tb_crc_isa.v`;
- `rtl/sim/tb_cluster.v`;
- `rtl/sim/tb_cpu_micro.v`; and
- `rtl/sim/tb_cpu_smoke.v`.

The implementation cutover updates `asm.py`, `emulator/megapad64.py`,
`emulator/accel/mp64_accel.cpp`, `emulator/accel_wrapper.py`, `rtl/pkg/mp64_pkg.vh`,
`rtl/pkg/mp64_cpu_funcs.vh`, `rtl/crypto/mp64_crc_isa.v`, both CPU decoders,
cluster arbitration/state, native snapshots, and runtime instruction-length
classification together. Full-core and microcore SKIP paths use
sub-operation lookahead for normal and redundant-REX `EXT.CRYPTO` encodings,
including two-byte reserved traps and the three-byte `CRC.FINRAW` form.

Python, native, and RTL apply the same high-half zero-extension rule after
every write-producing 32-bit CRC operation, including state injected through
the full-core accumulator CSR.

BIOS exposes the checked CRC surface in this document, and KDOS
`CRC32C-BUF` selects reflected mode 5. No compatibility aliases preserve the
removed no-status words. KDOS GPT now selects reflected IEEE mode 4 through
the checked surface. Headers use one resident-buffer transaction; partition
arrays reseed short per-sector transactions with the prior raw accumulator,
raw-finalize to release before each subsequent disk read, and apply XOR-out
once at completion. Unsupported and busy failures retain raw BIOS causes 1
and 2 in the partition ior and never use a software fallback.

`docs/BIOS-DICTIONARY.md` previously listed CRC DMA words that do not exist
after CRC moved to the ISA. Checkpoint 0 removes those phantom rows rather
than carrying them into the checked surface.

### SHA3/SHAKE and raw Keccak checkpoint 2

The native singleton, integrated RTL, BIOS, KDOS, and TLS callers now use the
selected checkpoint-2 interface:

- native `CryptoSHA3` owns the exact 96-byte aperture, visible tick-driven
  `BUSY` intervals, complete command decoding, automatic rate absorption,
  sequential 64-byte SHAKE windows, indexed raw lanes, atomic wide accesses,
  and abort/zeroization behavior;
- integrated RTL uses `mp64_sha3` with one `mp64_keccak_core`, whole-access
  width checks, held-`DIN` backpressure, packed phase/owner status, command 6
  raw permutations, and command 7 cleanup. The obsolete WOTS gate no longer
  removes SHA status/error responsiveness;
- BIOS exposes `SHA3-BEGIN`, `SHA3-UPDATE`, `SHA3-FINAL`, `SHAKE-FINAL`,
  `SHAKE-READ`, `SHA3-CLEAR`, and `KECCAK-F1600` with the common checked
  statuses. `SHA3-STATUS@` and `SHA3-MODE@` remain diagnostic reads; the old
  transaction and prototype WOTS words were removed without aliases;
- KDOS `SHA3`, `SHA3-512`, SHAKE, HMAC, HKDF, and `HASH` callers propagate
  checked failures; its SHAKE callers request at most 32 bytes per
  `SHAKE-READ` over the 64-byte hardware window; and
- TLS hash/HMAC/HKDF dispatch returns the selected backend's real status, and
  the private-suite empty hash is constructed through the checked SHA3
  wrapper.

Checkpoint 2 advertised bits 0 through 2 (`0x7`) in the execution model and
integrated RTL while WOTS bit 3 remained clear. The stale 32-byte command-5,
immediate-completion, advisory-lock, and non-waiting BIOS assumptions were
removed. Focused checkpoint-2 sources include
`tests/test_native_sha3_model.py`, the SHA/guard coverage in
`tests/test_concurrency_handoff.py`, and `rtl/sim/tb_sha3_keccak.v`; the stale
mixed `rtl/sim/tb_crypto.v` was split into SHA/Keccak and AES benches. The
final sequential source-load gate selected all seven
`TestKDOSSHA3Checkpoint2` cases plus the SHA-256 and SHA3 TLS dispatch cases;
all nine passed.

### WOTS implementation and bus topology

Checkpoint 2's inert `+0x8A0..+0x8BF` reservation remains historical evidence:
it exposed no functional prototype or capability and performed no memory
access, DMA, or Keccak claim. Checkpoint 3 replaces that reservation—without
an alias or compatibility mode—with the byte-only 64-bit context interface,
checked state machine, real read-only Bank 0 DMA, and one shared-Keccak owner
defined above. The removed modulo-wrapped, three-pointer behavior is not a
compatibility surface and has not returned.

WOTS is the third DMA endpoint, appended after disk so the existing NIC and
disk requester indices remain stable. Native/Python callback topology,
snapshots, arbitration, response classification, and port-count assertions
include all three DMA ports. `NUM_BUS_PORTS` is therefore full-core ports plus
microcluster ports plus NIC, disk, and WOTS. The WOTS port has immutable
weight 1 and unlimited bandwidth while its capability is advertised.

The native controller and Python reference expose the same command/status,
request-accept, one-outstanding-beat, classified-response, abort-drain,
zeroization, reset, and shared-owner behavior. Integrated RTL instantiates the
production controller, connects its requester to the main arbiter, and uses
the existing Keccak round service rather than a duplicate. The dedicated WOTS
unit bench and integrated bus/DMA/shared-owner coverage are part of the
checkpoint-3 qualification gate. Those tests and the checked BIOS path passed,
so the checked-in System Info surface now publishes capability bit 3.

Native `CSR_PERF_CTRL` now follows RTL/Python exactly: bit 0 alone selects
enable, bit 1 independently clears counters, and writing reset without enable
does not force the counter on. Differential truth-table coverage protects the
save/enable/restore behavior used by `WOTS-CHAIN` deadlines.

The public BIOS word is the checked `WOTS-CHAIN` contract above. It does not
retain the prototype print-and-return words, does not infer a requester count
from core topology, and does not publish any caller byte on a checked failure.
The caller's 16 ordinary result stores occur only after complete staging and a
successful CLEAR; this is ordered staged publication, not an atomic 16-byte
memory write.

Checkpoint 3 was not the downstream application cutover. Checkpoint 4 removed
KDOS's private GPT IEEE loop, adopted the reflected hardware path, added
authoritative diagnostics, reproduced fresh native and BIOS artifacts, and
completed the ordered focused matrix, full serial RTL sweep, and approved
Python regression. The MegaPad gate is closed. A user-selected Akashic
worktree may adopt the reflected CRC, raw-Keccak, and WOTS primitives only in
a separate authorized task.

### Guard and System Info checkpoint 2

The execution model and RTL implement the exact 64-byte, 16-lock spinlock
aperture. Lock 8 is reserved for checked MMIO crypto; KDOS reserves lock 9 for
HMAC/HKDF scratch, lock 10 for TLS workspace ownership, and lock 11 for short
TLS credential registry/cancellation transitions. Main-bus arbitration
preserves requester-valid and the winning architectural global core ID
through the response; cluster requests use the latched winning microcore
identity, while DMA and cluster-internal SHA traffic are requester-invalid.
Invalid and out-of-range requesters receive acknowledged non-mutating lock
responses.

BIOS publishes full-width core/task owner fields in an interrupt-state-
preserving critical section and verifies both fields on every continuation.
This supplies the same-core task exclusion that the reentrant hardware lock
alone cannot provide. Checked CRC remains separate: it uses topology-sized
BIOS owner records and the cluster's CRC transaction lock.

The execution model and RTL implement the exact System Info range
`[+0x00,+0x70)`, reject misaligned, crossing, and `+0x70` accesses, expose the
main-bus requester count at `+0x68`, and independently gate capabilities at
`+0x60`.

## Qualification anchors

Qualification uses shared independent known-answer data and covers at least:

- all six CRC checks for `123456789`, arbitrary seeds, byte/qword/mixed feeds,
  tails 1 through 7, raw finalization, zero-extension, cluster contention, and
  op-6 instruction length;
- SHA3/SHAKE empty, partial-rate, exact-rate, multi-rate, and multi-window
  inputs, including 32-byte BIOS reads across 64-, 72-, 136-, and 168-byte
  boundaries;
- the published zero-state Keccak-f[1600] vector, a nonzero full-state vector,
  byte/qword lane round trips, alternating caller states, access faults,
  abort, and wipe behavior;
- every valid WOTS start/step boundary against an independent SHAKE256
  reference, including zero steps, both Bank 0 ends, invalid spans, DMA fault
  and timeout, an outstanding-beat `CLEAR`, stable terminal output, and
  stale-result suppression; and
- full-core/microcore guard contention, two microcores sharing one main-bus
  port, owner-only release, same-core task re-entry, invalid DMA requesters,
  and delayed MMIO acknowledgements with stable requester metadata.

Smoke, integration, persistence, and other heavyweight suites remain
sequential under the repository resource-safety rules. Emulator success does
not qualify integrated RTL, and RTL simulation does not establish synthesis,
timing closure, board behavior, power, entropy quality, or side-channel
properties.

Checkpoint-4 closure ran these anchors sequentially against fresh artifacts.
`make -C rtl/sim -j1 all` completed successfully, and the definitive
`make test-sequential` run completed with 3,425 passed and three conditional
live-network skips. The skipped cases depend on external TAP/network reachability
and do not weaken the executed UDP, device, CRC/GPT, SHA3, Keccak, WOTS, bus,
or SoC coverage.
