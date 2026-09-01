# KDOS Word Reference

KDOS — the Kernel Dashboard Operating System — is a Forth-based OS that runs
on top of the Megapad-64 BIOS.  Its Bank 0 core provides buffers, compute
kernels, pipelines, a cooperative scheduler, a named filesystem, multicore
dispatch, and an interactive 9-screen TUI dashboard.  The loadable
`networking.f` module adds Ethernet through TLS, sockets, and the UDP-backed
data-port transport from the XMEM userland dictionary.

This reference documents words from the KDOS core and networking module,
organized by their source sections in `kdos.f` and `networking.f`.

> **Notation.**  `( before -- after )` is the Forth stack comment.
> Words from the BIOS are used freely (see `docs/bios-forth.md` for those).
> *desc* means a descriptor address (buffer, kernel, pipe, task, or file).

---

## Table of Contents

1. [§1 Utility Words](#1-utility-words)
   - [§1.1 Memory Allocator](#11-memory-allocator)
   - [§1.2 Exception Handling](#12-exception-handling)
   - [§1.3 CRC Integration](#13-crc-integration)
   - [§1.4 Hardware Diagnostics](#14-hardware-diagnostics)
   - [§1.5 AES-256/128-GCM Encryption](#15-aes-256128-gcm-encryption)
   - [§1.6 SHA-3 Hashing](#16-sha-3-hashing)
   - [§1.6a SHA-256 Hashing](#16a-sha-256-hashing)
   - [§1.6b SHA-512 Hashing](#16b-sha-512-hashing)
   - [§1.6c Checked WOTS Chain](#16c-checked-wots-chain)
   - [§1.7 Unified Crypto Words](#17-unified-crypto-words)
   - [§1.8 X25519 ECDH](#18-x25519-ecdh)
   - [§1.9 HKDF Key Derivation](#19-hkdf-key-derivation)
   - [§1.10 Field ALU](#110-field-alu)
   - [§1.11 NTT Engine](#111-ntt-engine)
   - [§1.12 ML-KEM-512 (Kyber)](#112-ml-kem-512-kyber)
   - [§1.13 Hybrid PQ Key Exchange](#113-hybrid-pq-key-exchange)
   - [HBW Math RAM Allocator](#hbw-math-ram-allocator)
   - [§1.15 Userland Memory Isolation](#115-userland-memory-isolation)
2. [§2 Buffer Subsystem](#2-buffer-subsystem)
3. [§3 Tile-Aware Buffer Operations](#3-tile-aware-buffer-operations)
4. [§4 Kernel Registry](#4-kernel-registry)
5. [§5 Sample Kernels](#5-sample-kernels)
6. [§6 Pipeline Engine](#6-pipeline-engine)
7. [§7 Storage & Persistence](#7-storage--persistence)
8. [§7.5 File Abstraction](#75-file-abstraction)
9. [§7.6 MP64FS Filesystem](#76-mp64fs-filesystem)
   - [§7.6.1 Filesystem Encryption](#761-filesystem-encryption)
10. [§7.7 Documentation Browser](#77-documentation-browser)
11. [§7.8 Dictionary Search](#78-dictionary-search)
12. [§8 Scheduler & Tasks](#8-scheduler--tasks)
13. [§8.1 Multicore Dispatch](#81-multicore-dispatch)
14. [§9 Interactive Screens (TUI)](#9-interactive-screens-tui)
15. [§10 Data Ports](#10-data-ports)
16. [§11–§12 Benchmarking & Dashboard](#1112-benchmarking--dashboard)
17. [§13 Help System](#13-help-system)
18. [§14 Startup](#14-startup)
19. [§15 Pipeline Bundles](#15-pipeline-bundles)
20. [§20 Module Registry](#20-module-registry)
21. [`networking.f` §16 Network Stack](#16-network-stack)
22. [`networking.f` §17 Socket API](#17-socket-api)

---

## §1 Utility Words

Small general-purpose helpers used throughout KDOS.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `.R` | `( n width -- )` | Print number *n* right-justified in a field of *width* characters.  Currently a simplified implementation that drops the width and calls `.`. |
| `SAMESTR?` | `( addr1 addr2 maxlen -- flag )` | Compare two zero-padded byte strings up to *maxlen* bytes.  Returns `-1` if identical, `0` if they differ.  Uses the BIOS `COMPARE` word internally. |
| `PARSE-NAME` | `( "name" -- )` | Parse the next blank-delimited word.  Preserve up to 127 bytes in `PATHBUF`, copy its first 23 bytes into the null-terminated 24-byte `NAMEBUF` used for MP64FS component lookup, and set `PN-LEN` to that clamped component length. |
| `NEEDS` | `( n -- )` | Stack safety guard — aborts with an error message if the data stack currently has fewer than *n* items.  Useful at the start of words that need a specific number of arguments. |
| `ASSERT` | `( flag -- )` | Abort with "Assertion failed" if the flag is false (zero).  Useful in tests and sanity checks. |
| `.DEPTH` | `( -- )` | Print the current stack depth in brackets, e.g., `[3 deep]`.  Handy for debugging stack issues. |
| `DEFER` | `( "name" -- )` | Create a deferred word whose action can be changed at run-time.  Defaults to `ABORT`.  Set the action with `IS`. |
| `IS` | `( xt "name" -- )` | Set the action of a deferred word.  E.g. `' my-open IS OPEN`. |

**Variables:** `NAMEBUF` (24-byte component scratch), `PATHBUF` (128-byte
path scratch), `PN-LEN` (clamped `NAMEBUF` length).

**Example:**
```forth
3 NEEDS          \ aborts if fewer than 3 items on stack
PARSE-NAME cat   \ copies "cat" into NAMEBUF, PN-LEN = 3
```

---

### §1.1 Memory Allocator

`ALLOCATE` is region-aware: it uses the XMEM free-list/bump allocator when
external memory is present and falls back to the Bank 0 first-fit heap when it
is not.  XMEM allocations carry an 8-byte size prefix.  Bank 0 blocks carry a
24-byte allocator header and are 8-byte aligned with a 16-byte minimum payload.
Use the `DMA-*` variants when storage must reside in Bank 0 regardless of XMEM
availability.  Allocation and mutation are core-0-only.

`HEAP-SETUP` tile-aligns the system dictionary and begins the Bank 0 heap after
the 32 KiB `LATE-DICT-RESERVE`, leaving room for late system-mode modules.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ALLOCATE` | `( u -- addr ior )` | Allocate a strictly positive *u* bytes from XMEM when available, otherwise Bank 0. Returns address and 0 on success, or 0 and -1 for zero, negative, unrepresentable, or unavailable sizes. |
| `FREE` | `( addr -- )` | Route by address and free a block returned by `ALLOCATE`.  Bank 0 frees coalesce adjacent blocks; XMEM frees return blocks to its reusable free-list. |
| `RESIZE` | `( addr u -- addr' ior )` | Resize an allocated block.  May move data.  Returns 0 on success. |
| `DMA-ALLOCATE` | `( u -- addr ior )` | Allocate explicitly from the Bank 0 heap, even when XMEM is present. |
| `DMA-FREE` | `( addr -- )` | Free a block returned by `DMA-ALLOCATE`. |
| `DMA-RESIZE` | `( addr u -- addr' ior )` | Resize a Bank 0 heap block. |
| `HEAP-SETUP` | `( -- )` | Initialize the Bank 0 heap (called automatically on its first allocation). |
| `LATE-DICT-RESERVE` | `( -- u )` | Constant: 32 KiB kept between the tile-aligned cold system dictionary and `HEAP-BASE` for late Bank 0 compilation. |
| `HEAP-FREE-BYTES` | `( -- n )` | Return total free bytes in the Bank 0 heap. |
| `.HEAP` | `( -- )` | Print Bank 0 heap statistics: total, free, largest block. |
| `MEM-SIZE` | `( -- n )` | Return total RAM in bytes (from SysInfo MMIO). |

> **Open `RESIZE` failure-address discrepancy.** The private Bank 0 source
> contract says a failed resize returns the original address with a nonzero
> `ior`, and the allocation/OOM failure path does. Its current early rejection
> of a zero, negative, or unroundable size instead returns `0 -1` while leaving
> the original allocation live. `RESIZE` and `DMA-RESIZE` inherit that split
> behavior. This note records the mismatch without deciding which result is the
> intended public contract; callers must treat the address as undefined when
> `ior` is nonzero until the source, reference, and tests are resolved together.

---

### §1.2 Exception Handling

ANS Forth CATCH/THROW mechanism for structured error handling. `HANDLER`
selects an exception-chain head for the complete execution context: core 0
uses the current BIOS `TASK-ID`, while physical worker cores use `COREID`.
Foreground, background-slot, and worker catches can therefore remain live
independently across `PAUSE`/`TASK-YIELD` and concurrent core execution. KDOS
also clears a background slot's exception-chain head whenever that slot is
stopped, replaced, or started again; an abandoned suspended `CATCH` frame can
therefore never be inherited by the next coroutine to use the slot.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CATCH` | `( xt -- exception# \| 0 )` | Execute *xt*; if it THROWs, return the exception number.  Returns 0 on normal completion. |
| `THROW` | `( n -- )` | If *n* is nonzero, unwind to the most recent CATCH frame and return *n*.  If *n* is 0, does nothing. |

---

### §1.3 CRC Integration

Convenience wrappers over the BIOS CRC ISA words. `CRC-BUF` feeds exact
buffer length: full 8-byte chunks use `CRC-FEED`, and a 0–7 byte tail uses
`CRC-FEED-BYTE` without zero padding. The result-producing wrappers use
`CRC-FINAL@`, so finalization and final-value capture are one shared-engine
operation.

Every status-bearing primitive is checked. KDOS throws the unchanged nonzero
BIOS status, allowing an enclosing `CATCH` to choose retry or error policy.
Normal wrapper signatures remain result-only. These wrappers cannot make an
unrelated exception transaction-safe: after successful `CRC-MODE!`, the owner
must still reach `CRC-FINAL@` because a trap or arbitrary `THROW` does not
release CRC state.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRC-BUF` | `( addr len -- )` | Feed a buffer into the active CRC transaction, using 8-byte acceleration plus exact byte tails. |
| `CRC32-BUF` | `( addr len -- crc )` | Mode 0 CRC-32/BZIP2 tuple over a buffer. |
| `CRC32C-BUF` | `( addr len -- crc )` | Standard reflected CRC-32C using mode 5; throws UNSUPPORTED when `CRC_REFLECT_RAW` is unavailable. |
| `CRC64-BUF` | `( addr len -- crc )` | Mode 2 CRC-64/WE tuple over a buffer. |
| `CRC32-STR` | `( c-addr len -- crc )` | Readability alias for `CRC32-BUF`. |
| `.CRC32` | `( addr len -- )` | Print `CRC32-BUF` in hexadecimal while preserving the caller's numeric base. |

`CRC32-BUF` and `CRC64-BUF` are the MSB-first modes 0 and 2. `CRC32C-BUF` is
the LSB-first reflected mode 5. All use all-ones init and XOR-out. The exact
polynomials and `"123456789"` check values are specified in the
[ISA reference](isa-reference.md).

GPT verification uses reflected IEEE mode 4; `CRC32-BUF` remains the
non-reflected BZIP2 tuple and is not a compatible alias. Header checks own one
checked resident-buffer transaction. Entry-array checks seed each resident
sector with the prior raw state, use `CRC-RAW-FINAL@` to release before the
next disk read, and apply the IEEE XOR-out once after the final partial chunk.
Missing capability is partition `UNSUPPORTED` with raw BIOS cause 1; owner
contention is partition `BUSY` with raw cause 2. Neither case falls back to
software or disturbs a preexisting transaction.

The checkpoint-4 adoption and authoritative diagnostics are implemented and
qualified. Fresh native and BIOS artifacts reproduced exactly, the complete
serial RTL suite passed, and the approved Python regression completed with
3,425 passed and three conditional live-network skips. Akashic refactoring is
now a separate task that still requires a user-selected worktree.

---

### §1.4 Hardware Diagnostics

Live hardware status and self-test words. `CRC-DIAG?` feeds one qword and one
byte of `"123456789"` through the checked BIOS surface and requires modes
0/1/2/4/5/6 plus mode-5 raw finalization to match their canonical values.
Each acquired transaction is released on success or checked failure; a busy
acquisition leaves the caller's existing transaction unchanged.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CRC-DIAG?` | `( -- flag )` | Return true only when all six canonical finalized CRC vectors and the reflected raw vector pass. |
| `.CRC-DIAG` | `( -- )` | Run and print the checked CRC standard-vector result. |
| `.PERF` | `( -- )` | Print performance counters. |
| `.BIST-STATUS` | `( -- )` | Print the retained memory-BIST status without rerunning destructive BIST. |
| `.TILE-DIAG` | `( -- )` | Run the tile datapath self-test and print its result. |
| `.ICACHE` | `( -- )` | Print instruction-cache hit and miss counters. |
| `DIAG` | `( -- )` | Run `.PERF`, `.CRC-DIAG`, `.BIST-STATUS`, `.TILE-DIAG`, and `.ICACHE` in order. |

---

### §1.5 AES-256/128-GCM Encryption

High-level AES-GCM words built on the BIOS AES accelerator. Supports both
AES-256 (default) and AES-128 (via `AES-KEY-MODE!`).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `AES-ENCRYPT` | `( key iv src dst len -- tag-addr )` | Encrypt a positive uint32 multiple of 16 bytes from *src* to *dst*. Returns the shared 16-byte GCM tag-buffer address. |
| `AES-DECRYPT` | `( key iv src dst len tag -- flag )` | Decrypt and verify a positive uint32 multiple of 16 bytes. Returns 0 if auth OK, -1 if auth failed. |
| `AES-ENCRYPT-BLK` | `( src dst -- )` | Process one 16-byte block (key/IV/CMD must already be set). |
| `AES-ENCRYPT-AEAD` | `( key iv aad aadlen src dst dlen -- tag-addr )` | AEAD encrypt in the current safe source domain: `aadlen` 1..16 and nonnegative uint32 `dlen`; partial data blocks are supported. |
| `AES-DECRYPT-AEAD` | `( key iv aad aadlen src dst dlen tag -- flag )` | AEAD decrypt + verify in the same current safe domain. |
| `.AES-STATUS` | `( -- )` | Print human-readable AES status. |

The bounds above describe the unchanged source as it exists, not a desired
fixed-capacity API. Plain zero/nonmultiple/high-cell lengths do not match the
32-bit engine length and can enter a nonterminating or incomplete loop.
The AEAD wrappers always submit exactly one AAD pad: zero AAD is misclassified,
while more than 16 bytes overruns that pad into live dictionary state and still
does not authenticate the full AAD. These are open KDOS source defects, not
simulator-imposed limits. Exact in-place buffers work; arbitrary overlap is not
qualified. Decryption also streams output before the final tag decision, so a
bad multi-block tag leaves earlier unauthenticated plaintext in the destination
and zeroes only the final block/tail. Callers must discard the entire output on
failure. `AES-TAG-BUF` is shared and overwritten by the next tag fetch.

---

### §1.6 SHA-3 Hashing

Checked SHA-3 and SHAKE convenience words built on the guarded BIOS Keccak
service. The qualified checkpoint-3 configuration reports
`CRYPTO_CAPS = 0xF`; bit 1 advertises this streaming interface, bit 2
advertises the inherited raw `KECCAK-F1600` BIOS word, and bit 3 independently
advertises the checked WOTS chain described below. All words below return the
first checked failure when required cleanup succeeds. A failed cleanup takes
precedence and retains the guard fail-closed.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA3` | `( addr len out -- status )` | Checked one-shot SHA3-256; begin, update, and fixed final publish 32 bytes on success. |
| `SHA3-512` | `( addr len out -- status )` | Checked one-shot SHA3-512; fixed final stages and publishes the complete 64-byte digest. |
| `SHAKE128` | `( addr len out outlen -- status )` | Checked one-shot SHAKE128; preflight the complete output, read it in chunks of at most 32 bytes, and clear on every handled terminal path. |
| `SHAKE256` | `( addr len out outlen -- status )` | Checked one-shot SHAKE256 with the same staged BIOS-read and cleanup rules. |
| `SHAKE-STREAM` | `( addr blocks -- status )` | From an already-finalized checked SHAKE transaction, validate owner/phase, preflight the complete overflow-safe `blocks*32` span, read 32-byte chunks, and then clear; a negative count returns RANGE and clears. |

The common constants `CRYPTO-OK`, `CRYPTO-UNSUPPORTED`, `CRYPTO-STATE`,
`CRYPTO-RANGE`, `CRYPTO-PROTECTED`, `CRYPTO-TIMEOUT`, and
`CRYPTO-HARDWARE` name statuses 0 through 6. The BIOS hardware window is 64
bytes, while each `SHAKE-READ` used by KDOS requests no more than 32 bytes.
Fixed-output hashing uses `SHA3-FINAL`; SHAKE uses `SHAKE-FINAL` and must end
with `SHA3-CLEAR`.

KDOS inherits `KECCAK-F1600 ( state-200 -- status )` directly from BIOS. The
200-byte image is 25 little-endian lanes in `x + 5*y` order:
`memory[8*(x+5*y)+b] = state[x+5*y][8*b +: 8]`. The operation is an in-place
raw 24-round permutation only: it does not absorb, pad, apply a domain
separator, squeeze, or reverse bytes, and a failure leaves the image
unchanged.

The hosted simulator qualification through `kdos.f` line 1216 uses the
derivative `CRYPTO_CAPS = 0x7` profile: reflected/raw CRC, checked SHA3/SHAKE,
and raw Keccak are present, while WOTS bit 3 remains clear. This is distinct
from the production checkpoint-3 `0xF` profile above. All checked input,
output, and raw-state spans use `CALLER-SPAN-STATUS` before transfer. A
nonempty Bank-0 span must lie between the static/dictionary protection floor
and the calling context's future result-cell boundary; other caller-managed
memory must fit wholly in one advertised external, HBW, or VRAM region. The
shared transaction is owned by the BIOS `(COREID,TASK-ID)` identity until its
terminal clear. Passing the span check establishes geometry and protection,
not allocation ownership or safe aliasing.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `.SHA3-STATUS` | `( -- )` | Print the low two status bits as idle, busy, done, or error. |
| `.SHA3` | `( addr len -- )` | Print each input byte as two uppercase hexadecimal digits, with no separator. |
| `RANDOM32` | `( -- u )` | Mask the low 32 bits of BIOS `RANDOM`. |
| `RANDOM16` | `( -- u )` | Mask the low 16 bits of BIOS `RANDOM`. |
| `RAND-RANGE` | `( max -- n )` | Apply signed `MOD` and `ABS` to one BIOS `RANDOM` value; valid only for a positive signed maximum. |

`.SHA3` uses `0 DO`, not `?DO`. Its qualified domain therefore requires a
positive, nonwrapping readable length. A zero or negative length can enter a
wrapping/nonterminating loop rather than print an empty string. `RAND-RANGE`
faults when `max` is zero, has no useful range contract for a negative
maximum, and is generally modulo-biased because it performs no rejection
sampling. It must not be treated as a uniform bounded sampler.

The hosted TRNG stream used by these random helpers is deterministic from an
explicit injected seed and guest read/seed schedule. It is test replay input,
not hardware or cryptographically secure randomness. Its synchronous SHA
service likewise proves terminal values and state, not an observable BUSY
interval or hardware timing. The hosted nonclaims and the current
native-executable/RTL SHA error-priority discrepancies are recorded in the
[simulator contract](simulator-contract.md#6-platform-services).

---

### §1.6a SHA-256 Hashing

SHA-256 (SHA-2) convenience words built on the BIOS SHA-256 hardware
accelerator.  Used by the TLS 1.3 cipher suite 0x1301
(TLS_AES_128_GCM_SHA256) and HKDF-SHA256 key derivation.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA256-INIT` | `( -- status )` | Initialize this core's checked streaming SHA-256 context. |
| `SHA256-UPDATE` | `( addr len -- status )` | Preflight and absorb a complete physical-memory span. |
| `SHA256-FINAL` | `( out -- status )` | On success publish 32 digest bytes; erase the complete context on every path. |
| `SHA256-CLEAR` | `( -- status )` | Idempotently abort, release, zeroize buffered/staged/visible state, and return 0. |
| `SHA2-SPAN-STATUS` | `( addr len -- status )` | Pure pre-`INIT` check for one physical window and either SHA-2 context arena; returns only 0, 2, or 3. |
| `SHA256` | `( addr len out -- status )` | Checked one-shot SHA-256; returns the first BIOS failure unchanged. |
| `HMAC-SHA256` | `( key-addr key-len msg-addr msg-len out-addr -- status )` | Checked HMAC-SHA256; hashes long keys and shares only its private no-lock core with HKDF-SHA256. |

Streaming state is core-local. `INIT` and `FINAL`/`CLEAR` must execute on the
same core. `SHA256-OK`, `SHA256-STATE`, `SHA256-RANGE`,
`SHA256-CONTEXT-ALIAS`, and `SHA256-LENGTH-OVERFLOW` name status values
0 through 4. UPDATE/FINAL reject the union of the SHA-256 and SHA-512
all-core context arenas. Every failure aborts and wipes; a failed `FINAL` does not
publish to a non-context destination. HMAC and HKDF return the first such
failure without dropping it.

### §1.6b SHA-512 Hashing

KDOS exposes the scoped BIOS streaming ABI directly and adds a one-shot
`SHA512` wrapper. The BIOS owns a private context per core; SHA-512's use of
R16–R19 and ACC0–ACC3 is confined to short interrupt-masked engine windows,
and the caller's prior register/TSRC0 transaction is restored afterward.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SHA512-INIT` | `( -- status )` | Initialize this core's streaming SHA-512 context. |
| `SHA512-UPDATE` | `( addr len -- status )` | Preflight and absorb a physical-memory span; arbitrary splits and 128-byte boundaries are supported. |
| `SHA512-FINAL` | `( out -- status )` | On success publish 64 digest bytes; erase the complete context on every path. |
| `SHA512-CLEAR` | `( -- status )` | Idempotently abort, release, zeroize saved/staged/buffered/visible state, and return 0. |
| `SHA512` | `( addr len out -- status )` | One-shot wrapper; stops at the first failure without losing its status. |

Streaming state is core-local. `INIT` and `FINAL`/`CLEAR` must execute on the
same core. Checked statuses are exposed both numerically and as
`SHA512-OK`, `SHA512-STATE`, `SHA512-RANGE`, `SHA512-CONTEXT-ALIAS`, and
`SHA512-LENGTH-OVERFLOW`. UPDATE/FINAL use the same union-of-SHA-2-arenas
decision as `SHA2-SPAN-STATUS`. They also require an exact active marker,
offset below 128, byte-aligned low length, and matching low-length
modulo-128 position before an empty UPDATE or destination preflight. Every
failure aborts and wipes; a failed `FINAL` does not publish a digest to a
non-context destination.

The hosted simulator's exact contiguous qualification through `kdos.f` line
1269 includes both one-shot wrappers and all ten status constants. Its
runtime-local service is per architectural core, uses no SHA-3 owner or
spinlock, has no MMIO aperture, and requires no `CRYPTO_CAPS` bit. `HASH`
continues to mean the SHA3-256 wrapper; `SHA256` and `SHA512` are distinct
SHA-2 transactions.

Hosted `SHA2-SPAN-STATUS` follows physical geometry rather than the stricter
caller-managed policy: address zero and static Bank-0 data are admissible when
the nonempty span fits in that region, while wrap, MMIO, unmapped, and
cross-region spans return RANGE. Native context arenas return CONTEXT-ALIAS.
The hosted contexts live outside guest memory, so an ordinary hosted span
cannot alias them unless a composition explicitly maps private arena ranges.
Every nonzero `UPDATE` or `FINAL` result logically clears the selected
context, and finalization stages the complete big-endian digest before
publication.

Hosted logical cleanup clears its explicit metadata/stage and releases its
incremental host hash object; it does not claim physical erasure inside the
host crypto library. It also supplies no EXT.CRYPTO, cycle, interrupt,
arbitration, constant-time, RTL, or hardware evidence. The current
working-native/current-RTL instruction-path discrepancy is recorded in the
[simulator contract](simulator-contract.md#6-platform-services).

---

### §1.6c Checked WOTS Chain

KDOS inherits the qualified production BIOS entry directly. The checked-in
checkpoint-3 configuration advertises bit 3. A derivative backend that keeps
the bit clear leaves the word discoverable, but it returns `UNSUPPORTED`
before argument or device access:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `WOTS-CHAIN` | `( context-64 start steps dst-16 -- status )` | Run one checked 0..15-step chain using the read-only Bank 0 DMA requester and shared Keccak service. |

`context-64` is exactly `PK.seed[16] || ADRS[32] || node[16]` and must be a
complete nonwrapping readable Bank 0 span. `dst-16` follows the ordinary
caller-writable-span policy and may overlap the context. The word checks
WOTS capability before all arguments, validates widened start/step geometry,
derives bounded request and clear waits from `NUM_BUS_PORTS`, and holds crypto
guard 8 without yielding. Zero steps still reads all 64 context bytes and
returns the input node without claiming Keccak.

On success BIOS stages all 16 result bytes, proves hardware CLEAR reached
IDLE, then copies those bytes to the caller. This is ordered staged
publication through ordinary stores, not one atomic 16-byte memory write.
Every checked failure leaves all destination bytes unchanged. If CLEAR itself
times out, BIOS returns TIMEOUT but retains the software owner and guard
fail-closed until machine reset. The common checked statuses are 0 OK, 1
UNSUPPORTED, 2 STATE/OWNER, 3 RANGE, 4 PROTECTED, 5 TIMEOUT, and 6
HARDWARE/PROTOCOL.

Checkpoint 3 delivered this BIOS primitive after the full qualification gate
enabled capability bit 3; it did not itself authorize Akashic adoption. The
KDOS GPT hardware CRC replacement, diagnostics, fresh artifacts, ordered
focused qualification, full serial RTL sweep, and approved Python regression
are now complete at checkpoint 4. Akashic CRC, raw-Keccak, and WOTS
refactoring remains a separately authorized, user-selected-worktree task.

---

### §1.7 Unified Crypto Words

High-level crypto API combining AES and SHA3.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HASH` | `( addr len out -- status )` | Checked alias for SHA3-256. |
| `HMAC` | `( key klen msg mlen out -- status )` | Checked HMAC-SHA3-256. Uses ipad/opad (XOR 0x36/0x5C), block size 136, hashes long keys, and returns the first SHA3 failure. |
| `ENCRYPT` | `( key iv src dst len -- tag-addr )` | AES-256-GCM encrypt (alias for AES-ENCRYPT). |
| `DECRYPT` | `( key iv src dst len tag -- flag )` | AES-256-GCM decrypt (alias for AES-DECRYPT). |
| `VERIFY` | `( addr1 addr2 len -- flag )` | Constant-time comparison.  Returns 0 if equal, -1 if different. |

The hosted simulator's exact contiguous qualification through `kdos.f` line
1431 executes this complete section unchanged. Its runtime-local 16-lock bank
supplies the nonblocking, physical-core-owned `SPIN@`/`SPIN!` contract needed
by lock 9. HMAC-SHA3-256 uses the 136-byte SHA3-256 rate, hashes keys longer
than 136 bytes, propagates the first checked SHA3 status, stages its final
32-byte publication through `SHA3-FINAL`, and wipes its 392 bytes of pads,
intermediate key/digest storage, and metadata before ordinary release.
`ENCRYPT` and `DECRYPT` remain source aliases to the already admitted AES
words. `VERIFY` visits the complete requested byte count and returns the
documented flag for a positive, nonwrapping length; hosted execution makes no
constant-time or side-channel claim. The unchanged word uses `0 DO`, not
`?DO`, so a zero length enters a wrapping loop rather than representing an
empty comparison and can fault on its first byte access. That source defect is
recorded rather than repaired in simulator-only code.

---

### §1.8 X25519 ECDH

Elliptic Curve Diffie-Hellman key exchange (RFC 7748) using the Field ALU
X25519 operation. All scalar, point, and result values are 32-byte
little-endian byte strings.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `X25519` | `( scalar point result -- )` | Clamp the scalar inside the operation and compute `result = scalar × point`. |
| `X25519-KEYGEN` | `( -- )` | Fill `X25519-PRIV` with 32 `RANDOM8` bytes and compute `X25519-PUB` against the base point. |
| `X25519-DH` | `( their-pub -- )` | Compute `X25519-SHARED = X25519-PRIV × their-pub`. |

The four 32-byte source buffers are `X25519-PRIV`, `X25519-PUB`,
`X25519-SHARED`, and the fixed `X25519-BASE = 09 00...00`. The stored private
bytes remain unclamped; clamping occurs only while `X25519` executes. These
global buffers are cooperative KDOS scratch, are not task/core-isolated, and
are not wiped by this section. The raw BIOS path has no checked status,
capability gate, lock, or low-order/all-zero-secret rejection; TLS applies its
separate all-zero policy where required.

The hosted simulator's exact unchanged qualification through `kdos.f` line
1481 executes these seven definitions over the six ordinary BIOS primitives,
not a host-side replacement for `X25519`, `X25519-KEYGEN`, or `X25519-DH`.
Inputs are consumed before the result is stored, so the result may alias the
scalar or point. Hosted deterministic `RANDOM8` makes key generation
reproducible for tests and is explicitly not cryptographically secure entropy.

The former table entries `X25519-CLAMP` and `X25519-PUBKEY` do not exist in
the checked-in KDOS source or BIOS dictionary. They were stale documentation,
not compatibility aliases.

---

### §1.9 HKDF Key Derivation

HMAC-based Key Derivation Function (RFC 5869).  Two families: SHA3-HMAC
(for cipher suite 0xFF01) and SHA-256 HMAC (for cipher suite 0x1301).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HKDF-EXTRACT` | `( salt slen ikm ilen out -- status )` | Checked SHA3-HMAC extract: PRK = HMAC(salt, IKM), with a 32-byte output; returns the HMAC status unchanged. |
| `HKDF-EXPAND` | `( prk info ilen len out -- status )` | Checked SHA3-HMAC expand: T(0) is empty, T(i) = HMAC(PRK, T(i−1) \|\| info \|\| i), and OKM concatenates T blocks up to 255×32 bytes. |
| `HKDF-SHA256-EXTRACT` | `( salt slen ikm ilen out -- status )` | Checked extract (SHA-256): PRK = HMAC-SHA256(salt, IKM). 32-byte output on success. |
| `HKDF-SHA256-EXPAND` | `( prk info ilen len out -- status )` | Checked SHA-256 expand with the same chained T(i−1) \|\| info \|\| i construction, up to 255×32 bytes; returns the first hash failure. |

`HMAC`, `HMAC-SHA256`, and both HKDF families serialize their shared KDOS
scratch with one nonblocking attempt on reserved hardware spinlock 9. Busy
returns `CRYPTO-STATE` for the SHA3 family or `SHA256-STATE` for the SHA-256
family. SHA3 capability absence is checked first and therefore still returns
`CRYPTO-UNSUPPORTED` even when lock 9 is busy. The lock is held through all
private no-lock HMAC stages and through zeroization of pads, normalized keys,
intermediate digests, HKDF state, and pointer/length metadata. These wrappers
do not yield; lock 9 serializes every full and microcore, and SHA3 calls use the
fixed lock order 9 then the BIOS-managed crypto lock 8. Applications must not
acquire lock 9 around these words or call them while retaining an active
`SHA3-BEGIN`/SHAKE or `SHA256-INIT` transaction. An unexpected Forth `THROW`
from a private HMAC/HKDF stage is caught at the lock-9 boundary. The selected
checked-hash transaction is aborted before the complete family scratch is wiped; after a
successful abort, lock 9 is released and the exact exception is rethrown. If
the lower abort fails, its cleanup status takes precedence and lock 9 remains
held after the family scratch is wiped. That retention excludes other cores,
but the hardware bank's depthless same-core reacquisition means it is not
fully fail-closed against a later task on the retaining core; the open design
choice is recorded in the
[crypto interface contract](crypto-interface-contract.md#portable-crypto-guard).
This boundary contains Forth exceptions, not architectural traps, and does
not by itself release an outer owner such as the networking module's TLS lock
10.

Capability-absent and busy-lock exits occur before the guard, so they consume
their public arguments but do not run checked-hash abort or wipe preexisting
private scratch. The cleanup and release contract applies only after lock 9
was acquired.

The null-salt convention is selected solely by `slen=0`; the salt pointer is
then ignored and 32 zero bytes are used. This is narrower than the source
comment saying "salt is 0 / slen=0." With a nonzero length, address zero is an
ordinary supplied pointer: SHA3 HKDF rejects it under the caller-managed-span
policy with `CRYPTO-RANGE`, while SHA-256 HKDF admits physical Bank 0 address
zero when that span is otherwise valid and hashes those bytes. This is a
documented source-comment/implementation discrepancy, not a decision that
either pointer-zero behavior should become the public convention.

HKDF expansion preflights the complete output and info spans plus its fixed
32-byte PRK, then publishes one successful 32-byte-or-smaller block at a time.
If a later checked hash operation fails, the word returns that first failure
and leaves the already-completed output prefix in place. No unrelated
8,160-byte staging arena is imposed. Multi-window SHAKE wrappers have the same
per-chunk publication rule, with each BIOS `SHAKE-READ` itself all-or-nothing.

An HKDF expansion destination may not overlap its fixed 32-byte PRK or its
nonempty info span, because both inputs are reread for each output block. Such
an alias returns `CRYPTO-RANGE` for SHA3 HKDF or `SHA256-RANGE` for SHA-256
HKDF before publishing output.

The named HMAC/HKDF pads, intermediate buffers, normalized keys, counters, and
metadata are private KDOS implementation storage. Application key, message,
salt, IKM, info, PRK, and destination spans must not alias them.

The hosted simulator executes these definitions from the exact unchanged
`kdos.f` block at lines 1635 through 2043. That source block begins with the
hybrid-exchange scratch, then defines both complete HKDF families and
`HMAC-SHA256`, and finally publishes the three hybrid words. All 59 definitions
are ordinary source definitions; none is replaced by a hosted whole-word
implementation.

---

### §1.10 Field ALU

The unchanged `kdos.f` section at lines 1483–1515 exposes the general
multi-prime Field ABI. It defines `PRIME-25519`, `PRIME-SECP`, `PRIME-P256`,
and `PRIME-CUSTOM`, plus four zero-initialized 32-byte scratch buffers `_FA`,
`_FB`, `_FR`, and `_FRH`. It does not define the formerly documented `F+`,
`F-`, or `F*` aliases.

All raw BIOS arguments below are addresses. Values are 32-byte little-endian
integers, and raw 512-bit results use distinct low/high buffers.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `GF-A!` | `( a-addr -- )` | Load ACC0–ACC3 from four ascending qwords. |
| `GF-R@` | `( r-addr -- )` | Store ACC0–ACC3 as four ascending qwords. |
| `GF-PRIME` | `( selector -- )` | Select Curve25519, secp256k1, P-256, or custom by the low two bits. |
| `LOAD-PRIME` | `( p-addr pinv-addr -- )` | Latch custom prime and Montgomery inverse without selecting custom mode. |
| `FADD` | `( a-addr b-addr r-addr -- )` | (a + b) mod p for canonical field inputs. |
| `FSUB` | `( a-addr b-addr r-addr -- )` | (a − b) mod p for canonical field inputs. |
| `FMUL` | `( a-addr b-addr r-addr -- )` | (a · b) mod p. |
| `FSQR` | `( a-addr r-addr -- )` | a² mod p. |
| `FINV` | `( a-addr r-addr -- )` | a^(p−2) mod p (Fermat exponentiation). |
| `FPOW` | `( a-addr e-addr r-addr -- )` | a^e mod p (ordinary modular exponentiation). |
| `FMUL-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Raw 256×256 product in two 32-byte outputs. |
| `FCMOV` | `( a-addr cond-addr -- )` | Replace ACC if `cond-addr C@` is nonzero; read `a` even when false. |
| `FCEQ` | `( a-addr b-addr r-addr -- )` | Store exact 256-bit equality as 1 or 0. |
| `FMAC` | `( a-addr b-addr r-addr -- )` | Add retained previous-low to the selected product. |
| `FMUL-ADD-RAW` | `( a-addr b-addr rlo-addr rhi-addr -- )` | Add the product to retained previous-low/high modulo `2^512`. |

ACC, operand/result addresses, prime configuration, and previous results are
physical-core state, not task-owned state. `FMUL`/`FSQR` use Montgomery REDC
only in custom mode with a nonzero latched inverse; `FINV` and `FPOW` always
use ordinary residues. Portable arithmetic assumes canonical inputs and a
valid prime/custom tuple. The exact qword fault/publication order and known
noncanonical, native raw-carry, reset, and RTL-integration discrepancies are
recorded in the [BIOS reference](bios-forth.md#field-alu--multi-prime-arithmetic-15-raw-words)
and [simulator contract](simulator-contract.md#6-platform-services).

The hosted simulator executes this exact source block after unchanged X25519,
advancing the contiguous KDOS frontier through line 1515. The adjacent NTT
slice now continues that same frontier through line 1584.

---

### §1.11 NTT Engine

256-point Number Theoretic Transform for lattice-based post-quantum crypto.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NTT-SETQ` | `( q -- )` | Set the NTT modulus (3329 for ML-KEM, 8380417 for ML-DSA). |
| `NTT-IDX!` | `( idx -- )` | Set the retained raw 16-bit coefficient index. |
| `NTT-LOAD` | `( addr buf -- )` | Load 256 uint32-LE coefficients. *buf*: 0 = poly A, every nonzero value = poly B. |
| `NTT-STORE` | `( addr -- )` | Store 256 result coefficients to memory. |
| `NTT-FWD` | `( -- )` | Forward NTT (time → frequency domain). |
| `NTT-INV` | `( -- )` | Inverse NTT (frequency → time domain). |
| `NTT-PMUL` | `( -- )` | Pointwise multiply A × B mod q. |
| `NTT-PADD` | `( -- )` | Pointwise add (A + B) mod q. |
| `NTT-STATUS@` | `( -- n )` | Read NTT status (0 = idle, 1 = busy, 2 = done). |
| `NTT-WAIT` | `( -- )` | Poll until NTT operation completes. |
| `NTT-POLYMUL` | `( a b r -- )` | Full polynomial multiply: r = a · b via forward NTT, pointwise multiply, inverse NTT. |
| `.NTT-STATUS` | `( -- )` | Print human-readable NTT status. |

Exact unchanged lines 1517 through 1584 define `Q-KYBER`, `Q-DILITHIUM`,
the two selectors, two 1024-byte global scratch buffers, `NTT-POLYMUL`, and
`.NTT-STATUS`. The raw engine and scratch buffers are shared cooperative state
with no lock or task owner. Ordinary input/output aliasing is safe after each
input load, but a caller must not alias `_NTT-TMP-A` or `_NTT-TMP-B`, and
concurrent `NTT-POLYMUL` calls are unsafe.

Despite the section's PQ labels, this primitive computes ordinary cyclic
convolution modulo `x^256-1`; it is not the specialized negacyclic
multiplication used by ML-KEM or ML-DSA. Current executable and RTL NTT paths
also disagree on register layout, transfer width, roots, configurable-q
behavior, and latency. See the
[BIOS reference](bios-forth.md#ntt-engine-10-raw-words) for the pinned
discrepancy rather than treating either backend as interchangeable evidence.

The hosted contiguous frontier continues through the complete adjacent
ML-KEM block at line 1633.

---

### §1.12 ML-KEM-512 (Kyber)

Lattice-based key encapsulation using the executable Python KEM device's
ML-KEM-specific value path. It does not use the generic cyclic NTT service.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KYBER-KEYGEN` | `( seed pk sk -- )` | Generate ML-KEM-512 keypair.  *seed*: 64 bytes, *pk*: 800 bytes, *sk*: 1632 bytes. |
| `KYBER-ENCAPS` | `( pk coin ct ss -- )` | Encapsulate with 32 caller-provided random bytes: produce ciphertext (768 bytes) and shared secret (32 bytes). |
| `KYBER-DECAPS` | `( ct sk ss -- )` | Decapsulate: recover shared secret from ciphertext using the secret key. |
| `KEM-STATUS@` | `( -- n )` | Read KEM accelerator status. |

Exact unchanged lines 1586 through 1633 define the five KEM buffer IDs and
sizes, the three wrappers, and `.KEM-STATUS` over the seven raw BIOS words
`KEM-SEL!`, `KEM-LOAD`, `KEM-STORE`, `KEM-KEYGEN`, `KEM-ENCAPS`,
`KEM-DECAPS`, and `KEM-STATUS@`. The wrappers fully load every input before
storing outputs, so ordinary input/output aliases are safe. If PK and SK
outputs exactly overlap, the later SK store wins; if CT and SS overlap, the
result is `SS || CT[32:]`.

The service owns five shared retained buffers plus one selector, byte index,
and status with no owner, lock, rollback, or automatic wipe. Commands complete
synchronously and retain status 2. Short loads retain old suffixes, indices
pin at capacity, excess stores write zero, and byte-transfer faults preserve
the executable read-before-write order. `KEM-SEED-SIZE` is visibly 32 even
though `KYBER-KEYGEN` literal-loads and consumes a 64-byte `d || z` input; this
document records that discrepancy without resolving it.

Generated/well-formed-key deterministic vectors were independently matched to
OpenSSL 3.5.2 ML-KEM-512. The implementation is not FIPS-certified,
constant-time, a hostile-key validator, or a protected host-secret boundary.
Its fixed 840-byte SHAKE sampling prefix also leaves a theoretical rare
capacity case. Current RTL has an incompatible register/timing contract and a
non-cryptographic deterministic stub; the hosted slice qualifies neither RTL
nor direct MMIO. The contiguous hosted frontier now continues through the
adjacent hybrid/HKDF block at line 2043.

---

### §1.13 Hybrid PQ Key Exchange

Combined X25519 + ML-KEM-512 key exchange.  Both shared secrets are
concatenated and passed through HKDF-Extract + HKDF-Expand to derive
a single 32-byte hybrid shared secret.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PQ-EXCHANGE-INIT` | `( peer-x25519 peer-pk ct ss -- status )` | Initiator side: X25519 ECDH + ML-KEM encapsulation, followed by checked hybrid-key derivation. |
| `PQ-EXCHANGE-RESP` | `( peer-x25519 ct sk ss -- status )` | Responder side: X25519 ECDH + ML-KEM decapsulation, followed by checked hybrid-key derivation. |
| `PQ-DERIVE` | `( out -- status )` | Derive the 32-byte hybrid key from the internal concatenated X25519 and ML-KEM secrets, propagating checked HKDF status. |

INIT and RESP first populate `_PQ-CAT` as `_PQ-SS-X || _PQ-SS-K`.
`PQ-DERIVE` assumes that 64-byte concatenation is already present, performs
SHA3-HMAC HKDF-Extract with the 32-zero-byte empty-salt convention, and expands
32 bytes with the literal 9-byte info string `pq-hybrid`.
`PQ-EXCHANGE-INIT` first performs X25519,
consumes 32 `RANDOM8` bytes into `_PQ-COIN`, publishes the ML-KEM ciphertext,
and only then derives the final key. `PQ-EXCHANGE-RESP` likewise completes
X25519 and ML-KEM decapsulation before derivation. Their returned status is
only the checked HKDF status; the raw X25519 and KEM stages have no checked
result to propagate.

The exchange has no outer owner or transaction. It uses the global
`X25519-PRIV` plus `_PQ-SS-X`, `_PQ-SS-K`, `_PQ-CAT`, `_PQ-PRK`, and
`_PQ-COIN`; those secret-bearing buffers are shared across callers and are not
wiped. Spinlock 9 covers only each HKDF call and its HMAC/HKDF scratch cleanup.
Concurrent exchanges can therefore interleave the X25519, KEM, PQ-scratch,
extract, and expand stages; each individual HKDF call excludes peer cores,
subject to the depthless same-core reacquisition caveat above.

Failure is correspondingly nontransactional. If extract cannot acquire lock
9, an initiator has already consumed entropy and published its ML-KEM
ciphertext, while `_PQ-SS-X`, `_PQ-SS-K`, `_PQ-CAT`, and `_PQ-COIN` have
changed; `_PQ-PRK` and the requested final-key output remain unchanged. If
extract succeeds but expand later contends or fails, `_PQ-PRK` has also been
published while the final-key output remains unchanged. A responder has
likewise completed its raw stages before either derivation failure. Initiator
callers must keep ciphertext and final-key output disjoint when both values
must survive, because the later key publication may overwrite an overlapping
ciphertext prefix. Ordinary external inputs are
consumed before final output, but callers must not alias the private PQ or
HMAC/HKDF scratch.

The hosted `RANDOM8` stream is deterministic development entropy. The source
does not reject an all-zero X25519 result, and the raw ML-KEM service is not a
hostile-key validator. This construction is therefore qualified as the exact
KDOS application composition and its byte values, lifecycle, and failures—not
as a standardized hybrid KEM, a security proof, constant-time execution, or a
protected secret boundary. Exact unchanged lines 1635 through 2043 advance the
contiguous hosted frontier to the blank line immediately before the now-admitted
HBW allocator section.

---

### HBW Math RAM Allocator

The source repeats the `§1.12` section number for this block even though it
follows §1.13; that numbering discrepancy is retained rather than silently
renumbering the executable source. The allocator reads its geometry through
the BIOS words `HBW-BASE` and `HBW-SIZE` and owns only two ordinary dictionary
variables:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HBW-BASE` | `( -- addr )` | BIOS word reading the bound SysInfo HBW base qword. |
| `HBW-SIZE` | `( -- u )` | BIOS word reading the bound SysInfo HBW size qword. |
| `HBW-HERE` | `( -- a-addr )` | Variable containing the shared next-allocation pointer. |
| `HBW-LIMIT` | `( -- a-addr )` | Variable containing the shared exclusive limit. |
| `HBW-INIT` | `( -- )` | Reload `HBW-HERE` and `HBW-LIMIT` from current SysInfo geometry. |
| `HBW-ALLOT` | `( u -- addr )` | Return the old pointer and advance exactly `u`; ordinary overflow emits `HBW overflow` and aborts before changing the pointer. |
| `HBW-ALLOT?` | `( u -- addr ior )` | Checked counterpart returning `(0,-1)` on ordinary overflow with the pointer unchanged. |
| `HBW-TALIGN` | `( -- )` | Round `HBW-HERE` upward to a 64-byte boundary without allocating or checking the limit. |
| `HBW-RESET` | `( -- )` | Reset the shared pointer to the current SysInfo base. |
| `HBW-FREE` | `( -- u )` | Return `HBW-LIMIT - HBW-HERE`. |
| `.HBW` | `( -- )` | Render live base, size, used, and free cells in the current numeric base using signed `.`. |

`HBW-INIT` runs once while the block loads. Zero-byte and exact-fit allocations
succeed; allocation does not align, write, or clear returned storage. The
pointer is shared by every context in one runtime and has no owner, lock,
ledger, floor, individual free, or rollback. `HBW-RESET` neither wipes bytes
nor revokes stale addresses, so callers must coordinate allocation and bulk
reuse themselves. Separate simulator runtimes retain independent guest memory
and allocator variables.

The allocator treats the complete advertised span as available. Separately,
`graphics.f` places its framebuffer at `HBW-BASE + 0x200000` without advancing
`HBW-HERE`; allocation into the third MiB can therefore overlap an active
framebuffer unless the composing system reserves that range cooperatively.

The ordinary qualified domain uses a mapped HBW span, a pointer within that
span, and a request no larger than the remaining capacity. Although the stack
comment calls the request `u`, both allocation words add before using signed
`>` and perform no wrap check. A high-cell request can therefore wrap the new
pointer and be reported as success; for example, request `-1` as a cell moves
the canonical base backward by one. `HBW-TALIGN` also has no bound check and
can cross a configured limit that is not 64-byte aligned. These source defects
are reproduced and documented, not repaired by a simulator-only allocator.

Canonical hardware and emulator geometry is base `0xFFD0_0000`, size 3 MiB.
The hosted factory can also represent an absent HBW region and then reports
base/size `(0,0)`; an ordinary small, nonwrapping positive allocation fails
while zero allocation succeeds. A configured-zero emulator instead retains
the fixed base with size zero. Canonical hardware is unaffected, and this
record does not choose a permanent absent-region convention.

Exact unchanged lines 2044 through 2108 define all nine source words and run
the load-time initializer over the two dynamic SysInfo-backed BIOS reads. The
next unchanged block, lines 2110 through 2388, admits the complete `§1.12a`
external-memory allocator and `§1.0b` public allocation dispatch through
`XBUF`.

### External Memory Allocator and Dispatch

`EXT-MEM-BASE` and `EXT-MEM-SIZE` dynamically read the actual SysInfo external
region. Load-time `XMEM-INIT` snapshots that geometry into `XMEM-HERE` and
`XMEM-LIMIT`; an absent region sets both to zero.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `XMEM?` | `( -- flag )` | True when the reported size is positive as a signed cell. |
| `XMEM-ALLOT` | `( u -- addr )` | Allocate a positive request, normalized to 16 bytes; abort on failure. |
| `XMEM-ALLOT?` | `( u -- addr ior )` | Checked counterpart returning `(0,-1)` on failure. |
| `XMEM-FREE-BLOCK` | `( addr u -- )` | Normalize and prepend one caller-owned span to the in-band free list after bounds checks. |
| `ALLOCATE` / `FREE` / `RESIZE` | standard | Prefer prefixed XMEM blocks when XMEM is present; otherwise retain the Bank-0 heap. |
| `DMA-ALLOCATE` / `DMA-FREE` / `DMA-RESIZE` | standard | Always use the Bank-0 heap. |
| `XMEM-TALIGN` | `( -- )` | Round the bump pointer upward to 64 bytes without a limit check. |
| `XMEM-RESET` | `( -- )` | Reset to `XMEM-FLOOR` or the base and clear the free list without wiping bytes. |
| `XMEM-FREE` | `( -- u )` | Return virgin bump-tail capacity; reclaimed list nodes are not included. |
| `XBUF` | `( u "name" -- )` | Allocate a persistent XMEM constant and advance the floor, or use `CREATE ALLOT` without XMEM. |

Free-list insertion is LIFO; allocation is first-fit, splits a block only when
at least one 16-byte node remains, and does not coalesce. Public XMEM
allocations carry an eight-byte total-size prefix. XMEM `RESIZE` allocates,
copies the lesser of the recorded old usable size and new request, and frees
the old block; allocation failure returns the original address and `-1`.

The current bounds checks are not an ownership proof. There is no live-block
ledger, alignment/overlap validation, or double-free detection. An interior
span can be returned and a repeated free can create a self-linked node. Public
`FREE` also treats every nonzero address at or above `MEM-SIZE` as XMEM and
reads the prefix first, so its contract is restricted to allocator results.
`XMEM-FLOOR` protects bulk reset only and is not checked by free insertion.
Raw XMEM allocation, free, alignment, and reset are unsynchronized and lack
the intended core-0 guard; XMEM `FREE` is likewise unguarded, while `RESIZE`
writes shared scratch before its nested allocation guard. These discrepancies
remain open and are not repaired by hosted execution.

`XBUF` allocation precedes constant publication and floor advancement, so a
dictionary fault in between can leak an unprotected block. Reset does not
revoke stale pointers. `XMEM-TALIGN` can cross a nonaligned configured limit.
The hosted and executable-emulator zero-size profiles mean absence, whereas
the RTL parameter value zero selects the maximum window up to VRAM; guest
words report each profile's actual SysInfo rather than normalizing this host
configuration difference.

Exact lines 2390 through 2423 then define `_DICT-POW2-FLOOR`,
`_DICT-INDEX-DONE`, and `_DICT-INDEX-INIT` and execute the one-shot
initializer. Canonical 128 MiB XMEM selects 65,536 slots (1 MiB); the table is
built newest-first and protected by advancing `XMEM-FLOOR`. Present capacity
below 2,048 bytes selects no table, while exactly 2,048 bytes selects one slot
and safely retains a saturated, linked-fallback state. The executable BIOS
implements `2/` as a logical shift despite its stale assembly comment calling
the operation arithmetic; this source uses only positive sizing values.

Index geometry proves neither allocator ownership nor disjointness, so callers
must reserve the supplied span exclusively. Rebuild clears it. Disable clears
the binding diagnostics but leaves old slot bytes, and the four values from
`DICT-INDEX@` are sequential BIOS loads rather than a coherent multicore
snapshot. KDOS also sets its DONE cell before allocation/install; an otherwise
unreachable status-1 rejection after allocation would consume the block,
leave the floor unchanged, disable retry, and abort.

The contiguous hosted frontier now includes the complete unchanged userland
and Arena sections plus Buffer's general `IDLE`, registry, constructors,
inspection, Arena integration, integer/FP16/BF16 operations, the kernel
registry and sample kernels, the pipeline engine, checked block-device and
bounded-volume objects, raw/MBR/GPT partition discovery, and the singleton
storage-compatibility, legacy file, and initial MP64FS cache/helper layers,
the MP64FS load/sync/ensure/format lifecycle, cached directory listing, exact
name lookup, metadata creation/deletion/rename, primary-extent file
publication, cache-only free-space reporting, primary-extent Buffer save/load,
the fixed FD pool with cached open/metadata-flush/final-close lifecycle, the
checked source compiler, nested two-extent filesystem `LOAD`, application
loader and ANSI helpers, whole-file encryption, and parent-byte directory
navigation/mutation through line 6296.
Their checked bounds, Bank-0/XMEM HERE transitions, cross-zone definitions,
allocator dispatch, descriptor lifecycle, snapshots, scoped stack, IDL
block/wake boundary, Buffer publication order, tile effects, storage identity,
guarded I/O, partition validation, transactional publication, selected-volume
lifecycle, diagnostic wrappers, permanent file descriptors, and composed
head/full/tail sector I/O, MP64FS cache geometry, bitmap mutation/search,
packed directory readers, raw-binding load, synchronization, conditional
autoload, metadata formatting, compact type publication, direct-child listing,
bitmap free-space reporting, exact-name lookup, deterministic timestamps,
ordered metadata mutation, byte-exact `CAT` output, global cached
fragmentation reporting, ordered Buffer/file transfers, descriptor allocation,
cached open snapshots, used-metadata flush, ordered close/release, source
evaluation/loading, encryption, and navigation/mutation are executable
semantic behavior rather than reporting-only shims. The frontier now ends at
line 6296; the Documentation Browser begins at line 6297.

---

### §1.15 Userland Memory Isolation

Provides separate dictionary space in external RAM for user-loaded modules
(`networking.f`, `tools.f`, and user scripts), protecting the core dictionary
in system RAM from overflow.  When `ENTER-USERLAND` is called, the Forth dictionary
pointer (`HERE`) is redirected to external memory.  All subsequent
`CREATE`, `ALLOT`, `:` definitions, `VARIABLE`s, etc. compile into the
userland zone.  System words remain accessible.

**Memory layout (ext mem present):**

| Region | Address Range | Contents |
|--------|--------------|----------|
| System RAM | `0x00000 .. HERE` | BIOS + KDOS core dictionary |
| System heap | cold aligned `HERE+32 KiB .. 0x7F000` | Explicit Bank 0 `DMA-ALLOCATE` / `DMA-FREE` blocks |
| Stacks | `0x80000 .. 0xFFFFF` | Data stack + return stack |
| BIOS dictionary index | `EXT-MEM-BASE .. index-end` | Permanent capacity-derived open-addressed table; 1 MiB/65,536 slots in the canonical 128 MiB arrangement |
| Other pre-init XMEM | `index-end .. U-DICT-BASE` | Persistent kernel objects and reclaimable loader buffers allocated before the partition |
| Userland dict | `U-DICT-BASE .. U-DICT-LIMIT` | User word definitions + data; inclusive base, exclusive limit |
| XMEM general | `U-DICT-LIMIT .. XMEM-LIMIT` | `XMEM-ALLOT` bump capacity plus safe reclaimed blocks below the dictionary base |

| Word | Stack | Description |
|------|-------|-------------|
| `ENTER-USERLAND` | `( -- )` | Save system HERE, redirect to userland dictionary zone. |
| `LEAVE-USERLAND` | `( -- )` | Save userland HERE, restore system dictionary pointer. |
| `ULAND` | `( -- addr )` | Variable: 0 = system mode, 1 = userland mode. |
| `U-HERE` | `( -- addr )` | Current userland dictionary pointer (even when in system mode). |
| `U-USED` | `( -- u )` | Bytes used in the userland dictionary. |
| `U-FREE` | `( -- u )` | Bytes remaining in the userland zone. |
| `.USERLAND` | `( -- )` | Display userland memory status. |
| `U-DICT-BASE` | `( -- addr )` | Variable containing the sealed inclusive dictionary base. |
| `U-DICT-LIMIT` | `( -- addr )` | Variable containing the sealed exclusive dictionary limit. |
| `U-ZONE-SIZE` | `( -- u )` | Derived size `U-DICT-LIMIT - U-DICT-BASE`. |
| `U-XMEM-RESERVE!` | `( u -- )` | Before initialization, request general-XMEM capacity rounded up to 16 bytes; zero selects the default half of remaining capacity. |

`USERLAND-INIT` aligns above the live XMEM high-water mark and derives the
partition from `XMEM-LIMIT`. The default splits the remaining capacity in
half; a positive `U-XMEM-RESERVE!` request is rounded to the allocator's
16-byte boundary and leaves the complementary span to the dictionary. Both
sides must remain nonempty, and the policy cannot change after initialization.
BIOS validates the candidate physical interval before KDOS publishes any
partition cell; `USERLAND-INIT` leaves that low-level bound disarmed until the
actual `ENTER-USERLAND` transition.

Before that partition, KDOS's one-shot index initializer reserves at most
1/128 of the virgin XMEM bump tail, rounded down to a power-of-two count of
16-byte slots. Reclaimed free-list nodes are not included in that sizing. It
uses checked allocation, advances `XMEM-FLOOR`, and leaves linked lookup active
if no table can be allocated. `XMEM-INIT` is itself one-shot; `XMEM-RESET`, not
reinitialization, is the supported allocator reset after boot.

The BIOS words `DICT-BOUNDS!`, `DICT-BOUNDS-OFF`, `DICT-BASE@`,
`DICT-LIMIT@`, and `DICT-FAULT-XT!` enforce the interval. They are the
low-level transition seam used by KDOS, not a second allocator API. Every
atomic HERE-growing emitter span and native `WORD` transient write preflights
its exact size. Exact fit is allowed; a wrap, overrun, or rewind below the base
changes neither that span's bytes nor its dictionary publication state.
Composite compiler words can contain several such spans, so checked source
owners still roll back their transaction checkpoint after a failure. Under a
surrounding `CATCH`, the KDOS fault hook throws standard code `-8` for either
Bank-0 or userland exhaustion; checked evaluation reports that as status 5 in
`EVAL-S-THROW`.

`MARKER`, `FORGET`, and checked compiler owners pass their saved pair to
`DICT-ROLLBACK`; direct stores to the private `var_latest` cell are unsupported.
Low-level owners that intentionally change only the dictionary head use the
coherent `LATEST!` word, which leaves `HERE` unchanged. The two-cell checkpoint
can reclaim only one contiguous active dictionary zone. If definitions made
after the checkpoint cross between Bank 0 and userland, rollback rejects the
pair before changing `HERE`, `LATEST`, the cache, or the index. Use separate
checkpoints on each side of a zone transition.

`XMEM-FREE-BLOCK` accepts only spans wholly below the current XMEM high-water
mark, then applies the dictionary-overlap check after initialization. This
prevents a forged pre-init free node from becoming later dictionary storage.

Several edge results follow directly from the current source and are not
normalized by the hosted backend. A failed partition calculation can retain
the `_U-AVAILABLE` scratch value while leaving all published partition cells
unchanged. Exotic high positive reserve rounding can cross the signed-cell
boundary and is then rejected by the signed minimum check. If the
hardware-reported external end is not 16-byte aligned, the derived dictionary
limit and the new XMEM HERE/floor inherit that misalignment; a 17-byte region
therefore passes with a one-byte dictionary and a 16-byte reserve. Before
initialization on a present-XMEM profile, `.USERLAND` prints zero base/limit
but reports `XMEM-LIMIT - 0` as “XMEM reserve,” which is the absolute external
end rather than available capacity. Treat that display as meaningful only
after successful initialization.

The transition cells are runtime-global and have no lock or `?CORE0` guard.
Their ordered stores are safe for valid, uncorrupted single-owner use but are
not a transaction against manual cell corruption or concurrent enter/leave.
The hosted one-core proof preserves that lifecycle and does not claim
multicore transition atomicity.

> **Important:** Do not call `ENTER-USERLAND` inside interpret-mode
> `IF … THEN`.  The BIOS clears temporary code between `var_interp_if_start`
> and the current `HERE` after execution; since `ENTER-USERLAND` moves `HERE`
> to ext mem, this clear loop would wipe system RAM.  Wrap the call in a
> colon definition instead: `: _GO  XMEM? IF ENTER-USERLAND THEN ; _GO`.

---

### §1.1b Arena Allocator

An Arena owns one preallocated backing span and advances a pointer inside it.
Its four-cell descriptor stores base, requested capacity, current pointer, and
source at offsets 0, 8, 16, and 24. `ARENA-NEW` appends that descriptor at the
active dictionary HERE; `ARENA-NEW-AT` writes the same cells into a
caller-supplied, writable, cell-aligned 32-byte span without advancing HERE.
Both return `ior`; callers that immediately define a constant must consume it.
For example, an application can define an interpretation-safe helper (this is
not a built-in KDOS word):

```forth
: MUST-ARENA  ( size source -- arena )
    ARENA-NEW ABORT" arena fail" ;
4096 A-XMEM MUST-ARENA CONSTANT work-arena
```

Writing `ARENA-NEW CONSTANT work-arena` is wrong: `CONSTANT` consumes the
topmost zero status and leaves the descriptor address on the data stack.
Putting `ABORT"` directly between those top-level words is also wrong because
`ABORT"` is compile-only; the checked helper must contain it.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ARENA-NEW` | `( size source -- arena ior )` | Allocate backing and append a permanent 32-byte dictionary descriptor. |
| `ARENA-NEW-AT` | `( desc size source -- ior )` | Allocate backing and publish into caller-owned descriptor storage. |
| `ARENA-ALLOT` | `( arena u -- addr )` | Round the requested length to eight bytes, bump the pointer, or abort on ordinary overflow/destruction. |
| `ARENA-ALLOT?` | `( arena u -- addr ior )` | Checked counterpart returning `(0,-1)` on ordinary overflow/destruction. |
| `ARENA-USED` / `ARENA-FREE` | `( arena -- u )` | Derive live accounting from descriptor cells. |
| `ARENA-RESET` | `( arena -- )` | Restore the pointer to base without wiping bytes. |
| `ARENA-DESTROY` | `( arena -- )` | Release reclaimable backing where supported and zero all descriptor cells. |
| `ARENA-SNAP` | `( arena -- snap )` | Return the current pointer as a bare token. |
| `ARENA-ROLLBACK` | `( arena snap -- )` | Replace the pointer after an inclusive descriptor-range check. |
| `ARENA-PUSH` / `ARENA-POP` | varied | Mutate the single four-entry current-arena stack. |
| `CURRENT-ARENA` | `( -- arena )` | Read the top entry or abort when the stack is empty. |
| `AALLOT` | `( u -- addr )` | Allocate through that selected descriptor. |
| `.ARENA` | `( arena -- )` | Print base, size, used, free, and source label. |

`A-HEAP` is the general reclaimable `ALLOCATE`/`FREE` route, not an invariant
Bank-0 route: it uses prefixed XMEM when external memory is present and Bank 0
otherwise. `A-XMEM` calls raw `XMEM-ALLOT?` and returns destroyed blocks to
the XMEM free list. `A-HBW` calls raw `HBW-ALLOT?`; destruction zeros only the
descriptor and leaves the bump span occupied until `HBW-RESET`. Zero size and
unknown source fail without a descriptor. Successful dictionary descriptors
remain committed after destruction; repeated temporary construction should
use `ARENA-NEW-AT`.

The normal allocation domain is a positive representable request no greater
than capacity. The current bump words do not prove that domain: wrapping
`7 + -8 AND` is followed by a signed `<` comparison. Cell patterns
`0xffff_ffff_ffff_fff9` through `0xffff_ffff_ffff_ffff` round to zero and
succeed without moving the pointer, while other sign-bit-set aligned values
can pass the signed comparison and wrap the pointer below base. HBW-backed
construction separately inherits the raw HBW high-cell wrap documented
above. These outcomes are defects reproduced by the hosted source, not an
unsigned-capacity contract.

Snapshot tokens carry no provenance. The rollback check admits any value in
`[base,base+size]`, including an unaligned or forward address that was never a
past pointer; it also admits token zero for a destroyed all-zero descriptor.
That interval is computed with wrapping addition and tested with signed
comparisons, so it describes the source behavior cleanly only for ordinary
low-half, nonwrapping descriptors. `ARENA-USED` and `ARENA-FREE` similarly
wrap rather than validate descriptor arithmetic.
Backing allocation occurs before four separate descriptor writes. A
dictionary-capacity failure can therefore leak a newly allocated span, and a
bad `ARENA-NEW-AT` destination can leak backing after partial descriptor
publication. Callers must preflight descriptor storage and dictionary
capacity themselves.

`AR-SZ`, `AR-SRC`, and `AR-BLK` are shared scratch, so construction and
destruction retain the source's core-0 guard. The `ARENA-STK` array and
`ARENA-SP` are also one runtime-global unsynchronized selection stack;
`CURRENT-ARENA` and `AALLOT` are not task-local despite the convenience API.
Separate owners can safely use direct `ARENA-ALLOT` only when they have
exclusive descriptors and coordinate backing lifecycle outside the worker.

Exact unchanged lines 2576 through 2780 contain 205 lines, 8,303 bytes, and
all 31 definitions. Hosted acceptance covers all three backing routes,
recycling/abandonment, dictionary and caller descriptors, alignment and exact
fit, ordinary failures, high-cell edges, reset, snapshot bounds, the scoped
stack, and `.ARENA`. Exact unchanged lines 2782 through 2796 add `IDLE`: `[` and
`]` interpret `0 C,` inside an open definition, and the emitted MP64 opcode
becomes a runtime-owned semantic IDL suspension rather than inert data or an
ordinary task yield. An exact one-shot interrupt/DMA receipt is required to
resume.

Exact unchanged lines 2797 through 2985 then add the complete linked Buffer
registry, four field readers, three ordinary constructors, byte sizing/fill,
inspection, and Arena integration. This 189-line, 7,191-byte slice is admitted
with SHA-256
`eb4d6d1bf072f854c667e86f428f49370bde4cd06e4770bd095d5f549906b2f1`.
Exact lines 2986 through 3109 publish seven definitions—six Buffer operations
plus `BTMP-NTILES` scratch—in 124
lines and 4,170 bytes, with SHA-256
`91d0fc5a15da85c31f9e4c4fcf17691c2bd32ba306b6b5bc338a7cf8b1ab96c4`.
Hosted qualification covers complete-tile integer effects and retains the
source defects documented in §3. Exact lines 3110 through 3216 then publish
the seven FP16/BF16 Buffer words in 107 lines and 2,869 bytes, with SHA-256
`cea60476207e132760c32cf2fb82773d6325d6d1895f0e7d73c40bf667b75065`.
Exact lines 3217 through 3754 add 109 kernel/pipeline definitions in 538 lines
and 16,586 bytes, with SHA-256
`ec724b8ca6f6887a2c4ce724edf9612726cf04a48416c29c2eb3ed9448949e40`.
They leave 23 kernels, three populated pipelines, and six load-time Buffers in
their ordinary registries. Exact lines 3755 through 4099 then add all 97
storage-object definitions through `VOL-FLUSH` in 345 lines and 11,424 bytes,
with SHA-256
`e4d09d0801838fc9721ba68e39f2c5a5dbc139101c9c4a3489fb66cab9b248b1`.
Exact lines 4100 through 4669 then add all 110 partition-discovery definitions
through `PART-SCAN` in 570 lines and 18,979 bytes, with SHA-256
`bf46ad3acc9deaf380ac4229fe9196219fc0111df8d8f5a6650ffa95fb766112`.
They implement raw fallback, MBR and dual-copy GPT validation, checked mode-4
CRC chaining, staged volume publication, and serialized public scanners. Line
4670 through 4803 then add all 24 singleton binding, compatibility I/O, Buffer
sector-I/O helper, and status-display definitions through `DISK-INFO` in 134 lines
and 4,127 bytes, with SHA-256
`7ba6cb19989623363d2e78ac45ae81b1b7e4bb2ad51864005bfbb35b1f768199`.
Load allocates the singleton bodies without explicitly clearing their extents;
virgin hosted memory supplies the zeros required by their first-construction
contract. It also creates zero-initialized diagnostic variables, points
`FS-VOLUME` at the still-invalid `SYSTEM-RAW-VOLUME`, and explicitly clears
`FS-OK`, all without touching storage. Exact lines 4804 through 5003 then add
all 38 file-abstraction definitions through `FILES` in 200 lines and 6,781
bytes, with SHA-256
`b022f3514605371f527a1e823b78ea26b5b09dad44198b4936272eaef1bb091b`.
Load initializes the registry count and scratch variables and allocates the
registry and sector scratch without executing `FILE`, touching media, or
printing. Exact lines 5004 through 5134 then add all 32 initial MP64FS cache,
geometry, bitmap, allocation-search, and packed directory definitions in 131
lines and 4,579 bytes, with SHA-256
`caf26787745bdf711a89130db7f8b30d45b0f9a63534b4ccb58a601bb2cea062`.
Load installs provisional 2,048-sector geometry, root `CWD`, zeroed scratch,
and cold-hosted cache storage without validating or touching media. Exact
lines 5135 through 5217 then add `FS-LOAD`, `FS-SYNC`, `FS-ENSURE`, and
`FORMAT` in 83 lines and 2,999 bytes, with SHA-256
`829268e2d06f11c19bda4a5fa0606e883fdf3ab4a3690a741f0cd2616ada4137`.
Loading those four definitions has no binding, I/O, flush, output, or
filesystem-state effect. Exact unchanged lines 5218 through 5285 then add
`.FTYPE`, `DIR`, and `CATALOG` in 68 lines and 2,167 bytes, with SHA-256
`c3c831bc183ee999c8b5a0d1fb4edd169890be1e5fa44ad726d3025923fdb3b7`.
Loading those three definitions installs only dictionary bodies and inline
strings, without binding, I/O, cache mutation, or output. Exact unchanged lines
5286 through 5408 then add five colon definitions through `RENAME` and six
zero-initialized scratch variables in 123 lines and 4,020 bytes, with SHA-256
`a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028`.
Loading that slice performs no clock read, parse, cache or media mutation,
sync, or output. Exact unchanged lines 5409 through 5436 then add `CAT-SLOT`
and `CAT` in 28 LF lines and 838 bytes, with SHA-256
`e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23`
and Git blob `2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5`. Load zero-initializes
`CAT-SLOT` and installs `CAT` and its inline strings without parsing, ensuring
the filesystem, touching cache or media, updating storage diagnostics, or
publishing output. Exact unchanged lines 5437 through 5471 then add `LF-BEST`,
`LF-RUN`, `FS-LARGEST-FREE`, and `FS-FREE` in 35 LF lines and 984 bytes, with
SHA-256
`6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c`
and Git blob `1884c81ba2b8aa48082d472250f13a2265fd1def`. Load zero-initializes the two
scratch variables and installs the two colon bodies and inline strings without
ensuring the filesystem, scanning bitmap or directory cache, touching media or
diagnostics, or publishing output. Exact unchanged lines 5472 through 5514
then add `SB-SLOT`, `SB-DESC`, `SAVE-BUFFER`, `LB-SLOT`, `LB-DESC`, and
`LOAD-BUFFER` in 43 LF lines and 1,317 bytes, with SHA-256
`7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104`
and Git blob `8b4645f16c7ac2f21036282a896b7ede6bad16b0`. Load zero-initializes the four
scratch variables and installs the two colon bodies and inline strings without
ensuring or parsing, dereferencing a Buffer, touching cache, media, or
diagnostics, flushing, or publishing output. Blank line 5515 leads into exact
unchanged lines 5515 through 5610. That 96-LF-line,
3,397-byte slice has SHA-256
`16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78`
and Git blob `e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9`. Its exact source-order
ledger is `FD-MAX`, `FD-SLOT-SZ`, `FD-POOL`, `FD-SLOT`, `FD-ALLOC`,
`(FCLOSE-NOFS)`, `FCLOSE`, `FD-FILL`, `OP-SLOT`, `(OPEN)`, `OPEN`, `F.SLOT`,
`FFLUSH`, and `(FCLOSE)`: 14 definitions. Load allocates and zero-fills the
1,152-byte pool, zero-initializes `OP-SLOT`, initially binds deferred `FCLOSE`
to `(FCLOSE-NOFS)`, binds deferred `OPEN` to `(OPEN)`, and finally rebinds
`FCLOSE` to `(FCLOSE)`. Those dictionary/allocation/vector mutations are its
only load effects; it does no filesystem or media I/O and emits nothing.
Subsequent exact fixtures qualify the checked compiler and filesystem loader,
application loader and ANSI helpers, filesystem encryption, and subdirectory
navigation through line 6296. Their provenance and edge contracts are recorded
in the corresponding sections below and in `docs/simulator-contract.md`; the
next uncovered seam is the Documentation Browser at line 6297.

---

## §2 Buffer Subsystem

Buffers are the core data container in KDOS. A buffer has a 4-cell (32-byte)
descriptor and a contiguous data span. Alignment depends on the constructor:
`BUFFER` and `HBW-BUFFER` align their data frontier to 64 bytes;
`ARENA-BUFFER` rounds only to the Arena allocator's eight-byte alignment; and
`XBUFFER` has the reclaimed-block defect described below. The registry is a
linked list with no fixed slot limit, rather than a 16-entry table.

### Buffer Descriptor Layout

```
Offset   Field         Meaning
───────  ────────────  ─────────────────────────────────────
+0       type          0=raw, 1=records, 2=tiles, 3=bitset
+8       elem_width    Bytes per element (1, 2, 4, or 8)
+16      length        Number of elements
+24      data_addr     Pointer to data region
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BUFFER` | `( type width length "name" -- )` | Append the descriptor and 64-byte-align the data at dictionary `HERE`, register it, then define *name* as a constant containing the descriptor address. |
| `HBW-BUFFER` | `( type width length "name" -- )` | Append the descriptor in the dictionary, 64-byte-align and allocate the data from HBW, register it, then define the descriptor constant. |
| `XBUFFER` | `( type width length "name" -- )` | Append the descriptor in the dictionary and request external-memory data. Its saved-pointer/free-list discrepancy is documented below. |
| `ARENA-BUFFER` | `( type width length arena "name" -- )` | Allocate both descriptor and data from *arena*, register the descriptor, and define its constant. Data is rounded to eight bytes, not necessarily tile-aligned. |
| `BUF-NTH` | `( n -- desc )` | Walk newest-first from `BUF-HEAD` and return the zero-based descriptor; *n* is not bounds-checked. |
| `B.TYPE` | `( desc -- type )` | Read the buffer type field. |
| `B.WIDTH` | `( desc -- width )` | Read the element width in bytes. |
| `B.LEN` | `( desc -- len )` | Read the element count. |
| `B.DATA` | `( desc -- addr )` | Read the data pointer. |
| `B.BYTES` | `( desc -- n )` | Total data size in bytes (length × width). |
| `B.TILES` | `( desc -- n )` | Number of 64-byte tiles needed to cover the data (ceiling division). |
| `B.FILL` | `( byte desc -- )` | Fill the entire buffer with a byte value. |
| `B.ZERO` | `( desc -- )` | Zero the entire buffer. |
| `B.INFO` | `( desc -- )` | Print a one-line summary: type, width, length, tiles, address. |
| `B.PREVIEW` | `( desc -- )` | Read exactly 64 bytes from `B.DATA` and print four rows of 16 values using the caller's current numeric `BASE`; it neither forces hexadecimal nor clips to `B.BYTES`. |
| `BUFFERS` | `( -- )` | Walk the registry newest-first and list each zero-based traversal index with `B.INFO`. |

**Variables:** `BUF-COUNT`, `BUF-HEAD` (head of the linked registry), `BDESC`,
`AB-AR`, and `AB-DESC` (shared constructor scratch). Every registration
allocates a 16-byte link node in the active dictionary. There is no configured
buffer-count ceiling; dictionary capacity is the practical bound.

**Example — creating and using a buffer:**
```forth
0 1 256 BUFFER my-signal       \ raw, 1 byte/elem, 256 elements
42 my-signal B.FILL             \ fill every byte with 42
my-signal B.INFO                \ prints descriptor summary
HEX my-signal B.PREVIEW DECIMAL \ show the fixed 64-byte preview in hex
BUFFERS                         \ list all registered buffers
```

The constructors publish through a sequence of ordinary writes and
allocations; they are not transactions. They do not validate the documented
type/width conventions, and length-times-width uses wrapping cell arithmetic.
A capacity or name-definition failure can leave descriptor cells, consumed
data capacity, or a registered link/count without the requested constant.
`B.BYTES` inherits that multiplication; `B.TILES` then adds 63 with wrapping
cell arithmetic and applies signed `/` by 64. Its ceiling result is meaningful
only in the ordinary nonnegative, nonoverflowing size domain. `B.FILL` and
`B.ZERO` operate on exactly `B.BYTES`; `B.ZERO` is the scalar `FILL` path, not
the tile engine.

`XBUFFER` aligns `XMEM-HERE`, saves that bump pointer into `data_addr`, then
calls `XMEM-ALLOT` and discards the address it returns. The values coincide on
the bump path. If the allocator instead satisfies the request from its free
list, the descriptor still points at the bump frontier while the reclaimed
block returned by `XMEM-ALLOT` is consumed and lost. This is an open source
defect, not a simulator substitution.

The redefined `ARENA-DESTROY` walks the registry and unlinks descriptors whose
addresses fall inside the Arena backing interval. It decrements `BUF-COUNT`
but cannot reclaim the dictionary link nodes, and the named constant remains
defined with the old descriptor address after backing destruction. Thus
automatic unregistration prevents normal enumeration of the dead descriptor;
it does not make the name safe to use or provide complete object reclamation.
`ARENA-BUFFER` and registration also use shared scratch/global list state and
are not a concurrent task-local publication path. `ARENA-RESET` does not
unregister anything: it makes descriptor/data storage eligible for reuse while
the old list entries and constants remain live. Dictionary rollback is also
unaware of the registry, so `MARKER`, `FORGET`, or numeric rollback past a
published link/name can leave `BUF-HEAD` and `BUF-COUNT` pointing at reclaimed
dictionary history.

---

## §3 Tile-Aware Buffer Operations

Five words use the **MEX tile engine** (hardware SIMD) one 64-byte tile at a
time, through `TSRC0!`, `TSRC1!`, `TDST!`, `TSUM`, `TMIN`, `TMAX`, `TADD`,
and `TSUB`. `B.SCALE` is instead an ordinary scalar Forth loop. Every
tile-backed word unconditionally selects mode `0` (8-bit unsigned, 64 lanes);
the descriptor width affects `B.BYTES` and `B.TILES`, not tile lane width.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `B.SUM` | `( desc -- n )` | Sum every physical lane in the rounded-up tile span with `TSUM`; a partial logical tile includes its trailing bytes. |
| `B.MIN` | `( desc -- n )` | Reduce one tile correctly. The current multi-tile source has the address defect described below. |
| `B.MAX` | `( desc -- n )` | Reduce one tile correctly. The current multi-tile source has the address defect described below. |
| `B.ADD` | `( src1 src2 dst -- )` | Wrapping unsigned-byte `TADD` over the tile count taken from `src1`; complete destination tiles are written. |
| `B.SUB` | `( src1 src2 dst -- )` | Wrapping unsigned-byte `TSUB` over the tile count taken from `src1`; complete destination tiles are written. |
| `B.SCALE` | `( n desc -- )` | Scale exactly `B.BYTES` bytes with scalar `C@`/`C!`; `255 AND` wraps each product modulo 256 rather than clamping it. |

These words do not validate descriptor types, widths, equal source/destination
sizes, or logical-tail padding. `B.ADD` and `B.SUB` can read beyond a shorter
source and overwrite bytes after a partial destination. `BTMP-NTILES` is shared
global scratch. `B.MIN` and `B.MAX` retain `(next-address running-extreme)`
after their first iteration, but the next `DUP TSRC0!` consumes the extreme as
the address. Multi-tile calls therefore reduce a low Bank-0 address in later
iterations rather than the next data tile.

`B.MIN` and `B.MAX` explicitly return zero for an empty buffer. The other four
words use `0 DO`; when their byte/tile count is zero, the loop executes and
cannot complete normally until the 64-bit index wraps, although an invalid
memory access can fault first. Empty calls are not
zero-trip safe. These are current source behaviors, not requirements for a
future corrected Buffer API.

**Example — tile-accelerated statistics:**
```forth
my-signal B.SUM .    \ print the sum of all bytes
my-signal B.MIN .    \ one physical tile is currently the safe domain
my-signal B.MAX .    \ one physical tile is currently the safe domain
```

### §3.1 FP16/BF16 Buffer Operations

These seven words interpret each physical 64-byte tile as 32 little-endian
half-format lanes. They do not inspect `B.TYPE` or use `B.WIDTH` to choose a
format: the word itself installs TMODE 4 or 5. Reduction results are raw IEEE
binary32 bits in ACC0, returned as an ordinary Forth cell by `ACC@`; they are
not converted to a decimal or fixed-point Forth number.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `F.SUM` | `( desc -- fp32-bits )` | Sum complete FP16 tiles and return the binary32 encoding. |
| `F.DOT` | `( src1 src2 -- fp32-bits )` | FP16 dot product over the leftmost `src1` descriptor's tile count. |
| `F.SUMSQ` | `( desc -- fp32-bits )` | Sum FP16 lane squares and return the binary32 encoding. |
| `F.ADD` | `( src1 src2 dst -- )` | Add FP16 lanes into complete destination tiles. |
| `F.MUL` | `( src1 src2 dst -- )` | Multiply FP16 lanes into complete destination tiles. |
| `BF.SUM` | `( desc -- fp32-bits )` | Sum complete BF16 tiles and return the binary32 encoding. |
| `BF.DOT` | `( src1 src2 -- fp32-bits )` | BF16 dot product over the leftmost `src1` descriptor's tile count. |

All seven inherit `B.TILES` rounding. A partial logical tail participates in a
reduction and is overwritten by F.ADD/F.MUL. With ordinary `BUFFER`, those
physical bytes can be the following registry link or dictionary data rather
than reserved padding. Two-input operations derive their loop count only from
the leftmost stack argument named `src1`, which is loaded into hardware TSRC0;
descriptor type, width, whether `B.BYTES` is even, and the other lengths are
unchecked.
The source example `0 1 64 BUFFER myfp16` allocates the right 64 bytes but its
descriptor says 64 one-byte elements. `0 2 32 BUFFER myfp16` describes 32
two-byte elements under the documented descriptor model.

Every zero-count loop uses `0 DO` and therefore enters rather than completing
normally before index wrap; it may fault first. Normal return resets TMODE to
hard-coded zero instead of restoring the caller's mode, and reductions leave
TCTRL at one. A tile-loop memory fault or budget fault before the final
`0 TMODE!` leaves FP16/BF16 mode active.

The hosted path follows the decoded Python emulator while the legacy FP
contract is unresolved. Python/hosted SUM and SUMSQ use the host `sum`
algorithm for one tile and then pack once to binary32; the native accelerator
currently falls back to that path, while its direct C++ body is sequential
binary32 and RTL uses a balanced binary32 tree. Cancellation can differ. The
Python and active native TDOT paths instead use an explicit binary64 loop
before one binary32 pack, while RTL again uses its own tree. The
ACC_ACC path widens the existing binary32 ACC0, adds it to the tile subtotal
in binary64, and repacks; that pack is the inter-tile rounding point. The
executable FP16 encoder also maps the exact product `0x0017 * 0x5190` to zero
where IEEE round-to-even would carry into minimum-normal `0x0400`. These are
recorded discrepancies, not KDOS requirements.

---

## §4 Kernel Registry

A "kernel" in KDOS is a **compute function** (an ordinary Forth colon
word) paired with a **metadata descriptor** that records its input/output
requirements and hardware acceleration status.  Up to **32 kernels** can
be registered.

### Kernel Descriptor Layout

```
Offset   Field         Meaning
───────  ────────────  ─────────────────────────────────────
+0       n_inputs      Number of input buffers expected
+8       n_outputs     Number of output buffers produced
+16      footprint     Estimated tile working set
+24      flags         0 = CPU only, 1 = tile-accelerated
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KERNEL` | `( n_in n_out footprint flags "name" -- )` | Register a new kernel.  Creates a descriptor and a CONSTANT.  The actual kernel body is a separate colon definition — this just records metadata. |
| `K.IN` | `( desc -- n )` | Number of input buffers. |
| `K.OUT` | `( desc -- n )` | Number of output buffers. |
| `K.FOOT` | `( desc -- n )` | Tile footprint. |
| `K.FLAGS` | `( desc -- flags )` | Flags (0=CPU, 1=tile). |
| `K.INFO` | `( desc -- )` | Print kernel descriptor details. |
| `KERNELS` | `( -- )` | List all registered kernels. |

**Variables:** `KERN-COUNT`, `KERN-TABLE` (32-slot registry), `KDESC` (internal temp).

The limit is a literal source table, not a checked allocator contract. Once 32
entries are present, `KERNEL` still allocates its descriptor and defines the
named constant but silently omits it from `KERN-TABLE`. There is no unregister
or reclamation path, and shared `KDESC` makes construction non-reentrant.

---

## §5 Sample Kernels

KDOS ships with **18 ready-to-use compute kernels** covering common
signal-processing and data-analysis tasks.  Each kernel is a callable
Forth word, plus a descriptor constant (named `<kernel>-desc`).

### Zero & Fill

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kzero` | `( desc -- )` | Zero an entire buffer. | No |
| `kfill` | `( byte desc -- )` | Fill buffer with a byte value. | No |

### Arithmetic

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kadd` | `( src1 src2 dst -- )` | Element-wise add two buffers → dst.  `dst[i] = src1[i] + src2[i]`. | **Yes** |
| `kscale` | `( n desc -- )` | Multiply every byte by *n* in-place. | No |
| `kinvert` | `( desc -- )` | Bitwise invert: every byte → `255 − val`. | No |

### Statistics & Measurement

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `ksum` | `( desc -- n )` | Sum all bytes, return on stack. | **Yes** |
| `kstats` | `( desc -- sum min max )` | Compute sum, minimum, and maximum in one pass. | **Yes** |
| `kcount` | `( val desc -- count )` | Count bytes matching a specific value. | No |
| `krms-buf` | `( desc -- rms )` | Compute the integer RMS (root mean square) using Newton's method for the square root.  8 iterations. | No |
| `kcorrelate` | `( a b -- dot )` | Dot product of two buffers via tile engine `TDOT`. | **Yes** |

### Signal Processing

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kthresh` | `( n desc -- )` | Threshold: bytes < n → 0, bytes ≥ n → 255.  Binary binarization. | No |
| `kclamp` | `( lo hi desc -- )` | Clamp all bytes to the range [lo, hi]. | No |
| `kavg` | `( window desc -- )` | Moving average with the given window size (simplified single-pass). | No |
| `kdelta` | `( src dst -- )` | Delta encoding: `out[i] = in[i] − in[i−1]` (first element = 0). | No |
| `knorm` | `( desc -- )` | Normalize buffer to full 0–255 range using tile min/max. | **Yes** |
| `kpeak` | `( thresh src dst -- )` | Peak detector: write 255 at local maxima ≥ threshold, 0 elsewhere. | No |
| `kconvolve3` | `( c0 c1 c2 desc -- )` | 3-tap FIR filter `[c0, c1, c2]` applied in-place, with edge replication. | No |

### Histogram

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `khistogram` | `( desc -- )` | Build a 256-bin histogram of all byte values into `hist-bins`. | No |
| `HIST@` | `( v -- count )` | Query histogram bin for byte value *v*. | — |
| `.HIST` | `( -- )` | Print all non-zero histogram bins. | — |

**Scratch buffers:** `mavg-scratch` (256 bytes), `hist-bins` (256×8-byte bins), `conv-scratch` (256 bytes).

The current executable source has important differences from several intended
descriptions above. `kavg` records its window but only copies the input through
`mavg-scratch`; it performs no averaging. `kdelta` initializes its previous
value to zero, so the first result is `src[0]`, not zero. `kpeak` produces the
documented result for byte counts of at least three, but for shorter buffers it
zeroes the destination and then executes one excess `DROP`, causing stack
underflow. The registered `krms-buf` divides by zero when mean square is one,
and eight Newton iterations do not produce the exact RMS over the entire byte
domain; the separate unused `krms` loses its descriptor before `B.BYTES`.
`kavg` and `kconvolve3` copy an unchecked byte count through fixed 256-byte
scratch, so larger buffers overwrite following dictionary state. `khistogram`
uses one global result buffer. These behaviors are source discrepancies, not
simulator replacements.

Unguarded `0 DO` also appears in `kthresh`, `kclamp`, `kavg`, `khistogram`,
`kdelta`, both RMS words, `kcorrelate`, `kconvolve3`, `kinvert`, and `kcount`.
Their zero-sized domains enter the loop and do not complete normally before
index wrap or another fault. Tile wrappers additionally inherit the Buffer
tail, descriptor-count, and multi-tile behaviors documented in §3.

**Example — basic signal analysis:**
```forth
0 1 256 BUFFER sensor-data      \ create a 256-byte buffer
\ ... fill with data ...
sensor-data kstats              \ leaves sum min max on stack
." Sum=" . ."  Min=" . ."  Max=" . CR
128 sensor-data kthresh          \ binarize: < 128 → 0, ≥ 128 → 255
sensor-data khistogram           \ build histogram
.HIST                            \ show non-zero bins
```

---

## §6 Pipeline Engine

A pipeline is an **ordered sequence of execution tokens** (XTs) that run
in series.  Think of it as a batch macro: chain several kernel calls
together, then run or benchmark the whole sequence with one word.
Up to **8 pipelines** can be registered.

### Pipeline Descriptor Layout

```
Offset   Field      Meaning
───────  ─────────  ─────────────────────────────────────
+0       capacity   Maximum number of steps
+8       count      Current number of steps
+16      steps[]    Array of execution tokens
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PIPELINE` | `( capacity "name" -- )` | Create a new pipeline with room for *capacity* steps.  Defines a CONSTANT. |
| `P.CAP` | `( pipe -- n )` | Read capacity. |
| `P.COUNT` | `( pipe -- n )` | Read current step count. |
| `P.DATA` | `( pipe -- addr )` | Address of the step array. |
| `P.GET` | `( pipe n -- xt )` | Get the execution token of step *n*. |
| `P.SET` | `( xt pipe n -- )` | Set step *n* to *xt*. |
| `P.ADD` | `( xt pipe -- )` | Append a step, incrementing the count. |
| `P.CLEAR` | `( pipe -- )` | Reset to zero steps. |
| `P.RUN` | `( pipe -- )` | Execute all steps in order. |
| `BENCH` | `( xt -- cycles )` | Time a single word's execution using the cycle counter.  Returns elapsed cycles. |
| `.BENCH` | `( xt -- )` | Time a word and print `cycles=N`. |
| `P.BENCH` | `( pipe -- )` | Execute and individually time each pipeline step. |
| `P.INFO` | `( pipe -- )` | Print pipeline descriptor details. |
| `PIPES` | `( -- )` | List all registered pipelines. |

Pipeline checks are similarly minimal. Once eight registry slots are full,
`PIPELINE` still allocates and defines its constant but silently omits the
entry. `P.ADD` silently drops an XT at capacity; `P.GET` and `P.SET` do no
bounds checking. `P.CLEAR` resets only the count, so old XT cells remain
readable and can be exposed again by a later count change. Capacity/count
corruption, including negative cells, is not rejected, and construction uses
shared `PDESC`, `P-XT`, and `P-PIPE` scratch.

On the machine, `BENCH` reads the intended wrapping 32-bit Timer COUNT through
`CYCLES`. The hosted profile instead returns the low 32 bits of a separate
per-runtime semantic-work clock, unaffected by `PERF-RESET`; it makes execution
order measurable but is not MP64 timing. Current RTL SoC Timer wiring exposes
only `COUNT_LO` to `CYCLES` and accepts only `COMPARE_LO` from `TIMER!`, while
emulator/native provide the intended 32-bit accesses. This remains an explicit
backend discrepancy.

### Demo Pipelines

KDOS ships with three pre-built demo pipelines:

| Pipeline | Steps | What It Does |
|----------|-------|-------------|
| `pipe-fill-sum` | 2 | Fill `demo-a` with 42, then sum and print. |
| `pipe-add-stats` | 3 | Fill `demo-a`=10 and `demo-b`=20, add them into `demo-c`, print stats. |
| `pipe-thresh` | 3 | Fill `demo-a` with a ramp 0..63, threshold at 32, print stats. |

**Example — building a custom pipeline:**
```forth
8 PIPELINE my-pipe

' my-init  my-pipe P.ADD    \ step 0: initialize
' my-proc  my-pipe P.ADD    \ step 1: process
' my-report my-pipe P.ADD   \ step 2: report results

my-pipe P.RUN               \ run all three steps
my-pipe P.BENCH             \ run and time each step
```

---

## §7 Storage & Persistence

Low-level disk access built on the BIOS disk words.  Provides
buffer-to-disk save/load using sector-based I/O.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DISK?` | `( -- flag )` | True if a storage device is attached (checks bit 7 of `DISK@`). |
| `B.SECTORS` | `( desc -- n )` | Number of 512-byte sectors needed to store this buffer's data. |
| `B.SAVE` | `( desc sector -- )` | Save buffer data to disk starting at the given sector. |
| `B.LOAD` | `( desc sector -- )` | Load buffer data from disk starting at the given sector. |
| `DISK-INFO` | `( -- )` | Print whether storage is present or not. |

**Constant:** `SECTOR` = 512 (bytes per sector).

The production object layer sits between those compatibility wrappers and the
checked BIOS disk words. A block device captures one attachment generation;
a volume is either the raw identity view or a validated half-open slice. The
currently qualified hosted frontier includes these public words unchanged:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BD-OPEN` | `( bd -- ior )` | Bind one caller-owned 128-byte descriptor to the current attachment. |
| `BD-CLOSE` | `( bd -- ior )` | Clear an unreferenced descriptor; report busy while a volume refers to it. |
| `BD-VALID?` / `BD-STALE?` | `( bd -- flag )` | Check permanent structure or current attachment identity. |
| `BD-READ` / `BD-WRITE` | `( dma lba count bd -- completed ior )` | Perform generation-bound checked I/O and retain submitted-operation diagnostics. |
| `BD-FLUSH` | `( bd -- ior )` | Perform a generation-bound checked flush. |
| `VOL-RAW` | `( bd vol -- ior )` | Construct the identity slice over the complete block device. |
| `VOL-SLICE` | `( base length scheme index bd vol -- ior )` | Transactionally replace a caller-owned 144-byte descriptor with a validated bounded slice. |
| `VOL-CLOSE` | `( vol -- ior )` | Clear a volume and release its parent reference. |
| `VOL-VALID?` / `VOL-STALE?` | `( vol -- flag )` | Check volume structure, parent cookie/bounds, or the complete generation chain. |
| `VOL-READ` / `VOL-WRITE` | `( dma lba count vol -- completed ior )` | Validate a relative request and translate it through the parent block device. |
| `VOL-FLUSH` | `( vol -- ior )` | Flush through the validated parent. |

The admitted compatibility layer binds ordinary KDOS storage users to one
selected volume:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `STORAGE-OPEN` | `( -- ior )` | Destructively replace `SYSTEM-BD` and `SYSTEM-RAW-VOLUME` with a raw view of the current attachment. |
| `FS-VOLUME!` | `( vol -- ior )` | Select a valid, current caller-owned volume without acquiring a reference; success clears `FS-OK`. |
| `STORAGE-ENSURE` | `( -- ior )` | Validate the selected volume, opening the singleton only for an invalid selection whose cache marker is already clear. |
| `_RAW-DISK-READ?` / `_RAW-DISK-WRITE?` | `( dma lba count -- flag )` | Checked raw-controller compatibility I/O with global result diagnostics. |
| `_RAW-DISK-FLUSH?` | `( -- flag )` | Checked raw-controller flush with global result diagnostics. |
| `_DISK-READ?` / `_DISK-WRITE?` | `( dma lba count -- flag )` | Checked selected-volume I/O with global result diagnostics. |
| `_DISK-FLUSH?` | `( -- flag )` | Checked selected-volume flush with global status and ior diagnostics. |
| `_DISK-READ` / `_DISK-WRITE` | `( dma lba count -- )` | Abort on a false checked result while retaining diagnostics. |
| `_DISK-FLUSH` | `( -- )` | Abort on a false checked flush while retaining diagnostics. |

`STORAGE-OPEN` attempts `VOL-CLOSE` and then `BD-CLOSE`, discards both results,
and only then attempts `BD-OPEN`; it is destructive and nontransactional. For
example, an extra live volume reference leaves that volume and the block
descriptor valid, clears the singleton raw volume, and makes the subsequent
open return `BD-E-BUSY`. Nothing restores the old raw binding. `STORAGE-OPEN`
also does not clear `FS-OK`: direct callers must invalidate filesystem cache
state before rebinding. The now-admitted `FS-LOAD` and `FORMAT` paths do that
themselves. `FS-VOLUME!` borrows rather than owns its selection, so the caller
must keep that volume alive. A rejected selection leaves the previous pointer
and cache marker unchanged.

`STORAGE-ENSURE` deliberately fails closed when invalid storage is paired with
a nonzero `FS-OK`: it clears the marker, returns `VOL-E-STALE`, and does not
auto-open until a later call. A structurally valid but stale selected volume
returns stale on every call until explicit replacement or reselection. These
singleton management operations, the selected-volume lifetime, and their
global diagnostics are not internally serialized.

The range predicate is unsigned and subtraction-based: count must be nonzero,
`count <= length`, and `lba <= length - count`. Bad descriptor precedes stale,
which precedes range for ordinary checks. A valid descriptor's saved read-only
flag is intentionally checked before stale/range/DMA for writes. Early
software errors leave prior block diagnostics untouched; submitted read/write
results replace ior, completed, LBA, and count, while submitted flush replaces
only ior and completed.

Descriptors are caller-owned lifecycle storage, not copyable values. Their
complete extents must be writable and nonoverlapping, and must begin zeroed or
as that caller's original live object. Copying or forging a live descriptor
can unbalance the block reference count. Cookies and constructor scratch are
global, wrapping, non-atomic KDOS state, and the structural validators do not
preflight the descriptor span itself. See the normative
[block/volume contract](block-volume-contract.md) for layouts, structured ior
fields, and lifetime rules.

`DISK-IO-STATUS`, `DISK-IO-COMPLETED`, and `DISK-IO-IOR` retain the last
compatibility result. Read and write wrappers publish all three values;
nonzero ior takes precedence, while a zero-ior short completion is converted
to raw status 14 plus `BD-E-INTERNAL` without losing the actual completed
count. Flush wrappers replace status and ior but intentionally preserve the
previous completed count. Stale read, write, and selected-volume flush results
clear `FS-OK`; `_RAW-DISK-FLUSH?` does not. Aborting wrappers do not roll back
partial DMA or media effects, and diagnostics remain readable after the abort.
Concurrent calls can expose a mix of fields from different operations rather
than a coherent diagnostic snapshot.

`B.SECTORS` is admitted as the pure sector-rounding helper. `B.SAVE` and
`B.LOAD` are admitted only when the Buffer has complete rounded backing: they
pass `B.DATA` directly as DMA storage, while ordinary Buffer constructors
reserve logical bytes rather than the complete sector tail. A
non-sector-multiple save reads and load writes up to 511 adjacent bytes. A
zero-byte Buffer produces a zero-sector request and aborts through the checked
wrapper. Use an exact sector payload or separately prove that caller-owned
backing includes the rounded tail. `B.SAVE` does not flush and is not a
durability boundary. These discrepancies are retained and documented rather
than hidden by a simulator-only buffer reservation.

`DISK-INFO` samples ambient `DISK?` presence only. It neither opens nor
validates the selected binding and says nothing about capabilities, staleness,
`FS-OK`, or durability.

---

## §7.5 File Abstraction

A legacy contiguous-file layer predating the named filesystem (§7.6). Each
`FILE` permanently compiles a dictionary descriptor and a named constant. Its
start sector is relative to whichever `FS-VOLUME` is selected when an operation
runs; the descriptor captures no volume identity, generation, or ownership.
Changing the selected volume therefore redirects every legacy descriptor.

### File Descriptor Layout

| Offset | Field | Meaning |
|---:|---|---|
| +0 | start sector | Relative LBA in the current selected volume. |
| +8 | declared maximum sectors | Logical bound only; `FILE` allocates and reserves no sectors. |
| +16 | used bytes | Logical end-of-file metadata, which may be grown without writing. |
| +24 | cursor | Unchecked current byte offset. |

There is no open/close state. `FILE-TABLE` is only an eight-pointer display
registry: the ninth and later `FILE` descriptors and constants remain usable
but are silently omitted from `FILES`. These permanent four-cell descriptors
are not the later MP64FS pool objects and must never be passed to `FCLOSE`,
which interprets memory before and beyond its argument using a different
layout.

`FSCRATCH` exposes the one-sector working span used for partial I/O. Its source
form (`VARIABLE` plus 511 `ALLOT` bytes) actually reserves 519 bytes, but only
the first 512 form the operational scratch sector.

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FILE` | `( start_sector max_sectors "name" -- )` | Permanently compile a four-cell descriptor and a constant; allocate, reserve, and validate no media. |
| `FSEEK` | `( pos fdesc -- )` | Store an unchecked cursor value. |
| `FREWIND` | `( fdesc -- )` | Reset cursor to 0 (start of file). |
| `FSIZE` | `( fdesc -- n )` | Return the used byte count. |
| `FTRUNCATE` | `( n fdesc -- )` | Change RAM metadata to `min(n, max*512)` and clamp the cursor; may grow or shrink and performs no I/O. |
| `FWRITE` | `( addr len fdesc -- )` | In the admitted ordinary domain, write at the cursor and update cursor/used after all stages succeed. Out of space only prints a message. |
| `FREAD` | `( addr len fdesc -- actual )` | In the admitted ordinary domain, read at most the available logical bytes and advance by the returned count. |
| `F.INFO` | `( fdesc -- )` | Print file descriptor summary. |
| `FILES` | `( -- )` | List indexed summaries for the first eight registered descriptors, not every `FILE`. |

The admitted I/O domain requires ordinary nonnegative values no greater than
`INT64_MAX`; nonwrapping `max_sectors*512`, `cursor+len`, and relative-sector
arithmetic; `used <= capacity`; complete ordinary-RAM caller spans; protected,
disjoint descriptor and global scratch storage; and a declared file extent
contained in the currently selected volume. In that domain, `FWRITE` preserves
partial head/tail surroundings with read-modify-write through `FSCRATCH`,
`FREAD` serves partial head/tail bytes by scratch read/copy, and whole middle
sectors transfer directly. In-bounds zero-length operations are no-ops, and
`FREAD` returns zero at or beyond logical EOF.

The implementation does not establish those preconditions. `FILE` permits
file extents to overlap one another or selected-volume filesystem/partition
metadata, and it accepts out-of-volume geometry. `FWRITE` performs only a
wrapping `cursor+len > max*512` check using signed `>`; an ordinary nonwrapping
out-of-capacity request prints `FWRITE: out of space` and returns normally
without an ior or flag. `FREAD` uses signed `<` for its EOF guard. Both paths
then use the currently executable unsigned `MIN`/`MAX`, whose signed public
contract remains unresolved. High-bit and wrapping inputs are therefore
unqualified; the current `FTRUNCATE -1` behavior, for example, clamps to
capacity rather than resolving what the public signedness should mean.

Descriptor fields are per-object, but construction/truncate use shared
`FDESC`/`FT-N` and I/O uses shared `FW-*`/`FR-*` plus `FSCRATCH`; the layer is
unlocked and non-reentrant. Per-sector checked locking does not make a
partial-sector read-modify-write or a multi-sector operation atomic. A later
storage abort may leave earlier media writes or destination bytes committed
while descriptor cursor/used metadata remains unchanged. Seeking past EOF
before a write and growing with `FTRUNCATE` expose unchanged old media in
logical holes. No operation allocates media, automatically zero-fills holes or
bytes exposed by truncate/growth, flushes, prevents overlap, or persists
descriptor metadata. Raw accessors and publishers do not validate their
descriptor pointers, and `F.INFO` reads fields sequentially rather than as a
coherent snapshot.

---

## Checked source compilation

`SOURCE-EVALUATE-CHECKED ( addr len -- status )` is the whole-buffer
compiler entry point for hosted editors and build tools.  It splits the
buffer on LF, strips an optional CR, assigns one-based line numbers, and
calls KDOS's checked evaluator for each non-empty physical line.  It stops at
the first failure, so neither later tokens on that line nor later lines are
executed.  At end-of-buffer it checks for an unfinished definition or
cross-line conditional.

| Constant | Value | Meaning |
|----------|-------|---------|
| `EVAL-S-OK` | 0 | Success |
| `EVAL-S-UNDEFINED` | 1 | Undefined token |
| `EVAL-S-LINE-TOO-LONG` | 2 | Physical line is longer than 255 bytes |
| `EVAL-S-DEPTH` | 3 | Evaluator nesting limit exceeded |
| `EVAL-S-UNFINISHED` | 4 | End of buffer left compiler state unfinished |
| `EVAL-S-THROW` | 5 | A nonzero source-level `THROW` was caught; inspect `EVAL-THROW @` |

On failure, read `EVAL-STATUS @`, `EVAL-LINE @`, `EVAL-COLUMN @`, and
`EVAL-TOKEN` for token diagnostics, and `EVAL-THROW @` for the exact exception
behind status 5.  Lines are one-based and columns are zero-based.
The token is a stable copy and remains valid after input-source restoration.

BIOS defines an early `EVALUATE-CHECKED` primitive returning statuses 0–3.
It cannot own `CATCH`, because KDOS later supplies the execution-context-local
`HANDLER` table.  Immediately above this whole-buffer compiler, KDOS
deliberately shadows that BIOS word with the same public name.  The KDOS
wrapper checkpoints `EVAL-DEPTH @`, runs legacy `EVALUATE` under `CATCH`, and
on a source exception invokes `EVALUATOR-UNWIND`, stores the code in
`EVAL-THROW`, and returns status 5 normally.  BIOS evaluator frames retain a
complete caller TIB, length, and `>IN`, so normal nesting and caught unwinds
both restore the exact caller source.  Status 5 remains sticky while enclosing
evaluators unwind, preventing either the failed source tail or an enclosing
line tail from executing.

Dictionary changes are not automatically transactional. A caller that
snapshots `HERE` and `LATEST` must pass that pair to `DICT-ROLLBACK` on failure,
then call `EVALUATOR-RESET`. That order is intentional: reset clears compiler
bookkeeping but does not move the dictionary pointers or disturb an enclosing
`EVALUATE` frame. The last status and diagnostics survive reset so the UI can
present them afterward.
Like `EVALUATE`, source-level data-stack effects are preserved.

## §7.6 MP64FS Filesystem

The **MP64FS** is a simple on-disk named filesystem with one draft format
marker and uniformly derived geometry through 65536 sectors (32 MiB).  It
supports 128 entries, 23-character names, and two extents per file.  See
`docs/filesystem.md` for the full on-disk format specification.

### Key Concepts

- **Superblock** (sector 0) — magic number `"MP64"`, marker, geometry
- **Bitmap** (starting at sector 1) — one bit per sector; count is
  `ceil(total_sectors / 4096)`
- **Directory** (the next 12 sectors) — 128 entries × 48 bytes each
- **Data area** — begins immediately after the derived directory

### Hosted Filesystem Frontier

The native hosted `MP64FS-VALID?` returns literal `1` or `0` after up to three
raw checked reads and the executable BIOS's narrow geometry/metadata
predicate. Ordinary admitted `FS-LOAD` now exercises it before publishing
KDOS caches. It still does not select a KDOS volume or make its reads a
coherent same-image content snapshot.

The hosted simulator continuously executes the unchanged source through
`kdos.f` line 6296. The foundation through line 5134 allocates `FS-SUPER`,
`FS-BMAP`, and `FS-DIR`; installs provisional `FS-TOTAL = 2048`,
`FS-BMAP-N = 1`, and root `CWD = 255`; and publishes the geometry, bitmap,
first-fit, and packed-entry helpers. It performs no storage I/O or validation
and leaves `FS-OK = 0`. Those defaults and cold-cache bytes are construction
state, not a claim that a filesystem is mounted. The three
`VARIABLE ... ALLOT` declarations each reserve seven bytes beyond their 512-,
8192-, and 6144-byte operational windows; the source does not explicitly
clear the `ALLOT` tails.

Exact unchanged lines 5135–5217 add the four lifecycle definitions in 83
lines and 2,999 bytes. Loading them has no side effects; focused execution
qualifies raw-binding load, ordered cache synchronization, conditional
autoload, and metadata-only formatting on pathless in-memory media.

Exact unchanged lines 5218–5285 add `.FTYPE`, `DIR`, and `CATALOG` in 68
lines and 2,167 bytes, with SHA-256
`c3c831bc183ee999c8b5a0d1fb4edd169890be1e5fa44ad726d3025923fdb3b7`.
Loading them only installs three definitions and their inline strings.
Focused execution qualifies pathless listing from the cached directory and
bitmap; it is not file-backed persistence evidence.

Exact unchanged lines 5286–5408 add five colon definitions through `RENAME`
and six scratch variables in 123 lines and 4,020 bytes, with SHA-256
`a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028`.
Load initializes those variables to zero without reading the epoch, parsing a
name, touching filesystem state or media, syncing, or publishing output.
Focused execution qualifies lookup and metadata mutation only on pathless
in-memory media in the safe domain described below.

Exact unchanged lines 5409–5436 add `CAT-SLOT` and `CAT` in 28 LF lines and
838 bytes, with SHA-256
`e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23`
and Git blob `2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5`. Loading zeroes `CAT-SLOT`
and installs the colon body and inline strings without parsing, ensuring the
filesystem, accessing cache or media, updating diagnostics, or publishing
output. Focused execution qualifies only the bounded primary-extent domain
described below.

Exact unchanged lines 5437–5471 add `LF-BEST`, `LF-RUN`,
`FS-LARGEST-FREE`, and `FS-FREE` in 35 LF lines and 984 bytes, with SHA-256
`6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c`
and Git blob `1884c81ba2b8aa48082d472250f13a2265fd1def`. Loading zeroes the scratch and
installs two colon bodies and their inline strings without ensuring the
filesystem, scanning cache, touching media or diagnostics, or publishing
output. Focused execution qualifies cache-only reporting in the valid-geometry
domain described below.

Exact unchanged lines 5472–5514 add `SB-SLOT`, `SB-DESC`, `SAVE-BUFFER`,
`LB-SLOT`, `LB-DESC`, and `LOAD-BUFFER` in 43 LF lines and 1,317 bytes, with
SHA-256
`7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104`
and Git blob `8b4645f16c7ac2f21036282a896b7ede6bad16b0`. Loading zeroes the four
scratch variables and installs the two colon bodies and inline strings. It
does not ensure or parse, dereference a Buffer, touch cache or media, update
diagnostics, flush, or publish output. Focused execution qualifies only the
single-primary-extent Buffer domain described below.

Exact unchanged lines 5515–5610 add the fixed FD pool, cached `OPEN`,
used-metadata `FFLUSH`, and final auto-flushing `FCLOSE` in 96 LF lines and
3,397 bytes, with SHA-256
`16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78`
and Git blob `e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9`. Its 14 definitions are
`FD-MAX`, `FD-SLOT-SZ`, `FD-POOL`, `FD-SLOT`, `FD-ALLOC`, `(FCLOSE-NOFS)`,
`FCLOSE`, `FD-FILL`, `OP-SLOT`, `(OPEN)`, `OPEN`, `F.SLOT`, `FFLUSH`, and
`(FCLOSE)` in source order. Loading zero-fills the 1,152-byte pool and zeroes
`OP-SLOT`; it binds `FCLOSE` first to `(FCLOSE-NOFS)` and finally to
`(FCLOSE)`, and binds `OPEN` to `(OPEN)`. It performs no ensure, parse,
filesystem/cache/media I/O, sync, flush, diagnostic update, or output.

| Word | Stack Effect | Admitted behavior |
|------|--------------|-------------------|
| `FS-DIR-START` | `( -- sector )` | Derive `1 + FS-BMAP-N`. |
| `FS-DSTART` | `( -- sector )` | Derive the first data sector, `13 + FS-BMAP-N`. |
| `BIT-MASK` | `( bitpos -- mask )` | Compute a cell-width `1 << bitpos`; bitmap callers pass only `0..7`. |
| `BIT-FREE?` | `( sector -- flag )` | Test the corresponding cached bitmap bit. |
| `BIT-SET` / `BIT-CLR` | `( sector -- )` | Mutate the corresponding cached bitmap bit. |
| `FIND-FREE` | `( count -- sector \| -1 )` | Return the first complete free run in `[FS-DSTART, FS-TOTAL)` without reserving it. |
| `DIRENT` | `( slot -- addr )` | Address one of the 128 packed 48-byte cache entries. |
| `DE.SEC` … `DE.EXT1-CNT` | `( de -- value )` | Read the packed little-endian directory fields. |
| `FIND-FREE-SLOT` | `( -- slot \| -1 )` | Return the first entry whose name byte zero is zero. |

The admitted helper domain assumes validator-approved geometry
(`1 <= FS-BMAP-N <= 16`, `13 + FS-BMAP-N < FS-TOTAL <= 65536`), an in-range
sector, a positive `FIND-FREE` count, a `DIRENT` index in `0..127`, and
complete cache spans. The helpers do not check `FS-OK`, bounds, or geometry.
Equal or reversed geometry bounds in `FIND-FREE` and out-of-domain negative or
very large `BIT-MASK` positions can drive ordinary `DO` across the modulo
64-bit cell space. Nonpositive and high-bit run counts are separately
unqualified and can produce nonsensical results under the source's signed
comparison, although valid geometry still bounds that scan. `FIND-FREE` uses
shared `FF-*` scratch and is not reentrant. `FIND-FREE-SLOT` deliberately
checks only `name[0]`. Canonical producers zero all 48 bytes of a free entry,
but the BIOS validator likewise ignores the remaining 47 bytes once the first
byte is zero; full-zero tails are not validator-enforced.

There is also a source-comment discrepancy at line 5026: the directory layout
calls `mtime` “seconds since boot,” while the later unchanged `TICKS@` computes
`EPOCH@ 1000 /`, i.e. Unix epoch seconds. The on-disk specification and
executable producer agree on epoch seconds; the simulator does not reinterpret
the field to preserve the stale comment.

### File Type Codes

| Code | Name | Typical Use |
|------|------|-------------|
| 0 | free | Empty directory slot |
| 1 | raw | Binary data |
| 2 | text | Plain text |
| 3 | forth | Forth source code |
| 4 | doc | Documentation topic |
| 5 | data | Structured data |
| 6 | tutorial | Step-by-step lesson |
| 7 | bundle | Pipeline bundle (declarative config) |
| 8 | directory | Parent for hierarchical entries |
| 9 | stream | Circular stream data |
| 10 | link | Symbolic link target |

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FS-LOAD` | `( -- )` | Clear `FS-OK`, force the singleton raw binding, ask BIOS to validate accepted marker-1 geometry and metadata, then cache its superblock, bitmap, and directory. A successful load sets `FS-OK`; it does not reset `CWD`. |
| `FS-SYNC` | `( -- )` | If loaded, write bitmap then directory and flush. It does not write the superblock and is not transactional. |
| `FS-ENSURE` | `( -- )` | If `FS-OK` is false and a disk is present, invoke `FS-LOAD`; otherwise do nothing. A true marker is not revalidated. |
| `FORMAT` | `( -- )` | **Initialize fresh filesystem metadata** using the attached capacity: write marker-1 geometry, mark metadata sectors, clear the directory, and flush. It does not wipe data sectors and is not transactional. |
| `.FTYPE` | `( type -- )` | Print `free`, `raw`, `text`, `forth`, `doc`, `data`, `tut`, `bdl`, `dir`, `stream`, or `link` for codes 0 through 10; otherwise print `?` and the signed value in the current `BASE`. |
| `DIR` | `( -- )` | List entries whose parent is `CWD`, showing name, size, and type, followed by a free-space summary. |
| `CATALOG` | `( -- )` | List name, byte size, primary sector count, numeric type, and flags, followed by a free-space summary. It does not print the start sector. |
| `FIND-BY-NAME` | `( -- slot \| -1 )` | Return the first occupied entry in `CWD` whose complete 24-byte name equals `NAMEBUF`; caller must populate its zero-padded bytes first. It does not check `FS-OK`. |
| `TICKS@` | `( -- seconds )` | Apply signed `/ 1000` to the explicit deterministic `EPOCH@` millisecond cell. |
| `MKFILE` | `( nsectors type "name" -- )` | Reserve one positive contiguous primary run, construct an empty entry, timestamp it, then sync. It does not initialize data sectors or validate its type/name/count domain. |
| `RMFILE` | `( "name" -- )` | Clear both cached extent runs and the complete entry, then sync. It does not wipe payload and is unsafe for a directory's zero primary count. |
| `RENAME` | `( "oldname" "newname" -- )` | Replace only the 24-byte cached name and sync. It retains `mtime`, rejects the same name as taken, and does not validate an empty replacement. |
| `CAT` | `( "name" -- )` | Read the complete primary extent into unreserved `HERE`, then emit exactly `DE.USED` bytes with LF converted to CRLF. It does not advance `HERE`, read a secondary extent, type-check, or append a newline. |
| `FS-LARGEST-FREE` | `( -- sectors )` | Without an `FS-OK` gate, reset global scratch and return the largest clear run in the cached data-sector bitmap. |
| `FS-FREE` | `( -- )` | Ensure the filesystem, then report cached total free sectors/bytes, largest run, and global occupied-entry count/max. |
| `SAVE-BUFFER` | `( buf "name" -- )` | Write the complete primary allocation from `B.DATA`, store low-u32 `B.LEN` as cached `used_bytes`, then sync. It does not follow a secondary extent or update `mtime`/CRC. |
| `LOAD-BUFFER` | `( buf "name" -- )` | Read the complete primary allocation, including padding, into `B.DATA`. It does not change `B.LEN`, Buffer metadata, or file metadata. |
| `FD-SLOT` | `( n -- addr )` | Compute an unchecked 72-byte pool-slot address. |
| `FD-ALLOC` | `( -- fdesc \| 0 )` | Mark and return the lowest free slot's `slot + 8`, retaining its payload; return zero when all 16 headers are busy. |
| `F.SLOT` | `( fdesc -- slot )` | Read the cached directory-slot snapshot at fdesc `+32`. |
| `OPEN` | `( "name" -- fdesc \| 0 )` | Ensure and find a cached name, allocate the lowest FD, and snapshot its directory fields with cursor zero; return zero on gate, miss, or exhaustion. |
| `FFLUSH` | `( fdesc -- )` | Store low-u32 `F.USED` into the cached entry selected by `F.SLOT`, then call `FS-SYNC`; it does not write payload. |
| `FCLOSE` | `( fdesc -- )` | Ignore zero; when `FS-OK` is true flush used metadata before release, otherwise silently release without persistence. |
| `LOAD` | `( "path" -- )` | Resolve an MP64FS Forth source path, concatenate its validated primary/secondary extents, and evaluate its physical lines. |
| `SOURCE-EVALUATE-CHECKED` | `( addr len -- status )` | Compile a complete in-memory source buffer with deterministic status and diagnostics; stop at first failure. |
| `PWD` | `( -- )` | Print root or at most the eight path components nearest CWD. It does not ensure/check the filesystem. |
| `CD` | `( "component" -- )` | Move to exact `..`, root `/`, or one direct type-8 child; embedded slash syntax is not resolved. |
| `MKDIR` | `( "component" -- )` | Create a metadata-only type-8 child in the lowest logically free slot, then sync. |
| `RMDIR` | `( "component" -- )` | Clear and sync one direct empty type-8 child; nonempty rejection leaks the target slot cell. |
| `DIRENT` | `( n -- addr )` | Address of directory entry *n* in the RAM cache (for low-level access). |

`FS-LOAD` clears `FS-OK`, destructively binds raw storage, delegates to the
BIOS validator, then reads superblock/geometry, bitmap, and directory in that
order. Only complete success sets `FS-OK`; `CWD` is retained. Validation and
cache reads do not share one lock or generation-bound snapshot, the reread
superblock is not revalidated, and a late abort can leave the binding,
geometry, and earlier caches published.

`FS-SYNC` writes bitmap then directory and flushes without writing the
superblock or rolling back earlier writes. A non-stale late failure may retain
true `FS-OK`; a stale compatibility result clears it. `FS-ENSURE` is silent
for false-plus-absent and never revalidates a true marker.

`FORMAT` clears `FS-OK`, destructively binds raw storage, accepts capacities
15 through 65,536, publishes geometry, then writes superblock, active bitmap,
and directory before flushing. Only flush success publishes true `FS-OK` and
root `CWD`. Failure retains constructed caches and any earlier media writes;
data sectors and the inactive bitmap-cache tail are untouched.

`.ZSTR` consumes its address before reading, publishes nonzero bytes one at a
time, stops without publishing the first NUL, and has no hidden length limit.
It does not sanitize or escape control bytes. If a later byte read faults, its
UART prefix remains visible. The BIOS validator accepts an occupied name
without a NUL in its 24-byte field, so unchanged `DIR` and `CATALOG` can then
publish adjacent entry bytes. Hosted listing admission requires canonical
producer-terminated names; it does not repair that validator gap.

`DIR` and `CATALOG` read occupied direct children of `CWD` from the global
cache and count free bitmap bits over `[FS-DSTART, FS-TOTAL)`. `DIR` publishes
`DE.USED`, compact `.FTYPE` output, and `/` for type 8. `CATALOG` publishes
`DE.USED`, only the primary `DE.COUNT`, numeric type, and flags. All numeric
fields use signed `.` in the current `BASE`. Neither command revalidates an
already-true `FS-OK`, so an absent or replaced attachment can leave stale
cache output eligible.

The hosted RTC surface is one runtime-local explicit epoch register at MMIO
`+0xB08..+0xB0F`. It defaults to zero, advances only through a host request or
direct write, wraps modulo 64 bits on host advance, and never consults host
wall time. A low-byte read latches the current eight-byte little-endian value;
`EPOCH@` performs that byte sequence. `MS@`, automatic time, uptime, calendar,
alarm, control, and realtime behavior remain unqualified. `TICKS@` uses signed
division; for admitted positive values it discards milliseconds, returns a
full cell, and `MKFILE` stores only its low 32 bits in the entry.

`FIND-BY-NAME` compares all 24 bytes and returns the first matching slot in
`CWD`. Because the validator accepts duplicate names and stale nonzero bytes
after a visible NUL, the visible spelling can fail lookup or a lower exact slot
can shadow later entries. The admitted mutation domain requires a nonempty
canonical component, a positive in-range primary run, a non-directory valid
type, a parent that is valid in the current cache, and disjoint exclusively
owned extents. `FS-LOAD` does not reset `CWD`, so a parent retained from a
previous image can make a new entry invalid on the next load.

`MKFILE` mutates bitmap and directory cache before `FS-SYNC`, records
`used_bytes = 0`, leaves the secondary extent zero, and does not wipe the
claimed sectors. An empty name instead allocates sectors while leaving the
entry visibly free; type 8 with a positive allocation is not a valid directory.
`RMFILE` clears both extent runs and the entry without erasing data. Its zero
primary-count `DO` makes directory deletion unsafe, and validator-accepted
overlapping extents let it free bits another entry still references.

`RENAME` changes no metadata other than the name, including no `mtime` update.
An empty new name hides the entry without freeing sectors. All three mutation
words change cache before `FS-SYNC` writes bitmap, directory, then flushes.
Late failure retains cache and possibly earlier media effects while non-stale
failure can retain true `FS-OK`; retry can short-circuit on the changed cache.
The state and scratch are global and unlocked.

If no filesystem is available, these commands return before consuming their
parsed name tokens, leaving them to the outer evaluator. An old-name miss in
`RENAME` likewise leaves the proposed new token. Those parser defects, empty
names, directory deletion, stale parents, and overlapping extents remain
outside the safe hosted domain.

`CAT` separately checks filesystem availability before parsing, a lookup miss
before metadata, and zero `DE.USED` before file I/O. These exits respectively
leave the filename token unconsumed and print `No filesystem`, print `Not
found` with the parsed name, or print `(empty file)`; each terminates with CRLF,
and miss/empty perform no data read. A nonempty match issues a generation-bound
read of all primary `DE.COUNT` sectors at `DE.SEC` into the unreserved current
`HERE` without advancing it. It then emits exactly `DE.USED` bytes, converting
LF to CRLF but passing every other byte, including CR, NUL, and ESC, unchanged.
It adds no trailing newline. A read failure aborts before content output, while
a partial lower-level DMA can retain its scratch prefix and diagnostics.

The admitted domain requires a stable mounted generation, a canonical matched
non-directory file, one small positive primary extent, no secondary extent,
`DE.USED <= DE.COUNT * 512`, and a complete unused mapped DMA span at `HERE`.
The word enforces none of the type, capacity, or scratch bounds. It ignores
the validator-approved secondary
extent, so a two-extent file crossing the primary boundary instead emits stale
unread bytes after the DMA span. `CAT-SLOT`, parser buffers, storage diagnostics,
and the unreserved `HERE` scratch are global and unlocked. The `CAT` fixture
ends at line 5436; blank line 5437 leads into the admitted free-space reporting
fixture.

`FS-LARGEST-FREE` resets `LF-BEST` and `LF-RUN`, then reads every cached bitmap
bit in `[FS-DSTART, FS-TOTAL)`. Updating the best on each clear bit includes a
trailing run. It has no `FS-OK` check or output. `FS-FREE` first ensures and
checks the filesystem; failure prints `No filesystem` and returns before either
scan, preserving prior `LF-*` values. Success separately counts clear bits,
invokes the largest-run helper, and counts all 128 entries whose first name
byte is nonzero. That occupied count ignores `CWD`, includes directories and
all parents, and is labeled `files` in the unchanged output. It prints free
sectors, their byte product, largest contiguous sectors, and occupied/max using
signed `.` in the current `BASE`.

The admitted reporting domain requires validator-approved positive geometry
and complete cached bitmap/directory spans. Direct helper use does not establish
those preconditions; invalid ordinary-`DO` bounds remain excluded.
`FS-ENSURE` trusts already-true `FS-OK`, so detached or replaced media can leave
stale cached results eligible without I/O. The two bitmap scans, directory
scan, and global `LF-*` scratch are unlocked and not one coherent allocation
snapshot. This adds no allocator, ownership validation, repair, compaction, or
persistence claim.

`SAVE-BUFFER` and `LOAD-BUFFER` both run `FS-ENSURE` and test `FS-OK` before
storing their descriptor or parsing a filename. The no-filesystem exit drops
the descriptor, leaves the name token for the outer evaluator, prints `No
filesystem`, and preserves all four `SB-*`/`LB-*` scratch cells. A miss occurs
after the descriptor is stored, parsing fills the global name state, and the
slot scratch becomes `-1`, but before any Buffer dereference or storage I/O.
The save miss includes `(create with MKFILE first)`; the load miss does not.

A match transfers all `DE.COUNT` primary sectors at `DE.SEC`; `DE.USED` does
not limit the transfer and neither word follows `DE.EXT1-SEC` or
`DE.EXT1-CNT`. `SAVE-BUFFER` first makes the generation-bound payload write
from `B.DATA`, then stores the low 32 bits of cell-sized `B.LEN` in cached
`used_bytes`, then calls ordered, nontransactional `FS-SYNC`. Name, extents,
type, flags, parent, `mtime`, and CRC are retained, so the payload can make the
stored CRC stale and there is no automatic timestamp update. Payload failure
happens before the metadata update but can leave a partial media prefix; a
bitmap, directory, or flush failure during sync can leave payload and metadata
partly published with changed cache state. Only complete success prints the
saved `B.LEN`.

`LOAD-BUFFER` reads the full primary allocation into `B.DATA`, including tail
padding beyond `DE.USED`, while leaving `B.LEN`, every other Buffer field, and
all file metadata unchanged. A failed generation-bound read prints no success
line but can leave a partial Buffer prefix. Its success message reports cached
`DE.USED`, not the transfer size. Both success numbers use signed `.` in the
ambient `BASE`.

The source uses `B.LEN` rather than `B.BYTES` for save metadata and output;
with multi-byte elements it stores an element count mislabeled as bytes while
still transferring whole sectors. Safe ordinary-constructor use therefore
requires byte width, `B.LEN = B.BYTES = DE.COUNT * 512`, and a complete mapped
`B.DATA` span readable for save or writable for load. `B.LEN` must represent
the intended unsigned 32-bit field, and save requires a writable selected
volume. The filesystem also must remain mounted to
the same generation, and the canonical matched non-directory entry must have
one positive in-range primary extent and no secondary extent. The source does
not enforce these constraints or per-entry read-only/system flags. Scratch
variables, parser state, cache, and
storage diagnostics are global and unlocked. This is primary-extent Buffer I/O,
not general two-extent persistence, CRC maintenance, or transaction recovery.

The FD pool contains 16 fixed 72-byte slots. The fdesc returned to callers is
eight bytes past the slot base so the pre-existing `F.*` accessors keep their
offsets:

| Slot offset | fdesc offset | Field |
|---:|---:|---|
| `+0` | — | `in_use`: zero free, `-1` allocated |
| `+8` | `+0` | primary start sector |
| `+16` | `+8` | maximum primary sector count |
| `+24` | `+16` | used bytes |
| `+32` | `+24` | cursor |
| `+40` | `+32` | cached directory slot |
| `+48` | `+40` | secondary start sector |
| `+56` | `+48` | secondary sector count |
| `+64` | `+56` | reserved |

`FD-ALLOC` scans from slot zero upward, marks the first zero header `-1`, and
returns its fdesc. Exhaustion returns zero. Allocation never clears the eight
payload cells. `FD-FILL` overwrites the seven cells through secondary count
from the directory cache, setting cursor to zero, but leaves reserved `+56`
alone. Consequently the reserved cell starts zero after the pool's load-time
fill and survives all subsequent fill, close, and reuse operations. The named
`(FCLOSE-NOFS)` body remains callable after final rebinding: zero is a no-op,
and nonzero clears only `fdesc - 8` with no flush or payload clearing.

Final deferred `OPEN` ensures and checks the filesystem before parsing. A
failed gate returns zero and prints `No filesystem`, while leaving the filename
token and `OP-SLOT` unchanged. A name miss sets `OP-SLOT = -1`, prints the name,
and returns zero before allocation. Exhaustion retains the matched slot in
`OP-SLOT`, prints `No free FD slots`, and returns zero. Success chooses the
lowest free fdesc, snapshots primary and secondary coordinates, used count,
and directory slot, resets its cursor, and prints nothing. When `FS-OK` is
already true, these paths use only parser/cache/pool state and perform no
storage or payload I/O; `FS-ENSURE` may run `FS-LOAD` when the marker begins
false.

The descriptor is a mutable snapshot, not an open-file identity. `OPEN` does
not check file type or flags, reject directories, capture the selected storage
binding/generation, revalidate a true `FS-OK`, prevent several descriptors for
one entry, or coordinate their independent cursor/used values. A directory
mutation, reload, or volume replacement can stale it, and close order among
duplicate opens can overwrite a newer used count. Copying secondary extent
coordinates into the structure qualifies only the snapshot layout; no
multi-extent `FREAD`, `FWRITE`, or other content I/O is admitted here.

`FFLUSH` rejects false `FS-OK` before dereferencing its argument, printing
`FS not loaded` and returning without I/O. Otherwise it copies only the low 32
bits of `F.USED` into cached `used_bytes` for `F.SLOT`, then invokes
nontransactional `FS-SYNC`. It leaves the descriptor allocated, writes no file
payload, and retains every other directory field, including `mtime`, CRC, and
flags. It checks neither descriptor/directory-slot validity nor used against
allocated capacity; `L!` truncates the cell to low u32. Cache mutation occurs
before bitmap/directory writes and flush, so an abort can leave changed cache
and partially published media.

Final deferred `FCLOSE` returns immediately for zero. With true `FS-OK`, it
calls `FFLUSH` and clears the in-use header only after a successful return; an
abort leaves the slot allocated despite any cache/media prefix. With false
`FS-OK`, it silently skips persistence and releases the slot. Release never
clears descriptor/reserved cells or file payload. No allocator/close/flush
operation validates pool membership, alignment, allocation state, or directory
identity. Lowest-first address reuse therefore permits stale-handle ABA: an old
fdesc can flush or close a new occupant. The pool, `OP-SLOT`, parser/cache
state, and deferred vectors are global and unlocked. The contiguous frontier
continues through subdirectory navigation at line 6296; the next uncovered seam
is the Documentation Browser heading at line 6297.

**Example — filesystem operations:**
```forth
DIR                          \ list all files
CAT getting-started          \ print a file's contents
4 4 MKFILE my-notes          \ create a 4-sector file of type "doc"
0 1 512 BUFFER disk-page      \ one full sector of byte-width backing
1 5 MKFILE my-data            \ matching one-sector data file
disk-page SAVE-BUFFER my-data \ save the complete primary allocation
LOAD my-script.f             \ resolve and evaluate an MP64FS Forth source
FS-FREE                      \ check remaining space
```

---

### §7.6.1 Filesystem Encryption

Optional at-rest encryption for MP64FS files, intended to use AES-256-GCM.
It operates on OPEN'd file descriptors and uses a system-level key stored in
`FS-KEY`. The wrappers rely on the shared AES engine's ambient key mode, so the
qualified AES-256 path requires that mode to be clean/default. The IV is
derived deterministically from the file's directory slot number.

On-disk layout of an encrypted file:
- Sectors contain: ciphertext over a 16-byte-rounded physical prefix `||` one 16-byte GCM tag
- `used_bytes` in directory = original plaintext length (unchanged)
- `flags` bit 2 = encrypted

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FS-KEY!` | `( addr -- )` | Copy 32-byte encryption key into `FS-KEY`. |
| `ENCRYPTED?` | `( fdesc -- flag )` | True (-1) if file has the encrypted flag set. |
| `FENCRYPT` | `( fdesc -- result... )` | Encrypt an open file in-place. Returns 0 on success/no-op, -1 for capacity or first-allocation failure, and malformed `0 -1` on second-allocation failure. Storage/sync failures throw. |
| `FDECRYPT` | `( fdesc -- result... )` | Authenticate and decrypt in-place. Returns 0 on success/no-op, -1 for authentication or first-allocation failure, and malformed `0 -1` on second-allocation failure. Storage/sync failures throw; authentication failure leaves disk/cache state unchanged. |

**Example:**
```forth
CREATE my-key 32 ALLOT   my-key 32 0 FILL   my-key FS-KEY!
OPEN secret              \ -- fdesc
DUP ENCRYPTED? .         \ 0 (not encrypted)
DUP FENCRYPT .           \ 0 (success)
FCLOSE                   \ release FD back to pool
```

The executable implementation is a single whole-file GCM transaction over a
primary contiguous extent, not per-sector encryption. It zeroes its staging
buffer and then reads whole sectors, so bytes from `used_bytes` through the
next 16-byte boundary are existing physical file slack rather than guaranteed
zero padding. The IV contains only the little-endian directory slot and four
zero bytes. Encrypting again after decrypt/flag-clear, or reusing a slot under
the same key, therefore repeats a GCM nonce; a direct call while flagged is a
no-op. File metadata is not authenticated as AAD. The source also trusts the
shared AES engine's ambient key mode, has no key-set check, ignores secondary
extents, omits the decrypt-side capacity check, and never checks AES status
after encryption before trusting the returned output and tag.

Payload and metadata updates are ordered but nontransactional. Disk, AES, or
sync exceptions after allocation leak both unwiped DMA buffers; key, IV, and
freed plaintext/ciphertext scratch persist. A failed second DMA allocation
does free the first allocation but returns two cells, `0 -1`, rather than the
documented single flag. The wrapper also ignores the MP64FS readonly flag;
lower storage layers still enforce volume bounds, media generation, and device
write protection. The detailed source comment says an unencrypted file
returns -1, while the executable early no-op returns 0; an encrypted empty file
returns 0 without clearing its flag. These are current source discrepancies,
not guarantees a caller should build new secure storage around.

---

### §7.6.2 Subdirectory Navigation

Runtime navigation uses the one-byte parent field in the flat MP64FS directory
cache. Root is 255; a non-root value is a directory-entry slot.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PWD` | `( -- )` | Print root or the retained current path with leading/trailing `/`. |
| `CD` | `( "component" -- )` | Move to exact `..`, root `/`, or one direct type-8 child component. |
| `MKDIR` | `( "component" -- )` | Create a metadata-only type-8 child in the lowest logically free slot and sync. |
| `RMDIR` | `( "component" -- )` | Clear and sync one direct empty type-8 child; the nonempty rejection has the stack discrepancy below. |

`PWD` does not call `FS-ENSURE`: root prints ` /` even without mounted media,
while non-root operation trusts CWD and the cached parent graph. It walks until
parent 255 but retains only the first eight slots nearest CWD, so a deeper path
silently omits its highest ancestors. Each displayed component ends in `/`.

`CD`, `MKDIR`, and `RMDIR` call `FS-ENSURE` and check `FS-OK` before parsing;
a failed gate leaves the would-be operand for the outer evaluator. Exact `..`
in `CD` moves to the cached parent except at root, exact `/` moves to root, and
every other token is a single exact current-directory lookup that must have
type 8. Despite older examples that show `CD /tools/crypto`, this word does not
call `_RESOLVE-PATH`; embedded slash syntax and `.` are ordinary component
bytes and normally miss. Successful CD is volatile and performs no sync or
storage command.

`MKDIR` rejects the first existing exact sibling, chooses the lowest entry whose
`name[0]` is zero, clears all 48 bytes, copies the zero-padded 24-byte NAMEBUF,
sets type 8 and the low CWD parent byte, and stores low-u32 epoch seconds as
mtime. `RMDIR` requires a direct type-8 child and scans all occupied entries
for children before clearing it. Both successful mutations use ordinary
`FS-SYNC` (unchanged bitmap write, complete directory write, then flush), do
not allocate/free data sectors, and do not update a parent mtime.

The executable edge behavior is intentionally documented rather than hidden.
A validator-accepted parent cycle makes `PWD` nonterminating, and a 24-byte
non-NUL name lets `.ZSTR` read beyond the packed field. Empty `MKDIR` writes
type/parent/mtime into a slot whose zero first name byte still makes it
logically free. Tokens longer than 23 bytes silently operate on the truncated
NAMEBUF prefix. Names `..` and `/` are accepted for creation but are shadowed
by CD's operators. Validator-accepted duplicate siblings are first-slot-wins.
MP64FS readonly/system policy bits are ignored. Cache is mutated before
nontransactional sync, and a late failure can retain partial
publication. On a nonempty-directory rejection, `RMDIR` drops only one of two
target-slot copies and returns with `( -- slot )` instead of a clean stack.
Shared CWD, NAMEBUF/PATHBUF/PN-LEN parser state, `_PWD-STK`, and cache state are
unlocked.

---

## §7.7 Documentation Browser

A built-in paging reader for documentation and tutorial files stored on
disk.  Files with type=4 (doc) and type=6 (tutorial) are browsable.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TOPICS` | `( -- )` | List all documentation files on disk (type=doc). |
| `LESSONS` | `( -- )` | List all tutorial files on disk (type=tutorial). |
| `DOC` | `( "name" -- )` | Open and page through a documentation file, pausing every 20 lines with a "--- more ---" prompt.  Automatically closes the FD when done. |
| `TUTORIAL` | `( "name" -- )` | Open and walk through a tutorial file (same pagination as DOC).  Automatically closes the FD when done. |
| `DESCRIBE` | `( "word" -- )` | Search for a documentation file matching the given word name.  If found, displays it (closes FD after).  If not, suggests using `TOPICS`. |
| `SHOW-FILE` | `( fdesc -- )` | Low-level: page through an open file descriptor with pagination.  Caller is responsible for `FCLOSE`. |
| `OPEN-BY-SLOT` | `( slot -- fdesc \| 0 )` | Open a file by its directory slot index.  Uses the FD pool; caller should `FCLOSE` when done. |

**Example:**
```forth
TOPICS              \ see what docs are available
DOC buffers         \ read the "buffers" documentation
LESSONS             \ see what tutorials are available
TUTORIAL hello-world  \ walk through the hello-world tutorial
DESCRIBE ksum       \ look up documentation for a word
```

---

## §7.8 Dictionary Search

Tools for exploring the Forth dictionary — finding words by pattern and
inspecting recent definitions.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `WORDS-LIKE` | `( "pattern" -- )` | Search the entire dictionary for words whose names contain *pattern* (case-insensitive substring match).  Prints all matches with a count. |
| `APROPOS` | `( "pattern" -- )` | Alias for `WORDS-LIKE`. |
| `.RECENT` | `( n -- )` | Show the last *n* words added to the dictionary, starting from `LATEST`. |
| `ICONTAINS?` | `( pa pl sa sl -- flag )` | Low-level: case-insensitive substring search.  True if the pattern (addr *pa*, len *pl*) appears anywhere in the string (addr *sa*, len *sl*). |
| `ENTRY>NAME` | `( entry -- addr len )` | Extract the name from a dictionary entry (skip 8-byte link + 1-byte flags/len). |
| `ENTRY>LINK` | `( entry -- next )` | Follow the link field to the previous dictionary entry. |

**Example:**
```forth
WORDS-LIKE buf      \ find all words containing "buf"
WORDS-LIKE pipe     \ find all pipeline-related words
APROPOS task        \ find all task-related words
10 .RECENT          \ show the 10 most recently defined words
```

---

## §8 Scheduler & Tasks

KDOS includes a **cooperative multitasking scheduler** with optional
timer-assisted preemption.  Up to **8 tasks** can be registered, each
with a **256-byte private data stack**.

### Task States

| State | Value | Meaning |
|-------|-------|---------|
| `T.FREE` | 0 | Slot is available (no task). |
| `T.READY` | 1 | Task is runnable, waiting for CPU time. |
| `T.RUNNING` | 2 | Task is currently executing. |
| `T.BLOCKED` | 3 | Task is waiting for an external event. |
| `T.DONE` | 4 | Task has finished; can be cleaned up or restarted. |

### Task Descriptor Layout

```
Offset   Field       Meaning
───────  ──────────  ─────────────────────────────────────
+0       status      T.FREE .. T.DONE
+8       priority    0 = highest, 255 = lowest
+16      xt          Execution token (the task body)
+24      dsp_save    Saved data stack pointer
+32      rsp_save    Saved return stack pointer
+40      name_addr   Pointer to name string (or 0)
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TASK` | `( xt priority "name" -- )` | Create a named task.  Allocates a 256-byte private stack area, initializes the descriptor as READY, and registers it in `TASK-TABLE`.  Defines a CONSTANT. |
| `TASKS` | `( -- )` | List all tasks showing state, priority, xt, and name. |
| `SCHEDULE` | `( -- )` | Run the scheduler: repeatedly find READY tasks and execute them round-robin until no READY tasks remain. |
| `SPAWN` | `( xt -- )` | Create an anonymous READY task with default priority 128. |
| `BG` | `( xt -- )` | Spawn a task and immediately run the scheduler ("background" a task). |
| `KILL` | `( tdesc -- )` | Force a task to DONE state (cancel it). |
| `RESTART` | `( tdesc -- )` | Reset a DONE task back to READY so it can run again. |
| `SCHED-YIELD` | `( -- )` | Mark the current core-0 KDOS task DONE. Scheduler-only primitive. |
| `YIELD` | `( -- )` | Compatibility wrapper for `SCHED-YIELD`; a no-op on dispatched secondary full cores. |
| `WORKER-CHECKPOINT` | `( -- )` | Check and clear the calling worker core's preemption flag without touching scheduler state. |
| `CORE-CHECKPOINT` | `( -- )` | Check and clear the calling core's preemption flag. On core 0 this also performs `YIELD`; secondary one-shot workers continue without touching `CURRENT-TASK`. |
| `YIELD?` | `( -- )` | Compatibility alias for `CORE-CHECKPOINT`. |
| `FIND-READY` | `( -- tdesc \| 0 )` | Find the first READY task in the table (0 if none). |
| `RUN-TASK` | `( tdesc -- )` | Low-level: set task to RUNNING, execute its XT, mark DONE on return. |
| `TASK-COUNT-READY` | `( -- n )` | Count tasks currently in READY state. |
| `PREEMPT-ON` | `( -- )` | Enable timer-based preemption.  Configures the hardware timer with `TIME-SLICE` cycles (default 50,000) and enables auto-reload.  Yield points (`YIELD?`) will check the preemption flag. |
| `PREEMPT-OFF` | `( -- )` | Disable timer preemption. |

**Variables:** `TASK-COUNT`, `TASK-TABLE`, `CURRENT-TASK`, `SCHED-RUNNING`, `PREEMPT-FLAG`, `TIME-SLICE` (default 50000), `PREEMPT-ENABLED`, `TASK-STACKS` (2048 bytes).

**Example — running background tasks:**
```forth
: blink  ( -- )  ." Blink! " CR ;
: count  ( -- )  10 0 DO I . LOOP CR ;

' blink 100 TASK my-blink    \ priority 100
' count 50 TASK my-count     \ priority 50 (higher)

SCHEDULE              \ run both tasks
\ Output: numbers print first (higher priority),
\         then "Blink!" prints

\ Or spawn and run in one shot:
' blink BG            \ runs immediately
```

### How Preemption Works

KDOS uses a "soft preemption" model. The hardware timer fires periodically
and sets a per-core preemption flag. Long-running code should call
`CORE-CHECKPOINT` (or the compatibility name `YIELD?`) at regular intervals.
On core 0, a set flag yields the current KDOS task back to the scheduler. A
secondary full core has no suspended KDOS task scheduler, so it clears its
own flag and continues its one-shot dispatch without reading or modifying
core 0's `CURRENT-TASK`.

---

## §8.1 Multicore Dispatch

KDOS v1.1 adds multicore dispatch on top of the BIOS multicore primitives
(COREID, NCORES, WAKE-CORE, CORE-STATUS, SPIN@, SPIN!).

### Dispatch Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CORE-RUN` | `( xt core -- )` | Dispatch XT to a secondary core via `WAKE-CORE`.  Does nothing if core is 0 (primary) or already busy. |
| `CORE-WAIT` | `( core -- )` | Busy-wait until the given core finishes (polls `CORE-STATUS` until 0). |
| `ALL-CORES-WAIT` | `( -- )` | Wait for all secondary cores to become idle. |
| `BARRIER` | `( -- )` | Synchronize: waits for all secondary cores to finish. |

### Synchronization Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `LOCK` | `( n -- )` | Acquire spinlock *n* with busy-wait (calls `SPIN@` in a loop). |
| `UNLOCK` | `( n -- )` | Release spinlock *n* (calls `SPIN!`). |

The 16 hardware locks have one machine-wide allocation: 0 dictionary, 1 UART,
2 filesystem, 3 heap, 4 ring buffers, 5 hash tables, 6 application runtime
concurrency (including Akashic `EVT-LOCK`), 7 IPI messaging, 8 the checked BIOS
crypto guard, 9 KDOS HMAC/HKDF scratch, 10 the KDOS TLS workspace owner, 11
the short TLS credential-registry/cancellation lock, and 12 the KDOS network
packet-workspace/NIC-descriptor owner. Locks 13 through 15 are currently
unassigned. Subsystems must not privately reuse a number from this map.

### Parallel Pipeline Execution

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `P.RUN-PAR` | `( pipe -- )` | Run pipeline steps in parallel across available cores.  Distributes steps round-robin to secondary cores via `CORE-RUN`, then waits for all to complete. |

### Introspection

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `CORES` | `( -- )` | Display per-core status (screen-compatible).  Shows core ID, idle/busy state for each hardware core. |

**Example — parallel pipeline execution:**
```forth
4 PIPELINE my-pipe
: step1 42 a B.FILL ;
: step2 99 b B.FILL ;
: step3 a b c B.ADD ;
: step4 c B.SUM . ;
' step1 my-pipe P.ADD
' step2 my-pipe P.ADD
' step3 my-pipe P.ADD
' step4 my-pipe P.ADD
my-pipe P.RUN-PAR     \ steps 1 & 2 run on different cores
```

---

## §9 Interactive Screens (TUI)

The SCREENS system is a full-screen terminal UI built on **ANSI escape
sequences**.  It provides a tabbed dashboard with 9 screens showing system
status in real time.

> **Threading rule:** All screen state (`NSCREENS`, `SCREEN-ID`, `SCR-SEL`,
> the `SCR-*` arrays) lives in shared dictionary memory and is **not
> thread-safe**.  `REGISTER-SCREEN`, `SWITCH-SCREEN`, `RENDER-SCREEN`, and
> `HANDLE-KEY` must only be called from the main core (core 0).  Background
> tasks on secondary cores that need to register or modify screens should
> send a request via the mailbox (IPI) and let the main-core event loop
> service it between iterations.

### Starting the TUI

```forth
SCREENS     \ enters the interactive dashboard
```

### Navigation Keys

| Key | Action |
|-----|--------|
| `0`–`9` | Switch to screen 0–9 |
| `a`–`f` | Switch to screen 10–15 (when registered) |
| `n` / `p` | Select next / previous item on selectable screens |
| `[` / `]` | Switch to previous / next subscreen |
| `Enter` / `Space` | Activate the selected item |
| `r` | Refresh the current screen |
| `A` | Toggle auto-refresh (5 M cycles interval) |
| `q` | Quit back to the Forth REPL |

### Key Priority Chain

When a key is pressed, `HANDLE-KEY` dispatches it in this order:

1. **Per-screen handler** via `SCR-KEY-XT` — the screen's custom key
   handler runs first.  If it returns a non-zero "consumed" flag, no
   further dispatch occurs.
2. **Screen switching** — digit keys `0`–`9` and hex keys `a`–`f` switch
   to the corresponding screen (if registered).
3. **Global bindings** — `q`, `r`, `A`, `[`, `]`, `n`, `p`, `Enter`,
   `Space`.

Per-screen handlers can intercept any key, including digits.  To claim a
key, return a non-zero flag from the handler xt.  Unclaimed keys fall
through to the global dispatch.

> **Note:** With 16 screens registered, keys `a`–`f` are consumed by
> screen switching.  If your screen needs those keys, install a
> per-screen handler via `SET-SCREEN-KEYS` and return a consumed flag
> for the keys you claim.

### The 9 Screens

| # | Name | What It Shows |
|---|------|---------------|
| 1 | **Home** | System overview — `HERE` (memory usage), buffer/kernel/pipeline/task/file counts, storage status, network status, scheduler mode, ready task count. |
| 2 | **Buffers** | All registered buffers with type (raw/rec/til/bit), element width, length, tile count, and data address. |
| 3 | **Kernels** | All registered kernels with input/output counts, footprint, and a color-coded `[tile]` or `[cpu]` tag. |
| 4 | **Pipes** | All registered pipelines with capacity and current step count. |
| 5 | **Tasks** | All tasks with **color-coded** state (dim=FREE, green=READY, yellow=RUNNING, red=BLOCKED, dim=DONE), priority, and XT. |
| 6 | **Help** | Quick-reference card listing key commands for all subsystems. |
| 7 | **Docs** | Documentation browser — lists available topics and tutorials from the filesystem, plus doc commands. |
| 8 | **Storage** | File browser — lists disk files with size, type, sector info; inline detail view for selected file; free sector count. |
| 9 | **Cores** | Multicore status — shows each core's state (RUNNING, BUSY, IDLE) with color coding. |

### ANSI Terminal Helpers

These are available for your own use outside of SCREENS:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PAGE` / `CLS` | `( -- )` | Clear screen and home cursor. |
| `AT-XY` | `( col row -- )` | Move cursor to column, row (1-based). |
| `BOLD` | `( -- )` | Enable bold text. |
| `DIM` | `( -- )` | Enable dim text. |
| `REVERSE` | `( -- )` | Enable reverse video. |
| `RESET-COLOR` | `( -- )` | Reset all text attributes. |
| `FG` | `( n -- )` | Set foreground color (0=black, 1=red, 2=green, 3=yellow, 4=blue, 5=magenta, 6=cyan, 7=white). |
| `BG-COLOR` | `( n -- )` | Set background color. |
| `HBAR` | `( -- )` | Draw a dim 60-character horizontal rule. |
| `SGR` | `( n -- )` | Emit a raw ANSI SGR (Select Graphic Rendition) code. |

---

## §10 Data Ports

The data port system provides **NIC-based external data ingestion**.  The
KDOS core defines the frame structures, buffer bindings, accessors, and
statistics.  `networking.f` supplies the UDP transport and routes each
received payload into a bound buffer based on the source ID.

### Frame Protocol

Every incoming frame has a 6-byte header:

```
Offset   Size   Field          Description
───────  ─────  ─────────────  ─────────────────────────
+0       1      SRC_ID         Source identifier (0–255)
+1       1      DTYPE          Data type (0=raw..5=cmd)
+2       2      SEQ            Sequence number (LE)
+4       2      PAYLOAD_LEN    Payload byte count (LE)
+6       ...    PAYLOAD        Actual data
```

The complete data-port frame is a UDP payload.  Its maximum is 1472 bytes:
the six-byte header plus at most 1466 payload bytes, leaving the IPv4 and UDP
headers inside the 1500-byte IP MTU.  Receivers require `PAYLOAD_LEN` to match
the captured UDP payload exactly; malformed frames are dropped without
changing the last routed buffer.

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PORT!` | `( buf id -- )` | Bind a buffer descriptor to source ID *id*.  Incoming frames from that source will be routed to this buffer. |
| `PORT@` | `( id -- buf \| 0 )` | Get the buffer bound to a source ID (0 if unbound). |
| `UNPORT` | `( id -- )` | Unbind a source ID. |
| `POLL` | `( -- id \| -1 )` | Receive and route one frame.  Returns the source ID, or −1 if no frame was available. |
| `INGEST` | `( n -- received )` | Receive and route up to *n* frames.  Returns the actual count received. |
| `RECV-FRAME` | `( -- flag )` | Receive and route one data-port frame; true only when a bound source was routed. |
| `ROUTE-FRAME` | `( -- id \| -1 )` | Low-level: receive a frame and route its payload to the bound buffer. |
| `PORT-SEND` | `( buf id -- )` | Send one buffer as a data-port UDP frame; reject data over 1466 bytes rather than sending a prefix. |
| `PORT-SEND-SLICE` | `( buf off len id -- )` | Send one complete in-bounds slice up to 1466 bytes; reject invalid or oversized slices. |
| `.FRAME` | `( -- )` | Print the last received frame's header (source, type, seq, length). |
| `PORTS` | `( -- )` | List all bound ports with stats. |
| `PORT-STATS` | `( -- )` | One-line summary: port count, received frames, dropped frames. |
| `FRAME-SRC` | `( -- id )` | Source ID of the last received frame. |
| `FRAME-TYPE` | `( -- type )` | Data type of the last received frame. |
| `FRAME-SEQ` | `( -- seq )` | Sequence number of the last received frame. |
| `FRAME-LEN` | `( -- len )` | Payload length of the last received frame. |
| `FRAME-DATA` | `( -- addr )` | Address of the payload in the frame buffer. |

**Example — ingesting sensor data from the network:**
```forth
0 1 256 BUFFER sensor    \ create a 256-byte buffer for sensor data
sensor 1 PORT!           \ bind buffer to source ID 1

\ Later, receive data:
10 INGEST .              \ receive up to 10 frames, print count
PORT-STATS               \ show port/rx/drop counts
sensor B.PREVIEW         \ inspect the received data
```

---

## §11–§12 Benchmarking & Dashboard

### Benchmarking

The `BENCH` and `.BENCH` words are defined in §6 (Pipeline Engine) but
are general-purpose:

```forth
' ksum .BENCH    \ times ksum and prints cycle count
```

### Dashboard

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DASHBOARD` | `( -- )` | Print a comprehensive text-mode system overview: memory, disk, buffers, kernels, pipelines, tasks, files, ports.  Like Screen 1 but in the REPL. |
| `STATUS` | `( -- )` | Quick one-line status showing all subsystem counts (buffers, kernels, pipes, tasks, files, ports). |
| `.MEM` | `( -- )` | Print current memory usage (value of HERE). |
| `HRULE` | `( -- )` | Print 60 dashes. |
| `THIN-RULE` | `( -- )` | Print 40 dots. |

---

## §13 Help System

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HELP` | `( -- )` | Print a comprehensive online reference for all KDOS subsystems — buffers, kernels, pipelines, storage, filesystem, scheduler, data ports, screens, and more.  This is the "man page" built into the running system. |

```forth
HELP    \ print the full reference
```

The HELP text covers: Buffer words, Kernel words, all 18 sample kernels,
Pipeline words, Storage words, MP64FS filesystem, File I/O, Scheduler
words, Data port words, Screens & tools, Documentation, Dictionary search,
Stack & diagnostics.

---

## §14 Startup

The startup section runs automatically when the KDOS core loads.  It:

1. Uses **JIT compilation** (`JIT-ON`) while `kdos.f` compiles into Bank 0
2. Prints the banner and usage hints
3. If a disk is attached (`DISK?`), loads the filesystem (`FS-LOAD`) so
   `DIR`, `CAT`, `LOAD`, and related words work immediately
4. Initializes the Bank 0 heap before any userland transition
5. Runs `autoexec.f` if present on disk
6. Disables JIT (`JIT-OFF`) so interactive use is non-JIT by default

The standard autoexec enables JIT for its own load, enters the capacity-derived
and BIOS-bounded XMEM userland dictionary, loads `networking.f` with KDOS
`REQUIRE`, configures DHCP
or the static fallback, loads `tools.f`, and disables JIT.  The module loader
batches validated MP64FS extents into a separate, temporary transfer
allocation, so the network stack does not enlarge the Bank 0 core dictionary
or alias the BIOS boot buffer.  That allocation resides in XMEM when available
and is reclaimed after evaluation.

Users can re-enable JIT for their own code with `JIT-ON`.

---

## §15 Pipeline Bundles

Pipeline bundles are **versioned, declarative configuration files** that
define complete data processing pipelines in a single loadable artifact.
They combine buffer schemas, kernel registrations, pipeline definitions,
scheduling config, access policies, and dashboard screen settings into one
atomic unit.

Bundles are stored as type-7 files on disk and can be loaded in **live mode**
(creating real objects) or **dry-run mode** (inspection without side effects).

### Why Bundles?

Instead of writing imperative Forth scripts like:
```forth
0 1 256 BUFFER temp
0 1 256 BUFFER output
1 1 0 1 KERNEL my-kern
4 PIPELINE my-pipe
' step1 my-pipe P.ADD
```

You write a **declarative bundle**:
```forth
1 BDL-BEGIN               \ version 1
0 1 256 BDL-BUF temp
0 1 256 BDL-BUF output
1 1 0 1 BDL-KERN my-kern
4 BDL-PIPE my-pipe
0 10000 3 BDL-SCHED       \ pipe 0, 10k cycle interval, auto+repeat
7 30 0 BDL-POLICY         \ read-only, 30-day retention, no export
1 255 BDL-SCREEN          \ default screen 1, all screens visible
BDL-END
```

Then load it: `BUNDLE-LOAD my-config` or inspect it: `BUNDLE-INFO my-config`.

### Bundle Lifecycle

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BDL-BEGIN` | `( version -- )` | **Start a new bundle definition.**  Resets tracking state (but preserves dry-run flag), sets the bundle version, and marks the bundle as active.  All subsequent `BDL-*` calls belong to this bundle. |
| `BDL-END` | `( -- )` | **Finalize the bundle.**  In dry-run mode, prints a detailed summary (version, object counts, scheduling, policies, dashboard config).  In live mode, applies `TIME-SLICE` and `SCREEN-ID` settings, then prints `"Bundle vN loaded: X bufs Y kerns Z pipes"`. |
| `BDL-RESET` | `( -- )` | **Clear bundle state.**  Resets version, counts, and config to zero but *preserves* the `BDL-DRY` flag so `BUNDLE-INFO` dry-runs work correctly.  Called automatically by `BDL-BEGIN`. |

### Bundle Object Creation

These words create KDOS objects (buffers, kernels, pipelines) or skip creation
if in dry-run mode.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BDL-BUF` | `( type width length "name" -- )` | **Add a buffer to the bundle.**  In live mode, calls `BUFFER` to create the buffer.  In dry-run mode, skips creation but increments the buffer count.  All modes track the count for `BDL-END` reporting. |
| `BDL-KERN` | `( n_in n_out footprint flags "name" -- )` | **Add a kernel to the bundle.**  In live mode, calls `KERNEL` to register it.  In dry-run mode, skips registration but increments the kernel count. |
| `BDL-PIPE` | `( capacity "name" -- )` | **Add a pipeline to the bundle.**  In live mode, calls `PIPELINE` to create it.  In dry-run mode, skips creation but increments the pipeline count. |

### Bundle Configuration

These words set global system config for scheduling, policies, and dashboard.
They store values in bundle state variables; `BDL-END` applies them in live mode.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BDL-SCHED` | `( pipe-idx interval flags -- )` | **Set scheduling config.**  *pipe-idx* is which pipeline to schedule (0-based), *interval* is the timer cycle interval, *flags* is a bitmask: bit 0 = auto-start on load, bit 1 = repeat indefinitely.  Stores values in `BDL-SCHED-P/I/F`. |
| `BDL-POLICY` | `( permissions retention export -- )` | **Set access policy.**  *permissions*: 0=read-write, 7=read-only.  *retention*: days to keep data (0=forever).  *export*: 0=no external export, 1=allow.  Stores in `BDL-POL-PERM/RET/EXP`. |
| `BDL-SCREEN` | `( default-screen screen-mask -- )` | **Set dashboard config.**  *default-screen* (1–9) is the initial screen on `SCREENS`.  *screen-mask* is a bitmask of visible screens (511 = all 9 visible).  Stores in `BDL-SCR-DEF/MASK`. |

### Loading & Inspection

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BUNDLE-LOAD` | `( "name" -- )` | **Load a bundle from disk in live mode.**  Sets `BDL-DRY=0`, then calls `LOAD` to read and evaluate the file.  The bundle file should contain `BDL-BEGIN ... BDL-END`.  All objects are created and config is applied. |
| `BUNDLE-INFO` | `( "name" -- )` | **Dry-run inspect a bundle without creating objects.**  Sets `BDL-DRY=1`, calls `LOAD` to evaluate the file (which skips object creation but tracks counts), then resets `BDL-DRY=0`.  `BDL-END` prints a detailed summary.  Use this to preview a bundle before loading it. |
| `.BUNDLE` | `( -- )` | **Show current bundle state.**  If a bundle is active (`BDL-ACTIVE=1`), prints version, buffer/kernel/pipeline counts, scheduling config, policies, and dashboard settings.  If no bundle is loaded, prints `"(no bundle loaded)"`. |

### State Variables

These are internal tracking variables — you don't normally call them directly.

| Variable | Meaning |
|----------|--------|
| `BDL-ACTIVE` | 1 if a bundle is currently being defined, 0 otherwise. |
| `BDL-DRY` | 1 = dry-run mode (skip object creation), 0 = live mode. |
| `BDL-VER` | Bundle version number. |
| `BDL-NBUFS` | Count of buffers added via `BDL-BUF`. |
| `BDL-NKERNS` | Count of kernels added via `BDL-KERN`. |
| `BDL-NPIPES` | Count of pipelines added via `BDL-PIPE`. |
| `BDL-SCHED-P` | Scheduled pipeline index (0-based). |
| `BDL-SCHED-I` | Scheduling interval in cycles. |
| `BDL-SCHED-F` | Scheduling flags (bit 0=auto-start, bit 1=repeat). |
| `BDL-POL-PERM` | Policy: permissions (0=RW, 7=RO). |
| `BDL-POL-RET` | Policy: retention in days. |
| `BDL-POL-EXP` | Policy: export allowed (0=no, 1=yes). |
| `BDL-SCR-DEF` | Dashboard: default screen (1–9). |
| `BDL-SCR-MASK` | Dashboard: screen visibility bitmask (511 = all 9). |

### File Type Constant

| Constant | Value | Description |
|----------|-------|-------------|
| `FTYPE-BUNDLE` | 7 | File type code for pipeline bundles.  Used when creating bundle files with `MKFILE`. |

### Example — Complete Bundle Workflow

**1. Create a bundle file:**
```forth
\ In a text editor or via CAT, create demo-bundle:
1 BDL-BEGIN
0 1 256 BDL-BUF sensor-in
0 1 256 BDL-BUF sensor-out
1 1 0 1 BDL-KERN ksmooth
4 BDL-PIPE data-flow
0 10000 3 BDL-SCHED     \ pipe 0, 10k cycles, auto+repeat
7 30 0 BDL-POLICY       \ read-only, 30 days, no export
2 255 BDL-SCREEN        \ start on screen 2, all visible
BDL-END
```

**2. Inject it into the filesystem:**
```forth
4 7 MKFILE demo-bundle   \ 4 sectors, type=bundle
\ (then manually write the content, or use diskutil.py)
```

**3. Inspect before loading:**
```forth
BUNDLE-INFO demo-bundle
\ Output:
\   Bundle v1 (dry-run)
\   - 2 buffers
\   - 1 kernel
\   - 1 pipeline
\   - Schedule: pipe 0 @ 10000 cycles, flags=3
\   - Policy: perm=7 ret=30 export=0
\   - Screen: default=2 mask=255
```

**4. Load for real:**
```forth
BUNDLE-LOAD demo-bundle
\ Output: Bundle v1 loaded: 2 bufs 1 kerns 1 pipes

BUFFERS         \ see sensor-in, sensor-out
KERNELS         \ see ksmooth
PIPES           \ see data-flow
.BUNDLE         \ show active bundle state
```

**5. Use the loaded objects:**
```forth
sensor-in B.INFO
data-flow P.RUN
```

### Design Notes

- **Idempotency**: `BDL-BEGIN` resets state, so you can re-load a bundle.
- **Dry-run safety**: `BUNDLE-INFO` uses `BDL-DRY=1` to prevent side effects — perfect for CI/CD validation or pre-flight checks.
- **Versioning**: The version number is for human tracking; KDOS doesn't enforce compatibility yet, but future versions could add migration logic.
- **File format**: Bundles are plain Forth source files (type=7) that call `BDL-*` words.  They're human-readable and can be edited with any text editor.
- **Config application**: `BDL-SCHED/POLICY/SCREEN` set global state; if you load multiple bundles, the last one wins.  For production, load one bundle per environment.

---

## §20 Module Registry

KDOS modules identify themselves with exact, case-sensitive evaluator tokens.
A logical module ID is independent of the MP64FS filename or path passed to
`REQUIRE`; filesystem component limits therefore do not truncate or otherwise
change module identity.  IDs are bounded to 1 through 246 bytes.  That is the
largest `PROVIDED ` declaration accepted by the evaluator's 255-byte physical
line (minus the eight-letter word and its separating blank), and the same
envelope applies to caller-owned `PROVIDED-SPAN` values and `MODULE?`.  Empty
or longer IDs throw rather than aliasing a shorter name.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PROVIDED` | `( "id" -- )` | Register the exact ID.  A duplicate is an allocation-neutral no-op.  A new entry leaves no result; an entry-allocation failure throws. |
| `PROVIDED-SPAN` | `( id-addr id-len -- )` | Register the exact caller-owned byte span with the same duplicate, allocation, and active-loader transaction semantics as `PROVIDED`. |
| `MODULE?` | `( "id" -- flag )` | Return one flag indicating whether the exact ID is pending or committed. |
| `REQUIRE` | `( "path" -- )` | Resolve and load a Forth source file.  When its first prescanned `PROVIDED` ID is already present, skip evaluation as a stack-neutral no-op.  A newly evaluated source may intentionally leave its own data-stack results. |
| `MODULES` | `( -- )` | Print every exact registered ID and the exact count, leaving no data-stack cells.  Enumeration order is unspecified. |

All five public operations are core-0-only.  Registration bookkeeping never
appears on the public data stack: `PROVIDED`, `PROVIDED-SPAN`, and `REQUIRE`
leave no private status cells, `MODULE?` leaves exactly its flag, and `MODULES`
only prints.  If
`REQUIRE` evaluates a new source, values intentionally left by that source are
preserved.  If the exact ID was already registered, the source is not evaluated
and the duplicate load changes neither the stack nor persistent allocation.

### Storage and growth

The registry is a module-specific chained hash table.  Each stable entry owns
the complete ID bytes in the Bank 0 heap through private wrappers over
`DMA-ALLOCATE` and `DMA-FREE`; it therefore remains valid across
`ENTER-USERLAND`, `LEAVE-USERLAND`, and `XMEM-RESET`.  A small inline bucket
vector is the initial lookup-performance seed, not an entry limit.  Entry
capacity is bounded by available Bank 0 heap memory.

A committed insertion may trigger best-effort allocation of a larger bucket
vector.  If that performance allocation fails, KDOS retains the old chains and
the new entry remains registered and findable.  By contrast, failure to
allocate a node for a previously absent ID throws.  `REQUIRE` performs that
provisional registration before evaluating any source line, so registry OOM
cannot execute a source prefix.  Exact duplicate lookup happens before
allocation, preserving idempotence even under memory pressure.

### Loading, cycles, and rollback

Before walking source, `REQUIRE` prescans for the first evaluator line whose
first token is `PROVIDED` and provisionally registers its exact following ID.
Presence includes provisional entries, so mutual and longer dependency cycles
terminate without recursively evaluating the same module.  Every additional
new `PROVIDED` ID declared while that source is active belongs to the same
loader frame.  Successful evaluation commits the complete frame-owned set.

If source evaluation throws, KDOS first unwinds the evaluator to that loader
frame's depth checkpoint, then removes and frees every registry entry owned by
the failing frame, releases its transfer allocation, restores loader and
relative-directory state, and rethrows.  A dependency that completed in its own
nested loader frame is already committed and survives a later parent failure.
This is a registry transaction, not transactional compilation: definitions,
output, and other source effects completed before the throw are not rewound.
After the source is corrected, its rolled-back IDs can be registered and loaded
normally on retry.

```forth
PROVIDED example.codec       \ parsed exact-ID registration
S" generated.codec" PROVIDED-SPAN  \ runtime exact-ID registration
MODULE? example.codec .      \ true
REQUIRE networking.f         \ guarded by networking.f's PROVIDED ID
MODULES                      \ exact IDs plus count
```

---

## Quick Reference Card

### Most-Used Words by Task

**Working with buffers:**
```forth
0 1 256 BUFFER name      \ create
42 name B.FILL           \ fill
name B.INFO              \ inspect
name B.SUM .             \ measure
BUFFERS                  \ list all
```

**Running kernels:**
```forth
name kstats              \ sum min max
name khistogram .HIST    \ histogram
name knorm               \ normalize to 0–255
```

**Building pipelines:**
```forth
8 PIPELINE p
' step1 p P.ADD
' step2 p P.ADD
p P.RUN                  \ run
p P.BENCH                \ benchmark
```

**Managing files:**
```forth
DIR                      \ list files
CAT filename             \ print file
LOAD script.f            \ evaluate Forth source
buf SAVE-BUFFER fname    \ requires full-primary mapped backing; see §7.6
```

**Managing modules:**
```forth
REQUIRE module.f         \ load once when source declares PROVIDED
MODULE? exact-id         \ query exact, case-sensitive identity
MODULES                  \ list exact identities and count
```

**Multitasking:**
```forth
' work BG                \ spawn + run
TASKS                    \ list tasks
SCHEDULE                 \ run all READY tasks
```

**Multicore:**
```forth
' work 1 CORE-RUN       \ dispatch to core 1
1 CORE-WAIT             \ wait for core 1
BARRIER                 \ wait for all cores
0 LOCK  0 UNLOCK        \ spinlock acquire/release
pipe P.RUN-PAR           \ parallel pipeline
CORES                    \ show core status
```

**Dashboard:**
```forth
SCREENS                  \ full TUI
DASHBOARD                \ text overview
STATUS                   \ one-liner
HELP                     \ reference
```

---

## §16 Network Stack

TCP/IP stack components built on the NIC hardware (§16–§16.11 in
`networking.f`). The standard autoexec loads them after entering userland.
Bottom-up: Ethernet → ARP → IPv4 → ICMP → UDP → DHCP → DNS → bounded TCP →
TLS 1.3. The feature inventory does not supersede the TCP and TLS qualification
limits stated below.

### §16 Ethernet Framing

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ETH-BUILD` | `( dst src etype payload paylen frame -- total )` | Build an Ethernet frame in caller-provided storage; return 0 without mutation when `paylen` is outside 0..1500. |
| `ETH-SEND` | `( buf len -- )` | Transmit one complete 14..1514-byte Ethernet frame; invalid lengths are not sent. |
| `ETH-RECV` | `( -- len \| 0 )` | Receive into `ETH-RX-BUF`; reject runts and frames over the 1514-byte no-FCS limit. |

### §16.1 ARP

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ARP-LOOKUP` | `( ip -- mac \| 0 )` | Return the MAC address for a reachable neighbor, or zero. |
| `ARP-INSERT` | `( ip mac -- )` | Publish/update a reachable neighbor; a late reply can rescue the same incomplete or failed entry. |
| `ARP-ENSURE` | `( ip -- entry \| 0 )` | Coalesce a neighbor-owned nonblocking discovery intent without transmitting. |
| `ARP-RESOLVE` | `( ip -- mac )` | Resolve IP to MAC via ARP request.  Blocks until reply. |
| `ARP-HANDLE` | `( -- flag )` | Validate and handle the frame in `ETH-RX-BUF`; sender L2/L3 identities must agree. |

### §16.2 IPv4

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `IP-BUILD` | `( proto dst payload paylen -- buf total )` | Build an IPv4 packet with the RFC ones-complement header checksum; payloads are limited to 1480 bytes and invalid input returns `0 0`. CRC is not an IP checksum. |
| `IP-SEND` | `( proto dst buf len -- ior )` | Send IP packet: route/ARP-resolve → Ethernet → NIC TX. Returns −1 for an invalid length or failed ARP resolution. |
| `IP-RECV` | `( -- hdr ip-len \| 0 0 )` | Accept only captured, checksum-valid IPv4/IHL=5 packets within the 1500-byte IP MTU; reject fragments. |
| `NEXT-HOP` | `( dst-ip -- hop-ip )` | Route: if dst is on subnet, return dst; otherwise return gateway. |

### §16.3 ICMP

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PING` | `( ip count -- )` | Send `count` ICMP echo requests and print results. Replies must match the target source address, identifier, and sequence. |
| `PING-IP` | `( a b c d count -- )` | Dotted-quad convenience wrapper for `PING`. |

### §16.4 UDP

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `UDP-SEND` | `( dst-ip dst-port src-port buf len -- ior )` | Send a UDP payload up to 1472 bytes; return −1 without sending for invalid lengths or failed ARP resolution. |
| `UDP-RECV` | `( -- src-ip udp-buf udp-len \| 0 0 0 )` | Receive only complete UDP headers whose declared length is bounded by and consistent with IPv4 before checksum verification. |

### §16.5 DHCP

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DHCP-START` | `( -- flag )` | Run full DHCP DISCOVER/OFFER/REQUEST/ACK.  Configures MY-IP, NETMASK, GW-IP. |

### §16.6 DNS

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DNS-RESOLVE` | `( c-addr len -- ip \| 0 )` | Resolve an A record; require matching server IP/ports, transaction ID, and echoed question, with every name/RR walk bounded by the UDP response length. |
| `DNS-LOOKUP` | `( "name" -- ip \| 0 )` | Parse domain from input, resolve via DNS. |

### §16.7 TCP

TCP has a bounded one-outstanding-segment send path, a 3-way handshake, receive
ring, passive open, TIME_WAIT reaper (60 s 2×MSL), and 1–256 TCB slots currently
derived from 25% of XMEM. The 256 ceiling is an implementation policy limit,
not a wire or architectural requirement. The standard networking loader
requires XMEM. A guarded one-connection Bank-0 allocation path remains for
manually composed builds, but it is not a qualified deployment profile. The
logical TLS-capable table cost is 238,328 bytes per connection; backing
allocator alignment rounds the four allocations independently. Exact XMEM
totals are 238,336, 476,656, and 714,992 bytes for one through three
connections.

Do not read the current fields as a general sliding window. The qualified data
profile retains one segment, admits against the peer/congestion windows,
advances only strict wrap-safe cumulative ACKs, trims an acknowledged prefix,
and replays the retained suffix from `SND-UNA` through bounded exponential RTO.
Fast retransmit and pure/window ACKs use the same retained intent model. Cache
loss is local backpressure: neighbor-owned ARP probing proceeds cooperatively,
TCP retry budget changes only after a replay reaches the NIC, and terminal
failure remains owner-visible until explicit cleanup.

Active open retains and replays its SYN from the original ISS. `SYN-SENT`
admits only an exact payload-free SYN+ACK acknowledging `ISS+1`; a bare SYN is
ignored because simultaneous open is outside this profile. Establishment
durably schedules the final ACK, and an exact duplicate SYN+ACK caused by a
lost final ACK is re-ACKed without disturbing established state. Passive open
admits only a bare SYN with no payload, reserves backlog before allocating a
child, records the exact listener incarnation, requires the expected sequence
and an ACK covering its SYN, and replays SYN+ACK with bounded exponential RTO.
Wire retry counts advance only after NIC admission; a separate bounded local
control-admission stall covers unresolved neighbors and NIC backpressure.
Retry or stall exhaustion reclaims the half-open child and releases its
reservation.

Each 5,952-byte TCB has an embedded **accept queue** (8 slots) plus exact
incarnation and authority state. `AQ-RESERVED` covers both half-open and queued
children, so the combined passive backlog cannot exceed eight. Queue slots
store `(slot+1, generation)`, not reusable pointers. `TCP-ACCEPT-CLAIM`
validates the exact attached listener, the child's generation and parent token,
and its queued/unowned state before transferring it to the new descriptor
owner. Listener teardown reclaims both half-open and queued children by exact
parent generation. Eight is the current safe implementation backlog, not a
universal capacity claim; making it caller/configuration-derived remains later
capacity work.

Attached transport authority is `(TCB address, generation, owner)`. A
1,000-byte TLS context stores the TCB generation at +968, its own incarnation
at +976, its reciprocal socket owner at +984, and slot/close lifecycle at
+992. `TLS-CLOSE-FREE` marks a released slot while preserving its last
generation, so one claim creates exactly one live incarnation. A 344-byte
socket descriptor stores the common generation at +32; its +40...+343 tail is
used only by secure listeners for copied credential/policy state, including the
protocol-defined 255-byte ALPN maximum. A TLS socket is valid only when
descriptor and context name each other and the context and TCB also form an
exact reciprocal pair. Graceful TLS
close retains the exact protected `close_notify` record until ACK and refuses
FIN while it remains in flight. FIN-WAIT-1, CLOSING, and LAST-ACK replay FIN
with bounded exponential RTO; FIN-WAIT-2 terminates after 60 seconds.
TIME-WAIT re-ACKs an exact duplicate FIN and restarts its 2MSL quarantine.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TCP-CONNECT` | `( ip remote-port local-port -- tcb \| 0 )` | Start an active open and return its SYN-SENT TCB. If the SYN cannot be emitted, reclaim the TCB and return 0. |
| `TCP-CONNECT-ATTACH` | `( ip remote-port local-port owner -- tcb generation ior )` | Atomically start an active open and attach its exact incarnation to one nonzero owner; reclaim the TCB if attachment fails. |
| `TCP-LISTEN` | `( port -- tcb )` | Passive open: listen for incoming SYN.  Initialises accept queue. |
| `TCB-HANDLE@` | `( tcb -- slot+1 generation )` | Return the exact incarnation token for a TCB. |
| `TCB-HANDLE-RESOLVE` | `( slot+1 generation -- tcb \| 0 )` | Resolve only a live, non-reserved exact incarnation. |
| `TCP-ATTACH` | `( tcb owner -- generation ior )` | Exclusively attach an eligible live TCB to a nonzero owner; one owner cannot attach to two TCBs. |
| `TCP-DETACH` | `( tcb generation owner -- ior )` | Remove only an exact reciprocal attachment. |
| `TCP-ACCEPT-CLAIM` | `( listener listener-generation listener-owner child-owner -- child child-generation ior )` | Validate the listener and queued child tokens, dequeue, and transfer the child to its new owner atomically. |
| `TCP-SEND-READY?` | `( tcb -- flag )` | Nonblocking readiness: established/close-wait, no retained flight, positive peer/congestion capacity, reachable neighbor, and idle NIC. A cache miss coalesces discovery but accepts no bytes. |
| `TCP-SEND` | `( tcb buf len -- actual )` | Accept at most one 1460-byte segment and the usable peer/congestion capacity. The cache-only send returns zero without advancing or overwriting retained state on neighbor/NIC backpressure. |
| `TCP-SEND-EXACT` | `( tcb buf len -- actual )` | All-or-none variant for an MSS-fitting protected record: zero or the exact requested length. |
| `TCP-RECV` | `( tcb buf maxlen -- len )` | Receive data.  Returns bytes read. |
| `TCP-POLL` | `( -- )` | Process at most one incoming IP frame, then perform at most one round-robin neighbor/TCP wire attempt or terminal publication. |
| `TCP-CLOSE-TRY` | `( unowned-tcb -- ior )` | Begin graceful close for an unattached TCB; reject attached authority and preserve state on failed FIN admission. |
| `TCP-CLOSE` | `( unowned-tcb -- )` | Compatibility wrapper that drops `TCP-CLOSE-TRY` status. |
| `TCP-OWNER-CLOSE` | `( tcb generation owner -- ior )` | Validate exact attached authority, begin graceful close, and detach only after success. Unacknowledged retained bytes return busy before FIN. |
| `.TCP` | `( -- )` | Print all active TCB connections. |
| `TCP-ABORT` | `( unowned-tcb -- status )` | Compatibility abort for an unattached connection; make one cache-only RST attempt when synchronized. |
| `TCP-OWNER-ABORT` | `( tcb generation owner -- status ior )` | Validate exact attached authority, optionally attempt one cached-route RST, and reclaim synchronously. |
| `TCB-USAGE` | `( -- used total )` | Count active (non-CLOSED) TCBs and pool size. |
| `TCB-REAP-TW` | `( -- )` | Reclaim TCBs stuck in TIME_WAIT past 2×MSL (60 s). |
| `TCB-FLUSH-TIMEWAIT` | `( -- )` | Force-reclaim all TIME_WAIT TCBs (test/debug). |
| `TCP-2MSL` | `( -- ms )` | TIME_WAIT duration constant (60 000 ms). |
| `/AQ-CAP` | `( -- n )` | Accept-queue capacity per listener (8). |
| `AQ-PUSH` | `( new-tcb listener -- flag )` | Validate passive lineage and enqueue the child's slot+1 and generation; -1 ok, 0 rejected. |
| `AQ-POP` | `( listener -- slot+1 generation \| 0 0 )` | Dequeue the oldest generation-bearing child token and release its backlog reservation. |

### §16.7a–§16.7d Certificate Verification

ASN.1/DER parsing, bounded X.509 path validation, P-256 ECDSA and fixed
RSA-2048 signature verification, and TLS Certificate/CertificateVerify
handler wiring.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DER-TAG@` | `( addr -- tag )` | Read DER tag byte. |
| `DER-LEN@` | `( addr -- len hdr-bytes )` | Decode DER length (short + long form 81/82/83/84). |
| `DER-NEXT` | `( addr -- val-addr val-len next-addr )` | Parse one TLV element, return value and next position. |
| `DER-ENTER` | `( addr -- inner-addr inner-len )` | Enter a constructed element (SEQUENCE, SET, context-tagged). |
| `DER-SKIP` | `( addr -- next-addr )` | Skip one TLV element entirely. |
| `DER-FIND-TAG` | `( addr limit tag -- val-addr val-len \| 0 0 )` | Find first element with matching tag. |
| `X509-PARSE` | `( cert clen -- ior )` | Parse a DER X.509 certificate into compatibility buffers. Returns 0, -1 for malformed or oversized copied fields, or -2 for a well-formed unsupported key profile. |
| `X509-DESC-PARSE` | `( cert clen desc -- ior )` | Parse one positive caller-bounded exact DER span into a borrowed-slice descriptor. Returns 0, -1 for malformed input, or -2 for a well-formed unsupported key profile. It has no generic 128..8192-byte policy bound; provisioning and wire consumers impose their own profile limits. |
| `X509-CHECK-HOST` | `( hostname hlen -- flag )` | Verify hostname against SAN dNSNames. Supports wildcards. 0=match, -1=no match. |
| `EC-DOUBLE` | `( Px Py Pz Rx Ry Rz -- )` | P-256 Jacobian point doubling. |
| `EC-ADD` | `( P1x P1y P1z P2x P2y P2z Rx Ry Rz -- )` | P-256 Jacobian point addition. |
| `EC-AFFINE` | `( Jx Jy Jz Ax Ay -- )` | Convert Jacobian → affine coordinates. |
| `EC-MUL` | `( k Px Py Rx Ry -- )` | Scalar multiplication k*P (double-and-add, 256-bit). |
| `ECDSA-P256-VERIFY` | `( hash pubkey sig slen -- flag )` | Verify ECDSA-P256-SHA256 signature. 0=valid, -1=invalid. |
| `RSA-E-BUSY` | `( -- -2 )` | RSA scratch is owned by another operation or the caller is off core 0; owner-pumped code should retry later. |
| `RSA2048-PUBLIC-BEGIN` | `( sig modulus em -- ior )` | Begin a core-0-only, owner-bound, fixed-exponent RSA-2048 public operation. Inputs are 256-byte big-endian values. |
| `RSA2048-PUBLIC-STEP` | `( -- status )` | Perform one bounded RSA unit. Returns 0 while pending, 1 when the encoded message is ready, or -1 outside the owning execution context/state. |
| `RSA2048-PUBLIC-FINAL` | `( -- ior )` | Finalize a ready operation and release its owner gate. Only the `(COREID,TASK-ID)` owner may call it. |
| `RSA2048-PUBLIC-CANCEL` | `( -- ior )` | Wipe incremental RSA scratch and release its owner gate. Only the owner may call it. |
| `RSA2048-PKCS1-SHA256-VERIFY` | `( hash modulus sig siglen -- flag )` | Verify exact RSA-2048 PKCS#1 v1.5 SHA-256 certificate padding. Blocking compatibility primitive. |
| `RSA2048-PSS-SHA256-VERIFY` | `( hash modulus sig siglen -- flag )` | Verify exact RSA-PSS/SHA-256 with MGF1-SHA256 and 32-byte salt. Blocking compatibility primitive. |
| `X509-VERIFY-CHAIN` | `( certs count hostname hlen now -- ior )` | Validate a bounded mixed ECDSA/RSA certificate path to a provisioned scoped anchor. |
| `TLS-PARSE-CERTIFICATE` | `( msg mlen -- ior )` | Parse the bounded client-side TLS Certificate message and authenticate its leaf through the configured path and hostname policy. This receive/path profile retains at most eight 128..8192-byte certificates. |
| `TLS-VERIFY-CERT-SIG` | `( ctx msg mlen -- flag )` | Verify RFC 8446 CertificateVerify using ECDSA-P256-SHA256 or RSA-PSS-RSAE-SHA256 according to the authenticated leaf key. |
| `TLS-CREDENTIAL-POOL-INIT` | `( count -- ior )` | Once on core 0, establish a caller-sized pool of exact 184-byte server-credential records. Capacity has no compile-time or connection-derived maximum. |
| `TLS-CREDENTIAL-PROVISION` | `( der-chain-a der-chain-u private-le -- slot+1 generation ior )` | Copy a leaf-first concatenation of self-delimiting DER certificates, structurally validate every Certificate envelope, deeply validate only the leaf, import one 32-byte little-endian P-256 scalar, prove its complete public point matches the leaf, and return a two-cell opaque handle. |
| `TLS-CREDENTIAL-PUBLIC` | `( slot+1 generation public-a scheme-a -- count ior )` | Resolve a live handle, optionally copy the 65-byte public point and `ecdsa_secp256r1_sha256` scheme cell, and return certificate count. Zero output addresses request only count/status. |
| `TLS-CREDENTIAL-CHAIN` | `( slot+1 generation out-a out-cap -- u ior )` | Copy the exact owned concatenated-DER chain. `0 0` is a length query; other failures leave output unchanged. |
| `TLS-CREDENTIAL-SIGN` | `( slot+1 generation hash-be der-a der-cap -- der-u ior )` | Sign one 32-byte SHA-256 digest through the lower-owned P-256 key. DER is staged and published atomically using its actual encoded length, up to 72 bytes. |
| `TLS-CREDENTIAL-SIGN-CANCEL` | `( slot+1 generation -- ior )` | Under lock 11 alone, request cancellation of that credential's exact active operation generation. Returns `BUSY` for same-core lock-10 activity and `NO-ACTIVE` when no sign is in progress. |
| `TLS-CREDENTIAL-DELETE` | `( slot+1 generation -- ior )` | On core 0, synchronously revoke a non-referenced credential, wipe its key/record and complete allocated DER-chain payload, free the payload, and stale the old handle. |
| `TLS-SERVER-CONTEXT-BEGIN` | `( ctx slot+1 credential-generation alpn-a alpn-u -- ctx-generation ior )` | Begin a server-role handshake with one pinned credential and an owned zero-or-one-name ALPN policy. Success returns the newly claimed nonzero context generation; every failure returns generation zero. Setup is atomic and the pin remains held until publish, abort, close, or another terminal path releases it. Callers must carry the returned generation rather than recover authority by rereading a reusable context slot. |
| `TLS-SERVER-ACCEPT-CLAIM` | `( listener-sd listener-h1 listener-generation -- ctx ctx-generation ior )` | Validate the exact secure-listener incarnation, copy its configured credential and ALPN policy into one newly pinned server context, and atomically transfer exactly one queued TCP child directly to that context. The child is never published as a plaintext socket. Empty backlog and lock/context contention return zero authority with `TLS-E-WOULD-BLOCK` or `TLS-E-BUSY`; every nonzero `ior` returns `(0,0,ior)`. An empty or contended claim does not advance a context generation. A stale listener triple cannot consume a replacement listener's queue. |
| `TLS-SERVER-ACCEPT-ATTACH` | `( ctx ctx-generation listener listener-generation listener-owner -- ior )` | Under TLS-to-NET lock order, validate the exact context incarnation, then consume at most one exact queued child into that prepared, pinned raw server context and publish reciprocal context/TCB generation authority atomically. A stale context generation is rejected before accept-queue mutation. An empty queue returns `TLS-E-WOULD-BLOCK` without mutation; other invalid context state is rejected before queue consumption, while a stale or malformed queued transport token follows the transport's bounded discard/reclaim rules. This is a lower attachment primitive, not a secure socket accept or lifecycle driver. |
| `TLS-SERVER-CLIENT-HELLO-STEP` | `( ctx ctx-generation -- progress alert ior )` | Make one bounded initial-handshake ingress step on the exact accepted child. It reassembles one ClientHello across arbitrary TCP segmentation and one or more nonempty TLSPlaintext handshake records; each ClientHello-fragment record may use legacy version `0x0301` or `0x0303`. A call completes at most one record and never consumes bytes from the following record. `NONE` plus `TLS-E-WOULD-BLOCK` means no complete record; `RECORD` plus zero `ior` means a nonfinal record committed and the cooperative upper owner should step again; `COMPLETE` plus zero `ior` means ClientHello admission committed. NET contention returns `NONE`/`TLS-E-BUSY`; peer framing/parser failure returns a fatal wire alert with zero `ior` and latches `CLOSING` while retaining the child and credential pin for alert/abort disposition. Dead exact transport is reclaimed; stale lower authority clears only the old binding and cannot touch a reused TCB incarnation. |
| `TLS-PARSE-CLIENT-HELLO` | `( ctx msg-a msg-u -- alert ior )` | Retain and transactionally admit one complete TLS 1.3 ClientHello on an unbound raw server context. Once TCB or socket authority is present, callers must use the attached ingress step. Peer protocol failures return a wire alert with zero `ior`; local failures use zero alert and a negative status. |
| `TLS-SERVER-PREPARE-HELLO-EXACT` | `( ctx ctx-generation -- alert ior )` | Generation-qualified server-hello preparation for a retained upper-layer operation. From an admitted ClientHello, apply pinned-chain signature policy, obtain checked ephemeral/random entropy, build exact ServerHello and EncryptedExtensions bytes, derive X25519/SHA-256 handshake secrets, install server-write/client-read record epochs at sequence zero, and publish the prepared server-hello phase last. A stale generation is rejected without touching a replacement context. Failures erase all phase output while retaining the admitted ClientHello and credential pin for alert/abort cleanup. |
| `TLS-SERVER-PREPARE-FLIGHT-EXACT` | `( ctx ctx-generation -- ior )` | Generation-qualified signed-flight preparation. From the prepared server-hello phase, stream the exact Certificate transcript, sign and construct CertificateVerify and Finished, commit the final transcript digest, derive master/application/exporter secrets without installing application record epochs, and initialize the post-ClientHello emitter union. Busy/cancelled signing preserves phase-one retry; admitted crypto failure is terminal. This word prepares immutable material but performs no transport callback, and stale authority cannot mutate a reused context. |
| `TLS-SERVER-FLIGHT-STEP-WITH` | `( ctx send-xt -- progress ior )` | Offer at most one retained server-flight record through `send-xt ( ctx record-a record-u -- actual )` without lock 10. The record is borrowed and read-only for the callback. Zero retains byte-identical retry state and returns `TLS-E-WOULD-BLOCK`; retries of that retained record must use the identical `send-xt`. The exact length commits the sequence/cursors and returns `TLS-SERVER-EMIT-RECORD` or `TLS-SERVER-EMIT-COMPLETE`; any short nonzero result, callback exception, or callback lock-10 leak is terminal. This socket-independent entry requires `TLS-CTX.TCB` to be zero. |
| `TLS-SERVER-FLIGHT-STEP` | `( ctx ctx-generation -- progress ior )` | Advance at most one retained server-flight record over the accepted-child pair sealed by flight preparation. The fixed adapter checks reciprocal authority inside its owner-qualified NET transaction and uses all-or-none TCP admission: NET contention returns `TLS-E-BUSY`, live zero-byte backpressure returns `TLS-E-WOULD-BLOCK`, and both retain the record and seal. A dead still-exact child is aborted before TLS binding and secrets are erased; stale lower authority is treated as already disposed, so only the old TLS binding is cleared and a reused TCB incarnation is untouched. Same-task NET ownership, caller-selected callbacks, and stale context incarnations cannot enter this attached path. |
| `TLS-SERVER-CLIENT-FLIGHT-BEGIN` | `( ctx early-wire-budget -- ior )` | From a completely emitted socket-independent server flight with both transport fields and the emitter seal zero, seal a nonnegative complete-wire-byte budget for discarding rejected 0-RTT records. Zero is valid. The budget is usable only when the owned ClientHello offered `early_data`; no hidden default is imposed. |
| `TLS-SERVER-CLIENT-FLIGHT-BEGIN-ATTACHED` | `( ctx ctx-generation early-wire-budget -- ior )` | Begin the same client-flight protocol state only when the caller carries the exact live server-context generation and the completed server flight retains a nonzero seal equal to the reciprocal accepted-child binding. Raw and stale authority are rejected before ingress state changes. |
| `TLS-SERVER-CLIENT-FLIGHT-FEED` | `( ctx bytes-a bytes-u -- consumed progress alert-desc ior )` | On the socket-independent zero-seal surface, copy at most through one complete client-flight record, retaining a partial header/body or Finished fragment per context; the caller retains and resubmits any unconsumed tail. Incomplete input returns the consumed count, `TLS-SERVER-INGRESS-NONE`, zero alert, and `TLS-E-WOULD-BLOCK`. Exact compatibility CCS is ignored. Failed C-HS trial decryption consumes the sealed 0-RTT budget without advancing sequence only until the first authenticated record. Successful exact client-Finished verification commits its transcript, installs C-AP read, and returns `TLS-SERVER-INGRESS-FINISHED`. Terminal progress returns an outbound alert description or the preserved peer alert description but does not claim wire transmission. |
| `TLS-SERVER-CLIENT-FLIGHT-STEP` | `( ctx ctx-generation -- progress alert-desc ior )` | Read and process at most one protected client-flight record over the completed flight's sealed accepted child. Each owner-qualified receive asks only for the missing header or exact declared record bound, so arbitrary TCP segmentation returns `NONE`/`TLS-E-WOULD-BLOCK`, a committed nonfinal record returns `RECORD`, and verified Finished returns `FINISHED` without consuming a following TCP record. NET contention is retryable. Known dead/stale transport is generation-exactly reclaimed; a receive throw wipes the complete retained lanes but preserves unresolved authority for `TLS-ABORT`. Protocol terminal results retain the S-AP write epoch for later protected disposition transmission. |
| `TLS-SERVER-INGRESS-DISPOSITION-STEP` | `( ctx ctx-generation -- progress ior )` | Consume the attached client flight's sticky terminal classification without accepting caller-provided alert bytes or a transport callback. `SEND-FATAL` emits protected level 2 plus the classified description; `SEND-CLOSE` emits protected warning `close_notify`; a non-close `PEER-ALERT` emits nothing. One 24-byte ciphertext remains connection-owned and byte-identical across send-window backpressure or NET contention, with S-AP sequence commit only after exact TCP admission. `TLS-SERVER-DISPOSITION-COMPLETE` means response admission or intentional no-response, not ACK or FIN. Pending disposition blocks `TLS-CLOSE-TRY`; after completion, close waits for any retained alert ACK before FIN. Dead/stale authority is generation-exactly reclaimed and cannot touch a replacement TCB incarnation. |
| `TLS-SERVER-SOCKET-PUBLISH` | `( ctx ctx-generation -- sd ior )` | Publish one fully authenticated attached server context as a reciprocal TLS descriptor. The transaction owns TLS, credential, then NET authority; revalidates the exact pinned context and sealed child; proves descriptor capacity before unpinning; and publishes handshake/context/socket state only after every fallible admission check. Wrong context/protocol state returns `(0,TLS-E-STATE)`, credential/NET contention or capacity returns `(0,TLS-E-BUSY)` without changing the ready context, stale child authority returns `(0,TLS-E-TRANSPORT)` while retaining sealed abort authority, and success returns `(sd,0)`. An exact attached child may already be in an owner-visible close/failure state; publication preserves that status for descriptor I/O and cleanup rather than orphaning it. Defensive post-unpin failure cleanup uses the held exact NET authority; an internal cleanup-invariant breach quarantines the private descriptor rather than making it allocator-visible. |
| `TLS-SERVER-CLOSE-EXACT-TRY` | `( ctx ctx-generation -- retired? ior )` | Attempt generation-qualified graceful close of a raw server context. Success or an already-stale incarnation returns true `retired?` and zero status; retryable lower contention returns false with its status. A stale saved generation never closes a live replacement in the same slot. |
| `TLS-ABORT-EXACT` | `( ctx ctx-generation -- retired? ior )` | Role-neutral, generation-qualified immediate secure teardown for cancellation and terminal cleanup. It reclaims exact transport authority, releases any credential pin, and wipes/releases only the matching context incarnation; an already-stale incarnation is idempotently reported retired. Busy cleanup returns false and remains retryable, without falling back to generationless authority. |

`TLSH-SERVER-FLIGHT-READY` (13) means that immutable plaintext flight material
and future secrets have published; it is not transport readiness or an
established connection. Initial ClientHello progress values are
`TLS-SERVER-CLIENT-HELLO-NONE` (0),
`TLS-SERVER-CLIENT-HELLO-RECORD` (1), and
`TLS-SERVER-CLIENT-HELLO-COMPLETE` (2).
Emitter progress values are none (0), one committed
record (1), and complete (2). Exact Finished admission installs only the S-AP
write epoch and publishes `TLSH-CLIENT-FINISHED-PENDING`, retaining the C-HS
read epoch and its sequence. Ingress progress is
`TLS-SERVER-INGRESS-NONE` (0), `TLS-SERVER-INGRESS-RECORD` (1),
`TLS-SERVER-INGRESS-FINISHED` (2), `TLS-SERVER-INGRESS-SEND-FATAL` (3),
`TLS-SERVER-INGRESS-SEND-CLOSE` (4), or
`TLS-SERVER-INGRESS-PEER-ALERT` (5). `SEND-FATAL` returns the fatal
description the future adapter must transmit; `SEND-CLOSE` returns
`close_notify`; `PEER-ALERT` preserves the peer description and requires no
response. Terminal and completed results are sticky and consume no further
input. Disposition progress is `TLS-SERVER-DISPOSITION-NONE` (0) or
`TLS-SERVER-DISPOSITION-COMPLETE` (1). Successful Finished leaves the context
authenticated in `TLSH-APPLICATION-READY`. A client or socket-independent
server uses `TLS-HANDSHAKE-PUBLISH`; a transport-bound
server must use exact-generation `TLS-SERVER-SOCKET-PUBLISH`, and the generic
entry refuses it without mutation. Budget overrun while the rejection window
remains open returns `TLS-E-EARLY-DATA-LIMIT` (-4220). Authenticated content
that cannot be the exact expected Finished—including a verify-data mismatch,
wrong handshake framing, or premature application data—returns
`TLS-E-PEER-FINISHED` (-4221). Protected peer alert/close returns
`TLS-E-PEER-ALERT` (-4201). Relevant alert descriptions are
`TLS-AD-BAD-RECORD-MAC` (20), `TLS-AD-RECORD-OVERFLOW` (22), and
`TLS-AD-DECRYPT-ERROR` (51). Owner/signer contention
returns `TLS-E-BUSY` (-4206), credential cancellation returns
`TLS-E-HANDSHAKE-CANCELLED` (-4217), and an admitted
signer/hash/key-schedule failure records terminal
`TLS-E-HANDSHAKE-CRYPTO` (-4216).

The public `EC-*` words above are the branch-bearing, public-data verification
path. They are not suitable for private scalars. Server signing uses a distinct
internal homogeneous-projective base-point operation with a fixed 256-round
architectural schedule and a fully scrubbed 960-byte owned workspace. Its
RFC 6979 ECDSA-P256-SHA256 composition uses a separate exact 856-byte lane,
four complete signing trials per ordinary fixed-work batch, unbounded
standards-correct batch continuation, and staged minimal DER publication by
actual caller capacity. These underscore-prefixed operations remain absent
from the public word table. The credential words above expose the lower-owned
key only through `(slot+1,generation)`; a raw private-key span is accepted only
at the core-0 import boundary and is never returned. The fixed schedule is an
architectural timing claim, not a physical DPA-resistance claim.

The provisioned server chain is a nonempty leaf-first concatenation of
self-delimiting DER Certificate values. There is no entry-count or private
8192-byte cap. Every entry must have an exact canonical outer Certificate
SEQUENCE with its three shallow children; only the leaf is parsed under the
native X.509 profile. Intermediates are otherwise opaque. The leaf must be a
non-CA uncompressed P-256 key, permit digital signatures and server
authentication when those extensions are present, and match the imported
scalar exactly. Provisioning proves identity consistency; it does not perform
client trust/path, hostname, validity, or chain-signature validation.

The handshake synthesizes each `CertificateEntry` as uint24 DER length, DER
bytes, and an initially empty uint16 extension vector. The admitted bound is
the exact wire sum `sum(DER length + 5) <= 0xFFFFFB`, derived from the TLS
uint24 handshake-body maximum after the empty request context and list length.
No TLS framing or connection-dependent extension bytes are stored in the
credential.

Credential status values are `0` success, then `-4320..-4334` for state,
range, allocation, capacity, stale handle, malformed DER chain, unsupported
leaf profile, invalid scalar, leaf/key mismatch, busy, alias, lower
crypto failure, cancelled, no active sign, and retired generation. Handle and
operation generations never publish zero; a slot or operation retires instead
of wrapping to a value that could revive stale authority. These are volatile
capability generations, not durable rollback counters.

### §16.8–§16.11 TLS 1.3

Authenticated bounded TLS 1.3 client profile plus a complete bounded
standard-profile listening server path. The server admits ClientHello,
transactionally constructs and emits its signed flight, bounds and discards
rejected 0-RTT TLSCiphertext, reassembles and authenticates client Finished
under C-HS, commits the transcript through that message, installs C-AP read,
and supports explicit establishment publication. It can atomically attach one
incarnation-safe accepted child to a prepared server TLS context, ingest the
initial ClientHello through owner-qualified TCP, and emit the complete
ACK-paced server flight through the same authority. It now reads the protected
client flight through that sealed child, authenticates Finished, preserves a
following TCP record, and publishes an exact reciprocal TLS descriptor. Sticky
terminal ingress can now admit one exact protected fatal/close response or
complete without a response for a non-close peer alert. `TLS-LISTEN` now
publishes copied listener policy and an exact credential pin atomically and
returns the opaque listener handle/generation used by the fused
`TLS-SERVER-ACCEPT-CLAIM` boundary;
the generic `LISTEN` entry remains fail closed for TLS descriptors, and
`SOCK-ACCEPT` remains fail closed for secure listeners. Akashic's persistent
listener owner drives the lower generation-qualified claim, ingress,
preparation, flight, disposition, publication, close, and abort entries through
XIO. Akashic owns scheduling, deadlines, cancellation, retained-result
adoption, and cooperative cleanup; KDOS continues to own credential authority,
wire protocol, authentication, socket publication, and exact teardown.
Successful descriptors enter the shared established KDOS-TLS NIO port and are
consumed unchanged by HCONN. Independent OpenSSL TLS 1.3 clients qualify the
actual TCP accept path, certificate and hostname verification, ALPN, HTTP I/O,
authenticated close, FIN, listener reuse, and recovery after cancellation,
timeout, malformed input, and cleanup contention.

A temporary caller-owned KDOS coordinator previously qualified this phase
sequence as a migration oracle. It and its public entries were removed after
the Akashic success and recovery journeys supplied equivalent composition
evidence; its recorded timings remain historical rather than current API
claims. Cipher-suite support is:

- **0x1301** — TLS_AES_128_GCM_SHA256 (standard RFC 8446 default)
- **0xFF01** — AES-256-GCM + SHA3-256 (explicit private profile)

Includes record-layer framing, bounded handshake reassembly, per-context
application receive retention, multi-message handshake processing, SNI
(Server Name Indication), Change Cipher Spec tolerance, and bounded
**server certificate verification** using
ECDSA-P256-SHA256 and RSA-2048/SHA-256 profiles. Public ClientHello messages
offer `ecdsa_secp256r1_sha256` and `rsa_pss_rsae_sha256` for
CertificateVerify, while `signature_algorithms_cert` separately permits
ECDSA-P256-SHA256 and RSA PKCS#1 v1.5 SHA-256 certificate signatures.
Record plaintext is bounded before scratch-buffer access. Application-data
and alert sends consume a write sequence number only after TCP accepts the
encrypted record.

The suite-dispatch and key-schedule words expose checked status from either
hash family rather than synthesizing success:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TLS-HASH` | `( ctx addr len out -- status )` | Dispatch from the context's sealed suite/hash pair and return either SHA3 or SHA-256 status unchanged. |
| `TLS-HMAC` | `( ctx key klen msg mlen out -- status )` | Dispatch to the context's negotiated HMAC and return its status. |
| `TLS-HKDF-EXTRACT` | `( ctx salt slen ikm ilen out -- status )` | Dispatch context-owned HKDF-Extract and return its status. |
| `TLS-HKDF-EXPAND` | `( ctx prk info ilen len out -- status )` | Dispatch context-owned HKDF-Expand and return its status. |
| `TLS-EXPAND-LABEL` | `( ctx secret label llen context clen olen out -- status )` | Build the TLS 1.3 HKDF label using the context's sealed hash profile. |
| `TLS-DERIVE-DERIVED` | `( ctx secret out -- status )` | Derive the TLS 1.3 `"derived"` secret and return status. |
| `TLS-DERIVE-SECRET` | `( ctx secret label llen out -- status )` | Hash the transcript under the context profile and derive a traffic secret. |
| `TLS-KS-HANDSHAKE` | `( ctx -- status )` | Derive both endpoint handshake traffic secrets and install local-write/peer-read record keys from the sealed client/server role. Stop at the first hash/HKDF failure and wipe admitted partial schedule state. |
| `TLS-KS-APPLICATION` | `( ctx -- status )` | Derive role-neutral application/exporter secrets. A client installs both record directions and enters `TLSH-APPLICATION-READY`; a server installs only its write direction and enters `TLSH-CLIENT-FINISHED-PENDING` while retaining its client-handshake read epoch. |
| `TLS-BUILD-FINISHED` | `( ctx rec -- reclen )` | Build one raw-context Finished record under exact TLS ownership. Socket-owned contexts and contexts retained by server flight/ingress return zero without destination, transcript, sequence, or owner-depth mutation; admitted owner-held handshake code uses the internal builder. |
| `TLS-HANDSHAKE-PUBLISH` | `( ctx -- ior )` | Publish `TLSS-ESTABLISHED` only from authenticated `TLSH-APPLICATION-READY` for client or unbound socket-independent composition. A server with live transport fields or retained transport-seal history is rejected without mutation and must use `TLS-SERVER-SOCKET-PUBLISH`, which couples establishment to exact descriptor publication. Superseded schedule secrets are wiped on success. |
| `TLS-EXPORT` | `( ctx label-a label-u context-a context-u out-a out-u -- ior )` | Derive 0..8160 authenticated exporter bytes into a non-aliasing caller span. Labels are printable 1..249-byte values; output is atomic and the exporter master is never exposed. |
| `TLS-ALPN-CONFIGURE` | `( ctx name-a name-u -- ior )` | Before handshake start, copy zero or one exact 1..255-byte ALPN ProtocolName into the connection context. Invalid input leaves the preceding configuration unchanged. |
| `TLS-ALPN-CONFIGURED` | `( ctx -- name-a name-u )` | Return the context-owned configured ProtocolName. |
| `TLS-ALPN-SELECTED` | `( ctx -- name-a name-u )` | Return the exact selection only for an established authenticated context; otherwise return `0 0`. |
| `TLS-ALPN-BUILD-OFFER` | `( ctx out-a out-u -- written ior )` | Atomically serialize the generic single-name ClientHello ALPN extension into bounded caller storage. |
| `TLS-ALPN-ACCEPT-SELECTION` | `( ctx ext-a ext-u -- ior )` | Validate and publish one exact EncryptedExtensions ALPN selection. |

Certificate and Finished verification map any nonzero hash status to their
existing failure result. Record/handshake builders map it to a zero-length or
failed result, so the higher-level `TLS-*` connection API remains fail-closed
without advancing connection state after a partially derived key schedule.
The private-suite empty hash is initialized through checked
`SHA3`; its retained failure is returned by `TLS-DERIVE-DERIVED` only in
SHA3 mode. The standard SHA-256 suite continues to use its independent fixed
empty-hash constant.

The public connection, record, application-data, alert, close/abort, crypto,
exporter, and ordinary credential entry points are serialized by
`TLS-OWNER-LOCK` (hardware lock 10). Recursion is tracked by
`(COREID,TASK-ID)` and a software depth, so a different task on the same
physical core cannot exploit the hardware lock's depthless same-core reacquire
behavior. Credential registry transitions briefly take hardware lock 11
beneath lock 10 and release it before calling lower crypto. Lock 11 is itself
bound to an exact nonrecursive `(COREID,TASK-ID)` software owner so depthless
same-core hardware reacquisition cannot steal or release it. The resulting
order is TLS 10, optionally credential 11 and release, KDOS HMAC/HKDF 9, then
checked BIOS crypto 8. `TLS-CREDENTIAL-SIGN-CANCEL` takes only lock 11, performs
no crypto, and therefore can publish an exact operation-generation request
while a signer retains lock 10. Contention is nonblocking: connection setup
records `TLS-CONNECT-E-BUSY`, exporters return `TLS-E-BUSY`, credential words
return their busy status, and transport operations make no scratch mutation.
Same-core cancellation while lock 10 is active returns credential busy;
different-core cancellation remains the concurrent path. A sequentially run
four-core emulator capstone has exercised a real full-batch signature and
peer-core cancellation with atomic output and complete owner/metadata cleanup.
TLS lock 10 may also nest the nonblocking network TX lock 12 for exact
transport admission. Lock 12 never acquires TLS, credential, or crypto locks;
it serializes shared Ethernet/IP/TCP staging and NIC descriptor ownership, not
independently parallel receive or TLS progress.
The server-flight callback is the inverse boundary: lock 10 is released before
the borrowed record is offered. A per-context nonrecursive driver claim keeps
the pending record and lifecycle exclusive while unlocked. Documented
same-context record, schedule, ALPN-publication, alert, post-handshake, receive,
and lifecycle mutators refuse from prepared emission through ingress completion
or terminal disposition until explicit publish, close, or abort. Callbacks that
return holding lock 10 are contained as terminal contract violations. Ingress
instead retains lock 10 while it copies caller bytes, authenticates at most one
record, and clears transient pointers before returning. The generic emitter and
client-flight feed still require an unbound context. A nonzero raw TCB pointer
never authorizes the generic callback; the separate attached emitter seals the
local pair, revalidates reciprocal generation authority inside the fixed NET
operation, and performs generation-safe terminal cleanup. Initial attached
ClientHello ingress uses the same TLS-to-NET order, retains partial record and
handshake bytes per context, and refuses the raw parser once transport
authority exists. The attached emitter is qualified through every ACK-paced
protected record and server Finished. Attached protected client-flight ingress
uses the same exact pair and record engine through client Finished. Attached
terminal disposition reuses that pair and the completed emitter's pending lane
through exact protected alert admission. Exact authenticated socket publication
now consumes that same generational authority and is no longer a transport
incompatibility. `TLS-LISTEN` now owns atomic listener-policy publication and
returns exact listener authority; `TLS-SERVER-ACCEPT-CLAIM` turns that
authority plus one queued child directly into a pinned server context. The
Akashic listener owner carries exact listener/context authority across
retryable empty waits and composes the complete lower handshake, disposition,
publication, close, and abort sequence through XIO. It adopts authenticated
publication into the shared NIO port; an independent peer qualifies that port
through HCONN application I/O and close. Broader protocol profiles and parallel
TLS execution remain separate maturity work.
The exporter uses 8,224 bytes of global staged-output
and intermediate scratch; its complete HkdfLabel scratch is 514 bytes. The TLS
context is 1,000 bytes: attached TCB generation at +968, context generation at
+976, reciprocal socket owner at +984, and slot/close lifecycle at +992.
`TLS-CLOSE-FREE` distinguishes a released slot while retaining its generation;
the next successful claim increments that generation and creates one live
incarnation.
Each context also owns a 230,688-byte receive/server
workspace:
a 16,896-byte partial-record lane and an aligned retained-data lane capable of
holding the bounded 73,732-byte post-handshake message, plus a 131,146-byte
ClientHello lane, an 8,192-byte one-way phase union, a 512-byte immutable
server-message ledger, and 200 bytes of exact flight metadata. The union is the
complete duplicate-extension bitmap during ClientHello admission; after flight
preparation it contains the leading TCP-MSS-sized pending-record lane, 152
bytes of emitter metadata, and 64 bytes of ingress metadata without changing
workspace geometry. Client-flight partial records use the existing 16,896-byte
record lane, and up to 36 fragmented Finished bytes use the per-context retained
lane. Incomplete encrypted application records, authenticated plaintext left
after a caller-sized read, and fragmented post-handshake messages therefore
survive across calls without aliasing another context. Cryptographic work and the transient global
plaintext buffer remain serialized by lock 10.  The high-level application
receive and owner-held blocking-handshake paths copy authenticated plaintext
into connection-owned or caller storage and scrub their complete global
staging buffer before releasing ownership.  The raw `TLS-DECRYPT-RECORD` word
writes to its caller-selected output and does not scrub that output.  Together
with the 5,952-byte TCB and two 344-byte socket
descriptors, the logical network-table cost is 238,328 bytes per connection,
before backing-allocator rounding.  Capacity is derived from the exact four
normalized table allocations rather than this logical quotient.

The caller-sized credential pool requires XMEM, is allocated once as
`count * 184` logical bytes plus only allocator alignment, and is protected
below `XMEM-FLOOR`. Requiring XMEM reflects the canonical networking-module
load geometry; it is not a fixed credential-count limit. Each concatenated
DER chain is a separate exact reclaimable allocation.
`TLS-CREDENTIAL-DELETE` wipes and frees that chain. The public `XMEM-RESET` is
credential-aware: with XMEM it refuses while any credential remains active,
then preserves the floor-protected pool after all credentials are deleted; on
a machine without XMEM its underlying bulk-reset action remains a no-op.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TLS-CONNECT` | `( rip rport lport -- tls \| 0 )` | TLS handshake over TCP without ALPN: ClientHello → authenticated key schedule → Finished. |
| `TLS-CONNECT-NAMED` | `( rip rport lport name-a name-u -- tls \| 0 )` | As above, requiring one exact caller-provided ALPN ProtocolName. |
| `TLS-CONNECT-HYBRID-NAMED` | `( rip rport lport name-a name-u -- tls \| 0 )` | Explicit private-hybrid ClientHello with the same generic ALPN contract. |
| `TLS-SEND` | `( tls buf len -- actual )` | Encrypt and use exact TCP admission; zero/backpressure does not consume the write sequence. |
| `TLS-RECV` | `( tls buf maxlen -- len )` | Receive and decrypt application data. |
| `TLS-CLOSE-TRY` | `( tls -- ior )` | Under the TLS owner, admit one exact protected `close_notify`, retain it through TCP acknowledgement, and only then begin owner-qualified FIN close. Backpressure or unacknowledged retained bytes return retryable status without wiping the context. |
| `TLS-CLOSE` | `( tls -- ior )` | Checked alias of `TLS-CLOSE-TRY`; zero means the TLS context is disposed, while nonzero retains retry authority. Raw calls reject socket-owned contexts. |
| `TLS-CLOSE-FINAL` | `( tls -- ior )` | Terminal raw-context cleanup: spend the bounded graceful-progress budget, then fall back to exact abort. Zero means the context is disposed; nonzero retains the context token for retry, although abort may already have reclaimed its transport before credential unpin reported busy. |
| `TLS-SEND-ALERT-TRY` | `( ctx level desc -- ior )` | Checked exact-send alert attempt. It reports `TLS-E-BUSY` on owner contention, `TLS-E-WOULD-BLOCK` when transport accepts nothing, and `TLS-E-TRANSPORT` after terminal TCP failure. A local fatal alert revokes authorization even when unsent; an accepted `close_notify` closes the TLS epoch. |
| `TLS-SEND-ALERT` | `( ctx level desc -- )` | Compatibility wrapper for alerts such as warning-level `1 0` `close_notify`; it drops the checked attempt status. |
| `TLS-IO-STATUS` | `( ctx -- ior )` | Return sticky TLS I/O status. Already-authenticated retained plaintext is stream-ordered first; while `APP-LEN` is nonzero a later terminal TCP failure is deferred, and the first observation after drain revokes the epoch and reclaims its exact TCB. |
| `TLS-ABORT` | `( ctx -- status )` | Immediately abort an exact raw-context TCB-generation/owner binding, then wipe and release the context without `close_notify`, ARP, polling, or waiting. Status is local, cached-route RST sent, no live transport, or busy. Busy retains the claimed context for retry; when credential unpin contends after transport reclamation, the TCB binding is already clear but the pin metadata remains exact. Socket-owned contexts use `SOCK-ABORT`. |
| `TLS-RECV-DATA` | `( ctx addr maxlen -- actual \| -1 )` | High-level receive: handles decryption plus per-context partial-record, retained-plaintext, and post-handshake-fragment state. An invalid or internal-alias destination returns zero without consuming connection state. |

The raw `TLS-READ-RECORD[-NB]`, `TLS-PROCESS-HS-MSG[S]`,
`TLS-RBUF-FILL[-NB]`, and `TLS-RBUF-CONSUME` words are internal building
blocks used beneath the owner-guarded connection and application paths. Direct
calls are reserved for sequential qualification fixtures; they are not
independently concurrent public transport operations.  Those global raw-record
helpers serve the owner-held blocking handshake path; application receive uses
its context-owned RX workspace for all state retained across public calls.

**Variables:** `TLS-SNI-HOST` (256-byte storage for a DNS name of at most 253 bytes),
`TLS-SNI-LEN` (current SNI length).

---

## §17 Socket API

BSD-style socket interface over TCP and TLS (§17 in `networking.f`). Each
344-byte descriptor stores its handle generation at +32: a TCB generation for
plain sockets or a TLS-context generation for TLS sockets. Plain operations
resolve reciprocal `(TCB, generation, descriptor-owner)` authority. TLS
operations first resolve reciprocal `(context, generation, descriptor-owner)`
authority and then the context's reciprocal TCB binding. Raw TLS-context entry
points reject a socket-owned context. Secure-listener lifecycle is composed by
the Akashic owner above this API: KDOS exposes the atomic listener and fused
claim plus the generation-qualified TLS server phases, while Akashic supplies
operation scheduling and lifecycle policy.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SOCKET` | `( type -- sd \| -1 )` | Create a socket descriptor.  *type*: 0 = TCP, 1 = TLS. |
| `BIND` | `( sd port -- ior )` | Set the local port; returns 0. |
| `LISTEN` | `( sd -- ior )` | Open a passive listener only for a TCP-marked descriptor. A TLS-marked descriptor fails closed with `-1`; use `TLS-LISTEN` so no plaintext or unconfigured listener state is published. |
| `TLS-LISTEN` | `( sd cred-h1 cred-gen alpn-a alpn-u early-wire-budget timeout-ms -- listener-h1 listener-generation ior )` | Atomically pin the exact server credential, copy the protocol-bounded ALPN and bounded-ingress/deadline values, create and attach the TCP listener, and publish the secure listener last. The caller retains its input descriptor; success returns the opaque TCB handle and generation needed with that descriptor by `TLS-SERVER-ACCEPT-CLAIM`, while failure returns `(0,0,ior)`. Ordinary failure rolls the TCB and credential reference back; secure-listener close reclaims queued/half-open children and releases the exact policy pin. |
| `TLS-SERVER-ACCEPT-CLAIM` | `( listener-sd listener-h1 listener-generation -- ctx ctx-generation ior )` | Nonblocking lower secure-accept boundary: claim one queued child directly into a prepared, credential-pinned server context using the listener's copied credential/ALPN policy. No plaintext accepted descriptor is exposed. Empty backlog and contention are retryable; every failure returns zero context authority. |
| `SOCK-ACCEPT` | `( sd -- sd' \| -1 )` | Reserve a descriptor, validate the exact listener and queued child tokens, and transfer the child owner before publishing an ordinary TCP socket. Refuse a TLS-marked listener before consuming its accept queue. |
| `CONNECT` | `( sd ip port -- ior )` | Open TCP and, for a TLS socket, complete the TLS handshake. |
| `SEND` | `( sd buf len -- n )` | Send data, return bytes sent. |
| `RECV` | `( sd buf maxlen -- n )` | Receive data, return bytes read. |
| `SOCKET-READY?` | `( sd -- flag )` | Level-ready for retained data or a terminal disposition. For TLS, authenticated `APP-LEN` remains ready and drains before a later transport failure is published. |
| `SOCK-TLS-IO-STATUS` | `( sd -- ior )` | Resolve exact reciprocal TLS descriptor authority and return sticky status. A fresh terminal transport result remains deferred while authenticated plaintext is retained. |
| `SOCK-TLS-CLOSE-EXACT-TRY` | `( sd -- ior )` | Nonblocking graceful close for a caller that already knows `sd` is a TLS descriptor. It acquires TLS and NET only with try operations, validates the reciprocal socket/context generation, and retains the one NET transaction through context teardown and exact descriptor release. TLS/NET/credential contention returns `TLS-E-BUSY` without discarding unresolved authority; zero proves the descriptor was released. It performs no generic descriptor-kind snapshot. |
| `SOCK-TLS-ABORT-EXACT-TRY` | `( sd -- status ior )` | Nonblocking immediate counterpart for a caller-known TLS descriptor. It validates and retires the reciprocal context and descriptor in one TLS-to-NET transaction; `status` is `TLS-ABORT-S-*`. Contention returns `TLS-ABORT-S-BUSY TLS-E-BUSY` and retains retry authority. It performs no generic kind snapshot or blocking NET acquisition. |
| `CLOSE-TRY` | `( sd -- ior )` | Close through the descriptor's exact authority; preserve the descriptor and handle on stale authority, backpressure, or contention. For a secure listener, drain the exact passive TCB lineage, release the credential pin, and wipe/release the descriptor. |
| `CLOSE` | `( sd -- ior )` | Checked alias of `CLOSE-TRY`; zero means the descriptor has been released, while nonzero preserves retry authority. |
| `SOCK-ABORT` | `( sd -- status ior )` | Immediately reclaim the descriptor's exact plain-TCB, reciprocal TLS-context, or secure-listener authority. A secure-listener abort drains its passive lineage, releases the credential pin, and wipes/releases the descriptor. `status` reports the transport disposition; nonzero `ior` leaves stale, busy, or wrong-state authority visible instead of releasing an unrelated descriptor. |
