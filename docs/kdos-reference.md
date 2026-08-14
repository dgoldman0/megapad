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
20. [`networking.f` §16 Network Stack](#16-network-stack)
21. [`networking.f` §17 Socket API](#17-socket-api)

---

## §1 Utility Words

Small general-purpose helpers used throughout KDOS.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `.R` | `( n width -- )` | Print number *n* right-justified in a field of *width* characters.  Currently a simplified implementation that drops the width and calls `.`. |
| `SAMESTR?` | `( addr1 addr2 maxlen -- flag )` | Compare two zero-padded byte strings up to *maxlen* bytes.  Returns `-1` if identical, `0` if they differ.  Uses the BIOS `COMPARE` word internally. |
| `PARSE-NAME` | `( "name" -- )` | Parse the next whitespace-delimited word from the input stream and copy it into `NAMEBUF` (a 16-byte scratch buffer), null-terminated.  Sets `PN-LEN` to the parsed length. |
| `NEEDS` | `( n -- )` | Stack safety guard — aborts with an error message if the data stack currently has fewer than *n* items.  Useful at the start of words that need a specific number of arguments. |
| `ASSERT` | `( flag -- )` | Abort with "Assertion failed" if the flag is false (zero).  Useful in tests and sanity checks. |
| `.DEPTH` | `( -- )` | Print the current stack depth in brackets, e.g., `[3 deep]`.  Handy for debugging stack issues. |
| `DEFER` | `( "name" -- )` | Create a deferred word whose action can be changed at run-time.  Defaults to `ABORT`.  Set the action with `IS`. |
| `IS` | `( xt "name" -- )` | Set the action of a deferred word.  E.g. `' my-open IS OPEN`. |

**Variables:** `NAMEBUF` (16-byte name scratch buffer), `PN-LEN` (parsed name length).

**Example:**
```forth
3 NEEDS          \ aborts if fewer than 3 items on stack
PARSE-NAME cat   \ copies "cat" into NAMEBUF, PN-LEN = 3
```

---

### §1.1 Memory Allocator

Dynamic heap allocator with first-fit free-list strategy.  The heap lives
above HERE with a 4 KiB dictionary guard.  All allocations are 8-byte
aligned with 16-byte minimum.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `ALLOCATE` | `( u -- addr ior )` | Allocate a strictly positive *u* bytes. Returns address and 0 on success, or 0 and -1 for zero, negative, unrepresentable, or unavailable sizes. |
| `FREE` | `( addr -- )` | Free a previously allocated block.  Merges adjacent free blocks. |
| `RESIZE` | `( addr u -- addr' ior )` | Resize an allocated block.  May move data.  Returns 0 on success. |
| `HEAP-SETUP` | `( -- )` | Initialize the heap (called automatically on first ALLOCATE). |
| `HEAP-FREE-BYTES` | `( -- n )` | Return total free bytes in the heap. |
| `.HEAP` | `( -- )` | Print heap statistics: total, free, largest block. |
| `MEM-SIZE` | `( -- n )` | Return total RAM in bytes (from SysInfo MMIO). |

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
| `AES-ENCRYPT` | `( key iv src dst len -- tag-addr )` | Encrypt *len* bytes from *src* to *dst*.  Returns address of 16-byte GCM tag. |
| `AES-DECRYPT` | `( key iv src dst len tag -- flag )` | Decrypt and verify.  Returns 0 if auth OK, -1 if auth failed. |
| `AES-ENCRYPT-BLK` | `( src dst -- )` | Process one 16-byte block (key/IV/CMD must already be set). |
| `AES-ENCRYPT-AEAD` | `( key iv aad aadlen src dst dlen -- tag-addr )` | Full AEAD encrypt with additional authenticated data (AAD). |
| `AES-DECRYPT-AEAD` | `( key iv aad aadlen src dst dlen tag -- flag )` | Full AEAD decrypt + verify with AAD.  Handles partial blocks correctly. |
| `.AES-STATUS` | `( -- )` | Print human-readable AES status. |

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

---

### §1.8 X25519 ECDH

Elliptic Curve Diffie-Hellman key exchange (RFC 7748) using the Field ALU
in mode 0 (X25519 scalar multiplication).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `X25519-CLAMP` | `( addr -- )` | Apply RFC 7748 clamping to a 32-byte scalar. |
| `X25519-PUBKEY` | `( priv pub -- )` | Compute public key from private key (base point × scalar). |
| `X25519` | `( priv peer shared -- )` | Full ECDH: shared = scalar × peer point.  All args are 32-byte addresses. |

---

### §1.9 HKDF Key Derivation

HMAC-based Key Derivation Function (RFC 5869).  Two families: SHA3-HMAC
(for cipher suite 0xFF01) and SHA-256 HMAC (for cipher suite 0x1301).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HKDF-EXTRACT` | `( salt slen ikm ilen out -- status )` | Checked SHA3-HMAC extract: PRK = HMAC(salt, IKM), with a 32-byte output; returns the HMAC status unchanged. |
| `HKDF-EXPAND` | `( prk info ilen len out -- status )` | Checked SHA3-HMAC expand: OKM = HMAC(PRK, info \|\| counter), up to 255×32 bytes; returns the first failure unchanged. |
| `HKDF-SHA256-EXTRACT` | `( salt slen ikm ilen out -- status )` | Checked extract (SHA-256): PRK = HMAC-SHA256(salt, IKM). 32-byte output on success. |
| `HKDF-SHA256-EXPAND` | `( prk info ilen len out -- status )` | Checked expand (SHA-256): OKM = HMAC-SHA256(PRK, info \|\| counter). Up to 255×32 bytes; returns the first hash failure. |

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
held fail-closed after the family scratch is wiped. This boundary contains
Forth exceptions, not architectural traps, and does not by itself release an
outer owner such as the networking module's TLS lock 10.

HKDF expansion preflights the complete output span and its fixed 32-byte PRK,
then publishes one successful 32-byte-or-smaller block at a time. If a later
checked hash operation fails, the word returns that first failure and leaves
the already-completed output prefix in place. No unrelated 8,160-byte staging
arena is imposed. Multi-window SHAKE wrappers have the same per-chunk
publication rule, with each BIOS `SHAKE-READ` itself all-or-nothing.

An HKDF expansion destination may not overlap its fixed 32-byte PRK or its
nonempty info span, because both inputs are reread for each output block. Such
an alias returns `CRYPTO-RANGE` for SHA3 HKDF or `SHA256-RANGE` for SHA-256
HKDF before publishing output.

The named HMAC/HKDF pads, intermediate buffers, normalized keys, counters, and
metadata are private KDOS implementation storage. Application key, message,
info, PRK, and destination spans must not alias them.

---

### §1.10 Field ALU

GF(2²⁵⁵−19) field arithmetic coprocessor with 8 operation modes.
Supersedes the original X25519-only interface.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FADD` | `( a b r -- )` | (a + b) mod p.  All args are 32-byte field element addresses. |
| `FSUB` | `( a b r -- )` | (a − b) mod p. |
| `FMUL` | `( a b r -- )` | (a · b) mod p. |
| `FSQR` | `( a r -- )` | a² mod p. |
| `FINV` | `( a r -- )` | a^(p−2) mod p (modular inverse via Fermat). |
| `FPOW` | `( a b r -- )` | a^b mod p (general modular exponentiation). |
| `FMUL-RAW` | `( a b r -- )` | Raw 256×256→512-bit multiply (64 bytes output, no reduction). |
| `F+` | `( a b r -- )` | Alias for `FADD`. |
| `F-` | `( a b r -- )` | Alias for `FSUB`. |
| `F*` | `( a b r -- )` | Alias for `FMUL`. |

---

### §1.11 NTT Engine

256-point Number Theoretic Transform for lattice-based post-quantum crypto.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `NTT-SETQ` | `( q -- )` | Set the NTT modulus (3329 for ML-KEM, 8380417 for ML-DSA). |
| `NTT-LOAD` | `( addr buf -- )` | Load 256 coefficients from memory.  *buf*: 0 = poly A, 1 = poly B. |
| `NTT-STORE` | `( addr -- )` | Store 256 result coefficients to memory. |
| `NTT-FWD` | `( -- )` | Forward NTT (time → frequency domain). |
| `NTT-INV` | `( -- )` | Inverse NTT (frequency → time domain). |
| `NTT-PMUL` | `( -- )` | Pointwise multiply A × B mod q. |
| `NTT-PADD` | `( -- )` | Pointwise add (A + B) mod q. |
| `NTT-STATUS@` | `( -- n )` | Read NTT status (0 = idle, 1 = busy, 2 = done). |
| `NTT-WAIT` | `( -- )` | Poll until NTT operation completes. |
| `NTT-POLYMUL` | `( a b r -- )` | Full polynomial multiply: r = a · b via forward NTT, pointwise multiply, inverse NTT. |
| `.NTT-STATUS` | `( -- )` | Print human-readable NTT status. |

---

### §1.12 ML-KEM-512 (Kyber)

Lattice-based key encapsulation mechanism (FIPS 203) using the KEM
accelerator and NTT engine.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KYBER-KEYGEN` | `( seed pk sk -- )` | Generate ML-KEM-512 keypair.  *seed*: 64 bytes, *pk*: 800 bytes, *sk*: 1632 bytes. |
| `KYBER-ENCAPS` | `( pk coin ct ss -- )` | Encapsulate with 32 caller-provided random bytes: produce ciphertext (768 bytes) and shared secret (32 bytes). |
| `KYBER-DECAPS` | `( ct sk ss -- )` | Decapsulate: recover shared secret from ciphertext using the secret key. |
| `KEM-STATUS@` | `( -- n )` | Read KEM accelerator status. |

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
| System heap | `HERE+4K .. 0x7F000` | `ALLOCATE` / `FREE` blocks |
| Stacks | `0x80000 .. 0xFFFFF` | Data stack + return stack |
| Userland dict | `EXT-MEM-BASE+N .. +U-ZONE-SIZE` | User word definitions + data |
| XMEM general | `+U-ZONE-SIZE .. end` | `XMEM-ALLOT` bump allocator |

| Word | Stack | Description |
|------|-------|-------------|
| `ENTER-USERLAND` | `( -- )` | Save system HERE, redirect to userland dictionary zone. |
| `LEAVE-USERLAND` | `( -- )` | Save userland HERE, restore system dictionary pointer. |
| `ULAND` | `( -- addr )` | Variable: 0 = system mode, 1 = userland mode. |
| `U-HERE` | `( -- addr )` | Current userland dictionary pointer (even when in system mode). |
| `U-USED` | `( -- u )` | Bytes used in the userland dictionary. |
| `U-FREE` | `( -- u )` | Bytes remaining in the userland zone. |
| `.USERLAND` | `( -- )` | Display userland memory status. |
| `U-ZONE-SIZE` | `( -- u )` | Constant: 32 MiB (size of the userland dictionary zone). |

> **Important:** Do not call `ENTER-USERLAND` inside interpret-mode
> `IF … THEN`.  The BIOS clears temporary code between `var_interp_if_start`
> and the current `HERE` after execution; since `ENTER-USERLAND` moves `HERE`
> to ext mem, this clear loop would wipe system RAM.  Wrap the call in a
> colon definition instead: `: _GO  XMEM? IF ENTER-USERLAND THEN ; _GO`.

---

## §2 Buffer Subsystem

Buffers are the core data container in KDOS.  A buffer is a contiguous,
**tile-aligned** (64-byte aligned) block of memory with a 4-cell (32-byte)
descriptor.  Up to **16 buffers** can be registered in the system.

### Buffer Descriptor Layout

```
Offset   Field         Meaning
───────  ────────────  ─────────────────────────────────────
+0       type          0=raw, 1=records, 2=tiles, 3=bitset
+8       elem_width    Bytes per element (1, 2, 4, or 8)
+16      length        Number of elements
+24      data_addr     Pointer to tile-aligned data region
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BUFFER` | `( type width length "name" -- )` | **Create a new buffer.**  Allocates a descriptor and a tile-aligned data region.  Registers it in `BUF-TABLE`.  Defines a CONSTANT named *"name"* that pushes the descriptor address.  This is the primary way to create buffers. |
| `B.TYPE` | `( desc -- type )` | Read the buffer type field. |
| `B.WIDTH` | `( desc -- width )` | Read the element width in bytes. |
| `B.LEN` | `( desc -- len )` | Read the element count. |
| `B.DATA` | `( desc -- addr )` | Read the data pointer. |
| `B.BYTES` | `( desc -- n )` | Total data size in bytes (length × width). |
| `B.TILES` | `( desc -- n )` | Number of 64-byte tiles needed to cover the data (ceiling division). |
| `B.FILL` | `( byte desc -- )` | Fill the entire buffer with a byte value. |
| `B.ZERO` | `( desc -- )` | Zero the entire buffer. |
| `B.INFO` | `( desc -- )` | Print a one-line summary: type, width, length, tiles, address. |
| `B.PREVIEW` | `( desc -- )` | Hex-dump the first tile (64 bytes) as 4 rows of 16 bytes.  Useful for quick data inspection. |
| `BUFFERS` | `( -- )` | List all registered buffers with their info. |

**Variables:** `BUF-COUNT`, `BUF-TABLE` (16-slot registry), `BDESC` (internal temp).

**Example — creating and using a buffer:**
```forth
0 1 256 BUFFER my-signal       \ raw, 1 byte/elem, 256 elements
42 my-signal B.FILL             \ fill every byte with 42
my-signal B.INFO                \ prints descriptor summary
my-signal B.PREVIEW             \ hex-dump first 64 bytes
BUFFERS                         \ list all registered buffers
```

---

## §3 Tile-Aware Buffer Operations

These words use the **MEX tile engine** (hardware SIMD) to perform fast
bulk operations on buffers.  They iterate over the buffer one 64-byte tile
at a time, using tile registers `TSRC0!`, `TSRC1!`, `TDST!`, and tile
instructions like `TSUM`, `TMIN`, `TMAX`, `TADD`, `TSUB`.

The default tile mode is `0` (8-bit unsigned, 64 lanes per tile).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `B.SUM` | `( desc -- n )` | Sum all bytes in the buffer using tile-accelerated reduction.  Iterates over tiles, accumulating with `TSUM`.  Returns the total. |
| `B.MIN` | `( desc -- n )` | Find the minimum byte value across the entire buffer.  Uses per-tile `TMIN`, then takes the minimum across tiles. |
| `B.MAX` | `( desc -- n )` | Find the maximum byte value.  Mirror of `B.MIN`. |
| `B.ADD` | `( src1 src2 dst -- )` | Element-wise addition of two buffers into a destination: `dst[i] = src1[i] + src2[i]`.  All three buffers must have the same tile count.  Uses `TADD` per tile — very fast. |
| `B.SUB` | `( src1 src2 dst -- )` | Element-wise subtraction: `dst[i] = src1[i] − src2[i]`.  Uses `TSUB` per tile. |
| `B.SCALE` | `( n desc -- )` | Multiply every byte in the buffer by *n* in-place.  This is a byte-by-byte loop (not tile-accelerated), clamping results to 0–255. |

**Example — tile-accelerated statistics:**
```forth
my-signal B.SUM .    \ print the sum of all bytes
my-signal B.MIN .    \ print the minimum byte
my-signal B.MAX .    \ print the maximum byte
```

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

---

## §7.5 File Abstraction

A **legacy file layer** built on raw sector access — before the named
filesystem (§7.6) was added.  Files here are identified by their starting
sector, not by name.  Up to **8 files** can be open.

### File Descriptor Layout

File descriptors are allocated from a fixed pool of 16 slots (1,152 bytes
total, allocated once at boot).  Each slot is 72 bytes; the returned
`fdesc` pointer starts at offset +8, so field accessors are unchanged.
Use `FCLOSE` to release a descriptor back to the pool when done.

```
Pool slot layout:
Offset   Field          Meaning
───────  ─────────────  ─────────────────────────────────────
−8       in_use         0 = free, −1 = in-use  (pool internal)
+0       start_sector   First sector on disk
+8       max_sectors    Allocated capacity in sectors
+16      used_bytes     How many bytes have been written
+24      cursor         Current read/write byte offset
+32      dir_slot       Directory slot index  (OPEN'd files)
+40      ext1_start     Second extent start sector
+48      ext1_count     Second extent sector count
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FILE` | `( start_sector max_sectors "name" -- )` | Create a file descriptor backed by disk sectors.  Defines a CONSTANT. |
| `FSEEK` | `( pos fdesc -- )` | Set the cursor to byte position *pos*. |
| `FREWIND` | `( fdesc -- )` | Reset cursor to 0 (start of file). |
| `FSIZE` | `( fdesc -- n )` | Return the used byte count. |
| `FTRUNCATE` | `( n fdesc -- )` | Set used bytes to *n* (clamped to capacity).  Adjusts cursor if past new size.  Does not zero freed bytes. |
| `FWRITE` | `( addr len fdesc -- )` | Write *len* bytes from *addr* at the current cursor.  Advances cursor.  Bounds-checked against capacity. |
| `FREAD` | `( addr len fdesc -- actual )` | Read up to *len* bytes at cursor into *addr*.  Returns actual bytes read.  Clamps to available data. |
| `F.INFO` | `( fdesc -- )` | Print file descriptor summary. |
| `FILES` | `( -- )` | List all registered legacy file descriptors. |

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

Dictionary changes are not automatically transactional.  A caller that
snapshots `HERE` and `LATEST` must restore both on failure, then call
`EVALUATOR-RESET`.  That order is intentional: reset clears compiler
bookkeeping but does not move the dictionary pointers or disturb an enclosing
`EVALUATE` frame.  The last status and diagnostics survive reset so the UI can
present them afterward.
Like `EVALUATE`, source-level data-stack effects are preserved.

## §7.6 MP64FS Filesystem

The **MP64FS** is a simple on-disk named filesystem with one draft format
marker and uniformly derived geometry through 8192 sectors (4 MiB).  It
supports 128 entries, 23-character names, and two extents per file.  See
`docs/filesystem.md` for the full on-disk format specification.

### Key Concepts

- **Superblock** (sector 0) — magic number `"MP64"`, marker, geometry
- **Bitmap** (starting at sector 1) — one bit per sector; count is
  `ceil(total_sectors / 4096)`
- **Directory** (the next 12 sectors) — 128 entries × 48 bytes each
- **Data area** — begins immediately after the derived directory

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

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FS-LOAD` | `( -- )` | Load and validate marker-1 superblock geometry against `DISK-SECTORS`, then cache the complete bitmap and directory.  Sets `FS-OK`. |
| `FS-SYNC` | `( -- )` | Write the in-RAM bitmap and directory back to disk.  Call after any changes. |
| `FS-ENSURE` | `( -- )` | Auto-load the filesystem if not yet loaded. |
| `FORMAT` | `( -- )` | **Initialize a fresh filesystem** using the attached media capacity.  Writes marker 1 and derived geometry, marks every metadata sector allocated, and clears the directory. |
| `DIR` | `( -- )` | List all files showing name, size, and type.  Also shows a free-space summary. |
| `CATALOG` | `( -- )` | Detailed directory listing with sector start, sector count, byte size, and type. |
| `FIND-BY-NAME` | `( -- slot \| -1 )` | Search the directory for a file matching `NAMEBUF`.  Caller must call `PARSE-NAME` first.  Returns the slot index or −1. |
| `MKFILE` | `( nsectors type "name" -- )` | Create a new file: allocate contiguous sectors, create directory entry, sync.  Checks for duplicate names. |
| `RMFILE` | `( "name" -- )` | Delete a file: free its bitmap sectors, clear the directory entry, sync. |
| `RENAME` | `( "oldname" "newname" -- )` | Rename a file.  Verifies the old name exists and the new name doesn't. |
| `CAT` | `( "name" -- )` | Print a file's contents to the terminal (reads sectors into memory, emits bytes). |
| `FS-FREE` | `( -- )` | Report disk free space: free sectors, bytes, and file count. |
| `SAVE-BUFFER` | `( buf "name" -- )` | Save a KDOS buffer's data to a named file on disk (file must already exist).  Updates `used_bytes` in the directory. |
| `OPEN` | `( "name" -- fdesc \| 0 )` | Open a file by name, returning a file descriptor from the FD pool for `FREAD`/`FWRITE` access.  Returns 0 if not found.  `OPEN` is a `DEFER` word — override with `' my-open IS OPEN` (e.g. for a VFS layer). |
| `FCLOSE` | `( fdesc -- )` | Release a file descriptor back to the FD pool.  No-op if `fdesc` is 0. |
| `LOAD` | `( "filename" -- )` | Open a Forth source file from disk, read it into memory, and EVALUATE each line.  This is how KDOS extensions and scripts are loaded. |
| `SOURCE-EVALUATE-CHECKED` | `( addr len -- status )` | Compile a complete in-memory source buffer with deterministic status and diagnostics; stop at first failure. |
| `DIRENT` | `( n -- addr )` | Address of directory entry *n* in the RAM cache (for low-level access). |

**Example — filesystem operations:**
```forth
DIR                          \ list all files
CAT getting-started          \ print a file's contents
4 MKFILE my-notes            \ create a 4-sector file of type "doc"
my-buffer SAVE-BUFFER my-data   \ save buffer to existing file
LOAD my-script.f             \ evaluate a Forth source file
FS-FREE                      \ check remaining space
```

---

### §7.6.1 Filesystem Encryption

Optional at-rest encryption for MP64FS files using AES-256-GCM.  Operates
on OPEN'd file descriptors.  Uses a system-level key stored in `FS-KEY`.
The IV is derived deterministically from the file's directory slot number.

On-disk layout of an encrypted file:
- Sectors contain: ciphertext (zero-padded to 16-byte boundary) `||` 16-byte GCM tag
- `used_bytes` in directory = original plaintext length (unchanged)
- `flags` bit 2 = encrypted

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FS-KEY!` | `( addr -- )` | Copy 32-byte encryption key into `FS-KEY`. |
| `ENCRYPTED?` | `( fdesc -- flag )` | True (-1) if file has the encrypted flag set. |
| `FENCRYPT` | `( fdesc -- ior )` | Encrypt an open file in-place on disk.  Returns 0 on success, -1 on error.  No-op if already encrypted or empty. |
| `FDECRYPT` | `( fdesc -- flag )` | Decrypt an encrypted file in-place.  Returns 0 if auth passed, -1 if failed.  On auth failure the file is unchanged. |

**Example:**
```forth
CREATE my-key 32 ALLOT   my-key 32 0 FILL   my-key FS-KEY!
OPEN secret              \ -- fdesc
DUP ENCRYPTED? .         \ 0 (not encrypted)
DUP FENCRYPT .           \ 0 (success)
FCLOSE                   \ release FD back to pool
```

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

The standard autoexec enables JIT for its own load, enters the 32 MiB XMEM
userland dictionary, loads `networking.f` with KDOS `REQUIRE`, configures DHCP
or the static fallback, loads `tools.f`, and disables JIT.  The module loader
batches both validated MP64FS extents into external memory, so the network
stack does not enlarge the Bank 0 core dictionary or alias the BIOS boot
buffer.

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
buf SAVE-BUFFER fname    \ save buffer to file
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
logical TLS-capable table cost is 237,720 bytes per connection; backing
allocator alignment rounds the four allocations independently. Exact XMEM
totals are 237,728, 475,440, and 713,168 bytes for one through three
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
generation, so one claim creates exactly one live incarnation. A 40-byte socket
descriptor stores at +32 either its plain TCB generation or its TLS-context
generation. A TLS socket is valid only when descriptor and context name each
other and the context and TCB also form an exact reciprocal pair. Graceful TLS
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
| `TLS-SERVER-ACCEPT-ATTACH` | `( ctx ctx-generation listener listener-generation listener-owner -- ior )` | Under TLS-to-NET lock order, validate the exact context incarnation, then consume at most one exact queued child into that prepared, pinned raw server context and publish reciprocal context/TCB generation authority atomically. A stale context generation is rejected before accept-queue mutation. An empty queue returns `TLS-E-WOULD-BLOCK` without mutation; other invalid context state is rejected before queue consumption, while a stale or malformed queued transport token follows the transport's bounded discard/reclaim rules. This is the lower attachment boundary, not yet a secure socket accept or handshake driver. |
| `TLS-SERVER-CLIENT-HELLO-STEP` | `( ctx ctx-generation -- progress alert ior )` | Make one bounded initial-handshake ingress step on the exact accepted child. It reassembles one ClientHello across arbitrary TCP segmentation and one or more nonempty TLSPlaintext handshake records; each ClientHello-fragment record may use legacy version `0x0301` or `0x0303`. A call completes at most one record and never consumes bytes from the following record. `NONE` plus `TLS-E-WOULD-BLOCK` means no complete record; `RECORD` plus zero `ior` means a nonfinal record committed and the coordinator should step again; `COMPLETE` plus zero `ior` means ClientHello admission committed. NET contention returns `NONE`/`TLS-E-BUSY`; peer framing/parser failure returns a fatal wire alert with zero `ior` and latches `CLOSING` while retaining the child and credential pin for alert/abort disposition. Dead exact transport is reclaimed; stale lower authority clears only the old binding and cannot touch a reused TCB incarnation. |
| `TLS-PARSE-CLIENT-HELLO` | `( ctx msg-a msg-u -- alert ior )` | Retain and transactionally admit one complete TLS 1.3 ClientHello on an unbound raw server context. Once TCB or socket authority is present, callers must use the attached ingress step. Peer protocol failures return a wire alert with zero `ior`; local failures use zero alert and a negative status. |
| `TLS-SERVER-PREPARE-HELLO` | `( ctx -- alert ior )` | From an admitted ClientHello, apply pinned-chain signature policy, obtain checked ephemeral/random entropy, build exact ServerHello and EncryptedExtensions bytes, derive X25519/SHA-256 handshake secrets, install server-write/client-read record epochs at sequence zero, and publish the prepared server-hello phase last. Failures erase all phase output while retaining the admitted ClientHello and credential pin for alert/abort cleanup. |
| `TLS-SERVER-PREPARE-FLIGHT` | `( ctx -- ior )` | From the prepared server-hello phase, stream the exact Certificate transcript, sign and construct CertificateVerify and Finished, commit the final transcript digest, derive master/application/exporter secrets without installing application record epochs, and initialize the post-ClientHello emitter union. Busy/cancelled signing preserves phase-one retry; admitted crypto failure is terminal. This word prepares immutable material but performs no transport callback. |
| `TLS-SERVER-FLIGHT-STEP-WITH` | `( ctx send-xt -- progress ior )` | Offer at most one retained server-flight record through `send-xt ( ctx record-a record-u -- actual )` without lock 10. The record is borrowed and read-only for the callback. Zero retains byte-identical retry state and returns `TLS-E-WOULD-BLOCK`; retries of that retained record must use the identical `send-xt`. The exact length commits the sequence/cursors and returns `TLS-SERVER-EMIT-RECORD` or `TLS-SERVER-EMIT-COMPLETE`; any short nonzero result, callback exception, or callback lock-10 leak is terminal. This socket-independent entry requires `TLS-CTX.TCB` to be zero. |
| `TLS-SERVER-FLIGHT-STEP` | `( ctx ctx-generation -- progress ior )` | Advance at most one retained server-flight record over the accepted-child pair sealed by flight preparation. The fixed adapter checks reciprocal authority inside its owner-qualified NET transaction and uses all-or-none TCP admission: NET contention returns `TLS-E-BUSY`, live zero-byte backpressure returns `TLS-E-WOULD-BLOCK`, and both retain the record and seal. A dead still-exact child is aborted before TLS binding and secrets are erased; stale lower authority is treated as already disposed, so only the old TLS binding is cleared and a reused TCB incarnation is untouched. Same-task NET ownership, caller-selected callbacks, and stale context incarnations cannot enter this attached path. |
| `TLS-SERVER-CLIENT-FLIGHT-BEGIN` | `( ctx early-wire-budget -- ior )` | From a completely emitted socket-independent server flight with both transport fields and the emitter seal zero, seal a nonnegative complete-wire-byte budget for discarding rejected 0-RTT records. Zero is valid. The budget is usable only when the owned ClientHello offered `early_data`; no hidden default is imposed. |
| `TLS-SERVER-CLIENT-FLIGHT-BEGIN-ATTACHED` | `( ctx ctx-generation early-wire-budget -- ior )` | Begin the same client-flight protocol state only when the caller carries the exact live server-context generation and the completed server flight retains a nonzero seal equal to the reciprocal accepted-child binding. Raw and stale authority are rejected before ingress state changes. |
| `TLS-SERVER-CLIENT-FLIGHT-FEED` | `( ctx bytes-a bytes-u -- consumed progress alert-desc ior )` | On the socket-independent zero-seal surface, copy at most through one complete client-flight record, retaining a partial header/body or Finished fragment per context; the caller retains and resubmits any unconsumed tail. Incomplete input returns the consumed count, `TLS-SERVER-INGRESS-NONE`, zero alert, and `TLS-E-WOULD-BLOCK`. Exact compatibility CCS is ignored. Failed C-HS trial decryption consumes the sealed 0-RTT budget without advancing sequence only until the first authenticated record. Successful exact client-Finished verification commits its transcript, installs C-AP read, and returns `TLS-SERVER-INGRESS-FINISHED`. Terminal progress returns an outbound alert description or the preserved peer alert description but does not claim wire transmission. |
| `TLS-SERVER-CLIENT-FLIGHT-STEP` | `( ctx ctx-generation -- progress alert-desc ior )` | Read and process at most one protected client-flight record over the completed flight's sealed accepted child. Each owner-qualified receive asks only for the missing header or exact declared record bound, so arbitrary TCP segmentation returns `NONE`/`TLS-E-WOULD-BLOCK`, a committed nonfinal record returns `RECORD`, and verified Finished returns `FINISHED` without consuming a following TCP record. NET contention is retryable. Known dead/stale transport is generation-exactly reclaimed; a receive throw wipes the complete retained lanes but preserves unresolved authority for `TLS-ABORT`. Protocol terminal results retain the S-AP write epoch for later protected disposition transmission. |
| `TLS-SERVER-INGRESS-DISPOSITION-STEP` | `( ctx ctx-generation -- progress ior )` | Consume the attached client flight's sticky terminal classification without accepting caller-provided alert bytes or a transport callback. `SEND-FATAL` emits protected level 2 plus the classified description; `SEND-CLOSE` emits protected warning `close_notify`; a non-close `PEER-ALERT` emits nothing. One 24-byte ciphertext remains connection-owned and byte-identical across send-window backpressure or NET contention, with S-AP sequence commit only after exact TCP admission. `TLS-SERVER-DISPOSITION-COMPLETE` means response admission or intentional no-response, not ACK or FIN. Pending disposition blocks `TLS-CLOSE-TRY`; after completion, close waits for any retained alert ACK before FIN. Dead/stale authority is generation-exactly reclaimed and cannot touch a replacement TCB incarnation. |

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
authenticated in `TLSH-APPLICATION-READY`; `TLS-HANDSHAKE-PUBLISH` remains the
explicit establishment boundary. Budget overrun while the rejection window
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

Authenticated bounded TLS 1.3 client profile plus a partially attached
standard-profile server handshake boundary. The server admits ClientHello,
transactionally constructs and emits its signed flight, bounds and discards
rejected 0-RTT TLSCiphertext, reassembles and authenticates client Finished
under C-HS, commits the transcript through that message, installs C-AP read,
and supports explicit establishment publication. It can atomically attach one
incarnation-safe accepted child to a prepared server TLS context, ingest the
initial ClientHello through owner-qualified TCP, and emit the complete
ACK-paced server flight through the same authority. It now reads the protected
client flight through that sealed child, authenticates Finished, preserves a
following TCP record, and reaches explicit establishment publication. Sticky
terminal ingress can now admit one exact protected fatal/close response or
complete without a response for a non-close peer alert. It does not yet accept
TLS sockets or demonstrate live socket interoperability with an independent
TLS implementation. Cipher-suite support is:

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
| `TLS-HANDSHAKE-PUBLISH` | `( ctx -- ior )` | Publish `TLSS-ESTABLISHED` only from authenticated `TLSH-APPLICATION-READY`. For a client this follows its local Finished boundary; for a server it follows `TLS-SERVER-CLIENT-FLIGHT-FEED` or the attached `TLS-SERVER-CLIENT-FLIGHT-STEP` verifying peer Finished, staging the completed transcript, and installing C-AP read. Superseded schedule secrets are wiped. |
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
through exact protected alert admission. Authenticated socket publication is
now the active transport incompatibility, not a reason for further TCP or
crypto expansion.
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
with the 5,952-byte TCB and two 40-byte socket
descriptors, the logical network-table cost is 237,720 bytes per connection,
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
40-byte descriptor stores its handle generation at +32: a TCB generation for
plain sockets or a TLS-context generation for TLS sockets. Plain operations
resolve reciprocal `(TCB, generation, descriptor-owner)` authority. TLS
operations first resolve reciprocal `(context, generation, descriptor-owner)`
authority and then the context's reciprocal TCB binding. Raw TLS-context entry
points reject a socket-owned context.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `SOCKET` | `( type -- sd \| -1 )` | Create a socket descriptor.  *type*: 0 = TCP, 1 = TLS. |
| `BIND` | `( sd port -- ior )` | Set the local port; returns 0. |
| `LISTEN` | `( sd -- ior )` | Open a passive listener only for a TCP-marked descriptor. A TLS-marked descriptor fails closed with `-1` without allocating a TCB or changing descriptor state/handle until authenticated secure accept exists. |
| `SOCK-ACCEPT` | `( sd -- sd' \| -1 )` | Reserve a descriptor, validate the exact listener and queued child tokens, and transfer the child owner before publishing an ordinary TCP socket. Refuse a TLS-marked listener before consuming its accept queue. |
| `CONNECT` | `( sd ip port -- ior )` | Open TCP and, for a TLS socket, complete the TLS handshake. |
| `SEND` | `( sd buf len -- n )` | Send data, return bytes sent. |
| `RECV` | `( sd buf maxlen -- n )` | Receive data, return bytes read. |
| `SOCKET-READY?` | `( sd -- flag )` | Level-ready for retained data or a terminal disposition. For TLS, authenticated `APP-LEN` remains ready and drains before a later transport failure is published. |
| `SOCK-TLS-IO-STATUS` | `( sd -- ior )` | Resolve exact reciprocal TLS descriptor authority and return sticky status. A fresh terminal transport result remains deferred while authenticated plaintext is retained. |
| `CLOSE-TRY` | `( sd -- ior )` | Close through the descriptor's exact authority; preserve the descriptor and handle on stale authority, backpressure, or contention. |
| `CLOSE` | `( sd -- ior )` | Checked alias of `CLOSE-TRY`; zero means the descriptor has been released, while nonzero preserves retry authority. |
| `SOCK-ABORT` | `( sd -- status ior )` | Immediately reclaim the descriptor's exact plain-TCB or reciprocal TLS-context authority. `status` reports the transport disposition; nonzero `ior` leaves stale, busy, or wrong-state authority visible instead of releasing an unrelated descriptor. |
