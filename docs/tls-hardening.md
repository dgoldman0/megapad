# Native TLS Hardening

Status: no usable listening TLS server yet; the bounded client profile,
socket-independent server handshake through client Finished, exact lower
transport ownership/close, and atomic queued-child attachment are qualified,
and attached initial ClientHello ingress plus the ServerHello exact-send
boundary are now qualified. ACK-paced protected-flight completion, attached
protected ingress, protected dispositions, and authenticated accepted-socket
publication are the single active critical path.
Last updated: 2026-08-14

## Purpose

MegaPad must authenticate a remote service on the machine itself. A Linux
companion, emulator service, or host certificate library is not part of the
security architecture. Host tools may generate fixtures and drive tests, but
KDOS parses the wire message, builds the path, verifies signatures, checks the
clock and hostname, and gates application traffic.

The relevant standards are [TLS 1.3 (RFC 8446)](https://www.rfc-editor.org/rfc/rfc8446),
[TLS 1.3 Traces (RFC 8448)](https://www.rfc-editor.org/rfc/rfc8448),
[the TLS exporter channel binding (RFC 9266)](https://www.rfc-editor.org/rfc/rfc9266),
and [PKIX certificates (RFC 5280)](https://www.rfc-editor.org/rfc/rfc5280).
The current implementation is a deliberately bounded profile of those
standards, not a complete WebPKI implementation.

## Server-Role Boundary and Readiness

TLS server support belongs in the KDOS networking module.  KDOS already owns
TCP, the TLS record layer, transcript and key scheduling, X.509 mechanics,
alerts, and connection cleanup.  Higher layers should receive an authenticated
secure byte stream and should not implement another TLS engine.  Listener
policy, application protocol behavior, peer authorization, and service
identity remain higher-layer responsibilities.

The current implementation is not yet a complete listening TLS server. KDOS
admits and retains a full-width ClientHello, applies the pinned credential
policy, obtains one checked 64-byte entropy transaction, constructs exact
ServerHello and EncryptedExtensions messages, performs X25519, hashes
ClientHello || ServerHello, and installs role-correct handshake epochs as one
atomic phase. A second transaction streams the exact pinned Certificate chain
into the transcript, signs the RFC 8446 server CertificateVerify context,
constructs exact CertificateVerify and Finished messages, commits the final
transcript digest, and derives master, application, and exporter secrets. A
connection-owned emitter now retains one exact record across backpressure,
emits plaintext ServerHello followed by MSS-fitting protected records through
Finished, commits its sequence and cursors only after exact admission, and
installs only the S-AP write epoch after Finished admission. Its transport
callback qualification remains socket-independent.
`TLS-SERVER-CONTEXT-BEGIN` returns its newly claimed context generation, and
`TLS-SERVER-ACCEPT-ATTACH` requires that carried token before it consumes one
generation-qualified queued child and publishes reciprocal context/TCB
authority in one TLS-to-NET transaction. Thus stale context authority is
rejected before queue mutation. `TLS-SERVER-FLIGHT-STEP` seals that exact pair
during flight preparation, uses a dedicated owner-qualified TCP adapter, and
retains byte-identical backpressure. It exact-aborts a dead current child;
stale lower authority clears only the old TLS binding and cannot reclaim a
reused TCB incarnation. `TLS-SERVER-CLIENT-HELLO-STEP` now reads the initial
handshake through that same pair. It reassembles arbitrary TCP segmentation
and multiple nonempty TLSPlaintext records; each ClientHello-fragment record
may use legacy version `0x0301` or `0x0303`. It consumes at most one record per
call. Fatal
peer input latches a closing disposition while preserving the binding and pin;
dead or stale transport follows the same exact-incarnation cleanup rule.
Protected client-flight ingress and disposition output are not yet bound to
the child.
The bounded inbound engine now rejects offered 0-RTT under a sealed caller
wire-byte budget, authenticates and reassembles the exact client Finished,
commits the transcript through that message, installs C-AP read, and leaves
establishment behind the existing explicit publication boundary. A
socket-independent composition test uses a Python-standard-library
SHA-256/HMAC/HKDF oracle plus a fixed externally generated AES-GCM
client-Finished record, reaches publication, checks published ALPN and
independently derived exporter output. This qualifies the byte-level protocol
boundary; it is not live interoperability with an independent TLS stack.
The remaining server work is ACK-paced completion of the protected outbound
flight, attached client-flight ingress, protected terminal output, secure
socket accept/publication, and
interoperability over that path with an independent TLS implementation. The
following lower-level facts
continue to bound an authenticated server role:

- P-256 `EC-MUL` branches on scalar bits and remains qualified only for public
  verification data. Private signing now uses the separate fixed-schedule
  operation documented below; the two paths are not interchangeable.
- The fixed RSA-2048 path implements the public exponent only.  There is no
  private RSA operation, PSS signing path, or blinding contract.
- KDOS now has a lower-owned private-key and TLS credential-handle lifecycle.
  Its two-cell opaque handle prevents accidental raw-pointer exposure to
  higher layers, but the storage remains ordinary KDOS memory and cannot claim
  HSM-style isolation from arbitrary local supervisor code on this machine.
- The checked `ENTROPY-FILL` BIOS interface fails atomically, but the checked-in
  RTL TRNG backend is deterministic development logic.  Emulator vectors can
  qualify protocol behavior; they cannot establish physical key-generation or
  ephemeral-secret entropy claims.
- Handshake transcript and certificate state, the blocking client-handshake
  record buffer, transient plaintext, and several cryptographic scratch arenas
  remain module-global.  The lower module serializes their public use under one
  exact owner.  Application partial records, retained plaintext, and
  post-handshake fragments are now connection-owned, so alternating receive
  calls cannot overwrite another context's cross-call state.  True concurrent
  TLS execution remains unsupported until the remaining shared state becomes
  connection-owned.

The first interoperable server signature profile is
`ecdsa_secp256r1_sha256`, which matches the existing certificate parser and
client signature offer. The native secret-scalar operation, deterministic
RFC 6979 generation, fixed-work signing arithmetic, canonical DER staging,
complete signer scratch cleanup, lower-owned credential storage, public-key
matching, and cancellation publication arbitration are implemented. Closing
the server gate still requires composing the now-atomic
generation/owner-qualified child transfer with the qualified outbound callback
and inbound feed contracts, secure listener/accept integration, and
external-stack socket interoperability.
Reusing `EC-MUL` or precomputing a fixture signature remains
test scaffolding rather than a server security result.

Generic ALPN bytes, the TLS 1.3 exporter construction, per-context negotiated
hash state, per-context application RX state, and enforced serialized scratch
ownership are now implemented as independently useful client/server substrate.
They do not by themselves make a listening socket TLS-capable.  `LISTEN`
therefore returns `-1` for a TLS-marked socket without allocating a listener
TCB or changing the descriptor state/handle; `SOCK-ACCEPT` also refuses a
TLS-marked listener before consuming its accept queue.  Evidence must keep
emulator protocol correctness, RTL behavior, and physical entropy/side-channel
claims separate.

The negotiated crypto profile is now connection-owned.  A live context carries
an immutable endpoint role and an exact cipher-suite/hash pair.  ServerHello
publishes the hash identifier first and the suite last; the suite is the sealed
marker.  Hash, HMAC, HKDF, Finished, key schedule, and each record operation
reject an unset or mismatched pair instead of consulting a process-global mode
bit.  Record operations also reselect the AES width from their context before
touching output.  The machine-wide TLS owner described below serializes the
AES selector and larger scratch arenas; this prevents cross-context corruption
but does not make operations concurrent.

The public `TLS-BUILD-FINISHED` builder is a raw-context operation. It rejects
socket-owned contexts and contexts retained by either half of the server
flight/ingress protocol, returning zero without changing the destination,
transcript, write sequence, or owner depth. Owner-held handshake code enters
the parenthesized builder only after its own lifecycle admission. This keeps a
reusable context address from bypassing descriptor or server-driver authority.

Handshake and application traffic secrets retain their RFC endpoint names in
the context (`c_*` and `s_*`), while record directions are selected from the
immutable endpoint role.  Thus client write/server read use client traffic
secrets and server write/client read use server traffic secrets.  The server
application transition is intentionally split. Flight preparation has already
derived the transcript-bound application/exporter secrets without installing
record epochs. After the exact Server Finished bytes are accepted, the server
installs only the prederived S-AP write epoch and enters
`TLSH-CLIENT-FINISHED-PENDING`. Its client-handshake read key and sequence
remain live; only a verified and transcript-committed client Finished permits
installation of the prederived C-AP read epoch and
`TLSH-APPLICATION-READY`. This prevents transport backpressure or a
premature state publication from discarding the key needed to authenticate the
peer's final handshake message.  Once an admitted key-schedule or epoch-install
operation fails, the context records the exact terminal error while wiping all
partial schedule and record secrets; neither the schedule nor the final server
read cutover can be retried from cleared prerequisites.

## Security Invariant

`TLS-SEND-DATA` is reachable only after all of the following succeed in order:

1. `ServerHello` and the handshake key schedule.
2. `EncryptedExtensions` with exact handshake framing.
3. A complete server `Certificate` message.
4. A path from leaf to an explicitly provisioned and hostname-scoped anchor.
5. Certificate validity, CA constraints, key usage, EKU, SAN, and hostname.
6. Every required ECDSA-P256-SHA256 or RSA-2048/PKCS#1-SHA256 certificate
   signature.
7. `CertificateVerify`, proving possession of the authenticated leaf key.
8. The server `Finished` MAC.
9. Exact ALPN byte selection when the caller configures a required protocol.

For the client role, only step 7 sets `TLS-CTX.PEER-AUTH`. The application key
schedule additionally requires `TLSH-SERVER-FINISHED`; it stages application
keys and the exporter master secret in `TLSH-APPLICATION-READY` without
publishing an established connection. For the server role the same bit means
that the required peer handshake proof is complete: it remains clear while the
server holds its client-handshake read epoch and is set only after client
Finished verifies and the client application read epoch installs. Only after
the complete local Finished boundary is accepted does `TLS-HANDSHAKE-PUBLISH`
set `TLSS-ESTABLISHED`, expose exporter use, and wipe superseded schedule
secrets. Starting a new ClientHello clears retained authentication and exporter
state.

## Implemented Native Profile

### Bounded DER and X.509

`DER-READ` accepts definite, canonical DER lengths of at most four length
octets and never reads beyond the caller's limit. `X509-DESC-PARSE` accepts a
positive caller-bounded span whose canonical outer DER object consumes that
span exactly, and records borrowed slices in a 208-byte `/X509-CERT`
descriptor. It has no private 128-through-8192-byte certificate limit. The
trust-bundle and client Certificate-message surfaces impose that range as
their own profile bound; the server credential surface instead uses the TLS
uint24 framing bound described below.

The parser currently requires:

- X.509 version 3.
- A P-256 uncompressed SubjectPublicKeyInfo, or a canonical full-width
  RSA-2048 `rsaEncryption` key with explicit NULL parameters and exponent
  65537.
- ECDSA with SHA-256, RSA PKCS#1 v1.5 with SHA-256, or ECDSA with SHA-384 on a
  certificate used only as an explicitly trusted anchor.
- Strict outer/TBS signature-algorithm agreement.
- UTC or Generalized time with seconds and `Z`.
- Bounded BasicConstraints, KeyUsage, EKU, SAN, SKI, and AKI parsing.
- Rejection of duplicate recognized extensions and unknown critical
  extensions.

DNS names use ASCII-only labels of at most 63 bytes. Empty labels, illegal
punctuation, edge hyphens, embedded wildcards, and broad forms such as `*.com`
are rejected. A wildcard covers exactly one leftmost label. IDNA and IP address
SANs are not implemented.

### P-256 ECDSA

The verifier converts network big-endian values to the Field ALU's
little-endian representation, enforces canonical DER integers, checks
`0 < r,s < n`, validates the public point against the P-256 curve, computes the
two scalar products, rejects infinity, and compares `R.x mod n` with `r`.

ECDSA verification operates only on public data. Its Jacobian
`EC-MUL`/`EC-ADD`/`EC-DOUBLE` path branches on scalar bits and exceptional
points, so it is not a private-key primitive and must not be reused for
signing.

Server-signing work uses a separate internal P-256 base-point substrate. It
uses complete homogeneous-projective formulas for `a=-3`, executes exactly
256 MSB-first double/add/select rounds for every admitted scalar, and selects
coordinates with Field arithmetic rather than `FCMOV`. Scalar range admission
scans all 32 bytes; an invalid scalar still executes the full path with a
dummy scalar and cannot publish either staged output. This gives a fixed
architectural instruction schedule with respect to valid scalar contents. It
does not claim physical power-analysis resistance or make caller-address
qualification constant-cycle.

The internal operation is serialized by the machine-wide TLS owner. Its
single exact 960-byte workspace includes scalar, projective points, staged
affine output, pointer metadata, and cleanup sinks. Success, rejection after
owner acquisition, and caught Forth exceptions clear the inversion state used
by the operation, clear Field ACC and raw-multiply previous-result halves,
scrub the complete workspace, and release ownership. A busy return acquires
nothing and leaves the current owner's workspace untouched. The raw internal
entry remains qualification substrate rather than a public raw-key API. The
credential words described below retain long-term private keys in lower-owned
records and expose only opaque two-cell generational handles.

The internal ECDSA-P256-SHA256 composition adds an exact 856-byte signing
lane, for 1,816 bytes across the two newly composed private lanes while it
invokes the 960-byte base-point operation. Existing lock-9 HMAC/SHA scratch is
separately owned and accounted. It implements RFC 6979 `bits2octets`, K/V
initialization, and deterministic candidates with public checked
HMAC-SHA256. Candidate processing is arranged in unrolled four-trial batches:
all four trials execute a real fixed-schedule base multiplication and complete
`r`/`s` arithmetic, even after an earlier usable result. Four is not a retry
cap. An exhausted batch performs candidate four's rejection transition and
continues with another batch, so zero/out-of-range `k` and the required
ECDSA-level `r=0` or `s=0` retry semantics are complete without a hidden
attempt limit. `TLS-CREDENTIAL-SIGN` arms one exact operation generation, and
the signer samples it only after a complete four-trial batch and before
accepting that batch's result. Cancellation therefore has batch-boundary
latency without truncating private arithmetic. The credential wrapper signs
into a private DER lane and arbitrates a request arriving after that sample
under lock 11 before caller publication. Every terminal cleanup disarms the
borrowed generation cell.

Modulo-order arithmetic uses ordinary residues under the custom P-256 order
with its deliberate zero Montgomery-selector buffer. The signer reduces the
SHA-256 digest and affine x-coordinate, computes
`s = k^-1 (h + r*d) mod n`, and selects the first complete result with
arithmetic byte masks. Only then does it construct minimal positive ASN.1
INTEGERs in a staged 72-byte DER lane. Publication checks the actual encoded
length, so a 71-byte signature accepts an exact 71-byte caller capacity rather
than imposing the mathematical 72-byte maximum as a blanket minimum. Low-S
normalization is not applied; it is not required by TLS and would disagree
with the RFC 6979 Appendix A vector.

Invalid keys run the complete dummy-key private path but cannot publish.
Range, capacity, and alias errors are atomic. Acquired-owner exits and caught
Forth exceptions scrub both private lanes, clear the Field inversion/ACC/raw
multiply state touched by signing, and restore exactly one recursive owner
depth. Lower HMAC operations independently own and wipe their scratch. The
qualification establishes an ordinary-case fixed architectural schedule, not
physical power-analysis resistance, and Forth `CATCH` does not contain an
architectural trap.

### Lower-owned server credentials

`TLS-CREDENTIAL-POOL-INIT` is a once-only core-0 control-plane operation. The
caller selects a positive slot count; KDOS allocates one exact 184-byte record
per slot, plus only the backing allocator's required alignment. The pool has
no compile-time maximum, is independent of TLS connection count, and is not
silently reduced to the client path builder's eight-certificate capacity.
The pool requires XMEM and is protected below `XMEM-FLOOR`. This is an
explicit platform requirement, not a credential-count cap: the canonical
loadable networking module already relies on the XMEM userland dictionary and
dynamic tables, while the 1 MiB Bank-0 layout cannot host that complete source
after KDOS has established its heap.

A public credential identity is the two-cell pair `(slot+1, generation)`.
Neither cell is a memory pointer, zero is not a valid generation, deletion
preserves the old generation so the preceding handle becomes stale, and a
slot is retired rather than allowing generation wrap to revive an old
authority. Each signing operation has a separate generation used only to bind
cancellation to that exact operation. These generations are in-memory
capability metadata, not durable anti-rollback counters.

`TLS-CREDENTIAL-PROVISION` accepts a leaf-first concatenation of
self-delimiting DER Certificate values and one 32-byte little-endian P-256
private scalar. The nonempty chain is copied into an exact lower allocation
before validation; TLS `CertificateEntry` lengths and extension vectors are
not credential state. There is no private entry-count or per-certificate
8192-byte ceiling. Every entry must have an exact canonical outer Certificate
SEQUENCE containing, in order, a TBS SEQUENCE, AlgorithmIdentifier SEQUENCE,
and a nonempty byte-aligned signature BIT STRING. Only the leaf is deeply
parsed by the implemented X.509 profile; intermediates remain opaque after
that shallow structural proof.

The protocol bound is the synthesized wire sum `sum(DER length + 5) <=
0xFFFFFB`: three bytes for each certificate length and two bytes for the
initially empty per-entry extension vector, within the TLS uint24 Certificate
body after its empty request context and list header. Server transcript
construction already synthesizes that exact framing while streaming the chain
through SHA-256. The protected emitter reuses those framing words while
streaming the same chain without forcing a large admitted chain through the
fixed transcript arena. Ordinary multi-record coverage exists; the
uint24-maximum emitted Certificate remains release evidence, but no maximum-
chain generator or executable capstone is currently checked in. That evidence
still requires a reproducible generator, an independently derived framing,
transcript, and application-secret oracle, and an explicit checked execution
limit.

The leaf must have an uncompressed P-256 public key, must not be a CA, and,
when present, KeyUsage must allow digital signatures and EKU must allow server
authentication or any usage. Provisioning computes `dG` with the private
fixed-schedule path and compares the complete 65-byte public point before it
publishes the slot. This proves certificate/key correspondence; it does not
authenticate the chain, check its signatures or validity interval, or apply a
client trust/hostname policy.

`TLS-CREDENTIAL-PUBLIC` copies the 65-byte public point and reports signature
scheme `ecdsa_secp256r1_sha256` plus the certificate count.
`TLS-CREDENTIAL-CHAIN` supports a zero-output length query or copies the exact
owned concatenated-DER chain. `TLS-CREDENTIAL-SIGN` accepts only an opaque
handle and a 32-byte SHA-256 digest, signs into lower staging, and publishes
only the actual DER span, bounded by the mathematical 72-byte P-256 maximum,
after capacity, alias, and late-cancellation checks. In particular, the
qualified 71-byte vector requires only 71 output bytes. None of these
operations returns a private-key address.

`TLS-CREDENTIAL-SIGN-CANCEL` takes the short registry lock without taking the
TLS workspace owner, so its metadata path is suitable for a different
physical core to mark the currently active operation generation. Lock 11 has
an explicit nonrecursive `(COREID,TASK-ID)` software owner because its
hardware primitive is depthless and same-core reentrant. Same-core
cancellation while lock 10 is active returns busy; a different physical core
may publish the request. The synchronous signer checks only after a complete
four-trial batch, and lock 11 arbitrates a later request before publication.
The four-core emulator capstone executes this path with a real full-batch
credential signature: a peer physical core cancels the exact operation,
caller output remains unchanged, and both credential and TLS owner state are
clean afterward. This is architectural emulator evidence, not a physical
board or interrupt-handler claim.

`TLS-CREDENTIAL-DELETE` is synchronous core-0 revocation. It refuses a live
reference, clears the complete record except for stale-generation metadata,
wipes the complete allocated DER-chain payload including allocator padding,
and frees that payload. Provisioning and signing failures publish neither a
partial handle nor caller output, release their owners, and wipe private and
allocation staging. Runtime `XMEM-RESET` is deferred through a credential-aware
wrapper: it refuses reset while credentials are active, preserves the
floor-protected pool after all credentials are deleted, and retains the
underlying no-XMEM reset as a no-op.

Credential statuses occupy `-4320` through `-4334`: state, range, allocation,
capacity, stale handle, malformed DER chain, unsupported leaf profile,
invalid key, key mismatch, busy, alias, lower crypto failure, cancelled, no
active signing operation, and retired generation respectively.

### Fixed RSA-2048

The RSA verifier accepts only a 2048-bit odd modulus and public exponent
65537. Its 32-limb CIOS Montgomery multiplication uses the BIOS `UM*` word,
which executes the existing `UMUL`/`UMULH` instructions; it does not replace
the native multiplication path with a software fallback. Certificate
signatures must have exact PKCS#1 v1.5 SHA-256 encoding. TLS
CertificateVerify must use `rsa_pss_rsae_sha256` with `emBits=2047`, SHA-256,
MGF1-SHA256, and an exact 32-byte salt.

RSA scratch is restricted to core 0 and serialized by an owner/phase gate.
Cooperative code uses `RSA2048-PUBLIC-BEGIN`, pumps one bounded unit per
`RSA2048-PUBLIC-STEP`, and then calls `RSA2048-PUBLIC-FINAL` or
`RSA2048-PUBLIC-CANCEL`. The operation is bound to the initiating
`(COREID,TASK-ID)`; another execution context cannot step, finalize, cancel,
or wipe it. Physical worker cores are rejected. `RSA-E-BUSY` (`-2`) reports
contention or off-core entry. The synchronous public and padding-verification
words remain compatibility primitives and are not suitable for a responsive
live owner loop.

### Trust Bundles

The default trust store is empty. `TLS-TRUST-LOAD` copies and validates an
in-memory bundle, allowing at most eight anchors and 32768 total bytes. An
anchor must use a supported P-256 or RSA-2048 public key, be a CA, and, when
KeyUsage is present, permit certificate signing. An RSA-2048 intermediate may
be provisioned directly as the scoped anchor even when its own borrowed
signature is 512 bytes from an unsupported RSA-4096 parent; anchor loading
does not pretend to validate that parent. An RSA-4096 certificate itself is
rejected as an unsupported anchor.

Bundle format, all integer fields big-endian:

```text
magic            4 bytes   "MPTA"
format_version   u16       1
anchor_count     u16       0..8
generation       u64       provisioning metadata

repeated anchor_count times:
flags            u16       bit 0: include subdomains
scope_length     u16       0..253
cert_length      u32       128..8192
scope            bytes     ASCII DNS name; empty means global
certificate      bytes     DER X.509 CA certificate
```

An exact scope authorizes only that hostname. `TTAF-SUBDOMAINS` additionally
authorizes names below it. Scope is checked whether the trusted certificate is
presented in the chain or omitted.

The format itself is not signed and `generation` is not an anti-rollback
counter. The caller must obtain the bundle through a trusted provisioning path.
Signed updates and durable rollback protection remain release requirements.

### Path Building

`X509-VERIFY-CHAIN` accepts one through eight parsed certificates. Descriptor
zero is always the server leaf; remaining certificates may be unordered and
may contain irrelevant entries. TLS Certificate processing requires the leaf
to parse, but ignores unusable non-leaf extras after validating their enclosing
TLS lengths. They cannot enter path construction; a required unsupported
intermediate therefore still leaves no path and fails closed. The bounded
builder:

- requires the leaf not to be a CA;
- checks digitalSignature and serverAuth when those extensions are present;
- checks SAN hostname and RTC validity;
- links issuer/subject and AKI/SKI when both key identifiers are available;
- checks CA BasicConstraints, keyCertSign, EKU restrictions, and pathLen;
- verifies each child with either ECDSA-P256-SHA256 or exact
  RSA-2048/PKCS#1-SHA256 according to its signature and issuer-key algorithms;
- terminates only at a scoped provisioned anchor.

Failures return stable `TLS-CERT-*` statuses from `-4101` through `-4109`.
`TLS-CERT-LAST-ERROR` retains the Certificate-message result. A clock earlier
than 2020 is considered untrustworthy and fails closed.

### TLS Certificate Messages

`TLS-PARSE-CERTIFICATE` validates the handshake header and exact body length,
requires the main-handshake empty request context, checks the exact
certificate-list length, bounds every certificate and per-entry extension
vector, and rejects more than eight entries. It requires SNI and a loaded
trust store. The leaf public key is copied to CertificateVerify scratch only
after the full path succeeds; every failure clears that scratch first.

Those are client receive/path-building bounds. In particular, this surface
admits only 128-through-8192-byte entries and retains at most eight
descriptors. They do not constrain the separately owned server credential
list, whose count follows its caller-provided TLS vector and whose total size
is bounded by the Certificate body's uint24 wire length.

The handshake dispatcher enforces the prototype's certificate-authenticated
sequence:

```text
ClientHello -> ServerHello -> EncryptedExtensions -> Certificate
            -> CertificateVerify -> Finished
```

Unknown, duplicate, truncated, and out-of-order handshake messages fail. This
profile does not currently support PSK-only handshakes, post-handshake client
authentication, or a server CertificateRequest.

Handshake messages are reassembled across protected records in a bounded
73732-byte buffer. The plaintext `ServerHello` may also span records, but it
must be the only plaintext handshake message before encrypted traffic begins.
Transcript and reassembly overflow are sticky fatal failures.

Server construction is deliberately committed before wire progress.
`TLS-SERVER-PREPARE-HELLO` publishes exact ServerHello and
EncryptedExtensions ledger bytes plus the role-correct handshake epochs.
`TLS-SERVER-PREPARE-FLIGHT` then streams the owned ClientHello, ledger, and
pinned DER chain through SHA-256; generates exact CertificateVerify and
Finished messages; and derives the final master, C-AP, S-AP, and exporter
secrets. Busy and cancelled signing leave the first phase retryable. An
admitted signer/crypto failure is terminal and wipes derived secrets without
orphaning the credential pin needed by abort cleanup.

`TLS-SERVER-FLIGHT-STEP-WITH` advances that immutable flight through a
socket-independent all-or-none callback. After flight preparation, the dead
8192-byte duplicate-extension bitmap becomes a one-way phase union containing
one exact TCP-MSS-sized pending TLS record, 152 bytes of emitter metadata, and
64 bytes of ingress metadata; workspace geometry does not grow. Client-flight
partial records later reuse the existing 16,896-byte record lane, while at most
36 fragmented Finished bytes use the per-context retained lane. Plaintext
ServerHello is emitted first without
consuming a protected-record sequence. EncryptedExtensions, the streamed
Certificate framing and DER, CertificateVerify, and Finished are then packed
into protected records whose complete wire images fit one TCP MSS.

The callback receives a borrowed read-only record only while it is executing,
and lock 10 is not held across that call. Zero means backpressure and retains
the exact record, nonce, sequence, and logical cursors. The exact record length
means the callback synchronously accepted or independently retained every
byte, after which sequence and cursors commit. A retained retry must use the
same callback execution token; changing adapters is refused without offering
or mutating the record. A short nonzero result, callback exception, or callback
return while owning lock 10 is terminal. Same-context
public record, key-schedule, ALPN-publication, alert, and post-handshake
mutators refuse throughout the pending-flight lifetime. Abort remains possible
between callback invocations and wipes the union and releases the credential.

`TLS-SERVER-FLIGHT-STEP` is the attached counterpart. Flight preparation seals
either the exact reciprocal `(TCB, generation)` or the socket-independent zero
pair. The attached entry accepts only the former and invokes a fixed
owner-qualified exact-send adapter; the generic callback entry accepts only
the latter. Local seal/binding agreement is checked under TLS ownership, while
reciprocal TCB authority and liveness are checked inside the fixed NET
transaction. A terminal result exact-aborts a dead current child before local
erasure; stale authority instead erases only the old TLS binding and leaves a
possible reused TCB untouched. If cleanup itself throws, pending wire bytes are
wiped but the seal, binding, and credential pin remain available to
`TLS-ABORT`.

`TLS-SERVER-CLIENT-HELLO-STEP` is the attached inbound counterpart before
flight preparation. The raw memory parser is refused as soon as TCB authority
exists. The step retains a partial TLSPlaintext record in the context record
lane and appends complete nonempty handshake fragments to the full-width
ClientHello lane. It derives the exact message bound from the uint24 handshake
header, rejects coalesced/trailing bytes, and never reads beyond the current
record. `TLS-SERVER-CLIENT-HELLO-RECORD` tells an edge-triggered coordinator to
step again immediately after a complete nonfinal record;
`TLS-SERVER-CLIENT-HELLO-COMPLETE` means the parser committed the exact
message. No or partial record progress returns `NONE` with
`TLS-E-WOULD-BLOCK`. Every reachable retained record is a strict prefix, so
the owner-qualified receive necessarily revalidates reciprocal authority before
append or parser commit. Unexpected lower throws release the TLS owner and
preserve the exact binding and credential pin for `TLS-ABORT`.

After exact Finished admission, the emitter installs only the prederived S-AP
write epoch, preserves the C-HS read epoch and its sequence, and publishes
client-Finished-pending. The caller-selected callback entry intentionally has
no raw-TCB authority; attached emission is available only through the fixed
sealed adapter.

`TLS-SERVER-CLIENT-FLIGHT-BEGIN` then seals a nonnegative caller-provided
wire-byte budget. Failed trial C-HS decryption can be discarded only when the
owned ClientHello offered `early_data`, the budget admits the complete record,
and no protected client-handshake record has authenticated. Discard does not
advance the read sequence. Exact compatibility CCS is ignored without spending
the budget or closing the window. The first authenticated record closes the
window permanently; a later tag failure is `bad_record_mac`, even if budget
remains.

`TLS-SERVER-CLIENT-FLIGHT-FEED` copies at most one record per call into
connection-owned storage and retains partial headers, bodies, and the exact
36-byte client Finished across calls and protected-record boundaries. It
accepts only handshake or alert content before authentication. Finished is
verified under the retained C-HS secret against the transcript through server
Finished; only then is the transcript replayed through client Finished and the
prederived C-AP read epoch installed. The completed context is authenticated
and `TLSH-APPLICATION-READY`, but remains `TLSS-HANDSHAKE` until
`TLS-HANDSHAKE-PUBLISH` releases the credential pin, wipes handshake-only
state, and publishes the secure application stream.

Feed progress distinguishes an ordinary consumed record, verified Finished,
an outbound fatal-alert disposition, an outbound close-notify disposition, and
a terminal peer alert. Terminal disposition is sticky across repeated calls.
Terminal cleanup wipes read, transcript, schedule, exporter, and fragment
state, normally releases credential ownership, and retains only S-AP write
state needed by the later authoritative adapter to encode a protected response.
If credential unpin is contended or fails, its generational handle remains for
`TLS-ABORT` retry and the disposition becomes `internal_error` with
`TLS-E-HANDSHAKE-CRYPTO`. The feed does not claim to send that response.
Protocol mutation remains excluded from emitter
completion through publish, close, or abort, and no raw TCB value grants input
or output authority.

`TLS-ALPN-CONFIGURE` copies zero or one exact ProtocolName into connection-owned
storage before the handshake.  A nonempty name is bounded by ALPN's one-byte
wire length, so its admitted size is 1 through 255 bytes.  ClientHello emits
the caller's bytes without interpreting them, and EncryptedExtensions must
select that one name exactly.  Missing, empty, duplicate, truncated, trailing,
or mismatched selections fail before a result is published.

`TLS-CONNECT-NAMED` and `TLS-CONNECT-HYBRID-NAMED` are the protocol-neutral
blocking entry points. `TLS-ALPN-CONFIGURED` returns the owned configured
bytes. `TLS-ALPN-SELECTED` publishes the result only after the connection is
established and the peer is authenticated; earlier calls return `0 0`. Plain
`TLS-CONNECT` requests no ALPN.  `TLS-CONNECT-ALPN` and
`TLS-CONNECT-HYBRID-ALPN` temporarily preserve the existing `http/1.1` caller
while Akashic migrates; they compose the same generic bytes and are not a
registry for additional application protocols.

### TLS exporters and serialized ownership

After the transcript includes the authenticated server Finished,
`TLS-KS-APPLICATION` derives `exporter_master_secret` with the TLS 1.3
`"exp master"` label. It stages that value alongside the application record
keys. A client installs both role-correct directions. A server installs its
application write direction only and retains its client-handshake read epoch
until client Finished authentication cuts that direction over. Exporter access
remains unavailable until `TLS-HANDSHAKE-PUBLISH` publishes the authenticated
connection.
The public `TLS-EXPORT` construction is:

```text
intermediate = Derive-Secret(exporter_master_secret, caller_label, "")
context_hash = Hash(raw_caller_context)
output       = HKDF-Expand-Label(intermediate, "exporter",
                                 context_hash, requested_length)
```

The caller label is 1 through 249 printable ASCII bytes, the raw context is
hashed without an application-specific interpretation, and the output bound
is the TLS 1.3 HKDF limit of 8160 bytes for the implemented 32-byte hashes.
All caller spans are checked before derivation. Input or output overlap with
TLS contexts, their RX workspaces, or exporter/HKDF scratch is rejected, as is
output overlap with the caller label or context. Output is staged and copied
once only after both expansions succeed; the exporter master secret is never
returned. Fatal
records, alerts, close, abort, context reuse, and handshake failure make it
unavailable and wipe the retained value.

The exporter known-answer test derives the RFC 8448 exporter master secret
from that RFC's master secret and transcript hash. It then applies RFC 9266's
`EXPORTER-Channel-Binding`, empty-context, 32-byte tuple. RFC 9266 specifies
the tuple rather than a final byte vector, so the checked expected output was
reproduced independently from the RFC 8448 inputs.

Hardware lock 10 is the machine-wide TLS workspace owner. Recursion is bound
to the exact `(COREID,TASK-ID)` with a software depth, which closes the
hardware lock's depthless same-core reacquire case. Public connection, record,
application-data, alert, close/abort, crypto-dispatch,
handshake-publication, exporter, and ordinary credential entry points acquire
it nonblockingly. Credential registry publication uses hardware lock 11 only
in a short section beneath lock 10 and releases it before lower crypto. The
resulting order is 10, optionally 11 and release, then KDOS HMAC/HKDF lock 9
and checked BIOS crypto lock 8. Credential cancellation takes only lock 11 and
does not call crypto while holding it.
Credential `TC.REFS` is a checked natural-cell reference count rather than a
binary signing bit. An internal server flight may pin immutable chain metadata
under lock order 10 then 11; a nested lower signing call adds and removes a
separate transient reference. Deletion remains blocked until both flight and
signer references are gone, and reference saturation refuses pin or sign
without mutation. Borrowed chain pointers never become public credential API
output.
Contention does not mutate shared TLS scratch; status-bearing operations return
their documented busy status and void/backpressure operations remain inert.
Internal handshake parsers and builders are covered by the owned blocking
connection path and are not independent concurrent entry points.

State that must survive an application receive call is not shared scratch.
Each 1,000-byte `/TLS-CTX` stores the exact attached TCB generation at +968,
its own nonzero incarnation at +976, its reciprocal socket owner at +984, and
the slot/close lifecycle at +992. `TLS-CLOSE-FREE` marks a released slot while
preserving its last generation; one successful claim increments that value and
creates exactly one live incarnation. The context indexes a 230,688-byte
`/TLS-RX-WORKSPACE`: a
16,896-byte partial-record lane plus an aligned retained-data lane bounded for
a 73,732-byte post-handshake message, a protocol-derived 131,146-byte
ClientHello lane, an 8,192-byte one-way phase union that begins as the complete
uint16 extension bitmap and later holds a TCP-MSS pending record plus 136-byte
emitter and 64-byte ingress metadata, and a 512-byte immutable server-message
ledger with 200 bytes of exact flight metadata. Client-flight fragments reuse
the per-context record and retained lanes. Incomplete encrypted records,
authenticated plaintext remainder, and fragmented post-handshake messages are
therefore isolated by context.  The high-level application receive and
owner-held blocking-handshake paths use the transient global plaintext buffer
only while lock 10 is held and scrub its complete contents before releasing
ownership.  Raw `TLS-DECRYPT-RECORD` instead writes to its caller-selected
output and does not scrub that output. With a 5,952-byte TCB and two 40-byte
socket descriptors, the logical network-table cost is 237,720 bytes per
connection. The four XMEM table allocations are normalized independently, so
one, two, and three connections reserve 237,728, 475,440, and 713,168 bytes;
capacity uses the exact aggregate.

The context generation protects the socket-published path. A TLS descriptor
stores `(context, context-generation)` while the context stores that descriptor
as `SOCKET-OWNER`; the context separately stores `(TCB, TCB-generation)` while
the TCB stores the context as owner. Publication and teardown update each
reciprocal pair under TLS-then-network lock order, and socket operations resolve
both pairs before entering TLS or TCP. Raw context operations are deliberately
disjoint and reject socket-owned contexts. A bare raw context pointer is still
a lifetime-scoped interface rather than an opaque generational handle; the
public descriptor pointer is likewise a caller-held lifetime token.

Ordinary `TLS-CONNECT`, `TLS-CONNECT-NAMED`, and the HTTP compatibility wrapper
use the interoperable public profile: TLS 1.3 `TLS_AES_128_GCM_SHA256` and
X25519. Its
`signature_algorithms` extension is exactly
`ecdsa_secp256r1_sha256, rsa_pss_rsae_sha256`; the separate
`signature_algorithms_cert` extension is exactly
`ecdsa_secp256r1_sha256, rsa_pkcs1_sha256`. The standard extension block is 77
bytes.

`TLS-CONNECT-HYBRID` and `TLS-CONNECT-HYBRID-NAMED` explicitly select MegaPad's
private X25519 plus ML-KEM-512 profile. The temporary hybrid HTTP wrapper uses
the same path. That profile uses IANA private-use
NamedGroup `0xFE00` and private cipher suite `0xFF01`, has a 915-byte extension
block, and is not advertised to public servers. The private key-share shape
must not be placed under a registered group code point belonging to another
construction.

Connection waits are target-state aware. A completed SYN handshake returns
immediately instead of running a fixed number of additional idle polls, and a
record fill returns as soon as the requested bytes are buffered. The generic
poll pump remains available for callers that intentionally want fixed polling.
`TLS-CONNECT-LAST-ERROR` preserves a bounded phase code across failed-context
cleanup so transports can distinguish configuration, TCP, ServerHello,
protected-handshake, Finished, and authentication failures.

The current TCP control block owns one retransmission buffer, so only one
unacknowledged data segment may be in flight. `TCP-SEND-READY?` exposes that
constraint and `TCP-SEND` returns zero without modifying the buffer while
`SND-NXT` differs from `SND-UNA`. TLS checks readiness before encrypting, so a
retry neither overwrites pending ciphertext nor advances the record sequence.
This is deliberate backpressure for the present bounded stack, not a claim of
multi-segment TCP throughput.

`TLS-SEND-ALERT-TRY` reports `TLS-E-WOULD-BLOCK` when exact transport
admission accepts nothing and `TLS-E-TRANSPORT` when the owned TCB has reached
terminal failure. `TLS-IO-STATUS` makes that terminal observation sticky,
revokes the traffic epoch, and explicitly reclaims the failed TCB. A later
terminal TCP disposition is stream-ordered behind plaintext that has already
authenticated into the context: status, readiness, and send probes defer the
destructive observation while `APP-LEN` is nonzero, receive drains the retained
bytes first, and the first observation after the remainder reaches zero
publishes `TLS-E-TRANSPORT` and performs exact reclamation.

The retained-data boundary is now reliable within this deliberately bounded
profile. Admission is at most `min(SND-WND, CWND, MSS)`, and
`TCP-SEND-EXACT` gives protected records an all-or-none result. Zero admission
does not advance sequence or overwrite retained state. ACKs advance only on a
strict wrap-safe in-flight increase; stale/future ACKs cannot release data,
partial ACKs trim the retained prefix, and fast/RTO replay emits the suffix
from `SND-UNA`. Empty `TCP-POLL` calls advance bounded exponential RTO,
neighbor discovery, and durable ACK intent. Only a replay admitted to the NIC
consumes TCP retry state; exact terminal failures remain owner-visible until
the TLS/socket owner observes and reclaims them. Lock 12 serializes shared
Ethernet/IP/TCP construction and the asynchronous NIC descriptor lifetime.

The active, passive, and graceful-close control boundary is now bounded.
Active open retains and replays its SYN at the original ISS. `SYN-SENT` accepts
only an exact payload-free SYN+ACK acknowledging `ISS+1`; it ignores a bare SYN
rather than treating it as simultaneous open. Establishment durably schedules
the final ACK, and an exact duplicate SYN+ACK caused by a lost final ACK is
re-ACKed without perturbing established state. A listener
reserves capacity across half-open and queued children before allocation,
admits only a bare SYN, records the exact listener generation, and publishes a
child only after the expected sequence and ACK cover its SYN. SYN+ACK replay
has bounded exponential retry and releases the reservation on expiry. Queue
entries carry child generations, and accept transfers ownership only after the
listener, parent token, child state, and both generations validate.

Control retry counts advance only when a SYN, SYN+ACK, or FIN replay reaches
the NIC. An independent bounded local-admission stall timestamp covers
unresolved neighbors and persistent NIC backpressure, preventing a control
state from living forever merely because no retry reached the wire.

Graceful TLS close admits one exact protected `close_notify` into retained TCP
state. Owner close refuses FIN while that record remains unacknowledged, so a
retry can emit FIN only after the close-notify ACK. FIN-WAIT-1, CLOSING, and
LAST-ACK replay the retained FIN with bounded exponential retry; FIN-WAIT-2
has a separate 60-second terminal timeout. TIME-WAIT re-ACKs an exact duplicate
FIN and restarts its 2MSL quarantine. Exact generation/owner checks guard TLS
and socket close/abort throughout. Secure server socket completion does not
require a general sliding window.

`TLS-CLOSE-TRY` and `TLS-CLOSE` are checked `(ctx -- ior)` operations and
retain authority on retryable failure. `TLS-CLOSE-FINAL` uses a bounded
graceful-progress budget followed by exact abort fallback; zero means the TLS
context has been disposed, while nonzero retains its context token for retry.
Abort fallback may already have reclaimed the transport before a contended
credential unpin returns busy; the exact pin metadata remains with the claimed
context.
`TLS-ABORT` performs immediate raw-context teardown without `close_notify` and
reports whether reclamation was local, emitted a cached-route RST, found no
live transport, or was busy. Socket-owned contexts are instead torn down by
`SOCK-ABORT (sd -- status ior)`, which validates both reciprocal generations
before releasing the descriptor. Socket `CLOSE-TRY` and `CLOSE` are both
checked `(sd -- ior)` operations: zero means the descriptor was released,
while nonzero preserves retry authority.

Application receive preserves decrypted record data across caller-sized reads.
It accumulates an incomplete encrypted record in the context's record lane.
If authenticated plaintext exceeds the destination slice, the remainder and
its offset are retained in that context's retained-data lane and drained before
decrypting another record.  On this high-level receive path the module-global
plaintext buffer is only transient staging under the TLS owner and its complete
contents are scrubbed before release.  A small HTTP receive buffer therefore
cannot silently truncate large response bodies
or expose its remainder to another connection.

`MS@` and `EPOCH@` reconstruct all eight RTC bytes. Certificate and token
deadlines therefore use the full-width clock rather than a truncated timer.

Generic and protected incoming records require legacy record version `0x0303`;
the initial attached ClientHello boundary separately accepts the interoperable
`0x0301` or `0x0303` TLSPlaintext versions. Plaintext and protected records are
bounded separately. A compatibility
ChangeCipherSpec is ignored only when it has the exact one-byte `0x01` form
permitted during the handshake. Incoming alerts clear peer authorization and
distinguish clean `close_notify` from fatal or malformed records. A clean peer
close retains only the write epoch needed for the answering `close_notify`;
successful emission then wipes that epoch. Fatal, malformed, and truncated
receive paths erase record secrets immediately. Application
send and receive both require an established, authenticated context.
Application-key derivation likewise returns `TLS-E-STATE` unless certificate
authentication, the server-Finished state, and the exact configured ALPN bytes
are all satisfied. Zero means the application schedule and exporter secret were
staged, not that the connection was published; `TLS-HANDSHAKE-PUBLISH` is the
separate establishment boundary after complete local-Finished acceptance.

Session resumption is not implemented. Authenticated post-handshake
`NewSessionTicket` messages are reassembled in the bounded retained-data lane
owned by their connection, validated through every nested nonce, ticket, and
extension length, and then discarded; lifetime is capped at seven days, every
extension type must be unique, and `early_data` requires its exact four-byte
payload. KeyUpdate,
CertificateRequest, other post-handshake
messages, malformed tickets, and non-handshake records interleaved with a
ticket fragment fail closed with `TLS-E-POST-HANDSHAKE`.

## Deployment Reality

The code does not validate arbitrary public certificate chains. It lacks P-384,
Ed25519, ECDSA-SHA384 child-signature verification, variable-width RSA, and a
curated root program. Public chains commonly cross those boundaries.

When an endpoint's representable leaf or intermediate is signed by an
unsupported parent, that supported certificate may instead be provisioned as
a narrowly hostname-scoped anchor. For example, pinning an RSA-2048
intermediate is valid even if its own signature came from an RSA-4096 root.
This is an explicit, updateable deployment profile, not equivalent to
validating or trusting the unsupported parent. Intermediate rotation requires
a trust-bundle update.

No remote API token or equivalent application secret should be provisioned
until the intended endpoint's current chain is representable by the installed
bundle and a credential-free live handshake succeeds on the machine. That
client-side application secret is unrelated to a local TLS server credential.

## Verified Tests

Native guest tests cover:

- canonical DER signature integers and a real certificate signature;
- valid, corrupt, and out-of-range ECDSA inputs;
- RFC 6979 P-256/SHA-256 `sample` and `test` candidates, signatures, and exact
  72-byte/71-byte DER encodings, plus fifth-candidate state continuation;
- fixed four-complete-trial source structure with unbounded batch
  continuation, every first-valid selection position, `bits2octets` and order
  boundaries, equal fresh-snapshot private-core cycles, and minimal DER trim
  and sign-padding edges;
- signer invalid-key, exact-capacity, alias, busy, recursive-owner, and caught
  exception atomicity, including complete private/HMAC workspace and touched
  Field-state cleanup;
- differential RSA Montgomery/public-operation vectors, widened multiply
  boundaries, and representative-range rejection;
- exact PKCS#1 v1.5 and PSS padding failure axes, plus fixed real RSA
  certificate and CertificateVerify signatures;
- RSA-only and mixed RSA/ECDSA paths, unsupported RSA-4096 anchors, and a
  directly pinned RSA-2048 intermediate carrying a 512-byte parent signature;
- owner-bound incremental RSA stepping, contention, cancellation, and
  synchronous-phase cancellation rejection;
- deterministic root/intermediate/leaf fixtures with CA, KU, EKU, SAN,
  SKI/AKI, pathLen, and validity constraints;
- hostname, wildcard, clock, signature, scope, empty-store, and truncation
  failures;
- reordered and extraneous presented certificates;
- exact TLS Certificate framing and bounded entry extensions;
- full-width server ClientHello admission, complete-chain signature-policy
  classification, exact ServerHello/EncryptedExtensions construction, direct
  X25519, and `ClientHello || ServerHello` transcript vectors;
- attached initial ClientHello ingress over real Ethernet/IP/TCP segmentation,
  including `0x0301`/`0x0303` TLSPlaintext fragmentation, explicit record
  progress, exact following-record retention, fatal-alert latching, EOF
  reclamation, raw-parser exclusion, and stale-TCB reuse isolation;
- streamed one- and multi-certificate server transcript framing with no copied
  Certificate message or certificate-count cap;
- exact server CertificateVerify, Finished, final transcript, master,
  application, and exporter derivation, including busy/cancel retry and
  terminal signer/THROW rollback;
- exact socket-independent ServerHello and protected-flight record lengths and
  SHA-256 oracles, byte-identical zero-result retry, commit-only sequence and
  cursor advance, and final S-AP-write/C-HS-read cutover;
- a socket-independent composition through publication using independent
  SHA-256/HMAC/HKDF calculations, a fixed externally generated AES-GCM client
  Finished, and independently checked traffic-key, transcript, ALPN, and
  exporter values;
- one-record bounded ingress with fragmented record and Finished reassembly,
  exact CCS handling, no-offer tag failure, wrong Finished, premature
  application data, record overflow, protected peer alert, and sticky terminal
  disposition;
- rejected-0-RTT accounting in complete wire bytes, no sequence advance on
  discard, exact budget exhaustion, and permanent closure after the first
  authenticated handshake fragment;
- owner-recursion and crypto-fault containment, conservative mutable-arena
  alias refusal, pointer/scratch cleanup, and credential-unpin retry through
  abort;
- MSS fragmentation of a multi-certificate chain with decrypted plaintext
  reconstruction, including retry of the first full Certificate record;
- short-send, callback-THROW, staging-THROW, callback-owner-leak, durable-result
  contention, stale-finalizer, pending-lifetime mutation, cancellation, secret
  wipe, credential-reference, and driver-owner adversarial cases;
- stale-key clearing on every failed Certificate message;
- a real CertificateVerify signature from the fixture leaf key;
- rejection of early Finished and unauthenticated application-key derivation;
- generic non-HTTP ALPN encoding, exact selection, bounds, duplicate and
  malformed refusal, output atomicity, and per-context result state;
- RFC 8448 exporter-master derivation plus the RFC 9266 channel-binding tuple,
  context binding, state/bounds/alias refusal, atomic output, and scratch wipe;
- application-secret staging before explicit establishment publication, with
  superseded handshake/application schedule material wiped afterward;
- recursive TLS ownership, different-task contention, outermost release, and
  record/application regressions under the owner;
- local fatal-alert revocation under transport backpressure, including
  exporter wipe and atomic post-fatal refusal;
- standard-only and explicit private-hybrid ClientHello wire layouts;
- immediate established/readable TCP waits and record-fill completion;
- TCP/TLS send backpressure without retransmission-buffer or sequence loss;
- strict stale/future/duplicate/partial-wrap ACK handling, retained-suffix
  fast/RTO replay from `SND-UNA`, peer/CWND and exact-send admission, terminal
  owner observation, and cooperative neighbor/durable-ACK recovery;
- exact active-open SYN replay/SYN+ACK admission and lost-final-ACK recovery,
  bounded active/passive/FIN local-admission stalls, retained FIN replay,
  FIN-WAIT-2 expiry, and TIME-WAIT duplicate-FIN re-ACK/quarantine restart;
- multi-read delivery of application records larger than the caller buffer;
- alternating connections with isolated partial records, retained plaintext,
  and fragmented post-handshake messages, including exact per-context wipe;
- fail-closed refusal of TLS-marked `LISTEN` and `SOCK-ACCEPT` before a secure
  accept path exists;
- full-width `MS@` and `EPOCH@` reconstruction across byte boundaries;
- handshake reassembly across arbitrary protected-record boundaries;
- legacy record version, size-class, and compatibility CCS validation;
- clean, fatal, and malformed incoming alert handling;
- the surrounding record, handshake, and application-data regressions.

Final affected sequential source-mode qualification passed 39/39 TLS
application-data, 25/25 socket/readiness, 28/28 tools, and 43/43 complete
server-handshake tests. The preceding unchanged lower baseline passed 279/279
network-stack and 65/65 adjacent hardening/source-selection tests. The
corrected four-core credential and server-flight cancellation capstones passed
together, 2/2 in 665.79 seconds, after proving complete KDOS and networking
source loads and execution of a terminal body marker. The 450,000,000-step
allowance applies only to networking snapshot construction; each capstone
retains its independent 400,000,000-step execution ceiling. A dedicated
post-server-phase regression also proves that later `VARIABLE`, `CREATE`, and
`ALLOT` definitions remain reachable, so the capstone's comparison storage can
be prepared before the concurrency transition without hiding dictionary
damage.

These tests prove deterministic construction plus bounded socket-independent
server-flight emission, rejected-0RTT handling, client-Finished
authentication/C-AP cutover, explicit publication, ALPN, exporter agreement,
and failure atomicity. Focused secure-accept evidence additionally proves
empty-queue retry, pre-consumption rejection, stale-child reclamation, exact
reciprocal context/TCB publication, continued ClientHello parsing, and abort
cleanup without disturbing the listener. Focused attached-emitter evidence now
also proves exact ServerHello TCP bytes, retained zero-window retry, generic
callback exclusion, exact child reclamation on dead transport, reused-TCB
isolation, and exception-fallback authority retention. The subsequent 5/5
focused attached-initial-ingress tests prove real Ethernet/IP/TCP and
multi-record reassembly, explicit nonfinal-record progress, exact no-overread,
fatal input latching, terminal EOF cleanup, and stale-incarnation isolation.
They do not yet prove ACK-paced protected-flight completion, attached
protected ingress, secure socket acceptance, terminal-alert transmission, or
interoperability over sockets with an independent TLS stack.
The uint24-maximum Certificate capstone is separate maturity evidence and must
not delay this vertical closure.

Signer and credential fixtures use only standardized or synthetic test
scalars, including the RFC 6979 Appendix A P-256 key and a synthetic `d=3`
credential. None enters a product trust bundle or production credential slot.

## Active Transport Closure and Deferred Maturity

### Secure server transport

- Drive the ACK-paced protected server-flight remainder, then adapt
  authoritative nonblocking TCP input to the qualified
  rejected-0RTT/client-Finished boundary, including protected terminal-alert
  output, without exposing a plaintext accepted child.
- Publish a TLS accepted socket only after client Finished authentication and
  explicit handshake publication. Prove credential-pin/reference and exact
  TCB-owner cleanup on every failure.
- Qualify socket lifecycle, application bytes, exporter equality, and
  close-notify/FIN completion against an independent TLS 1.3 implementation.

### Post-closure trust lifecycle

- Define a signed native trust-bundle update format and immutable bootstrap
  verification key or reviewed physical provisioning ceremony.
- Persist accepted trust-bundle generation state if rollback resistance is
  required. In-memory credential-handle generations are stale-authority
  protection, not durable rollback state.
- Establish an explicit root/intermediate policy and expiry/rotation process.
- Decide whether revocation is supported through stapled OCSP, short-lived
  scoped anchors, or another bounded policy.

### Post-closure protocol maturity

- Either keep the reusable raw-context interface internal or replace its bare
  pointer lifetime with an opaque generational handle. Socket-owned TLS
  contexts already use reciprocal generations; raw pointers do not.
- Add bounded `KeyUpdate` support before long-lived streaming connections are
  considered production-ready; it is currently rejected fail-closed.
- Run credential-free live interoperability against every intended endpoint
  after provisioning a reviewed scoped trust bundle.
- Keep the public and private ClientHello profiles separate; do not assign
  experimental wire formats to registered NamedGroup values.

### Deferred maturity evidence

- Implement and run a reproducible uint24-maximum Certificate capstone through
  the existing streamed emitter, with an independent oracle for exact framing,
  record boundaries, retry, transcript, and application-secret agreement.

### Algorithm coverage

- Add ECDSA-SHA384 and P-384 verification for common GTS chains.
- Add Ed25519 only with native vectors and bounded key sizes. If RSA-PSS is
  later accepted as an X.509 certificate-signature AlgorithmIdentifier, give
  its parameters the same exact, bounded treatment as CertificateVerify.
- Advertise only signature algorithms whose complete certificate and
  CertificateVerify paths are implemented. The current ClientHello advertises
  `0x0403,0x0804` in `signature_algorithms` and `0x0403,0x0401` in
  `signature_algorithms_cert`.

### Concurrency

ALPN result, endpoint role, negotiated suite/hash, traffic keys, exporter
master, authorization state, connection errors, application partial records,
retained plaintext, and post-handshake fragments are per-context. Handshake
transcript, certificate descriptors, cryptographic and transient plaintext
scratch, the blocking client-handshake record buffer, and hybrid key-exchange
buffers remain global. Public mutating and cryptographic TLS operations
therefore acquire machine-wide lock 10 under an exact `(COREID,TASK-ID)` owner
for the complete operation. A second connection receives nonblocking
contention and cannot execute concurrently while that shared scratch is in
use, but alternating application receive calls retain independent state.
Credential provisioning, deletion, and signing also use lock 10. They take
lock 11 only for a short registry transition and release it before calling
lower crypto, preserving the order 10, 11 (released), then KDOS HMAC/HKDF lock
9 and checked BIOS crypto lock 8. `TLS-CREDENTIAL-SIGN-CANCEL` is the narrow
exception: it takes only lock 11 so cancellation metadata can be published
while a signer owns lock 10; it performs no cryptography while holding 11.

RSA's core-0 phase gate remains an additional protection for RSA scratch. The
BIOS SHA-256 transaction uses a complete private context per core, validates
all caller spans, and wipes on every terminal path; it is isolated from
SHA-256 work on other cores. It is not per task, so a cooperative task must
not yield with an open `SHA256-INIT` transaction and permit another task on the
same core to reinitialize it. The TLS owner prevents that situation on the
networking paths covered above. True parallel TLS progress still requires
moving the remaining module-global state into connection-owned workspaces.

TLS lock 10 may nest the nonblocking network TX lock 12. Lock 12 never
acquires TLS, credential, or crypto locks; the crypto hierarchy remains
10 → optional 11 (released) → 9 → 8. Lock 12 protects shared packet staging
and NIC descriptor ownership, not independently parallel receive/TLS progress.

## Acceptance Before Remote API Secrets

1. All native TLS/X.509/ECDSA/RSA tests pass with no unresolved KDOS words.
2. The installed trust bundle is reviewed and scoped to the target endpoint.
3. The RTC is valid and survives the intended boot/power model.
4. A credential-free live handshake authenticates the expected chain.
5. HTTP response bytes can be streamed without overflowing TLS or transcript
   buffers.
6. Only then may a provider retrieve an in-memory API token or similar
   application secret and construct an Authorization header. This gate does
   not describe the local TLS server-credential pool.
