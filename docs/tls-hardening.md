# Native TLS Hardening

Status: authenticated bounded client profile, generic ALPN, exporters, serialized TLS crypto ownership, and per-context application RX implemented; server role gated on a qualified signer
Last updated: 2026-08-12

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

The current implementation is not a TLS server.  In particular, copying the
client handshake and reversing its traffic keys would omit the mandatory
server proof of possession in `CertificateVerify`.  The following lower-level
facts gate an authenticated server role:

- P-256 `EC-MUL` branches on scalar bits and is qualified only for public
  verification data.  It must not process a long-term signing key or an ECDSA
  nonce.
- The fixed RSA-2048 path implements the public exponent only.  There is no
  private RSA operation, PSS signing path, or blinding contract.
- KDOS has no protected private-key store or TLS credential-handle lifecycle.
  A future opaque handle can prevent accidental exposure to higher layers, but
  it cannot claim HSM-style isolation from arbitrary local supervisor code on
  the current machine.
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

The first interoperable server signature profile should be
`ecdsa_secp256r1_sha256`, which matches the existing certificate parser and
client signature offer.  Closing that gate requires a native secret-scalar
operation with an appropriate constant-work argument, deterministic RFC 6979
nonce derivation, fixed-work scalar arithmetic, canonical DER output, owned
credential storage, public-key matching, cancellation, and complete software
and hardware scratch cleanup.  Reusing `EC-MUL`, injecting a host callback, or
precomputing a fixture signature is test scaffolding rather than a server
security result.

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

Only step 7 sets `TLS-CTX.PEER-AUTH`. The application key schedule additionally
requires `TLSH-SERVER-FINISHED`; it stages application keys and the exporter
master secret in `TLSH-APPLICATION-READY` without publishing an established
connection. Only after `TCP-SEND` accepts the complete local Finished record
does `TLS-HANDSHAKE-PUBLISH` set `TLSS-ESTABLISHED`, expose exporter use, and
wipe superseded schedule secrets. Starting a new ClientHello clears the
retained leaf key, certificate status, authentication bit, and exporter state.

## Implemented Native Profile

### Bounded DER and X.509

`DER-READ` accepts definite, canonical DER lengths of at most four length
octets and never reads beyond the caller's limit. `X509-DESC-PARSE` accepts
certificates from 128 through 8192 bytes and records borrowed slices in a
208-byte `/X509-CERT` descriptor.

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

ECDSA verification operates only on public data. The scalar-multiplication
routine is not a general constant-time private-key primitive and must not be
reused for signing or secret scalar operations.

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
keys, but exporter access remains unavailable until the local Finished record
has been accepted in full and `TLS-HANDSHAKE-PUBLISH` publishes the connection.
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
hardware lock's depthless same-core reacquire case. The acquisition order is
TLS lock 10, KDOS HMAC/HKDF lock 9, then checked BIOS crypto lock 8. Public
connection, record, application-data, alert, close/abort, crypto-dispatch,
handshake-publication, and exporter entry points acquire it nonblockingly.
Contention does not mutate shared TLS scratch; status-bearing operations return
their documented busy status and void/backpressure operations remain inert.
Internal handshake parsers and builders are covered by the owned blocking
connection path and are not independent concurrent entry points.

State that must survive an application receive call is not shared scratch.
Each 968-byte `/TLS-CTX` indexes a 90,632-byte `/TLS-RX-WORKSPACE`: a
16,896-byte partial-record lane plus an aligned retained-data lane bounded for
a 73,732-byte post-handshake message.  Incomplete encrypted records,
authenticated plaintext remainder, and fragmented post-handshake messages are
therefore isolated by context.  The high-level application receive and
owner-held blocking-handshake paths use the transient global plaintext buffer
only while lock 10 is held and scrub its complete contents before releasing
ownership.  Raw `TLS-DECRYPT-RECORD` instead writes to its caller-selected
output and does not scrub that output.  With a 5,816-byte TCB and two 32-byte socket
descriptors, the logical network-table cost is 97,480 bytes per connection;
the four XMEM table allocations are normalized independently, so one
connection reserves 97,504 bytes and capacity uses the exact aggregate.

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

Incoming records require legacy record version `0x0303` and are bounded
separately for plaintext and protected records. A compatibility
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

No remote API credential should be provisioned until the intended endpoint's
current chain is representable by the installed bundle and a credential-free
live handshake succeeds on the machine.

## Verified Tests

Native guest tests cover:

- canonical DER signature integers and a real certificate signature;
- valid, corrupt, and out-of-range ECDSA inputs;
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

The test private scalars are deliberately trivial and never enter a product
trust bundle.

## Remaining Release Blockers

### Trust lifecycle

- Define a signed native trust-bundle update format and immutable bootstrap
  verification key or reviewed physical provisioning ceremony.
- Persist accepted generation state if rollback resistance is required.
- Establish an explicit root/intermediate policy and expiry/rotation process.
- Decide whether revocation is supported through stapled OCSP, short-lived
  scoped anchors, or another bounded policy.

### Protocol correctness

- Convert the currently blocking connection handshake into a bounded
  cooperative state machine with cancellation and precise timeout statuses.
- Finish graceful close draining and distinguish EOF, retryable I/O, timeout,
  and protocol failure throughout the public connection API.
- Add bounded `KeyUpdate` support before long-lived streaming connections are
  considered production-ready; it is currently rejected fail-closed.
- Run credential-free live interoperability against every intended endpoint
  after provisioning a reviewed scoped trust bundle.
- Keep the public and private ClientHello profiles separate; do not assign
  experimental wire formats to registered NamedGroup values.

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
use, but alternating application receive calls retain independent state. The
enforced lock order is 10, then KDOS HMAC/HKDF lock 9, then checked BIOS crypto
lock 8.

RSA's core-0 phase gate remains an additional protection for RSA scratch. The
BIOS SHA-256 transaction uses a complete private context per core, validates
all caller spans, and wipes on every terminal path; it is isolated from
SHA-256 work on other cores. It is not per task, so a cooperative task must
not yield with an open `SHA256-INIT` transaction and permit another task on the
same core to reinitialize it. The TLS owner prevents that situation on the
networking paths covered above. True parallel TLS progress still requires
moving the remaining module-global state into connection-owned workspaces.

## Acceptance Before Provider Credentials

1. All native TLS/X.509/ECDSA/RSA tests pass with no unresolved KDOS words.
2. The installed trust bundle is reviewed and scoped to the target endpoint.
3. The RTC is valid and survives the intended boot/power model.
4. A credential-free live handshake authenticates the expected chain.
5. HTTP response bytes can be streamed without overflowing TLS or transcript
   buffers.
6. Only then may a provider retrieve an in-memory credential and construct an
   Authorization header.
