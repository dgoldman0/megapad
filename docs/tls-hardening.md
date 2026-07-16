# Native TLS Hardening

Status: authenticated bounded P-256/RSA-2048 profile implemented; general WebPKI incomplete
Last updated: 2026-07-14

## Purpose

MegaPad must authenticate a remote service on the machine itself. A Linux
companion, emulator service, or host certificate library is not part of the
security architecture. Host tools may generate fixtures and drive tests, but
KDOS parses the wire message, builds the path, verifies signatures, checks the
clock and hostname, and gates application traffic.

The relevant standards are [TLS 1.3 (RFC 8446)](https://www.rfc-editor.org/rfc/rfc8446)
and [PKIX certificates (RFC 5280)](https://www.rfc-editor.org/rfc/rfc5280).
The current implementation is a deliberately bounded profile of those
standards, not a complete WebPKI implementation.

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
9. Exact `http/1.1` ALPN selection when the caller requests that profile.

Only step 7 sets `TLS-CTX.PEER-AUTH`. The application key schedule additionally
requires `TLSH-SERVER-FINISHED`; it otherwise returns without changing the
connection to `TLSS-ESTABLISHED`. Starting a new ClientHello clears the retained
leaf key, certificate status, and authentication bit.

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

`TLS-CONNECT-ALPN` stores the requested and negotiated application protocol in
the connection context. The currently implemented application profile is
`http/1.1`; its ClientHello offer and EncryptedExtensions selection are both
checked exactly. Plain `TLS-CONNECT` requests no ALPN profile.

Ordinary `TLS-CONNECT` and `TLS-CONNECT-ALPN` use the interoperable public
profile: TLS 1.3 `TLS_AES_128_GCM_SHA256` and X25519. Its
`signature_algorithms` extension is exactly
`ecdsa_secp256r1_sha256, rsa_pss_rsae_sha256`; the separate
`signature_algorithms_cert` extension is exactly
`ecdsa_secp256r1_sha256, rsa_pkcs1_sha256`. The standard extension block is 77
bytes.

`TLS-CONNECT-HYBRID` and `TLS-CONNECT-HYBRID-ALPN` explicitly select MegaPad's
private X25519 plus ML-KEM-512 profile. That profile uses IANA private-use
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
If a TLS record exceeds the destination slice, the context records an offset
and remaining length into the shared plaintext buffer and drains that data
before decrypting another record. A small HTTP receive scratch buffer therefore
cannot silently truncate large response bodies.

`MS@` and `EPOCH@` reconstruct all eight RTC bytes. Certificate and token
deadlines therefore use the full-width clock rather than a truncated timer.

Incoming records require legacy record version `0x0303` and are bounded
separately for plaintext and protected records. A compatibility
ChangeCipherSpec is ignored only when it has the exact one-byte `0x01` form
permitted during the handshake. Incoming alerts clear peer authorization and
distinguish clean `close_notify` from fatal or malformed records. Application
send and receive both require an established, authenticated context.

Session resumption is not implemented. Authenticated post-handshake
`NewSessionTicket` messages are reassembled in the bounded handshake buffer,
validated through every nested nonce, ticket, and extension length, and then
discarded; lifetime is capped at seven days, every extension type must be
unique, and `early_data` requires its exact four-byte payload. KeyUpdate,
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
- ALPN offer, exact selection, missing-selection, and per-context state;
- standard-only and explicit private-hybrid ClientHello wire layouts;
- immediate established/readable TCP waits and record-fill completion;
- TCP/TLS send backpressure without retransmission-buffer or sequence loss;
- multi-read delivery of application records larger than the caller buffer;
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

ALPN result, traffic keys, authorization state, and connection errors are
per-context. Handshake transcript, certificate descriptors, cryptographic
scratch, record buffers, and hybrid key-exchange buffers remain global. The
current API does not yet enforce a machine-wide TLS/crypto owner, so callers
must serialize all TLS and cryptographic use. RSA's core-0 phase gate protects
only RSA scratch; it does not make the BIOS's single `sha_blk_buf` and
`sha_blk_off` per-core. An unrelated parallel `SHA256-*` operation remains
forbidden because it can corrupt X.509, PSS, or transcript hashing. The
cooperative TLS owner is a required integration boundary until all shared
scratch, including SHA block scratch, becomes per-context.

## Acceptance Before Provider Credentials

1. All native TLS/X.509/ECDSA/RSA tests pass with no unresolved KDOS words.
2. The installed trust bundle is reviewed and scoped to the target endpoint.
3. The RTC is valid and survives the intended boot/power model.
4. A credential-free live handshake authenticates the expected chain.
5. HTTP response bytes can be streamed without overflowing TLS or transcript
   buffers.
6. Only then may a provider retrieve an in-memory credential and construct an
   Authorization header.
