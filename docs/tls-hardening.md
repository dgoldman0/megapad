# Native TLS Hardening

Status: authenticated P-256 profile implemented; general WebPKI incomplete
Last updated: 2026-07-10

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
6. Every required ECDSA-P256-SHA256 certificate signature.
7. `CertificateVerify`, proving possession of the authenticated leaf key.
8. The server `Finished` MAC.

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
- P-256 uncompressed SubjectPublicKeyInfo.
- ECDSA with SHA-256, or ECDSA with SHA-384 on a certificate used only as an
  explicitly trusted anchor.
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

### Trust Bundles

The default trust store is empty. `TLS-TRUST-LOAD` copies and validates an
in-memory bundle, allowing at most eight anchors and 32768 total bytes. An
anchor must be a CA and, when KeyUsage is present, permit certificate signing.

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
may contain irrelevant entries. The bounded builder:

- requires the leaf not to be a CA;
- checks digitalSignature and serverAuth when those extensions are present;
- checks SAN hostname and RTC validity;
- links issuer/subject and AKI/SKI when both key identifiers are available;
- checks CA BasicConstraints, keyCertSign, EKU restrictions, and pathLen;
- verifies each P-256/SHA-256 child signature;
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

## Deployment Reality

The code does not yet validate arbitrary public certificate chains. It lacks
P-384, RSA, Ed25519, SHA-384 certificate-signature verification, and a curated
root program. Those omissions matter because modern chains commonly cross
algorithm boundaries.

For example, the `api.openai.com` chain observed on 2026-07-10 used a P-256
leaf signed with ECDSA-SHA256 by Google Trust Services `WE1`; `WE1` was signed
with ECDSA-SHA384 by the P-384 `GTS Root R4`. The current profile can connect
only if the exact `WE1` certificate is provisioned as an anchor scoped to
`api.openai.com` (or an intentionally selected parent scope). That is a narrow,
updateable deployment profile, not equivalent to trusting the GTS root or the
public WebPKI. Intermediate rotation requires a trust-bundle update.

No remote API credential should be provisioned until the intended endpoint's
current chain is representable by the installed bundle and a credential-free
live handshake succeeds on the machine.

## Verified Tests

Native guest tests cover:

- canonical DER signature integers and a real certificate signature;
- valid, corrupt, and out-of-range ECDSA inputs;
- deterministic root/intermediate/leaf fixtures with CA, KU, EKU, SAN,
  SKI/AKI, pathLen, and validity constraints;
- hostname, wildcard, clock, signature, scope, empty-store, and truncation
  failures;
- reordered and extraneous presented certificates;
- exact TLS Certificate framing and bounded entry extensions;
- stale-key clearing on every failed Certificate message;
- a real CertificateVerify signature from the fixture leaf key;
- rejection of early Finished and unauthenticated application-key derivation;
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

- Harden `ServerHello` bounds and detect HelloRetryRequest.
- Reassemble handshake messages fragmented across TLS records.
- Make transcript overflow an explicit fatal status instead of truncation.
- Parse incoming alerts and distinguish close, retryable I/O, and fatal errors.
- Validate compatibility-mode CCS contents and all record versions/lengths.
- Add ALPN for HTTP/1.1 before relying on general HTTPS endpoints.
- Finish failed-connect cleanup and graceful close behavior across all states.

### Algorithm coverage

- Add ECDSA-SHA384 and P-384 verification for common GTS chains.
- Add RSA-PSS and Ed25519 only with native vectors and bounded key sizes.
- Advertise only signature algorithms whose complete certificate and
  CertificateVerify paths are implemented. The current ClientHello advertises
  only `ecdsa_secp256r1_sha256`.

### Concurrency

Handshake transcript, certificate descriptors, cryptographic scratch, and
hybrid key-exchange buffers are global. The current owner loop permits one
handshake at a time. True concurrent handshakes require per-context state or a
machine-wide handshake lock before multitasking can expose parallel TLS calls.

## Acceptance Before Provider Credentials

1. All native TLS/X.509/ECDSA tests pass with no unresolved KDOS words.
2. The installed trust bundle is reviewed and scoped to the target endpoint.
3. The RTC is valid and survives the intended boot/power model.
4. A credential-free live handshake authenticates the expected chain.
5. HTTP response bytes can be streamed without overflowing TLS or transcript
   buffers.
6. Only then may a provider retrieve an in-memory credential and construct an
   Authorization header.
