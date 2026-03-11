# TLS 1.3 Hardening Roadmap

Status: **Phase 1 Complete — Phase 2 Next**
Last updated: 2025-07-08

## Current State

The TLS 1.3 stack (kdos.f §16.8–§16.11) provides a structurally complete
client-side handshake with dual cipher suite support (0x1301 standard +
0xFF01 private), hybrid PQ key exchange (X25519 + ML-KEM-512), full key
schedule, record encryption/decryption, Finished MAC verification, and a
BSD socket API.

**Fatal gap**: the server is never authenticated.  Certificates and
CertificateVerify are hashed into the transcript but their contents are
ignored.  Every connection is vulnerable to active MITM.

---

## Issues Inventory

Every bug and gap found during the 2026-03-11 audit, mapped to the phase
that addresses it.

### P0 — CRITICAL (no security without fixing)

| # | Issue | Location | Description | Fix Phase |
|---|-------|----------|-------------|-----------|
| 1 | **No certificate validation** | kdos.f L9937, L10496–10499 | `TLSHT-CERTIFICATE` handler only appends to transcript. No X.509 parsing, no chain walk, no trust anchor, no expiry, no hostname/SAN match, no revocation. Any MITM succeeds. | **1b, 1e** |
| 2 | **CertificateVerify never checked** | kdos.f L10500–10504 | `TLSHT-CERT-VERIFY` handler hashes into transcript and returns success. Signature algorithm, signature bytes, and verification input are all ignored. Server identity is never proven. | **1c, 1d** |

### P1 — HIGH (will cause failures or silent misbehaviour with real servers)

| # | Issue | Location | Description | Fix Phase |
|---|-------|----------|-------------|-----------|
| 3 | **Incoming alerts ignored** | kdos.f L10654 | `TLS-RECV-DATA` silently drops any decrypted record with content type ≠ `TLS-CT-APP-DATA` (returns 0). Server `close_notify`, `bad_record_mac`, `handshake_failure` etc. are all swallowed. Client can't distinguish "no data" from "server terminated". | **3a** |
| 4 | **HelloRetryRequest not detected** | kdos.f L9938, `TLS-PARSE-SERVER-HELLO` | Comment says "HRR → abort" but the code never checks if ServerHello random == SHA-256("HelloRetryRequest"). HRR is misparsed as normal SH, producing garbage keys. | **3b** |
| 5 | **Handshake state ordering not enforced** | kdos.f `TLS-PROCESS-HS-MSG` L10459+ | Dispatches on message type without validating the current `HS-STATE` permits that transition. Finished accepted before Certificate, EncryptedExtensions after Finished, duplicates not rejected. RFC 8446 §4.4 mandates strict ordering. | **3c** |
| 6 | **TCB leak on failed TCP connect** | kdos.f L10753 | If TCP doesn't reach ESTABLISHED, `TLS-CONNECT` returns 0 but never calls `TCP-CLOSE` on the allocated TCB. TLS context is recoverable (zero-filled = TLSS-NONE) but the TCB slot is permanently lost. | **4e** |

### P2 — MEDIUM (correctness / interop)

| # | Issue | Location | Description | Fix Phase |
|---|-------|----------|-------------|-----------|
| 7 | **Outer record content type not validated** | kdos.f `TLS-CONNECT` L10773+ | After ServerHello, all encrypted records must have outer type 23 (application_data) per RFC 8446 §5.1. Code only checks for CCS=20 vs "everything else" — doesn't reject illegal outer types. | **4b** |
| 8 | **`TLS-CLOSE` skips half-close** | kdos.f L10674–10681 | State jumps ESTABLISHED→NONE immediately after sending `close_notify`, without waiting for server's `close_notify`. `TLSS-CLOSING` is defined but never used. TCP connection closed before server can acknowledge. | **4d** |
| 9 | **No ALPN extension** | kdos.f `TLS-BUILD-CLIENT-HELLO` | `HTTPS-GET` uses HTTP/1.1 but ClientHello doesn't advertise it via ALPN (RFC 7301). Some servers reject connections without ALPN. | **4a** |
| 10 | **Post-handshake messages silently dropped** | kdos.f L10654 | `TLS-RECV-DATA` returns 0 for non-app-data records. Server-sent `NewSessionTicket` and `KeyUpdate` messages are lost with no indication. `KeyUpdate` in particular means the connection will break when the server starts using new keys. | **3a, 6** |
| 11 | **Global scratch buffers prevent concurrent handshakes** | kdos.f L9984–9990 | `TLS-HS-KYBER-*`, `TLS-HS-TRANSCRIPT`, `TLS-FINISHED-KEY`, `TLS-VERIFY-DATA` etc. are all globals. 16 TLS context slots exist but only one handshake can run at a time. Multi-core parallel TLS will corrupt state. | **6** |
| 12 | **No record size validation** | kdos.f `TLS-READ-RECORD` | No check that record payload ≤ 16640 bytes (2^14 + 256) per RFC 8446 §5.1. Oversized records accepted, could overflow TLS-RECV-REC (16896 bytes). | **4c** |
| 13 | **Ambiguous `TLS-RECV-DATA` return** | kdos.f L10641–10657 | Returns 0 for both "no data available" and "received non-app-data record". Caller cannot distinguish the two cases. Should return distinct error codes or process non-app-data internally. | **3a** |

### P3 — LOW (documentation / fragility / future)

| # | Issue | Location | Description | Fix Phase |
|---|-------|----------|-------------|-----------|
| 14 | **AES-STATUS@ documentation inconsistency** | bios-forth.md L667 vs kdos.f L749 | bios-forth.md says `0=busy, 1=done, 2=auth fail`. kdos.f says `0=idle, 2=done, 3=auth-fail`. extended-tpu-spec.md says "busy/done/auth-fail flags". The actual hardware uses `0=idle, 2=done, 3=auth-fail` (confirmed by `.AES-STATUS` at L819 and test_system.py). bios-forth.md and BIOS-DICTIONARY.md need correction. | **doc fix** |
| 15 | **Hardcoded ClientHello extension length** | kdos.f L10234 | Magic number `907` (=7+878+12+10) for base extension length. If any extension sizes change, this must be manually updated. No runtime validation that `_TBCH-POS` matches the predicted total. | **4a** (when adding ALPN, refactor to compute dynamically) |
| 16 | **No session resumption / PSK / 0-RTT** | — | `TLS-CTX.PSK` field exists (+520) but is never populated. Every connection requires a full handshake. Performance penalty for repeated connections. | **6** |
| 17 | **No KeyUpdate support** | — | Long-lived connections cannot rotate keys. If a server sends KeyUpdate, the client will silently drop it (issue #10) and then fail to decrypt subsequent records. | **6** |
| 18 | **No post-handshake client auth** | — | No `CertificateRequest` handling. Server cannot request client certificates. | **6** |

---

## Phase 1 — Server Authentication (P0)  ← COMPLETE

**Implemented** in kdos.f §16.7a–§16.7d, tested in tests/test_system.py.

| Sub-phase | Status | Section | Words |
|-----------|--------|---------|-------|
| 1a. ASN.1/DER parser | **Done** | §16.7a | `DER-TAG@`, `DER-LEN@`, `DER-NEXT`, `DER-ENTER`, `DER-SKIP`, `DER-FIND-TAG` |
| 1b. X.509 leaf parser | **Done** | §16.7b | `X509-PARSE`, `X509-PARSE-SPKI`, `X509-PARSE-EXTENSIONS`, `X509-CHECK-HOST`, `X509-OID-MATCH` |
| 1c. P-256 ECDSA verify | **Done** | §16.7c | `EC-DOUBLE`, `EC-ADD`, `EC-AFFINE`, `EC-MUL`, `ECDSA-DECODE-SIG`, `ECDSA-P256-VERIFY` |
| 1d. CertificateVerify wiring | **Done** | §16.7d | `TLS-VERIFY-CERT-SIG` — builds RFC 8446 §4.4.3 content, dispatches to ECDSA |
| 1e. Certificate handler wiring | **Done** | §16.7d | `TLS-PARSE-CERTIFICATE` — extracts leaf cert, calls `X509-PARSE`, hostname check |
| FSM wiring | **Done** | §16.9 | `TLS-PROCESS-HS-MSG` Certificate/CertVerify branches call new verifiers |

**Test classes added**: `TestKDOSASN1`, `TestKDOSX509`, `TestKDOSECDSA`, `TestKDOSTLSCertVerify`

**Remaining Phase 1 gaps** (deferred to Phase 2+):
- No certificate chain validation (only leaf parsed)
- No trust anchor / CA store
- No certificate expiry check
- No CRL / OCSP revocation check
- Ed25519 and RSA-PSS verify not yet implemented (only ECDSA-P256-SHA256)

### Phase 1 Implementation Details

### 1b. X.509 leaf certificate parser
**File**: kdos.f §1.13 (new)
- `X509-PARSE ( cert clen -- flag )`
  - Extract: subject, issuer, notBefore/notAfter, signature algorithm,
    subject public key info (algorithm OID + key bytes), signature.
  - Store results in global `_X509-*` scratch buffers.
  - Returns 0 on success, -1 on parse error.
- `X509-EXTRACT-PUBKEY ( cert clen out -- algo klen )`
  - `algo`: 0x0403=ECDSA-P256, 0x0807=Ed25519, 0x0804=RSA-PSS
  - Copy raw public key bytes to `out`, return algorithm + length.
- `X509-CHECK-HOST ( hostname hlen -- flag )`
  - Compare SNI hostname against subject CN / SAN dNSName.
  - Support wildcard `*.example.com` matching.

### 1c. P-256 ECDSA verification
**File**: kdos.f §1.14 (new)
- Built on existing Field ALU primitives: `PRIME-P256`, `FADD`, `FSUB`,
  `FMUL`, `FSQR`, `FINV`, `FCMOV`, `FCEQ`.
- P-256 curve constants (G, n, a, b) as 32-byte CREATE buffers.
- Jacobian point operations:
  - `EC-DOUBLE ( Jx Jy Jz Rx Ry Rz -- )` — point doubling
  - `EC-ADD    ( Jx Jy Jz Px Py Pz Rx Ry Rz -- )` — point addition
  - `EC-MUL    ( k Px Py Rx Ry -- )` — scalar multiply (double-and-add)
  - `EC-AFFINE ( Jx Jy Jz Ax Ay -- )` — Jacobian → affine via z⁻¹
- ECDSA verify:
  - `ECDSA-P256-VERIFY ( hash hlen pubkey sig slen -- flag )`
    - Decode DER signature → (r, s)
    - Compute u1 = z·s⁻¹ mod n, u2 = r·s⁻¹ mod n
    - Compute R = u1·G + u2·Q
    - Verify r ≡ R.x mod n
    - Constant-time throughout (FCMOV/FCEQ for comparisons)

### 1d. TLS CertificateVerify verification
**File**: kdos.f §16.9 (modify existing handler)
- `TLS-VERIFY-CERT-SIG ( ctx msg mlen -- flag )`
  - Build verification input per RFC 8446 §4.4.3:
    `0x20×64 ‖ "TLS 1.3, server CertificateVerify" ‖ 0x00 ‖ H(transcript)`
  - Extract signature algorithm from CertificateVerify message
  - Dispatch to `ECDSA-P256-VERIFY` (0x0403) or reject unsupported
  - Returns 0 on valid, -1 on failure
- Wire into `TLS-PROCESS-HS-MSG` → `TLSHT-CERT-VERIFY` branch

### 1e. TLS Certificate message parser
**File**: kdos.f §16.9 (modify existing handler)
- `TLS-PARSE-CERTIFICATE ( msg mlen -- flag )`
  - Walk the certificate_list from the Certificate handshake message
  - Extract leaf certificate (first entry)
  - Call `X509-PARSE` / `X509-EXTRACT-PUBKEY` on the leaf
  - Store server public key in handshake scratch buffer for CertVerify
  - Call `X509-CHECK-HOST` against TLS-SNI-HOST if set
- Wire into `TLS-PROCESS-HS-MSG` → `TLSHT-CERTIFICATE` branch

**Deliverable**: MITM protection for ECDSA-P256-SHA256 servers (covers
~85% of the real internet as of 2026).

---

## Phase 2 — Chain Validation & Trust Store (P1)

### 2a. Root CA trust anchor store
- Embed 4–8 most critical root CAs as DER-encoded public keys:
  - ISRG Root X1 (Let's Encrypt)
  - DigiCert Global Root G2
  - GlobalSign Root R3
  - Google Trust Services (GTS Root R1)
  - Cloudflare Root CA (for CF-fronted sites)
- `TLS-TRUST-FIND ( issuer-hash -- ca-pubkey | 0 )`
- ~2 KB of data space for compressed root keys.

### 2b. Certificate chain walk
- `X509-VERIFY-CHAIN ( cert-list n -- flag )`
  - For each cert[i]: verify cert[i].signature using cert[i+1].pubkey
  - Final cert: verify against trust anchor store
  - Reject chains deeper than 4 (configurable)
  - Return 0 on full chain verified, -1 on failure

### 2c. Validity period checking
- `X509-CHECK-VALIDITY ( cert -- flag )`
  - Parse notBefore / notAfter (UTCTime or GeneralizedTime)
  - Compare against RTC (`TIME@` from BIOS)
  - Reject expired or not-yet-valid certificates

---

## Phase 3 — Alert & Error Handling (P1)

### 3a. Incoming alert processing
- `TLS-PROCESS-ALERT ( ctx data len -- )`
  - Parse alert level + description
  - Fatal alerts → abort connection, set error state
  - `close_notify` → half-close, set TLSS-CLOSING
- Wire into `TLS-RECV-DATA` non-app-data path

### 3b. HelloRetryRequest detection
- In `TLS-PARSE-SERVER-HELLO`: check if random == SHA-256("HelloRetryRequest")
- If HRR detected: either re-send ClientHello with requested changes or abort

### 3c. Handshake state transition validation
- `TLS-HS-EXPECT ( current-state msg-type -- flag )`
- Enforce RFC 8446 §4.4 ordering:
  SH → EE → Cert* → CV* → Finished
- Reject out-of-order or duplicate messages

---

## Phase 4 — Robustness & Interop (P2)

### 4a. ALPN extension
- Add `application_layer_protocol_negotiation` (0x0010) to ClientHello
- Advertise "http/1.1" for HTTPS, "h2" optional
- Parse server's ALPN response in EncryptedExtensions

### 4b. Outer record type enforcement
- After ServerHello, verify all encrypted records have outer type 23
- Reject any non-23 outer type (except CCS = 20, which is already handled)

### 4c. Record size validation
- Enforce max 16640 bytes (2^14 + 256) per RFC 8446 §5.1
- Reject oversized records with record_overflow alert

### 4d. Proper half-close
- `TLS-CLOSE`: send close_notify, transition to TLSS-CLOSING
- Wait for server's close_notify (with timeout) before TCP-CLOSE
- Handle server-initiated close_notify in recv path

### 4e. TCB leak fix in TLS-CONNECT
- On TCP connect failure: call TCP-CLOSE on the allocated TCB before returning 0

---

## Phase 5 — Extended Signature Support (P2)

### 5a. Ed25519 verification
- Edwards curve point operations using existing Field ALU (PRIME-25519)
- `ED25519-VERIFY ( msg mlen pubkey sig -- flag )`
- Wire into CertificateVerify dispatcher for 0x0807

### 5b. RSA-PSS verification
- Multi-word modular exponentiation using `FPOW` with custom prime
- PKCS#1 v2.1 PSS signature verification
- Wire into CertificateVerify dispatcher for 0x0804
- Lower priority: most modern servers prefer ECDSA

---

## Phase 6 — Future (P3)

- **Session resumption**: process NewSessionTicket, PSK handshake mode
- **KeyUpdate**: handle post-handshake key rotation
- **Client certificates**: CertificateRequest handling
- **OCSP stapling**: certificate status in EncryptedExtensions
- **Concurrent handshakes**: per-connection scratch buffers (multi-core safe)

---

## Implementation Notes

### Building Blocks Available
| Primitive | Source | Notes |
|-----------|--------|-------|
| Field ALU GF(P-256) | Per-core ISA + BIOS | `PRIME-P256`, `FADD/FSUB/FMUL/FSQR/FINV` |
| SHA-256 | Per-core ISA | Streaming: `SHA256-INIT`/`UPDATE`/`FINAL` |
| SHA-3/256 | MMIO 0x780 | For 0xFF01 suite |
| HMAC/HKDF | Forth (kdos.f) | Dual-mode SHA256/SHA3 |
| Constant-time ops | Field ALU ISA | `FCMOV`, `FCEQ`, `VERIFY` |
| AES-GCM | MMIO 0x700 | 128/256-bit key modes |

### Size Budget
- P-256 ECDSA verify: ~200 lines Forth
- Jacobian EC point ops: ~150 lines Forth
- ASN.1 DER parser: ~120 lines Forth
- X.509 minimal parser: ~150 lines Forth
- CertificateVerify wiring: ~60 lines Forth
- Certificate message parser: ~80 lines Forth
- P-256 constants (G, n, a, b): ~20 lines data
- Trust anchor store: ~100 lines data
- **Total Phase 1**: ~750 lines added to kdos.f

### Test Plan
- `TestKDOSASN1`: DER parser on synthetic TLV inputs
- `TestKDOSX509`: Certificate parsing with known test certs
- `TestKDOSECDSA`: P-256 point operations + ECDSA verify with NIST vectors
- `TestKDOSTLSCertVerify`: Full CertificateVerify against test transcripts
- All existing TLS tests must continue to pass
