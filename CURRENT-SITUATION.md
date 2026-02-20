# Megapad-64 — Current Situation

**Date:** 2026-02-21  
**Branch:** `main`  
**HEAD:** `095a7bc`  
**Status:** 1,387 tests passing (3 `TestPrivilege` tests expected-fail — C++ accel trap path).

---

## 1. Recent changes (last 2 days, 16 commits)

### Crypto hardware

- **SHA3-512** — full RTL + emulator + BIOS + KDOS support.  Byte-level
  registers, 7-bit addressing, 64-byte digest via multi-squeeze.
- **SHA-256 accelerator** — new hardware block at MMIO 0x0940.  RTL
  (`mp64_sha256.v`), emulator device, 5 BIOS words, KDOS `SHA256` /
  `HMAC-SHA256` / `HKDF-SHA256-EXTRACT` / `HKDF-SHA256-EXPAND`.
- **AES-128 mode** — `AES-KEY-MODE!` selects 128-bit key (10-round
  schedule vs 14).  Used by TLS cipher suite 0x1301.
- **CRC relocation** — moved from 0x7C0 → 0x0980 across all layers
  (RTL, emulator, BIOS, C++ accel, Forth) to make room for SHA-256/KEM.
- **AES-GCM partial block masking** — fixed last-block encryption to
  AND the keystream mask for sub-16-byte blocks.

### TLS 1.3

- **Dual-mode cipher suites** — 0xFF01 (AES-256-GCM + SHA3-256) and
  0x1301 (AES-128-GCM + SHA-256, RFC 8446 standard).
- **Record framing** — reassembly buffers (`TLS-RBUF-*`), proper
  multi-message handshake processing, CCS tolerance.
- **SNI** — `TLS-SNI-HOST` / `TLS-SNI-LEN` for Server Name Indication.

### Network tools (tools.f)

- **HTTP/HTTPS client** with TLS, chunked transfer, content-length.
- **FTP/FTPS** active-mode client, TLS upgrade for FTPS.
- **Gopher** client.
- **DNS-LOOKUP** convenience word.

### System

- **Userland memory isolation** — per-process dictionary in ext mem.
- **Headless mode** — `--headless` TCP terminal server on port 6464.
- **Ext mem** — `--extmem` flag (default 16 MiB), C++ accel routing fix.
- **Micro-cluster fix** — `_cluster_dispatch` writes code to correct region.
- **NET-IDLE** — `IDL` instruction + GIL yield in all protocol loops.
- **autoexec.f → graphics.f → tools.f** boot chain.

## 2. What's known-broken

### TestPrivilege (3 tests)

The C++ accelerator raises `TrapError` directly instead of dispatching
through the IVT.  Pure-Python mode passes all 3.  Low priority — the
privilege enforcement itself works; only the trap *delivery path* differs.

## 3. Architecture at a glance

| Layer | File(s) | Lines |
|-------|---------|-------|
| BIOS | `bios.asm` → `bios.rom` | 12,162 (346 words) |
| KDOS | `kdos.f` | 10,225 (871 colon + 490 var = 1,361 entities) |
| Tools | `tools.f` | 990 |
| CPU emulator | `megapad64.py` | 2,868 |
| SoC | `system.py` | 994 |
| Devices | `devices.py` | 1,875 |
| CLI | `cli.py` | 1,347 |
| C++ accel | `accel/mp64_accel.cpp` | ~1,930 |
| Tests | `tests/test_system.py` | 19,216 (1,316 tests, 69 classes) |
|       | `tests/test_networking.py` | 1,239 (48 tests, 9 classes) |
|       | `tests/test_megapad64.py` | 2,193 (23 tests) |

**Total tests: 1,387**

## 4. MMIO address map (crypto region)

| Offset | Peripheral |
|--------|-----------|
| 0x0700 | AES-256/128-GCM |
| 0x0780 | SHA-3/SHAKE (96 bytes) |
| 0x0800 | TRNG |
| 0x0880 | Field ALU |
| 0x08C0 | NTT Engine |
| 0x0900 | KEM (ML-KEM-512) |
| 0x0940 | SHA-256 |
| 0x0980 | CRC32/CRC64 |
| 0x0A00 | Framebuffer |

## 5. How to run things

```bash
make test             # all 1,387 tests (~23s with C++ accel)
make test-one K=X     # single class/test
make test-status      # check progress
make test-net         # networking tests (requires TAP)
make disk             # rebuild sample.img

# Interactive:
python cli.py --bios bios.asm --storage sample.img --nic-tap

# Headless:
python cli.py --bios bios.asm --storage sample.img --headless
python cli.py --connect localhost:6464
```
